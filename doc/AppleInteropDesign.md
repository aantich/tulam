# Apple/macOS Interop Design: Native Backend with ObjC Runtime Bridge

## 1. Architecture Overview

tulam on macOS compiles to **native machine code** (ARM64/x86_64). All tulam types, pattern matching, closures, and dispatch compile natively — no Objective-C source generation.

Interaction with Apple frameworks (AppKit, Foundation, etc.) happens through the **ObjC runtime C API**:
- `objc_msgSend` for method calls
- `objc_getClass` / `sel_registerName` for class/selector lookup
- `objc_allocateClassPair` / `class_addMethod` for subclassing (rare)

```
+--------------------------------------------------+
|  tulam pure code          → native machine code   |
|  tulam action blocks      → native + objc_msgSend |
|  tulam extends ObjCClass  → ObjC class registered |
|                              via runtime C API     |
+--------------------------------------------------+
|  ObjC Runtime (libobjc)  — always present on macOS |
+--------------------------------------------------+
|  Apple Frameworks (AppKit, Foundation, Metal...)   |
+--------------------------------------------------+
```

### Core Principle

**ObjC objects are opaque pointer values.** They can exist anywhere in tulam code. It's the *operations on them* that are classified as pure or effectful:

- **Pure**: readonly property access, deterministic queries (`title`, `frame`, `length`, `isEqual`)
- **Effectful**: mutations, IO, allocation (`setTitle`, `addSubview`, `makeKeyAndOrderFront`, `.new(...)`)

Effectful operations must be inside `action` blocks. Pure operations can be called from anywhere.

---

## 2. Import and Metadata

### 2.1 Importing Apple Frameworks

```tulam
import AppKit target native;             // ObjC framework — full header parse
import Foundation target native;
import CoreGraphics target native;       // C framework — same target, C API
import { NSWindow, NSView } from AppKit target native;  // selective
```

All Apple frameworks use `target native` — the metadata resolver detects whether a framework contains ObjC classes (via presence of `@interface` declarations in headers) and enables ObjC dispatch automatically. No separate `target objc` needed.

### 2.2 Metadata Source

The resolver parses framework headers via **libclang** from the Xcode SDK:
```
$(xcrun --show-sdk-path)/System/Library/Frameworks/<Name>.framework/Headers/
```

From headers, the resolver extracts:

| What | How It's Used |
|------|---------------|
| `@interface` declarations | Extern class with fields, methods, inheritance |
| `@protocol` declarations | Algebra (with optional method defaults) |
| `@property` attributes | Purity classification: `readonly` → pure, `readwrite` setter → effectful |
| Method signatures | Parameter types, return types, selector name |
| `NS_ENUM` / `NS_OPTIONS` | Primitive type + Bits/Eq instances |
| Nullability annotations | `_Nullable` → `Maybe(T)`, `_Nonnull` → `T` |
| Inheritance chains | Subtype relationships for `as` downcast |
| Protocol conformances | Auto-generated algebra instances |

### 2.3 Selector Name Translation

ObjC selectors use colons: `tableView:objectValueForTableColumn:row:`. tulam translates:

```
tulam method call                                ObjC selector
----------------------------------------------   ------------------------------------
window.makeKeyAndOrderFront(sender)              makeKeyAndOrderFront:
label.setStringValue(text)                       setStringValue:
tv.tableView_objectValueFor_row(tv, col, row)    tableView:objectValueFor:row:
window.center()                                  center
```

**Rules:**
1. No args, no colon: `center` → `center`
2. One arg, colon appended: `setTitle(t)` → `setTitle:`
3. Multi-arg, underscores → colons: `a_b_c(x,y,z)` → `a:b:c:`
4. Override with `@selector` annotation when conventions don't match:
   ```tulam
   @selector("application:didFinishLaunchingWithOptions:")
   function application_didFinishLaunching(self:AppDelegate, app:NSApplication, opts:NSDictionary) : Bool
   ```

---

## 3. Purity Classification

### 3.1 Rules

The compiler classifies each extern method as **pure** or **effectful**:

| ObjC Declaration | tulam Classification | Rationale |
|-----------------|---------------------|-----------|
| `@property (readonly) NSString *title` | **Pure** getter | Cannot mutate state |
| `@property (readwrite) NSString *title` | **Pure** getter, **effectful** setter | Getter reads, setter mutates |
| `- (NSRect)frame` | **Pure** | Returns value, no visible mutation |
| `- (void)setTitle:(NSString *)t` | **Effectful** | Mutates object state |
| `- (void)addSubview:(NSView *)v` | **Effectful** | Mutates view hierarchy |
| `- (void)makeKeyAndOrderFront:(id)s` | **Effectful** | IO (shows window on screen) |
| `- (BOOL)isEqual:(id)obj` | **Pure** | Deterministic query |
| `- (NSUInteger)count` | **Pure** | Readonly query |
| `- (instancetype)init*` | **Effectful** | Allocation |
| `+ (instancetype)new*` / `alloc` | **Effectful** | Allocation |
| `+ (NSColor *)redColor` | **Pure** | Singleton/constant factory |

### 3.2 Inference Heuristics

1. **`readonly` property** → pure getter
2. **`readwrite` property** → pure getter + effectful setter
3. **Return type `void`** → almost certainly effectful (why call it if not for side effects?)
4. **`set*` / `add*` / `remove*` / `insert*` / `replace*` naming** → effectful
5. **`init*` / `new*` / `alloc`** → effectful (allocation)
6. **Returns a value, no `void`, not a constructor** → pure by default
7. **Class methods returning the same type** (`+ (NSColor *)redColor`) → pure (factory/constant)

### 3.3 User Overrides

When the heuristic is wrong:

```tulam
// Mark a method as pure even though the default says effectful
extern class NSColor target native {
    @pure static function colorWithRed_green_blue_alpha(
        r:Float64, g:Float64, b:Float64, a:Float64) : NSColor
};

// Mark a method as effectful even though it returns a value
extern class NSMutableArray target native {
    @effectful function objectAtIndex(i:Int) : NSObject
    // Array might be mutated by another thread — user decides it's unsafe to call pure
};
```

### 3.4 What This Means in Practice

```tulam
// PURE — no action block needed
function describeWindow(w:NSWindow) : String =
    "Window: " ++ w.title() ++ " ("
        ++ show(w.frame().size.width) ++ "x"
        ++ show(w.frame().size.height) ++ ")";

function isWideWindow(w:NSWindow) : Bool =
    gt(w.frame().size.width, 800.0);

function colorName(c:NSColor) : String =
    match c.isEqual(NSColor.red())
        | True  -> "red"
        | False -> "other";

// EFFECTFUL — must be in action block
action setupWindow(w:NSWindow, title:String) = {
    w.setTitle(title);           // mutation
    w.center();                  // this is actually effectful (moves window)
    w.makeKeyAndOrderFront(Nothing);  // IO
};
```

---

## 4. Type Mapping

### 4.1 Automatic Translations

| tulam Type | ObjC/Apple Type | Notes |
|-----------|----------------|-------|
| `Int` | `NSInteger` / `int64_t` | Platform-width |
| `Int32` | `int32_t` | Fixed-width |
| `Float64` | `CGFloat` / `double` | `CGFloat` = `double` on 64-bit |
| `Bool` | `BOOL` | `YES`/`NO` |
| `String` | `NSString *` | Bridge at boundary |
| `Char` | `unichar` | UTF-16 code unit |
| `Unit` | `void` | |
| `Maybe(T)` | `T _Nullable` | `nil` ↔ `Nothing` |
| `Array(T)` | `NSArray<T> *` | Immutable |
| `MutArray(T)` | `NSMutableArray<T> *` | Mutable |
| `A -> B` | `B (^)(A)` | ObjC Block |
| tulam record | C struct or ObjC class | Depending on context |
| tulam sum type | Tagged C union | Not ObjC objects |

### 4.2 Struct Types

Apple uses many C structs (`NSRect`, `NSPoint`, `NSSize`, `CGRect`, etc.). These map directly to tulam records:

```tulam
// Auto-imported from headers:
record NSPoint = { x:Float64, y:Float64 };
record NSSize = { width:Float64, height:Float64 };
record NSRect = { origin:NSPoint, size:NSSize };

// Used directly in pure code:
function area(r:NSRect) : Float64 =
    r.size.width * r.size.height;

function inset(r:NSRect, dx:Float64, dy:Float64) : NSRect =
    NSRect { origin = NSPoint { x = r.origin.x + dx, y = r.origin.y + dy },
             size = NSSize { width = r.size.width - 2.0 * dx,
                             height = r.size.height - 2.0 * dy } };
```

These are **pure values** — no ObjC runtime involvement, just native structs.

### 4.3 NS_ENUM and NS_OPTIONS

```tulam
// NS_OPTIONS → primitive + Bits algebra
primitive NSWindowStyleMask;
instance Bits(NSWindowStyleMask) = intrinsic;

// Usage — combine with union from Bits algebra:
let style = NSWindowStyleMask.titled
    .union(NSWindowStyleMask.closable)
    .union(NSWindowStyleMask.resizable);

// NS_ENUM → tulam sum type with repr
type NSComparisonResult = OrderedAscending | OrderedSame | OrderedDescending;
repr NSComparisonResult as Int where {
    function toRepr(x:NSComparisonResult) : Int =
        match | OrderedAscending -> neg(1) | OrderedSame -> 0 | OrderedDescending -> 1,
    function fromRepr(i:Int) : NSComparisonResult =
        match | _ -> if lt(i, 0) then OrderedAscending
                     else if eq(i, 0) then OrderedSame
                     else OrderedDescending
};
```

### 4.4 Null Handling

```tulam
import AppKit target native [nullable = annotated];

// _Nonnull returns stay unwrapped:
let title = window.title();          // : String (guaranteed non-nil)

// _Nullable returns get Maybe:
let view = window.contentView();     // : Maybe(NSView)
match view
    | Just(v) -> v.setNeedsDisplay(True)
    | Nothing -> ();

// Default (no annotation) — configurable:
//   [nullable = strict]    → all reference returns are Maybe(T)
//   [nullable = annotated] → follow Apple's annotations (recommended)
//   [nullable = unsafe]    → no wrapping, nil = runtime crash
```

---

## 5. Subclassing ObjC Classes

This is the **only** scenario where tulam generates ObjC class metadata (registered via runtime C API at program startup). Everything else is just `objc_msgSend`.

### 5.1 When You Need It

You subclass when Apple's framework requires it:
- Custom views (`NSView`) for drawing
- Application/window delegates (`NSObject` + protocol)
- View controllers (`NSViewController`)
- Custom cells (`NSTableCellView`)

### 5.2 Syntax

```tulam
import AppKit target native;

/// Custom view with gradient background
class GradientView() extends NSView = {
    override function drawRect(self:GradientView, dirtyRect:NSRect) : Unit =
        action {
            let gradient = NSGradient.new(
                startingColor = NSColor.blue(),
                endingColor = NSColor.purple()
            );
            gradient.drawInRect(self.bounds(), angle = 270.0);
        }
};
```

### 5.3 What the Compiler Does

At program startup, before `main` runs:

```c
// Generated native code (conceptual):
Class GradientView = objc_allocateClassPair(objc_getClass("NSView"), "TLGradientView", 0);

// Register overridden method:
class_addMethod(GradientView,
    sel_registerName("drawRect:"),
    (IMP)tl_GradientView_drawRect,  // pointer to compiled tulam function
    "v@:{NSRect={NSPoint=dd}{NSSize=dd}}");

objc_registerClassPair(GradientView);
```

The `tl_GradientView_drawRect` function is compiled native tulam code that:
1. Receives `self` and `dirtyRect` as C parameters
2. Runs the tulam `action` block body
3. Makes `objc_msgSend` calls for `NSGradient.new`, `drawInRect`, etc.

### 5.4 Protocol Implementation

```tulam
class AppDelegate() extends NSObject implements NSApplicationDelegate = {
    function applicationDidFinishLaunching(self:AppDelegate, note:NSNotification) : Unit =
        action { ... }
};
```

`implements NSApplicationDelegate` means:
1. The registered ObjC class declares protocol conformance
2. The compiler checks that all `@required` protocol methods are implemented
3. `@optional` methods that aren't implemented simply aren't registered (ObjC `respondsToSelector:` returns NO)

### 5.5 ObjC Objects vs Pure tulam Objects

| | Pure tulam class | `extends ObjCClass` |
|--|---|---|
| Runtime representation | `CLMCON` (tagged tuple) | ObjC object (heap-allocated, refcounted) |
| Method dispatch | Tag-based via `ClassMeta` | `objc_msgSend` |
| Memory management | GC / native strategy | ARC |
| Visible to Apple frameworks | No | Yes |
| Pattern matching | Yes (match on constructors) | Via `as` downcast only |
| Fields | Positional tuple slots | ObjC instance variables |

Pure tulam classes that don't `extend` an ObjC class are **never** registered with the ObjC runtime. They compile to the same tagged tuples as sum types.

---

## 6. Protocols and Algebras

### 6.1 ObjC Protocol -> tulam Algebra (Auto-imported)

```objc
// In AppKit headers:
@protocol NSTableViewDataSource <NSObject>
@required
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView;
@optional
- (id)tableView:(NSTableView *)tv objectValueForTableColumn:(NSTableColumn *)col row:(NSInteger)row;
@end
```

```tulam
// Auto-generated by metadata resolver:
algebra NSTableViewDataSource(a:Type) = {
    function numberOfRowsInTableView(self:a, tableView:NSTableView) : Int,
    function tableView_objectValueForTableColumn_row(
        self:a, tableView:NSTableView, column:NSTableColumn, row:Int
    ) : Maybe(NSObject)
        default = Nothing    // @optional → default implementation
};
```

### 6.2 tulam Algebra -> ObjC Protocol (Codegen)

```tulam
algebra Drawable(a:Type) = {
    function draw(self:a, context:CGContext) : Unit,
    function boundingBox(self:a) : CGRect
};
```

When an ObjC subclass `implements Drawable`, the compiler registers the methods with the ObjC runtime so Apple frameworks can call them via message dispatch.

### 6.3 The `maps` Bridge

```tulam
// Connect tulam's standard algebras to Apple's protocols:
algebra Eq(a:Type) maps NSObject target native {
    isEqual = eq
};

algebra Ord(a:Type) maps NSComparable target native {
    compare = compare
};

// Now: NSString automatically gets Eq (it implements isEqual:)
// And: any tulam type with Eq can respond to isEqual: when passed to ObjC
```

---

## 7. Blocks (Closures)

ObjC Blocks are Apple's closure type. tulam lambdas crossing to ObjC become Blocks:

```tulam
action {
    // tulam lambda → ObjC Block automatically
    button.setAction(\sender -> action {
        label.setStringValue("Clicked!");
    });

    // Works with any Block-taking API:
    NSAnimationContext.runAnimationGroup(\context -> action {
        context.setDuration(0.3);
        window.animator().setAlphaValue(0.5);
    });

    // And callback-style APIs:
    let url = NSURL.new(string = "https://example.com");
    NSURLSession.shared().dataTask_withURL_completionHandler(
        url,
        \data, response, error -> action {
            match data
                | Just(d) -> putStrLn("Got " ++ show(d.length()) ++ " bytes")
                | Nothing -> putStrLn("Error: " ++ show(error));
        }
    );
};
```

At the native level, the compiler emits Block literals using the Block ABI (`__block_literal` struct with invoke function pointer). ARC handles Block memory (copy to heap when escaping).

---

## 8. Memory Management

### 8.1 ARC for ObjC Objects

All ObjC object pointers emitted by the tulam compiler are ARC-managed. The compiler emits retain/release calls following ARC rules:

- **Assignment** → retain new, release old
- **Scope exit** → release
- **Return** → autorelease (or caller-retained per convention)
- **Block capture** → retain captured ObjC objects

tulam doesn't expose retain/release to the programmer. ARC is invisible.

### 8.2 Pure tulam Values

Pure tulam values (tagged unions, closures not escaping to ObjC, records) use the native backend's own memory strategy (GC, reference counting, or regions — a separate design decision independent of ObjC interop).

### 8.3 Weak References

For delegate patterns (which are cyclic by nature):

```tulam
class MyController() extends NSObject = {
    @weak property delegate : Maybe(SomeDelegate) = Nothing
};
```

`@weak` emits a `__weak` ObjC pointer. When the referenced object is deallocated, the pointer becomes `nil` (→ `Nothing` in tulam).

---

## 9. Worked Example: Complete macOS App

```tulam
module HelloApp;

import AppKit target native;
import Foundation target native;

/// Pure business logic — no effects, no ObjC mutations
function nextMessage(current:String) : String =
    match current
        | "Hello, tulam!" -> "You clicked the button!"
        | _               -> "Hello, tulam!";

function buttonTitle(current:String) : String =
    match current
        | "Hello, tulam!" -> "Click me"
        | _               -> "Click again";

/// Application delegate — needs ObjC subclass for AppKit callback
class AppDelegate() extends NSObject implements NSApplicationDelegate = {

    function applicationDidFinishLaunching(self:AppDelegate, notification:NSNotification) : Unit =
        action {
            // Create window
            let window = NSWindow.new(
                contentRect = NSMakeRect(0.0, 0.0, 400.0, 200.0),
                styleMask = NSWindowStyleMask.titled
                    .union(NSWindowStyleMask.closable)
                    .union(NSWindowStyleMask.miniaturizable),
                backing = NSBackingStoreBuffered,
                shouldDefer = False
            );
            window.setTitle("tulam App");

            // Create label
            let label = NSTextField.labelWithString("Hello, tulam!");
            label.setFrame(NSMakeRect(50.0, 120.0, 300.0, 40.0));
            label.setFont(NSFont.systemFontOfSize(24.0));
            label.setAlignment(NSTextAlignmentCenter);

            // Mutable state for the message
            let state = newRef("Hello, tulam!");

            // Create button
            let button = NSButton.new(
                title = "Click me",
                target = Nothing,
                action = Nothing
            );
            button.setFrame(NSMakeRect(130.0, 40.0, 140.0, 40.0));
            button.setBezelStyle(NSBezelStyleRounded);
            button.setAction(\sender -> action {
                let current = readRef(state);
                let next = nextMessage(current);      // pure call
                let title = buttonTitle(next);         // pure call
                writeRef(state, next);
                label.setStringValue(next);
                button.setTitle(title);
            });

            // Assemble view hierarchy
            window.contentView().addSubview(label);
            window.contentView().addSubview(button);
            window.center();
            window.makeKeyAndOrderFront(Nothing);
        },

    function applicationShouldTerminateAfterLastWindowClosed(
        self:AppDelegate, sender:NSApplication
    ) : Bool = True    // pure — just returns a value
};

/// Entry point
action main() = {
    let app = NSApplication.shared();
    let delegate = AppDelegate.new();
    app.setDelegate(delegate);
    app.run();
};
```

**What compiles to what:**

| tulam | Native output |
|-------|--------------|
| `nextMessage`, `buttonTitle` | Pure native functions (no ObjC involvement) |
| `AppDelegate` | ObjC class registered at startup via `objc_allocateClassPair` |
| `NSWindow.new(...)` | `objc_msgSend(objc_getClass("NSWindow"), sel("alloc"))` then `objc_msgSend(obj, sel("initWithContentRect:styleMask:backing:defer:"), ...)` |
| `window.setTitle(...)` | `objc_msgSend(window, sel("setTitle:"), nsstring)` |
| `window.contentView()` | `objc_msgSend(window, sel("contentView"))` — pure, readonly property |
| `\sender -> action { ... }` | ObjC Block literal wrapping compiled tulam code |
| `newRef(...)` / `readRef` / `writeRef` | Native Ref operations (not ObjC) |
| String literals | Bridged to `NSString` at call boundaries via `objc_msgSend(NSString, sel("stringWithUTF8String:"), cstr)` |

---

## 10. SwiftUI (Future)

SwiftUI requires Swift-specific features (property wrappers, result builders, opaque return types) that don't exist in the ObjC runtime. Two paths:

### 10.1 Thin Swift Bridge (Near-term)

A small hand-written Swift file exposes SwiftUI as ObjC-compatible hosting:

```swift
// TLBridge.swift — ships with tulam's macOS runtime
@objc class TLSwiftUIBridge: NSObject {
    @objc static func hostingView(_ rootView: @escaping () -> AnyView) -> NSView {
        return NSHostingView(rootView: rootView())
    }
}
```

```tulam
// tulam code embeds SwiftUI views inside AppKit windows:
action {
    let swiftView = TLSwiftUIBridge.hostingView(\() ->
        // Limited SwiftUI via bridge — details TBD
    );
    window.contentView().addSubview(swiftView);
};
```

### 10.2 Full Swift Backend (Long-term)

A `target swift` backend that emits Swift source. This is a separate, larger project.

### 10.3 Recommendation

Start with **AppKit** — it's stable, well-documented, fully ObjC-based, and sufficient for professional macOS apps. SwiftUI bridge comes later as needed.

---

## 11. C-Level Apple Frameworks

Many Apple frameworks have C APIs (Core Graphics, Core Audio, Metal C API, Security, IOKit). These use the existing native C interop directly — no ObjC involvement:

```tulam
import CoreGraphics target native;

/// Pure — these are just C struct operations
function circleRect(center:CGPoint, radius:Float64) : CGRect =
    CGRectMake(
        center.x - radius,
        center.y - radius,
        radius * 2.0,
        radius * 2.0
    );

/// Effectful — draws to a graphics context
action drawCircle(ctx:CGContext, center:CGPoint, radius:Float64, color:CGColor) = {
    CGContextSetFillColorWithColor(ctx, color);
    CGContextFillEllipseInRect(ctx, circleRect(center, radius));
};
```

---

## 12. Implementation Phases

### Phase A: ObjC Runtime Bridge
- `objc_msgSend` wrapper in native codegen
- Selector name translation (underscore → colon)
- ObjC header parsing (extend libclang integration for `@interface`, `@protocol`, `@property`)
- Purity classification engine (Section 3)
- Type mapping at boundaries (String ↔ NSString, nil ↔ Nothing, etc.)

### Phase B: Subclassing
- `objc_allocateClassPair` / `class_addMethod` emission for `extends ObjCClass`
- Protocol conformance registration
- Method override signature validation
- ARC integration for ObjC-visible objects

### Phase C: AppKit / Foundation
- Full AppKit header import
- NS_ENUM / NS_OPTIONS mapping
- Block emission for lambdas crossing to ObjC
- Delegate pattern support
- Event handling (target-action)

### Phase D: Build and Packaging
- Framework linking (`-framework AppKit -framework Foundation`)
- `.app` bundle creation (Info.plist, executable, resources)
- Code signing integration
- Xcode project generation (optional)

### Phase E: Advanced
- KVO / Bindings support
- SwiftUI bridge (thin Swift shim)
- Core Data model generation
- Metal shader integration

---

## 13. Open Questions

1. **ARC vs GC boundary**: Pure tulam values use the native backend's memory strategy. ObjC objects use ARC. When a tulam closure captures an ObjC object and escapes to ObjC (as a Block), ARC handles it. But when an ObjC object is stored in a pure tulam data structure (e.g., `List(NSView)`), who manages the retain? Likely: the native backend must emit retain/release around ObjC pointers regardless of the tulam-side memory strategy.

2. **Thread safety**: ObjC's `@synchronized`, `dispatch_queue`, and `NSLock` patterns. Should tulam have a dedicated concurrency effect, or just expose these as effectful ObjC calls?

3. **Purity escape hatch**: Should there be an `@unsafe_pure` annotation that lets you call an effectful ObjC method from pure code? Like Haskell's `unsafePerformIO`. Useful for known-safe factory methods that the heuristic misclassifies.

4. **Optional protocol methods**: Currently modeled as default implementations in the algebra. Alternative: a dedicated `@optional` annotation in algebra declarations. The default-implementation approach seems cleaner.

5. **C struct vs ObjC class detection**: Some Apple types (CGRect, NSRect) are C structs; others (NSValue, NSNumber) are ObjC classes that *wrap* C values. The metadata resolver must distinguish these correctly. Headers make this clear (`typedef struct` vs `@interface`) but edge cases exist (e.g., `NSDecimal` is a struct, `NSDecimalNumber` is a class).

---

## 14. Relation to Existing Design Documents

| Document | Relationship |
|----------|-------------|
| `InteropDesign.md` | This is a specialization of the native backend (Section 11.3) with ObjC runtime additions. All principles carry over: zero-ceremony import, algebras as bridge, Maybe for null, action blocks for mutation. |
| `ClassDesign.md` | tulam classes with `extends ObjCClass` are the *only* case that touches the ObjC runtime. Pure tulam classes remain tagged tuples. |
| `CategoricalDesign.md` | Algebras ↔ ObjC protocols. `maps` declarations connect tulam's Eq/Ord to NSObject's `isEqual:`/`compare:`. |
| `PrimitiveDesign.md` | NS_ENUM/NS_OPTIONS import as primitives. `repr` handles enum ↔ integer conversion. |
