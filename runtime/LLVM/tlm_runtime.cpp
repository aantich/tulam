// tulam Runtime System — Implementation
// Phase A.1: malloc-based allocation, no real GC.
#include "tlm_runtime.hpp"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <new>

// ---- Allocation (bump-pointer arena with free lists) ----

static constexpr size_t ARENA_SIZE = 64 * 1024 * 1024;  // 64 MB

// Exported arena globals for inline bump allocation from LLVM IR
extern "C" char* tlm_arena_ptr  = nullptr;
extern "C" char* tlm_arena_end  = nullptr;
static char* arena_base = nullptr;

// Size-class free lists: 16, 24, 32, 48, 64, 96, 128, 256 bytes
static constexpr int NUM_SIZE_CLASSES = 8;
static constexpr size_t SIZE_CLASSES[NUM_SIZE_CLASSES] = {16, 24, 32, 48, 64, 96, 128, 256};
static void* free_lists[NUM_SIZE_CLASSES] = {};

static int sizeClass(size_t size) {
    for (int i = 0; i < NUM_SIZE_CLASSES; i++) {
        if (size <= SIZE_CLASSES[i]) return i;
    }
    return -1;  // too large for free list
}

// Slow path: called when arena is exhausted or first allocation
extern "C" tlm::Object* tlm_alloc_slow(int32_t tag, int32_t num_fields) {
    size_t size = tlm::objectSize(num_fields);
    // Align to 8 bytes
    size = (size + 7) & ~static_cast<size_t>(7);

    // Check free list first
    int sc = sizeClass(size);
    if (sc >= 0 && free_lists[sc]) {
        auto* obj = static_cast<tlm::Object*>(free_lists[sc]);
        free_lists[sc] = *reinterpret_cast<void**>(obj);
        new (&obj->header) tlm::Header(static_cast<uint16_t>(tag),
                                        static_cast<uint8_t>(num_fields));
        if (num_fields > 0) {
            std::memset(obj->fields, 0, static_cast<size_t>(num_fields) * sizeof(uint64_t));
        }
        return obj;
    }

    // Allocate new arena if needed
    if (!arena_base || tlm_arena_ptr + size > tlm_arena_end) {
        arena_base = static_cast<char*>(std::malloc(ARENA_SIZE));
        if (!arena_base) {
            std::fprintf(stderr, "tulam: out of memory (arena %zu bytes)\n", ARENA_SIZE);
            std::exit(1);
        }
        tlm_arena_ptr = arena_base;
        tlm_arena_end = arena_base + ARENA_SIZE;
    }

    auto* obj = reinterpret_cast<tlm::Object*>(tlm_arena_ptr);
    tlm_arena_ptr += size;

    new (&obj->header) tlm::Header(static_cast<uint16_t>(tag),
                                    static_cast<uint8_t>(num_fields));
    if (num_fields > 0) {
        std::memset(obj->fields, 0, static_cast<size_t>(num_fields) * sizeof(uint64_t));
    }
    return obj;
}

// Backward-compatible wrapper (calls slow path)
extern "C" tlm::Object* tlm_alloc(int32_t tag, int32_t num_fields) {
    return tlm_alloc_slow(tag, num_fields);
}

// Free an object back to its size-class free list
extern "C" void tlm_free(tlm::Object* obj) {
    if (!obj) return;
    size_t size = tlm::objectSize(obj->header.arity);
    size = (size + 7) & ~static_cast<size_t>(7);
    int sc = sizeClass(size);
    if (sc >= 0) {
        *reinterpret_cast<void**>(obj) = free_lists[sc];
        free_lists[sc] = obj;
    }
    // Objects too large for free list are leaked (arena reclaimed on exit)
}

// ---- Field access ----

extern "C" uint64_t tlm_get_field(tlm::Object* obj, int32_t index) {
    return obj->getField(index);
}

extern "C" void tlm_set_field(tlm::Object* obj, int32_t index, uint64_t value) {
    obj->setField(index, value);
}

extern "C" uint16_t tlm_get_tag(tlm::Object* obj) {
    return obj->getTag();
}

// ---- Printing ----

extern "C" void tlm_print_int(int64_t val) {
    std::printf("%lld", static_cast<long long>(val));
}

extern "C" void tlm_print_float(double val) {
    std::printf("%g", val);
}

extern "C" void tlm_print_string(const char* s) {
    std::printf("%s", s);
}

extern "C" void tlm_print_newline() {
    std::putchar('\n');
    std::fflush(stdout);
}

extern "C" void tlm_print_char(int32_t codepoint) {
    // Simple ASCII for now; UTF-8 encoding in future phases
    if (codepoint < 128) {
        std::putchar(codepoint);
    } else {
        // UTF-8 encode
        if (codepoint < 0x800) {
            std::putchar(0xC0 | (codepoint >> 6));
            std::putchar(0x80 | (codepoint & 0x3F));
        } else if (codepoint < 0x10000) {
            std::putchar(0xE0 | (codepoint >> 12));
            std::putchar(0x80 | ((codepoint >> 6) & 0x3F));
            std::putchar(0x80 | (codepoint & 0x3F));
        } else {
            std::putchar(0xF0 | (codepoint >> 18));
            std::putchar(0x80 | ((codepoint >> 12) & 0x3F));
            std::putchar(0x80 | ((codepoint >> 6) & 0x3F));
            std::putchar(0x80 | (codepoint & 0x3F));
        }
    }
}

extern "C" void tlm_print_bool(int32_t val) {
    std::printf("%s", val ? "True" : "False");
}

// ---- String operations ----

extern "C" const char* __string_concat(const char* a, const char* b) {
    size_t la = std::strlen(a);
    size_t lb = std::strlen(b);
    char* result = static_cast<char*>(std::malloc(la + lb + 1));
    std::memcpy(result, a, la);
    std::memcpy(result + la, b, lb + 1);
    return result;
}

extern "C" void __print_string(const char* s) {
    std::printf("%s", s);
}

extern "C" void __print_newline() {
    std::putchar('\n');
    std::fflush(stdout);
}

extern "C" const char* __show_i64(int64_t val) {
    char buf[32];
    std::snprintf(buf, sizeof(buf), "%lld", static_cast<long long>(val));
    return strdup(buf);
}

extern "C" const char* __show_f64(double val) {
    char buf[64];
    std::snprintf(buf, sizeof(buf), "%g", val);
    return strdup(buf);
}

extern "C" const char* __show_bool(int64_t val) {
    return val ? "True" : "False";
}

// ---- Mutable arrays (i64 arrays for Phase A.1) ----

extern "C" int64_t* __newmutarray(int64_t size, int64_t initVal) {
    auto* arr = static_cast<int64_t*>(std::malloc(static_cast<size_t>(size + 1) * sizeof(int64_t)));
    arr[0] = size;  // store length at index 0
    for (int64_t i = 1; i <= size; i++) {
        arr[i] = initVal;
    }
    return arr;
}

extern "C" int64_t __mutread(int64_t* arr, int64_t index) {
    return arr[index + 1];  // skip length header
}

extern "C" void __mutwrite(int64_t* arr, int64_t index, int64_t value) {
    arr[index + 1] = value;  // skip length header
}

extern "C" int64_t __mutlength(int64_t* arr) {
    return arr[0];
}

// ---- Mutable references ----

extern "C" int64_t* __newref(int64_t initVal) {
    auto* ref = static_cast<int64_t*>(std::malloc(sizeof(int64_t)));
    *ref = initVal;
    return ref;
}

extern "C" int64_t __readref(int64_t* ref) {
    return *ref;
}

extern "C" void __writeref(int64_t* ref, int64_t val) {
    *ref = val;
}

// ---- Error handling ----

extern "C" [[noreturn]] void tlm_error(const char* msg) {
    std::fprintf(stderr, "tulam runtime error: %s\n", msg);
    std::exit(1);
}

// ---- RC operations ----
// Phase A.1: increment/decrement for debugging, no actual freeing.
// Phase E will replace with Perceus drop + reuse.

// ---- Clock ----

#if defined(__APPLE__)
#include <mach/mach_time.h>
extern "C" int64_t __clock_nanos() {
    static mach_timebase_info_data_t info = {0, 0};
    if (info.denom == 0) mach_timebase_info(&info);
    uint64_t ticks = mach_absolute_time();
    return static_cast<int64_t>(ticks * info.numer / info.denom);
}
#else
#include <time.h>
extern "C" int64_t __clock_nanos() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return static_cast<int64_t>(ts.tv_sec) * 1000000000LL + ts.tv_nsec;
}
#endif

// ---- Reference counting (stub) ----

extern "C" void tlm_rc_inc(tlm::Object* obj) {
    if (obj) obj->header.rc++;
}

extern "C" void tlm_rc_dec(tlm::Object* obj) {
    if (obj) {
        obj->header.rc--;
        // Phase A.1: leak — no free. Phase E: Perceus drop + reuse.
    }
}
