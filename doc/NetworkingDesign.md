# Networking Library Design

## 1. Design Philosophy

tulam's networking library follows the same effect-based approach as the rest of the standard library:

1. **Networking as effects** — every network operation is an effect operation. Function signatures tell you exactly what I/O a function performs: `requires Http` vs `requires Net` vs `requires WebSocket`.
2. **Testability by default** — every effect has a mock handler. Unit-test HTTP clients without touching the network.
3. **Layered abstraction** — low-level sockets for power users, high-level HTTP for the common case. Each layer is independently usable.
4. **Streaming-native** — large responses, SSE, and WebSocket messages use `Stream(a)`, the same pull-based type used everywhere in the collection system.

### Why Effects, Not Objects?

| Approach | Why Not |
|----------|---------|
| Go `net/http` | Global state, no effect tracking, hard to mock |
| Java `HttpClient` | Object-oriented, requires DI frameworks for testability |
| Rust `reqwest` | Async runtime coupling, trait-object gymnastics |
| Python `requests` | Global session state, monkey-patching for tests |

tulam's approach: the *handler* is the configuration (timeouts, TLS certs, connection pooling). Swap the handler, swap the behavior — no DI framework needed.

## 2. Module Structure

```
lib/Net/
  Types.tl        — Data types: IpAddr, SocketAddr, Uri, HttpRequest, HttpResponse, etc.
  Stream.tl       — Stream(a) type + combinators (promoted to Collection.tl)
  Net.tl          — Low-level effect: TCP, UDP, DNS
  Tls.tl          — TLS effect: encryption layer over sockets
  Http.tl         — HTTP client + minimal server effect
  WebSocket.tl    — WebSocket effect
  Base.tl         — Umbrella re-export
```

## 3. New Primitive Types

### 3.1 Bytes

```tulam
/// Opaque byte buffer. Backed by ByteString (Haskell), Buffer (JS), byte[] (.NET).
/// Optimized for bulk I/O: zero-copy slicing, O(1) length, O(1) concatenation (rope).
primitive Bytes;
```

Added to `lib/Prelude.tl` alongside existing primitives.

### 3.2 Bytes Algebras & Morphism

```tulam
// In lib/Net/Types.tl or lib/Instances.tl

instance Semigroup(Bytes) = intrinsic;   // concatenation
instance Monoid(Bytes) = intrinsic;      // emptyBytes
instance Eq(Bytes) = intrinsic;
instance Ord(Bytes) = intrinsic;
instance Show(Bytes) = intrinsic;        // hex display
instance Sized(Bytes) = intrinsic;       // O(1) length
instance Indexable(Bytes) = intrinsic;   // O(1) byte access → Byte
instance Sliceable(Bytes) = intrinsic;   // O(1) zero-copy slice

/// Convert between Bytes and Array(Byte).
/// toRepr copies into Array; fromRepr copies into Bytes.
morphism Convertible(Bytes, Array(Byte)) = intrinsic;
```

Convenience functions:

```tulam
/// Encode String as UTF-8 bytes
function toBytes(s:String) : Bytes = intrinsic;

/// Decode UTF-8 bytes to String (may fail on invalid UTF-8)
function fromBytes(b:Bytes) : Maybe(String) = intrinsic;

/// Unsafe decode (replaces invalid sequences with U+FFFD)
function fromBytesLossy(b:Bytes) : String = intrinsic;

/// Empty byte buffer
value emptyBytes : Bytes = intrinsic;

/// Concatenate a list of byte chunks
function concatBytes(chunks:List(Bytes)) : Bytes =
    foldl(combine, emptyBytes, chunks);
```

## 4. Stream(a) — Core Collection Type

Added to `lib/Collection.tl` (not networking-specific).

```tulam
/// Pull-based lazy stream. Produces elements on demand.
///
/// Unlike List, Stream is not a data structure — it's a computation.
/// Elements are produced lazily and consumed once. Ideal for:
/// - Large/infinite sequences
/// - I/O streaming (file lines, network chunks, SSE events)
/// - Generator patterns
///
/// For finite in-memory data, prefer List or Array.
type Stream(a) = StreamCons * head:a * tail:Stream(a)
               + StreamEnd;

/// Algebra: Streamable containers can produce elements one at a time.
algebra Streamable(s:Type -> Type) = {
    /// Pull the next element. Returns Nothing when exhausted.
    function next(s:s(a)) : Maybe(Pair(a, s(a)));
};

instance Functor(Stream) = {
    function fmap(f, s) = match s
        | StreamEnd -> StreamEnd
        | StreamCons(x, rest) -> StreamCons(f(x), fmap(f, rest))
};

instance Foldable(Stream) = {
    function foldr(f, acc, s) = match s
        | StreamEnd -> acc
        | StreamCons(x, rest) -> f(x, foldr(f, acc, rest));
    function foldl(f, acc, s) = match s
        | StreamEnd -> acc
        | StreamCons(x, rest) -> foldl(f, f(acc, x), rest)
};

instance Semigroup(Stream(a)) = {
    function combine(s1, s2) = match s1
        | StreamEnd -> s2
        | StreamCons(x, rest) -> StreamCons(x, combine(rest, s2))
};

instance Monoid(Stream(a)) = {
    value empty = StreamEnd
};

// --- Stream combinators ---

/// Take first n elements
function takeStream(n:Int, s:Stream(a)) : Stream(a) =
    if n <= 0 then StreamEnd
    else match s
        | StreamEnd -> StreamEnd
        | StreamCons(x, rest) -> StreamCons(x, takeStream(n - 1, rest));

/// Drop first n elements
function dropStream(n:Int, s:Stream(a)) : Stream(a) =
    if n <= 0 then s
    else match s
        | StreamEnd -> StreamEnd
        | StreamCons(_, rest) -> dropStream(n - 1, rest);

/// Filter stream elements
function filterStream(p:a -> Bool, s:Stream(a)) : Stream(a) =
    match s
    | StreamEnd -> StreamEnd
    | StreamCons(x, rest) ->
        if p(x) then StreamCons(x, filterStream(p, rest))
        else filterStream(p, rest);

/// Collect entire stream into a List
function collectStream(s:Stream(a)) : List(a) =
    foldr(Cons, Nil, s);

/// Collect entire stream of Bytes into one Bytes
function drainBytes(s:Stream(Bytes)) : Bytes =
    foldl(combine, emptyBytes, s);

/// Create stream from a List
function streamFromList(xs:List(a)) : Stream(a) =
    match xs
    | Nil -> StreamEnd
    | Cons(x, rest) -> StreamCons(x, streamFromList(rest));

/// Infinite stream by repeatedly applying f
function iterate(f:a -> a, seed:a) : Stream(a) =
    StreamCons(seed, iterate(f, f(seed)));

/// Infinite stream of a single value
function repeat(x:a) : Stream(a) =
    StreamCons(x, repeat(x));

/// Zip two streams
function zipStream(s1:Stream(a), s2:Stream(b)) : Stream(Pair(a, b)) =
    match s1
    | StreamEnd -> StreamEnd
    | StreamCons(x, r1) -> match s2
        | StreamEnd -> StreamEnd
        | StreamCons(y, r2) -> StreamCons(Pair(x, y), zipStream(r1, r2));
```

## 5. Layer 1: Net Effect — TCP, UDP, DNS

```tulam
module Net.Net;
import Net.Types;

/// Low-level networking: TCP connections, UDP datagrams, DNS resolution.
///
/// Bundles the three most common socket-level operations into one effect
/// for ergonomic signatures. Functions that only do DNS still show
/// `requires Net` — acceptable because DNS is almost always a precursor
/// to socket operations anyway.
effect Net = {
    // --- DNS ---

    /// Resolve hostname to IP addresses (may return multiple for load balancing).
    function resolve(hostname:String) : List(IpAddr);

    /// Reverse DNS lookup.
    function reverseLookup(addr:IpAddr) : Maybe(String);

    // --- TCP ---

    /// Connect to a remote TCP endpoint. Blocks until connected or error.
    function tcpConnect(addr:SocketAddr) : Socket;

    /// Bind and listen for incoming TCP connections.
    /// backlog = max pending connections in OS queue.
    function tcpListen(addr:SocketAddr, backlog:Int) : Listener;

    /// Accept next incoming connection. Blocks until a client connects.
    function accept(listener:Listener) : Socket;

    /// Send bytes over a TCP connection. Returns number of bytes sent.
    function send(sock:Socket, data:Bytes) : Int;

    /// Receive up to maxBytes from a TCP connection.
    /// Returns empty Bytes on connection close.
    function recv(sock:Socket, maxBytes:Int) : Bytes;

    /// Send all bytes (retries until complete or error).
    function sendAll(sock:Socket, data:Bytes) : Unit;

    /// Close a TCP connection.
    function closeSocket(sock:Socket) : Unit;

    /// Close a TCP listener.
    function closeListener(listener:Listener) : Unit;

    // --- UDP ---

    /// Bind a UDP socket to a local address.
    function udpBind(addr:SocketAddr) : UdpSocket;

    /// Send a datagram to a specific address.
    function udpSend(sock:UdpSocket, data:Bytes, to:SocketAddr) : Int;

    /// Receive a datagram. Returns (data, sender address).
    function udpRecv(sock:UdpSocket, maxBytes:Int) : Pair(Bytes, SocketAddr);

    /// Close a UDP socket.
    function closeUdp(sock:UdpSocket) : Unit;
};

/// Default handler backed by OS sockets (POSIX / Winsock).
handler StdNet : Net = default {
    function resolve(hostname) = intrinsic;
    function reverseLookup(addr) = intrinsic;
    function tcpConnect(addr) = intrinsic;
    function tcpListen(addr, backlog) = intrinsic;
    function accept(listener) = intrinsic;
    function send(sock, data) = intrinsic;
    function recv(sock, maxBytes) = intrinsic;
    function sendAll(sock, data) = intrinsic;
    function closeSocket(sock) = intrinsic;
    function closeListener(listener) = intrinsic;
    function udpBind(addr) = intrinsic;
    function udpSend(sock, data, to) = intrinsic;
    function udpRecv(sock, maxBytes) = intrinsic;
    function closeUdp(sock) = intrinsic;
};

/// Mock handler for testing. Records all operations, returns canned data.
handler MockNet(config:MockNetConfig) : Net = {
    function resolve(hostname) = config.resolveResult;
    function reverseLookup(addr) = config.reverseLookupResult;
    function tcpConnect(addr) = config.mockSocket;
    function tcpListen(addr, backlog) = config.mockListener;
    function accept(listener) = config.mockSocket;
    function send(sock, data) = length(data);
    function recv(sock, maxBytes) = config.recvData;
    function sendAll(sock, data) = Unit;
    function closeSocket(sock) = Unit;
    function closeListener(listener) = Unit;
    function udpBind(addr) = config.mockUdpSocket;
    function udpSend(sock, data, to) = length(data);
    function udpRecv(sock, maxBytes) = Pair(config.recvData, config.mockAddr);
    function closeUdp(sock) = Unit;
};

// --- TCP convenience ---

/// Connect by hostname + port (resolves DNS automatically).
function tcpConnectHost(host:String, port:Int) : Socket requires Net = {
    addrs <- resolve(host);
    match addrs
    | Cons(addr, _) -> tcpConnect(SocketAddr(addr, port))
    | Nil -> error("DNS resolution failed: " ++ host)
};

/// Receive all data until connection closes. Returns as Stream.
function recvStream(sock:Socket, chunkSize:Int) : Stream(Bytes) requires Net = {
    chunk <- recv(sock, chunkSize);
    if length(chunk) == 0
    then StreamEnd
    else StreamCons(chunk, recvStream(sock, chunkSize))
};

/// Receive all data until connection closes. Returns as single Bytes.
function recvAll(sock:Socket) : Bytes requires Net =
    drainBytes(recvStream(sock, 65536));
```

## 6. Layer 2: TLS Effect

```tulam
module Net.Tls;
import Net.Types;

/// TLS encryption layer. Wraps an existing TCP Socket into an encrypted stream.
///
/// Separate from Net because:
/// 1. Not all programs need TLS (embedded, local services, testing)
/// 2. TLS configuration (certs, verification) is a distinct concern
/// 3. Allows TLS-free builds for constrained environments
effect Tls = {
    /// Perform TLS handshake as client. `hostname` used for SNI.
    function tlsConnect(sock:Socket, hostname:String, config:TlsConfig) : TlsStream;

    /// Perform TLS handshake as server (requires cert + key in config).
    function tlsAccept(sock:Socket, config:TlsConfig) : TlsStream;

    /// Send encrypted data. Returns bytes sent.
    function tlsSend(stream:TlsStream, data:Bytes) : Int;

    /// Receive decrypted data. Returns up to maxBytes.
    function tlsRecv(stream:TlsStream, maxBytes:Int) : Bytes;

    /// Close TLS session (sends close_notify alert) and underlying socket.
    function tlsClose(stream:TlsStream) : Unit;
};

/// Default handler using system TLS (OpenSSL / Secure Transport / SChannel).
handler StdTls : Tls = default {
    function tlsConnect(sock, hostname, config) = intrinsic;
    function tlsAccept(sock, config) = intrinsic;
    function tlsSend(stream, data) = intrinsic;
    function tlsRecv(stream, maxBytes) = intrinsic;
    function tlsClose(stream) = intrinsic;
};

/// TLS configuration builder helpers
function withCert(config:TlsConfig, certFile:String, keyFile:String) : TlsConfig =
    TlsConfig(Just(certFile), Just(keyFile), config.caCertFile, config.verifyPeer);

function withCA(config:TlsConfig, caFile:String) : TlsConfig =
    TlsConfig(config.certFile, config.keyFile, Just(caFile), config.verifyPeer);

function insecure(config:TlsConfig) : TlsConfig =
    TlsConfig(config.certFile, config.keyFile, config.caCertFile, False);
```

## 7. Layer 3: HTTP Effect

### 7.1 Types

```tulam
/// HTTP method
type HttpMethod = GET + POST + PUT + DELETE + PATCH + HEAD + OPTIONS + CONNECT + TRACE;

/// HTTP version
type HttpVersion = HTTP10 + HTTP11 + HTTP20;

/// HTTP header (case-insensitive name)
type Header = name:String * value:String;

/// HTTP request
type HttpRequest =
    method:HttpMethod *
    url:String *
    headers:List(Header) *
    body:Bytes;

/// HTTP response (fully buffered body)
type HttpResponse =
    status:Int *
    statusText:String *
    headers:List(Header) *
    body:Bytes;

/// HTTP response with streaming body
type HttpStreamResponse =
    status:Int *
    statusText:String *
    headers:List(Header) *
    body:Stream(Bytes);

/// HTTP client configuration (for handler construction)
type HttpConfig =
    followRedirects:Bool *
    maxRedirects:Int *
    timeoutMs:Int *
    defaultHeaders:List(Header);

function defaultHttpConfig() : HttpConfig =
    HttpConfig(True, 10, 30000, [
        Header("User-Agent", "tulam-http/1.0")
    ]);
```

### 7.2 Client Effect

```tulam
module Net.Http;
import Net.Types;

/// HTTP client operations.
effect Http = {
    /// Send a request, receive full response (body fully buffered in memory).
    function httpRequest(req:HttpRequest) : HttpResponse;

    /// Send a request, receive streaming response (body as lazy Stream(Bytes)).
    function httpStream(req:HttpRequest) : HttpStreamResponse;
};

/// Default handler: intrinsic-backed (wraps libcurl / system HTTP stack).
handler StdHttp : Http = default {
    function httpRequest(req) = intrinsic;
    function httpStream(req) = intrinsic;
};

/// Configurable handler with custom settings.
handler ConfigurableHttp(config:HttpConfig) : Http = default {
    function httpRequest(req) = intrinsic;
    function httpStream(req) = intrinsic;
};

/// Reference implementation: HTTP/1.1 built on Net + Tls effects.
/// Educational / customization purposes. Not production-grade (no connection
/// pooling, no HTTP/2, basic chunked encoding support).
handler TulamHttp : Http = {
    function httpRequest(req) = {
        uri <- parseUri(req.url);
        let useTls = uri.scheme == "https";
        let port = match uri.port
            | Just(p) -> p
            | Nothing -> if useTls then 443 else 80;
        sock <- tcpConnectHost(uri.host, port);
        stream <- if useTls
            then tlsConnect(sock, uri.host, defaultTlsConfig())
            else sock;  // simplified — real impl needs transport abstraction
        let reqBytes = formatHttpRequest(req, uri);
        sendAll(stream, reqBytes);
        parseHttpResponse(stream)
    };
    function httpStream(req) = {
        // Similar to above but returns Stream(Bytes) for body
        // Supports chunked transfer-encoding
        httpStreamImpl(req)
    }
};

/// Mock handler for testing. Returns preconfigured responses.
handler MockHttp(responses:List(HttpResponse)) : Http = {
    let idx = newRef(0);
    function httpRequest(req) = {
        i <- readRef(idx);
        writeRef(idx, i + 1);
        index(responses, i)
    };
    function httpStream(req) = {
        resp <- httpRequest(req);
        HttpStreamResponse(resp.status, resp.statusText, resp.headers,
                          StreamCons(resp.body, StreamEnd))
    }
};
```

### 7.3 Convenience Functions

```tulam
// --- Request builders ---

/// Simple GET request.
function get(url:String) : HttpResponse requires Http =
    httpRequest(HttpRequest(GET, url, [], emptyBytes));

/// GET with custom headers.
function getWith(url:String, headers:List(Header)) : HttpResponse requires Http =
    httpRequest(HttpRequest(GET, url, headers, emptyBytes));

/// POST with raw body.
function post(url:String, body:Bytes) : HttpResponse requires Http =
    httpRequest(HttpRequest(POST, url, [], body));

/// POST with Content-Type header.
function postWith(url:String, contentType:String, body:Bytes) : HttpResponse requires Http =
    httpRequest(HttpRequest(POST, url, [Header("Content-Type", contentType)], body));

/// POST JSON string body.
function postJson(url:String, jsonBody:String) : HttpResponse requires Http =
    postWith(url, "application/json", toBytes(jsonBody));

/// POST form-encoded body.
function postForm(url:String, fields:List(Pair(String, String))) : HttpResponse requires Http =
    postWith(url, "application/x-www-form-urlencoded", toBytes(encodeFormFields(fields)));

/// PUT with body.
function put(url:String, body:Bytes) : HttpResponse requires Http =
    httpRequest(HttpRequest(PUT, url, [], body));

/// DELETE request.
function delete(url:String) : HttpResponse requires Http =
    httpRequest(HttpRequest(DELETE, url, [], emptyBytes));

/// PATCH with body.
function patch(url:String, body:Bytes) : HttpResponse requires Http =
    httpRequest(HttpRequest(PATCH, url, [], body));

/// HEAD request (response body will be empty).
function head(url:String) : HttpResponse requires Http =
    httpRequest(HttpRequest(HEAD, url, [], emptyBytes));

// --- Request modification (builder pattern via functions) ---

/// Add a header to a request.
function withHeader(req:HttpRequest, name:String, value:String) : HttpRequest =
    HttpRequest(req.method, req.url, Cons(Header(name, value), req.headers), req.body);

/// Set the Authorization header (Bearer token).
function withBearerToken(req:HttpRequest, token:String) : HttpRequest =
    withHeader(req, "Authorization", "Bearer " ++ token);

/// Set basic auth header.
function withBasicAuth(req:HttpRequest, user:String, pass:String) : HttpRequest =
    withHeader(req, "Authorization", "Basic " ++ base64Encode(user ++ ":" ++ pass));

// --- Response inspection ---

/// Is the response status 2xx?
function isSuccess(r:HttpResponse) : Bool =
    r.status >= 200 && r.status < 300;

/// Is the response a redirect (3xx)?
function isRedirect(r:HttpResponse) : Bool =
    r.status >= 300 && r.status < 400;

/// Is the response a client error (4xx)?
function isClientError(r:HttpResponse) : Bool =
    r.status >= 400 && r.status < 500;

/// Is the response a server error (5xx)?
function isServerError(r:HttpResponse) : Bool =
    r.status >= 500 && r.status < 600;

/// Decode response body as UTF-8 String.
function bodyText(r:HttpResponse) : String =
    fromBytesLossy(r.body);

/// Get first header value by name (case-insensitive).
function getHeader(r:HttpResponse, name:String) : Maybe(String) =
    match find(fn(h:Header) = toLower(h.name) == toLower(name), r.headers)
    | Just(h) -> Just(h.value)
    | Nothing -> Nothing;

/// Get all header values by name (case-insensitive).
function getHeaders(r:HttpResponse, name:String) : List(String) =
    fmap(fn(h:Header) = h.value,
         filter(fn(h:Header) = toLower(h.name) == toLower(name), r.headers));

/// Get Content-Type header.
function contentType(r:HttpResponse) : Maybe(String) = getHeader(r, "Content-Type");

/// Get Content-Length header as Int.
function contentLength(r:HttpResponse) : Maybe(Int) =
    match getHeader(r, "Content-Length")
    | Just(s) -> parseInt(s)
    | Nothing -> Nothing;
```

### 7.4 Minimal HTTP Server

```tulam
/// Minimal HTTP server — just enough to write a simple API.
/// No routing framework, no middleware stack, no keep-alive management.
/// For production servers, build routing/middleware as pure functions on top.
effect HttpServer = {
    /// Start listening for HTTP requests on the given address.
    /// Returns a ServerHandle for accept loop.
    function httpListen(addr:SocketAddr) : ServerHandle;

    /// Accept the next incoming HTTP request. Blocks until a request arrives.
    function httpAccept(server:ServerHandle) : HttpRequest;

    /// Send an HTTP response back to the client (for the most recent accepted request).
    function httpRespond(server:ServerHandle, resp:HttpResponse) : Unit;

    /// Stop the server.
    function httpServerClose(server:ServerHandle) : Unit;
};

primitive ServerHandle;

handler StdHttpServer : HttpServer = default {
    function httpListen(addr) = intrinsic;
    function httpAccept(server) = intrinsic;
    function httpRespond(server, resp) = intrinsic;
    function httpServerClose(server) = intrinsic;
};

// --- Server convenience ---

/// Create an HTTP response with status, content-type, and body.
function respond(status:Int, contentType:String, body:String) : HttpResponse =
    HttpResponse(status, statusTextFor(status),
        [Header("Content-Type", contentType)], toBytes(body));

/// 200 OK with text body.
function ok(body:String) : HttpResponse = respond(200, "text/plain", body);

/// 200 OK with JSON body.
function okJson(body:String) : HttpResponse = respond(200, "application/json", body);

/// 404 Not Found.
function notFound() : HttpResponse = respond(404, "text/plain", "Not Found");

/// 500 Internal Server Error.
function serverError(msg:String) : HttpResponse = respond(500, "text/plain", msg);

/// Simple request router — match method + path to handler.
function route(req:HttpRequest, routes:List(Pair(Pair(HttpMethod, String), HttpRequest -> HttpResponse)))
    : HttpResponse =
    let uri = parseUri(req.url) in
    match find(fn(r) = fst(fst(r)) == req.method && snd(fst(r)) == uri.path, routes)
    | Just(r) -> snd(r)(req)
    | Nothing -> notFound();

/// Run a simple server with a request handler function.
function serve(addr:SocketAddr, handler:HttpRequest -> HttpResponse) : Unit
    requires HttpServer, Console = {
    server <- httpListen(addr);
    putStrLn("Listening on " ++ show(addr));
    serveLoop(server, handler)
};

function serveLoop(server:ServerHandle, handler:HttpRequest -> HttpResponse) : Unit
    requires HttpServer = {
    req <- httpAccept(server);
    resp <- handler(req);
    httpRespond(server, resp);
    serveLoop(server, handler)
};
```

## 8. Layer 4: WebSocket Effect

```tulam
module Net.WebSocket;
import Net.Types;

/// WebSocket message types
type WsMessage = WsText * data:String
               + WsBinary * data:Bytes
               + WsPing * data:Bytes
               + WsPong * data:Bytes
               + WsClose * code:Int * reason:String;

/// WebSocket connection handle (opaque)
primitive WsConn;

/// WebSocket close codes (RFC 6455 §7.4.1)
value wsNormalClosure : Int = 1000;
value wsGoingAway : Int = 1001;
value wsProtocolError : Int = 1002;
value wsUnsupported : Int = 1003;

/// WebSocket client + server operations.
effect WebSocket = {
    /// Connect to a WebSocket server (performs HTTP upgrade handshake).
    function wsConnect(url:String, headers:List(Header)) : WsConn;

    /// Send a message.
    function wsSend(conn:WsConn, msg:WsMessage) : Unit;

    /// Receive the next message. Blocks until a message arrives.
    function wsRecv(conn:WsConn) : WsMessage;

    /// Close the connection with a code and reason.
    function wsClose(conn:WsConn, code:Int, reason:String) : Unit;
};

handler StdWebSocket : WebSocket = default {
    function wsConnect(url, headers) = intrinsic;
    function wsSend(conn, msg) = intrinsic;
    function wsRecv(conn) = intrinsic;
    function wsClose(conn, code, reason) = intrinsic;
};

// --- Convenience ---

/// Connect with no extra headers.
function wsOpen(url:String) : WsConn requires WebSocket =
    wsConnect(url, []);

/// Send a text message.
function wsSendText(conn:WsConn, text:String) : Unit requires WebSocket =
    wsSend(conn, WsText(text));

/// Send a binary message.
function wsSendBinary(conn:WsConn, data:Bytes) : Unit requires WebSocket =
    wsSend(conn, WsBinary(data));

/// Close with normal closure code.
function wsCloseNormal(conn:WsConn) : Unit requires WebSocket =
    wsClose(conn, wsNormalClosure, "");

/// Receive messages as a Stream (ends on WsClose).
function wsMessages(conn:WsConn) : Stream(WsMessage) requires WebSocket = {
    msg <- wsRecv(conn);
    match msg
    | WsClose(_, _) -> StreamEnd
    | _ -> StreamCons(msg, wsMessages(conn))
};

/// Receive only text messages as a Stream (ignores ping/pong, ends on close).
function wsTextMessages(conn:WsConn) : Stream(String) requires WebSocket = {
    msg <- wsRecv(conn);
    match msg
    | WsText(t) -> StreamCons(t, wsTextMessages(conn))
    | WsClose(_, _) -> StreamEnd
    | WsPing(d) -> { wsSend(conn, WsPong(d)); wsTextMessages(conn) }
    | _ -> wsTextMessages(conn)
};
```

## 9. URI Parsing

```tulam
/// URI components (RFC 3986)
type Uri =
    scheme:String *
    userInfo:Maybe(String) *
    host:String *
    port:Maybe(Int) *
    path:String *
    query:Maybe(String) *
    fragment:Maybe(String);

/// Parse a URI string. Returns Nothing on invalid input.
function parseUri(s:String) : Maybe(Uri) = intrinsic;

/// Render a Uri back to a string.
function renderUri(uri:Uri) : String = intrinsic;

/// Encode a string for use in a URI (percent-encoding).
function uriEncode(s:String) : String = intrinsic;

/// Decode a percent-encoded string.
function uriDecode(s:String) : String = intrinsic;

/// Parse query string "a=1&b=2" into key-value pairs.
function parseQuery(s:String) : List(Pair(String, String));

/// Encode key-value pairs into query string.
function encodeQuery(params:List(Pair(String, String))) : String;

/// URL-encode form fields (same as encodeQuery but with + for spaces).
function encodeFormFields(fields:List(Pair(String, String))) : String;
```

## 10. Usage Examples

### 10.1 Simple HTTP Client

```tulam
action main() = {
    // Simple GET
    resp <- get("https://httpbin.org/get");
    putStrLn("Status: " ++ show(resp.status));
    putStrLn(bodyText(resp));

    // POST JSON
    resp2 <- postJson("https://httpbin.org/post", "{\"name\": \"tulam\"}");
    putStrLn("POST status: " ++ show(resp2.status))
};
```

### 10.2 Streaming Download

```tulam
action downloadFile(url:String, path:String) = {
    resp <- httpStream(HttpRequest(GET, url, [], emptyBytes));
    let allBytes = drainBytes(resp.body);
    writeFile(path, fromBytesLossy(allBytes));
    putStrLn("Downloaded " ++ show(length(allBytes)) ++ " bytes")
};
```

### 10.3 WebSocket Chat Client

```tulam
action chatClient(url:String) = {
    conn <- wsOpen(url);
    wsSendText(conn, "Hello from tulam!");

    // Process incoming messages
    let msgs = wsTextMessages(conn);
    foldl(fn(_, msg) = putStrLn("Received: " ++ msg), Unit, takeStream(10, msgs));

    wsCloseNormal(conn)
};
```

### 10.4 Simple HTTP Server

```tulam
action main() = {
    let myRoutes = [
        Pair(Pair(GET, "/"), fn(req) = ok("Hello, World!")),
        Pair(Pair(GET, "/health"), fn(req) = okJson("{\"status\": \"ok\"}")),
        Pair(Pair(POST, "/echo"), fn(req) = ok(bodyText(req)))
    ];
    serve(SocketAddr(IPv4(0,0,0,0), 8080), fn(req) = route(req, myRoutes))
};
```

### 10.5 Testing with Mock Handlers

```tulam
action testApiClient() = {
    let mockResponses = [
        HttpResponse(200, "OK", [Header("Content-Type", "application/json")],
                    toBytes("{\"users\": []}"))
    ];

    // Override Http handler for this call
    resp <- get [MockHttp(mockResponses)] ("https://api.example.com/users");
    assert(resp.status == 200);
    assert(bodyText(resp) == "{\"users\": []}");
    putStrLn("API client test passed!")
};
```

### 10.6 Raw TCP Client

```tulam
action tcpExample() = {
    sock <- tcpConnectHost("example.com", 80);
    sendAll(sock, toBytes("GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"));
    data <- recvAll(sock);
    closeSocket(sock);
    putStrLn(fromBytesLossy(data))
};
```

## 11. Dual-Backend Implementation Strategy

tulam targets two backends: the **bytecode interpreter** (Haskell-hosted) and the **LLVM native backend** (C++ runtime). The networking API (effect definitions, types, convenience functions) is identical across both — only the intrinsic implementations differ.

### 11.1 Architecture: Shared Intrinsic Name Table

Both backends share the same intrinsic name convention. Effect handler `intrinsic` keywords compile to `CLMPRIMCALL` nodes with known names. Each backend maps that name to its own implementation:

```
tulam source:     function tcpConnect(addr) = intrinsic;
                              │
                    ┌─────────┴─────────┐
                    ▼                   ▼
              CLMPRIMCALL          CLMPRIMCALL
              "net_tcpConnect"     "net_tcpConnect"
                    │                   │
          ┌────────┴──┐          ┌─────┴──────┐
          ▼           ▼          ▼            ▼
    BC VM opcode   Haskell    LIR LCall    C runtime
    OP_INTRINSIC   Network    @tlm_net_    POSIX
                   .Socket    tcpConnect   socket()
                   .connect
```

**No tulam code changes. No effect signature changes.** The abstraction holds across backends.

### 11.2 Bytecode Interpreter Backend (Haskell Host)

Intrinsics are Haskell function calls dispatched from the VM. The Haskell ecosystem provides mature, battle-tested libraries for every networking layer.

#### Dispatch Path

```
tulam effect op
  → CLMPRIMCALL "net_tcpConnect"
  → BC compiler: OP_INTRINSIC opcode
  → VM dispatch table: calls Haskell function
  → Haskell: Network.Socket.connect
```

#### Haskell Library Mapping

| Networking Layer | Haskell Package | Notes |
|-----------------|-----------------|-------|
| TCP/UDP sockets | `network` | `Socket`, `connect`, `bind`, `listen`, `accept`, `send`, `recv` |
| DNS resolution | `network` | `getAddrInfo`, `getNameInfo` |
| TLS | `tls` + `x509-store` | Pure Haskell TLS 1.2/1.3, no OpenSSL dependency |
| HTTP client | `http-client` + `http-client-tls` | Connection pooling, redirects, chunked encoding |
| HTTP server | `warp` (or hand-rolled on `network`) | Minimal server can use raw sockets |
| WebSocket | `websockets` | Client + server, RFC 6455 compliant |
| URI parsing | `network-uri` | RFC 3986 parser |
| Base64 | `base64-bytestring` | Encode/decode |
| Bytes backing | `bytestring` | Already a dependency — `ByteString` backs `Bytes` primitive |

#### Implementation Notes

- GHC's runtime system handles I/O multiplexing transparently — blocking `recv` in tulam code doesn't block the entire program; GHC uses `epoll`/`kqueue` under the hood.
- `ByteString` is the natural backing type for `Bytes` — zero-copy slicing via `Data.ByteString.take`/`drop`, O(1) length.
- Socket handles stored on the VM heap table as opaque values (like `Ref` today).
- Streaming responses: the `http-client` `brRead` function naturally maps to `Stream(Bytes)` — each `brRead` call produces the next chunk.

#### Intrinsic Registry Extension (Interpreter.hs / VM.hs)

```haskell
-- New entries in intrinsic dispatch (conceptual)
dispatchNetIntrinsic :: Text -> [CLMExpr] -> IO CLMExpr
dispatchNetIntrinsic "net_resolve"      [hostname]       = ... -- getAddrInfo
dispatchNetIntrinsic "net_tcpConnect"   [addr]           = ... -- socket + connect
dispatchNetIntrinsic "net_send"         [sock, bytes]    = ... -- Network.Socket.send
dispatchNetIntrinsic "net_recv"         [sock, maxBytes] = ... -- Network.Socket.recv
dispatchNetIntrinsic "tls_connect"      [sock, host, cfg]= ... -- TLS.contextNew + handshake
dispatchNetIntrinsic "http_request"     [req]            = ... -- http-client request
-- ... etc
```

### 11.3 LLVM Native Backend (C++ Runtime)

The native backend uses a C++ runtime (`runtime/LLVM/`) with `extern "C"` functions called from generated LLVM IR. Networking intrinsics become C function calls linked at compile time.

#### Dispatch Path

```
tulam effect op
  → CLMPRIMCALL "net_tcpConnect"
  → CLMToLIR: LCall @tlm_net_tcp_connect [addr]
  → LIRToLLVM: call @tlm_net_tcp_connect(%addr)
  → clang++ links with tlm_net.o + tlm_tls.o + tlm_http.o
  → C runtime: POSIX socket() + connect()
```

#### C Library Mapping

| Networking Layer | C/C++ Implementation | Dependency | Notes |
|-----------------|---------------------|------------|-------|
| TCP/UDP sockets | POSIX sockets | None (libc) | `socket`, `connect`, `bind`, `listen`, `accept`, `send`, `recv` — ~20 functions, zero deps |
| DNS resolution | POSIX `getaddrinfo` | None (libc) | Thread-safe, supports IPv4+IPv6 |
| TLS | OpenSSL / LibreSSL | System library | `SSL_new`, `SSL_connect`, `SSL_read`, `SSL_write` — ~10 wrapper functions |
| HTTP client | **Option A**: libcurl | Link-time dep | Full HTTP/1.1, HTTP/2, HTTP/3, redirects, cookies. Single `curl_easy_perform` call. |
| HTTP client | **Option B**: Hand-rolled HTTP/1.1 | None | ~200 LOC on top of sockets+TLS. Parse status/headers/chunked. Sufficient for most use cases. |
| HTTP server | Hand-rolled on sockets | None | Accept loop + HTTP/1.1 request parser + response formatter |
| WebSocket | Hand-rolled | None | ~300 LOC — frame parsing, masking, upgrade handshake. Protocol is simple. |
| URI parsing | Hand-rolled or `uriparser` lib | Optional | RFC 3986 is ~100 LOC to parse |
| Base64 | Hand-rolled | None | ~50 LOC encode/decode |
| Bytes backing | `uint8_t*` + length | None | Heap-allocated, reference-counted (Perceus RC when available, Boehm GC as stopgap) |

#### Recommended Approach: Layered Dependencies

```
Dependency-free (POSIX only):  TCP, UDP, DNS, HTTP server, WebSocket, URI, Base64
One system library:            TLS (OpenSSL/LibreSSL — available everywhere)
Optional convenience:          HTTP client via libcurl (or hand-rolled on sockets+TLS)
```

This minimizes the dependency footprint. The only required external library is OpenSSL for TLS, which is pre-installed on virtually every system.

#### Runtime File Plan

```
runtime/LLVM/
  tlm_runtime.hpp          — existing: core runtime (alloc, RC, print, error)
  tlm_runtime.cpp          — existing: core runtime implementation
  tlm_object.hpp           — existing: object layout, header struct
  tlm_bytes.hpp            — NEW: Bytes type (uint8_t* + length + capacity)
  tlm_bytes.cpp            — NEW: Bytes operations (concat, slice, compare, encode/decode)
  tlm_net.hpp              — NEW: Socket types, address types
  tlm_net.cpp              — NEW: TCP/UDP/DNS via POSIX sockets (~300 LOC)
  tlm_tls.hpp              — NEW: TLS stream type
  tlm_tls.cpp              — NEW: TLS via OpenSSL (~150 LOC)
  tlm_http.hpp             — NEW: HTTP request/response types
  tlm_http.cpp             — NEW: HTTP client + server (~400 LOC, or ~50 LOC with libcurl)
  tlm_ws.hpp               — NEW: WebSocket types
  tlm_ws.cpp               — NEW: WebSocket protocol (~300 LOC)
  tlm_uri.hpp              — NEW: URI type
  tlm_uri.cpp              — NEW: URI parser (~100 LOC)
```

#### C Runtime API Surface (`extern "C"`)

```cpp
// === tlm_net.hpp ===

// Opaque handle types (pointers to internal structs)
typedef struct TlmSocket_    TlmSocket;
typedef struct TlmListener_  TlmListener;
typedef struct TlmUdpSocket_ TlmUdpSocket;

// DNS
extern "C" TlmObject* tlm_net_resolve(TlmObject* hostname);        // → List(IpAddr)
extern "C" TlmObject* tlm_net_reverse_lookup(TlmObject* addr);     // → Maybe(String)

// TCP
extern "C" TlmSocket*   tlm_net_tcp_connect(TlmObject* addr);      // SocketAddr → Socket
extern "C" TlmListener* tlm_net_tcp_listen(TlmObject* addr, int64_t backlog);
extern "C" TlmSocket*   tlm_net_accept(TlmListener* listener);
extern "C" int64_t      tlm_net_send(TlmSocket* sock, uint8_t* data, int64_t len);
extern "C" TlmObject*   tlm_net_recv(TlmSocket* sock, int64_t max_bytes); // → Bytes
extern "C" void         tlm_net_send_all(TlmSocket* sock, uint8_t* data, int64_t len);
extern "C" void         tlm_net_close_socket(TlmSocket* sock);
extern "C" void         tlm_net_close_listener(TlmListener* listener);

// UDP
extern "C" TlmUdpSocket* tlm_net_udp_bind(TlmObject* addr);
extern "C" int64_t       tlm_net_udp_send(TlmUdpSocket* s, uint8_t* data,
                                           int64_t len, TlmObject* to);
extern "C" TlmObject*    tlm_net_udp_recv(TlmUdpSocket* s, int64_t max); // → (Bytes, Addr)
extern "C" void          tlm_net_close_udp(TlmUdpSocket* s);


// === tlm_tls.hpp ===

typedef struct TlmTlsStream_ TlmTlsStream;

extern "C" TlmTlsStream* tlm_tls_connect(TlmSocket* sock, TlmObject* hostname,
                                           TlmObject* config);
extern "C" TlmTlsStream* tlm_tls_accept(TlmSocket* sock, TlmObject* config);
extern "C" int64_t        tlm_tls_send(TlmTlsStream* s, uint8_t* data, int64_t len);
extern "C" TlmObject*     tlm_tls_recv(TlmTlsStream* s, int64_t max_bytes);
extern "C" void           tlm_tls_close(TlmTlsStream* s);


// === tlm_http.hpp ===

extern "C" TlmObject* tlm_http_request(TlmObject* req);             // → HttpResponse
extern "C" TlmObject* tlm_http_stream(TlmObject* req);              // → HttpStreamResponse

typedef struct TlmServerHandle_ TlmServerHandle;

extern "C" TlmServerHandle* tlm_http_listen(TlmObject* addr);
extern "C" TlmObject*       tlm_http_accept(TlmServerHandle* server); // → HttpRequest
extern "C" void              tlm_http_respond(TlmServerHandle* server, TlmObject* resp);
extern "C" void              tlm_http_server_close(TlmServerHandle* server);


// === tlm_ws.hpp ===

typedef struct TlmWsConn_ TlmWsConn;

extern "C" TlmWsConn* tlm_ws_connect(TlmObject* url, TlmObject* headers);
extern "C" void        tlm_ws_send(TlmWsConn* conn, TlmObject* msg);
extern "C" TlmObject*  tlm_ws_recv(TlmWsConn* conn);                // → WsMessage
extern "C" void        tlm_ws_close(TlmWsConn* conn, int64_t code, TlmObject* reason);
```

#### Example: `tlm_net_tcp_connect` Implementation

```cpp
// tlm_net.cpp
#include "tlm_net.hpp"
#include "tlm_runtime.hpp"
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>

struct TlmSocket_ {
    int fd;
};

extern "C" TlmSocket* tlm_net_tcp_connect(TlmObject* addr_obj) {
    // Extract SocketAddr fields: host:IpAddr, port:Int
    TlmObject* ip_obj = tlm_field(addr_obj, 0);  // IpAddr
    int64_t port = tlm_unbox_int(tlm_field(addr_obj, 1));

    // Build sockaddr_in from IpAddr (IPv4 case, tag == 0)
    struct sockaddr_in sa = {};
    sa.sin_family = AF_INET;
    sa.sin_port = htons((uint16_t)port);

    if (tlm_tag(ip_obj) == 0) {  // IPv4
        uint8_t a = (uint8_t)tlm_unbox_int(tlm_field(ip_obj, 0));
        uint8_t b = (uint8_t)tlm_unbox_int(tlm_field(ip_obj, 1));
        uint8_t c = (uint8_t)tlm_unbox_int(tlm_field(ip_obj, 2));
        uint8_t d = (uint8_t)tlm_unbox_int(tlm_field(ip_obj, 3));
        sa.sin_addr.s_addr = htonl((a << 24) | (b << 16) | (c << 8) | d);
    }

    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) tlm_error("tcpConnect: socket() failed");

    if (connect(fd, (struct sockaddr*)&sa, sizeof(sa)) < 0) {
        close(fd);
        tlm_error("tcpConnect: connect() failed");
    }

    TlmSocket* sock = (TlmSocket*)tlm_alloc_raw(sizeof(TlmSocket_));
    sock->fd = fd;
    return sock;
}
```

### 11.4 Intrinsic Name Convention

All networking intrinsics follow the `<module>_<operation>` naming convention, consistent with existing intrinsics (`add_Int`, `concat_String`, etc.):

| Intrinsic Name | Net Layer | Both Backends |
|---------------|-----------|---------------|
| `net_resolve` | DNS | Yes |
| `net_reverseLookup` | DNS | Yes |
| `net_tcpConnect` | TCP | Yes |
| `net_tcpListen` | TCP | Yes |
| `net_accept` | TCP | Yes |
| `net_send` | TCP | Yes |
| `net_recv` | TCP | Yes |
| `net_sendAll` | TCP | Yes |
| `net_closeSocket` | TCP | Yes |
| `net_closeListener` | TCP | Yes |
| `net_udpBind` | UDP | Yes |
| `net_udpSend` | UDP | Yes |
| `net_udpRecv` | UDP | Yes |
| `net_closeUdp` | UDP | Yes |
| `tls_connect` | TLS | Yes |
| `tls_accept` | TLS | Yes |
| `tls_send` | TLS | Yes |
| `tls_recv` | TLS | Yes |
| `tls_close` | TLS | Yes |
| `http_request` | HTTP | Yes |
| `http_stream` | HTTP | Yes |
| `http_listen` | HTTP Server | Yes |
| `http_accept` | HTTP Server | Yes |
| `http_respond` | HTTP Server | Yes |
| `http_serverClose` | HTTP Server | Yes |
| `ws_connect` | WebSocket | Yes |
| `ws_send` | WebSocket | Yes |
| `ws_recv` | WebSocket | Yes |
| `ws_close` | WebSocket | Yes |
| `bytes_toBytes` | Bytes | Yes |
| `bytes_fromBytes` | Bytes | Yes |
| `bytes_fromBytesLossy` | Bytes | Yes |
| `uri_parse` | URI | Yes |
| `uri_render` | URI | Yes |
| `uri_encode` | URI | Yes |
| `uri_decode` | URI | Yes |
| `base64_encode` | Encoding | Yes |
| `base64_decode` | Encoding | Yes |

### 11.5 Platform Considerations

#### macOS / Linux (Primary Targets)

| Layer | macOS | Linux |
|-------|-------|-------|
| Sockets | POSIX (libc) | POSIX (libc) |
| DNS | `getaddrinfo` (libc) | `getaddrinfo` (libc) |
| TLS | LibreSSL (system) or OpenSSL (brew) | OpenSSL (system) |
| Async I/O (future) | `kqueue` | `epoll` |

#### Windows (Future)

| Layer | Windows |
|-------|---------|
| Sockets | Winsock2 (`ws2_32.lib`) |
| DNS | `getaddrinfo` (Winsock2) |
| TLS | SChannel (system) or OpenSSL |
| Async I/O (future) | IOCP |

The C runtime should use `#ifdef _WIN32` guards for Winsock differences (startup, `closesocket` vs `close`, etc.). These are minor — the POSIX socket API is nearly identical on all three platforms.

### 11.6 Bytes Primitive — Backend Representation

| Backend | Bytes Representation | Notes |
|---------|---------------------|-------|
| BC Interpreter | Haskell `ByteString` on heap table | Zero-copy slice via BS internals, O(1) length |
| LLVM Native | `struct { uint8_t* data; int64_t len; int64_t cap; }` | Heap-allocated, RC'd (or GC'd in stopgap). Slice = pointer + offset (no copy). |

#### BC Interpreter: Bytes as Heap Object

```haskell
-- In Backends/Bytecode/Value.hs, extend HeapObject:
data HeapObject
    = HOCon !Int !(V.Vector Word64)     -- existing: constructor
    | HOClosure !Int !(V.Vector Word64)  -- existing: closure
    | HOArray !(V.Vector Word64)         -- existing: array
    | HORef !(IORef Word64)              -- existing: mutable ref
    | HOString !Text                     -- existing: string
    | HOBytes !ByteString                -- NEW: byte buffer
    | HOSocket !Socket                   -- NEW: TCP socket handle
    | HOListener !Socket                 -- NEW: TCP listener handle
    | HOUdpSocket !Socket                -- NEW: UDP socket handle
    | HOTlsStream !TlsContext            -- NEW: TLS connection
    | HOServerHandle !ServerState        -- NEW: HTTP server state
    | HOWsConn !WsConnection             -- NEW: WebSocket connection
```

#### LLVM Native: Bytes as Tagged Pointer

```cpp
// tlm_bytes.hpp
struct TlmBytes {
    TlmHeader header;   // standard 8-byte object header (tag=BYTES, rc, flags)
    uint8_t*  data;     // pointer to byte data
    int64_t   len;      // current length
    int64_t   cap;      // allocated capacity (for builder pattern)
};

extern "C" TlmBytes* tlm_bytes_new(int64_t capacity);
extern "C" TlmBytes* tlm_bytes_from_data(const uint8_t* src, int64_t len);
extern "C" TlmBytes* tlm_bytes_slice(TlmBytes* b, int64_t start, int64_t end); // zero-copy
extern "C" TlmBytes* tlm_bytes_concat(TlmBytes* a, TlmBytes* b);
extern "C" int64_t   tlm_bytes_length(TlmBytes* b);
extern "C" uint8_t   tlm_bytes_index(TlmBytes* b, int64_t i);
extern "C" int       tlm_bytes_eq(TlmBytes* a, TlmBytes* b);
extern "C" TlmObject* tlm_bytes_to_string(TlmBytes* b);          // UTF-8 decode
extern "C" TlmBytes* tlm_bytes_from_string(TlmObject* str);      // UTF-8 encode
```

## 12. Implementation Phases (Dual-Backend)

The bytecode interpreter always goes first (faster iteration, no compile-link cycle), then native follows with C runtime wrappers. Each phase delivers working functionality on at least one backend.

### Phase N.1: Foundation (Both Backends)

**Pure types — no I/O, no backend-specific work.**

1. Add `primitive Bytes` to `lib/Prelude.tl`
2. Implement `Bytes` intrinsics:
   - BC: `HOBytes ByteString` heap object + intrinsic dispatch in `VM.hs`
   - Native: `TlmBytes` struct in `tlm_bytes.cpp`
3. Add `Stream(a)` type + instances + combinators to `lib/Collection.tl`
4. Add `Bytes` algebra instances to `lib/Instances.tl`
5. URI parsing:
   - BC: `network-uri` package
   - Native: hand-rolled C parser (~100 LOC)
6. Base64 encode/decode:
   - BC: `base64-bytestring`
   - Native: ~50 LOC C

### Phase N.2: Net Effect — BC Interpreter

1. Define `Net` effect + `StdNet` + `MockNet` handlers in `lib/Net/Net.tl`
2. Add types (`IpAddr`, `SocketAddr`, `Socket`, etc.) to `lib/Net/Types.tl`
3. Implement TCP/UDP/DNS intrinsics via Haskell `network` package
4. Extend `HeapObject` with `HOSocket`, `HOListener`, `HOUdpSocket`
5. Test: raw TCP echo client, DNS resolution, UDP send/recv

### Phase N.3: Net Effect — Native Backend

1. Implement `tlm_net.cpp` (~300 LOC POSIX sockets)
2. Add intrinsic mapping in `CLMToLIR.hs` for `net_*` names
3. Link with `-lc` (sockets are in libc on Unix)
4. Test: same test programs as N.2, compiled natively

### Phase N.4: TLS Effect — BC Interpreter

1. Define `Tls` effect + `StdTls` handler in `lib/Net/Tls.tl`
2. Implement via Haskell `tls` + `x509-store` packages
3. Add `HOTlsStream` to heap objects
4. Test: connect to `https://httpbin.org`, verify handshake

### Phase N.5: TLS Effect — Native Backend

1. Implement `tlm_tls.cpp` (~150 LOC OpenSSL wrappers)
2. Link with `-lssl -lcrypto`
3. Test: same TLS test programs compiled natively

### Phase N.6: HTTP Effect — BC Interpreter

1. Define `Http` + `HttpServer` effects in `lib/Net/Http.tl`
2. Implement `StdHttp` via Haskell `http-client` + `http-client-tls`
3. Implement `StdHttpServer` via raw sockets (or `warp` for production)
4. Implement `TulamHttp` reference handler (builds on Net + Tls)
5. Add convenience functions, response helpers
6. Test: GET/POST against httpbin.org, simple server

### Phase N.7: HTTP Effect — Native Backend

Two options (decide at implementation time):

- **Option A: libcurl** — `tlm_http.cpp` wraps `curl_easy_perform` (~50 LOC). Link with `-lcurl`. Gets HTTP/2 for free.
- **Option B: Hand-rolled** — `tlm_http.cpp` builds HTTP/1.1 on sockets+TLS (~400 LOC). Zero additional deps.

Recommendation: start with Option B (no deps), add libcurl as an optional "fast path" later.

### Phase N.8: WebSocket Effect — Both Backends

1. Define `WebSocket` effect in `lib/Net/WebSocket.tl`
2. BC: implement via Haskell `websockets` package
3. Native: implement `tlm_ws.cpp` — RFC 6455 frame parser (~300 LOC) on top of sockets+TLS
4. Stream integration (`wsMessages`, `wsTextMessages`)

### Phase N.9: Polish & Testing

1. Comprehensive test suite with `MockHttp`/`MockNet` (no network needed)
2. Integration tests against real endpoints (opt-in, CI flag)
3. `lib/Net/Base.tl` umbrella re-export
4. Documentation: update `doc/LanguageReference.md`, `doc/StandardLibrary.md`

## 13. Dependencies

### Bytecode Interpreter (Haskell packages)

| Package | Purpose | Already a dep? |
|---------|---------|----------------|
| `bytestring` | Bytes backing type | Yes |
| `network` | TCP/UDP sockets, DNS | No — add to `package.yaml` |
| `tls` | Pure Haskell TLS | No |
| `x509-store` | TLS certificate store | No |
| `http-client` | HTTP client | No |
| `http-client-tls` | HTTPS support for http-client | No |
| `websockets` | WebSocket protocol | No |
| `network-uri` | URI parsing | No |
| `base64-bytestring` | Base64 encoding | No |

### LLVM Native Backend (C/C++ libraries)

| Library | Purpose | Required? | Link Flags |
|---------|---------|-----------|------------|
| libc (POSIX) | Sockets, DNS, basic I/O | Yes (always available) | (none) |
| OpenSSL / LibreSSL | TLS encryption | Yes (system lib) | `-lssl -lcrypto` |
| libcurl | HTTP client (optional fast path) | No (hand-rolled fallback) | `-lcurl` |

Total external dependency for native: **one library** (OpenSSL), which is pre-installed on virtually every Unix system.

## 14. Testing Strategy

### Unit Tests (No Network Required)

```tulam
// All test with MockHttp / MockNet — run in CI without network access

action testGetRequest() = {
    let mock = MockHttp([HttpResponse(200, "OK", [], toBytes("hello"))]);
    resp <- get [mock] ("https://example.com");
    assert(resp.status == 200);
    assert(bodyText(resp) == "hello")
};

action testDnsResolve() = {
    let config = MockNetConfig { resolveResult = [IPv4(93,184,216,34)] };
    addrs <- resolve [MockNet(config)] ("example.com");
    assert(length(addrs) == 1)
};
```

### Integration Tests (Opt-in, Requires Network)

- TCP: connect to `httpbin.org:80`, send raw HTTP, verify response
- TLS: connect to `httpbin.org:443`, verify handshake + response
- HTTP: `get("https://httpbin.org/get")`, verify JSON response
- WebSocket: connect to a public echo server, send/receive

### Cross-Backend Tests

Same `.tl` test programs run on both backends, output compared:
```bash
# BC interpreter
stack exec tulam -- run tests/net/test_http.tl > bc_output.txt

# Native compiled
stack exec tulam -- compile tests/net/test_http.tl -o test_http
./test_http > native_output.txt

diff bc_output.txt native_output.txt  # must match
```

## 15. Open Design Questions (Future)

- **Connection pooling**: Handler concern — `PooledHttp(poolSize, StdHttp)` wraps StdHttp with keep-alive connection reuse
- **Cookie management**: Handler wrapper that intercepts Set-Cookie and injects Cookie headers
- **Retry/backoff**: Composable via handler wrapping — `RetryHttp(maxRetries, backoff, StdHttp)`
- **Timeouts**: Requires `Async` effect — `race(httpRequest(req), sleep(5000))`. Single-threaded: use `SO_RCVTIMEO`/`SO_SNDTIMEO` socket options as stopgap.
- **HTTP/2 and HTTP/3**: Backend concern — handler can upgrade internally without changing the effect interface. Native: link `nghttp2` or `quiche`. BC: `http2` Haskell package.
- **SSE (Server-Sent Events)**: Natural fit for `Stream(SseEvent)` return type on top of `httpStream`
- **gRPC**: Separate module built on HTTP/2 + protobuf serialization, future work
- **Multipart form uploads**: Helper function to construct multipart Bytes body with boundary
- **Unix domain sockets**: Add to Net effect — `unixConnect(path:String) : Socket`
- **Async I/O (post-concurrency)**: `AsyncNet` handler backed by `epoll`/`kqueue`/IOCP event loop. Same effect interface, non-blocking implementation.
