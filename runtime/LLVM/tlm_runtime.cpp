// tulam Runtime System — Implementation
// Phase A.1: malloc-based allocation, no real GC.
#include "tlm_runtime.hpp"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <new>

// ---- Allocation ----

extern "C" tlm::Object* tlm_alloc(int32_t tag, int32_t num_fields) {
    size_t size = tlm::objectSize(num_fields);
    auto* obj = static_cast<tlm::Object*>(std::malloc(size));
    if (!obj) {
        std::fprintf(stderr, "tulam: out of memory (requested %zu bytes)\n", size);
        std::exit(1);
    }
    new (&obj->header) tlm::Header(static_cast<uint16_t>(tag),
                                    static_cast<uint8_t>(num_fields));
    if (num_fields > 0) {
        std::memset(obj->fields, 0, static_cast<size_t>(num_fields) * sizeof(uint64_t));
    }
    return obj;
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

// ---- Error handling ----

extern "C" [[noreturn]] void tlm_error(const char* msg) {
    std::fprintf(stderr, "tulam runtime error: %s\n", msg);
    std::exit(1);
}

// ---- RC operations ----
// Phase A.1: increment/decrement for debugging, no actual freeing.
// Phase E will replace with Perceus drop + reuse.

extern "C" void tlm_rc_inc(tlm::Object* obj) {
    if (obj) obj->header.rc++;
}

extern "C" void tlm_rc_dec(tlm::Object* obj) {
    if (obj) {
        obj->header.rc--;
        // Phase A.1: leak — no free. Phase E: Perceus drop + reuse.
    }
}
