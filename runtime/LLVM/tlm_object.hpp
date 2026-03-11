// tulam Runtime System — Object Layout
// Phase A.1: Single-threaded, Boehm GC or leak-and-exit.
//
// Header layout is STABLE across all phases. Only the interpretation of
// rc_or_idx changes (bit 31 = 0: local RC; bit 31 = 1: side table index).
#pragma once
#include <cstdint>
#include <cstddef>

namespace tlm {

/// Object header: 8 bytes total.
/// Bytes 0-1: tag (constructor discriminant)
/// Byte 2:   arity (number of fields)
/// Byte 3:   flags (reserved)
/// Bytes 4-7: rc (refcount; bit 31 reserved for Phase 2 side-table promotion)
struct Header {
    uint16_t tag;
    uint8_t  arity;
    uint8_t  flags;
    uint32_t rc;

    Header(uint16_t tag, uint8_t arity)
        : tag(tag), arity(arity), flags(0), rc(1) {}
};

static_assert(sizeof(Header) == 8, "Header must be exactly 8 bytes");

/// A heap-allocated tulam object: header + N fields (each 8 bytes).
/// Fields store either unboxed values (int64, double via bitcast) or pointers.
struct Object {
    Header header;
    uint64_t fields[];  // flexible array member

    uint16_t getTag() const { return header.tag; }
    uint8_t  getArity() const { return header.arity; }

    uint64_t getField(int index) const { return fields[index]; }
    void     setField(int index, uint64_t value) { fields[index] = value; }

    Object* getFieldPtr(int index) const {
        return reinterpret_cast<Object*>(static_cast<uintptr_t>(fields[index]));
    }
    void setFieldPtr(int index, Object* ptr) {
        fields[index] = static_cast<uint64_t>(reinterpret_cast<uintptr_t>(ptr));
    }
};

/// Compute allocation size for an object with N fields.
constexpr size_t objectSize(int numFields) {
    return sizeof(Header) + static_cast<size_t>(numFields) * sizeof(uint64_t);
}

} // namespace tlm
