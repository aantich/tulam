// tulam Runtime System — Public API
// All functions use extern "C" linkage for LLVM IR interop.
#pragma once
#include "tlm_object.hpp"

extern "C" {

// ---- Allocation ----
tlm::Object* tlm_alloc(int32_t tag, int32_t num_fields);

// ---- Field access ----
uint64_t tlm_get_field(tlm::Object* obj, int32_t index);
void     tlm_set_field(tlm::Object* obj, int32_t index, uint64_t value);
uint16_t tlm_get_tag(tlm::Object* obj);

// ---- Printing ----
void tlm_print_int(int64_t val);
void tlm_print_float(double val);
void tlm_print_string(const char* s);
void tlm_print_newline();
void tlm_print_char(int32_t codepoint);
void tlm_print_bool(int32_t val);

// ---- Error handling ----
[[noreturn]] void tlm_error(const char* msg);

// ---- RC operations (Phase A.1: tracking only) ----
void tlm_rc_inc(tlm::Object* obj);
void tlm_rc_dec(tlm::Object* obj);

} // extern "C"
