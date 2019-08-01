#ifndef __FN_VALUES_H
#define __FN_VALUES_H

#include <stdint>

namespace fn {

// All values fit within 8 bytes. The first few bits are a tag indicating the value's type.
typedef Val uint64_t;

// 3-bit type tags
const uint8_t TAG_NUM =     0x00;
const uint8_t TAG_CONS =    0x20;
const uint8_t TAG_STR =     0x40;
const uint8_t TAG_OBJ =     0x60;
const uint8_t TAG_FUN =     0x80;

// indicates extended tag
const uint8_t TAG_EXT =     0xe0;

// 8-bit extended tags
const uint8_t ETAG_NULL =   0xe0;
const uint8_t ETAG_TRUE =   0xe1;
const uint8_t ETAG_FALSE =  0xe2;
const uint8_t ETAG_SYM =    0xf0;
const uint8_t ETAG_BYTES =  0xf1;

// value types (i.e. tags)
typedef VType enum {
    V_NUM,
    V_CONS,
    V_STR,
    V_OBJ,
    V_FUN,
    V_NULL,
    V_TRUE,
    V_FALSE,
    V_SYM,
    V_BYTES,
    // unrecognized tag (usually an error)
    V_UNKNOWN
};

inline VType vtypeOf(Val v) {
    switch ((v >> 56) & 0xe0) {
    case TAG_NUM:
        return V_NUM;
    case TAG_CONS:
        return V_CONS;
    case TAG_STR:
        return V_STR;
    case TAG_OBJ:
        return V_OBJ;
    case TAG_FUN:
        return V_FUN;
    case TAG_EXT:
        switch ((v >> 56) & 0xff) {
        case ETAG_NULL:
            return V_NULL;
        case ETAG_TRUE:
            return V_TRUE;
        case ETAG_FALSE:
            return V_FALSE;
        case ETAG_SYM:
            return V_SYM;
        case ETAG_BYTES:
            return V_BYTES;
        }
    default:
        return V_UNKNOWN;
    }
}

typedef Obj union {
    Cons *x;
};

// get the pointer from an fn value. All pointers are multiples of 8 so we only need 61-bits of data
// after the tag
inline Obj *getPointer(Val v) {
    return v << 3;
}


#endif
