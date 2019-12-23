// data.hpp -- low-level representation of fn data

#ifndef FNBASE_TYPES_HPP
#define FNBASE_TYPES_HPP

#include <stdint.h>

namespace fnbase {


typedef union {
    // used to hold fixnums, bools, etc
    int64_t fixnum;
    // IMPLNOTE: these better be 64 bits or we're in trouble
    double floatnum;
    void* obj;
} Data;

typedef uint32_t Tag;

struct Object {
    Tag tag;
    Data data;

    Object(tag,data) {
        this->tag = tag;
        this->data = data;
    }
};

enum TAGS {
    ARRAY,              // (Mutable) array of normal fn objects
    CLASS,              // Class
    BOOL,               // True/False
    BIGINT,             // Arbitrary precision integer
    BYTES,              // Array of bytes. Can hold references to foreign objects or untagged fn
                        // data
    FIXNUM,             // 64-bit signed integer
    FLOAT,              // 64-bit floating point number
    FOREIGN_STRUCTURE,  // Foreign structure
    FUNCTION,           // Function
    LIST,               // Previously evaluated cell in a list
    EMPTY,              // Empty list
    UNEV_LIST,          // Unevaluated cell in a list
    METHOD,             // Method
    MODULE,             // Module
    NULL,               // Null value
    STRING,             // (ASCII) String
    STRUCTURE,          // Structure (defined directly in fn source)
    SYMBOL,             // Symbol
    TABLE,              // (Immutable) hash table
    VECTOR              // (Immutable) vector
};



// IMPLNOTE: bool, fixnum, and float values are stored directly in the data union.


// Built-in structure types
struct Class {
    Symbol name;
};

struct Function {
    // SBCL object
};

struct List {
    bool evaluated;
    Object head;
    List *tail;
};

struct Module {
    String name;
    SymbolTable symTab;
};

struct SymbolTable {
    // hash table from strings to symbols
};

struct Method {
    // SBCL object
};

// TODO: these should probably be immutable
struct String {
    uint32_t len;
    uint8_t* chars;
};

struct Structure {
    Class _class;
    // hash table from symbols to objects
};


// flags describing symbol binding properties
const uint8_t SYM_HAS_VALUE = 0x01;
const uint8_t SYM_HAS_MACRO = 0x02;
const uint8_t SYM_MUTABLE = 0x04;
const uint8_t SYM_DYNAMIC = 0x08;
const uint8_t SYM_INTERNAL = 0x10;
const uint8_t SYM_EXPORTED = 0x20;
const uint8_t SYM_IMPORTED = 0x40;

struct Symbol {
    String name;
    Module* module;

    uint8_t flags;

    Object* value;
    String valueDoc;

    Function* macro;
    String macroDoc;
};


}

#endif
