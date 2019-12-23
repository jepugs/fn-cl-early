// numbers.hpp -- basic operations on integers and floats

#ifndef __FNBASE_NUMBERS_HPP
#define __FNBASE_NUMBERS_HPP

#include "data.hpp"
#include "simple.hpp"

namespace fnbase {

bool primIsFloat(Object *obj) {
    return obj->tag == TAGS.FLOAT;
}

Object *unsafeAddFloat(Object x, Object y) {
    return new Object(TAGS.FLOAT, x.data.floatnum + y.data.floatnum);
}
Object *unsafeSubFloat(Object x, Object y) {
    return new Object(TAGS.FLOAT, x.data.floatnum - y.data.floatnum);
}
Object *unsafeMulFloat(Object x, Object y) {
    return new Object(TAGS.FLOAT, x.data.floatnum * y.data.floatnum);
}
Object *unsafeDivFloat(Object x, Object y) {
    return new Object(TAGS.FLOAT, x.data.floatnum / y.data.floatnum);
}

}

#endif
