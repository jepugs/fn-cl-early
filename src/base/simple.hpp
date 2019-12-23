// simple.hpp -- null and boolean type definitions

#ifndef __FNBASE_NUMBERS_HPP
#define __FNBASE_NUMBERS_HPP

#include "data.hpp"

namespace fnbase {

Object FALSE_OBJ;
Object TRUE_OBJ;
Object NULL_OBJ;

Object primIsNull(Object *x) {
    return x->tag == 
}

Object primToBool(bool x) {
    return x ? TRUE_OBJ : FALSE_OBJ;
}

}

#endif
