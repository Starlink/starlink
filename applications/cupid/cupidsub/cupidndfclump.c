/*
*  Name:
*    cupidndfclump.c

*  Purpose:
*    This file expands the generic C code held in cupidndfclump.cgen to provide
*    the required type-specific implementations which can be called by
*    other functions.

*  Notes:
*    - This file is generated automatically at build time (see
*    cupidsub/Makefile.am)
*/

#include "prm_par.h"
#include "cgeneric.h"

#define CGEN_CODE_TYPE CGEN_DOUBLE_TYPE
#include "cgeneric_defs.h"
#include "cupidndfclump.cgen"
#undef CGEN_CODE_TYPE

#define CGEN_CODE_TYPE CGEN_FLOAT_TYPE
#include "cgeneric_defs.h"
#include "cupidndfclump.cgen"
#undef CGEN_CODE_TYPE

