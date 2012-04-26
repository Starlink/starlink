#include <stdio.h>
#include <prm_par.h>

#include "cgeneric.h"

#define CGEN_CODE_TYPE CGEN_DOUBLE_TYPE
#include "cgeneric_defs.h"
#include "generic_func.cgen"
#undef CGEN_CODE_TYPE

#define CGEN_CODE_TYPE CGEN_FLOAT_TYPE
#include "cgeneric_defs.h"
#include "generic_func.cgen"
#undef CGEN_CODE_TYPE

#define CGEN_CODE_TYPE CGEN_INT_TYPE
#include "cgeneric_defs.h"
#include "generic_func.cgen"
#undef CGEN_CODE_TYPE

#define CGEN_CODE_TYPE CGEN_WORD_TYPE
#include "cgeneric_defs.h"
#include "generic_func.cgen"
#undef CGEN_CODE_TYPE

#define CGEN_CODE_TYPE CGEN_UWORD_TYPE
#include "cgeneric_defs.h"
#include "generic_func.cgen"
#undef CGEN_CODE_TYPE

#define CGEN_CODE_TYPE CGEN_BYTE_TYPE
#include "cgeneric_defs.h"
#include "generic_func.cgen"
#undef CGEN_CODE_TYPE

#define CGEN_CODE_TYPE CGEN_UBYTE_TYPE
#include "cgeneric_defs.h"
#include "generic_func.cgen"
#undef CGEN_CODE_TYPE

#define CGEN_CODE_TYPE CGEN_INT64_TYPE
#include "cgeneric_defs.h"
#include "generic_func.cgen"
#undef CGEN_CODE_TYPE

int main( int argc, char *argv[] )
{
    int status = 1;
    status = status && kpg1_callD( "double" );
    status = status && kpg1_callF( "float" );
    status = status && kpg1_callI( "integer" );
    status = status && kpg1_callW( "word" );
    status = status && kpg1_callUW( "unsigned word" );
    status = status && kpg1_callB( "byte" );
    status = status && kpg1_callUB( "unsigned byte" );
    status = status && kpg1_callK( "64-bit int" );
    return (status == 0);
}

