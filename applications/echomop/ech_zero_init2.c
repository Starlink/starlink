#include "f77.h"

F77_SUBROUTINE( ech_zero_realc )( INTEGER(count), INTEGER(array) )
{
GENPTR_INTEGER( count )
GENPTR_INTEGER( array )

F77_INTEGER_TYPE i;
F77_INTEGER_TYPE *ptr = array;

    i = *count >> 6;

    switch ( *count % 64 ) {
        case(  0 ): do { *ptr++ = 0;
        case( 63 ): *ptr++ = 0;
        case( 62 ): *ptr++ = 0;
        case( 61 ): *ptr++ = 0;
        case( 60 ): *ptr++ = 0;
        case( 59 ): *ptr++ = 0;
        case( 58 ): *ptr++ = 0;
        case( 57 ): *ptr++ = 0;
        case( 56 ): *ptr++ = 0;
        case( 55 ): *ptr++ = 0;
        case( 54 ): *ptr++ = 0;
        case( 53 ): *ptr++ = 0;
        case( 52 ): *ptr++ = 0;
        case( 51 ): *ptr++ = 0;
        case( 50 ): *ptr++ = 0;
        case( 49 ): *ptr++ = 0;
        case( 48 ): *ptr++ = 0;
        case( 47 ): *ptr++ = 0;
        case( 46 ): *ptr++ = 0;
        case( 45 ): *ptr++ = 0;
        case( 44 ): *ptr++ = 0;
        case( 43 ): *ptr++ = 0;
        case( 42 ): *ptr++ = 0;
        case( 41 ): *ptr++ = 0;
        case( 40 ): *ptr++ = 0;
        case( 39 ): *ptr++ = 0;
        case( 38 ): *ptr++ = 0;
        case( 37 ): *ptr++ = 0;
        case( 36 ): *ptr++ = 0;
        case( 35 ): *ptr++ = 0;
        case( 34 ): *ptr++ = 0;
        case( 33 ): *ptr++ = 0;
        case( 32 ): *ptr++ = 0;
        case( 31 ): *ptr++ = 0;
        case( 30 ): *ptr++ = 0;
        case( 29 ): *ptr++ = 0;
        case( 28 ): *ptr++ = 0;
        case( 27 ): *ptr++ = 0;
        case( 26 ): *ptr++ = 0;
        case( 25 ): *ptr++ = 0;
        case( 24 ): *ptr++ = 0;
        case( 23 ): *ptr++ = 0;
        case( 22 ): *ptr++ = 0;
        case( 21 ): *ptr++ = 0;
        case( 20 ): *ptr++ = 0;
        case( 19 ): *ptr++ = 0;
        case( 18 ): *ptr++ = 0;
        case( 17 ): *ptr++ = 0;
        case( 16 ): *ptr++ = 0;
        case( 15 ): *ptr++ = 0;
        case( 14 ): *ptr++ = 0;
        case( 13 ): *ptr++ = 0;
        case( 12 ): *ptr++ = 0;
        case( 11 ): *ptr++ = 0;
        case( 10 ): *ptr++ = 0;
        case(  9 ): *ptr++ = 0;
        case(  8 ): *ptr++ = 0;
        case(  7 ): *ptr++ = 0;
        case(  6 ): *ptr++ = 0;
        case(  5 ): *ptr++ = 0;
        case(  4 ): *ptr++ = 0;
        case(  3 ): *ptr++ = 0;
        case(  2 ): *ptr++ = 0;
        case(  1 ): *ptr++ = 0;
        } while ( --i > 0 );
    }

return;
}

F77_SUBROUTINE( ech_zero_dble )( INTEGER(count), DOUBLE(array) )
{
GENPTR_INTEGER( count )
GENPTR_DOUBLE( array )

F77_INTEGER_TYPE i;
F77_DOUBLE_TYPE *ptr = array;

    i = *count >> 6;

    switch ( *count % 64 ) {
        case(  0 ): do { *ptr++ = 0.0;
        case( 63 ): *ptr++ = 0.0;
        case( 62 ): *ptr++ = 0.0;
        case( 61 ): *ptr++ = 0.0;
        case( 60 ): *ptr++ = 0.0;
        case( 59 ): *ptr++ = 0.0;
        case( 58 ): *ptr++ = 0.0;
        case( 57 ): *ptr++ = 0.0;
        case( 56 ): *ptr++ = 0.0;
        case( 55 ): *ptr++ = 0.0;
        case( 54 ): *ptr++ = 0.0;
        case( 53 ): *ptr++ = 0.0;
        case( 52 ): *ptr++ = 0.0;
        case( 51 ): *ptr++ = 0.0;
        case( 50 ): *ptr++ = 0.0;
        case( 49 ): *ptr++ = 0.0;
        case( 48 ): *ptr++ = 0.0;
        case( 47 ): *ptr++ = 0.0;
        case( 46 ): *ptr++ = 0.0;
        case( 45 ): *ptr++ = 0.0;
        case( 44 ): *ptr++ = 0.0;
        case( 43 ): *ptr++ = 0.0;
        case( 42 ): *ptr++ = 0.0;
        case( 41 ): *ptr++ = 0.0;
        case( 40 ): *ptr++ = 0.0;
        case( 39 ): *ptr++ = 0.0;
        case( 38 ): *ptr++ = 0.0;
        case( 37 ): *ptr++ = 0.0;
        case( 36 ): *ptr++ = 0.0;
        case( 35 ): *ptr++ = 0.0;
        case( 34 ): *ptr++ = 0.0;
        case( 33 ): *ptr++ = 0.0;
        case( 32 ): *ptr++ = 0.0;
        case( 31 ): *ptr++ = 0.0;
        case( 30 ): *ptr++ = 0.0;
        case( 29 ): *ptr++ = 0.0;
        case( 28 ): *ptr++ = 0.0;
        case( 27 ): *ptr++ = 0.0;
        case( 26 ): *ptr++ = 0.0;
        case( 25 ): *ptr++ = 0.0;
        case( 24 ): *ptr++ = 0.0;
        case( 23 ): *ptr++ = 0.0;
        case( 22 ): *ptr++ = 0.0;
        case( 21 ): *ptr++ = 0.0;
        case( 20 ): *ptr++ = 0.0;
        case( 19 ): *ptr++ = 0.0;
        case( 18 ): *ptr++ = 0.0;
        case( 17 ): *ptr++ = 0.0;
        case( 16 ): *ptr++ = 0.0;
        case( 15 ): *ptr++ = 0.0;
        case( 14 ): *ptr++ = 0.0;
        case( 13 ): *ptr++ = 0.0;
        case( 12 ): *ptr++ = 0.0;
        case( 11 ): *ptr++ = 0.0;
        case( 10 ): *ptr++ = 0.0;
        case(  9 ): *ptr++ = 0.0;
        case(  8 ): *ptr++ = 0.0;
        case(  7 ): *ptr++ = 0.0;
        case(  6 ): *ptr++ = 0.0;
        case(  5 ): *ptr++ = 0.0;
        case(  4 ): *ptr++ = 0.0;
        case(  3 ): *ptr++ = 0.0;
        case(  2 ): *ptr++ = 0.0;
        case(  1 ): *ptr++ = 0.0;
        } while ( --i > 0 );
    }

return;
}
