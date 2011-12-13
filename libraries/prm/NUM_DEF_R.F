#include "config.h"

*+
*  Name:
*     NUM_DEF_R

*  Purpose:
*     Definitions of NUM_ REAL functions

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
*     Copyright (C) 2003 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     R.F. Warren-Smith (DUVAD::RFWS)
*     Norman Gray (Starlink)

*  History:
*     16-AUG-1988 (RFWS):
*         Original version
*     12-MAR-2003 (NXG):
*         autoconfed

*  Notes:
*    The definitions below use SIND and friends as well as SIN, if these
*    are available as intrinsics (they take and return arguments in
*    degrees instead of radians).  Whether these are available or not is
*    determined by ./configure, which tests whether SIND is available
*    and if it is, assumes that the others are, too.  It configures this
*    file by defining macro HAVE_INTRINSIC_SIND in config.h.  This file
*    must be run through a Fortran preprocessor before use.

*-

*******************************************
*   Arithmetic between two REAL numbers   *
*******************************************


*   Add.
      NUM_ADDR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          NUM_ARGR1 + NUM_ARGR2

*   Subtract.
      NUM_SUBR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          NUM_ARGR1 - NUM_ARGR2

*   Floating multiply.
      NUM_MULR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          NUM_ARGR1 * NUM_ARGR2

*   Floating divide.
      NUM_DIVR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          NUM_ARGR1 / NUM_ARGR2

*   Integer divide.
      NUM_IDVR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          AINT( NUM_ARGR1 / NUM_ARGR2 )

*   Raise to power.
      NUM_PWRR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          NUM_ARGR1 ** NUM_ARGR2



**************************************************
*   REAL functions with a single REAL argument   *
**************************************************


*   Negate argument.
      NUM_NEGR( NUM_ARGR ) = - NUM_ARGR

*   Square root.
      NUM_SQRTR( NUM_ARGR ) = SQRT( NUM_ARGR )

*   Natural logarithm (base e).
      NUM_LOGR( NUM_ARGR ) = LOG( NUM_ARGR )

*   Common logarithm (base 10).
      NUM_LG10R( NUM_ARGR ) = LOG10( NUM_ARGR )

*   Exponential function.
      NUM_EXPR( NUM_ARGR ) = EXP( NUM_ARGR )

*   Sine function (argument in radians).
      NUM_SINR( NUM_ARGR ) = SIN( NUM_ARGR )

*   Cosine function (argument in radians).
      NUM_COSR( NUM_ARGR ) = COS( NUM_ARGR )

*   Tangent function (argument in radians).
      NUM_TANR( NUM_ARGR ) = TAN( NUM_ARGR )

*   Sine function (argument in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_SINDR( NUM_ARGR ) = SIND( NUM_ARGR )
#else
      NUM_SINDR( NUM_ARGR ) = SIN( NUM_ARGR / 57.2957802 )
#endif

*   Cosine function (argument in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_COSDR( NUM_ARGR ) = COSD( NUM_ARGR )
#else
      NUM_COSDR( NUM_ARGR ) = COS( NUM_ARGR / 57.2957802 )
#endif

*   Tangent function (argument in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_TANDR( NUM_ARGR ) = TAND( NUM_ARGR )
#else
      NUM_TANDR( NUM_ARGR ) = TAN( NUM_ARGR / 57.2957802 )
#endif

*   Inverse sine function (result in radians).
      NUM_ASINR( NUM_ARGR ) = ASIN( NUM_ARGR )

*   Inverse cosine function (result in radians).
      NUM_ACOSR( NUM_ARGR ) = ACOS( NUM_ARGR )

*   Inverse tangent function (result in radians).
      NUM_ATANR( NUM_ARGR ) = ATAN( NUM_ARGR )

*   Inverse sine function (result in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_ASNDR( NUM_ARGR ) = ASIND( NUM_ARGR )
#else
      NUM_ASNDR( NUM_ARGR ) = 57.2957802 * ASIN( NUM_ARGR )
#endif

*   Inverse cosine function (result in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_ACSDR( NUM_ARGR ) = ACOSD( NUM_ARGR )
#else
      NUM_ACSDR( NUM_ARGR ) = 57.2957802 * ACOS( NUM_ARGR )
#endif

*   Inverse tangent function (result in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_ATNDR( NUM_ARGR ) = ATAND( NUM_ARGR )
#else
      NUM_ATNDR( NUM_ARGR ) = 57.2957802 * ATAN( NUM_ARGR )
#endif

*   Hyperbolic sine function.
      NUM_SINHR( NUM_ARGR ) = SINH( NUM_ARGR )

*   Hyperbolic cosine function.
      NUM_COSHR( NUM_ARGR ) = COSH( NUM_ARGR )

*   Hyperbolic tangent function.
      NUM_TANHR( NUM_ARGR ) = TANH( NUM_ARGR )

*   Absolute (positive) value.
      NUM_ABSR( NUM_ARGR ) = ABS( NUM_ARGR )

*   Nearest integer.
      NUM_NINTR( NUM_ARGR ) = ANINT( NUM_ARGR )

*   Truncation to integer.
      NUM_INTR( NUM_ARGR ) = AINT( NUM_ARGR )



*********************************************
*  REAL functions with two REAL arguments   *
*********************************************


*   Minimum of two numbers.
      NUM_MINR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          MIN( NUM_ARGR1, NUM_ARGR2 )

*   Maximum of two numbers.
      NUM_MAXR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          MAX( NUM_ARGR1, NUM_ARGR2 )

*   Fortran DIM (positive difference) function.
      NUM_DIMR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          DIM( NUM_ARGR1, NUM_ARGR2 )

*   Fortran MOD (remainder) function.
      NUM_MODR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          MOD( NUM_ARGR1, NUM_ARGR2 )

*   Fortran SIGN (transfer of sign) function.
      NUM_SIGNR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          SIGN( NUM_ARGR1, NUM_ARGR2 )

*   Fortran ATAN2 (inverse tangent with two arguments) function (result
*   in radians).
      NUM_ATN2R( NUM_ARGR1, NUM_ARGR2 ) =
     :                          ATAN2( NUM_ARGR1, NUM_ARGR2 )

*   Fortran ATAN2 (inverse tangent with two arguments) function (result
*   in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_AT2DR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          ATAN2D( NUM_ARGR1, NUM_ARGR2 )
#else
      NUM_AT2DR( NUM_ARGR1, NUM_ARGR2 ) =
     :             57.2957802 * ATAN2( NUM_ARGR1, NUM_ARGR2 )
#endif


****************************************************
*   Logical functions comparing two REAL numbers   *
****************************************************


*   Equality.
      NUM_EQR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          NUM_ARGR1 .EQ. NUM_ARGR2

*   Inequality.
      NUM_NER( NUM_ARGR1, NUM_ARGR2 ) =
     :                          NUM_ARGR1 .NE. NUM_ARGR2

*   Greater than.
      NUM_GTR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          NUM_ARGR1 .GT. NUM_ARGR2

*   Greater than or equal.
      NUM_GER( NUM_ARGR1, NUM_ARGR2 ) =
     :                          NUM_ARGR1 .GE. NUM_ARGR2

*   Less than.
      NUM_LTR( NUM_ARGR1, NUM_ARGR2 ) =
     :                          NUM_ARGR1 .LT. NUM_ARGR2

*   Less than or equal.
      NUM_LER( NUM_ARGR1, NUM_ARGR2 ) =
     :                          NUM_ARGR1 .LE. NUM_ARGR2



************************************************************************
*                               END                                    *
*                                                                      *
*                Definitions of NUM_ REAL functions                    *
*                                                                      *
*                               END                                    *
************************************************************************
