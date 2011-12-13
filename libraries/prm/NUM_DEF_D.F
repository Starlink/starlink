#include "config.h"

*+
*  Name:
*     NUM_DEF_D

*  Purpose:
*     Definitions of NUM_ DOUBLE PRECISION functions

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
*     RFWS: R.F. Warren-Smith (Durham)
*     NXG: Norman Gray (Starlink)

*    History:
*     16-AUG-1988 (RFWS):
*         Original version
*     12-MAR-2003 (NXG):
*         autoconfed

*  Notes:
*     The definitions below use SIND and friends as well as SIN, if these
*     are available as intrinsics (they take and return arguments in
*     degrees instead of radians).  Whether these are available or not is
*     determined by ./configure, which tests whether SIND is available
*     and if it is, assumes that the others are, too.  It configures this
*     file by defining macro HAVE_INTRINSIC_SIND in config.h.  This file must
*     be run through a Fortran preprocessor before use.

*-

*******************************************************
*   Arithmetic between two DOUBLE PRECISION numbers   *
*******************************************************


*   Add.
      NUM_ADDD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          NUM_ARGD1 + NUM_ARGD2

*   Subtract.
      NUM_SUBD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          NUM_ARGD1 - NUM_ARGD2

*   Floating multiply.
      NUM_MULD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          NUM_ARGD1 * NUM_ARGD2

*   Floating divide.
      NUM_DIVD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          NUM_ARGD1 / NUM_ARGD2

*   Integer divide.
      NUM_IDVD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          AINT( NUM_ARGD1 / NUM_ARGD2 )

*   Raise to power.
      NUM_PWRD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          NUM_ARGD1 ** NUM_ARGD2



************************************************************************
*   DOUBLE PRECISION functions with a single DOUBLE PRECISION argument *
************************************************************************


*   Negate argument.
      NUM_NEGD( NUM_ARGD ) = - NUM_ARGD

*   Square root.
      NUM_SQRTD( NUM_ARGD ) = SQRT( NUM_ARGD )

*   Natural logarithm (base e).
      NUM_LOGD( NUM_ARGD ) = LOG( NUM_ARGD )

*   Common logarithm (base 10).
      NUM_LG10D( NUM_ARGD ) = LOG10( NUM_ARGD )

*   Exponential function.
      NUM_EXPD( NUM_ARGD ) = EXP( NUM_ARGD )

*   Sine function (argument in radians).
      NUM_SIND( NUM_ARGD ) = SIN( NUM_ARGD )

*   Cosine function (argument in radians).
      NUM_COSD( NUM_ARGD ) = COS( NUM_ARGD )

*   Tangent function (argument in radians).
      NUM_TAND( NUM_ARGD ) = TAN( NUM_ARGD )

*   Sine function (argument in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_SINDD( NUM_ARGD ) = SIND( NUM_ARGD )
#else
      NUM_SINDD( NUM_ARGD ) = SIN( NUM_ARGD / 57.295779513082323 )
#endif

*   Cosine function (argument in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_COSDD( NUM_ARGD ) = COSD( NUM_ARGD )
#else
      NUM_COSDD( NUM_ARGD ) = COS( NUM_ARGD / 57.295779513082323 )
#endif

*   Tangent function (argument in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_TANDD( NUM_ARGD ) = TAND( NUM_ARGD )
#else
      NUM_TANDD( NUM_ARGD ) = TAN( NUM_ARGD / 57.295779513082323)
#endif

*   Inverse sine function (result in radians).
      NUM_ASIND( NUM_ARGD ) = ASIN( NUM_ARGD )

*   Inverse cosine function (result in radians).
      NUM_ACOSD( NUM_ARGD ) = ACOS( NUM_ARGD )

*   Inverse tangent function (result in radians).
      NUM_ATAND( NUM_ARGD ) = ATAN( NUM_ARGD )

*   Inverse sine function (result in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_ASNDD( NUM_ARGD ) = ASIND( NUM_ARGD )
#else
      NUM_ASNDD( NUM_ARGD ) = 57.295779513082323 * ASIN( NUM_ARGD )
#endif

*   Inverse cosine function (result in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_ACSDD( NUM_ARGD ) = ACOSD( NUM_ARGD )
#else
      NUM_ACSDD( NUM_ARGD ) = 57.295779513082323 * ACOS( NUM_ARGD )
#endif

*   Inverse tangent function (result in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_ATNDD( NUM_ARGD ) = ATAND( NUM_ARGD )
#else
      NUM_ATNDD( NUM_ARGD ) = 57.295779513082323 * ATAN( NUM_ARGD )
#endif

*   Hyperbolic sine function.
      NUM_SINHD( NUM_ARGD ) = SINH( NUM_ARGD )

*   Hyperbolic cosine function.
      NUM_COSHD( NUM_ARGD ) = COSH( NUM_ARGD )

*   Hyperbolic tangent function.
      NUM_TANHD( NUM_ARGD ) = TANH( NUM_ARGD )

*   Absolute (positive) value.
      NUM_ABSD( NUM_ARGD ) = ABS( NUM_ARGD )

*   Nearest integer.
      NUM_NINTD( NUM_ARGD ) = ANINT( NUM_ARGD )

*   Truncation to integer.
      NUM_INTD( NUM_ARGD ) = AINT( NUM_ARGD )



*********************************************************************
*  DOUBLE PRECISION functions with two DOUBLE PRECISION arguments   *
*********************************************************************


*   Minimum of two numbers.
      NUM_MIND( NUM_ARGD1, NUM_ARGD2 ) =
     :                          MIN( NUM_ARGD1, NUM_ARGD2 )

*   Maximum of two numbers.
      NUM_MAXD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          MAX( NUM_ARGD1, NUM_ARGD2 )

*   Fortran DIM (positive difference) function.
      NUM_DIMD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          DIM( NUM_ARGD1, NUM_ARGD2 )

*   Fortran MOD (remainder) function.
      NUM_MODD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          MOD( NUM_ARGD1, NUM_ARGD2 )

*   Fortran SIGN (transfer of sign) function.
      NUM_SIGND( NUM_ARGD1, NUM_ARGD2 ) =
     :                          SIGN( NUM_ARGD1, NUM_ARGD2 )

*   Fortran ATAN2 (inverse tangent with two arguments) function (result
*   in radians).
      NUM_ATN2D( NUM_ARGD1, NUM_ARGD2 ) =
     :                          ATAN2( NUM_ARGD1, NUM_ARGD2 )

*   Fortran ATAN2 (inverse tangent with two arguments) function (result
*   in degrees).
#if HAVE_INTRINSIC_SIND
      NUM_AT2DD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          ATAN2D( NUM_ARGD1, NUM_ARGD2 )
#else
      NUM_AT2DD( NUM_ARGD1, NUM_ARGD2 ) =
     :              57.295779513082323 * ATAN2( NUM_ARGD1, NUM_ARGD2 )
#endif



****************************************************************
*   Logical functions comparing two DOUBLE PRECISION numbers   *
****************************************************************


*   Equality.
      NUM_EQD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          NUM_ARGD1 .EQ. NUM_ARGD2

*   Inequality.
      NUM_NED( NUM_ARGD1, NUM_ARGD2 ) =
     :                          NUM_ARGD1 .NE. NUM_ARGD2

*   Greater than.
      NUM_GTD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          NUM_ARGD1 .GT. NUM_ARGD2

*   Greater than or equal.
      NUM_GED( NUM_ARGD1, NUM_ARGD2 ) =
     :                          NUM_ARGD1 .GE. NUM_ARGD2

*   Less than.
      NUM_LTD( NUM_ARGD1, NUM_ARGD2 ) =
     :                          NUM_ARGD1 .LT. NUM_ARGD2

*   Less than or equal.
      NUM_LED( NUM_ARGD1, NUM_ARGD2 ) =
     :                          NUM_ARGD1 .LE. NUM_ARGD2



************************************************************************
*                               END                                    *
*                                                                      *
*           Definitions of NUM_ DOUBLE PRECISION functions             *
*                                                                      *
*                               END                                    *
************************************************************************
