      DOUBLE PRECISION FUNCTION GAI1_BSWAPD( VALUE )
*+
*  Name:
*     GAI1_SWAPD

*  Purpose:
*     Byte swap a double precision data value (assumes wrong endianness of data).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     NEWVAL = GAI1_BSWAPD( VALUE )

*  Description:
*     This routine swaps the byte order of the given value and returns
*     the result. It is for use when dealing with data (such as FITS)
*     that may have a different endianness to that of the native machine.

*  Arguments:
*     VALUE = DOUBLE PRECISION (Given)
*        The value to byte swap.

*  Notes:
*     -  Not portable, assumes size of Fortran variables and uses
*        non-standard data types.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Authors:
*     PWD: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-JAN-2000 (PWD):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      DOUBLE PRECISION VALUE

*  Local Variables:
      CHARACTER * ( 1 ) CVAL( 8 )
      CHARACTER * ( 1 ) TMP
      DOUBLE PRECISION DVAL

*  Equivalence variable so we can address bytes.
      EQUIVALENCE ( CVAL, DVAL )

*.

*  Swap bytes using equivalenced characters.
      DVAL = VALUE
      TMP = CVAL( 1 )
      CVAL( 1 ) = CVAL( 8 )
      CVAL( 8 ) = TMP

      TMP = CVAL( 2 )
      CVAL( 2 ) = CVAL( 7 )
      CVAL( 7 ) = TMP

      TMP = CVAL( 3 )
      CVAL( 3 ) = CVAL( 6 )
      CVAL( 6 ) = TMP

      TMP = CVAL( 4 )
      CVAL( 4 ) = CVAL( 5 )
      CVAL( 5 ) = TMP

*  Return swapped value.
      GAI1_BSWAPD = DVAL
      RETURN
      END
