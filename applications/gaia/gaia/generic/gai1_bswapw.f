      INTEGER FUNCTION GAI1_BSWAPW( VALUE )
*+
*  Name:
*     GAI1_SWAPW

*  Purpose:
*     Byte swap an integer*2 (word) data value (assumes wrong endianness
*     of data). 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     NEWVAL = GAI1_BSWAPW ( VALUE )

*  Description:
*     This routine swaps the byte order of the given value and returns
*     the result. It is for use when dealing with data (such as FITS)
*     that may have a different endianness to that of the native machine.

*  Arguments:
*     VALUE = INTEGER*2 (Given)
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
      INTEGER*2 VALUE

*  Local Variables:
      CHARACTER * ( 1 ) CVAL( 2 )           
      CHARACTER * ( 1 ) TMP
      INTEGER*2 RVAL

*  Equivalence variable to access individual bytes.
      EQUIVALENCE ( CVAL, RVAL )

*.

*  Swap bytes using equivalenced characters.
      RVAL = VALUE
      TMP = CVAL( 1 )
      CVAL( 1 ) = CVAL( 2  )
      CVAL( 2  ) = TMP

*  Return swapped value.
      GAI1_BSWAPW  = RVAL
      RETURN
      END
