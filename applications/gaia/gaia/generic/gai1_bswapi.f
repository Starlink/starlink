      INTEGER FUNCTION GAI1_BSWAPI( VALUE )
*+
*  Name:
*     GAI1_SWAPI

*  Purpose:
*     Byte swap an integer data value (assumes wrong endianness of data).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     NEWVAL = GAI1_BSWAPI( VALUE )

*  Description:
*     This routine swaps the byte order of the given value and returns
*     the result. It is for use when dealing with data (such as FITS)
*     that may have a different endianness to that of the native machine.

*  Arguments:
*     VALUE = INTEGER (Given)
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
      INTEGER VALUE

*  Local Variables:
      CHARACTER * ( 1 ) CVAL( 4 )
      CHARACTER * ( 1 ) TMP
      INTEGER RVAL

*  Equivalence variable to access individual bytes.
      EQUIVALENCE ( CVAL, RVAL )

*.

*  Swap bytes using equivalenced characters.
      RVAL = VALUE
      TMP = CVAL( 1 )
      CVAL( 1 ) = CVAL( 4 )
      CVAL( 4 ) = TMP
      TMP = CVAL( 2 )
      CVAL( 2 ) = CVAL( 3 )
      CVAL( 3 ) = TMP

*  Return swapped value.
      GAI1_BSWAPI = RVAL
      RETURN
      END
