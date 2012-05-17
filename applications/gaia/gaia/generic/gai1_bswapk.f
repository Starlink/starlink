      INTEGER FUNCTION GAI1_BSWAPK( VALUE )
*+
*  Name:
*     GAI1_SWAPK

*  Purpose:
*     Byte swap an integer*8 data value (assumes wrong endianness of data).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     NEWVAL = GAI1_BSWAPK( VALUE )

*  Description:
*     This routine swaps the byte order of the given value and returns
*     the result. It is for use when dealing with data (such as FITS)
*     that may have a different endianness to that of the native machine.

*  Arguments:
*     VALUE = INTEGER*8 (Given)
*        The value to byte swap.

*  Notes:
*     -  Not portable, assumes size of Fortran variables and uses
*        non-standard data types.

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     PWD: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-MAY-2012 (PWD):
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
      CHARACTER * ( 1 ) CVAL( 8 )
      CHARACTER * ( 1 ) TMP
      INTEGER*8 RVAL

*  Equivalence variable to access individual bytes.
      EQUIVALENCE ( CVAL, RVAL )

*.

*  Swap bytes using equivalenced characters.
      RVAL = VALUE

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
      GAI1_BSWAPK = RVAL
      RETURN
      END
