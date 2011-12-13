      SUBROUTINE CHR_UCASE( STRING )
*+
*  Name:
*     CHR_UCASE

*  Purpose:
*     Convert a string to uppercase.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_UCASE( STRING )

*  Description:
*     The characters in the string are all converted to uppercase
*     in situ.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string to be converted to uppercase.

*  Algorithm:
*     Portable method:
*        Use CHR_LEN to find the string length, and then CHR_UPPER to
*        convert each character.
*     Vax-specific method:
*        Use STR$UPCASE from the VMS Run-Time Library.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1994 Science & Engineering Research Council.
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
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     16-NOV-1984 (ACD):
*        Documentation improved.
*     23-AUG-1988 (AJC):
*        Use VMS Run-Time Library.
*     13-SEP-1988 (AJC):
*        Improve documentation.
*     10-MAR-1994 (ACC):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      CHARACTER STRING * ( * )

*  Portable version.
*  External References:
      CHARACTER * 1 CHR_UPPER    ! Convert character to uppercase

      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Variables:
      INTEGER IPOSN              ! Current position in the string
      INTEGER SIZE               ! Length of the string

*.

*  Convert string to uppercase.
      SIZE = CHR_LEN( STRING )

      DO 10 IPOSN = 1, SIZE
         STRING( IPOSN : IPOSN ) = CHR_UPPER( STRING( IPOSN : IPOSN ) )
 10   CONTINUE

*  VMS-specific version.
*      CALL STR$UPCASE( STRING, STRING )

      END
