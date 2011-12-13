      SUBROUTINE CHR_CLEAN( STRING )
*+
*  Name:
*     CHR_CLEAN

*  Purpose:
*     Remove all unprintable characters from a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_CLEAN( STRING )

*  Description:
*     Replace all unprintable characters in the given string with blanks.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        String to be cleaned.

*  Copyright:
*     Copyright (C) 1983, 1984, 1990 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     7-NOV-1984 (ACD):
*        Documentation improved.
*     12-SEP-1990 (PCTR):
*        Prologue changes and portable version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      CHARACTER STRING * ( * )

*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Variables:
      CHARACTER CVALUE           ! Character value

      INTEGER ICHR               ! Character loop index
      INTEGER SIZE               ! Character count

*.

*  Perform clean-up.
      SIZE = CHR_LEN( STRING )

      DO 10 ICHR = 1, SIZE
         CVALUE = STRING( ICHR : ICHR )

         IF ( LLT( CVALUE, ' ' ) .OR. LGT( CVALUE, '~' ) ) THEN
            STRING( ICHR : ICHR ) = ' '
         END IF
 10   CONTINUE

      END
