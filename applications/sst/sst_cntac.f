      SUBROUTINE SST_CNTAC( STR, NCH )
*+
*  Name:
*     SST_CNTAC

*  Purpose:
*     Count the number of alphanumeric characters in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_CNTAC( STR, NCH )

*  Description:
*     The routine returns an integer giving the number of alphanumeric
*     characters in the string supplied.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String whose characters are to be counted.
*     NCH = INTEGER (Returned)
*        Number of alphanumeric characters.

*  Notes:
*     -  Underscore '_' is counted as an alphanumeric character by this
*     routine.

*  Algorithm:
*     -  Loop, looking at each character and counting it if it is
*     alphanumeric.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-AUG-1989 (RFWS):
*        Original version.
*     3-AUG-1990 (RFWS):
*        Changed routine name.
*     8-AUG-1990 (RFWS):
*        Changed to call CHR_ISALM to identify alphanumeric characters.
*     9-AUG-1990 (RFWS):
*        Removed use of CHR_LEN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR

*  Arguments Returned:
      INTEGER NCH

*  External References:
      LOGICAL CHR_ISALM          ! Is character alphanumeric?

*  Local Variables:
      INTEGER I                  ! Loop counter for characters

*.

*  Initialise, then test each character for being alphanumeric.
      NCH = 0
      DO 1 I = 1, LEN( STR )
         IF ( CHR_ISALM( STR( I : I ) ) ) THEN

*  Count those which are.
            NCH = NCH + 1
         END IF
1     CONTINUE

      END
* @(#)sst_cntac.f   1.1   94/12/05 11:31:23   96/07/05 10:27:26
