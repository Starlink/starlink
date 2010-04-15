      SUBROUTINE ARY_BAD( IARY, CHECK, BAD, STATUS )
*+
*  Name:
*     ARY_BAD

*  Purpose:
*     Determine if an array may contain bad pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_BAD( IARY, CHECK, BAD, STATUS )

*  Description:
*     The routine returns a logical value indicating whether an array
*     may contain bad pixels for which checks must be made when its
*     values are processed. Only if the returned value is .FALSE. can
*     such checks be omitted.  If the CHECK argument to this routine is
*     set .TRUE., then it will perform an explicit check (if necessary)
*     to see whether bad pixels are actually present.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     CHECK = LOGICAL (Given)
*        Whether to perform an explicit check to see if bad pixels are
*        actually present.
*     BAD = LOGICAL (Returned)
*        Whether it is necessary to check for bad pixels when processing
*        the array's values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If CHECK is set .FALSE., then the returned value of BAD will
*     indicate whether bad pixels might be present and should therefore
*     be checked for during subsequent processing. However, even if BAD
*     is returned .TRUE. in such circumstances, it is still possible
*     that there may not actually be any bad pixels present (for
*     instance, in an array section, the region of the base array
*     accessed might happen to avoid all the bad pixels).
*     -  If CHECK is set .TRUE., then an explicit check will be made,
*     if necessary, to ensure that BAD is only returned .TRUE. if bad
*     pixels are actually present.
*     -  If the array is mapped for access through the identifier
*     supplied, then the value of BAD will refer to the actual mapped
*     values. It may differ from its original (unmapped) value if
*     conversion errors occurred during the mapping process, or if an
*     initialisation option of '/ZERO' was specified for an array which
*     was initially undefined, or if the mapped values have subsequently
*     been modified.
*     -  The BAD argument will always return a value of .TRUE. if the
*     array is in an undefined state.

*  Algorithm:
*     -  Import the array identifier.
*     -  Determine whether bad pixels are present.
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JUL-1989 (RFWS):
*        Original version.
*     20-OCT-1989 (RFWS):
*        Removed unnecessary call to ARY1_CHMOD.
*     21-NOV-1989 (RFWS):
*        Implemented the CHECK option.
*     7-MAR-1990 (RFWS):
*        Minor change to error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      INTEGER IARY
      LOGICAL CHECK

*  Arguments Returned:
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )

*  Determine whether bad pixels are present.
      CALL ARY1_BAD( IACB, CHECK, BAD, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_BAD_ERR',
     :   'ARY_BAD: Error determining if an array may contain bad ' //
     :   'pixels.', STATUS )
         CALL ARY1_TRACE( 'ARY_BAD', STATUS )
      END IF

      END
