      SUBROUTINE NDF1_CMPFL( NAME1, NAME2, SAME, STATUS )
*+
*  Name:
*     NDF1_CMPFL

*  Purpose:
*     Compare two file names for equality.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CMPFL( NAME1, NAME2, SAME, STATUS )

*  Description:
*     The routine compares two file names (or file name fields) for
*     equality, taking account of whether the host file system uses
*     case sensitive file names.

*  Arguments:
*     NAME1 = CHARACTER * ( * ) (Given)
*        The first file name (or file name field) to be compared.
*     NAME2 = CHARACTER * ( * ) (Given)
*        The second file name (or file name field) to be compared.
*     SAME = LOGICAL (Returned)
*        Whether the two file names are the same.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine does not perform any file name expansion and does
*     not compare the files themselves. For instance, it is possible
*     for two file names to be judged unequal by this routine, but for
*     them to refer to the same file.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_FNFMT = INTEGER (Read)
*           Code identifying the file name format of the host file
*           system.

*  Arguments Given:
      CHARACTER * ( * ) NAME1
      CHARACTER * ( * ) NAME2

*  Arguments Returned:
      LOGICAL SAME

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      LOGICAL INIT               ! Initialisation performed?

      SAVE INIT                  ! Remember if initialised

*  Local Data:
      DATA INIT / .FALSE. /      ! Start out un-initialised

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If initialisation has not yet been performed, then ensure that the
*  TCB is initialised. Note if initialisation succeeds.
      IF ( .NOT. INIT ) THEN
         CALL NDF1_INTCB( STATUS )
         INIT = ( STATUS .EQ. SAI__OK )
      END IF

*  If OK, then compare the two file names for equality. Use case
*  sensitivity if necessary, as indicated by the TCB file name format
*  code.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  VMS file names are case insensitive.
         IF ( TCB_FNFMT .EQ. NDF__VMS ) THEN
            SAME = CHR_SIMLR( NAME1, NAME2 )

*  POSIX file names are case sensitive.
         ELSE IF ( TCB_FNFMT .EQ. NDF__POSIX ) THEN
            SAME = ( NAME1 .EQ. NAME2 )

*  If the TCB file name format code is not recognised, then report an
*  error.
         ELSE
            STATUS = NDF__FATIN
            CALL MSG_SETI( 'FNFMT', TCB_FNFMT )
            CALL ERR_REP( 'NDF1_CMPFL_CODE',
     : 'Invalid file name format code (value = ^FNFMT) encountered ' //
     : 'in the NDF_ system Tuning Control Block (internal ' //
     : 'programming error).',
     :                    STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CMPFL', STATUS )

      END
