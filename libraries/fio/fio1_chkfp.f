      SUBROUTINE FIO1_CHKFP( FD, FP, RFP, STATUS )
*+
*  Name:
*     FIO1_CHKFP

*  Purpose:
*     Check that there is a file parameter associated with a file
*     descriptor

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO1_CHKFP( FD, FP, RFP, STATUS )

*  Description:
*     Given an FIO file descriptor, check that there is an associated
*     file parameter and return the associated file parameter descriptor
*     and relative file parameter descriptor.

*  Arguments:
*     FD = INTEGER (Given)
*        The file descriptor
*     FP = INTEGER (Returned)
*        The file parameter descriptor
*     RFP = INTEGER (Returned)
*        The relative file parameter descriptor
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     8-FEB-1993 (PMA):
*        Original version.
*     16-FEB-1993 (PMA):
*        Add a check on PFREE to see if a file parameter descriptor
*        is in use.
*     18-FEB-1993 (PMA):
*        Add an include of PAR_PAR.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! Parameter system constants
      INCLUDE 'FIO_ERR'          ! FIO error constants
      INCLUDE 'FIOPAR_SYS'       ! FIO parameter system constants

*  Global Variables:
      INCLUDE 'FIOPA_CMN'        ! FIO parameter table
*        PDESC( FIO__MXPAR ) = INTEGER (Read)
*           File descriptor for parameter

*  Arguments Given:
      INTEGER FD

*  Arguments Returned:
      INTEGER FP
      INTEGER RFP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to find FD in the list of file descriptors associated with file
*  parameters.
      DO I = 1, FIO__MXPAR
         IF ( ( .NOT. PFREE( I ) ) .AND. ( PDESC( I ) .EQ. FD ) ) THEN
            RFP = I
            GOTO 999
         END IF
      END DO

*  Could not find a match. Set STATUS and report an error.
      RFP = 0
      STATUS = FIO__FDNFP
      CALL EMS_REP( 'FIO_ANNUL_NOPAR',
     :   'The file descriptor is not associated with a file parameter',
     :   STATUS )

  999 CONTINUE

      END
