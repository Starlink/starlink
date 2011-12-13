      SUBROUTINE FIO_ANNUL( FD, STATUS )
*+
*  Name:
*     FIO_ANNUL

*  Purpose:
*     Annul a file descriptor and close the file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_ANNUL( FD, STATUS )

*  Description:
*     This routine closes the file associated with the file descriptor
*     FD, resets the file descriptor and removes the association with
*     the ADAM parameter. It does not cancel the ADAM parameter though.
*     This allows the value of the ADAM parameter to be reused.

*  Arguments:
*     FD = INTEGER (Given)
*        The file descriptor
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*    -  If STATUS is not SAI__OK on input, then the routine
*       will still attempt to execute, but will return with STATUS set
*       to the import value.

*  Algorithm:
*     -  Check that there is a file parameter associated with the file
*        descriptor.
*     -  Close the file.
*     -  Reset the file parameter descriptor.

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
*     9-FEB-1993 (PMA):
*        Original version.
*     12-FEB-1993 (PMA):
*        If there is no file parameter descriptor associated with the
*        file descriptor, annul the error message reported by
*        FIO1_CHKFP.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! ADAM parameter system constants
      INCLUDE 'FIO_ERR'          ! FIO error constants
      INCLUDE 'FIOPAR_SYS'       ! FIO parameter system constants

*  Global Variables:
      INCLUDE 'FIOPA_CMN'        ! FIO parameter table
*        PFREE( FIO__MXPAR ) = LOGICAL (Write)
*           Whether slot used
*        PDESC( FIO__MXPAR ) = INTEGER (Write)
*           File descriptor for parameter
*        PTNAME( FIO__MXPAR ) = CHARACTER * ( PAR__SZNAM ) (Write)
*           Parameter names
*        PACMOD( FIO__MXPAR ) = CHARACTER * ( PAR__SZMOD ) (Write)
*           Parameter access modes

*  Arguments Given:
      INTEGER FD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER RFP                ! Relative file parameter
      INTEGER FP                 ! File parameter

*.

*  Begin a new error reporting environment so that this routine
*  executes even if status is bad on entry.
      CALL EMS_BEGIN( STATUS )

*  Check that there is an ADAM parameter associated with the file
*  descriptor.
      CALL FIO1_CHKFP( FD, FP, RFP, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Close the file.
         CALL FIO_CLOSE( FD, STATUS )

*  Reset the FIO ADAM parameter table entry
         PTNAME( RFP ) = ' '
         PDESC( RFP ) = 0
         PFREE( RFP ) = .TRUE.
         PACMOD( RFP ) = ' '

*  If there is no parameter descriptor associated with the file
*  descriptor, annul the error message.
      ELSE IF ( STATUS .EQ. FIO__FDNFP ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Return to the previous error reporting environment.
      CALL ERR_END( STATUS )

      END
