      SUBROUTINE FIO_CANCL( PNAME, STATUS )
*+
*  Name:
*     FIO_CANCL

*  Purpose:
*     Close a file and cancel the parameter

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_CANCL( PNAME, STATUS )

*  Description:
*     Close any open file that is associated with the parameter and
*     cancel the parameter.

*  Arguments:
*     PNAME = CHARACTER * ( * ) (Given)
*        Expression giving the name of a file parameter which has
*        previously been associated with a file using FIO_ASSOC.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*    -  If STATUS is not SAI__OK on input, then the routine
*       will still attempt to execute, but will return with STATUS set
*       to the import value.

*  Algorithm:
*     The file descriptor associated with this parameter is found using
*     FIO1_FNDFP, then the device is deassigned using FIO_CLOSE and the
*     parameter is cancelled using PAR_CANCL.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     AJC: A Chipperfield  (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     29-Feb-1988 (AJC):
*        Original.
*     28-Aug-1990 (AJC):
*        Cancel everything regardless of errors.
*     12-FEB-1992 (PMA):
*        Converted to new style prologue.
*        Removed the include file for the common block FIO_GO as it was
*        never referenced.
*     23-FEB-1992 (PMA):
*        Add INCLUDE 'PAR_PAR' for use on Unix systems.
*     12-MAR-1992 (PMA):
*        Tidy the layout of the code by adding some blank lines.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     1-JUL-1992 (PMA):
*        Replace saving the value of STATUS on entry and restoring it
*        on exit with EMS_BEGIN and EMS_END.
*     2-JUL-1992 (PMA):
*        Change the calls to EMS to calls to ERR.
*    27-JUL-1992 (PMA):
*        Check that the parameter is not in the annulled state before
*        closing the file. Use direct calls to SUBPAR to cancel the
*        parameter. Clear the value of PACMOD.
*    29-JUL-1992 (PMA):
*        Check that the file descriptor is valid before passing it on to
*        FIO_CLOSE. This will prevent a spurious error message in the
*        case that we have a valid parameter not associated with an open
*        file. This certainly happens when a parameter is in the
*        annulled state.
*        Replace the calls to SUBPAR with calls to PAR.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     13-MAY-1993 (PMA):
*        Move the call to PAR_CANCL to after the IF statement.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! ADAM parameter system constants
      INCLUDE 'FIOPAR_SYS'       ! FIO internal constants

*  Global Variables:
      INCLUDE 'FIOPA_CMN'        ! FIO parameter table
*        PFREE( FIO__MXPAR ) = LOGICAL (Write)
*           Whether slot used
*        PDESC( FIO__MXPAR ) = INTEGER (Read and Write)
*           File descriptor for parameter
*        PTNAME( FIO__MXPAR ) = CHARACTER * ( PAR__SZNAM ) (Write)
*           Parameter names
*        PACMOD( FIO__MXPAR ) = CHARACTER * ( PAR__SZMOD ) (Write)
*           Parameter access modes

*  Arguments Given:
      CHARACTER * ( * ) PNAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FP                 ! File parameter descriptor
      INTEGER RFP                ! Relative file parameter descriptor
      INTEGER FD                 ! File descriptor
      INTEGER RFD                ! Relative file descriptor

*.

*  Begin a new error reporting environment so that this routine
*  executes even if status is bad on entry.
      CALL ERR_BEGIN( STATUS )

*  Check that this is a valid file parameter
      CALL FIO1_FNDFP( PNAME, FP, RFP, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the parameter has a valid file descriptor.
         FD = PDESC( RFP )
         CALL FIO1_CHKFD( FD, RFD, STATUS )

*  If the file descriptor is valid, close the file.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL FIO_CLOSE( FD, STATUS )

*  Otherwise, just annul the error. There is not an open file associated
*  with the parameter, so we do not need to close it.
         ELSE
            CALL ERR_ANNUL( STATUS )
         END IF

*  Reset the FIO parameter table entry
         PTNAME( RFP ) = ' '
         PDESC( RFP ) = 0
         PFREE( RFP ) = .TRUE.
         PACMOD( RFP ) = ' '
      END IF

*  Cancel the parameter. This is done outside the IF statement since
*  PNAME may be a valid ADAM program parameter, but not have an FIO
*  file parameter descriptor associated with it. This will be the case
*  if the user responded with a null value when prompted by FIO_ASSOC.
      CALL PAR_CANCL( PNAME, STATUS )

*  Return to the previous error reporting environment.
      CALL ERR_END( STATUS )

      END
