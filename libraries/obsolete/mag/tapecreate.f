      SUBROUTINE TAPECREATE( STATUS )
*+
*  Name:
*     TAPECREATE

*  Purpose:
*     Create a Mag. Tape dataset.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TAPECREATE( [p]... )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Create the Mag. Tape dataset specified by the %TAPE parameter.

*  ADAM Parameters:
*     DRIVE     =MAG_TAPE(READ)
*        Name of the tape dataset to be created.
*     DEVICE    =_CHAR(READ)
*        device name of tape drive
*     DEVDATASET=UNIV(WRITE)
*        name of device dataset

*  Algorithm:
*     Get the location of the dataset, name of the tape drive and
*     name of tape drive as known to the HOST system.
*     Use MAG1_CRTDS to create data structure.

*  Copyright:
*     Copyright (C) 1982, 1983, 1984, 1990, 1993 Science & Engineering Research Council.
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
*     SLW: Sid Wright. (UCL)
*     AJC: Alan Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     22-APR-1982 (SLW):
*         Device dataset modifications.
*     16-JUL-1983 (SLW):
*         Starlink-ised version.
*     23-AUG-1984 (AJC):
*         Name changed from crtape.
*      5-SEP-1984 (AJC):
*         Added parameters to prolog.
*     28-FEB-1990 (AJC):
*         Create container file if it doesn't exist
*     03-MAR-1993 (AJC):
*         Change the call to MAG$_CRTDS to one to MAG1_CRTDS
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'         ! DAT HDS constants
      INCLUDE 'PAR_ERR'			! Par Errors
      INCLUDE 'MAG_SYS'			! Mag Constants

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER*(DAT__SZLOC) ELOC	! Locator to dataset storage
      CHARACTER*(DAT__SZNAM) TAPE	! Tape dataset name
      CHARACTER*(MAG__SZNAM) DEVICE	! Tape drive id.

*.


*    Allowed to execute ?
      IF (STATUS .NE. SAI__OK) RETURN

*
*     Get locator to device dataset container
*
      CALL DAT_EXIST ( 'DEVDATASET', 'UPDATE', ELOC, STATUS )

      IF ( ( STATUS .EQ. PAR__NULL ) .OR. ( STATUS .EQ. PAR__ABORT ) )
     :  THEN

         RETURN

      ELSE IF ( STATUS .NE. SAI__OK ) THEN

*       Does not exist so try and create it
         STATUS = SAI__OK
         CALL DAT_CREAT ( 'DEVDATASET', 'DEVICES', 0, 0, STATUS )
         CALL DAT_ASSOC ( 'DEVDATASET', 'UPDATE', ELOC, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            RETURN
         ENDIF
      ENDIF

*    Get name of tape drive
      CALL PAR_GET0C( 'DRIVE', TAPE, STATUS )
      IF ( (STATUS.EQ.PAR__NULL) .OR. (STATUS.EQ.PAR__ABORT) ) THEN
         RETURN
      ELSEIF (STATUS .NE. SAI__OK) THEN
         CALL MSG_SETC( 'DRIVE', TAPE )
         CALL ERR_REP( 'MTS_ERROR', '^DRIVE : ^STATUS', STATUS)
         RETURN
      ENDIF

*    Get name of tape drive as known to HOST SYSTEM
      CALL PAR_GET0C( 'DEVICE', DEVICE, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_SETC( 'DRIVE', DEVICE )
         CALL ERR_REP( 'MTS_ERROR', '^DRIVE : ^STATUS', STATUS)
         RETURN
      ENDIF

*    Create data structure
      CALL MAG1_CRTDS( ELOC, TAPE, DEVICE, STATUS )
      CALL DAT_ANNUL( ELOC, STATUS )
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP( 'MTS_ERROR', '^STATUS', STATUS )
      ENDIF

      END
