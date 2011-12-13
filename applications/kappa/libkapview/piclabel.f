      SUBROUTINE PICLABEL( STATUS )
*+
*  Name:
*     PICLABEL

*  Purpose:
*     Labels the current graphics-database picture.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PICLABEL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application annotates the current graphics-database picture
*     of a specified device with a label you define.  This provides an
*     easy-to-remember handle for selecting pictures in subsequent
*     processing.

*  Usage:
*     piclabel label [device]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The graphics device. [Current graphics device]
*     LABEL = LITERAL (Read)
*        The label to be given to the current picture.  It is limited
*        to 15 characters, but may be in mixed case.  If it is null (!)
*        a blank label is inserted in the database.

*  Examples:
*     piclabel GALAXY
*        This makes the current picture of the current graphics device
*        have a label of "GALAXY".
*     piclabel A3 xwindows
*        This labels the current xwindows picture "A3".

*  Notes:
*     The label must be unique for the chosen device.  If the new label
*     clashes with an existing one, then the existing label is deleted.

*  Algorithm:
*     -  Get the label.  If it is null set the label to be blank.
*     -  Open the graphics device and start database activity.
*     -  Put the label into the database.
*     -  Annul the AGI device.

*  Related Applications:
*     KAPPA: PICDEF, PICLIST, PICSEL.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 Jan 12 (MJC):
*        Original.
*     1991 March 19 (MJC):
*        Converted to the SST prologue.
*     1991 April 9 (MJC):
*        Added AGI begin-and-end block.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'       ! Global SSE definitions
      INCLUDE  'DAT_PAR'       ! Data-system constants
      INCLUDE  'PAR_ERR'       ! Parameter-system errors

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER
     :  PICID                  ! AGI input picture identifier

      LOGICAL                  ! True if :
     :  DEVCAN                 ! Image-display parameter is to be
                               ! cancelled

      CHARACTER*( DAT__SZNAM )
     :  LABEL                  ! Picture label

*.

*    Check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Start a new error context.

      CALL ERR_MARK

*    Obtain the label to be given to the current picture.  It can be
*    mixed case.

      CALL PAR_GET0C( 'LABEL', LABEL, STATUS )

*    A null value indicates that the current picture's label is to be
*    erased.  This is not actually needed because the database permits
*    only unique names. It is present in case a user does type a null
*    and expects the label to be erased.

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         LABEL = ' '
      END IF

*    Release the new error context.

      CALL ERR_RLSE

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Start an AGI scope.

      CALL AGI_BEGIN

*    Open GKS workstation to update the database.

      CALL AGI_ASSOC( 'DEVICE', 'UPDATE', PICID, STATUS )

*    If the graphics device was not available, report the error and
*    leave the programme.

      IF ( STATUS .NE. SAI__OK ) THEN

         IF ( STATUS .NE. PAR__ABORT ) THEN
            CALL ERR_REP( 'ERR_PICLABEL_NID',
     :        'PICLABEL: Graphics device not available or not '/
     :        /'recognised.', STATUS )
         END IF
         DEVCAN = .TRUE.
         GOTO 999
      END IF

*    Store the label in the database for the current picture.

      CALL AGI_SLAB( PICID, LABEL, STATUS )

 999  CONTINUE

*    Close AGI workstation.

      IF ( DEVCAN ) THEN
         CALL AGI_CANCL( 'DEVICE', STATUS )
      ELSE
         CALL AGI_ANNUL( PICID, STATUS )
      END IF

*    End the AGI scope.

      CALL AGI_END( -1, STATUS )

      END
