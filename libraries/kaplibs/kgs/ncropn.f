      SUBROUTINE NCROPN( PNAME1, PNAME2, PNAME3, PNAME4, COMMNT,
     :                   DEVOPN, PIC0, PIC1, ZONE1, STATUS )
*+
*  Name:
*     NCROPN

*  Purpose:
*     Acquires an SGS workstation and opens it for use with the
*     AUTOGRAPH package within NCAR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NCROPN( PNAME1, PNAME2, PNAME3, PNAME4, COMMNT, DEVOPN, PIC0,
*                  PIC1, ZONE1, STATUS )

*  Description:
*     This routine gets the name of an SGS workstation from the user
*     and opens it, using AGI to create a zone corresponding to the
*     current AGI picture.   If a null status is returned when accessing
*     the device, it is handled transparent, and a flag is returned to
*     indicate that the device was not opened.  This enables the calling
*     routine to permit optional graphics.
*
*     A new zone is created within this first zone of a size specified
*     by the user. This new zone is stored in the AGI database as the
*     'FRAME' picture for future plotting and is the current SGS zone
*     on exit from this routine. A call to SNX_AGWV is made before
*     returning to ensure that AUTOGRAPH uses the full zone for
*     plotting.
*
*     Before returning, the AGI picture which was current on entry to
*     this routine is reinstated as the current AGI picture.

*  Arguments:
*     PNAME1 = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to see if the
*        user wishes the display to be cleared.
*     PNAME2 = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be associated with
*        the graphics workstation.
*     PNAME3 = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the
*        required X extent of the FRAME picture in metres.
*     PNAME4 = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter which will be used to get the
*        required Y extent of the FRAME picture in metres.
*     COMMNT = CHARACTER * ( * ) (Given)
*        A comment to store in the AGI database with the FRAME picture.
*     DEVOPN = LOGICAL (Returned)
*        If true the device was opened (and will need closing); if false
*        this indicates that a null was supplied to the PNAME2 parameter
*        and the device was not opened.
*     PIC0 = INTEGER (Returned)
*        The picture identifier for the picture which was current on
*        entry.
*     PIC1 = INTEGER (Returned)
*        The picture identifier for the new FRAME picture.
*     ZONE1 = INTEGER (Returned)
*        The SGS zone identifier for the new FRAME picture.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine starts an AGI context.  Therefore, there
*     must be a call to AGS_DEASS to tidy and close down the database
*     and workstation.

*  Copyright:
*     Copyright (C) 1990, 1991, 1992 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-JUN-1990 (DSB):
*        Original version.
*     1991 July 31 (MJC):
*        No longer redefines colours of SGS pens to predefined state if
*        workstation has dynamic colour representation, now there is
*        palette control.
*     1991 August 21 (MJC):
*        Permit the device to be null so that optional plotting may
*        be enabled.  A flag (DEVOPN) is returned to indicate whether
*        the device is open or not.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants

*  Arguments Given:
      CHARACTER * ( * ) PNAME1
      CHARACTER * ( * ) PNAME2
      CHARACTER * ( * ) PNAME3
      CHARACTER * ( * ) PNAME4
      CHARACTER * ( * ) COMMNT

*  Arguments Returned:
      INTEGER  PIC0
      INTEGER  PIC1
      INTEGER  ZONE1
      LOGICAL DEVOPN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL  CLEAR             ! True to clear screen before plotting.
      INTEGER  TSTAT             ! Temporary status
      INTEGER  ZONE0             ! SGS zone associated with picture
                                 ! current on entry to this routine.
*.

*  Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the device-open flag here in case something goes
*  wrong obtaining the clear flag.

      DEVOPN = .FALSE.

*  See if user wants the display to be cleared before producing the
*  plot.

      CALL PAR_GTD0L( PNAME1, .TRUE., .TRUE., CLEAR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Start a new error context.

      CALL ERR_MARK

*  Get a device to plot on and open AGI.

      IF ( CLEAR ) THEN
         CALL AGS_ASSOC( PNAME2, 'WRITE', ' ', PIC0, ZONE0, STATUS )

      ELSE
         CALL AGS_ASSOC( PNAME2, 'UPDATE', ' ', PIC0, ZONE0, STATUS )

      END IF

*  Handle the null case transparently.

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE
         DEVOPN = .TRUE.

*  Create the frame picture.
*  =========================

         CALL KPG1_FRPIC( PNAME3, PNAME4, COMMNT, .FALSE., ZONE1, PIC1,
     :                    STATUS )

*  Release the original SGS zone.

         CALL SGS_RELZ( ZONE0, STATUS )

*  Make AUTOGRAPH use the whole of the current SGS zone for plotting.

         IF ( STATUS .EQ. SAI__OK ) CALL SNX_AGWV

*  Re-instate the original current picture.  Allow for a bad status
*  which would otherwise prevent this from happening.

         IF ( STATUS .NE. SAI__OK ) THEN
            TSTAT = STATUS
            STATUS = SAI__OK

            CALL AGI_SELP( PIC0, STATUS )
            IF ( STATUS .EQ. SAI__OK ) STATUS = TSTAT
         ELSE
            CALL AGI_SELP( PIC0, STATUS )
         END IF
      END IF

*  Release the error context.

      CALL ERR_RLSE

  999 CONTINUE

      END
