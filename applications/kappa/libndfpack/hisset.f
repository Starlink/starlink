      SUBROUTINE HISSET( STATUS )
*+
*  Name:
*     HISSET

*  Purpose:
*     Sets the NDF history update mode.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL HISSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This task controls the level of history recording in an NDF,
*     and can also erase the history information.
*
*     The level is called the history update mode and it is a permanent
*     attribute of the HISTORY component of the NDF, and remains with
*     the NDF and any NDF created therefrom until the history is erased
*     or the update mode is modified (say by this task).

*  Usage:
*     hisset ndf [mode] ok=?

*  ADAM Parameters:
*     MODE = LITERAL (Read)
*        The history update mode.  It can take one of the following
*        values.
*
*           "Disabled"  ---  No history recording is to take place.
*           "Erase"     ---  Erases the history of the NDF.
*           "Normal"    ---  Normal history recording is required.
*           "Quiet"     ---  Only brief history information is to be
*                            recorded.
*           "Verbose"   ---  The fullest-possible history information
*                            is to be recorded.
*
*        The suggested default is "Normal".  ["Normal"]
*     NDF = (Read and Write)
*        The NDF whose history update mode to be modified or history
*        information erased.
*     OK = _LOGICAL (Read)
*        This is used to confirm whether or not the history should be
*        erased.  OK=TRUE lets the history records be erased; if
*        OK=FALSE the history is retained and a message will be issued
*        to this effect.

*  Examples:
*     hisset final
*        This sets the history-recording level to be normal for the NDF
*        called final.
*     hisset final erase ok
*        This erases the history information from the NDF called final.
*     hisset mode=disabled ndf=spectrum
*        This disables history recording in the NDF called spectrum.
*     hisset test42 v
*        This sets the history-recording level to be verbose for the NDF
*        called test42 so that the fullest-possible history is included.
*     hisset ndf=test42 mode=q
*        This sets the history-recording level to be quiet for the NDF
*        called test42, so that only brief information is recorded.

*  Notes:
*     -  A HISTORY component is created if it does not exist within the
*     NDF, except for MODE="Erase".
*     -  The task records the new history update mode within the
*     history records, even if MODE="Disabled" provided the mode has
*     changed.  Thus the history information will show where there may
*     be gaps in the recording.

*  Related Applications:
*     KAPPA: HISCOM, HISLIST, NDFTRACE.

*  Implementation Deficiencies:
*     There is no facility to erase history records before a given date
*     or number.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     RFWS: R.F.Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 June 3 (RFWS):
*        Original version.
*     1995 June 24 (MJC):
*        Added the documentation.  Corrected some typo's and placed
*        the local variables in alphabetical order.  Used PAR_CHOIC.
*        Corrected bug in NDF_HINFO call.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'MSG_PAR'          ! MSG_ public constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER INDF               ! NDF identifier
      INTEGER LP                 ! Length of prompt
      CHARACTER * ( NDF__SZHUM ) NEW ! New history update mode
      LOGICAL OK                 ! Confirm erasure?
      CHARACTER * ( NDF__SZHUM ) OLD ! Old history update mode
      CHARACTER * ( MSG__SZMSG ) PROMPT ! Confirmation prompt string
      CHARACTER * ( NDF__SZHUM ) TEMP ! Temporary history update mode
      CHARACTER * ( NDF__SZHIS ) TEXT( 1 ) ! History text
      LOGICAL THERE              ! HISTORY component present?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  Determine the update mode to be used.
      CALL PAR_CHOIC( 'MODE', 'Normal',
     :                'Verbose,Normal,Quiet,Disabled,Erase', .FALSE.,
     :                NEW, STATUS )

*  Check whether or not there is a HISTORY component present.
      CALL NDF_STATE( INDF, 'History', THERE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If 'ERASE' was specified, then generate a prompt containing the NDF
*  name to seek confirmation.
         IF ( NEW .EQ. 'ERASE' ) THEN
            IF ( THERE ) THEN
               CALL NDF_MSG( 'NDF', INDF )
               CALL MSG_LOAD( ' ', 'Erase history for ^NDF - OK?',
     :                        PROMPT, LP, STATUS )
               CALL PAR_PROMT( 'OK', PROMPT( : LP ), STATUS )

*  Obtain confirmation.
               CALL PAR_GET0L( 'OK', OK, STATUS )

*  If confirmation was given, then erase the HISTORY component.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( OK ) THEN
                     CALL NDF_RESET( INDF, 'History', STATUS )

*  Otherwise, conform non-erasure.
                  ELSE
                     CALL NDF_MSG( 'NDF', INDF )
                     CALL MSG_OUT( 'HISSET_NOERASE',
     :                             'History not erased for ^NDF',
     :                             STATUS )
                  END IF
               END IF
            END IF

*  If HISTORY component creation or modification is required, then
*  ensure that a HISTORY component exists.
         ELSE
            IF ( .NOT. THERE ) THEN
               CALL NDF_HCRE( INDF, STATUS )

*  If it already exists, then obtain the existing update mode.
            ELSE
               CALL NDF_HINFO( INDF, 'MODE', 0, OLD, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If a change of update mode is being performed, then select a
*  temporary update mode. For preference use the old one, but use the
*  new one if this is 'DISABLED'.
                  IF ( OLD .NE. NEW ) THEN
                     TEMP = OLD
                     IF ( TEMP .EQ. 'DISABLED' ) TEMP = NEW

*  Set this update mode and write history text.
                     CALL NDF_HSMOD( TEMP, INDF, STATUS )
                     CALL MSG_SETC( 'OLD', OLD )
                     CALL MSG_SETC( 'NEW', NEW )
                     TEXT( 1 ) =
     :                 'History update mode changed from ^OLD to ^NEW.'
                     CALL NDF_HPUT( TEMP, ' ', .FALSE., 1, TEXT,
     :                              .TRUE., .TRUE., .TRUE., INDF,
     :                              STATUS )

*  If there is no change in the recording mode, then note this in the
*  history (this will not actually appear if the mode is 'DISABLED').
                  ELSE
                     CALL MSG_SETC( 'MODE', NEW )
                     TEXT( 1 ) = 'History update mode not changed '/
     :                           /'(remains as ^MODE).'
                     CALL NDF_HPUT( 'QUIET', ' ', .FALSE., 1, TEXT,
     :                              .TRUE., .TRUE., .TRUE., INDF,
     :                              STATUS )
                  END IF

*  Write default history information (before the update mode is
*  changed).
                  CALL NDF_HDEF( INDF, ' ', STATUS )
               END IF
            END IF

*  Set the update mode actually required.
            CALL NDF_HSMOD( NEW, INDF, STATUS )
         END IF
      END IF

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'HISSET_ERR',
     :     'HISSET: Error setting the history recording mode of an '/
     :     /'NDF.', STATUS )
      END IF

      END
