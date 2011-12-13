      SUBROUTINE CCD1_NDFIN( NAME, GID, NNDF, STATUS )
*+
*  Name:
*     CCD1_NDFIN

*  Purpose:
*     To access a group of NDFs using NDG.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NDFIN( NAME, GID, NNDF, STATUS )

*  Description:
*     The routine accesses a group of NDFs whose ADAM parameter is
*     given by NAME. The NDF group identifier is returned in GID. No
*     limit is placed on the number of members of the group.  The
*     actual number of NDFs is returned as NNDF.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The ADAM parameter name.
*     GID = INTEGER (Returned)
*        NDG identifier for the group of NDF names.
*     NNDF = INTEGER (Returned)
*        The number of NDF identifiers returned from user.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - the GID identifier should be annulled (by calling CCD1_GRDEL)
*     after use of the group has finished.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     PDRAPER: Peter Draper (STARLINK)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUL-1991 (PDRAPER):
*        Original Version.
*     3-JUN-1992 (RFWS):
*        Renamed and added an access mode argument.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter system error codes
      INCLUDE 'FIO_ERR'          ! FIO system status codes
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'NDG_ERR'          ! NDG system error codes

*  Arguments Given:
      CHARACTER NAME * ( * )

*  Arguments Returned:
      INTEGER GID
      INTEGER NNDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ADDED              ! Number of NDFs added this loop.
      LOGICAL TERM               ! Returned true if a termination
                                 ! character is issued.
      LOGICAL AGAIN              ! Controls looping for NDF names
      INTEGER NTRY               ! Number of attempts to get prompt
                                 ! right quit after 10, probably batch
                                 ! job in error.
      INTEGER ONNDF              ! Number of NDFs at last iteration

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the NDF names through NDG. Set GID to no previous entries.
*  Set the termination character to '-' if this is added to any lines
*  of data then a continuation line is used.
      GID = GRP__NOID
      NNDF = 0
      NTRY = 0
 1    CONTINUE                   ! start of repeat until loop
         TERM = .FALSE.
         ONNDF = NNDF
         CALL NDG_ASSOC( NAME, .TRUE., GID, NNDF, TERM, STATUS )
         ADDED = NNDF - ONNDF

*  Get out if a given a par_abort. Also quit after an unreasonble
*  number of attempts.
         IF ( STATUS .EQ. PAR__ABORT )THEN
            GO TO 99
         ELSE IF ( NTRY .GT. 10 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'CCD1_NDFIN',
     :      '  Unable to obtain valid list of NDF names using'//
     :      ' parameter %^NAME' , STATUS )
            GO TO 99
         END IF

*  Check that the number of NDFs returned is in the permitted range. If
*  the continuation character has been used then reprompt.
         AGAIN = .FALSE.
         IF ( TERM ) THEN

*  Continuation character has been issued reprompt, appending to the
*  present group.
            CALL PAR_CANCL( NAME, STATUS )
            AGAIN = .TRUE.

*  Status may have been set by NDG for a good reason.. check for this
*  and reprompt.
         ELSE IF ( STATUS .EQ. NDG__NOFIL ) THEN

*  Issue the error.
             CALL ERR_FLUSH( STATUS )

*  Reset everything and try again.
             CALL CCD1_GRDEL( GID, STATUS )
             NNDF = 0
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )

*  Try to trap a special case of ' ' string return. This should leave
*  added unmodified. This will be taken as a request to exit. The normal
*  stop entry request from an blank line will be `!' .
         ELSE IF ( ( ADDED .EQ. 0 ) .AND. ( .NOT. TERM ) ) THEN
            AGAIN = .FALSE.

         ELSE IF ( NNDF .LT. 1 ) THEN

*  No continuation character etc and still less enough NDFs, without
*  get out request. Issue warning and try again. Issue any error
*  messages and annull any errors first.
             CALL MSG_OUT( ' ', 'You must supply at least one NDF -'//
     :                          ' try again', STATUS )

*  Reset everything and try again.
             CALL CCD1_GRDEL( GID, STATUS )
             NNDF = 0
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )

*  If status is set to par__null then reset status, note that this is
*  only tested if the checks for number of NDFs etc. have been passed.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            AGAIN = .FALSE.
         END IF
      IF ( AGAIN ) GO TO 1

*  Write out the actual number of NDFs accessed.
      CALL MSG_SETC( 'PARNAME', NAME )
      CALL MSG_SETI( 'NDFAC_NNDF', NNDF )
      IF ( NNDF .GT. 1 ) THEN
         CALL MSG_OUT( ' ',
     :   '  ^NDFAC_NNDF NDFs accessed using parameter %^PARNAME',
     :   STATUS )
      ELSE
         CALL MSG_OUT( ' ',
     :   '  ^NDFAC_NNDF NDF accessed using parameter %^PARNAME',
     :   STATUS )
      END IF

99    END
* $Id$
