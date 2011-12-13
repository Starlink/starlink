      SUBROUTINE CCD1_NDFPR( NAME, NDFIN, CLIST, NDFOUT, STATUS )
*+
*  Name:
*     CCD1_NDFPR

*  Purpose:
*     To propagate an NDF using the NDG system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NDFPR( NAME, NDFIN, CLIST, NDFOUT, STATUS )

*  Description:
*     The routine creates a new NDF propagating the components as
*     specified in clist (see SUN/33 for description of clist). The
*     output NDF name is obtained through the NDG system. This is done
*     to keep a uniformity of interface, between all NDG commands specs
*     and prompts (i.e. can be enclosed in quotes, read from a file
*     etc.). The propagation actually occurs using the NDG_NDFPR
*     routine.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The ADAM parameter name.
*     NDFIN = INTEGER (Given)
*        The NDF identifier of the NDF to be propagated.
*     CLIST = CHARACTER * ( * ) (Given)
*        The component propagation list (see SUN/33).
*     NDFOUT = INTEGER (Given)
*        The identifier of the output NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1997, 2000 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUL-1991 (PDRAPER):
*        Original Version.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator code (no longer used as HDS closes
*        files correctly).
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_further_changes_here}
*
*     {note_new_bugs_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter system error codes
      INCLUDE 'FIO_ERR'          ! FIO system error codes
      INCLUDE 'CCD1_PAR'         ! CCDPACK system constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'NDG_ERR'          ! NDG system error codes

*  Arguments Given:
      CHARACTER NAME * ( * )
      CHARACTER CLIST * ( * )
      INTEGER NDFIN

*  Arguments Returned:
      INTEGER NDFOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER GID                ! Group identifier
      INTEGER ADDED              ! Number of NDFs added this loop.
      LOGICAL TERM               ! Returned true if a termination
                                 ! character is issued.
      LOGICAL AGAIN              ! Controls looping for NDf names
      INTEGER NTRY               ! Number of attempts to get prompt
                                 ! right quit after 10, probably batch
                                 ! job in error.
      INTEGER NNDF               ! Number of NDFs returned.
      INTEGER ONNDF              ! Number of NDFs at last iteration
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the NDF name through IRG. Set GID to no previous entries.
*  Set the termination character to '-' if this is added to any lines
*  of data it will be stripped and ignored.
      GID = GRP__NOID
      NNDF = 0
      NTRY = 0
 1    CONTINUE                   ! start of repeat until loop
         TERM = .FALSE.
         ONNDF = NNDF
         CALL NDG_CREAT( NAME, GRP__NOID, GID, NNDF, TERM, STATUS )
         ADDED = NNDF - ONNDF

*  Get out if a given a par_abort. Also quit after an unreasonble
*  number of attempts.
         IF ( STATUS .EQ. PAR__ABORT )THEN
            GO TO 99
         ELSE IF ( NTRY .GT. 10 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'CCD1_NDFPR',
     :      '  Unable to obtain valid list of NDF names using'//
     :      ' parameter %^NAME' , STATUS )
            GO TO 99
         END IF

*  Check that the number of NDFs returned is in the permitted range. If
*  the continuation character has been used ignore it IRG has stripped
*  it off.
         AGAIN = .FALSE.

*  One NDF only is allowed.
         IF ( NNDF .GT. 1 ) THEN
             CALL MSG_SETI( 'MAXVAL', 1 )
             CALL MSG_OUT( ' ', 'You cannot supply more than ^MAXVAL '//
     :                     'NDF names - try again', STATUS )

*  Reset everything and try again.
             CALL CCD1_GRDEL( GID, STATUS )
             NNDF = 0
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )
         ELSE IF ( TERM ) THEN

*  Continuation character has been issued ignore it.
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

*  only tested if the checks for number of NDFs etc. have been passed.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            AGAIN = .FALSE.
         END IF
      IF ( AGAIN ) GO TO 1

*  If we've got a valid list then get the NDF identifier.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDG_NDFPR( NDFIN, CLIST, GID, 1, NDFOUT, STATUS )
      END IF

 99   CONTINUE

*  Release group resources.
      CALL CCD1_GRDEL( GID, STATUS )

      END
* $Id$
