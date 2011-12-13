      SUBROUTINE CCD1_NDFGU( GID, NNDF, NAME, MINNDF, MAXNDF, STATUS )
*+
*  Name:
*     CCD1_NDFGU

*  Purpose:
*     To access a group of NDFs for UPDATE using NDG.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NDFGU( GID, NNDF, NAME, MAXNDF, MINNDF, STATUS )

*  Description:
*     The routine accesses a group of NDFs whose ADAM parameter is
*     given by 'NAME'. The NDF group identifier is returned in
*     GID. A minimum of MINNDF and up to MAXNDF entries are made to
*     the group. The actual number of NDFs is returned is NNDF.
*     The NDFs are set access for UPDATE.

*  Arguments:
*     GID = INTEGER (Returned)
*        NDG identifier for the group of NDF names.
*     NNDF = INTEGER (Returned)
*        The number of NDF identifiers returned from user (less than
*        MAXNDF)
*     NAME = CHARACTER * ( * ) (Given)
*        The ADAM parameter name.
*     MINNDF = INTEGER (Given)
*        The minimum number of NDFs allowed.
*     MAXNDF = INTEGER (Given)
*        The maximum number of NDFs allowed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - the GID identifier should be annulled (by calling CCD1_GRDEL)
*     after use of the group has finished.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-JUL-1992 (PDRAPER):
*        Changed to have a lower limit and access the group for
*        update. Based on CCD1_NDFGL.
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
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'NDG_ERR'          ! NDG system error codes

*  Arguments Given:
      INTEGER MINNDF
      INTEGER MAXNDF
      CHARACTER NAME * ( * )

*  Arguments Returned:
      INTEGER GID
      INTEGER NNDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL TERM               ! Returned true if a termination
                                 ! character is issued.
      LOGICAL AGAIN              ! Controls looping for NDf names
      INTEGER NTRY               ! Number of attempts to get prompt
                                 ! right quit after 10, probably batch
                                 ! job in error.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the NDF names through NDG. Set GID to no previous entries.
*  Set the termination character to '-' if this is added to any lines
*  of data then a continuation line is used.
      GID = GRP__NOID
      NTRY = 0
 1    CONTINUE                   ! start of repeat until loop
         CALL NDG_ASSOC( NAME, .TRUE., GID, NNDF, TERM, STATUS )

*  Get out if a null return has been given or a par_abort. Also quit
*  after an unreasonble number of attempts.
         IF ( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT )THEN
            GO TO 99
         ELSE IF ( NTRY .GT. 10 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'CCD1_NDFGL',
     :      '  Unable to obtain valid list of NDF names using'//
     :      ' parameter %^NAME' , STATUS )
            GO TO 99
         END IF

*  Check that the number of NDFs returned it has to be greater than zero
*  and less than MAXNDF + 1. If the continuation character has been used
*  then reprompt.
         AGAIN = .FALSE.

*  Check that maximum number has not been exceeded, if so reset the
*  group and reprompt.
         IF ( NNDF .GT. MAXNDF ) THEN
             CALL MSG_SETI( 'MAXNDF', MAXNDF )
             CALL MSG_OUT( ' ', 'You cannot supply more than ^MAXNDF '//
     :                     'NDF names - try again', STATUS )

*  Reset everything and try again.
             CALL CCD1_GRDEL( GID, STATUS )
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )
         ELSE IF ( NNDF .LT. MINNDF ) THEN
             CALL MSG_SETI( 'MINNDF', MINNDF )
             CALL MSG_OUT( ' ', 'You must supply at least ^MINNDF '//
     :                     'NDF names - try again', STATUS )

*  Reset everything and try again.
             CALL CCD1_GRDEL( GID, STATUS )
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )
         ELSE IF ( TERM ) THEN

*  Continuation character has been issued reprompt, appending to the
*  present group.
            CALL PAR_CANCL( NAME, STATUS )
            AGAIN = .TRUE.
         ELSE IF ( NNDF .LT. 1 ) THEN

*  No continuation character and less than one NDF, without get out
*  request. Issue warning and try again.
             CALL MSG_OUT( ' ', 'You must supply at least one NDF',
     :                     STATUS )
             AGAIN = .TRUE.

*  Status may have been set by NDG for a good reason.. check for this
*  and reprompt.
         ELSE IF ( STATUS .EQ. NDG__NOFIL ) THEN
            CALL PAR_CANCL( NAME, STATUS )
            AGAIN =.TRUE.

*  Reset everything and try again.
            CALL CCD1_GRDEL( GID, STATUS )
            NTRY = NTRY + 1
            CALL PAR_CANCL( NAME, STATUS )
         END IF
      IF ( AGAIN ) GO TO 1

*  Write out how many NDFs we've got.
      CALL MSG_SETI( 'NDFAC_NNDF', NNDF )
      CALL MSG_SETC( 'PARNAME', NAME )
      IF ( NNDF .GT. 1 ) THEN
         CALL MSG_OUT( ' ',
     :   '  ^NDFAC_NNDF input NDFs accessed using parameter %^PARNAME',
     :    STATUS )
      ELSE
         CALL MSG_OUT( ' ',
     :   '  ^NDFAC_NNDF NDF accessed using parameter %^PARNAME',
     :   STATUS )
      END IF

 99   END
* $Id$
