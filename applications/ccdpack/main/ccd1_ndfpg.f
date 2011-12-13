      SUBROUTINE CCD1_NDFPG( NAME, GIDIN, NVALS, GIDOUT, STATUS )
*+
*  Name:
*     CCD1_NDFPG

*  Purpose:
*     To create an output group of NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NDFPG( NAME, GIDIN, NVALS, GIDOUT, STATUS )

*  Description:
*     The routine calls the NDG package routine NDG_CREAT to create
*     a group of output NDF names. The group GIDIN is used as a
*     modification group on which to base the output names. I.e.
*     *_tmp means that all the output NDFs are to have the same names as
*     the input group except that there will be a trailing _TMP on
*     every name. The number of output NDF names is restricted to be
*     exactly those of the input group.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter to be used for accessing the
*        input data string.
*     GIDIN = INTEGER (Given)
*        The NDG group identifier of the NDFs to be used as modification
*        elements for the output group.
*     NVALS = INTEGER (Given)
*        The number of NDF names required for the output group (same as
*        the number in the input group).
*     GIDOUT = INTEGER (Returned)
*        Group identifier for the output list of NDF names.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     9-JUL-1991 (PDRAPER):
*        Original version.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parmeter system error codes
      INCLUDE 'FIO_ERR'          ! FIO system status values
      INCLUDE 'GRP_PAR'          ! Standard GRP system constants
      INCLUDE 'NDG_ERR'          ! NDG system error codes

*  Arguments Given:
      CHARACTER * ( * ) NAME
      INTEGER GIDIN
      INTEGER NVALS

*  Arguments Returned:
      INTEGER GIDOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NNDF               ! Number of output NDFs
      INTEGER ADDED              ! Number of NDFs added this loop.
      LOGICAL TERM               ! Returned true if a termination
                                 ! character is issued.
      LOGICAL AGAIN              ! Controls looping for NDf names
      INTEGER NTRY               ! Number of attempts to get prompt
                                 ! right quit after 10, probably batch
                                 ! job in error.
      INTEGER ONNDF              ! Number of NDFs at last iteration

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the names of the output NDFs.
      GIDOUT = GRP__NOID
      NNDF = 0
      NTRY = 0
 1    CONTINUE
         TERM = .FALSE.
         ONNDF = NNDF
         CALL NDG_CREAT( NAME, GIDIN, GIDOUT, NNDF, TERM, STATUS )
         ADDED = NNDF - ONNDF

*  Get out if a given a par_abort. Also quit after an unreasonble
*  number of attempts.
         IF ( STATUS .EQ. PAR__ABORT )THEN
            GO TO 99
         ELSE IF ( NTRY .GT. 10 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'CCD1_NDFPG',
     :      '  Unable to obtain valid list of NDF names using'//
     :      ' parameter %^NAME' , STATUS )
            GO TO 99
         END IF

*  Check that the number of NDFs returned is equal to the number
*  required. If the continuation character has been used then reprompt.
         AGAIN = .FALSE.

*  Check that maximum number has not been exceeded, if so reset the
*  group and reprompt.
         IF ( NNDF .GT. NVALS ) THEN
             CALL MSG_SETI( 'MAXVAL', NVALS )
             CALL MSG_OUT( ' ', 'You cannot supply more than ^MAXVAL '//
     :                     'NDF names - try again', STATUS )

*  Reset everything and try again.
             CALL CCD1_GRDEL( GIDOUT, STATUS )
             NNDF = 0
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )

*  If not enough NDFs have been given then ask for some more.
         ELSE IF ( NNDF .LT. NVALS ) THEN
             CALL MSG_SETI( 'MAXVAL', NVALS )
             CALL MSG_OUT( ' ', 'You must supply ^MAXVAL '//
     :                     'NDF names - try again', STATUS )

*  and try again.
             CALL CCD1_GRDEL( GIDOUT, STATUS )
             NNDF = 0
             AGAIN = .TRUE.
             NTRY = NTRY + 1
             CALL PAR_CANCL( NAME, STATUS )
         ELSE IF ( TERM ) THEN

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
            CALL CCD1_GRDEL( GIDOUT, STATUS )
            NNDF = 0
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( NAME, STATUS )

*  Try to trap a special case of ' ' string return. This should leave
*  added unmodified. This will be taken as a request to exit. The normal
*  stop entry request from an blank line will be `!' .
         ELSE IF ( ( ADDED .EQ. 0 ) .AND. ( .NOT. TERM ) ) THEN
            AGAIN = .FALSE.

*  If status is set to par__null then reset status, note that this is
*  only tested if the checks for number of NDFs etc. have been passed.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            AGAIN = .FALSE.
         END IF
      IF ( AGAIN ) GO TO 1

99    END
* $Id$
