      SUBROUTINE CCD1_NDFMI( PARAM, KEYGRP, GOTGRP, STATUS )
*+
*  Name:
*     CCD1_NDFMI

*  Purpose:
*     Get NDFs matching Set Index attributes from a parameter.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_NDFMI( PARAM, KEYGRP, GOTGRP, STATUS )

*  Description:
*     This routine gets a group of NDFs from an ADAM parameter with
*     reference to a group of Set Index keys stored in KEYGRP.
*     The value GOTGRP is returned as a GRP identifier for an NDG
*     group of NDFs; the Nth member of GOTGRP will have a Set Index
*     attribute value which matches the value of the Nth member of
*     KEYGRP.  The members of KEYGRP must therefore be character
*     representations of integers.  If the user does not provide
*     a set of NDFs which allows such a GOTGRP to be constructed,
*     he is re-prompted until he does.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter via which to obtain the NDFs.
*     KEYGRP = INTEGER (Given)
*        The GRP identifier for a group containing string
*        representations of integers.  For each one, the returned group
*        GOTGRP will contain an NDF in the corresponding position which
*        has that Set Index attribute value.
*     GOTGRP = INTEGER (Returned)
*        An NDG identifier for a group of NDFs obtained as described
*        above.  In the event of an error, the value GRP__NOID will
*        be returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-FEB-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'PAR_ERR'          ! PAR system error codes
      INCLUDE 'NDG_ERR'          ! NDG system error codes
      INCLUDE 'CCD1_PAR'         ! Private CCDPACK constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER KEYGRP

*  Arguments Returned:
      INTEGER GOTGRP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ADDED              ! Number of NDFs added this iteration
      INTEGER I                  ! Loop index
      INTEGER IGOT               ! Index of matched group
      INTEGER INGRP              ! NDG identifier of group got from parameter
      INTEGER NGOT               ! Number of NDFs in output group
      INTEGER NKEY               ! Number of members in KEYGRP
      INTEGER NNDF               ! Number of members of INGRP
      INTEGER NSUB               ! Number of subgroups
      INTEGER NTRY               ! Number of attempts to get input
      INTEGER ONNDF              ! NNDF from last iteration
      INTEGER SIZ                ! Size of subgroup
      INTEGER SUBGRP( CCD1__MXNDF ) ! Subgroups split by Set Index
      INTEGER SUBKEY             ! GRP identifier for keys of subgroups
      LOGICAL AGAIN              ! Continue with another iteration?
      LOGICAL TERM               ! Termination character added to input
      CHARACTER * ( GRP__SZNAM ) CINDEX ! Index string value
      CHARACTER * ( GRP__SZNAM ) FIELDS( 6 ) ! NDG supplementary information
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of an NDF

*.

*  Set up error condition return values.
      GOTGRP = GRP__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the size of the input (and hence output) group.
      CALL GRP_GRPSZ( KEYGRP, NKEY, STATUS )

*  Access the NDF names through NDG.
      NNDF = 0
      NTRY = 0
      INGRP = GRP__NOID
 1    CONTINUE
         TERM = .FALSE.
         ONNDF = NNDF
         CALL NDG_ASSOC( PARAM, .TRUE., INGRP, NNDF, TERM, STATUS )
         ADDED = NNDF - ONNDF

*  Get out if given a PAR__ABORT.  Also quite after an unreasonable
*  number of attempts.
         IF ( STATUS .EQ. PAR__ABORT ) THEN
            GO TO 99
         ELSE IF ( NTRY .GT. 10 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'PARAM', PARAM )
            CALL ERR_REP( 'CCD1_NDFMI',
     :      '  Unable to obtain valid list of NDF names using' //
     :      ' parameter ^PARAM', STATUS )
            GO TO 99
         END IF
         AGAIN = .FALSE.

*  Check that the maximum number has not been exceeded, if so reset
*  the group and reprompt.
         IF ( NNDF .GT. CCD1__MXNDF ) THEN
            CALL MSG_SETI( 'MAXVAL', CCD1__MXNDF )
            CALL MSG_OUT( ' ', 'You cannot supply more than ^MAXVAL '//
     :                    'NDF names - try again', STATUS )

*  Reset everything and try again.
            CALL CCD1_GRDEL( INGRP, STATUS )
            NNDF = 0
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( PARAM, STATUS )
         ELSE IF ( TERM ) THEN

*  Continuation character has been issued; reprompt, appending to the
*  current group.
            CALL PAR_CANCL( PARAM, STATUS )
            AGAIN = .TRUE.

*  Status may have been set by NDG for a good reason.  Check for this
*  and reprompt.
         ELSE IF ( STATUS .EQ. NDG__NOFIL ) THEN

*  Issue the error.
            CALL ERR_FLUSH( STATUS )

*  Reset everything and try again.
            CALL CCD1_GRDEL( INGRP, STATUS )
            NNDF = 0
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( PARAM, STATUS )

*  Try to trap a special case of ' ' string return.  This should leave
*  ADDED unmodified.  This will be taken as a request to exit.  The
*  normal stop entry request from a blank line will be '!'.
         ELSE IF ( ( ADDED .EQ. 0 ) .AND. ( .NOT. TERM ) ) THEN
            AGAIN = .FALSE.

*  If a null response is given then reset status; note that this is
*  only tested if the checks for number of NDFs etc. have been passed.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            AGAIN = .FALSE.
         END IF

*  If the user has indicated that there is no more input, assess this
*  group to see whether it has the correct Set attributes.
         IF ( .NOT. AGAIN .AND. STATUS .EQ. SAI__OK ) THEN

*  Split the new group of NDFs into subgroups by Set Index.
            CALL CCD1_SETSP( INGRP, 'INDEX', CCD1__MXNDF, SUBGRP, NSUB,
     :                       SUBKEY, STATUS )

*  For each key in the input group, try to find a single corresponding
*  one from the group we've just got from the user, and copy it to
*  the right place in a new group.
            DO I = 1, NKEY

*  Get the Index value for this group member.
               CALL GRP_GET( KEYGRP, I, 1, CINDEX, STATUS )

*  Try to find a subgroup corresponding to that Index value.
               CALL GRP_INDEX( CINDEX, SUBKEY, 1, IGOT, STATUS )

*  If no subgroup has that Index value it's no good; try again.
               IF ( IGOT .LE. 0 ) THEN
                  CALL MSG_SETC( 'IND', CINDEX )
                  CALL MSG_OUT( ' ', 'No NDF supplied with Set Index '//
     :            'of ^IND - try again', STATUS )
                  CALL CCD1_GRDEL( INGRP, STATUS )
                  CALL CCD1_GRDEL( GOTGRP, STATUS )
                  NNDF = 0
                  AGAIN = .TRUE.
                  NTRY = NTRY + 1
                  CALL PAR_CANCL( PARAM, STATUS )
                  GO TO 2
               ELSE

*  If the corresponding subgroup has more than one entry it's no good;
*  try again.
                  CALL GRP_GRPSZ( SUBGRP( IGOT ), SIZ, STATUS )
                  IF ( SIZ .GT. 1 ) THEN
                     CALL MSG_SETC( 'IND', CINDEX )
                     CALL MSG_OUT( ' ', 'Multiple NDFs supplied with '//
     :               'Set Index of ^IND - try again', STATUS )
                     CALL CCD1_GRDEL( INGRP, STATUS )
                     CALL CCD1_GRDEL( GOTGRP, STATUS )
                     NNDF = 0
                     AGAIN = .TRUE.
                     NTRY = NTRY + 1
                     CALL PAR_CANCL( PARAM, STATUS )
                     GO TO 2

*  This subgroup is suitable.  If it's the first in the list, create
*  a new output group using GRP_COPY to save the values of the
*  various GRP flags, but for subsequent groups just add names
*  using GRP_PUT.
                  ELSE
                     IF ( I .EQ. 1 ) THEN
                        CALL GRP_COPY( SUBGRP( IGOT ), 1, 1, .FALSE.,
     :                                 GOTGRP, STATUS )
                     ELSE
                        CALL GRP_GET( SUBGRP( IGOT ), 1, 1, NDFNAM,
     :                                STATUS )
                        CALL GRP_PUT( GOTGRP, 1, NDFNAM, I, STATUS )
                     END IF

*  Copy NDG supplementary information too.
                     CALL NDG_GTSUP( SUBGRP( IGOT ), 1, FIELDS, STATUS )
                     CALL NDG_PTSUP( GOTGRP, I, FIELDS, STATUS )
                  END IF
               END IF
            END DO
 2          CONTINUE

*  Deallocate group resources associated with creating the subgroups.
            DO I = 1, NSUB
               CALL CCD1_GRDEL( SUBGRP( I ), STATUS )
            END DO
            CALL CCD1_GRDEL( SUBKEY, STATUS )
         END IF

*  If we don't yet have a complete group as required, keep trying.
      IF ( AGAIN ) GO TO 1

*  Success.  Tell the user how many NDFs we've got.
      CALL GRP_GRPSZ( GOTGRP, NGOT, STATUS )
      CALL MSG_SETC( 'PARAM', PARAM )
      CALL MSG_SETI( 'NGOT', NGOT )
      IF ( NGOT .GT. 1 ) THEN
         CALL MSG_OUT( ' ',
     :   '  ^NGOT NDFs accessed using parameter %^PARAM', STATUS )
      ELSE
         CALL MSG_OUT( ' ',
     :   '  ^NGOT NDF accessed using parameter %^PARAM', STATUS )
      END IF

*  Error exit label.
 99   CONTINUE
      CALL CCD1_GRDEL( INGRP, STATUS )

      END
* $Id$
