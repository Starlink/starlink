      SUBROUTINE CCD1_NDFAC( NAME, ACCESS, MINVAL, MAXVAL, NNDF,
     :                       STACK, STATUS )
*+
*  Name:
*     CCD1_NDFAC

*  Purpose:
*     To access a group of NDFs using NDG.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NDFAC( NAME, ACCESS, MINVAL, MAXVAL, NNDF, STACK,
*                      STATUS )

*  Description:
*     The routine accesses a group of NDFs whose ADAM parameter is
*     given by 'NAME'. The NDF identifiers are written to the array
*     STACK. Between MAXVAL and MINVAL entries are made to the stack.
*     The actual number of NDFs is returned is NNDF.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The ADAM parameter name.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The access method used to get the NDFs. Should be 'READ' or
*        'UPDATE'.
*     MAXVAL = INTEGER (Given)
*        The maxiumum number of NDFs allowed.
*     MINVAL = INTEGER (Given)
*        The minimum number of NDFs allowed.
*     NNDF = INTEGER (Returned)
*        The number of NDF identifiers returned from user (less than
*        MAXNDF).
*     STACK( MAXNDF ) = INTEGER (Returned)
*        The stack of NDF identifiers returned from user.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - the routine uses the CCDPACK NDG continuation character '-'
*     to force reprompting. At present an internal maximum number of
*     NDFs is set, this is the value CCD1__MXNDF.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
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
*     19-MAR-1991 (PDRAPER):
*        Original version.
*     20-JUN-1991 (PDRAPER):
*        Changed to use IRG package, instead of repeated prompts.
*     9-JUL-1991 (PDRAPER):
*        Changed to use new IRG and IRH packages.
*     2-FEB-1994 (PDRAPER):
*        Added ACCESS parameter.
*     3-MAR-1997 (PDRAPER):
*        Removed all code dealing with top-level locators. HDS can now
*        perform this task for itself. This is part of the changes to
*        IRG to allow foreign data access.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'GRP_PAR'          ! GRP system constants
      INCLUDE 'PAR_ERR'          ! Parameter system error codes
      INCLUDE 'FIO_ERR'          ! FIO system error codes
      INCLUDE 'CCD1_PAR'         ! CCDPACK system constants
      INCLUDE 'NDG_ERR'          ! NDG system error codes

*  Arguments Given:
      CHARACTER NAME * ( * )
      CHARACTER ACCESS * ( * )
      INTEGER MAXVAL
      INTEGER MINVAL

*  Arguments Returned:
      INTEGER STACK( MAXVAL )
      INTEGER NNDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ADDED              ! Number of NDFs added this loop.
      INTEGER GID                ! NDG group identifier
      INTEGER INDEX              ! Current index into NDF group
      INTEGER NTRY               ! Number of attempts to get prompt right quit after 10, probably batch job in error.
      INTEGER ONNDF              ! Value of NNDF from previous iteration
      LOGICAL AGAIN              ! Controls looping for NDf names
      LOGICAL TERM               ! Returned true if a termination character is issued.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the NDF names through NDG. Set GID to no previous entries.
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
            CALL ERR_REP( 'CCD1_NDFAC',
     :      '  Unable to obtain valid list of NDF names using'//
     :      ' parameter %^NAME' , STATUS )
            GO TO 99
         END IF

*  Check that the number of NDFs returned is in the permitted range. If
*  the continuation character has been used then reprompt.
         AGAIN = .FALSE.

*  Check that maximum number has not been exceeded, if so reset the
*  group and reprompt.
         IF ( NNDF .GT. MAXVAL  .OR. NNDF .GT. CCD1__MXNDF ) THEN
             IF ( NNDF .GT. CCD1__MXNDF ) THEN
                CALL MSG_SETI( 'MAXVAL', CCD1__MXNDF )
             ELSE
                CALL MSG_SETI( 'MAXVAL', MAXVAL )
             END IF
             CALL MSG_OUT( ' ', 'You cannot supply more than ^MAXVAL '//
     :                     'NDF names - try again', STATUS )

*  Reset everything and try again.
             CALL CCD1_GRDEL( GID, STATUS )
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

         ELSE IF ( NNDF .LT. MINVAL ) THEN

*  No continuation character etc and still less enough NDFs, without
*  get out request. Issue warning and try again. Issue any error
*  messages and annull any errors first.
             CALL MSG_SETI( 'MINVAL', MINVAL )
             CALL MSG_OUT( ' ', 'You must supply at least ^MINVAL'//
     :                          ' NDF names - try again',
     :                     STATUS )

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

*  If we've got a valid list then get all the NDF identifiers. Store
*  them in the common block for later annulment (these are primary).
      IF ( STATUS .EQ. SAI__OK ) THEN
         DO 2 INDEX = 1, NNDF
            CALL NDG_NDFAS( GID, INDEX, ACCESS, STACK( INDEX ), STATUS )
 2       CONTINUE

*  Write out how many NDFs we've got.
         CALL MSG_SETC( 'NAME', NAME )
         CALL MSG_SETI( 'NDFAC_NNDF', NNDF )
         IF ( NNDF .GT. 1 ) THEN
            CALL MSG_OUT( ' ',
     :      '  ^NDFAC_NNDF NDFs accessed using parameter %^NAME',
     :      STATUS )
         ELSE
            CALL MSG_OUT( ' ',
     :      '  ^NDFAC_NNDF NDF accessed using parameter %^NAME',
     :      STATUS )
         END IF
      END IF

 99   CONTINUE

*  Release group resources.
      CALL CCD1_GRDEL( GID, STATUS )

      END
* $Id$
