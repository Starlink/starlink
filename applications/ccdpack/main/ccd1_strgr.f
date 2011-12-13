      SUBROUTINE CCD1_STRGR( NAME, INGRP, MINVAL, MAXVAL, OUTGRP,
     :                       NRET, STATUS )
*+
*  Name:
*     CCD1_STRGR

*  Purpose:
*     Gets a group of strings from a single prompt.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_STRGR( NAME, INGRP, MINVAL, MAXVAL, OUTGRP, NRET,
*                      STATUS )

*  Description:
*     The routine uses the GRP system to access a list of strings.
*     The returns may be based on previous group of names (so that
*     global substitions and modifications to the names can be made).
*     The number of returns is required to be in the range MINVAL to
*     MAXVAL.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The ADAM parameter name which is used to assess the input group
*        of names.
*     INGRP = INTEGER (Given)
*        The GRP group identifier for the input group on which
*        modifications etc. may be performed to form the output group.
*     MINVAL = INTEGER (Given)
*        The minimum number of values which can be returned.
*     MAXVAL = INTEGER (Given)
*        The maximum number of values that can be returned (may be
*        equal to MINVAL).
*     OUTGRP = INTEGER (Returned)
*        The GRP group identifier pointing to the strings.
*     NRET = INTEGER (Returned)
*        The actual number of returned values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - the routine uses the default GRP flag character '-' for
*     continuation to force reprompting.

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
*     9-NOV-1992 (PDRAPER):
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
      INCLUDE 'PAR_ERR'          ! Parameter system error codes

*  Arguments Given:
      CHARACTER NAME * ( * )
      INTEGER MAXVAL
      INTEGER MINVAL
      INTEGER INGRP

*  Arguments Returned:
      INTEGER NRET
      INTEGER OUTGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      EXTERNAL FIO_TEST
      LOGICAL FIO_TEST           ! Tests FIO returns for general
                                 ! conditions

*  Local Variables:
      INTEGER NTRY               ! Number of attempts to get string
      INTEGER ADDED              ! Number of strings added this iteration
      LOGICAL TERM               ! Set if the continuation character
                                 ! is set
      LOGICAL AGAIN              ! Controls the looping for new values

*.
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access a number of character strings using GRP. Reprompt on failure
*  caused by insufficient returns, too many returns etc. or when a
*  reprompt is forced.
      NTRY = 0

*  Create a new group to associate names with
      CALL GRP_NEW( 'CCDPACK:STRGS', OUTGRP, STATUS )
      NRET = 0
      ADDED = -1
 3    CONTINUE                      ! Return here on re-try.

*  Get the user return.
         TERM = .FALSE.
         CALL GRP_GROUP( NAME, INGRP, OUTGRP, NRET, ADDED, TERM,
     :                   STATUS )

*  Get out if a null return has been given or a PAR__ABORT. Also quit
*  after an unreasonble number of attempts.
         IF ( STATUS .EQ. PAR__ABORT .OR. STATUS .EQ. PAR__NULL )THEN
            GO TO 99
         ELSE IF ( NTRY .GT. 10 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', NAME )
            CALL ERR_REP( 'CCD1_STRGR1',
     :      '  Unable to obtain valid list of values using parameter'//
     :      ' %^NAME ' , STATUS )
            GO TO 99
         END IF

*  Check that the number of values returned. This has to be between the
*  the limits MINVAL and MAXVAL. If the continuation character has been
*  used then reprompt if appropriate. etc.
         AGAIN = .FALSE.
         IF ( NRET .GT. MAXVAL ) THEN

*  Comment on this futile exercise.
            CALL MSG_SETI( 'MAXVAL', MAXVAL )
            CALL MSG_OUT( ' ', 'Too many values given only need '//
     :                       '^MAXVAL - try again', STATUS )

*  Reset everything ready for next attempt.
            CALL CCD1_GRDEL( OUTGRP, STATUS )
            NRET = 0
            CALL GRP_NEW( 'CCDPACK:STRINGS', OUTGRP, STATUS )
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( NAME, STATUS )

         ELSE IF ( NRET .LT. MINVAL ) THEN

*  Comment on this and try again.
            CALL MSG_SETI( 'MINVAL', MINVAL )
            CALL MSG_OUT( ' ', 'Too few values given need ^MINVAL'//
     :      '- try again', STATUS )

*  Reset everything ready for next attempt.
            CALL CCD1_GRDEL( OUTGRP, STATUS )
            CALL GRP_NEW( 'CCDPACK:STRINGS', OUTGRP, STATUS )
            NRET = 0
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( NAME, STATUS )
         ELSE IF ( TERM ) THEN

*  Continuation character has been issued reprompt, appending to the
*  present group.
            AGAIN = .TRUE.
            CALL PAR_CANCL( NAME, STATUS )

*  Status may have been set by GRP for a good reason.. check for this
*  and reprompt. (Note FIO system errors are not captured by GRP).
         ELSE IF ( FIO_TEST( 'OPEN error', STATUS ) ) THEN

*  Issue the error.
            CALL ERR_FLUSH( STATUS )

*  Reset everything and try again.
            CALL CCD1_GRDEL( OUTGRP, STATUS )
            CALL GRP_NEW( 'CCDPACK:STRINGS', OUTGRP, STATUS )
            NRET = 0
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( NAME, STATUS )

*  Try to trap a special case of ' ' string return. This should leave
*  added unmodified. This will be taken as a request to exit. The normal
*  stop entry request from an blank line will be `!' .
         ELSE IF ( ( ADDED .EQ. -1 ) .AND. ( .NOT. TERM ) ) THEN
            AGAIN = .FALSE.

*  If status is set to par__null then reset status, note that this is
*  only tested if the checks for number of values etc. have been passed.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            AGAIN = .FALSE.
         END IF
      IF ( AGAIN ) GO TO 3

*  Exit.
 99   CONTINUE
      END
* $Id$
