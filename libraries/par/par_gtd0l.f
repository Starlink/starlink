      SUBROUTINE PAR_GTD0L( PARAM, DEFAUL, NULL, VALUE, STATUS )

*+
*  Name:
*     PAR_GTD0L

*  Purpose:
*     Obtains a logical value from a parameter with a dynamic
*     default.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PAR_GTD0L( PARAM, DEFAUL, NULL, VALUE, STATUS )

*  Description:
*     This routine obtains a scalar logical value from a parameter.
*     A dynamic default is defined.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter.
*     DEFAUL = LOGICAL (Given)
*        The suggested default value for the parameter.
*     NULL = LOGICAL (Given)
*        NULL controls the behaviour of this routine when the parameter
*        is in the null state.  If NULL is .FALSE., this routine
*        returns with STATUS=PAR__NULL.  If NULL is .TRUE., the
*        returned VALUE takes the value of DEFAUL and, if the MSG filtering
*        level (see SUN/104) is 'verbose', a message informs the user of the
*        value used for the parameter. The routine then returns with
*        STATUS=SAI__OK.  This feature is intended for
*        convenient handling of null values.  NULL should only be set
*        to .TRUE. when the value of DEFAUL will always give a
*        reasonable value for the parameter.
*     VALUE  = LOGICAL (Returned)
*        The value associated with the parameter.  It will only be
*        valid if STATUS is not set to an error value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Suggest the default value to the data system.
*     -  Loop until an acceptable value is obtained or an error occurs.
*     Get a value from the parameter system.
*     -  If a bad status is returned from the parameter-system get, set
*     the returned value to the suggested default.  When the bad status
*     is PAR__NULL, annul the error and output a message.  The loop is
*     exited.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     MJC: Malcolm J. Currie  (STARLINK)
*     AJC: Alan J. Chipperfield   (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 January 7 (MJC):
*        Original based upon AIF_GTDLG.
*     1992 November 18 (MJC):
*        Simplified the function and clarified the description of
*        parameter NULL.
*     1999 September 16 (AJC):
*        Warn in MSG__VERB mode if NULL operates to adopt default.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! Switch off default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Environment constants
      INCLUDE 'PAR_ERR'        ! Parameter-system error constants
      INCLUDE 'MSG_PAR'        ! Message-system constants

*  Arguments Given:
      CHARACTER * ( * )
     :  PARAM                  ! Parameter name associated with value
                               ! to be obtained

      LOGICAL
     :  DEFAUL                 ! Suggested default value for value to be
                               ! obtained

      LOGICAL                  ! True if:
     :  NULL                   ! Default value used when bad status is
                               ! returned by the parameter get.

*  Arguments Returned:
      LOGICAL
     :  VALUE                  ! Value only valid if STATUS does not
                               ! have an error value

*  Status:
      INTEGER STATUS           ! Global status

*  Local Variables:
      LOGICAL                  ! True if:
     :  NOTOK                  ! No acceptable value obtained

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Suggest the given default to the parameter system as a
*  dynamic default.
      CALL PAR_DEF0L( PARAM, DEFAUL, STATUS )

*  Start a new error context.
      CALL ERR_MARK

*  Loop to obtain the value of the parameter.
*  ==========================================

*  Initialise NOTOK to start off the loop.
      NOTOK = .TRUE.

  100 CONTINUE

*  The loop will keep going as long as a suitable value has not be
*  obtained and there is no error.
         IF ( .NOT. NOTOK .OR. ( STATUS .NE. SAI__OK ) ) GOTO 120

*  Get a value for the parameter.
         CALL PAR_GET0L( PARAM, VALUE, STATUS )

*  Check for an error.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  The value must be satisfactory so terminate the loop.
            NOTOK = .FALSE.

*  Use the default value following an error and when the null flag is
*  set.

*  Use the default value following an error.
         ELSE

*  Annul a null error to prevent an error report about null appearing.
*  Create a message informing the user of what has happened.
            IF ( STATUS .EQ. PAR__NULL .AND. NULL ) THEN
               CALL ERR_ANNUL( STATUS )

*  Inform the user what has happened.
               CALL MSG_SETL( 'DEFAULT', DEFAUL )
               CALL MSG_SETC( 'PARAM', PARAM )
               CALL MSG_OUTIF( MSG__VERB, 'PAR_GTD0L_DEFA',
     :           'A value of ^DEFAULT has been adopted '/
     :           /'for parameter ^PARAM.', STATUS )
            END IF

*  Set the returned value to the default.
            VALUE = DEFAUL

*  Terminate the loop.
            NOTOK = .FALSE.

         END IF

*  Go to the head of the loop.
         GOTO 100

*  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*  Come here when the loop has been exited.
  120 CONTINUE

*  Release the new error context.
      CALL ERR_RLSE

      END
