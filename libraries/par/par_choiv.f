      SUBROUTINE PAR_CHOIV( PARAM, MAXVAL, OPTS, VALUES, ACTVAL,
     :                      STATUS )

*+
*  Name:
*     PAR_CHOIV

*  Purpose:
*     Obtains from a parameter a list of character values selected from
*     a menu of options.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PAR_CHOIV( PARAM, MAXVAL, OPTS, VALUES, ACTVAL, STATUS )

*  Description:
*     This routine obtains a vector of character values from a
*     parameter.  Each value must be one of a supplied list of
*     acceptable values, and can be an abbreviation provided it is
*     unambiguous.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter.
*     MAXVAL = INTEGER (Given)
*        The maximum number of values required.  A PAR__ERROR status is
*        returned when the number of values requested is less than one.
*     OPTS = CHARACTER * ( * ) (Given)
*        The list of acceptable options for the values obtained from the
*        parameter.  Items should be separated by commas.  The list is
*        case-insensitive.
*     VALUES( MAXVAL ) = CHARACTER * ( * ) (Returned)
*        The selected options from the list in the order supplied to the
*        parameter.  They are in uppercase and in full, even if an
*        abbreviation has been given for the actual parameter.
*     ACTVAL = INTEGER (Returned)
*        The actual number of values obtained.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The search for a match of each obtained character value with an
*     item in the menu adheres to the following rules.
*        o  All comparisons are performed in uppercase.  Leading blanks
*        are ignored.
*        o  A match is found when the value equals the full name of an
*        option.  This enables an option to be the prefix of another
*        item without it being regarded as ambiguous.  For example,
*        "10,100,200" would be an acceptable list of options.
*        o  If there is no exact match, an abbreviation is acceptable.
*        A comparison is made of the value with each option for the
*        number of characters in the value.  The option that best fits
*        the value is declared a match, subject to two provisos.
*        Firstly, there must be no more than one character different
*        between the value and the start of the option.  (This allows
*        for a mistyped character.)  Secondly, there must be only one
*        best-fitting option.  Whenever these criteria are not
*        satisfied, the user is told of the error, and is presented
*        with the list of options, before being prompted for a new
*        value.
*        If a nearest match is selected, the user is informed unless the
*        MSG filtering level (see SUN/104) is 'quiet'.

*  Algorithm:
*     -  If the number of values is not positive then report the error
*     and exit.
*     -  Obtain the vector of values of the parameter.  Loop until a set
*     of acceptable values are obtained or an error condition exists.
*     Acceptable means it is in the menu and an unambiguous selection
*     was given.  If an unacceptable value is supplied an error is
*     reported immediately, and the list of options is reported.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
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
*     AJC: Alan J. Chipperfield  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 March 23 (MJC):
*        Original version.
*     1999 September 20 (AJC):
*        Prologue add Warn in MSG__NORM mode if nearest match adopted.
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
     :  PARAM,                 ! Parameter name corresponding to
                               ! variable VALUES
     :  OPTS                   ! List of possible options for VALUES

      INTEGER
     :  MAXVAL                 ! Maximum number of values to obtain

*  Arguments Returned:
      CHARACTER * ( * )
     :  VALUES( MAXVAL )       ! Character array for which values are
                               ! to be obtained
      INTEGER
     :  ACTVAL                 ! Number of values obtained

*  Status:
      INTEGER STATUS           ! Global status

*  Local Variables:
      LOGICAL                  ! True if:
     :  NOTOK                  ! No acceptable value obtained

      INTEGER
     :  I,                     ! Loop counter
     :  NCV,                   ! Number of characters in the value
     :  PENALT                 ! Number of characters mismatched

      CHARACTER
     :  OPTION * ( 132 )       ! The selected option from the menu

*.

*  Initialise returned value to safe values in case an error occurs.
      ACTVAL = 0

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the number of values needed is positive.
      IF ( MAXVAL .LT. 1 ) THEN

*  Too few values requested.
         STATUS = PAR__ERROR
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'PAR_CHOIV_TOOFEW',
     :     'A non-positive number of values was requested for '/
     :     /'parameter ^PARAM. (Probable programming error.)', STATUS )

*  Exit the routine.
         GOTO 999

      END IF

*  Obtain the value of the parameter.
*  ==================================
*
*  Initialise NOTOK to start off the loop.
      NOTOK = .TRUE.

*  Start a new error context.
      CALL ERR_MARK

*  Repeat until an acceptable value obtained or an error occurs.
  140 CONTINUE
         IF ( .NOT. ( NOTOK ) .OR. ( STATUS .NE. SAI__OK ) ) GOTO 200

*  Get the values from the parameter system.
         CALL PAR_GET1C( PARAM, MAXVAL, VALUES, ACTVAL, STATUS )

*  Check for an error.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Test each value in turn.
            DO 160 I = 1, ACTVAL

*  Check if we have an acceptable value.
*  =====================================

*  Permit one mistyped character in the value.
               CALL PAR1_MENU( VALUES( I ), OPTS, ',', 1, OPTION, NCV,
     :                         PENALT, STATUS )

               IF ( STATUS .NE. SAI__OK ) THEN

*  Make a contextual error report.
                  CALL MSG_SETC( 'PARAM', PARAM )
                  CALL ERR_REP( 'PAR_CHOIV_INVOPT',
     :              'Invalid selection for parameter ^PARAM.', STATUS )

*  Note that the error is flushed immediately as we are in a loop.
                  CALL ERR_FLUSH( STATUS )

*  Try again to obtain the value, so we must cancel the incorrect
*  attempt.
                  CALL PAR_CANCL( PARAM, STATUS )

*  Exit the loop.
                  GOTO 140

*  A valid option was chosen, so we can exit the loop.
               ELSE
                  VALUES( I ) = OPTION( :NCV )

*  Warn the user that the nearest match was used.
                  IF ( PENALT .NE. 0 ) THEN
                     CALL MSG_SETC( 'VAL', VALUES( I ) )
                     CALL MSG_SETI( 'I', I )
                     CALL MSG_SETC( 'PARAM', PARAM )
                     CALL MSG_OUTIF( MSG__NORM, 'PAR_CHOIV_MISMAT',
     :                 'Selected the nearest match "^VAL" for value '/
     :                 /'number ^I of parameter ^PARAM.', STATUS )
                  END IF
               END IF

  160       CONTINUE
         END IF

*  Terminate the loop as all the values were validated.
         NOTOK = .FALSE.

*  Go to the head of the loop.
         GOTO 140

*  Come here when the main loop has been exited.
  200 CONTINUE

*  Release the new error context.
      CALL ERR_RLSE

  999 CONTINUE

      END
