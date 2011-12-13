      SUBROUTINE PAR_CHOIC( PARAM, DEFAUL, OPTS, NULL, VALUE, STATUS )

*+
*  Name:
*     PAR_CHOIC

*  Purpose:
*     Obtains from a parameter a character value selected from a menu
*     of options.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PAR_CHOIC( PARAM, DEFAUL, OPTS, NULL, VALUE, STATUS )

*  Description:
*     This routine obtains a scalar character value from a parameter.
*     The value must be one of a supplied list of acceptable values,
*     and can be an abbreviation provided it is unambiguous.  A dynamic
*     default may be suggested.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter.
*     DEFAUL = CHARACTER * ( * ) (Given)
*        The suggested default value for the parameter.  No default
*        will be suggested when DEFAUL is not one of the options defined
*        by OPTS.  A status of PAR__AMBIG is returned if the default is
*        ambiguous, i.e. an abbreviation of more than one of the
*        options.
*     OPTS = CHARACTER * ( * ) (Given)
*        The list of acceptable options for the value obtained from the
*        parameter.  Items should be separated by commas.  The list is
*        case-insensitive.
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
*     VALUE = CHARACTER * ( * ) (Returned)
*        The selected option from the list.  It is in uppercase and
*        in full, even if an abbreviation has been given for the actual
*        parameter.  If STATUS is returned not equal to SAI__OK, VALUE
*        takes the value of DEFAUL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The search for a match of the obtained character value with an
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
*     -  See whether or not a default is required by seeing if it is
*     one of the options in the list.  Report an error and exit if the
*     default is ambiguous.
*     -  Set the actual default value in the parameter system.
*     -  Obtain the value of the parameter.  Loop until an acceptable
*     value is obtained or an error condition exists.  Acceptable
*     means it is in the menu and an unambiguous selection was given.
*     If an unacceptable value is supplied an error is reported
*     immediately, and the list of options is reported.
*     -  If a bad status is returned from the parameter-system get, set
*     the returned value to the suggested default.  When the bad status
*     is PAR__NULL, annul the error and output a message.  The loop is
*     exited.

*  Copyright:
*     Copyright (C) 1990, 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1990 December 31 (MJC):
*        Original based on AIF_CHOID.
*     1991 January 31 (MJC):
*        Added the no-default option.
*     1991 June 24 (MJC):
*        Made to cope with the special case when an option in full
*        equals the start of another option, e.g. integers.
*     1992 November 18 (MJC):
*        Simplified the function and clarified the description of
*        parameter NULL.  Removed the restrictions on the number and
*        length of the options.
*     1999 September 16 (AJC):
*        Warn in MSG__VERB mode if NULL operates to adopt default.
*        Prologue add Warn in MSG__NORM mode if nearest match adopted.
*     2006 December 26 (TIMJ):
*        Initialise return value regardless of status
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
     :  DEFAUL,                ! Suggested default value
     :  PARAM,                 ! Parameter name corresponding to
                               ! variable VALUE
     :  OPTS                   ! List of possible options for VALUE

      LOGICAL                  ! True if:
     :  NULL                   ! Default value used when bad status is
                               ! returned by the parameter get.

*  Arguments Returned:
      CHARACTER * ( * )
     :  VALUE                  ! Character variable for which value is
                               ! to be obtained

*  Status:
      INTEGER STATUS           ! Global status

*  Local Variables:
      LOGICAL                  ! True if:
     :  NOTOK,                 ! No acceptable value obtained
     :  SUGDEF                 ! Suggest a default

      INTEGER
     :  NCD,                   ! Number of characters in the default
     :  NCV,                   ! Number of characters in the value
     :  PENALT                 ! Number of characters mismatched

      CHARACTER
     :  DEF * ( 132 ),         ! The selected default from the list of
                               ! menu options
     :  OPTION * ( 132 )       ! The selected option from the menu

*.

*  Initialise return value regardless of status
      VALUE = ' '

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find whether or not there is a suggested default.
*  =================================================
      SUGDEF = .TRUE.

*  By definition, the option cannot contain a blank value.
      IF ( DEFAUL .EQ. ' ' ) THEN
         SUGDEF = .FALSE.
         DEF = ' '
      ELSE

*  Start a new error context.
         CALL ERR_MARK

*  See if the suggested default is in the menu.  Allow no mismatched
*  characters.
         CALL PAR1_MENU( DEFAUL, OPTS, ',', 0, DEF, NCD, PENALT,
     :                   STATUS )

*  Look for an ambiguous default.  Add a contextual error report.
         IF ( STATUS .EQ. PAR__AMBIG ) THEN
            CALL MSG_SETC( 'DEF', DEFAUL )
            CALL MSG_SETC( 'PARAM', PARAM )
            CALL ERR_REP( 'PAR_CHOIC_AMBIG',
     :        'Programming error.  The suggested default ^DEF '/
     :        /'for parameter ^PARAM is ambiguous.', STATUS )

*  There will be a bad status whenever the default is not one of the
*  options.  We need to annul this error as it is expected, and merely
*  tells us that no suggested default is wanted.
         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            SUGDEF = .FALSE.
         END IF

*  Release the error context.
         CALL ERR_RLSE

*  Exit for the ambiguous case.
         IF ( STATUS .NE. SAI__OK ) GOTO 999
      END IF

*  Set the actual default value.
      IF ( SUGDEF ) CALL PAR_DEF0C( PARAM, DEF, STATUS )

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

*  Get a value from the parameter system.
         CALL PAR_GET0C( PARAM, VALUE, STATUS )

*  Check for an error.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check if we have an acceptable value.
*  =====================================

*  Permit one mistyped character in the value.
            CALL PAR1_MENU( VALUE, OPTS, ',', 1, OPTION, NCV, PENALT,
     :                      STATUS )

            IF ( STATUS .NE. SAI__OK ) THEN

*  Make a contextual error report.
               CALL MSG_SETC( 'PARAM', PARAM )
               CALL ERR_REP( 'PAR_CHOIC_INVOPT',
     :           'Invalid selection for parameter ^PARAM.', STATUS )

*  Note that the error is flushed immediately as we are in a loop.
               CALL ERR_FLUSH( STATUS )

*  Try again to obtain the value, so we must cancel the incorrect
*  attempt.
               CALL PAR_CANCL( PARAM, STATUS )

*  Reset the dynamic default value in the parameter system.
               IF ( SUGDEF ) CALL PAR_DEF0C( PARAM, DEF, STATUS )

*  A valid option was chosen, so we can exit the loop.
            ELSE
               VALUE = OPTION( :NCV )
               NOTOK = .FALSE.

*  Warn the user that the nearest match was used.
               IF ( PENALT .NE. 0 ) THEN
                  CALL MSG_SETC( 'VAL', VALUE )
                  CALL MSG_SETC( 'PARAM', PARAM )
                  CALL MSG_OUTIF( MSG__NORM, 'PAR_CHOIC_MISMAT',
     :              'Selected the nearest match "^VAL" for parameter '/
     :              /'^PARAM.', STATUS )
               END IF
            END IF

*  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*  Use the default value following an error.
         ELSE

*  Annul a null error to prevent an error report about null appearing.
*  Create a message informing the user of what has happened.
            IF ( STATUS .EQ. PAR__NULL .AND. NULL ) THEN
               CALL ERR_ANNUL( STATUS )

*  Inform the user what has happened.
               CALL MSG_SETC( 'DEFAULT', DEF )
               CALL MSG_SETC( 'PARAM', PARAM )
               CALL MSG_OUTIF( MSG__VERB, 'PAR_CHOIC_DEFA',
     :           'A value of ^DEFAULT has been adopted '/
     :           /'for parameter ^PARAM.', STATUS )
            END IF

*  Set the returned value to the default.
            VALUE = DEF

*  Make the value uppercase.
            CALL CHR_UCASE( VALUE )

*  Terminate the loop.
            NOTOK = .FALSE.

         END IF

*  Go to the head of the loop.
         GOTO 140

*  Come here when the main loop has been exited.
  200 CONTINUE

*  Release the new error context.
      CALL ERR_RLSE

  999 CONTINUE

      END
