      SUBROUTINE PAR1_MENU( CHOICE, OPTS, SEPAR, PENLIM, OPTION, NCHAR,
     :                      PENALT, STATUS )
*+
*  Name:
*     PAR1_MENU

*  Purpose:
*     Finds a choice from a menu of options, permitting abbreviations.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PAR1_MENU( CHOICE, OPTS, SEPAR, PENLIM, OPTION, NCHAR,
*                     PENALT, STATUS )

*  Description:
*     This routine searches a list of options separated by a specified
*     delimiter for a given choice.  The choice may be an abbreviation
*     of one of the options, but it must not be ambiguous.  The match
*     need not be exact provided there is only one option with the
*     minimum number of matches.

*  Arguments:
*     CHOICE = CHARACTER * ( * ) (Given)
*        The choice to be found from the list of options.  It is
*        case insensitive and may be an abbreviation of the options.
*        It must not contain the delimiter SEPAR, otherwise no choice
*        is identified and a bad status of PAR__ERROR is returned.
*     OPTS = CHARACTER * ( * ) (Given)
*        A list of the menu options.  Each option is delimited from the
*        next by the separator SEPAR.  The menu may contain items whose
*        full names equal the start of another option, for example,
*        "DO" and "DOCK".  OPTS is case insensitive.
*     SEPAR = CHARACTER * ( * ) (Given)
*        The delimiter that separates the options in the menu.  This
*        need not be one character, and may include spaces.
*     PENLIM = INTEGER (Given)
*        The maximum number of characters mismatched between the
*        supplied choice and the best-matching item in the menu.  So
*        for example, PENLIM=0 would demand an exact match, and
*        PENLIM=1 would tolerate one mistyped character.
*     OPTION = CHARACTER * ( * ) (Returned)
*        The chosen option.  It is in uppercase, and in full.
*     NCHAR = INTEGER (Returned)
*        The number of characters in OPTION, ignoring any trailing
*        blanks.  If the option is blank, NCHAR is 1.
*     PENALT = INTEGER (Returned)
*        The number of characters mismatched between the choice and the
*        returned nearest option.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  An ambiguous choice causes a bad status of PAR__AMBIG to be
*     returned.  Should the choice not be in the menu, a bad status of
*     PAR__ERROR is returned.
*     -  The search for a match of the obtained character value with an
*     item in the menu adheres to the following rules.
*        o  All comparisons are performed in uppercase.  Leading blanks
*        are ignored.
*        o  A match is found when the value equals the full name of an
*        option.  This enables an option to be the prefix of another
*        item without it being regarded as ambiguous.  For example,
*        "10,100,200" would be an acceptable list of options with a
*        comma delimiter.
*        o  If there is no exact match, an abbreviation is acceptable.
*        A comparison is made of the value with each option for the
*        number of characters in the value.  The option that best fits
*        the value is declared a match, subject to two provisos.
*        Firstly, there must be no more than one character different
*        between the value and the start of the option.  (This allows
*        for a mistyped character.)  Secondly, there must be only one
*        best-fitting option.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 November 15 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      CHARACTER * ( * ) CHOICE
      CHARACTER * ( * ) OPTS
      CHARACTER * ( * ) SEPAR
      INTEGER PENLIM

*  Arguments Returned:
      CHARACTER * ( * ) OPTION
      INTEGER NCHAR
      INTEGER PENALT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! String length ignoring trailing
                                 ! blanks

*  Local Variables:
      LOGICAL DUPLIC             ! True if the match is ambiguous
      INTEGER CDELIM             ! Column containing the last delimiter
      INTEGER CLEN               ! Number of characters in the choice
      INTEGER I                  ! Loop counter
      INTEGER ILEN               ! Number of characters in a menu item
      CHARACTER * ( 132 ) ITEM   ! Item from the menu
      LOGICAL LAST               ! True if testing the last option in
                                 ! the menu
      INTEGER MAXPEN             ! Maximum penalty
      INTEGER OLEN               ! Number of characters in the options
      INTEGER SEPARC             ! Column containing the delimiter
      INTEGER SLEN               ! Number of characters in the separator
      CHARACTER * ( 132 ) UCHOIC ! Choice in uppercase and without
                                 ! leading blanks

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the option and the duplicate flag.
      OPTION = ' '
      DUPLIC = .FALSE.
      NCHAR = 0

*  Cater for a choice which is blank.  By definition it is deemed to
*  have one character.
      UCHOIC = CHOICE
      IF ( CHOICE .EQ. ' ' ) THEN
         CLEN = 1
      ELSE

*  Convert the item to uppercase and remove leading blanks.
         CALL CHR_UCASE( UCHOIC )
         CALL CHR_LDBLK( UCHOIC )

*  Get the length of the choice.
         CLEN = CHR_LEN( UCHOIC )
      END IF

*  Cater for a separator which is blank.  By definition it is deemed to
*  have one character.
      IF ( SEPAR .EQ. ' ' ) THEN
         SLEN = 1
      ELSE

*  Get the length of the separator.
         SLEN = CHR_LEN( SEPAR )
      END IF

*  Find the length of the options.
      OLEN = CHR_LEN( OPTS )

*  See if the choice contains the delimiter, in other words to determine
*  if there is more than one choice supplied.  If it does, the routine
*  cannot work, so an error report is made.
      IF ( INDEX( UCHOIC( : CLEN ), SEPAR( : SLEN ) ) .NE. 0 ) THEN
         STATUS = PAR__ERROR
         CALL MSG_SETC( 'SEP', SEPAR )
         CALL MSG_SETC( 'CH', CHOICE )
         CALL ERR_REP( 'PAR1_MENU_MULCHOIC',
     :     'The choice "^CH" contains the menu delimiter "^SEP".',
     :     STATUS )
         GOTO 999
      END IF

*  Initialise the column of the last delimiter, and the maximum penalty.
      CDELIM = 1 - SLEN
      MAXPEN = 100000000

*  Loop until the last value has been found or a match has been found.
  100 CONTINUE

*  Search for the delimiter.
         SEPARC = INDEX( OPTS( CDELIM + SLEN : ), SEPAR( : SLEN ) )

*  See whether or not this is the last item in the list.
         LAST = SEPARC .EQ. 0

*  Deal with a blank option.
         IF ( SEPARC .EQ. 1 ) THEN
            ITEM = ' '
            ILEN = 1
         ELSE IF ( .NOT. LAST ) THEN

*  Convert the item to uppercase and remove leading blanks.
            ITEM = OPTS( CDELIM + SLEN : CDELIM + SLEN + SEPARC - 2 )
            CALL CHR_UCASE( ITEM )
            CALL CHR_LDBLK( ITEM )

*  Get its length.  We know it cannot be more than SEPARC - 1
*  characters long.
            ILEN = CHR_LEN( ITEM( : SEPARC - 1 ) )

         ELSE

*  Convert the item to uppercase and remove leading blanks.
            ITEM = OPTS( CDELIM + SLEN : OLEN )
            CALL CHR_UCASE( ITEM )
            CALL CHR_LDBLK( ITEM )

*  Get its length.  Make efficient but allow for leading spaces.
            ILEN = CHR_LEN( ITEM( :OLEN - CDELIM - SLEN + 1 ) )
         END IF

*  First check whether or not the choice exactly matches the option.
*  If it does, this is deemed to be a match, and the search ends.  By
*  definition there is no penalty.
         IF ( ILEN .EQ. CLEN ) THEN
            IF ( UCHOIC( : CLEN ) .EQ. ITEM( : ILEN ) ) THEN
               OPTION = ITEM( :ILEN )
               NCHAR = ILEN
               PENALT = 0
               GOTO 999
            END IF
         END IF

*  Compare each character to form the penalty function.
         PENALT = 0
         DO 120 I = 1, CLEN
            IF ( UCHOIC( I : I ) .NE. ITEM( I : I ) ) THEN
               PENALT = PENALT + 1
            END IF
  120    CONTINUE

*  See whether this is the best match so far.  If it is, record the
*  number of the option, its penalty, and the option itself.
         IF ( PENALT .LT. MAXPEN ) THEN
            MAXPEN = PENALT
            DUPLIC = .FALSE.
            OPTION = ITEM( : ILEN )
            NCHAR = ILEN

*  Watch for duplicate penalties.  It is not counted as a duplicate
*  if the full option is repeated in the list.
         ELSE IF ( PENALT .EQ. MAXPEN ) THEN
            DUPLIC = ITEM .NE. OPTION
         END IF

*  Increment the column pointer within the list of options, so that
*  the next search begins after the previous delimiter.
         CDELIM = CDELIM + SEPARC + SLEN - 1

*  Loop to the next item, unless it is the last.
         IF ( .NOT. LAST ) GOTO 100

*  Test whether or not there is still a duplicate match.  If so, report
*  an error.  The content depends on the goodness of the match.  A
*  perfect match indicates that the choice is ambiguous and more
*  characters are required.
      IF ( DUPLIC ) THEN
         CALL MSG_SETC( 'CHOICE', CHOICE )
         CALL MSG_SETC( 'OPTS', OPTS )
         IF ( MAXPEN .GT. PENLIM ) THEN
            STATUS = PAR__ERROR
            CALL ERR_REP( 'PAR1_MENU_NOTFOUND',
     :        'The choice ^CHOICE is not in the menu.  The '/
     :        /'options are ^OPTS.', STATUS )
         ELSE
            STATUS = PAR__AMBIG
            CALL ERR_REP( 'PAR1_MENU_AMBIG',
     :        'The choice ^CHOICE is ambiguous.  The options are '/
     :        /'^OPTS.', STATUS )
         END IF

*  The option is not acceptable when the penalty is too large.
      ELSE IF ( MAXPEN .GT. PENLIM ) THEN
         CALL MSG_SETC( 'CHOICE', CHOICE )
         CALL MSG_SETC( 'OPTS', OPTS )
         STATUS = PAR__ERROR
         CALL ERR_REP( 'PAR1_MENU_NOTFOUND',
     :     'The choice ^CHOICE is not in the menu.  The options are '/
     :     /'^OPTS.', STATUS )

      END IF

*  The penalty is the smallest value.
      PENALT = MAXPEN

*  Finally we are left with a sufficiently good match and no ambiguity.
*  This option is returned.

  999 CONTINUE

      END
