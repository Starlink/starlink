      SUBROUTINE IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )
*+
*  Name:
*     IRA1_CHSCS

*  Purpose:
*     Identify a sky coordinate system and get equinox date.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )

*  Description:
*     The input string (SCS) should contain an unambiguous abbreviation
*     of a sky coordinate system. An epoch describing the mean equinox
*     to which the coordinates are referred may optionally be appended
*     to the abbreviation, enclosed in parenthesise. The epoch is given
*     in an "equinox specifier", as a year with upto 4 decimal places,
*     preceded with the letter B or J to indicate a Besselian epoch or
*     a Julian epoch (eg "EQUAT(J2000.0)", "ECLIPT(B1983.3)" ). If the
*     epoch is not preceded with either B or J, a Besselian epoch is
*     assumed if the date is less than 1984.0, and a Julian epoch is assumed
*     otherwise.  If no equinox specifier is included in the input
*     SCS name, a value of B1950.0 is assumed (if required). If the SCS is
*     not referred to the equinox, any equinox specifier is ignored.
*
*     The NAME argument is returned holding the full name of the sky
*     coordinate system (with no equinox specifier). The epoch of the
*     reference equinox is returned as a double precision variable in
*     EQU, and BJ is returned holding either B (for Besselian epochs)
*     or J (for Julian epochs). If the input SCS name is not legal, or
*     is ambiguous, an error is generated.

*  Arguments:
*     SCS = CHARACTER * ( * ) (Given)
*        The input string; an abbreviation of an SCS name, with an
*        optional equinox specifier appended at the end.
*     NAME = CHARACTER * ( * ) (Returned)
*        The full name of the sky coordinate system. No equinox
*        specifier is included. This string should have a length given
*        by the symbolic name IRA__SZSCS.
*     EQU = DOUBLE PRECISION (Returned)
*        The epoch of the reference equinox. A value of 1950.0D0 is
*        returned if no epoch is specified in the input string. If the
*        SCS is not reference to the equinox (eg GALACTIC) then EQU is
*        returned equal to the Starlink "BAD" value VAL__BADD.
*     BJ = CHARACTER * ( * ) (Returned)
*        "B" is returned (without the quotes) if the epoch in the
*        equinox specifier represents a Besselian epoch, and "J"
*        if it represents a Julian epoch. If no epoch is specified in
*        argument SCS, then a value of B is returned (unless the epoch is
*        2000 in which case J is returned). If the SCS is not reference to
*        the equinox (eg GALACTIC) then BJ is returned blank.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1984, 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1998, 2000 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JAN-1991 (DSB):
*        Original version.
*     29-APR-1991 (DSB):
*        Modified for IRA version 2.
*     22-AUG-1994 (DSB):
*        Assume Julian default for epoch 2000.
*     3-DEC-1998 (DSB):
*        Assume Julian epoch if date is >= 1984.0 (unless a specific B or J
*        is included to indicate otherwise).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! BAD data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      CHARACTER SCS*(*)

*  Arguments Returned:
      CHARACTER NAME*(*)
      DOUBLE PRECISION EQU
      CHARACTER BJ*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CHR_LEN            ! Function giving used length of a string.
      INTEGER EEND               ! End of the epoch string.
      INTEGER END                ! End position of a name within a list.
      INTEGER ESPECE             ! End of the equinox specifier.
      INTEGER ESPECS             ! Start of the equinox specifier.
      INTEGER ESTART             ! Start of the epoch string.
      LOGICAL CHR_ISDIG          ! Function which is true if the
                                 ! argument is a digit character.
      CHARACTER LIST*(IRA__SZCLS)! List of supported SCS names.
      INTEGER LSCS               ! Used length of SCS.
      LOGICAL MORE               ! True if more SCS names remain.
      INTEGER NMATCH             ! No. of matches found between IN and
                                 ! LIST.
      INTEGER START              ! Start position of a name within a
                                 ! list.
      CHARACTER TSCS*(IRA__SZSCS)! Local copy of argument SCS.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Take a local copy of SCS, remove any embedded blanks and convert to
*  upper case.
      TSCS = SCS
      CALL CHR_RMBLK( TSCS )
      CALL CHR_UCASE( TSCS )

*  Find the start of any equinox specifier within IN.
      ESPECS = INDEX( TSCS, '(' )

*  If present, find the end of the equinox specifier.
      IF( ESPECS .NE. 0 ) THEN
         ESPECE = INDEX( TSCS, ')' )

         IF( ESPECE .EQ. 0 ) THEN
            STATUS = IRA__BADEQ
            CALL MSG_SETC( 'EQ', TSCS( ESPECS: ) )
            CALL ERR_REP( 'IRA1_CHSCS_ERR1',
     :             'IRA1_CHSCS: Unable to read equinox specifier: ^EQ',
     :                    STATUS )
            GO TO 999
         END IF

*  Save the start and end of the actual epoch string (excluding
*  parenthesise and BJ specifier).
         ESTART = ESPECS + 2
         EEND = ESPECE - 1

*  Copy the BJ specifier to argument BJ.
         BJ = TSCS( ESPECS + 1 : ESPECS + 1 )

*  If a numeric character was found, defer the decision about the type of
*  epoch until the numerical epoch value has been determined. Re-wind the
*  pointer by one character so that he digit is included in the date value.
         IF( CHR_ISDIG( BJ ) ) THEN
            ESTART = ESTART - 1
            BJ = ' '

*  If any non-numeric character other than B or J was found, report an
*  error.
         ELSE IF( BJ .NE. 'B' .AND. BJ .NE. 'J' ) THEN
            STATUS = IRA__BADEQ
            CALL MSG_SETC( 'EQ', TSCS( ESPECS:ESPECE ) )
            CALL ERR_REP( 'IRA1_CHSCS_ERR2',
     :              'IRA1_CHSCS: Unable to read equinox specifier: ^EQ',
     :                    STATUS )
            GO TO 999

         END IF

*  Convert the actual epoch string to a double precision floating point
*  value.
         CALL CHR_CTOD( TSCS( ESTART:EEND ), EQU, STATUS )

*  If unsuccesful, report an error.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'EQ', TSCS( ESPECS:ESPECE ) )
            CALL ERR_REP( 'IRA1_CHSCS_ERR3',
     :              'IRA1_CHSCS: Unable to read equinox specifier: ^EQ',
     :                    STATUS )
            GO TO 999

         END IF

*  Round the date to 4 decimal places.
         EQU = 1.0D-4*NINT( EQU*1.0D4 )

*  If the decision about the type of epoch has been deferred, make the
*  decision now using the IAU 1984.0 rule.
         IF( BJ .EQ. ' ' ) THEN
            IF( EQU .LT. 1984.0 ) THEN
               BJ = 'B'
            ELSE
               BJ = 'J'
            END IF
         END IF

*  If no equinox specifier was found, set the default value. This will
*  later be changed to VAL__BADD if the SCS is not referred to the
*  equinox.
      ELSE
         ESPECS = CHR_LEN( TSCS ) + 1
         EQU = 1950.0D0
         BJ = 'B'

      END IF

*  Get the length of the SCS name or abbreviation (without the equinox
*  specifier).
      LSCS = ESPECS - 1

*  Get a string containing the list of supported sky coordinate systems
*  (without any equinox specifiers).
      CALL IRA_ISCS( LIST, STATUS )

*  Loop round each name in the list.
      MORE = .TRUE.
      START = 1
      NMATCH = 0

      DO WHILE( MORE )

*  Get the next name from the list.
         END=INDEX( LIST(START:), ',' ) + START - 2
         IF( END .EQ. START - 2 ) THEN
            END = CHR_LEN( LIST)
            MORE = .FALSE.
         END IF

*  See if the input string is an abbreviation of this name. If it is
*  increment the number of matches found and store the full name.
         IF( INDEX( LIST( START:END ), TSCS( :LSCS ) ) .EQ. 1 ) THEN
            NMATCH = NMATCH + 1
            NAME = LIST( START:END )
         END IF

*  Set the start of the next name in the list
         START = END + 2

      END DO

*  If no matches were found, give an error.
      IF( NMATCH .EQ. 0 ) THEN
         STATUS = IRA__BADSC
         CALL MSG_SETC( 'SCS', TSCS(:LSCS) )
         CALL ERR_REP( 'IRA1_CHSCS_ERR4',
     :  'IRA1_CHSCS: Unsupported sky coordinate system specified: ^SCS',
     :                 STATUS )

*  If the input SCS was ambiguous, give an error message.
      ELSE IF( NMATCH .GT. 1 ) THEN
         STATUS = IRA__AMBSC
         CALL MSG_SETC( 'SCS', TSCS(:LSCS) )
         CALL ERR_REP( 'IRA1_CHSCS_ERR5',
     :    'IRA1_CHSCS: Ambiguous sky coordinate system specified: ^SCS',
     :                 STATUS )

*  If the SCS is not referred to the equinox, set the equinox related
*  arguments to indicate that no equinox is specified.
      ELSE
         IF( NAME .NE. 'EQUATORIAL' .AND.
     :       NAME .NE. 'ECLIPTIC' ) THEN

            EQU = VAL__BADD
            BJ = ' '

         END IF

      END IF

 999  CONTINUE

      END
