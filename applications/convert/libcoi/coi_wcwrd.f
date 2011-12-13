      SUBROUTINE COI_WCWRD( HEADER, PARAM, VALUE, THERE, STATUS )
*+
*  Name:
*     COI_WCWRD

*  Purpose:
*     Extracts IRAF MWCS parameter values from header cards.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_WCWRD( HEADER, PARAM, VALUE, THERE, STATUS )

*  Description:
*     This routine takes an IRAF MWCS header card of the form
*
*        WATd_nnn= 'param1=value1 param2=value2'
*
*     and returns the value for a named parameter.  Values are either
*     terminated by spaces or the trailing quotes.  Values may be
*     enclosed in "" quotes; these are necessary whenever the value
*     has embedded spaces.
*
*     A returned flag indicates whether the parameter was present.

*  Arguments:
*     HEADER = CHARACTER * ( * ) (Given)
*        The IRAF MCWS WATd_nnn header card.  It should be 80 characters
*        long.
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter whose value is required.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The parameter value, when THERE is returned .TRUE..  The value
*        is truncated to the length of this argument, should the former
*        exceed the latter.
*     THERE = LOGICAL (Returned)
*        It is .TRUE. when the named parameter is present in the
*        supplied header.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     It does assume that the parameter name in the WAT header will
*     always be in lowercase.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 July 18 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) HEADER
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      CHARACTER * ( * ) VALUE
      LOGICAL THERE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! String length sans trailing blanks

*  Local Variables:
      LOGICAL DQPRES             ! Value begins with a double quote "
      CHARACTER * ( 12 ) LPARAM  ! Lowercase version of the parameter
      INTEGER NCHEAD             ! Number of characters in header
      INTEGER NCPAR              ! Number of characters in parameter
                                 ! value
      INTEGER POSEND             ! Position of the end of the value
      INTEGER POSPAR             ! Position of the start of the
                                 ! parameter name
      INTEGER POSUPP             ! Position of the end of the parameter
                                 ! name
      INTEGER POSVAL             ! Position of the start of the value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise returned value and presence.
      VALUE = ' '
      THERE = .FALSE.

*  Make lowercase versions of the parameter for comparison
*  purposes.  Find its and the headers effective lengths too.
      LPARAM = PARAM
      CALL CHR_LCASE( LPARAM )
      NCPAR = CHR_LEN( LPARAM )
      NCHEAD = CHR_LEN( HEADER )

*  Look for the parameter name.  Allow for a variety of syntaxes.
      POSPAR = MAX( INDEX( HEADER, LPARAM( :NCPAR )//' =' ),
     :              INDEX( HEADER, LPARAM( :NCPAR )//'=' ) )

      IF ( POSPAR .GT. 0 ) THEN
         THERE = .TRUE.

*  See if the value is enclosed in quotes.  Note that the search is
*  constrained to the specific parameter allowing for a space following
*  the equals sign.
         POSUPP = POSPAR + NCPAR + 4
         DQPRES = INDEX( HEADER( POSPAR:POSUPP ), '="' ) .NE. 0 .OR.
     :            INDEX( HEADER( POSPAR:POSUPP ), '= "' ) .NE. 0

*  First locate the start of the value.  Starting from the next
*  character following the parameter name, move forward in the buffer
*  until the next character is not an equals sign, a space, or a double
*  quote.
         POSVAL = POSPAR + NCPAR
         CALL CHR_SKCHR( '= "', HEADER, .TRUE., POSVAL )

*  Now locate the end of the value.  First if there was a leading ",
*  look for a trailing ".
         IF ( DQPRES ) THEN
            POSEND = INDEX( HEADER( POSVAL: ), '"' )

*  No trailing quote.  Here we assume that the initial " was not part of
*  the value and that the value does not span header cards.  So use the
*  assign the rest of the header to the value.
            IF ( POSEND .GT. 0 ) THEN
               POSEND = POSEND + POSVAL - 2
            ELSE
               POSEND = NCHEAD
            END IF

*  No quotes.  The next space delimiters the value.
         ELSE
            POSEND = INDEX( HEADER( POSVAL: ), ' ' )

*  No space, so go to the end of the card.
            IF ( POSEND .GT. 0 ) THEN
               POSEND = POSEND + POSVAL - 2
            ELSE
               POSEND = NCHEAD
            END IF
         END IF

*  Finally extract the value.
         VALUE = HEADER( POSVAL:POSEND )
      END IF

      END
