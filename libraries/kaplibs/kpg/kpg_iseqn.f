      SUBROUTINE KPG_ISEQN( IN, INCREM, OUT, STATUS )
*+
*  Name:
*     KPG_ISEQN

*  Purpose:
*     Increments a sequence string. 

*  Language:
*     Starlink

*  Invocation:
*     CALL KPG_ISEQN( IN, INCREM, OUT, STATUS )

*  Description:
*     This routine takes an input string comprising two parts: an
*     alphanumeric string ending in an non-numeric character; followed
*     by a sequence number of digits, possibly with leading zeroes.  The
*     sequence number is incremented or decremented by a supplied number
*     in a returned string of the same length as the input string.
*     For example, u20100320_0084 would become u20100320_0085 for an
*     increment of 1, and u20100320_0074 for an increment of -10.
*
*     An error results if the supplied string is not of the correct
*     form, or the increment takes the sequence counter beyond its
*     range.  The sequence number cannot be negative or require
*     additional characters.  For example incrementing string DATA999
*     by one would be rejected as it would require an extra character.

*  Arguments:
*     IN = CHARACTER * ( * ) (Given)
*        String containing a trailing sequence number.
*     INCREM = INTEGER (Given)
*        The increment to apply to string IN.  It may be positive or
*        negative.  A value of 0 causes IN to be merely copied to OUT.
*     OUT  = CHARACTER * ( * ) (Returned)
*        The supplied string but with its sequence number incremented by
*        INCREM.  It must have at least the length of IN.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.  An SAI__ERROR is returned, should any
*        of the errors listed above be detected.

*  Notes:
*     The sequence number can have up to twelve digits.

*  Copyright:
*     Copyright (C) 2010 Science & Tecxhnology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2010 August 26 (MJC):
*        Original version inspired by NXTNAM.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! No default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'         ! SSE global definitions

*  Arguments Given:
      CHARACTER * ( * ) IN
      INTEGER INCREM

*  Arguments Returned:
      CHARACTER * ( * ) OUT
 
*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! String length without trailing blanks
      LOGICAL CHR_ISDIG          ! Is a character a digit?

*  Local Constants:
      INTEGER MAXDIG             ! Maximum number of digits in sequence
      PARAMETER ( MAXDIG = 12 ) ! number

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER INSEQ              ! Sequence number of input string
      LOGICAL ISDIG              ! Is current character a digit?
      INTEGER NCHAR              ! Number of valid digits in output string
      INTEGER NCIN               ! Length of current filename string
      INTEGER NDIGIT             ! Number of valid digits in sequence
      CHARACTER*(MAXDIG) NEWSEQ  ! Sequence number of output string
      INTEGER OUTSEQ             ! Sequence number of output string


*.

*  Initialise returned string.
      OUT = ' '

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the length of the supplied input string ignoring trailing
*  blanks.
      NCIN = CHR_LEN( IN )

*  Validate the output string's length.
      IF ( LEN( OUT ) .LT. NCIN ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'OC', LEN( OUT ) )
         CALL MSG_SETI( 'IC', NCIN )
         CALL ERR_REP( 'KPG_ISEQN_ERR1',
     :      'KPG_ISEQN: programming error.  Insufficient characters '/
     :      /'(^OC) in output string.  Must be at least ^IC.', STATUS )
         GOTO 999
      END IF

*  Count the number of trailing digits in the string.
      I = NCIN
      ISDIG = .TRUE.
      DO WHILE ( NCIN .GT. 1 .AND. ISDIG )
         ISDIG = CHR_ISDIG( IN( I:I ) )
         I = I - 1
      END DO
      NDIGIT = NCIN - I - 1

*  Find the number of characters in the prefix.
      I = I + 1

*  Validate the supplied string for too many or no trailing digits
*  that form the sequence number.
      IF ( NDIGIT .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IN', IN )
         CALL ERR_REP( 'KPG_ISEQN_ERR2',
     :      'KPG_ISEQN: possible programming error.  Supplied string '/
     :      /'(^IN) has no trailing digits.', STATUS )
         GOTO 999

      ELSE IF ( NDIGIT .GT. MAXDIG ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'IN', IN )
         CALL MSG_SETI( 'MD', MAXDIG )
         CALL ERR_REP( 'KPG_ISEQN_ERR3',
     :      'KPG_ISEQN: possible programming error.  Supplied string '/
     :      /'(^IN) has more than ^MD trailing digits.', STATUS )
         GOTO 999
      END IF

*  Convert the sequence number from a string to an integer.
      CALL CHR_CTOI( IN( NCIN - NDIGIT + 1 : NCIN ), INSEQ, STATUS )

*  Check for error.  Handle the null operation too.
      IF ( STATUS .NE. SAI__OK .OR. INCREM .EQ. 0 ) THEN
         OUT = IN
         GOTO 999
      END IF

*  Increment or decrement to form the output sequence number.
*  Validate the new sequence number.
      OUTSEQ = INSEQ + INCREM
      IF ( OUTSEQ .LT. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ICM', INCREM )
         CALL ERR_REP( 'KPG_ISEQN_ERR4',
     :      'KPG_ISEQN: possible programming error.  Increment '/
     :      /'(^ICM) will make the sequence number negative.', STATUS )
         GOTO 999
      END IF

      NCHAR = INT( LOG10( REAL( OUTSEQ ) ) ) + 1
      IF ( NCHAR .GT. NDIGIT ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ICM', INCREM )
         CALL ERR_REP( 'KPG_ISEQN_ERR5',
     :      'KPG_ISEQN: possible programming error.  Increment '/
     :      /'(^ICM) will take the sequence number out of range.',
     :      STATUS )
         GOTO 999
      END IF

*  Convert new number back into a string with leading zeroes, by
*  adding the next higher power of ten.
      CALL CHR_ITOC( OUTSEQ + 10**NDIGIT, NEWSEQ, NCHAR )

*  Form the result, first copy the prefix and then append the new
*  sequence number, but ignoring the the next order of magnitude
*  added to provide the leading zeroes.
      OUT = IN( 1:I )
      CALL CHR_APPND( NEWSEQ( 2: ), OUT, I )

 999  CONTINUE

      END
