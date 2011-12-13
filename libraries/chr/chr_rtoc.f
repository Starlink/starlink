      SUBROUTINE CHR_RTOC( RVALUE, STRING, NCHAR )
*+
*  Name:
*     CHR_RTOC

*  Purpose:
*     Encode a REAL value as a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_RTOC( RVALUE, STRING, NCHAR )

*  Description:
*     Encode a REAL value as a character string, using as concise a
*     format as possible, and return the number of characters used.
*     In the event of an error, '*'s are written to the string.

*  Arguments:
*     RVALUE = REAL (Given)
*        The value to be encoded.
*     STRING = CHARACTER * ( * ) (Returned)
*        The string into which the value is to be encoded.
*     NCHAR = INTEGER (Returned)
*        The field width used in encoding the value.

*  Algorithm:
*     -  Base field width on string size, subject to the maximum
*     required to display any real value.
*     -  Attempt to write to an internal character string buffer using
*     an F format.
*        o  If the string is written successfully, replace any trailing
*        zeros with blanks.
*        o  If the attempt to write the string is unsuccessful, attempt
*        to use an E format.
*        o  If the string is written successfully, replace any trailing
*        or redundant zeros and any '+' signs with blanks.
*        o  Remove the blanks.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1989, 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1996, 1997, 2004 Central Laboratory of the Research Councils.
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
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     2-OCT-1984 (ACD):
*        Improve the documentation.
*     26-OCT-1984 (ACD):
*        Maximum possible number of decimal places inreased from 6 to 7.
*     1-SEP-1988 (AJC):
*        Use LEN instead of CHR_SIZE.
*     15-JUN-1989 (AJC):
*        Improve speed and comments.
*        Rely on WRITE to clear buffer.
*        Remove + only from exponent when there is one.
*        Use string size of FIELD rather than multiple finding used
*        length etc.
*     24-MAY-1991 (PCTR):
*        Rewrite to cure pathological problems with the previous
*        algorithm.
*      1-JUN-1994 (AJC):
*        Check for rounding increasing the number of digits before
*        the decimal point in F format as it causes an error
*     20-DEC-1994 (AJC):
*        Restore precision MXPREC from 6 to 7
*        and MXE 13 to 14
*     19-MAY-1995 (AJC):
*        Revise to remove jump into IF
*     23-MAY-1995 (AJC):
*        Revise algorithm again to get precision right.
*        Treat zero as special case
*        Allow for no digit to left of decimal point.
*      1-JUN-1995 (AJC):
*        Limit number of zeroes in F format fractions
*     28-MAR-1996 (AJC):
*        Restore MXE from 13 to 14
*     22-JAN-1997 (AJC)
*        Restrict CHR_LEN search to FIELD width (was SIZE)
*     12-AUG-2004 (TIMJ):
*        Initialise variables that were generating warnings
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      REAL RVALUE

*  Arguments Returned:
      CHARACTER STRING * ( * )

      INTEGER NCHAR

*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Constants:
      INTEGER MXE                ! Maximum E format string length
      PARAMETER ( MXE = 14 )

      INTEGER MXZ                ! Maximum zeroes after decimal point
      PARAMETER ( MXZ = 3 )

      INTEGER MXPREC             ! Maximum E format precision
      PARAMETER ( MXPREC = 7 )

*  Local Variables:
      LOGICAL ISNEG              ! Whether RVALUE is negative
      LOGICAL ISFRAC             ! Whether RVALUE is fractional

      INTEGER BEFORE             ! Number of digits before decimal point
      INTEGER AFTER              ! Number of digits after decimal point
      INTEGER EXTRAS             ! Extra characters needed
      INTEGER FIELD              ! Field width
      INTEGER I                  ! Loop index
      INTEGER IEXPON             ! Exponent size
      INTEGER IOSTAT             ! I/O status
      INTEGER IPOWER             ! Integer POWER
      INTEGER NPOWER             ! Integer power of rounded value
      INTEGER I1                 ! Loop start index
      INTEGER LSTAT              ! Local status
      INTEGER MNSIZE             ! Minimum returned string length.
      INTEGER SIZE               ! Character count

      REAL POWER                 ! Lg( VALUE )
      REAL VALUE                 ! Absolute value of RVALUE

      CHARACTER * 12 FORMAT      ! Fortran 77 format string
      CHARACTER * ( MXE ) STRBUF ! String buffer
      CHARACTER * 1 STRCHR       ! String element

*.

*  Initialise variables that might possibly be used
*  without being intialised otherwise
      ISNEG = .FALSE.
      ISFRAC = .FALSE.
      IPOWER = 0

*  Initialise the returned string.
      STRBUF = ' '

*  Initialise LSTAT.
      LSTAT = 1

*  Construct a suitable F format string.
      SIZE = LEN( STRING )

*  Treat zero as a special case
      IF ( RVALUE .NE. 0.0 ) THEN

*     For non-zeroes, find the order of the given value.
         VALUE = ABS( RVALUE )

*     EXTRAS are decimal point and possible sign
*     If the value is negative, we need the sign
         IF ( RVALUE .LT. 0.0 ) THEN
            ISNEG = .TRUE.
            EXTRAS = 2
         ELSE
            ISNEG = .FALSE.
            EXTRAS = 1
         END IF

         POWER = LOG10( VALUE )
         IPOWER = INT( POWER )
         IF ( POWER-REAL( IPOWER ) .LT. 0.0 ) IPOWER = IPOWER - 1

*     Check if the given value is fractional.
*     and calculate the number of digits required before and after
*     the decimal point to give the required precision.
         IF ( IPOWER .LT. 0 ) THEN
            ISFRAC = .TRUE.
            BEFORE = 1
*        If the maximum allowed number of zeroes after decimal point is
*        exceeded, force the number to be displayed in E format.
            IF ( IPOWER .LT. -(MXZ+1) ) THEN
               AFTER = -BEFORE -EXTRAS
            ELSE
               AFTER = MXPREC - (IPOWER + 1)
            ENDIF

         ELSE
            ISFRAC = .FALSE.
            BEFORE = IPOWER + 1
            AFTER = MXPREC - BEFORE
         END IF

*     Calculate the field width needed for maximum precision
*     This will be zero if we want to force E format
         FIELD = BEFORE + AFTER + EXTRAS
*     If this is greater than the minimum size of the given buffers,
*     adjust AFTER
         MNSIZE = MIN( SIZE, MXE )
         IF ( FIELD .GT. MNSIZE ) THEN
            AFTER = AFTER - ( FIELD - MNSIZE )
            FIELD = MNSIZE
         END IF

         IF ( AFTER .GE. 0 ) THEN
            IF ( .NOT. ISFRAC ) THEN
*           It isn't a fraction or zero so rounding could cause an extra
*           digit before the point
*           Check if rounding causes any difference
               POWER = LOG10( VALUE + 0.5*10**(-REAL(AFTER)) )
               NPOWER = INT( POWER )
               IF ( POWER-REAL( NPOWER ) .LT. 0.0 )
     :            NPOWER = NPOWER - 1

               IF ( NPOWER .NE. IPOWER ) THEN
*              Rounding has caused an additional digit before the point
*              Adjust BEFORE and AFTER.
                  BEFORE = BEFORE + 1
                  AFTER = AFTER - 1
               END IF
            END IF
         END IF

*     If AFTER is -1, there is still room for the stripped number in STRING,
*     but we will need to adjust FIELD and AFTER to get the WRITE into
*     STRBUF to work.
         IF ( AFTER .EQ. -1 ) THEN
            AFTER = 0
            FIELD = FIELD + 1
         END IF

*  Proceed with the F format only if the returned string can contain
*  the encoded string. Note that this decision does not include the
*  possible effects of rounding.
         IF ( AFTER .GE. 0 ) THEN

*        Write the F format string.
            WRITE( FORMAT, '( ''(F'', I3, ''.'', I3,'')'' )',
     :          IOSTAT=IOSTAT ) FIELD, AFTER

*        Check if the format string was written successfully: if so,
*        write the string buffer.
            IF ( IOSTAT .EQ. 0 ) WRITE( STRBUF( 1 : FIELD ), FORMAT,
     :                               IOSTAT=IOSTAT ) RVALUE

*     Check if the value was written into the string successfully.
            IF ( ( IOSTAT .EQ. 0 )
     :      .AND.( STRBUF( 1 : 1 ) .NE. '*' ) ) THEN
               LSTAT = 0

*           Remove superfluous trailing zeros and decimal characters.
               DO 10 I = FIELD, 1, -1
                  STRCHR = STRBUF( I : I )

                  IF ( ( STRCHR .NE. '0' )
     :            .AND. ( STRCHR .NE. '.' ) ) THEN
                     GO TO 20
                  ELSE IF ( STRCHR .EQ. '0' ) THEN
                     STRBUF( I : I ) = ' '
                  ELSE IF ( STRCHR .EQ. '.' ) THEN
                     STRBUF( I : I ) = ' '
                     GO TO 20
                  END IF
 10            CONTINUE
 20            CONTINUE
               NCHAR = CHR_LEN( STRBUF )
            END IF
         END IF

*     The value is zero
*  Just return "0"
      ELSE
         LSTAT = 0
         STRBUF = '0'
         NCHAR = 1
      END IF

*  Check if LSTAT is set to a non-zero value: if so, then attempt
*  to use an E format string to encode the given value.
      IF ( LSTAT .NE. 0 ) THEN

*     Clear the buffer
         STRBUF = ' '

*     Find the size of the exponent.
         IF ( IPOWER .EQ. 0 ) THEN
            IEXPON = 0
         ELSE IF ( ABS( IPOWER ) .LT. 9 ) THEN
            IEXPON = 2
         ELSE IF ( ABS( IPOWER ) .LT. 99 ) THEN
            IEXPON = 3
         ELSE
            IEXPON = 4
         END IF

         IF ( ISFRAC ) IEXPON = IEXPON + 1

*     Find the allowed size for the string buffer.
         IF ( ISNEG ) THEN
            MNSIZE = IEXPON + 2
         ELSE
            MNSIZE = IEXPON + 1
         END IF

*     Proceed with the E format only if the returned string can contain
*     the encoded string. Note that this decision does not include the
*     possible effects of rounding.
         IF ( ( MNSIZE .LE. MXE ) .AND. ( MNSIZE .LE. SIZE ) ) THEN

*        Find the precision for the given value.
            IF ( ISNEG ) THEN
               EXTRAS = IEXPON + 3
            ELSE
               EXTRAS = IEXPON + 2
            END IF

            AFTER = MIN( SIZE-EXTRAS, MXPREC )
            AFTER = MAX( AFTER, 0 )

*        Find the field width for the given value.
            IF ( ISNEG ) THEN
               FIELD = AFTER + 7
            ELSE
               FIELD = AFTER + 6
            END IF

*        Write the E format string.
            WRITE( FORMAT, '( ''(1PE'', I3, ''.'', I3,'')'' )',
     :             IOSTAT=IOSTAT ) FIELD, AFTER

*        Check if the format string was written successfully: if so,
*        write the string buffer.
            IF ( IOSTAT .EQ. 0 ) THEN
               WRITE( STRBUF( 1 : FIELD ), FORMAT, IOSTAT=IOSTAT
     :                ) RVALUE

*           Check if the value was written into the string successfully.
               IF ( IOSTAT .EQ. 0 ) THEN

*              Find the beginning of the exponent.
                  I1 = INDEX( STRBUF( 1 : FIELD ), 'E' )

*              Remove superfluous trailing zeros and decimal characters
*              from the fractional part.
                  DO 30 I = I1-1, 1, -1
                     STRCHR = STRBUF( I : I )

                     IF ( ( STRCHR .NE. '0' )
     :                    .AND. ( STRCHR .NE. '.' ) ) THEN
                        GO TO 40
                     ELSE IF ( STRCHR .EQ. '0' ) THEN
                        STRBUF( I : I ) = ' '
                     ELSE IF ( STRCHR .EQ. '.' ) THEN
                        STRBUF( I : I ) = ' '
                        GO TO 40
                     END IF
 30               CONTINUE
 40               CONTINUE

*              Remove superfluous '+' signs and leading zeros from the
*              exponent.
                  DO 50 I = I1+1, FIELD
                     STRCHR = STRBUF( I : I )

                     IF ( ( STRCHR .NE. '+' ) .AND. ( STRCHR .NE. '-' )
     :                    .AND. ( STRCHR .NE. '0' ) ) THEN
                        GO TO 60
                     ELSE IF ( ( STRCHR .EQ. '+' )
     :                         .OR. ( STRCHR .EQ. '0' ) ) THEN
                        STRBUF( I : I ) = ' '
                     END IF
 50               CONTINUE
 60               CONTINUE

*              Remove all spaces from the returned string.
                  CALL CHR_RMBLK( STRBUF( 1 : FIELD ) )
                  NCHAR = CHR_LEN(STRBUF( 1 : FIELD ) )
               END IF
            ELSE

*           On error, fill the returned string with '*'s.
               CALL CHR_FILL( '*', STRBUF )
               NCHAR = SIZE
            END IF
         ELSE

*        On error, fill the returned string with '*'s.
            CALL CHR_FILL( '*', STRBUF )
            NCHAR = SIZE
         END IF
      END IF

*  Assign the returned string and get its length, ignoring trailing
*  blanks.
      STRING = STRBUF( 1 : NCHAR )

      END
