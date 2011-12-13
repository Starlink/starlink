      SUBROUTINE CON_ANGDC( UNITS, ANGLEC, ANGLER, STATUS )
*+
*  Name:
*     CON_ANGDC

*  Purpose:
*     Decodes an angle from a CHARACTER string.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL CON_ANGDC( UNITS, ANGLEC; ANGLER; STATUS )

*  Description:
*     Decode an angle from a CHARACTER string.
*
*     The hours or degrees, minutes and seconds must be separated by a
*     colon (':').  Either the seconds or the minutes and the seconds
*     may be omitted.  Only the least significant unit (hours or
*     degrees, minutes or seconds, as appropriate) may contain a fractional
*     part; the more significant items must be integer numbers.

*  Arguments:
*     UNITS  =  CHARACTER*(*) (Given)
*        Units in which the angle is expressed.  The options are:
*        ANGLE   - sexagesimal hours or degrees; an unsigned number is
*           assumed to be in hours, a signed one in degrees (thus a
*           an angle in degrees must always be signed, even if it is
*           positive),
*        HOURS   - sexagesimal hours,
*        DEGREES - sexagesimal degrees,
*        ARCMIN  - sexagesimal minutes of arc (optionally subdivided
*           into seconds),
*        ARCSEC  - seconds of arc.
*        TIMEMIN - sexagesimal minutes of time (optionally subdivided
*           into seconds),
*        TIMESEC - seconds of time.
*     ANGLEC  =  CHARACTER*(*) (Given)
*        String containing the angle expressed in a sexagesimal format.
*     ANGLER  =  DOUBLE PRECISION (Returned)
*        Corresponding angle in radians.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Take a local copy of the units.
*     Convert the local copy of the units to upper case.
*     If the string is not entirely blank then
*       Take a copy of the angle.
*       Remove any leading blanks.
*       If the units are 'ANGLE' then
*         If the first character is a sign then
*           Set the units to 'DEGREES'.
*         else
*           Set the units to 'HOURS'.
*         end if
*       end if
*       Determine the length of the string.
*       Replace any colons with spaces.
*       Attempt to convert the angle to radians (assuming it is in
*       degrees).
*       If the conversion is ok then
*         Multiply the angle by the appropriate conversion factor for
*         its units.
*       else
*         Set the status.
*         Report an error.
*       end if
*     else (the string is blank)
*       Set the result to zero.
*     end if

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
*     ACD: A C Davenhall (Leicester)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     14/8/97 (ACD):
*        Original version, created from CAP_ANGDC.  The only change was
*        to report messages directly with MSG_OUT rather than through
*        the CURSA/CAP specific routine CAP_INFO.
*     28/8/97 (ACD):
*        Changed the routine used to report messages from MSG_OUT to
*        MSG_OUTIF.
*     2009 June 29 (MJC):
*        Used modern coding style.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants
      INCLUDE 'MSG_PAR'          ! Message system constants

*  Arguments Given:
      CHARACTER*(*) UNITS
      CHARACTER*(*) ANGLEC

*  Arguments Returned:
      DOUBLE PRECISION ANGLER

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! String length less trailing blanks

*  Local Variables:
      CHARACTER*75 ANGCPY        ! Copy of ANGLEC
      INTEGER ANGPOS             ! Current position in ANGCPY
      CHARACTER*10 CUNITS        ! Copy of UNITS
      INTEGER ERRLEN             ! Current position in ERRTXT
      CHARACTER*75 ERRTXT        ! Text of error message
      INTEGER LANGCP             ! Length of ANGCPY (excl. trail. blanks)
      INTEGER LOOP               ! Loop index
      INTEGER LSTAT              ! Local status decode value from string

*.

*  Check global inherited status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Take a local copy of the units and convert it to upper case.
         CUNITS = UNITS
         CALL CHR_UCASE( CUNITS )

*  Check that the string is not completely blank.
         IF ( ANGLEC .NE. ' ' ) THEN

*  Take a copy of the angle (this copy will be modified).
            ANGCPY = ANGLEC

*  Remove any leading blanks.
            CALL CHR_LDBLK( ANGCPY )

*  If the units are 'ANGLE' then determine the actual units by checking
*  whether the first character is a sign ('+' or '-').  If it is then
*  the angle is considered to be in degrees; otherwise it is considered
*  to be in hours.
            IF ( CUNITS .EQ. 'ANGLE' ) THEN
               IF ( ANGCPY( 1:1 ) .EQ. '+' .OR.
     :              ANGCPY( 1:1 ) .EQ. '-' ) THEN
                  CUNITS = 'DEGREES'
               ELSE
                  CUNITS = 'HOURS'
               END IF
            END IF

*  Determine the length of the string.
            LANGCP = CHR_LEN(ANGCPY)

*  Replace any colons with spaces (to get the string in the
*  format required by SLA_DAFIN).
            DO LOOP = 1, LANGCP
               IF ( ANGCPY( LOOP:LOOP ) .EQ. ':' ) THEN
                  ANGCPY( LOOP:LOOP ) = ' '
               END IF
            END DO

*  Attempt to convert the angle to radians (the conversion is
*  performed implicitly assuming it is in degrees).
            ANGPOS = 1

            CALL SLA_DAFIN( ANGCPY( 1:LANGCP ), ANGPOS, ANGLER, LSTAT )

*  Proceed if the conversion is was successful.
            IF ( LSTAT .EQ. 0 ) THEN

*  Multiply the appropriate conversion factor to compensate for the
*  original units not being degrees.
               IF ( CUNITS .EQ. 'HOURS' ) THEN
                  ANGLER = ANGLER * 1.5D1
               ELSE IF ( CUNITS .EQ. 'DEGREES' ) THEN
                  CONTINUE

               ELSE IF ( CUNITS .EQ. 'ARCMIN' ) THEN
                  ANGLER = ANGLER / 6.0D1
               ELSE IF ( CUNITS .EQ. 'ARCSEC' ) THEN
                  ANGLER = ANGLER / ( 6.0D1 * 6.0D1 )

               ELSE IF ( CUNITS .EQ. 'TIMEMIN' ) THEN
                  ANGLER = ANGLER * 1.5D1 / 6.0D1
               ELSE IF ( CUNITS .EQ. 'TIMESEC' ) THEN
                  ANGLER = ANGLER * 1.5D1 / ( 6.0D1 * 6.0D1 )

               ELSE
                  CALL MSG_SETC( 'CUNITS', CUNITS )
                  CALL MSG_OUTIF( MSG__NORM, ' ', 'Unknown units for '/
     :              /'an angle: ^CUNITS; degrees assumed.', STATUS )
               END IF

            ELSE

*  The conversion failed.  Set the status and report an error.
               STATUS = SAI__ERROR

               ERRTXT = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('Failed to decode angle (', ERRTXT,
     :                         ERRLEN )

               IF ( LSTAT .EQ. -1 ) THEN
                  CALL CHR_PUTC( 'bad degrees', ERRTXT, ERRLEN )

               ELSE IF ( LSTAT .EQ. -2 ) THEN
                  CALL CHR_PUTC( 'bad minutes', ERRTXT, ERRLEN )

               ELSE IF ( LSTAT .EQ. -3 ) THEN
                  CALL CHR_PUTC( 'bad seconds', ERRTXT, ERRLEN )

               ELSE
                  ANGLER = 0.0D0

                  CALL CHR_PUTC( 'bad angle', ERRTXT, ERRLEN )

               END IF

               CALL CHR_PUTC( '): ', ERRTXT, ERRLEN )

               CALL CHR_PUTC( ANGCPY( 1:LANGCP ), ERRTXT, ERRLEN )

               CALL ERR_REP( 'CON_ANGDC_ERR', ERRTXT( 1:ERRLEN ),
     :                       STATUS )

            END IF

         ELSE

*  The string is completely blank; set the angle to zero.
            ANGLER = 0.0D0

         END IF

      END IF

      END
