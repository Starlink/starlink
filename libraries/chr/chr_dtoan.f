      SUBROUTINE CHR_DTOAN( DVALUE, UNITS, STRING, IPOSN )
*+
*  Name:
*     CHR_DTOAN

*  Purpose:
*     Write a DOUBLE PRECISION value into a string as hr/deg:min:sec.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_DTOAN( DVALUE, UNITS, STRING, IPOSN )

*  Description:
*     Format a DOUBLE PRECISION value as hours/degrees:minutes:seconds
*     and write it into a character string. This routine is for writing
*     angular measures into a character string in a format suitable
*     for presentation to an astronomer.
*
*     If the absolute value of the number to be written exceeds
*     a predefined maximum a conversion is not attempted, but the
*     number is written as a real number in Fortran `exponential'
*     format and a couple of question marks are appended to its
*     end. This prevents silly results when very large numbers
*     are input. The variable UNITS controls the maximum permitted
*     value for the conversion to be carried out.
*
*     The value is written into the part of the string beginning
*     at position IPOSN+1 and IPOSN is returned updated to the
*     position of the end of the encoded angle in STRING.

*  Arguments:
*     DVALUE = DOUBLE PRECISION (Given)
*        The value to be encoded into the string. This value should
*        represent an angular measure.
*     UNITS = CHARACTER * ( * ) (Given)
*        This string controls the maximum value which will be formatted
*        as hr/deg:min:sec: if UNITS = 'HOURS', the maximum permitted
*        value is 24.0; if UNITS = 'DEGREES', the  maximum permitted is
*        360.0. In all other cases the maximum is 1000.0.
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string into which DVALUE is written.
*     IPOSN = INTEGER (Given and Returned)
*        Given as the last element in STRING before the beginning of the
*        encoded angle. Returned as the element in STRING corresponding
*        to the end of the encoded angle.

*  Algorithm:
*     If units = 'HOURS' then
*       Set the maximum permitted value = 24.0.
*     else if units = 'DEGREES' then
*       Set the maximum permitted value = 360.0.
*     else
*       Set the maximum permitted value = 1000.0.
*     end if
*     if (the absolute value of the input number .le. the permitted
*     maximum ) then
*       Compute the integer hours/degrees.
*       Compute the minutes.
*       Compute the seconds.
*       Start the string with a minus sign if the value is negative.
*       Write the values to the output string.
*     else
*       Write the value into the string as a real number.
*       Append ' ???' to the output string.
*     end if
*
*     Negative values are handled the way they are to avoid the
*     minus sign being lost when the value is negative but the number
*     of degrees or hours is zero.

*  Copyright:
*     Copyright (C) 1984, 1985, 1991, 1994 Science & Engineering Research Council.
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
*     ACD: A.C. Davenhall (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-OCT-1984 (ACD):
*        Original version.
*     16-NOV-1984 (ACD):
*        Name changed from CHR_RTOANG to CHR_RTOAN to meet the naming
*        convention.
*     9-OCT-1985 (ACD):
*        Changed the way negative numbers are handled.
*     28-OCT-1991 (PCTR):
*        DOUBLE PRECISION version.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     31-MAR-1994 (ACC):
*        Change from call to CHR_PUTR to call to CHR_PUTD to write DVALUE
*        to STRING.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      DOUBLE PRECISION DVALUE

      CHARACTER UNITS * ( * )

*  Arguments Given and Returned:
      CHARACTER STRING * ( * )

      INTEGER IPOSN

*  External References:
      LOGICAL CHR_SIMLR          ! Caseless string equality

*  Local Constants:
      DOUBLE PRECISION MAXANY    ! Maximum in all other cases
      PARAMETER ( MAXANY = 1.0D+03 )

      DOUBLE PRECISION MAXDEG    ! Maximum number of Degrees
      PARAMETER ( MAXDEG = 3.60D+02 )

      DOUBLE PRECISION MAXHR     ! Maximum number of Hours
      PARAMETER ( MAXHR = 2.4D+01 )

      DOUBLE PRECISION SIXTY     ! Number of minutes in hour, seconds in
                                 ! minute etc.
      PARAMETER ( SIXTY = 6.0D+01 )

*  Local Variables:
      INTEGER IVALUE             ! Integer part of DVALUE (Hours or degrees)
      INTEGER MINUTE             ! Minutes

      DOUBLE PRECISION ABSVAL    ! Absolute value of DVALUE
      DOUBLE PRECISION DMIN      ! Minutes held as a DOUBLE PRECISION variable
      DOUBLE PRECISION MAXVAL    ! Maximum permitted value
      DOUBLE PRECISION SEC       ! Seconds

*.

*  Check the value of the units field to determine the appropriate
*  maximum value.
      IF ( CHR_SIMLR( UNITS, 'HOURS' ) ) THEN
         MAXVAL = MAXHR
      ELSE IF ( CHR_SIMLR( UNITS, 'DEGREES' ) ) THEN
         MAXVAL = MAXDEG
      ELSE
         MAXVAL = MAXANY
      END IF

*  Check if the value lies within the permitted range. If it does, then
*  format it as hours/degrees:min:sec, otherwise write it as a floating
*  point number.
      ABSVAL = ABS( DVALUE )

      IF ( ABSVAL .LE. MAXVAL ) THEN

*     Compute the hours/degrees.
         IVALUE = INT( ABSVAL )

*     Compute the minutes.
         DMIN = ( ABSVAL-DBLE( IVALUE ) ) * SIXTY
         MINUTE = INT( DMIN )

*     Compute the seconds.
         SEC = ( DMIN-DBLE( MINUTE ) ) * SIXTY

*     If the original value was negative then start the output string
*     with a minus sign.
         IF ( DVALUE .LT. 0.0D+00 ) CALL CHR_PUTC( '-', STRING, IPOSN )

*     Append the values to the output string.
         CALL CHR_PUTI( IVALUE, STRING, IPOSN )
         CALL CHR_PUTC( ':', STRING, IPOSN )

         IF ( MINUTE .LT. 10 ) CALL CHR_PUTC( '0', STRING, IPOSN )

         CALL CHR_PUTI( MINUTE, STRING, IPOSN )
         CALL CHR_PUTC( ':', STRING, IPOSN )

         IF ( SEC .LT. 1.0D+01 ) CALL CHR_PUTC( '0', STRING, IPOSN )

         CALL CHR_PUTD( SEC, STRING, IPOSN )
      ELSE

*     The given value is outside the range. Write it to the string as
*     a real number and append a couple of question marks to the string.
         CALL CHR_PUTD( DVALUE, STRING, IPOSN )
         CALL CHR_PUTC( ' ???', STRING, IPOSN )
      END IF

      END
