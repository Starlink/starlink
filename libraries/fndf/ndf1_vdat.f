      SUBROUTINE NDF1_VDAT( YMDHM, SEC, STATUS )
*+
*  Name:
*     NDF1_VDAT

*  Purpose:
*     Validate a date/time specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VDAT( YMDHM, SEC, STATUS )

*  Description:
*     The routine checks a date/time specification for validity. If it
*     is OK, the routine returns without action. Otherwise an error is
*     reported and STATUS is set.

*  Arguments:
*     YMDHM( 5 ) = INTEGER (Given)
*        Values of the year, month, day, hour and minute fields of the
*        date/time specification (in that order), stored as integers.
*     SEC = REAL (Given)
*        Value of the seconds (and fractions of seconds) field.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S. Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     28-MAY-1993 (RFWS):
*        Original version.
*     8-SEP-1993 (RFWS):
*        Changed name. Also allow up to 2 leap seconds in any minute
*        (to accommodate times returned from the C run time library).
*     23-JAN-2009 (DSB):
*        Refer to NDF1_TIME rather than NDF1_GTIME in comments.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      INTEGER YMDHM( 5 )
      REAL SEC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DAYS( 12 )         ! Days in each month
      INTEGER MXDAY              ! Maximum day number in month

*  Local Data:
      DATA DAYS / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the month field.
      IF ( ( YMDHM( 2 ) .LT. 1 ) .OR.
     :     ( YMDHM( 2 ) .GT. 12 ) ) THEN
         STATUS = NDF__DTMIN
         CALL MSG_SETI( 'MONTH', YMDHM( 2 ) )
         CALL ERR_REP( 'NDF1_VDAT_MON',
     :                 'Error in date/time specification; invalid ' //
     :                 'month number ^MONTH encountered.', STATUS )

*  Extract the number of days in the month, omitting February.
      ELSE
         IF ( YMDHM( 2 ) .NE. 2 ) THEN
            MXDAY = DAYS( YMDHM( 2 ) )

*  If the month is February, then see if it is a leap year and set the
*  number of days accordingly.
         ELSE
            MXDAY = 28
            IF( ( MOD( YMDHM( 1 ), 4 ) .EQ. 0 ) .AND.
     :          ( ( MOD( YMDHM( 1 ), 100 ) .NE. 0 ) .OR.
     :            ( MOD( YMDHM( 1 ), 400 ) .EQ. 0 ) ) ) MXDAY = 29
         END IF

*  Validate the day field.
         IF ( ( YMDHM( 3 ) .LT. 1 ) .OR.
     :        ( YMDHM( 3 ) .GT. MXDAY ) ) THEN
            STATUS = NDF__DTMIN
            CALL MSG_SETI( 'DAY', YMDHM( 3 ) )
            CALL ERR_REP( 'NDF1_VDAT_DAY',
     :                    'Error in date/time specification; ' //
     :                    'invalid day number ^DAY encountered.',
     :                    STATUS )

*  Validate the hour field.
         ELSE IF ( ( YMDHM( 4 ) .LT. 0 ) .OR.
     :             ( YMDHM( 4 ) .GT. 23 ) ) THEN
            STATUS = NDF__DTMIN
            CALL MSG_SETI( 'HOUR', YMDHM( 4 ) )
            CALL ERR_REP( 'NDF1_VDAT_HR',
     :                    'Error in date/time specification; ' //
     :                    'invalid hour number ^HOUR encountered.',
     :                    STATUS )

*  Validate the minute field.
         ELSE IF ( ( YMDHM( 5 ) .LT. 0 ) .OR.
     :             ( YMDHM( 5 ) .GT. 59 ) ) THEN
            STATUS = NDF__DTMIN
            CALL MSG_SETI( 'MIN', YMDHM( 5 ) )
            CALL ERR_REP( 'NDF1_VDAT_MIN',
     :                    'Error in date/time specification; ' //
     :                    'invalid minute number ^MIN encountered.',
     :                    STATUS )

*  Validate the second field (allow for up to 2 leap seconds, since
*  these can in principle be returned by the NDF1_TIME function as a
*  result of calling the ANSI C run time library date/time functions -
*  see the ANSI C standard for details).
         ELSE IF ( ( SEC .LT. 0.0 ) .OR.
     :             ( SEC .GT. 61.0 ) ) THEN
            STATUS = NDF__DTMIN
            CALL MSG_SETR( 'SEC', SEC )
            CALL ERR_REP( 'NDF1_VDAT_SEC',
     :                    'Error in date/time specification; ' //
     :                    'invalid seconds value ^SEC encountered.',
     :                    STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VDAT', STATUS )

      END
