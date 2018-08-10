      SUBROUTINE NDF1_FMHDT( YMDHM, SEC, STR, STATUS )
*+
*  Name:
*     NDF1_FMHDT

*  Purpose:
*     Format a date and time as a string in standard history format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_FMHDT( YMDHM, SEC, STR, STATUS )

*  Description:
*     The routine formats a date and time into the standard format for
*     recording date/time information in NDF history records, for
*     example: '1993-MAY-25 02:32:53.152'.

*  Arguments:
*     YMDHM( 5 ) = INTEGER (Given)
*        The year, month, day, hour and minute fields of the date and
*        time (in that order), stored as integers.
*     SEC = REAL (Given)
*        The seconds field.
*     STR = CHARACTER * ( * ) (Returned)
*        The formatted date/time string. A character variable with a
*        length of at least NDF__SZHDT characters should be provided to
*        hold this result.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     {enter_new_authors_here}

*  History:
*     25-MAY-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Global Variables:
      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_FIXDT = LOGICAL (Read)
*           Use a fixed date and time in new history records?

*  Arguments Given:
      INTEGER YMDHM( 5 )
      REAL SEC

*  Arguments Returned:
      CHARACTER * ( * ) STR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) MNAME( 12 ) ! Month name abbreviations
      CHARACTER * ( NDF__SZHDT ) BUF ! Local buffer to hold result
      INTEGER I                  ! Loop counter for characters

*  Local Data:
      DATA MNAME / 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     :             'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the FIXDT tuning flag indicates that we are to use a fixed date and
*  time in place of the real date and time, then just return an arbitrary
*  (but fixed) string. This is intended to facilitate regression testing,
*  where a change in date/time could cause tests to fail.
      IF( TCB_FIXDT ) THEN
         STR = '10-AUG-2018 11:00:00'
         RETURN
      END IF

*  Format the data and time as a character string using the defined
*  standard history record format.
      WRITE( BUF, 9000 ) YMDHM( 1 ), MNAME( YMDHM( 2 ) ), YMDHM( 3 ),
     :                   YMDHM( 4 ), YMDHM( 5 ), SEC
 9000 FORMAT( I4, '-', A3, '-', I2, ' ', I2, ':', I2, ':', F6.3 )

*  Replace any leading blank characters in the resulting string with
*  zeros. Then restore the blank between the date and time fields.
      DO 1 I = 1, NDF__SZHDT
         IF ( BUF( I : I ) .EQ. ' ' ) BUF( I : I ) = '0'
 1    CONTINUE
      BUF( 12 : 12 ) = ' '

*  Return the result.
      CALL NDF1_CCPY( BUF, STR, STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_FMHDT', STATUS )

      END
