      SUBROUTINE NDF1_CHTIM( DATE, MJD, STATUS )
*+
*  Name:
*     NDF1_CHTIM

*  Purpose:
*     Validate a date/time string and return the corresponding MJD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CHTIM( DATE, MJD, STATUS )

*  Description:
*     The routine attempts to convert the supplied date/time string
*     into a Modified Julian Date. An error is reported if this fails.

*  Arguments:
*     DATE  = CHARACTER * ( * ) (Given)
*        The date/time string to be validated. See "Date and Time
*         Formats:" below.
*     MJD = DOUBLE PRECISION (Returned)
*        The corresponding MJD, or AST__BAD if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Date and Time Formats:
*     The formats accepted for the "DATE" argument are listed
*     below. They are all case-insensitive and are generally tolerant
*     of extra white space and alternative field delimiters:
*
*     - Gregorian Calendar Date: With the month expressed either as an
*     integer or a 3-character abbreviation, and with optional decimal
*     places to represent a fraction of a day ("1996-10-2" or
*     "1996-Oct-2.6" for example). If no fractional part of a day is
*     given, the time refers to the start of the day (zero hours).
*
*     - Gregorian Date and Time: Any calendar date (as above) but with
*     a fraction of a day expressed as hours, minutes and seconds
*     ("1996-Oct-2 12:13:56.985" for example). The date and time can be
*     separated by a space or by a "T" (as used by ISO8601 format).
*
*     - Modified Julian Date: With or without decimal places
*     ("MJD 54321.4" for example).
*
*     - Julian Date: With or without decimal places ("JD 2454321.9" for
*     example).
*
*     - Besselian Epoch: Expressed in decimal years, with or without
*     decimal places ("B1950" or "B1976.13" for example).
*
*     - Julian Epoch: Expressed in decimal years, with or without
*     decimal places ("J2000" or "J2100.9" for example).
*
*     - Year: Decimal years, with or without decimal places ("1996.8"
*     for example).  Such values are interpreted as a Besselian epoch
*     (see above) if less than 1984.0 and as a Julian epoch otherwise.

*  Copyright:
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
*     DSB: David S Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-JAN-2009 (DSB):
*        Original version.
*     03-FEB-2009 (TIMJ):
*        Fix token name for DATE
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST functions and constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  External References:
      INTEGER CHR_LEN            ! Used string length

*  Arguments Given:
      CHARACTER * ( * ) DATE

*  Arguments Returned:
      DOUBLE PRECISION MJD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TFRM               ! Pointer to AST TimeFrame
      INTEGER NC                 ! Number of DATE characters used

*.

*  Initialise
      MJD = AST__BAD

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create an AST TimeFrame that uses MJD to describe moment sin time.
      TFRM = AST_TIMEFRAME( 'System=MJD', STATUS )

*  Use this TimeFrame to parse the supplied string, returning the
*  corresponding MJD if succesful.
      NC = AST_UNFORMAT( TFRM, 1, DATE, MJD, STATUS )

*  Check the whole string was used. If not, reset the MJD value to
*  AST__BAD and report an error.
      IF( NC .EQ. 0 .OR. NC .LT. CHR_LEN( DATE ) ) THEN
         MJD = AST__BAD
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = NDF__BDTIM
            CALL MSG_SETC( 'DATE', DATE )
            CALL ERR_REP( 'NDF_HSDAT_INV', 'Invalid date/time string '//
     :                    '''^DATE'' specified (possible programming '//
     :                    'error).', STATUS )
         END IF
      END IF

*  Free the TimeFrame.
      CALL AST_ANNUL( TFRM, STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CHTIM', STATUS )

      END
