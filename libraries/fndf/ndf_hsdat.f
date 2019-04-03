      SUBROUTINE NDF_HSDAT( DATE, INDF, STATUS )
*+
*  Name:
*     NDF_HSDAT

*  Purpose:
*     Set the history date for an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HSDAT( DATE, INDF, STATUS )

*  Description:
*     The routine sets the date and time that will be used for subsequent
*     history records added to an NDF (both default history records and
*     those added using NDF_HPUT). If no date and time is set using
*     this routine, then the curent date and time will be used. Any
*     date and time established by a previous call to this function can be
*     removed by supplying a blank value for argument "DATE", in which
*     case the current date and time will be used for subsequent history
*     records.

*  Arguments:
*     DATE = CHARACTER * ( * ) (Given)
*        The time and date to be used for subsequent history records, or
*        blank to re-establish the default behaviour (i.e. to use the
*        current time). The allowed formats are described later in the
*        "Date and Time Formats:" section.
*     INDF = INTEGER (Given)
*        NDF identifier.
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
*     {enter_new_authors_here}

*  History:
*     23-JAN-1993 (SDSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_CONST'        ! NDF private constants
      INCLUDE 'NDF_PAR'          ! NDF public constants
      INCLUDE 'DAT_PAR'          ! HDS public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HTIME( NDF__MXDCB ) = DOUBLE PRECISION (Write)
*           The date/time to attach to the next history record to be
*           created, as a UTC Modified Julian Date. If negative, then
*           the current time will be used.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) DATE
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION MJD       ! Supplied date/time as an MJD
      INTEGER IACB               ! Index of NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that WRITE access to the NDF is available.
         CALL NDF1_CHACC( IACB, 'WRITE', STATUS )

*  Validate the supplied date/time string and convert to an MJD. If the
*  string is blank, use a negative MJD to indicate that history records
*  should be date-stamped with the current time.
         IF( DATE .NE. ' ' ) THEN
            CALL NDF1_CHTIM( DATE, MJD, STATUS )
         ELSE
            MJD = -1.0D0
         END IF

*  Obtain an index to the data object entry in the DCB and ensure that
*  DCB history information is available.
         IDCB = ACB_IDCB( IACB )
         CALL NDF1_DH( IDCB, STATUS )

*  If no error has occurred, store the MJD in the DCB.
         IF ( STATUS .EQ. SAI__OK ) DCB_HTIME( IDCB ) = MJD
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HSDAT_ERR', 'NDF_HSDAT: Error setting '//
     :                 'the history date for an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_HSDAT', STATUS )
      END IF

      END
