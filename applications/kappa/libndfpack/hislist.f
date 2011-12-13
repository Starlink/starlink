      SUBROUTINE HISLIST( STATUS )
*+
*  Name:
*     HISLIST

*  Purpose:
*     Lists NDF history records.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL HISLIST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This lists all the history records in an NDF.  The reported
*     information comprises the date, time, and application name,
*     and optionally the history text.

*  Usage:
*     hislist ndf

*  ADAM Parameters:
*     BRIEF = _LOGICAL (Read)
*        This controls whether a summary or the full history information
*        is reported.  BRIEF=TRUE requests that only the date and
*        application name in each history record is listed.  BRIEF=FALSE
*        causes the task to report the history text in addition.
*        [FALSE]
*     NDF = NDF (Read)
*        The NDF whose history information is to be reported.

*  Examples:
*     hislist vcc953
*        This lists the full history information for the NDF called
*        vcc935.  The information comprises the names of the
*        applications and the times they were used, and the associated
*        history text.
*     hislist vcc953 brief
*        This gives a summary of the history information for the NDF
*        called vcc935.  It comprises the names of the applications
*        and the times they were used.

*  Related Applications:
*     KAPPA: HISCOM, HISSET, NDFTRACE.

*  Implementation Deficiencies:
*     There is no facility to list only certain history records, say by
*     time or application name.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F.Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 June 2 (RFWS):
*        Original version.
*     1995 June 24 (MJC):
*        Added the documentation.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL NDF_HECHO         ! Display history text

*  Local Variables:
      CHARACTER * ( NDF__SZAPP ) APPN ! Application name
      LOGICAL BRIEF              ! Produce brief listing?
      CHARACTER * ( NDF__SZHDT ) CREATD ! History creation date
      CHARACTER * ( NDF__SZHDT ) DATE ! History record date
      INTEGER INDF               ! NDF identifier
      INTEGER IREC               ! Loop counter for history records
      INTEGER NREC               ! Number of history records

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF.
      CALL LPG_ASSOC( 'NDF', 'READ', INDF, STATUS )

*  See if a brief history listing is required.
      CALL PAR_GET0L( 'BRIEF', BRIEF, STATUS )

*  Display a heading.
      CALL MSG_BLANK( STATUS )
      CALL NDF_MSG( 'NDF', INDF )
      CALL MSG_OUT( ' ', '   History listing for NDF structure ^NDF:',
     :              STATUS )
      CALL MSG_BLANK( STATUS )


*  Obtain the creation date.  Convert its format to the KAPPA style.
      CALL NDF_HINFO( INDF, 'CREATED', 0, CREATD, STATUS )
      CALL KPG1_FHDAT( CREATD, STATUS )

*  Complete the heading.
      CALL MSG_SETC( 'CREATD', CREATD )
      CALL MSG_OUT( ' ', '   History structure created ^CREATD',
     :              STATUS )
      CALL MSG_BLANK( STATUS )

*  Determine how many history records there are.
      CALL NDF_HNREC( INDF, NREC, STATUS )

*  Loop to display each record.
      IF ( STATUS .EQ. SAI__OK ) THEN
         DO IREC = 1, NREC

*  Obtain the date and application name for each record.
            CALL NDF_HINFO( INDF, 'DATE', IREC, DATE, STATUS )
            CALL NDF_HINFO( INDF, 'APPLICATION', IREC, APPN, STATUS )

*  Convert the date format to KAPPA style.
            CALL KPG1_FHDAT( DATE, STATUS )

*  Display these.  Ignore the decimal seconds in the brief listing.
            CALL MSG_SETI( 'IREC', IREC )
            IF ( .NOT. BRIEF ) THEN
               CALL MSG_SETC( 'DATE', DATE )
            ELSE
               CALL MSG_SETC( 'DATE', DATE( : 20 ) )
            END IF
            CALL MSG_SETC( 'APPN', APPN )
            CALL MSG_OUT( ' ', '^IREC: ^DATE - ^APPN', STATUS )

*  Display the associated history text.
            IF ( .NOT. BRIEF ) THEN
               CALL MSG_BLANK( STATUS )
               CALL NDF_HOUT( INDF, IREC, NDF_HECHO, STATUS )
               CALL MSG_BLANK( STATUS )
            END IF
         END DO

*  Ensure a final blank line.
         IF ( BRIEF ) CALL MSG_BLANK( STATUS )
      END IF

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'HISLIST_ERR',
     :     'HISLIST: Error listing NDF history information.', STATUS )
      END IF

      END
