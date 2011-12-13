      SUBROUTINE COF_WHISR( NDF, FUNIT, STATUS )
*+
*  Name:
*     COF_WHISR

*  Purpose:
*     Appends NDF history records to the current FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_WHISR( NDF, FUNIT, STATUS )

*  Description:
*     This appends all the NDF HISTORY records in an easy-to-read format
*     to the current FITS header.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF whose HISTORY records are to be
*        written to the FITSD headers.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The NDF and the FITS file must already be open.  The current
*     HDU in the FITS file should be the primary and the other headers
*     should have been written.

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
*     1997 January 13 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ public constants
      INCLUDE 'PRM_PAR'          ! VAL__ public constants

*  Global Variables:
      INCLUDE 'COF_CMN'          ! Common block for passing required
                                 ! additional arguments
*        FFUNIT = INTEGER (Write)
*           The Fortran logical unit number of the FITS file (equals
*           FUNIT).

*  Arguments Given:
      INTEGER NDF
      INTEGER FUNIT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL COF_HECHO         ! Write history text to FITS

*  Local Constants:
      INTEGER   FITSOK           ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      CHARACTER * ( NDF__SZAPP ) APPN ! Application name
      CHARACTER * ( 80 ) CARD    ! FITS header card
      CHARACTER * ( NDF__SZHDT ) CREATD ! History creation date
      INTEGER CURREC             ! Current record number
      CHARACTER * ( NDF__SZHDT ) DATE ! History record date
      CHARACTER * ( 4 ) FMT      ! Format string
      CHARACTER * ( 3 ) FMTL     ! A-format length for update mode
      INTEGER FSTAT              ! FITSIO status
      INTEGER HLEN               ! Number of characters in history
      CHARACTER * ( 70 ) HISTRY  ! FITS header card
      CHARACTER * ( NDF__SZHST ) HOST ! Host machine
      INTEGER IREC               ! Loop counter for history records
      CHARACTER * ( NDF__SZHUM ) MODE ! Update mode
      INTEGER NC                 ! Number of characters in update-mode
                                 ! format
      INTEGER NREC               ! Number of history records
      CHARACTER * ( NDF__SZREF ) REFER ! Reference dataset
      CHARACTER * ( NDF__SZUSR ) USER ! Username
      CHARACTER * ( VAL__SZI ) WIDTH ! Width in characters of history
                                 ! text

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the unit number argument to a global variable.
      FFUNIT = FUNIT

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Write a blank header.
      CARD = ' '
      CALL FTPREC( FUNIT, CARD, FSTAT )

*  Obtain the creation date.  Convert its format to the IAU style.
      CALL NDF_HINFO( NDF, 'CREATED', 0, CREATD, STATUS )
      CALL COF_HDATE( CREATD, STATUS )

*  Complete the heading.
      CALL MSG_SETC( 'CREATD', CREATD )
      CALL MSG_LOAD( ' ', 'History structure created ^CREATD', HISTRY,
     :               HLEN, STATUS )
      CALL FTPHIS( FUNIT, HISTRY, FSTAT )

*  Obtain the update mode of the history.
      CALL NDF_HINFO( NDF, 'MODE', 0, MODE, STATUS )

*  Obtain the current record of the history.
      CALL NDF_HNREC( NDF, CURREC, STATUS )

*  Report it below the heading as it is a global parameter of the
*  history.
      CALL CHR_ITOC( NDF__SZHUM, FMTL, NC )
      FMT = 'A'//FMTL( :NC )
      CALL MSG_FMTC( 'MODE', FMT, MODE )
      CALL MSG_SETI( 'CR', CURREC )
      CALL MSG_LOAD( ' ', 'Update mode: ^MODE   Current record: ^CR ',
     :               HISTRY, HLEN, STATUS )
      CALL FTPHIS( FUNIT, HISTRY, FSTAT )
      CALL FTPREC( FUNIT, CARD, FSTAT )

*  Determine how many history records there are.
      CALL NDF_HNREC( NDF, NREC, STATUS )

*  Loop to display each record.
      IF ( STATUS .EQ. SAI__OK ) THEN
         FFUNIT = FUNIT
         DO IREC = 1, NREC

*  Obtain the date and application name for each record.
            CALL NDF_HINFO( NDF, 'DATE', IREC, DATE, STATUS )
            CALL NDF_HINFO( NDF, 'APPLICATION', IREC, APPN, STATUS )

*  Convert the date format to IAU style.
            CALL COF_HDATE( DATE, STATUS )

*  Write these to the FITS header.
            CALL MSG_SETI( 'IREC', IREC )
            CALL MSG_SETC( 'DATE', DATE )
            CALL MSG_SETC( 'APPN', APPN )
            CALL MSG_LOAD( ' ', '^IREC: ^DATE - ^APPN', HISTRY, HLEN,
     :                     STATUS )
            CALL FTPHIS( FUNIT, HISTRY, FSTAT )

*  Obtain the username, host machine, and text width for each record.
            CALL NDF_HINFO( NDF, 'USER', IREC, USER, STATUS )
            CALL NDF_HINFO( NDF, 'HOST', IREC, HOST, STATUS )
            CALL NDF_HINFO( NDF, 'WIDTH', IREC, WIDTH, STATUS )

*  Write these to the FITS header.
            CALL MSG_SETC( 'USER', USER )
            CALL MSG_SETC( 'HOST', HOST )
            CALL MSG_SETC( 'WIDTH', WIDTH )
            CALL MSG_LOAD( ' ', 'User: ^USER   Host: ^HOST  Width: '/
     :                     /'^WIDTH', HISTRY, HLEN, STATUS )
            CALL FTPHIS( FUNIT, HISTRY, FSTAT )

*  Obtain the name of the dataset.
            CALL NDF_HINFO( NDF, 'REFERENCE', IREC, REFER, STATUS )

*  Write these to the FITS header.
            CALL MSG_SETC( 'REFER', REFER )
            CALL MSG_LOAD( ' ', 'Dataset: ^REFER', HISTRY, HLEN,
     :                     STATUS )
            CALL FTPHIS( FUNIT, HISTRY, FSTAT )

*  Write the associated history text to the headers.
            CALL NDF_HOUT( NDF, IREC, COF_HECHO, STATUS )

*  Write a blank header.
            CALL FTPREC( FUNIT, CARD, FSTAT )
         END DO
      END IF

*  Check for an error.  Handle a bad status.  Negative values are
*  reserved for non-fatal warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_WHISR_ERR', 'FTPHIS',
     :                   'Error writing to HISTORY header card.',
     :                   STATUS )
      END IF

      END
