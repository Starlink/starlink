      SUBROUTINE COF_LWS2( FUNIT, NDF, XL, XU, YL, YU, WADIM, SCL,
     :                     SCU, NOBS, NCOL, X, Y, WAVEL, SCOUNT, DATA,
     :                     ERROR, QUALITY, USED, FLUX, STDDEV, DETID,
     :                     SCADIR, STATUS )
*+
*  Name:
*     COF_LWS2

*  Purpose:
*     Assigns values to the NDF array components for LWS AN data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_LWS2( FUNIT, NDF, XL, XU, YL, YU, WADIM, SCL, SCU, NOBS,
*                    NCOL, X, Y, WAVEL, SCOUNT, DATA, ERROR, QUALITY,
*                    USED, FLUX, STDDEV, DETID, SCADIR, STATUS )

*  Description:
*     This routine assigns values from an LWS AN data product into a
*     four-dimensional NDF's array components and its units.  It uses
*     supplied pixel indices along each dimension for each of the
*     observations, and obtains the relevant columns from the binary
*     table, in order to fill the data, error, and quality arrays.
*     The quality array stores the detector identification in the
*     first four bits, and the scan direction in its fifth bit.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        Logical-unit number of the FITS file.
*     NDF = INTEGER (Given)
*        The NDF identifier of the output NDF.
*     XL = INTEGER (Given)
*        The first (x) lower bound of the NDF arrays.
*     XU = INTEGER (Given)
*        The first (x) upper bound of the NDF arrays.
*     YL = INTEGER (Given)
*        The second (y) lower bound of the NDF arrays.
*     YU = INTEGER (Given)
*        The second (y) upper bound of the NDF arrays.
*     WADIM = INTEGER (Given)
*        The third (wavelength) dimension of the NDF arrays.  By
*        definition its lower bound is 1, hence there is no need to have
*        two arguments.
*     SCL = INTEGER (Given)
*        The fourth (scan counter) lower bound of the NDF arrays.
*     SCU = INTEGER (Given)
*        The fourth (scan counter) upper bound of the NDF arrays.
*     NOBS = INTEGER (Given)
*        Number of observations in the binary table.
*     NCOL = INTEGER (Given)
*        Number of columns in the binary table (and dimension of USED).
*     X( NOBS ) = INTEGER (Given)
*        The X pixel indices for each of the observations.
*     Y( NOBS ) = INTEGER (Given)
*        The Y pixel indices for each of the observations.
*     WAVEL( NOBS ) = REAL (Given)
*        The wavelengths for each of the observations.  Note that
*        this is floating point.
*     SCOUNT( NOBS ) = INTEGER (Given)
*        The scan counter for each of the observations.
*     DATA( XL:XU, YL:YU, WADIM, SCL:SCU ) = REAL (Given and Returned)
*        The NDF data array.  On input it is filled with the bad value.
*        On exit it contains the LWS flux values.  These may not fill
*        the array, hence the need to initialise with bad values.
*     ERROR( XL:XU, YL:YU, WADIM, SCL:SCU ) = REAL (Given and Returned)
*        The NDF error array.  On input it is filled with the bad value.
*        On exit it contains the LWS flux-uncertainty values.  These
*        may not fill the array, hence the need to initialise with bad
*        values.
*     QUALITY( XL:XU, YL:YU, WADIM, SCL:SCU) = BYTE (Given and Returned)
*        The NDF quality array.  On input it is filled with the bad
*        value.  On exit it contains the LWS detector identification in
*        the four four bits, and the scan direction in bit 5.  These
*        may not fill the array, hence the need to initialise with bad
*        values.
*     USED( NCOL ) = INTEGER (Given and Returned)
*        A set of flags that indicate whether or not a column has been
*        used to define some attribute of the NDF.  Those columns
*        accessed in this routine will be flagged .TRUE. in this array.
*     FLUX( NOBS ) = REAL (Returned)
*        The flux for each of the observations.
*     STTDEV( NOBS ) = REAL (Returned)
*        The uncertainty in the flux for each of the observations.
*     DETID( NOBS ) = INTEGER (Returned)
*        The detector identification flux for each of the observations.
*     SCADIR( NOBS ) = INTEGER (Returned)
*        The scan direction for each of the observations.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     1996 April 30 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER FUNIT
      INTEGER NDF
      INTEGER XL
      INTEGER XU
      INTEGER YL
      INTEGER YU
      INTEGER WADIM
      INTEGER SCL
      INTEGER SCU
      INTEGER NOBS
      INTEGER NCOL
      INTEGER X( NOBS )
      INTEGER Y( NOBS )
      REAL WAVEL( NOBS )
      INTEGER SCOUNT( NOBS )

*  Arguments Given and Returned:
      REAL DATA( XL:XU, YL:YU, WADIM, SCL:SCU )
      REAL ERROR( XL:XU, YL:YU, WADIM, SCL:SCU )
      BYTE QUALITY( XL:XU, YL:YU, WADIM, SCL:SCU )
      LOGICAL USED( NCOL )

*  Arguments Returned:
      REAL FLUX( NOBS )
      REAL STDDEV( NOBS )
      INTEGER DETID( NOBS )
      INTEGER SCADIR( NOBS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      LOGICAL BAD                ! Column array contains bad values?
      INTEGER COLNUM             ! Column number in the binary table
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      INTEGER FSTAT              ! FITSIO status
      INTEGER I                  ! Loop for the observations
      CHARACTER * ( 8 ) KEYWRD   ! FITS header keyword
      LOGICAL THERE              ! Header keyword is present?
      CHARACTER * ( 72 ) UNITS   ! Units of the data array
      INTEGER WAVI               ! Wavelength-axis pixel index

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Obtain the column of fluxes and store its units and label in the NDF.
*  =====================================================================

*  Find the column number of the flux.
      CALL FTGCNO( FUNIT, .FALSE., 'LSANFLX', COLNUM, FSTAT )

*  Read the column into the flux array.
      CALL FTGCVE( FUNIT, COLNUM, 1, 1, NOBS, VAL__BADR, FLUX, BAD,
     :             FSTAT )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_LWS_RDDAT', 'FTGCVE',
     :     'Error reading the column of LWS fluxes.', STATUS )
         GOTO 999
      END IF

*  Obtain the units for this column, and place it into the NDF.  To do
*  this first form the names of the units keyword for this column.
      CALL FTKEYN( 'TUNIT', COLNUM, KEYWRD, FSTAT )
      CALL COF_GKEYC( FUNIT, KEYWRD, THERE, UNITS, COMENT, STATUS )
      IF ( THERE ) CALL NDF_CPUT( UNITS, NDF, 'Units', STATUS )

*  Set the label for the NDF.
      CALL NDF_CPUT( 'Flux', NDF, 'Label', STATUS )

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the column of standard deviations.
*  =========================================

*  Find the column number of the errors.
      CALL FTGCNO( FUNIT, .FALSE., 'LSANFLXU', COLNUM, FSTAT )

*  Read the column into the flux-error array.
      CALL FTGCVE( FUNIT, COLNUM, 1, 1, NOBS, VAL__BADR, STDDEV, BAD,
     :             FSTAT )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_LWS_RDVAR', 'FTGCVE',
     :     'Error reading the column of LWS standard deviations.',
     :     STATUS )
         GOTO 999
      END IF

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the column of detector numbers.
*  ======================================

*  Find the column number of the detector numbers.
      CALL FTGCNO( FUNIT, .FALSE., 'LSANDET', COLNUM, FSTAT )

*  Read the column into the detector id array.  Note that there are no
*  bad values.  This assumes that the detector number does not exceed
*  223 (255-32), although the data type is nominally J (_INTEGER).
      CALL FTGCVJ( FUNIT, COLNUM, 1, 1, NOBS, 0, DETID, BAD, FSTAT )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_LWS_RDDID', 'FTGCVB',
     :     'Error accessing the LWS detector numbers.', STATUS )
         GOTO 999
      END IF

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the column of scan directions.
*  =====================================

*  Find the column number of the scan directions.
      CALL FTGCNO( FUNIT, .FALSE., 'LSANSDIR', COLNUM, FSTAT )

*  Read the column into the direction array.  This routine assumes that
*  the scan direction is binary, although the data type is nominally J
*  (_INTEGER).
      CALL FTGCVJ( FUNIT, COLNUM, 1, 1, NOBS, 0, SCADIR, BAD, FSTAT )

*  Check that the transfer was correct.
      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_LWS_RDSDA', 'FTGCVB',
     :     'Error accessing the LWS scan directions.', STATUS )
         GOTO 999
      END IF

*  Record that this column is used.
      USED( COLNUM ) = .TRUE.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Loop through each observation.
*  ==============================

      DO I = 1, NOBS

*  Find the nearest pixel index for the wavelength.
         WAVI = NINT( WAVEL( I ) )

*  Store the data and error values.  Protect against negative flux
*  errors, as found in the test data file.
         DATA( X( I ), Y( I ), WAVI, SCOUNT( I ) ) = FLUX( I )
         ERROR( X( I ), Y( I ), WAVI, SCOUNT( I ) ) = ABS( STDDEV( I ) )

*  Store the quality.  This uses the first 4 bits for the detector
*  identification and the fifth for the scan direction.
         QUALITY( X( I ), Y( I ), WAVI, SCOUNT( I ) ) = DETID( I ) +
     :                                                  32 * SCADIR( I )

      END DO

  999 CONTINUE

      END
