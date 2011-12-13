      SUBROUTINE KPS1_FSWPE( NDFI, XMIN, XMAX, YMIN, YMAX, NXPAR, NYPAR,
     :                       MCOEF, CHCOEF, VARIAN, RSMAX, RMS, COSYS,
     :                       STATUS )
*+
*  Name:
*     KPS1_FSWPE

*  Purpose:
*     Writes the SURFACEFIT.FIT NDF extension for storing Chebyshev fit
*     coefficients.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL KPS1_FSWPE( NDFI, XMIN, XMAX, YMIN, YMAX, NXPAR, NYPAR,
*                       MCOEF, CHCOEF, VARIAN, RSMAX, RMS, COSYS,
*                       STATUS )

*  Description:
*     This routine creates a SURFACEFIT extension in a supplied NDF.
*     Within in that in makes a FIT structure of type POLYNOMIAL,
*     variant CHEBYSHEV, for storing the fit coefficients; plus RMS,
*     RESIDMAX, and COSYS objects for the rms difference of the fit,
*     the maximum residual, and the co-ordinate system respectively.

*  Arguments:
*     NDFI = INTEGER (Given)
*        The identifier of the NDF to contain the SURFACEFIT extension.
*     XMIN = DOUBLE PRECISION (Given)
*        Lower end of the x range of the fit.  It must not be greater
*        than the x position of the first pixel in the data array.
*     YMIN = DOUBLE PRECISION (Given)
*        Lower end of the y range of the fit.  It must not be greater
*        than the y position of the first pixel in the data array.
*     XMAX = DOUBLE PRECISION (Given)
*        Upper end of the x range of the fit.  It must not be less
*        than the x position of the last pixel in the data array.
*     YMAX = DOUBLE PRECISION (Given)
*        Lower end of the y range of the fit.  It must not be less
*        than the y position of the last pixel in the data array.
*     NXPAR = INTEGER (Given)
*        The number of parameters of the FIT in the x direction, i.e
*        the degree of the polynomial plus one.
*     NYPAR = INTEGER (Given)
*        The number of parameters of the FIT in the y direction, i.e
*        the degree of the polynomial plus one.
*     MCOEF = INTEGER (Given)
*        The dimension of the array of Chebyshev coefficients.
*     CHCOEF( MCOEF ) = DOUBLE PRECISION (Given)
*        The Chebyshev polynomial coefficients, in the order increasing
*        x power for each increasing y power.  Thus coefficient Aij in
*        the standard convention is %CHCOEF(i*(%NYPAR)+j+1).  The array
*        may be rectangular, i.e. the highest x and y orders do not
*        have to be the same.
*     VARIAN( MCOEF ) = DOUBLE PRECISION (Given)
*        The variance of the Chebyshev coefficients.
*     RSMAX = REAL (Given)
*        The maximum residual.
*     RMS = REAL (Given)
*        The rms difference between the raw array and the fitted array.
*     COSYS = CHARACTER * ( * ) (Given)
*        The co-ordinate system.  Originally 'Data' or 'World', but
*        probably better to supply the AST Domain.  The current AST
*        Frame identifier would be better.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     2007 June 28 (MJC):
*        Original version based upon code in FITSURFACE from 1993.
*     2007 June 29 (MJC):
*        Remove unused NCOEF argument.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given:
      INTEGER NDFI
      DOUBLE PRECISION XMIN
      DOUBLE PRECISION XMAX
      DOUBLE PRECISION YMIN
      DOUBLE PRECISION YMAX
      INTEGER NXPAR
      INTEGER NYPAR
      INTEGER MCOEF
      DOUBLE PRECISION CHCOEF( MCOEF )
      DOUBLE PRECISION VARIAN( MCOEF )
      REAL RSMAX
      REAL RMS
      CHARACTER*(*) COSYS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXPAR              ! Maximum number of parameters which
                                 ! can be handled in each direction
      PARAMETER ( MXPAR = 15 )

*  Local Variables:
      CHARACTER*( DAT__SZLOC ) FLOC ! Locator to FIT polynomial
                                 ! structure
      DOUBLE PRECISION WORK( MXPAR, MXPAR ) ! Workspace for flipped
                                 ! polynomial coefficients
      CHARACTER*( DAT__SZLOC ) XLOC ! Locator to SURFACEFIT extension

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the results to an extension named SURFACEFIT.  The
*  coefficients will be stored in a structure within this called FIT.
*  FIT has type POLYNOMIAL (see SGP/38 for a description of the
*  contents of a POLYNOMIAL structure).
      CALL NDF_XNEW( NDFI, 'SURFACEFIT', 'EXT', 0, 0, XLOC, STATUS )

*  Check which type of surface fit is required.
      CALL DAT_NEW( XLOC, 'FIT', 'POLYNOMIAL', 0, 0, STATUS )
      CALL DAT_FIND( XLOC, 'FIT', FLOC, STATUS )

      CALL KPG1_PL2PU( FLOC, 'CHEBYSHEV', NXPAR, NYPAR, .TRUE., XMIN,
     :                 XMAX, YMIN, YMAX, CHCOEF, VARIAN, WORK, STATUS )

*  In addition to the coefficients, write the RMS error, the maximum
*  residual to the SURFACEFIT extension, and the co-ordinate system.
      CALL NDF_XPT0R( RMS, NDFI, 'SURFACEFIT', 'RMS', STATUS )
      CALL NDF_XPT0R( RSMAX, NDFI, 'SURFACEFIT', 'RESIDMAX', STATUS )
      CALL NDF_XPT0C( COSYS, NDFI, 'SURFACEFIT', 'COSYS', STATUS )

      END
