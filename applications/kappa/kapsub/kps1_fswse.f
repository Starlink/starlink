      SUBROUTINE KPS1_FSWSE( NDFI, NXKNOT, NYKNOT, XKNOT, YKNOT, NCOEF,
     :                       COEFF, SCALE, RSMAX, RMS, COSYS, STATUS )
*+
*  Name:
*     KPS1_FSW

*  Purpose:
*     Writes the SURFACEFIT.FIT NDF extension for storing B-spline fit
*     coefficients.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL KPS1_FSWSE( NDFI, NXKNOT, NYKNOT, XKNOT, YKNOT, NCOEF,
*                       COEFF, SCALE, RSMAX, RMS, COSYS, STATUS )

*  Description:
*     This routine creates a SURFACEFIT extension in a supplied NDF.
*     Within in that in makes a FIT structure of type POLYNOMIAL,
*     variant BSPLINE, for storing the fit coefficients; plus RMS,
*     RESIDMAX, and COSYS objects for the rms difference of the fit,
*     the maximum residual, and the co-ordinate system respectively.

*  Arguments:
*     NDFI = INTEGER (Given)
*        The identifier of the NDF to contain the SURFACEFIT extension.
*     NXKNOT = INTEGER (Given)
*        The number of interior knots in the x direction.
*     NYKNOT = INTEGER (Given)
*        The number of interior knots in the y direction.
*     XKNOT( NXKNOT+8 ) = REAL (Given)
*        The x positions of complete set of knots associated with x.
*     YKNOT( NYKNOT+8 ) = REAL (Given)
*        The y positions of complete set of knots associated with y.
*     NCOEF = INTEGER (Given)
*        The number of bi-cubic coefficients.  Must equal (%NXKNOT+4) *
*        (%NYKNOT+4).
*     COEFF( NCOEF ) = REAL (Given)
*        The bi-cubic B-spline coefficients, defined at the knots in the
*        order increasing x knot, then increasing y.  Thus coefficient
*        Cij in the standard convention is %COEFF((i-1)*(%NYKNOT+8)+j).
*     SCALE = REAL (Returned)
*        The scale factor applied to the data values before calculating
*        the spline.
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
*     Copyright (C) 2007, 2009 Science & Technology Facilities Council.
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
*     2007 July 3 (MJC):
*        Original version based upon KPS1_FSPWE.
*     2009 December 19 (MJC):
*        Use dynamic workspace for the work array.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'CNF_PAR'          ! CNF_PVAL

*  Arguments Given:
      INTEGER NDFI
      INTEGER NXKNOT
      INTEGER NYKNOT
      REAL XKNOT( NXKNOT + 8 )
      REAL YKNOT( NYKNOT + 8 )
      INTEGER NCOEF
      REAL COEFF( NCOEF )
      REAL SCALE
      REAL RSMAX
      REAL RMS
      CHARACTER*(*) COSYS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*( DAT__SZLOC ) FLOC ! Locator to FIT polynomial
                                 ! structure
      INTEGER KPNTR              ! Pointer to workspace
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

      CALL PSX_CALLOC( ( NXKNOT + 4 ) * ( NYKNOT + 4 ), '_REAL', KPNTR,
     :                 STATUS )
      CALL KPS1_BS2PU( FLOC, NXKNOT, NYKNOT, XKNOT, YKNOT, SCALE,
     :                 NCOEF, COEFF, %VAL( CNF_PVAL( KPNTR ) ),
     :                 STATUS )
      CALL PSX_FREE( KPNTR, STATUS )

*  In addition to the coefficients, write the RMS error, the maximum
*  residual to the SURFACEFIT extension, and the co-ordinate system.
      CALL NDF_XPT0R( RMS, NDFI, 'SURFACEFIT', 'RMS', STATUS )
      CALL NDF_XPT0R( RSMAX, NDFI, 'SURFACEFIT', 'RESIDMAX', STATUS )
      CALL NDF_XPT0C( COSYS, NDFI, 'SURFACEFIT', 'COSYS', STATUS )

      END
