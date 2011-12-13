      SUBROUTINE KPS1_BS2PU( LOC, NXKNOT, NYKNOT, XKNOT, YKNOT, SCALE,
     :                       NCOEF, COEFF, WORK, STATUS )
*+
*  Name:
*     KPS1_BS2PU

*  Purpose:
*     Writes two-dimensional B-spline information to a POLYNOMIAL
*     structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_BS2PU( LOC, NXKNOT, NYKNOT, XKNOT, YKNOT, SCALE,
*                      NCOEF, COEFF, WORK, STATUS )

*  Description:
*     This routine writes information describing a two-dimensional
*     bi-cubic polynomial surface to a standard Starlink POLYNOMIAL
*     structure, as defined in SGP/38, but with a variant 'BSPLINE' not
*     promulgated there.  An empty POLYNOMIAL structure should already
*     have been created.  All floating-point components within the
*     structure are written as DOUBLE PRECISION.
*
*     It is assumed the calling programme has a one-dimensional array of
*     coefficients, where COEFF( ( IX - 1 ) * ( NYKNOT + 4 ) + IY )
*     contains the coefficient at the (IX,IY)th knot number, where
*     NYKNOT + 4 is the total number of Y terms.  Such a
*     one-dimensional array is used by the PDA_SURFIT and PDA_BISPEV
*     routines, and defined in the NAG manual (see Chapter E02 on
*     "Curve and Surface fitting").
*
*     This routine will convert the coefficient array to
*     two-dimensional, flip it around and store it in the POLYNOMIAL
*     structure so that DATA_ARRAY(IX,IY) contains the coefficient for
*     the (IX,IY)th term.
*
*     The routine will also load the knot positions in the vector KNOTS
*     storing the first dimension knots followed by the second
*     dimension, and a data-scaling factor in SCALE, both of which have
*     type _REAL.

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator to existing but empty POLYNOMIAL structure.
*     NXKNOT = INTEGER (Given)
*        The number of interior knots in the x direction.
*     NYKNOT = INTEGER (Given)
*        The number of interior knots in the y direction.
*     XKNOT( NXKNOT + 8 ) = REAL (Given)
*        The x positions of complete set of knots associated with x.
*     YKNOT( NYKNOT + 8 ) = REAL (Given)
*        The y positions of complete set of knots associated with y.
*     SCALE = REAL (Given)
*        The scale factor applied to the data values before calculating
*        the spline.
*     NCOEF = INTEGER (Given)
*        The number of bi-cubic coefficients.  It must equal
*        (%NXKNOT+4) * (%NYKNOT+4).
*     COEFF( NCOEF ) = REAL (Given)
*        The bi-cubic B-spline coefficients, defined at the knots in the
*        order increasing x knot, then increasing y.  Thus coefficient
*        Cij in the standard convention is %COEFF((i-1)*(%NYKNOT+4)+j).
*     WORK( NXKNOT + 4, NYKNOT + 4 ) = REAL (Returned)
*        Work array used to flip the polynomial coefficients.  On exit
*        it will contain the two-dimensional array of coefficients
*        written to the POLYNOMIAL structure.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Notes:
*     -  Uses the magic-value method for bad or undefined pixels.

*  Copyright:
*     Copyright (C) 2007, 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 July 4 (MJC):
*        Original version derived from KPG1_PL2PU.
*     2009 December 17 (MJC):
*        Use a single generic KNOTS vector rather than XKNOTS and
*        YKNOTS, since naming is not easily extensible beyond three
*        dimensions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! DAT__ global constants
      INCLUDE 'NDF_PAR'          ! NDF public constants
      INCLUDE 'CNF_PAR'          ! CNF_PVAL

*  Arguments Given:
      CHARACTER*( DAT__SZLOC ) LOC
      INTEGER NXKNOT
      INTEGER NYKNOT
      REAL XKNOT( NXKNOT + 8 )
      REAL YKNOT( NYKNOT + 8 )
      INTEGER NCOEF
      REAL COEFF( NCOEF )
      REAL SCALE

*  Arguments Returned:
      REAL WORK( NXKNOT + 4, NYKNOT + 4 )

*  Status:
      INTEGER STATUS             ! Global inherited status

*  Local Constants:
      INTEGER MAXDIM             ! Only two-dimensional polynomials can
                                 ! be handled
      PARAMETER ( MAXDIM = 2 )

*  Local Variables:
      INTEGER DIM( MAXDIM )      ! Dimensions of coefficients array
      INTEGER IDIMS( NDF__MXDIM ) ! Dimensions of knots along one axis
      INTEGER IX                 ! Loop counter
      INTEGER IY                 ! Loop counter
      INTEGER KPNTR              ! Pointer to knots' vector workspace
      INTEGER NKNOTS             ! Total number of knots
      INTEGER ODIMS( NDF__MXDIM ) ! Dimensions of combined knots vector
      INTEGER OFFSET( NDF__MXDIM ) ! Offsets to paste knots' array

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the VARIANT element to the polynomial structure, ignoring
*  trailing blanks.
      CALL DAT_NEW0C( LOC, 'VARIANT', 7, STATUS )
      CALL CMP_PUT0C( LOC, 'VARIANT', 'BSPLINE', STATUS )

*  Write the coeffients array to the DATA_ARRAY of the polynomial
*  structure.
      DIM( 1 ) = NXKNOT + 4
      DIM( 2 ) = NYKNOT + 4

*  Flip the coefficients array to normal Fortran order.
      DO IY = 1, DIM( 2 )
         DO IX = 1, DIM( 1 )
            WORK( IX, IY ) = COEFF( ( IX - 1 ) * ( NYKNOT + 4 ) + IY )
         END DO
      END DO

*  Store the reordered array in the POLYNOMIAL structure's DATA_ARRAY.
      CALL DAT_NEW( LOC, 'DATA_ARRAY', '_REAL', MAXDIM, DIM, STATUS )
      CALL CMP_PUTNR( LOC, 'DATA_ARRAY', MAXDIM, DIM, WORK, DIM,
     :                STATUS )

*  Obtain workspace of the length of the KNOTS array.
      NKNOTS = NXKNOT + NYKNOT + 16
      CALL PSX_CALLOC( NKNOTS, '_REAL', KPNTR, STATUS )

*  Prepare for pasting array.  The routine uses the maximum number of
*  dimensions.
      IDIMS( 1 ) = NXKNOT + 8
      ODIMS( 1 ) = NKNOTS
      OFFSET( 1 ) = 0

      DO IX = 2, NDF__MXDIM
         IDIMS( IX ) = 1
         ODIMS( IX ) = 1
         OFFSET( IX ) = 0
      END DO

*  Paste the X-axis set of knots into the work vector.
      CALL KPG1_PASTR( .FALSE., .FALSE., OFFSET, IDIMS, NXKNOT + 8,
     :                 XKNOT, ODIMS, NKNOTS, %VAL( CNF_PVAL( KPNTR ) ),
     :                 STATUS )

*  Paste the Y-axis set of knots into the work vector immediately
*  the X-axis knots.
      IDIMS( 1 ) = NYKNOT + 8
      OFFSET( 1 ) = NXKNOT + 8

      CALL KPG1_PASTR( .FALSE., .FALSE., OFFSET, IDIMS, NYKNOT + 8,
     :                 YKNOT, ODIMS, NKNOTS, %VAL( CNF_PVAL( KPNTR ) ),
     :                 STATUS )


*  Copy the knots to the KNOTS array within the POLYNOMIAL structure.
      CALL DAT_NEW1R( LOC, 'KNOTS', NKNOTS, STATUS )
      CALL CMP_PUT1R( LOC, 'KNOTS', NKNOTS, %VAL( CNF_PVAL( KPNTR ) ),
     :                STATUS )

      CALL PSX_FREE( KPNTR, STATUS )

*  Store the data scaling factor.
      CALL DAT_NEW0R( LOC, 'SCALE', STATUS )
      CALL CMP_PUT0R( LOC, 'SCALE', SCALE, STATUS )

      END
