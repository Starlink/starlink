      SUBROUTINE KPS1_MFTA( QUICK, ITYPE, INTERP, MXKNOT, NKNOT, FKNOT,
     :                      AXIS, NRANGE, RANGES, USEVAR, IPVAR, USEMSK,
     :                      MASK, DIMS, IPDAT, X, Z, W, NFIT, NGOOD,
     :                      KNOT, COEFF, NCOEF, SCALE, STATUS )
*+
*  Name:
*     KPG1_MFTA

*  Purpose:
*     Fits cubic B-splines to all lines of data that lie parallel to
*     an axis fopr all numeric data types.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MFTA( QUICK, ITYPE, INTERP, MXKNOT, NKNOT, FKNOT, AXIS,
*                     NRANGE, RANGES, USEVAR, IPVAR, USEMSK, MASK, DIMS,
*                     IPDAT, X, Z, W, NFIT, NGOOD, KNOT, COEFF, NCOEF,
*                     SCALE, STATUS )

*  Description:
*     This routine fits cubic B-splines to all lines of data that lie
*     parallel to a given axis, and lie within ranges along that axis.
*     It is essentially code lifted from MFITTREND to make the task
*     code more manageable and readable.

*     It invokes KPS1_MFTx or KPS1_MFTQx for the supplied ITYPE data
*     type to do the work.  The latter routine set is used for quick
*     processing when there are no bad pixels or weights (see QUICK).
*     See these two routines for details of the various mapped arrays.

*     The fits can be weighted by the inverse values of the associated
*     variances.  Features can also be excluded using a mask.
*     The spline can be a smoothing or interpolation.  In the former
*     case the knots are found automatically.  In the latter, the knots
*     are set through FKNOT or spaced equally.
*
*     The spline coefficients and knots are returned in the COEFF and
*     KNOT arrays respectively.

*  Arguments:
*     QUICK = LOGICAL (Given)
*        Use the quicker code that expects neither bad pixels nor
*        weights.  Otherwise the slower routine is used; it can weight
*        the fits by the inverses of the associated variances.
*     ITYPE = CHARACTER * ( * ) (Given)
*        The implementation HDS data type.
*     INTERP = LOGICAL (Given)
*        If set to true an interpolating least-squares spline is found
*        and the NKNOT interior knots are equally spaced.  Otherwise a
*        smoothing spline is fit using a smoothing factor equal to the
*        number of data points; and the spline fit sets the knots.
*        The order of polynomial to fit.  Starts at 0 for a constant.
*     MXKNOT = INTEGER (Given)
*        The maximum number of knots.  This should be at least NKNOT+8.
*     NKNOT = INTEGER (Given)
*        The number of interior knots.
*     FKNOT( NKNOT ) = REAL (Given)
*        Grid co-ordinates of the user-defined knot positions to be
*        applied to every trend when using an interpolating spline
*        (INTERP is true).  If the first element is negative, then
*        equally spaced knots are used.
*     AXIS = INTEGER (Given)
*        The axis defining the lines to be fit.
*     NRANGE = INTEGER (Given)
*        The number of values in RANGES, must be an even number.
*     RANGES( NRANGE ) = INTEGER*8 (Given)
*        Pairs of array coordinates along the axis.  Only data within
*        these ranges will be used in the fit.
*     USEVAR = LOGICAL (Given)
*        If .TRUE., then the variances will be used to weight the fits.
*     IPVAR = INTEGER (Given)
*        Pointer to the variances of the data, mapped using the type
*        ITYPE.  These will be used to weight the fits if USEVAR is true.
*     USEMSK = LOGICAL (Given)
*        If .TRUE. then the supplied mask (argument MASK) is used to
*        omit parts of the data from the fitting process.
*     MASK( * ) = BYTE (Given)
*        A mask in which bad values indicate that a given element of
*        the data be ignored from the fitting process.  It is only
*        accessed if USEMSK is .TRUE..  The array should have dimensions
*        matching the input data.
*     DIMS( NDF__MXDIM ) = INTEGER*8 (Given)
*        The dimensions of the input data.  Extra dimension should have
*        size 1 up to NDF__MXDIM.
*     IPDAT = INTEGER (Given)
*        Pointer to the data to be fitted mapped with type ITYPE.  The
*        fit will be subtracted from these data, if requested.
*     X( DIMS( AXIS ) + 2, * ) = REAL (Returned)
*        Workspace for the co-ordinates of ordered trends to fit.  The
*        size of the second dimension of this should be the product of
*        all the dimensions that are not the fit axis.
*     Z( DIMS( AXIS ) + 2, * ) = REAL (Returned)
*        Workspace for the values of ordered trends to fit.  The size of
*        the second dimension of this should be the product of all the
*        dimensions that are not the fit axis.
*     W( DIMS( AXIS ) + 2, * ) = REAL (Returned)
*        Workspace for the co-ordinates of ordered trends to fit.  The
*        size of the last dimension of this should be the product of
*        all the dimensions that are not the fit axis.
*     NFIT = INTEGER (Returned)
*        The number of trends successfully fitted.
*     NGOOD( * ) = INTEGER*8 (Returned)
*        Workspace to store the number of good values in each trend
*        fit.  The dimension of this should be the product of all the
*        dimensions that are not the fit axis.
*     KNOT( MXKNOT, * ) = REAL (Returned)
*        The knot positions of the cubic B-splines for each trend in
*        grid co-ordinates.  The size of the last dimension of this
*        should be the product of all the dimensions that are not the
*        fit axis.
*     COEFF( MXKNOT, * ) = REAL (Returned)
*        The coefficients of the cubic B-splines for each trend.  The
*        size of the last dimension of this should be the product of
*        all the dimensions that are not the fit axis.
*     NCOEF( * ) = INTEGER (Returned)
*        The number coefficents of the cubic B-splines for each trend.
*        The dimension of this should be the product of all the
*        dimensions that are not the fit axis.
*     SCALE( * ) = REAL (Returned)
*        The scale factors of ordered trends that were used to scale
*        each trend vector to the -1 to +1 range.  The dimension of this
*        should be the product of all the dimensions that are not the
*        fit axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics & Astronomy Research
*     Council.
*     Copyright (C) 2008, 2012 Science and Technology Facilities
*     Council.
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
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     2008 May 15 (MJC):
*        Original version.
*     2008 May 21 (MJC):
*        Added FKNOT argument.
*     2012 May 11 (MJC):
*        Add support for 64-bit integers.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      LOGICAL QUICK
      CHARACTER*( * ) ITYPE
      LOGICAL INTERP
      INTEGER MXKNOT
      INTEGER NKNOT
      REAL FKNOT( NKNOT )
      INTEGER AXIS
      INTEGER NRANGE
      INTEGER*8 RANGES( NRANGE )
      LOGICAL USEVAR
      INTEGER IPVAR
      LOGICAL USEMSK
      BYTE MASK( * )
      INTEGER*8 DIMS( NDF__MXDIM )
      INTEGER IPDAT

*  Arguments Returned:
      REAL X( DIMS( AXIS ) + 2, * )
      REAL Z( DIMS( AXIS ) + 2, * )
      REAL W( DIMS( AXIS ) + 2, * )
      INTEGER NFIT
      INTEGER*8 NGOOD( * )
      REAL KNOT( MXKNOT, * )
      REAL COEFF( MXKNOT, * )
      INTEGER NCOEF( * )
      REAL SCALE( * )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( .NOT. QUICK ) THEN
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPS1_MFTB( INTERP, MXKNOT, NKNOT, FKNOT, AXIS, NRANGE,
     :                      RANGES, USEVAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                      USEMSK, MASK, DIMS,
     :                      %VAL( CNF_PVAL( IPDAT ) ),
     :                      X, Z, W, NFIT, NGOOD, KNOT, COEFF, NCOEF,
     :                      SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPS1_MFTUB( INTERP, MXKNOT, NKNOT, FKNOT, AXIS, NRANGE,
     :                       RANGES, USEVAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                       USEMSK, MASK, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       X, Z, W, NFIT, NGOOD, KNOT, COEFF, NCOEF,
     :                       SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_MFTD( INTERP, MXKNOT, NKNOT, FKNOT, AXIS, NRANGE,
     :                      RANGES, USEVAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                      USEMSK, MASK, DIMS,
     :                      %VAL( CNF_PVAL( IPDAT ) ),
     :                      X, Z, W, NFIT, NGOOD, KNOT, COEFF, NCOEF,
     :                      SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_MFTI( INTERP, MXKNOT, NKNOT, FKNOT, AXIS, NRANGE,
     :                      RANGES, USEVAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                      USEMSK, MASK, DIMS,
     :                      %VAL( CNF_PVAL( IPDAT ) ),
     :                      X, Z, W, NFIT, NGOOD, KNOT, COEFF, NCOEF,
     :                      SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL KPS1_MFTK( INTERP, MXKNOT, NKNOT, FKNOT, AXIS, NRANGE,
     :                      RANGES, USEVAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                      USEMSK, MASK, DIMS,
     :                      %VAL( CNF_PVAL( IPDAT ) ),
     :                      X, Z, W, NFIT, NGOOD, KNOT, COEFF, NCOEF,
     :                      SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_MFTR( INTERP, MXKNOT, NKNOT, FKNOT, AXIS, NRANGE,
     :                      RANGES, USEVAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                      USEMSK, MASK, DIMS,
     :                      %VAL( CNF_PVAL( IPDAT ) ),
     :                      X, Z, W, NFIT, NGOOD, KNOT, COEFF, NCOEF,
     :                      SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPS1_MFTW( INTERP, MXKNOT, NKNOT, FKNOT, AXIS, NRANGE,
     :                      RANGES, USEVAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                      USEMSK, MASK, DIMS,
     :                      %VAL( CNF_PVAL( IPDAT ) ),
     :                      X, Z, W, NFIT, NGOOD, KNOT, COEFF, NCOEF,
     :                      SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPS1_MFTUW( INTERP, MXKNOT, NKNOT, FKNOT, AXIS, NRANGE,
     :                       RANGES, USEVAR, %VAL( CNF_PVAL( IPVAR ) ),
     :                       USEMSK, MASK, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       X, Z, W, NFIT, NGOOD, KNOT, COEFF, NCOEF,
     :                       SCALE, STATUS )
         END IF
      ELSE

*  As there are no variances, no bad values and no mask, we can use
*  the fastest method.
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPS1_MFTQB( INTERP, MXKNOT, NKNOT, FKNOT, AXIS,
     :                       NRANGE, RANGES, DIMS,
     %                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       X, Z, W, NFIT, NGOOD, KNOT,
     :                       COEFF, NCOEF, SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPS1_MFTQUB( INTERP, MXKNOT, NKNOT, FKNOT, AXIS,
     :                        NRANGE, RANGES, DIMS,
     :                        %VAL( CNF_PVAL( IPDAT ) ),
     :                        X, Z, W, NFIT, NGOOD, KNOT,
     :                        COEFF, NCOEF, SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_MFTQD( INTERP, MXKNOT, NKNOT, FKNOT, AXIS,
     :                       NRANGE, RANGES, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       X, Z, W, NFIT, NGOOD, KNOT,
     :                       COEFF, NCOEF, SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_MFTQI( INTERP, MXKNOT, NKNOT, FKNOT, AXIS,
     :                       NRANGE, RANGES, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       X, Z, W, NFIT, NGOOD, KNOT,
     :                       COEFF, NCOEF, SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL KPS1_MFTQK( INTERP, MXKNOT, NKNOT, FKNOT, AXIS,
     :                       NRANGE, RANGES, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       X, Z, W, NFIT, NGOOD, KNOT,
     :                       COEFF, NCOEF, SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_MFTQR( INTERP, MXKNOT, NKNOT, FKNOT, AXIS,
     :                       NRANGE, RANGES, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       X, Z, W, NFIT, NGOOD, KNOT,
     :                       COEFF, NCOEF, SCALE, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPS1_MFTQW( INTERP, MXKNOT, NKNOT, FKNOT, AXIS,
     :                       NRANGE, RANGES, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       X, Z, W, NFIT, NGOOD, KNOT,
     :                       COEFF, NCOEF, SCALE, STATUS )

        ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPS1_MFTQUW( INTERP, MXKNOT, NKNOT, FKNOT, AXIS,
     :                        NRANGE, RANGES, DIMS,
     :                        %VAL( CNF_PVAL( IPDAT ) ),
     :                        X, Z, W, NFIT, NGOOD, KNOT,
     :                        COEFF, NCOEF, SCALE, STATUS )
         END IF
      END IF

      END
