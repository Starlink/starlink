      SUBROUTINE KPS1_LFTA( QUICK, ITYPE, ORDER, AXIS, NRANGE, RANGES,
     :                      USEVAR, IPVAR, USEMSK, MASK, DIMS, IPDAT,
     :                      AS, BS, WRK1, WRK2, STATUS )

*+
*  Name:
*     KPG1_LFTA

*  Purpose:
*     Fits polynomials to all lines of data that lie parallel to
*     an axis for all numeric data types.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LFTA( QUICK, ITYPE, ORDER, AXIS, NRANGE, RANGES, USEVAR,
*                     IPVAR, USEMSK, MASK, DIMS, IPDAT, AS, BS, WRK1,
*                     WRK2, STATUS )

*  Description:
*     This wrapper routine fits polynomials, of a given order, to all
*     lines of data that lie parallel to a given axis, and lie within
*     ranges along that axis for any numeric data type.  It is
*     essentially code lifted from MFITTREND to make the task code more
*     manageable and readable.
*
*     It invokes KPS1_LFTx or KPS1_LFTQx for the supplied ITYPE data
*     type to do the work.  The latter routine set is used for quick
*     processing when there are no bad pixels or weights (see QUICK).
*     See these two routines for details of the various mapped arrays.

*     The fits can be weighted by the inverse  values of the associated
*     variances.  Features can also be excluded using a mask.
*
*     The polynomial coefficients returned in the BS array.

*  Arguments:
*     QUICK = LOGICAL (Given)
*        Use the quicker code that expects neither bad pixels nor
*        weights.  Otherwise the slower routine is used; it can weight
*        the fits by the inverses of the associated variances.
*     ITYPE = CHARACTER * ( * ) (Given)
*        The implementation HDS data type.
*     ORDER = INTEGER (Given)
*        The order of polynomial to fit.  Starts at 0 for a constant.
*     AXIS = INTEGER (Given)
*        The axis defining the lines to be fit.
*     NRANGE = INTEGER (Given)
*        The number of values in RANGES, must be an even number.
*     RANGES( NRANGE ) = INTEGER (Given)
*        Pairs of array coordinates along the axis.  Only data within
*        these ranges will be used in the fit.
*     USEVAR = LOGICAL (Given)
*        If .TRUE., then the variances will be used to weight the fits.
*     IPVAR = INTEGER (Given)
*        Pointer to the variances of the data, mapped using type ITYPE.
*        These variaces will be used to weight the fits if USEVAR is
*        true.
*     USEMSK = LOGICAL (Given)
*        If .TRUE. then the supplied mask (argument MASK) is used to
*        omit parts of the data from the fitting process.
*     MASK( * ) = BYTE (Given)
*        A mask in which bad values indicate that a given element of
*        the data be ignored from the fitting process.  It is only
*        accessed if USEMSK is .TRUE..  The array should have dimensions
*        matching the input data.
*     DIMS( NDF__MXDIM ) = INTEGER (Given)
*        The dimensions of the input data.  Extra dimensions should have
*        size 1 up to NDF__MXDIM.
*     IPDAT = INTEGER (Given)
*        Pointer to the data to be fitted, mapped using type ITYPE.
*     AS( ORDER + 1, ORDER + 1, * ) = DOUBLE PRECISION (Returned)
*        Workspace for least-squares A matrices of cumulative sums.
*        The size of the last dimension of this should be the product of
*        all the dimensions that are not the fit axis.
*     BS( ORDER + 1, * ) = DOUBLE PRECISION (Returned)
*        Workspace for least-squares B vectors cumulative sums.
*        On exit this contains the coefficients of the polynomials.
*
*        Since there are ORDER + 1 of these for each line, the size of
*        the last dimension of this should be the product of all the
*        dimensions that are not the fit axis.  The coefficients will be
*        accessed in Fortran order for the axis, for instance if the
*        third axis if fitted in a cube then this array has an effective
*        shape of (ORDER+1,NX,NY), if the second axis is chosen then the
*        shape is (ORDER+1,NX,NZ) etc.
*     WRK1( ORDER + 1 ) = DOUBLE PRECISION (Returned)
*        Workspace.
*     WRK2( ORDER + 1 ) = INTEGER (Returned)
*        Workspace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics & Astronomy Research
*     Council.
*     Copyright (C) 2008, 2012, 2016 Science and Technology Facilities
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
*     PWD: Peter W. Draper (JAC, Durham University)
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     2008 May 15 (MJC):
*        Original version based upon KPS1_LFT.
*     2012 May 11 (MJC):
*        Add support for 64-bit integers.
*     2016 July 18 (MJC):
*        Fixed calls to KPS1_LFTQx where Argument AS was not being
*        passed through.
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
      INTEGER ORDER
      INTEGER AXIS
      INTEGER NRANGE
      INTEGER RANGES( NRANGE )
      LOGICAL USEVAR
      INTEGER IPVAR
      LOGICAL USEMSK
      BYTE MASK( * )
      INTEGER DIMS( NDF__MXDIM )
      INTEGER IPDAT

*  Arguments Given and Returned:
      DOUBLE PRECISION AS( ORDER + 1, ORDER + 1, * )
      DOUBLE PRECISION BS( ORDER + 1, * )
      DOUBLE PRECISION WRK1( * )
      INTEGER WRK2( * )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( .NOT. QUICK ) THEN
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPS1_LFTB( ORDER, AXIS, NRANGE, RANGES, USEVAR,
     :                      %VAL( CNF_PVAL( IPVAR ) ), USEMSK, MASK,
     :                      DIMS, %VAL( CNF_PVAL( IPDAT ) ), AS, BS,
     :                      WRK1, WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPS1_LFTUB( ORDER, AXIS, NRANGE, RANGES, USEVAR,
     :                       %VAL( CNF_PVAL( IPVAR ) ), USEMSK, MASK,
     :                       DIMS, %VAL( CNF_PVAL( IPDAT ) ), AS, BS,
     :                       WRK1, WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_LFTD( ORDER, AXIS, NRANGE, RANGES, USEVAR,
     :                      %VAL( CNF_PVAL( IPVAR ) ), USEMSK, MASK,
     :                      DIMS, %VAL( CNF_PVAL( IPDAT ) ), AS, BS,
     :                      WRK1, WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_LFTI( ORDER, AXIS, NRANGE, RANGES, USEVAR,
     :                      %VAL( CNF_PVAL( IPVAR ) ), USEMSK, MASK,
     :                      DIMS, %VAL( CNF_PVAL( IPDAT ) ), AS, BS,
     :                      WRK1, WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL KPS1_LFTK( ORDER, AXIS, NRANGE, RANGES, USEVAR,
     :                      %VAL( CNF_PVAL( IPVAR ) ), USEMSK, MASK,
     :                      DIMS, %VAL( CNF_PVAL( IPDAT ) ), AS, BS,
     :                      WRK1, WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_LFTR( ORDER, AXIS, NRANGE, RANGES, USEVAR,
     :                      %VAL( CNF_PVAL( IPVAR ) ), USEMSK, MASK,
     :                      DIMS, %VAL( CNF_PVAL( IPDAT ) ), AS, BS,
     :                      WRK1, WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPS1_LFTW( ORDER, AXIS, NRANGE, RANGES, USEVAR,
     :                      %VAL( CNF_PVAL( IPVAR ) ), USEMSK, MASK,
     :                      DIMS, %VAL( CNF_PVAL( IPDAT ) ), AS, BS,
     :                      WRK1, WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPS1_LFTUW( ORDER, AXIS, NRANGE, RANGES, USEVAR,
     :                       %VAL( CNF_PVAL( IPVAR ) ), USEMSK, MASK,
     :                       DIMS, %VAL( CNF_PVAL( IPDAT ) ), AS, BS,
     :                       WRK1, WRK2, STATUS )
         END IF
      ELSE

*  As there are no variances, no bad values and no mask, we can use
*  the fastest method.
         IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPS1_LFTQB( ORDER, AXIS, NRANGE, RANGES, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ), AS, BS, WRK1,
     :                       WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
           CALL KPS1_LFTQUB( ORDER, AXIS, NRANGE, RANGES, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ), AS, BS, WRK1,
     :                       WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_LFTQD( ORDER, AXIS, NRANGE, RANGES, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ), AS, BS, WRK1,
     :                       WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_LFTQI( ORDER, AXIS, NRANGE, RANGES, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ), AS, BS, WRK1,
     :                       WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
            CALL KPS1_LFTQK( ORDER, AXIS, NRANGE, RANGES, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ), AS, BS, WRK1,
     :                       WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_LFTQR( ORDER, AXIS, NRANGE, RANGES, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ), AS, BS, WRK1,
     :                       WRK2, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPS1_LFTQW( ORDER, AXIS, NRANGE, RANGES, DIMS,
     :                       %VAL( CNF_PVAL( IPDAT ) ), AS, BS, WRK1,
     :                       WRK2, STATUS )

        ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPS1_LFTQUW( ORDER, AXIS, NRANGE, RANGES, DIMS,
     :                        %VAL( CNF_PVAL( IPDAT ) ), BS, WRK1, WRK2,
     :                        STATUS )
         END IF
      END IF

      END
