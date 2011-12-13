      SUBROUTINE CCD1_SZLSF( IFLAG, M, N, XC, FVECC, FJACC, LJC )
*+
*  Name:
*     CCD1_SZLSF

*  Purpose:
*     Least squares function for scale factor and zero point
*     optimisation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_SZLSF( IFLAG, M, N, XC, FVECC, FJACC, LJC, IW, LIW, W,
*                      LW )

*  Description:
*     This is an instance of the FCN service routine required by the PDA
*     library routine PDA_DNLS1 which solves non-linear least-squares
*     minimisation problems. It is configured for use when determining
*     an optimum set of logarithmic scale factor and zero point
*     corrections for a set of data arrays which have been
*     inter-compared in pairs and for which a set of individual scale
*     and zero differences have been found. The form of this routine is
*     governed by the requirements of the PDA routine.

*  Arguments:
*     IFLAG = INTEGER (Given)
*        Specifies whether residual values and/or their first
*        derivatives are required.
*
*        A value of 0 means that the values in XC and FVECC are
*        available for printing (this service isn't used)
*
*        A value of 1 means that we should calculate FVECC at XC.
*
*        A value of 2 means that we should calculate FJACC at XC.
*
*        The value 3 should never occur.
*     M = INTEGER (Given)
*        Number of residuals.
*     N = INTEGER (Given)
*        Number of unknowns.
*     XC( N ) = DOUBLE PRECISION (Given)
*        Value of the unknowns for which residuals and/or their
*        derivatives are required. The first N/2 of these will be the
*        logarithmic scale factor corrections for each data array; the
*        second N/2 will be the zero point corrections.
*     FVECC( M ) = DOUBLE PRECISION (Returned)
*        Array of residuals (if required). The first (M-2)/2 of these
*        will be the weighted differences between the logarithmic scale
*        factor difference expected between a pair of data arrays on
*        the basis of the current corrections and the differences that
*        were actually observed. The next (M-2)/2 residuals will be the
*        equivalent values for the zero point differences. The final 2
*        residuals are the weighted mean logarithmic scale factor and
*        zero point corrections (or those for the "reference data
*        array", if given); these serve simply to constrain these
*        corrections to be zero.
*     FJACC( LJC, N ) = DOUBLE PRECISION (Returned)
*        Jacobian array of partial derivatives (if required). This
*        gives the partial derivative of each residual with respect to
*        each correction, evaluated with the current set of
*        corrections.
*     LJC = INTEGER (Given)
*        First dimension size of the FJACC array.

*  Notes:
*     This routine reads data from global variables held in common.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PDRAPER: P.W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  History:
*     30-APR-1992 (RFWS):
*        Original version.
*     4-AUG-1992 (RFWS):
*        Added ability to constrain the corrections applied to the
*        "reference data array".
*     14-AUG-1992 (RFWS):
*        Installed scaling factors for the variables being optimised
*        and converted the zero point corrections to refer to the mean
*        false origin values stored in global variables.
*     19-SEP-1996 (PDRAPER):
*        Converted to be used from PDA routine PDA_DNLS1, rather than
*        the NAG routine E04GBF.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'CCD1_PAR'         ! General CCDPACK constants
      INCLUDE 'CCD1_MOSPR'       ! Constants specific to MAKEMOS

*  Global Variables:
      INCLUDE 'CCD1_MOSCM'       ! Global variables for MAKEMOS
*        CCD1_DIFS( CCD1__MXCMP + 1 ) = DOUBLE PRECISION (Read)
*           Array of observed logarithmic scale factor differences.
*        CCD1_DIFZ( CCD1__MXCMP + 1 ) = DOUBLE PRECISION (Read)
*           Array of observed zero point differences.
*        CCD1_IPAIR( 2, CCD1__MXCMP ) = INTEGER (Read)
*           Array of pairs of indices identifying which two data arrays
*           contributed to which observed scale factor and zero point
*           difference.
*        CCD1_IREF = INTEGER (Read)
*           Index of the "reference data array" to which the optimised
*           scale factor and zero point corrections should be
*           normalised (set to zero if there is no reference data
*           array).
*        CCD1_MORIG( CCD1__MXCMP ) = DOUBLE PRECISION (Read)
*           Array giving the "mean false origin" values for each data
*           array, to which the zero point corrections being optimised
*           are referred.
*        CCD1_ORIG( CCD1__MXCMP ) = DOUBLE PRECISION (Read)
*           Array giving the false origin values used when obtaining
*           the scale factor and zero point differences.
*        CCD1_RNG1 = DOUBLE PRECISION (Read)
*           Expected range of logarithmic scale factor corrections
*           (used as scaling factor for these corrections).
*        CCD1_RNG2 = DOUBLE PRECISION (Read)
*           Expected range of zero point corrections (used as scaling
*           factor for these corrections).
*        CCD1_WT1( CCD1__MXCMP ) = DOUBLE PRECISION (Read)
*           Array of weighting factors to be applied to each
*           logarithmic scale factor residual.
*        CCD1_WT2( CCD1__MXCMP ) = DOUBLE PRECISION (Read)
*           Array of weighting factors to be applied to each zero point
*           residual.

*  Arguments Given:
      INTEGER IFLAG
      INTEGER M
      INTEGER N
      DOUBLE PRECISION XC( N )
      INTEGER LJC

*  Arguments Returned:
      DOUBLE PRECISION FVECC( M )
      DOUBLE PRECISION FJACC( LJC, N )

*  Local Variables:
      DOUBLE PRECISION DS        ! Expected log scale factor difference
      DOUBLE PRECISION DZ        ! Expected zero point difference
      DOUBLE PRECISION S1        ! Log scale factor correction 1
      DOUBLE PRECISION S2        ! Log scale factor corection 2
      DOUBLE PRECISION SUMS      ! Sum of log scale factor corrections
      DOUBLE PRECISION SUMZ      ! Sum of zero point corrections
      DOUBLE PRECISION WT        ! Large weighting factor
      DOUBLE PRECISION Z1        ! Zero point correction 1
      DOUBLE PRECISION Z2        ! Zero point correction 2
      INTEGER I                  ! Loop counter
      INTEGER I1                 ! Index of data array 1
      INTEGER I2                 ! Index of data array 2
      INTEGER J                  ! Loop counter
      INTEGER NCMP               ! Number of inter-comparisons
      INTEGER NIN                ! Number of data arrays
      LOGICAL DERIV              ! Derivatives required?
      LOGICAL RESID              ! Residuals required?

*.

*  Do nothing if IFLAG is 0 (important not to modify anything).
      IF ( IFLAG .EQ. 0 ) RETURN

*  Determine the number of data arrays and the number of
*  inter-comparisons which were made.
      NIN = N / 2
      NCMP = ( M - 2 ) / 2

*  See if residuals and/or derivatives are required.
      RESID = ( IFLAG .EQ. 1 )
      DERIV = ( IFLAG .EQ. 2 )

*  If derivatives are being calculated, initialise the Jacobian matrix
*  to zero.
      IF ( DERIV ) THEN
         DO 2 J = 1, N
            DO 1 I = 1, M
               FJACC( I, J ) = 0.0D0
 1          CONTINUE
 2       CONTINUE
      END IF

*  Loop to consider each residual. First identify the pair of data
*  arrays used in each inter-comparison.
      DO 3 I = 1, NCMP
         I1 = CCD1_IPAIR( 1, I )
         I2 = CCD1_IPAIR( 2, I )

*  Extract the current estimate of the logarithm of the scale factor
*  correction for each of these data arrays, correcting for the scaling
*  factor used to normalise the values handled by PDA. Derive the
*  expected logarithm of the relative scale factor when the two
*  un-corrected data arrays are inter-compared.
         S1 = XC( I1 ) * CCD1_RNG1
         S2 = XC( I2 ) * CCD1_RNG1
         DS = S2 - S1

*  Form a residual by comparing this relative scale factor with what
*  was actually obtained, appropriately weighted (by the measured
*  uncertainty).
         IF ( RESID ) THEN
            FVECC( I ) = CCD1_WT1( I ) * ( DS - CCD1_DIFS( I ) )
         END IF

*  Form the associated derivatives if required.
         IF ( DERIV ) THEN
            FJACC( I, I1 ) = - CCD1_WT1( I ) * CCD1_RNG1
            FJACC( I, I2 ) = CCD1_WT1( I ) * CCD1_RNG1
         END IF

*  Extract the current estimate of the zero point corrections for the
*  two data arrays (allow for scaling factors and for each correction
*  being referred to different false origins as given by CCD1_MORIG).
*  Derive the expected relative zero point when the two un-corrected
*  data arrays are inter-compared
         Z1 = XC( I1 + NIN ) * CCD1_RNG2 - CCD1_MORIG( I1 ) * EXP( S1 )
         Z2 = XC( I2 + NIN ) * CCD1_RNG2 - CCD1_MORIG( I2 ) * EXP( S2 )
         DZ = ( Z2 - Z1 ) * EXP( - S1 )

*  Correct the expected relative zero point for the false origin used
*  when the data arrays were actually inter-compared.
         DZ = DZ + CCD1_ORIG( I ) * EXP( DS )

*  Form a residual by comparing this relative zero point with what was
*  actually obtained, appropriately weighted (by the measured
*  uncertainty).
         IF ( RESID ) THEN
            FVECC( I + NCMP ) = CCD1_WT2( I ) * ( DZ - CCD1_DIFZ( I ) )
         END IF

*  Form the associated derivatives if required.
         IF ( DERIV ) THEN
            FJACC( I + NCMP, I1 ) = - CCD1_WT2( I ) * CCD1_RNG1 *
     :                                ( DZ - CCD1_MORIG( I1 ) )
            FJACC( I + NCMP, I2 ) = CCD1_WT2( I ) * CCD1_RNG1 *
     :                 EXP( DS ) * ( CCD1_ORIG( I ) - CCD1_MORIG( I2 ) )
            FJACC( I + NCMP, I1 + NIN ) = - CCD1_WT2( I ) *
     :                                    EXP( - S1 ) * CCD1_RNG2
            FJACC( I + NCMP, I2 + NIN ) = CCD1_WT2( I ) *
     :                                    EXP( - S1 ) * CCD1_RNG2
         END IF
 3    CONTINUE

*  Since we only consider inter-data array differences above, there is
*  no constraint on the mean log scale factor or the mean zero point
*  correction, so we add extra constraints here. We either constrain the
*  mean correction, or the correction applied to the "reference data
*  array". Set up indices to identify the corrections to be constrained.
      IF ( ( CCD1_IREF .LT. 1 ) .OR. ( CCD1_IREF .GT. NIN ) ) THEN
         I1 = 1
         I2 = NIN
      ELSE
         I1 = CCD1_IREF
         I2 = CCD1_IREF
      END IF

*  Sum all the current estimates of each of the corrections to be
*  constrained (allowing for scaling and different zero point
*  references).
      IF ( RESID ) THEN
         SUMS = 0.0D0
         SUMZ = 0.0D0
         DO 4 I = I1, I2
            SUMS = SUMS + XC( I ) * CCD1_RNG1
            SUMZ = SUMZ + XC( I + NIN ) * CCD1_RNG2 -
     :             CCD1_MORIG( I ) * EXP( XC( I ) * CCD1_RNG1 )
 4       CONTINUE

*  Add the means (derived from these sums) as two extra residuals, thus
*  constraining them to be zero. These constraints can always be met
*  exactly, so use a weight which is "large" compared with the sum of
*  all other weights. If this is not done, the error in meeting these
*  constraints will contribute to the error estimates for the result.
         WT = CCD1__BIGWT * ( DBLE( NCMP ) / DBLE( I2 - I1 + 1 ) )
         FVECC( M - 1 ) = WT * SUMS
         FVECC( M ) = WT * SUMZ
      END IF

*  Form the associated derivatives if required.
      IF ( DERIV ) THEN
         WT = CCD1__BIGWT * ( DBLE( NCMP ) / DBLE( I2 - I1 + 1 ) )
         DO 5 I = I1, I2
            FJACC( M - 1, I ) = WT * CCD1_RNG1
            FJACC( M, I + NIN ) = WT * CCD1_RNG2
            FJACC( M, I ) = - WT * CCD1_RNG1 * CCD1_MORIG( I ) *
     :                        EXP( XC( I ) * CCD1_RNG1 )
 5       CONTINUE
      END IF

      END
* $Id$
