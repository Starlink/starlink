      SUBROUTINE CCD1_CALCR( M, N, XC, NF, FVECC, UI, UR, UF )
*+
*  Name:
*     CCD1_CALCR

*  Purpose:
*     Least squares function for scale factor and zero point
*     optimisation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CALCR( M, N, XC, NF, FVECC, UI, UR, UF )

*  Description:
*     This is an instance of the CALCR service routine required by the PDA
*     library routine PDA_DNL2S1 which solves non-linear least-squares
*     minimisation problems. It is configured for use when determining
*     an optimum set of logarithmic scale factor and zero point
*     corrections for a set of data arrays which have been
*     inter-compared in pairs and for which a set of individual scale
*     and zero differences have been found. The form of this routine is
*     governed by the requirements of the PDA routine.

*  Arguments:
*     M = INTEGER (Given)
*        Number of residuals.
*     N = INTEGER (Given)
*        Number of unknowns.
*     XC( N ) = DOUBLE PRECISION (Given)
*        Value of the unknowns for which residuals are required. 
*        The first N/2 of these will be the logarithmic scale factor 
*        corrections for each data array; the second N/2 will be the 
*        zero point corrections.
*     NF = INTEGER (Given)
*        Unused.
*     FVECC( M ) = DOUBLE PRECISION (Returned)
*        Array of residuals. The first (M-2)/2 of these
*        will be the weighted differences between the logarithmic scale
*        factor difference expected between a pair of data arrays on
*        the basis of the current corrections and the differences that
*        were actually observed. The next (M-2)/2 residuals will be the
*        equivalent values for the zero point differences. The final 2
*        residuals are the weighted mean logarithmic scale factor and
*        zero point corrections (or those for the "reference data
*        array", if given); these serve simply to constrain these
*        corrections to be zero.
*     UI( * ) = INTEGER (Returned)
*        Unused.
*     UR( * ) = DOUBLE PRECISION (Returned)
*        Unused.
*     UF( * ) = EXTERNAL (Returned)
*        Unused.

*  Notes:
*     This routine reads data from global variables held in common.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     13-SEP-1996 (PDRAPER):
*        Converted to be used from PDA routine PDA_DNL2S1, rather than
*        the NAG routine E04GBF (was CCD1_SZLSF).
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
      INTEGER M
      INTEGER N
      DOUBLE PRECISION XC( N )
      INTEGER NF

*  Arguments Returned:
      DOUBLE PRECISION FVECC( M )
      INTEGER UI( * )
      DOUBLE PRECISION UR( * )
      EXTERNAL UF

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
      INTEGER NCMP               ! Number of inter-comparisons
      INTEGER NIN                ! Number of data arrays

*.

*  Determine the number of data arrays and the number of
*  inter-comparisons which were made.
      NIN = N / 2
      NCMP = ( M - 2 ) / 2

*  Loop to consider each residual. First identify the pair of data
*  arrays used in each inter-comparison.
      DO 3 I = 1, NCMP
         I1 = CCD1_IPAIR( 1, I )
         I2 = CCD1_IPAIR( 2, I )

*  Extract the current estimate of the logarithm of the scale factor
*  correction for each of these data arrays, correcting for the scaling
*  factor used to normalise the values handled by NAG. Derive the
*  expected logarithm of the relative scale factor when the two
*  un-corrected data arrays are inter-compared.
         S1 = XC( I1 ) * CCD1_RNG1
         S2 = XC( I2 ) * CCD1_RNG1
         DS = S2 - S1

*  Form a residual by comparing this relative scale factor with what
*  was actually obtained, appropriately weighted (by the measured
*  uncertainty).
         FVECC( I ) = CCD1_WT1( I ) * ( DS - CCD1_DIFS( I ) )

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
         FVECC( I + NCMP ) = CCD1_WT2( I ) * ( DZ - CCD1_DIFZ( I ) )
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
      SUMS = 0.0D0
      SUMZ = 0.0D0
      DO 4 I = I1, I2
         SUMS = SUMS + XC( I ) * CCD1_RNG1
         SUMZ = SUMZ + XC( I + NIN ) * CCD1_RNG2 -
     :        CCD1_MORIG( I ) * EXP( XC( I ) * CCD1_RNG1 )
 4    CONTINUE

*  Add the means (derived from these sums) as two extra residuals, thus
*  constraining them to be zero. These constraints can always be met
*  exactly, so use a weight which is "large" compared with the sum of
*  all other weights. If this is not done, the error in meeting these
*  constraints will contribute to the error estimates for the result.
      WT = CCD1__BIGWT * ( DBLE( NCMP ) / DBLE( I2 - I1 + 1 ) )
      FVECC( M - 1 ) = WT * SUMS
      FVECC( M ) = WT * SUMZ
      END
* $Id$
