      SUBROUTINE CCD1_SZSLV( GETS, GETZ, NIN, NCMP, NMAX, SCALE, DSCALE,
     :                       ZERO, DZERO, ORIG, WRK1, WRK2, WRK3,
     :                       WRK4, WRK5, STATUS )
*+
*  Name:
*     CCD1_SZSLV

*  Purpose:
*     Solve for optimum scale factor and zero point corrections.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_SZSLV( GETS, GETZ, NIN, NCMP, NMAX, SCALE, DSCALE, ZERO,
*                      DZERO, ORIG, WRK1, WRK2, WRK3, WRK4, WRK5,
*                      STATUS )

*  Description:
*     The routine takes as input a set of differences obtained by
*     inter-comparing members of a set of data arrays in pairs.  It is
*     assumed that the arrays all represent the same observations but
*     that each has been perturbed by an unknown scale factor and/or
*     zero point.  The routine solves for a set of corrections for
*     these perturbations which, when applied to the data arrays, will
*     best correct for the set of differences found, subject to the
*     constraint that the median scale factor correction should be
*     unity and the median zero point correction should be zero (unless
*     a reference array is identified, in which case the corrections
*     are normalised with respect to it).
*
*     The returned corrections should be applied to the data arrays
*     such that the I'th array is corrected using the formula:
*
*        result = ( DATA - ORIG( I ) ) * SCALE( I ) + ZERO( I )
*
*     Statistical error estimates are also calculated for the SCALE and
*     ZERO values.

*  Arguments:
*     GETS = LOGICAL (Given)
*        Whether scale factor corrections should be calculated
*        (otherwise they are simply set to unity).
*     GETZ = LOGICAL (Given)
*        Whether zero point corrections should be calculated (otherwise
*        they are simply set to zero).
*     NIN = INTEGER (Given)
*        Number of data arrays.
*     NCMP = INTEGER (Given)
*        Number of data array inter-comparisons.
*     NMAX = INTEGER (Given)
*        NMAX is the maximum of 2*(NIN+1) and 2*(NCMP+1). Is is used
*        when declaring workspace sizes.
*     SCALE( NIN ) = DOUBLE PRECISION (Returned)
*        Array of scale factor corrections.
*     DSCALE( NIN ) = DOUBLE PRECISION (Returned)
*        Array of scale factor standard errors associated with the
*        corrections in SCALE.
*     ZERO( NIN ) = DOUBLE PRECISION (Returned)
*        Array of zero point corrections.
*     DZERO( NIN ) = DOUBLE PRECISION (Returned)
*        Array of zero point standard errors associated with the
*        corrections in ZERO.
*     ORIG( NIN ) = DOUBLE PRECISION (Returned)
*        Array of false origin values. These do not affect the
*        corrections derived (any change in the value of ORIG being
*        compensated by a corresponding change in ZERO), but are chosen
*        so that the errors on the SCALE and ZERO values are expected
*        to be un-correlated.
*     WRK1( i ) = DOUBLE PRECISION (Returned)
*        Workspace. If (GETS.AND.GETZ) is true, then i should equal
*        NMAX, otherwise, if (GETS.OR.GETZ) is .TRUE., then i should
*        equal 2*(NIN+1), otherwise, this array is not used.
*     WRK2( 17*NMAX+40 ) = INTEGER (Returned)
*        Workspace. This is only required if (GETS.AND.GETZ) is .TRUE..
*     WRK3( NMAX*(NMAX+2) ) = DOUBLE PRECISION (Returned)
*        Workspace. This is only required if (GETS.AND.GETZ) is .TRUE..
*     WRK4( 4 * NMAX * NMAX + 86 * NMAX + 141 ) = DOUBLE PRECISION (Returned)
*        Workspace. This is only required if (GETS.AND.GETZ) is .TRUE..
*     WRK5( NMAX * NMAX ) = DOUBLE PRECISION (Returned)
*        Workspace. This is only required if (GETS.AND.GETZ) is .TRUE..
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine accesses data via global variables held in
*     common.
*     -  If the set of data array inter-comparisons provided for this
*     routine to use (via global variables) is inadequate to determine
*     the required corrections, then an error will be reported and
*     STATUS will be returned set to the value USER__001, as defined in
*     the include file USER_ERR.

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
*     PDRAPER: P. W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  History:
*     28-FEB-1992 (RFWS):
*        Original version.
*     21-APR-1992 (RFWS):
*        Changed to optimise scale factor and zero point corrections
*        simultaneously.
*     29-MAY-1992 (RFWS):
*        Added passing of workspace arrays.
*     2-JUN-1992 (RFWS):
*        Return USER__001 error code if required.
*     3-JUN-1992 (RFWS):
*        Added the IREF argument.
*     5-JUN-1992 (RFWS):
*        Corrected error in normalisation; normalise zero points before
*        shifting the origin (instead of after).
*     27-JUL-1992 (RFWS):
*        Improved the algorithm for normalising the weights to allow
*        for the typically different ranges of the scale factor and
*        zero point differences.
*     5-AUG-1992 (RFWS):
*        Removed the IREF argument (now a global variable).
*     6-AUG-1992 (RFWS):
*        Installed correct handling of reference data array
*        constraints.
*     22-SEP-1992 (RFWS):
*        Installed scaling to prevent numerical problems during
*        simultaneous optimisation of scale factor and zero point
*        corrections.
*     23-SEP-1992 (RFWS):
*        Ignore IFAIL>2 returned by E04YCF.
*     16-SEP-1996 (PDRAPER):
*        Changed all NAG routine calls to PDA versions. Substantial
*        changes to non-linear solutions. Linear solutions more or
*        less remain the same.
*     15-OCT-1996 (PDRAPER):
*        More substantial changes to non-linear solutions, these now use
*        a constraint based solver. Linear solutions more or less remain
*        the same (a good replacement for these was found, the
*        non-linear case, due to the conditioning imposed by the median
*        scale and zero points constraint proved too much for all the
*        global minimization routines I found).
*     01-JUL-1999 (PDRAPER):
*        Added initialization of ORIG values. These where not set if
*        GETS or GETZ only where true (Linux random values).
*     18-MAY-2009 (PDRAPER):
*        Correct error messages from PDA_LSQR calls to show correct
*        error value and associated parameter name. Proceed with the
*        solution if the iteration limit was reached (ISTOP=7).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! General CCDPACK constants
      INCLUDE 'CCD1_MOSPR'       ! Constants specific to MAKEMOS
      INCLUDE 'PRM_PAR'          ! Primitive data constants
      INCLUDE 'USER_ERR'         ! General purpose "user" error codes

*  Global Variables:
      INCLUDE 'CCD1_MOSCM'       ! Global variables for MAKEMOS
*        CCD1_DDIFS( CCD1__MXCMP ) = DOUBLE PRECISION (Read)
*           Array of standard errors associated with the observed scale
*           factor differences.
*        CCD1_DDIFZ( CCD1__MXCMP ) = DOUBLE PRECISION (Read)
*           Array of standard errors associated with the observed zero
*           point differences.
*        CCD1_DIFS( CCD1__MXCMP + 1 ) = DOUBLE PRECISION (Read and
*        Write)
*           Array of observed scale factor differences. These are
*           converted to their logarithms by this routine.
*        CCD1_DIFZ( CCD1__MXCMP + 1 ) = DOUBLE PRECISION (Read)
*           Array of observed zero point differences.
*        CCD1_IPAIR( 2, CCD1__MXCMP ) = INTEGER (Read)
*           Array of pairs of indices identifying which two data arrays
*           contributed to which observed scale factor and zero point
*           difference.
*        CCD1_IREF = INTEGER (Read)
*           Index of the "reference data array" to which optimised
*           scale factor and zero point corrections should be
*           normalised (set to zero if there is no reference data
*           array).
*        CCD1_MORIG( CCD1__MXCMP ) = DOUBLE PRECISION (Write)
*           Array giving the "mean false origin" values for each data
*           array, to which the zero point corrections being optimised
*           are referred.
*        CCD1_RNG1 = DOUBLE PRECISION (Write)
*           Expected range of logarithmic scale factor corrections
*           (used as scaling factor for these corrections).
*        CCD1_RNG2 = DOUBLE PRECISION (Write)
*           Expected range of zero point corrections (used as scaling
*           factor for these corrections).
*        CCD1_WT1( CCD1__MXCMP ) = DOUBLE PRECISION (Write)
*           Array of weighting factors to be applied to each
*           logarithmic scale factor residual (unless
*           (GETZ.AND.(.NOT.GETS)) is .TRUE., in which case it holds
*           weights for the zero point residuals).
*        CCD1_WT2( CCD1__MXCMP ) = DOUBLE PRECISION (Write)
*           Array of weighting factors to be applied to each zero point
*           residual.
*        CCD1_N = INTEGER (Write)
*           The number of unknowns in the non-linear system.
*        CCD1_M = INTEGER (Write)
*           The number of residuals in the non-linear system.

*  Arguments Given:
      LOGICAL GETS
      LOGICAL GETZ
      INTEGER NIN
      INTEGER NCMP

*  Arguments Returned:
      DOUBLE PRECISION SCALE( NIN )
      DOUBLE PRECISION DSCALE( NIN )
      DOUBLE PRECISION ZERO( NIN )
      DOUBLE PRECISION DZERO( NIN )
      DOUBLE PRECISION ORIG( NIN )
      INTEGER NMAX
      DOUBLE PRECISION WRK1( * )
      INTEGER WRK2( * )
      DOUBLE PRECISION WRK3( * )
      DOUBLE PRECISION WRK4( NMAX, * )
      DOUBLE PRECISION WRK5( NMAX, NMAX )

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CCD1_SZPRD       ! Service routine for PDA_LSQR
      EXTERNAL CCD1_DQEDS       ! Service routine for PDA_DQED

*  Local Variables:
      DOUBLE PRECISION ACOND    ! (unused)
      DOUBLE PRECISION ANORM    ! (unused)
      DOUBLE PRECISION ARNORM   ! (unused)
      DOUBLE PRECISION ATOL     ! Tolerance criterion A
      DOUBLE PRECISION BTOL     ! Tolerance criterion B
      DOUBLE PRECISION CNORM    ! Normalisation constant
      DOUBLE PRECISION CONLIM   ! Condition number limit
      DOUBLE PRECISION COVAR    ! Covariance between the 2 corrections
      DOUBLE PRECISION DAMP     ! Damping factor
      DOUBLE PRECISION FSUMSQ   ! Half sum of fit residuals
      DOUBLE PRECISION HI1      ! Max. normalised scale factor weight
      DOUBLE PRECISION HI2      ! Max. normalised zero point weight
      DOUBLE PRECISION LO1      ! Min. normalised scale factor weight
      DOUBLE PRECISION LO2      ! Min. normalised zero point weight
      DOUBLE PRECISION MEANWT   ! Mean "safe" weighting factor
      DOUBLE PRECISION RATIO1   ! Ratio of scale factor weights to mean
      DOUBLE PRECISION RATIO2   ! Ratio of zero point weights to mean
      DOUBLE PRECISION RNORM    ! (unused)
      DOUBLE PRECISION SVAR     ! Variance on scale factor correction
      DOUBLE PRECISION TINY     ! Value too small to handle
      DOUBLE PRECISION W( 1 )   ! Dummy array
      DOUBLE PRECISION WTSUM1   ! Sum of scale factor weights
      DOUBLE PRECISION WTSUM2   ! Sum of zero point weights
      DOUBLE PRECISION XNORM    ! (unused)
      DOUBLE PRECISION ZVAR     ! Variance on zero point correction
      INTEGER I                 ! Loop counter
      INTEGER I1                ! Image index
      INTEGER I2                ! Image index
      INTEGER INFO              ! Exit status for PDA routines
      INTEGER ITN               ! No. iterations performed
      INTEGER ITNLIM            ! Iteration limit
      INTEGER IW( 1 )           ! Dummy array
      INTEGER M                 ! Number of least-squares residuals
      INTEGER MXWT              ! Maximum possible number of weights
      INTEGER N                 ! Number of unknowns
      INTEGER NWT1              ! Number of scale factor weights
      INTEGER NWT2              ! Number of zero point weights
      INTEGER IOPT( 17 )        ! PDA_DQED options
      INTEGER IND( CCD1__MXNDF * 2 ) ! Constraint types
      INTEGER MCON              ! Linear constraint count
      DOUBLE PRECISION ROPT( 1 ) ! PDA_DQED options
      DOUBLE PRECISION BL( CCD1__MXNDF * 2 ) ! Lower bounds on solution
      DOUBLE PRECISION BU( CCD1__MXNDF * 2 ) ! Upper bounds on solution
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Pre-processing and production of weighting factors.
*  ==================================================
*  Initialise, and determine the maximum number of weights to be
*  considered.
      NWT1 = 0
      NWT2 = 0
      WTSUM1 = 0.0D0
      WTSUM2 = 0.0D0
      MXWT = 1
      IF ( GETS .AND. GETZ ) MXWT = 2

*  Origins are zero for GETZ and GETS only.
      DO 16 I = 1, NIN
         ORIG( I ) = 0.0D0
 16   CONTINUE


*  Loop to determine normalisation factors to be used to generate
*  weights for the logarithmic scale factor and zero point differences
*  in the fitting process.
      DO 1 I = 1, NCMP

*  First consider the scale factor errors. Find the smallest error
*  value which can safely be used to form a weight for the logarithmic
*  scale factor difference.
         IF ( GETS ) THEN
            TINY = CCD1_DIFS( I ) * ( DBLE( MXWT ) / NUM__MAXD )

*  If the weight can be calculated safely, then do so, and include it
*  in the appropriate sum of weights. Otherwise, flag it using the
*  NUM__MAXD value.
            IF ( CCD1_DDIFS( I ) .GT. TINY ) THEN
               CCD1_WT1( I ) = CCD1_DIFS( I ) / CCD1_DDIFS( I )
               WTSUM1 = WTSUM1 + CCD1_WT1( I )
               NWT1 = NWT1 + 1
            ELSE
               CCD1_WT1( I ) = NUM__MAXD
            END IF

*  Convert the scale factor differences to their natural logarithms.
            CCD1_DIFS( I ) = LOG( CCD1_DIFS( I ) )
         END IF

*  Similarly form weights for the zero point differences.
         IF ( GETZ ) THEN
            TINY = DBLE( MXWT ) / NUM__MAXD
            IF ( CCD1_DDIFZ ( I ) .GT. TINY ) THEN
               CCD1_WT2( I ) = 1.0D0 / CCD1_DDIFZ( I )
               WTSUM2 = WTSUM2 + CCD1_WT2( I )
               NWT2 = NWT2 + 1
            ELSE
               CCD1_WT2( I ) = NUM__MAXD
            END IF
         END IF
 1    CONTINUE

*  Find the mean "safe" weighting factor for the complete set of
*  differences, supplying a default of unity.
      IF ( ( NWT1 + NWT2 ) .GT. 0 ) THEN
         MEANWT = ( WTSUM1 + WTSUM2 ) / ( NWT1 + NWT2 )
      ELSE
         MEANWT = 1.0D0
      ENDIF

*  Find the ratio by which the mean "safe" weighting factor for each
*  set of differences when assessed separately exceeds the mean for the
*  set of weights taken as a whole.
      IF ( NWT1 .GT. 0 ) THEN
         RATIO1 = ( WTSUM1 / DBLE( NWT1 ) ) / MEANWT
      ELSE
         RATIO1 = 1.0D0
      END IF
      IF ( NWT2 .GT. 0 ) THEN
         RATIO2 = ( WTSUM2 / DBLE( NWT2 ) ) / MEANWT
      ELSE
         RATIO2 = 1.0D0
      END IF

*  Set limits for the normalised weights which restrict the range of
*  weights allowed within each set of differences to prevent numerical
*  problems, but which allow the mean weights applied to each set of
*  differences to differ. (This is necessary because each set of
*  differences will typically have a different range of values.)
      LO1 = RATIO1 / CCD1__BIGWT
      HI1 = RATIO1 * CCD1__BIGWT
      LO2 = RATIO2 / CCD1__BIGWT
      HI2 = RATIO2 * CCD1__BIGWT

*  Normalise the weights to a mean of unity and limit the results to
*  lie within the ranges defined above.
      DO 2 I = 1, NCMP
         IF ( GETS ) THEN
            IF ( CCD1_WT1( I ) .NE. NUM__MAXD ) CCD1_WT1( I ) =
     :                                          CCD1_WT1( I ) / MEANWT
            CCD1_WT1( I ) = MIN( MAX( LO1, CCD1_WT1( I ) ), HI1 )
         END IF
         IF ( GETZ ) THEN
            IF ( CCD1_WT2( I ) .NE. NUM__MAXD ) CCD1_WT2( I ) =
     :                                          CCD1_WT2( I ) / MEANWT
            CCD1_WT2( I ) = MIN( MAX( LO1, CCD1_WT2( I ) ), HI2 )

*  If only zero point corrections are being found, then transfer the
*  weighting factors to the first weight array (this is where the PDA
*  library service routines expect to find them).
            IF ( .NOT. GETS ) CCD1_WT1( I ) = CCD1_WT2( I )
         END IF
 2    CONTINUE

*  Scale factors and zero points:
*  =============================
*  See whether we need to determine both scale factor and zero point
*  corrections.  If we are determining both simultaneously, then the
*  problem is non-linear and must be solved iteratively.
      IF ( GETS .AND. GETZ ) THEN

*  It is important to ensure that the least-squares problem to be
*  solved is well scaled (particularly for large numbers of unknowns)
*  in order to avoid numerical problems. For this, we must first find a
*  mean "false origin" value for each data array to be used as the
*  reference point for applying zero point corrections. Initialise
*  arrays for finding these means.
         DO 44 I = 1, NIN
            CCD1_MORIG( I )= 0.0D0
            WRK1( I ) = 0.0D0
 44      CONTINUE

*  Form sums for finding the weighted mean of the origin values used
*  for each data array when it was inter-compared with others. Use the
*  zero point weighting factors determined earlier.
         DO 45 I = 1, NCMP
            I2 = CCD1_IPAIR( 2, I )
            CCD1_MORIG( I2 ) = CCD1_MORIG( I2 ) +
     :                         CCD1_ORIG( I ) * CCD1_WT2( I )
            WRK1( I2 ) = WRK1( I2 ) + CCD1_WT2( I )

*  If the data array was used as the ordinate during inter-comparison,
*  then use the origin projected on to the other axis (the result of
*  this process is the value stored as the zero point difference).
            I1 = CCD1_IPAIR( 1, I )
            CCD1_MORIG( I1 ) = CCD1_MORIG( I1 ) +
     :                         CCD1_DIFZ( I ) * CCD1_WT2( I )
            WRK1( I1 ) = WRK1( I1 ) + CCD1_WT2( I )
 45      CONTINUE

*  Form the weighted mean "false origin" value for each data array.
*  (Note that these have been calculated so that if the zero point
*  corrections are referred to these origins, they will have relatively
*  little dependence on the corresponding scale factor corrections and
*  will differ little from their values referred to the original origin
*  used during inter-comparison. This now allows us to estimate the
*  approximate range of zero point corrections required without
*  worrying about what the scale factor corrections might be. By using
*  these values as a zero point reference, we also ensure a good
*  starting point for the subsequent optimisation.)
         DO 46 I = 1, NIN
            CCD1_MORIG( I ) = CCD1_MORIG( I ) / WRK1( I )
 46      CONTINUE

*  Initialise for finding the approximate range of corrections
*  required, ensuring that the result is sufficiently larger than zero.
         CCD1_RNG1 = VAL__SMLD / VAL__EPSD
         CCD1_RNG2 = VAL__SMLD / VAL__EPSD

*  Find the maximum absolute value of the logarithmic scale factor
*  correction for all the inter-comparisons made as an estimate of the
*  range of scale factor corrections required (relative to the expected
*  value, which is zero).
         DO 55 I = 1, NCMP
            I2 = CCD1_IPAIR( 2, I )
            CCD1_RNG1 = MAX( CCD1_RNG1, ABS( CCD1_DIFS( I ) ) )

*  Form a similar maximum for the zero point corrections (in this case
*  we do not necessarily expect the final values to be zero, but we use
*  it as an estimate of how far we may be away from the final
*  solution).
            CCD1_RNG2 = MAX( CCD1_RNG2, ABS( CCD1_DIFZ( I ) *
     :                                  EXP( CCD1_DIFS( I ) ) ) )
 55      CONTINUE

*  Set the number of residuals and unknowns for the images we've
*  processed (we also store these to pass to the service routine).
         M = 2 * NCMP
         CCD1_M = M
         N = 2 * NIN
         CCD1_N = N

*  Initialise estimates of the (scaled) optimum logarithmic scale
*  factor and zero point corrections. We now expect these values to
*  extend over the approximate range [-1.0D0:+1.0D0].
         DO 3 I = 1, N
            WRK1( I ) = 0.0D0

*  There are no constraints on the non-reference data, so let PDA_DQED know
*  about this.
            IND( I ) = 4
            BL( I ) = 0.0D0
            BU( I ) = 0.0D0
 3       CONTINUE

*  If a reference array isn't given then we add two false unknowns with
*  residuals. These are constrained to be 0 and the residuals are
*  the mean value of all scale factors and zero points. If a reference
*  array is given then we constraint it to 0.
         IF ( CCD1_IREF .EQ. 0 ) THEN

*  Reference array is imaginary and is based on the mean scale and zero
*  point corrections being 0 (+/- a bit for rounding etc.). These extra
*  residuals are added to the end of the existing lists.
            IND( N + 1 ) = 3
            BU( N + 1 ) = 10.D0 * VAL__EPSD
            BL( N + 1 ) = -BU( N + 1 )
            IND( N + 2 ) = 3
            BL( N + 2 ) = -BU( N + 1 )
            BU( N + 2 ) = BU( N + 1 )

*  Increment the number of unknowns and residuals and initial our
*  "guess" at the values.
            N = N + 2
            CCD1_N = N
            M = M + 2
            CCD1_M = M
            WRK1( N - 1 ) = 0.0D0
            WRK1( N ) = 0.0D0
         ELSE

*  Reference array is part of existing system, so just constrain it.
            IND( CCD1_IREF ) = 3
            BU( CCD1_IREF ) = 10.0D0 * VAL__EPSD
            BL( CCD1_IREF ) = BU( CCD1_IREF )
            IND( CCD1_IREF + NIN ) = 3
            BL( CCD1_IREF + NIN ) = BU( CCD1_IREF )
            BU( CCD1_IREF + NIN ) = BU( CCD1_IREF )
         END IF

*  Perform initialisations for PDA_DQED.
         MCON = 0               ! No linear constraints (just simple ones)
         IOPT( 1 ) = 1          ! Print no information
         IOPT( 2 ) = 0          ! ""
         IOPT( 3 ) = 99         ! No more options to change
         WRK2( 1 ) = 4 * NMAX * NMAX + 86 * NMAX + 141 ! Workspace length
         WRK2( 2 ) = 17 * NMAX + 40 ! Workspace length

*  And solve the system.
         CALL PDA_DQED( CCD1_DQEDS, M, N, MCON, IND, BL, BU, WRK1, WRK3,
     :                  M, FSUMSQ, INFO, IOPT, ROPT, WRK2, WRK4 )

*  If there is no solution, then report an error.
         IF ( INFO .GT. 8 ) THEN
            STATUS = USER__001
            CALL MSG_SETI( 'INFO', INFO )
            CALL ERR_REP( 'CCD1_SZSLV_SOL',
     :                    'Error solving for consistent ' //
     :                    'scale-factor and zero-point corrections ' //
     :                    '- IV(1)=^INFO returned by routine ' //
     :                    'PDA_DQED.', STATUS )
         ELSE

*  Work out the properly corrected sum of squares of the residuals.
            FSUMSQ = FSUMSQ * FSUMSQ
            IF ( M .GT. N ) THEN
               FSUMSQ = FSUMSQ / DBLE( M - N )
            END IF

*  Remove the artificial reference data from consideration, if used.
            IF ( CCD1_IREF .EQ. 0 ) THEN
               M = M - 2
               N = N - 2
            END IF

*  Now derive the optimised corrections covariance matrix .
            CALL CCD1_FCOV( M, N, FSUMSQ, WRK1, WRK5, NMAX, WRK3( 1 ),
     :                      WRK3( NMAX + 2 ), WRK4,
     :                      WRK3( NMAX + NMAX + 3 ), INFO )

*  Check for errors.
            IF ( INFO .EQ. 1 ) THEN
               STATUS = USER__001
               CALL MSG_SETI( 'INFO', INFO )
               CALL ERR_REP( 'CCD1_SZSLV_FCOV',
     :                       'Error estimating errors on optimised ' //
     :                       'corrections - INFO=^INFO returned ' //
     :                       'by routine CCD1_FCOV.', STATUS )
            ELSE

*  Now extract the results and their error estimates,
*  correcting for the scaling used during optimisation. Also correct
*  the covariance values in the WRK5 array which will be used later.
               DO 4 I = 1, NIN
                  SCALE( I ) = WRK1( I ) * CCD1_RNG1
                  DSCALE( I ) = SQRT( ABS( WRK5( I, I ) ) ) * CCD1_RNG1
                  ZERO( I ) = WRK1( I + NIN ) * CCD1_RNG2
                  DZERO( I ) = SQRT( ABS( WRK5( I + NIN, I + NIN ) ) )
     :                 * CCD1_RNG2
                  WRK5( I, I + NIN  ) = WRK5( I, I + NIN )
     :                 * CCD1_RNG1 * CCD1_RNG2

*  Adjust the zero point corrections to refer to an origin of zero,
*  rather than the mean false origin used during optimisation. Also
*  make corresponding adjustments to the associated error and
*  covariance estimates.
                  ZERO( I ) = ZERO( I ) -
     :                 CCD1_MORIG( I ) * EXP( SCALE( I ) )
                  DZERO( I ) = SQRT( ABS( DZERO( I ) ** 2 -
     :                 2.0D0 * CCD1_MORIG( I ) *
     :                 EXP( SCALE( I ) ) *
     :                 WRK5( I, I + NIN ) +
     :                 ( CCD1_MORIG( I ) *
     :                 EXP( SCALE( I ) ) *
     :                 DSCALE( I ) ) ** 2 ) )
                  WRK5( I, I + NIN ) = WRK5( I, I + NIN  ) -
     :                 CCD1_MORIG( I ) *
     :                 EXP( SCALE( I ) ) *
     :                 DSCALE( I ) ** 2
 4             CONTINUE

*  If no reference data array has been specified, then normalise the
*  logarithmic scale factor corrections to a median of zero (this
*  corresponds to scaling the corrected data array so that the median
*  scale factor correction is unity). Since constraints are imposed
*  when solving for these values to ensure that their mean is zero, we
*  are simply correcting here for the difference between the mean and
*  the median. We assume this makes negligible difference to the
*  statistics.
               IF ( ( CCD1_IREF .LT. 1 ) .OR.
     :              ( CCD1_IREF .GT. NIN ) ) THEN
                  CALL CCD1_NRMED( NIN, SCALE, CNORM, WRK1, STATUS )

*  If a reference data array has been specified, then normalise the
*  logarithmic scale factor corrections to it.  Since constraints are
*  imposed when solving for these values to ensure that they are
*  already normalised to this reference data array, we are simply
*  correcting here for very minor imperfections in the solution process
*  (e.g. due to the inability to assign an infinite weight to this
*  constraint).
               ELSE
                  CNORM = SCALE( CCD1_IREF )
                  DO 5 I = 1, NIN
                     SCALE( I ) = SCALE( I ) - CNORM
 5                CONTINUE

*  Set the errors on the corrections for the reference data array and
*  their covariance to zero (again simply correcting for very minor
*  imperfections in the solution).
                  DSCALE( CCD1_IREF ) = 0.0D0
                  DZERO( CCD1_IREF ) = 0.0D0
                  WRK5( CCD1_IREF, CCD1_IREF + NIN ) = 0.0D0
               END IF

*  Correct the zero point corrections (and their errors) for the
*  normalising scale factor applied above.
               CNORM = EXP( CNORM )
               DO 6 I = 1, NIN
                  ZERO( I ) = ZERO( I ) / CNORM
                  DZERO( I ) = DZERO( I ) / CNORM

*  Extract the covariance between the errors on the logarithmic scale
*  factor and the zero point corrections and correct it for the same
*  normalising scale factor.
                  COVAR = WRK5( I, I + NIN ) / CNORM

*  Convert the scale factor corrections back from their logarithms and
*  similarly modify the covariance estimate to refer to the converted
*  scale factor errors. Re-store the corrected covariance.
                  SCALE( I ) = EXP( SCALE( I ) )
                  DSCALE( I ) = DSCALE( I ) * SCALE( I )
                  WRK5( I, I + NIN ) = COVAR * SCALE( I )
 6             CONTINUE

*  Normalise the zero point corrections, either to a median of zero
*  (this corresponds to adding a constant to the corrected data array),
*  or to the reference data array. (As above, we are either correcting
*  for the difference between the mean and the median, or for minor
*  imperfections in the solution.) This does not affect any other
*  corrections or error estimates.
               IF ( ( CCD1_IREF .LT. 1 ) .OR.
     :              ( CCD1_IREF .GT. NIN ) ) THEN
                  CALL CCD1_NRMED( NIN, ZERO, CNORM, WRK1, STATUS )
               ELSE
                  CNORM = ZERO( CCD1_IREF )
                  DO 7 I = 1, NIN
                     ZERO( I ) = ZERO( I ) - CNORM
 7                CONTINUE
               END IF

*  The covariance will usually be substantially non-zero, so it must be
*  taken into account when a data value is corrected using the scale
*  factor and zero point corrections obtained above. In general, the
*  expected variance on a corrected value will have a component which
*  arises from the errors in the correction parameters themselves -
*  this component varies parabolically with data value and the position
*  of the minimum is determined by the covariance. Here, we obtain an
*  origin value which co-incides with this minimum and correct the zero
*  point to apply to this new origin. With this choice of origin, the
*  correction parameters may then be considered independent.
               DO 8 I = 1, NIN
                  SVAR = DSCALE( I ) ** 2
                  COVAR = WRK5( I, I + NIN )
                  TINY = ABS( COVAR ) / NUM__MAXD
                  IF ( SVAR .GT. TINY ) THEN
                     ORIG( I ) = - COVAR / SVAR
                  ELSE
                     ORIG( I ) = 0.0D0
                  END IF
                  ZERO( I ) = ZERO( I ) + SCALE( I ) * ORIG( I )

*  Also correct the error estimate on the zero point for this change of
*  origin (effectively removing the dependence on the scale factor
*  error).
                  ZVAR = DZERO( I ) ** 2
                  ZVAR = ZVAR + COVAR * ORIG( I )
                  DZERO( I ) = SQRT( ABS( ZVAR ) )
 8             CONTINUE
            END IF
         END IF

*  Scale factors only:
*  ==================
*  If we are determining scale factors alone, then the problem is
*  linear.
      ELSE IF ( GETS ) THEN

*  Apply the weights to the logarithmic scale factor differences. Set a
*  value of zero beyond the final element to act as a constraint on the
*  mean correction obtained (this is used by the routine CCD1_SZPRD -
*  an extra element exists in the array for this purpose).
         DO 9 I = 1, NCMP
            CCD1_DIFS( I ) = CCD1_DIFS( I ) * CCD1_WT1( I )
 9       CONTINUE
         CCD1_DIFS( NCMP + 1 ) = 0.0D0

*  Invoke a PDA routine to solve for a set of consistent scale factor
*  corrections.
         DAMP = 0.0D0
         ATOL = 0.0D0
         BTOL = 0.0D0
         CONLIM = 0.0D0
         ITNLIM = 4 * NCMP
         CALL PDA_LSQR( NCMP + 1, NIN, CCD1_SZPRD, DAMP, 1, 1, W, IW,
     :                  CCD1_DIFS, WRK1( 1 ), WRK1( NIN + 1 ), SCALE,
     :                  DSCALE, ATOL, BTOL, CONLIM, ITNLIM, INFO, ITN,
     :                  ANORM, ACOND, RNORM, ARNORM, XNORM )

*  If there is no solution, then report an error.
         IF ( INFO .EQ. 3 .OR. INFO .EQ. 6  ) THEN
            STATUS = USER__001
            CALL MSG_SETI( 'INFO', INFO )
            CALL ERR_REP( 'CCD1_SZSLV_LSQR',
     :                    'Error solving for consistent scale ' //
     :                    'factor corrections - ISTOP=^INFO ' //
     :                    'returned by routine PDA_LSQR.', STATUS )
         ELSE

*  If successful, normalise the logarithmic scale factor corrections,
*  either to have a median of zero, or to the reference data array.
*  Because the solution has already been constrained, this corresponds
*  either to correcting for the difference between the mean and the
*  median, or for any minor imperfections in the solution.
            IF ( ( CCD1_IREF .LT. 1 ) .OR.
     :           ( CCD1_IREF .GT. NIN ) ) THEN
               CALL CCD1_NRMED( NIN, SCALE, CNORM, WRK1, STATUS )
            ELSE
               CNORM = SCALE( CCD1_IREF )
               DO 10 I = 1, NIN
                  SCALE( I ) = SCALE( I ) - CNORM
 10            CONTINUE

*  Set the error on the correction for the reference data array to zero
*  (again correcting for minor imperfections in the solution).
               DSCALE( CCD1_IREF ) = 0.0D0
            END IF

*  Convert the corrections back from their logarithms and adjust the
*  associated error estimates. Set the associated zero point
*  corrections to zero.
            DO 11 I = 1, NIN
               SCALE( I ) = EXP( SCALE( I ) )
               DSCALE( I ) = DSCALE( I ) * SCALE( I )
               ZERO( I ) = 0.0D0
               DZERO( I ) = 0.0D0
 11         CONTINUE
         END IF

*  Zero points only:
*  ================
*  The problem is also linear if we are determining zero point
*  corrections alone.
      ELSE IF ( GETZ ) THEN

*  Apply the weights to the zero point differences.  Set a value of
*  zero beyond the final element to act as a constraint on the mean
*  correction obtained (this is used by the routine CCD1_SZPRD - an
*  extra element exists in the array for this purpose).
         DO 12 I = 1, NCMP
            CCD1_DIFZ( I ) = CCD1_DIFZ( I ) * CCD1_WT1( I )
 12      CONTINUE
         CCD1_DIFZ( NCMP + 1 ) = 0.0D0

*  Invoke a PDA routine to solve for a set of consistent scale factor
*  corrections.
         DAMP = 0.0D0
         ATOL = 0.0D0
         BTOL = 0.0D0
         CONLIM = 0.0D0
         ITNLIM = 4 * NCMP
         CALL PDA_LSQR( NCMP + 1, NIN, CCD1_SZPRD, DAMP, 1, 1, W, IW,
     :                  CCD1_DIFZ, WRK1( 1 ), WRK1( NIN + 1 ), ZERO,
     :                  DZERO, ATOL, BTOL, CONLIM, ITNLIM, INFO, ITN,
     :                  ANORM, ACOND, RNORM, ARNORM, XNORM )

*  If there is no solution, then report an error.
         IF ( INFO .EQ. 3 .OR. INFO .EQ. 6  ) THEN
            STATUS = USER__001
            CALL MSG_SETI( 'INFO', INFO )
            CALL ERR_REP( 'CCD1_SZSLV_F04QAF',
     :                    'Error solving for consistent zero ' //
     :                    'point corrections - ISTOP=^INFO ' //
     :                    'returned by routine PDA_LSQR.', STATUS )
         ELSE

*  If successful, normalise the zero point corrections, either to have
*  a median of zero, or to the reference data array.  Because the
*  solution has already been constrained, this corresponds either to
*  correcting for the difference between the mean and the median, or
*  for any minor imperfections in the solution.
            IF ( ( CCD1_IREF .LT. 1 ) .OR.
     :           ( CCD1_IREF .GT. NIN ) ) THEN
               CALL CCD1_NRMED( NIN, ZERO, CNORM, WRK1, STATUS )
            ELSE
               CNORM = ZERO( CCD1_IREF )
               DO 13 I = 1, NIN
                  ZERO( I ) = ZERO( I ) - CNORM
 13            CONTINUE

*  Set the error on the correction for the reference data array to zero
*  (again correcting for minor imperfections in the solution).
               DZERO( CCD1_IREF ) = 0.0D0
            END IF

*  Set the associated scale factor corrections to unity.
            DO 14 I = 1, NIN
               SCALE( I ) = 1.0D0
               DSCALE( I ) = 0.0D0
 14         CONTINUE
         END IF

*  If neither scale factors nor zero points are required, then return
*  null values.
      ELSE
         DO 15 I = 1, NIN
            SCALE( I ) = 1.0D0
            DSCALE( I ) = 0.0D0
            ZERO( I ) = 0.0D0
            DZERO( I ) = 0.0D0
 15       CONTINUE
      END IF

      END
* $Id$
