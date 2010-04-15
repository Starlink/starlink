      SUBROUTINE SPD_WFPC( NELM, FITPAR, XVAL, WGHT,
     :   THETA, ALPHA, TEMPE, UNITY, HESSE, COVAR, FITTED, STATUS )
*+
*  Name:
*     SPD_WFPC

*  Purpose:
*     Calculate covariance for multi-diluted-Planck fit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WFPC( NELM, FITPAR, XVAL, WGHT, THETA, ALPHA, TEMPE,
*        UNITY, HESSE, COVAR, FITTED, STATUS )

*  Description:
*     This routine calculates the Hesse matrix of second derivatives of
*     chi-squared for a multi-diluted-Planck function with partly fixed
*     parameters, and inverts the Hessian to work out the covariance
*     matrix.
*     The approximation is used that the (i,j)-th element of the Hessian
*     is the product of the derivatives of the fitting function w.r.t.
*     i-th and w.r.t. j-th parameter. (Bevington 1969, Press et al.
*     1986).
*
*         d^2 chi^2         [  1  df(x_l) df(x_l) ]
*        ----------- ~= sum [ --- ------- ------- ]
*         da_i da_j      l  [ V_l  da_i    da_j   ]
*
*     Since 10^f is the sum of components 10^f_j, the derivatives of f
*     are not a simple sum of derivatives of f_j:
*
*         df/dc_j = 10^(f_j-f) * df_j/dc_j
*                 + sum{ 10^(f_m-f) * df_m/dc_m }

*  Arguments:
*     NELM = INTEGER (Given)
*        Number of data points fitted.
*     FITPAR = INTEGER (Given)
*        Number of free parameters.
*     XVAL( NELM ) = REAL (Given)
*        Abscissa values x_l.
*     WGHT( NELM ) = REAL (Given)
*        Weight values w_l. These should be 1/variance or
*        1/(covariance_row_sum)
*     THETA( 6 ) = REAL (Given)
*        Theta scaling constant for each component.
*     ALPHA( 6 ) = REAL (Given)
*        Alpha emissivity exponent for each component.
*     TEMPE( 6 ) = REAL (Given)
*        Log colour temperature for each component.
*     UNITY( FITPAR, FITPAR ) = DOUBLE PRECISION (Returned)
*        Unity matrix.
*     HESSE( FITPAR, FITPAR ) = DOUBLE PRECISION (Returned)
*        Hesse matrix.
*     COVAR( FITPAR, FITPAR ) = DOUBLE PRECISION (Returned)
*        If VARUSE is true, COVAR returns the covariance matrix.
*        If VARUSE is false, COVAR returns the covariance matrix divided
*        by the square of the rms.
*     FITTED = LOGICAL (Returned)
*        True if calculating the covariance matrix was successful.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     Bevington, P.R., 1969, Data reduction and error analysis for
*     the physical sciences, McGraw-Hill, New York, San Francisco,
*     St. Louis, Toronto, London, Sydney
*
*     Press, W.H., Flannery, B.P., Teukolsky, S.A., Vetterlink, W.T.,
*     1986, Numerical recipes, The art of scientific computing,
*     Cambridge University Press, Cambridge, London, New York, New
*     Rochelle, Melbourne, Sydney

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28 Jan 1993 (hme):
*        Adapted from SPABG and SPACT.
*        Restructured like SPACT, because f is not a simple sum of f_j.
*     24 Jun 1993 (hme):
*        Escape to label 500 after warning.
*        Replace the half-baked concept of warnings with proper error
*        reporting. The calling routine must then supress reports and
*        ignore status.
*     21 Nov 1995 (hme):
*        Use PDA instead of NAG
*     15 Aug 2005 (timj):
*        ** -LOGDIF is not standards compliant. Use parentheses
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'SPD_WFCM'         ! Line fitting common block

*  Arguments Given:
      INTEGER NELM
      INTEGER FITPAR
      REAL XVAL( NELM )
      REAL WGHT( NELM )
      REAL THETA( MAXCMP )
      REAL ALPHA( MAXCMP )
      REAL TEMPE( MAXCMP )

*  Arguments Returned:
      DOUBLE PRECISION UNITY( FITPAR, FITPAR )
      DOUBLE PRECISION HESSE( FITPAR, FITPAR )
      DOUBLE PRECISION COVAR( FITPAR, FITPAR )
      LOGICAL FITTED

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER K1                 ! Free parameters' outer loop index
      INTEGER K2                 ! Free parameters' inner loop index
      INTEGER M                  ! Loop index to count and identify ties
      INTEGER I                  ! Loop index trough data
      INTEGER J                  ! Loop index to count components
      INTEGER LV( 3*MAXCMP )     ! All parameters' index corresp. to K1
      INTEGER JV( 3*MAXCMP )     ! K1's component number
      INTEGER NV( 3*MAXCMP )     ! Number of comps tied to J1 wrt K1
      INTEGER TV( MAXCMP, 3*MAXCMP ) ! List of ties to J1 wrt K1
      INTEGER IPVT( 3*MAXCMP )   ! Pivot vector
      INTEGER IFAIL              ! Error code from NAG routine
      DOUBLE PRECISION XI        ! Current x value
      DOUBLE PRECISION FJ( MAXCMP ) ! f_j(x_i)
      DOUBLE PRECISION FI        ! f(x_i)
      DOUBLE PRECISION LOGDIF    ! Relative importance of components
      DOUBLE PRECISION DFDAK1    ! df/da_K1
      DOUBLE PRECISION DFDAK2    ! df/da_K2
      DOUBLE PRECISION WKSPCE( 3*MAXCMP ) ! Workspace for NAG routine
      DOUBLE PRECISION HESMIN    ! Minimum matrix element
      DOUBLE PRECISION HESMAX    ! Maximum matrix element
      DOUBLE PRECISION HESCAL    ! Matrix scaling factor

*  Internal declarations:
      REAL SPD_UAAVR             ! Fit function derivative value
      DOUBLE PRECISION SPD_UAAQD ! Planck function value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Double loop through elements of unity matrix and Hesse matrix.
      DO 2 K1 = 1, FITPAR
         DO 1 K2 = 1, FITPAR
            HESSE(K2,K1) = 0D0
            UNITY(K2,K1) = 0D0
            IF ( K1.EQ.K2 ) UNITY(K2,K1) = 1D0
 1       CONTINUE
 2    CONTINUE

*  Work out index relations and ties.
*  This is needed to work out the derivatives below.
      DO 7 K1 = 1, FITPAR

*     K1 is an index in the array of free parameters. The corresponding
*     index in the array of all (free, fixed, tied, unused) parameters
*     (all Thetas followed by all alphas followed by all log(T)) is
*     LV(K1). It is derived from K1 by inverting the permutation PARNO.
         LV(K1) = 1
 3       CONTINUE                ! Start of 'DO WHILE' loop
         IF ( PARNO(LV(K1)) .NE. K1 ) THEN
            LV(K1) = LV(K1) + 1
            GO TO 3
         END IF

*     From comparing LV(K1) with multiples of MAXCMP follows the
*     component number JV(K1) to which the free parameter in question
*     belongs. With the component JV(K1) known, we can scan the array of
*     flags for other parameters tied to this one. NV(K1) tells how many
*     there are and TV(,K1) is the array telling which components these
*     are. (Only corresponding parameters are tied, e.g. Thetas with
*     Thetas.)

*     K1-th free parameter is JV(K1)-th Theta.
         IF ( LV(K1) .LE. MAXCMP ) THEN
            JV(K1) = LV(K1)
            NV(K1) = 0
            DO 4 M = JV(K1)+1, NCOMP
               IF ( PARFLG(M) .EQ. JV(K1) ) THEN
                  NV(K1) = NV(K1) + 1
                  TV(NV(K1),K1) = M
               END IF
 4          CONTINUE

*     K1-th free parameter is JV(K1)-th alpha.
         ELSE IF ( LV(K1) .LE. 2*MAXCMP ) THEN
            JV(K1) = LV(K1) - MAXCMP
            NV(K1) = 0
            DO 5 M = JV(K1)+1, NCOMP
               IF ( PARFLG(M+MAXCMP) .EQ. JV(K1) ) THEN
                  NV(K1) = NV(K1) + 1
                  TV(NV(K1),K1) = M
               END IF
 5          CONTINUE

*     K1-th free parameter is JV(K1)-th dispersion.
         ELSE
            JV(K1) = LV(K1) - 2 * MAXCMP
            NV(K1) = 0
            DO 6 M = JV(K1)+1, NCOMP
               IF ( PARFLG(M+2*MAXCMP) .EQ. JV(K1) ) THEN
                  NV(K1) = NV(K1) + 1
                  TV(NV(K1),K1) = M
               END IF
 6          CONTINUE
         END IF
 7    CONTINUE

*  Contrary to SPABG, we make the data loop (I) the outer loop.
*  This is necessary, because for each df/da_k(x_i) we need to know
*  all f_j(x_i), and we want to work that out only once per x_i.
      DO 18 I = 1, NELM
         XI = DBLE( XVAL(I) )

*     For each component j=J, work out the f_j(x_i).
         DO 8 J = 1, NCOMP
            FJ(J) = DBLE(THETA(J)) + DBLE(ALPHA(J)) * XI
     :            + SPD_UAAQD( DBLE(TEMPE(J)), XI )
 8       CONTINUE

*     Sum up into f(x_i), but keep all f_j(x_i) in mind.
         FI = FJ(1)
         DO 9 J = 2, NCOMP
            LOGDIF = ABS( FI - FJ(J) )
            FI = MAX( FI, FJ(J) )
            IF ( LOGDIF .LT. 15D0 )
     :         FI = FI + LOG10( 1D0 + 1D1 ** (-LOGDIF) )
 9       CONTINUE

*     Now add to each Hesse matrix element the contribution of this data
*     point x_i. We do this now only for the K2 .GE. K1 part and copy
*     the other half later, when the data loop is finished.
         DO 17 K1 = 1, FITPAR
            K2 = K1

*        Work out df/dak1(x_i).

*        If this is a Theta parameter.
            IF ( LV(K1) .LE. MAXCMP ) THEN
               DFDAK1 = 1D1 ** ( FJ(JV(K1)) - FI )
     :                * DBLE( SPD_UAAVR( 1, THETA(JV(K1)),
     :                  ALPHA(JV(K1)), TEMPE(JV(K1)), XVAL(I) ) )

*           For each tie m to j TV(1..NV(K1),K1).
               DO 10 M = 1, NV(K1)
                  DFDAK1 = DFDAK1 + 1D1 ** ( FJ(TV(M,K1)) - FI )
     :                  * DBLE( SPD_UAAVR( 1, THETA(TV(M,K1)),
     :                    ALPHA(TV(M,K1)), TEMPE(TV(M,K1)), XVAL(I) ) )
 10            CONTINUE

*        Else if this is an alpha parameter.
            ELSE IF ( LV(K1) .LE. 2*MAXCMP ) THEN
               DFDAK1 = 1D1 ** ( FJ(JV(K1)) - FI )
     :                * DBLE( SPD_UAAVR( 2, THETA(JV(K1)),
     :                  ALPHA(JV(K1)), TEMPE(JV(K1)), XVAL(I) ) )

*           For each tie m to j TV(1..NV(K1),K1).
               DO 11 M = 1, NV(K1)
                  DFDAK1 = DFDAK1 + 1D1 ** ( FJ(TV(M,K1)) - FI )
     :                  * DBLE( SPD_UAAVR( 2, THETA(TV(M,K1)),
     :                    ALPHA(TV(M,K1)), TEMPE(TV(M,K1)), XVAL(I) ) )
 11            CONTINUE

*        Else (this is a lg(T) parameter).
            ELSE
               DFDAK1 = 1D1 ** ( FJ(JV(K1)) - FI )
     :                * DBLE( SPD_UAAVR( 3, THETA(JV(K1)),
     :                  ALPHA(JV(K1)), TEMPE(JV(K1)), XVAL(I) ) )

*           For each tie m to j TV(1..NV(K1),K1).
               DO 12 M = 1, NV(K1)
                  DFDAK1 = DFDAK1 + 1D1 ** ( FJ(TV(M,K1)) - FI )
     :                  * DBLE( SPD_UAAVR( 3, THETA(TV(M,K1)),
     :                    ALPHA(TV(M,K1)), TEMPE(TV(M,K1)), XVAL(I) ) )
 12            CONTINUE
            END IF

*        Add contribution to diagonal element of Hesse matrix.
            HESSE(K1,K1) = HESSE(K1,K1) + DFDAK1 * DFDAK1 * WGHT(I)

*        Now the off-diagonal elements K2 .GT. K1.
            DO 16 K2 = K1+1, FITPAR

*           Work out df/dak2(x_i).

*           If this is a Theta parameter.
               IF ( LV(K2) .LE. MAXCMP ) THEN
                  DFDAK2 = 1D1 ** ( FJ(JV(K2)) - FI )
     :               * DBLE( SPD_UAAVR( 1, THETA(JV(K2)), ALPHA(JV(K2)),
     :                 TEMPE(JV(K2)), XVAL(I) ) )

*              For each tie m to j TV(1..NV(K2),K2).
                  DO 13 M = 1, NV(K2)
                     DFDAK2 = DFDAK2 + 1D1 ** ( FJ(TV(M,K2)) - FI )
     :                  * DBLE( SPD_UAAVR( 1, THETA(TV(M,K2)),
     :                    ALPHA(TV(M,K2)), TEMPE(TV(M,K2)), XVAL(I) ) )
 13               CONTINUE

*           Else if this is an alpha parameter.
               ELSE IF ( LV(K2) .LE. 2*MAXCMP ) THEN
                  DFDAK2 = 1D1 ** ( FJ(JV(K2)) - FI )
     :               * DBLE( SPD_UAAVR( 2, THETA(JV(K2)), ALPHA(JV(K2)),
     :                 TEMPE(JV(K2)), XVAL(I) ) )

*              For each tie m to j TV(1..NV(K2),K2).
                  DO 14 M = 1, NV(K2)
                     DFDAK2 = DFDAK2 + 1D1 ** ( FJ(TV(M,K2)) - FI )
     :                  * DBLE( SPD_UAAVR( 2, THETA(TV(M,K2)),
     :                    ALPHA(TV(M,K2)), TEMPE(TV(M,K2)), XVAL(I) ) )
 14               CONTINUE

*           Else (this is a lg(T) parameter).
               ELSE
                  DFDAK2 = 1D1 ** ( FJ(JV(K2)) - FI )
     :               * DBLE( SPD_UAAVR( 3, THETA(JV(K2)), ALPHA(JV(K2)),
     :                 TEMPE(JV(K2)), XVAL(I) ) )

*              For each tie m to j TV(1..NV(K2),K2).
                  DO 15 M = 1, NV(K2)
                     DFDAK2 = DFDAK2 + 1D1 ** ( FJ(TV(M,K2)) - FI )
     :                  * DBLE( SPD_UAAVR( 3, THETA(TV(M,K2)),
     :                    ALPHA(TV(M,K2)), TEMPE(TV(M,K2)), XVAL(I) ) )
 15               CONTINUE
               END IF

*           Add contribution to off-diagonal element of Hesse matrix.
               HESSE(K2,K1) = HESSE(K2,K1) + DFDAK1 * DFDAK2 * WGHT(I)
 16         CONTINUE
 17      CONTINUE
 18   CONTINUE

*  Must still copy the K2 .LT. K1 part of the Hesse matrix.
      DO 20 K1 = 1, FITPAR
         DO 19 K2 = 1, K1-1
            HESSE(K2,K1) = HESSE(K1,K2)
 19      CONTINUE
 20   CONTINUE

*  Get the extreme absolute values of diagonal elements.
*  The hope is that these include the most extreme values.
*  The motivation is that matrix elements should not be too great so
*  that F04AFF does not crash.
      HESMIN = ABS( HESSE(1,1) )
      HESMAX = HESMIN
      DO 21 K1 = 2, FITPAR
         HESMIN = MIN( HESMIN, ABS( HESSE(K1,K1) ) )
         HESMAX = MAX( HESMAX, ABS( HESSE(K1,K1) ) )
 21   CONTINUE

*  Scaling factor.
      HESCAL = SQRT(HESMIN) * SQRT(HESMAX)
      IF ( HESCAL .GT. 0D0 ) THEN
         HESCAL = 1D0 / HESCAL
      ELSE
         HESCAL = 1D0
      END IF

*  Preemptive scaling of the matrix.
      DO 23 K1 = 1, FITPAR
         DO 22 K2 = 1, FITPAR
            COVAR(K2,K1) = HESSE(K2,K1) * HESCAL
 22      CONTINUE
 23   CONTINUE

*  Inverting the Hessian results in the covariance.
      FITTED = .TRUE.
      CALL PDA_DGEFA( COVAR, FITPAR, FITPAR, IPVT, IFAIL )
      IF ( IFAIL .EQ. 0 ) THEN
         CALL PDA_DGEDI( COVAR, FITPAR, FITPAR, IPVT, 0D0, WKSPCE, 1 )
      END IF

*  Deal with error conditions. A failure of F04AAF is non-fatal.
      IF ( IFAIL .NE. 0 ) THEN

*     In case of any failure of F04AAF set covariance matrix to zero.
         DO 25 K1 = 1, FITPAR
            DO 24 K2 = 1, FITPAR
               COVAR(K2,K1) = 0D0
 24         CONTINUE
 25      CONTINUE
         FITTED = .FALSE.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WFPC_NAGERR', 'SPD_WFPC: Error ' //
     :      'inverting Hesse matrix (calulating covariance).', STATUS )
         GO TO 500
      END IF

*  Post-inversion scaling of the matrix.
      DO 27 K1 = 1, FITPAR
         DO 26 K2 = 1, FITPAR
            COVAR(K2,K1) = COVAR(K2,K1) * HESCAL
 26      CONTINUE
 27   CONTINUE

*  Return.
 500  CONTINUE
      END
