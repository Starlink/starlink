      SUBROUTINE SPD_WZEA( COVRSX, NELM, NCOMP, FITPAR, FITDIM,
     :   CONT, XDWC, CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, FWHM,
     :   CHISQR, COVAR, NITER, STATUS )
*+
*  Name:
*     SPD_WZEA

*  Purpose:
*     Fit multi-Gauss profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZEA( COVRSX, NELM, NCOMP, FITPAR, FITDIM,
*        CONT, XDWC, CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, FWHM,
*        CHISQR, COVAR, NITER, STATUS )

*  Description:
*     Interface FITGAUSS with E04DGF, SPFGAU and SPFHSS.
*     This routine shares the COMMON block SPFCM1 with SPFGAU and
*     SPFHSS. SPFGAU supplies the object function for E04DGF and its
*     gradient. SPFHSS evaluates the curvature of chi-squared (or
*     psi-squared) to work out fit parameter covariances.

*  Arguments:
*     COVRSX = INTEGER (Given)
*        Non-zero if 2nd weights are available and to be used for the Hesse
*        matrix. This is advantageous if the fitted data have been
*        resampled. The 2nd weights then should be the reciprocals of
*        the sums over any row of the covariance matrix of the given
*        data set. See Meyerdierks, 1992.
*     NELM = INTEGER (Given)
*        Size of the data arrays.
*     NCOMP = INTEGER (Given)
*        The number of Gauss components to be fitted.
*     FITPAR = INTEGER (Given)
*        The number of free parameters to be fitted.
*     FITDIM = INTEGER (Given)
*        max( 1, FITPAR ). For dimension purposes.
*     CONT = REAL (Given)
*        The level of the continuum underlying the Gauss components.
*        Any constant value for the continuum is possible. This is not
*        a fit parameter, but must be known a priori.
*     XDWC( 4 * NELM ) = REAL (Given)
*        The pointer to the array of masked x, data, weights, and
*        covariance row sums. The array is REAL and of length 4*NELM.
*        Weights should be 1/variance, 1 if variance is not known.
*        All the arrays must not contain bad values.
*           XDWC(        1 :   NELM ): x values,
*           XDWC(   NELM+1 : 2*NELM ): data values,
*           XDWC( 2*NELM+1 : 3*NELM ): weight values,
*           XDWC( 3*NELM+1 : 4*NELM ): 2nd weight values.
*        If available, the 2nd weights are passed to SPFHSS. Otherwise
*        the weights are passed.
*     CFLAGS( MAXGAU ) = INTEGER (Given)
*        For each Gauss component I a value CFLAGS(I)=0 indicates that
*        CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J).
*     PFLAGS( MAXGAU ) = INTEGER (Given)
*        For each Gauss component I a value PFLAGS(I)=0 indicates that
*        PEAK(I) holds a guess which is free to be fitted.
*        A positive value PFLAGS(I)=I indicates that PEAK(I) is fixed.
*        A positive value PFLAGS(I)=J<I indicates that PEAK(I) has to
*        keep a fixed ratio to PEAK(J).
*     SFLAGS( MAXGAU ) = INTEGER (Given)
*        For each Gauss component I a value SFLAGS(I)=0 indicates that
*        FWHM(I) holds a guess which is free to be fitted.
*        A positive value SFLAGS(I)=I indicates that FWHM(I) is fixed.
*        A positive value SFLAGS(I)=J<I indicates that FWHM(I) has to
*        keep a fixed ratio to FWHM(J).
*     CENTRE( MAXGAU ) = REAL (Given and Returned)
*        Centre position for each Gauss component.
*     PEAK( MAXGAU ) = REAL (Given and Returned)
*        Peak height for each Gauss component.
*     FWHM( MAXGAU ) = REAL (Given and Returned)
*        Full width at half maximum for each Gauss component.
*     CHISQR = REAL (Returned)
*        The final chi-squared value for the fit. That is, if the
*        weights were 1/variance. If all weights were 1, then this is
*        the sum of squared residuals. Then the rms would be
*        SQRT(CHISQR/degrees_of_freedom).
*     COVAR( FITDIM, FITDIM ) = DOUBLE PRECISION (Returned)
*        The matrix of covariances between fitted parameters. That is,
*        if the weights were 1/variance. If all weights were 1, then
*        the covariance would be COVAR*CHISQR/degrees_of_freedom.
*     NITER = INTEGER (Returned)
*        Positive if fit was successful, -1 else. A positive value
*        indicated the number of iterations needed.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine returns a bad status if
*        -  it was entered with one,
*        -  SPFHSS returns one,
*        -  there are no free parameters or no degrees of freedom,
*        -  there are errors in the ties between parameters,
*        -  E04DGF returns with a bad NAG status (including the case
*           where the fit did not converge before the iteration limit),
*        -  a free peak or width parameters is fitted as exactly zero.
*     NGAUSS = INTEGER (Common /SPFCM1/)
*        Number of Gauss components.
*     DCONT = DOUBLE PRECISION (Common /SPFCM1/)
*        The constant continuum underlying the Gauss profiles.
*     FSCALE = DOUBLE PRECISION (Common /SPFCM1/)
*        Scaling factor. The objective function is proportional to
*        1/FSCALE. FSCALE should be such that the objective function is
*        of order unity near its minimum.
*     PARNO( 3*MAXGAU ) = INTEGER (Common /SPFCM1/)
*        Permutation vector for parameters.
*        PAR1(PARNO(I)) corresponds to PAR0(I). PAR1 is not part of this
*        COMMON block.
*        First in the array PAR1 come all the free line centres in
*        ascending order of component number. Then come all free line
*        peaks in ascending order of component number. Then come all
*        free line dispersions.
*        Last in the array PAR1 come all fixed or tied or unused centres
*        in descending order of component number. Before that come all
*        fixed or tied or unused peaks in descending order of component
*        number. Before that come all fixed or tied of unused line
*        dispersions in descending order of component number.
*     PARFLG( 3*MAXGAU ) = INTEGER (Common /SPFCM1/)
*        A packed version of the fit flags:
*        PARFLG={CFLAGS(1) ... CFLAGS(NGAUSS),NGAUSS+1,...,MAXGAU,
*                PFLAGS(1) ... PFLAGS(NGAUSS),NGAUSS+1,...,MAXGAU,
*                SFLAGS(1) ... SFLAGS(NGAUSS),NGAUSS+1,...,MAXGAU}
*        For each Gauss component I a value e.g. CFLAGS(I)=0 indicates
*        that CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J). A positive value PFLAGS(I)
*        or SFLAGS(I)=J<I indicates that PEAK(I) or SIGMA(I) has to keep
*        a fixed ratio to PEAK(J) or SIGMA(J).
*        Deviating from CFLAGS etc., unused components' parameters are
*        flagged as fixed.
*     PAR0( 3*MAXGAU ) = DOUBLE PRECISION (Common /SPFCM1/)
*        A packed version of the unscaled guess parameters:
*        PAR0={CENTRE(1) ... CENTRE(NGAUSS),0,0,...,0,
*                PEAK(1) ...   PEAK(NGAUSS),0,0,...,0,
*               SIGMA(1) ...  SIGMA(NGAUSS),1,1,...,1}
*        Deviating from CENTRE etc., unused components' parameters are
*        set to 0 or 1, whichever causes less harm.

*  References:
*     Meyerdierks, H., 1992, Covariance in Resampling and Model Fitting,
*     Starlink, Spectroscopy Special Interest Group

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 May 1991 (hme):
*        Original version (DOFIT).
*     26 Jun 1991 (hme):
*        Calculate the covariance matrix for Gauss fit by calling
*        SPFHSS.
*        Proper error check after AUTOFIT.
*        Make CONTFIT oblivious, call E02ADF directly.
*        Replace variance by weights. Masked x, data, weights DOUBLE.
*     19 Jul 1991 (hme):
*        Replace AUTOFIT. Call E04DGF instead. Its objective function is
*        SPFGAU. Use a packed array XDWC instead of three separate.
*     22 Jul 1991 (hme):
*        Correct size of ALFA and UNITY. Scrap oblivious arguments.
*        Rename matrix A to COVAR. FITDIM.
*     23 Jul 1991 (hme):
*        Report no. of iterations from E04DGF.
*        Put fit results in ADAM parameters FITCENT, FITPEAK, FITFWHM.
*     29 Oct 1991 (hme):
*        Call the re-written SPFHSS. Rename ALFA to HESSE.
*     27 Nov 1991 (hme):
*        Call NAG with IFAIL=+1 to suppress NAG's error messages.
*        Polish FITGAUSS messages.
*     22 Apr 1992 (hme):
*        Disable polynomial fit. Make masked arrays _REAL. Provide for
*        using covariance row sums. Pass x values and weights separately
*        to SPFHSS. Don't put fitted values into ADAM parameters any
*        more.
*     12 Aug 1992 (hme):
*        Re-arrange COMMON block, N*8 byte groups first, NGAUSS last.
*     08 Jun 1993 (hme):
*        Also return chi-squared. This simplifies scaling of COVAR
*        matrix in case where all weights were 1.
*     10 Jun 1993 (hme):
*        Replace the half-baked concept of warnings with proper error
*        reporting. The calling routine must then supress reports and
*        ignore status.
*     23 Jun 1993 (hme):
*        After fit, see that sigma is positive.
*        Increase iteration limit to 200.
*     30 Jun 1993 (hme):
*        Reject zero peak or with as a result.
*     26 Apr 1994 (hme):
*        Adapt from SPFDFT. Small changes to make this callable from a C
*        routine that has only a pointer to the packed array of masked
*        vectors. Replace sigma with FWHM in the argument list (but not
*        in the common block).
*     10 May 1994 (hme):
*        Change the packed array back to being a REAL array, not a
*        pointer.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  External References:
      EXTERNAL SPFGAU            ! Objective function for E04DGF

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INTEGER MAXGAU             ! Maximum number of Gauss components
      PARAMETER ( MAXGAU = 6 )

*  Global Variables:
      INTEGER NGAUSS
      DOUBLE PRECISION DCONT
      DOUBLE PRECISION FSCALE
      INTEGER PARNO( 3*MAXGAU )
      INTEGER PARFLG( 3*MAXGAU )
      DOUBLE PRECISION PAR0( 3*MAXGAU )
      COMMON /SPFCM1/ DCONT, FSCALE, PARNO, PARFLG, PAR0, NGAUSS

*  Arguments Given:
      INTEGER COVRSX
      INTEGER NELM
      INTEGER NCOMP
      INTEGER FITPAR
      INTEGER FITDIM
      REAL CONT
      REAL XDWC( 4 * NELM )
      INTEGER CFLAGS( MAXGAU )
      INTEGER PFLAGS( MAXGAU )
      INTEGER SFLAGS( MAXGAU )

*  Arguments Given and Returned:
      REAL CENTRE( MAXGAU )
      REAL PEAK(   MAXGAU )
      REAL FWHM(   MAXGAU )

*  Arguments Returned:
      REAL CHISQR
      DOUBLE PRECISION COVAR( FITDIM, FITDIM )
      INTEGER NITER

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER REALSZ             ! Bytes per real
      PARAMETER ( REALSZ = 4 )
      REAL RT8LN2                ! Square root of 8 ln(2)
      PARAMETER ( RT8LN2 = 2.354820 )

*  Local Variables:
      REAL SIGMA( MAXGAU )
      LOGICAL FITTED             ! Fit success flag
      INTEGER I, J, K            ! Loop indices
      INTEGER IFAIL1             ! NAG status
      INTEGER IUSER(1)           ! Copy of NELM
      INTEGER IWORK( 3*MAXGAU+1 )! Work space for E04DGF
      DOUBLE PRECISION
     :   PAR1(     3*MAXGAU )    ! Permuted and scaled PAR0
      DOUBLE PRECISION FVAL      ! Value of objective function
      DOUBLE PRECISION
     :   GRADF(    3*MAXGAU )    ! Objective function's gradient
      DOUBLE PRECISION
     :   WORK3( 13*3*MAXGAU )    ! Work space for E04DGF
      DOUBLE PRECISION
     :   UNITY( 9*MAXGAU*MAXGAU )! Work space for SPFHSS
      DOUBLE PRECISION
     :   HESSE( 9*MAXGAU*MAXGAU )! Work space for SPFHSS

*.

*  Check.
      IF ( STATUS .NE. SAI__OK ) RETURN
      IF ( FITPAR .LT. 1 .OR.
     :     NCOMP  .LT. 1 .OR. NCOMP .GT. MAXGAU .OR.
     :     NELM   .LT. FITPAR ) THEN
         FITTED = .FALSE.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WZEA_E01', 'SPD_WZEA: Error: No fit ' //
     :      'performed. Probably too few or too many free ' //
     :      'parameters, or no degrees of freedom.', STATUS )
         GO TO 500
      END IF

*  Convert FWHM to dispersion.
      DO 1002 I = 1, NCOMP
         SIGMA(I) = FWHM(I) / RT8LN2
 1002 CONTINUE

*  Check flags and parameters and put them into the common block.
      DO 1 I = 1, NCOMP

*     Check that ties are only to earlier components.
         IF ( CFLAGS(I) .LT. 0 .OR. CFLAGS(I) .GT. I .OR.
     :        PFLAGS(I) .LT. 0 .OR. PFLAGS(I) .GT. I .OR.
     :        SFLAGS(I) .LT. 0 .OR. SFLAGS(I) .GT. I ) THEN
            FITTED = .FALSE.
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_WZEA_E02', 'SPD_WZEA: Error: Can tie ' //
     :         'only to earlier component.', STATUS )
            GO TO 500
         END IF

*     Check that centre ties are only to free centres.
         IF ( CFLAGS(I) .NE. 0 .AND. CFLAGS(I) .NE. I ) THEN
            IF ( CFLAGS(CFLAGS(I)) .NE. 0 ) THEN
               FITTED = .FALSE.
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_WZEA_E03', 'SPD_WZEA: Error: Can ' //
     :            'tie only to free parameter.', STATUS )
               GO TO 500
            END IF
         END IF

*     Check that peak ties are only to free peaks.
         IF ( PFLAGS(I) .NE. 0 .AND. PFLAGS(I) .NE. I ) THEN
            IF ( PFLAGS(PFLAGS(I)) .NE. 0 ) THEN
               FITTED = .FALSE.
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_WZEA_E03', 'SPD_WZEA: Error: Can ' //
     :            'tie only to free parameter.', STATUS )
               GO TO 500
            END IF
         END IF

*     Check that width ties are only to free widths.
         IF ( SFLAGS(I) .NE. 0 .AND. SFLAGS(I) .NE. I ) THEN
            IF ( SFLAGS(SFLAGS(I)) .NE. 0 ) THEN
               FITTED = .FALSE.
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_WZEA_E03', 'SPD_WZEA: Error: Can ' //
     :            'tie only to free parameter.', STATUS )
               GO TO 500
            END IF
         END IF

*     Packed array of all Gauss parameters.
         PAR0(I)          = DBLE(CENTRE(I))
         PAR0(I+MAXGAU)   = DBLE(PEAK(I))
         PAR0(I+2*MAXGAU) = DBLE(SIGMA(I))

*     Packed array of all fit flags.
         PARFLG(I)          = CFLAGS(I)
         PARFLG(I+MAXGAU)   = PFLAGS(I)
         PARFLG(I+2*MAXGAU) = SFLAGS(I)
 1    CONTINUE

*  Fill the COMMON block, except for PARNO.
*  FSCALE is set to 2 for the initial direct call to SPFGAU. This is
*  in anticipation that the final value of the objective function
*  will be 1/2 of its value for the guess.
*  All flags for unused components are set to 1 (fixed) in the COMMON
*  block. Also all unused components' parameters are set to 0 or 1,
*  whichever causes less harm.
      NGAUSS = NCOMP
      DCONT  = DBLE(CONT)
      FSCALE = 2D0
      DO 2 I = NCOMP+1, MAXGAU
         PAR0(I)          = 0D0
         PAR0(I+MAXGAU)   = 0D0
         PAR0(I+2*MAXGAU) = 1D0
         PARFLG(I)          = I
         PARFLG(I+MAXGAU)   = I
         PARFLG(I+2*MAXGAU) = I
 2    CONTINUE

*  PARNO: Permutation of parameters,
*  such that free parameters come to the beginning of PAR1.
*  (PAR1 itself need not worry us here, since its guess value is
*  always == 0. But this is done in passing.)
      J = 0
      K = 3 * MAXGAU + 1
      DO 3 I = 1, 3*MAXGAU
         IF ( PARFLG(I) .EQ. 0 ) THEN

*        PAR0(I) is free and corresponds to PAR1(J).
            J = J + 1
            PARNO(I) = J
         ELSE

*        PAR0(I) is not free and corresponds to PAR1(K).
            K = K - 1
            PARNO(I) = K
         END IF
         PAR1(I) = 0D0
 3    CONTINUE

*  Check if permutation went all right.
      IF ( K .NE. J+1 .OR. J .NE. FITPAR ) THEN
         FITTED = .FALSE.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WZEA_E04', 'SPD_WZEA: Error: No fit ' //
     :      'performed. Flags inconsistent with number of free' //
     :      'parameters.', STATUS )
         GO TO 500
      END IF

*  Derive FSCALE by direct call to SPFGAU with initial FSCALE = 2.
      IFAIL1 = 0
      IUSER(1) = NELM
      CALL SPFGAU( IFAIL1, FITPAR, PAR1, FVAL, GRADF, 1,
     :   IUSER, XDWC )
      FSCALE = FVAL

*  This gives the initial chi squared.
      CHISQR = SNGL( FVAL * FSCALE )

*  Set non-default optional parameters for E04DGF.
*  Itns is normally max(50,5n).
      CALL E04DKF( 'Nolist' )
      CALL E04DKF( 'Print Level = 0' )
      CALL E04DKF( 'Verify No' )
      CALL E04DKF( 'Itns = 200 ' )

*  Call E04DGF.
      IFAIL1 = 1
      CALL E04DGF( FITPAR, SPFGAU, NITER, FVAL, GRADF, PAR1, IWORK,
     :   WORK3, IUSER, XDWC, IFAIL1 )

*  Post-fit processing.
      IF ( IFAIL1 .EQ. 0 ) THEN
         FITTED = .TRUE.

*     Last objective function value gives final chi squared.
         CHISQR = SNGL( FVAL * FSCALE )

*     Retrieve free parameters.
         DO 4 J = 1, NCOMP
            IF ( PARFLG(J) .EQ. 0 )
     :         CENTRE(J) = SNGL( PAR0(J)
     :                   + 2D0 * PAR0(J+2*MAXGAU) * PAR1(PARNO(J)) )
            IF ( PARFLG(J+MAXGAU) .EQ. 0 )
     :         PEAK(J)   = SNGL( PAR0(J+MAXGAU)
     :                   * ( 1D0 + PAR1(PARNO(J+MAXGAU)) ) )
            IF ( PARFLG(J+2*MAXGAU) .EQ. 0 )
     :         SIGMA(J)  = ABS( SNGL( PAR0(J+2*MAXGAU)
     :                   * ( 1D0 + PAR1(PARNO(J+2*MAXGAU)) ) ) )
 4       CONTINUE

*     Retrieve tied parameters.
         DO 5 J = 1, NCOMP
            IF ( PARFLG(J) .NE. J .AND. PARFLG(J) .NE. 0 )
     :         CENTRE(J) = CENTRE(PARFLG(J))
     :                   + SNGL( PAR0(J) - PAR0(PARFLG(J)) )
            IF ( PARFLG(J+MAXGAU) .NE. J .AND.
     :           PARFLG(J+MAXGAU) .NE. 0 )
     :         PEAK(J)   = PEAK(PARFLG(J+MAXGAU))
     :                   * SNGL( PAR0(J+MAXGAU)
     :                   / PAR0(PARFLG(J+MAXGAU)+MAXGAU) )
            IF ( PARFLG(J+2*MAXGAU) .NE. J .AND.
     :           PARFLG(J+2*MAXGAU) .NE. 0 )
     :         SIGMA(J)  = SIGMA(PARFLG(J+2*MAXGAU))
     :                   * SNGL( PAR0(J+2*MAXGAU)
     :                   / PAR0(PARFLG(J+2*MAXGAU)+2*MAXGAU) )
 5       CONTINUE

*     If any of the peak or width parameters turns out to be exactly
*     zero, we have an error condition. For one it is a non-existing
*     component. More severely, this will result in problems either with
*     the Hesse matrix or with parameter variances, i.e. there will be
*     divisions by zero later on.
         DO 6 J = 1, NCOMP
            IF ( PEAK(J) .EQ. 0. .OR. SIGMA(J) .EQ. 0. ) THEN
               FITTED = .FALSE.
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_WZEA_E05', 'SPD_WZEA: Error: Zero ' //
     :            'peak or width after fit.', STATUS )
               GO TO 500
            END IF
 6       CONTINUE

*     Covariance matrix. Use 2nd weights if possible, weights otherwise.
         IF ( COVRSX .NE. 0 ) THEN
            CALL SPFHSS( NELM, FITPAR, XDWC(1), XDWC(3*NELM+1),
     :         CENTRE, PEAK, SIGMA,
     :         UNITY, HESSE, COVAR, FITTED, STATUS )
         ELSE
            CALL SPFHSS( NELM, FITPAR, XDWC(1), XDWC(2*NELM+1),
     :         CENTRE, PEAK, SIGMA,
     :         UNITY, HESSE, COVAR, FITTED, STATUS )
         END IF
      ELSE

*     NAG failed.
         FITTED = .FALSE.
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'INT', IFAIL1 )
         CALL ERR_REP( 'SPD_WZEA_E06', 'SPD_WZEA: Error: NAG/E04DGF ' //
     :      'returned with error status ^INT.', STATUS )

*     Explain the most common failure.
         IF ( IFAIL1 .EQ. 3 ) CALL ERR_REP( 'SPD_WZEA_E07',
     :         'No convergence before iteration limit.', STATUS )
         GO TO 500
      END IF

*  Return.
 500  CONTINUE

*  Indicate success in NITER.
      IF ( .NOT. FITTED ) NITER = -1

*  Get FWHM from dispersion.
      DO 1003 I = 1, NCOMP
         FWHM(I) = SIGMA(I) * RT8LN2
 1003 CONTINUE

      END
