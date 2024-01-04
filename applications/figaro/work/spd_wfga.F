#if HAVE_CONFIG_H
#  include "config.h"
#endif
      SUBROUTINE SPD_WFGA( INFO, COVRSX, NELM, NCOMP_ARG,
     :   FITPAR, FITDIM,
     :   CONT, XDWC, CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, SIGMA,
     :   CHISQR, COVAR, FITTED, STATUS )
*+
*  Name:
*     SPD_WFGA

*  Purpose:
*     Fit multi-Gauss profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WFGA( INFO, COVRSX, NELM, NCOMP, FITPAR, FITDIM,
*        CONT, XDWC, CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, SIGMA,
*        CHISQR, COVAR, FITTED, STATUS )

*  Description:
*     This routine interfaces FITGAUSS with the fit routine,
*     its object function routine and the Hesse matrix evaluation.

*  Arguments:
*     INFO = LOGICAL (Given)
*        Ignored.
*     COVRSX = LOGICAL (Given)
*        True if 2nd weights are available and to be used for the Hesse
*        matrix. This is advantageous if the fitted data have been
*        resampled. The 2nd weights then should be the reciprocals of
*        the sums over any row of the covariance matrix of the given
*        data set. See Meyerdierks, 1992.
*     NELM = INTEGER (Given)
*        Size of the data arrays.
*     NCOMP = INTEGER (Given)
*        The number of components to be fitted.
*     FITPAR = INTEGER (Given)
*        The number of free parameters to be fitted.
*     FITDIM = INTEGER (Given)
*        max( 1, FITPAR ). For dimension purposes.
*     CONT = REAL (Given)
*        The level of the continuum underlying the Gauss components.
*        Any constant value for the continuum is possible. This is not
*        a fit parameter, but must be known a priori.
*     XDWC( 4*NELM ) = REAL (Given)
*        The packed array of masked x, data, weight and covariance row
*        sums. Weights should be 1/variance, 1 if variance is not known.
*        All the arrays must not contain bad values.
*        XDWC(        1 :   NELM ): x values,
*        XDWC(   NELM+1 : 2*NELM ): data values,
*        XDWC( 2*NELM+1 : 3*NELM ): weight values,
*        XDWC( 3*NELM+1 : 4*NELM ): 2nd weight values.
*        If available, the 2nd weights are passed for calculating the
*        covariance. Otherwise the weights are passed.
*     CFLAGS( 6 ) = INTEGER (Given)
*        For each component I a value CFLAGS(I)=0 indicates that
*        CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J).
*     PFLAGS( 6 ) = INTEGER (Given)
*        For each component I a value PFLAGS(I)=0 indicates that
*        PEAK(I) holds a guess which is free to be fitted.
*        A positive value PFLAGS(I)=I indicates that PEAK(I) is fixed.
*        A positive value PFLAGS(I)=J<I indicates that PEAK(I) has to
*        keep a fixed ratio to PEAK(J).
*     SFLAGS( 6 ) = INTEGER (Given)
*        For each component I a value SFLAGS(I)=0 indicates that
*        SIGMA(I) holds a guess which is free to be fitted.
*        A positive value SFLAGS(I)=I indicates that SIGMA(I) is fixed.
*        A positive value SFLAGS(I)=J<I indicates that SIGMA(I) has to
*        keep a fixed ratio to SIGMA(J).
*     CENTRE( 6 ) = REAL (Given and Returned)
*        Centre position for each component.
*     PEAK( 6 ) = REAL (Given and Returned)
*        Peak height for each component.
*     SIGMA( 6 ) = REAL (Given and Returned)
*        Dispersion for each component.
*     CHISQR = REAL (Returned)
*        If the weights were 1/variance, then this is the final
*        chi-squared value for the fit. If all weights were 1, then this
*        is the sum of squared residuals. In the latter case the rms
*        would be SQRT(CHISQR/degrees_of_freedom).
*     COVAR( FITDIM, FITDIM ) = DOUBLE PRECISION (Returned)
*        If the weights were 1/variance, then this is the matrix of
*        covariances between fitted parameters. If all weights were 1,
*        then the covariance would be COVAR*CHISQR/degrees_of_freedom.
*     FITTED = LOGICAL (Returned)
*        True if fit was successful (including calculating the
*        covariance matrix).
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine returns a bad status if
*        -  it was entered with one,
*        -  covariance calculation returns bad status,
*        -  there are no free parameters or no degrees of freedom,
*        -  there are errors in the ties between parameters,
*        -  the fit routine returns with a bad status (including the case
*           where the fit did not converge before the iteration limit),
*        -  a free peak or width parameters is fitted as exactly zero.

*  References:
*     Meyerdierks, H., 1992, Covariance in resampling and model fitting,
*     Starlink, Spectroscopy Special Interest Group

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 May 1991 (hme):
*        Original version (DOFIT).
*     26 Jun 1991 (hme):
*        Calculate the covariance matrix for Gauss fit by calling SPFHSS.
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
*        to SPFHSS. Don't put fitted values into ADAM parameters any more.
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
*     25 Nov 1994 (hme):
*        Renamed from SPFDFT, common block in include file.
*     15 Mar 1995 (hme):
*        Replace E04DGF/E04DKF with PDA_UNCMND. (Iteration limit is
*        fixed at 150.)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  External References:
      EXTERNAL SPD_WFGD          ! Objective function

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'SPD_WFCM'         ! Line fitting common block
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Arguments Given:
      LOGICAL INFO
      LOGICAL COVRSX
      INTEGER NELM
      INTEGER NCOMP_ARG
      INTEGER FITPAR
      INTEGER FITDIM
      REAL CONT
      REAL XDWC( 4*NELM )
      INTEGER CFLAGS( MAXCMP )
      INTEGER PFLAGS( MAXCMP )
      INTEGER SFLAGS( MAXCMP )

*  Arguments Given and Returned:
      REAL CENTRE( MAXCMP )
      REAL PEAK(   MAXCMP )
      REAL SIGMA(  MAXCMP )

*  Arguments Returned:
      REAL CHISQR
      DOUBLE PRECISION COVAR( FITDIM, FITDIM )
      LOGICAL FITTED

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J, K            ! Loop indices
      INTEGER IFAIL1             ! NAG status
      DOUBLE PRECISION PAR1( 3*MAXCMP ) ! Permuted and scaled PAR0, fit
      DOUBLE PRECISION GUES( 3*MAXCMP ) ! dto., guess
      DOUBLE PRECISION FVAL      ! Value of objective function
      DOUBLE PRECISION WORK3( 3*MAXCMP*(3*MAXCMP+10) ) ! Work space
      DOUBLE PRECISION UNITY( 9*MAXCMP*MAXCMP )! Work space for covar.
      DOUBLE PRECISION HESSE( 9*MAXCMP*MAXCMP )! Work space for covar.
      LOGICAL ISREG              ! Pointer already registered with CNF
*.

*  Check.
      IF ( STATUS .NE. SAI__OK ) RETURN
      IF ( FITPAR .LT. 1 .OR.
     :     NCOMP_ARG  .LT. 1 .OR. NCOMP_ARG .GT. MAXCMP .OR.
     :     NELM   .LT. FITPAR ) THEN
         FITTED = .FALSE.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WFGA_E01', 'SPD_WFGA: Error: No fit ' //
     :      'performed. Probably too few or too many free ' //
     :      'parameters, or no degrees of freedom.', STATUS )
         GO TO 500
      END IF


*  Check flags and parameters and put them into the common block.
      DO 1 I = 1, NCOMP_ARG

*     Check that ties are only to earlier components.
         IF ( CFLAGS(I) .LT. 0 .OR. CFLAGS(I) .GT. I .OR.
     :        PFLAGS(I) .LT. 0 .OR. PFLAGS(I) .GT. I .OR.
     :        SFLAGS(I) .LT. 0 .OR. SFLAGS(I) .GT. I ) THEN
            FITTED = .FALSE.
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_WFGA_E02', 'SPD_WFGA: Error: ' //
     :         'Can tie only to earlier component.', STATUS )
            GO TO 500
         END IF

*     Check that centre ties are only to free centres.
         IF ( CFLAGS(I) .NE. 0 .AND. CFLAGS(I) .NE. I ) THEN
            IF ( CFLAGS(CFLAGS(I)) .NE. 0 ) THEN
               FITTED = .FALSE.
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_WFGA_E03', 'SPD_WFGA: Error: ' //
     :            'Can tie only to free parameter.', STATUS )
               GO TO 500
            END IF
         END IF

*     Check that peak ties are only to free peaks.
         IF ( PFLAGS(I) .NE. 0 .AND. PFLAGS(I) .NE. I ) THEN
            IF ( PFLAGS(PFLAGS(I)) .NE. 0 ) THEN
               FITTED = .FALSE.
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_WFGA_E03', 'SPD_WFGA: Error: ' //
     :            'Can tie only to free parameter.', STATUS )
               GO TO 500
            END IF
         END IF

*     Check that width ties are only to free widths.
         IF ( SFLAGS(I) .NE. 0 .AND. SFLAGS(I) .NE. I ) THEN
            IF ( SFLAGS(SFLAGS(I)) .NE. 0 ) THEN
               FITTED = .FALSE.
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_WFGA_E03', 'SPD_WFGA: Error: ' //
     :            'Can tie only to free parameter.', STATUS )
               GO TO 500
            END IF
         END IF

*     Packed array of all Gauss parameters.
         PAR0(I)          = DBLE(CENTRE(I))
         PAR0(I+MAXCMP)   = DBLE(PEAK(I))
         PAR0(I+2*MAXCMP) = DBLE(SIGMA(I))

*     Packed array of all fit flags.
         PARFLG(I)          = CFLAGS(I)
         PARFLG(I+MAXCMP)   = PFLAGS(I)
         PARFLG(I+2*MAXCMP) = SFLAGS(I)
 1    CONTINUE

*  Fill the COMMON block, except for PARNO.
*  FSCALE is set to 2 for the initial direct call to SPD_WFGB. This is
*  in anticipation that the final value of the objective function
*  will be 1/2 of its value for the guess.
*  All flags for unused components are set to 1 (fixed) in the COMMON
*  block. Also all unused components' parameters are set to 0 or 1,
*  whichever causes less harm.
      NCOMP  = NCOMP_ARG
      DCONT  = DBLE(CONT)
      FSCALE = 2D0
      DO 2 I = NCOMP_ARG+1, MAXCMP
         PAR0(I)          = 0D0
         PAR0(I+MAXCMP)   = 0D0
         PAR0(I+2*MAXCMP) = 1D0
         PARFLG(I)          = I
         PARFLG(I+MAXCMP)   = I
         PARFLG(I+2*MAXCMP) = I
 2    CONTINUE

*  PARNO: Permutation of parameters,
*  such that free parameters come to the beginning of PAR1.
*  (PAR1 itself need not worry us here, since its guess value is
*  always == 0. But this is done in passing.)
      J = 0
      K = 3 * MAXCMP + 1
      DO 3 I = 1, 3*MAXCMP
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
         CALL ERR_REP( 'SPD_WFGA_E04', 'SPD_WFGA: Error: No fit ' //
     :      'performed. Flags inconsistent with number of free' //
     :      'parameters.', STATUS )
         GO TO 500
      END IF

*  Derive FSCALE by direct call to SPD_WFGB with initial FSCALE = 2.
*     IFAIL1 = 0
*     IUSER(1) = NELM
*     CALL SPD_WFGB( IFAIL1, FITPAR, PAR1, FVAL, GRADF, 1, IUSER, XDWC )
*     FSCALE = FVAL

*  This gives the initial chi squared.
*     CHISQR = SNGL( FVAL * FSCALE )

*  Set non-default optional parameters for E04DGF.
*  Itns is normally max(50,5n).
*     CALL E04DKF( 'Nolist' )
*     CALL E04DKF( 'Print Level = 0' )
*     CALL E04DKF( 'Verify No' )
*     CALL E04DKF( 'Itns = 200 ' )

*  Call E04DGF.
*     IFAIL1 = 1
*     CALL E04DGF( FITPAR, SPD_WFGB, ITNO, FVAL, GRADF, PAR1, IWORK,
*    :   WORK3, IUSER, XDWC, IFAIL1 )

*  Make the spectral data available to objective function. Register
*  with CNF for 64bit systems.
      NDATA = NELM
#if HAVE_INTRINSIC_LOC
      DATAP = CNF_PREG( LOC(XDWC), ISREG )
#elif HAVE_PERCENTLOC
      DATAP = CNF_PREG( %LOC(XDWC), ISREG )
#else
      Do not have LOC or %LOC
#endif

*  Set the guess to zero, as has been done above for the fit PAR1.
      DO 1001 I = 1, 3*MAXCMP
         GUES(I) = 0D0
 1001 CONTINUE

*  Derive FSCALE by direct call to SPD_WFGD with initial FSCALE = 2.
*  This also gives the initial chi squared.
      CALL SPD_WFGD( FITPAR, GUES, FVAL )
      CHISQR = SNGL( FVAL * FSCALE )
      FSCALE = FVAL

*  Do the fit.
      CALL PDA_UNCMND( FITPAR, GUES, SPD_WFGD, PAR1, FVAL, IFAIL1,
     :   WORK3, 3*MAXCMP*(3*MAXCMP+10) )

*  Post-fit processing.
      IF ( IFAIL1 .GE. 0 .AND. IFAIL1 .LE. 3 ) THEN
         FITTED = .TRUE.

*     Last objective function value gives final chi squared.
         CHISQR = SNGL( FVAL * FSCALE )

*     Retrieve free parameters.
         DO 4 J = 1, NCOMP_ARG
            IF ( PARFLG(J) .EQ. 0 )
     :         CENTRE(J) = SNGL( PAR0(J)
     :                   + 2D0 * PAR0(J+2*MAXCMP) * PAR1(PARNO(J)) )
            IF ( PARFLG(J+MAXCMP) .EQ. 0 )
     :         PEAK(J)   = SNGL( PAR0(J+MAXCMP)
     :                   * ( 1D0 + PAR1(PARNO(J+MAXCMP)) ) )
            IF ( PARFLG(J+2*MAXCMP) .EQ. 0 )
     :         SIGMA(J)  = ABS( SNGL( PAR0(J+2*MAXCMP)
     :                   * ( 1D0 + PAR1(PARNO(J+2*MAXCMP)) ) ) )
 4       CONTINUE

*     Retrieve tied parameters.
         DO 5 J = 1, NCOMP_ARG
            IF ( PARFLG(J) .NE. J .AND. PARFLG(J) .NE. 0 )
     :         CENTRE(J) = CENTRE(PARFLG(J))
     :                   + SNGL( PAR0(J) - PAR0(PARFLG(J)) )
            IF ( PARFLG(J+MAXCMP) .NE. J .AND.
     :           PARFLG(J+MAXCMP) .NE. 0 )
     :         PEAK(J)   = PEAK(PARFLG(J+MAXCMP))
     :                   * SNGL( PAR0(J+MAXCMP)
     :                   / PAR0(PARFLG(J+MAXCMP)+MAXCMP) )
            IF ( PARFLG(J+2*MAXCMP) .NE. J .AND.
     :           PARFLG(J+2*MAXCMP) .NE. 0 )
     :         SIGMA(J)  = SIGMA(PARFLG(J+2*MAXCMP))
     :                   * SNGL( PAR0(J+2*MAXCMP)
     :                   / PAR0(PARFLG(J+2*MAXCMP)+2*MAXCMP) )
 5       CONTINUE

*     If any of the peak or width parameters turns out to be exactly
*     zero, we have an error condition. For one it is a non-existing
*     component. More severely, this will result in problems either with
*     the Hesse matrix or with parameter variances, i.e. there will be
*     divisions by zero later on.
         DO 6 J = 1, NCOMP_ARG
            IF ( PEAK(J) .EQ. 0. .OR. SIGMA(J) .EQ. 0. ) THEN
               FITTED = .FALSE.
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_WFGA_E05', 'SPD_WFGA: Error: Zero ' //
     :            'peak or width after fit.', STATUS )
               GO TO 500
            END IF
 6       CONTINUE

*     Covariance matrix. Use 2nd weights if possible, weights otherwise.
         IF ( COVRSX ) THEN
            CALL SPD_WFGC( NELM, FITPAR, XDWC(1), XDWC(3*NELM+1),
     :         CENTRE, PEAK, SIGMA,
     :         UNITY, HESSE, COVAR, FITTED, STATUS )
         ELSE
            CALL SPD_WFGC( NELM, FITPAR, XDWC(1), XDWC(2*NELM+1),
     :         CENTRE, PEAK, SIGMA,
     :         UNITY, HESSE, COVAR, FITTED, STATUS )
         END IF
      ELSE

*     Fit failed.
         FITTED = .FALSE.
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'INT', IFAIL1 )
         CALL ERR_REP( 'SPD_WFGA_E06', 'SPD_WFGA: Error: PDA_UNCMND ' //
     :      'returned with error status ^INT.', STATUS )

*     Explain the most common failure.
         IF ( IFAIL1 .EQ. 4 ) CALL ERR_REP( 'SPD_WFGA_E07',
     :         'No convergence before iteration limit.', STATUS )
         GO TO 500
      END IF

*  Return.
 500  CONTINUE
      END
