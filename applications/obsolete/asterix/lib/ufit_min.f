*+  UFIT_MIN - Iteratively refines parameters to minimise statistic
      SUBROUTINE UFIT_MIN( OPCHAN, NITMAX, FSTATC, COMPARATOR, NPAR,
     :                     LB, UB, FROZEN, SSCALE, INITIALISE, MINSLO,
     :                     PARAM, DPAR, PEGGED, STAT, NIT, FINISHED,
     :                     FITERR, STATUS )
*
*    Description :
*
*     Iteratively refines the model parameters PARAM, subject to the lower
*     and upper bounds LB & UB, until the fit statistic is minimised (or
*     NITMAX iterations are up).
*     Progress is reported to logical unit OPCHAN, if set >0.
*     INITIALISE should be set true on the very first call of the routine (but
*     false on subsequent calls) to ensure correct initialisation.
*     The fit statistic is selected using the FSTATC which must be a valid
*     abbreviation of either CHISQ or LOGL.
*
*     The minimum slope (of the scaled statistic with respect to the scaled
*     parameters) required to force continued iteration can be specified as
*     part of the initialisation. If MINSLO<=0 is entered then a default
*     value is taken.
*     FITERR returns a non-zero error code if an error arises in the fitting:
*
*          FITERR = 1	Upper bound <= lower bound for a parameter
*          FITERR = 2	No unfrozen parameters
*          FITERR = 3   No unpegged parameters
*          FITERR = 4	Singular matrix of 2nd derivs (e.g. arises if two
*                       parameters have identical (or scaled) effects on data
*          FITERR = 5	Matrix of 2nd derivs is ill-conditioned (i.e. nearly
*                       singular), rounding errors prevent reliable solution
*          FITERR = 6   Maximum value of lambda exceeded. Arises if
*
*     Note that NIT is only zeroed when routine is initialised, otherwise it
*     will just be incremented from its value on entry.
*
*     SSCALE is the statistic scale factor : this should be the number of
*     degrees of freedom less the number of unfrozen parameters when FSTATC is
*     'CHISQ', but should be the sum over all the observed data when
*     equal to 'LOGL'.
*
*    Method :
*
*     Combined gradient/quadratic-fitting minimisation routine based on
*     Bevington CURFIT program. Parameter bounds are incorporated by 'pegging'
*     parameters on bounds and excluding them from the fitting process until
*     the chi-squared gradient takes them back into the allowed region. 'FROZEN'
*     parameters are never allowed to vary.
*     N.B. Following Bevington, computing time can be saved by using an
*     approximation to the statistic second derivs within UFIT_DERIVS, rather
*     than a rigorous evaluation.
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
      Richard Beard (Birmingham)
*
*    History :
*
*     21 Sep 93 : Adapted from old UFIT_CHIMIN and current FIT_MIN. Added
*                 FSTATC and COMPARATOR arguments (DJA)
*     23 Feb 93 : Asterix and Starlink include files removed. (DJA)
*      6 Jun 97 : Convert to PDA (RB)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIT_PAR'
*
*    Import :
*
      INTEGER 		  OPCHAN		! Output channel for diagnostic
						! messages ( <1 for no messages)
      INTEGER 		  NITMAX		! Return when NIT reaches NITMAX
      CHARACTER*(*)       FSTATC                ! Statistic to use
      EXTERNAL 		  COMPARATOR            ! Data vs. Model comparator
                                                ! routine
      INTEGER 		  NPAR			! No of parameters
      REAL 		  LB(*)			! Parameter lower bounds
      REAL 		  UB(*)			! Parameter upper bounds
      LOGICAL 		  FROZEN(*)		! Frozen parameter flag
      INTEGER 		  SSCALE		! Statistic scale factor
*
*    Import-Export :
*
      LOGICAL 		  INITIALISE		! Should be set true on first
						! call only - always returned F
      REAL 		  MINSLO		! Min scaled slope in statistic
                                                ! forcing continuation
      REAL 		  PARAM(*)		! Model parameters
      REAL 		  DPAR(*)		! Param increments for differencing
      LOGICAL 		  PEGGED(*)		! Parameter pegged on bound
      DOUBLE PRECISION	  STAT			! Fit statistic
      INTEGER 		  NIT			! Iteration number
*
*    Export :
*
      LOGICAL 		  FINISHED		! Minimum found
      INTEGER 		  FITERR		! Fitting error encountered
*
*    Status :
*
      INTEGER 		  STATUS
*
*    Local constants :
*
      REAL 		  DELFAC        	! Parameter increment scale
	PARAMETER         (DELFAC=5.0E-4)	! factor = increment/param range
      REAL 		  CHANGELIM		! Min fractional change in stat
	PARAMETER         (CHANGELIM=0.005)	! which will force continuation
      REAL 		  DEFMINSLO     	! Default for MINSLO
        PARAMETER         (DEFMINSLO=20.)
      REAL                MAXLOGLAM             ! Maximum value of log(lambda)
        PARAMETER         ( MAXLOGLAM = 8.5 )
      REAL 		  TDELTA		! Fractional change in stat and
	PARAMETER         (TDELTA=1.0E-7)	! slope which allows termination
*
*    Functions :
*
      INTEGER		  CHR_LEN
      LOGICAL             STR_ABBREV
*
*    Local variables :
*
      CHARACTER*16        LFSTATC		! Local copy of FSTATC
      CHARACTER*16        LFSTATCS		! Named of scaled statistic

      DOUBLE PRECISION    DERIV1(NPAMAX)	! Array of derivs of statistic
                                                ! w.r.t. params
      DOUBLE PRECISION    DERIV2(NPAMAX,NPAMAX)	! Matrix of 2nd derivs (approx.)
      DOUBLE PRECISION    DSLOPE		! Double prec accumulator for slope**2
      DOUBLE PRECISION    OLDSLOPE              ! Slope on last iteration
      DOUBLE PRECISION	  PARSTEP(NPAMAX)	! Step in parameter space (scaled)
      DOUBLE PRECISION    SCDERIV1(NPAMAX)	! Minus statistic derivs w.r.t.
                                                ! scaled free parameters
      DOUBLE PRECISION    SCDERIV2(NPAMAX,NPAMAX) ! Scaled statistic 2nd derivs
      DOUBLE PRECISION	  SLOPE			! Norm of the reduced chisq/
                                                ! scaled C-statistic gradient in
						! the scaled parameter space

      REAL 		  LAMBDA		! Marquardt's parameter
      SAVE 		  LAMBDA
      REAL 		  NEWPAR(NPAMAX)	! New parameter values
      DOUBLE PRECISION	  NEWSTAT		! New statistic
      DOUBLE PRECISION	  OLDNEWSTAT		! New statistic on last iteration
      REAL 		  PSCALE(NPAMAX)	! Parameter scale factors
      SAVE 		  PSCALE

c     INTEGER             FSTAT                 ! Statistic to use
      INTEGER 		  NUNFROZEN		! No of unfrozen parameters
      INTEGER 		  NPFREE		! No of free parameters
      INTEGER 		  IPFREE(NPAMAX)	! Free parameter indices
      INTEGER 		  J,K,L,M		! Parameter indices
      INTEGER             LNITER                ! Local number of iterations
      INTEGER 		  LSSCALE		! SSCALE proofed against zero value
      INTEGER             RITER                 ! # repeats of gradient calc'n

      LOGICAL 		  NEWPEG(NPAMAX)	! New parameters pegged
*
*    PDA related declarations :
*
      DOUBLE PRECISION	  WKS1(NPAMAX),WKS2(NPAMAX)  ! Work space
      INTEGER		  IWKS(NPAMAX)
      INTEGER 		  FAIL			! PDA error code
      INTEGER		  ACC
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Choose statistic
      IF ( STR_ABBREV( FSTATC, 'CHISQUARED' ) ) THEN
        LFSTATC = 'Chi-sq'
        LFSTATCS = 'Chi-red'
      ELSE IF  ( STR_ABBREV( FSTATC, 'LOGLIKELIHOOD' ) ) THEN
        LFSTATC = 'Cash statistic'
        LFSTATCS = 'scaled Cash'
      ELSE
        CALL MSG_SETC( 'S', FSTATC )
        CALL MSG_OUT( ' ', 'Unrecognised fit statistic ^S, '/
     :                 /'defaulting to chi-squared', STATUS )
        LFSTATC = 'Chi-sq'
        LFSTATCS = 'Chi-red'
      END IF

*    Initialisation
      IF ( INITIALISE ) THEN
	FINISHED = .FALSE.
	NIT = 0
	FITERR = 0
	LAMBDA = 0.001
	NUNFROZEN = 0

	IF ( MINSLO .LE. 0.0 ) MINSLO = DEFMINSLO
	IF ( SSCALE .GT. 0 ) THEN
	  LSSCALE = SSCALE
	ELSE IF ( SSCALE .EQ. 0 ) THEN
	  LSSCALE = 1			! Set to 1 to avoid zero divide
	ELSE
	  STATUS = SAI__ERROR
          IF ( OPCHAN .GT. 0 ) THEN
            WRITE( OPCHAN, '(1X,A)' ) 'More free parameters than'/
     :                                /' data values !'
          END IF
	  GOTO 99
	END IF

*      Set up parameter scale factors (s.t.full range = PARRANGE) and increments
	DO J = 1, NPAR

	  IF ( .NOT. FROZEN(J) ) THEN
	    NUNFROZEN = NUNFROZEN + 1
	    PSCALE(J) = (UB(J)-LB(J))/PARRANGE
	    DPAR(J) = DELFAC*(UB(J)-LB(J))

*          Check for bad bounds
	    IF ( LB(J) .GE. UB(J) ) THEN
	      FITERR = 1
	      GOTO 99
	    END IF

*          Peg parameters outside allowed limits
	    IF ( PARAM(J) .LT. LB(J) ) THEN
	      PARAM(J) = LB(J)
	      PEGGED(J) = .TRUE.
	    ELSE IF ( PARAM(J) .GT. UB(J) ) THEN
	      PARAM(J) = UB(J)
	      PEGGED(J) = .TRUE.
	    ELSE
	      PEGGED(J) = .FALSE.
	    END IF
	    IF ( PEGGED(J) .AND. (OPCHAN.GT.0) ) THEN
	      WRITE(OPCHAN,*) '-- Parameter',J,' pegged from start at',
     :          PARAM(J)
	    END IF
	  ELSE
	    DPAR(J) = 0.0
	  END IF

	END DO

*      Get initial statistic and report
        CALL COMPARATOR( PARAM, STAT, STATUS )
	IF ( OPCHAN .GT. 0 ) THEN
 200	  FORMAT(/' Initial ',A,' & ',A,' : ',1PG12.5,2X,1PG11.5/)
	  WRITE(OPCHAN,200) LFSTATC(:CHR_LEN(LFSTATC)),
     :                      LFSTATCS(:CHR_LEN(LFSTATCS)),
     :                      STAT, STAT/LSSCALE
	END IF

*      Check that there are some unfrozen parameters
	IF ( NUNFROZEN .LE. 0 ) THEN
	  FITERR = 2
	  GOTO 99
	END IF

      END IF

*    Main iteration loop - improved parameter set found at each iteration
      LNITER = 0
      DO WHILE ( (NIT.LT.NITMAX) .AND. .NOT. FINISHED )

*      Increment iteration counters
	NIT = NIT + 1
	LNITER = LNITER + 1

*      Get first and second derivatives of statistic w.r.t. parameters
	CALL UFIT_DERIVS( NPAR, PARAM, COMPARATOR, LB, UB,
     :                    DPAR, FROZEN, DERIV1, DERIV2, STATUS )

*      Test 1st derivs to see if any pegged parameters want to come off bounds
	NPFREE = 0
	DSLOPE = 0.0D0
	DO J = 1, NPAR
	  IF ( PEGGED(J) .AND. .NOT. FROZEN(J) ) THEN
	    PEGGED(J) = (((PARAM(J).EQ.LB(J)).AND.(DERIV1(J).GT.0.)).OR.
     :                   ((PARAM(J).EQ.UB(J)).AND.(DERIV1(J).LT.0.)))
	    IF ( (OPCHAN.GT.0) .AND. .NOT. PEGGED(J) )THEN
	      WRITE(OPCHAN,*) '-- Unpegging parameter',J
	    END IF
	  END IF

*        Set up array of free (i.e. not frozen or pegged) parameter indices
	  IF ( (.NOT.FROZEN(J)) .AND. .NOT. PEGGED(J) )THEN
	    NPFREE=NPFREE+1
	    IPFREE(NPFREE)=J

*          Set up arrays of 1st and second derivs of REDUCED chi-squared w.r.t.
*          the SCALED parameters, for free parameters only
	    SCDERIV1(NPFREE) = -DERIV1(J)*PSCALE(J)/LSSCALE ! Minus for F04ATF
	    DO K=1,NPFREE-1	! N.B. Diagonal element not evaluated here
	      SCDERIV2(K,NPFREE)=DERIV2(IPFREE(K),J)*PSCALE(IPFREE(K))*
     :           PSCALE(J)/LSSCALE
	      SCDERIV2(NPFREE,K)=SCDERIV2(K,NPFREE)
	    END DO

*          Scaled statistic gradient in scaled space
	    DSLOPE = DSLOPE + SCDERIV1(NPFREE)**2

	  END IF
	END DO
	SLOPE = SQRT(DSLOPE)

*      Give up if all parameters are pegged
	IF ( NPFREE .LE. 0 ) THEN
	  FITERR = 3
	  GOTO 99
	END IF

*      Diagonal of matrix of second derivs is scaled up by (1+lambda)
        OLDNEWSTAT = 0.0
        RITER = 0
 1000	CONTINUE		! Loop start when retrying with larger lambda
	DO K = 1, NPFREE
	  J = IPFREE(K)
	  SCDERIV2(K,K) = DERIV2(J,J)*DBLE(PSCALE(J)**2)*
     :                                       (1.D0+LAMBDA)/LSSCALE
	END DO

*      Call PDA routine to solve for parameter offsets
	FAIL=0
        DO L = 1, NPFREE
          PARSTEP(K) = SCDERIV1(K)
        END DO
        CALL PDA_DGEFS( SCDERIV2, NPAMAX, NPFREE, PARSTEP, 1, ACC,
     :                  WKS1, WKS2, FAIL )
        IF ( ACC .LT. 0 ) THEN
          IF ( ACC .EQ. -3 ) THEN
            FITERR = 4
          ELSE IF ( ACC .EQ. -10 ) THEN
            FITERR = 5
          END IF
          GOTO 99
        END IF

        RITER = RITER + 1

*      Check for error in NAG routine
	IF ( FAIL .NE. 0 ) THEN
	  FITERR = FAIL + 3
*...........( FITERR=4 for singular DERIV2 matrix )
*...........(       =5 for ill-conditioned matrix )
	  GOTO 99
	END IF

*      Evaluate new (unscaled) parameter values and check against bounds
	DO J = 1, NPAR
	  NEWPAR(J) = PARAM(J)
	  NEWPEG(J) = .FALSE.
	END DO
	DO K = 1, NPFREE
	  J = IPFREE(K)
	  NEWPAR(J) = PARAM(J)+PARSTEP(K)*PSCALE(J)
	  IF ( NEWPAR(J) .LE. LB(J) ) THEN
	    NEWPEG(J) = .TRUE.
	    NEWPAR(J) = LB(J)
	  ENDIF
	  IF ( NEWPAR(J) .GE. UB(J) ) THEN
	    NEWPEG(J) = .TRUE.
	    NEWPAR(J) = UB(J)
	  END IF
	END DO

*      Evaluate new statistic and try again with increased lambda if it hasn't
*      improved
        OLDNEWSTAT = NEWSTAT
        CALL COMPARATOR( NEWPAR, NEWSTAT, STATUS )
	IF ( NEWSTAT .GT. STAT ) THEN

*        Increase lambda unless the threshold has been passed. In the
*        latter case, raise a fit error. The MAX function traps LAMBDA
*        going to zero due to large number of iterations
          IF ( LOG10(MAX(LAMBDA,1.0E-30)) .LT. MAXLOGLAM ) THEN
            IF ( (OLDNEWSTAT.EQ.0.0) .OR. (NEWSTAT.GT.OLDNEWSTAT)) THEN
	      LAMBDA = 10*LAMBDA
            ELSE
	      LAMBDA = 3*LAMBDA
            END IF
          ELSE IF ( (RITER.GT.20) .OR. (NEWSTAT .GT. OLDNEWSTAT) ) THEN
            FITERR = 6
            GOTO 99
          END IF
	  GOTO 1000
	ELSE

*        Successful iteration - update parameters
	  DO J = 1, NPAR
	    PARAM(J) = NEWPAR(J)
	    IF ( NEWPEG(J) ) THEN
	      IF ( (OPCHAN.GT.0) .AND. .NOT. PEGGED(J) ) THEN
	        WRITE(OPCHAN,*) '-- Pegging parameter',J,' at',PARAM(J)
	      END IF
	      PEGGED(J) = NEWPEG(J)
	    END IF
	  END DO

*        Output fitting information
	  IF ( OPCHAN .GT. 0 ) THEN
 2000	    FORMAT( ' it,',A,',lambda,slope : ',I3,7X,1PG11.5,4X,I3,
     :              4X,1PG11.4)
	    WRITE(OPCHAN,2000) NIT, LFSTATCS(:CHR_LEN(LFSTATCS)),
     :                         NEWSTAT/LSSCALE, NINT(LOG10(LAMBDA)),
     :                         SLOPE
	  END IF

*        Reduce lambda for next time
	  LAMBDA = MAX(LAMBDA/10.0,1.0E-10)

	END IF

*      Check to see if minimum has been found
	FINISHED = ((STAT-NEWSTAT).LT.ABS(CHANGELIM*STAT)).AND.
     :                                        (SLOPE.LT.MINSLO)

*      Allow termination after 1st iteration if STAT and SLOPE unchanged.
*      Be careful to allow for zero value of statistic
        IF ( (LNITER.GT.1) .AND. .NOT. FINISHED ) THEN
          FINISHED = (ABS(STAT-NEWSTAT).LE.ABS(TDELTA*STAT)) .AND.
     :               (ABS(SLOPE-OLDSLOPE).LE.ABS(TDELTA*SLOPE))
        END IF

	STAT = NEWSTAT
        OLDSLOPE = SLOPE

      END DO
      INITIALISE = .FALSE.

*    Exit
 99   IF ( (STATUS .NE. SAI__OK) .AND. (OPCHAN.GT.0) ) THEN
        WRITE( OPCHAN, '(1X,A)' ) 'Error from UFIT_MIN'
      END IF

      END
