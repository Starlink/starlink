      SUBROUTINE FIT_MIN1( NDS, IMOD, MCTRL, OPCHAN, PRGRES, NPAR,
     :                     LB, UB, FROZEN, SSCALE, LNDFAC, FSTAT,
     :                     PREDICTOR, PARAM, DPAR, PEGGED, STAT,
     :                     FINISHED, FITERR, STATUS )
*+
*  Name:
*     FIT_MIN1

*  Purpose:
*     Iteratively refines parameters to minimise statistic using CURFIT style algorithm

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FIT_MIN1( NDS, OBDAT, INSTR, MODEL, MCTRL, OPCHAN, PRGRES, NPAR,
*                    LB, UB, FROZEN, SSCALE, FSTAT, PREDICTOR, PREDDAT,
*                    PARAM, DPAR, PEGGED, STAT, FINISHED, FITERR, STATUS )

*  Description:
*     Iteratively refines the model parameters PARAM, subject to the lower
*     and upper bounds LB & UB, until the fit statistic is minimised (or
*     NITMAX iterations are up).
*     Progress is reported to logical unit OPCHAN, if set >0.
*     The fit statistic is selected using the FSTAT argument and is accessed
*     using the symbolic variables FIT__CHISQ and FIT__LOGL.
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
*     degrees of freedom less the number of unfrozen parameters when FSTAT is
*     FIT__CHISQ, but should be the sum over all the observed data when
*     equal to FIT__LOGL.

*  Arguments:
*     NDS = INTEGER (given)
*        Number of datasets to fit
*     OBDAT[NDS] = /DATASET/ (given)
*        Datasets to fit
*     INSTR[NDS] = /INSTR_RESP/ (given)
*        Responses
*     MODEL = /MODEL/ (given)
*        Model specification
*     MCTRL = INTEGER (given)
*        ADI identifier of minimisation control object
*     OPCHAN = INTEGER (given)
*        Output channel, or none if < 1
*     PRGRES = LOGICAL (given)
*        Issue progress reports and update model file?
*     NPAR = INTEGER (given)
*        Number of model parameters
*     LB[NPAR] = REAL (given)
*        Lower parameters bounds
*     UB[NPAR] = REAL (given)
*        Upper parameters bounds
*     FROZEN[NPAR] = LOGICAL (given)
*        Parameters frozen?
*     SSCALE = INTEGER (given)
*        Statistic scale factor
*     FSTAT = INTEGER (given)
*        Statistic to use
*     PERDICTOR = EXTERNAL (given)
*        Procedure to generate a predicted dataset
*     PREDDAT[NDS] = /PREDICTION/ (given and returned)
*        The new predicted data
*     PARAM[NPAR] = REAL (given and returned)
*        Model parameters
*     DPAR[NPAR] = REAL (given and returned)
*        Param increments for differencing
*     PEGGED[NPAR] = LOGICAL (given and returned)
*        Parameter pegged on bound?
*     STAT = DOUBLE PRECISION (given and returned)
*        Best fit statistic
*     FINISHED = LOGICAL (returned)
*        Reached a minimum?
*     FITERR = INTEGER (returned)
*        Fit error code
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     Combined gradient/quadratic-fitting minimisation routine based on
*     Bevington CURFIT program. Parameter bounds are incorporated by 'pegging'
*     parameters on bounds and excluding them from the fitting process until
*     the chi-squared gradient takes them back into the allowed region. 'FROZEN'
*     parameters are never allowed to vary.
*     N.B. Following Bevington, computing time can be saved by using an
*     approximation to the statistic second derivs within FIT_DERIVS, rather
*     than a rigorous evaluation.

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     Still uses Fortran structures. Must be removed for Linux

*  References:
*     FIT Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fit.html

*  Keywords:
*     package:fit, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      7 Apr 1987 (TJP):
*        Original version
*     29 Apr 1987 (TJP):
*        Calculate parameter increments, new FIT_DERIVS interface
*     15 May 1987 (TJP):
*        Return DPAR to calling program
*     11 Jun 1987 (TJP):
*        CHANGELIM termination criterion changed
*     24 Jul 1987 (TJP):
*        MINSLO entered as a program argument
*     29 Apr 1988 (TJP):
*        Use of globals eliminated by passing structures
*      1 Jul 1988 (TJP):
*        Amended to handle NDOF=0 case
*     24 May 1989 (TJP):
*        Amended to calculate chisq even if all params frozen
*     25 Mar 1992 (RJV):
*        Renamed, FIT_PREDDAT made external PREDICTOR
*     27 May 1992 (DJA):
*        Allow maximum likelihood fitting using Cash statistic. Correct error
*        handling and allow -ve statistics. Now uses D.P.
*     15 Jun 1992 (DJA):
*        Added FSTAT argument, changed NDOF to SSCALE
*     18 Aug 1992 (DJA):
*        Statistic now double precision
*     27 Nov 1992 (DJA):
*        Allow termination if STAT and SLOPE unchanged by amount
*        TDELTA after 2nd or subsequent iterations
*     16 Feb 1994 (DJA):
*        Adjust termination condition to correctly handle TDELTA
*        option for -ve statistics
*     19 May 1994 (DJA):
*        Added parameter constraint handling
*     16 Apr 1996 (DJA):
*        Changed from NAG to PDA routine. Renamed FIT_MIN1. Fit control
*        data moved to MCTRL
*     10 May 1996 (DJA):
*        Changed back from PDA to NAG. PDA routine not robust enough
*      6 Jun 1997 (RB):
*        Re-convert to PDA 'cause we have to!
*      4 Jul 1997 (RB)
*        Ensure that a sensible status/flag is returned
*     27 Feb 1998 (RB)
*        Comment out the error dumps
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIT_PAR'
      INCLUDE 'FIT_STRUC'

*  Arguments Given:
      INTEGER                   NDS, OPCHAN, MCTRL, NPAR, SSCALE, FSTAT
c     RECORD /DATASET/          OBDAT(NDS)
c     RECORD /INSTR_RESP/       INSTR(NDS)
c     RECORD /MODEL_SPEC/       MODEL
      INTEGER			IMOD
      REAL                      LB(*), UB(*)
      LOGICAL                   PRGRES, FROZEN(*)
      EXTERNAL                  PREDICTOR
      DOUBLE PRECISION          LNDFAC

*  Arguments Given and Returned:
      LOGICAL                   PEGGED(*)
c     RECORD /PREDICTION/       PREDDAT(NDS)
      REAL                      PARAM(*), DPAR(*)
      DOUBLE PRECISION          STAT

*  Arguments Returned:
      LOGICAL                   FINISHED
      INTEGER                   FITERR

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:

*  Local Constants:
      REAL 		  DELFAC        	! Parameter increment scale
	PARAMETER         (DELFAC=5.0E-4)	! factor = increment/param range
      REAL 		  CHANGELIM		! Min fractional change in stat
	PARAMETER         (CHANGELIM=0.005)	! which will force continuation
      REAL 		  DEFMINSLO     	! Default for MINSLO
        PARAMETER         (DEFMINSLO=20.)
      REAL                MAXLOGLAM             ! Maximum value of log(lambda)
        PARAMETER         ( MAXLOGLAM = 8.5 )
      REAL 		  TDELTA		! Fractional change in stat and
	PARAMETER         (TDELTA=5.0E-6)	! slope which allows termination

*  Local Variables:
      DOUBLE PRECISION    	DERIV1(NPAMAX)	! Array of derivs of statistic
                                                ! w.r.t. params
      DOUBLE PRECISION    DERIV2(NPAMAX,NPAMAX)	! Matrix of 2nd derivs (approx.)
      DOUBLE PRECISION    DSLOPE		! Double prec accumulator for slope**2
      DOUBLE PRECISION	  NEWSTAT		! New statistic
      DOUBLE PRECISION	  OLDNEWSTAT		! New statistic on last iteration
      DOUBLE PRECISION    OLDSLOPE              ! Slope on last iteration
      DOUBLE PRECISION	  PARSTEP(NPAMAX)	! Step in parameter space (scaled)
      DOUBLE PRECISION    SCDERIV1(NPAMAX)	! Minus statistic derivs w.r.t.
                                                ! scaled free parameters
      DOUBLE PRECISION    SCDERIV2(NPAMAX,NPAMAX) ! Scaled statistic 2nd derivs

#ifndef NAG
      DOUBLE PRECISION    SCDERIV3(NPAMAX,NPAMAX)
#endif

      DOUBLE PRECISION	  SLOPE			! Norm of the reduced chisq/
                                                ! scaled C-statistic gradient in
						! the scaled parameter space

      REAL 		  	LAMBDA			! Marquardt's parameter
      REAL			MINSLO			! Slope
      REAL 		  	NEWPAR(NPAMAX)		! New parameter values
      REAL 		  	PSCALE(NPAMAX)		! Parameter scale factors
        SAVE 		  	PSCALE

      INTEGER 		  	IPFREE(NPAMAX)		! Free parameter indices
      INTEGER			ISTAT			! i/o status code
      INTEGER 		  	J,K			! Parameter indices
      INTEGER             	LNITER                	! Local number of iterations
      INTEGER 		  	LSSCALE			! SSCALE proofed against zero value
      INTEGER			NIT			! Local copy of Niter
      INTEGER			NITMAX			! Local copy of MaxIt
      INTEGER 		  	NPFREE			! # free parameters
      INTEGER			NSINCE			! Iterations since report
      INTEGER 		  	NUNFROZEN		! # unfrozen parameters
      INTEGER			NUP			! Iterations between report
      INTEGER             	RITER                 	! # repeats of gradient calc'n

      LOGICAL			LFROZEN(NPAMAX)		! Local frozen array
        SAVE                    LFROZEN
      LOGICAL 		  	NEWPEG(NPAMAX)		! New parameters pegged
*
*    Nag or PDA related declarations :
*
#ifdef NAG
      DOUBLE PRECISION          LU(NPAMAX,NPAMAX)       ! Work space and LU dec$
      DOUBLE PRECISION          WKS1(NPAMAX),WKS2(NPAMAX)  ! Work space
      INTEGER                   FAIL                    ! NAG error code
#else
      INTEGER                   ACC                     ! Accuracy code
      DOUBLE PRECISION          WKS1(NPAMAX)            ! Work space
      INTEGER                   WKS2(NPAMAX)            ! Work space
#endif
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract control data
      CALL ADI_CGET0I( MCTRL, 'MaxIt', NITMAX, STATUS )
      CALL ADI_CGET0I( MCTRL, 'Niter', NIT, STATUS )
      CALL ADI_CGET0I( MCTRL, 'UpdateInterval', NUP, STATUS )
      CALL ADI_CGET0R( MCTRL, 'MinSlope', MINSLO, STATUS )
      CALL ADI_CGET0R( MCTRL, 'Lambda', LAMBDA, STATUS )

*  Initialisation required?
      IF ( NIT .EQ. 0 ) THEN

	FINISHED = .FALSE.
	FITERR = 0
	NUNFROZEN = 0

*    Extend FROZEN array to handle constrained parameters
        CALL FIT1_LOCFRO( IMOD, NPAR, FROZEN, LFROZEN, STATUS )

*    Set up slope stopping criterion
	IF ( MINSLO .LE. 0.0 ) MINSLO = DEFMINSLO

*    Check statistic scaling
	IF ( SSCALE .GT. 0 ) THEN
	  LSSCALE = SSCALE
	ELSE IF ( SSCALE .EQ. 0 ) THEN
	  LSSCALE = 1			! Set to 1 to avoid zero divide
	ELSE
	  STATUS = SAI__ERROR
	  CALL ERR_REP( ' ','More free parameters than data values!',
     :                                                       STATUS )
	  GOTO 99
	END IF

*    Set up parameter scale factors (s.t.full range = PARRANGE) and increments
	DO J = 1, NPAR

	  IF ( .NOT. LFROZEN(J) ) THEN
	    NUNFROZEN = NUNFROZEN + 1
	    PSCALE(J) = (UB(J)-LB(J))/PARRANGE
	    DPAR(J) = DELFAC*(UB(J)-LB(J))

*        Check for bad bounds
	    IF ( LB(J) .GE. UB(J) ) THEN
	      FITERR = 1
	      GOTO 99
	    END IF

*        Peg parameters outside allowed limits
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
	      WRITE(OPCHAN,*,IOSTAT=ISTAT) '-- Parameter',J,
     :           ' pegged from start at', PARAM(J)
	    END IF
	  ELSE
	    DPAR(J) = 0.0
	  END IF

	END DO

*    Ensure ties up to date
        CALL FIT_APPTIE( IMOD, .FALSE., PARAM, LB, UB, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get initial statistic and report
	CALL FIT_STAT( NDS, IMOD, PARAM, FSTAT, PREDICTOR, STAT,
     :                 LNDFAC, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
	IF ( OPCHAN .GT. 0 ) THEN
          IF ( FSTAT .EQ. FIT__CHISQ ) THEN
	    WRITE(OPCHAN,200,IOSTAT=ISTAT)STAT,STAT/LSSCALE
 200	    FORMAT(/' Initial chisq & chi-red : ',1PG11.5,2X,1PG12.5/)
          ELSE
	    WRITE(OPCHAN,205,IOSTAT=ISTAT)STAT,STAT/LSSCALE
 205	    FORMAT(/' Initial Cash statistic & scaled statistic : ',
     :                                          1PG15.8,2X,1PG15.8/)
          END IF
	END IF

*    Check that there are some unfrozen parameters
	IF ( NUNFROZEN .LE. 0 ) THEN
	  FITERR = 2
	  GOTO 99
	END IF

*  End of initialisation test
      END IF

*  Main loop
      DO WHILE ( (NIT.LT.NITMAX) .AND. .NOT. FINISHED )

*    Increment iteration counters
	NIT = NIT + 1
	LNITER = LNITER + 1

*    Get first and second derivatives of statistic w.r.t. parameters
	CALL FIT_DERIVS( NDS, IMOD, NPAR, PARAM, LB, UB,
     :                   DPAR, LFROZEN, FSTAT, PREDICTOR, DERIV1,
     :                   DERIV2, STATUS )

*    Test 1st derivs to see if any pegged parameters want to come off bounds
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

*      Set up array of free (i.e. not frozen or pegged) parameter indices
	  IF ( (.NOT.LFROZEN(J)) .AND. .NOT. PEGGED(J) )THEN
	    NPFREE=NPFREE+1
	    IPFREE(NPFREE)=J

*        Set up arrays of 1st and second derivs of REDUCED chi-squared w.r.t.
*        the SCALED parameters, for free parameters only
	    SCDERIV1(NPFREE) = -DERIV1(J)*PSCALE(J)/LSSCALE
	    DO K=1,NPFREE-1	! N.B. Diagonal element not evaluated here
	      SCDERIV2(K,NPFREE)=DERIV2(IPFREE(K),J)*PSCALE(IPFREE(K))*
     :           PSCALE(J)/LSSCALE
	      SCDERIV2(NPFREE,K)=SCDERIV2(K,NPFREE)
	    END DO

*        Scaled statistic gradient in scaled space
	    DSLOPE = DSLOPE + SCDERIV1(NPFREE)**2

	  END IF
	END DO
	SLOPE = SQRT(DSLOPE)

*    Give up if all parameters are pegged
	IF ( NPFREE .LE. 0 ) THEN
	  FITERR = 3
	  GOTO 99
	END IF

*    Diagonal of matrix of second derivs is scaled up by (1+lambda)
        OLDNEWSTAT = 0.0
        RITER = 0
 1000	CONTINUE		! Loop start when retrying with larger lambda
	DO K = 1, NPFREE
	  J = IPFREE(K)
	  SCDERIV2(K,K) = DERIV2(J,J)*DBLE(PSCALE(J)**2)*
     :                                       (1.D0+LAMBDA)/LSSCALE
	END DO

#ifdef NAG
*    Call NAG routine to solve for parameter offsets
        FAIL = 1
        CALL F04ATF( SCDERIV2, NPAMAX, SCDERIV1, NPFREE, PARSTEP,
     :               LU, NPAMAX, WKS1, WKS2, FAIL )

*    Check for error in NAG routine
        IF ( FAIL .NE. 0 ) THEN
          FITERR = FAIL + 3
*...........( FITERR=4 for singular DERIV2 matrix )
*...........(       =5 for ill-conditioned matrix )
          GOTO 99
        END IF
#else
*    Call PDA routine to solve for parameter offsets
        DO K = 1, NPFREE
          PARSTEP(K) = SCDERIV1(K)
          DO J = 1, NPFREE
            SCDERIV3(J,K) = SCDERIV2(J,K)
          END DO
        END DO
        CALL PDA_DGEFS( SCDERIV2, NPAMAX, NPFREE, PARSTEP, 1, ACC,
     :                  WKS1, WKS2, STATUS )

        IF ( ACC .LT. 0 ) THEN
          IF ( ACC .EQ. -3 ) THEN
            FITERR = 4
          ELSE IF ( ACC .EQ. -4 ) THEN
            FITERR = 5
          ELSE IF ( ACC .EQ. -10 ) THEN
            FITERR = 6
          END IF
          STATUS = 0
          GOTO 99
        END IF
#endif

*    Increment iteration counter
        RITER = RITER + 1

*    Evaluate new (unscaled) parameter values and check against bounds
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
        CALL FIT_APPTIE( IMOD, .FALSE., NEWPAR, LB, UB, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Evaluate new statistic and try again with increased lambda if it hasn't
*    improved
        OLDNEWSTAT = NEWSTAT
	CALL FIT_STAT( NDS, IMOD, NEWPAR, FSTAT, PREDICTOR, NEWSTAT,
     :                 LNDFAC, STATUS )
	IF ( NEWSTAT .GT. STAT ) THEN

*      Increase lambda unless the threshold has been passed. In the
*      latter case, raise a fit error. The MAX function traps LAMBDA
*      going to zero due to large number of iterations
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

*      Successful iteration - update parameters
	  DO J = 1, NPAR
	    PARAM(J) = NEWPAR(J)
	    IF ( NEWPEG(J) ) THEN
	      IF ( (OPCHAN.GT.0) .AND. .NOT. PEGGED(J) ) THEN
	        WRITE(OPCHAN,*) '-- Pegging parameter',J,' at',PARAM(J)
	      END IF
	      PEGGED(J) = NEWPEG(J)
	    END IF
	  END DO

*      Output fitting information
	  IF ( OPCHAN .GT. 0 ) THEN
            IF ( FSTAT .EQ. FIT__CHISQ ) THEN
	      WRITE(OPCHAN,2000,IOSTAT=ISTAT) NIT,NEWSTAT/LSSCALE,
     :                        NINT(LOG10(LAMBDA)),SLOPE
 2000	      FORMAT(' it,chi-red,lambda,slope : ',I3,7X,1PG11.5,4X,I3,
     :        4X,1PG11.4)
	    ELSE
	      WRITE(OPCHAN,2005,IOSTAT=ISTAT) NIT,NEWSTAT/LSSCALE,
     :                        NINT(LOG10(LAMBDA)),SLOPE
 2005	      FORMAT(' it,scaled Cash,lambda,slope : ',I3,7X,1PG15.8,4X,
     :        I3,4X,1PG11.4)
	    END IF
	  END IF

*      Reduce lambda for next time
	  LAMBDA = MAX(LAMBDA/10.0,1.0E-10)

	END IF

*    Check to see if minimum has been found
	FINISHED = ((STAT-NEWSTAT).LT.ABS(CHANGELIM*STAT)).AND.
     :                                        (SLOPE.LT.MINSLO)

*    Allow termination after 1st iteration if STAT and SLOPE unchanged.
*    Be careful to allow for zero value of statistic
        IF ( (LNITER.GT.1) .AND. .NOT. FINISHED ) THEN
          FINISHED = (ABS(STAT-NEWSTAT).LE.ABS(TDELTA*STAT)) .AND.
     :               (ABS(SLOPE-OLDSLOPE).LE.ABS(TDELTA*SLOPE))
        END IF

*    Update new statistic
	STAT = NEWSTAT
        OLDSLOPE = SLOPE

*    Report progress?
        NSINCE = NSINCE + 1
        IF ( PRGRES .AND.
     :       ((NSINCE .EQ. NUP) .OR. (NIT.EQ.NITMAX)) ) THEN
          CALL FIT_PRGRS( NIT, PARAM, FROZEN, PEGGED, IMOD, FINISHED,
     :                    STATUS )
          NSINCE = 0
        END IF

*  Next iteration
      END DO

*  Update control data
 99   CALL ADI_CPUT0I( MCTRL, 'Niter', NIT, STATUS )
      CALL ADI_CPUT0R( MCTRL, 'Lambda', LAMBDA, STATUS )
      CALL ADI_CPUT0L( MCTRL, 'Finished', FINISHED, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FIT_MIN1', STATUS )

      END
