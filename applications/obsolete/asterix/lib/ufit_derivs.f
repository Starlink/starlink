*+  UFIT_DERIVS - Evaluates 1st and 2nd derivs of chi-sq w.r.t. model params
      SUBROUTINE UFIT_DERIVS( NPAR, PARAM, COMPARATOR, LB, UB,
     :                        DPAR, FROZEN, DERIV1, DERIV2, STATUS )
*
*    Description :
*
*     First and second derivatives of chi-squared fit between observed data
*     and the data predicted by the model are evaluated with respect to the
*     model parameters (apart from those which are frozen).
*     This calls the user-supplied routine COMPARATOR which returns
*     fit for a given set of parameters. All other information (such as data
*     values and errors) must be passed into this routine in common blocks.
*
*    Method :
*
*     First derivatives and homogeneous second derivatives are evaluated by
*     stepping off in both directions along each parameter dimension, using
*     parameter increments DPAR. The evaluation is centred provided that this
*     would not involve crossing one of the parameter bounds; in this case
*     the increment is truncated at the bound.
*     The mixed second derivatives are evaluated in a similar way by stepping
*     off in two orthogonal directions.
*
*    Deficiencies :
*
*     Requires many more function evaluations than the method used in
*     FIT_DERIVS.
*
*    Bugs :
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*
*    History :
*
*      4 May 88 : Original, adapted from FIT_DERIVS (BHVAD::TJP)
*      1 Jun 88 : Amended to omit FROZEN parameters (TJP)
*     21 Sep 93 : Added COMPARATOR argument (DJA)
*
*    Type Definitions :
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
      INTEGER NPAR			! No of parameters
      EXTERNAL			COMPARATOR
      REAL PARAM(NPAMAX)		! Model parameters
      REAL LB(NPAMAX)			! Parameter lower bounds
      REAL UB(NPAMAX)			! Parameter upper bounds
      REAL DPAR(NPAMAX)			! Desired parameter increments
      LOGICAL FROZEN(NPAMAX)		! Frozen parameter flag
*
*    Export :
*
      DOUBLE PRECISION DERIV1(NPAMAX)	! Array of chisq derivs w.r.t. params
      DOUBLE PRECISION DERIV2(NPAMAX,NPAMAX)	! Matrix of chisq 2nd derivs (approx.)
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL DPUP(NPAMAX)			! Actual upper increment
      REAL DPDOWN(NPAMAX)		! Actual lower increment
      REAL PARTEMP(NPAMAX)		! Temporary (i.e. current) param values
      DOUBLE PRECISION S		! Chisq for unperturbed param values
      DOUBLE PRECISION SSHIFTED		! Chisq for shifted (from bound) centre
      DOUBLE PRECISION SU		! Chisq for upward pertn of one param
      DOUBLE PRECISION SD		! Chisq for downward pertn of one param
      DOUBLE PRECISION SUU(NPAMAX)	! Chisq for upward pertn of 2 params
      DOUBLE PRECISION SUD(NPAMAX)	! Chisq for 1st param up and 2nd down
      DOUBLE PRECISION SDU(NPAMAX)	! Chisq for 1st param down and 2nd up
      DOUBLE PRECISION SDD(NPAMAX)	! Chisq for downward pertn of 2 params

      INTEGER J,K			! Parameter indices
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set up upper and lower increments for parameters, taking bounds into
*    account and initialise current parameter array
      DO J=1,NPAR
	PARTEMP(J)=PARAM(J)
	IF ( .NOT. FROZEN(J) ) THEN
	  IF(PARAM(J)+DPAR(J).LE.UB(J))THEN
	    DPUP(J)=DPAR(J)
	  ELSE
	    DPUP(J)=UB(J)-PARAM(J)
	  END IF
	  IF(PARAM(J)-DPAR(J).GE.LB(J))THEN
	    DPDOWN(J)=DPAR(J)
	  ELSE
	    DPDOWN(J)=PARAM(J)-LB(J)
	  END IF
	END IF
      END DO

*    Unperturbed chisquared
      CALL COMPARATOR( PARAM, S, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 9000

*    Loop through parameters calling COMPARATOR to calculate derivatives
*    of statistic
      DO J=1,NPAR

	IF(.NOT.FROZEN(J))THEN

*        First derivative
	  PARTEMP(J)=PARAM(J)+DPUP(J)
	  CALL COMPARATOR(PARTEMP,SU,STATUS)
	  PARTEMP(J)=PARAM(J)-DPDOWN(J)
	  CALL COMPARATOR(PARTEMP,SD,STATUS)
	  DERIV1(J)=DBLE((SU-SD)/(DPUP(J)+DPDOWN(J)))
	  IF(STATUS.NE.SAI__OK) GO TO 9000

*        JJ second derivative (guarding against poss that one offset could
*        be zero)
	  IF ( DPUP(J) .EQ.0.0 ) THEN
	    PARTEMP(J)=PARAM(J)-0.5*DPDOWN(J)	  ! Centre on lower offset
	    CALL COMPARATOR(PARTEMP,SSHIFTED,STATUS)
	    DERIV2(J,J)=(SU-2*SSHIFTED+SD)/(0.5*DPDOWN(J))**2
	  ELSE IF(DPDOWN(J).EQ.0.0)THEN
	    PARTEMP(J)=PARAM(J)+0.5*DPUP(J)	  ! Centre on upper offset
	    CALL COMPARATOR(PARTEMP,SSHIFTED,STATUS)
	    DERIV2(J,J)=(SU-2*SSHIFTED+SD)/(0.5*DPUP(J))**2
	  ELSE
	    DERIV2(J,J)=((SU-S)/DPUP(J)-(S-SD)/DPDOWN(J))/
     :        (0.5*(DPUP(J)+DPDOWN(J)))
	  END IF
	  IF(STATUS.NE.SAI__OK) GO TO 9000
	  PARTEMP(J)=PARAM(J)

*        Mixed second derivatives
	  DO K=1,J-1
	    IF(.NOT.FROZEN(K))THEN
	      PARTEMP(J)=PARAM(J)+DPUP(J)
	      PARTEMP(K)=PARAM(K)+DPUP(K)
	      CALL COMPARATOR(PARTEMP,SUU(K),STATUS)
	      PARTEMP(K)=PARAM(K)-DPDOWN(K)
	      CALL COMPARATOR(PARTEMP,SUD(K),STATUS)
	      PARTEMP(J)=PARAM(J)-DPDOWN(J)
	      CALL COMPARATOR(PARTEMP,SDD(K),STATUS)
	      PARTEMP(K)=PARAM(K)+DPUP(K)
	      CALL COMPARATOR(PARTEMP,SDU(K),STATUS)
	      DERIV2(J,K)=DBLE((SUU(K)-SDU(K)-SUD(K)+SDD(K))/((DPUP(J)
     :          +DPDOWN(J))*(DPUP(K)+DPDOWN(K))))
	      DERIV2(K,J)=DERIV2(J,K)	  	  ! Fill in upper right triangle
	      PARTEMP(K)=PARAM(K)
	    END IF
	  END DO
	  PARTEMP(J)=PARAM(J)
	END IF
      END DO

*    Exit
 9000 IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from UFIT_DERIVS', STATUS )
      END IF

      END
