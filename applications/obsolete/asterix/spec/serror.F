      SUBROUTINE SERROR( STATUS )
*+
*  Name:
*     SERROR

*  Purpose:
*     Spectral error program

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL SERROR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Evaluates confidence interval corresponding to specified
*     chi-squared increase, along each (non-frozen) parameter dimension in
*     the fit space. For likelihood fitting, the Cash statistic is used, since
*     this is still chi-squared distributed about its minimum.

*  Usage:
*     serror {parameter_usage}

*  Environment Parameters:
*     LIK = LOGICAL (read)
*        Likelihood fit (else chi-squared)
*     INP = CHAR (read)
*        Input data (either a single dataset or a file of references).
*     Z = REAL (read)
*        Redshift of spectrum
*     MODEL = CHAR (read)
*        Data object containing model specification
*     DS = REAL (read)
*        Increase in chi-squared which defines confidence region
*     MAX = INTEGER (read)
*        Max number of iterations to be performed
*     PARS = CHAR (read)
*        List of parameters to have error evaluated
*     MIN = REAL (read)
*        Minimum reduced chi-squared slope forcing continued iteration
*     OP = LOGICAL (read)
*        Spool (or send to file) summary of fit results?
*     FITOUT = CHAR (read)
*        Name of fit text output
*     APPEND = LOGICAL (read)
*        Append output to existing file?

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
*     The confidence regions are evaluated by FIT_PARCON.
*     Each parameter in turn is adjusted (using initial guesses LE,UE which
*     are either picked up from the model file or generated from the parameter
*     range) and frozen. FIT_MIN is then called to evaluate the optimum fit
*     obtainable by allowing the other parameters to vary. From the actual
*     fitstat values attained, the parameter offset giving the specified
*     chi-sq. increase (SOFF) is estimated on the assumption
*     that the minimum fitstat surface is a parabolic function of the
*     parameter whose error is being evaluated. The parabola may have a
*     different shape on each side of the minimum, the curve on each side
*     being defined by the assumption of zero slope at the minimum, and the
*     requirement that it should pass through the single offset point
*     evaluated.  e.g.
*
*           .                             !
*            *                            !            .
*             .                           !
*               .<----------------------->!<--------->.      -
*                  .                      !          *       ^
*                       .                 !        .        SOFF
*                              .          !     .            ^
*                                         M                  ^
*
*      where * are the evaluated points, and the resulting confidence limits
*      are shown by <---->.
*
*      From Lampton, Margon & Bowyer (Ap.J. 208, p177, 1976) a value SOFF=1
*      gives a 1 sigma (i.e. 68% confidence) interval and SOFF=2.71 a 90%
*      confidence interval for the case where the model is linear (i.e. the
*      predicted values are linear functions of the parameters). Where this
*      is not the case (i.e. always!) the results may still be approximately
*      right (see LMB and also Avni Ap.J. 210, p642, 1976). For likelihood
*      fitting the Cash statistic C=-2*log(P/P_min) is chi-squared distributed
*      about its minimum (see Cash, Ap.J. 228, 939 (1979)).
*      A safe limit in all cases is obtained by projecting the full
*      NPAR-dimensional confidence interval onto the subspace desired (a single
*      parameter axis in this case). This corresponds to taking
*         SOFF = chi-squared with NPAR d.o.f.( n% conf)
*      for a safe upper bound on the n% confidence limit. For large NPAR this
*      will be way above the linear approximation result.
*
*     If the number of free parameters (apart from that whose interval is
*     being evaluated) is reduced as a result of parameter bounds being
*     encountered, then a warning is issued. The confidence region for an
*     affected parameter must be treated with caution. Pegging of a parameter
*     is likely to truncate the confidence limit (since an extra impediment
*     to fstat minimisation is introduced). Where the limit encountered
*     is real (i.e. the user KNOWS it cannot be crossed) the derived confidence
*     limit stands. If the bound is arbitrary then it should be removed and the
*     confidence region reevaluated.
*
*     Note that the assumption that the chi-squared surface is parabolic
*     about the best fit may NOT be a good one if the minimum is up
*     against a bound (the minimum of the parabola may then be BEYOND
*     this bound). In this case the initial estimate produced by SERROR
*     can be quite poor, but should converge to the correct value on
*     further invocations.

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     serror, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     22 Jun 1988 V0.6-1 (TJP):
*        Original version.
*      1 Jul 1988 V0.6-2 (TJP):
*        APPEND option
*     11 Aug 1988 V0.6-3 (TJP):
*        New (more efficient) SPEC_BH and FIT_FOLD
*     26 Oct 1988 V0.6-4 (TJP):
*        Model file listed in printout
*     19 Jun 1989 V1.0-1 (TJP):
*        First ASTERIX88 version
*      7 Dec 1990 V1.4-1 (TJP):
*        Bug with multiple spectra and model compts fixed
*      5 Aug 1991 V1.5-1 (TJP):
*        INIT flag added
*     29 Aug 1991 V1.5-2 (TJP):
*        Confidence region zeroed for FROZEN parameters
*      1 Apr 1992 V1.5-3 (RJV):
*        FIT_ERRORS merged into top level
*     24 Jun 1992 V1.6-1 (TJP):
*        Supports likelihood fitting
*     23 Sep 1992 V1.6-2 (DJA):
*        Statistic in D.P.
*     24 Nov 1992 V1.6-3 (TJP):
*        Write (non-zero) redshift to output listing
*      4 Dec 1992 V1.7-0 (DJA):
*        Fit error report and red-shift in subroutines
*      8 Dec 1992 V1.7-1 (DJA):
*        Direct terminal i/o replaced by MSG_
*     11 Jan 1993 V1.7-2 (DJA):
*        Output open/close moved to subroutines
*     13 Jan 1993 V1.7-3 (DJA):
*        User can select parameters for error analysis (DJA)
*      8 Sep 1993 V1.7-4 (DJA):
*        Added SPEC_INIT call and removed reference to SPEC_CMN_RZ
*     21 Jul 1994 V1.7-5 (DJA):
*        Updated output file handling
*     25 Jul 1994 V1.7-6 (DJA):
*        Use AIO for all text output
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     21 Apr 1995 V1.8-1 (DJA):
*        Removed explicit use of HDS
*      1 Dec 1995 V2.0-0 (DJA):
*        ADI port
*     17 Apr 1996 V2.0-1 (DJA):
*        New minimisation control
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'USER_ERR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'FIT_PAR'

*  Structure Definitions:
      INCLUDE 'FIT_STRUC'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			FIT_PREDDAT

*  Local Constants:
      INTEGER 			OPCHAN			! Output channel for diagnostic
							! messages ( <1 for no messages)
	PARAMETER 		( OPCHAN = 6 )

      REAL 			OFFRAC			! Fraction of parameter range used
	PARAMETER 		( OFFRAC = 0.01 )		! for offset if no estimates available

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'SERROR Version 2.2-1' )

*  Local Variables:
c     RECORD /DATASET/ 		OBDAT(NDSMAX)		! Observed datasets
c     RECORD /INSTR_RESP/ 	INSTR(NDSMAX)		! Instrument responses
c     RECORD /PREDICTION/ 	PREDDAT(NDSMAX) 	! Data predicted by model
c     RECORD /MODEL_SPEC/ 	MODEL			! Model specification
      INTEGER			IMOD

      DOUBLE PRECISION 		STAT           		! Fit statistic
      DOUBLE PRECISION 		LNDFAC

      REAL 			DPAR(NPAMAX)		! Differential parameter increments
      REAL 			LB(NPAMAX)	        ! Parameter lower bounds
      REAL 			LE(NPAMAX)		! Lower error estimate
      REAL 			PARAM(NPAMAX)		! Model parameters
      REAL 			SOFF			! Chi-squared increase for conf.region
      REAL 			UB(NPAMAX)		! Parameter upper bounds
      REAL 			UE(NPAMAX)	        ! Upper error estimate
      REAL 			Z			! Redshift [ Eobs/Esource=1/(1+z) ]

      INTEGER 			FSTAT		        ! Fit statistic flag (1=chisq, 2=l'hood)
      INTEGER			IFID			! Input dataset id
      INTEGER			MFID			! Model spec id

      INTEGER 			NDS			! No of datasets
      INTEGER 			NGOOD			! # good data elements
      INTEGER 			NDOF			! Degrees of freedom
      INTEGER 			NEPAR                   ! # pars for error analysis
      INTEGER 			SSCALE			! Factor for scaling fitstat
      INTEGER 			NPAR			! No of parameters
      INTEGER 			FITERR			! Fitting error encountered
      INTEGER 			J			! Parameter index
      INTEGER			MCTRL			! Minimisation control
      INTEGER 			OCI			! Logical unit number for o/p file
      INTEGER 			PARS(NPAMAX)            ! Parameters to have errors evaluated
      INTEGER 			PEGC(NPAMAX)		! Pegging code (flags pegging changes)

      LOGICAL 			FINISHED		! Iterations finished
      LOGICAL 			FROZEN(NPAMAX)		! Frozen parameter flag
      LOGICAL 			LIKSTAT			! Fitstat is Cash likelihood statistic?
      LOGICAL 			OP			! Printout required?
      LOGICAL 			PEGGED(NPAMAX)		! Parameter pegged on bound
      LOGICAL 			WORKSPACE		! Set up workspace for fstat gradients?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Set up genus in MODEL structure
      IMOD = 1
      MODEL_SPEC_GENUS(IMOD) = 'SPEC'

*  Initialise ASTERIX
      CALL AST_INIT()
      CALL SPEC_INIT( STATUS )

*  Chi-squared or likelihood statistic?
      CALL USI_GET0L('LIK',LIKSTAT,STATUS)
      IF ( LIKSTAT ) THEN
	FSTAT = FIT__LOGL
      ELSE
	FSTAT = FIT__CHISQ
      END IF

*  Get observed data (setting up data weights) and response
      CALL USI_ASSOC( 'INP', 'FileSet|BinDS', 'READ', IFID, STATUS )
      WORKSPACE = .TRUE.
      CALL FIT_GETDAT( ADI__NULLID, IFID, 'SPEC', FSTAT, WORKSPACE,
     :                 (.NOT. LIKSTAT), NDS, NGOOD, SSCALE,
     :                 LNDFAC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Look for redshift
      CALL SFIT_GETZ( Z, STATUS )

*  Apply red-shift and check data structures
      CALL SFIT_PRECHK( NDS, Z, STATUS )

*  Get model specification
      CALL USI_ASSOC( 'MODEL', '*', 'UPDATE', MFID, STATUS )
      CALL FIT_MODGET( MFID, IMOD, NPAR, PARAM, LB, UB, LE, UE,
     :                 FROZEN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Number of degrees of freedom for chi-squared
      IF ( .NOT. LIKSTAT ) THEN

*    Derive from model spec and frozen array
        CALL FIT1_NDOF( NGOOD, IMOD, FROZEN, NDOF, STATUS )

*    NDOF to be used for scaling chisq statistic
	SSCALE = NDOF

      END IF

*  Chi-squared offset
      CALL USI_GET0R( 'DS', SOFF, STATUS )
      IF ( SOFF .LE. 0.0 ) THEN
	STATUS=SAI__ERROR
	CALL ERR_REP( ' ', 'Non-positive confidence interval', STATUS )
	GOTO 99
      END IF

*  Get minimisation control
      CALL FCI_GETMC( MCTRL, STATUS )

*  Parameters to have errors evaluated
      CALL PRS_GETLIST( 'PARS', NPAR, PARS, NEPAR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      DO J = 1, NEPAR
        IF ( (PARS(J).LE.0) .OR. (PARS(J).GT.NPAR) ) THEN
          CALL MSG_SETI( 'NP', PARS(J) )
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Invalid parameter number ^NP', STATUS )
          GOTO 99
        END IF
      END DO

*  Set up workspace for model stack
      CALL SFIT_MAPMODSTK( NDS, MODEL_SPEC_STACKPTR(IMOD), STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Set up offsets for parameters
      DO J = 1, NPAR

*    Don't do errors for frozen parameters
	IF ( .NOT. FROZEN(J) ) THEN

*      Upper offset
	  IF ( UE(J) .LE. 0.0 ) THEN
	    IF(LE(J).GT.0.0)THEN
* Use parabolically scaled upper error
	      UE(J)=LE(J)*SQRT(SOFF)
	    ELSE
*        No existing error estimates available, use OFFRAC*param range
              UE(J)=OFFRAC*(UB(J)-LB(J))
	    END IF
	  ELSE
*        Scale the 1 sigma error (parabolically) to confidence SOFF
	    UE(J)=UE(J)*SQRT(SOFF)
	  END IF

*     Repeat for lower error
	    IF(LE(J).LE.0.0)THEN
	      IF(UE(J).GT.0.0)THEN
	        LE(J)=UE(J)	! UE is already scaled
	      ELSE
*        No existing error estimates available, use OFFRAC*param range
	        LE(J)=OFFRAC*(UB(J)-LB(J))
	      ENDIF
	    ELSE
*        Scale the 1 sigma error (parabolically) to confidence SOFF
	      LE(J)=LE(J)*SQRT(SOFF)
	    ENDIF

*    Zero error array values for FROZEN parameters
        ELSE
	  LE(J) = 0.0
	  UE(J) = 0.0

	END IF

*  Next parameter
      END DO

*  Check that starting point is a minimum
      FINISHED = .FALSE.
      CALL MSG_PRNT( '* Checking initial minimum' )
      CALL FIT_MIN( NDS, IMOD, MCTRL, OPCHAN, .FALSE.,
     :              NPAR, LB, UB, FROZEN, SSCALE, LNDFAC, FSTAT,
     :              FIT_PREDDAT, PARAM, DPAR, PEGGED, STAT,
     :              FINISHED, FITERR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Fitting error?
      IF ( FITERR .NE. 0 ) THEN
        CALL FIT_REPFERR( FITERR, STATUS )
	GOTO 99
      END IF

*  Warning if minimum not found
      IF ( .NOT. FINISHED ) THEN
	CALL MSG_PRNT( '!! Warning - minimum not achieved !!' )
      END IF

*  Update parameters if necessary
      CALL FIT_MODUP( MFID, MODEL_SPEC_NCOMP(IMOD), NPAR, PARAM,
     :                LE, UE, 0.0, STATUS )				!

*  Repeat until successful error run
      FINISHED = .FALSE.
      CALL MSG_PRNT( '* Calculating confidence limits' )
      DO WHILE ( .NOT. FINISHED )

*    Calculate parameter confidence limits
	CALL FIT_PARCON( NDS, IMOD, MCTRL, SOFF,
     :                   OPCHAN, NEPAR, PARS, NPAR, LB, UB, FROZEN,
     :                   SSCALE, DPAR, FSTAT, FIT_PREDDAT,
     :                   STAT, PARAM, PEGGED, LE, UE, PEGC,
     :                   LNDFAC, STATUS )

*    Point lower than "minimum" encountered, try again
        IF ( STATUS .EQ. USER__001 ) THEN
	  CALL ERR_ANNUL( STATUS )
	  CALL MSG_PRNT('Point below "minimum" found -'//
     :      ' trying again with updated minimum')
          CALL FIT_MODUP(MFID,MODEL_SPEC_NCOMP(IMOD),NPAR,PARAM,LE,UE,
     :      0.0,STATUS )
	ELSE IF(STATUS.NE.SAI__OK)THEN
	  GOTO 99
	ELSE
	  FINISHED = .TRUE.
        END IF

      END DO

*  Update fit_model object
      CALL FIT_MODUP( MFID, MODEL_SPEC_NCOMP(IMOD), NPAR, PARAM,
     :                LE, UE, SOFF, STATUS )
      CALL MSG_PRNT( '* Model spec updated' )

*  Display results to terminal
      CALL SFIT_OPETABLE( NPAR, PARAM, NEPAR, PARS, LE, UE, FROZEN,
     :                    PEGC, IMOD, 0, STATUS )

*  Fit statistic at minimum
      CALL SFIT_OPSTAT( FSTAT, STAT, NDOF, NGOOD, 0, STATUS )
      IF ( Z .NE. 0.0 ) THEN
        CALL MSG_FMTR( 'Z', 'F7.5', Z )
        CALL MSG_PRNT( '--- Redshift = ^Z' )
        CALL MSG_BLNK()
      END IF

*  Printed summary of fit results?
      CALL SFIT_OPOPEN( OP, OCI, STATUS )

*  Output?
      IF ( OP ) THEN

*    Write header
        CALL AIO_TITLE( OCI, VERSION, STATUS )

*    Write file list
        CALL SFIT_OPFILES( FSTAT, NDS, IMOD, OCI, STATUS )

*    Red-shift
	IF ( Z .NE. 0.0 ) THEN
          CALL MSG_FMTR( 'Z', 'F7.5', Z )
          CALL AIO_WRITE( OCI, '--- Redshifted to z = ^Z', STATUS )
          CALL AIO_BLNK( OCI, STATUS )
	END IF

*    Report confidence interval
        CALL MSG_FMTR( 'SOFF', '1PG11.4', SOFF )
        CALL AIO_WRITE( OCI, 'Confidence region corresponds to '/
     :                 /'chi-squared increase of ^SOFF', STATUS )
        CALL AIO_BLNK( OCI, STATUS )

*    Parameter error table
        CALL SFIT_OPETABLE( NPAR, PARAM, NEPAR, PARS, LE, UE, FROZEN,
     :                                     PEGC, IMOD, OCI, STATUS )

*    Statistic at minimum
        CALL SFIT_OPSTAT( FSTAT, STAT, NDOF, NGOOD, OCI, STATUS )

*    Close file
        CALL SFIT_OPCLOSE( OCI, STATUS )

      END IF

*  Tidy up & exit
 99   CALL USI_ANNUL( 'MODEL', STATUS )
      CALL USI_ANNUL( 'INP', STATUS )
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
