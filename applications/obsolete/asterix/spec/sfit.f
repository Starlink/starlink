*+  SFIT - Spectral fitting program
      SUBROUTINE SFIT( STATUS )
*
*    Description :
*
*     Adjusts parameters in a multicomponent model to achieve minimum
*     chi-squared or maximum likelihood fit to one or more datasets.
*     Only a single input dataset can be entered - if multiple datasets are
*     to be fitted a file containing references to them must be first set up
*     using SDATA.
*     The user can abort the program with ^C and will retain the last update
*     to the parameter values, which can then be used to generate plotted
*     output and taken as the starting point for further iteration.
*     Up to three user-defined models: FIT_USER1, _USER2 and _USER3, can be
*     linked to a private version of the program.
*
*    Environment parameters :
*
*     LIK=LOGICAL(R)
*            lilelihood fit (else chi-squared)
*     INP=UNIV(R)
*            input data (either a single dataset or a file of references).
*     Z=REAL(R)
*            redshift of spectrum
*     MODEL=UNIV(R)
*            data object containing model specification
*     MAX=INTEGER(R)
*            max number of iterations to be performed
*     MINS=REAL(R)
*            minimum `reduced statistic' slope forcing continued iteration
*     NUPE=INTEGER(R)
*            number of iterations between updates of model parameter file
*     ERR=LOGICAL(R)
*            evaluate approximate parameter errors and write to model file?
*     OUT=LOGICAL(R)
*            spool (or send to file) summary of fit results?
*     FITOUT=CHAR(R)
*            File name for fit text output
*     APPEND=LOGICAL(R)
*            append o/p to existing file?
*
*    Method :
*
*               Get genus, data, response and model spec.
*               Set up arrays required for fitting
*               Loop NITMAX/NUPDATE times
*		  Call FIT_MIN performing NUPDATE iterations
*		  Type diagnostic info
*		  Update model parameters in model_spec object
*		End loop
*		Optionally compute approximate errors for free parameters and
*                                                write to model file
*		Display and optionally print the results
*
*     The fit optimisation is done by FIT_MIN - a combined
*     gradient/quadratic-fitting minimisation routine based on the CURFIT
*     program of Bevington. Parameter bounds are incorporated by 'pegging'
*     parameters on bounds and excluding them from the fitting process until
*     the fitstat gradient takes them back into the allowed region. 'FROZEN'
*     parameters are never allowed to vary. Computing time is saved by using an
*     approximation to the fitstat second derivs within FIT_DERIVS, rather
*     than a rigorous evaluation.
*     Details of the model prescription (though not the parameter values and
*     bounds) and the observed and predicted data are not required by the
*     main fitting routine itself, only by the routines which evaluate the
*     fitstat value and its derivatives. These details (or in some cases
*     pointers to them) are stored in FORTRAN structures (for neatness given
*     the possibility of multiple datasets) and passed through FIT_MIN
*     intact.
*     For the spectral fitting case, a redshift may be incorporated by scaling
*     all the model space bounds by 1+z, to shift them into the source frame.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*     David J. Allan(JET-X,University of Birmingham)
*
*    History :
*
*     31 Mar 87 : V0.6-1 Original GENFIT (BHVAD::TJP)
*     15 May 87 : V0.6-2 Parameter error estimates & printout of results (TJP)
*     11 Jun 87 : V0.6-3 Data file names returned from FIT_DATGET & output (TJP)
*      5 Nov 87 : V0.6-4 Changes to FIT_MODGET,_MODUP & _CHIMIN. Selectable
*                        MINSLOPE  (TJP)
*      3 May 88 : V0.6-5 New structures,user model hooks,global eliminated (TJP)
*     23 May 88 : Coverted to subroutine FIT_CHIFIT, GENUS passed in (TJP)
*      1 Jul 88 : APPEND option added (TJP)
*     26 Oct 88 : Model spec and name included in printed output (TJP)
*     28 Feb 89 : ASTERIX88 version, includes redshifting (TJP)
*     24 May 89 : Orderly completion where no params are free (TJP)
*     14 Nov 89 : Warning if no response found (TJP)
*     11 Dec 90 : LUN closed after use (TJP)
*     17 Dec 90 : Bug with NDOF=0 fixed (TJP)
*      5 Aug 91 : Bug fix in printout for NIT=0 (TJP)
*     23 May 88 : V0.6-1 Original (BHVAD::TJP)
*      1 Jul 88 : V0.6-2 APPEND option, NDOF=0 handled (TJP)
*     11 Aug 88 : V0.6-3 New (more efficient) SPEC_BH and FIT_FOLD (TJP/MPW)
*     26 Oct 88 : V0.6-4 Model listed in printed output (TJP)
*      7 Jun 89 : V1.0-1 ASTERIX88 version (TJP)
*      7 Dec 90 : V1.4-1 Bug with multiple spectra and model compts fixed (TJP)
*      1 May 91 : V1.5-1 Common block allows 1st run to be flagged (TJP)
*      5 Aug 91 : V1.5-2 Minor fix to allow printout when all params frozen (TJP)
*     26 Mar 92 :        FIT_PREDDAT passed as external (RJV)
*      1 Apr 92 : V1.5-3 FIT_CHIFIT merged into top level (RJV)
*     15 Jun 92 : V1.6-1 Supports likelihood fitting (TJP)
*      3 Jul 92 :        GENUS added to FIT_DATINGET interface (TJP)
*      9 Aug 92 : V1.6-2 Internal write replaces overwrite in printed o/p (TJP)
*     23 Sep 92 : V1.6-3 Use D.P. for statistic (DJA)
*      4 Dec 92 : V1.7-0 Fit error report and red-shift in subroutines (DJA)
*     21 Dec 92 : V1.7-1 Terminal output via MSG (DJA)
*     11 Jan 93 : V1.7-2 Output open/close moved to subroutines (DJA)
*      8 Sep 93 : V1.7-3 Added SPEC_INIT call and removed reference to
*                        SPEC_CMN_RZ (DJA)
*     23 May 94 : V1.7-4 Added parameter tying (DJA)
*     21 Jul 94 : V1.7-5 File output moved to SFIT_OPFILES (DJA)
*     25 Jul 94 : V1.7-6 File output now completely using AIO (DJA)
*      7 Sep 94 : V1.8-0 Print out fit probability (DJA)
*     24 Nov 94 : V1.8-1 Now use USI for user interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Structure definitions :
*
      INCLUDE 'FIT_STRUC'
*
*    Status :
*
      INTEGER STATUS
*
*    External references :
*
      EXTERNAL FIT_PREDDAT		! Data prediction routine
*
*    Local constants :
*
      INTEGER OPCHAN			! Output channel for diagnostic
	PARAMETER (OPCHAN=6)		! messages ( <1 for no messages)
*
*    Local variables :
*
      RECORD /DATASET/    	OBDAT(NDSMAX)		! Observed datasets
      RECORD /INSTR_RESP/ 	INSTR(NDSMAX) 		! Instrument responses
      RECORD /PREDICTION/ 	PREDDAT(NDSMAX) 	! Data predicted by model
      RECORD /MODEL_SPEC/ 	MODEL			! Model specification

      CHARACTER*(DAT__SZLOC) ILOC	! Locator to input data (or ref object)
      CHARACTER*(DAT__SZLOC) MLOC	! Locator to model_spec object
      CHARACTER*2		SPAR			! String version of int

      DOUBLE PRECISION 		STAT			! Fit statistic
      DOUBLE PRECISION 		FPROB			! Fit probability

      REAL 			PARAM(NPAMAX)		! Model parameters
      REAL MINSLO			! Reduced stat threshold for FIT_MIN
      REAL LB(NPAMAX)			! Parameter lower bounds
      REAL UB(NPAMAX)			! Parameter upper bounds
      REAL LE(NPAMAX)			! Lower error estimate
      REAL UE(NPAMAX)			! Upper error estimate
      REAL DPAR(NPAMAX)			! Differential parameter increments
      REAL PARSIG(NPAMAX)		! Parameter 1 sigma errors (approx)
      REAL Z				! Redshift [ Eobs/Esource=1/(1+z) ]

      INTEGER 			NDS			! No of datasets
      INTEGER 			NGOOD			! No. good data elements
      INTEGER SSCALE			! Factor for scaling fitstat
      INTEGER			NC			! No. chars used in SPAR
      INTEGER NDOF			! No of degrees of freedom - should be
					!  no of data - no of unfrozen params
      INTEGER NITMAX			! Return when NIT reaches NITMAX
      INTEGER NITUP			! Period for updating model spec (&
					! typing progress diagnostics)
      INTEGER 			NPAR			! No of parameters
      INTEGER 			NIT			! Iteration number
      INTEGER FITERR			! Fitting error encountered
      INTEGER 			N			! Dataset index
      INTEGER 			J			! Parameter index
      INTEGER NRET			! Itern no for return from FIT_MIN
      INTEGER OCI			! Logical unit number for o/p file
      INTEGER FSTAT			! Fit statistic flag (1=chisq, 2=l'hood)

	LOGICAL CHISTAT			! Fitstat is chi-squared?
	LOGICAL LIKSTAT			! Fitstat is Cash likelihood statistic?
	LOGICAL WORKSPACE		! Set up workspace for STAT gradients?
	LOGICAL FROZEN(NPAMAX)		! Frozen parameter flag
	LOGICAL INITIALISE		! Should be set true on first call only
					! - always returned false
	LOGICAL PEGGED(NPAMAX)		! Parameter pegged on bound
	LOGICAL PRIM			! Primitive input dataset?
	LOGICAL FINISHED		! Minimum found
	LOGICAL NOFREE			! No parameters free
	LOGICAL ER			! Parameter error calculation required?
	LOGICAL OP			! Printout required?
*
*    Version :
*
      CHARACTER*30		VERSION
	PARAMETER		(VERSION='SFIT Version 1.8-1' )
*-

*    Announce version
      CALL MSG_PRNT( VERSION )

*    Set up genus in MODEL structure
      MODEL.GENUS='SPEC'

*    Initialise ASTERIX packages
      CALL AST_INIT
      CALL SPEC_INIT( STATUS )

*    Chi-squared or likelihood fitting?
      CALL USI_GET0L( 'LIK', LIKSTAT, STATUS )
      CHISTAT=.NOT.LIKSTAT
      IF ( LIKSTAT ) THEN
        FSTAT=FIT__LOGL
      ELSE
        FSTAT=FIT__CHISQ
      END IF

*    Get observed data (setting up data weights) and response
      CALL USI_ASSOCI('INP','READ',ILOC,PRIM,STATUS)
      WORKSPACE=.TRUE.
      CALL FIT_DATINGET(ILOC,'SPEC',FSTAT,WORKSPACE,CHISTAT,NDS,OBDAT,
     :  NGOOD,SSCALE,PREDDAT,INSTR,STATUS)
      IF(STATUS.NE.SAI__OK) GOTO 9000

*    Look for redshift
      CALL SFIT_GETZ( Z, STATUS )

*    Apply red-shift and check instrument response
      DO N = 1, NDS

*      Apply redshift to model space energy bounds
        CALL SFIT_APPRED( Z, PREDDAT(N).NMBOUND,
     :                    %VAL(PREDDAT(N).MLBNDPTR),
     :                    %VAL(PREDDAT(N).MUBNDPTR), STATUS )

*      Report on success in finding instrument response if appropriate
	IF ( PREDDAT(N).CONVOLVE ) THEN
	  IF ( NDS .EQ. 1 ) THEN
	    CALL MSG_PRNT('Instrument response found')
	  ELSE
	    CALL MSG_SETI('NDS',N)
	    CALL MSG_PRNT('Instrument response found for dataset ^NDS' )
	  END IF
	ELSE
	  IF ( NDS .EQ. 1 ) THEN
	    CALL MSG_PRNT('!! Warning - no instrument response found'//
     :          ', results may not be meaningful !!')
	  ELSE
	    CALL MSG_SETI('NDS',N)
	    CALL MSG_PRNT('!! Warning - no instrument response for '//
     :          'dataset ^NDS !!')
	  END IF
	END IF
      END DO
      IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH(STATUS)

*    Get model specification
      CALL USI_ASSOCI( 'MODEL', 'UPDATE', MLOC, PRIM, STATUS )
      CALL FIT_MODGET(MLOC,MODEL,NPAR,PARAM,LB,UB,LE,UE,FROZEN,STATUS)
      IF(STATUS.NE.SAI__OK) GOTO 9000

*    Number of degrees of freedom for chi-squared
      IF ( CHISTAT ) THEN

*      Finds NDOF from frozen array and constraints
        CALL FIT1_NDOF( NGOOD, MODEL, FROZEN, NDOF, STATUS )

*      NDOF to be used for scaling chisq statistic
	SSCALE = NDOF

      END IF

*    Set iteration limits
      CALL USI_GET0I('MAX',NITMAX,STATUS)
      CALL USI_GET0R('MINS',MINSLO,STATUS)
      CALL USI_GET0I('NUP',NITUP,STATUS)
      IF ( STATUS .NE. SAI__OK ) GOTO 9000

*    Set up workspace for model stack
      CALL SFIT_MAPMODSTK( NDS, PREDDAT, MODEL.STACKPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 9000

*    Zero iterations means just print the statistic
      IF ( NITMAX .EQ. 0 ) THEN

*      Evaluate the statistic
        CALL FIT_STAT( NDS, OBDAT, INSTR, MODEL, PARAM, FSTAT,
     :                    FIT_PREDDAT, PREDDAT, STAT, STATUS )

*      Report value of statistic
        CALL SFIT_OPSTAT( FSTAT, STAT, SSCALE, NGOOD, 0, STATUS )

*      Goodness of fit
        CALL FIT_MPROB( NDS, OBDAT, FSTAT, SSCALE, PREDDAT, STAT,
     :                  FPROB, STATUS )
        CALL MSG_SETD( 'FPROB', FPROB )
        CALL MSG_PRNT( 'Prob = ^FPROB' )
        CALL MSG_BLNK()
        GOTO 9000

      END IF

*    Main loop
      NIT=0
      INITIALISE=.TRUE.
      FINISHED=.FALSE.
      NOFREE=.FALSE.
      DO WHILE(NIT.LT.NITMAX.AND..NOT.FINISHED)

	NRET=NIT+NITUP
	IF(NRET.GT.NITMAX) NRET=NITMAX
	CALL FIT_MIN(NDS,OBDAT,INSTR,MODEL,OPCHAN,NRET,NPAR,LB,UB,
     :    FROZEN,SSCALE,INITIALISE,MINSLO,FSTAT,FIT_PREDDAT,PREDDAT,
     :    PARAM,DPAR,PEGGED,STAT,NIT,FINISHED,FITERR,STATUS)
	IF(STATUS.NE.SAI__OK) GOTO 9000

*      Fitting error
	IF(FITERR.EQ.2.OR.FITERR.EQ.3)THEN

*        No free parameters - terminate
	  NOFREE=.TRUE.
	  FINISHED=.TRUE.

	ELSE IF(FITERR.NE.0)THEN

*        Fatal fitting error
          CALL FIT_REPFERR( FITERR, STATUS )
	  GOTO 9000

	END IF

*      Report progress
	CALL MSG_BLNK()
        CALL MSG_SETI( 'NIT', NIT )
        CALL MSG_PRNT( '* Iteration ^NIT *' )
	DO J=1,NPAR
	  IF(FROZEN(J))THEN
            CALL MSG_SETC( 'STATE', 'frozen' )
	  ELSE IF(PEGGED(J))THEN
            CALL MSG_SETC( 'STATE', 'pegged' )
	  ELSE
            IF ( (MODEL.NTIE.GT.0) .AND. (MODEL.TGROUP(J).GT.0) ) THEN
              IF ( J .NE. MODEL.TSTART(MODEL.TGROUP(J)) ) THEN
                CALL CHR_ITOC( MODEL.TSTART(MODEL.TGROUP(J)), SPAR, NC )
                CALL MSG_SETC( 'STATE', 'free (tied to '/
     :                                     /SPAR(:NC)//')' )
              ELSE
                CALL MSG_SETC( 'STATE', 'free' )
              END IF
            ELSE
              CALL MSG_SETC( 'STATE', 'free' )
            END IF
	  ENDIF
          CALL MSG_FMTR( 'PVAL', '1PG14.6', PARAM(J) )
          CALL MSG_PRNT( MODEL.PARNAME(J)//' ^PVAL   ^STATE' )
	END DO
	IF ( FINISHED ) THEN
	  CALL MSG_BLNK()
	  IF ( NOFREE ) THEN
	    CALL MSG_PRNT( '+++ No parameters free +++' )
	  ELSE
	    CALL MSG_PRNT( '+++ Minimum found +++' )
	  END IF
	  CALL MSG_BLNK()
	END IF

*     Update model_spec object
	CALL MSG_BLNK()
	CALL MSG_PRNT( '** Updating model spec - do not exit '/
     :                                  /'until completed **' )
	CALL FIT_MODUP(MLOC,MODEL.NCOMP,NPAR,PARAM,LE,UE,-99.0,STATUS)
	IF ( STATUS .NE. SAI__OK ) THEN
	  CALL ERR_FLUSH(STATUS)
	ELSE
	  CALL MSG_PRNT( '** Model updated **' )
	  CALL MSG_BLNK()
	END IF

      END DO

*    Calculate approximate parameter errors and display
      IF ( NOFREE ) THEN
	ER = .FALSE.
      ELSE
	CALL USI_GET0L('ERR',ER,STATUS)
      END IF
      IF ( ER ) THEN
	CALL MSG_BLNK()
	CALL MSG_PRNT( 'Calculating approximate parameter errors' )
	CALL FIT_PARERR(NDS,OBDAT,INSTR,MODEL,NPAR,PARAM,LB,UB,DPAR,
     :    FROZEN,PEGGED,SSCALE,FSTAT,FIT_PREDDAT,PREDDAT,PARSIG,STATUS)
        CALL ARR_COP1R( NPAR, PARSIG, LE, STATUS )
        CALL ARR_COP1R( NPAR, PARSIG, UE, STATUS )

      ELSE

*      Reset array to ensure tidy print out
        CALL ARR_INIT1R( 0.0, NPAR, PARSIG, STATUS )

      END IF

*    Report parameter values
      CALL SFIT_OPTABLE( NPAR, PARAM, FROZEN, PEGGED, PARSIG, MODEL,
     :                                                   0, STATUS )

*    Report value of statistic
      CALL SFIT_OPSTAT( FSTAT, STAT, NDOF, NGOOD, 0, STATUS )

*    Report red-shift
      IF ( Z .NE. 0.0 ) THEN
        CALL MSG_BLNK()
        CALL MSG_FMTR( 'Z', 'F7.5', Z )
        CALL MSG_PRNT( '--- Redshift = ^Z' )
        CALL MSG_BLNK()
      END IF

*    Update model_spec errors
      IF ( ER ) THEN
	CALL FIT_MODUP(MLOC,MODEL.NCOMP,NPAR,PARAM,LE,UE,1.0,STATUS)
	CALL MSG_PRNT( '** Error estimates entered in model spec. **' )
	CALL MSG_BLNK()
      END IF

*    Printed summary of fit results wanted?
      CALL SFIT_OPOPEN( OP, OCI, STATUS )

*    Write output?
      IF ( OP ) THEN

*      Write header
        CALL AIO_TITLE( OCI, VERSION, STATUS )

*      File list
        CALL SFIT_OPFILES( FSTAT, NDS, OBDAT, MODEL, OCI, STATUS )

*      Red-shift
	IF ( Z .NE. 0.0 ) THEN
          CALL MSG_FMTR( 'Z', 'F7.5', Z )
          CALL AIO_WRITE( OCI, '--- Redshifted to z = ^Z', STATUS )
          CALL AIO_BLNK( OCI, STATUS )
	END IF

*      Status of minimisation
        CALL AIO_BLNK( OCI, STATUS )
        CALL MSG_SETI( 'NIT', NIT )
        CALL AIO_WRITE( OCI, 'Terminated after ^NIT iterations',
     :                  STATUS )
	IF ( FINISHED ) THEN
          CALL AIO_WRITE( OCI, 'Minimum found', STATUS )
	ELSE
          CALL AIO_WRITE( OCI, 'Minimum not found', STATUS )
	END IF

*      Report parameter values
        CALL SFIT_OPTABLE( NPAR, PARAM, FROZEN, PEGGED, PARSIG,
     :                                     MODEL, OCI, STATUS )

*      Value of statistic
        CALL SFIT_OPSTAT( FSTAT, STAT, NDOF, NGOOD, OCI, STATUS )

*      Close file
        CALL SFIT_OPCLOSE( OCI, STATUS )

      END IF

*    Tidy up & exit
 9000 CALL USI_ANNUL(MLOC,STATUS)
      CALL USI_ANNUL(ILOC,STATUS)
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
