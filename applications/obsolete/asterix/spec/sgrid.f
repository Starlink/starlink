      SUBROUTINE SGRID( STATUS )
*+
*  Name:
*     SGRID

*  Purpose:
*     Spectral gridding program

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL SGRID( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Select one or more parameters in a multicomponent model over which
*     grid or grid(s) of fit statistic/reoptimised parameters will be
*     constructed.
*
*     Those parameters chosen as the grid axes are automatically unfrozen.
*
*     Adjusts parameters in a to achieve minimum
*     chi-squared or maximum likelihood fit to one or more datasets.
*     Only a single input dataset can be entered - if multiple datasets are
*     to be fitted a file containing references to them must be first set up
*     using SDATA.

*  Usage:
*     sgrid {parameter_usage}

*  Environment Parameters:
*     LIK = LOGICAL (read)
*        Likelihood fit (else chi-squared)
*     INP = CHAR (read)
*        Input data (either a single dataset or a file of references).
*     Z = REAL (read)
*        Redshift of spectrum
*     FIT_MOD = CHAR (read)
*        Data object containing model specification
*     MAX = INTEGER (read)
*        Max number of iterations to be performed
*     MINS = REAL (read)
*        Minimum `reduced statistic' slope forcing continued iteration
*     PARS = INTEGER (read)
*        List of parameters for grid axes
*     OPT = LOGICAL (read)
*        Mode for specifying grid spacing
*     ERR = LOGICAL (read)
*        Take AXISn defaults from parameter errors?
*     AXIS1..7 = CHAR (read)
*        Parameter range to grid
*     LOG1..7 = LOGICAL (read)
*        Log spacing for this axis
*     GPARS = INTEGER (read)
*        Values to be gridded
*     SUBSTAT = LOGICAL (read)
*        Subtract minimum value of statistic from grid
*     UP = LOGICAL (read)
*        Update model with best fit in the grid?
*     OUTROOT = CHAR (read)
*        Root of output filenames in AUTO mode
*     OUT = CHAR (read)
*        Name of output file in non-AUTO mode, where NGRID=1
*     OUT2..5 = CHAR (read)
*        Output file names for extra grids

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
*     {algorithm_description}...

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
*     sgrid, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      9 Nov 1992 V1.7-0 (DJA):
*        Original, derived from SFIT
*      3 Dec 1992 V1.7-1 (DJA):
*        Corrected calculation of SSCALE in chi-squared fitting
*      4 Jan 1993 V1.7-2 (DJA):
*        Updated quality handling
*      1 Apr 1993 V1.7-3 (DJA):
*        Add minimum statistic to history
*      7 Apr 1993 V1.7-4 (DJA):
*        Removed limits on size of axes
*      8 Sep 1993 V1.7-5 (DJA):
*        Change in behaviour in AUTO mode to handle parameter
*        system funnies. Added SPEC_INIT call
*     28 Feb 1994 V1.7-6 (DJA):
*        Use BIT_ routines to do bit manipulations
*      7 Sep 1994 V1.8-0 (DJA):
*        Added fit probability option
*     24 Nov 1994 V1.8-1 (DJA):
*        Now use USI for user interface
*      1 Dec 1995 V2.0-0 (DJA):
*        ADI port
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'FIT_PAR'

*  Structure Definitions:
      INCLUDE 'FIT_STRUC'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
      EXTERNAL			FIT_PREDDAT

*  Local Constants:
      INTEGER                	MXGRID             	! Max # grids
        PARAMETER            	( MXGRID = 5 )

      INTEGER                	OPCHAN             	! Output channel for FIT_GRID
        PARAMETER            	( OPCHAN = 6 )

      INTEGER		     	PROB_REPLY
	PARAMETER	     	( PROB_REPLY = 99 )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'SGRID Version V2.0-0' )

*  Local Variables:
      RECORD /GRID_AXIS/     	GAX(ADI__MXDIM)    	! Grid axes
      RECORD /INSTR_RESP/    	INSTR(NDSMAX)      	! Instrument responses
      RECORD /MODEL_SPEC/    	MODEL              	! Model specification
      RECORD /DATASET/       	OBDAT(NDSMAX)      	! Observed datasets
      RECORD /PREDICTION/    	PREDDAT(NDSMAX)    	! Data predicted by model

      CHARACTER*132          	HBUF(7)            	! History buffer
      CHARACTER*40           	LABEL              	! Grid label
      CHARACTER*1            	PC                 	! Parameter code
      CHARACTER*80           	ROOT, OUTFN        	! Output grid file names
      CHARACTER*80           	TEXT               	! Various o/p text

      DOUBLE PRECISION       	STATMIN            	! Fit statistic

      REAL                   BASE, SCALE        ! Grid axis values
      REAL                   LB(NPAMAX)		! Parameter lower bounds
      REAL                   LE(NPAMAX)		! Parameter lower errors
      REAL                   ALO, AHI           ! Grid axis bounds
      REAL                   MINSLO		! Reduced stat threshold for FIT_MIN
      REAL                   PARAM(NPAMAX)	! Model parameters
      REAL                   RANGE(2)           ! User supplied axis range
      REAL                   UB(NPAMAX)		! Parameter upper bounds
      REAL                   UE(NPAMAX)		! Parameter upper errors
      REAL                   Z			! Redshift [ Eobs/Esource=1/(1+z) ]

      INTEGER			BDID			! New BinDS object
      INTEGER                	FSTAT			! Fit statistic flag (1=chisq, 2=l'hood)
      INTEGER                	GDPTR(MXGRID)      	! Grid data pointer
      INTEGER                	GDIMS(ADI__MXDIM)  	! Grid dimensions
      INTEGER 			GFID(MXGRID)       	! Fit grid datasets
      INTEGER                	GNELM              	! # grid elements
      INTEGER                	GPARS(MXGRID)      	! Things to be gridded
      INTEGER                	GPS(ADI__MXDIM)    	! Grid parameters
      INTEGER                	GQPTR              	! Grid quality pointer
      INTEGER                	I, J            	! Loop variables
      INTEGER			IFID			! Input dataset id
      INTEGER			MFID			! Model spec dataset id
      INTEGER                NDOF		! d.o.f. - no of data values
					        ! - no of unfrozen params
      INTEGER                	NDS			! No of datasets
      INTEGER                	NGOOD			! # good data elements
      INTEGER                	NGRIDAX            	! # grid axes
      INTEGER                	NGRID              	! # grids
      INTEGER                	NHBUF              	! # lines used in HBUF
      INTEGER                NITMAX		! Return when NIT reaches NITMAX
      INTEGER                NRANGE             ! # range values entered
      INTEGER                	NPAR			! No of parameters
      INTEGER                	PCOMP(NPAMAX)		! Model par nos.
      INTEGER                	PPAR(NPAMAX)		! Model comp nos.
      INTEGER                	OPTION             	! Grid definition options
      INTEGER                	SSCALE			! Factor for scaling fitstat
      INTEGER                	TLEN               	! Used length of TEXT

      BYTE                   	GQMASK             	! Grid quality mask

      LOGICAL                	ANYBAD             	! Any bad grid points?
      LOGICAL                	ANYBADISH          	! Any bad(ish) grid points?
      LOGICAL                	AUTO               	! Automatic o/p file naming?
      LOGICAL                	CHISTAT			! Fitstat is chi-squared?
      LOGICAL                	ERR                	! Take AXISn defaults from
							! parameter errors?
      LOGICAL                	FROZEN(NPAMAX)		! Frozen parameter flag
      LOGICAL                	LIKSTAT			! Fitstat is Cash likelihood statistic?
      LOGICAL                	LOGARITHMIC        	! Logarithmic grid axis?
      LOGICAL                	OPTIMISING         	! Using FIT_MIN in FIT_GRID?
      LOGICAL                	PFLAG(NPAMAX)		! Parameter in use flags
      LOGICAL                	SUBSTAT            	! Subtract minimum statistic?
      LOGICAL                	UP                 	! Update model after grid
      LOGICAL                	WORKSPACE		! Set up workspace for STAT gradients?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Set up genus in MODEL structure
      MODEL.GENUS = 'SPEC'

*  Initialise ASTERIX
      CALL AST_INIT()
      CALL SPEC_INIT( STATUS )

*  Chi-squared or likelihood fitting?
      CALL USI_GET0L( 'LIK', LIKSTAT, STATUS )
      CHISTAT=.NOT.LIKSTAT
      IF ( LIKSTAT ) THEN
	FSTAT = FIT__LOGL
      ELSE
	FSTAT = FIT__CHISQ
      END IF

*  Get observed data (setting up data weights) and response
      CALL USI_ASSOC( 'INP', 'FileSet|BinDS', 'READ', IFID, STATUS )
      WORKSPACE = .TRUE.
      CALL FIT_GETDAT( IFID, 'SPEC', FSTAT, WORKSPACE, CHISTAT, NDS,
     :                OBDAT, NGOOD, SSCALE, PREDDAT, INSTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Look for redshift
      CALL SFIT_GETZ( Z, STATUS )

*  Apply red-shift and check data structures
      CALL SFIT_PRECHK( NDS, Z, PREDDAT, STATUS )

*  Get model specification
      CALL USI_ASSOC( 'MODEL', '*', 'UPDATE', MFID, STATUS )
      CALL FIT_MODGET( MFID, MODEL, NPAR, PARAM, LB, UB, LE, UE,
     :                                          FROZEN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Number of degrees of freedom for chi-squared
      IF ( CHISTAT ) THEN
        CALL FIT1_NDOF( NGOOD, MODEL, FROZEN, NDOF, STATUS )
	SSCALE = NDOF
      END IF

*  Allocate space for model stack
      CALL SFIT_MAPMODSTK( NDS, PREDDAT, MODEL.STACKPTR, STATUS )

*  List parameters
      CALL SEDIT_LISTPAR( MFID, NPAR, PCOMP, PPAR, 6, STATUS )

*  Select parameters for grid axes
      CALL PRS_GETLIST( 'PARS', ADI__MXDIM, GPS, NGRIDAX, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check free parameters
      IF ( (NDOF+NGRIDAX) .LT. 0 ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP( ' ','More free parameters than data values!',
     :                                                     STATUS )
	GOTO 99
      END IF

*  Get AXISn defaults from errors
      CALL USI_GET0L( 'ERR', ERR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Option for choosing binning characteristics
      CALL USI_GET0I( 'OPT', OPTION, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( (OPTION .LT. 1) .OR. (OPTION.GT.3) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'OPT must lie between 1 and 3', STATUS )
      ELSE IF ( OPTION .EQ. 1 ) THEN
        CALL MSG_PRNT( 'All axes will be linearly spaced' )
      ELSE IF ( OPTION .EQ. 2 ) THEN
        CALL MSG_PRNT( 'All axes will be spaced linearly in '/
     :                               /'log(parameter) value' )
      END IF

*  Check each grid axis
      DO I = 1, NGRIDAX

*    Construct parameter code
        WRITE( PC, '(I1.1)' ) I

*    Is this a valid fit parameter number
        IF ( (GPS(I).LT.1) .OR. (GPS(I).GT.NPAR) ) THEN
          STATUS = SAI__ERROR
          CALL MSG_SETI( 'P', GPS(I) )
          CALL ERR_REP( ' ', 'Invalid parameter number /^P/', STATUS )
          GOTO 99
        END IF

*    Already selected as a grid axis?
        DO J = 1, I-1
          IF ( GAX(J).PAR .EQ. GPS(I) ) THEN
            CALL MSG_SETI( 'AX', J )
            CALL MSG_PRNT( '! WARNING : Already using this '/
     :                        /'parameter in grid axis ^AX' )
          END IF
        END DO

*    Define default for parameter range
        IF ( ERR ) THEN
          CALL MSG_SETR( 'LO', PARAM(GPS(I))-LE(GPS(I)) )
          CALL MSG_SETR( 'HI', PARAM(GPS(I))+UE(GPS(I)) )
        ELSE
          CALL MSG_SETR( 'LO', LB(GPS(I)) )
          CALL MSG_SETR( 'HI', UB(GPS(I)) )
        END IF
        CALL MSG_MAKE( '^LO:^HI', TEXT, TLEN )
        CALL USI_DEF0C( 'AXIS'//PC, TEXT(:TLEN), STATUS )

*    Remind user of units
        IF ( MODEL.UNITS(GPS(I)) .GT. ' ' ) THEN
          CALL MSG_SETC( 'PAR', MODEL.PARNAME(GPS(I)) )
          CALL MSG_SETC( 'UNIT', MODEL.UNITS(GPS(I)) )
          CALL MSG_PRNT( 'Enter ^PAR values in units of ^UNIT' )
        END IF

*    Get parameter range
        CALL PRS_GETRANGES( 'AXIS'//PC, 2, 1, LB(GPS(I)), UB(GPS(I)),
     :                                        RANGE, NRANGE, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get # grid values
        CALL USI_GET0I( 'NBIN'//PC, GDIMS(I), STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        IF ( GDIMS(I) .LT. 1 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Number of bins must be 1 or greater',
     :                                                     STATUS )
          GOTO 99
        END IF

*    Regular or logarithmic
        IF ( OPTION .EQ. 1 ) THEN
          LOGARITHMIC = .FALSE.
        ELSE IF ( OPTION .EQ. 2 ) THEN
          LOGARITHMIC = .TRUE.
        ELSE
          CALL USI_GET0L( 'LOG'//PC, LOGARITHMIC, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99
        END IF

*    Extract extrema of range supplied
        ALO = RANGE(1)
        AHI = RANGE(2)

*    Check within bounds
        IF ( ALO .LT. LB(GPS(I)) ) THEN
          ALO = LB(GPS(I))
          CALL MSG_PRNT( 'Lower limit adjusted to parameter'/
     :                                      /' lower bound' )
        END IF
        IF ( AHI .GT. UB(GPS(I)) ) THEN
          AHI = UB(GPS(I))
          CALL MSG_PRNT( 'Upper limit adjusted to parameter'/
     :                                      /' upper bound' )
        END IF

*    Check log axis is sensible
        IF ( LOGARITHMIC ) THEN
          IF ( (ALO.LE.0.0) .OR. (AHI.LE.0.0) ) THEN
            CALL MSG_PRNT( 'Cannot have logarithmic axis containing '/
     :                                    /'zero or negative values' )
          END IF
        END IF

*    Calculate base and scale
        IF ( LOGARITHMIC ) THEN
          SCALE = (LOG10(AHI)-LOG10(ALO))/GDIMS(I)
          BASE = LOG10(ALO)+SCALE/2.0
        ELSE
          SCALE = (AHI-ALO)/GDIMS(I)
          BASE = ALO + SCALE/2.0
        END IF

*    Define the grid axis
        CALL FIT_DEFREGRID( GPS(I), GDIMS(I), LOGARITHMIC, BASE,
     :                                   SCALE, GAX(I), STATUS )

*    Adjust SSCALE
        IF ( CHISTAT ) SSCALE = SSCALE + 1

      END DO
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Total number of grid elements
      CALL ARR_SUMDIM( NGRIDAX, GDIMS, GNELM )

*  Are any parameters left to be optimised?
      DO I = 1, NPAR
        PFLAG(I) = (.NOT.FROZEN(I))
      END DO
      DO I = 1, NGRIDAX
        PFLAG(GAX(I).PAR) = .FALSE.
      END DO
      OPTIMISING = .FALSE.
      DO I = 1, NPAR
        OPTIMISING = (OPTIMISING.OR.PFLAG(I))
      END DO

*  Get things to be gridded
      CALL USI_GET1I( 'GPARS', MXGRID, GPARS, NGRID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check valid
      IF ( NGRID .GT. 0 ) THEN
        DO I = 1, NGRID
          IF ( ((GPARS(I) .LT. 0) .OR. (GPARS(I).GT.NPAR)) .AND.
     :         (GPARS(I).NE.PROB_REPLY) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ITEM', GPARS(I) )
            CALL ERR_REP( ' ', 'Invalid grid item, ^ITEM', STATUS )
            GOTO 99
          END IF
        END DO
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Must grid at least one value', STATUS )
      END IF

*  Automatic naming or ask for each one?
      IF ( NGRID .GT. 1 ) THEN
        CALL USI_GET0L( 'AUTO', AUTO, STATUS )
        IF ( AUTO ) THEN
          CALL USI_GET0C( 'OUTROOT', ROOT, STATUS )
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Open grid data files
      DO I = 1, NGRID

*    Create interface object
        CALL BDI_NEW( 'BinDS', NGRIDAX, GDIMS, 'REAL', BDID, STATUS )

*    First input in automatic mode, or sole output in non-automatic
*    Automatic file naming?
        IF ( AUTO ) THEN

*      Create file name
          WRITE( OUTFN, '(2A,I2.2)' ) ROOT(:CHR_LEN(ROOT)),'_g',
     :                                                  GPARS(I)

*      Open the file
          CALL ADI_FCREAT( OUTFN, BDID, GFID(I), STATUS )

*    Manual file naming
        ELSE

          IF ( I .EQ. 1 ) THEN
            IF ( NGRID .GT. 1 ) THEN
              CALL USI_PROMT( 'OUT', 'Output filename for 1st grid',
     :                                                      STATUS )
            END IF
            CALL USI_CREAT( 'OUT', BDID, GFID(I), STATUS )

*      Prompt for o/p filename
          ELSE
            WRITE( PC, '(I1.1)' ) I
            CALL USI_CREAT( 'OUT'//PC, BDID, GFID(I), STATUS )

          END IF

        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Create grid axes
        CALL FIT_CREGRIDAX( GFID(I), NGRIDAX, GAX, STATUS )

*    Write axis labels and units
        DO J = 1, NGRIDAX
          CALL BDI_AXPUT0C( GFID(I), J, 'Label',
     :                      MODEL.PARNAME(GAX(J).PAR), STATUS )
          CALL BDI_AXPUT0C( GFID(I), J, 'Units',
     :                      MODEL.PARNAME(GAX(J).PAR), STATUS )
        END DO

*    Title and units for this grid
        IF ( GPARS(I) .EQ. 0 ) THEN
          IF ( CHISTAT ) THEN
            LABEL = 'Chi-squared'
          ELSE
            LABEL = 'Cash statistic'
          END IF
        ELSE IF ( GPARS(I) .EQ. PROB_REPLY ) THEN
          LABEL = 'Fit probability'
        ELSE
          LABEL = MODEL.PARNAME(GPARS(I))
        END IF

*    Write as axis label for 1-d, otherwise title
        IF ( NGRIDAX .EQ. 1 ) THEN
          CALL BDI_PUT0C( GFID(I), 'Label', LABEL, STATUS )
        ELSE
          CALL BDI_PUT0C( GFID(I), 'Title', LABEL, STATUS )
        END IF

*    Grid units
        IF ( (GPARS(I) .NE. 0) .AND. (GPARS(I).NE.PROB_REPLY) ) THEN
          IF ( MODEL.UNITS(GPARS(I)) .GT. ' ' ) THEN
            IF ( NGRIDAX .GT. 1 ) THEN
              CALL BDI_PUT0C( GFID(I), 'Title', LABEL(:CHR_LEN(LABEL))//
     :                               ' ('//MODEL.UNITS(GPARS(I))(:
     :              CHR_LEN(MODEL.UNITS(GPARS(I))))//')', STATUS )
            ELSE
              CALL BDI_PUT0C( GFID(I), 'Units', MODEL.UNITS(GPARS(I)),
     :                                                  STATUS )
            END IF
          END IF
        END IF

*    Write some history - copy first bit from first grid
        IF ( I .EQ. 1 ) THEN

*      Version id
          CALL HSI_ADD( GFID(I), VERSION, STATUS )

*      Input file(s) and model
          HBUF(1) = 'Data {INP}'
          HBUF(2) = 'Model {MODEL}'
          NHBUF = 6
          CALL USI_TEXT( 2, HBUF, NHBUF, STATUS )
          CALL HSI_PTXT( GFID(1), NHBUF, HBUF, STATUS )

*      Grid parameters
          TEXT = 'Gridded parameters ('
          TLEN = CHR_LEN(TEXT)
          DO J = 1, NGRIDAX
            CALL MSG_SETI( 'P', GPS(J) )
            CALL MSG_MAKE( TEXT(:TLEN)//' ^P', TEXT, TLEN )
          END DO
          CALL HSI_PTXT( GFID(1), 1, TEXT(:TLEN)//' )', STATUS )

        ELSE
          CALL HSI_COPY( GFID(1), GFID(I), STATUS )
        END IF

*    The item being gridded
        IF ( GPARS(I) .EQ. 0 ) THEN
          CALL FIT_STATTOK( FSTAT, 'STAT', STATUS )
          CALL MSG_MAKE( 'Gridded values are ^STAT', TEXT, TLEN )
        ELSE IF ( GPARS(I) .EQ. PROB_REPLY ) THEN
          CALL MSG_MAKE( 'Gridded values are fit probability',
     :                   TEXT, TLEN )
        ELSE
          CALL MSG_SETI( 'P', GPARS(I) )
          CALL MSG_SETC( 'TAG', MODEL.PARNAME(GPARS(I)) )
          CALL MSG_MAKE( 'Gridded values are parameter ^P, ^TAG',
     :                                               TEXT, TLEN )
        END IF
        CALL HSI_PTXT( GFID(I), 1, TEXT(:TLEN), STATUS )

*    Create and map data array
        CALL BDI_MAPR( GFID(I), 'Data', 'WRITE', GDPTR(I), STATUS )

      END DO

*  Map grid quality array
      CALL DYN_MAPB( NGRIDAX, GDIMS, GQPTR, STATUS )

*  Set iteration limits
      IF ( OPTIMISING ) THEN
        CALL USI_DEF0I( 'MAX', 30, STATUS )
        CALL USI_GET0I( 'MAX', NITMAX, STATUS )
        CALL USI_GET0R( 'MINS', MINSLO, STATUS )
        CALL MSG_PRNT( 'There are free non-grid parameters - '/
     :                          /' optimising at each grid point.' )
      ELSE
        CALL MSG_PRNT( 'No free non-grid parameters - simple model'/
     :                               /' evaluation at each point.' )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Evaluate grid
      CALL FIT_GRID( NDS, OBDAT, INSTR, MODEL, OPCHAN, NGRIDAX, GAX,
     :               NGRID, GPARS, NITMAX, NPAR, LB, UB, FROZEN, SSCALE,
     :               MINSLO, FSTAT, FIT_PREDDAT, PREDDAT, PARAM,
     :               STATMIN, GDPTR, %VAL(GQPTR), GQMASK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Report minimum & write to history
      CALL FIT_STATTOK( FSTAT, 'STAT', STATUS )
      CALL MSG_SETR( 'VAL', REAL(STATMIN) )
      CALL MSG_MAKE( 'The minimum value of ^STAT in the grid'/
     :                              /' is ^VAL.', TEXT, TLEN )
      CALL MSG_PRNT( TEXT(:TLEN) )
      DO I = 1, NGRID
        CALL HSI_PTXT( GFID(I), 1, TEXT(:TLEN), STATUS )
      END DO

*  Update the model?
      CALL USI_GET0L( 'UP', UP, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( UP ) THEN
        CALL MSG_PRNT( '** Updating model spec - do not exit'/
     :                                /' until completed **' )
        CALL FIT_MODUP( MFID, MODEL.NCOMP, NPAR, PARAM, LE, UE,
     :                                          -99.0, STATUS )
        CALL MSG_PRNT( '** Model dataset updated **' )
      END IF

*  Did user grid the statistic ?
      DO I = 1, NGRID
        IF ( GPARS(I) .EQ. 0 ) THEN

*      Subtract minimum from statistic grid?
          CALL USI_GET0L( 'SUBSTAT', SUBSTAT, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Do subtraction
          IF ( SUBSTAT ) THEN
            CALL SGRID_SUBSTAT( GNELM, %VAL(GDPTR(I)), %VAL(GQPTR),
     :                              GQMASK, REAL(STATMIN), STATUS )
          END IF

        END IF
      END DO

*  Any bad points in quality array?
      CALL SGRID_ANYBAD( GNELM, %VAL(GQPTR), ANYBAD, ANYBADISH,
     :                                                 STATUS )
      IF ( ANYBADISH ) THEN
        CALL MSG_PRNT( '! WARNING : There are points on the grid'/
     :                      /' where a minimum was not achieved' )
        CALL MSG_PRNT( '            due to insufficient iterations' )
      END IF

*  If bad quality points, write a copy of quality to each output file
      IF ( ANYBAD .OR. ANYBADISH ) THEN

*    Write quality to each grid
        DO I = 1, NGRID
          CALL BDI_PUTUB( GFID(I), 'Quality', NGRIDAX, GDIMS,
     :                    %VAL(GQPTR), STATUS )
          CALL BDI_PUT0UB( GFID(I), 'QualityMask', GQMASK, STATUS )
        END DO

      END IF

*  Close o/p grid files
      DO I = 1, NGRID
        IF ( AUTO ) THEN
          CALL ADI_FCLOSE( GFID(I), STATUS )
        ELSE
          WRITE( PC, '(I1.1)' ) I
          CALL USI_ANNUL( 'OUT'//PC, STATUS )
        END IF
      END DO

*  Tidy up & exit
 99   CALL USI_ANNUL( 'MODEL', STATUS )
      CALL USI_ANNUL( 'INP', STATUS )
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  SGRID_ANYBAD - Any bad values in quality array, regardless of mask?
      SUBROUTINE SGRID_ANYBAD( N, QUAL, ANYBAD, ANYBADISH, STATUS )
*
*    Description :
*
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      9 Nov 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER                N                  ! # quality values
      BYTE                   QUAL(N)            ! Quality values
*
*    Export :
*
      LOGICAL                ANYBAD             ! Any MISSING quality values
      LOGICAL                ANYBADISH          ! Any IGNORE quality values
*
*    Local variables :
*
      INTEGER                I                  ! Loop over quality values
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      ANYBAD = .FALSE.
      ANYBADISH = .FALSE.

*    Try to find first non-good value
      DO I = 1, N
        IF ( QUAL(I) .EQ. QUAL__MISSING ) THEN
          ANYBAD = .TRUE.
          GOTO 89
        END IF
      END DO

 89   DO I = 1, N
        IF ( QUAL(I) .EQ. QUAL__IGNORE ) THEN
          ANYBADISH = .TRUE.
          GOTO 99
        END IF
      END DO

 99   CONTINUE

      END



*+  SGRID_SUBSTAT - Subtract minimum value of statistic from grid
      SUBROUTINE SGRID_SUBSTAT( N, STAT, QUAL, MASK, SMIN, STATUS )
*
*    Description :
*
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      9 Nov 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER                N                  ! # quality values
      BYTE                   QUAL(N)            ! Quality values
      BYTE                   MASK               ! Quality mask value
      REAL                   SMIN               ! Minimum value of statistic
*
*    Import / export :
*
      REAL                   STAT(N)            ! Quality values
*
*    Functions :
*
      BYTE		     BIT_ANDUB
*
*    Local variables :
*
      INTEGER                I                  ! Loop over quality values
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Try to find first non-good value
      DO I = 1, N
        IF ( BIT_ANDUB(QUAL(I),MASK) .EQ. QUAL__GOOD )
     :                            STAT(I) = STAT(I) - SMIN
      END DO

      END
