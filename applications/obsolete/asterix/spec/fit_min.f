      SUBROUTINE FIT_MIN( NDS, IMOD, MCTRL, OPCHAN,
     :                    PRGRES, NPAR, LB, UB, FROZEN, SSCALE,
     :                    LNDFAC, FSTAT, PREDICTOR, PARAM, DPAR,
     :                    PEGGED, STAT, FINISHED, FITERR, STATUS )
*+
*  Name:
*     FIT_MIN

*  Purpose:
*     Minimise a statistic with respect to model parameters

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FIT_MIN( NDS, OBDAT, INSTR, MODEL, MCTRL, OPCHAN, PRGRES, NPAR,
*                   LB, UB, FROZEN, SSCALE, FSTAT, PREDICTOR, PREDDAT,
*                   PARAM, DPAR,PEGGED, STAT, FINISHED, FITERR, STATUS )

*  Description:
*     Chooses one of a number of minimisation algorithms given the
*     algorithm described by the MCTRL object.

*  Arguments:
*     NDS = INTEGER (given)
*        Number of datasets to fit
*     OBDAT[NDS] = /DATASET/ (given)
*        Datasets to fit
*     INSTR[NDS] = /INSTR_RESP/ (given)
*        Responses
*     MODEL = /MODEL/ (given)
*        Model specification
*     MCTRL = INTEGER (given and returned)
*        ADI identifier of minimisation control object. MCTRL is not
*        altered but its ADI data may be.
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
*     PREDICTOR = EXTERNAL (given)
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
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     FIT Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fit.html

*  Keywords:
*     package:fit, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     17 Apr 1996 (DJA):
*        Original version. Old FIT_MIN became FIT_MIN1
*     28 May 1998 (ELD):
*        FIT_MIN4 called based on style; LNDFAC added
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
      INTEGER			NDS, OPCHAN, MCTRL, NPAR, SSCALE, FSTAT
c     RECORD /DATASET/    	OBDAT(NDS)
c     RECORD /INSTR_RESP/ 	INSTR(NDS)
c     RECORD /MODEL_SPEC/ 	MODEL
      INTEGER			IMOD
      REAL			LB(*), UB(*)
      LOGICAL			PRGRES, FROZEN(*)
      EXTERNAL			PREDICTOR
      DOUBLE PRECISION          LNDFAC

*  Arguments Given and Returned:
      LOGICAL			PEGGED(*)
c     RECORD /PREDICTION/	PREDDAT(NDS)
      REAL			PARAM(*), DPAR(*)
      DOUBLE PRECISION	  	STAT

*  Arguments Returned:
      LOGICAL 		  	FINISHED
      INTEGER 		  	FITERR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*6		STYLE			! Algorithm name
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on style of minimisation. This will be a method once all
*  structures are removed
      CALL ADI_CGET0C( MCTRL, 'Style', STYLE, STATUS )
      IF ( STYLE .EQ. 'CURFIT'  )THEN

*    CURFIT algorithm
	CALL FIT_MIN1( NDS, IMOD, MCTRL, OPCHAN,
     :                 PRGRES, NPAR, LB, UB, FROZEN, SSCALE, LNDFAC,
     :                 FSTAT, PREDICTOR, PARAM, DPAR,
     :                 PEGGED, STAT, FINISHED, FITERR, STATUS )

      ELSEIF ( STYLE .EQ. 'GENALG' )THEN

                 CALL FLUSH(6)

*    Genetic algorithm
        CALL FIT_MIN4( NDS, IMOD, PRGRES, NPAR,
     :          LB, UB, FROZEN, SSCALE, LNDFAC, FSTAT, PREDICTOR,
     :          PARAM, PEGGED, STAT, FINISHED, FITERR,
     :          MCTRL, STATUS )



*  Otherwise unknown
      ELSE
        CALL MSG_SETC( 'STYLE', STYLE )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unknown minimisation algorithm /^STYLE/',
     :                STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FIT_MIN', STATUS )

      END



      SUBROUTINE FIT_MIN4( NDS, IMOD, PRGRES, NPAR,
     :          LB, UB, FROZEN, SSCALE, LNDFAC, FSTAT, PREDICTOR,
     :          PARAM, PEGGED, STAT, FINISHED, FITERR,
     :          MCTRL, STATUS )


      INTEGER STATUS

      STATUS = 1

      END
