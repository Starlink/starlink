*+  UFIT_CHISQ - Evaluates chi-squared fit between predicted and observed data
	SUBROUTINE UFIT_CHISQ(PARAM,CHISQ,STATUS)
*    Description :
*     Returns chi-squared fit between the model whose parameters are passed in
*     the array PARAM and data values which must be passed in (along with
*     their errors) in COMMON.
*
*     THIS ROUTINE IS JUST A DUMMY, TO BE REPLACED BY A USER-GENERATED ROUTINE
*     TO COMPUTE AND FIT THE REQUIRED MODEL.
*
*    Method :
*     Statistic calculated is
*            CHISQ= Sum over all data [ (DATA-PREDICTION)**2/VARIANCE ].
*     The sum is best accumulated in double precision.
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*
*    History :
*
*     28 Apr 88 : Original (BHVAD::TJP)
*      1 Mar 94 : Statistic now D.P. (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      REAL 			PARAM(*)		! Model parameters
*
*    Export :
*
      DOUBLE PRECISION 		CHISQ			! Chi-squared
*
*    Status :
*
      INTEGER STATUS
*
*    Local constant :
*
      INTEGER NPAMAX			! Maximum number of model parameters
	PARAMETER(NPAMAX=60)
*
*    Local variables :
*    Global variables :
*    Internal References :
*    Local data :
*-

*    Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

      PRINT *,'No user supplied UFIT_CHISQ routine available.'
      PRINT *,'See file UFIT.INFO for information on the use of '//
     :  'this package.'

*    Exit
 9000 IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( 'EXERR', 'from UFIT_CHISQ', STATUS )
      END IF

      END
