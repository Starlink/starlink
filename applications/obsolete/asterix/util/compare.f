      SUBROUTINE COMPARE( STATUS )
*+
*  Name:
*     COMPARE

*  Purpose:
*     Chi-squared comparison of model array and data array

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL COMPARE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Calculates the chi-squared fit of a data array to a model array. The
*     resulting chi-squared value is listed, together with the normalization
*     factor which brings the model into closest agreement with the data.

*  Usage:
*     compare {parameter_usage}

*  Environment Parameters:
*     DATA = CHAR(read)
*      	 Dataset containing data values
*     MODEL = CHAR (read)
*        Dataset containing model values
*     POISSON = LOGICAL (read)
*        Poisson data errors to be assumed

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
*     Chi-squared is calculated as    Sum[(data-model)**2/variance] ,
*     where variance is the variance of the data, unless the model array also
*     has a VARIANCE component, in which case it is the sum of the model and
*     data variances. i.e the program can be used to compare two datasets for
*     compatibility.
*     If the data have no error component then either Poisson or unit errors
*     may be assumed.
*     The number of degrees of freedom is taken to be the number of data points
*     (i.e. model is assumed to be independent of data).
*     Bad quality data points are excluded from the statistic, but the model
*     array is assumed to be 100% good.

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
*     compare, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     28 Apr 1986 V0.6-0 (TJP):
*        Original version.
*     28 May 1986 V1.0-0 (TJP):
*        Option of unit errors
*     14 Dec 1988 V1.0-1 (DJA):
*        Asterix88 upgrade
*     18 Nov 1990 V1.3-0 (DJA):
*        Minor bug fix
*     20 Mar 1992 V1.6-0 (DJA):
*        Uses quality properly. Calculations done using variance rather than
*        error. Checks zero variance explicitly.
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*      4 Dec 1995 V2.0-0 (DJA):
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

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'COMPARE Version 2.1-0' )

*  Local Variables:
      REAL                   	FACTOR          ! Renormalization factor for model
      REAL                   	S               	! Chi-squared fit
      REAL                   	SNEW            ! Chi-squared for scaled model
      REAL                   	ZEQUIV          	! Equivalent normal z

      INTEGER			DFID			! Data file id
      INTEGER                	DPTR             	! Pointer to data array
      INTEGER                	DQPTR           	! Data quality
      INTEGER                	DVPTR           	! Data variance
      INTEGER			MFID			! Model file id
      INTEGER                	MPTR            	! Model data
      INTEGER                	MVPTR           	! Model variance
      INTEGER                	NDOF            ! # degrees of freedom for S
      INTEGER                	NELD            	! # data elements
      INTEGER                	NELM            	! # model elements
      INTEGER                	NZV             ! # points with zero variance

      LOGICAL                	MOD_ERR         	! Model errors present
      LOGICAL                	OK              	! Data item acceptable
      LOGICAL                	QUAL            	! Data quality present
      LOGICAL                	POISS           	! Assume Poisson errors
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get the data
      CALL USI_ASSOC( 'DATA', 'BinDS|Array', 'READ', DFID, STATUS )
      CALL BDI_CHK( DFID, 'Data', OK, STATUS )
      CALL BDI_GETNEL( DFID, NELD, STATUS )
      IF ( .NOT. OK ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP( ' ', 'Numeric data required.', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get the model
      CALL USI_ASSOC( 'MODEL', 'BinDS|Array', 'READ', MFID, STATUS )
      CALL BDI_CHK( MFID, 'Data', OK, STATUS )
      CALL BDI_GETNEL( DFID, NELM, STATUS )
      IF ( .NOT. OK ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP( ' ', 'Numeric data required.', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check sizes compatible
      IF ( NELD .NE. NELM ) THEN
	STATUS = SAI__ERROR
	CALL ERR_REP( ' ','Data and model arrays are of'
     :                     //' different size', STATUS )
	GOTO 99
      END IF

*  Model
      CALL BDI_MAPR( MFID, 'Data', 'READ', MPTR, STATUS )
      CALL BDI_CHK( MFID, 'Variance', MOD_ERR, STATUS )
      IF ( MOD_ERR ) THEN
        CALL BDI_MAPR( MFID, 'Variance', 'READ', MVPTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Data
      CALL BDI_MAPR( DFID, 'Data', 'READ', DPTR, STATUS )
      CALL BDI_CHK( DFID, 'Variance', OK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      IF ( OK ) THEN
        CALL BDI_MAPR( DFID, 'Variance', 'READ', DVPTR, STATUS )

      ELSE

*    No existing errors - options are Poisson errors or unit errors
	CALL USI_GET0L( 'POISSON', POISS, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

        CALL DYN_MAPR( 1, NELM, DVPTR, STATUS )

	IF ( POISS ) THEN
          CALL ARR_COP1R( NELM, %VAL(DPTR), %VAL(DVPTR), STATUS )
	ELSE
          CALL ARR_INIT1R( 1.0, NELM, %VAL(DVPTR), STATUS )
          CALL MSG_PRNT( 'Assuming unit data errors' )
	END IF

      END IF

*  Data quality present?
      CALL BDI_CHK( DFID, 'Quality', QUAL, STATUS )
      IF ( QUAL ) THEN
        CALL BDI_MAPL( DFID, 'LogicalQuality', 'READ', DQPTR, STATUS )
      END IF

*  Calculate chi-squared fit, renormalization factor, and improved fit
      CALL COMPARE_CHIFIT( NELM, %VAL(DPTR), %VAL(DVPTR), QUAL,
     :           %VAL(DQPTR), %VAL(MPTR), MOD_ERR, %VAL(MVPTR),
     :                     S, NDOF, FACTOR, SNEW, NZV, STATUS )

*  Find equivalent normal z
      ZEQUIV = SQRT(2*S)-SQRT(2.0*NDOF-1)

*  Output results to terminal
      CALL MSG_PRNT( ' ' )
      CALL MSG_SETI( 'NDOF', NDOF )
      CALL MSG_PRNT( '^NDOF good data values ( = d.o.f )' )
      IF ( NZV .GT. 0 ) THEN
        CALL MSG_SETI( 'N', NZV )
        CALL MSG_PRNT( '^N points ignored due to zero data variance' )
      END IF
      CALL MSG_SETR( 'S', S )
      CALL MSG_PRNT( 'Chi-squared fit to model : ^S' )
      CALL MSG_SETR( 'ZEQUIV', ZEQUIV )
      CALL MSG_PRNT( 'Equivalent normal z : ^ZEQUIV' )
      CALL MSG_SETR( 'FAC', FACTOR )
      CALL MSG_PRNT( 'Model is optimised by scaling factor ^FAC' )
      CALL MSG_SETR( 'SNEW', SNEW )
      CALL MSG_PRNT( 'New chi-squared : ^SNEW' )

      ZEQUIV=SQRT(2*SNEW)-SQRT(2.0*NDOF-3)	! NDOF is reduced by 1
      CALL MSG_SETR( 'NEQZ', ZEQUIV )

      CALL MSG_PRNT( 'New equiv. normal z : ^NEQZ' )

*  Tidy up and exit
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  COMPARE_CHIFIT - Compare and model and data
      SUBROUTINE COMPARE_CHIFIT( N, DATA, DVAR, QFLAG, QUAL,
     :                           MODEL, ERRFLAG, MVAR, CHISQ,
     :                           NDOF, FACTOR, NCHISQ, NZV, STATUS )
*    Description :
*
*     Calculates chi-squared fit of MODEL and DATA arrays, also the
*     renormalization factor required to optimize the model fit, and
*     the reduced chi-squared obtained with this scaling of the model.
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
*     29 Apr 86 : Original (?)
*     20 Mar 92 : Tidied (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER           N                      ! Number of data points
      REAL              DATA(N)                ! Data values
      REAL              DVAR(N)                ! Data variance
      LOGICAL           QFLAG                  ! Data quality present?
      LOGICAL           QUAL(N)                ! Data quality
      REAL              MODEL(N)               ! Model values
      LOGICAL           ERRFLAG                ! Model errors present?
      REAL              MVAR(N)                ! Model variances
*
*    Export :
*
      INTEGER           NDOF                   ! Degrees of freedom
      REAL              CHISQ                  ! Chi-squared
      REAL              FACTOR                 ! Normalisation factor
      REAL              NCHISQ                 ! Chi-squared after norm'n
      INTEGER           NZV                    ! # points with 0 variance
*
*    Status :
*
      INTEGER           STATUS
*
*    Local variables :
*
      DOUBLE PRECISION  SSUM                   ! Chi-squared accumlator
      DOUBLE PRECISION  MDSUM,MMSUM            !

      REAL              VAR                    ! Combined data & model variance

      INTEGER           I                      ! Loop over data
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise sums
      SSUM = 0.0D0
      MDSUM = 0.0D0
      MMSUM = 0.0D0
      NDOF = N
      NZV = 0

*    Summation - Model errors and data quality present
      IF ( ERRFLAG .AND. QFLAG ) THEN
	DO I=1,N
	  IF ( QUAL(I) ) THEN
	    VAR = DVAR(I) + MVAR(I)
            IF ( VAR .GT. 0.0 ) THEN
	      MDSUM=MDSUM+DBLE(MODEL(I)*DATA(I)/VAR)
	      MMSUM=MMSUM+DBLE(MODEL(I)**2/VAR)
	      SSUM=SSUM+DBLE((DATA(I)-MODEL(I))**2/VAR)
            ELSE
              NDOF = NDOF - 1
              NZV = NZV + 1
            END IF
          ELSE
            NDOF = NDOF - 1
          END IF
	END DO

*    Quality but no model errors
      ELSE IF ( QFLAG ) THEN
	DO I = 1 , N
	  IF ( QUAL(I) ) THEN
            IF ( DVAR(I) .GT. 0.0 ) THEN
	      MDSUM=MDSUM+DBLE(MODEL(I)*DATA(I)/DVAR(I))
	      MMSUM=MMSUM+DBLE(MODEL(I)**2/DVAR(I))
	      SSUM=SSUM+DBLE((DATA(I)-MODEL(I))**2/DVAR(I))
            ELSE
              NDOF = NDOF - 1
              NZV = NZV + 1
            END IF
          ELSE
            NDOF = NDOF - 1
          END IF
	END DO

*    Model errors but no quality
      ELSE IF ( ERRFLAG ) THEN
	DO I=1,N
	  VAR = DVAR(I)+MVAR(I)
          IF ( VAR .GT. 0.0 ) THEN
	    MDSUM=MDSUM+DBLE(MODEL(I)*DATA(I)/VAR)
	    MMSUM=MMSUM+DBLE(MODEL(I)**2/VAR)
	    SSUM=SSUM+DBLE((DATA(I)-MODEL(I))**2/VAR)
          ELSE
            NDOF = NDOF - 1
            NZV = NZV + 1
          END IF
	END DO

* No quality or model errors
      ELSE
	DO I=1,N
          IF ( DVAR(I) .GT. 0.0 ) THEN
	    MDSUM=MDSUM+DBLE(MODEL(I)*DATA(I)/DVAR(I))
	    MMSUM=MMSUM+DBLE(MODEL(I)**2/DVAR(I))
	    SSUM=SSUM+DBLE((DATA(I)-MODEL(I))**2/DVAR(I))
          ELSE
            NDOF = NDOF - 1
            NZV = NZV + 1
          END IF
	END DO

      END IF

*    Calculate statistics from the sums
      CHISQ = REAL(SSUM)
      FACTOR = MDSUM/MMSUM
      NCHISQ = CHISQ - MMSUM*(FACTOR-1)**2

      END
