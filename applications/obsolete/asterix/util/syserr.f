      SUBROUTINE SYSERR( STATUS )
*+
*  Name:
*     SYSERR

*  Purpose:
*     Adds constant fractional term to variance component

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL SYSERR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     A constant fractional systematic error is added to the variance component
*     of a binned dataset.

*  Usage:
*     syserr {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Dataset to be updated
*     ERR = REAL (read)
*        Percentage error to be added
*     OVER = LOGICAL (read)
*        Overwriting to take place ( default is N )

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
*     syserr, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      2 Apr 1985 V0.5-0 (TJP):
*        Original version.
*     25 Sep 1986 V0.5-1 (JCMP):
*        ERRSUB renamed
*     21 Jul 1988 V1.0-0 (DJA):
*        Rewritten for new STARLINK definitions and ASTERIX88. Replacement of
*        DATA_ERROR with VARIANCE and appropriate changes in algorithm (see SYSERR_ERRSUB).
*      5 Oct 1988 V1.0-1 (DJA):
*        OVER parameter added to enable user not to overwrite.
*     12 Dec 1988 V1.0-1 (DJA):
*        USIs now used for user interface
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     20 Nov 1995 V2.0-0 (DJA):
*        Full ADI port
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      INTEGER                   MAXLIN
         PARAMETER              ( MAXLIN = 10 )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'SYSERR Version V2.0-0' )

*  Local Variables:
      CHARACTER*80 		ACTION(MAXLIN)		! History file entries

      INTEGER                   ACTLEN          	! Length of ACTION sting
      INTEGER 			DPTR			! Pointer to data array
      INTEGER		  	IFID			! Input dataset
      INTEGER			NELM			! # data/variance values
      INTEGER                   NREC            	! # of history records
      INTEGER		  	OFID			! Output dataset
      INTEGER                   USE             	! History records no.
      INTEGER 			VPTR			! Pointer to variance array

      REAL 			PERCERR			! Percentage systematic error
      REAL 			SERR			! Fractional systematic error

      LOGICAL			OK			! General test
      LOGICAL 			NEWVARA			! New VARIANCE created?
      LOGICAL                   OVER       		! Overwriting?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  See whether we're going to overwrite
      CALL USI_GET0L( 'OVER', OVER, STATUS )

*  Obtain data object, access and check it
      IF ( OVER ) THEN
        CALL USI_ASSOC( 'INP', 'BinDS', 'UPDATE', OFID, STATUS )
      ELSE
        CALL USI_ASSOC( 'INP', 'BinDS', 'READ', IFID, STATUS )
        CALL USI_CLONE( 'INP', 'OUT', 'BinDS', OFID, STATUS )
        CALL USI_SHOW( 'Output dataset {OUT}', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check input
      CALL BDI_CHK( OFID, 'Data', OK, STATUS )
      IF ( OK ) THEN
        CALL BDI_GETNEL( OFID, NELM, STATUS )
	CALL BDI_MAPR( OFID, 'Data', 'READ', DPTR, STATUS )
      ELSE
	STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'ERROR: No data object found.', STATUS )
        GOTO 99
      END IF

*  Obtain size of systematic error
      CALL USI_GET0R( 'ERROR', PERCERR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      SERR = 0.01 * PERCERR

*  See if VARIANCE is present. If it is check it and map it ; otherwise
*  create a new VARIANCE structure.
      CALL BDI_CHK( OFID, 'Variance', OK, STATUS )

      IF ( OK ) THEN
        CALL BDI_MAPR( OFID, 'Variance', 'UPDATE', VPTR, STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
        CALL BDI_MAPR( OFID, 'Variance', 'WRITE', VPTR, STATUS )
        CALL MSG_PRNT( 'No variance present - new array created' )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      NEWVARA = ( .NOT. OK )

*  Pass to subroutine to add in systematic errors
      CALL SYSERR_ERRSUB( SERR, NELM, NEWVARA, %VAL(DPTR), %VAL(VPTR),
     :                    STATUS )

*  History file entry
      CALL HSI_ADD( OFID, VERSION, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      CALL MSG_SETR( 'PCERR', PERCERR )
      IF ( .NOT. OVER ) THEN
        ACTION(1) = 'Input dataset {INP}'
        USE = 2
      ELSE
        USE = 1
      END IF
      CALL MSG_MAKE( '^PCERR percent systematic error added',
     :                                  ACTION(USE), ACTLEN )

      NREC = MAXLIN
      CALL USI_TEXT( USE, ACTION, NREC, STATUS )
      CALL HSI_PTXT( OFID, NREC, ACTION, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  SYSERR_ERRSUB - Adds errors to previous errors
      SUBROUTINE SYSERR_ERRSUB( SERR, NDAT, NEWVAR, DAT, VAR, STATUS )
*
*   Description :
*     If the variances are new then they are set to the square of the error.
*     Otherwise we add the square of the error (variances add algebraically).
*
*   Authors :
*     Trevor Ponman (BHVAD::TJP)
*
*   History :
*     -- --- -- : Original undocumented!
*     21 Jul 88 : Used to add systematic errors in quadrature. Now operates on
*                 VARIANCE (ie error squared) simplifying code a little (dja).
*
*   Type Definitions :
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
*   Import :
      INTEGER			NDAT		! No of variances

      LOGICAL			NEWVAR		! True if new variances

      REAL			SERR		! Fractional error to add
      REAL			DAT(NDAT)	! Data values
*
*   Export :
      REAL			VAR(NDAT)	! Result variances
*  Status:
      INTEGER			STATUS             	! Global status
*
*   Local variables :
      INTEGER			I		! Loop counter
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( NEWVAR ) THEN
	DO I=1,NDAT
	  VAR(I) = (SERR*DAT(I))**2
	END DO

      ELSE
	DO I=1,NDAT
	  VAR(I) = VAR(I) + ( SERR*DAT(I) )**2
	END DO

      END IF

      END
