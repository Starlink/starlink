      SUBROUTINE SDATA( STATUS )
*+
*  Name:
*     SDATA

*  Purpose:
*     Sets up file of data object names for fitting of multiple datasets

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL SDATA( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Accepts a sequence of up to NDSMAX dataset names from the user and writes
*     references to them into a reference data object. This is then assigned to
*     global parameter FIT_DATA, so as to be picked up automatically by
*     subsequent applications. If an object entered is a spectral set then the
*     user can enter a range of detectors to be selected from the set.

*  Usage:
*     sdata {parameter_usage}

*  Environment Parameters:
*     INP1..NDSMAX = CHAR (read)
*        Dataset name
*     DETNO = CHAR (read)
*        String specifying selected detector range
*     OUT = CHAR (read)
*        Dataset to contain references

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
*     sdata, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      1 Jun 1987 V0.6-1 (TJP):
*        Original version.
*      7 Jun 1989 V1.0-1 (TJP):
*        ASTERIX88 version - handles spectral sets
*     14 Nov 1989 V1.0-2 (TJP):
*        BDA_CLOSE on error
*     18 May 1990 V1.2-1 (TJP):
*        Use SPEC_SETSEARCH
*      1 Mar 1993 V1.7-0 (DJA):
*        Allows maximum number of files. SHOW mode added.
*        Error handling corrected
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     21 Nov 1995 V2.0-0 (DJA):
*        Full ADI port
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIT_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'SDATA Version 2.1-0' )

*  Local Variables:
      CHARACTER*132		FNAME 			! Referenced file name
      CHARACTER*2            	STRING			! String containing int

      REAL			RANGES(2*NDETMAX)	! Range values

      INTEGER        		DETSEL(NDSMAX)		! Detectors selected
      INTEGER        		DIMS(ADI__MXDIM)   	! Array dimensions
      INTEGER			FLEN			! Length of FNAME
      INTEGER			FSID			! FileSet object
      INTEGER        		I,J			! Loop indices
      INTEGER			IFID			! New input ref file
      INTEGER        		LSTRING			! Length of non-blank string
      INTEGER        		N			! Dataset index
      INTEGER        		NDIM			! I/p dimensionality
      INTEGER        		NFILE              	! # components in ref file
      INTEGER        		NRANGES			! # ranges entered
      INTEGER        		NSEL			! # detectors selected
      INTEGER			OFID			! Ref file
      INTEGER			RFID			! Referenced file
      INTEGER        		SETSIZE			! Size of spectral set

      LOGICAL        		OK			! Data OK?
      LOGICAL        		SET			! Spectral set?
      LOGICAL        		SHOW               	! Show mode?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Show mode?
      CALL USI_GET0L( 'SHOW', SHOW, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( SHOW ) THEN

*    Access old reference object
	CALL USI_ASSOC( 'OUT', 'FileSet', 'READ', OFID, STATUS )
	IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Loop over contents of reference file
        CALL ADI_CGET0I( OFID, 'NFILE', NFILE, STATUS )
        J = 0
	DO I = 1, NFILE

*      Open I'th file
          CALL FSI_FOPEN( OFID, I, 'BinDS', RFID, STATUS )

*      Spectral set?
	  CALL SPEC_SETSRCH( RFID, SET, STATUS )
          IF ( SET ) THEN

*        Get selected spectra, defaulting to the lot
            CALL FSI_GETSEL( OFID, I, NDETMAX, DETSEL, NSEL, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              CALL ERR_ANNUL( STATUS )
              NSEL = 0
            END IF

          END IF

*      Report the file
          CALL ADI_FOBNAM( RFID, FNAME, FLEN, STATUS )
          CALL MSG_PRNT( 'Dataset : '//FNAME(:FLEN) )

*      And detectors if a set
          IF ( SET .AND. (NSEL.GT.0) ) THEN
            CALL STR_DIMTOC( NSEL, DETSEL, FNAME )
            CALL MSG_SETC( 'DETS', FNAME )
          ELSE
            CALL MSG_SETC( 'DETS', 'All' )
          END IF
          CALL MSG_PRNT( '    Detectors : ^DETS' )

*      Close the file
          CALL ADI_FCLOSE( RFID, STATUS )

        END DO
	IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Create new file
      ELSE

*    Create reference object
        CALL ADI_NEW0( 'FileSet', FSID, STATUS )
        CALL USI_CREAT( 'OUT', FSID, OFID, STATUS )
	IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Enter references, terminated with null entry
	DO N = 1, NDSMAX

*      Construct parameter name
	  CALL USI_IASSOC( 'INP', N, 'BinDS', 'READ', IFID, STATUS )
          CALL FSI_PUTREF( OFID, N, IFID, STATUS )
	  IF ( STATUS .NE. SAI__OK ) GOTO 50

*      Check for spectral set
	  CALL SPEC_SETSRCH( IFID, SET, STATUS )
	  IF ( SET ) THEN

* Spectral set - allow selection of spectra from set

*    Get set size
            CALL BDI_CHK( IFID, 'Data', OK, STATUS )
            CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
	    IF(STATUS.NE.SAI__OK) GO TO 99
	    IF ( .NOT. OK ) THEN
	      STATUS = SAI__ERROR
	      CALL ERR_REP( 'BADDAT', 'Bad input dataset', STATUS )

	    ELSE IF ( NDIM .NE. 2 ) THEN
	      STATUS = SAI__ERROR
	      CALL ERR_REP( 'BADDIM', 'Spectral set has incorrect '//
     :        'dimensionality', STATUS )
	    END IF
	    IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Assumes 1st dimension is spectral
	    SETSIZE = DIMS(2)		!

*        Inform user
	    CALL MSG_SETI( 'NSPEC', SETSIZE )
	    CALL MSG_PRNT( 'Spectral set containing ^NSPEC spectra' )

*        Get required detector ranges
	    CALL CHR_ITOC( SETSIZE, STRING, LSTRING )
	    CALL USI_DEF0C( 'DETNO', '1:'//STRING(:LSTRING), STATUS )
	    CALL PRS_GETRANGES( 'DETNO', NDETMAX, 1, 1, SETSIZE, RANGES,
     :                          NRANGES, STATUS )
	    IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Convert to an array of selected detector numbers
	    IF ( NRANGES .GT. 0 ) THEN
	      NSEL = 0
	      DO I = 1, NRANGES
	        DO J = NINT(RANGES(2*I-1)), NINT(RANGES(2*I))
	          IF ( J .GT. SETSIZE ) THEN
	            STATUS = SAI__ERROR
	            CALL ERR_REP('BADNO','Spectrum number out of bounds',
     :              STATUS)
	            GOTO 99
	          END IF
	          NSEL = NSEL + 1
	          DETSEL(NSEL) = J
	        END DO
	      END DO

*          Write selection array to reference file
              CALL FSI_PUTSEL( OFID, N, NSEL, DETSEL, STATUS )
	      IF(STATUS.NE.SAI__OK) GOTO 99

	    END IF
	    CALL USI_CANCL('DETNO',STATUS)
	  END IF
	END DO

      END IF

*  Annul null status
 50   IF ( STATUS .EQ. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )
      END IF

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
