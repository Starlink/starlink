      SUBROUTINE AXFLIP( STATUS )
*+
*  Name:
*     AXFLIP

*  Purpose:
*     Reverses specified axes of a binned dataset

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL AXFLIP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  Usage:
*     axflip {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Name of the input dataset
*     SELAX = CHAR (read)
*        The selected axes to be flipped. Standard list notation
*     OUT = CHAR (read)
*        Name of the output dataset

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
*     axflip, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Dec 1989 V1.0-0 (DJA):
*        Original version
*      4 Jan 1990 V1.0-1 (DJA):
*        Bug where existing irregular widths were mapped incorrectly fixed
*     18 Jun 1990 V1.2-0 (DJA):
*        Bug with irregular axes fixed
*     10 Apr 1991 V1.4-0 (DJA):
*        Copes with primitives and datasets with no axes
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*      6 Dec 1995 V2.0-0 (DJA):
*        ADI port, and a bit of a rewrite too
*      5 Mar 1996 V2.0-1 (DJA):
*        Added facility to flip the Grouping array
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

*  External References:
      EXTERNAL                  CHR_LEN
        INTEGER                 CHR_LEN

*  Local Constants:
      INTEGER                   NAXOBJ
        PARAMETER               ( NAXOBJ = 4 )
      INTEGER                   NOBJ
        PARAMETER               ( NOBJ = 6 )
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'AXFLIP Version V2.0-1' )

*  Local Variables:
      CHARACTER*80            	HTXT			! History text
      CHARACTER*10              NSTR              	!
      CHARACTER*5		TYPE			! Mapping type

      INTEGER                   DIMS(ADI__MXDIM)  	! Input dimensions
      INTEGER                   HLEN              	! Length of history text
      INTEGER                   I, J                 	! Loop over dimensions
      INTEGER                   IPTR                    ! I/p data
      INTEGER			IFID			! Input dataset id
      INTEGER			IFILES			! Input file list
      INTEGER                   NDIGIT            	! Length of number in char
      INTEGER                   NDIM              	! Dimensionality
      INTEGER                   OPTR                    ! O/p data
      INTEGER			OFID			! Output dataset id
      INTEGER                   NSEL              	! Number of axes to flip
      INTEGER                   SELAX(ADI__MXDIM) 	! Axes to flip

      LOGICAL                   FLIP(ADI__MXDIM)  	! Flip these axes?
      LOGICAL                   FLOK            	! Carry on with flip?
      LOGICAL                   ISDS            	! Input is a dataset?
      LOGICAL                   OK                      ! Validity check

*  Local Data:
      CHARACTER*7               AXOBJ(NAXOBJ)
      CHARACTER*8               OBJ(NOBJ)
      DATA                      AXOBJ/'Data','Width',
     :                                'LoWidth','HiWidth'/
      DATA                      OBJ/'Data','Quality','Variance',
     :                              'LoError', 'HiError', 'Grouping'/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input object
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )
      CALL ADI_DERVD( IFID, 'BinDS', ISDS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check and map the data
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR : Invalid data', STATUS )
      ELSE IF ( NDIM .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Object is scalar - no axes to flip!',
     :                STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Select axes to flip if NDIM > 1
      IF ( NDIM .GT. 1 ) THEN

*    Tell user about different axes
        IF ( ISDS ) THEN
          CALL MSG_PRNT( 'Axes present in dataset are:' )
          CALL MSG_BLNK()
          CALL AXIS_TLIST( IFID, NDIM, STATUS )
        ELSE
          CALL MSG_BLNK()
          CALL MSG_SETI( 'N', NDIM )
          CALL MSG_PRNT( 'There are ^N dimensions in this primitive'/
     :                                                   /' object' )
        END IF
        CALL MSG_BLNK()

*    Select axes
        NSEL = 0
        CALL PRS_GETLIST( 'SELAX', NDIM, SELAX, NSEL, STATUS )
        IF ( ( STATUS .NE. SAI__OK ) .OR. ( NSEL .EQ. 0 ) ) GOTO 99

      ELSE
        NSEL = 1
        SELAX(1) = 1

      END IF

*  Set up the FLIP array
      CALL ARR_INIT1L( .FALSE., ADI__MXDIM, FLIP, STATUS )
      FLOK = .FALSE.
      IF ( ( NDIM .EQ. 1 ) .OR. ( NSEL .EQ. 1 ) ) THEN
        HTXT = 'Swapped axis : '
      ELSE
        HTXT = 'Swapped axes : '
      END IF
      HLEN = CHR_LEN(HTXT)+1
      DO I = 1, NSEL
        IF ( ( SELAX(I) .GT. 0 ) .AND. ( SELAX(I) .LE. NDIM ) ) THEN
          FLIP(SELAX(I)) = .TRUE.
          FLOK = .TRUE.
          CALL CHR_ITOC( SELAX(I), NSTR, NDIGIT )
          HTXT = HTXT(:HLEN)//NSTR(:NDIGIT)//' '
          HLEN = HLEN + NDIGIT + 1
        ELSE
          CALL MSG_SETI( 'N', SELAX(I) )
          CALL MSG_PRNT( 'No such axis number ^N' )
        END IF
      END DO

*  Output dataset
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      CALL BDI_LINK( 'BinDS', NDIM, DIMS, 'REAL', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Pad out dimensions for 7D
      CALL AR7_PAD( NDIM, DIMS, STATUS )

*  Copy axes
      DO I = 1, NDIM

*    Flipping this axis?
        IF ( FLIP(I) ) THEN

*      Loop over things which can be flipped
          DO J = 1, NAXOBJ

*        Does it exist?
            CALL BDI_AXCHK( IFID, I, AXOBJ(J), OK, STATUS )
            IF ( OK ) THEN

*          Map for read and write
              CALL BDI_AXMAPR( IFID, I, AXOBJ(J) , 'READ', IPTR,
     :                         STATUS )
              CALL BDI_AXMAPR( OFID, I, AXOBJ(J) , 'WRITE', OPTR,
     :                         STATUS )

*          Perform 1-D flip
              CALL ARR_FLIP1R( DIMS(I), %VAL(IPTR), %VAL(OPTR),
     :                         STATUS )

*          Release data
              CALL BDI_AXUNMAP( IFID, I, AXOBJ(J), IPTR, STATUS )
              CALL BDI_AXUNMAP( OFID, I, AXOBJ(J), OPTR, STATUS )

            END IF

*        Copy axis ancillaries
            CALL BDI_AXCOPY( IFID, I, 'Normalised,Label,Units',
     :                       OFID, I, STATUS )

*      Next axis object to flip
          END DO

*    Non-flipped axis
        ELSE

*        Simple axis copy
            CALL BDI_AXCOPY( IFID, I, ' ', OFID, I, STATUS )

        END IF

      END DO

*  Loop over things which can be flipped
      DO J = 1, NOBJ

*    Does it exist?
        CALL BDI_CHK( IFID, OBJ(J), OK, STATUS )
        IF ( OK ) THEN

*      Decide on type
          IF ( OBJ(J) .EQ. 'Quality' ) THEN
            TYPE = 'UBYTE'
          ELSE IF ( OBJ(J) .EQ. 'Grouping' ) THEN
            TYPE = 'INTEGER'
          ELSE
            TYPE = 'REAL'
          END IF

*      Map for read and write
          CALL BDI_MAP( IFID, OBJ(J) , TYPE, 'READ', IPTR, STATUS )
          CALL BDI_MAP( OFID, OBJ(J) , TYPE, 'WRITE', OPTR, STATUS )

*      Perform n-D flip
          IF ( TYPE .EQ. 'UBYTE' ) THEN
            CALL AR7_AXFLIP_B( DIMS, %VAL(IPTR), FLIP, %VAL(OPTR),
     :                         STATUS )
          ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
            CALL AR7_AXFLIP_I( DIMS, %VAL(IPTR), FLIP, %VAL(OPTR),
     :                         STATUS )
          ELSE
            CALL AR7_AXFLIP_R( DIMS, %VAL(IPTR), FLIP, %VAL(OPTR),
     :                         STATUS )
          END IF

*      Release data
          CALL BDI_UNMAP( IFID, OBJ(J), IPTR, STATUS )
          CALL BDI_UNMAP( OFID, OBJ(J), OPTR, STATUS )

        END IF

*  Next object to flip
      END DO

*  Copy other bits and bobs
      CALL BDI_COPY( IFID, 'Title,Label,Units,QualityMask', OFID,
     :               ' ', STATUS )

*  Copy ancillary stuff
      CALL UDI_COPANC( IFID, 'grf,grp', OFID, STATUS )

*  History
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )

*  Get input file spec
      CALL USI_NAMES( 'I', IFILES, STATUS )
      CALL HSI_PTXTI( OFID, IFILES, .TRUE., STATUS )
      CALL HSI_PTXT( OFID, 1, HTXT, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
