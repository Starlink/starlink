      SUBROUTINE BINSUBSET( STATUS )
*+
*  Name:
*     BINSUBSET

*  Purpose:
*     Subsets a binned dataset or array

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL BINSUBSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     A binned dataset is reduced according to user specified
*     ranges on its axis and/or data values.
*       If KEEP = .TRUE. then data .GE. lower axis bound and .LT. upper axis bound are included.
*        EXCEPTION - if upper axis bound = highest axis value; .LE. upper axis bound are included.
*       If KEEP = .FALSE. then data .LT. lower axis bound .OR. .GT. opper axis bound are included.

*  Usage:
*     binsubset {parameter_usage}

*  Environment Parameters:
*     {parameter_name}[pdims] = {parameter_type} ({parameter_access_mode})
*        {parameter_description}

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
*     Should update sort information

*  References:
*     {task_references}...

*  Keywords:
*     binsubset, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     JCMP: Jim Peden (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     29 Jun 1984 V0.4-0 (JCMP):
*        Original version
*     21 Nov 1985 V0.4-1 (JCMP)
*        Rewrite with more facilities
*     28 Nov 1985 V0.4-2 (JCMP)
*        Some minor mods
*     17 Dec 1985 V0.4-3 (JCMP)
*        ADAM version
*     26 Sep 1986 V0.5-1 (JCMP)
*        DTA__MXRANG changed
*      1 Oct 1986 V0.5-2 (JCMP)
*        Bug fix? now handles 2D datasets!
*     29 Oct 1987 V0.6-1 (ADM)
*        Improved use of HISTORY etc
*     24 Nov 1987 V0.6-2 (ADM):
*        Modified to ROSAT BINSUBSET specs.
*     28 Jun 1988 V1.0-1 (ADM):
*        ASTERIX88 version
*     25 Sep 1989 V1.0-2 (RJV):
*        Now handles decreasing axis values
*     10 Dec 1989 V1.0-3 (DJA):
*        Primitive now spelt correctly! No longer
*        prompts for axis numbers if 1D
*     19 Jan 1990 V1.0-4 (RJV):
*        Bug fix and efficiency improvements. Remove selection on data value
*        add slicing as an option
*      6 Jun 1990 V1.0-5 (RJV):
*        Option to select ranges by index number
*      4 Jul 1990 V1.2-0 (DJA):
*        Range selection now works on bin centres, and not bin bounds
*     27 Mar 1991 V1.4-0 (DJA):
*        Handles primitive inputs
*      7 Jun 1991 V1.5-1 (DJA):
*        History improved. Bug dealing with structured objects without axes fixed
*     20 Oct 1992 V1.7-0 (DJA):
*        Proofed against failure to map output
*     19 Nov 1992 V1.7-1 (DJA):
*        Changed arguments to AXIS_VAL2PIX
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     10 Dec 1995 V2.0-0 (DJA):
*        ADI port. Simplify logic by not treating regular axis arrays
*        explicitly (as they are expanded on output anyhow)
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
      INTEGER                   DTA__MXRANG        ! max no. permissible ranges
        PARAMETER               ( DTA__MXRANG = 20 )
      INTEGER                   MX__HTEXT
        PARAMETER               ( MX__HTEXT = ADI__MXDIM )
      INTEGER			NOBJ			!
        PARAMETER		( NOBJ = 5 )
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'BINSUBSET Version V2.0-0' )

*  Local Variables:
      CHARACTER*7 		AXID
      CHARACTER*80           	AXUNT(ADI__MXDIM)  	! Units for each axis
      CHARACTER*132          	HTEXT(MX__HTEXT)   	! History text
      CHARACTER*6		MTYPE			! Object mapping type
      CHARACTER*6            	PARNAM
      CHARACTER*40           	TEM                	! Dummy string
      CHARACTER*80           	TEXTI(4)           	! Input file spec

      REAL                   	AXLO(ADI__MXDIM)   	! Axis low
      REAL                   	AXHI(ADI__MXDIM)   	! Axis high
      REAL                   	DIR(ADI__MXDIM)		! axis direction indicator
      REAL                   	RANGES(2,DTA__MXRANG,ADI__MXDIM)  ! item ranges

      INTEGER                DIMS(ADI__MXDIM)   ! Input DATA_ARRAY dimensions
      INTEGER                HU                 ! History lines used
      INTEGER                	IDPTR              	! Pointer to input data
      INTEGER			IFID			! Input dataset id
      INTEGER			IOBJ			! Loop over subset objs
      INTEGER                IWPTR(ADI__MXDIM)  ! Pointer to input axis widths
      INTEGER                NDIM               ! Number of input dimensions
      INTEGER                NRANGE(ADI__MXDIM) ! # item ranges
      INTEGER                ODIMS(ADI__MXDIM)  ! Output DATA_ARRAY dimensions
      INTEGER                	ODPTR              	! Pointer to output data
      INTEGER                OWPTR(ADI__MXDIM)  ! Pointer to output axis widths
      INTEGER                ONDIM              ! Number of output dimensions
      INTEGER                PARENT(ADI__MXDIM) ! parent axis of output
      INTEGER                	TPTR               	! pointer to temp mapped array of logicals
      INTEGER                	I, J, K            	! Loop counters
      INTEGER                	NELM               	! Total length of input data
      INTEGER			OFID			! Output dataset id
      INTEGER                	INLINES            	! Number of TEXTI lines
      INTEGER                AXRANGE(2,DTA__MXRANG,ADI__MXDIM)
						! Pixel equivalent of RANGES
      INTEGER                IAXPTR(ADI__MXDIM) ! Pointers to input axes
      INTEGER                	NAX                	! Number of dataset axes
      INTEGER                NSEL,ISEL
      INTEGER                OAXPTR(ADI__MXDIM) ! Pointers to output axes
      INTEGER                SELAX(ADI__MXDIM)
      INTEGER                	TLEN               	! Text length

      LOGICAL                	INPRIM             	! Input is primitive?
      LOGICAL                	WIDOK(ADI__MXDIM)  	! Width component od axes ok?
      LOGICAL                	OK                 	! Object is ok
      LOGICAL                	KEEP(ADI__MXDIM)   	! Are ranges those to keep?
      LOGICAL                	KEEPDATA
      LOGICAL			PRIM			! Input is primitive?
      LOGICAL 		     	SLICE			!
      LOGICAL                	INDEX              	! Select by index?
      LOGICAL                	SEL(ADI__MXDIM)    	! Has axis been selected on?

*  Local Data:
      CHARACTER*10		OBJ(NOBJ)
      DATA			OBJ/'Data','Variance','Quality',
     :                              'LoError','HiError'/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

      HU = 0

*  Get file names
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get status and size of primary data
      CALL ADI_DERVD( IFID, 'Array', PRIM, STATUS )
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF (.NOT. OK) THEN
        CALL MSG_PRNT('AST_ERR: Invalid data')
        STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      IF ( INPRIM ) THEN
        CALL MSG_PRNT( 'Primitive input - use pixel numbers in ranges' )
        INDEX = .TRUE.
      END IF

*  Define axis ranges
      NELM = 1
      DO I = 1, ADI__MXDIM
        SEL(I) = .FALSE.
        IF ( I .GT. NDIM ) THEN
          DIMS(I) = 1
        END IF
        NRANGE(I) = 1
        AXRANGE(1,1,I) = 1
        AXRANGE(2,1,I) = DIMS(I)
        KEEP(I) = .TRUE.
        NELM = NELM * DIMS(I)
      END DO

*  See if selected ranges to be kept or discarded
      CALL USI_GET0L( 'KEEP', KEEPDATA, STATUS )

*  See if slicing required
      CALL USI_GET0L( 'SLICE', SLICE, STATUS )

*  See if selecting by index number
      IF ( .NOT. INPRIM ) CALL USI_GET0L('INDEX',INDEX,STATUS)

*  Display axis labels, and get min & max values
      CALL BINSUBSET_DISPAX( IFID, INPRIM, NDIM, DIMS, AXUNT, AXLO,
     :                                     AXHI, DIR, NAX, STATUS )
      INDEX = ( INDEX .OR. ( NAX .EQ. 0 ) )

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Choose axes to select on unless 1D
      IF ( NDIM .EQ. 1 ) THEN
        SELAX(1) = 1
        NSEL = 1
      ELSE
        NSEL=0
        CALL PRS_GETLIST( 'AXES', NDIM, SELAX, NSEL, STATUS )
      END IF
      AXID = '1234567'

*  go through selected axes
      DO ISEL=1,NSEL
        I=SELAX(ISEL)
        SEL(I) = .TRUE.
        KEEP(I)=KEEPDATA

*    Get required ranges for this axis
        PARNAM='AXIS'//AXID(I:I)
        CALL MSG_SETI( 'AX', I )

        IF ( INDEX ) THEN
          CALL CHR_ITOC( DIMS(I), TEM, TLEN )
          CALL MSG_PRNT( 'Axis ^AX has '//TEM(:TLEN)//' elements' )
          CALL USI_DEF0C( PARNAM, '1:'//TEM(:TLEN), STATUS )
          CALL PRS_GETRANGES (PARNAM, DTA__MXRANG, 1,1.0,
     :               REAL(DIMS(I)),RANGES(1,1,I),NRANGE(I), STATUS )
          IF ( RANGES(1,1,I) .LT. 1.0 ) THEN
            RANGES(1,1,I) = 1.0
          END IF
          IF (RANGES(2,NRANGE(I),I).GT.REAL(DIMS(I))) THEN
            RANGES(2,NRANGE(I),I)=REAL(DIMS(I))
          END IF

        ELSE
          CALL MSG_SETR( 'LO', AXLO(I) )
          CALL MSG_SETR( 'HI', AXHI(I) )
          CALL MSG_SETC( 'UNT', AXUNT(I) )
          CALL MSG_PRNT( 'The range for axis ^AX is ^LO to ^HI ^UNT')
          CALL MSG_SETR( 'LO', AXLO(I) )
          CALL MSG_SETR( 'HI', AXHI(I) )
          CALL MSG_MAKE( '^LO:^HI', TEM, TLEN )
          CALL USI_DEF0C( PARNAM, TEM(:TLEN), STATUS )
          CALL PRS_GETRANGES( PARNAM, DTA__MXRANG, 1, AXLO(I),
     :               AXHI(I),RANGES(1,1,I),NRANGE(I), STATUS )

*  if ranges given are outside limits then set equal to limits
          IF ((RANGES(1,1,I)-AXLO(I))*DIR(I).LT.0.0) THEN
            RANGES(1,1,I) = AXLO(I)
          END IF
          IF ((AXHI(I)-RANGES(2,NRANGE(I),I))*DIR(I).LT.0.0) THEN
            RANGES(2,NRANGE(I),I) = AXHI(I)
          END IF

        END IF

*      Write history text
        HU = HU + 1
        CALL MSG_SETI( 'AX', I )
        CALL MSG_MAKE( 'Axis ^AX subset', HTEXT(HU), TLEN )
        TLEN = TLEN + 1
        DO K = 1, NRANGE(I)
          CALL MSG_SETR( 'LO', RANGES(1,K,I) )
          CALL MSG_SETR( 'HI', RANGES(2,K,I) )
          CALL MSG_SETC( 'BIT', HTEXT(HU)(:TLEN) )
          CALL MSG_MAKE( '^BIT, ^LO:^HI', HTEXT(HU), TLEN )
        END DO
        IF ( INDEX ) THEN
          HTEXT(HU) = HTEXT(HU)(:TLEN)//' pixels'
        ELSE
          HTEXT(HU) = HTEXT(HU)(:TLEN)//' '//AXUNT(I)
        END IF

      END DO

*  Convert ranges into integer pixel ranges
      IF ( INDEX ) THEN
        DO I = 1,ADI__MXDIM
          IF (SEL(I)) THEN
            DO J=1,NRANGE(I)
              AXRANGE(1,J,I)=INT(RANGES(1,J,I))
              AXRANGE(2,J,I)=INT(RANGES(2,J,I))
            END DO
          END IF
        END DO
      ELSE
        CALL BINSUBSET_AXRAN( IFID,DIMS,NRANGE,RANGES,SEL,DIR,AXRANGE,
     :                                                         STATUS)
      END IF

*  Set up and map temp. logical array
      CALL DYN_MAPL( NDIM, DIMS, TPTR, STATUS )
      CALL ARR_INIT1L( (.NOT. KEEPDATA), NELM, %VAL(TPTR), STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Find out which elements are to be written to the output
      CALL BINSUBSET_SETSEL( DIMS(1),DIMS(2),DIMS(3),DIMS(4),
     :                       DIMS(5),DIMS(6),DIMS(7),NRANGE,AXRANGE,
     :                                     KEEPDATA,%VAL(TPTR),STATUS)

*  Find dimensions of output dataset
      ONDIM = 0
      DO I = 1, NDIM

*    Axis wasn't changed
        IF (.NOT.SEL(I)) THEN

          ONDIM=ONDIM+1
          ODIMS(ONDIM)=DIMS(I)
          PARENT(ONDIM)=I

*    Was changed
        ELSE

*      Case of axis reduced to one bin
          IF (NRANGE(I).EQ.1.AND.AXRANGE(2,1,I).EQ.AXRANGE(1,1,I)) THEN

*  if slicing skip this axis
            IF (SLICE) THEN
              CONTINUE
*  otherwise set output accordingly
            ELSE
              ONDIM=ONDIM+1
              ODIMS(ONDIM)=1
              PARENT(ONDIM)=I
            END IF

*  case of data being kept
          ELSEIF (KEEP(I)) THEN
            ONDIM        = ONDIM + 1
            ODIMS(ONDIM) = 0

*  scan through ranges incrementing size
            DO J = 1, NRANGE(I)
              DO K = AXRANGE(1,J,I), AXRANGE(2,J,I)
                ODIMS(ONDIM) = ODIMS(ONDIM) + 1
              END DO
            END DO
            PARENT(ONDIM) = I

*  case of data being discarded
          ELSE
            ONDIM        = ONDIM + 1
            ODIMS(ONDIM) = DIMS(I)

*  scan through ranges decrementing size
            DO J = 1, NRANGE(I)
              DO K = AXRANGE(1,J,I), AXRANGE(2,J,I)
                ODIMS(ONDIM) = ODIMS(ONDIM) - 1
              END DO
            END DO
            PARENT(ONDIM) = I
          END IF

        END IF

      END DO
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Case of total reduction to one bin
      IF (ONDIM .EQ. 0) THEN
        ONDIM    = 1
        ODIMS(1) = 1
        PARENT(1) = 1
      END IF

*  Create output file
      CALL BDI_LINK( 'BinDS', ONDIM, ODIMS, 'REAL', OFID, STATUS )

*  Copy other stuff
      CALL BDI_COPY( IFID, 'Title,Label,Units', OFID, ' ', STATUS )

*  Loop over objects which require subsetting
      DO IOBJ = 1, NOBJ

*    Does object exist
        CALL BDI_CHK( IFID, OBJ(I), OK, STATUS )
        IF ( OK ) THEN

*      Choose mapping type
          IF ( OBJ(I) .EQ. 'Quality' ) THEN
            MTYPE = 'UBYTE'
            CALL BDI_COPY( IFID, 'QualityMask', OFID, ' ', STATUS )
          ELSE
            MTYPE = 'REAL'
          END IF

*      Map the object
          CALL BDI_MAP( IFID, OBJ(I), MTYPE, 'READ', IDPTR, STATUS )
          CALL BDI_MAP( OFID, OBJ(I), MTYPE, 'WRITE', ODPTR, STATUS )

*      Copy from input to output
          IF ( MTYPE .EQ. 'REAL' ) THEN
            CALL ARR_CCOP1R( NELM, %VAL(IDPTR), %VAL(TPTR), %VAL(ODPTR),
     :                       STATUS )
          ELSE
            CALL ARR_CCOP1B( NELM, %VAL(IDPTR), %VAL(TPTR), %VAL(ODPTR),
     :                       STATUS )
          END IF

*      Unmap object
          CALL BDI_UNMAP( IFID, OBJ(I), IDPTR, STATUS )
          CALL BDI_UNMAP( OFID, OBJ(I), ODPTR, STATUS )

        END IF

      END DO

*  Finished with logical array
      CALL DYN_UNMAP( TPTR, STATUS )

*  Now deal with axes
      IF ( ( NAX .GT. 0 ) .AND. .NOT. INPRIM ) THEN

*    Loop over input axes
        DO I = 1, ONDIM

*      Get parent axis
          J = PARENT(I)

*      If unchanged then just copy
          IF ( .NOT. SEL(J) ) THEN

            CALL BDI_AXCOPY( IFID, J, ' ', OFID, I, STATUS )

          ELSE

*        See if width component needed
            CALL BDI_AXCHK( IFID, J, 'Width', WIDOK(J), STATUS )

*        Map axis values
            CALL BDI_AXMAPR( IFID, J, 'Data', 'READ', IAXPTR(J),
     :                       STATUS )
            CALL BDI_AXMAPR( OFID, I, 'Data', 'WRITE', OAXPTR(I),
     :                       STATUS )
            IF ( WIDOK(J) ) THEN
              CALL BDI_AXMAPR( IFID, J, 'Width', 'READ', IWPTR(J),
     :                         STATUS )
              CALL BDI_AXMAPR( OFID, I, 'Width', 'WRITE', OWPTR(I),
     :                         STATUS )
            END IF

*        Copy axis data and widths
            CALL BINSUBSET_AXCOP(DIMS(J),NRANGE(J),AXRANGE(1,1,J),
     :                         KEEP(J),%VAL(IAXPTR(J)),%VAL(IWPTR(J)),
     :                         WIDOK(J),%VAL(OAXPTR(I)), %VAL(OWPTR(I)),
     :                                                           STATUS)

*        Copy labels etc.
            CALL BDI_AXCOPY( IFID, J, 'Label,Units,Normalised',
     :                       OFID, I, STATUS )

          END IF

        END DO

      END IF

*  History component
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL USI_NAMEI( INLINES, TEXTI, STATUS )
      CALL HSI_PTXT( OFID, INLINES, TEXTI, STATUS )
      CALL HSI_PTXT( OFID, HU, HTEXT, STATUS )

*  Copy all ancilliary stuff
      CALL UDI_COPANC( IFID, 'grf', OFID, STATUS )

*  Clean up
 99   CALL AST_CLOSE()
      CALL AST_ERR(STATUS)

      END



*+  BINSUBSET_DISPAX - Display axes
      SUBROUTINE BINSUBSET_DISPAX( FID, PRIM, NDIM, DIMS, AXUNT, AXLO,
     :                                        AXHI, DIR, NAX, STATUS )
*
*    Description :
*
*     Returns the range of axis data in AXMIN,AXMAX (REAL); the axis
*     label in AXLAB; an the axis units in AXUN for the DATA_ARRAY
*     subclass object located by FID.
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER			FID			! Dataset identifier
      LOGICAL                PRIM               ! Input primitive?
      INTEGER			NDIM			! Dimensionality
      INTEGER                NDIM               ! Number of dimensions
      INTEGER                DIMS(*)            ! Length of each axis
*    Export :
      CHARACTER*80           AXUNT(*)           ! Units for each axis
      REAL                   AXLO(*)            ! low value  for each axis
      REAL                   AXHI(*)            ! high value for each axis
      REAL                   DIR(*)             ! axis direction indicator
      INTEGER			NAX
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*80 		AXLAB              	! axis labels

      INTEGER			AXPTR			! Ptr to axis values
      INTEGER                	I                  	! loop variable
*-

*  Status check
      IF (STATUS .NE. SAI__OK) RETURN

      NAX = 0

      IF ( .NOT. PRIM ) THEN
        CALL MSG_PRNT ('The axes present are:')

        DO I = 1, NDIM
          CALL BDI_CHK( FID, I, 'Data', OK, STATUS )
          IF ( OK ) THEN
            NAX=NAX+1
            CALL BDI_AXGET0C( FID, I, 'Label', AXLAB, STATUS )
            CALL BDI_AXGET0C( FID, I, 'Units', AXUNT(I), STATUS )
            CALL MSG_SETI( 'I', I )
            CALL MSG_SETC( 'NAME', AXLAB )
            CALL MSG_PRNT( ' ^I ^NAME' )
            CALL BDI_AXMAPR( FID, I, 'Data', 'READ', AXPTR, STATUS )
            CALL ARR_ELEM1R( AXPTR, DIMS(I), 1, AXLO(I), STATUS )
            CALL ARR_ELEM1R( AXPTR, DIMS(I), DIMS(I), AXHI(I), STATUS )

          ELSE
            AXUNIT(I) = 'pixels'
            AXLO(I) = 1.0
            AXHI(I) = DIMS(I)
          CALL MSG_SETI( 'NA', I )
          CALL MSG_SETI( 'NP', DIMS(I) )
          CALL MSG_PRNT( 'The range for axis ^NA is 1 to ^NP pixels' )

          END IF

*    set direction indicator for axis
          IF (AXHI(I).GT.AXLO(I)) THEN
            DIR(I)=1.0
          ELSEIF (AXHI(I).LT.AXLO(I)) THEN
            DIR(I)=-1.0
          ELSE
            DIR(I)=0.0
          END IF

        END DO
      ELSE

        CALL MSG_SETI( 'ND', NDIM )
        CALL MSG_PRNT( 'Object has ^ND dimensions' )
        DO I = 1, NDIM
          AXLO(I) = 1
          AXLO(I) = DIMS(I)
          DIR(I) = 1.0
          CALL MSG_SETI( 'NA', I )
          CALL MSG_SETI( 'NP', DIMS(I) )
          CALL MSG_PRNT( 'The range for axis ^NA is 1 to ^NP pixels' )
        END DO

      END IF

      IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'BINSUBSET_DISPAX', STATUS)
      END IF

      END




*+  BINSUBSET_AXRAN - Converts selected axis range to pixel values
      SUBROUTINE BINSUBSET_AXRAN(FID,DIMS,NRANGE,RANGES,SEL,DIR,
     :                                             AXRANGE,STATUS)
*    Description :
*     Converts the NRANGE RANGES of the selected axes into pixel values.
*    Type definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'

      INTEGER                DTA__MXRANG        ! max no. permissible ranges
        PARAMETER           (DTA__MXRANG = 20)
*    Import :
      INTEGER			FID			! Input dataset id
      INTEGER                NRANGE(*)          ! Nuber of ranges for each axis
      INTEGER                DIMS(*)            ! Length of each axis
      REAL                   DIR(*)		! direction indicator
      LOGICAL                SEL(*)             ! Axis selected on?
*    Import-Export :
      REAL                   RANGES(2,DTA__MXRANG,ADI__MXDIM)  ! AXIS ranges
*    Export :
      INTEGER                AXRANGE(2,DTA__MXRANG,ADI__MXDIM) ! AXIS pixel ranges
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER                IAX                ! Loop counters
      INTEGER APTR,WPTR

*-
      IF (STATUS.EQ.SAI__OK) THEN


*    Loop over all possible axes
        DO IAX = 1, ADI__MXDIM
          IF (SEL(IAX)) THEN
            CALL BDI_MAPAXVAL(FID,'R',IAX,APTR,STATUS)
            CALL BDI_MAPAXWID(FID,'R',IAX,WPTR,STATUS)
            CALL BINSUBSET_AXRAN_AXISN(%VAL(APTR),%VAL(WPTR),DIMS(IAX),
     :           DIR(IAX),NRANGE(IAX),RANGES(1,1,IAX),AXRANGE(1,1,IAX),
     :                                                          STATUS)
          END IF
        END DO

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT( 'BINSUBSET_AXRAN', STATUS )
        END IF

      END IF

      END




*+  BINSUBSET_AXRAN_AXISN
      SUBROUTINE BINSUBSET_AXRAN_AXISN (AXVAL,AXWID,DIM,DIR,
     :                           NRANGE,RANGES,AXRANGE,STATUS)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'

      INTEGER                DTA__MXRANG          ! max no. permissible ranges
        PARAMETER           (DTA__MXRANG = 20)
*    Import :
      INTEGER                NRANGE               ! Nuber of ranges for each axis
      INTEGER                DIM                  ! Length of each axis

      REAL                   AXVAL(*)	          ! Axis values
      REAL                   AXWID(*)		  ! Axis widths
      REAL                   DIR
*    Import-Export :
      REAL                   RANGES(2,DTA__MXRANG)  ! AXIS ranges
*    Export :
      INTEGER                AXRANGE(2,DTA__MXRANG) ! AXIS pixel ranges
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER JR
*-

      IF (STATUS.EQ.SAI__OK) THEN

*      Range selection should work on bin centres
        DO JR = 1,NRANGE
          CALL AXIS_VAL2PIX( DIM, AXVAL, .FALSE., RANGES(1,JR),
     :              RANGES(2,JR), AXRANGE(1,JR), AXRANGE(2,JR),
     :                                                 STATUS )
        END DO

c        IDIM=1
c
c*  loop over selected ranges matching to axis values
c        DO JR = 1, NRANGE
c
c*  match lower bound
c          MATCHLO=.FALSE.
c          DO WHILE (.NOT.MATCHLO.AND.IDIM.LE.DIM)
c*  calculate upper bin boundary
c            UPBOUND=AXVAL(IDIM)+0.5*DIR*AXWID(IDIM)
c*  calculate offset from this
c            OFFSET=(UPBOUND-RANGES(1,JR))*DIR
c*  match found
c            IF (OFFSET.LE.AXWID(IDIM).AND.OFFSET.GT.0.0) THEN
c              AXRANGE(1,JR)=IDIM
c              MATCHLO=.TRUE.
c
c*  match upper bound
c              MATCHUP=.FALSE.
c              JDIM=IDIM
c              DO WHILE (.NOT.MATCHUP.AND.JDIM.LE.DIM)
c                UPBOUND=AXVAL(JDIM)+0.5*DIR*AXWID(JDIM)
c                OFFSET=(UPBOUND-RANGES(2,JR))*DIR
c*  match found
c                IF (OFFSET.LE.AXWID(JDIM).AND.OFFSET.GT.0.0) THEN
c                  AXRANGE(2,JR)=JDIM
c                  MATCHUP=.TRUE.
c* no match found
c                ELSE
c                  JDIM=JDIM+1
c                END IF
c              END DO
c
c* no match yet for lower bound
c            ELSE
c              IDIM=IDIM+1
c            END IF
c
c          END DO
c
c          IDIM=IDIM+1
c
c        END DO

c* trap case of final range value = upper bin boundary
c        IF (MATCHLO.AND..NOT.MATCHUP) THEN
c          AXRANGE(2,NRANGE)=DIM
c        END IF

      END IF
      END





*+  BINSUBSET_SETSEL - Set values to copy to output
      SUBROUTINE BINSUBSET_SETSEL (D1,D2,D3,D4,D5,D6,D7,NRANGE,AXRANGE,
     :                                               KEEP,COPY,STATUS)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INTEGER                DTA__MXRANG        ! max no. permissible ranges
        PARAMETER           (DTA__MXRANG = 20)
*    Import :
      INTEGER D1,D2,D3,D4,D5,D6,D7
      INTEGER                NRANGE(ADI__MXDIM)          ! # item ranges
      INTEGER                AXRANGE(2,DTA__MXRANG,ADI__MXDIM)


      LOGICAL                KEEP               ! Keep axis ranges selected?
*    Export :
      LOGICAL                COPY(D1,D2,D3,D4,D5,D6,D7)
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER                A,B,C,D,E,F,G,H,I,J,K,L,M,N !Loop counters

*-
      IF (STATUS.EQ.SAI__OK) THEN
        DO A = 1, NRANGE(7)
          DO B = AXRANGE(1,A,7), AXRANGE(2,A,7)
            DO C = 1, NRANGE(6)
              DO D = AXRANGE(1,C,6), AXRANGE(2,C,6)
                DO E = 1, NRANGE(5)
                  DO F = AXRANGE(1,E,5), AXRANGE(2,E,5)
                    DO G = 1, NRANGE(4)
                      DO H = AXRANGE(1,G,4), AXRANGE(2,G,4)
                        DO I = 1, NRANGE(3)
                          DO J = AXRANGE(1,I,3), AXRANGE(2,I,3)
                            DO K = 1, NRANGE(2)
                              DO L = AXRANGE(1,K,2), AXRANGE(2,K,2)
                                DO M = 1, NRANGE(1)
                                  DO N = AXRANGE(1,M,1), AXRANGE(2,M,1)

                                    COPY(N,L,J,H,F,D,B)= KEEP

                                  END DO
                                END DO
                              END DO
                            END DO
                          END DO
                        END DO
                      END DO
                    END DO
                  END DO
                END DO
              END DO
            END DO
          END DO
        END DO

      END IF
      END



*+  BINSUBSET_AXCOP - Write output axis values & widths
      SUBROUTINE BINSUBSET_AXCOP (NPTS, NR, R, KEEP, IAXIS, IWIDTH,
     :                                 WDCOP, OAXIS, OWIDTH, STATUS)
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER                NPTS               ! Length of input axis
      INTEGER                NR                 ! Number of ranges
      INTEGER                R(2,NR)            ! Axis pixel values of ranges

      LOGICAL                KEEP               ! Are ranges those to keep?
      LOGICAL                WDCOP              ! Copy width data?

      REAL                   IAXIS(*)           ! Input axis
      REAL                   IWIDTH(*)          ! Input width

*    Export :
      REAL                   OAXIS(*)           ! Output axis
      REAL                   OWIDTH(*)          ! Output width
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER                I, J, K            ! Counters
*-
      IF (STATUS.NE.SAI__OK) RETURN

* data to be kept
      IF (KEEP) THEN
        K = 1
        DO I = 1, NR
          DO J = R(1,I), R(2,I)
            OAXIS(K)=IAXIS(J)
            IF (WDCOP) THEN
              OWIDTH(K) = IWIDTH(J)
            END IF

            K= K + 1

          END DO
        END DO

* data to be discarded
      ELSE
        K=1
        J = 1
        DO I = 1, NPTS
          IF (J .LE. NR) THEN
            IF (I .LT. R(1,J)) THEN
              OAXIS(K)  = IAXIS(I)
              IF (WDCOP) THEN
                OWIDTH(K) = IWIDTH(I)
              END IF
              K = K + 1

            ELSE IF (I .EQ. R(2,J)) THEN
              J  = J + 1

            END IF
          ELSE
            OAXIS(K)  = IAXIS(I)
            IF (WDCOP) THEN
              OWIDTH(K) = IWIDTH(I)
            END IF
            K = K + 1

          END IF
        END DO

      END IF

      END
