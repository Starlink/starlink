      SUBROUTINE SMOOTH( STATUS )
*+
*  Name:
*     SMOOTH

*  Purpose:
*     Smooth an N-dimensional data array

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL SMOOTH( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Smooth an n-dimensional data array with a choice of different
*     blurring functions, or a user supplied mask.

*  Usage:
*     smooth {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input dataset
*     OUT = CHAR (read)
*        Output dataset name
*     OVER = LOGICAL (read)
*        Overwrite input or create new output?
*     MSK_IDIM = INTEGER (read)
*        Data dimension to be done first
*     MSK_IDIM1 = INTEGER (read)
*        Data dimension to be done second (0 for no second)
*     MSK_MASK = CHAR (read)
*        Type of mask, TOP hat, LAPlacian, GAUssian, COSine bell. If none
*        of these then the mask name is interpreted as a filename in which
*        case the input should be a text file with ASCII format :
*            Line 1		IDLEN = length of template ( < 257; preferably odd)
*            Line 2 - (IDLEN+2)	template values
*     MSK_WIDTH = INTEGER (read)
*        Mask parameter (bins)
*     MSK_DO = CHAR (read)
*        GAPS if want gaps in data re-established at end
*     MSK_NEWUNITS = CHAR (read)
*        If MASK is filename then can change units of data (if use a
*        gradient mask then might change units to GRADIENT)

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
*     smooth, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RW: Dick Willingale (University of Leicester)
*     MD: Mike Denby (University of Leicester)
*     RDS: Richard Saxton (Starlink, University of Leicester)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      7 Jun 1985 : Original, son of LUE (RW)
*        Apr 87 : Compatible with new QCL (MD)
*        Apr 88 : Compatible with new QCL (MD)
*     10 Jun 1988 V1.0-0 (RDS):
*        ASTERIX88 version
*     22 Sep 1989 V1.0-4 (RDS):
*        Uses AXIS_ routines and the DATA(DIM1,DIM2) and DATA(DIM1,DIM2)
*        notation. Includes different methods for treating the ends
*      1 Aug 1991 V1.0-5 (RDS):
*        Bug with 2-d smooths fixed
*     23 Apr 1993 V1.6-1 (RDS):
*        User selectable length of gaussian width
*      6 Jul 1993 V1.7-0 (DJA):
*        Use FIO for file i/o, direct console i/o removed and header updated
*     13 Sep 1993 V1.7-1 (DJA):
*        Allow non-integer widths
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     26 Mar 1995 V1.8-1 (DJA):
*        Use new data interface
*      5 Sep 1995 V1.8-2 (DJA):
*        Don't upper case mask name
*      7 Dec 1995 V2.0-0 (DJA):
*        ADI port. Several ghastly bugs in quality handling fixed.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'MATH_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_SIMLR
        LOGICAL			CHR_SIMLR

*  Local Constants:
      INTEGER                   NMASK,NMAX
        PARAMETER               ( NMASK = 256 )
        PARAMETER               ( NMAX = 16384 )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'SMOOTH Version V2.0-0' )

*  Local Variables:
      CHARACTER*1             DO                ! Are gaps to be restored (Y/N)
      CHARACTER*10            EMETH		! How to treat the data ends
      CHARACTER*40	      LINE		! Line from mask file
      CHARACTER*40            MASK,DUNIT
      CHARACTER*80            PATH(8)           ! Input data path including
                                                ! Text string for history file

      REAL                    RMASK(NMASK)
      REAL                    RN
      REAL                    RR,XX
      REAL START_DATA,END_DATA                  ! Value to use at both ends
      REAL                    START_ERR,END_ERR ! ERROR to use at both ends
      REAL                    TT

      INTEGER			IFID			! Input dataset
      INTEGER			OFID			! Output dataset

      INTEGER                 DPNTR		! Pointer to output data array
      INTEGER                 QPNTR             ! Pointer to output quality
      INTEGER VPNTR                               !Pointer to output variance
      INTEGER EBUFP                               !Pointer to workspace
      INTEGER ABUFP                               !Pointer to workspace
      INTEGER QBUFP                               !Pointer to workspace
      INTEGER RESP                                !Pointer to workspace
      INTEGER ERESP                               !Pointer to workspace
      INTEGER ETEMPP                              !Pointer to workspace
      INTEGER NDIMS                               !Number of axes in data array
      INTEGER NELS(4)                             !No. of elements in each axis
      INTEGER NQDIMS                              !Number of axes in quality
      INTEGER NQELS(4)                            !No. of elements in each axis
      INTEGER NVDIMS                              !Number of axes in variance
      INTEGER NVELS(4)                            !No. of elements in each axis
      INTEGER IDIM                                !The axis to smooth
      INTEGER IDIM1                               !A second axis to smooth
      INTEGER IDO                                 !Fill gaps in ?
      INTEGER IDO1                                !Fill gaps in on 1st pass?
      REAL    WDTH                                !Width of mask in bins.
      INTEGER LL
      INTEGER                 LMASK		! Width of mask made odd.
      INTEGER NLINES
      INTEGER LM2
      INTEGER                 NLIM		! Dimension of temp arrays
      INTEGER IDUM
      INTEGER I,J
      INTEGER                 IDAT              ! FIO file number
      INTEGER START1,END1,START2,END2             !Pixel range to fine mean end
      INTEGER IDLEN,J1,J2

      LOGICAL                 JUMPOUT
      LOGICAL                 LDQUAL            ! Is quality array present?
      LOGICAL                 LDVAR             ! Is variance array present?
      LOGICAL  	              OK		! General validity test
      LOGICAL                 OVER              ! Overwrite input file?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Should input file be overwritten ?
      CALL USI_GET0L( 'OVER', OVER, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Get input and output data objects
      IF ( OVER ) THEN
        CALL USI_ASSOC( 'INP', 'BinDS|Array', 'UPDATE', IFID, STATUS )
        OFID = IFID

      ELSE
        CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )
        CALL USI_CLONE( 'INP', 'OUT', 'BinDS', OFID, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Trace path of input data.
      CALL USI_NAMEI( NLINES, PATH, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check data in file
      CALL BDI_CHK( OFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( OFID, 4, NELS, NDIMS, STATUS )
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Data array not found', STATUS )
        GOTO 99
      END IF

*  Map the output data array
      CALL BDI_MAPR( OFID, 'Data', 'UPDATE', DPNTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP(' ','Error mapping input data array',STATUS)
        GOTO 99
      END IF

*  Map the quality and variance if present
      CALL BDI_CHK( OFID, 'Quality', LDQUAL, STATUS )
      IF ( LDQUAL ) THEN
        CALL BDI_MAPL( OFID, 'LogicalQuality', 'READ', QPNTR, STATUS )
      END IF
      CALL BDI_CHK( OFID, 'Variance', LDVAR, STATUS )
      IF ( LDVAR ) THEN
        CALL BDI_MAPR( OFID, 'Variance', 'UPDATE', VPNTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP(' ','Error mapping input variance array', STATUS )
        GOTO 99
      END IF

*  List stuff about data
      IF (.NOT. LDQUAL) CALL MSG_PRNT('Quality array is not present')
      IF (.NOT. LDVAR)  CALL MSG_PRNT('Variance array is not present')

*  Get dimensions to be done
      IF (NDIMS .GT. 1) THEN

*    Display axis labels
        CALL MSG_PRNT( 'Axes are:' )
        CALL MSG_BLNK()
        CALL AXIS_TLIST(IFID,NDIMS,STATUS)
        IF (STATUS .NE. SAI__OK) GOTO 99

*    While no correct response
        JUMPOUT = .FALSE.
        DO WHILE (.NOT. JUMPOUT)

*      Get first dimension
          CALL USI_GET0I('MSK_IDIM',IDIM,STATUS)
          IF (STATUS .NE. SAI__OK) GOTO 99

	  IF ( IDIM.LE.0 .OR. IDIM.GT.NDIMS ) THEN
            CALL MSG_SETI(IDIM, IDIM)
	    CALL MSG_PRNT('Error: there is no dimension ^IDIM')
            CALL USI_CANCL('MSK_IDIM',STATUS)
          ELSE
            JUMPOUT = .TRUE.
	  END IF
        END DO
      ELSE
        IDIM = 1

      ENDIF

*  Obtain a second dimension to smooth over if that is possible
      IF ( NDIMS .GT. 1 ) THEN

        JUMPOUT=.FALSE.
        DO WHILE (.NOT. JUMPOUT)
          CALL USI_GET0I('MSK_IDIM1',IDIM1,STATUS)
          IF (STATUS .NE. SAI__OK) GOTO 99

	  IF (IDIM1.LT.0 .OR. IDIM1.GT.NDIMS) THEN
            CALL MSG_SETI(IDIM, IDIM)
	    CALL MSG_PRNT('Error: there is no dimension ^IDIM')
            CALL USI_CANCL('MSK_IDIM1',STATUS)
          ELSE
            JUMPOUT = .TRUE.
	  END IF

        END DO

      ELSE
        IDIM1=0

      END IF

*  Test data size
      IF ( NELS(IDIM) .GT. NMAX ) THEN
        CALL MSG_SETI(NMAX, NMAX)
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Error: too much data - the maximum'/
     :                                       /' is ^NMAX', STATUS )
        GOTO 99
      END IF
*
      IF ( (IDIM1.GT.0) .AND. (NELS(IDIM1).GT.NMAX) ) THEN
        CALL MSG_SETI(NMAX, NMAX)
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Error: too much data - the maximum'/
     :                                    /' is ^NMAX', STATUS )
        GOTO 99
      END IF

*  Ask if gaps should be re-instated
      CALL USI_GET0C( 'MSK_DO', DO, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99
      IF (DO .EQ. 'Y') THEN
        IF (.NOT.LDQUAL) THEN
	  CALL MSG_PRNT('   No quality array so no gap info.')
	  IDO=0
        ELSE
          IDO=1
	ENDIF
      ELSE
        IDO=0
      END IF

*  Loop around asking for mask type until user gets it right
      JUMPOUT = .FALSE.
      DO WHILE ( .NOT. JUMPOUT )

*    Display list of masks
        CALL MSG_PRNT(' *** Mask options *** ')
        CALL MSG_PRNT('   TOP hat function   ')
        CALL MSG_PRNT('   LAPlacian mask     ')
        CALL MSG_PRNT('   GAUssian  mask     ')
        CALL MSG_PRNT('   COSine bell        ')
        CALL MSG_PRNT('   MASk file          ')
        CALL USI_GET0C('MSK_MASK',MASK,STATUS)
        IF (STATUS .NE. SAI__OK) GOTO 99

*    Top-hat?
	IF ( CHR_SIMLR( MASK(1:3), 'TOP') ) THEN

*      Get mask width
          JUMPOUT=.FALSE.
          DO WHILE (.NOT. JUMPOUT)

*        Get users width
            CALL USI_GET0R('MSK_WIDTH',WDTH,STATUS)
            IF (STATUS .NE. SAI__OK) GOTO 99

*        Ok?
	    LL = WDTH/2.0
	    LMASK=(LL+1)*2
	    IF ( LMASK .GT. NMASK ) THEN
	      CALL MSG_PRNT( 'SMOOTH Error mask too big' )
              CALL USI_CANCL( 'MSK_WIDTH', STATUS )
            ELSE
              JUMPOUT = .TRUE.
	    END IF

          END DO

*      Set up the top hat
	  LM2 = LMASK/2
	  LL = MIN(LL,LM2-1)
          RN = 1./(LL*2+1)
	  DO J = 1, LMASK
	    RMASK(J) = 0.0
	    IF ( IABS(LM2-J) .LE. LL ) RMASK(J) = RN
          END DO

*    Laplacian mask?
        ELSE IF ( CHR_SIMLR( MASK(1:3),'LAP' ) ) THEN

*      Set up the mask
	  LM2 = 2
	  LMASK = 4
	  DO J = 1, LMASK
	    RMASK(J) = 0.0
	  END DO
	  RMASK(LM2) = 1.
	  RMASK(LM2-1) = -0.5
	  RMASK(LM2+1) = -0.5

*      Change units
          CALL BDI_PUT0C( OFID, 'Units', 'Curvature', STATUS )
          JUMPOUT = .TRUE.

*    Gaussian mask?
	ELSE IF ( CHR_SIMLR( MASK(1:3), 'GAU' ) ) THEN

*      Get mask width
          JUMPOUT = .FALSE.
          DO WHILE ( .NOT. JUMPOUT )

*      Get user width
            CALL USI_GET0R( 'MSK_WIDTH', WDTH, STATUS )
            IF (STATUS .NE. SAI__OK) GOTO 99

*      Width ok?
	    IF ( WDTH .GT. 25.0 ) THEN
	      CALL MSG_PRNT('SMOOTH Error: WIDTH too large '/
     :                           /'must be less than 25')
              CALL USI_CANCL('MSK_WIDTH',STATUS)
            ELSE
              JUMPOUT=.TRUE.
	    END IF
          END DO

*      Ask user for mask length. Set the default to be 4 times the
*      standard deviation.
	  LMASK = 4.0*WDTH
          CALL USI_DEF0I( 'MSK_LGAU', LMASK, STATUS )
          CALL USI_GET0I( 'MSK_LGAU', LMASK, STATUS )
          IF (STATUS .NE. SAI__OK) GOTO 99

*      Set up the mask
	  LM2 = LMASK/2
	  RR = 1.0/(2.0*WDTH**2)
	  TT = 0.0
	  DO J = 1, LMASK
	    XX = IABS(LM2-J)
	    RMASK(J) = EXP(-RR*XX**2)
	    TT = TT + RMASK(J)
	  END DO
	  TT = 1.0/TT
	  DO J = 1, LMASK
	    RMASK(J) = RMASK(J)*TT
	  END DO

*    Cosine bell?
	ELSE IF ( CHR_SIMLR( MASK(1:3), 'COS') ) THEN

*      Get mask width
          JUMPOUT = .FALSE.
          DO WHILE (.NOT. JUMPOUT)

*        Get user width
            CALL USI_GET0R('MSK_WIDTH',WDTH,STATUS)
            IF ( STATUS .NE. SAI__OK ) GOTO 99
*
	    LM2 = WDTH*1.1
	    LMASK = LM2*2
	    IF ( LMASK .GT. NMASK ) THEN
	      CALL MSG_PRNT('SMOOTH Error mask too wide')
              CALL USI_CANCL( 'MSK_WIDTH', STATUS )
            ELSE
              JUMPOUT = .TRUE.
	    END IF
          END DO

*      Set up the mask
	  TT = 0.0
	  DO J = 1, LMASK
	    XX = REAL(LM2-J)*2.0*MATH__PI/WDTH
	    RMASK(J) = 0.0
	    IF(ABS(XX).LE.MATH__PI) RMASK(J) = COS(XX)+1.0
	    TT = TT + RMASK(J)
	  END DO
	  TT = 1.0/TT
	  DO J = 1, LMASK
	    RMASK(J)=RMASK(J)*TT
	  END DO

*    Otherwise user supplied mask
        ELSE

*      Try from file
          CALL FIO_OPEN( MASK, 'READ', 'LIST', 0, IDAT, STATUS )
	  IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL MSG_SETC( 'MASK', MASK )
	    CALL MSG_PRNT( 'SMOOTH Error - no such mask /^MASK/' )
            CALL USI_CANCL('MSK_MASK',STATUS)

	  ELSE

*        First record is length of mask in bins , preferably odd !
            CALL FIO_READF( IDAT, LINE, STATUS )
	    READ(LINE,*) IDLEN
	    LM2 = IDLEN+1
	    LMASK = LM2*2
	    IF ( IDLEN .GT. NMASK ) THEN
              CALL MSG_SETI(  NMASK, NMASK )
              STATUS = SAI__ERROR
              CALL ERR_REP(' ','Error: template length greater'/
     :                             /' than ^NMASK',STATUS)
              GOTO 99
	    END IF

*        Mask asymmetric if IDLEN even !
	    J1 = LM2 - IDLEN/2 + MOD(IDLEN+1,2)
	    J2 = LM2 + IDLEN/2
	    DO J = 1, LMASK
	      IF ( (J.LT.J1) .OR. (J.GT.J2) ) THEN
		RMASK(J) = 0.0
	      ELSE
                CALL FIO_READF( IDAT, LINE, STATUS )
		READ(LINE,*) RMASK(J)
	      END IF
	    END DO
            CALL FIO_CLOSE( IDAT, STATUS )
            JUMPOUT=.TRUE.

*        Change units. Get original units and use these as a default
            CALL BDI_GET0C( IFID, 'Units', DUNIT, STATUS )

*        Reset status if gone bad.
            IF ( STATUS .NE. SAI__OK ) STATUS = SAI__OK

*        Define default and get new units
            IF ( DUNIT .GT. ' ' ) THEN
              CALL USI_DEF0C( 'MSK_NEWUNITS', DUNIT, STATUS )
            END IF
            CALL USI_GET0C('MSK_NEWUNITS',DUNIT,STATUS)
            CALL BDI_PUT0C(OFID,'Units',DUNIT,STATUS)
            IF (STATUS .NE. SAI__OK) GOTO 99

          END IF

	END IF

*  Got mask specification?
      END DO

*  Fill NELS() up to 4 dimensions
      DO J = NDIMS + 1, 4
        NELS(J) = 1
      END DO

*  Find the largest size which will be needed in smooth_array
      IF ( IDIM1 .GT. 0 ) THEN
        NLIM = MAX( NELS(IDIM),NELS(IDIM1) )
      ELSE
        NLIM = NELS(IDIM)
      END IF

*  Map temporary arrays for use as workspace in smooth_array
      CALL DYN_MAPR( 1, NLIM+513, EBUFP, STATUS )
      CALL DYN_MAPR( 1, NLIM+513, ABUFP, STATUS )
      CALL DYN_MAPL( 1, NLIM+513, QBUFP, STATUS )
      CALL DYN_MAPR( 1, NLIM, RESP, STATUS )
      CALL DYN_MAPR( 1, NLIM, ERESP, STATUS )
      CALL DYN_MAPR( 1, NLIM, ETEMPP, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP(' ','Error mapping temporary space',STATUS)
        GOTO 99
      END IF

*  Find how the user wants to treat the ends of the data
      CALL SMOOTH_ENDS( NELS(IDIM), EMETH, START1, END1, START2, END2,
     :              START_DATA, END_DATA, START_ERR, END_ERR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Set gap variable temporarily false if doing a second smooth
      IF ( IDIM1 .GT. 0 ) THEN
        IDO1 = 0
      ELSE
        IDO1 = IDO
      END IF

*  Now use subroutine call to map data indices and do calculation
      CALL SMOOTH_ARRAY(RMASK,LMASK,IDIM,NELS(IDIM),IDO1,.TRUE.,
     :      NELS(1),NELS(2),NELS(3),NELS(4),LDQUAL,LDVAR,EMETH,START1,
     :         END1,START2,END2,START_DATA,END_DATA,START_ERR,END_ERR,
     :     %VAL(EBUFP),%VAL(ABUFP),%VAL(QBUFP),%VAL(RESP),%VAL(ERESP),
     :        %VAL(ETEMPP),%VAL(QPNTR),%VAL(DPNTR),%VAL(VPNTR))

*  Smooth in second direction
      IF ( IDIM1 .GT. 0 ) THEN
	CALL SMOOTH_ARRAY(RMASK,LMASK,IDIM1,NELS(IDIM1),IDO,.FALSE.,
     :       NELS(1),NELS(2),NELS(3),NELS(4),LDQUAL,LDVAR,EMETH,START1,
     :       END1,START2,END2,START_DATA,END_DATA,START_ERR,END_ERR,
     :      %VAL(EBUFP),%VAL(ABUFP),%VAL(QBUFP),%VAL(RESP),%VAL(ERESP),
     :        %VAL(ETEMPP),%VAL(QPNTR),%VAL(DPNTR),%VAL(VPNTR))
      END IF

*  Update the history.
      CALL HSI_ADD( OFID, VERSION, STATUS )

      CALL MSG_SETC( 'MAS', MASK )
      CALL MSG_SETR( 'WID', WDTH )
      CALL MSG_MAKE( 'Smoothed with a ^MAS mask of width ^WID pixels',
     :               PATH(NLINES+1), IDUM )
      IF (IDO .EQ. 1) THEN
        PATH(NLINES+2)='Bad quality data have been set to zero'
      ELSE
        PATH(NLINES+2)='Bad quality data have been interpolated'
      END IF

*  Write input file and action into history
      CALL HSI_PTXT( OFID, NLINES+2, PATH, STATUS )

*  Tidy up
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END


*+  SMOOTH_ARRAY - Subroutine to smooth data array
      SUBROUTINE SMOOTH_ARRAY(RMASK,LMASK,IDIM,NWORK,IDO,FIRST,
     :             NELS1,NELS2,NELS3,NELS4,LDQUAL,LDVAR,EMETH,
     :             START1,END1,START2,END2,START_DATA,END_DATA,
     :             START_ERR,END_ERR,EBUF,
     :             ABUF,QBUF,RES,ERES,ETEMP,QUAL,ARRAY,VAR,STATUS)
* Description :
* History :
*   Modified M Denby 1987-Apr to be compatible with new QCL
*   June 10 1988  Asterix88 version   (LTVAD::RDS)
*      Asterix88 using quality to define a bad pixel rather than a negative
*     variance, so large changes were made to this routine.
* Type Definitions :
        IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Import :
        INTEGER LMASK                    !Size of mask
        REAL RMASK(LMASK)                !Smoothing mask
        INTEGER IDIM                     !Dimension of array being smoothed
        INTEGER NWORK                    !Number of elements in workspace
        INTEGER IDO                      !Reset gaps to 0 ?
        LOGICAL FIRST                    !First smoothing direction ?
        INTEGER NELS1,NELS2,NELS3,NELS4  !Dimensions of input data array
        LOGICAL LDQUAL                   !Is quality present ?
        LOGICAL LDVAR                    !Are errors present ?
        CHARACTER*(*) EMETH              !How to treat the data ends
        INTEGER START1,END1,START2,END2 !Pixel range to fine mean end
        REAL START_DATA,END_DATA        !Value to use at both ends
        REAL START_ERR,END_ERR          !ERROR to use at both ends
	REAL EBUF(-256:NWORK+256)        ! Workspace
        REAL ABUF(-256:NWORK+256)        ! Workspace
        LOGICAL QBUF(-256:NWORK+256)        ! Workspace
	REAL RES(NWORK)                  ! Workspace
        REAL ERES(NWORK)                 ! Workspace
        REAL ETEMP(NWORK)                ! Workspace
        LOGICAL QUAL(NELS1,NELS2,NELS3,NELS4)  ! Quality array
* Import-Export :
	REAL ARRAY(NELS1,NELS2,NELS3,NELS4) ! Data array
	REAL VAR (NELS1,NELS2,NELS3,NELS4)  ! Variance array

*  Status:
      INTEGER			STATUS             	! Global status

* Local variables :
	REAL RMASK2(512)                            ! Square of smoothing mask
	INTEGER IGAPS(250),IGAPE(250),IND(4),ILOOP(4)
        REAL RE,RS,RW
        INTEGER I,J,K,L,II,JJ,KK
        INTEGER LM,LM2,LDIM
        INTEGER NGAP,IGAP,NGG
        INTEGER DIMS(4)                     ! Dimensions loop
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up a dimensions array
      DIMS(1)=NELS1
      DIMS(2)=NELS2
      DIMS(3)=NELS3
      DIMS(4)=NELS4

*  Set up useful numbers
      LM2 = LMASK/2

*  Calculate square of mask if errors present
      IF ( LDVAR ) THEN
	DO LM = 1, LMASK
	  RMASK2(LM) = RMASK(LM) ** 2
	END DO
      END IF

*  Set up loop variables, inner loop must be dimension requested
      II=0
      DO J=1,4
        IF ( IDIM .NE. J ) THEN
	  II = II + 1
	  ILOOP(II)=J
	END IF
      END DO
      LDIM = NWORK
      DO J=1,DIMS(ILOOP(1))
	IND(ILOOP(1))=J
        DO K=1,DIMS(ILOOP(2))
	  IND(ILOOP(2))=K
          DO L=1,DIMS(ILOOP(3))
	    IND(ILOOP(3))=L

*        This inner loop extracts slice from requested dimension
	    DO I=1,LDIM
	      IND(IDIM)=I
	      ABUF(I)=ARRAY(IND(1),IND(2),IND(3),IND(4))
	      IF ( LDVAR ) THEN
		EBUF(I)=VAR(IND(1),IND(2),IND(3),IND(4))
	      END IF
              IF ( LDQUAL ) THEN
		QBUF(I) = QUAL(IND(1),IND(2),IND(3),IND(4))
              ELSE
                QBUF(I) = .TRUE.
	      END IF
	    END DO

*        Now scan for gaps, tabulate them
	    IF ( LDQUAL ) THEN
	      NGAP = 0
	      IGAP = 0

*        Check quality of each pixel
	      DO JJ = 1, LDIM

*            Pixel is bad?
		IF ( .NOT. QBUF(JJ) ) THEN
		  ETEMP(JJ) = EBUF(JJ)
		  EBUF(JJ)=0.0
		  IF ( IGAP .EQ. 0 ) THEN
		    IGAP = 1
		    NGAP = NGAP + 1
		    IGAPS(NGAP) = JJ - 1
		  END IF
		ELSE
		  IF ( IGAP .EQ. 1 ) THEN
		    IGAP = 0
		    IGAPE(NGAP) = JJ
		  END IF
		END IF

	      END DO

*          Fudge if starts with a gap
	      IF ( IGAPS(1) .EQ. 0 ) THEN
		ABUF(0) = ABUF(IGAPE(1))
	      END IF

*          Fudge if ends with a gap, use start value
	      IF ( IGAP .EQ. 1 ) THEN
		IGAPE(NGAP) = LDIM + 1
		ABUF(LDIM+1) = ABUF(IGAPS(NGAP))
	      END IF

*          Interpolate across gaps - if this is the first smooth
              IF ( FIRST ) THEN
	        DO JJ=1,NGAP
                  RE = ABUF(IGAPE(JJ))
		  RS = ABUF(IGAPS(JJ))
		  NGG = IGAPE(JJ)-IGAPS(JJ)
		  RW = (RE-RS)/FLOAT(NGG)
		  DO KK = 1, NGG-1
		    ABUF(IGAPS(JJ)+KK)=ABUF(IGAPS(JJ)) + RW*REAL(KK)
		  END DO
		END DO
	      END IF

*        End of test if quality present
	    END IF

*        Fill out start with first value and end with last value
            CALL SMOOTH_SETENDS(EMETH, START1, END1, START2, END2,
     :                   START_DATA, END_DATA, START_ERR, END_ERR,
     :                   NWORK, LM2, LDIM, QBUF, ABUF, EBUF)

*        Do the crosscorrelation
	    CALL SMOOTH_CORR(ABUF(1-(LM2-1)),RMASK,LMASK,LDIM,RES)
	    IF ( LDVAR ) THEN
	      CALL SMOOTH_CORR(EBUF(1-(LM2-1)),RMASK2,LMASK,LDIM, ERES)
	    END IF

*        Mask out non-exposed stuff if not reinstating gaps
	    IF ( (IDO.EQ.1) .AND. LDQUAL ) THEN
	      DO JJ = 1, NGAP
		NGG = IGAPE(JJ)-IGAPS(JJ)
		DO KK = 1, NGG-1
		  RES(IGAPS(JJ)+KK) = 0.0
		END DO
	      END DO
	    END IF

*        Put back in data and error arrays
	    DO I = 1, LDIM
	      IND(IDIM) = I
	      ARRAY(IND(1),IND(2),IND(3),IND(4)) = RES(I)
	      IF ( LDVAR ) THEN
		VAR(IND(1),IND(2),IND(3),IND(4)) = ERES(I)
	      END IF
	    END DO
          END DO
        END DO

      END DO

      END


*+ SMOOTH_CORR - Cross-correlates a data array with a mask.
	SUBROUTINE SMOOTH_CORR(OPAND, KERN, NK, NR, RES)
* Description :
*       Linear correlation of Operand with Kernel
* History :
*     date:  original (institution::username)
* Type Definitions :
      IMPLICIT NONE
* Import :
        INTEGER NK                        !Length of kernel vector
        INTEGER NR                        !Length of results vector
	REAL OPAND(0:NR+NK-2)             !Input Operand vector
        REAL KERN(0:NK-1)                 !Input Kernel vector
* Import-Export :
* Export :
        REAL RES(0:NR-1)                  !Output results vector
* Local variables :
        INTEGER I,J
*-
	DO I = 0, NR-1
	  RES (I) = 0.0
	  DO J = 0, NK - 1
	    RES (I) = RES (I) + OPAND (I + J) * KERN (J)
	  END DO
	END DO

	END


*+  SMOOTH_SETENDS - Obtain option for dealing with the ends of the data.
      SUBROUTINE SMOOTH_SETENDS( EMETH, START1, END1, START2, END2,
     &                START_DATA, END_DATA, START_ERR, END_ERR, NVAL,
     &                LM2, LDIM, QBUF, ABUF, EBUF, STATUS )
*    Description :
*     Obtains from the user which ENDS option is to be used, and takes
*     appropriate action.
*    Parameters :
*     EMETH    = LITERAL(Def: ENDAVERAGE) : Choose how to deal with the ends.
*                 ENDAVERAGE       - Extend each end with a local average.
*                 REGIONAVERAGE    - Extend each end with an average over a
*                                    specified region.
*                 ONESPECIFY       - Extend both ends with one specified value.
*                 TWOSPECIFY       - Extend each end with its own specified value.
*                 NOEXTEND         - Don't extend end regions.
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Phillip Andrews ( pla_rosat@uk.ac.bham.sr.star)
*    History :
*     17 Sep 87: Original. (pla)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) EMETH               ! End processing method
      INTEGER START1,END1               ! Pixels to use in finding mean
      INTEGER START2,END2               ! Pixels to use in finding mean
      REAL START_DATA,END_DATA          ! Data Value to use at each end
      REAL START_ERR,END_ERR            ! Error Value to use at each end
      INTEGER                NVAL                                       ! No of data values.
      INTEGER LM2,LDIM
      LOGICAL                QBUF(-256:NVAL+256)                              ! Mapped DATA_QUALITY.
*    Import-Export :
      REAL                   ABUF(-256:NVAL+256)                               ! Mapped data array.
      REAL                   EBUF(-256:NVAL+256)                              ! Mapped DATA_ERROR.

*  Status:
      INTEGER			STATUS             	! Global status

*    Local variables :
      INTEGER            	NUMPIX                  ! Number of pixels.
      INTEGER 			JJ			!
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  End averaging? (the default)
      IF ( EMETH(1:5) .EQ. 'ENDAV' ) THEN

*    Extend each end of the data array with an average taken over a
*    specified No. of values.
        NUMPIX = END1 - START1
        IF ( NUMPIX .EQ. 0 ) NUMPIX = 1

        CALL SMOOTH_MEAN( NVAL, 1, NUMPIX, ABUF(1), EBUF(1),
     :                    QBUF(1), START_DATA, START_ERR )
        CALL SMOOTH_MEAN( NVAL, (NVAL - NUMPIX), NVAL, ABUF(1),
     :                    EBUF(1), QBUF(1), END_DATA, END_ERR )

*  Region averaging
      ELSE IF ( EMETH(1:4) .EQ. 'REGI' ) THEN

        CALL SMOOTH_MEAN( NVAL, START1, END1, ABUF(1), EBUF(1),
     :                             QBUF(1), START_DATA, START_ERR )
        CALL SMOOTH_MEAN( NVAL, START2, END2, ABUF(1), EBUF(1),
     :                                 QBUF(1), END_DATA, END_ERR )

*  End values
      ELSE IF ( EMETH(1:5) .EQ. 'ENDVA' ) THEN
        START_DATA = ABUF(1)
        END_DATA = ABUF(NVAL)
        START_ERR = EBUF(1)
        END_ERR = EBUF(NVAL)

      END IF

*  Set the end values
      DO JJ = 1, LM2-1
	ABUF(1-JJ) = START_DATA
        EBUF(1-JJ) = START_ERR
      END DO
      DO JJ=1,LM2
	ABUF(LDIM+JJ) = END_DATA
        EBUF(LDIM+JJ) = END_ERR
      ENDDO

      END


*+  SMOOTH_MEAN - Calculate a mean or weighted mean.
      SUBROUTINE SMOOTH_MEAN( NVAL, START, STOP, DATA, ERROR,
     :                                   QUALITY, MEAN, ERR )

*    Description :
*     Calculates mean or weighted mean, taking QUALITY into account.
*    History :
*     24 Sep 87: Original (pla)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER                NVAL                                       ! No of data values.
      INTEGER                START                                      ! Index for start of data
      INTEGER                STOP                                       ! Index for end of data

      REAL                   DATA( NVAL )                               ! Mapped data array.
      REAL                   ERROR( NVAL )                              ! Mapped DATA_ERROR.
      LOGICAL                QUALITY( NVAL )                            ! Mapped DATA_QUALITY.
*    Export :
      REAL                   MEAN                                       ! Calculated mean.
      REAL                   ERR                                        ! Error on mean.
*    Local variables :
      INTEGER                LOOP                                       ! Loops over selected region.

      REAL                   SUM                                        ! Sum of weighed values
      REAL                   SUMERR                                        ! Sum of weighed values
      INTEGER                COUNT
*-

*  Initialise
      SUM = 0.0
      SUMERR = 0.0
      COUNT = 0

*  Loop over end values
      DO LOOP = START, STOP
        IF ( QUALITY(LOOP) ) THEN
          SUM = SUM + DATA(LOOP)
          SUMERR = SUMERR + ERROR(LOOP)
          COUNT = COUNT + 1
        END IF
      END DO

*  Find mean and error on mean
      IF ( COUNT .GT. 0 ) THEN
        MEAN = SUM / REAL(COUNT)
        ERR  = SUMERR / REAL(COUNT)
      ELSE
        MEAN=0.0
        ERR=0.0
      END IF

      END


*+  SMOOTH_ENDS - Obtain option for dealing with the ends of the data.
      SUBROUTINE SMOOTH_ENDS( NVAL, ENDS, START1, END1, START2, END2,
     :                      START_DATA, END_DATA, START_ERR, END_ERR,
     :                                                         STATUS)
*    Description :
*     Obtains from the user which ENDS option is to be used.
*    Parameters :
*     ENDS    = LITERAL(Def: ENDAVERAGE) : Choose how to deal with the ends.
*                 ENDAVERAGE       - Extend each end with a local average.
*                 REGIONAVERAGE    - Extend each end with an average over a
*                                    specified region.
*                 ONESPECIFY       - Extend both ends with one specified value.
*                 TWOSPECIFY       - Extend each end with its own specified value.
*                 ENDVALUE         - extend end regions using the last value
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Phillip Andrews ( pla_rosat@uk.ac.bham.sr.star)
*    History :
*     17 Sep 87: Original. (pla)
*     25 MAY 1990  changed a bit       (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER NVAL
*    Import-Export :
*    Export :
      CHARACTER*(*) ENDS                 ! End processing method
      INTEGER START1,END1               ! Pixels to use in finding mean
      INTEGER START2,END2               ! Pixels to use in finding mean
      REAL START_DATA,END_DATA          ! Data Value to use at each end
      REAL START_ERR,END_ERR            ! Error Value to use at each end
*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER            NUMPIX                                         ! Number of pixels.

      LOGICAL            INPUT                                          ! Is input required?
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      INPUT = .TRUE.

      CALL MSG_SETI('VALUES', NVAL)

*  Loop while we don't have a good reply
      DO WHILE ( INPUT )

*    Get averaging option
        CALL USI_GET0C( 'ENDS', ENDS, STATUS )
        IF (STATUS .NE. SAI__OK) GOTO 99
        CALL CHR_UCASE(ENDS)

*    Default is to end the input
        INPUT = .FALSE.

*    End averaging?
        IF ( ENDS(1:5) .EQ. 'ENDAV' ) THEN

*     'Extend' each end of the data array with an average taken over a
*      specified No. of values. Ask user how many values.
          CALL MSG_PRNT( 'There are ^VALUES data values in each slice' )
          CALL MSG_PRNT( 'Extending each end by averaging over' )
          CALL USI_PROMT( 'NUMPIX',
     :                   'the following number of data points', STATUS )
          CALL USI_GET0I( 'NUMPIX', NUMPIX, STATUS )

*      Set start and end pixel position
          START1 = 1
          END1 = NUMPIX

*    One specified value
        ELSE IF ( ENDS(1:5) .EQ. 'ONESP' ) THEN

*      'Extend' array at both ends using one specified value and error
          CALL USI_GET0R( 'DVALUE', START_DATA, STATUS )
          END_DATA = START_DATA
          CALL USI_GET0R( 'EVALUE', START_ERR, STATUS )
          END_ERR = START_ERR

*    Region averaging
        ELSE IF ( ENDS(1:4) .EQ. 'REGI' ) THEN

*      Specify separatly the region to average over to extend each end.
          CALL MSG_PRNT( 'There are ^VALUES data values in each '/
     :       /' slice. The average data value will be calculated'/
     :       /' within a region you specify, and used to extend '/
     :       /'the end regions for the smoothing.' )
            CALL MSG_OUT( 'MSG',
     :                      'You will specify 2 such regions:', STATUS )
            CALL MSG_OUT( 'MSG',
     :                 'Enter lower bound of region to extend', STATUS )
            CALL USI_GET0I( 'START1', START1, STATUS )
*
            CALL MSG_OUT( 'MSG',
     :             'Enter upper bound of the region to extend', STATUS )
            CALL USI_GET0I( 'END1', END1, STATUS )
*
            CALL MSG_OUT( 'MSG',
     :                 'Enter lower bound of region to extend', STATUS )
            CALL USI_GET0I( 'START2', START2, STATUS )
*
            CALL MSG_OUT( 'MSG',
     :             'Enter upper bound of the region to extend', STATUS )
            CALL USI_GET0I( 'END2', END2, STATUS )

*    Two specified values and errors
        ELSE IF ( ENDS(1:3) .EQ. 'TWO' ) THEN

*      Specify separately extend values for each end.
          CALL USI_GET0R( 'VAL1', START_DATA, STATUS )
          CALL USI_GET0R( 'ERR1', START_ERR, STATUS )
          CALL USI_GET0R( 'VAL2', END_DATA, STATUS )
          CALL USI_GET0R( 'ERR2', END_ERR, STATUS )

*    No input required for end values option
        ELSE IF ( ENDS(1:5) .EQ. 'ENDVA' ) THEN

*    Otherwise duff response
        ELSE IF ( STATUS .EQ. SAI__OK ) THEN
          CALL MSG_PRNT( 'Incorrect specification of ends handling' )
          CALL USI_CANCL( 'ENDS', STATUS )
          INPUT = .TRUE.

        END IF

      END DO

 99   CONTINUE

      END
