*+  CORRECT - Normalise a binned dataset in time
      SUBROUTINE CORRECT( STATUS )
*
*    Description :
*
*     Normalises binned datasets in time, ie. converts the input file
*     with units X to X/second (X could be counts, or counts per unit
*     area).
*
*    Environment parameters :
*
*     INP = UNIV(R)
*       Input dataset name
*     OVER = LOGICAL(R)
*       Overwrite the input file?
*     OUT = UNIV(R)
*       Output dataset. Only used if OVER is false
*     TEXP = REAL(R)
*       Exposure time. Only used if not present in file
*     POISS = LOGICAL(R)
*       Copy data to variance if not present on input?
*
*    Method :
*
*     The complexity of the operation required depends on the components
*     present in the input.
*
*      T_RESOLVED = NO
*      IF LIVE_TIMEs are present THEN
*        T_RESOLVED = YES
*      END IF
*      IF header.EFF_EXPOSURE is present
*        TEXP = HEADER.EFF_EXPOSURE
*      ELSE IF header.EXPOSURE_TIME is present
*        TEXP = HEADER.EXPOSURE_TIME
*      ELSE
*        TEXP = user supplied exposure time
*      ENDIF
*      Divide data by TEXP
*      IF T_RESOLVED and input dataset has a time axis
*        Correct for dead time as function(T)
*      END IF
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     12 Nov 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER 			STATUS
*
*    Function declarations :
*
      INTEGER			CHR_LEN
*
*    Local constants :
*
      INTEGER                   MAXHTEXT
        PARAMETER               ( MAXHTEXT = 8 )
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)	HLOC			! Input dataset header
      CHARACTER*(DAT__SZLOC)	ILOC			! Input dataset
      CHARACTER*(DAT__SZLOC)	LLOC			! Output LIVE_TIMEs
      CHARACTER*(DAT__SZLOC)	OLOC			! Output dataset
      CHARACTER*80		HTXT(MAXHTEXT)		! History text
      CHARACTER*80		TEXT			!

      REAL                      DEADC        		! Dead time correction
      REAL                      TEXP         		! Exposure time

      INTEGER			APTR			! Axis data pointer
      INTEGER			AWPTR			! Axis widths pointer
      INTEGER			DCPTR			! Dead time corr array
      INTEGER			DIMS(DAT__MXDIM)	! Dimensions
      INTEGER			DPTR			! Data pointer
      INTEGER			I			! Loop over SNAMES
      INTEGER                   NDIM			! Dimensionality
      INTEGER                   NELM			! Total no. of points
      INTEGER                   NLINES			! No. lines of history
      INTEGER                   NSLOT			! No. of LIVE_TIME slots
      INTEGER			ON_PTR, OFF_PTR, DUR_PTR! LIVE_TIME data
      INTEGER			QPTR			! Quality pointer
      INTEGER                   T_AX, X_AX, Y_AX 	! Axis numbers
      INTEGER			TDIMS(DAT__MXDIM)	! Temp dimensions
      INTEGER                   TLEN			! Length of a string!
      INTEGER			TNDIM			! Temp dimensionality
      INTEGER			VPTR			! Variance pointer

      LOGICAL			ANYBAD			! Any bad points?
      LOGICAL			AXNORM			! T axis normalised?
      LOGICAL			GOT_TEXP		! Got an exposure time?
      LOGICAL			INPRIM			! Input is primitive?
      LOGICAL 			L_ON, L_OFF, L_DUR	! LIVE_TIME components?
      LOGICAL			LIVE_OK			! LIVE_TIME ok?
      LOGICAL			OK			! General validity test
      LOGICAL			OVER			! Overwrite input
      LOGICAL                   POISS			! Create Poisson var'ce
      LOGICAL			QOK			! Quality present?
      LOGICAL			T_RESOLVED		! Got live times?
      LOGICAL			VOK			! Variance present?
*
*    Version :
*
      CHARACTER*30 VERSION
        PARAMETER  ( VERSION = 'CORRECT Version 1.7-0' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise Asterix
      CALL AST_INIT()

*    Version id
      CALL MSG_PRNT( VERSION )

*    Overwrite input?
      CALL PAR_GET0L( 'OVER', OVER, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get either one or two dataset names
      IF ( OVER ) THEN
        CALL USI_ASSOCI( 'INP', 'UPDATE', ILOC, INPRIM, STATUS )
        CALL DAT_CLONE( ILOC, OLOC, STATUS )

      ELSE
        CALL USI_ASSOC2( 'INP', 'OUT', 'READ', ILOC, OLOC, INPRIM,
     :                   STATUS )
        CALL HDX_COPY( ILOC, OLOC, STATUS )

      END IF

*    See if corrected structure is there
      CALL PRO_GET( OLOC, 'CORRECTED.EXPOSURE', OK, STATUS )
      IF ( OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'This dataset has already been '/
     :                               /'corrected!', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get input data
      CALL BDA_CHKDATA( OLOC, OK, NDIM, DIMS, STATUS )
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid numeric data', STATUS )
        GOTO 99
      END IF
      CALL BDA_MAPDATA( OLOC, 'UPDATE', DPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Total number of data points
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*    Variance present?
      CALL BDA_CHKVAR( OLOC, VOK, TNDIM, TDIMS, STATUS )
      IF ( VOK ) THEN
        CALL BDA_MAPVAR( OLOC, 'UPDATE', VPTR, STATUS )

*    Given user option of creating Poisson array
      ELSE
        CALL PAR_GET0L( 'POISS', POISS, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Create variance array from data?
        IF ( POISS ) THEN
          CALL BDA_CREVAR( OLOC, NDIM, DIMS, STATUS )
          CALL BDA_MAPVAR( OLOC, 'WRITE', VPTR, STATUS )
          CALL ARR_COP1R( NELM, %VAL(DPTR), %VAL(VPTR), STATUS )
          VOK = (STATUS.EQ.SAI__OK)
        END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Quality present?
      CALL BDA_CHKQUAL( OLOC, QOK, TNDIM, TDIMS, STATUS )
      IF ( QOK ) THEN
        CALL BDA_MAPLQUAL( OLOC, 'READ', QPTR, ANYBAD, STATUS )
        IF ( .NOT. ANYBAD ) THEN
          CALL BDA_UNMAPLQUAL( OLOC, STATUS )
          QOK = .FALSE.
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Initialise
      T_RESOLVED = .FALSE.
      GOT_TEXP = .FALSE.

*    Get exposure information. First try for the MORE.ASTERIX component. If
*    absent we can't get either LIVE_TIMEs or exposure times.
      CALL BDA_CHKAST( OLOC, OK, STATUS )
      IF ( OK ) THEN

*      Live time structure present?
        CALL BDA_CHKLIVE( OLOC, LIVE_OK, STATUS )
        IF ( LIVE_OK ) THEN

*        Locate it and check whether the 3 supported structures are present
          CALL BDA_LOCLIVE( OLOC, LLOC, STATUS )
          CALL DAT_THERE( LLOC, 'ON', L_ON, STATUS )
          CALL DAT_THERE( LLOC, 'OFF', L_OFF, STATUS )
          CALL DAT_THERE( LLOC, 'DURATION', L_DUR, STATUS )

*        We must have ON, OFF and DURATION
          IF ( .NOT. (L_ON .AND. L_OFF .AND. L_DUR) ) THEN
            CALL MSG_PRNT( '** LIVE_TIME structure does not contain'/
     :               /' expected components and will be ignored **' )

          ELSE

*          Map LIVE_TIME data
            CALL CMP_MAPV( LLOC, 'ON', '_REAL', 'READ', ON_PTR, NSLOT,
     :                     STATUS )
            CALL CMP_MAPV( LLOC, 'OFF', '_REAL', 'READ', OFF_PTR, NSLOT,
     :                     STATUS )
            CALL CMP_MAPV( LLOC, 'DURATION', '_REAL', 'READ', DUR_PTR,
     :                     NSLOT, STATUS )

*          Set flags
            T_RESOLVED = (STATUS.EQ.SAI__OK)
            CALL MSG_SETC( 'WHERE', 'LIVE_TIME slots' )

          END IF

        END IF

*      Is HEADER present?
        CALL BDA_CHKHEAD( OLOC, OK, STATUS )
        IF ( OK ) THEN

*        Locate the header
          CALL BDA_LOCHEAD( OLOC, HLOC, STATUS )

*        Effective exposure present?
          CALL DAT_THERE( HLOC, 'EFF_EXPOSURE', OK, STATUS )
          IF ( OK ) THEN
            CALL CMP_GET0R( HLOC, 'EFF_EXPOSURE', TEXP, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
              CALL MSG_SETC( 'WHERE', 'effective exposure time' )
              GOT_TEXP = .TRUE.
            END IF
          END IF

*        Try simple exposure time
          IF ( .NOT. GOT_TEXP ) THEN
            CALL DAT_THERE( HLOC, 'EXPOSURE_TIME', OK, STATUS )
            IF ( OK ) THEN
              CALL CMP_GET0R( HLOC, 'EXPOSURE_TIME', TEXP, STATUS )
              IF ( STATUS .EQ. SAI__OK ) THEN
                CALL MSG_SETC( 'WHERE', 'exposure time in header' )
                GOT_TEXP = .TRUE.
              END IF
            END IF
          END IF

        END IF

      END IF

*    Default dead time correction
      DEADC = 1.0

*    Time resolved correction possible? We only do this if dataset contains
*    a time like axis
      IF ( T_RESOLVED ) THEN

*      Look for time axis
        CALL AXIS_FINDXYT( OLOC, NDIM, X_AX, Y_AX, T_AX, STATUS )

*      No time axis?
        IF ( T_AX .EQ. 0 ) THEN
          CALL CORRECT_DEAD0( NSLOT, %VAL(ON_PTR), %VAL(OFF_PTR),
     :                             %VAL(DUR_PTR), DEADC, STATUS )
          T_RESOLVED = .FALSE.

        ELSE

*        Ambiguous axis?
          IF ( T_AX .LT. 0 ) THEN
            T_AX = - T_AX
            CALL BDA_GETAXLABEL( OLOC, T_AX, TEXT, STATUS )
            CALL MSG_SETC( 'LABEL', TEXT )
            CALL MSG_PRNT( 'WARNING : Ambiguous time axis, using'/
     :                                           /' axis ^LABEL' )
          END IF

*        Warn if not 1st dimension
          IF ( T_AX .NE. 1 ) THEN
            CALL MSG_PRNT( '** CORRECT cannot handle dead time '/
     :             /'correction unless the time axis is the **' )
            CALL MSG_PRNT( '** first axis. Swap the axes using '/
     :             /'AXORDER and CORRECT should work ok...  **' )
            T_RESOLVED = .FALSE.
            LIVE_OK = .FALSE.

          ELSE

*          Map time axis data and widths
            CALL BDA_MAPAXVAL( OLOC, 'READ', T_AX, APTR, STATUS )
            CALL BDA_MAPAXWID( OLOC, 'READ', T_AX, AWPTR, STATUS )

*          Has data been normalised wrt the time axis already?
            CALL BDA_GETAXNORM( OLOC, T_AX, AXNORM, STATUS )

*          If it has, we have to denormalise and renormalise again afterwards
            IF ( AXNORM ) THEN

*            Pad dimensions to 7D
              DO I = NDIM+1, DAT__MXDIM
                DIMS(I) = 1
              END DO

*            Issue warning
              CALL MSG_PRNT( 'Denormalising wrt time axis...' )

*            Denormalise data
              CALL AR7_DENORM( %VAL(AWPTR), DIMS, T_AX, %VAL(DPTR),
     :                         STATUS )

*            and variance if present
              IF ( VOK ) THEN
                CALL AR7_DENORMV( %VAL(AWPTR), DIMS, T_AX, %VAL(DPTR),
     :                           STATUS )
              END IF

            END IF

*          Construct dead time correction array
            CALL DYN_MAPR( 1, DIMS(T_AX), DCPTR, STATUS )

*          Find dead time correction as function of time
            CALL CORRECT_DEAD1( DIMS(T_AX), %VAL(APTR), %VAL(AWPTR),
     :                          NSLOT, %VAL(ON_PTR), %VAL(OFF_PTR),
     :                          %VAL(DUR_PTR), %VAL(DCPTR), STATUS )

          END IF

        END IF

      END IF

*    Inform user of where we got the exposure time
      IF ( GOT_TEXP ) THEN
        CALL MSG_SETR( 'T', TEXP )
        CALL MSG_PRNT( 'Exposure of ^T seconds derived from ^WHERE' )

      ELSE

*      Get it from the user
        CALL PAR_GET0R( 'TEXP', TEXP, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Write it into the EXPOSURE_TIME field in the header
        CALL BDA_CHKHEAD( OLOC, OK, STATUS )
        IF ( .NOT. OK ) THEN
          CALL BDA_CREHEAD( OLOC, STATUS )
        END IF
        CALL BDA_LOCHEAD( OLOC, HLOC, STATUS )
        CALL DAT_NEW0R( HLOC, 'EXPOSURE_TIME', STATUS )
        CALL CMP_PUT0R( HLOC, 'EXPOSURE_TIME', TEXP, STATUS )

      END IF

*    Simply divide the data by the exposure time multiplied by the global
*    dead time correction.
      CALL CORRECT_INT( NELM, %VAL(DPTR), VOK, %VAL(VPTR), QOK,
     :                  %VAL(QPTR), TEXP*DEADC, STATUS )

*    Time resolved and dataset has time axis
      IF ( LIVE_OK ) THEN

*      Apply the dead time correction
        CALL CORRECT_DEADCOR( DIMS(T_AX), NELM/DIMS(T_AX), %VAL(DCPTR),
     :                        %VAL(DPTR), VOK, %VAL(VPTR), QOK,
     :                        %VAL(QPTR), STATUS )

        IF ( T_RESOLVED ) THEN
          CALL MSG_PRNT( 'Performing time resolved dead time '/
     :                                          /'correction' )
        ELSE IF ( DEADC .NE. 1.0 ) THEN
          CALL MSG_SETR( 'DEAD', DEADC )
          CALL MSG_PRNT( 'Applied a total dead time correction of ^D' )
        END IF
      END IF

*    Renormalise?
      IF ( AXNORM .AND. T_RESOLVED ) THEN

*      Issue warning
        CALL MSG_PRNT( 'Renormalising wrt time axis...' )

*      Renormalise data
        CALL AR7_NORM( %VAL(AWPTR), DIMS, T_AX, %VAL(DPTR), STATUS )

*      and variance if present
        IF ( VOK ) THEN
          CALL AR7_NORMV( %VAL(AWPTR), DIMS, T_AX, %VAL(VPTR), STATUS )
        END IF

      END IF

*    Adjust the data label
      CALL BDA_GETLABEL( OLOC, TEXT, STATUS )
      IF ( TEXT .GT. ' ' ) THEN
        CALL BDA_PUTLABEL( OLOC, '('//TEXT(:CHR_LEN(TEXT))//') / s',
     :                     STATUS )
      END IF

*    Flag dataset as corrected
      CALL PRO_SET( OLOC, 'CORRECTED.EXPOSURE', .TRUE., STATUS )
      IF ( LIVE_OK ) THEN
        CALL PRO_SET( OLOC, 'CORRECTED.DEAD_TIME', .TRUE., STATUS )
      END IF

*    Update HISTORY
      CALL HIST_ADD( OLOC, VERSION, STATUS )
      HTXT(1) = 'Input {INP}'
      NLINES = MAXHTEXT
      CALL USI_TEXT( 1, HTXT, NLINES, STATUS )
      CALL HIST_PTXT( OLOC, NLINES, HTXT, STATUS )
      CALL MSG_SETR( 'TEXP', TEXP )
      CALL MSG_MAKE( 'Corrected for exposure time of ^TEXP seconds',
     :               TEXT, TLEN )
      CALL HIST_PTXT( OLOC, 1, TEXT(:TLEN), STATUS )
      IF ( LIVE_OK ) THEN
        IF ( T_RESOLVED ) THEN
          TEXT = 'Performed time resolved dead time correction'
        ELSE IF ( DEADC .NE. 1.0 ) THEN
          CALL MSG_SETR( 'DEAD', DEADC )
          CALL MSG_MAKE( 'Applied a total dead time correction of ^D',
     :                   TEXT, TLEN )
        END IF
        CALL HIST_PTXT( OLOC, 1, TEXT, STATUS )
      END IF

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  CORRECT_INT - Normalise a binned dataset in time
      SUBROUTINE CORRECT_INT( N, DATA, VOK, VAR, QOK, QUAL, TEXP,
     :                        STATUS )
*
*    Description :
*
*     Divides DATA by TEXP taking account of quality and variance
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     12 Nov 93 : Original (DJA)
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
      INTEGER			N			! Number of data points
      LOGICAL			VOK			! Variance present?
      REAL                      VAR(*) 			! Input variance
      LOGICAL			QOK			! Quality present?
      LOGICAL                   QUAL(*) 		! Input quality
      REAL			TEXP			! Exposure time
*
*    Import / Export :
*
      REAL                      DATA(*) 		! Input & output data
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      INTEGER			I			! Loop over data

      LOGICAL			OK			! General validity test
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set default for no quality present
      OK = .TRUE.

*    Loop over data
      DO I = 1, N
        IF ( QOK ) OK = QUAL(I)

*      This point ok?
        IF ( OK ) THEN

*        Correct it
          DATA(I) = DATA(I) / TEXP

*        Correct variance if present
          IF ( VOK ) THEN
            VAR(I) = VAR(I) / (TEXP**2)
          END IF

        END IF

      END DO

      END



*+  CORRECT_DEAD0 - Finds the total dead time correction from LIVE_TIME data
      SUBROUTINE CORRECT_DEAD0( N, ON, OFF, DUR, DEADC, STATUS )
*
*    Description :
*
*     Accumulates a time integrated dead time correction given the LIVE_TIME
*     start and stop times + actual instrument duration during that interval.
*     The definition of DUR is such that DUR(i) <= (OFF(i)-ON(i)).
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     12 Nov 93 : Original (DJA)
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
      INTEGER			N			! Number of data points
      REAL                      ON(*), OFF(*) 		! On and off times
      REAL                      DUR(*) 			! Durations
*
*    Export :
*
      REAL			DEADC			! Dead time correction
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      INTEGER			I			! Loop over data
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Accumluate dead time correction
      DEADC = 0.0
      DO I = 1, N
        DEADC = DEADC + (OFF(I)-ON(I))/DUR(I)
      END DO

      END


*+  CORRECT_DEAD1 - Dead time correction from LIVE_TIME data as function of T
      SUBROUTINE CORRECT_DEAD1( NAX, AX, AXWID, N, ON, OFF, DUR,
     :                          DEADC, STATUS )
*
*    Description :
*
*     Calculates the dead time correction as a function of time given a
*     series of time bin centres and widths, and a set of LIVE_TIME data.
*
*    Method :
*
*     FOR each time bin
*       Identify those contributing LIVE_TIME slots
*       Zero the accumulated exposure for this time bin
*       FOR each such slot
*         Calculate the amount of overlap between the time bin and slot
*         Add the contribution from the overlap region allowing for the
*         average dead time correction over the LIVE_TIME slot.
*       NEXT live time slot
*     NEXT time bin
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     12 Nov 93 : Original (DJA)
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
      INTEGER			NAX			! Number of time points
      REAL                      AX(*), AXWID(*) 	! Time bin centre/width
      INTEGER			N			! Number of data points
      REAL                      ON(*), OFF(*) 		! On and off times
      REAL                      DUR(*) 			! Durations
*
*    Export :
*
      REAL			DEADC(*)		! Dead time as fn(T)
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      REAL			DLO, DHI		! Overlap region
      REAL 			TEXP			! Exposure into a bin
      REAL			TLO, THI		! Extrema of time bin

      INTEGER			I, J			! Loop over data
      INTEGER			JLO, JHI		! LIVE_TIME bin indices
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop over time bins
      DO I = 1, NAX

*      Zero the exposure accumulator
        TEXP = 0.0

*      The extrema of this time bin
        TLO = AX(I) - AXWID(I)/2.0
        THI = AX(I) + AXWID(I)/2.0

*      Find indices of all LIVE_TIME slots which contribute to this time bin.
*      The binary search routine rounds down to the nearest index number so
*      add on an extra bin at the top end and do a test in the next loop.
        CALL UTIL_BINSEARCH( N, ON, TLO, JLO )
        CALL UTIL_BINSEARCH( N, OFF, THI, JHI )
        JHI = JHI + 1

*      Loop over the LIVE_TIME bins contributing to the current time bin
        DO J = JLO, JHI

*        Check that this LIVE_TIME bin does actually overlap our time bin.
*        One of the live time boundaries must lie inside our time bin, or
*        they must lie to the left and right.
          IF ( ((ON(J).GE.TLO) .AND. (ON(J).LE.THI)) .OR.
     :         ((OFF(J).GE.TLO) .AND. (OFF(J).LE.THI)) .OR.
     :         ((ON(J).LT.TLO) .AND. (OFF(J).GT.THI)) ) THEN

*          Workout the overlap of the LIVE_TIME slot and the time bin
            DLO = MAX(TLO,ON(J))
            DHI = MIN(THI,OFF(J))

*          Work out how much real exposure has gone into this time bin
*          from this LIVE_TIME slot
            TEXP = TEXP + (DHI-DLO) * (OFF(J)-ON(J)) / DUR(J)

          END IF

        END DO

*      The units of the input data are unnormalised, eg. just counts. The
*      dead time correction factor is therefore simply the sum of the
*      exposure contributions from the various LIVE_TIME bins divided by the
*      bin width.
        DEADC(I) = TEXP / AXWID(I)

      END DO

      END


*+  CORRECT_DEADCOR - Apply dead time correction
      SUBROUTINE CORRECT_DEADCOR( NTAX, NT, DEADC, DATA, VOK, VAR, QOK,
     :                            QUAL, STATUS )
*
*    Description :
*
*     Applies dead time correction to a stack of 1-d arrays.
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     12 Nov 93 : Original (DJA)
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
      INTEGER			NTAX			! No. point per T slice
      INTEGER			NT			! No. slices
      REAL                      DATA(NTAX,NT) 		! Input data
      LOGICAL			VOK			! Variance present?
      REAL                      VAR(NTAX,NT) 		! Input variance
      LOGICAL			QOK			! Quality present?
      LOGICAL                   QUAL(NTAX,NT) 		! Input quality
      REAL			DEADC(NTAX)		! Dead time as fn(T)
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      INTEGER			I, J			! Loops over data
      LOGICAL                   OK			! This data point ok?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set default for no quality present
      OK = .TRUE.

*    For each series in the stack
      DO J = 1, NT

*      Correct each series
        DO I = 1, NTAX

*        Check quality if present
          IF ( QOK ) OK = QUAL(I,J)

*        This point ok?
          IF ( OK ) THEN

*          Correct it
            DATA(I,J) = DATA(I,J) * DEADC(I)

*          Correct variance if present
            IF ( VOK ) THEN
              VAR(I,J) = VAR(I,J) * (DEADC(I)**2)
            END IF

          END IF

        END DO

      END DO

      END
