*+  PSS_CONTROL - Master control procedure for PSS
      SUBROUTINE PSS_CONTROL( IFILE, STATUS )
*
*    Description :
*
*     Selects the statistic routines at the different stages of processing.
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     24 Feb 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER                  IFILE                   ! File number
*
*    Function declarations :
*
      INTEGER                  UTIL_PLOC               ! Replaces %LOC
      INTEGER                  PSS_RTN_SEL
      LOGICAL                  STR_ABBREV
*
*    Local constants :
*
      REAL                     VSB                     ! Default min bgnd
        PARAMETER              ( VSB = 1.0E-9 )
*
*    Local variables :
*
      CHARACTER*40             SOPT                    ! Statistic to use

      REAL                     VAL                     ! Manual background

      INTEGER                  BNELM                   ! Background elements
      INTEGER                  NDPTR                   ! Dynamic data ptr

      LOGICAL                  BPRIM                   ! Background primitive?
      LOGICAL                  BPROCESS                ! Process background?
*
*    External references :
*
      EXTERNAL                 PSS_STAT_GAUSS,  PSS_STAT_GAUSSU
      EXTERNAL                 PSS_STAT_CASH,   PSS_STAT_CASHU
      EXTERNAL                 PSS_STAT_CASHR,  PSS_STAT_CASHRU
      EXTERNAL                 PSS_STAT_CASHS,  PSS_STAT_CASHSV
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the statistic option
      IF ( IFILE .EQ. 1 ) THEN
        IF ( CP_OPT .AND. .NOT. CP_SPOT ) THEN
          CALL USI_GET0C( 'SOPT', SOPT, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            GOTO 99
          ELSE IF ( STR_ABBREV(SOPT,'CASH') ) THEN
            CP_CASH = .TRUE.
          ELSE IF ( STR_ABBREV(SOPT,'GAUSSIAN') ) THEN
            CP_CASH = .FALSE.
            IF ( .NOT. IM_BGND_SUBTRACTED ) THEN
              CALL MSG_PRNT( 'WARNING : Image is not background '/
     :                       /'subtracted - failure imminent...' )
            END IF

          ELSE
            CALL MSG_SETC( 'OPT', SOPT )
            CALL MSG_PRNT( 'Unrecognised statistic option /^OPT/' )
            STATUS = SAI__ERROR
          END IF
        ELSE
          CP_CASH = .TRUE.
        END IF
      END IF

*    Multi file mode?
      IF ( CP_MULTI ) THEN
        BPROCESS = (.NOT. MU_SAME_BCK )
      ELSE
        BPROCESS = .TRUE.
      END IF
      IF ( CP_MULTI .AND. .NOT. MU_SAME_BCK ) THEN

*      Interpret this as real number?
        CALL CHR_CTOR( MU_BCK, VAL, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          BPRIM = .TRUE.
        ELSE
          BPRIM = .FALSE.
          CALL ERR_ANNUL( STATUS )

*        Open file directly
          CALL HDS_OPEN( MU_BCK, 'READ', BG_LOC, STATUS )

        END IF
        BPROCESS = .TRUE.

      ELSE IF ( IFILE .EQ. 1 ) THEN

*      Get option from user
        CALL USI_ASSOCI( 'BGND', 'READ', BG_LOC, BPRIM, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Primitive?
        IF ( BPRIM ) THEN

*        Check only one element
          CALL DAT_SIZE( BG_LOC, BNELM, STATUS )
          IF ( BNELM .NE. 1 ) THEN
            CALL MSG_PRNT( '! Background must be file or single '/
     :                                          /'numeric value' )
            STATUS = SAI__ERROR
          END IF

*        Get value
          CALL DAT_GET0R( BG_LOC, VAL, STATUS )
          BPROCESS = .TRUE.

        END IF

      ELSE
        BPROCESS = .FALSE.

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Process bgnd?
      IF ( BPROCESS ) THEN

*      If primitive, try to get real number
        IF ( BPRIM ) THEN

*        Invent dynamic data
          CALL DYN_MAPR( BDS_NDIM, BDS_DIMS, BG_DATA_PTR, STATUS )

*        Fill with value if ok
          IF ( STATUS .EQ. SAI__OK ) THEN
            VAL = MAX(VSB,VAL)
            CALL MSG_SETR( 'VAL', VAL )
            CALL MSG_PRNT( 'Using background value ^VAL for '/
     :                                    /'entire image...' )
            BG_OK = .TRUE.
            BG_DYNAMIC = .TRUE.
            BG_DATA_OK = .TRUE.
            BG_DATA_DYNAMIC = .TRUE.

*          Set bgnd values in slice
            CALL PSS_BGND_SET( BDS_DIMS(1), BDS_DIMS(2), VAL,
     :             %VAL(BG_DATA_PTR), STATUS )

          END IF

        ELSE

*        Get and validate file
          CALL PSS_BGND_LOAD( STATUS )

        END IF

      END IF

*    If user supplied a background subtracted image, recreate raw data
      IF ( CP_CASH .AND. IM_BGND_SUBTRACTED ) THEN
        CALL MSG_PRNT( 'Re-creating raw data...' )
        CALL DYN_MAPR( BDS_NDIM, BDS_DIMS, NDPTR, STATUS )
        CALL ARR_SUMS( '+', BDS_NELM, %VAL(IM_DATA_PTR), 0, 0,
     :                      BDS_NELM, %VAL(BG_DATA_PTR), 0, 0,
     :                      BDS_NELM, %VAL(NDPTR), 0, 0 )
        IM_DATA_DYNAMIC = .TRUE.
        IM_DATA_PTR = NDPTR
        IM_DYNAMIC = .TRUE.
        IM_OK = .TRUE.
      END IF

*    Select the grid and fit routines
      IF ( IFILE .EQ. 1 ) THEN
        IF ( CP_CASH ) THEN
          GR_ROUTINE = PSS_RTN_SEL( PSS_STAT_CASH, PSS_STAT_CASHU )
        ELSE
          GR_ROUTINE = PSS_RTN_SEL( PSS_STAT_GAUSS, PSS_STAT_GAUSSU )
        END IF

*      Re-scaling with cash mode
        IF ( CP_CASH .AND. CP_EXPERT ) THEN

*        Rescaling the background?
          CALL USI_GET0L( 'RESCALE', CP_RESCALE, STATUS )
          IF ( CP_RESCALE .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*          Get starting value for scale factor
            CALL USI_GET0R( 'ISCALE', CP_IBSCALE, STATUS )

*          and choose routine
            GR_ROUTINE = PSS_RTN_SEL( PSS_STAT_CASHR,
     :                                PSS_STAT_CASHRU )

          END IF

        ELSE
          CP_IBSCALE = 1.0
          CP_RESCALE = .FALSE.

        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Create log background array if cash mode
      IF ( BPROCESS ) THEN
        IF ( CP_CASH .AND. (CP_MODE.NE.PSS__M_SENMAP) ) THEN
          CALL DYN_MAPR( BDS_NDIM, BDS_DIMS, BDS_LBGND_PTR, STATUS )

*        Single value?
          IF ( BPRIM .AND. (BNELM.EQ.1) ) THEN
            CALL PSS_BGND_SET( BDS_DIMS(1), BDS_DIMS(2), LOG(VAL),
     :                               %VAL(BDS_LBGND_PTR), STATUS )
          ELSE
            CALL PSS_BGND_LOG( BDS_DIMS(1), BDS_DIMS(2),
     :             %VAL(BG_DATA_PTR), %VAL(BDS_LBGND_PTR),
     :             STATUS )
          END IF
        ELSE
          BDS_LBGND_PTR = BG_DATA_PTR
        END IF
      END IF

*    Set grid routine
      IF ( IFILE .EQ. 1 ) THEN
        IF ( CP_MODE .EQ. PSS__M_SENMAP ) THEN
          GR_ROUTINE = UTIL_PLOC(PSS_STAT_CASHS)
        ELSE IF ( CP_MODE .EQ. PSS__M_SIGVAR ) THEN
          GR_ROUTINE = UTIL_PLOC(PSS_STAT_CASHSV)
        END IF
      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_CONTROL', STATUS )
      END IF

      END
