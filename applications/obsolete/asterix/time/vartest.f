*+  VARTEST - Test whether a source time series is significantly variable
      SUBROUTINE VARTEST( STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Simon Duck  (BHVAD::SRD)
*     David Allan (BHVAD::DJA)
*
*    History :
*
*     11 May 90 : V1.0-0  Original (SRD)
*     18 May 90 : V1.2-0  Improved (SRD)
*      8 Oct 92 : V1.7-0  Uses D.P. NAG for portability (DJA)
*
*    Type Definitions :
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
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) ILOC          ! Source time series
      CHARACTER*(DAT__SZLOC) BLOC          ! Background time series
      REAL                   AREA          ! Area correction factor
      REAL                   PFUNC         ! Variability statistic

      INTEGER                BDIMS(DAT__MXDIM)
      INTEGER                IACT, BACT    ! Area correction arrays
      INTEGER                INDIM, BNDIM,LDIM! Input dimesnionalities
      INTEGER                IDIMS(DAT__MXDIM)
      INTEGER                NDIMS(DAT__MXDIM)
      INTEGER                IDPTR, BDPTR  ! Data pointers
      INTEGER                IVPTR, BVPTR  ! Variance pointers
      INTEGER                IQPTR,BQPTR   ! Quality pointers
      INTEGER                IWPTR         ! Axis widths pointer
      INTEGER                VALPTR
      INTEGER                NELM          ! Length of time series

      LOGICAL                IOK, BOK      ! Datasets ok?
      LOGICAL                PRIM,BAD
      LOGICAL                IQUALOK,BQUALOK
*
*    Version id :
*
      CHARACTER*30            VERSION
         PARAMETER           (VERSION = 'VARTEST Version 1.7-0')
*-

*    Check status.
      IF (STATUS .NE. SAI__OK) RETURN

*    Version ID.
      CALL MSG_PRNT( VERSION )

*    Initialize
      CALL AST_INIT

*    Obtain data object name.
      CALL USI_ASSOCI('INP', 'READ', ILOC, PRIM, STATUS)
      CALL USI_ASSOCI('BACK', 'READ', BLOC, PRIM, STATUS)
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map input data.
      CALL BDA_CHKDATA(ILOC, IOK, INDIM, IDIMS, STATUS)
      CALL BDA_CHKDATA(BLOC, BOK, BNDIM, BDIMS, STATUS)

      IF ( IOK .AND. BOK ) THEN
        IF ( INDIM .NE. 1 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Has to be 1D', STATUS )
        ELSE IF ( IDIMS(1) .NE. BDIMS(1) ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Background series must be same length'/
     :                                 /' as source series', STATUS )
        END IF
        NELM = IDIMS(1)
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Input datasets invalid', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map data
      CALL BDA_MAPDATA( ILOC, 'READ', IDPTR, STATUS )
      CALL BDA_MAPVAR( ILOC, 'READ', IVPTR, STATUS )
      CALL BDA_MAPAXWID( ILOC, 'READ', 1, IWPTR, STATUS )
      CALL BDA_MAPDATA( BLOC, 'READ', BDPTR, STATUS )
      CALL BDA_MAPVAR( BLOC, 'READ', BVPTR, STATUS )

*    Get relative area factor
      CALL USI_GET0R( 'AREA', AREA, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map QUALITY as a logical.
      BAD = .FALSE.
      CALL BDA_CHKQUAL (ILOC, IQUALOK, NDIMS, LDIM, STATUS)
      IF (IQUALOK) THEN
        CALL BDA_MAPLQUAL(ILOC,'READ',BAD,IQPTR,STATUS)
      ELSE
        CALL DYN_MAPL(INDIM,IDIMS,IQPTR,STATUS)
        CALL ARR_INIT1L(.TRUE.,IDIMS,%VAL(IQPTR),STATUS)
      END IF
      CALL BDA_CHKQUAL(BLOC,BQUALOK,NDIMS,LDIM,STATUS)
      IF(BQUALOK)THEN
        CALL BDA_MAPLQUAL(BLOC,'READ',BAD,BQPTR,STATUS)
      ELSE
        CALL DYN_MAPL(INDIM,IDIMS,BQPTR,STATUS)
        CALL ARR_INIT1L(.TRUE.,IDIMS,%VAL(BQPTR),STATUS)
      ENDIF

*    Map memory for area correction factors
      CALL DYN_MAPR( 1, NELM, IACT, STATUS )
      CALL DYN_MAPR( 1, NELM, BACT, STATUS )

*    Map memory for 'VAL' array
      CALL DYN_MAPR(1,NELM,VALPTR,STATUS)

*    Test for variability
      CALL VARTEST_CALC( NELM, %VAL(IDPTR), %VAL(IVPTR), %VAL(IWPTR),
     :              %VAL(IACT), %VAL(BDPTR), %VAL(BVPTR), %VAL(BACT),
     :                  %VAL(IQPTR),%VAL(BQPTR),%VAL(VALPTR),AREA,
     :                                                PFUNC, STATUS )

*    Write parameter to user and external
      CALL MSG_SETR( 'STAT', PFUNC )
      CALL MSG_PRNT( 'Variability statistic ^STAT' )
      CALL USI_PUT0R( 'PFUNC', PFUNC, STATUS )

*    Free dynamic memory
      CALL DYN_UNMAP( IACT, STATUS )
      CALL DYN_UNMAP( BACT, STATUS )

*    Free datasets
      CALL BDA_RELEASE( ILOC, STATUS )
      CALL BDA_RELEASE( BLOC, STATUS )

*   Exit
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END




*+  VARTEST_CALC - Do work for variability analysis
      SUBROUTINE VARTEST_CALC(NUM,SOU,SOUV,AXWID,VIGS,BACK,
     :                   BACKV,VIGB,SQUAL,BQUAL,
     :                          VAL,AREA,PFUNC,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     Simon Duck  (BHVAD::SRD)
*     David Allan (BHVAD::DJA)
*
*    History :
*
*     11 May 90 : Original (SRD)
*     18 May 90 : Improved (SRD)
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
*    Import :
*
      INTEGER NUM
      REAL SOU(NUM), SOUV(NUM)
      REAL AXWID(NUM)
      REAL VIGS(NUM)
      REAL VIGB(NUM)
      REAL BACK(NUM), BACKV(NUM)
      LOGICAL SQUAL(NUM),BQUAL(NUM)
      REAL AREA
*
*    Export :
*
      REAL          PFUNC
*
*    Status :
      INTEGER STATUS
*
*    Local variables :
      REAL VAL(NUM)
      REAL MAX
      REAL MIN
      REAL STEP1,MINC
      REAL CR
      REAL SIG,MAXSIG

      DOUBLE PRECISION RLAMBDAS,RLAMBDAB
      DOUBLE PRECISION PLEKS,PLEKB,PGTKS,PGTKB,PEQKS,PEQKB

      REAL BESTCRMAX,C1,MAXC
      INTEGER BI,A,B
      INTEGER MAXSIGI
      INTEGER MAXI
      INTEGER MINI
      INTEGER KS,KB,IFAIL,I,J,NBAD
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find VIGS and VIGB from variance arrays
      DO I = 1, NUM
         IF(SOU(I).EQ.0.0)THEN
            VIGS(I)=1
         ELSE
            VIGS(I) = SOU(I) / SOUV(I)
         ENDIF
         IF(BACK(I).EQ.0.0)THEN
            VIGB(I)=1
         ELSE
            VIGB(I) = BACK(I) / BACKV(I)
         ENDIF
      END DO
      J=1
      DO WHILE(.NOT.SQUAL(J).OR..NOT.BQUAL(J))
         J=J+1
      END DO
      MIN=SOU(J)-(BACK(J)*AREA)
      MAX=MIN
      MINI=J
      MAXI=J
      NBAD=J-1
      DO I=J,NUM
         IF(SQUAL(I).AND.BQUAL(I))THEN
            VAL(I)=SOU(I)-(BACK(I)*AREA)
            IF(VAL(I).LT.MIN)THEN
               MINI=I
               MIN=VAL(I)
            ENDIF
            IF(VAL(I).GT.MAX)THEN
               MAXI=I
               MAX=VAL(I)
            ENDIF
         ELSE
            NBAD=NBAD+1
         ENDIF
      ENDDO

      MAXC=0
      MINC=0
      STEP1=(MAX-MIN)/100
      DO CR=MIN,MAX,STEP1
         C1=0
         MAXSIG=0
         DO I=J,NUM
           IF(SQUAL(I).AND.BQUAL(I))THEN
              SIG=ABS(VAL(I)-CR)/SQRT(SOUV(I)+BACKV(I)*AREA*AREA)
              IF(SIG.GT.MAXSIG)THEN
                 MAXSIG=SIG
                 MAXSIGI=I
              ENDIF
           ENDIF
         ENDDO
         A=NINT((VIGB(MAXSIGI)*BACK(MAXSIGI))-
     :             2*SQRT(BACK(MAXSIGI)*VIGB(MAXSIGI)))
         IF(A.LT.0)A=0
         B=NINT((VIGB(MAXSIGI)*BACK(MAXSIGI))+
     :             2*SQRT(BACK(MAXSIGI)*VIGB(MAXSIGI)))
         DO BI=A,B
           KS=NINT((VAL(MAXSIGI)*VIGS(MAXSIGI))+
     :                (BI*VIGS(MAXSIGI)*AREA/VIGB(MAXSIGI)))
           RLAMBDAS = DBLE((CR+BACK(MAXSIGI)*AREA)*VIGS(MAXSIGI))
           CALL G01BKF(RLAMBDAS,KS,PLEKS,PGTKS,PEQKS,IFAIL)
           RLAMBDAB = DBLE(BACK(MAXSIGI)*VIGB(MAXSIGI))
           KB=BI
           CALL G01BKF(RLAMBDAB,KB,PLEKB,PGTKB,PEQKB,IFAIL)
           IF(VAL(MAXSIGI).GT.CR)THEN
              C1=C1+((PGTKS+PEQKS)*PEQKB)
           ELSE
              C1=C1+(PLEKS*PEQKB)
           ENDIF
         ENDDO
         IF(C1.GT.MAXC)MAXC=C1
      ENDDO
      PFUNC=(NUM-NBAD-1)*MAXC
      END
