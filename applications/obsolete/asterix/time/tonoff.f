*+  TONOFF - writes a file of on/off times from selected ranges
      SUBROUTINE TONOFF(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     rjv@star.sr.bham.ac.uk
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) ILOC
      INTEGER FID
      LOGICAL PRIM

*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'TONOFF Version 1.8-0')
*-
      CALL MSG_PRNT(VERSION)

*  interactive system not active - standalone mode
      IF (.NOT.T_OPEN) THEN

*  general initialisation
        CALL AST_INIT()

*  get input time series
        CALL USI_ASSOCI('INP','READ',ILOC,PRIM,STATUS)

        CALL TIM_CHECK(ILOC,STATUS)
        CALL TIM_MAP(ILOC,STATUS)

*  default to whole time series
        CALL TIM_NOCHOP(STATUS)
        CALL TIM_SCALE(%VAL(T_APTR),%VAL(T_WPTR),STATUS)

      ENDIF


*  get file name for on/off times
      CALL USI_GET0C('FILE',FILE,STATUS)
      CALL FIO_OPEN(FILE,'WRITE','NONE',0,FID,STATUS)

*  do the business
      CALL TONOFF_DOIT(%val(T_APTR),%val(I_WPTR),%val(T_QPTR),T_MASK,
     :                                                    FID,STATUS)

*  close the file
      CALL FIO_CLOSE(FID,STATUS)

*  if standalone shut down time series
      IF (.NOT.T_OPEN) THEN

        CALL BDA_RELEASE(ILOC,STATUS)
        CALL AST_CLOSE(STATUS)
      ENDIF

      END



*+
      SUBROUTINE TONOFF_DOIT(A,W,Q,MASK,FID,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     rjv@star.sr.bham.ac.uk
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Import :
      REAL A(*),W(*)
      BYTE Q(*),MASK
      INTEGER FID
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB
*    Local constants :
*    Local variables :
      CHARACTER*40 BUFFER,UNITS
      DOUBLE PRECISION MJD1,MJD2,CONV
      INTEGER ISECT
      INTEGER I
*-
      IF (STATUS.EQ.SAI__OK) THEN

        UNITS=T_UNITS
        CALL CHR_UCASE(UNITS)
        IF (INDEX(UNITS,'SEC').NE.0) THEN
          CONV=86400.0D0
        ELSEIF (INDEX(UNITS,'MIN').NE.0) THEN
          CONV=1440.0D0
        ELSEIF (INDEX(UNITS,'H').NE.0) THEN
          CONV=24.0D0
        ELSEIF (INDEX(UNITS,'DAY').NE.0) THEN
          CONV=1.0D0
        ELSEIF (INDEX(UNITS,'MJD').NE.0) THEN
          CONV=1.0D0
        ELSE
          CALL MSG_PRNT(
     :         '*** unrecognised time units - using seconds ***')
          conv=86400.0D0
        ENDIF
        DO ISECT=1,T_NSECT
          IF (T_SEL(ISECT)) THEN

            I=T_SECTPIX(1,ISECT)
            J=T_SECTPIX(2,ISECT)
            DO WHILE (I.LT.J)
*  find start of good period
              FINI=.FALSE.
              DO WHILE(BIT_ANDUB(Q(I),MASK).NE.QUAL__GOOD.AND.
     :                                               .NOT.FINI)
                IF (I.LT.J) THEN
                  I=I+1
                ELSE
                  FINI=.TRUE.
                ENDIF
              ENDDO
              IF (.NOT.FINI) THEN
                MJD1=T_BASEMJD+DBLE(A(I)-W(I)/2.0)/CONV
*  find end of good period
                FINI=.FALSE.
                DO WHILE(BIT_ANDUB(Q(I),MASK).EQ.QUAL__GOOD.AND.
     :                                                 .NOT.FINI)
                  IF (I.LT.J) THEN
                    I=I+1
                  ELSE
                    FINI=.TRUE.
                  ENDIF
                ENDDO
                IF (FINI) THEN
                  MJD2=T_BASEMJD+DBLE(A(J)+W(J)/2.0)/CONV
                ELSE
                  MJD2=T_BASEMJD+DBLE(A(I-1)+W(I-1)/2.0)/CONV
                ENDIF
                BUFFER=' '
                BUFFER(1:1)='M'
                BUFFER(15:15)='M'
                WRITE(BUFFER(2:13),'(F12.6)') MJD1
                WRITE(BUFFER(16:27),'(F12.6)') MJD2
                CALL FIO_WRITE(FID,BUFFER,STATUS)
              ENDIF
            ENDDO

          ENDIF
        ENDDO



      ENDIF

      END
