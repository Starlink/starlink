*+  TIMSIM - Time-series simulation program
      SUBROUTINE TIMSIM(STATUS)
*    Description :
*     The program produces a simulated time-series with either a time
*     axis from another data set or a regular one created by the user.
*     Various components can be added into the time series:-
*     Constant level
*     Ramp
*     Sine(s)
*     Square Wave(s)
*     Saw Tooth Wave(s)
*     Flare(s)
*     Poisson Scatter
*
*    Environment parameters :
*    INP = UNIV(R)
*    Input file name(if time axis is to be read in)
*    OUT = UNIV(W)
*    Output filename
*    AXIS = LOGICAL(R)
*    Is axis from another file required?
*    DUR = REAL(R)
*    Duration of time-series
*    WID = REAL(R)
*    Width of time bins
*    INIT = REAL(R)
*    Initial time value
*    OPT = INTEGER(R)
*    Options required
*    DLAB = CHAR(R)
*    Data label
*    DUNI = CHAR(R)
*    Data units
*    AXLAB = CHAR(R)
*    Axis label
*    AXUNI = CHAR(R)
*    Axis units
*    Method : The program starts by establishing the time axis of the output
*    file, for which it prompts for the name. This time axis can either be
*    a time axis taken from another standard ASTERIX file (it must be axis
*    number 1) or by stating the duration,initial time and bin width and
*    creating a regular axis. The program then creates and maps the data and
*    variance components and prompts the user for which of the options
*    displayed on the option menu are to be used. Each required subroutine is
*    in turn called and the particular components added to the data.
*
*    Deficiencies :
*
*    Bugs :
*
*    Authors :
*     Simon Duck   (BHVAD::SRD)
*     Geoff Mellor (LTVAD::GRM)
*    History :
*      9 Mar 90 : V1.0-0 Original (BHVAD::SRD)
*      8 Nov 91 : V1.1-0 Corrected (LTVAD::GRM)
*     19 Oct 92 : V1.7-0 History handling corrected (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     20 Apr 95 : V1.8-1 Updated data interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      INTEGER NMAX
      PARAMETER (NMAX=20)
*
*    Local variables :
*
      CHARACTER*20 DLAB,DUNI,AXLAB,AXUNI
      INTEGER NBACK,AXPTR,WIPTR,OPTR,QPTR,DIM,I,VPTR
      INTEGER CHOICE(NMAX),IFID,OFID
      LOGICAL LOGCHO(NMAX),AXIS,OK,REG
      REAL DUR,WID,INIT
*
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'TIMSIM Version 1.8-1')
*-

*    Check status
      IF (STATUS.NE.SAI__OK) RETURN

	CALL AST_INIT()

        CALL MSG_PRNT(VERSION)

*	Get name of output file

	CALL USI_TASSOCO('OUT','TIME',OFID,STATUS)

*       Create axis structure and write labels and units

	CALL BDI_CREAXES(OFID,1,STATUS)
	CALL USI_GET0C('AXLAB',AXLAB,STATUS)
	CALL BDI_PUTAXLABEL(OFID,1,AXLAB,STATUS)
	CALL USI_GET0C('AXUNI',AXUNI,STATUS)
	CALL BDI_PUTAXUNITS(OFID,1,AXUNI,STATUS)
	CALL USI_GET0C('DLAB',DLAB,STATUS)
	CALL BDI_PUTLABEL(OFID,DLAB,STATUS)
	CALL USI_GET0C('DUNI',DUNI,STATUS)
	CALL BDI_PUTUNITS(OFID,DUNI,STATUS)
	IF (STATUS.NE.SAI__OK) GOTO 9000

*	Establish whether time axis will need to be read in

	CALL USI_GET0L('AXIS',AXIS,STATUS)

*          Axis read in

	IF(AXIS)THEN
	   CALL USI_TASSOCI('INP','*','READ',IFID,STATUS)
	   IF (STATUS.NE.SAI__OK) GOTO 9000
	   CALL BDI_COPAXIS(IFID,OFID,1,1,STATUS)
	   CALL BDI_MAPAXVAL(IFID,'READ',1,AXPTR,STATUS)
	   CALL BDI_CHKAXVAL(IFID,1,OK,REG,DIM,STATUS)
	   CALL BDI_MAPAXWID(IFID,'READ',1,WIPTR,STATUS)
	   IF (STATUS.NE.SAI__OK) GOTO 9000

*          Axis created

	ELSE
	   CALL USI_GET0R('DUR',DUR,STATUS)
	   CALL USI_GET0R('WID',WID,STATUS)
	   CALL USI_GET0R('INIT',INIT,STATUS)
	   IF (STATUS.NE.SAI__OK) GOTO 9000
	   DIM = NINT(DUR/WID)
	   CALL BDI_PUTAXVAL(OFID,1,INIT,WID,DIM,STATUS)
           CALL DYN_MAPR(1,DIM,AXPTR,STATUS)
           CALL ARR_REG1R( INIT, WID, DIM, %VAL(AXPTR),STATUS )
	   CALL BDI_PUTAXWID(OFID,1,WID,STATUS)
           CALL DYN_MAPR(1,DIM,WIPTR,STATUS)
           CALL ARR_INIT1R(WID,DIM,%VAL(WIPTR),STATUS)
	   IF (STATUS.NE.SAI__OK) GOTO 9000
	ENDIF

*	Data Simulation

	CALL BDI_CREDATA(OFID,1,DIM,STATUS)
	CALL BDI_MAPDATA(OFID,'WRITE',OPTR,STATUS)
	CALL ARR_INIT1R(0.0,DIM,%VAL(OPTR),STATUS)
	CALL BDI_CREVAR(OFID,1,DIM,STATUS)
	CALL BDI_MAPVAR(OFID,'WRITE',VPTR,STATUS)
	IF (STATUS.NE.SAI__OK) GOTO 9000

*	Option Menu

	CALL MSG_PRNT(' 1) Background')
	CALL MSG_PRNT(' 2) Ramp')
	CALL MSG_PRNT(' 3) Sine(s)')
	CALL MSG_PRNT(' 4) Square Wave(s)')
	CALL MSG_PRNT(' 5) Saw Tooth Wave(s)')
	CALL MSG_PRNT(' 6) Flare(s)')
	CALL MSG_PRNT(' 7) Poisson Scatter')
	CALL PRS_GETLIST('OPT',NMAX,CHOICE,NBACK,STATUS)
	IF (STATUS.NE.SAI__OK) GOTO 9000
	DO I=1,NBACK
	   LOGCHO(CHOICE(I))=.TRUE.
	ENDDO

*	Calling the option subroutines

	IF (LOGCHO(1)) CALL TIMSIM_BG(DIM,%VAL(AXPTR),%VAL(WIPTR),
     :  %VAL(OPTR),STATUS)
	   IF (STATUS.NE.SAI__OK) GOTO 9000
	IF (LOGCHO(2)) CALL TIMSIM_RA(DIM,%VAL(AXPTR),%VAL(WIPTR),
     :  %VAL(OPTR),STATUS)
	   IF (STATUS.NE.SAI__OK) GOTO 9000
	IF (LOGCHO(3)) CALL TIMSIM_SI(DIM,%VAL(AXPTR),%VAL(WIPTR),
     :  %VAL(OPTR),STATUS)
	   IF (STATUS.NE.SAI__OK) GOTO 9000
	IF (LOGCHO(4)) CALL TIMSIM_SQ(DIM,%VAL(AXPTR),%VAL(WIPTR),
     :  %VAL(OPTR),STATUS)
	   IF (STATUS.NE.SAI__OK) GOTO 9000
	IF (LOGCHO(5)) CALL TIMSIM_SA(DIM,%VAL(AXPTR),%VAL(WIPTR),
     :  %VAL(OPTR),STATUS)
	   IF (STATUS.NE.SAI__OK) GOTO 9000
	IF (LOGCHO(6)) CALL TIMSIM_FL(DIM,%VAL(AXPTR),%VAL(WIPTR),
     :  %VAL(OPTR),STATUS)
	   IF (STATUS.NE.SAI__OK) GOTO 9000
	IF (LOGCHO(7)) CALL TIMSIM_SC(DIM,%VAL(AXPTR),%VAL(WIPTR),
     :  %VAL(OPTR),STATUS)
	   IF (STATUS.NE.SAI__OK) GOTO 9000
	CALL TIMSIM_VA(DIM,%VAL(OPTR),%VAL(WIPTR),%VAL(VPTR),STATUS)

*    Tidy up
 9000 CONTINUE

*    Set output quality to good
      CALL BDI_CREQUAL(OFID,1,DIM,STATUS)
      CALL BDI_MAPQUAL(OFID,'WRITE',QPTR,STATUS)
      CALL ARR_INIT1B(QUAL__GOOD,DIM,%VAL(QPTR),STATUS)
      CALL BDI_PUTMASK(OFID,QUAL__MASK,STATUS)

*    History :
      CALL HSI_NEW(OFID,STATUS)
      CALL HSI_ADD(OFID,VERSION,STATUS)

      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

	END



*+  TIMSIM_BG - Adds a constant level onto the data component
	SUBROUTINE TIMSIM_BG(DIM,AXIS,AXWI,DATA,STATUS)
*    Description :
*
*    Environment parameters :
*    LEV1 = REAL(R)
*    Constant level
*
*    Method :
*
*    Authors :
*     Simon Duck (BHVAD::SRD)
*    History :
*     09-MAR-90:  Original (BHVAD::SRD)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER DIM
*    Import-Export :
      REAL DATA(DIM),AXIS(DIM),AXWI(DIM)
*    Export :
*
*    Status :
      INTEGER STATUS
*    Local constants :
*
*    Local variables :
      INTEGER I
      REAL LEV
*    Local data :
*
*-
	CALL USI_GET0R('LEV1',LEV,STATUS)
	DO I=1,DIM
	   DATA(I)=DATA(I)+LEV
	ENDDO
	END

*+  TIMSIM_RA - Adds a ramp to the data
      SUBROUTINE TIMSIM_RA(DIM,AXIS,AXWI,DATA,STATUS)
*    Description :
*
*    History :
*     09-MAR-90:  Original (BHVAD::SRD)
*     08-NOV-91   Corrected (LTVAD::GRM)
*    Type definitions :
      IMPLICIT NONE
*    Environment Parameters:
*    RAM1 = REAL(R)
*    Initial level of ramp
*    RAM2 = REAL(R)
*    Final level of ramp
*    Import :
      INTEGER DIM
      REAL AXIS(DIM),AXWI(DIM)
*    Import-Export :
      INTEGER STATUS
      REAL DATA(DIM)
*    Export :
*
*    Local constants :

*    Local variables :
      INTEGER I
      REAL LEV(2)
*-
	CALL USI_GET0R('RAM1',LEV(1),STATUS)
	CALL USI_GET0R('RAM2',LEV(2),STATUS)
	DO I=1,DIM
	   DATA(I)=DATA(I)+LEV(1)+(LEV(2)-LEV(1))*(AXIS(I)-AXIS(1))
     :     /(AXIS(DIM)-AXIS(1))
	ENDDO
	END
*+  TIMSIM_SI - Adds a sine wave to the data
      SUBROUTINE TIMSIM_SI(DIM,AXIS,AXWI,DATA,STATUS)
*    Description :
*
*    History :
*     09-MAR-90:  Original (BHVAD::SRD)
*    Type definitions :
      IMPLICIT NONE
*    Environment Parameters :
*    NUMSIN = INTEGER(R)
*    Number of sine waves
*    PER1 = REAL(R)
*    Period of sine wave
*    PHA1 = REAL(R)
*    Initial phase of sine wave
*    AMP1 = REAL(R)
*    Amplitude of sine wave
*    BSF = INTEGER(R)
*    Bin splitting factor
*    Import :
      INTEGER DIM
      REAL AXIS(DIM),AXWI(DIM)
*    Import-Export :
      INTEGER STATUS
      REAL DATA(DIM)
*    Export :
*
*    Local constants :

*    Local variables :
      INTEGER I,J,K,NUMSIN,BSF
      REAL Z,PER,PHA,AMP,TOT
*-
	CALL USI_GET0I('BSF',BSF,STATUS)
	CALL USI_GET0I('NUMSIN',NUMSIN,STATUS)
	DO I=1,NUMSIN
	   IF(NUMSIN.GT.1)THEN
	      CALL MSG_SETI('NUMSIN',I)
	      CALL MSG_OUT('E','Sine wave number ^numsin',STATUS)
	   ENDIF
	   CALL USI_GET0R('PER1',PER,STATUS)
	   CALL USI_CANCL('PER1',STATUS)
	   CALL USI_GET0R('PHA1',PHA,STATUS)
	   CALL USI_CANCL('PHA1',STATUS)
	   CALL USI_GET0R('AMP1',AMP,STATUS)
	   CALL USI_CANCL('AMP1',STATUS)
	   DO J=1,DIM
	      DO K=0,BSF
	         Z=SIND(((AXIS(J)-AXWI(J)/2+(AXWI(J)*K/BSF))/PER*360)+PHA)
	         TOT=TOT+Z
	      ENDDO
	      DATA(J)=DATA(J)+AMP*TOT/(BSF+1)
	      TOT=0
	   ENDDO
	ENDDO
	END

*+  TIMSIM_SQ - Adds a square wave to the data
      SUBROUTINE TIMSIM_SQ(DIM,AXIS,AXWI,DATA,STATUS)
*    Description :
*
*    History :
*     09-MAR-90:  Original (BHVAD::SRD)
*    Type definitions :
      IMPLICIT NONE
*    Environment Parameters :
*    NUMSQU = INTEGER(R)
*    Number of square waves
*    PER2 = REAL(R)
*    Period of square wave
*    PHA2 = REAL(R)
*    Initial phase of square wave
*    AMP2 = REAL(R)
*    Amplitude of square wave
*    BSF = INTEGER(R)
*    Bin splitting factor
*    Import :
      INTEGER DIM
      REAL AXIS(DIM),AXWI(DIM)
*    Import-Export :
      INTEGER STATUS
      REAL DATA(DIM)
*    Export :

*    Local constants :

*    Local variables :
      INTEGER I,J,K,NUMSQU,BSF
      REAL PER,PHA,AMP,Y,TOT,Z
*-
	CALL USI_GET0I('BSF',BSF,STATUS)
	CALL USI_GET0I('NUMSQU',NUMSQU,STATUS)
	DO I=1,NUMSQU
	   IF(NUMSQU.GT.1)THEN
	      CALL MSG_SETI('NUMSQU',I)
	      CALL MSG_OUT('E','Square wave number ^numsqu',STATUS)
	   ENDIF
	   CALL USI_GET0R('PER2',PER,STATUS)
	   CALL USI_CANCL('PER2',STATUS)
	   CALL USI_GET0R('PHA2',PHA,STATUS)
	   CALL USI_CANCL('PHA2',STATUS)
	   CALL USI_GET0R('AMP2',AMP,STATUS)
	   CALL USI_CANCL('AMP2',STATUS)
	   DO J=1,DIM
	      DO K=0,BSF
	         Z=((AXIS(J)-AXWI(J)/2+(AXWI(J)*K/BSF))/PER)
	         Z=ABS(Z-INT(Z))
	         IF(Z.LE.0.5)THEN
	            Y=1
	         ELSE
	            Y=-1
	         ENDIF
	         TOT=TOT+Y
	      ENDDO
	      DATA(J)=DATA(J)+AMP*TOT/(BSF+1)
	      TOT=0
	   ENDDO
	ENDDO
	END

*+  TIMSIM_SA - Adds a square wave to the data
      SUBROUTINE TIMSIM_SA(DIM,AXIS,AXWI,DATA,STATUS)
*    Description :
*
*    History :
*     09-MAR-90:  Original (BHVAD::SRD)
*    Type definitions :
      IMPLICIT NONE
*    Enviroment Parameters :
*    NUMSAW = INTEGER(R)
*    Number of saw tooth waves
*    PER3 = REAL(R)
*    Period of saw tooth wave
*    PHA3 = REAL(R)
*    Initial phase of saw tooth wave
*    AMP3 = REAL(R)
*    Amplitude of saw tooth wave
*    BSF = INTEGER(R)
*    Bin splitting factor
*    Import :
      INTEGER DIM
      REAL AXIS(DIM),AXWI(DIM)
*    Import-Export :
      INTEGER STATUS
      REAL DATA(DIM)
*    Export :

*    Local constants :

*    Local variables :
      INTEGER I,J,K,NUMSAW,BSF
      REAL PER,PHA,AMP,Y,TOT,Z
*-
	CALL USI_GET0I('BSF',BSF,STATUS)
	CALL USI_GET0I('NUMSAW',NUMSAW,STATUS)
	DO I=1,NUMSAW
	   IF(NUMSAW.GT.1)THEN
	      CALL MSG_SETI('NUMSAW',I)
	      CALL MSG_OUT('E','Saw tooth wave number ^numsaw',STATUS)
	   ENDIF
	   CALL USI_GET0R('PER3',PER,STATUS)
	   CALL USI_CANCL('PER3',STATUS)
	   CALL USI_GET0R('PHA3',PHA,STATUS)
	   CALL USI_CANCL('PHA3',STATUS)
	   CALL USI_GET0R('AMP3',AMP,STATUS)
	   CALL USI_CANCL('AMP3',STATUS)
	   DO J=1,DIM
	      DO K=0,BSF
	         Z=((AXIS(J)-AXWI(J)/2+(AXWI(J)*K/BSF))/PER)
	         Z=ABS(Z-INT(Z))
	         IF(Z.LE.0.5)THEN
	            Y=1-Z*4
	         ELSE
	            Y=(Z-0.5)*4
	         ENDIF
	         TOT=TOT+Y
	      ENDDO
	      DATA(J)=DATA(J)+AMP*TOT/(BSF+1)
	      TOT=0
	   ENDDO
	ENDDO
	END

*+  TIMSIM_FL - Adds a flare to the data
      SUBROUTINE TIMSIM_FL(DIM,AXIS,AXWI,DATA,STATUS)
*    Description :
*
*    History :
*     09-MAR-90:  Original (BHVAD::SRD)
*    Type definitions :
      IMPLICIT NONE
*    Environment Parameters :
*    NUMFLA = INTEGER(R)
*    Number of flares
*    RIS = REAL(R)
*    Rise time
*    DEC = REAL(R)
*    Decay time
*    AMP4 = REAL(R)
*    Amplitude of flare
*    TIM = REAL(R)
*    Time of max. of flare
*    BSF = INTEGER(R)
*    Bin splitting factor
*    Import :
      INTEGER DIM
      REAL AXIS(DIM),AXWI(DIM)
*    Import-Export :
      INTEGER STATUS
      REAL DATA(DIM)
*    Export :

*    Local constants :

*    Local variables :
      INTEGER I,J,K,NUMFLA,BSF
      REAL DEC,RIS,AMP,TIM,Z,Y,TOT
*-
	CALL USI_GET0I('BSF',BSF,STATUS)
	CALL USI_GET0I('NUMFLA',NUMFLA,STATUS)
	DO I=1,NUMFLA
	   IF(NUMFLA.GT.1)THEN
	      CALL MSG_SETI('NUMFLA',I)
	      CALL MSG_OUT('E','Flare number ^numfla',STATUS)
	   ENDIF
	   CALL USI_GET0R('RIS',RIS,STATUS)
	   CALL USI_CANCL('RIS',STATUS)
	   CALL USI_GET0R('DEC',DEC,STATUS)
	   CALL USI_CANCL('DEC',STATUS)
	   CALL USI_GET0R('AMP4',AMP,STATUS)
	   CALL USI_CANCL('AMP4',STATUS)
	   CALL USI_GET0R('TIM',TIM,STATUS)
	   CALL USI_CANCL('TIM',STATUS)
	   DO J=1,DIM
	      DO K=0,BSF
	         Z=AXIS(J)-AXWI(J)/2+AXWI(J)*K/BSF
	         IF(Z.GT.(TIM-RIS).AND.Z.LT.TIM) THEN
	            Y=(Z-(TIM-RIS))/RIS
	         ELSEIF(Z.GT.TIM)THEN
                    Y=EXP(-(Z-TIM)/DEC)
	         ENDIF
	         TOT=TOT+Y
	      ENDDO
	      DATA(J)=DATA(J)+AMP*TOT/(BSF+1)
	      TOT=0
	      Y=0
	   ENDDO
	ENDDO
	END

*+  TIMSIM_SC - Scatters the data points according to the Poisson distribution
      SUBROUTINE TIMSIM_SC(DIM,AXIS,AXWI,DATA,STATUS)
*    Description :
*
*    History :
*     09-MAR-90:  Original (BHVAD::SRD)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER DIM
      REAL AXIS(DIM),AXWI(DIM)
*    Import-Export :
      INTEGER STATUS
      REAL DATA(DIM)
*    Export :

*    Function declarations :
      INTEGER MATH_POISS
*    Local constants :

*    Local variables :
      INTEGER I
      REAL COUNTS
*-
	DO I=1,DIM
	   COUNTS=DATA(I)*AXWI(I)
	   IF(COUNTS.LT.0)COUNTS=0
	   DATA(I)=REAL(MATH_POISS(COUNTS))/AXWI(I)
	ENDDO
	END

*+  TIMSIM_VA - Calculates variance for each data point
      SUBROUTINE TIMSIM_VA(DIM,DATA,AXWI,VAR,STATUS)
*    Description :
*
*    History :
*     09-MAR-90:  Original (BHVAD::SRD)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER DIM
      REAL AXWI(DIM),DATA(DIM)
*    Import-Export :
      INTEGER STATUS
*    Export :
      REAL VAR(DIM)
*    Local constants :

*    Local variables :
      INTEGER I
      REAL COUNTS
*-
	DO I=1,DIM
	   COUNTS=DATA(I)*AXWI(I)
	   VAR(I)=ABS(COUNTS)/(AXWI(I)*AXWI(I))
	   IF(VAR(I).LT.(1/(AXWI(I)*AXWI(I))))VAR(I)=1/(AXWI(I)*AXWI(I))
	ENDDO
	END
