      SUBROUTINE OBSERVE(STATUS)
*+
*  Name:
*     OBSERVE
*
*  Purpose:
*     Calculate rise/set times for an object/observatory combination
*     and display the results or output them to a file.
*
*  Language:
*     Starlink Fortran 77
*
*  Invocation:
*     CALL OBSERVE( STATUS )
*
*  Arguments:
*     STATUS = INTEGER(Given and Returned)
*     The global status.
*
*  Description:
*     Determines values for:
*        Object rise/set time (UT)
*        Object below/above 30 degrees from horizon.
*        Moon phase versus date
*        Moon rise/set time
*        Twilight begins/ends
*        Moon/object distance
*     Outputs value as a file or a graph.
*
*  Usage:
*     OBSERVE NUMBER STAR RA DEC YEAR DEVICE TEXTF OUT AGAIN
*
*  ADAM Parameters:
*     NUMBER = _INTEGER (Read)
*        Observatory reference number.
*     STAR = _CHAR (Read)
*        The name of the object.
*     RA = _CHAR (Read)
*        Right ascension of the object.
*     DEC = _CHAR (Read)
*        Declination of the object.
*     YEAR = _INTEGER (Read)
*        The year for which the data should be calculated.
*     TEXTF = _LOGICAL (Read)
*        Is a text file output required?
*     OUT = _CHAR (Read)
*        The output file name for the text.
*     AGAIN = _CHAR (Read)
*        Repeat the run?
*
*  Authors:
*     MG: Manfred Gottwald (EXOSAT)
*     JO: Julian Osborne (Leicester)
*     JKA: ?
*     GJP: Grant Privett (Starlink, CAR)
*     BLY: Martin Bly (Starlink, RAL)
*
*  History:
*     ??-JUN-1988 (MG):
*        Original version.
*     26-JAN-1989 (MG):
*        Input to character variables.
*     26-MAR-1991 (MG):
*        Allow different star altitude warnings.
*     02-OCT-1991 (JO): Version 1.2
*        Moon position, rise & set times & phase added.
*        Starlink - link observe,pgplot_dir:grpshr/lib.
*     17-JAN-1992 (JKA): Version 1.3
*        Midnight local time centered on plot.
*        Contour line styles adjusted for improved visibility.
*     24-MAY-1993 (JKA):
*        Obervatory details input via SCALIB, RA & DEC input via SLALIB
*     01-AUG-1994 (JKA):
*        Observatory time zones automatic for SLALIB observatories.
*     02-AUG-1994 (JKA): Version 2.0
*        Now uses the ADAM parameter system.
*     18-OCT-1994 (JKA): Version 2.1
*        Fixed bug in co-ordinate translation
*     30-JAN-1996 (JO):
*        Added new observatory data to obsinfo() to match SLA_OBS.
*     22-MAY-1996 (GJP):
*        Removed Asterix dependance by removing AST_INIT and MSG_PRNTs.
*     23-MAY-1996 (GJP):
*        Added text output option via TEXTF.
*     29-MAY-1996 (GJP):
*        Modified to calculate rise/set times for every day but still
*           display moon results for every two days.
*     26-SEP-2000 (BLY): Version 2.3
*        Added new observatory data to obsinfo() to match SLA_OBS.
*
*  Notes
*     The parameters are calculated for every day of the year but the
*     lunar phase information is only displayed every two days to avoid
*     huge black areas on the printout. However, the full dataset
*     is provided in the optional output file.

*-
*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Status:
      INTEGER STATUS                  ! Global status

*  External reference:
      INTEGER CHR_LEN                 ! Length of string
      EXTERNAL RANGE

*  Local variables:
      CHARACTER*2   DYS               ! 2 char representatoin of the day
      CHARACTER*128 JUNK              ! Temporary string
      CHARACTER*10  MONS(12)          ! Months of the year
      CHARACTER *1  SPACE             ! The space character
      CHARACTER*10  STRA(10)          ! A time or distance string
      INTEGER DAY                     ! Current day
      INTEGER FIOD                    ! File output identifier
      INTEGER LMONTH                  ! Previous month
      INTEGER MONTH                   ! Current month month
      INTEGER NCHAR                   ! Number of chars in a string
      LOGICAL TEXTF                   ! Is a text file required
      LOGICAL EXCLAIM                 ! Was a ! found as a file name
      LOGICAL OPENF                   ! Did the file open okay
      REAL RVALUE                     ! Real value
      REAL TIMO                       ! Y axis time offset

      INTEGER LYEAR(14),MDAY(12),NDAY(12),ENDYR,DELTA
      INTEGER IHMSF(4),IDMSF(4)
      INTEGER IALT,I,INO,JR,ILEAP,K
      INTEGER ITXT1,ITXT2,ITXT7,ITXT8,J,L,LTIME,LL
      INTEGER NP,NL,NLL,JJ,LENGTH
      REAL XPL1(400),XPL2(400),xpl3(400),YPL1(400),YPL2(400)
      REAL YPL3(400),YPL4(400),YPL5(400),YPL6(400),YPl7(400)
      REAL YPl8(400),F(400),DIST(400)
      REAL XTICK1(2),XTICK2(2),YTICK1(2),YTICK2(2),XP(2),YP(2)
      REAL INCH,XDELT,YDELT
      REAL XV1,XV2,YV1,YV2
      DOUBLE PRECISION RA,DEC,ARG1,ARG2,PI,PIR,PIG,ALONG,ALAT,JD
      DOUBLE PRECISION LSTR,LSTS,GSTR,GSTS,GMTR,GMTS,T,HRANG1,HRANG2
      DOUBLE PRECISION X,Y,DECPR,DT,ECL90,ECLPER,ECC,N,M,LATSUN,RASUN
      DOUBLE PRECISION DECSUN,EC,OBL
      DOUBLE PRECISION T0(400)
      DOUBLE PRECISION L0,P0,N0,INC,LONG,MM,EV,AE,A3,MMPR,A4,LPR,V,LPRPR
      DOUBLE PRECISION NPR,LM1,BM1,LM2,BM2,D,SM,RA1,DEC1,RA2,DEC2,LSTR1
      DOUBLE PRECISION LSTS1,GSTR1,GSTS1,LSTR2,LSTS2,GSTR2,GSTS2,DB,DL
      DOUBLE PRECISION T00PR,MMR,LONGR,DELT,PSI,JD1990,DA,CD
      DOUBLE PRECISION ALT,DYNO,THETA,PARA,EMOON,TH0,PI0,RHOPR
      CHARACTER*1 SIGN,AGAIN
      CHARACTER*2 CALT
      CHARACTER*3 XLAB,YLAB
      CHARACTER*4 C4
      CHARACTER*12 NAME,RACH,DECCH
      CHARACTER*40 OBSNAM,DEVICE,VALUE
      CHARACTER*128 TEXT1,TEXT2,TEXT3,TEXT4,TEXT5,TEXT6,TEXT7,TEXT8
      LOGICAL TWILGHT_FLG(400),TWILGHT
      LOGICAL GET_TELE, GET_STAR

      DATA MDAY/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA NDAY/0,31,59,90,120,151,181,212,243,273,304,334/
      DATA LYEAR/1980,1984,1988,1992,1996,2000,2004,2008,2012,
     &           2016,2020,2024,2028,2032/
      DATA GET_TELE/.TRUE./
      DATA GET_STAR/.TRUE./

      CHARACTER*30 VERSION
      PARAMETER (VERSION='OBSERVE V2.3')

*.


*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

C   Opening credits.
      CALL MSG_BLANK(STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999
      CALL MSG_OUT(' ',VERSION,STATUS)
      CALL MSG_BLANK(STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

C   Full names of month.
      MONS(1)='January'
      MONS(2)='February'
      MONS(3)='March'
      MONS(4)='April'
      MONS(5)='May'
      MONS(6)='June'
      MONS(7)='July'
      MONS(8)='August'
      MONS(9)='September'
      MONS(10)='October'
      MONS(11)='November'
      MONS(12)='December'

C   Useful constants.
      SPACE=' '
      PI=3.141592654D0
      PIR=PI/180.d0
      PIG=180.D0/PI
C
  800 CONTINUE
C
      DO I=1,400
        YPL1(I)=0.0
        YPL2(I)=0.0
        YPL3(I)=0.0
        YPL4(I)=0.0
        YPL5(I)=0.0
        YPL6(I)=0.0
        YPL7(I)=0.0
        YPL8(I)=0.0
      END DO
      INO=0

C
C---  GET TELESCOPE DETAILS (NAME,LAT(RAD), LONG(RAD) & TIME DIFFERENCE)
      IF (GET_TELE) THEN
         CALL GETOBS(OBSNAM,ALAT,ALONG,DELTA,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
C
C---     CONVERT LONGITUTE FROM RADIANS TO DAYS
         ALONG = ALONG * PIG / 15.0
	 GET_TELE = .FALSE.
      ENDIF
C
C---  GET STAR DETAILS (NAME, RA(RAD), DEC(RAD) & YEAR OF OBSERVATION)
      IF (GET_STAR) THEN

         CALL GETSTAR(NAME,RA,DEC,JR,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
C
C---     WRITE COORDINATES INTO STRINGS
         CALL SLA_DR2TF(1,RA,SIGN,IHMSF)
         WRITE(RACH,11) IHMSF(1),IHMSF(2),IHMSF(3),IHMSF(4)
 11      FORMAT(I2,'h',I2,'m',I2,'.',I2)

         CALL SLA_DR2AF(1,DEC,SIGN,IDMSF)
         WRITE(DECCH,12) SIGN,IDMSF(1),IDMSF(2),IDMSF(3),IDMSF(4)
 12      FORMAT(A,I2,'d',I2,'m',I2,'.',I2)
	 GET_STAR = .FALSE.
C---     CONVERT RA FROM RADIANS TO DAYS
	 RA = RA * PIG / 15.0D0

      ENDIF

C   Inform the user.
      CALL MSG_OUT(' ','Calculating rise times etc.',STATUS)

C
      ILEAP=0
      DO I=1,14
        IF (JR.EQ.LYEAR(I)) ILEAP=1
      END DO
C
      K=1
      MDAY(2) = 28
      IF (ILEAP.EQ.1) MDAY(2)=29
      ITXT1=0
      ITXT2=0
      ITXT7=0
      ITXT8=0
C
C  get T0 for GST to GMT calc (sect 13)
      CALL JULD(JR,1,0.0,JD)		!jd=julian day of start of year
      IF (ILEAP.EQ.0) ENDYR=365
      IF (ILEAP.EQ.1) ENDYR=366
      DO J=1,ENDYR,1
        DYNO=J				! j=1 is 0hrs UT Jan 1.0
        T=(JD+dyno-2451545.d0)/36525.d0
        T0(j)=6.697374558d0+2400.051336d0*T+0.000025862d0*T**2
        call range(t0(j),24.d0)		! reduce to within 0 to 24
C
C  RISE AND SETTING OF OBJECT WITH RA,DEC
C  (PRACTICAL ASTRONOMY WITH YOUR CALCULATOR - P. DUFFETT-SMITH)
        ARG1=-DTAN(ALAT)*DTAN(DEC)
        IF (ARG1.GE.1.0) GO TO 500
        IF (ARG1.LE.-1.0) GO TO 501
        HRANG1=PIG*DACOS(ARG1)/15.0
        LSTR=24-HRANG1+RA
        LSTS=HRANG1+RA
        IF (LSTR.GT.24.0) LSTR=LSTR-24.0
        IF (LSTS.GT.24.0) LSTS=LSTS-24.0
        IF (LSTR.LT.0.0) LSTR=LSTR+24.0
        IF (LSTS.LT.0.0) LSTS=LSTS+24.0
C
C  LST TO GST
        CALL LGST (ALONG,LSTR,LSTS,GSTR,GSTS)

C  GST TO GMT
        CALL GGMT (T0(j),GSTR,GSTS,GMTR,GMTS)
        YPL1(K)=GMTR
        YPL2(K)=GMTS
        GO TO 502
  501   TEXT2='The star never sets'
        ITXT2=1
  502   CONTINUE
C
C  STAR ABOVE ialt DEGREES
C
	ialt=30
        calt = '30'
C
	alt=ialt*pir
        ARG2=(dsin(alt)-DSIN(ALAT)*DSIN(DEC))/(DCOS(ALAT)*DCOS(DEC))
        IF (ARG2.LT.1.0.AND.ARG2.GT.-1.0) GO TO 602
        IF (ARG2.GE.1.0) GO TO 600
        IF (ARG2.LE.-1.0) GO TO 601
  600   TEXT7='The star never gets above '//calt//'\\(0718)'
        ITXT7=1
        GO TO 603
  601   TEXT8='The star never gets below '//calt//'\\(0718)'
        ITXT8=1
        GO TO 603
  602   CONTINUE
        HRANG2=PIG*DACOS(ARG2)/15.0
C
        LSTR=24-HRANG2+RA
        LSTS=HRANG2+RA
        IF (LSTR.GT.24.0) LSTR=LSTR-24.0
        IF (LSTS.GT.24.0) LSTS=LSTS-24.0
        IF (LSTR.LT.0.0) LSTR=LSTR+24.0
        IF (LSTS.LT.0.0) LSTS=LSTS+24.0
C
C  LST TO GST
        CALL LGST (ALONG,LSTR,LSTS,GSTR,GSTS)
C
C  GST TO GMT
        CALL GGMT (T0(j),GSTR,GSTS,GMTR,GMTS)
        YPL3(K)=GMTR
        YPL4(K)=GMTS
C
  603   CONTINUE
C
        XPL1(K)=DYNO
        K=K+1

      END DO
      NP=K-1
      GO TO 503
C
C  TEXT HEADER
  500 TEXT1='The star never rises'
      ITXT1=1
  503 CONTINUE
      WRITE(TEXT3,50) RAch,DECch
   50 FORMAT('RA: ',a,'   DEC: ',A)
      WRITE (TEXT4,40) obsnam
   40 FORMAT(A)
      WRITE (TEXT5,51) JR
   51 FORMAT('Year ',I4)
      WRITE (TEXT6,52) name
   52 FORMAT('Visibility of ',A)
C
C  BEGINNING AND END OF TWILIGHT
c solar constants
      ECL90=279.403303
      ECLPER=282.768422
      ECC=0.016713
      OBL=23.441884*PIR
c lunar constants
      l0=318.351648d0
      p0=36.340410d0
      n0=318.510107d0
      inc=5.145396d0*pir
      emoon=0.054900d0
      th0=0.5181d0*pir
      pi0=0.9507d0*pir
c
      L=1
      ll=1
      call juld(1990,1,0.0,jd1990)
      DO J=1,ENDYR,1
C
C  FIND GEOCENTRIC ECLIPTIC LONGITUDE OF SUN (sect 46)
        call juld(jr,1,0.0,d)
        D=d-jd1990+j
        N=360*D/365.242191
  700   CONTINUE
        IF (N.LT.0.0) N=N+360.0
        IF (N.LT.0.0) GO TO 700
  701   CONTINUE
        IF (N.GT.360.0) N=N-360.0
        IF (N.GT.360.0) GO TO 701
        M=N+ECL90-ECLPER
        IF (M.LT.0.0) M=M+360.0
        M=M*PIR
        EC=360*ECC*DSIN(M)/PI
        LATSUN=N+EC+ECL90
        IF (LATSUN.GT.360.0) LATSUN=LATSUN-360.0
        DYNO=J
C
C  FIND RA,DEC OF SUN
	latsun=latsun*pir
	call eclip_equat(latsun,0.d0,rasun,decsun)
	rasun=rasun*pig/15.d0	! in hrs
C
C  SUNRISE AND SUNSET NEGLECTING SUNS APPARENT MOTION
	TWILGHT_FLG(L) = .FALSE.
        ARG1=-DTAN(ALAT)*DTAN(DECSUN)
        IF (ARG1.GE.1.0) GO TO 900
        IF (ARG1.LE.-1.0) GO TO 900
        HRANG1=PIG*DACOS(ARG1)/15.0
        LSTR=24-HRANG1+RASUN
        LSTS=HRANG1+RASUN
C
C  LST TO GST
        CALL LGST (ALONG,LSTR,LSTS,GSTR,GSTS)
C
C  GST TO GMT
        CALL GGMT (T0(j),GSTR,GSTS,GMTR,GMTS)
C
C  HOUR ANGLE AT SETTING WHEN SUN'S ZENITH DISTANCE=108 DEGREES
        ARG2=(DCOS(108*PIR)-DSIN(ALAT)*DSIN(DECSUN))/(DCOS(ALAT)*DCOS(DE
     *CSUN))
        IF (ARG2.GT.1.0) GO TO 900
        IF (ARG2.LT.-1.0) GO TO 900
        HRANG2=PIG*DACOS(ARG2)/15.0
C
C  START AND END OF TWILIGHT
        DT=(HRANG2-HRANG1)*0.997270
        GMTR=GMTR-DT
        GMTS=GMTS+DT
        IF (GMTR.GT.24.0) GMTR=GMTR-24.0
        IF (GMTS.GT.24.0) GMTS=GMTS-24.0
        IF (GMTR.LT.0.0) GMTR=GMTR+24.0
        IF (GMTS.LT.0.0) GMTS=GMTS+24.0
        YPL5(L)=GMTR
        YPL6(L)=GMTS
        XPL2(L)=DYNO
        TWILGHT_FLG(L) = .TRUE.
	IF (L.EQ.1) TWILGHT = TWILGHT_FLG(L)
  900 CONTINUE

C calculate validity of twilight points+add extra points for start/stop
	IF (L.EQ.1) TWILGHT = TWILGHT_FLG(L)
	IF (.not.TWILGHT_FLG(L)) THEN
	  YPL5(L)=0
	  YPL6(L)=0
	  XPL2(L)=DYNO
	ENDIF
	IF ((L.NE.1).and.TWILGHT_FLG(L).and.(.not.TWILGHT)) THEN
	  TWILGHT_FLG(L-1) = .TRUE.
	  TWILGHT = .TRUE.
	ENDIF
	IF (.not.TWILGHT_FLG(L).and.TWILGHT) THEN
	  TWILGHT_FLG(L) = .TRUE.
	  TWILGHT = .FALSE.
	ENDIF
	L=L+1

c Moonrise and moonset (sect 70)
c Find position of moon (sect 65)
	SM=DSIN(M)
	LONG=13.1763966D0*D+L0
	CALL RANGE(LONG,360.D0)
	MM=LONG-0.111404D0*D-P0
	CALL RANGE(MM,360.D0)
	N=N0-0.0529539D0*D
	CALL RANGE(N,360.D0)
	LONGR=LONG*PIR
	MMR=MM*PIR
	EV=1.2739D0*DSIN(2.D0*(LONGR-LATSUN)-MMR)
	AE=0.1858D0*SM
	A3=0.37D0*SM
	MMPR=MM+EV-AE-A3
	MMPR=MMPR*PIR
	EC=6.2886D0*DSIN(MMPR)
	A4=0.214D0*DSIN(2.D0*MMPR)
	LPR=LONG+EV+EC-AE+A4
	LPR=LPR*PIR
	V=0.6583D0*DSIN(2.D0*(LPR-LATSUN))
	V=V*PIR
	LPRPR=LPR+V
	NPR=N-0.16D0*SM
	NPR=NPR*PIR
	Y=DSIN(LPRPR-NPR)*DCOS(INC)
	X=DCOS(LPRPR-NPR)
	LM1=DATAN2(Y,X)+NPR
	BM1=DASIN(DSIN(LPRPR-NPR)*DSIN(INC))

c convert ecliptic lunar posn to ra, dec
	CALL ECLIP_EQUAT(LM1,BM1,RA1,DEC1)
	RA1=RA1*PIG/15.D0

C Find distance between star & moon (sect 32)
	DA=PIR*(RA1-RA)*15.D0
	CD=DSIN(DEC1)*DSIN(DEC)+DCOS(DEC1)*DCOS(DEC)*DCOS(DA)
	DIST(LL)=DACOS(CD)*PIG/15.0

c Find posn of moon 12 hrs later (sect 66)
	DB=0.05D0*DCOS(LPRPR-NPR)
	DL=0.55D0+0.06D0*DCOS(MMPR)
	LM2=LM1+DL*12.D0*PIR
	BM2=BM1+DB*12.D0*PIR
	CALL ECLIP_EQUAT(LM2,BM2,RA2,DEC2)
	RA2=RA2*PIG/15.D0

c Get LST rise and setting times
	ARG1=-DTAN(ALAT)*DTAN(DEC1)
	IF(ARG1.GE.1.0.OR.ARG1.LE.-1.)GOTO 1000
	HRANG1=PIG*DACOS(ARG1)/15.D0
	LSTR1=24.D0-HRANG1+RA1
	LSTS1=HRANG1+RA1
	CALL RANGE(LSTR1,24.D0)
	CALL RANGE(LSTS1,24.D0)
	ARG2=-DTAN(ALAT)*DTAN(DEC2)
	IF(ARG2.GE.1.0.OR.ARG2.LE.-1.)GOTO 1000
	HRANG2=PIG*DACOS(ARG2)/15.D0
	LSTR2=24.D0-HRANG2+RA2
	LSTS2=HRANG2+RA2
	CALL RANGE(LSTR2,24.D0)
	CALL RANGE(LSTS2,24.D0)

c Lst to gst
	CALL LGST(ALONG,LSTR1,LSTS1,GSTR1,GSTS1)
	CALL LGST(ALONG,LSTR2,LSTS2,GSTR2,GSTS2)
	IF(GSTR1.GT.GSTR2)GSTR2=GSTR2+24.D0
	IF(GSTS1.GT.GSTS2)GSTS2=GSTS2+24.D0
c
	T00PR=T0(J)-1.002738D0*ALONG
	IF(T00PR.LT.0.D0)T00PR=T00PR+24.D0
	IF(GSTR1.LT.T00PR)THEN
	  GSTR1=GSTR1+24.D0
	  GSTR2=GSTR2+24.D0
	ENDIF
	IF(GSTS1.LT.T00PR)THEN
	  GSTS1=GSTS1+24.D0
	  GSTS2=GSTS2+24.D0
	ENDIF
	GSTR=(12.03D0*GSTR1-T0(J)*(GSTR2-GSTR1))/(12.03D0+GSTR1-GSTR2)
	GSTS=(12.03D0*GSTS1-T0(J)*(GSTS2-GSTS1))/(12.03D0+GSTS1-GSTS2)
c calculate corrections
	DECPR=(DEC1+DEC2)/2.D0
c1-paralax(sect 69)
	EC=EC*PIR
	RHOPR=(1.D0-EMOON**2)/(1.D0+EMOON*DCOS(MMPR+EC))
	THETA=TH0/RHOPR
	PARA=PI0/RHOPR
c2-refraction(sect 37)
	PSI=DACOS(DSIN(ALAT)/DCOS(DECPR))
	X=-PARA+THETA/2.D0+PIR*34.D0/60.D0
	Y=DASIN(DSIN(X)/DSIN(PSI))
	DELT=PIG*240.D0*Y/DCOS(DECPR)
	DELT=DELT/3600.D0
	GSTR=GSTR-DELT
	GSTS=GSTS+DELT
c gst to gmt
	CALL GGMT(T0(J),GSTR,GSTS,GMTR,GMTS)
	CALL RANGE(GMTR,24.D0)
	CALL RANGE(GMTS,24.D0)
	YPL7(LL)=GMTR
	YPL8(LL)=GMTS
	XPL3(LL)=DYNO

c find fractional illumination of moon (sect 67)
	F(LL)=0.5*(1.-DCOS(LPRPR-LATSUN))
C
	LL=LL+1
 1000	CONTINUE
      END DO
      NL=L-1
      NLL=LL-1
C
C  PLOT
      INCH=25.40
c for ws INCH=33.0
      VALUE = 'CLOSED'
      DO WHILE (VALUE.NE.'OPEN')
         CALL PAR_GET0C('DEVICE',DEVICE,STATUS)
         CALL PAR_CANCL('DEVICE',STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
	 IF (DEVICE.EQ.'?') THEN
	    CALL PGLDEV()
         ELSE
            CALL PGBEGIN(0,DEVICE,1,1)
	    CALL PGQINF('STATE',VALUE,LENGTH)
         ENDIF
      ENDDO

C
C---  PRINT A STATUS MESSAGE
      CALL MSG_OUT(' ','Plotting Visibility graph.',STATUS)

C
C Initialise window coordinates
      CALL PGVSTAND()
      CALL PGQVP(1,XV1,XV2,YV1,YV2)
      CALL PGVSIZE(XV1, XV2 - 1.5, YV1, YV2 - 0.3)
      IF (ILEAP.EQ.0) CALL PGWINDOW(0.0,365.0,0.0,24.0)
      IF (ILEAP.EQ.1) CALL PGWINDOW(0.0,366.0,0.0,24.0)
      CALL PGBOX('BC',0.0,0,'BC',0.0,0)
      CALL PGSCH(1.2)
      CALL PGSCF(2)
C
C  WRITE HEADER
      CALL PGTEXT(0.0,27.2,TEXT6)
      CALL PGTEXT(200.0,27.2,TEXT4)
      CALL PGSCH(1.0)
      CALL PGTEXT(0.0,26.0,TEXT5)
      CALL PGTEXT(0.0,25.0,TEXT3)
      IF (ITXT1.EQ.1) CALL PGTEXT(200.0,26.0,TEXT1)
      IF (ITXT2.EQ.1) CALL PGTEXT(200.0,26.0,TEXT2)
      IF (ITXT7.EQ.1) CALL PGTEXT(200.0,25.0,TEXT7)
      IF (ITXT8.EQ.1) CALL PGTEXT(200.0,25.0,TEXT8)
      IF (ALAT.GT.1.1519) THEN
        CALL PGTEXT(320.0,26.0,'Observatory above northern')
        CALL PGTEXT(320.0,25.0,'polar circle')
      ELSEIF (ALAT.LT.-1.1519) THEN
        CALL PGTEXT(320.0,26.0,'Observatory below southern')
        CALL PGTEXT(320.0,25.0,'polar circle')
      ENDIF
      CALL PGSCH(0.8)
      CALL PGPTEXT(415.0,0.0,90.0,0.0,'\\(2262) star rises')
      CALL PGPTEXT(415.0,5.0,90.0,0.0,'\\(2264) star sets')
      CALL PGPTEXT(415.0,10.0,90.0,0.0,'\\(0901) star gets above '
     *//calt//'\\(0718)')
      CALL PGPTEXT(415.0,18.0,90.0,0.0,'\\(0850) star gets below '
     *//calt//'\\(0718)')
      CALL PGSCF(1)
      CALL PGSLS(2)
      CALL PGSCH(0.65)
      CALL PGPTEXT(427.0,0.0,90.0,0.0,'-')
      CALL PGPTEXT(427.0,0.3,90.0,0.0,'X')
      CALL PGPTEXT(427.0,0.6,90.0,0.0,'-')
      CALL PGSCF(2)
      CALL PGSLS(1)
      CALL PGSCH(0.8)
      CALL PGPTEXT(427.0,1.05,90.0,0.0,'twilight ends')
      CALL PGSCF(1)
      CALL PGSLS(2)
      CALL PGSCH(0.65)
      CALL PGPTEXT(427.0,7.0,90.0,0.0,'-')
      CALL PGPTEXT(427.0,7.3,90.0,0.0,'V')
      CALL PGPTEXT(427.0,7.6,90.0,0.0,'-')
      CALL PGSCF(2)
      CALL PGSLS(1)
      CALL PGSCH(0.8)
      CALL PGPTEXT(427.0,8.05,90.0,0.0,'twilight begins')
      CALL PGPTEXT(427.0,14.5,90.0,0.0,'- - g.c. moon-star dist. (hrs)')
      CALL PGPTEXT(439.0,0.0,90.0,0.0,'moon above horizon:')
      CALL PGPTEXT(439.0,7.5,90.0,0.0,
     *       'thin line - moon 25%-50% illuminated')
      CALL PGPTEXT(449.0,7.5,90.0,0.0,
     *       'medium line - moon 50%-75% illuminated')
      CALL PGPTEXT(459.0,7.5,90.0,0.0,
     *       'thick line - moon 75%-100% illuminated')
      CALL PGSCH(1.0)
      CALL PGSCF(2)
C
C  TICKMARKS X-AXIS
      DO I=1,12
        CALL PGSLS(1)
        XTICK1(1)=NDAY(I)*1.0
        XTICK1(2)=XTICK1(1)
        XTICK2(1)=NDAY(I)+MDAY(I)/2.0
        XTICK2(2)=XTICK2(1)
        YTICK1(1)=0.0
        YTICK1(2)=0.4
        YTICK2(1)=0.0
        YTICK2(2)=0.25
        CALL PGLINE(2,XTICK1,YTICK1)
        CALL PGLINE(2,XTICK2,YTICK2)
        YTICK1(1)=24.0
        YTICK1(2)=23.6
        YTICK2(1)=24.0
        YTICK2(2)=23.75
        CALL PGLINE(2,XTICK1,YTICK1)
        CALL PGLINE(2,XTICK2,YTICK2)
        CALL PGSLS(4)
        YTICK1(1)=0.4
        YTICK1(2)=23.6
        CALL PGLINE(2,XTICK1,YTICK1)
C
C  TICKMARKS Y-AXIS
        CALL PGSLS(1)
        XTICK1(1)=0.0
        XTICK1(2)=4.5
        XTICK2(1)=0.0
        XTICK2(2)=2.8
        YTICK1(1)=2.0+(I-1)*2.0
        YTICK1(2)=YTICK1(1)
        YTICK2(1)=YTICK1(1)-1.0
        YTICK2(2)=YTICK2(1)
        CALL PGLINE(2,XTICK1,YTICK1)
        CALL PGLINE(2,XTICK2,YTICK2)
        IF (ILEAP.EQ.0) THEN
          XTICK1(1)=361.5
          XTICK1(2)=365.0
          XTICK2(1)=362.5
          XTICK2(2)=365.0
        ELSEIF (ILEAP.EQ.1) THEN
          XTICK1(1)=362.5
          XTICK1(2)=366.0
          XTICK2(1)=363.5
          XTICK2(2)=366.0
        ENDIF
        YTICK1(1)=2.0+(I-1)*2.0
        YTICK1(2)=YTICK1(1)
        YTICK2(1)=YTICK1(1)-1.0
        YTICK2(2)=YTICK2(1)
        CALL PGLINE(2,XTICK1,YTICK1)
        CALL PGLINE(2,XTICK2,YTICK2)
        CALL PGSLS(4)
        XTICK1(1)=1.0
        IF (ILEAP.EQ.0) XTICK1(2)=364.0
        IF (ILEAP.EQ.1) XTICK1(2)=365.0
        YTICK1(1)=2.0+(I-2)*2.0
        YTICK1(2)=YTICK1(1)
        CALL PGLINE(2,XTICK1,YTICK1)
      END DO
      CALL PGSLS(1)
C
C  LABEL X-AXIS
      CALL PGSCH(0.8)
      YDELT=-0.95
      DO I=1,12
        JUNK=MONS(I)
        XLAB=JUNK(1:3)
        CALL PGTEXT(NDAY(I)+8.0,YDELT,XLAB)
      END DO
C
C  LABEL Y-AXIS
      XDELT = 365.0+3.0
      DO I=1,13
	LTIME = ((I-1)*2) + 12
	IF (LTIME.GT.24.0) THEN
	  LTIME = LTIME - 24
	ENDIF
	WRITE(YLAB,'(I3)') LTIME
	CALL PGTEXT(XDELT,-0.15+(I-1)*2.0,YLAB)
      ENDDO

      XDELT = -16.5
      DO I=1,13
	LTIME = ((I-1)*2) + 12 - DELTA
	IF (LTIME.gt.24.0) THEN
	  LTIME = LTIME - 24
	ENDIF
	WRITE(YLAB,'(I3)') LTIME
        IF(I.EQ.1) TIMO=LTIME
        CALL PGTEXT(XDELT,-0.15+(I-1)*2.0,YLAB)
      END DO

      CALL PGSCH(1.2)
      CALL PGPTEXT(-21.75,11.4,90.0,0.0,'UT')
      CALL PGPTEXT(395.0,9.5,90.0,0.0,'Local Time')
      CALL PGSCH(1.0)
C
C  PLOT RISE AND SETTING TIMES
      CALL PGSLW(2)
      CALL PGSCH(1.0)
      CALL CYARR(DELTA,NP,YPL1)
      CALL CYARR(DELTA,NP,YPL2)
      IF (ITXT1.NE.1.AND.ITXT2.NE.1) THEN
        CALL PANNLINE(NP,XPL1,YPL1,30,.false.,.false.)
        CALL PANNLINE(NP,XPL1,YPL2,31,.false.,.false.)
      ENDIF
C
C  PLOT TIMES WHEN STAR IS ABOVE ialt DEGREES
      CALL CYARR(DELTA,NP,YPL3)
      CALL CYARR(DELTA,NP,YPL4)
      IF (ITXT7.NE.1.AND.ITXT8.NE.1) THEN
        CALL PGSCH(1.2)
        CALL PANNLINE(NP,XPL1,YPL3,21,.false.,.false.)
        CALL PANNLINE(NP,XPL1,YPL4,17,.false.,.false.)
      ENDIF
C
C  PLOT BEGINNING AND END TIMES OF TWILIGHT
      CALL CYARR(DELTA,NL,YPL5)
      CALL CYARR(DELTA,NL,YPL6)
      CALL PGSCH(0.8)
      CALL PGSCF(1)
      CALL PANNLINE(NL,XPL2,YPL5,ICHAR('V'),TWILGHT_FLG,.true.)
      CALL PANNLINE(NL,XPL2,YPL6,ICHAR('X'),TWILGHT_FLG,.true.)
C
C  PLOT moon rise and setting times, line width gives phase
      CALL PGSCH(0.7)
      CALL PGSCF(1)
      CALL CYARR(DELTA,NLL,YPL7)
      CALL CYARR(DELTA,NLL,YPL8)
      DO i=1,nll,2
	  jj=int(4.*f(i))

	  if(jj.eq.4)jj=3

c	  if(jj.eq.3)jj=4

	  if(jj.ne.0)then
	    jj=jj*2
	    call pgslw(jj)
	    xp(1)=xpl3(i)
	    xp(2)=xpl3(i)
	    if(ypl7(i).gt.ypl8(i))then
	      yp(1)=ypl7(i)
	      yp(2)=24.0
	      call pgline(2,xp,yp)
	      yp(1)=0.
	      yp(2)=ypl8(i)
	      call pgline(2,xp,yp)
	    else
	      yp(1)=ypl7(i)
	      yp(2)=ypl8(i)
	      call pgline(2,xp,yp)
	    endif
	  endif
	ENDDO
c
c  plot moon - star distance dashed
      CALL PGSLS(2)
      CALL PGSLW(1)
      CALL CYARR(0,NLL,DIST)
      CALL PGLINE(NLL,XPL3,DIST)
C
      CALL PGSLS(1)
C
C---  ASK IF A TEXT FILE IS TO BE CREATED.

      CALL PAR_GET0L('TEXTF',TEXTF,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

C   Create an output text file as required.
      IF (TEXTF) THEN

*      Determine the output text file name. If the file name chosen fails,
*      the user is reprompted
         OPENF=.FALSE.
         EXCLAIM=.FALSE.
         CALL ERR_MARK
         DO WHILE((.NOT.OPENF).AND.(.NOT.EXCLAIM)
     :             .AND.(STATUS.EQ.SAI__OK))
            CALL OBS_AIF_ASFIO('OUT','WRITE','LIST',128,FIOD,
     :                          OPENF,EXCLAIM,STATUS)
            IF ((.NOT.OPENF).AND.(.NOT.EXCLAIM)) THEN
               CALL ERR_REP(' ','Bad file name.',STATUS)
               CALL ERR_REP(' ','For no file, type !',STATUS)
               CALL ERR_ANNUL(STATUS)
            END IF
         END DO
         CALL ERR_RLSE
         IF (STATUS.NE.SAI__OK) GOTO 999

C      Headings.
         CALL FIO_WRITE(FIOD,SPACE,STATUS)
         JUNK='TEXT OUTPUT FROM OBSERVE'
         IF (STATUS.NE.SAI__OK) GOTO 999

         NCHAR=CHR_LEN(JUNK)
         CALL FIO_WRITE(FIOD,JUNK(1:NCHAR),STATUS)
         CALL FIO_WRITE(FIOD,SPACE,STATUS)
         CALL FIO_WRITE(FIOD,SPACE,STATUS)
         CALL FIO_WRITE(FIOD,SPACE,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

C      Object name.
         JUNK='Object selected was: '//name
         NCHAR=CHR_LEN(JUNK)
         CALL FIO_WRITE(FIOD,JUNK(1:NCHAR),STATUS)
         CALL FIO_WRITE(FIOD,SPACE,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

C      Object position.
         JUNK='Located at RA:       '//RACH
         NCHAR=CHR_LEN(JUNK)
         CALL FIO_WRITE(FIOD,JUNK(1:NCHAR),STATUS)
         JUNK='Located at Dec:      '//DECCH
         NCHAR=CHR_LEN(JUNK)
         CALL FIO_WRITE(FIOD,JUNK(1:NCHAR),STATUS)
         CALL FIO_WRITE(FIOD,SPACE,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

C      Observatory name.
         JUNK='From:                '//obsnam
         NCHAR=CHR_LEN(JUNK)
         CALL FIO_WRITE(FIOD,JUNK(1:NCHAR),STATUS)
         CALL FIO_WRITE(FIOD,SPACE,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

C      Year.
         CALL CHR_ITOC(JR,C4,I)
         JUNK='During:              '//C4
         NCHAR=CHR_LEN(JUNK)
         CALL FIO_WRITE(FIOD,JUNK(1:NCHAR),STATUS)
         CALL FIO_WRITE(FIOD,SPACE,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

C      Times + date.
         LMONTH=0
         DO 201 K=1,NP

C         Find the current month and day thereof.
            CALL DATEC(MDAY,K,DAY,MONTH,DYS,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 999

C         Output month and column heading.
            IF (MONTH.NE.LMONTH) THEN

C            Month.
               CALL FIO_WRITE(FIOD,SPACE,STATUS)
               CALL FIO_WRITE(FIOD,SPACE,STATUS)
               JUNK=MONS(MONTH)
               LMONTH=MONTH
               CALL FIO_WRITE(FIOD,JUNK,STATUS)
               CALL FIO_WRITE(FIOD,SPACE,STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 999

C            Heading.
               JUNK='Day     Rise      Set       Above '//
     :         '30  Below 30    Twi. beg  Twi. end    '//
     :         'Moon rise Moon set    '//
     :         'Moon dist  Moon phase'
               NCHAR=CHR_LEN(JUNK)
               CALL FIO_WRITE(FIOD,JUNK(1:NCHAR),STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 999

            END IF

C         Get value for moon illumination.
            J=NINT(F(K)*100.)
            CALL CHR_ITOC(J,STRA(10),I)

C         Create time strings.
            CALL CONTIM(TIMO,YPL1(K),STRA(1),STATUS)
            CALL CONTIM(TIMO,YPL2(K),STRA(2),STATUS)
            CALL CONTIM(TIMO,YPL3(K),STRA(3),STATUS)
            CALL CONTIM(TIMO,YPL4(K),STRA(4),STATUS)
            CALL CONTIM(TIMO,YPL5(K),STRA(5),STATUS)
            CALL CONTIM(TIMO,YPL6(K),STRA(6),STATUS)
            CALL CONTIM(TIMO,YPL7(K),STRA(7),STATUS)
            CALL CONTIM(TIMO,YPL8(K),STRA(8),STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 999

C         Get value for moon-object distance.
            RVALUE=0.0E0
            CALL CONTIM(RVALUE,DIST(K),STRA(9),STATUS)

C         Create output string.
            JUNK=' '//DYS//'   '//STRA(1)//STRA(2)//'  '
     :           //STRA(3)//STRA(4)//'  '
     :           //STRA(5)//STRA(6)//'  '
     :           //STRA(7)//STRA(8)//'  '
     :           //STRA(9)//'    '//STRA(10)

C         Output the string.
            NCHAR=CHR_LEN(JUNK)
            CALL FIO_WRITE(FIOD,JUNK(1:NCHAR),STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 999

  201    CONTINUE

C      Close the file down.
         CALL FIO_WRITE(FIOD,SPACE,STATUS)
         CALL FIO_CLOSE(FIOD,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

      END IF

C
C---  CLOSE THE GRAPHICS PORT
      CALL PGEND

C
C---  RUN THROUGH THE PROGRAM AGAIN
      CALL PAR_GET0C('AGAIN',AGAIN,STATUS)
      CALL PAR_CANCL('AGAIN',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

C
C---  TEST THE "AGAIN" RESPONSE
      CALL CHR_UCASE(AGAIN)
      GET_TELE = ((AGAIN.EQ.'T').OR.(AGAIN.EQ.'B'))
      GET_STAR = ((AGAIN.EQ.'S').OR.(AGAIN.EQ.'B'))
      IF (AGAIN.NE.'Q') GOTO 800

999   CONTINUE
      IF (STATUS.NE.SAI__OK) THEN
	 CALL ERR_REP(' ',VERSION,STATUS)
      ENDIF

      END


C
C
C  SUBROUTINE TO CALCULATE JD
C  *********************************************************************
C
      SUBROUTINE JULD (JYEAR,JMON,JDAY,JLD)
c
C  AD dates only (sect 4)
c
      INTEGER AS,BS,DS,jmon,i1,i2,jyear
      INTEGER CS
      REAL JDAY
      DOUBLE PRECISION JLD
C
      IF (JMON.LE.2) THEN
        I1=JYEAR-1
        I2=JMON+12
      ENDIF
      AS=INT(I1/100)
      BS=2-AS+INT(AS/4)
      CS=INT(365.25*I1)
      DS=INT(30.6001*(I2+1))
      JLD=BS+CS+DS+JDAY+1720994.5
C
      RETURN
      END
C
C
C  SUBROUTINE TO CONVERT LST TO GST
C  *********************************************************************
C
      SUBROUTINE LGST (LONGH,LTIM1,LTIM2,GTIM1,GTIM2)
C
      DOUBLE PRECISION LONGH,LTIM1,LTIM2,GTIM1,GTIM2
C
      GTIM1=LTIM1-LONGH
      GTIM2=LTIM2-LONGH
      IF (GTIM1.GT.24.0) GTIM1=GTIM1-24.0
      IF (GTIM2.GT.24.0) GTIM2=GTIM2-24.0
      IF (GTIM1.LT.0.0) GTIM1=GTIM1+24.0
      IF (GTIM2.LT.0.0) GTIM2=GTIM2+24.0
C
      RETURN
      END
C
C
C  SUBROUTINE TO CONVERT GST TO GMT
C  *********************************************************************
C
      SUBROUTINE GGMT (PAR,GSTI1,GSTI2,GMTI1,GMTI2)
C
      DOUBLE PRECISION PAR,GSTI1,GSTI2,GMTI1,GMTI2
C
      GSTI1=GSTI1-PAR
      GSTI2=GSTI2-PAR
      IF (GSTI1.LT.0.0) GSTI1=GSTI1+24.0
      IF (GSTI2.LT.0.0) GSTI2=GSTI2+24.0
      GMTI1=GSTI1*0.9972695663
      GMTI2=GSTI2*0.9972695663
C
      RETURN
      END
c
c
c  subroutine to ensure variable lies within 0. to r
c  *********************************************************************
c
	subroutine range (z,r)
	real*8 z,y,r
	integer iy
	y=z/r
	iy=int(y)
	if(iy.gt.0)then
	  z=z-iy*r
	elseif(y.lt.0.)then
	  z=z+(-iy+1)*r
	endif
	if(z.eq.r)z=0.d0
	return
	end
c
c
c subroutine to convert ecliptic to equatorial coords (sect 27)
c ************************************************************************
c
	subroutine eclip_equat(lat,b,ra,dec)
c  lat, b i/p in rads
c  ra, dec o/p in rads
	real*8 lat,b,ra,dec,pi,pir,y,x,sd,obl
	PI=3.141592654d0
	PIR=PI/180.d0
        OBL=23.441884*PIR
        sd=dsin(b)*dcos(obl)+dcos(b)*DSIN(OBL)*DSIN(LAT)
        DEC=DASIN(sd)
        Y=DSIN(LAT)*DCOS(OBL)-dtan(b)*dsin(obl)
        X=DCOS(LAT)
        RA=DATAN2(Y,X)
        IF (Y.LT.0.0) RA=2.0*PI+RA
	return
	end
c
c subroutine to convert character string to degrees
c*****************************************************************************
c

	subroutine chpos(c,pos,ierr)
	character*12 c
	real*8 pos,pos3
	integer i1,i2,i3,pos1,pos2,ierr
	i1=1
	do while (i1.le.12.and.c(i1:i1).ne.' ')
	  i1=i1+1
	end do
	read(c(:i1-1),*,iostat=ierr,err=999)pos1
	i2=i1+1
	do while (i2.le.12.and.c(i2:i2).ne.' ')
	  i2=i2+1
	end do
	read(c(i1+1:i2-1),*,iostat=ierr,err=999)pos2
	i3=i2+1
	do while (i3.le.12.and.c(i3:i3).ne.' ')
	  i3=i3+1
	end do
	read(c(i2+1:i3),*,iostat=ierr,err=999)pos3
	pos=abs(pos1)+dfloat(pos2)/60.d0+pos3/3600.d0
	if(c(:1).eq.'-')pos=-pos
999	return
	end

c
c
c converts 0-24hrs UT(midnight-midnight)PGPLOT Y array to 12-12hrs LT(noon-noon)
c*****************************************************************************
c
	subroutine cyarr(delta,N,Ycarr)
	real*4 Ycarr(*)
	integer*4 N,i,delta

	do i=1,N
	    ycarr(i) = cy(delta,ycarr(i))
	enddo
	return
	end
c
c
c converts 0-24hrs UT(midnight-midnight)PGPLOT Y coord to 12-12hrs LT(noon-noon)
c*****************************************************************************
c
	function cy(delta,Ycoord)
	real*4 Ycoord,CYcoord
	integer*4 delta

C
C CONVERT UT INTO LOCAL TIME
	Ycoord = Ycoord + delta
	if (Ycoord.gt.24.0) THEN
	   Ycoord = Ycoord - 24.0
	elseIF (CYcoord.lt.0.0) THEN
	   Ycoord = Ycoord + 24.0
	endif
C
C CONVERT (24-12-24) TO (12-24-12)
	if ((Ycoord.le.24.0).and.(Ycoord.gt.12.0)) then
	   CYcoord = Ycoord - 12.0
	else
	   CYcoord = Ycoord + 12.0
	endif
	cy = CYcoord
	return
	end
c
c
c Draws an annotated line through points xn,yn
c*****************************************************************************
c
	subroutine PANNLINE(N,XN,YN,CHR,D_FLAG,USE_F)
	real*4 XN(*),YN(*),tx(2),ty(2)
	LOGICAL*4 D_FLAG(*),USE_F
	integer*4 N,I,CHR,a

	tx(1) = xn(1)
	ty(1) = yn(1)
	a = 10
	do i=2,N
	  tx(2) = xn(i)
	  ty(2) = yn(i)
	  IF (i.eq.a) THEN
	    a = i + 25
	    CALL PGPOINT(1,tx(2),ty(2),chr)
	    ty(2) = -99999.9
	  ELSE IF ((ty(1).lt.ty(2)+1.0).and.(ty(1).gt.ty(2)-1.0)) then
	    IF ((.NOT.USE_F).or.(D_FLAG(i))) then
              CALL PGLINE(2,tx,ty)
	    ENDIF
	  ENDIF
	  tx(1) = tx(2)
	  ty(1) = ty(2)
	enddo
	return
	end

C----------------------------------------------------------------------------
C GETOBS - GET OBSERVATORY DETAILS
C----------------------------------------------------------------------------
      SUBROUTINE GETOBS(TELESCOPE,LAT,LONG,DELTA,STATUS)
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      CHARACTER*(*) TELESCOPE
      DOUBLE PRECISION LAT,LONG,HEIGHT
      INTEGER DELTA,UT,TZONE
      CHARACTER*10 ID
      CHARACTER*40 NAME
      CHARACTER*85 LINE
      INTEGER OBSNO,COUNT,COLUMN,STATUS
      DATA ID/' '/
      DATA NAME/' '/
      DATA COLUMN/1/
      OBSNO = 1

      IF (STATUS.NE.SAI__OK) RETURN
C
C---  DISPLAY LIST OF KNOWN OBSERVATORIES
      CALL SLA_OBS(OBSNO,ID,NAME,LONG,LAT,HEIGHT)
      DO WHILE (NAME.NE.'?')
          COLUMN = MOD(OBSNO-1,3)
          WRITE(LINE(((COLUMN)*26)+1:),10) OBSNO,NAME
 10       FORMAT(I3,') ',A21)
          IF (COLUMN.EQ.2) CALL MSG_OUT(' ',LINE,STATUS)
          OBSNO = OBSNO + 1
          CALL SLA_OBS(OBSNO,ID,NAME,LONG,LAT,HEIGHT)
      END DO
      IF (COLUMN.LT.2) CALL MSG_OUT(' ',LINE,STATUS)
      COUNT = OBSNO - 1
C
C---  PROMPT FOR AN OBSERVATORY NUMBER
      CALL MSG_BLANK(STATUS)
      DO WHILE ((OBSNO.GT.COUNT).OR.(OBSNO.LT.0))
	  CALL PAR_GET0I('NUMBER',OBSNO,STATUS)
	  CALL PAR_CANCL('NUMBER',STATUS)
	  IF (STATUS.NE.SAI__OK) GOTO 999
      END DO
C
C---  FIND DETAILS OF SPECIFIED SITE
      IF (OBSNO.NE.0) THEN
          CALL SLA_OBS(OBSNO,ID,NAME,LONG,LAT,HEIGHT)
	  CALL OBSUT(OBSNO,NAME,UT,STATUS)
          TELESCOPE = NAME
	  DELTA = UT - 12
	  LONG = -LONG
      ELSE
C
C---      USER WANTS ANOTHER OBSERVATORY SITE
C---      INPUT TELESCOPE NAME
	  CALL PAR_GET0C('TELESCOPE',TELESCOPE,STATUS)
	  CALL PAR_CANCL('TELESCOPE',STATUS)
	  IF (STATUS.NE.SAI__OK) GOTO 999
C
C---      LATITUDE COORDINATE
          CALL GETCOORDS('LATITUDE',LAT,STATUS)
C
C---      LONGITUDE COORDINATE
          CALL GETCOORDS('LONGITUDE',LONG,STATUS)
C
C---      TIME DIFFERENCE FROM UT
	  CALL PAR_GET0I('TIMEZONE',TZONE,STATUS)
	  CALL PAR_CANCL('TIMEZONE',STATUS)
	  DELTA = TZONE
      ENDIF

999   CONTINUE
      IF (STATUS.NE.SAI__OK) THEN
	 CALL ERR_REP(' ','From GETOBS',STATUS)
      ENDIF
      END


C----------------------------------------------------------------------------
C GETSTAR - GET STAR DETAILS
C----------------------------------------------------------------------------
      SUBROUTINE GETSTAR(NAME,RA,DEC,YEAR,STATUS)

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      CHARACTER*(*) NAME
      INTEGER YEAR,STATUS
      DOUBLE PRECISION RA,DEC

      IF (STATUS.NE.SAI__OK) RETURN
C
C---  INPUT STAR NAME
      CALL PAR_GET0C('STAR',NAME,STATUS)
      CALL PAR_CANCL('STAR',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999
C
C---  RA COORDINATE
      CALL GETCOORDS('RA',RA,STATUS)
      RA = RA * 15
C
C---  DEC COORDINATE
      CALL GETCOORDS('DEC',DEC,STATUS)
C
C---  YEAR OF OBSERVATION
      CALL PAR_GET0I('YEAR',YEAR,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

999   CONTINUE
      IF (STATUS.NE.SAI__OK) THEN
         CALL ERR_REP(' ','From GETSTAR',STATUS)
      ENDIF

      END

C--------------------------------------------------------------------
C GETCOORDS - get coordinates
C--------------------------------------------------------------------
      SUBROUTINE GETCOORDS(IDENT,DVAL,STATUS)

      INCLUDE 'SAE_PAR'

      CHARACTER*(*) IDENT
      DOUBLE PRECISION DVAL
      INTEGER STATUS
      CHARACTER*10 CVAL
      INTEGER START

      IF (STATUS.NE.SAI__OK) RETURN

      DO WHILE (.TRUE.)
	  START = 1
	  STATUS = SAI__OK

	  CALL PAR_GET0C(IDENT,CVAL,STATUS)
	  IF (STATUS.NE.SAI__OK) GOTO 999
	  CALL PAR_CANCL(IDENT, STATUS)

	  CALL SLA_DAFIN(CVAL,START,DVAL,STATUS)
	  IF (STATUS.EQ.SAI__OK) GOTO 999
          CALL MSG_OUT(' ','Invalid coordinate.'//
     :                 'Please respecify',STATUS)
      ENDDO

999   CONTINUE
      END

C--------------------------------------------------------------------
C OBSUT - determine the UT time zone of a known telescope
C--------------------------------------------------------------------
      SUBROUTINE OBSUT(OBSNO,OBSNAME,UT,STATUS)

*  Starlink Applications Environment cosntants
      INCLUDE 'SAE_PAR'

*  Global Parameters
      INTEGER OBSNO             ! Observatory Number (as in SLA_OBS)
      CHARACTER*(*) OBSNAME     ! Observatory Name (as in SLA_OBS)
      INTEGER UT                ! UT correction for Observatory (UT+12)
      INTEGER STATUS            ! Inherited status

*  Local Parameters
      INTEGER MAXOBS            ! Maximum number of Observatories
      PARAMETER (MAXOBS = 83)
      INTEGER LL
      INTEGER OL
      CHARACTER*80 OBSINFO(MAXOBS),LINE
      CHARACTER*3 AUT           ! UT correction for Observatory (UT+12)

*  Externals

      INTEGER CHR_LEN

*  Data
*     Format for obsinfo is: obsnum+obsname+aut

      data obsinfo(1) / '1+Anglo-Australian 3.9m Telescope+22' /
      data obsinfo(2) / '2+William Herschel 4.2m Telescope+12' /
      data obsinfo(3) / '3+Isaac Newton 2.5m Telescope+12' /
      data obsinfo(4) / '4+Jacobus Kapteyn 1m Telescope+12' /
      data obsinfo(5) / '5+Lick 120 inch+4 ' /
      data obsinfo(6) / '6+MMT, Mt Hopkins+5 ' /
      data obsinfo(7) / '7+DAO Victoria BC 1.85 metre+4 ' /
      data obsinfo(8) / '8+Du Pont 2.5m Telescope, Las Campanas+8 ' /
      data obsinfo(9) / '9+Mt Hopkins 1.5 metre+5 ' /
      data obsinfo(10) / '10+Mount Stromlo 74 inch+22' /
      data obsinfo(11) / '11+Siding Spring 2.3 metre+22' /
      data obsinfo(12) / '12+Greenbank 140 foot+7 ' /
      data obsinfo(13) / '13+Cerro Tololo 4 metre+8 ' /
      data obsinfo(14) / '14+Cerro Tololo 1.5 metre+8 ' /
      data obsinfo(15) / '15+Tidbinbilla 64 metre+22' /
      data obsinfo(16) / '16+Bloemfontein 1.52 metre+14' /
      data obsinfo(17) / '17+Bosque Alegre 1.54 metre+9 ' /
      data obsinfo(18) / '18+USNO 61 inch astrograph, Flagstaff+5 ' /
      data obsinfo(19) / '19+Perkins 72 inch, Lowell:5 ' /
      data obsinfo(20) / '20+Harvard College Observatory 1.55m+7 ' /
      data obsinfo(21) / '21+Okayama 1.88 metre+21' /
      data obsinfo(22) / '22+Kitt Peak 158 inch+5 ' /
      data obsinfo(23) / '23+Kitt Peak 90 inch+5 ' /
      data obsinfo(24) / '24+Kitt Peak 84 inch+5 ' /
      data obsinfo(25) / '25+Kitt Peak 36 foot+5 ' /
      data obsinfo(26) / '26+Kottamia 74 inch+14' /
      data obsinfo(27) / '27+ESO 3.6 metre+8 ' /
      data obsinfo(28) / '28+Mauna Kea 88 inch+2 ' /
      data obsinfo(29) / '29+UK Infra Red Telescope+2 ' /
      data obsinfo(30) / '30+Quebec 1.6 metre+7 ' /
      data obsinfo(31) / '31+Mt Ekar 1.82 metre+13' /
      data obsinfo(32) / '32+Mt Lemmon 60 inch+5 ' /
      data obsinfo(33) / '33+McDonald 2.7 metre+5 ' /
      data obsinfo(34) / '34+McDonald 2.1 metre+5 ' /
      data obsinfo(35) / '35+Palomar 200 inch+4 ' /
      data obsinfo(36) / '36+Palomar 60 inch+4 ' /
      data obsinfo(37) / '37+David Dunlap 74 inch:7 ' /
      data obsinfo(38) / '38+Haute Provence 1.93 metre+13' /
      data obsinfo(39) / '39+Haute Provence 1.52 metre+13' /
      data obsinfo(40) / '40+San Pedro Martir 83 inch+4 ' /
      data obsinfo(41) / '41+Sutherland 74 inch+14' /
      data obsinfo(42) / '42+Tautenburg 2 metre+13' /
      data obsinfo(43) / '43+Catalina 61 inch+5 ' /
      data obsinfo(44) / '44+Steward 90 inch+5 ' /
      data obsinfo(45) / '45+USSR 6 metre+15' /
      data obsinfo(46) / '46+Arecibo 1000 foot+8 ' /
      data obsinfo(47) / '47+Cambridge 5km+12' /
      data obsinfo(48) / '48+Cambridge 1 mile+12' /
      data obsinfo(49) / '49+Effelsberg 100 metre+13' /
      data obsinfo(50) / '50+Greenbank 300 foot+7 ' /
      data obsinfo(51) / '51+Jodrell Bank 250 foot+12' /
      data obsinfo(52) / '52+Parkes 64 metre+22' /
      data obsinfo(53) / '53+Very Large Array+5 ' /
      data obsinfo(54) / '54+Sugar Grove 150 foot+7 ' /
      data obsinfo(55) / '55+USSR 600 foot+15' /
      data obsinfo(56) / '56+Nobeyama 45 metre+21' /
      data obsinfo(57) / '57+JCMT 15 metre+2 ' /
      data obsinfo(58) / '58+ESO 3.5 metre NTT+8 ' /
      data obsinfo(59) / '59+St Andrews+12' /
      data obsinfo(60) / '60+Apache Point 3.5m+5 ' /
      data obsinfo(61) / '61+Keck 10m Telescope #1+2 ' /
      data obsinfo(62) / '62+Tautenberg 1.34 metre Schmidt+13' /
      data obsinfo(63) / '63+Palomar 48-inch Schmidt+4 ' /
      data obsinfo(64) / '64+UK 1.2 metre Schmidt, Siding Spring+22' /
      data obsinfo(65) / '65+Kiso 1.05 metre Schmidt, Japan+21' /
      data obsinfo(66) / '66+ESO 1 metre Schmidt, La Silla+8 ' /
      data obsinfo(67) / '67+Australia Telescope Compact Array+22' /
      data obsinfo(68) / '68+ATNF Mopra Observatory+22' /
      data obsinfo(69) / '69+Subaru 8m telescope+2 ' /
      data obsinfo(70) / '70+Canada-France-Hawaii 3.6m Telescope+2 ' /
      data obsinfo(71) / '71+Keck 10m Telescope #2+2 ' /
      data obsinfo(72) / '72+Gemini North 8-m telescope+2 ' /
      data obsinfo(73) / '73+Five College Radio Astronomy Obs+7' /
      data obsinfo(74) / '74+NASA IR Telescope Facility, Mauna Kea+2 ' /
      data obsinfo(75) / '75+Caltech Sub-mm Observatory, Mauna Kea+2 ' /
      data obsinfo(76) / '76+ESO VLT, Paranal, Chile: UT1+8' /
      data obsinfo(77) / '77+ESO VLT, Paranal, Chile: UT2+8' /
      data obsinfo(78) / '78+ESO VLT, Paranal, Chile: UT3+8' /
      data obsinfo(79) / '79+ESO VLT, Paranal, Chile: UT4+8' /
      data obsinfo(80) / '80+Gemini South 8-m telescope+8' /
      data obsinfo(81) / '81+KOSMA 3m telescope, Gornergrat+13' /
      data obsinfo(82) / '82+Magellan 1, 6.5m, Las Campanas+8' /
      data obsinfo(83) / '83+Magellan 2, 6.5m, Las Campanas+8' /

C
C---  CHECK ARRAY BOUNDS ARE WITHIN RANGE
      IF (OBSNO .GT.MAXOBS .OR. OBSNO .LT.0) THEN
	  CALL MSG_OUT(' ','** Error looking up'//
     :                 ' observatory number **',STATUS)
          STATUS = SAI__ERROR
	  GOTO 999
      ENDIF
C
C---  EXTRACT INFORMATION FROM TABLE
      LINE = OBSINFO(OBSNO)
      LL = CHR_LEN(LINE)
      OL = CHR_LEN(OBSNAME)
C
C---  CHECK NAMES MATCH (USING SAME SLA_LIB VERSIONS)
      IF (INDEX(LINE,OBSNAME(1:OL)).EQ.0) THEN
	  CALL MSG_OUT(' ','** Mismatch in name lookup'//
     :                 ' for observatory **',STATUS)
	  STATUS = SAI__ERROR
	  GOTO 999
      ENDIF
C
C---  DECODE THE UNIVERSAL TIME VALUE
      AUT = LINE( INDEX(LINE(4:LL),'+')+4 :LL)
      CALL CHR_CTOI(AUT,UT,STATUS)

999   CONTINUE

C
C---  SHOW ERROR MESSAGE
      IF (STATUS.NE.SAI__OK) THEN
	 CALL MSG_OUT(' ','** Lookup table out of date,'//
     :                ' obtain new version **',STATUS)
	 CALL ERR_REP(' ','from OBSUT',STATUS)
      ENDIF

      END


C
C   Convert a fractional day value to 10 character string representation.
C
      SUBROUTINE CONTIM(OFFS,VALUE,STRING,STATUS)

*+
*  Name:
*     CONTIM

*  Purpose:
*     Create an hours:minutes:seconds string containing the time.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CONTIM(OFFS,VALUE,STRING,STATUS)

*  Description:
*     Given the fractional part of a day the routine calculates the number
*     of hours minutes and seconds that represents. It then creates a 2
*     character string of each and puts them together to make a string
*     formatted as HH:MM:SS

*  Arguments:
*     OFFS = REAL (Given)
*        Time offset.
*     VALUE = REAL (Given)
*        The fractional part of a day.
*     STRING = CHARACTER*10 (Returned)
*        The output string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     29-MAY-1996
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      REAL OFFS                       ! Offset
      REAL VALUE                      ! Temporary sum

*  Arguments Returned:
      CHARACTER *10 STRING            ! Output time string

*  Local variables:
      CHARACTER *2 P1                 ! First part of string
      CHARACTER *2 P2                 ! Second part of string
      CHARACTER *2 P3                 ! Third part of string
      INTEGER HRS                     ! Hours
      INTEGER I                       ! String length
      INTEGER MIN                     ! Minutes
      INTEGER SEC                     ! seconds

*.
      INTEGER STATUS

*    Check global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

C    Add the offset.
      VALUE=VALUE+OFFS

C    Correct range to 0 to 24.
      IF(VALUE.LT.0.0) VALUE=VALUE+24.
      IF(VALUE.GT.24.0) VALUE=VALUE-24.0

C   Convert to hours, minutes and secs.
      HRS=INT(VALUE)
      MIN=INT((VALUE-REAL(HRS))*60.)
      SEC=INT((VALUE-HRS-MIN/60.)*3600.)

C   Convert the numbers to strings.
      CALL CHR_ITOC(HRS,P1,I)
      IF (I.EQ.0) P1='00'
      IF (I.EQ.1) P1='0'//P1
      CALL CHR_ITOC(MIN,P2,I)
      IF (I.EQ.0) P2='00'
      IF (I.EQ.1) P2='0'//P2
      CALL CHR_ITOC(SEC,P3,I)
      IF (I.EQ.0) P3='00'
      IF (I.EQ.1) P3='0'//P3

C   Create output string.
      STRING=P1//':'//P2//':'//P3//'  '

      END

C
C   Convert a fractional day value to 10 character string representation.
C
      SUBROUTINE DATEC(MDAY,DAYS,DAY,MONTH,DYS,STATUS)
*+
*  Name:
*     DATE

*  Purpose:
*     Identify the day and month from the number of days into the year.
*     Also return the day of the month as a 2 char string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DATEC(MDAY,DAYS,DAY,MONTH,DYS,STATUS)

*  Description:
*     Given a certain day of the year calculates what month it is in
*     and then what day of the month it must be. Then converts that
*     day into a 2 character string form.

*  Arguments:
*     MDAYS(12) = INTEGER (Given)
*        The number of days in each month.
*     DAYS = INTEGER (Given)
*        The number of days of the year so far.
*     DAY = INTEGER (Returned)
*        Day of the current month.
*     MONTH = INTEGER (Returned)
*        Current month.
*     DYS = CHARACTER *2 (Returned)
*        Which day of the month as a string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     27-MAY-1996
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER DAYS                    ! Number of days into year
      INTEGER MDAY(12)                ! DAys in each month

*  Arguments Returned:
      CHARACTER *2 DYS                ! Day of month
      INTEGER DAY                     ! Day of month
      INTEGER MONTH                   ! Year of month

*  Local variables:
      CHARACTER *2 P1                 ! Temporary string
      INTEGER DAYT(12)                ! Days after so many month
      INTEGER I                       ! Temporary variable
      INTEGER TOTAL                   ! Temporary sum

*.

*    Check global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

C   Find totals at end of month.
      TOTAL=0
      DO 100 I=1,12
         TOTAL=TOTAL+MDAY(I)
         DAYT(I)=TOTAL
 100  CONTINUE

C   Find which month.
      DO 200 I=12,1,-1
         IF (DAYS.LE.DAYT(I)) MONTH=I
 200  CONTINUE

C   Find which day of month.
      IF (MONTH.NE.1) THEN
         DAY=DAYS-DAYT(MONTH-1)
      ELSE
         DAY=DAYS
      END IF

C   Convert the numbers to strings.
      CALL CHR_ITOC(DAY,P1,I)
      IF (I.EQ.0) DYS='00'
      IF (I.EQ.1) DYS='0'//P1
      IF (I.EQ.2) DYS=P1

      END


C
C Opens a sequential file and makes sure a ! character is treated
C properly.
C
      SUBROUTINE OBS_AIF_ASFIO(PNFILE,ACMODE,FORM,RECSZ,FD,OPEN,
     :                          EXCLAIM,STATUS)
*+
*    Description :
*
*     This routine opens a sequential file via FIO_ASSOC.  Up to four
*     attempts may be made to open the file.  If a null response is
*     supplied the file is not opened, and the flag returned indicates
*     this fact.
*
*    Invocation :
*
*      CALL OBS_AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN,
*                      EXCLAIM,STATUS)

*
*    Arguments :
*
*     PNFILE=CHARACTER*(*)
*         Parameter name by which file is to be opened
*     ACMODE=CHARACTER*(*)
*         Expression giving the required access mode.
*           Valid modes are: 'READ', 'WRITE', 'UPDATE' and 'APPEND'.
*           For details, see FIO_OPEN.
*     FORM=CHARACTER*(*)( READ )
*         Expression giving the required formatting of the file.
*           Valid formats are: 'FORTRAN', 'LIST', 'NONE' and
*           'UNFORMATTED'. For details, see FIO_OPEN.
*     RECSZ=INTEGER( READ )
*         Expression giving the maximum record size in bytes.
*           Set it to zero if the Fortran default is required.
*     FD=INTEGER( WRITE )
*         Variable to contain the file descriptor.
*     OPEN=LOGICAL( WRITE )
*         If true the file has been opened.
*     EXCLAIM=LOGICAL( WRITE )
*         If true then the user input was '!'.
*     STATUS=INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*

*     Check for error on entry - return if not o.k.
*     Initialise looping flag
*     Do while no error obtaining the name and opening the output file
*       and maximum number of attempts not exceeded
*        Get file name and open file
*        If null returned then
*           Set flag so that a log file will not be created
*           Annul the error
*           Exit from the loop
*        Else if error occurred then
*           If abort requested, do so
*           Increment loop counter
*           If maximum number of attempts not exceeded then
*              Report error
*           Else
*              Set looping flag to exit
*           Endif
*             Cancel parameter used to get filename
*        Else
*           Set flag to indicate that the file has been opened
*           Set looping flag to false
*        Endif
*     Enddo
*     If error then
*        Report and abort
*     Endif
*     Return
*
*    Bugs :
*
*     None known.
*-
*    Authors :
*
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     1989 Jul 25: Original (RL.STAR::CUR).
*     1990 Feb 20: Renamed from AIF_OPFIO (RAL::CUR).
*     1994 mar 1: Modified to return EXCLAIM (CARDIFF::GJP).
*
*    Type definitions :

      IMPLICIT  NONE           ! no implicit typing allowed

*    Global constants :
      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE  'PAR_ERR'       ! parameter-system errors

*    Import :
      CHARACTER*(*) PNFILE     ! File Parameter Name
      CHARACTER*(*) ACMODE     ! File access mode
      CHARACTER*(*) FORM       ! Required form of carriagecontrol
      INTEGER RECSZ            ! File record size

*    Export :
      LOGICAL OPEN             ! File opened successfully
      LOGICAL EXCLAIM          ! File name was exclaimation
      INTEGER FD               ! File descriptor

*    Status :
      INTEGER STATUS

*    Local Constants :
      INTEGER MXLOOP           ! Maximum number of attempts at
                               ! opening a data file
      PARAMETER ( MXLOOP=4 )

      INTEGER LOOP             ! Number of attempts to open the file

      LOGICAL LOOPAG           ! Loop again to open output file

*.

*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

      LOOP=0
      LOOPAG=.TRUE.
      OPEN=.FALSE.
      DO WHILE ( LOOPAG )

*       attempt to obtain and open a file to output listing

         CALL FIO_ASSOC( PNFILE, ACMODE, FORM, RECSZ, FD, STATUS )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            OPEN=.FALSE.
            LOOPAG=.FALSE.
            EXCLAIM=.TRUE.
            CALL ERR_ANNUL( STATUS )
         ELSE IF ( STATUS .NE. SAI__OK ) THEN

            IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

*         Here if filename is not allowed or file is not opened
*         - try again
*         Need to flush error here, as not quitting routine

            LOOP=LOOP + 1
            IF ( LOOP .LE. MXLOOP ) THEN
               CALL MSG_SETC( 'FILNAM', PNFILE )
               CALL ERR_REP( 'ERR_AIF_ASFIO_NOFI',
     :           'AIF_ASFIO: Could not open file $^FILNAM - try again',
     :           STATUS )
               CALL ERR_FLUSH( STATUS )
            ELSE

*             end looping as user is having serious problems

               LOOPAG=.FALSE.
            END IF

            CALL PAR_CANCL( PNFILE, STATUS )

         ELSE

*          no problem, so exit loop

            LOOPAG=.FALSE.
            OPEN=.TRUE.

*       end of file-opened-successfully check

         END IF
      END DO

*    abort for repeated error

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_AIF_ASFIO_NOOPEN',
     :     'AIF_ASFIO: Repeatedly unable to open a file.', STATUS )
      END IF

 999  CONTINUE

      END



