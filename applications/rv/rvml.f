      SUBROUTINE RVML (LUREP,LUINP,LUPRO,LUECH)
*+
*  - - - - -
*   R V M L
*  - - - - -
*
*  Observer radial velocity utility - main routine.
*
*  The RV utility outputs a report in which observer radial velocity
*  components for a given RA,Dec and for times throughout a given day
*  are listed.
*
*  Given:
*     LUREP    i    I/O unit; output; report (including error warnings)
*     LUINP    i    I/O unit; input; commands/data
*     LUPRO    i    I/O unit; output; prompts
*     LUECH    i    I/O unit; output; echo of input
*
*  Called:  INPUT, KTEST, TRAN, sla_OBS, sla_INTIN, sla_DFLTIN,
*           sla_DAF2R, sla_CALDJ,  sla_DTT, sla_EPJ, sla_DTF2R,
*           sla_DBJIN, sla_KBJ, sla_PRECES, sla_FK45Z, sla_DCS2C,
*           sla_MAP, sla_EQGAL, sla_EQECL,  sla_RVLSRD,  sla_RVLSRK,
*           sla_RVGALC, sla_RVLG, sla_DR2AF, sla_DJCAL, sla_DR2TF,
*           sla_GMST, sla_EVP, sla_RVEROT, sla_DVDV
*
*------------------------------------------------------------------------
*
*  RV outputs prompts soliciting the following sequence of input records:
*     Observatory Position
*     Date & number of days
*     Star Position
*
*  Observatory Position Record:
*     This is either the station identifier for use by the
*     sla_OBS routine, or a longitude and latitude.  The
*     longitude and latitude are each three numeric fields,
*     degrees, arcminutes, arcseconds;  West and North are
*     positive.
*
*  Date & Number of days record:
*     The date is three numeric fields, year, month, day,
*     and the number of days is a single numeric field.
*     THE DATE IS THE UT DATE, NOT THE LOCAL DATE, in the
*     conventional (Gregorian) calendar.
*
*  Star Position Record:
*     The star position is supplied as a mean RA,Dec in either
*     the old (FK4) or new (FK5) system.  There are seven numeric
*     fields;  fields 1-3 are the RA hours, minutes, seconds,
*     fields 4-6 are the Dec degrees, arcminutes, arcseconds,
*     and field 7 is the equinox, optionally preceded by B
*     or J.
*
*  All records are free format and, after the first iteration,
*  default to the values entered previously.  (The number of
*  days defaults to 1 first time.)  An input record beginning
*  '?' causes rudimentary help information to be output.
*
*  The program terminates if, at any point, a record beginning
*  'END' or an EOF is detected.
*
*  P.T.Wallace   Starlink   18 April 1994
*-

      IMPLICIT NONE

      INTEGER LUREP,LUINP,LUPRO,LUECH

*  Functions
      INTEGER KTEST
      REAL sla_RVLSRK,sla_RVLSRD
      REAL sla_RVGALC
      REAL sla_RVLG
      REAL sla_RVEROT
      DOUBLE PRECISION sla_DTT
      DOUBLE PRECISION sla_EPJ
      DOUBLE PRECISION sla_GMST
      DOUBLE PRECISION sla_DVDV

*  Radians to degrees
      DOUBLE PRECISION R2D
      PARAMETER (R2D=57.29577951308232087679815D0)

*  AU to km and sec
      DOUBLE PRECISION AUKM,AUSEC
      PARAMETER (AUKM=149.597870D6,AUSEC=499.004782D0)

*  Variables
      CHARACTER INREC*80,IDENT*10,NAME*40,JORB*1
      CHARACTER SLONG*1,SLAT*1
      CHARACTER SR*1,SD*1,SR2*1,SD2*1,SRA*1,SDA*1

      INTEGER JFTF,J,N,I,IWD,JW,IWM,IPD,JP,IPM
      INTEGER IY,IM,ID,NDTEMP,ND,IRD,JR,IRM,IDD,JD,IDM
      INTEGER JB,LONG(4),LAT(4),IR(4),IDC(4)
      INTEGER IR2(4),ID2(4),IRA(4),IDA(4),JDAY
      INTEGER IYMDF(4),J30M,NUTH,NUTM

      REAL R2S,D2S

      DOUBLE PRECISION W,P,H,WS,WTEMP,PS,PTEMP,SINP,COSP
      DOUBLE PRECISION DJTEMP,DJM0,DJM1,DELTAT,TDBMID
      DOUBLE PRECISION RS,RTEMP,DS,DTEMP,ETEMP
      DOUBLE PRECISION R,D,E,R2,D2,RW,DW,V2(3),RA,DA,SIND,COSD
      DOUBLE PRECISION GL,GB,EL,EB,VCLSRD,VCLSRK,VCGALC,VCLG
      DOUBLE PRECISION TDB,UT,ST,HA,COSZ,ZD
      DOUBLE PRECISION DVB(3),DPB(3),DVH(3),DPH(3)
      DOUBLE PRECISION VCROT,VCORB
      DOUBLE PRECISION RVTOT1,RVTOT2,RVTOT3,RVTOT4,RVTOT5,TL




*
*  Preliminaries
*  -------------

*  Set first time flag
      JFTF=1

*  Announcement
      WRITE (LUPRO,'(/,1X,''*  Radial Velocity Components'')')
      WRITE (LUPRO,'(/,1X,''(Enter ? for help, END to terminate)''/)')

*
*  Start of main loop
*  ------------------

 100  CONTINUE

*
*  Observatory record
*  ------------------

*  Prompt and input
      WRITE (LUPRO,'(/1X,''Observatory?'//
     :    '  (d m s (West)  d m s (North), or name - or END)'')')
      WRITE (LUPRO,'(1X,''> '',$)')
      CALL INPUT(LUINP,LUECH,INREC,J)

*  Check for termination or help request
      IF (J.EQ.-1) GO TO 9000
      IF (J.NE.1) GO TO 110

*  Help
      WRITE (LUPRO,'(/1X,''Enter either the observatory longitude'//
     :    ' (West +ve) and latitude,'')')
      WRITE (LUPRO,'(1X,''or an observatory identifier,'//
     :    ' for example:'')')
      WRITE (LUPRO,'(/1X,''     -149 03 57.9  -31 16 37'')')
      WRITE (LUPRO,'(1X,''or:'')')
      WRITE (LUPRO,'(1X,''     AAT'')')
      WRITE (LUPRO,'(/1X,''For a list of observatory identifiers,'//
     :    ' enter ? again:'')')
      WRITE (LUPRO,'(1X,''> '',$)')

*  Input
      CALL INPUT(LUINP,LUECH,INREC,J)

*  Check for termination or supplementary help request
      IF (J.EQ.-1) GO TO 9000
      IF (J.NE.1) GO TO 110

*  Supplementary help - table of observatory IDs
      IDENT=' '
      N=1
      DO WHILE (.TRUE.)
         CALL sla_OBS(N,IDENT,NAME,W,P,H)
         IF (NAME.EQ.'?') GO TO 100
         WRITE (LUPRO,'(3X,A,4X,A)') IDENT,NAME
         N=N+1
      END DO

*  Interpret input
 110  CONTINUE

*  Check for default case (not allowed first time)
      IF (JFTF.NE.0.OR.INREC.NE.' ') THEN

*     Look for observatory ID, perhaps preceded by spaces
         N=1
         IDENT=' '
         DO WHILE (N.LE.80)
            IF (INREC(N:N).EQ.' ') THEN
               N=N+1
            ELSE
               I=1
               DO WHILE (INREC(N:N).NE.' '.AND.N.LE.80)
                  IDENT(I:I)=INREC(N:N)
                  I=I+1
                  N=N+1
               END DO
            END IF
         END DO

*     Check for trailing spaces
         IF (KTEST(INREC,N).NE.0) GO TO 190

*     Request observatory parameters
         CALL sla_OBS(0,IDENT,NAME,W,P,H)
         IF (NAME.EQ.'?') THEN
            NAME='(anon)'

*        Valid observatory ID not found;  decode long & lat
            N=1
            CALL sla_INTIN(INREC,N,IWD,JW)
            IF (JW.GT.0) GO TO 190
            CALL sla_INTIN(INREC,N,IWM,J)
            IF (J.NE.0) GO TO 190
            CALL sla_DFLTIN(INREC,N,WS,J)
            IF (J.NE.0) GO TO 190
            CALL sla_DAF2R(ABS(IWD),IWM,WS,WTEMP,J)
            IF (J.NE.0) GO TO 190
            CALL sla_INTIN(INREC,N,IPD,JP)
            IF (JP.GT.0) GO TO 190
            CALL sla_INTIN(INREC,N,IPM,J)
            IF (J.NE.0) GO TO 190
            CALL sla_DFLTIN(INREC,N,PS,J)
            IF (J.NE.0) GO TO 190
            CALL sla_DAF2R(ABS(IPD),IPM,PS,PTEMP,J)
            IF (J.NE.0) GO TO 190

*        Check for trailing spaces
            IF (KTEST(INREC,N).NE.0) GO TO 190

*        Accept new data
            IF (JW.LT.0) WTEMP=-WTEMP
            IF (JP.LT.0) PTEMP=-PTEMP
            W=WTEMP
            P=PTEMP
         END IF

*     Functions of latitude
         SINP=SIN(P)
         COSP=COS(P)
      END IF
      GO TO 200

*  Errors
 190  CONTINUE
      WRITE (LUPRO,'(1X,''?'')')
      GO TO 100

*
*  Date record
*  -----------

 200  CONTINUE

*  Prompt and input
      WRITE (LUPRO,'(/1X,''Date & number of days?  (y m d  n)'')')
      WRITE (LUPRO,'(1X,''> '',$)')
      CALL INPUT(LUINP,LUECH,INREC,J)

*  Check for termination or help request
      IF (J.EQ.-1) GO TO 9000
      IF (J.NE.1) GO TO 210

*  Help
      WRITE (LUPRO,'(/'' Enter the UT calendar date and'//
     :    ' the number of days.  For example:'')')
      WRITE (LUPRO,'(/1X,''     1985 10 21  3'')')
      WRITE (LUPRO,'(/1X,''will generate a report covering'//
     :    ' 0 hrs UTC on 21st October 1985'')')
      WRITE (LUPRO,'(1X,''to 0 hrs UTC on 24th October 1985.''/)')
      GO TO 200

*  Interpret input
 210  CONTINUE

*  Check for default case (not allowed first time)
      IF (JFTF.NE.0.OR.INREC.NE.' ') THEN

*     Decode date,days
         N=1
         CALL sla_INTIN(INREC,N,IY,J)
         IF (J.NE.0) GO TO 290
         CALL sla_INTIN(INREC,N,IM,J)
         IF (J.NE.0) GO TO 290
         CALL sla_INTIN(INREC,N,ID,J)
         IF (J.NE.0) GO TO 290
         CALL sla_CALDJ(IY,IM,ID,DJTEMP,J)
         IF (J.NE.0.AND.J.NE.3) GO TO 290
         IF (JFTF.EQ.1) NDTEMP=1
         CALL sla_INTIN(INREC,N,NDTEMP,J)
         IF (J.GT.1.OR.NDTEMP.LT.1) GO TO 290

*     Check for trailing spaces
         IF (KTEST(INREC,N).NE.0) GO TO 190

*     Accept new data
         DJM0=DJTEMP
         ND=NDTEMP

*     Correction from UTC to TDT (days)
         DELTAT=sla_DTT(DJM0)/86400D0

*     TDB for middle of report
         TDBMID=DJM0+DELTAT+DBLE(ND)/2D0

      END IF
      GO TO 300

*  Errors
 290  CONTINUE
      WRITE (LUPRO,'(1X,''?'')')
      GO TO 200

*
*  Star record
*  -----------

 300  CONTINUE

*  Prompt and input
      WRITE (LUPRO,'(/'' Source position?  (h m s  d m s  e)'')')
      WRITE (LUPRO,'(1X,''> '',$)')
      CALL INPUT(LUINP,LUECH,INREC,J)

*  Check for termination or help request
      IF (J.EQ.-1) GO TO 9000
      IF (J.NE.1) GO TO 310

*  Help
      WRITE (LUPRO,'(/1X,'' Enter the RA,Dec and equinox.'//
     :    '  For example:'')')
      WRITE (LUPRO,'(/1X''     08 33 39.30  -45 00 10.3  B1950'')')
      GO TO 300

*  Interpret input
 310  CONTINUE

*  Check for default case (not allowed first time)
      IF (JFTF.NE.0.OR.INREC.NE.' ') THEN

*     Decode RA,Dec,Equinox
         N=1
         CALL sla_INTIN(INREC,N,IRD,JR)
         IF (JR.NE.0) GO TO 390
         CALL sla_INTIN(INREC,N,IRM,J)
         IF (J.NE.0) GO TO 390
         CALL sla_DFLTIN(INREC,N,RS,J)
         IF (J.NE.0) GO TO 390
         CALL sla_DTF2R(IRD,IRM,RS,RTEMP,J)
         IF (J.NE.0) GO TO 390
         CALL sla_INTIN(INREC,N,IDD,JD)
         IF (JD.GT.0) GO TO 390
         CALL sla_INTIN(INREC,N,IDM,J)
         IF (J.NE.0) GO TO 390
         CALL sla_DFLTIN(INREC,N,DS,J)
         IF (J.NE.0) GO TO 390
         CALL sla_DAF2R(ABS(IDD),IDM,DS,DTEMP,J)
         IF (J.NE.0) GO TO 390
         CALL sla_DBJIN(INREC,N,ETEMP,J,JB)
         IF (J.NE.0.OR.
     :       ETEMP.LT.1900D0.OR.
     :       ETEMP.GT.2100D0) GO TO 390
         CALL sla_KBJ(JB,ETEMP,JORB,J)
         IF (J.NE.0) GO TO 390

*     Check for trailing spaces
         IF (KTEST(INREC,N).NE.0) GO TO 190

*     Accept new data
         IF (JR.LT.0) RTEMP=-RTEMP
         IF (JD.LT.0) DTEMP=-DTEMP
         R=RTEMP
         D=DTEMP
         E=ETEMP

*     Mean J2000 place
         R2=R
         D2=D
         IF (JORB.EQ.'J') THEN
            CALL sla_PRECES('FK5',E,2000D0,R2,D2)
         ELSE
            CALL sla_PRECES('FK4',E,1950D0,R2,D2)
            CALL sla_FK45Z(R2,D2,sla_EPJ(TDBMID),RW,DW)
            R2=RW
            D2=DW
         END IF
         CALL sla_DCS2C(R2,D2,V2)

*     Geocentric apparent place
         CALL sla_MAP(R2,D2,0D0,0D0,0D0,0D0,2000D0,TDBMID,RA,DA)
         SIND=SIN(DA)
         COSD=COS(DA)

*     Galactic and ecliptic coordinates
         CALL sla_EQGAL(R2,D2,GL,GB)
         CALL sla_EQECL(R2,D2,TDBMID,EL,EB)

*
*     Slowly changing velocity components
*
         R2S=REAL(R2)
         D2S=REAL(D2)
*     Sun with respect to dynamical and kinematical LSRs
         VCLSRD=DBLE(sla_RVLSRD(R2S,D2S))
         VCLSRK=DBLE(sla_RVLSRK(R2S,D2S))
*     LSR with respect to galaxy
         VCGALC=DBLE(sla_RVGALC(R2S,D2S))
*     Sun with respect to local group
         VCLG=DBLE(sla_RVLG(R2S,D2S))

      END IF

      GO TO 1000

*  Errors
 390  CONTINUE
      WRITE (LUPRO,'(1X,''?'')')
      GO TO 300

*
*  Reports
*  -------

 1000 CONTINUE

*  Headings
      IF (JFTF.EQ.1) THEN
         WRITE (LUREP,'(''1'')')
         WRITE (LUREP,
     :   '(/1X,''---------------------------------------'')')
         WRITE (LUREP,
     :   '(1X,''RADIAL COMPONENT OF OBSERVER''''S VELOCITY'')')
         WRITE (LUREP,
     :   '(1X,''---------------------------------------'')')
         WRITE (LUREP,'(/1X,''Units: km/s'')')
         WRITE (LUREP,'(/1X,''Sign convention:'')')
         WRITE (LUREP,'(1X,''   +ve means the observer is moving'//
     :                        ' away from the position:  to correct'')')
         WRITE (LUREP,'(1X,''   an observed radial velocity to one'//
     :                               ' of the given rest standards,'')')
         WRITE (LUREP,'(1X,''   SUBTRACT the appropriate tabulated'//
     :                                                     ' value.'')')
         WRITE (LUREP,'(/1X,''Local Standards of Rest:'')')
         WRITE (LUREP,'(1X,''  Two forms of LSR are provided for.'//
     :                        '  LSR (K) is a "kinematical" LSR and'')')
         WRITE (LUREP,'(1X,''  (loosely) refers to the average motion'//
     :                           ' of nearby stars.  LSR (D) is the'')')
         WRITE (LUREP,'(1X,''  "dynamical" LSR, the point in the'//
     :                            ' solar neighbourhood which is in'')')
         WRITE (LUREP,'(1X,''  circular orbit around the galactic'//
     :                                                    ' centre.'')')
         WRITE (LUREP,'(/1X,''  The solar velocity defining the'//
     :                         ' adopted kinematical LSR is 20 km/s'')')
         WRITE (LUREP,'(1X,''  towards 18 00 00.0 +30 00 00 B1900'//
     :                            ' (= 18 03 50.3 +30 00 17 J2000).'')')
         WRITE (LUREP,'(/1X,''  The solar velocity with respect to'//
     :                      ' the dynamical LSR is (+9,+12,+7) km/s'')')
         WRITE (LUREP,'(1X,''  in galactic cartesian coordinates, the'//
     :                            ' equivalent of 16.6 km/s towards'')')
         WRITE (LUREP,'(1X,''  17 49 58.7 +28 07 04 J2000.'')')
         WRITE (LUREP,'(/1X,''Galactic rotation:'')')
         WRITE (LUREP,'(1X,''  The velocity of the dynamical LSR with'//
     :                          ' respect to the galactic centre is'')')
         WRITE (LUREP,'(1X,''  220 km/s towards L2=90, B2=0.'')')
         WRITE (LUREP,'(/1X,''Supergalactic:'')')
         WRITE (LUREP,'(1X,''  The solar velocity with respect to the'//
     :                          ' mean motion of the local group is'')')
         WRITE (LUREP,'(1X,''  300 km/s towards L2=90, B2=0.'')')
         WRITE (LUREP,'(/1X,''Heliocentric light-time:'')')
         WRITE (LUREP,'(1X,''  The light time component to the Sun'//
     :                         ' (listed following the heliocentric'')')
         WRITE (LUREP,'(1X,''  velocity component) is in seconds and'//
     :                           ' is +ve when the position is less'')')
         WRITE (LUREP,'(1X,''  than 90deg from the Sun: to correct an'//
     :                                 ' observed time SUBTRACT the'')')
         WRITE (LUREP,'(1X,''  tabulated light time.'')')
      END IF

*  Observatory
      CALL sla_DR2AF(1,W,SLONG,LONG)
      CALL sla_DR2AF(0,P,SLAT,LAT)
      IF (SLONG.EQ.'+') THEN
         SLONG='W'
      ELSE
         SLONG='E'
      END IF
      IF (SLAT.EQ.'+') THEN
         SLAT='N'
      ELSE
         SLAT='S'
      END IF
      WRITE (LUREP,'(''1'')')
      WRITE (LUREP,'(/1X,''Observatory:   '',A)') NAME
      WRITE (LUREP,'(16X,A,I4,2I3.2,''.'',I1,3X,A,I3,2I3.2/)')
     :              SLONG,LONG,SLAT,(LAT(I),I=1,3)

*  Date
      CALL sla_DJCAL(0,DJM0,IYMDF,J)
      WRITE (LUREP,'(1X,''Starting UTC date:'',I7,2(''/'',I2.2),'//
     :               '''  =  JD'',F10.1/)')
     :               (IYMDF(I),I=1,3),DJM0+2400000.5D0

*  Mean Ra,Dec as supplied
      CALL sla_DR2TF(2,R,SR,IR)
      CALL sla_DR2AF(1,D,SD,IDC)
      WRITE (LUREP,'(1X,''Equatorial coordinates:  '''//
     :               '3I3.2,''.'',I2.2,2X,'//
     :               'A,I2.2,2I3.2,''.'',I1,3X,A,F6.1)')
     :               IR,SD,IDC,JORB,E

*  Mean J2000 place if necessary
      IF (JORB.NE.'J'.OR.E.NE.2000D0) THEN
         CALL sla_DR2TF(2,R2,SR2,IR2)
         CALL sla_DR2AF(1,D2,SD2,ID2)
         WRITE (LUREP,'(26X,3I3.2,''.'',I2.2,2X,'//
     :                  'A,I2.2,2I3.2,''.'',I1,''   J2000.0'')')
     :                  IR2,SD2,ID2
      END IF

*  Geocentric apparent place
      CALL sla_DR2TF(2,RA,SRA,IRA)
      CALL sla_DR2AF(1,DA,SDA,IDA)
      WRITE (LUREP,'(26X,3I3.2,''.'',I2.2,2X,'//
     :               'A,I2.2,2I3.2,''.'',I1,'//
     :               '''   geocentric apparent''/)')
     :               IRA,SDA,IDA

*  Galactic and ecliptic coordinates
      WRITE (LUREP,'(1X,''Galactic coordinates:   L2 ='',SS,F9.4,'//
     :               '''  B2 ='',SP,F9.4)') GL*R2D,GB*R2D
      WRITE (LUREP,'(/1X,''Ecliptic coordinates:   L  ='',SS,F9.4,'//
     :               '''  B  ='',SP,F9.4,'//
     :               '''  (mean equinox of date)''/)')
     :               EL*R2D,EB*R2D

*  Table heading
      WRITE (LUREP,'(/7X''UTC'',12X,''ZD'',5X,''EARTH'','//
     :               '9X,''SUN'',12X,''LSR (K)   LSR (D)'','//
     :               '5X,''GALAXY'',4X,''LOCAL GROUP''/)')

*  Loop: days from starting date
      DO JDAY=0,ND-1

*     Calendar date
         DJM1=DJM0+DBLE(JDAY)
         CALL sla_DJCAL(0,DJM1,IYMDF,J)

*     Loop: half hours from 0 hours UTC

      DO J30M=0,47

*        UT1 (neglecting UT1-UTC)
            UT=DJM1+DBLE(J30M)/48D0

*        Local ST (radians)
            ST=sla_GMST(UT)-W

*        ZD (degrees)
            HA=ST-RA
            COSZ=SINP*SIND+COSP*COSD*COS(HA)
            ZD=R2D*(ATAN2(SQRT(1D0-MIN(1D0,COSZ**2)),COSZ))

*        Don't report if source is (roughly) below horizon
            IF (ZD.LT.90D0) THEN

*           UTC h,m
               NUTH=J30M/2
               NUTM=30*MOD(J30M,2)

*           TDB
               TDB=UT+DELTAT

*           Earth/Sun velocity and position
               CALL sla_EVP(TDB,2000D0,DVB,DPB,DVH,DPH)

*
*           Velocity components
*
*           Earth rotation
               VCROT=DBLE(
     :               sla_RVEROT(REAL(P),REAL(RA),REAL(DA),REAL(ST)))
*           Earth orbit
               VCORB=-sla_DVDV(V2,DVH)*AUKM
*           Totals
               RVTOT1=VCROT
               RVTOT2=RVTOT1+VCORB
               RVTOT3=RVTOT2+VCLSRD
               RVTOT4=RVTOT3+VCGALC
               RVTOT5=RVTOT2+VCLG

*           Heliocentric light time component
               TL=-sla_DVDV(V2,DPH)*AUSEC

*           Report
               WRITE (LUREP,'(1X,I4.4,3I3.2,'':'',I2.2,'//
     :                        'F8.1,SP,F9.2,F10.2,'' ('','//
     :                        'F6.1,'')'',F11.2,F10.2,F12.2,F13.2)')
     :                        (IYMDF(I),I=1,3),NUTH,NUTM,ZD,
     :                        RVTOT1,RVTOT2,TL,
     :                        RVTOT2+VCLSRK,RVTOT3,RVTOT4,RVTOT5

            END IF

*        Next half hour
         END DO

*     Next day
      END DO

*  Reset first time flag
      JFTF=0

*
*  End of main loop
*  ----------------

      GO TO 100

*
*  Wrap up
*  -------

 9000 CONTINUE
      WRITE (LUPRO,'(/)')

      END
