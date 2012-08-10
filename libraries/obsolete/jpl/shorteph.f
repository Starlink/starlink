C----------------------- MAIN PROGRAM 'SHORTEPH' -----------------------
C
C     THIS PROGRAM READS THE DIRECT-ACCESS JPL EPHEMERIS AND
C     PRODUCES AN OUTPUT EPHEMERIS WITH A SPECIFIED SHORTER
C     TIME SPAN. THE INPUT EPHEMERIS IS EXPECTED ON FILE 'JPLEPH',
C     AND THE NEW EPHEMERIS WILL BE WRITTEN ON FILE 'JPLEPHS'
C
C     THE USER WILL BE PROMPTED FOR NEW START AND END DATES, IN THE FORM
C
C                            YYYYMMDD,YYYYMMDD
C     WHERE
C            YYYY = YEAR
C              MM = MONTH (01 .LE. MM .LE. 12)
C              DD = DAY (01 .LE. DD .LE. NBR OF DAYS IN MONTH MM)
C
C     IF EITHER THE ORIGINAL START OR END DATE IS DESIRED,
C     ENTER A 0 IN THAT FIELD.
C
*------------------------------------------------------------------------
*
*  The original JPL source code has been modified by the addition of
*  an EXTERNAL COMDAT statement, to make sure that the BLOCK DATA
*  subprogram COMDAT is loaded at link time.
*
*  P.T.Wallace   Starlink   26 February 1993
*
*------------------------------------------------------------------------
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL COMDAT
C
C       MISCELLANEOUS DECLARATIONS
C
      INTEGER C(2,2)
      INTEGER CAL(5)
C
*
*  Variables re-ordered to avoid alignment problems.  This has
*  also necessitated the explicit 1652.
*
*  P.T.Wallace   Starlink   14 April 1994
*
      INTEGER IBSZ
      INTEGER IB(1652)
      COMMON/EPCOMM/IB,IBSZ
C
      DOUBLE PRECISION BASEJD
      DOUBLE PRECISION SEC
      DOUBLE PRECISION SSN(2)
      DOUBLE PRECISION J1950
      DOUBLE PRECISION J2000
C
      CHARACTER*5 CHDAT
      CHARACTER*5 EL(2)
C
C       COMMON AREA FOR CHARACTER DATA IN RECORD 1
C
      CHARACTER*6 TTL(14,3)
      CHARACTER*6 CNAM(400)
      COMMON/CHRHDR/TTL,CNAM
C
C       COMMON AREA FOR CONSTANTS AND POINTERS IN RECORD 1
C
      DOUBLE PRECISION SS(3)
      INTEGER NCON
      DOUBLE PRECISION CVAL(400)
      DOUBLE PRECISION AU
      DOUBLE PRECISION EMRAT
      INTEGER IPT(36)
      INTEGER DENUM
      INTEGER LPT(3)
*
*  Variables re-ordered to avoid alignment problems.
*
*  P.T.Wallace   Starlink   14 April 1994
*
      COMMON/EPHHDR/SS,CVAL,AU,EMRAT,NCON,IPT,DENUM,LPT
C
      CHARACTER*84 HDG(3)
      EQUIVALENCE(HDG(1),TTL(1,1))
C
C       COMMON AREA FOR INPUT FILE NUMBER
C
      INTEGER FILE
      COMMON/EPUNIT/FILE
C
C       DATA STATEMENTS
C
      DATA C/4*0/
      DATA SSN/2*0.D0/
      DATA J1950/2433282.5D0/
      DATA J2000/2451545.D0/
      DATA EL/'EARLI','  LAT'/
C
C
C       ENTRY POINT -- OPEN INPUT FILE AND PRINT 3 TITLE LINES
C
      CALL EPHOPN(.TRUE.)
      INQUIRE(UNIT=12,RECL=IRECSZ)
      WRITE(6,100)((TTL(K1,K2),K1=1,13),K2=1,3)
  100 FORMAT('1',20X,'SHORTEPH entered. Input ephemeris heading:',
     * //,3(1X,13A6,/))
C
C       OPEN OUTPUT FILE
C
      OPEN(UNIT=10,
     * FILE='JPLEPHS',
     * ACCESS='DIRECT',
     * FORM='UNFORMATTED',
     * RECL=IRECSZ,
     * STATUS='UNKNOWN')
C
C       PRINT MESSAGE AND GET NEW START AND END DATES
C
      WRITE(6,101)
  101 FORMAT('0 Enter start and end dates as YYYYMMDD,YYYYMMDD')
      READ(5,*)C(1,1),C(1,2)
      DO 1 I=1,2
      IF(C(1,I).GT.0) CALL CTOJ(C(1,I),SSN(I))
      IF(SSN(I).EQ.0.D0) SSN(I)=SS(I)
    1 CONTINUE
C
C       GET ACTUAL NEW DATES -- FIND START OF NEAREST EXISTING RECORD
C
      SSN(1)=DMAX1(SSN(1),SS(1))
      SSN(2)=DMIN1(SSN(2),SS(2))
C
      SSN(1)=SS(1)+DINT((SSN(1)-SS(1))/SS(3))*SS(3)
      SSN(2)=SS(2)-DINT((SS(2)-SSN(2))/SS(3))*SS(3)
      IF(SSN(2)-SSN(1).LT.SS(3)) THEN
        WRITE(6,102)SSN
  102   FORMAT('0 Input JED error -- dates are',2F11.1)
        STOP
      ENDIF
C
      NR1=IDINT((SSN(1)-SS(1))/SS(3))+3
      NR2=IDINT((SSN(2)-SS(1))/SS(3))+2
C
C       DETERMINE EQUINOX OF EPHEMERIS FROM FIRST HEADING LINE
C
      IX=INDEX(HDG(1),'-')
      IF(IX.LE.0) THEN
        CHDAT='1950 '
        BASEJD=J1950
      ELSE
        READ(HDG(1)(IX+1:IX+3),107)DENUM
  107   FORMAT(I3)
        IF(DENUM.GE.200) THEN
          CHDAT='J2000'
          BASEJD=J2000
        ELSE
          CHDAT='1950 '
          BASEJD=J1950
        ENDIF
      ENDIF
C
C       GENERATE NEW TITLE LINES
C
      DO 2 I=1,2
      SS(I)=SSN(I)
      CALL JTOC(SS(I),CAL,SEC,0)
      SEC=(SS(I)-BASEJD)*86400.D0
      HDG(I+1)=' '
      WRITE(HDG(I+1),103)EL(I),SS(I),(CAL(K),K=1,3),SEC,CHDAT
  103 FORMAT(' ',A5,'EST EPOCH: JED=',F10.1,', ',
     * I2,'/',I2.2,'/',I4,', ',F13.0,' SECS PAST ',A5)
    2 CONTINUE
      WRITE(6,104)(HDG(K)(1:80),K=2,3)
  104 FORMAT('0 New ephemeris span:',//,2(1X,A80,/))
C
C       WRITE HEADER RECORDS
C
      NRW=1
      WRITE(10,REC=NRW)TTL,CNAM,SS,NCON,AU,EMRAT,IPT,DENUM,LPT
      NRW=2
      WRITE(10,REC=NRW)CVAL
C
C       READ CORRECT RECORDS FROM OLD FILE AND COPY ONTO NEW FILE
C
      DO 3 I=NR1,NR2
      READ(FILE,REC=I)(IB(K),K=1,IBSZ)
      NRW=NRW+1
      WRITE(10,REC=NRW)(IB(K),K=1,IBSZ)
      IF(MOD(NRW-2,50).EQ.0) WRITE(6,105)NRW-2
  105 FORMAT(I6,' records copied ... continuing.')
    3 CONTINUE
C
C       CLOSE FILE AND PRINT FINAL RECORD COUNT
C
      CLOSE (10)
      WRITE(6,106)NRW-2
  106 FORMAT('0',I6,' data records copied into new file.')
      STOP
C
      END
