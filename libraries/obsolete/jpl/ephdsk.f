C--------------------- MAIN PROGRAM 'EPHDSK' ---------------------------
C
C     THIS PROGRAM READS THE JPL EXPORT PLANETARY EPHEMERIS
C     TAPE AND WRITES THE CONVERTED EPHEMERIS ON A MASS-STORAGE
C     DIRECT-ACCESS FILE.  THE INPUT TAPE CAN BE EITHER THE
C     ENCODED ASCII FORMAT OR THE DIRECT-READABLE BINARY FORMAT,
C     IF AVAILABLE FOR THE USER'S MACHINE.
C
C     THE USER WILL GET TWO PROMPTS AT THE START OF THE RUN. THE
C     FIRST PROMPT REQUESTS AN INPUT CODE LETTER DESIGNATING
C     THE TAPE FORMAT. THEN, AFTER A DISPLAY OF THE CURRENT SPAN,
C     A REQUEST IS MADE TO ENTER THE (OPTIONAL) START AND END
C     EPOCHS ON THE OUTPUT EPHEMERIS FILE.
C
C     THE INPUT FILE IS 'EPHTAP'; THE OUTPUT FILE IS 'JPLEPH'.
C
*-----------------------------------------------------------------------
*
*  An EXTERNAL COMDAT statement has been added to make sure that the
*  BLOCK DATA subprogram COMDAT is loaded at link time.
*
*  P.T.Wallace   Starlink   26 February 1993
*
*-----------------------------------------------------------------------
C
      EXTERNAL COMDAT
C
C-----------------------------------------------------------------------
C
C       HEADER BUFFER
C
      INTEGER HED(5)
C
C       TRAILER-DETECT FLAG (.TRUE. IF CURRENT REC IS A TRAILER)
C
      LOGICAL TLR
C
C       FLAG SET .TRUE. IF ASCII D.P. IMAGE TO BE DECODED
C
      LOGICAL DECIM
C
C       BUFFER FOR INPUT LINE IMAGE FROM TAPE
C
      CHARACTER*80 IMAGE
      CHARACTER*8 CC(2)
C
C       RECORD WORD COUNT AND D.P. ENCODED NUMBERS
C
      INTEGER N
      INTEGER NDP(3,3)
      INTEGER NR1
      INTEGER NR2
      INTEGER NR
      INTEGER NRW
C
C       TAPE-FORMAT CHARACTER
C
      CHARACTER*1 FMT
C
C       INPUT TAPE-FORMAT CONTROL FLAG
C
      LOGICAL ASC
C
C       BUFFER FOR POWERS OF 2
C
      DOUBLE PRECISION PW2(221)
C
C       STORAGE AREA FOR INPUT-FILE NON-CHARACTER RECORDS
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
      DOUBLE PRECISION DB(1)
      EQUIVALENCE(IB,DB)
C
C       STORAGE AREA FOR INPUT-FILE CHARACTER RECORDS
C
      CHARACTER*6 CB(400)
C
C       STORAGE NEEDED FOR SHORTENING THE INPUT EPHEMERIS
C
      INTEGER C(2,2)
      INTEGER CAL(5)
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
      INTEGER EFIL
      COMMON/EPUNIT/EFIL
C
      CHARACTER*84 HDG(3)
      EQUIVALENCE(HDG(1),TTL(1,1))
C
C       DATA STATEMENTS
C
      DATA C/4*0/
      DATA SSN/2*0.D0/
      DATA J1950/2433282.5D0/
      DATA J2000/2451545.D0/
      DATA EL/'EARLI','  LAT'/
      DATA NR/0/
      DATA NRW/1/
      DATA NR1/1/
      DATA NR2/1000000/
      DATA BASEJD/0.D0/
      DATA CHDAT/' '/
C
C       PRINT PROGRAM ENTRY MESSAGE AND TAPE-FORMAT PROMPT
C
      WRITE(6,100)
  100 FORMAT('1',10X,'Planetary Ephemeris Tape-to-Disk Program',//,
     * ' Enter input tape format code: A if ASCII, or B if Binary.')
C
C      READ FORMAT CODE LETTER; SET CORRESPONDING VARIABLES
C
    8 READ(5,107)FMT
  107 FORMAT(A1)
      ASC=FMT.EQ.'A' .OR. FMT.EQ.'a'
      IF((.NOT.ASC) .AND. FMT.NE.'B' .AND. FMT.NE.'b') THEN
        WRITE(6,111)FMT
  111   FORMAT('0Character ',A1,' not recognized. Re-enter code.')
        GO TO 8
      ENDIF
C
C       OPEN INPUT AND OUTPUT FILES
C
      IF(ASC) THEN
C                        'OPEN' STATEMENT FOR ASCII-FORMAT TAPE
        OPEN(9,
     *       FILE='EPHTAP',
     *       ACCESS='SEQUENTIAL',
     *       FORM='FORMATTED',
     *       STATUS='OLD')
C
      ELSE
C                        'OPEN' STATEMENT FOR BINARY-FORMAT TAPE
        OPEN(9,
     *       FILE='EPHTAP',
     *       ACCESS='SEQUENTIAL',
     *       FORM='UNFORMATTED',
C    *       RECORDTYPE='VARIABLE', ! REMOVE 'C' IN COL 1 FOR VAX I/O
     *       STATUS='OLD')
      ENDIF
C
      CALL EPHOPN(.FALSE.)
C
C       SET UP DOUBLE PRECISION TABLE OF POWERS OF 2 FROM
C       2**(-110) THROUGH 2**(110).  THIS TABLE IS NEEDED
C       TO DECODE DP DATA ON ASCII-FORMAT FILES.
C
      IF(ASC) THEN
        PW2(110)=.5D0
        PW2(111)=1.D0
        PW2(112)=2.D0
        DO 50 K=1,109
        PW2(110-K)=PW2(111-K)*PW2(110)
        PW2(112+K)=PW2(111+K)*PW2(112)
   50   CONTINUE
      ENDIF
C
C       BEGINNING OF GROUP-READING LOOP. READ AND PRINT HEADER.
C
    1 CONTINUE
      IF(ASC) THEN
        CALL RCI(IMAGE)
        READ(IMAGE,108)NDUM,HED
  108   FORMAT(I5,1X,6I12)
      ELSE
        READ(9)HED
      ENDIF
      WRITE(6,101)HED
  101 FORMAT('0 HEADER:',5I10)
C
C       IF GROUP TYPE (WORD 2) IS 5, WE ARE DONE.
C
    9 IF(HED(2).EQ.5 .OR. (HED(4).EQ.1070 .AND. NR.GE.NR2)) THEN
        CLOSE (EFIL)
        WRITE(6,102)NRW-2
  102   FORMAT('0',I6,' data records in file.')
        STOP 'Conversion complete.'
      ENDIF
C
C       CHECK TO SEE IF THE BUFFER SIZE IS LARGE ENOUGH TO ACCOMMODATE
C       THE EPHEMERIS DATA RECORDS.
C
      IF(HED(4).EQ.1070 .AND. HED(1)-1.GT.IBSZ) THEN
        IX=HED(1)-1
        WRITE(6,120)IX,IBSZ
  120   FORMAT('0Insufficient buffer size for data records.',/,
     *   I6,' words needed and only',I6,' available in buffer.',/,
     *   ' Change the parameter KSIZE in the block data program',/,
     *   ' COMDAT to a sufficiently large value.')
        STOP
      ENDIF
C
C       RESET RECORD COUNT AND READ NEXT RECORD
C
      NR=0
C
    2 NR=NR+1
      IF(ASC) CALL RCI(IMAGE)
C
C         DOUBLE-PRECISION RECORD PROCESSING
C
      IF(HED(2).EQ.2) THEN
        IF(ASC) THEN
          READ(IMAGE,109)N,NDP
  109     FORMAT(I5,3(I3,2I11))
          NWB=0
          DO 15 I=1,N,3
          DECIM=HED(4).NE.1070 .OR. NR.GE.NR1 .OR. I.EQ.1
          IF(I.GT.1) THEN
            CALL RCI(IMAGE)
            IF(DECIM) READ(IMAGE,109)NDUM,NDP
          ENDIF
          IF(DECIM) THEN
            NW=MIN0(3,N-I+1)
            DO 12 K=1,NW
            NX=81+NDP(1,K)
            NWB=NWB+1
            IF(NDP(2,K).EQ.0) THEN
              DB(NWB)=0.D0
            ELSEIF(NDP(1,K).GT.-51) THEN
              DB(NWB)=DBLE(NDP(2,K))*PW2(NX)
     *               +DBLE(NDP(3,K))*PW2(NX-30)
            ELSE
              DB(NWB)=DBLE(NDP(2,K))*(2.D0**(NDP(1,K)-30))
     *               +DBLE(NDP(3,K))*(2.D0**(NDP(1,K)-60))
            ENDIF
   12       CONTINUE
          ENDIF
   15     CONTINUE
        ELSE
          READ(9)N,(DB(K),K=1,N)
        ENDIF
        TLR=DB(1).EQ.0.D0
C
C         INTEGER RECORD PROCESSING
C
      ELSEIF(HED(2).EQ.3) THEN
        IF(ASC) THEN
          READ(IMAGE,108)N,(IB(K),K=1,6)
          DO 22 I=1,N,6
          IF(I.GT.1) THEN
            CALL RCI(IMAGE)
            READ(IMAGE,108)NDUM,(IB(I+K-1),K=1,6)
          ENDIF
   22     CONTINUE
        ELSE
          READ(9)N,(IB(K),K=1,N)
        ENDIF
        TLR=IB(1).EQ.0
C
C         CHARACTER RECORD PROCESSING
C
      ELSE
        IF(ASC) THEN
          READ(IMAGE,110)N
  110     FORMAT(I5)
          DO 32 I=1,N,12
          IF(I.GT.1) CALL RCI(IMAGE)
          IX=MIN0(N-I+1,12)
          DO 32 J=1,IX
   32     CB(J+I-1)=IMAGE(6*J+1:6*J+6)
        ELSE
          READ(9)N,(CB(K),K=1,N)
        ENDIF
        TLR=CB(1).EQ.'000000'
C
      ENDIF
C
C       CHECK FOR ACTUAL TRAILER
C
      TLR=TLR .AND. N.EQ.1
      IF(TLR) THEN
        NR=NR-1
        WRITE(6,103)NR
  103   FORMAT(I7,' records in this group.')
        GO TO 1
      ENDIF
C
C       PROCESS DESIGNATED GROUPS. THE EPHEMERIS FILE
C       HEADING RECORDS, START AND STOP JED'S, NAMES AND
C       VALUES OF CONSTANTS, AND POINTERS ARE PUT INTO
C       RECORD 1 OF FILE.
C
C         GROUP 1010 IS HEADING GROUP -- PRINT EACH LINE
C
      IF(HED(4).EQ.1010) THEN
        WRITE(6,104)(CB(K),K=1,13)
  104   FORMAT(' ',13A6)
        DO 3 I=1,N
    3   TTL(I,NR)=CB(I)
C
C           DETERMINE EQUINOX OF EPHEMERIS FROM FIRST HEADING LINE
C
        IF(NR.EQ.1) THEN
          IX=INDEX(HDG(1),'-')
          IF(IX.LE.0) THEN
            CHDAT='1950 '
            BASEJD=J1950
          ELSE
            READ(HDG(1)(IX+1:IX+3),116)DENUM
  116       FORMAT(I3)
            IF(DENUM.GE.200) THEN
              CHDAT='J2000'
              BASEJD=J2000
            ELSE
              CHDAT='1950 '
              BASEJD=J1950
            ENDIF
          ENDIF
        ENDIF
        GO TO 2
      ENDIF
C
C       GROUP 1030 -- LIMITS GROUP
C
      IF(HED(4).EQ.1030 .AND. NR.EQ.1) THEN
        DO 4 I=1,3
    4   SS(I)=DB(I)
C
C         PRINT MESSAGE AND GET POSSIBLE NEW START AND END DATES
C
        WRITE(6,112)
  112   FORMAT('0To create a short ephemeris, enter new',
     *   ' start and end dates as',//,' YYYYMMDD,YYYYMMDD',//,
     *   ' If full span is wanted, press <RETURN>',/)
C
        READ(5,113)IMAGE
  113   FORMAT(A)
        SSN(1)=SS(1)
        SSN(2)=SS(2)
        NR1=1
        NR2=IDINT((SS(2)-SS(1))/SS(3))
        IF(IMAGE(1:1).NE.' ') THEN
          IX=INDEX(IMAGE,',')
          CC(1)=' '
          CC(2)=' '
          IF(IX.GT.1) THEN
            CC(1)=IMAGE(MAX0(1,IX-8):IX-1)
            CC(2)=IMAGE(IX+1:IX+8)
          ENDIF
          DO 19 K=1,2
          READ(CC(K),119)C(1,K)
  119     FORMAT(I8)
   19     CONTINUE
          DO 13 I=1,2
          IF(C(1,I).GT.0) CALL CTOJ(C(1,I),SSN(I))
   13     CONTINUE
C
C           GET ACTUAL NEW DATES -- FIND START OF NEAREST EXISTING RECORD
C
          SSN(1)=DMAX1(SSN(1),SS(1))
          SSN(2)=DMIN1(SSN(2),SS(2))
C
          SSN(1)=SS(1)+DINT((SSN(1)-SS(1))/SS(3))*SS(3)
          SSN(2)=SS(2)-DINT((SS(2)-SSN(2))/SS(3))*SS(3)
          IF(SSN(2)-SSN(1).LT.SS(3)) THEN
            WRITE(6,115)SSN
  115       FORMAT('0 Input JED error -- dates are',2F11.1)
            STOP
          ENDIF
C
C           GET RANGE OF RECORDS TO BE TRANSFERRED
C
          NR1=IDINT((SSN(1)-SS(1))/SS(3))+1
          NR2=IDINT((SSN(2)-SS(1))/SS(3))
C
C           GENERATE NEW TITLE LINES
C
          DO 14 I=1,2
          SS(I)=SSN(I)
          CALL JTOC(SS(I),CAL,SEC,0)
          SEC=(SS(I)-BASEJD)*86400.D0
          HDG(I+1)=' '
          WRITE(HDG(I+1),114)EL(I),SS(I),(CAL(K),K=1,3),SEC,CHDAT
  114     FORMAT(' ',A5,'EST EPOCH: JED=',F10.1,', ',
     *     I2,'/',I2.2,'/',I4,', ',F13.0,' SECS PAST ',A5)
   14     CONTINUE
          WRITE(6,117)(HDG(K)(1:80),K=2,3)
  117     FORMAT('0Output ephemeris span:',//,2(1X,A80,/))
        ENDIF
        WRITE(6,118)NR1,NR2
  118   FORMAT(' Records',I6,' through',I6,' will be transferred.',/)
        GO TO 2
      ENDIF
C
C         GROUP 1040, RECORD 4, CONTAINS NAMES OF CONSTANTS
C
      IF(HED(4).EQ.1040 .AND. NR.EQ.4) THEN
        NCON=N
        DO 5 I=1,N
    5   CNAM(I)=CB(I)
        GO TO 2
      ENDIF
C
C         GROUP 1041, RECORD 4, HAS VALUES OF CONSTANTS
C
      IF(HED(4).EQ.1041 .AND. NR.EQ.4) THEN
        DO 6 I=1,N
        CVAL(I)=DB(I)
        IF(CNAM(I).EQ.'AU    ') THEN
          AU=CVAL(I)
        ELSEIF(CNAM(I).EQ.'EMRAT ') THEN
          EMRAT=CVAL(I)
        ENDIF
    6   CONTINUE
        GO TO 2
      ENDIF
C
C         GROUP 1050 CONTAINS POINTERS NEEDED BY INTERP
C
      IF(HED(4).EQ.1050 .AND. NR.EQ.2) THEN
        DO 7 I=1,36
    7   IPT(I)=IB(I)
        DO 20 I=1,3
        IF(N.GT.36) THEN
          LPT(I)=IB(36+I)
        ELSE
          LPT(I)=0
        ENDIF
   20   CONTINUE
        NRW=1
        WRITE(EFIL,REC=NRW)TTL,CNAM,SS,NCON,AU,EMRAT,IPT,DENUM,LPT
        NRW=2
        WRITE(EFIL,REC=NRW)CVAL
        GO TO 2
      ENDIF
C
C         GROUP 1070 CONTAINS THE EPHEMERIS DATA RECORDS
C
      IF(HED(4).EQ.1070 .AND. NR.GE.NR1) THEN
        NRW=NRW+1
        WRITE(EFIL,REC=NRW,ERR=99)(DB(K),K=1,N)
        IF(MOD(NRW-2,50).EQ.0) WRITE(6,105)NRW-2
  105   FORMAT(I7,' data records written ... continuing.')
        IF(NR.GE.NR2) GO TO 9
        GO TO 2
      ENDIF
C
C       IF ANY OTHER GROUP IS BEING READ, IGNORE RECORDS
C
      GO TO 2
C
C       ERROR RETURN FROM RECORD-WRITE PROBLEMS
C
   99 WRITE(6,106)
  106 FORMAT('0 Error while writing output file.')
      STOP 'I/O errors'
C
      END
