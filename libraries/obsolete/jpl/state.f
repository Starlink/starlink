C--------------------------- SUBROUTINE 'STATE' ------------------------
C
C++++++++++++++++++++++++++++++++
C
      SUBROUTINE STATE (JED, LIST, PV, NUT, OK)           !!! Starlink
C
C++++++++++++++++++++++++++++++++
C
C     THIS SUBROUTINE READS AND INTERPOLATES THE JPL PLANETARY EPHEMERIS
C     FILE.
C
*------------------------------------------------------------------------
*
*  !!! The original JPL source code has been modified as follows:
*
*      *  If the call failed, due to epoch out of range or I/O
*         errors, control was returned, via a `*' dummy argument,
*         to the statement in the calling program whose label
*         was specified in the call.  Because this technique is
*         discouraged by the Starlink Fortran programming standard,
*         the code has been changed to use a logical status
*         return OK instead.
*
*      *  The original comment "ALL OUTPUT VECTORS ARE REFERENCED TO
*         THE EARTH MEAN EQUATOR AND EQUINOX OF EPOCH."  was wrong,
*         or at least incomplete.  The comment should read "... AND
*         EQUINOX OF EPOCH J2000.".  This was confirmed by
*         E. Myles Standish in a letter dated 17 November 1992
*         (reply to a letter from P.T.Wallace dated 10 November 1992).
*
*  A.J.J.Broderick   Starlink   28 October 1992
*  P.T.Wallace   Starlink   22 April 1994
*
*------------------------------------------------------------------------
C
C     CALLING SEQUENCE PARAMETERS:
C
C     INPUT:
C
C         JED   DP 2-WORD JULIAN EPHEMERIS EPOCH AT WHICH INTERPOLATION
C               IS WANTED.  ANY COMBINATION OF JED(1)+JED(2) WHICH FALLS
C               WITHIN THE TIME SPAN ON THE FILE IS A PERMISSIBLE EPOCH.
C
C                A. FOR EASE IN PROGRAMMING, THE USER MAY PUT THE
C                   ENTIRE EPOCH IN JED(1) AND SET JED(2)=0.
C
C                B. FOR MAXIMUM INTERPOLATION ACCURACY, SET JED(1) =
C                   THE MOST RECENT MIDNIGHT AT OR BEFORE INTERPOLATION
C                   EPOCH AND SET JED(2) = FRACTIONAL PART OF A DAY
C                   ELAPSED BETWEEN JED(1) AND EPOCH.
C
C                C. AS AN ALTERNATIVE, IT MAY PROVE CONVENIENT TO SET
C                   JED(1) = SOME FIXED EPOCH, SUCH AS START OF INTEGRATION,
C                   AND JED(2) = ELAPSED INTERVAL BETWEEN THEN AND EPOCH.
C
C        LIST   12-WORD INTEGER ARRAY SPECIFYING WHAT INTERPOLATION
C               IS WANTED FOR EACH OF THE BODIES ON THE FILE.
C
C                         LIST(I)=0, NO INTERPOLATION FOR BODY I
C                                =1, POSITION ONLY
C                                =2, POSITION AND VELOCITY
C
C               THE DESIGNATION OF THE ASTRONOMICAL BODIES BY I IS:
C
C                         I = 1: MERCURY
C                           = 2: VENUS
C                           = 3: EARTH-MOON BARYCENTER
C                           = 4: MARS
C                           = 5: JUPITER
C                           = 6: SATURN
C                           = 7: URANUS
C                           = 8: NEPTUNE
C                           = 9: PLUTO
C                           =10: GEOCENTRIC MOON
C                           =11: NUTATIONS IN LONGITUDE AND OBLIQUITY
C                           =12: LUNAR LIBRATIONS (IF ON FILE)
C
C
C     OUTPUT:
C
C          PV   DP 6 X 11 ARRAY THAT WILL CONTAIN REQUESTED INTERPOLATED
C               QUANTITIES.  THE BODY SPECIFIED BY LIST(I) WILL HAVE ITS
C               STATE IN THE ARRAY STARTING AT PV(1,I).  (ON ANY GIVEN
C               CALL, ONLY THOSE WORDS IN 'PV' WHICH ARE AFFECTED BY THE
C               FIRST 10 'LIST' ENTRIES (AND BY LIST(12) IF LIBRATIONS ARE
C               ON THE FILE) ARE SET.  THE REST OF THE 'PV' ARRAY
C               IS UNTOUCHED.)  THE ORDER OF COMPONENTS STARTING IN
C               PV(1,I) IS: X,Y,Z,DX,DY,DZ.
C
C               ALL OUTPUT VECTORS ARE REFERENCED TO THE EARTH MEAN
C               EQUATOR AND EQUINOX OF EPOCH J2000. THE MOON STATE IS ALWAYS
C               GEOCENTRIC; THE OTHER NINE STATES ARE EITHER HELIOCENTRIC
C               OR SOLAR-SYSTEM BARYCENTRIC, DEPENDING ON THE SETTING OF
C               COMMON FLAGS (SEE BELOW).
C
C               LUNAR LIBRATIONS, IF ON FILE, ARE PUT INTO PV(K,11) IF
C               LIST(12) IS 1 OR 2.
C
C         NUT   DP 4-WORD ARRAY THAT WILL CONTAIN NUTATIONS AND RATES,
C               DEPENDING ON THE SETTING OF LIST(11).  THE ORDER OF
C               QUANTITIES IN NUT IS:
C
C                        D PSI  (NUTATION IN LONGITUDE)
C                        D EPSILON (NUTATION IN OBLIQUITY)
C                        D PSI DOT
C                        D EPSILON DOT
C
*  Starlink mods
C         OK STATUS RETURN IN CASE OF EPOCH OUT OF RANGE OR I/O ERRORS
C            .FALSE. = ERROR
C            .TRUE.  = OK
*  End of Starlink mods
C
C     PARAMETERS IN COMMON:
C
C     COMMON AREA EPUNIT:
C
C        FILE   INTEGER OF THE UNIT CONTAINING THE EPHEMERIS. DEFAULT = 12
C
C     COMMON AREA STCOMM:
C
C          KM   LOGICAL FLAG DEFINING PHYSICAL UNITS OF THE OUTPUT
C               STATES. KM = .TRUE., KM AND KM/SEC
C                          = .FALSE., AU AND AU/DAY
C               DEFAULT VALUE = .FALSE.  (KM DETERMINES TIME UNIT
C               FOR NUTATIONS AND LIBRATIONS.  ANGLE UNIT IS ALWAYS RADIANS.)
C
C        BARY   LOGICAL FLAG DEFINING OUTPUT CENTER.
C               ONLY THE 9 PLANETS ARE AFFECTED.
C                        BARY = .TRUE. => CENTER IS SOLAR-SYSTEM BARYCENTER
C                             = .FALSE. => CENTER IS SUN
C               DEFAULT VALUE = .FALSE.
C
C       PVSUN   DP 6-WORD ARRAY CONTAINING THE BARYCENTRIC POSITION AND
C               VELOCITY OF THE SUN.
C
C
      SAVE
C
      DOUBLE PRECISION JED(2)
      INTEGER LIST(12)
      DOUBLE PRECISION PV(6,11)
      DOUBLE PRECISION NUT(4)
      LOGICAL OK
C
      DOUBLE PRECISION T(2)
      DOUBLE PRECISION AUFAC
      DOUBLE PRECISION JD(4)
      DOUBLE PRECISION S
C
      EXTERNAL COMDAT
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
      DOUBLE PRECISION BUF(1)
      EQUIVALENCE(IB,BUF)
C
      LOGICAL FIRST
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
      INTEGER L(3,12)
      INTEGER DENUM
      INTEGER LPT(3)
*
*  Variables re-ordered to avoid alignment problems.
*
*  P.T.Wallace   Starlink   14 April 1994
*
      COMMON/EPHHDR/SS,CVAL,AU,EMRAT,NCON,L,DENUM,LPT
C
      INTEGER FILE
      COMMON/EPUNIT/FILE
C
      LOGICAL KM
      LOGICAL BARY
      DOUBLE PRECISION PVSUN(3,2)
      COMMON/STCOMM/KM,BARY,PVSUN
C
      INTEGER NRL
C
C       DATA STATEMENTS
C
      DATA AUFAC/1.D0/
      DATA FIRST/.TRUE./
      DATA NRL/0/
C
C
C       ENTRY POINT -- 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL EPHOPN(.TRUE.)
        IF(KM) THEN
          T(2)=SS(3)*86400.D0
        ELSE
          T(2)=SS(3)
          AUFAC=1.D0/AU
        ENDIF
      ENDIF
C
C       MAIN ENTRY POINT -- CHECK EPOCH AND READ RIGHT RECORD
C
      S=JED(1)-.5D0
      CALL SPLIT(S,JD(1))
      CALL SPLIT(JED(2),JD(3))
      JD(1)=JD(1)+JD(3)+.5D0
      JD(2)=JD(2)+JD(4)
      CALL SPLIT(JD(2),JD(3))
      JD(1)=JD(1)+JD(3)
C
C       ERROR RETURN OF EPOCH OUT OF RANGE
C
      IF ( JD(1).LT.SS(1) .OR. JD(1)+JD(4).GT.SS(2) ) GO TO 1000
C
C       CALCULATE RECORD # AND RELATIVE TIME IN INTERVAL
C
      NR=IDINT((JD(1)-SS(1))/SS(3))+3
      IF(JD(1).EQ.SS(2)) NR=NR-1
      T(1)=((JD(1)-(DBLE(NR-3)*SS(3)+SS(1)))+JD(4))/SS(3)
C
C       READ CORRECT RECORD IF NOT IN CORE
C
      IF(NR.NE.NRL) THEN
        NRL=NR
        READ(UNIT=FILE,REC=NR,ERR=1000)(IB(K),K=1,IBSZ)
      ENDIF
C
C       INTERPOLATE SSBARY SUN
C
      CALL INTERP(BUF(L(1,11)),T,L(2,11),3,L(3,11),2,PVSUN)
      DO 6 I=1,6
    6 PVSUN(I,1)=PVSUN(I,1)*AUFAC
C
C       CHECK AND INTERPOLATE WHICHEVER BODIES ARE REQUESTED
C
      DO 3 I=1,10
      IF(LIST(I).GT.0) THEN
        CALL INTERP(BUF(L(1,I)),T,L(2,I),3,L(3,I),LIST(I),PV(1,I))
        DO 4 J=1,LIST(I)*3
        IF(I.LE.9 .AND. .NOT.BARY) THEN
          PV(J,I)=PV(J,I)*AUFAC-PVSUN(J,1)
        ELSE
          PV(J,I)=PV(J,I)*AUFAC
        ENDIF
    4   CONTINUE
      ENDIF
    3 CONTINUE
C
C       DO NUTATIONS IF REQUESTED (AND IF ON FILE)
C
      IF(LIST(11).GT.0 .AND. L(2,12).GT.0)
     * CALL INTERP(BUF(L(1,12)),T,L(2,12),2,L(3,12),LIST(11),NUT)
C
C       GET LIBRATIONS IF REQUESTED (AND IF ON FILE)
C
      IF(LPT(2).GT.0 .AND. LIST(12).GT.0)
     * CALL INTERP(BUF(LPT(1)),T,LPT(2),3,LPT(3),LIST(12),PV(1,11))
C
*  Starlink mods
C       SET STATUS RETURN TO OK, AND EXIT

      OK = .TRUE.
      GO TO 9999

C       ERRORS: SET STATUS RETURN TO ERROR VALUE
 1000 CONTINUE
      OK = .FALSE.

*  End of Starlink mods

C       EXIT
 9999 CONTINUE

      END
