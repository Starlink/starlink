C-------------------- MAIN PROGRAM 'TESTEPH' ---------------------------
C
C     THIS PROGRAM READS THE JPL EPHEMERIS AND INTERPOLATES
C     ALL POSSIBLE PLANET COMBINATIONS AT VARIOUS JULIAN DATES.
C     IT COMPARES THE RESULTS WITH FIGURES CONTAINED IN A FILE
C     THAT WAS GENERATED AT JPL AND PRINTS DIFFERENCES THAT
C     EXCEED A SPECIFIED TOLERANCE.
C
*------------------------------------------------------------------------
*
*  !!! The original JPL source code has been modified as follows:
*
*      *  The Starlink version of the PLEPH routine is used.
*         In earlier releases from JPL, there was no PLEPH
*         routine.  The Starlink PLEPH was implemented following
*         suggestions from JPL in unpublished correspondence.
*         The latest JPL releases include a PLEPH implementation.
*         The version supplied in the circa 1990 JPL release which
*         forms the basis of the Starlink release was not compatible
*         with the old Starlink PLEPH, in that it used a `*' dummy
*         argument to allow calling programs to specify an error
*         return (a technique discouraged, incidentally, by the
*         Starlink Fortran programming standard).
*
*      *  An EXTERNAL COMDAT has been added, to make sure the
*         BLOCK DATA subprogram COMDAT is included at link time.
*
*  A.J.J.Broderick   Starlink   6 November 1992
*  P.T.Wallace    Starlink   25 April 1994
*
*------------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      EXTERNAL COMDAT
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
C       MISCELLANEOUS INTERNAL STORAGE
C
      CHARACTER*6 NAM(400)
      CHARACTER*6 JNAM(400)
      CHARACTER*72 LBL
C
      DOUBLE PRECISION VAL(400)
      DOUBLE PRECISION JVAL(400)
      DOUBLE PRECISION SSS(3)
      DOUBLE PRECISION JSS(3)
      DOUBLE PRECISION RRD(6)
      DOUBLE PRECISION JRRD(6)
      DOUBLE PRECISION PW2(221)
C
      DOUBLE PRECISION JED
      DOUBLE PRECISION CFRAC
C
      INTEGER N
      INTEGER JIPT(39)
      INTEGER TARG
      INTEGER CENT
      INTEGER LTARG
      INTEGER NEX
      INTEGER NFR
      INTEGER NER
      INTEGER NCMP
      INTEGER ENM(3,400)
      LOGICAL OK                      !!! added by Starlink
C
C       DATA STATEMENTS
C
      DATA CFRAC/1.D-15/
      DATA NEX/0/
      DATA NFR/0/
      DATA NER/0/
      DATA NCMP/0/
      DATA LTARG/0/
C
C       CHECK EPHEMERIS RECORD SIZE
C
      WRITE(6,112)IBSZ
  112 FORMAT('1This software expects the ephemeris records to have',
     * I6,' words each.')
C
C       BUILD UP TABLE OF POWERS OF 2 FOR D.P. DECODING
C
      PW2(110)=.5D0
      PW2(111)=1.D0
      PW2(112)=2.D0
C
      DO 50 K=1,109
      PW2(110-K)=PW2(111-K)*PW2(110)
      PW2(112+K)=PW2(111+K)*PW2(112)
   50 CONTINUE
C
C       OPEN INPUT FILE 'TESTEPHINPUT', CONTAINING JPL VALUES
C
      OPEN(UNIT=8,
     *     FILE='testephinput.dat',
     *     FORM='FORMATTED',
     *     ACCESS='SEQUENTIAL',
     *     STATUS='OLD')
C
C       READ AND PRINT TEST FILE LABEL
C
      READ(8,210)LBL
  210 FORMAT(A)
      WRITE(6,211)LBL
  211 FORMAT('0',A,//)
C
C       CALL 'CONST' TO FILL COMMON AREA
C
      CALL CONST(NAM,VAL,SSS,N)
      DO 3 I=1,3
      WRITE(6,212)(TTL(K,I),K=1,13)
  212 FORMAT(1X,13A6)
    3 CONTINUE
C
C       GET DE-NUMBER, AU, AND EMRAT
C
      READ(8,109)IDNUM,AUX,EMRX
  109 FORMAT(I5,2(1PD23.15))
      WRITE(6,110)DENUM,IDNUM
  110 FORMAT('0DE Numbers: File =',I4,', Test deck =',I4)
      IF(IDNUM.NE.DENUM) STOP 'Non-matching DE Numbers.'
      WRITE(6,111)AU,AUX,EMRAT,EMRX
  111 FORMAT('0   AU: File =',1PD23.15,', Test deck =',1PD23.15,/,
     * ' EMRAT: File =',1PD23.15,', Test deck =',1PD23.15)
C
C       READ NBR OF CONSTANTS FROM TEST OUTPUT FILE
C
      READ(8,100)NJ
  100 FORMAT(I6)
      WRITE(6,200)NJ,N
  200 FORMAT('0',I6,' constants expected, and',I6,' read.')
C
C       READ TEST START AND STOP DATES; COMPARE WITH 'CONST' VALUES
C
      READ(8,101)JSS
  101 FORMAT(2F11.1,F8.3)
      WRITE(6,103)
  103 FORMAT('0Comparing start, stop, and step.')
      DO 11 I=1,3
   11 IF(SSS(I).NE.JSS(I)) WRITE(6,201)I,SSS(I),JSS(I)
  201 FORMAT('0 Non-matching limit',I2,'. File value =',F11.1,
     * ', test deck value =',F11.1)
C
C       READ TEST CONSTANT NAMES AND VALUES; DO FRACTIONAL COMPARE
C       (PRINT ERROR MESSAGE IF DIFFERENCE NOT WITHIN VALUE OF 'CFRAC')
C
      READ(8,102)(JNAM(K),(ENM(K1,K),K1=1,3),K=1,N)
  102 FORMAT(1X,A6,I3,2I11,1X,A6,I3,2I11)
      DO 22 K=1,N
      IF(ENM(2,K).EQ.0) THEN
        JVAL(K)=0.D0
      ELSE
        NX=81+ENM(1,K)
        JVAL(K)=DBLE(ENM(2,K))*PW2(NX)
     *         +DBLE(ENM(3,K))*PW2(NX-30)
      ENDIF
   22 CONTINUE
C
      WRITE(6,104)CFRAC
  104 FORMAT('0Comparing constants. Fractional compare value =',
     * 1PD12.3)
      DO 12 I=1,N
      IF(JVAL(I).EQ.VAL(I)) THEN
        FRAC=0.D0
        NEX=NEX+1
      ELSE
        FRAC=DABS((JVAL(I)-VAL(I))/DMAX1(DABS(JVAL(I)),DABS(VAL(I))))
      ENDIF
      IF(JNAM(I).NE.NAM(I) .OR. FRAC.GT.CFRAC) THEN
        WRITE(6,202)I,JNAM(I),JVAL(I),NAM(I),VAL(I),FRAC
  202   FORMAT(' Constant',I4,' ',A6,1PD23.15,6X,A6,1PD23.15,1PD12.3)
      ELSEIF(FRAC.NE.0.D0) THEN
        NFR=NFR+1
      ENDIF
   12 CONTINUE
C
      WRITE(6,208)N,NEX,NFR
  208 FORMAT('0Of',I4,' constants,',I4,' compared exactly and',
     * I4,' others were within fractional limit.')
C
      NEX=0
      NFR=0
C       READ AND CHECK ALL POINTERS
C
      READ(8,108)JIPT
  108 FORMAT((3I10))
      WRITE(6,105)
  105 FORMAT('0Comparing pointers.')
      DO 13 I=1,39
      IF(I.LE.36) THEN
        KPT=IPT(I)
      ELSE
        KPT=LPT(I-36)
      ENDIF
      IF(JIPT(I).NE.KPT) WRITE(6,204)I,JIPT(I),KPT
  204 FORMAT(' Compare error for pointer',I3,':',2I3)
   13 CONTINUE
C
C      THIS SECTION READS THE JPL OUTPUT AND COMPARES INTERPOLATED RESULTS.
C      ANY FRACTIONAL DIFFERENCE EXCEEDING 'CFRAC' IS PRINTED.
C
      WRITE(6,106)
  106 FORMAT('0Interpolating and comparing ephemeris values.',/)
C
C       START OF MAIN READING AND INTERPOLATION LOOP
C
   14 READ(8,207,END=1)JED,TARG,CENT,((ENM(K1,K2),K1=1,3),K2=1,6)
  207 FORMAT(F16.5,2I3,/,1X,3(I3,2I11),/,1X,3(I3,2I11))
C
      IF(JED.GT.SS(2) .OR. JED.EQ.0.D0) THEN
        GO TO 1
      ELSEIF(JED.LT.SS(1)) THEN
        GO TO 14
      ENDIF
C
      IF(TARG.NE.LTARG) THEN
        IF(LTARG.NE.0) WRITE(6,203)LTARG
  203   FORMAT(' End of testing loop for target',I3)
        LTARG=TARG
        WRITE(6,205)LTARG,JED
  205   FORMAT('  Begin testing loop for target',I3,'. JED =',F14.5)
      ENDIF
C
C       NUTATIONS IF TARG = 14 -- OTHERWISE, EPHEMERIS OR LIBRATIONS
C
      IF(TARG.EQ.14) THEN
        IX=4
      ELSE
        IX=6
      ENDIF
C
      DO 23 K=1,IX
      IF(ENM(2,K).EQ.0) THEN
        JRRD(K)=0.D0
      ELSE
        NX=81+ENM(1,K)
        JRRD(K)=DBLE(ENM(2,K))*PW2(NX)
     *         +DBLE(ENM(3,K))*PW2(NX-30)
      ENDIF
   23 CONTINUE
C
C       CALL EPHEMERIS INTERPOLATING SUBROUTINE 'PLEPH'
C
      CALL PLEPH(JED,TARG,CENT,RRD,OK)  ! Starlink-
      IF (.NOT.OK) GO TO 2              ! modified

      DO 15 K=1,IX
      IF(RRD(K).EQ.JRRD(K)) THEN
        FRAC=0.D0
        NEX=NEX+1
      ELSE
        FRAC=DABS((JRRD(K)-RRD(K))/DMAX1(DABS(JRRD(K)),DABS(RRD(K))))
      ENDIF
      NCMP=NCMP+1
      IF(FRAC.GT.CFRAC) THEN
        WRITE(6,206)TARG,CENT,K,JED,JRRD(K),RRD(K),FRAC
  206   FORMAT(' TARG=',I2,' CENT=',I2,' COMPONENT=',I2,
     *   ' JED=',F15.5,/,
     *   ' JPL VAL =',1PD23.15,'  THIS VAL =',1PD23.15,'  FRAC DIFF=',
     *   1PD12.3)
        NER=NER+1
      ELSEIF(FRAC.NE.0.D0) THEN
        NFR=NFR+1
      ENDIF
   15 CONTINUE
C
      GO TO 14
C
C       END OF TEST
C
    1 IF(JED.EQ.0.D0) THEN
        WRITE(6,203)LTARG
        READ(8,210)LBL
        WRITE(6,107)LBL
  107   FORMAT('0',A)
      ELSE
        WRITE(6,113)JED,SSS(2)
  113   FORMAT('0End of testing -- JED',F14.5,' exceeds file limit',
     *   F10.1)
      ENDIF
C
      WRITE(6,209)NCMP,NEX,NFR,NER
  209 FORMAT('0Of',I5,' comparisons:',//,
     * I8,' were exact',/,
     * I8,' others were within the fractional limit',/,
     * I8,' exceeded the fractional limit.',/)
      STOP 'Normal end of test'
    2 STOP 'Error return from PLEPH'
      END
