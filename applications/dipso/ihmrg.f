*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE IHMRG
*
*   MERGE TWO STACK ENTRIES & PLACE RESULT IN CURRENT ARRAYS
*
*   METHOD:
*      ABUTTS NON-OVERLAPPING DATA
*      OVERLAPPING DATA:  CREATES 'SW' WAVELENGTH GRID
*      (BY INTERPOLATING OVER 'BREAKS');  INTERPOLATES
*      LW FLUXES ONTO THIS GRID WITH GAUSSIAN WEIGHTING;
*      GIVES WEIGHTED AVERAGE OF SW & LW FLUXES.   IF
*      VARRAY(5) = 0, OVERLAPPING SWP/LWR IUE DATA ARE
*      FURTHER WEIGHTED BY THE INSTRUMENTAL SENSITIVITY
*      FUNCTION.
*
*   IMPORTS:
*      VARRAY(10) -
*        NINT(VARRAY(1)) - 1st STACK entry
*        NINT(VARRAY(2)) - 2nd
*        VARRAY(3) - WEIGHT (EXPOSURE TIME) OF ENTRY 1
*        VARRAY(4) -                                 2
*        VARRAY(5) - NON-ZERO VALUE OVERRIDES IUE WEIGHTING
*                  - VALUE OF -1 GIVES SUM INSTEAD OF AVERAGE
*                  - VALUE LE -2 GIVES PRODUCT INSTEAD OF AVERAGE
*      STKSZE   (INTEGER) [MAXIMUM] SIZE OF STACK
*      XSTACK   (REAL) X VALUES FOR STACK
*      YSTACK   (REAL) Y VALUES FOR STACK
*      MAXSTK   (INTEGER) MAXIMUM NUMBER OF STACK ENTRIES
*      POINTR   (INTEGER) ARRAY OF START POINTS
*      STKNPT   (INTEGER) ARRAY OF ENTRY SIZES
*      BPOINT   (INTEGER) ARRAY OF FIRST BREAKS
*      BSTSZE   (INTEGER) [MAXIMUM] SIZE OF STACK BREAK DATA
*      BSTACK   (INTEGER) ARRAY OF STACK BREAK POINTS
*      BSTNPT   (INTEGER) ARRAY OF BREAKS PER SPECTRUM
*      ASIZE1   (INTEGER) [MAXIMUM] SIZE OF CURRENT ARRAYS
*      MAXBRK   (INTEGER) MAXIMUM NO. OF BREAKS ALLOWED
*
*      WORKSZ   (INTEGER) SIZE OF WORK SPACE
*      WORK     (REAL) ARRAY OF WORK SPACE
*
*   EXPORTS:
*      NBREAK   (INTEGER) NUMBER OF BREAKS
*      BREAK    (INTEGER) ARRAY OF BREAK POINTS
*      NPOINT   (INTEGER) NUMBER OF DATUM POINTS
*      WAVE     (REAL) ARRAY OF X VALUES
*      FLUX     (REAL) ARRAY OF Y VALUES
*      TITLE    (CHARACTER) STRING
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE IHMRG(VARRAY,STKSZE,XSTACK,YSTACK,MAXSTK,POINTR,
     : STKNPT,BPOINT,BSTSZE,BSTACK,BSTNPT,ASIZE1,WAVE,
     : FLUX,TITLE,MAXBRK,BREAK,NPOINT,NBREAK,WORKSZ,WORK)
*
*   DECLARATIONS
*
       IMPLICIT NONE
*
*
       INTEGER STKSZE, MAXSTK, NONSTK, ASIZE1
       INTEGER MAXBRK, NPOINT, NBREAK, WORKSZ
       INTEGER BSTSZE
*
*
       INTEGER POINTR(MAXSTK), STKNPT(MAXSTK), BPOINT(MAXSTK)
       INTEGER BREAK(MAXBRK), BSTACK(BSTSZE), BSTNPT(MAXSTK)
*
       REAL XSTACK(STKSZE), YSTACK(STKSZE)
       REAL WAVE(ASIZE1), FLUX(ASIZE1)
       REAL WORK(WORKSZ)
       REAL VARRAY(10)
*
*
       INTEGER NSTKS, NSTKL
       INTEGER PTS, PTL
       INTEGER NBRKS, NBRKL
       INTEGER BRK1S, BRK1L
       INTEGER I, J, K, L, M, N
       INTEGER ITEMP
       INTEGER BRK, BRK1
       INTEGER NSUM, NSPTS, NUMX
       INTEGER ITAG, NPTS, NPTL
       INTEGER MODE
       INTEGER K0, K1
*
       REAL TEMP
       REAL WTS, WTL
       REAL WTOT
       REAL WSPACE, DWAV, XSPACE
       REAL WV, W1, W2
       REAL GWT, WTFS, WTFL, FS, FL, TOTWT
       REAL DWAVE, FTOT, WL
       REAL FLX, WT
       REAL WAVESL, ADDWV1
       REAL WMRG1, ENDMRG
       REAL SIGMRG
       REAL A, B, C, D, E, F
       REAL TSTWV1, TSTWV2, TSTVAL
       REAL WVX, WV1, WV2
       INTEGER IERR, INDEX
       LOGICAL OK
*
       CHARACTER*80 TITLE
*
*   FIND 'SHORT' AND 'LONG' DATA, AND ASSOCIATED VARIABLES
*
       MODE = NINT(VARRAY(5))
       IF (MODE.LT.0) THEN
          VARRAY(3) = 1.0
          VARRAY(4) = 1.0
       ENDIF
       NSTKS = NINT(VARRAY(1))
       NSTKL = NINT(VARRAY(2))
       WTS = VARRAY(3)
       WTL = VARRAY(4)
*
       IF (XSTACK(POINTR(NSTKL)).LT.XSTACK(POINTR(NSTKS))) THEN
          ITEMP = NSTKS
          NSTKS = NSTKL
          NSTKL = ITEMP
          TEMP = WTS
          WTS = WTL
          WTL = TEMP
       ENDIF
*
       NPTS = STKNPT(NSTKS)
       NPTL = STKNPT(NSTKL)
       PTS = POINTR(NSTKS)
       PTL = POINTR(NSTKL)
       NBRKS = BSTNPT(NSTKS)
       NBRKL = BSTNPT(NSTKL)
       BRK1S = BPOINT(NSTKS)
       BRK1L = BPOINT(NSTKL)
*
*   FIND OUT WHERE OVERLAP (IF ANY) STARTS
*
       J = 0
       DO 100 I = 1, NPTS
          IF (XSTACK(PTS-1+I).GT.XSTACK(PTL)) GOTO 500
          J = J + 1
  100  CONTINUE
*
*   NO OVERLAP
*
       BRK = BRK1S
       NBREAK = 0
       DO 200 I = 1, NPTS
          IF (I.GE.ASIZE1) GOTO 2800
          ITEMP = PTS - 1 + I
          WAVE(I) = XSTACK(ITEMP)
          FLUX(I) = YSTACK(ITEMP)
          IF (ITEMP.GE.(BSTACK(BRK)+PTS-1)) THEN
             NBREAK = NBREAK + 1
             IF (NBREAK.GE.MAXBRK) GOTO 2700
             BREAK(NBREAK) = I
             BRK = BRK + 1
          ENDIF
  200  CONTINUE
*
       IF (BREAK(NBREAK).NE.NPTS) THEN
          NBREAK = NBREAK + 1
          IF (NBREAK.GE.MAXBRK) GOTO 2700
          BREAK(NBREAK) = NPTS
       ENDIF
*
       BRK = BRK1L
       DO 300 I = 1, NPTL
          J = NPTS + I
          IF (J.GE.ASIZE1) GOTO 2800
          ITEMP = PTL - 1 + I
          WAVE(J) = XSTACK(ITEMP)
          FLUX(J) = YSTACK(ITEMP)
          IF (ITEMP.GE.(BSTACK(BRK)+PTL-1)) THEN
             NBREAK = NBREAK + 1
             IF (NBREAK.GE.MAXBRK) GOTO 2700
             BREAK(NBREAK) = J
             BRK = BRK + 1
          ENDIF
  300  CONTINUE
*
       NPOINT = J
       IF (BREAK(NBREAK).NE.NPOINT) THEN
          NBREAK = NBREAK + 1
          IF (NBREAK.GE.MAXBRK) GOTO 2700
          BREAK(NBREAK) = NPOINT
       ENDIF
*
       DO 400 I = 12, 80
          TITLE(I:I) = ' '
  400  CONTINUE
       TITLE(1:11) = 'MERGEd data'
       GOTO 3000
*
*   OVERLAPPING DATA
*
*
*   Find first BREAK in overlap SW data
*
  500  CONTINUE
       DO 600 I = 1, NBRKS
          BRK = BRK1S - 1 + I
          IF ((BSTACK(BRK)+PTS-1).GE.(PTS-1+J)) GOTO 700
  600  CONTINUE
       WRITE (*,'(''   MERGE:  end of SW data not found'')')
       GOTO 3000
*
*   Find mean SW wavelength spacing
*
  700  CONTINUE
       BRK1 = BRK
       NSUM = 0
       WTOT = 0.0
*
       IF (NPTS.GT.J) THEN
          DO 750 I = J, NPTS - 1
             ITEMP = PTS - 1 + I
             IF (ITEMP.GE.(BSTACK(BRK)+PTS-1)) THEN
                BRK = BRK + 1
             ELSE
                NSUM = NSUM + 1
                WTOT = WTOT + (XSTACK(PTS+I)-XSTACK(ITEMP))
             ENDIF
  750     CONTINUE
       ENDIF
*
       IF (NSUM.EQ.0) THEN
          BRK = BRK1S
          DO 800 I = 1, J
             ITEMP = PTS - 1 + I
             IF (ITEMP.GE.(BSTACK(BRK)+PTS-1)) THEN
                BRK = BRK + 1
             ELSE
                NSUM = NSUM + 1
                WTOT = WTOT + (XSTACK(PTS+I)-XSTACK(ITEMP))
             ENDIF
  800     CONTINUE
       ENDIF
*
       IF (NSUM.EQ.0) THEN
          NSUM = NPTS - 1
          WTOT = XSTACK(PTS-1+NPTS) - XSTACK(PTS)
       ENDIF
*
       IF (NSUM.EQ.0) THEN
          WRITE (*,'(''   MERGE:  algorithm failure'')')
          GOTO 3000
       ELSE
          WSPACE = WTOT/REAL(NSUM)
       ENDIF
*
*   LOAD SW DATA INTO WORK ARRAY
*
       K = 0
       BRK = BRK1
       DO 900 I = J, NPTS
          ITEMP = PTS - 1 + I
          K = K + 1
          IF (K.GE.WORKSZ) GOTO 2600
          WORK(K) = XSTACK(ITEMP)
          K = K + 1
          IF (K.GE.WORKSZ) GOTO 2600
          WORK(K) = YSTACK(ITEMP)
          IF (ITEMP.GE.(BSTACK(BRK)+PTS-1)) THEN
             BRK = BRK + 1
             IF (ITEMP.LT.(NPTS+PTS-1)) THEN
                DWAV = XSTACK(PTS+I) - XSTACK(ITEMP)
                NUMX = NINT(DWAV/WSPACE) - 1
                IF (NUMX.GT.0) THEN
                   XSPACE = DWAV/REAL(NUMX+1)
                   ADDWV1 = WORK(K-1)
                   DO 805 L = 1, NUMX
                      K = K + 1
                      IF (K.GE.WORKSZ) GOTO 2600
                      WORK(K) = ADDWV1 + L*XSPACE
                      K = K + 1
                      IF (K.GE.WORKSZ) GOTO 2600
                      WORK(K) = 0.0
  805              CONTINUE
                ENDIF
             ENDIF
          ENDIF
  900  CONTINUE
*
       NUMX = K/2
*
*   FIND LW SPACING
*
       WAVESL = WORK(K-1)
       NSUM = 0
       WTOT = 0
       BRK = BRK1L
       DO 1000 I = 1, NPTL - 1
          ITEMP = PTL - 1 + I
          IF (ITEMP.GE.(BSTACK(BRK)+PTL-1)) THEN
             BRK = BRK + 1
          ELSE
             IF (XSTACK(ITEMP).GE.WAVESL .AND. NSUM.GT.0) GOTO 1100
             NSUM = NSUM + 1
             WTOT = WTOT + (XSTACK(PTL+I)-XSTACK(ITEMP))
          ENDIF
 1000  CONTINUE
*
 1100  CONTINUE
       IF (NSUM.LE.0) THEN
          WRITE (*,'('' MERGE:  algorithm has failed'')')
          GOTO 3000
       ELSE
          XSPACE = WTOT/REAL(NSUM)
       ENDIF
*
*   SIGMA for weighting LW data
*
       SIGMRG = MAX(XSPACE,WSPACE)
*
*   CALCULATE LW FLUXES AT SW GRID POINTS
*
       L = 2*NUMX
       K1 = 1
       DO 1200 I = 1, NUMX
          WV = WORK(2*I-1)
          FLX = 0.0
          WT = 0.0
          IF (WV.GE.XSTACK(PTL)) THEN
             W1 = WV - SIGMRG
             W2 = WV + SIGMRG
             W1 = MAX(W1,XSTACK(PTL))
             W2 = MIN(W2,WAVESL)
             K0 = K1
             DO 1120 K = K0, NPTL
                WL = XSTACK(PTL-1+K)
                IF (WL.LT.W1) THEN
                   K1 = K
                ELSEIF (WL.GT.W2) THEN
                   GOTO 1140
                ELSE
                   DWAVE = ABS(WL-WV)/SIGMRG
                   GWT = 1.0 - DWAVE
                   FLX = FLX + (YSTACK(PTL-1+K))*GWT
                   WT = WT + GWT
                ENDIF
 1120        CONTINUE
 1140        CONTINUE
             IF (WT.NE.0.0) THEN
                FLX = FLX/WT
             ENDIF
          ENDIF
          L = L + 1
          IF (L.GE.WORKSZ) GOTO 2600
          WORK(L) = FLX
 1200  CONTINUE
*
*   ADD UP DATA IN OVERLAP REGION
*
*   IUE WEIGHTING?
       IF (MODE.EQ.0 .AND. WORK(1).GE.1800.0 .AND.
     : WORK(2*NUMX-1).LE.2100.0) THEN
          A = 1.84
          B = 3.0
          C = 5.94666E-02
          D = 8.08533E-04
          E = -2.11333E-02
          F = 8.08889E-05
       ELSE
          A = 1.0
          B = 1.0
          C = 0.0
          D = 0.0
          E = 0.0
          F = 0.0
       ENDIF
*
       DO 1300 I = 1, NUMX
          FS = WORK(2*I)
          FL = WORK(2*NUMX+I)
          WV = WORK(2*I-1)
          WTFS = WTS/A
          DWAVE = WV - 1950.0
          IF (DWAVE.LE.1950.0) THEN
             WTFL = WTL/(B+DWAVE*(C+DWAVE*D))
          ELSE
             WTFL = WTL/(B+DWAVE*(E+DWAVE*F))
          ENDIF
          FS = FS*WTFS
          FL = FL*WTFL
          IF (FS.EQ.0.0) THEN
             TOTWT = WTFL
          ELSEIF (FL.EQ.0.0) THEN
             TOTWT = WTFS
          ELSE
             TOTWT = WTFS + WTFL
          ENDIF
*          FTOT = FS + FL
          IF (MODE.LT.0) TOTWT = 1.0
          IF (MODE.GE.-1) THEN
             FTOT = FS + FL
          ELSE
             FTOT = FS*FL
          ENDIF
          IF (TOTWT.EQ.0.0) THEN
             WORK(2*I) = 0.0
          ELSE
             WORK(2*I) = FTOT/TOTWT
          ENDIF
 1300  CONTINUE
*
*   SHOVEL DATA INTO 'CURRENT' ARRAYS
*
       BRK = BRK1S
       WMRG1 = WORK(1)
       NBREAK = 0
       L = 0
*
       DO 1400 I = 1, NPTS
          WV = XSTACK(PTS-1+I)
          IF (WV.GE.WMRG1) GOTO 1500
          L = L + 1
          IF (L.GE.ASIZE1) GOTO 2800
          WAVE(L) = WV
          FLUX(L) = YSTACK(PTS-1+I)
          IF ((PTS-1+I).GE.(BSTACK(BRK)+PTS-1)) THEN
             BRK = BRK + 1
             NBREAK = NBREAK + 1
             IF (NBREAK.GE.MAXBRK) GOTO 2700
             BREAK(NBREAK) = L
          ENDIF
 1400  CONTINUE
*
 1500  CONTINUE
       ITAG = 0
       DO 1600 I = 1, NUMX
          IF (WORK(2*I).NE.0.0) THEN
             L = L + 1
             IF (L.GE.ASIZE1) GOTO 2800
             WAVE(L) = WORK(2*I-1)
             FLUX(L) = WORK(2*I)
             ITAG = 0
          ELSEIF (ITAG.EQ.0) THEN
             ITAG = 1
             NBREAK = NBREAK + 1
             IF (NBREAK.GE.MAXBRK) GOTO 2700
             BREAK(NBREAK) = L
          ENDIF
 1600  CONTINUE
       ENDMRG = WAVE(L)
*
       IF (XSTACK(PTL-1+NPTL).LE.ENDMRG) GOTO 2300
       DO 1700 I = 1, NPTL
          ITEMP = PTL - 1 + I
          WV = XSTACK(ITEMP)
          IF (WV.GT.ENDMRG) THEN
             DO 1620 J = 1, NBRKL
                BRK = BSTACK(BRK1L-1+J) + PTL - 1
                IF (BRK.EQ.(ITEMP-1)) GOTO 1800
 1620        CONTINUE
             GOTO 1900
          ENDIF
*
 1700  CONTINUE
       GOTO 1900
 1800  CONTINUE
       NBREAK = NBREAK + 1
       IF (NBREAK.GE.MAXBRK) GOTO 2700
       BREAK(NBREAK) = L
       IF (NBREAK.GT.1) THEN
          IF (BREAK(NBREAK).EQ.BREAK(NBREAK-1)) NBREAK = NBREAK - 1
       ENDIF
*
 1900  CONTINUE
       DO 2000 I = 1, NBRKL
          BRK = BRK1L - 1 + I
          IF ((BSTACK(BRK)+PTL-1).GE.ITEMP) GOTO 2100
 2000  CONTINUE
       WRITE (*,'(''   MERGE:  end of LW data not found'')')
       NPOINT = 0
       GOTO 3000
*
 2100  CONTINUE
       DO 2200 I = 1, NPTL
          WV = XSTACK(PTL-1+I)
          IF (WV.GT.ENDMRG) THEN
             L = L + 1
             IF (L.GE.ASIZE1) GOTO 2800
             WAVE(L) = WV
             FLUX(L) = YSTACK(PTL-1+I)
             IF ((PTL-1+I).GE.(BSTACK(BRK)+PTL-1)) THEN
                BRK = BRK + 1
                NBREAK = NBREAK + 1
                IF (NBREAK.GE.MAXBRK) GOTO 2700
                BREAK(NBREAK) = L
             ENDIF
          ENDIF
 2200  CONTINUE
*
*
*
 2300  CONTINUE
       NPOINT = L
       IF (BREAK(NBREAK).NE.NPOINT) THEN
          NBREAK = NBREAK + 1
          IF (NBREAK.GE.MAXBRK) GOTO 2700
          BREAK(NBREAK) = NPOINT
       ENDIF
*
*  Check that no spurious data have been introduced
*
       TSTWV1 = XSTACK(PTS+NPTS-1)
       TSTWV2 = XSTACK(PTL)
       TSTVAL = -321.654
       IERR = 0

       DO 2400 I = 1, NPOINT
          WVX = WAVE(I)
          IF (WVX.GE.TSTWV2 .AND. WVX.LE.TSTWV1) THEN
             DO 2320 J = 1, NBRKS - 1
                INDEX = PTS - 1 + BSTACK(BRK1S+J-1)
                WV1 = XSTACK(INDEX)
                WV2 = XSTACK(INDEX+1)
                IF (WVX.GT.WV1 .AND. WVX.LT.WV2) THEN
                   DO 2305 K = 1, NBRKL - 1
                      INDEX = PTL - 1 + BSTACK(BRK1L+K-1)
                      WV1 = XSTACK(INDEX)
                      WV2 = XSTACK(INDEX+1)
                      IF (WVX.GT.WV1 .AND. WVX.LT.WV2) THEN
                         FLUX(I) = TSTVAL
                         IERR = IERR + 1
                      ENDIF
 2305              CONTINUE
                ENDIF
 2320        CONTINUE
          ENDIF
 2400  CONTINUE

       IF (IERR.NE.0) THEN
          CALL SRTBRK
     :    (ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,TSTVAL,OK)
          IF (.NOT.OK) THEN
             WRITE (*,'(''   MERGE: error sorting breaks'')')
             NPOINT = 0
             TITLE = ' '
             GOTO 3000
          ENDIF
       ENDIF
       DO 2500 I = 12, 80
          TITLE(I:I) = ' '
 2500  CONTINUE
       TITLE(1:11) = 'MERGEd data'
       WRITE (*,'(''   MERGE:  completed'')')
       GOTO 3000
*
*  OVERFLOW ESCAPES
*
 2600  CONTINUE
       WRITE (*,
     : '(''   MERGE:  too many overlapping data - arrays overflow'')')
       GOTO 3000
*
 2700  CONTINUE
       WRITE (*,'(''   MERGE:  too many BREAKs in MERGEd data'')')
       GOTO 3000
*
 2800  CONTINUE
       WRITE(*,'(''   MERGE:  too many MERGEd data;  array overflow'')')

 3000  CONTINUE

       END
