      SUBROUTINE TRIPLOT(BIN_ERR,
     &IT,Q,U,QV,UV,WAVE,NPTS,TMAX,TMIN,
     &PMAX,PMIN,IMAX,IMIN,WMIN,WMAX,TITLE,BOX,WAUTOLIM,
     &IAUTOLIM,PAUTOLIM,TAUTOLIM,LROT,CROT,T_HI,T_LOW,POLY,XLAB,ILAB,
     &PLAB,PFLAB,TLAB,
     &PFLUX,PFMAX,PFMIN,PFAUTOLIM,LCOL,LSTYLE,HIST,MARK,
     &QU_TRIPLOT,QAUTOLIM,QMAX,QMIN,UAUTOLIM,UMAX,UMIN)
C+
C
C Subroutine:
C
C   T R I P L O T
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C BIN_ERR (<), IT (<), Q (<), U (<), QV (<), UV (<), WAVE (<),
C NPTS (<), TMAX (<), TMIN (<), PMAX (<), PMIN (<), IMAX (<), IMIN (<),
C WMIN (<), WMAX (<), TITLE (<), BOX (<), WAUTOLIM (<),
C IAUTOLIM (<), PAUTOLIM (<), TAUTOLIM (<), LROT (<), CROT (<),
C T_HI (<), T_LOW (<), POLY (<), XLAB (<), ILAB (<), PLAB (<), PFLAB (<),
C TLAB (<), PFLUX (<), PFMAX (<), PFMIN (<), PFAUTOLIM (<), LCOL (<),
C LSTYLE (<), HIST (<), MARK (<), QU_TRIPLOT (<), QAUTOLIM (<), QMAX (<),
C QMIN (<), UAUTOLIM (<), UMAX (<), UMIN (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C
C Plots the polarization spectrum in 'triplot' form:
C Stokes I in the bottom third, percentage polarization in the middle
C third and PA in the top third.
C
C
C
C-
C
      IMPLICIT NONE
C
      INCLUDE 'array_size.inc'
      INTEGER J
      INTEGER LCOL,LSTYLE
      INTEGER NEW_NPTS
C
C The polarization spectrum
C
      INTEGER NPTS
      REAL IT(*)
      REAL Q(*)
      REAL U(*)
      REAL UV(*)
      REAL QV(*)
      REAL WAVE(*)
C
C
C Plotting ranges...
C
      REAL TMIN
      REAL TMAX
      REAL PMAX
      REAL PMIN
      REAL IMAX
      REAL IMIN
      REAL WMIN
      REAL WMAX
      REAL QMAX,QMIN,UMAX,UMIN
      REAL PFMAX,PFMIN
C
C Plotting flags
C
      LOGICAL WAUTOLIM
      LOGICAL IAUTOLIM
      LOGICAL PAUTOLIM
      LOGICAL QAUTOLIM
      LOGICAL UAUTOLIM
      LOGICAL PFAUTOLIM
      LOGICAL BOX
      LOGICAL PFLUX
      LOGICAL LROT
      LOGICAL CROT
      LOGICAL TAUTOLIM
      LOGICAL T_HI
      LOGICAL T_LOW
      LOGICAL POLY
      LOGICAL HIST
      LOGICAL MARK
      LOGICAL QU_TRIPLOT
C
C Temporary polarization spectrum arrays
C
      REAL NEW_I(MAXPTS)
      REAL NEW_I_TMP(MAXPTS)
      REAL NEW_Q(MAXPTS)
      REAL NEW_QV(MAXPTS)
      REAL NEW_U(MAXPTS)
      REAL NEW_UV(MAXPTS)
      REAL NEW_LAMBDA(MAXPTS)
      REAL PFLUXTEMP(MAXPTS)
C
C
C Misc.
C
      CHARACTER*(*) XLAB,ILAB,PLAB,PFLAB,TLAB
      REAL POLTEMP(MAXPTS)
      REAL BIN_ERR
      REAL PA(MAXPTS)
      REAL SIGPA(MAXPTS),SIGPOLTEMP(MAXPTS),SIGPFLUX(MAXPTS)
C
C
C
C
C
C
C
      REAL TLB,THB
      INTEGER WS_NEW,WE_NEW
      INTEGER WS_OLD,WE_OLD
      CHARACTER*80 TITLE
C
C
C
C Bin the spectrum (not I) into the temp. arrays
C
      CALL TRI_CONST_BIN(
     &   BIN_ERR,WAVE,
     &   IT,Q,QV,
     &   U,UV,NPTS,NEW_I,NEW_I_TMP,NEW_Q,
     &   NEW_QV,NEW_U,NEW_UV,NEW_LAMBDA,NEW_NPTS)
C
C Set the wavelength range (if necessary)
C
      IF (WAUTOLIM.AND.BOX) THEN
        WMIN = WAVE(1)
        WMAX = WAVE(NPTS)
      ENDIF
      CALL LOCATE(WAVE,NPTS,WMIN,WS_OLD)
      CALL LOCATE(WAVE,NPTS,WMAX,WE_OLD)
      CALL LOCATE(NEW_LAMBDA,NEW_NPTS,WMIN,WS_NEW)
      CALL LOCATE(NEW_LAMBDA,NEW_NPTS,WMAX,WE_NEW)
      IF (WS_NEW.EQ.0) WS_NEW = 1
      IF (WS_OLD.EQ.0) WS_OLD = 1
C
      DO J = 1,NEW_NPTS
       CALL GET_ERRORS(NEW_I(J),NEW_Q(J),SQRT(NEW_QV(J)),NEW_U(J),
     &                 SQRT(NEW_UV(J)),POLTEMP(J),SIGPOLTEMP(J),PA(J),
     &                 SIGPA(J))
       PFLUXTEMP(J) = POLTEMP(J)*NEW_I_TMP(J)/100.
       SIGPFLUX(J) =  SIGPOLTEMP(J)*NEW_I_TMP(J)/100.
      ENDDO
      IF (QU_TRIPLOT) THEN
         DO J=1,NEW_NPTS
          IF (NEW_I(J).NE.0.) THEN
           PA(J)=100.*NEW_Q(J)/NEW_I(J)
           SIGPA(J)=100.*SQRT(NEW_QV(J))/NEW_I(J)
           POLTEMP(J)=100.*NEW_U(J)/NEW_I(J)
           SIGPOLTEMP(J)=100.*SQRT(NEW_UV(J))/NEW_I(J)
          ELSE
           PA(J)=0.
           SIGPA(J)=0.
           POLTEMP(J)=0.
           SIGPOLTEMP(J)=0.
          ENDIF
         ENDDO
      ENDIF

C
C Set the PA range
C
      IF (QU_TRIPLOT.AND.BOX.AND.QAUTOLIM) THEN
          CALL GET_LIMITS(PA,1,NEW_NPTS,QMAX,QMIN)
      ENDIF
C
      IF (TAUTOLIM.AND.BOX) THEN
       TMIN = 0.
       TMAX = 180.
      ENDIF
C
C Set up the high or low PA plotting boundaries
C
      THB=180.-ABS(TMIN)
      TLB=TMAX-180.

C
C Set up the PA window (doesn't actually plot anything).
C
      IF (PFLUX) THEN
       CALL PGVPORT(0.1,0.9,0.7,0.9)
       ELSE
       CALL PGVPORT(0.1,0.9,0.65,0.9)
      ENDIF
      IF (QU_TRIPLOT) THEN
      CALL PGWINDOW(WMIN,WMAX,QMIN,QMAX)
      ELSE
      CALL PGWINDOW(WMIN,WMAX,TMIN,TMAX)
      ENDIF
C
C Set up the line style for this plot
C
      IF (BOX.AND.LROT) THEN
       LSTYLE = 1
      ENDIF
      IF (BOX.AND.CROT) THEN
       LCOL = 1
      ENDIF
      IF ((.NOT.BOX).AND.LROT) THEN
       CALL PGQLS(LSTYLE)
       LSTYLE = MAX(1,MOD(LSTYLE+1,6))
      ENDIF
      IF ((.NOT.BOX).AND.CROT) THEN
       CALL PGQCI(LCOL)
       LCOL = MAX(1,MOD(LCOL+1,6))
      ENDIF

C
C Draw the box (if required)
C
      IF (BOX) THEN
      CALL PGPAGE
      CALL PGSLS(1)
      CALL PGSCI(1)
      CALL PGBOX('BCST',0.0,0.0,'NBCST',0.0,0.0)
      IF (QU_TRIPLOT) THEN
      CALL PGMTEXT('L',2.5,0.5,0.5,'Stokes Q (%)')
       ELSE
      CALL PGMTEXT('L',2.5,0.5,0.5,TLAB)
      ENDIF
      CALL PGMTEXT('T',1.5,0.5,0.5,TITLE)
      ENDIF
      IF (.NOT.QU_TRIPLOT) THEN
       DO J = 1,NEW_NPTS
        IF (T_LOW) THEN
         IF (PA(J).GT.THB) PA(J)=PA(J)-180.
        ENDIF
        IF (T_HI) THEN
         IF (PA(J).LT.TLB) PA(J)=PA(J)+180.
        ENDIF
       ENDDO
      ENDIF
C
C Plot the PA
C
       CALL PGSLS(LSTYLE)
       CALL PGSCI(LCOL)
       IF (POLY) THEN
        CALL PGLINE(NEW_NPTS,NEW_LAMBDA,PA)
       ELSE IF (HIST) THEN
        CALL PGBIN(NEW_NPTS,NEW_LAMBDA,PA,.TRUE.)
       ELSE IF (MARK) THEN
         CALL PGPOINT(NEW_NPTS,NEW_LAMBDA,PA,2)
         DO J=1,NEW_NPTS
          CALL PGERRY(1,NEW_LAMBDA(J),PA(J)+SIGPA(J),PA(J)-SIGPA(J),0.5)
         ENDDO
       ENDIF
C
C Now it's the percentage polarization
C
C
C Now the polarized flux box if required
C

      IF (PFAUTOLIM.AND.BOX.AND.PFLUX) THEN
       CALL GET_LIMITS(PFLUXTEMP,WS_NEW,WE_NEW,PFMAX,PFMIN)
      ENDIF
      IF (PFLUX) THEN
        CALL PGSLS(1)
        CALL PGSCI(1)
        CALL PGVPORT(0.1,0.9,0.5,0.7)
        CALL PGWINDOW(WMIN,WMAX,PFMIN,PFMAX)
        IF (BOX) THEN
        CALL PGBOX('BCST',0.0,0.0,'NBCST',0.0,0.0)
        CALL PGMTEXT('L',2.5,0.5,0.5,PFLAB)
        ENDIF
        CALL PGSLS(LSTYLE)
        CALL PGSCI(LCOL)
        IF (POLY) THEN
          CALL PGLINE(NEW_NPTS,NEW_LAMBDA,PFLUXTEMP)
         ELSE IF (HIST) THEN
          CALL PGBIN(NEW_NPTS,NEW_LAMBDA,PFLUXTEMP,.TRUE.)
         ELSE IF (MARK) THEN
          CALL PGPOINT(NEW_NPTS,NEW_LAMBDA,PFLUXTEMP,2)
          DO J=1,NEW_NPTS
           CALL PGERRY(1,NEW_LAMBDA(J),PFLUXTEMP(J)+SIGPFLUX(J),
     &                 PFLUXTEMP(J)-SIGPFLUX(J),0.5)
          ENDDO
        ENDIF

       ENDIF
C
C Percentage polarization
C
      IF (QU_TRIPLOT) THEN
       IF (UAUTOLIM.AND.BOX) THEN
        CALL GET_LIMITS(POLTEMP,WS_NEW,WE_NEW,UMAX,UMIN)
       ENDIF
       ELSE
       IF (PAUTOLIM.AND.BOX) THEN
        CALL GET_LIMITS(POLTEMP,WS_NEW,WE_NEW,PMAX,PMIN)
       ENDIF
      ENDIF
      IF (PFLUX) THEN
      CALL PGVPORT(0.1,0.9,0.3,0.5)
      ELSE
      CALL PGVPORT(0.1,0.9,0.4,0.65)
      ENDIF
       IF (QU_TRIPLOT) THEN
        CALL PGWINDOW(WMIN,WMAX,UMIN,UMAX)
        ELSE
        CALL PGWINDOW(WMIN,WMAX,PMIN,PMAX)
      ENDIF
      IF (BOX) THEN
       CALL PGSLS(1)
       CALL PGSCI(1)
       CALL PGBOX('BCST',0.0,0.0,'NBCST',0.0,0.0)
       IF (QU_TRIPLOT) THEN
        CALL PGMTEXT('L',2.5,0.5,0.5,'Stokes U (%)')
        ELSE
        CALL PGMTEXT('L',2.5,0.5,0.5,PLAB)
       ENDIF
      ENDIF
      CALL PGSLS(LSTYLE)
      CALL PGSCI(LCOL)
      IF (POLY) THEN
      CALL PGLINE(NEW_NPTS,NEW_LAMBDA,POLTEMP)
      ELSE IF (HIST) THEN
      CALL PGBIN(NEW_NPTS,NEW_LAMBDA,POLTEMP,.TRUE.)
      ELSE  IF (MARK) THEN
        CALL PGPOINT(NEW_NPTS,NEW_LAMBDA,POLTEMP,2)
        DO J=1,NEW_NPTS
         CALL PGERRY(1,NEW_LAMBDA(J),POLTEMP(J)+SIGPOLTEMP(J),
     &                               POLTEMP(J)-SIGPOLTEMP(J),0.5)
        ENDDO
      ENDIF
C
C And finally the Stokes I spectrum
C
      IF (IAUTOLIM.AND.BOX) THEN
       CALL GET_LIMITS(IT,WS_OLD,WE_OLD,IMAX,IMIN)
      ENDIF
      IF (PFLUX) THEN
       CALL PGVPORT(0.1,0.9,0.1,0.3)
       ELSE
       CALL PGVPORT(0.1,0.9,0.1,0.4)
      ENDIF
      CALL PGWINDOW(WMIN,WMAX,IMIN,IMAX)
      IF (BOX) THEN
        CALL PGSLS(1)
        CALL PGSCI(1)
        CALL PGBOX('NBCST',0.0,0.0,'NBCST',0.0,0.0)
        CALL PGMTEXT('B',2.5,0.5,0.5,XLAB)
        CALL PGMTEXT('L',2.5,0.5,0.5,ILAB)
      ENDIF
      CALL PGSLS(LSTYLE)
      CALL PGSCI(LCOL)
      IF (MARK) THEN
       CALL PGPOINT(NPTS,WAVE,IT,2)
         ELSE IF (HIST) THEN
       CALL PGBIN(NPTS,WAVE,IT,.TRUE.)
         ELSE
       CALL PGLINE(NPTS,WAVE,IT)
      ENDIF
C
666   CONTINUE
      END
