      SUBROUTINE TRIPLOT_IFACE(TOP_STK,STK_TITLE,
     &STK_NPTS,STK_LAMBDA,
     &STK_STOKES_I,STK_STOKES_Q,STK_STOKES_QV,
     &STK_STOKES_U,STK_STOKES_UV,
     &TITLE,NPTS,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,
     &STOKES_U,STOKES_UV,
     &NPARAMS,PARAMS,TMAX,TMIN,
     &PMAX,PMIN,IMAX,IMIN,WMIN,WMAX,BOX,WAUTOLIM,
     &IAUTOLIM,PAUTOLIM,TAUTOLIM,LROT,CROT,T_HI,T_LOW,
     &POLY,XLAB,ILAB,PLAB,PFLAB,TLAB,PFLUX,PFMAX,PFMIN,PFAUTOLIM,LCOL,
     &LSTYLE,
     &HIST,MARK,OUT_LU,
     &QU_TRIPLOT,QAUTOLIM,QMAX,QMIN,UAUTOLIM,UMAX,UMIN)
C+
C
C Subroutine: 
C
C  T R I P L O T _ I F A C E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C     TOP_STK (<), STK_TITLE (<),
C     STK_NPTS (<), STK_LAMBDA (<),
C     STK_STOKES_I (<), STK_STOKES_Q (<), STK_STOKES_QV (<),
C     STK_STOKES_U (<), STK_STOKES_UV (<),
C     TITLE (<), NPTS (<), LAMBDA (<), STOKES_I (<), STOKES_Q (<),
C     STOKES_QV (<), STOKES_U (<), STOKES_UV (<),
C     NPARAMS (<), PARAMS (<), TMAX (<), TMIN (<),
C     PMAX (<), PMIN (<), IMAX (<), IMIN (<), WMIN (<), WMAX (<),
C     BOX (<), WAUTOLIM (<)
C     IAUTOLIM (<), PAUTOLIM (<), TAUTOLIM (<), LROT (<), CROT (<),
C     T_HI (<), T_LOW (<),
C     POLY (<), XLAB (<), ILAB (<), PLAB (<), PFLAB (<), TLAB (<),
C     PFLUX (<), PFMAX (<), PFMIN (<), PFAUTOLIM (<),LCOL (<),
C     LSTYLE (<), HIST (<), MARK (<),OUT_LU (<),
C     QU_TRIPLOT (<), QAUTOLIM (<), QMAX (<), QMIN (<), UAUTOLIM (<)
C     UMAX (<), UMIN (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C
C The this is the triplot interface routine. A separate procedure is 
C required since there are several ways of calling the rouine.
C
C
C
C
C-

      IMPLICIT NONE
      INTEGER OUT_LU
C
C The array sizes.
C
      INCLUDE 'array_size.inc'
C
C The stack arrays
C
      INTEGER STK_NPTS(MAXSPEC)
      REAL STK_LAMBDA(MAXPTS,MAXSPEC)
      REAL STK_STOKES_I(MAXPTS,MAXSPEC)
      REAL STK_STOKES_Q(MAXPTS,MAXSPEC)
      REAL STK_STOKES_QV(MAXPTS,MAXSPEC)
      REAL STK_STOKES_U(MAXPTS,MAXSPEC)
      REAL STK_STOKES_UV(MAXPTS,MAXSPEC)
      CHARACTER*80 STK_TITLE(MAXSPEC) 
      INTEGER TOP_STK
C
C The current arrays
C
      INTEGER NPTS
      REAL LAMBDA(*)
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      CHARACTER*(*) TITLE
C
C Temp arrays
C
      INTEGER TMP_NPTS
      REAL TMP_LAMBDA(MAXPTS)
      REAL TMP_STOKES_I(MAXPTS)
      REAL TMP_STOKES_Q(MAXPTS)
      REAL TMP_STOKES_QV(MAXPTS)
      REAL TMP_STOKES_U(MAXPTS)
      REAL TMP_STOKES_UV(MAXPTS)
      CHARACTER*(80) TMP_TITLE
C
C The command parameters
C
      INTEGER NPARAMS
      REAL PARAMS(*)
C
C Plotting flags
C 
      LOGICAL BOX
      LOGICAL LROT
      LOGICAL CROT
      LOGICAL OK
      LOGICAL POLY,HIST,MARK
      LOGICAL TAUTOLIM
      LOGICAL PAUTOLIM
      LOGICAL PFAUTOLIM
C
      LOGICAL WAUTOLIM
      LOGICAL IAUTOLIM
      LOGICAL QAUTOLIM,UAUTOLIM
      LOGICAL FIRST
      LOGICAL TBOX
      LOGICAL PFLUX
      LOGICAL QU_TRIPLOT
      LOGICAL T_HI,T_LOW
C
C Plotting ranges
C
      REAL TMAX,TMIN 
      REAL PMAX,PMIN
      REAL IMAX,IMIN
      REAL WMAX,WMIN
      REAL QMAX,QMIN
      REAL UMAX,UMIN
      REAL PFMAX,PFMIN
C
C Misc.
C
      INTEGER LCOL,LSTYLE
      CHARACTER*(*) XLAB,ILAB,PLAB,PFLAB,TLAB
      REAL BINERR
      INTEGER I,J,SPEC
C
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Bin error',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS = 1
      ENDIF
C

      BINERR = PARAMS(1)
C
C If there is only one parameter (ie the bin error) then we plot the
C polarization spectrum from the current arrays.
C
      IF (NPARAMS.EQ.1) THEN
         IF (NPTS.EQ.0) THEN
          CALL WR_ERROR('No data in current arrays',OUT_LU)
         ELSE
          CALL TRIPLOT(BINERR,STOKES_I,STOKES_Q,
     &     STOKES_U,STOKES_QV,
     &     STOKES_UV,LAMBDA,NPTS,TMAX,TMIN,
     &     PMAX,PMIN,IMAX,IMIN,WMIN,WMAX,TITLE,BOX,WAUTOLIM,
     &     IAUTOLIM,PAUTOLIM,TAUTOLIM,LROT,CROT,T_HI,T_LOW,POLY,
     &     XLAB,ILAB,PLAB,PFLAB,TLAB,PFLUX,PFMAX,PFMIN,PFAUTOLIM,
     &     LCOL,LSTYLE,
     &     HIST,MARK,
     &     QU_TRIPLOT,QAUTOLIM,QMAX,QMIN,UAUTOLIM,UMAX,UMIN)
         ENDIF
      ENDIF
C
C Now go through the rest of the parameters. The first spectrum is 
C different (if box is set) since this sets up any autolimited
C plotting ranges
C
      IF (NPARAMS.GT.1) THEN
       FIRST = .TRUE.
       TBOX = BOX
       DO I = 2,NPARAMS
        SPEC = INT(PARAMS(I))
        IF ( (SPEC.LT.0).OR.(SPEC.GT.TOP_STK) ) THEN
           CALL WR_ERROR('Stack entry out of range',OUT_LU)
          ELSE
           TMP_TITLE = STK_TITLE(SPEC)
           TMP_NPTS = STK_NPTS(SPEC)
           DO J = 1,TMP_NPTS
            TMP_LAMBDA(J) = STK_LAMBDA(J,SPEC)
            TMP_STOKES_I(J) = STK_STOKES_I(J,SPEC)
            TMP_STOKES_Q(J) = STK_STOKES_Q(J,SPEC)
            TMP_STOKES_QV(J) = STK_STOKES_QV(J,SPEC)
            TMP_STOKES_U(J) = STK_STOKES_U(J,SPEC)
            TMP_STOKES_UV(J) = STK_STOKES_UV(J,SPEC)
           ENDDO
           CALL TRIPLOT(BINERR,TMP_STOKES_I,TMP_STOKES_Q,
     &       TMP_STOKES_U,TMP_STOKES_QV,
     &       TMP_STOKES_UV,TMP_LAMBDA,TMP_NPTS,TMAX,TMIN,
     &       PMAX,PMIN,IMAX,IMIN,WMIN,WMAX,TMP_TITLE,TBOX,WAUTOLIM,
     &       IAUTOLIM,PAUTOLIM,TAUTOLIM,LROT,CROT,T_HI,T_LOW,POLY,
     &       XLAB,ILAB,PLAB,PFLAB,TLAB,PFLUX,PFMAX,PFMIN,
     &       PFAUTOLIM,LCOL,LSTYLE,
     &       HIST,MARK,
     &       QU_TRIPLOT,QAUTOLIM,QMAX,QMIN,UAUTOLIM,UMAX,UMIN)
           IF (FIRST.AND.BOX) THEN
             TBOX = .FALSE.
             FIRST = .FALSE.
           ENDIF
        ENDIF
       ENDDO
      ENDIF
666   CONTINUE
      END
