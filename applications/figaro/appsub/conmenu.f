C
      SUBROUTINE CONMENU(NX,XVALS,WID,ZVALS,ERRORS,ERRUSE,ICST,CX,
     :   ORD,ICONO,CONVALS,ZRESID,ITN,SIG,ERR,XVST,XVEN,HIGH,LOW,
     :   HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,HARD,DEVICEH,
     :   LCON,CRES,GRES,GTOT,GALL,FAULT)
C
C     C O N M E N U
C
C     This is the menu command interpreter for continuum fitting.
C     The command is read from the user input and the relevant parameter
C     set or command executed. On return the start and length of the fitted
C     continuum is set, the x and y values of the continuum points, and
C     the array of indicators for continuum points ( excluding lines ).
C
C     Parameters - (">" input, "<" output )
C
C     (>)  NX      (Integer) Number of elements in XVALS and ZVALS
C     (>)  XVALS   (Real array) The abscissae of the observed points
C     (>)  WID     (Real) Width of X channels
C     (>)  ZVALS   (Real array) The Y values of the observed points
C     (>)  ERRORS  (Real array) The errors on the Y values
C     (>)  ERRUSE  (Logical) >TRUE. if errors on Y values are present
C     (<)  ICST    (Integer) The start of the continuum fitted region
C     (<)  CX      (Integer) The number of points in the continuum fited region
C     (=)  ORD     (Integer) Order of final polynomial fit
C     (<)  ICONO   (Integer array) The array indicating points to be included
C                  in the continuum fit
C     (<)  CONVALS (Real array) The Y values of the CX continuum fitted points
C     (<)  ZRESID  (Real array) The Y values of the residuals on the continuum
C                  fit
C     (=)  ITN     (Integer) Number of iterations for continuum point rejection
C     (=)  SIG     (Real) Factor*sigma for continuum point rejection
C     (=)  ERR     (Real) Factor*error for continuum point rejection
C     (=)  XVST    (Real) The actual x-start value for the plot.
C     (=)  XVEN    (Real) The actual x-end value for the plot.
C     (=)  HIGH    (Real) The maximum value for the plot.
C     (=)  LOW     (Real) The minimum value for the plot.
C     (=)  HIGHR   (Real) The maximum value for the residuals plot.
C     (=)  LOWR    (Real) The minimum value for the residuals plot.
C     (<)  FAULT   (Logical) .TRUE. if an error occurs in the fitting
C
C     Subroutines called:
C     CONTFIT    Fits a polynomial to the continuum points using NAG
C                routines E02ADF and E02AEF
C     NORMI      Normalises the rms or fractional error arrays so
C                that values lie in range 0. to 10.
C
C                                                JRW / AAO  March 1987
C
C     Modified:
C       Initial values of ORD, SIG, ERR and ITN now set in GAUSS so
C       any modification is reflected in the default values written in the
C       RDVAL prompt.
C       Highest order continuum fit plotted after the call to FIT. If an
C       old plot exists it and its residuals are erased.
C
C      1993 Jul 27 HME / UoE, Stalrink.  Disuse PAR_RDUSER, use
C                  PAR_ABORT. Added parameters CCMD and GCMD.
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,ICST,CX,ORD,ITN,ICONO(NX)
      REAL XVALS(NX),WID,ZVALS(NX),ERRORS(NX),CONVALS(NX),
     :ZRESID(NX),SIG,ERR,XVST,XVEN,HIGH,LOW,HIGHR,LOWR
      CHARACTER*(*) DEVICES,DEVICEH,XLAB,PLAB,ZLAB
      LOGICAL ERRUSE,HARD,ERASE,LCON,CRES,GRES,GTOT,GALL,FAULT
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_ENCODE,ICH_FOLD,ICH_KEY
C
C     Local variables
C
      INTEGER I,J,K,STATUS,KEY,NEXT,ISIG,IERR,IITN,IORD,CUR,
     :FIT,GAU,HELP,QUEST,INVOKE,IGNORE,IL,IR,IA,IB,NP,TCONO(4096),
     :KORD,NREJ,NPT,STAT
      REAL XC,YC,VALUE,RMS(8),RMSN(8),NFAC,RAT,FERR(8),DUM(1)
      DOUBLE PRECISION A(8,8)
      CHARACTER*1 CC
      CHARACTER*80  RESPON,STRING
      LOGICAL LERR,LICON,CONDON
C
C     Set values for menu key
C
      PARAMETER ( CUR=1,IORD=2,ISIG=3,IERR=4,IITN=5,FIT=6,GAU=7,
     :HELP=8,QUEST=9 )
C
C     Set logical variables to control plotting limitations
C
      LERR=.FALSE.  ! Errors not used for point rejection
      LICON=.FALSE. ! Continuum points not indicated
      KORD=ORD+1
C
C     Issue menu prompt
C
10    CALL PAR_WRUSER('  ',STATUS)
      CALL PAR_WRUSER('GAUSS continuum fitting menu.'//
     : ' use ''HELP'' or ''?'' for assistance',STATUS)
C
C     Continuum fitting menu entry. Alternatives are:
C
C        CUR    -    indicate continuum regions by cursor
C        ORD    -    order of polynomial fit
C        SIG    -    factor*sigma on fit for continuum point rejection
C        ERR    -    factor*error on point for rejection from continuum fit
C        ITN    -    number of iterations for continuum point rejection
C        FIT    -    perform the polynomial fitting
C        GAU    -    proceed to Gaussian fitting ( continuum set )
C
20    CALL PAR_WRUSER('  ',STATUS)
      CALL PAR_CNPAR('CCMD')
      CALL PAR_RDCHAR('CCMD',' ',RESPON)
      IF (PAR_ABORT()) GO TO 999
      INVOKE=ICH_FOLD(RESPON)
C
C     Test reply to determine if a valid prompt
C
      KEY=ICH_KEY(RESPON,1,', ','CUR:ORD:SIG:ERR:ITN:FIT:GAU'//
     :            ':HELP:?:','Abbr.',NEXT)
      IF (KEY.EQ.0) THEN
        IF (RESPON.NE.' ') THEN
          CALL PAR_WRUSER('Unrecognized command.  Use "?" or '//
     :                    '"HELP" for help',STATUS)
          GO TO 20
        END IF
        GO TO 20
      END IF
C
C     Act on the command
C
50    IF (KEY.EQ.CUR) THEN
C
C       Cursor used to indicate the left and right edges of sections
C       of line free continuum. Points indicated in pairs for left and right
C       edges. Values of 1 in ICONO indicate continuum points.
C
        CALL PAR_WRUSER('Hit any key to record cursor position. '//
     :                  'Q to quit',STATUS)
51      XC=XVALS(NX/2) ! Somewhere to put the cursor initially
        YC=ZVALS(NX/2)
C
C       Initialize ICONO and TCONO and CONVALS
C
        DO I=1,NX
          ICONO(I)=0
          TCONO(I)=0
          CONVALS(I)=0.0
        END DO

        DO I=1,100
52        CALL PAR_WRUSER('Use cursor to indicate left edge '//
     :           'of line-free continuum section ',STATUS)
          CALL PGCURSE(XC,YC,CC)
          IF (CC.EQ.'Q'.OR.CC.EQ.'q') GO TO 55
          IA=NINT((XC-XVALS(1))/WID) + 1
          CALL PAR_WRUSER('Use cursor to indicate right edge '//
     :           'of line-free continuum section ',STATUS)
          CALL PGCURSE(XC,YC,CC)
          IF (CC.EQ.'Q'.OR.CC.EQ.'q') GO TO 55
          IB=NINT((XC-XVALS(1))/WID) + 1
C
C         Check values of IA and IB to determine IB>IA and reasonable
C
          IF (IA.GE.IB.OR.IA.LT.1.OR.IB.GT.NX) THEN
            CALL PAR_WRUSER('Edges of continuum section invalid',STATUS)
            XC=XVALS(NX/2)
            YC=ZVALS(NX/2)
            GO TO 52
          END IF
C
C         Fill array ICONO with 1's for points between IA and IB
C
          DO J=IA,IB
            ICONO(J)=1
          END DO
        END DO
C
C     Check that there is at least 1 continuum point indicated in ICONO
C
55      J=0
        DO I=1,NX
          IF (ICONO(I).EQ.1) THEN
            J=J+1
          END IF
        END DO
        NP=J
        IF (NP.LT.2) THEN
          CALL PAR_WRUSER('Less than 2 continuum points '//
     :                    'selected',STATUS)
          GO TO 51
        END IF
C
C       Set array of X values of continuum points
C
        DO I=1,NX,1
          IF ((ICONO(I).EQ.0).AND.(ICONO(I+1).EQ.1)) THEN
            IL=I+1
            GO TO 57
          END IF
        END DO
57      DO I=NX,1,-1
          IF ((ICONO(I).EQ.0).AND.(ICONO(I-1).EQ.1)) THEN
            IR=I-1
            GO TO 58
          END IF
        END DO
58      ICST=IL
        CX=IR-IL+1
C
C       Don't proceed if continuum points out of range or too few
C
        IF (ICST.LE.0.OR.CX.LE.3) THEN
          CALL PAR_WRUSER('Range of continuum points invalid',STATUS)
          GO TO 20
        END IF
        LICON=.TRUE.
        GO TO 20
      END IF

60    IF (KEY.EQ.IORD) THEN
C
C       ORDER of polynomial fit
C
        CALL PAR_CNPAR('ORD')
        CALL PAR_RDVAL('ORD',0.,7.,FLOAT(ORD),' ',VALUE)
        IF (PAR_ABORT()) GO TO 999
        ORD=INT(VALUE)
        KORD=ORD+1
        IF (ORD.LT.0.OR.ORD.GT.7) THEN
          CALL PAR_WRUSER('Order of polynomial must be between '//
     :                    '0 and 7',STATUS)
          GO TO 60
        END IF
        GO TO 20
      END IF

70    IF (KEY.EQ.ISIG) THEN
C
C       SIGMA * r.m.s. for rejection of points
C
        CALL PAR_CNPAR('SIG')
        CALL PAR_RDVAL('SIG',0.,1000.,SIG,' ',VALUE)
        IF (PAR_ABORT()) GO TO 999
        SIG=VALUE
        ERR=0.0
        GO TO 20
      END IF

80    IF (KEY.EQ.IERR) THEN
        IF (ERRUSE) THEN
C
C       Factor *ERROR on point for rejection from continuum fit
C
          CALL PAR_CNPAR('ERR')
          CALL PAR_RDVAL('ERR',0.,1000.,ERR,' ',VALUE)
          IF (PAR_ABORT()) GO TO 999
          ERR=VALUE
          LERR=.TRUE.
          SIG=0.0
          GO TO 20
        ELSE
          CALL PAR_WRUSER('No errors available',STATUS)
          LERR=.FALSE.
          GO TO 20
        END IF
      END IF

90    IF (KEY.EQ.IITN) THEN
C
C       Number of iterations for point rejection
C
        CALL PAR_CNPAR('ITN')
        CALL PAR_RDVAL('ITN',0.,10.,FLOAT(ITN),' ',VALUE)
        IF (PAR_ABORT()) GO TO 999
        ITN=INT(VALUE)
        IF (ITN.LT.0.OR.ITN.GT.10) THEN
          CALL PAR_WRUSER('Number of iterations for continuum '//
     :    'point rejection must be between 0 and 10',STATUS)
          GO TO 90
        END IF
        GO TO 20
      END IF

100   IF (KEY.EQ.FIT) THEN
C
C       Fit the continuum with a polynomial of order ORD. If no errors
C       then with ITN iterations for rejection of points greater than
C       SIG*r.m.s. from fitted continuum. If errors then ITN iterations
C       with rejection of points greater than ERR*actual error from fitted
C       continuum
C
C       Copy values in ICONO to an array TCONO in which rejected
C       discrepant continuum points are indicated
C
        DO I=ICST,ICST+CX-1,1
          TCONO(I)=ICONO(I)
        END DO
C
C       Ready for polynomial fitting. Check that continuum points
C       indicated in ICONO
C
        IF (.NOT.LICON) THEN
          CALL PAR_WRUSER('Need to indicate continuum sections '//
     :                    'with cursor',STATUS)
          GO TO 20
        END IF
        IF (LICON) THEN
          IF (.NOT.LERR) THEN
C
C           Initialize array RMS in case refitting and order changed
C
            DO I=1,8
              RMS(I)=0.0
              RMSN(I)=0.0
            END DO
            CALL PAR_WRUSER('Rms residuals on polynomial fit to '//
     :                   'line-free continuum points',STATUS)
            STRING=' with rejection of points '
            INVOKE=ICH_ENCODE(STRING,SIG,27,7,NEXT)
            STRING(NEXT:)=' * sigma on continuum fit'
            CALL PAR_WRUSER(STRING(:NEXT+25),STATUS)
            CALL PAR_WRUSER(' Iter                      Rms (Order)'//
     :             '                          No.Pnt  No.Rej',STATUS)
            CALL PAR_WRUSER('          0      1      2      3     '//
     :         ' 4      5      6      7',STATUS)
C
C           First fit with no point rejection
C
            CALL CONTFIT(NX,XVALS,ZVALS,CX,ICST,TCONO,KORD,A,CONVALS,
     : RMS,FAULT)
            IF (FAULT) GO TO 999
C
C           Write results to terminal
C
            NREJ=0
            I=0
C
C           Normalise so that results in range 0. to 10.
C
            CALL NORMI(KORD,RMS,NFAC,RMSN)
            WRITE (STRING,106,IOSTAT=IGNORE) I,RMSN,NP,NREJ
106         FORMAT(I4,2X,8F7.3,1X,I5,2X,I5)
            CALL PAR_WRUSER(STRING(:80),STATUS)
            IF (ITN.EQ.0) THEN
              WRITE(STRING,107,IOSTAT=IGNORE) NFAC
107           FORMAT('  Normalising factor = ',E12.1)
              CALL PAR_WRUSER(STRING(:35),STATUS)
              CONDON=.TRUE.
              GO TO 20
            ENDIF
C
C           Step from 1 to ITN writing results each iteration.
C
            DO I=1,ITN
              NREJ=0
              DO J=ICST,ICST+CX-1,1
                IF (ICONO(J).EQ.1) THEN
                  IF (ABS(ZVALS(J)-CONVALS(J)).GT.
     :                                  (SIG*RMS(KORD))) THEN
                    TCONO(J)=0
                    NREJ=NREJ+1
                  END IF
                END IF
              END DO
              CALL CONTFIT(NX,XVALS,ZVALS,CX,ICST,TCONO,KORD,A,CONVALS,
     : RMS,FAULT)
              IF (FAULT) GO TO 999
              NPT=NP-NREJ ! No. of remaining points
              DO J=1,KORD,1
                RMSN(J)=RMS(J)/NFAC
              END DO
              WRITE (STRING,106,IOSTAT=IGNORE) I,RMSN,NPT,NREJ
              CALL PAR_WRUSER(STRING(:80),STATUS)
            END DO
            WRITE(STRING,107,IOSTAT=IGNORE) NFAC
            CALL PAR_WRUSER(STRING(:35),STATUS)
          END IF

          IF (LERR) THEN
C
C           Initialise FERR and RMSN
C
            DO I=1,8
              FERR(I)=0.0
              RMSN(I)=0.0
            END DO
            CALL PAR_WRUSER('Fractional error on polynomial fit to '//
     :                   'line-free continuum points',STATUS)
            STRING=' with rejection of points '
            INVOKE=ICH_ENCODE(STRING,ERR,27,7,NEXT)
            STRING(NEXT:)=' * error on observed values'
            CALL PAR_WRUSER(STRING(:NEXT+27),STATUS)
            CALL PAR_WRUSER(' Iter                Fractional Error '//
     :             '(Order)                   No.Pnt  No.Rej',STATUS)
            CALL PAR_WRUSER('          0      1      2      3     '//
     :         ' 4      5      6      7',STATUS)
C
C           Calculate the fractional error on fit in terms of the
C           error on values for each order of polynomial with no
C           point rejection initially
C
            DO K=1,KORD,1
              CALL CONTFIT(NX,XVALS,ZVALS,CX,ICST,TCONO,KORD,A,CONVALS,
     :                       RMS,FAULT)
              IF (FAULT) GO TO 999
              RAT=0.0
              DO J=ICST,ICST+CX-1,1
                IF (TCONO(J).EQ.1) THEN
                  RAT=RAT + ABS((ZVALS(J)-CONVALS(J))
     :                                          /ERRORS(J))
                END IF
              END DO
              FERR(K)=RAT/REAL(NP)
            END DO
C
C           Normalise so that results in range 0. to 10.
C
            I=0
            NREJ=0
            CALL NORMI(KORD,FERR,NFAC,RMSN)
            WRITE (STRING,106,IOSTAT=IGNORE) I,RMSN,NP,NREJ
            CALL PAR_WRUSER(STRING(:80),STATUS)
            IF (ITN.EQ.0) THEN
              WRITE(STRING,107,IOSTAT=IGNORE) NFAC
              CALL PAR_WRUSER(STRING(:35),STATUS)
              CONDON=.TRUE.
              GO TO 20
            END IF
C
C           Step from 1 to ITN writing results each iteration
C
            DO I=1,ITN
C
C             Calculate the fractional error on fit in terms of the
C             error on values for each order of polynomial
C
              DO K=1,KORD,1
                NREJ=0
                DO J=ICST,ICST+CX-1
                  IF (ICONO(J).EQ.1) THEN
                    IF (ABS(ZVALS(J)-CONVALS(J)).GT.
     :                                (ERR*ERRORS(J))) THEN
                      TCONO(J)=0
                      NREJ=NREJ+1
                    END IF
                  END IF
                END DO
                CALL CONTFIT(NX,XVALS,ZVALS,CX,ICST,TCONO,KORD,A,
     :                       CONVALS,RMS,FAULT)
                IF (FAULT) GO TO 999
                NPT=NP-NREJ ! No. of remaining points
                RAT=0.0
                DO J=ICST,ICST+CX-1,1
                  IF (TCONO(J).EQ.1) THEN
                    RAT=RAT + ABS((ZVALS(J)-CONVALS(J))
     :                                               /ERRORS(J))
                  END IF
                END DO
                RMSN(K)=(RAT/REAL(NPT))/NFAC
              END DO
              WRITE (STRING,106,IOSTAT=IGNORE) I,RMSN,NPT,NREJ
              CALL PAR_WRUSER(STRING(:80),STATUS)
            END DO
            WRITE(STRING,107,IOSTAT=IGNORE) NFAC
            CALL PAR_WRUSER(STRING(:35),STATUS)
          END IF
        END IF
C
C       Plot the highest order, max point rejection continuum fit
C       Calculate the Z residuals on the continuum fit and find the maximum
C       and minimum
C
        HIGHR=-1.E36
        LOWR=1.E36
        DO I=ICST,ICST+CX-1,1
          IF (ICONO(I).EQ.1) THEN
            ZRESID(I)=ZVALS(I)-CONVALS(I)
            IF (ZRESID(I).GT.HIGHR) THEN
              HIGHR=ZRESID(I)
            END IF
            IF (ZRESID(I).LT.LOWR) THEN
              LOWR=ZRESID(I)
            END IF
          ELSE
            ZRESID(I)=0.0
          END IF
        END DO
        HIGHR=HIGHR + ABS(HIGHR-LOWR)/12.
        LOWR=LOWR - ABS(HIGHR-LOWR)/12.
C
C       Plot the resulting continuum with residuals
C
        ERASE=.TRUE.
        LCON=.TRUE.
        CRES=.TRUE.
        CALL GAUS_XZPLOT(XVALS,DUM,ZVALS,ERRORS,
     :    ZRESID,CONVALS,ICONO,DUM,DUM,DUM,NX,CX,1,1,
     :    ICST,0,HIGH,LOW,HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,
     :    HARD,DEVICEH,ERRUSE,LCON,CRES,GRES,GTOT,GALL,XVST,XVEN,
     :    STATUS)
        IF (STATUS.NE.0) THEN
          CALL PAR_WRUSER('Error in plotting',STAT)
        END IF
        CONDON=.TRUE.
        GO TO 20
      END IF

110   IF (KEY.EQ.GAU) THEN
C
C       Continuum fitting complete; ready to proceed to Gaussian fitting.
C       Check that continuum set.
C
        IF (CONDON) THEN
C
C         If errors available but not used for point rejection then
C         set ERR to zero
C
          IF (ERRUSE.AND..NOT.LERR) THEN
            ERR=0.0
          ENDIF
          GO TO 999
        ELSE
          CALL PAR_WRUSER('Need to fit continuum',STATUS)
          GO TO 20
        END IF
      END IF

120   IF (KEY.EQ.HELP.OR.KEY.EQ.QUEST) THEN
C
C       Get the Figaro help for GAUSS
C
        CALL FIG_HELP('gauss',STATUS)
        IF (STATUS.NE.0) THEN
          CALL PAR_WRUSER('Unable to access help text file',STATUS)
        END IF
        GO TO 20
      END IF

999   END
