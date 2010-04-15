C
      SUBROUTINE GAUMENU(XVALS,WID,ZVALS,ERRORS,ZRESID,
     :       CONVALS,ICONO,FITTOT,NX,CX,ICST,HIGH,LOW,HIGHR,
     :       LOWR,XLAB,ZLAB,PLAB,DEVICES,DEVICEH,ERRUSE,XVST,
     :       XVEN,ORD,SIG,ERR,XUNITS,ZUNITS,LREC,LU,XTOLI,LSECT,SAVE)
C
C     This is the menu command interpreter for Gaussian fitting.
C     The command is read from the user input and executed.
C     The line extent can be selected with cursor; a single Gaussian
C     fit to a line can be made; multiple Gaussian fits or manual
C     fits can be made using cursor to set position of lines and
C     terminal prompts to shift peak position, widen or heighten
C     lines; fit is optimized by call to NAG routine E04JBF; any
C     of the fit parameters ( position, height or width ) can
C     be locked or chained to other values and the residuals can
C     be weighted if desired.
C     Fits and residuals are plotted as the analysis proceeds
C     with minimum interference with the existing plot ( observed
C     spectrum, continuum fit and error bars ). Results of the fit
C     are printed to the terminal and written to a data file if so
C     selected. A previous fit can be read from a data file and
C     plotted. The final fit can be saved as a file whose name is
C     prompted for on return to subroutine GAUSS.
C     On return another continuum section in the same spectrum can
C     be selected, or a simple quit.
C
C     Parameters - (">" input, "<" output )
C
C     (>) XVALS    (Real array) The abscissae for the plotted points.
C     (>) WID      (Real) Width of X channels
C     (>) ZVALS    (Real array) The observed data to be plotted.
C     (>) ERRORS   (Real array) The Y errors on the observed data.
C     (>) ZRESID   (Real array) The observed -fitted residuals on the
C                  continuum fit ( values over the line not plotted )
C     (>) CONVALS  (Real array) The Y values of the fitted continuum
C     (>) ICONO    (Integer array) A value of 1 indicates a continuum point
C     (<) FITTOT   (Real array) The final continuum and Gaussian fit
C                  spectrum
C     (>) NX       (Integer) Number of elements in XVALS and ZVALS.
C     (>) CX       (Integer) Number of elements in fitted continuum
C     (>) ICST     (Integer) The first element of XVALS to be plotted
C                  as continuum
C     (>) HIGH     (Real) The maximum value for the plot.
C     (>) LOW      (Real) The minimum value for the plot.
C     (>) HIGHR    (Real) The maximum value for the residuals plot.
C     (>) LOWR     (Real) The minimum value for the residuals plot.
C     (>) XLAB     (Character) The X-label for the plot.
C     (>) ZLAB     (Character) The Z-label for the plot.
C     (>) PLAB     (Character) The label for the plot as a whole.
C     (>) ERASE    (Logical) True if device is to be erased first.
C     (>) DEVICES  (Character) The device/type to be used for the
C                  soft plot - see PGPLOT documentation for details.
C     (>) DEVICEH  (Character) The device/type to be used for the
C                  hard plot - see PGPLOT documentation for details.
C     (>) ERRUSE   (Logical) True if erros are available
C     (>) XVST     (Real) The actual x-start value for the plot.
C     (>) XVEN     (Real) The actual x-end value for the plot.
C     (>) ORD      (Integer) Order of polynomial continuum fit
C     (>) SIG      (Real) Factor* rms for continuum fit point rejection
C     (>) ERR      (Real) Factor* error for continuum point rejection
C     (>) XUNITS   (Character) Name of X units of data
C     (>) ZUNITS   (Character) Name of Y units of data
C     (>) LREC     (Logical) True if Gaussian fit data to be written to file
C     (>) LU       (Integer) Logical unit number for output file
C     (>) XTOLI    (Double Precision) Initial value for tolerance on function
C                  for NAG E04JBF optimization
C     (<) LSECT    (Logical) True if another section of line+continuum to
C                  be analysed
C     (<) SAVE     (Logical) True if fitting spectrum is to be saved as a
C                  file
C                                         JRW / AAO February 1987
C
C     Subroutines called:
C      FWHMIT  -  determines the FWHM of a profile from the half-height points
C      GAUF1   -  sums the signal for all the fitting Gaussians on the continuum
C      GAUF2   -  sums the signal for all the fitted Gaussians
C      GAUF3   -  determines the position of the next peak in a partially
C                  fitted profile
C      GAUF4   -  copies the signal for each fitting Gaussian into an array
C      GAUS_XZPLOT  -  handles all plotting of lines, fits residuals and
C                      bars
C      ERASER  -  plots in pen zero a line or set of points to erase them
C                 from the graphics screen
C      RMSER   -  calculates residuals on fit, rms and mean fractional error
C                 ( if errors available )
C      CONSTR  -  handles the prompting for and reporting of singly constrained
C                 and multiply constrained ( chained ) Gaussian parameters
C      AUTOFIT  -  performs the optimization of the Gaussian fit using NAG
C                  routines E04HBF and E04JBF ( calls DOIT1,DOIT2 AND FUNCT )
C      TERMWRIT  -  writes the parameters of the fitted Gaussians to the
C                   terminal
C      GAUS_RECORD  -  writes the parameters of the fitted Gaussians to the
C                 results file
C
C      Modified:
C        Can fit further new lines after optimization ( LLIM set FALSE
C        after fit complete ); data on all fits are recorded in results file.
C        Handles weighting of residuals by value or 1/error**2 ( if available );
C        no weighting also allowed ( ie. units weights ).
C        Acceptance of manual fit prompt changed to PAR_RDKEY from PAR_QUEST
C        Any peaks occuring outside range of GXVALS are deleted
C        Handles constraints on the Gaussian fit parameters - either as
C        single constraints or as chaining to other values ( eg. line heights
C        in given ratio ).
C        Can now exit from Gaussian fitting menu without having
C        fitted a line. This can be useful if only a continuum
C        fit is required.
C        In Gaussian fitting menu SIN now tests for a sensible
C        extent for the line. If no left edge is available the
C        left edge of the spectrum is used; similarly for right
C        edge. Warning messages are issued in these cases.
C        Have to specify at least three characters in the menu prompt
C        to prevent ambiguity with LIM and LIS, and NEW and NEXT
C        ( 'Exact' form used in ICH_KEY ).
C
C      1993 Jul 27 HME / UoE, Stalrink.  Disuse PAR_RDUSER, use
C                  PAR_ABORT. Added parameters CCMD and GCMD.
C                  The parameters DELE and INDEX were cancelled with a
C                  call that included a status variable. That is not
C                  supposed to be there and gets mixed up with the
C                  passed string length. This would result in parameters
C                  being sought that were not declared in the interface.
C      1994 Dec 21 HME / UoE, Starlink.  Make OPEN statement comply with
C                  Fortran standard. Prevent specifying more than 10
C                  Gaussians with NEXT. RECALL will still read as many
C                  Gaussians as found in the file, which screws up the
C                  memory neighbouring the GINF* variables.
C      1996 Mar 20 HME / UoE, Starlink.  Fixed broken strings (the lines
C                  just broke in the middle of the strings, now the
C                  strings are completed on a single line.)
C      1997 Sep 04 ACD / UoE, Starlink.  Fixed a bug in entering values
C                  for the Inch options P (position), H (height) and W
C                  (width).  If one of these options was specified
C                  without an in-line value then the Linux version of
C                  GAUMENU crashed and the other versions issued an
C                  error message.  I changed the code so that if an
C                  option is supplied without a value then the previous
C                  value is used.
C      2001 Feb 23 ACD / UoE, Starlink. Removed technically illegal
C                  jumps into an IF block.
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,CX,ICST,ICONO(NX),ORD,LU
      REAL XVALS(NX),WID,ZVALS(NX),ZRESID(NX),ERRORS(NX),
     :CONVALS(NX),FITTOT(NX),HIGH,LOW,HIGHR,LOWR,XVST,XVEN,SIG,ERR
      DOUBLE PRECISION XTOLI
      CHARACTER*(*) DEVICES,DEVICEH,XLAB,ZLAB,PLAB
      CHARACTER*16 XUNITS,ZUNITS
      LOGICAL ERRUSE,LCON,LREC,SAVE
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_ENCODE,ICH_FOLD,ICH_KEY
C
C     Local variables
C
      INTEGER I,J,STATUS,NEXT,LIM,SIN,NEW,NEX,INCH,SEL,LIS,OPT,
     :RECAL,DEL,IHARD,ISAVE,CON,QUIT,HELP,QUEST,INVOKE,IGNORE,KEY,IA,
     :IB,IGST,IP,NG,GX,LG,MG,STAT,IPOL,CURRENT,NDEL,NOPEN,RUBI,ILEN1,
     :ILEN2,CONP(10),CONH(10),CONW(10),ICHAINP(10),ICHAINH(10),
     :ICHAINW(10),FITF3,IFAIL1,IFAIL2,LUR,RECSTAT
      REAL XC,YC,LEFT,RIGHT,GXVALS(4096),GZVALS(4096),GZRESID(4096),
     :GAUSUM(4096),WHT(4096),TOTW,GAUFS(4096,10),GINFP(10),GINFH(10),
     :GINFW(10),GONFP(10),GONFH(10),GONFW(10),FWHM,PSTEP,HSTEP,WSTEP,
     :RSTEP,MLM,HNORM,RANG,RANGW,VALUE(1),MAXZ,VALX,VALZ,GPOS,GPK,
     :CHAINP(10),CHAINH(10),CHAINW(10),RUB,EW,RMS,
     :MERR,MILIM,MALIM
      DOUBLE PRECISION XTOL
      CHARACTER*1 CC,RESP1,WGHT
      CHARACTER*32 RNAME,RECNAME,RLAB1,RLAB2
      CHARACTER*80  RESPON,RESP2,STRING
      CHARACTER*132 RUBB
      LOGICAL LLIM,NOCO,CRES,GRES,GTOT,GALL,LSECT,HARD,ERASE,ANS,
     :LOPEN,LALL,LCONQ,FINAL,FAULT,LWRIT

C
C     Set up parameter values for menu
C
      PARAMETER (LIM=1,SIN=2,NEW=3,NEX=4,INCH=5,LIS=6,SEL=7,
     :DEL=8,OPT=9,RECAL=10,IHARD=11,ISAVE=12,CON=13,QUIT=14,HELP=15,
     :QUEST=16 )
C
C     Set defaults
C
      NOCO=.FALSE.   ! Whether line extent overlapping continuum region
      LSECT=.FALSE.  ! Whether to work on another continuum section
      LLIM=.FALSE.   ! Whether line extent demarcated
      ERASE=.FALSE.  ! Whether to erase existing plot
      HARD=.FALSE.   ! Whether a hard copy is made
      LCON=.FALSE.   ! Whether to plot continuum fit
      CRES=.FALSE.   ! Whether to plot continuum residuals
      LWRIT=.FALSE.  ! Whether to results have been recorded to data file
      LALL=.TRUE.    ! Whether to print all of fit results to terminal
      LCONQ=.FALSE.  ! Whether any Gaussians are constrained
      FINAL=.FALSE.  ! Whether a satifactory Gaussian fit has been achieved
C
C     No Gaussians to fit yet
C
      NG=0
C
C     Issue menu prompt
C
10    CALL PAR_WRUSER('  ',STATUS)
      CALL PAR_WRUSER('GAUSS continuum fitting menu.'//
     : ' use ''HELP'' or ''?'' for assistance',STATUS)
C
C     Gaussian fitting menu entry. Alternatives are:
C
C        LIM    _    delimit edges of line ( default is adjacent
C                    continuum edges )
C        SIN    -    fit a single Gaussian to an indicated line
C        NEW    -    introduce a Gaussian at the cursor defined position
C        NEX    -    introduce a new Gaussian at the peak
C        INCH   -    interactively alter peak position, height or width
C        LIS    -    list the Gaussians fitted
C        SEL    -    select a line to be modified
C        DEL    -    delete a selected Gaussian
C        OPT    -    optimize the fit
C        RECAL  -    read a previous fit data file and plot this fit
C        HARD   -    plot results of fit for hardcopy device
C        SAVE   -    save the Gaussian fit spectrum as a file ( name
C                    prompted for on quitting )
C        CON    -    move to another section of continuum for more fitting
C        QUIT   -    quit from program ( spectrum analysis complete )
C
20    CALL PAR_WRUSER('  ',STATUS)
      CALL PAR_CNPAR('GCMD')
      CALL PAR_RDCHAR('GCMD',' ',RESPON)
      IF (PAR_ABORT()) GO TO 999
      INVOKE=ICH_FOLD(RESPON)
C
C     Test reply to determine if a valid prompt
C
      KEY=ICH_KEY(RESPON(:3),1,', ','LIM:SIN:NEW:NEX:INC:LIS'//
     :          ':SEL:DEL:OPT:REC:HAR:SAV:CON'//
     :          ':QUI:HEL:?:','Exact',NEXT)
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
50    IF (KEY.EQ.LIM) THEN
C
C       Cursor used to indicate the left and right edges of line extent.
C       Values checked against ICONO to ensure that not included as
C       part of continuum
C
        CALL PAR_WRUSER('Hit any key to record cursor position. '//
     :                  'Q to quit',STATUS)
51      XC=XVALS(NX/2) ! Somewhere to put the cursor initially
        YC=ABS(HIGH-LOW)/2. + LOW

52      CALL PAR_WRUSER('Use cursor to indicate left edge '//
     :           'of line ',STATUS)
        CALL PGCURSE(XC,YC,CC)
        IA=NINT((XC-XVALS(1))/WID) + 1
        CALL PAR_WRUSER('Use cursor to indicate right edge '//
     :           'of line ',STATUS)
        CALL PGCURSE(XC,YC,CC)
        IB=NINT((XC-XVALS(1))/WID) + 1
C
C       Check values of IA and IB to determine IB>IA and reasonable
C
        IF (IA.GE.IB.OR.IA.LT.1.OR.IB.GT.NX) THEN
          CALL PAR_WRUSER('Edges of continuum section invalid',STATUS)
          XC=XVALS(NX/2)
          YC=ABS(HIGH-LOW)/2. + LOW
          GO TO 52
        END IF
C
C       Check that these points are not included as continuum
C
        DO J=IA,IB
          IF (ICONO(J).EQ.1) THEN
            NOCO=.TRUE.
          END IF
        END DO
C
C       If continuum points within this section issue a warning
C
        IF (NOCO) THEN
          DO J=ICST,ICST+CX-1,1
            IF (ICONO(J).EQ.1.AND.ICONO(J+1).EQ.1.AND.J.EQ.IA) THEN
              LEFT=XVALS(J)
              STRING='Warning! Left edge of continuum section at'
              IGNORE=ICH_ENCODE(STRING,LEFT,44,10,NEXT)
              STRING(NEXT:)=' ( > left edge of line )'
              CALL PAR_WRUSER(STRING(:NEXT+24),STATUS)
              GO TO 53
            END IF
          END DO
53        DO J=ICST+CX,ICST,-1
            IF (ICONO(J).EQ.1.AND.ICONO(J-1).EQ.1.AND.J.EQ.IB) THEN
              RIGHT=XVALS(J)
              STRING='Warning! Right edge of continuum section at'
              IGNORE=ICH_ENCODE(STRING,RIGHT,45,10,NEXT)
              STRING(NEXT:)=' ( < right edge of line )'
              CALL PAR_WRUSER(STRING(:NEXT+24),STATUS)
              GO TO 55
            END IF
          END DO
        END IF
C
C       Set left edge of extent of line and width and indicate with an
C       asterisk. Set the arrays of X and Y values over the line extent
C
55      IGST=IA
        GX=IB-IA+1
        CALL PGPOINT(1,XVALS(IA),0.5*ABS(HIGH-LOW)+LOW,3)
        CALL PGPOINT(1,XVALS(IB),0.5*ABS(HIGH-LOW)+LOW,3)
        J=1
        DO I=IGST,IGST+GX-1,1
         GXVALS(J)=XVALS(I)
         GZVALS(J)=ZVALS(I)-CONVALS(I)
         J=J+1
        END DO
C
C       Set step size for INCHing the position, height and width
C       PSTEP and WSTEP=width of line extent
C       HSTEP=Peak height from continuum ( always positive )
C
        RSTEP=0.0E0
        PSTEP=ABS(GXVALS(GX)-GXVALS(1)+WID)
        WSTEP=PSTEP
        HSTEP=-1.E36
        DO I=1,GX
          IF (ABS(GZVALS(I)).GT.HSTEP) THEN
            HSTEP=ABS(GZVALS(I))
          END IF
        END DO
C
C       Set up some scaling parameters for peak position, height and width
C       for the E04JBF optimization to ensure solution for values in range
C       -1...+1. Values postfixed by C
C       are in COMMON
C
        MLM=(GXVALS(1) + GXVALS(GX))/2.
        HNORM=1.1*HSTEP
        RANG=ABS(GXVALS(GX) - GXVALS(1))/2.
        RANGW=RANG/2.3540

        NOCO=.FALSE.
        LLIM=.TRUE.
        GO TO 20
      END IF

60    IF (KEY.EQ.SIN) THEN
C
C       SINGLE Gaussian fit to the peak indicated by the cursor
C
        CALL PAR_WRUSER('Indicate position of line peak',STATUS)
61      XC=XVALS(NX/2) ! Somewhere to put the cursor initially
        YC=ABS(HIGH-LOW)/2. + LOW
        CALL PGCURSE(XC,YC,CC)
        IP=NINT((XC-XVALS(1))/WID) + 1
C
C       Check that this position is inside line extent.
C
        IF (LLIM) THEN
          IF (IP.LT.IGST.OR.IP.GT.(IGST+GX-1)) THEN
            CALL PAR_WRUSER('This position not inside line extent',
     :                      STATUS)
            GO TO 50
          END IF
        ELSE
C
C         Take extent of line as edges of continuum sections
C         Set the arrays of X and Y values over the line extent
C
64        DO I=IP,ICST,-1
            IF (ICONO(I).EQ.0.AND.ICONO(I-1).EQ.1) THEN
              IGST=I-1
              GO TO 65
            END IF
          END DO
65        DO I=IP,ICST+CX-1,1
            IF (ICONO(I).EQ.0.AND.ICONO(I+1).EQ.1) THEN
              GX=I+1-IGST+1
              GO TO 66
            END IF
          END DO
C
C         Issue a warning that line extent set by continuum
C
          CALL PAR_WRUSER('Warning! Line extent defined by nearest '//
     :        'edges of continuum sections',STATUS)
C
C         Check that IGST and GX are reasonable. If not set to
C         edges of spectrum but issue a warning
C
          IF (IGST.LT.1.OR.IGST.GT.NX) THEN
            CALL PAR_WRUSER('Warning! Left edge of line set to left'
     :                      //' edge of spectrum',STATUS)
            IGST=1
          ENDIF
          IF (GX.LT.1) THEN
            CALL PAR_WRUSER('Warning! Right edge of line set to right'
     :                      //' edge of spectrum',STATUS)
            GX=NX-IGST+1
          ENDIF
66        J=1
          DO I=IGST,IGST+GX-1,1
            GXVALS(J)=XVALS(I)
            GZVALS(J)=ZVALS(I)-CONVALS(I)
            J=J+1
          END DO
C
C         Set step size for INCHing the position, height and width
C         PSTEP and WSTEP=width of line extent
C         HSTEP=Peak height from continuum
C         ( required if further Gaussians are introduced )
C
          RSTEP=0.0E0
          PSTEP=ABS(GXVALS(GX)-GXVALS(1)+WID)
          WSTEP=PSTEP
          HSTEP=-1.E36
          DO I=1,GX
            IF (ABS(GZVALS(I)).GT.HSTEP) THEN
              HSTEP=ABS(GZVALS(I))
            END IF
          END DO
C
C         Set up some scaling parameters for peak position, height and width
C         for the E04JBF optimization to ensure solution for values in range
C         -1...+1. Values postfixed by C
C         are in COMMON
C
          MLM=(GXVALS(1) + GXVALS(GX))/2.
          HNORM=1.1*HSTEP
          RANG=ABS(GXVALS(GX) - GXVALS(1))/2.
          RANGW=RANG/2.3540
        END IF
C
C       Set LLIM false so that don't need to delimit each single line
C
        LLIM=.FALSE.
C
C       Determine line peak position
C
        MAXZ=-1.E36
        DO I=1,GX
          IF (ABS(GZVALS(I)).GT.MAXZ) THEN
            MAXZ=ABS(GZVALS(I))
            IP=I
          END IF
        END DO
C
C       Centroid this line over 5 channels around peak
C
        VALX=0.0
        VALZ=0.0
        DO I=IP-2,IP+2,1
          VALZ=VALZ + (ABS(GZVALS(I))*GXVALS(I))
          VALX=VALX + ABS(GZVALS(I))
        END DO
        GINFP(1)=VALZ/VALX
        IP=NINT((GINFP(1)-GXVALS(1))/WID) + 1
        GINFH(1)=GZVALS(IP)
C
C       Determine FWHM of this line by searching for positions where
C       signal is half peak height
C
        CALL FWHMIT(GX,GXVALS,WID,GZVALS,IP,FWHM)
        GINFW(1)=FWHM/2.3540
C
C       Form this line as GAUSUM
C
        CALL GAUF2(GX,GXVALS,1,GINFP,GINFH,GINFW,GAUSUM)
C
C       Get residuals and rms on line fit plus mean error in terms
C       of error bar ( if applicable ). Set residual max and min
C       to large and small numbers to prevent alteration of
C       residual scaling
C
        MILIM=-1.E30
        MALIM=1.E30
        CALL RMSER(GX,GZVALS,GAUSUM,ERRUSE,NX,ERRORS,IGST,
     :             GZRESID,RMS,MERR,MALIM,MILIM,ERASE)
C
C       Initial fit to Gaussian complete. Optimize the fit
C
        NG=1
        CURRENT=1
        GO TO 141
      END IF

80    IF (KEY.EQ.NEW) THEN
C
C       NEW - introduce a first Gaussian at a position defined by cursor
C
        IF (NG.GE.1) THEN
          CALL PAR_WRUSER('Line already set',STATUS)
          GO TO 20
        ENDIF
        CALL PAR_WRUSER('Indicate position of line peak',STATUS)
81      XC=XVALS(NX/2) ! Somewhere to put the cursor initially
        YC=ABS(HIGH-LOW)/2. + LOW

        CALL PGCURSE(XC,YC,CC)
        IP=NINT((XC-XVALS(1))/WID) + 1
C
C  Check that this position is inside line extent.
C
        IF (LLIM) THEN
          IF (IP.LT.IGST.OR.IP.GT.(IGST+GX-1)) THEN
            CALL PAR_WRUSER('This position not inside line extent',
     :                      STATUS)
            GO TO 20
          END IF
        ELSE
C
C         Take extent of line as edges of continuum sections
C         Set the arrays of X and Y values over the line extent
C
84        DO I=IP,ICST,-1
            IF (ICONO(I).EQ.0.AND.ICONO(I-1).EQ.1) THEN
              IGST=I-1
              GO TO 85
            END IF
          END DO
85        DO I=IP,ICST+CX-1,1
            IF (ICONO(I).EQ.0.AND.ICONO(I+1).EQ.1) THEN
              GX=I+1-IGST+1
              GO TO 86
            END IF
          END DO
C
C         Test for crazy values of IGST and GX if continuum wrongly
C         fitted.
C
          IF (IGST.LT.ICST.OR.GX.LE.0) THEN
            CALL PAR_WRUSER('Bad estimate of line extent',STATUS)
            GO TO 20
          ENDIF

86        J=1
          DO I=IGST,IGST+GX-1,1
            GXVALS(J)=XVALS(I)
            GZVALS(J)=ZVALS(I)-CONVALS(I)
           J=J+1
          END DO
C
C         Set step size for INCHing the position, height and width
C         PSTEP and WSTEP=width of line extent
C         HSTEP=Peak height from continuum
C
          RSTEP=0.0E0
          PSTEP=ABS(GXVALS(GX)-GXVALS(1)+WID)
          WSTEP=PSTEP
          HSTEP=-1.E36
          DO I=1,GX
            IF (ABS(GZVALS(I)).GT.HSTEP) THEN
              HSTEP=ABS(GZVALS(I))
            END IF
          END DO
C
C         Set up some scaling parameters for peak position, height and width
C         for the E04JBF optimization to ensure solution for values in range
C         -1...+1. Values postfixed by C
C         are in COMMON
C
          MLM=(GXVALS(1) + GXVALS(GX))/2.
          HNORM=1.1*HSTEP
          RANG=ABS(GXVALS(GX) - GXVALS(1))/2.
          RANGW=RANG/2.3540
C
C         Issue a warning that line extent set by continuum
C
          CALL PAR_WRUSER('Warning! Line extent defined by nearest '//
     :        'edges of continuum sections',STATUS)
          LLIM=.TRUE.
        END IF
C
C       Introduce a Gaussian of width 2* 2 channels and height equal
C       to the signal value at this position
C
        GINFP(NG+1)=GXVALS(IP-IGST+1)
        GINFH(NG+1)=GZVALS(IP-IGST+1)
        GINFW(NG+1)=2.*(2.*WID)/2.3540
        NG=NG+1
        CURRENT=NG
C
C       Add this Gaussian to others and the continuum
C
        CALL GAUF1(GX,GXVALS,ICST,NX,CX,IGST,CONVALS,NG,GINFP,
     :                  GINFH,GINFW,GAUSUM)
C
C       Plot this Gaussian sum without residuals
C
        GRES=.FALSE.
        GTOT=.TRUE.
        GALL=.FALSE.
        CALL GAUS_XZPLOT(XVALS,GXVALS,ZVALS,ERRORS,
     :  ZRESID,CONVALS,ICONO,GAUSUM,GAUFS,GZRESID,NX,CX,GX,NG,
     :  ICST,IGST,HIGH,LOW,HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,
     :  HARD,DEVICEH,ERRUSE,LCON,CRES,GRES,GTOT,GALL,XVST,XVEN,STATUS)
        IF (STATUS.NE.0) THEN
          CALL PAR_WRUSER('Error in plotting',STAT)
        END IF
        GO TO 20
      END IF

90    IF (KEY.EQ.NEX) THEN
C
C       Introduce new Gaussian of width 2* 2 channels and height equal
C       to the signal value at next peak position after subtracting
C       off existing Gaussians
C
        IF (NG.EQ.0) THEN
          CALL PAR_WRUSER('No Gaussians introduced yet',STATUS)
          GO TO 20
        ELSE IF (NG.GE.10) THEN
          CALL PAR_WRUSER('Cannot handle more than 10 Gaussians',STATUS)
          GO TO 20
        END IF
        CALL GAUF2(GX,GXVALS,NG,GINFP,GINFH,GINFW,GAUSUM)
        CALL GAUF3(GX,GXVALS,GZVALS,GAUSUM,GPOS,GPK)

        GINFP(NG+1)=GPOS
        GINFH(NG+1)=GPK
        GINFW(NG+1)=2.*(2.*WID)/2.3540
        NG=NG+1
        CURRENT=NG
        CALL GAUF1(GX,GXVALS,ICST,NX,CX,IGST,CONVALS,NG,GINFP,
     :                  GINFH,GINFW,GAUSUM)
C
C       Plot this Gaussian sum without residuals
C
        GRES=.FALSE.
        GTOT=.TRUE.
        GALL=.FALSE.
        CALL GAUS_XZPLOT(XVALS,GXVALS,ZVALS,ERRORS,
     :  ZRESID,CONVALS,ICONO,GAUSUM,GAUFS,GZRESID,NX,CX,GX,NG,
     :  ICST,IGST,HIGH,LOW,HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,
     :  HARD,DEVICEH,ERRUSE,LCON,CRES,GRES,GTOT,GALL,XVST,XVEN,STATUS)
        IF (STATUS.NE.0) THEN
          CALL PAR_WRUSER('Error in plotting',STAT)
        END IF
        GO TO 20
      END IF

100   IF (KEY.EQ.INCH) THEN
C
C       Inch peak position, height or width of Gaussian no. CURRENT
C       Valid responses are: P-100 to P100; H-100 to H100; W-100 to
C       W100; S
C
        IF (NG.EQ.0) THEN
          CALL PAR_WRUSER('No Gaussians to modify',STATUS)
          GO TO 20
        END IF
        IF (CURRENT.EQ.0) THEN
          CALL PAR_WRUSER('No Gaussian selected to alter',STATUS)
          GO TO 20
        ENDIF
        CALL PAR_WRUSER(' P for position. H for height. W for width'//
     :                  ' S to stop',STATUS)
101     CALL PAR_CNPAR('Pn_Hn_Wn_S')
        CALL PAR_RDCHAR('Pn_Hn_Wn_S',' ',STRING)
        IF (PAR_ABORT()) GO TO 999
C
C       Decode STRING
C
        RESP1=STRING(:1)
        RESP2=STRING(2:5)
        IF (RESP1.EQ.'P'.OR.RESP1.EQ.'p') THEN
          IF (RESP2 .NE. ' ') THEN
            READ(RESP2,FMT=*,ERR=106) RSTEP
          END IF
          GINFP(CURRENT)=GINFP(CURRENT) + (RSTEP*PSTEP/100.)
          IPOL=1
        ELSE IF (RESP1.EQ.'H'.OR.RESP1.EQ.'h') THEN
          IF (RESP2 .NE. ' ') THEN
            READ(RESP2,FMT=*,ERR=106) RSTEP
          END IF
          GINFH(CURRENT)=GINFH(CURRENT) + (RSTEP*HSTEP/100.)
          IPOL=1
        ELSE IF (RESP1.EQ.'W'.OR.RESP1.EQ.'w') THEN
          IF (RESP2 .NE. ' ') THEN
            READ(RESP2,FMT=*,ERR=106) RSTEP
          END IF
          GINFW(CURRENT)=GINFW(CURRENT) +
     :                   ((RSTEP*WSTEP/100.)/2.3540)
          IF (GINFW(CURRENT).LT.(WID/2.3540)) THEN
            CALL PAR_WRUSER('FWHM less than one channel.'//
     :                       'Will reset to 1 channel',STATUS)
            GINFW(CURRENT)=WID/2.3540
          ENDIF
          IPOL=1
        ELSE IF (RESP1.EQ.'S'.OR.RESP1.EQ.'s') THEN
          IPOL=2
        ELSE
          CALL PAR_WRUSER('Error reading input',STATUS)
          GO TO 101
        END IF
        GO TO 201
106     CALL PAR_WRUSER('Error reading input',STATUS)
        GO TO 101
201     CONTINUE
C
C       Erase previous Gaussian sum and plot out new profile
C       without residuals
C
        IF (IPOL.EQ.1) THEN
          CALL ERASER(GX,GXVALS,GAUSUM,XVST,XVEN,HIGH,LOW,HIGHR,
     :                LOWR,2)
          CALL GAUF1(GX,GXVALS,ICST,NX,CX,IGST,CONVALS,NG,GINFP,
     :                  GINFH,GINFW,GAUSUM)
          GRES=.FALSE.
          GTOT=.TRUE.
          GALL=.FALSE.
          CALL GAUS_XZPLOT(XVALS,GXVALS,ZVALS,ERRORS,
     :    ZRESID,CONVALS,ICONO,GAUSUM,GAUFS,GZRESID,NX,CX,GX,NG,
     :    ICST,IGST,HIGH,LOW,HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,
     :    HARD,DEVICEH,ERRUSE,LCON,CRES,GRES,GTOT,GALL,XVST,XVEN,STATUS)
          IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Error in plotting',STAT)
          END IF
          GO TO 101
        END IF
C
C       Erase previous Gaussian fit and residuals and plot
C       out new fit and residuals. Report r.m.s. ( in terms
C       of errors if available )
C
        IF (IPOL.EQ.2) THEN
          CALL GAUF2(GX,GXVALS,NG,GINFP,GINFH,GINFW,GAUSUM)
C
C       Get residuals and rms on line fit plus mean error in terms
C       of error bar ( if applicable ). If residuals larger than
C       those previously plotted then redraw whole plot to encompass them
C
          CALL RMSER(GX,GZVALS,GAUSUM,ERRUSE,NX,ERRORS,IGST,
     :             GZRESID,RMS,MERR,HIGHR,LOWR,ERASE)
          IF (ERRUSE) THEN
            STRING='               R.m.s. on fit = '
            INVOKE=ICH_ENCODE(STRING,RMS,32,10,NEXT)
            CALL PAR_WRUSER(STRING(:NEXT),STATUS)
            STRING=' Mean fractional error on fit = '
            INVOKE=ICH_ENCODE(STRING,MERR,32,10,NEXT)
            CALL PAR_WRUSER(STRING(:NEXT),STATUS)
          ELSE
            STRING='               R.m.s. on fit = '
            INVOKE=ICH_ENCODE(STRING,RMS,32,10,NEXT)
            CALL PAR_WRUSER(STRING(:NEXT),STATUS)
          END IF
          IF (ERASE) THEN
            LCON=.TRUE.
            CRES=.TRUE.
          END IF
C
C         Plot the fit to the line and the residuals on the fit
C
          CALL GAUF1(GX,GXVALS,ICST,NX,CX,IGST,CONVALS,NG,GINFP,
     :                  GINFH,GINFW,GAUSUM)
          GRES=.TRUE.
          GTOT=.TRUE.
          GALL=.FALSE.
          CALL GAUS_XZPLOT(XVALS,GXVALS,ZVALS,ERRORS,
     :    ZRESID,CONVALS,ICONO,GAUSUM,GAUFS,GZRESID,NX,CX,GX,NG,
     :    ICST,IGST,HIGH,LOW,HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,
     :    HARD,DEVICEH,ERRUSE,LCON,CRES,GRES,GTOT,GALL,XVST,XVEN,STATUS)
          ERASE=.FALSE.
          LCON=.FALSE.
          CRES=.FALSE.
          IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Error in plotting',STAT)
          END IF
        END IF
        GO TO 20
      END IF

110   IF (KEY.EQ.LIS) THEN
C
C       LIST Gaussians fitted so far
C
        IF (NG.EQ.0) THEN
          CALL PAR_WRUSER('No Gaussians to list',STATUS)
          GO TO 20
        END IF
        CALL PAR_WRUSER('Fitted Gaussians:-',STATUS)
        CALL PAR_WRUSER('  Index      Posn        Height       Sigma',
     :    STATUS)
        DO I=1,NG
          WRITE(STRING,'(1X,I4,4X,F10.3,2X,E12.4,2X,F10.3)') I,GINFP(I),
     :            GINFH(I),GINFW(I)
          CALL PAR_WRUSER(STRING,STATUS)
        END DO
        GO TO 20
      END IF

120   IF (KEY.EQ.SEL) THEN
C
C       SELect Guassian of particular index number to modify
C
        IF (NG.EQ.0) THEN
          CALL PAR_WRUSER('No Gaussians to modify',STATUS)
          GO TO 20
        END IF
        CALL PAR_WRUSER('Fitted Gaussians:-',STATUS)
        CALL PAR_WRUSER('  Index      Posn        Height       Sigma',
     :    STATUS)
        DO I=1,NG
          WRITE(STRING,'(1X,I4,4X,F10.3,2X,E12.4,2X,F10.3)') I,GINFP(I),
     :            GINFH(I),GINFW(I)
          CALL PAR_WRUSER(STRING,STATUS)
        END DO
        CALL PAR_CNPAR('INDEX')
        CALL PAR_RDVAL('INDEX',1.,REAL(NG),1.,' ',VALUE)
        IF (PAR_ABORT()) GO TO 999
        CURRENT=INT(VALUE(1))
        GO TO 20
      END IF

130   IF (KEY.EQ.DEL) THEN
C
C       DELete a selected Guassian of particular index number
C
        IF (NG.LE.1) THEN
          CALL PAR_WRUSER('Only one or no Gaussians to delete',STATUS)
          GO TO 20
        END IF
        CALL PAR_WRUSER('Fitted Gaussians:-',STATUS)
        CALL PAR_WRUSER('  Index      Posn        Height       Sigma',
     :    STATUS)
        DO I=1,NG
          WRITE(STRING,'(1X,I4,4X,F10.3,2X,E12.4,2X,F10.3)') I,GINFP(I),
     :            GINFH(I),GINFW(I)
          CALL PAR_WRUSER(STRING,STATUS)
        END DO
        CALL PAR_CNPAR('DELE')
        CALL PAR_RDVAL('DELE',1.,REAL(NG),1.,' ',VALUE)
        IF (PAR_ABORT()) GO TO 999
        NDEL=INT(VALUE(1))
        IF (NDEL.GT.NG) THEN
          CALL PAR_WRUSER('No such Gaussian',STATUS)
          GO TO 20
        ENDIF
C
C       Delete this Gaussian from the list
C
        J=1
        DO I=1,NG
          IF (I.NE.NDEL) THEN
            GINFP(J)=GINFP(I)
            GINFH(J)=GINFH(I)
            GINFW(J)=GINFW(I)
            J=J+1
          END IF
        END DO
        NG=J-1
C
C       Form the total sum of the fitting Gaussians
C
        CALL GAUF2(GX,GXVALS,NG,GINFP,GINFH,GINFW,GAUSUM)
C
C       Get the residuals and rms on this fit
C
        CALL RMSER(GX,GZVALS,GAUSUM,ERRUSE,NX,ERRORS,IGST,
     :             GZRESID,RMS,MERR,HIGHR,LOWR,ERASE)
C
C       Plot the fit to the line and the residuals on the fit
C
        CALL GAUF1(GX,GXVALS,ICST,NX,CX,IGST,CONVALS,NG,GINFP,
     :                  GINFH,GINFW,GAUSUM)
C
C       Plot the continuum, sum of fitting Gaussians and residuals
C
        LCON=.TRUE.
        CRES=.TRUE.
        GRES=.TRUE.
        GTOT=.TRUE.
        GALL=.FALSE.
        ERASE=.TRUE.
        CALL GAUS_XZPLOT(XVALS,GXVALS,ZVALS,ERRORS,
     :   ZRESID,CONVALS,ICONO,GAUSUM,GAUFS,GZRESID,NX,CX,GX,NG,
     :   ICST,IGST,HIGH,LOW,HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,
     :   HARD,DEVICEH,ERRUSE,LCON,CRES,GRES,GTOT,GALL,XVST,XVEN,STATUS)
        ERASE=.FALSE.
        LCON=.FALSE.
        CRES=.FALSE.
        IF (STATUS.NE.0) THEN
          CALL PAR_WRUSER('Error in plotting',STAT)
        END IF
        GO TO 20
      END IF

140   IF (KEY.EQ.OPT) THEN
C
C       OPTimization of the interactive fit.
C
        IF (NG.LT.1) THEN
          CALL PAR_WRUSER('No Gaussians to optimize',STATUS)
          GO TO 20
        END IF
      END IF
141   CONTINUE
      IF (KEY.EQ.OPT) THEN
C
C       Check that no peaks occur outside the range GXVALS(1) to
C       GXVALS(GX). If so delete.
C
        J=1
        DO I=1,NG
          IF (GINFP(I).GE.GXVALS(1).AND.GINFP(I).LE.GXVALS(GX)) THEN
            GINFP(J)=GINFP(I)
            GINFH(J)=GINFH(I)
            GINFW(J)=GINFW(I)
            J=J+1
          ENDIF
        END DO
        NG=J-1
        IF (NG.EQ.0) THEN
          CALL PAR_WRUSER('No Gaussians to optimize',STATUS)
          GO TO 20
        END IF
        IF (KEY.EQ.SIN) THEN
C
C          Don't constrain or chain fit
C
           LCONQ=.FALSE.
           GO TO 1415
        ENDIF
C
C       Prompt for constraints
C
        CALL PAR_CNPAR('CONSTR')
        CALL PAR_RDKEY('CONSTR',.FALSE.,LCONQ)
        IF (PAR_ABORT()) GO TO 999
1415    IF (LCONQ) THEN
C
C         Call the subroutine for setting up constraints
C
          CALL CONSTR(NG,GINFP,GINFH,GINFW,CONP,CONH,CONW,
     :                ICHAINP,ICHAINH,ICHAINW,CHAINP,CHAINH,
     :                CHAINW)
          LALL=.TRUE.
        ELSE
C
C         No constraints; set arrays to zero
C
          DO I=1,NG,1
            CONP(I)=0
            CONH(I)=0
            CONW(I)=0
            ICHAINP(I)=0
            ICHAINH(I)=0
            ICHAINW(I)=0
          ENDDO
        ENDIF
C
C       Determine what sort of weighting of residuals is required.
C       Options are: N for none ( ie. unit weighting )
C                    E for weighting by error values
C                    S for weighting by values
C
142     CALL PAR_CNPAR('WGHT')
        CALL PAR_RDCHAR('WGHT','N',WGHT)
        IF (PAR_ABORT()) GO TO 999
        INVOKE=ICH_FOLD(WGHT)
C
C       Initialize the weights
C
        DO I=1,GX
          WHT(I)=1.0
        END DO
        TOTW=0.0
        IF (WGHT.EQ.'N') THEN
C
C         Unit weigthing
C
          DO I=1,GX
            WHT(I)=1.0
            TOTW=TOTW+WHT(I)
          END DO
          GO TO 143
        ENDIF
        IF (WGHT.EQ.'E') THEN
C
C         Check that errors available. If so form weight as ratio
C         of value to error
C
          IF (.NOT.ERRUSE) THEN
            CALL PAR_WRUSER('No errors available',STATUS)
            GO TO 141
          ENDIF
          DO I=1,GX
            IF (ERRORS(I-1+IGST).EQ.0.0) THEN
              WHT(I)=1.0
            ELSE
              WHT(I)=1./(ERRORS(I-1+IGST)**2.)
            ENDIF
            TOTW=TOTW+WHT(I)
          END DO
          GO TO 143
        ENDIF
        IF (WGHT.EQ.'V') THEN
C
C         Weight by signal value ( including continuum )
C
          DO I=1,GX
            IF (ZVALS(I-1+IGST).EQ.0.0) THEN
              WHT(I)=1.0
            ELSE
              WHT(I)=ABS(ZVALS(I-1+IGST))
            ENDIF
            TOTW=TOTW+WHT(I)
          END DO
          GO TO 143
        ENDIF
        GO TO 142
143     XTOL=XTOLI
        FITF3=0
144     CALL AUTOFIT(GX,GXVALS,WID,MLM,HNORM,RANG,RANGW,GZVALS,
     :               GZRESID,WHT,TOTW,NG,GINFP,GINFH,GINFW,CONP,
     :               CONH,CONW,ICHAINP,ICHAINH,ICHAINW,CHAINP,
     :               CHAINH,CHAINW,XTOL,GONFP,GONFH,GONFW,IFAIL1,
     :               IFAIL2)
C
C       If sucessful fitting write out the results to the terminal.
C
        IF (IFAIL2.EQ.0) THEN
          FINAL=.TRUE.
          CALL PAR_WRUSER('Succesful fit. Gaussian fit parameters'//
     :            ' are:',STATUS)
        ELSE
          IF (IFAIL1.NE.0) THEN
            CALL PAR_WRUSER('Severe problems calculating function '//
     :             'slope. Suggest a manual fit',STATUS)
            GO TO 20
          END IF
          IF (IFAIL2.EQ.2) THEN
            CALL PAR_WRUSER('No minimum found. More evaluations '//
     :      'may be required.',STATUS)
          END IF
          IF (IFAIL2.EQ.3) THEN
C
C           Double value of XTOL and refit upto a maximum of three times
C
            FITF3=FITF3+1
            IF (FITF3.LT.3) THEN
              XTOL=XTOLI*2.*DBLE(FITF3)
              GO TO 144
            ELSE
              CALL PAR_WRUSER('No minimum found. Tolerance may '//
     :                    'be set too low.',STATUS)
            END IF
          END IF
          CALL PAR_WRUSER('Possible that Gaussian(s) not a '//
     :    'satisfactory fit. Suggest trying another fit.',STATUS)
          CALL PAR_CNPAR('MAN')
          CALL PAR_RDKEY('MAN',.TRUE.,ANS)
          IF (PAR_ABORT()) GO TO 999
          IF (ANS) THEN
            DO I=1,NG
              GONFP(I)=GINFP(I)
              GONFH(I)=GINFH(I)
              GONFW(I)=GINFW(I)
            END DO
            FINAL=.TRUE.
            GO TO 145
          ELSE
            ERASE=.FALSE.
            GO TO 20
          END IF
        END IF
C
C       Calculate equivalent width
C
145     EW=0.0
        DO I=IGST,IGST+GX-1,1
          IF (CONVALS(I).EQ.0.0) THEN
            CALL PAR_WRUSER('Error calculating equivalent width.'//
     :          ' Division by zero',STATUS)
            EW=0.0
            GO TO 147
          ENDIF
          EW=EW+(1. - (ZVALS(I)/CONVALS(I)))
        END DO
        EW=WID*EW
C
C       Form the total sum of the fitting Gaussians
C
147     CALL GAUF2(GX,GXVALS,NG,GONFP,GONFH,GONFW,GAUSUM)
C
C       Get residuals and rms on line fit plus mean error in terms
C       of error bar ( if applicable ). If residuals larger than
C       those previously plotted then redraw whole plot to encompass them
C
        CALL RMSER(GX,GZVALS,GAUSUM,ERRUSE,NX,ERRORS,IGST,
     :             GZRESID,RMS,MERR,HIGHR,LOWR,ERASE)
C
C       Write results to terminal
C
        CALL TERMWRIT(NG,GONFP,GONFH,GONFW,EW,RMS,ERRUSE,MERR,
     :                XUNITS,ZUNITS,LALL)
C
C       Record results on file if required
C
        IF (LREC) THEN
          CALL GAUS_RECORD(NG,GONFP,GONFH,GONFW,EW,RMS,MERR,ORD,SIG,
     :                                                ERR,LU,FAULT)
          LWRIT=.TRUE.
          IF (FAULT) THEN
            CALL PAR_WRUSER('Failed to write results to data file',
     :                       STATUS)
          END IF
        END IF
C
C       Set up an array containing the profile for each fitting
C       Gaussian on the continuum for plotting
C
        CALL GAUF4(GX,GXVALS,ICST,NX,CX,IGST,CONVALS,NG,GONFP,
     :               GONFH,GONFW,GAUFS)
C
C       Set up an array of the total fit on the continuum
C
        CALL GAUF1(GX,GXVALS,ICST,NX,CX,IGST,CONVALS,NG,GONFP,
     :                  GONFH,GONFW,GAUSUM)
C
C       Plot this fit and its residuals
C
        IF (ERASE) THEN
          LCON=.TRUE.
          CRES=.TRUE.
        END IF
        GRES=.TRUE.
        GTOT=.TRUE.
        GALL=.TRUE.
        CALL GAUS_XZPLOT(XVALS,GXVALS,ZVALS,ERRORS,
     :  ZRESID,CONVALS,ICONO,GAUSUM,GAUFS,GZRESID,NX,CX,GX,NG,
     :  ICST,IGST,HIGH,LOW,HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,
     :  HARD,DEVICEH,ERRUSE,LCON,CRES,GRES,GTOT,GALL,XVST,XVEN,STATUS)
        ERASE=.FALSE.
        LCON=.FALSE.
        CRES=.FALSE.
        IF (STATUS.NE.0) THEN
          CALL PAR_WRUSER('Error in plotting',STAT)
        END IF
C
C       Reset LLIM to false so that another line can be started as NEW
C       if required
C
        LLIM=.FALSE.
        GO TO 20
      END IF

150   IF (KEY.EQ.RECAL) THEN
C
C       The data file giving the results of a previous fit to this line
C       is recalled and the Gaussians set by this fit. The parameters
C       of the fit are written to the terminal and the result plotted.
C
        IF (.NOT.LLIM) THEN
C         Line extent must be demarcated by cursor first
          CALL PAR_WRUSER('Need to indicate line extent with cursor',
     :                     STAT)
          GO TO 20
        ENDIF
C
C       Try to get a logical unit on which to read the file
C
C       RECSTAT=LIB$GET_LUN(LUR)
C       IF (.NOT.RECSTAT) THEN
        RECSTAT=0
        CALL DSA_GET_LU(LUR,RECSTAT)
        IF (RECSTAT.NE.0) THEN
          CALL PAR_WRUSER('Unable to get logical unit for file',
     :                    STATUS)
          GO TO 20
        END IF
C
C       Prompt for name of data file. Check that it exists. If so
C       read values for peak position, height and width into GINF
C       arrays
C
        CALL PAR_RDCHAR('RECNAME',' ',RECNAME)
        IF (PAR_ABORT()) GO TO 999
        ILEN1=INDEX(RECNAME,' ')
        ILEN2=INDEX(RECNAME,'.')
        IF (ILEN2.LT.ILEN1.AND.ILEN2.GT.0) THEN
          RNAME=RECNAME(:ILEN2-1)//'.dat'
        ELSE
          RNAME=RECNAME(:ILEN1-1)//'.dat'
        ENDIF
C       Determine if this file already exists
C
        INQUIRE(FILE=RNAME,EXIST=LOPEN)
        IF (LOPEN) THEN
C
C         Open this file for reading
C
          OPEN(UNIT=LUR,FILE=RNAME,STATUS='OLD',
     :           FORM='FORMATTED',IOSTAT=NOPEN)
          IF (NOPEN.NE.0) THEN
            CALL PAR_WRUSER('Failed to open file',STAT)
            GO TO 20
          END IF
C
C         Read the header information at the top of this file
C
          READ(LUR,151) RUBB
          READ(LUR,151) RUBB
          READ(LUR,151) RUBB
          READ(LUR,152) RUBB,RLAB1
          READ(LUR,152) RUBB,RLAB2
          READ(LUR,151) RUBB
          READ(LUR,151) RUBB
          READ(LUR,151) RUBB
          READ(LUR,151) RUBB
          READ(LUR,151) RUBB
          READ(LUR,151) RUBB
          READ(LUR,151) RUBB
C
C         Read the Gaussian fit parameters
C
          READ(LUR,153,ERR=168,END=166) GINFP(1),GINFH(1),GINFW(1),
     :     RUB,RUB,RUB,RUB,RUBI,RUB,RUB
C
C         Check that the centre position of this line lies in the
C         range of values for the line extent. If not issue a warning
C         and don't include this Gaussian in the GINF arrays
C
          J=1
          LG=INT((GINFP(1)-GXVALS(1))/WID)
          MG=INT((GXVALS(GX)-GINFP(1))/WID)
          I=2
          IF (LG.LE.0.OR.MG.LE.0) THEN
            WRITE (STRING,160,IOSTAT=IGNORE) J
160         FORMAT('Gaussian number ',I3)
            STRING(20:43)=' not inside line extent'
            CALL PAR_WRUSER(STRING(:43),STATUS)
            I=1
          ENDIF
          J=2
          DO WHILE (.TRUE.)
            READ(LUR,154,ERR=168,END=166) GINFP(I),GINFH(I),GINFW(I),
     :             RUB
            LG=NINT((GINFP(I)-GXVALS(1))/WID)
            MG=NINT((GXVALS(GX)-GINFP(I))/WID)
            IF (LG.LE.0.OR.MG.LE.0) THEN
              WRITE (STRING,160,IOSTAT=IGNORE) J
              STRING(20:43)=' not inside line extent'
              CALL PAR_WRUSER(STRING(:43),STATUS)
            ELSE
              I=I+1
            ENDIF
            J=J+1
          END DO
151       FORMAT(A132)
152       FORMAT(A17,A32)
153       FORMAT(1X,F12.3,1X,E14.4,1X,F10.4,1X,E12.4,1X,F10.3,1X,
     :         E11.4,1X,F10.4,6X,I5,4X,F8.3,4X,F8.3)
154       FORMAT(1X,F12.3,1X,E14.4,1X,F10.4,1X,E12.4)
166       CLOSE(UNIT=LUR)
          NG=I-1
C
C         Make sure at least one Gaussian has been recalled
C
          IF (NG.LT.1) THEN
            CALL PAR_WRUSER('No Gaussians recalled',STATUS)
            GO TO 20
          ENDIF
C
C         Write name of file and label to terminal as reminder
C
          STRING=' Name of recalled spectrum: '
          STRING(28:)=RLAB1
          CALL PAR_WRUSER(STRING(:60),STAT)
          STRING='Label of recalled spectrum: '
          STRING(28:)=RLAB2
          CALL PAR_WRUSER(STRING(:60),STAT)
C
C         Form the total sum of the fitting Gaussians
C
          CALL GAUF2(GX,GXVALS,NG,GINFP,GINFH,GINFW,GAUSUM)
C
C         Calculate equivalent width
C
          EW=0.0
          DO I=IGST,IGST+GX-1,1
            IF (CONVALS(I).EQ.0.0) THEN
              CALL PAR_WRUSER('Error calculating equivalent width.'//
     :          ' Division by zero',STAT)
              EW=0.0
              GO TO 167
            ENDIF
            EW=EW+(1. - (ZVALS(I)/CONVALS(I)))
          END DO
          EW=WID*EW
C
C         Get the rms on this fit and write the results to the terminal
C
167       CALL RMSER(GX,GZVALS,GAUSUM,ERRUSE,NX,ERRORS,IGST,
     :             GZRESID,RMS,MERR,HIGHR,LOWR,ERASE)
          CALL TERMWRIT(NG,GINFP,GINFH,GINFW,EW,RMS,ERRUSE,MERR,
     :                XUNITS,ZUNITS,LALL)
C
C         Plot the fit to the line and the residuals on the fit
C
          CALL GAUF1(GX,GXVALS,ICST,NX,CX,IGST,CONVALS,NG,GINFP,
     :                  GINFH,GINFW,GAUSUM)
C
C         Set up an array containing the profile for each fitting
C         Gaussian on the continuum for plotting
C
          CALL GAUF4(GX,GXVALS,ICST,NX,CX,IGST,CONVALS,NG,GINFP,
     :               GINFH,GINFW,GAUFS)
C
C         Plot the continuum, sum of fitting Gaussians, fitting Gaussians
C         and residuals
C
          LCON=.TRUE.
          CRES=.TRUE.
          GRES=.TRUE.
          GTOT=.TRUE.
          GALL=.TRUE.
          ERASE=.TRUE.
          CALL GAUS_XZPLOT(XVALS,GXVALS,ZVALS,ERRORS,
     :    ZRESID,CONVALS,ICONO,GAUSUM,GAUFS,GZRESID,NX,CX,GX,NG,
     :    ICST,IGST,HIGH,LOW,HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,
     :    HARD,DEVICEH,ERRUSE,LCON,CRES,GRES,GTOT,GALL,XVST,XVEN,STATUS)
          ERASE=.FALSE.
          LCON=.FALSE.
          CRES=.FALSE.
          IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Error in plotting',STAT)
          END IF
        ELSE
          CALL PAR_WRUSER('Data file not found',STAT)
        END IF
        GO TO 20
C
C       Error exit
C
168     CALL PAR_WRUSER('Error reading data',STAT)
        GO TO 20
      ENDIF

170   IF (KEY.EQ.IHARD) THEN
C
C       Make a HARD copy plot of the fit. Check that there is at least
C       an attempt at a fit by checking that at least one Gaussian has
C       been fitted
C
        IF (NG.GT.0) THEN
          LCON=.TRUE.
          CRES=.TRUE.
          GRES=.TRUE.
          GTOT=.TRUE.
          GALL=.TRUE.
          HARD=.TRUE.
          CALL GAUS_XZPLOT(XVALS,GXVALS,ZVALS,ERRORS,
     :    ZRESID,CONVALS,ICONO,GAUSUM,GAUFS,GZRESID,NX,CX,GX,NG,
     :    ICST,IGST,HIGH,LOW,HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,
     :    HARD,DEVICEH,ERRUSE,LCON,CRES,GRES,GTOT,GALL,XVST,XVEN,STATUS)
          IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Error in plotting',STAT)
          END IF
        ELSE
          CALL PAR_WRUSER('No Gaussian fit to plot',STATUS)
        END IF
        GO TO 20
      END IF

180   IF (KEY.EQ.ISAVE) THEN
C
C       Gaussian and continuum fit to be saved on a file on quitting
C       from the main subroutine. Copy continuum values and final
C       fit on continuum ( GAUSUM ) into array FITTOT. If no lines
C       fitted then just copy continuum fit to FITTOT. Set the logical
C       variable SAVE so that FITTOT for the current fitted section will
C       be copied to an array which is written out as a file on exit from
C       the program
C
        IF (NG.GE.1) THEN
          CALL GAUF1(GX,GXVALS,ICST,NX,CX,IGST,CONVALS,NG,GINFP,
     :                  GINFH,GINFW,GAUSUM)
          DO I=ICST,IGST,1
            FITTOT(I)=CONVALS(I)
          END DO
          J=1
          DO I=IGST,IGST+GX-1,1
            FITTOT(I)=GAUSUM(J)
            J=J+1
          END DO
          DO I=IGST+GX-1,ICST+CX-1,1
            FITTOT(I)=CONVALS(I)
          END DO
        ELSE
          DO I=ICST,ICST+CX-1,1
            FITTOT(I)=CONVALS(I)
          ENDDO
        ENDIF
        SAVE=.TRUE.
        GO TO 20
      ENDIF

190   IF (KEY.EQ.CON) THEN
C
C       Gaussian fitting in this section complete. More lines to
C       fit in another section.
C
        IF (NG.EQ.0) THEN
          LSECT=.TRUE.
          GO TO 999
        ENDIF
        IF (.NOT.LWRIT.AND.LREC) THEN
C
C         Results not written to data file ( no optimization performed ),
C         yet required. Form the total sum of the fitting Gaussians
C         and calculate rms and equivalent width and record
C
          CALL GAUF2(GX,GXVALS,NG,GINFP,GINFH,GINFW,GAUSUM)
C
C         Get the rms on this fit
C
          CALL RMSER(GX,GZVALS,GAUSUM,ERRUSE,NX,ERRORS,IGST,
     :             GZRESID,RMS,MERR,HIGHR,LOWR,ERASE)
C
C         Calculate the equivalent width
C
          EW=0.0
          DO I=IGST,IGST+GX-1,1
            IF (CONVALS(I).EQ.0.0) THEN
              CALL PAR_WRUSER('Error calculating equivalent width.'//
     :          ' Division by zero',STATUS)
              EW=0.0
              GO TO 192
            ENDIF
            EW=EW+(1. - (ZVALS(I)/CONVALS(I)))
          END DO
          EW=WID*EW

192       CALL GAUS_RECORD(NG,GINFP,GINFH,GINFW,EW,RMS,MERR,ORD,SIG,
     :                                                ERR,LU,FAULT)
          LWRIT=.TRUE.
          IF (FAULT) THEN
            CALL PAR_WRUSER('Failed to write results to data file',
     :                       STATUS)
          END IF
        ENDIF
        LSECT=.TRUE.
        GO TO 999
      END IF

200   IF (KEY.EQ.QUIT) THEN
C
C       Spectral analysis complete. Program will close down on return.
C
        IF (NG.EQ.0) THEN
          GO TO 999
        ENDIF
        IF (.NOT.LWRIT.AND.LREC) THEN
C
C         Results not written to data file ( no optimization performed ),
C         yet required. Form the total sum of the fitting Gaussians
C         and calculate rms and equivalent width and record
C
          CALL GAUF2(GX,GXVALS,NG,GINFP,GINFH,GINFW,GAUSUM)
C
C         Get the rms on this fit
C
          CALL RMSER(GX,GZVALS,GAUSUM,ERRUSE,NX,ERRORS,IGST,
     :             GZRESID,RMS,MERR,HIGHR,LOWR,ERASE)
C
C         Calculate the equivalent width
C
          EW=0.0
          DO I=IGST,IGST+GX-1,1
            IF (CONVALS(I).EQ.0.0) THEN
              CALL PAR_WRUSER('Error calculating equivalent width.'//
     :          ' Division by zero',STATUS)
              EW=0.0
              GO TO 202
            ENDIF
            EW=EW+(1. - (ZVALS(I)/CONVALS(I)))
          END DO
          EW=WID*EW

202       CALL GAUS_RECORD(NG,GINFP,GINFH,GINFW,EW,RMS,MERR,ORD,SIG,
     :                                                ERR,LU,FAULT)
          LWRIT=.TRUE.
          IF (FAULT) THEN
            CALL PAR_WRUSER('Failed to write results to data file',
     :                       STATUS)
          END IF
        ENDIF
        LSECT=.FALSE.
        GO TO 999
      END IF

210   IF (KEY.EQ.HELP.OR.KEY.EQ.QUEST) THEN
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
