*+CLEANPOW  -  Power spectrum program using a CLEAN algorithm.
      SUBROUTINE CLEANPOW(STATUS)
*    Description :
*       Given a timeseries the program calculates a periodogram,
*       for a given timeseries and CLEANs it.
*    Environment parameters :
*    Method :
*
*        For a theoretical explanation see Roberts, et. al,
*        A.J., 93, 968 (1987)   and
*
*        Clark, Astron. Astroph., 89, 377 (1980)
*
*    Deficiencies :
*    Bugs :
*    Authors :
*     Harry Lehto  (SOTON::HL)
*    History :
*     15 Aug 90 : Original
*      7 Jun 91 : Asterix version   (LTVAD::RDS)
*     11 Apr 95 : V1.8-0  Updated data interfaces (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PAR_ERR'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      INTEGER NCL,NBIN
         PARAMETER       (NCL=10000)
         PARAMETER       (NBIN=100)

*    Local variables :
      INTEGER NTOT                       ! Total number of data points
      INTEGER NGOOD                      ! Number of GOOD data points
      INTEGER TPNTR                      ! Pointer to times array
      INTEGER DPNTR                      ! Pointer to data array
      INTEGER VPNTR                      ! Pointer to variance array
      INTEGER WPNTR                      ! Pointer to WEIGHTS array
      INTEGER FPTR                       ! Pointer to frequency array
      INTEGER PPTR                       ! Pointer to POWER array
      INTEGER PHPTR                      ! Pointer to PHASE array
      INTEGER SFPTR                      ! Pointer to S.P. frequency array
      INTEGER SPPTR                      ! Pointer to S.P. POWER array
      INTEGER SPHPTR                     ! Pointer to S.P. PHASE array
      INTEGER WIPTR                      ! Pointer to window array
      INTEGER WPHPTR                     ! Pointer to PHASE window array
      INTEGER RPPTR                      ! Pointer to RESIDUAL POWER array
      INTEGER RPHPTR                     ! Pointer to RESIDUAL PHASE array
      INTEGER SWPTR                      ! Pointer to Imaginary window array
      INTEGER CWPTR                      ! Pointer to Real window array
      INTEGER ICPTR                      ! Pointer to Imaginary cleaned array
      INTEGER RCPTR                      ! Pointer to Real cleaned array
      INTEGER IRPTR                      ! Pointer to Imaginary residual array
      INTEGER RRPTR                      ! Pointer to Real residuals
      INTEGER WK1PTR                     ! Pointer to workspace array
      INTEGER WK2PTR                     ! Pointer to workspace array
      INTEGER WK3PTR                     ! Pointer to workspace array
      INTEGER AVERFL                     ! Average subtraction
      INTEGER NOAVER                     ! 0 if average to be subtracted
      INTEGER MAXITE                     ! Max no. of iterations to try
      INTEGER KNUM,KSPEC                 ! Used in calc'ing no. of freq. els.
      INTEGER NPOI                       ! No of frequencies
      INTEGER LOOP, LOOP2, FILFLG
      INTEGER LGXFL,LGYFL                ! Log X and Y ?
      INTEGER SMOOTH                     ! Smooth type (0=gaussian)
*
      LOGICAL LVAR                       ! Are variances present in datafile ?
      LOGICAL AVSUB                      ! Subtract the mean first ?
      LOGICAL LCHANGE                    ! Change any properties ?

      CHARACTER       FILE1*30, FILE2*30, CHAINT(0:9),
     &                TITLE*80, XAXIS*72, YAXIS*72, ZAXIS*72,
     &                FILTIT*6

      INTEGER			IFID, OFID		! Dataset identifiers

      DOUBLEPRECISION GAIN, FREINT,  A

      REAL CLNLIM                              ! Noise limit to clean to.
      REAL AMINFR,AMAXFR                       ! Min and max freqs.
      REAL SPIKFC                              ! Used in maxite calc.
      REAL FMIN,FMAX,FINC                      ! Min, max and inc. freqs.
      REAL XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX       ! Plotting limits
      REAL VAR0                                ! Extra weight component
      REAL            TIMRES, TIMDIF, TMAX, TMIN,
     &                RPOI, RMIN,
     &                ACRES,
     &                POWMAX(NBIN), POWMIN(NBIN), SUMBIN(NBIN),
     &                FREMAX(NBIN), FREMIN(NBIN), AVEFRE(NBIN),
     &                VARBIN(NBIN), BINXWD, MINFRX,
     &                HPBW, HPBWR, NPHPBW, TWOMOM, FORMOM, FREDIF

      DOUBLEPRECISION
     &                TAVE1, WSUM1, TAVE2, WSUM2

      INTEGER         NOPRT,  LGFXL, LGFYL
*
      REAL            MAXFR, MINFR, FREST
*
*    Local data :
      DATA CHAINT/'0','1','2','3','4','5','6','7','8','9'/
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'CLEANPOW  version 1.8-0')
*-
      IF (STATUS .NE. SAI__OK) RETURN

*  Initialise
      CALL MSG_PRNT(VERSION)
      CALL AST_INIT()

*  Get input data from file
      CALL TIM_GETDAT( IFID, NTOT, NGOOD, TPNTR, DPNTR, LVAR,
     :                                        VPNTR, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Set data format flag depending on the presence of variance
      IF (LVAR) THEN
*
         FILFLG = 3
*
*   Ask for extra weighting factor. Weights are defined as
*   W = 1.0 / (variance + var0)
*
         CALL USI_GET0R('VAR0', VAR0, STATUS)
*
      ELSE
         FILFLG = 0
      ENDIF
*
** Chop this section for now **
C        PRINT *,
C     +    'Do you want the axis of your plotted periodograms to be '
C        PRINT *,'Linear or logarithmic? Linear=0, logarithmic=1'
C        PRINT *,'You want to fiddle later=10 (for binning etc)'
C        PRINT *,'Enter frequency axis AND poweraxis'
C        READ *, LGXFL,LGYFL

* We shall plot linear graphs in this version of CLEAN. The files will
* be saved and so can be replotted in any way, off-line.
      LGXFL=0
      LGYFL=0
*
* Ask if average should be subtracted first.
      CALL USI_GET0L('AVSUB', AVSUB, STATUS)
*
      IF (AVSUB) THEN
         AVERFL=1
      ELSE
         AVERFL=0
      ENDIF
*
* Map a weights array
      CALL DYN_MAPR(1, NGOOD, WPNTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining dynamic memory')
         GOTO 99
      ENDIF
*
* Fill the weights array
      IF (LVAR) THEN
         CALL CLEANPOW_WEIGHTS(NGOOD, %val(VPNTR), VAR0, %val(WPNTR))
      ELSE
*
*   Set the weights to one if no variances in input file
         CALL ARR_INIT1R(1.0, NGOOD, %val(WPNTR), STATUS )
      ENDIF
*
****************************************************************
*************** Calculate now the relevant parameters, and give*
*************** the possibility to change them.                *
****************************************************************

*  Calculate the min and max times in the data
      CALL ARR_RANG1R(NGOOD, %val(TPNTR), TMIN, TMAX, STATUS )

*  Calc a rough time resolution
      TIMDIF = TMAX - TMIN
      TIMRES = TIMDIF / (NGOOD-1)
*
***********Maximum timescale --> HPBW
***********Minimum resolution --> FREST
*
      CALL CLEANPOW_SMOMENTS(%val(DPNTR), %val(WPNTR), NGOOD,
     &                                          2, TWOMOM, 0, 0)
      CALL CLEANPOW_SMOMENTS(%val(DPNTR), %val(WPNTR), NGOOD,
     &                                          4, FORMOM, 0, 0)
*
* Set a few values
      CLNLIM = 0.
      SMOOTH = 0
      HPBWR  = 1.12/TIMDIF
      NPHPBW = 8.0D0
      GAIN   = 0.5
      AMAXFR = 0.5/TIMRES
      AMINFR = 0.
      NOAVER = 1-AVERFL
      NOPRT  = 0
      SPIKFC = (1.-(FORMOM/((NGOOD-1.)*TWOMOM*TWOMOM)))
      MAXITE = INT((0.50/GAIN)*SQRT(10.*NGOOD)*(SPIKFC**2.))
      MAXITE = AMIN0(NCL,MAXITE)
*
      IF (MAXITE.EQ.0)THEN
         CLNLIM=1E10
         MAXITE=1
      ENDIF
*
* Give the user info. on parameters
      CALL MSG_SETD('GAIN', GAIN)
      CALL MSG_PRNT('GAIN = ^GAIN (allowable range: 0.001 - 1.0) ')
*
      CALL MSG_SETI('MAX', MAXITE)
      CALL MSG_PRNT('MAXITE = ^MAX (maximum allowable value: 9999)')
*
      CALL MSG_PRNT('The power of the highest residual is not '/
     &               /'specified explicitely')
*
      CALL MSG_SETR('AMAX', AMAXFR)
      CALL MSG_PRNT('Max freq. set to Nukvist freq. =^AMAX')
*
      CALL MSG_PRNT('The HPBW of your clean beam is 1.12/maximum '/
     +            /'time difference.')
      CALL MSG_SETR('HPB', HPBWR)
      CALL MSG_PRNT('In units of frequency this equals ^HPB')
*
      CALL MSG_SETR('NPH', NPHPBW)
      CALL MSG_PRNT('The formal resolution is ^NPH points per '/
     &             /'HPBW of clean beam (max=10.)')
*
      CALL MSG_SETR('CLN', CLNLIM)
      CALL MSG_PRNT('The noise limit for cleaning the spectrum '/
     &             /'to is ^CLN')
*
      CALL MSG_PRNT(' ')
      CALL MSG_PRNT(' ')
*
* Ask if any changes required
      CALL USI_GET0L('CHANGE', LCHANGE, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 99
*
* Set defaults and get new values if changes wanted
      IF (LCHANGE) THEN
*
         CALL USI_DEF0D('GAIN', GAIN, STATUS)
         CALL USI_GET0D('GAIN', GAIN, STATUS)
*
         CALL USI_DEF0I('MAXITE', MAXITE, STATUS)
         CALL USI_GET0I('MAXITE', MAXITE, STATUS)
*
         CALL USI_DEF0R('MAXFR', AMAXFR, STATUS)
         CALL USI_GET0R('MAXFR', AMAXFR, STATUS)
*
         CALL USI_DEF0R('HPBW', HPBWR, STATUS)
         CALL USI_GET0R('HPBW', HPBWR, STATUS)
*
         CALL USI_DEF0R('NPHPBW', NPHPBW, STATUS)
         CALL USI_GET0R('NPHPBW', NPHPBW, STATUS)
*
         CALL USI_DEF0R('CLNLIM', CLNLIM, STATUS)
         CALL USI_GET0R('CLNLIM', CLNLIM, STATUS)
*
         CALL MSG_PRNT('GAIN, AMAXFR, NPHPBW will be fixed, very minor')
         CALL MSG_PRNT('adjustments will be made later by the program'/
     &                /' to CLNLIM/MAXITE and HPBW')
      ENDIF
*
      IF (STATUS .NE. SAI__OK) GOTO 99
*
* Set some more values
      KNUM   = MIN0( INT(((AMAXFR-AMINFR)/HPBWR)*NPHPBW)+1, 6*NGOOD )
      KSPEC  = 2*KNUM+1
      FREST  = (AMAXFR-AMINFR)/REAL(KNUM)
      HPBW   = HPBWR/FREST
*
* Output info. to screen
      CALL MSG_SETR('HP', HPBW)
      CALL MSG_PRNT('HPBW=^HP unitsteps')
*
      CALL MSG_SETI('KS', KSPEC)
      CALL MSG_PRNT('Number of points in transform ^KS')
*
* Map some dynamic arrays
*   Frequency array (formerly nufreq)
      CALL DYN_MAPD(1, KSPEC+5, FPTR, STATUS)
*   Power array
      CALL DYN_MAPD(1, KSPEC+5, PPTR, STATUS)
*   Phase array
      CALL DYN_MAPD(1, KSPEC+5, PHPTR, STATUS)
*   Window array
      CALL DYN_MAPD(1, KSPEC+5, WIPTR, STATUS)
*   Window phase array
      CALL DYN_MAPD(1, KSPEC+5, WPHPTR, STATUS)
*   Real term of power
      CALL DYN_MAPD(1, KSPEC+5, RPPTR, STATUS)
*   Real term of Phase
      CALL DYN_MAPD(1, KSPEC+5, RPHPTR, STATUS)
*   Imaginary comonent of window
      CALL DYN_MAPD(1, KSPEC+5, SWPTR, STATUS)
*   Real component of window
      CALL DYN_MAPD(1, KSPEC+5, CWPTR, STATUS)
*   Imaginary component of clean spectrum
      CALL DYN_MAPD(1, KSPEC+5, ICPTR, STATUS)
*   Real component of clean spectrum
      CALL DYN_MAPD(1, KSPEC+5, RCPTR, STATUS)
*   Imaginary component of residual spectrum
      CALL DYN_MAPD(1, KSPEC+5, IRPTR, STATUS)
*   Real component of residual spectrum
      CALL DYN_MAPD(1, KSPEC+5, RRPTR, STATUS)
*   Work arrays
      CALL DYN_MAPD(1, 10000, WK1PTR, STATUS)
      CALL DYN_MAPD(1, 10000, WK2PTR, STATUS)
      CALL DYN_MAPD(1, 10000, WK3PTR, STATUS)
*
*   Also map some single precision arrays for later use
*    Frequency array
      CALL DYN_MAPR(1, KSPEC+5, SFPTR, STATUS)
*    Power array
      CALL DYN_MAPR(1, KSPEC+5, SPPTR, STATUS)
*    Phase array
      CALL DYN_MAPR(1, KSPEC+5, SPHPTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error mapping dynamic memory')
         GOTO 99
      ENDIF
*
* Call the routine that does the business
      CALL SCLNFT( %val(TPNTR), %val(DPNTR), %val(WPNTR), NGOOD,
     &         KSPEC, MAXITE, CLNLIM, SMOOTH, HPBW, GAIN, AMAXFR,
     &         AMINFR, FREST, NOAVER, NOPRT, %VAL(FPTR), %VAL(PPTR),
     &         %VAL(PHPTR), %VAL(WIPTR), %VAL(WPHPTR), %VAL(RPPTR),
     &         %VAL(RPHPTR), %VAL(SWPTR), %VAL(CWPTR), %VAL(IRPTR),
     &         %VAL(RRPTR), %VAL(ICPTR), %VAL(RCPTR), %VAL(WK1PTR),
     &         %VAL(WK2PTR), %VAL(WK3PTR) )
*
* Plot the results
* NB: Harry's original had lots of options about rebinning the power
* spectrum before replotting. I've got rid of all that.
        XMAX=+1.E20
        YMAX=+1.E20
        YMIN=-1.E20
        ZMAX=+1.E20
        ZMIN=1E-15
        XAXIS='FREQ'
C        IF(LGXFL.EQ.1)XMIN =0.5*NUFREQ(1)
        IF(LGYFL.EQ.0)XMIN=-1E20
        YAXIS='PHASE'
        ZAXIS='POWER'
        FILTIT='SPECT1'
        TITLE=
     :    'POWER SPECTRUM ( '//FILE1(:INDEX(FILE1,' '))//')'
     :     //' '//CHAINT(FILFLG)//'-'//CHAINT(AVERFL)//' '

*  Calc the number of points in periodogram
      NPOI = KNUM+1

*  Copy double precision results into single precision arrays
      CALL CLEANPOW_REWRITE(NPOI, %val(FPTR), %val(PPTR),
     :            %val(PHPTR), %val(SFPTR), %val(SPPTR), %val(SPHPTR))

*  Plot the output periodogram
      CALL PLTXYZ( %val(SFPTR), %val(SPHPTR), %val(SPPTR),
     :              NPOI, XAXIS, YAXIS, ZAXIS, TITLE, 0, 0, 0, 0,
     :              XMAX, XMIN, YMAX, YMIN, ZMAX, ZMIN)

*  Find the frequency base and increment
      CALL ARR_RANG1R(NPOI, %val(SFPTR), FMIN, FMAX, STATUS )
      FINC = (FMAX - FMIN) / REAL (NPOI-1)

*  Write the output file. Periodogram
      CALL TIM_PUTOUT( 'OUT', 'POWER_SPECTRUM', NPOI,
     :                  %val(SPPTR), FMIN, FINC, OFID, STATUS )

*  Add axis label
      CALL BDI_PUTAXTEXT(OFID, 1, 'Frequency', 'Hz', STATUS)

*  Add history
      CALL HSI_NEW(OFID, STATUS)
      CALL HSI_ADD(OFID, VERSION, STATUS)

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*
         SUBROUTINE SCLNFT(T,X,W,N,NN,MAXITE,CLNLIM,
     +              SMOOTH,WIDTH,GAIN,MAXFR,MINFR,FREQST,NOAVER,
     +              NOPRT,NUFREQ,
     +              POWER,PPHASE,WINDOW,WPHASE,
     +              RPOWER,RPHASE,DSINWN,DCOSWN,
     +              RSINTR,RCOSTR,DSINTR,DCOSTR,
     +              CLLOC,REC,IMC)

         INTEGER    N, NN, MAXITE, NOAVER, NOPRT,
     +              KMIN, KMAX, MAXBIN, DMAX, SMOOTH,
     +              ITECNT,ITEFLG, OVERFR, DIST1, DIST2

         REAL       T(N), X(N), W(N), CLNLIM, WIDTH, MAXFR, MINFR,
     +              FREQST, NFREQ, POWR, WIND0, WI, CNST,
     +              MAXFRE, TLRG, TSML, RK

         DOUBLEPRECISION  GAIN, FREQ, FRSTEP, FRSTP, ANGLE,
     +                    NUFREQ(0:NN+4),
     +                     POWER(0:NN+4), PPHASE(0:NN+4),
     +                    WINDOW(0:NN+4), WPHASE(0:NN+4),
     +                    RPOWER(0:NN+4), RPHASE(0:NN+4),
     +                    DSINWN(0:NN+4), DCOSWN(0:NN+4),
     +                    RSINTR(0:NN+4), RCOSTR(0:NN+4),
     +                    DSINTR(0:NN+4), DCOSTR(0:NN+4),
     +                    CLLOC(10000),
     +                    REC(10000), IMC(10000),
     +                    SUMT,SUMX, SUMW, AVET, AVEX, AVEW, NRW,
     +                    NRWSQ, NR, MAXF, MAXOLD, MXOLDR, WINDO2,
     +                    DSII, DCII, DSWI, DCWI, DSI, DCI, DELTS,
     +                    DELTC, DSTR, DCTR, DC1, DC2, WINFLG, DD1,
     +                    DD2, DS1, DS2, DCT, DST, DWI, DXI, DPOWR,
     +                    PI, TWOPI, LN2,
     +                    DDC1, DDS1, DNC1, DNC2, SUMNOM,
     +                    GAINRE, GAINIM

*                Version August 15, 1990.  Harry Lehto
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                           *
*    This program calculates a CLEANed power spectrum of a given timeseries.*
*    The parameters in the subroutine call are:                             *
*                                                                           *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                INPUTS                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*            T = Time (array)
*            X = Datapoints (array)
*            W = Weights (array)
*            N = Number of datapoints
*           NN = Number of frequencies to be calculated N*5 to 10 are
*                suitable for smooth enough curves
*        MAXITE= The maximum number of iterations.
*        CLNLIM= The noise limit to which the spectrum is cleaned.
*                If MAXITE = 0 AND CLNLIM = 0, then the limit is set
*                automatically. Otherwise the first of the two values
*                achieved is the cutoff point of CLEANing. Note that
*                if MAXITE = 0 AND CLNLIM >0, then no CLEANing is done.
*        SMOOTH= if 0, then the CLEAN beam is a gaussian
*                if 1, then the CLEAN beam is a boxcar (NOT COMPLETE)
*        WIDTH = If Smoothing is gaussian, then this is the Half
*                Power Beam Width of the CLEAN beam.
*                If smoothing is boxcar. Gives the width of the boxcar.
*                If set to zero, then a suitable WIDTH is chosen.
*         GAIN = The gain of the CLEAN loops, 0.1 to 0.5 are suitable.
*                If GAIN is <0 or >2, the gain is set to 0.5.
*        MAXFR = The maximum frequency for which the CLEAN spectrum is
*                to be calculated in units of inverse time unit.
*                The dirty spectrum will be calculated up to a frequency
*                twice this value.
*                The maximum value of MAXFR is set to the Nykvist frequency
*                if MAXFR = 0 is given
*        MINFR = The minimum frequency for which the CLEAN spectrum is
*                to be calculated . 0 is suitable. The dirty spectrum
*                will be calculated from zero, in any case.
*        FREQST= Frequency interval at which spectrum is calculated.
*                If set to zero, it will be chosen suitably using
*                other parameters.
*        NOAVER= if 0, an average is subtracted from the values of X.
*                if 1, the spectrum is taken without subtracting an average.
*         NOPRT= 3 = 2+1 (see below)
*                2 if you want the dirty spectrum written in a file DIRTSP.DAT
*                1 if you want the cleaned spectrum printed on your screen.
*                0 if you don't.
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                             OUTPUTS                                       *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*
*
*         POWER= The amplitude of CLEAN power in units of (x/2)**2.(array)
*        PPHASE= The phase of CLEAN power in degrees.(array)
*        WINDOW= The window function. Normalized to unity at zero.(array)
*        WPHASE= The phase of the window in degrees.(array)
*        RPOWER= The amplitude of the residual power, after all clean
*                components have been subtracted from the dirty
*                spectrum. (array)
*        RPHASE= The phase of the residual spectrum.(array)
*        NUFREQ= The frequency. (array)
*        DSINWN= The imaginary component of window.(array)
*        DCOSWN= The real component of window.(array)
*        DSINTR= The imaginary component of CLEAN spectrum.(array)
*        DCOSTR= The real component of CLEAN spectrum.(array)
*        RSINTR= The imaginary component of the residual spectrum.(array)
*        RCOSTR= The real component of the residual spectrum.(array)
*         CLCOM= The uncorrected amplitude of a CLEAN component.(array)
*         CLLOC= The bin of a CLEAN component.(array)
*           REC= The real component of a CLEAN component.(array)
*           IMC= The imaginary component of a CLEAN component.(array)
*
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                           *
* For a theoretical explanation see Roberts, et. al, A.J., 93, 968 (1987)   *
*                                                                           *
*                                   Clark, Astron. Astroph., 89, 377 (1980) *
*                                                                           *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


*      Define some numerical variables


         PI =  3.14159265358979323846D0
         TWOPI=6.28318530717958647692D0
         LN2 = .69314718055994530941D0
         NR = N


*******************************************************************************
******************* VARIOUS CHECKING PROCEDURES *******************************
*******************************************************************************


*
*      Check that the resolution is high enough.
*

        IF(NN.LE.(2*N)) THEN
           CALL MSG_PRNT('**************** WARNING *******************')
           CALL MSG_PRNT(' ')
           CALL MSG_PRNT('  Your resolution is pretty low.           ')
           CALL MSG_PRNT('  Please increase the value of NN          ')
           CALL MSG_PRNT('  (fourth variable in the subroutine call).')
           CALL MSG_PRNT('  We suggest a value 5 to 10 times N       ')
           CALL MSG_PRNT('  (third variable in the subroutine call). ')
           CALL MSG_PRNT('  Note the increased requirement for space ')
           CALL MSG_PRNT('  in array definitions.                    ')
        ENDIF
        IF(NN.GE.1.D6) THEN
           CALL MSG_PRNT('  We do not recommend that you use')
           CALL MSG_PRNT('  NN>10**6.                       ')
        ENDIF


*
*      Set the upperlimit to the maximum frequency. If the value given
*      is larger than twice the nykvist frequency, set the upperlimit
*      to this value.
*      The post nykvist frequencies are needed for CLEANing.
*
        TLRG = -1E20
        TSML = 1E20

        DO 30 I = 1,N
           TLRG=AMAX1(TLRG,T(I))
           TSML=AMIN1(TSML,T(I))
30      CONTINUE

        MAXFRE = NR/((TLRG-TSML)*2.)
        IF (MAXFR.NE.0) MAXFRE = MAXFR

*
*      Check that the value of the minimum frequency makes sense, if not
*      warn the user.
*
        IF (MINFR.LT.0) THEN
           CALL MSG_PRNT('  WARNING: Minimum frequency less than 0.'/
     +              /' Minimum frequency set to 0.')
           MINFR=0.
        ENDIF

        IF (MINFR.GT.MAXFRE) THEN
           CALL MSG_PRNT('  WARNING, Minimum frequency '/
     +             /'larger than the  maximum freqency.')
           CALL MSG_SETR('MIN', MINFR)
           CALL MSG_SETR('MAX', MAXFRE)
           CALL MSG_PRNT('MAXFRE=^MAX, MINFRE=^MIN')
        ENDIF
*
        IF (MINFR.EQ.MAXFRE) THEN
            CALL MSG_PRNT(' WARNING, Minimum frequency equal to '/
     +              /'maximum frequency.')
            GO TO 99
        ENDIF

*
*      Set absolute lower limit for the frequency interval
*

        FRSTEP = FREQST
        FRSTP = 2.*(MAXFRE - MINFR)/(NN-1)
        IF (FRSTEP.EQ.0) FRSTEP = FRSTP
        IF (FRSTEP.GT.FRSTP)
     +     CALL MSG_PRNT('  WARNING, Frequency steps very wide')

        IF (FRSTEP.LE.0) THEN
           CALL MSG_PRNT(' WARNING, Frequency step negative')
        ENDIF
*
*       Set Gains to a suitable value
*

        IF ((GAIN.LE.0.).OR.(GAIN.GT.(2.D0+1D-6))) THEN
*
           IF (GAIN.NE.0) THEN
              CALL MSG_SETD('GAIN', GAIN)
              CALL MSG_PRNT('  Gain was ^GAIN, now now to 0.5')
           ENDIF
*
           GAIN=5.D-1
        ENDIF

*
*       Check WIDTH and MAXITE. If required set them suitably
*

        IF (WIDTH.LE.0.0) THEN
           IF (SMOOTH.EQ.0) THEN
              WIDTH = 1.12/((TLRG-TSML)*FRSTP)
              CALL MSG_SETR('WID', WIDTH)
              CALL MSG_PRNT('HPBW set to ^WID, (frequency bins)')
           ELSEIF (SMOOTH.EQ.1) THEN
              WIDTH = 1.00/((TLRG-TSML)*FRSTP)
              CALL MSG_SETR('WID', WIDTH)
              CALL MSG_PRNT('BOXCAR width set to ^WID (frequency bins)')
           ENDIF
        ENDIF

        IF ((MAXITE.EQ.0).AND.(CLNLIM.EQ.0))
     +     MAXITE =(NN**.5+1)*0.5/GAIN


        IF(MAXITE.GT.10000) THEN
           CALL MSG_SETI('MAX', MAXITE)
           CALL MSG_PRNT(' MAXITE too large (^MAX). Reset to 10000.')
           MAXITE = 10000
        ENDIF

*************************************************************************
****************** CALCULATE THE DIRTY SPECTRUM *************************
*************************************************************************


        KMIN  = MINFR/FRSTEP
        KMAX  = KMIN + 1 + (MAXFRE-MINFR)/(FRSTEP)
        DMAX  = 2*(KMAX+1)
        IF (DMAX.GT.NN+4) THEN
           DMAX=NN+4
           KMAX=(DMAX/2)-1
           CALL MSG_SETI('DMAX', DMAX)
           CALL MSG_PRNT(' Number of points in transform reduced '/
     &                  /'to ^DMAX ')
        ENDIF
*
*       Calculate the average time
*

        SUMT = 0
        SUMW = 0
        SUMX = 0

        DO  70 I = 1,N
           WI   = W(I)
           SUMT = WI*T(I)+SUMT
           SUMW = WI+SUMW
           SUMX = WI*X(I)+SUMX

 70     CONTINUE

           AVET = SUMT/SUMW
           AVEX = SUMX/SUMW
           NRW  = SUMW
           NRWSQ= NRW*NRW
*
           CALL MSG_PRNT(' ')
           CALL MSG_SETD('SUMT', SUMT)
           CALL MSG_PRNT('  SUM      OF      TIMES  = ^SUMT')
           CALL MSG_SETD('SUMW', SUMW)
           CALL MSG_PRNT('  SUM      OF      WEIGHTS = ^SUMW')
           CALL MSG_SETD('SUMX', SUMX)
           CALL MSG_PRNT('  SUM      OF      VALUES  = ^SUMX')
           CALL MSG_SETD('AVET', AVET)
           CALL MSG_PRNT('AVERAGE WEIGHTED TIME = ^AVET')
           CALL MSG_SETD('AVEX', AVEX)
           CALL MSG_PRNT('AVERAGE WEIGHTED VALUE = ^AVEX')
*
           IF (NOAVER.EQ.0) THEN
              CALL MSG_PRNT(' ')
              DO 76 I = 1,N
                 X(I)= X(I)-AVEX
 76           CONTINUE
              CALL MSG_PRNT(' AVERAGE HAS BEEN SUBTRACTED FROM ALL'/
     +               /' DATA POINTS')
           ELSE
              CALL MSG_PRNT(' AVERAGE HAS **NOT** BEEN SUBTRACTED FROM'/
     +                 /' DATA POINTS')
           ENDIF



           DO 80 K = 0,DMAX
              DSINTR(K)=0
              DCOSTR(K)=0
              DSINWN(K)=0
              DCOSWN(K)=0
 80        CONTINUE


*      Calculate the powerspectrum for equally spaced frequencies
*      using the equation of sine ( or cosine) of the sum of angles
*      to obtain ths sine ( or cosine) terms in Deemings equations.
*

           DO 100 I=1,N

              ANGLE = FRSTEP*(T(I)-AVET)*TWOPI
              DWI   = DBLE(W(I))
              DXI   = DBLE(X(I))
              DELTS = DSIN(ANGLE)
              DELTC = DCOS(ANGLE)

              DSII = 0
              DCII = DWI*DXI
              DSWI = 0
              DCWI = DWI


*
*      In Deeming one calculates the complex transform of the points at
*      a given frequency. We will instead calculate the contrbution of
*      each point to all equally spaced frequencies, and sum up those
*      complex terms. The same result is achieved, by this different
*      computational approach
*

              DO 90 K = 0, DMAX

                 DSINTR(K) = DSII + DSINTR(K)
                 DCOSTR(K) = DCII + DCOSTR(K)
                 DSINWN(K) = DSWI + DSINWN(K)
                 DCOSWN(K) = DCWI + DCOSWN(K)

                       DSI = DSII
                       DSW = DSWI

                      DSII = DELTC*DSI  + DELTS*DCII
                      DCII = DELTC*DCII - DELTS*DSI
                      DSWI = DELTC*DSW  + DELTS*DCWI
 90                   DCWI = DELTC*DCWI - DELTS*DSW
 100       CONTINUE

           CALL MSG_PRNT('  DIRTY SPECTRUM CALCULATED')

C           IF (NOPRT.GE.2) THEN
C              OPEN (UNIT=14, FILE='DIRTSP.DAT', STATUS='UNKNOWN')
C              WRITE (14,1010)
C1010          FORMAT ('    BIN     FREQUENCY     POWER     SINCOMP
C     +    COSCOMP   WINDOW')
C              DO 1012 K=0,DMAX
C                 POWR=(DSINTR(K)**2.+DCOSTR(K)**2.)/NRWSQ
C                 WINDO=(DSINWN(K)**2.+DCOSWN(K)**2.)/NRWSQ
C                 NFREQ=MINFR+K*FRSTEP
C                 WRITE (14,1011) K,NFREQ,POWR,DSINTR(K),
C     +                                       DCOSTR(K),WINDO
C1011             FORMAT(1X,I6,5(2X,E10.4))
C1012          CONTINUE
C              CLOSE (14)
C              NOPRT=NOPRT-2
C           ENDIF
********************************************************************************
**************************** START CLEANING ************************************

*      Initialize  CLEAN, choose the criterion for MAXITE (maximum number
*                         of iterations) if not specified in the subroutine
*                         call
*                         choose an appropriate size for the CLEAN Gaussian
*                         beam if not specified in the subroutine call
*





           ITEFLG = 0
           I = 0
           ITECNT = 0


*
*       Start CLEANing, clean MAXITE times
*

           CALL MSG_PRNT('  START CLEANING.')
*
           GNCONV=0.5*DSQRT(GAIN**2.D0+DEXP(LN2*(1./WIDTH)**2.))
           MAXOLD=1.E15
           MXOLDR=(1.+1.1*GNCONV)*MAXOLD
           DO 300 WHILE (ITEFLG.LT..5)

              I = I+1
              MAXF = 0

*
*           Find the maximum of power in this round of cleaning
*
              DO 101 II = 0, KMAX
                 DPOWR=(DCOSTR(II)**2.D0+DSINTR(II)**2.D0)/NRWSQ
                 IF (DPOWR.GT.MAXF) THEN
                    MAXBIN = II
                    MAXF = DPOWR
                 ENDIF
  101         CONTINUE

*             PRINT*,I,MAXBIN,DPOWR,DCOSTR(MAXBIN),DSINTR(MAXBIN)
*
*          Store the location of the CLEAN component and calculate the
*          real and imaginary parts of the CLEAN component corrected
*          for the effects of the window (aliasing)
*

              CLLOC(I) = MAXBIN
              IF (((MAXF/(1.+GNCONV)).GE.MAXOLD)
     +         .AND.((MAXOLD/(1.+GNCONV)).GE.MXOLDR)) THEN
               ITEFLG = 1
               PRINT *,'***********!!!!!!!!!!!!!!!!!!!!!***********'
               PRINT *,'!              W A R N I N G              !'
               PRINT *,'!        ITERATION GOING UNSTABLE         !'
               PRINT *,'! The power of the CLEANEd component has  !'
               PRINT *,'! increased by a factor greater or equal  !'
               PRINT *,'!               to (1+GAIN)               !'
               PRINT *,'! CLEAN ITERATIONS CUT OFF AUTOMATICALLY  !'
               PRINT *,'!-----------------------------------------!'
               PRINT *,'!  The CLEAN iteration, the maximum power !'
               PRINT *,'!  in the respective residual spectra and !'
               PRINT *,'!  the respective bins were               !'
               PRINT *,'!', I-2,MXOLDR,CLLOC(I-2),'!'
               PRINT *,'!', I-1,MAXOLD,CLLOC(I-1),'!'
               PRINT *,'!', I,MAXF,CLLOC(I),'!'
               PRINT *,'--------------------------------------------'
              ENDIF
              IF((ITECNT.GE.MAXITE).OR.(I.GE.10000).
     +          OR.(MAXF.LT.CLNLIM)) ITEFLG = 1
              IF(ITEFLG.GT.0.5) GO TO 300
              IF(MAXBIN.GE.(4*(NN/NR)))ITECNT = ITECNT+1
              MXOLDR=MAXOLD
              MAXOLD=MAXF
              DNC1 = 0
              DNC2 = 0
              SUMNOM=0
              DCT  = DCOSTR(MAXBIN)
              DST  = DSINTR(MAXBIN)


              DO 120  II = 1, KMAX

                 DIST1 = ABS(II-MAXBIN)
                 DIST2 = ABS(II+MAXBIN)
                 DCTR = DCOSTR(II)
                 DSTR = DSINTR(II)

                 IF (II.GE.MAXBIN) THEN
                    WINFLG = 1.D0
                 ELSE
                    WINFLG = -1.D0
                 ENDIF

                 DC1 = DCOSWN(DIST1)
                 DC2 = DCOSWN(DIST2)
                 DS1 = WINFLG*DSINWN(DIST1)
                 DS2 = DSINWN(DIST2)
                 DDC1=(DC1+DC2)*DCT+(DS2-DS1)*DST
                 DDS1=(DC1-DC2)*DST+(DS1+DS2)*DCT


                 DNC1=DNC1+2.D0*(DCTR*DDC1+DSTR*DDS1)
                 DNC2=0
                 SUMNOM=SUMNOM+2.D0*(DDC1*DDC1+DDS1*DDS1)

  120         CONTINUE

              DC1 = DCOSWN(MAXBIN)
              DS1 = DSINWN(MAXBIN)
              DDC1=2.D0*(DC1*DCT+DS1*DST)

              DNC1=DNC1+DCOSTR(0)*DDC1
              SUMNOM=SUMNOM+DDC1*DDC1

              GAINRE=GAIN*DNC1*NRW/SUMNOM

              WINDO2= (DSINWN(2*MAXBIN)**2.D0+
     +                 DCOSWN(2*MAXBIN)**2.D0)/NRWSQ

              IF ((1.D0-WINDO2).GT.1E-3) THEN

                 REC(I)=GAINRE*DCT
                 IMC(I)=GAINRE*DST

              ELSEIF (ABS(MAXBIN).LE..5) THEN

                 REC(I)= GAINRE*DCOSTR(0)
                 IMC(I)= GAINRE*DSINTR(0)

              ENDIF

*
*           CLEAN this component out, and calculate the residual
*           spectrum. At line 300 return to the start of do loop,
*           and procede with the next component
*

              DO 200  II = 0, KMAX

                 DIST1 = ABS(II-MAXBIN)
                 DIST2 = ABS(II+MAXBIN)
                 DCTR = DCOSTR(II)
                 DSTR = DSINTR(II)

                 IF (II.GE.MAXBIN) THEN
                    WINFLG = 1.D0
                 ELSE
                    WINFLG = -1.D0
                 ENDIF

                 DC1 = DCOSWN(DIST1)
                 DC2 = DCOSWN(DIST2)
                 DS1 = WINFLG*DSINWN(DIST1)
                 DS2 = DSINWN(DIST2)

                 DCOSTR(II) = DCTR-(REC(I)*(DC1+DC2)-
     +                              IMC(I)*(DS1-DS2))/NRW

                 DSINTR(II) = DSTR-(REC(I)*(DS1+DS2)+
     +                              IMC(I)*(DC1-DC2))/NRW



  200         CONTINUE



  300      CONTINUE
           CLOSE(18)
           CLNDEP=MAXF
           NUMCLN = I-1

           CALL MSG_SETI('NUM', NUMCLN)
           CALL MSG_SETI('ITE', ITECNT)
           CALL MSG_PRNT('  Number of CLEAN components =^NUM'/
     &         /' of which ^ITE were after the first 3.6 WIDTHs.')
           CALL MSG_SETI('CLE', CLNDEP)
           CALL MSG_PRNT('The maximum power in the last residual '/
     &             /'spectrum CLEANed ^CLE')

* Let us now save the residual spectrum in RCOSTR and RSINTR arrays

           DO 121 II = 0,KMAX
              RCOSTR(II)=DCOSTR(II)
 121       CONTINUE
           DO 122 II = 0,KMAX
              RSINTR(II)=DSINTR(II)
 122       CONTINUE

*
*            Knowing the CLEAN components and the residual spectrum
*            and the WIDTH of the CLEAN beam, we may calculate the
*            CLEAN real and imaginary components
*

*            Rearrange the CLEAN components
           DO 131 II = NUMCLN,2, -1
              IF ((REC(II).NE.0).AND.(IMC(II).NE.0)) THEN
                 DO 132 JJ = II-1, 1, -1
                    IF(CLLOC(II).EQ.CLLOC(JJ))THEN
                       REC(JJ)=REC(II)+REC(JJ)
                       IMC(JJ)=IMC(II)+IMC(JJ)
                       REC(II)=0
                       IMC(II)=0
                    ENDIF
132              CONTINUE
              ENDIF

131        CONTINUE

           IF (SMOOTH.EQ.0) THEN
              CNST=-LN2/((WIDTH/2.)**2.)
              DO 500  I = NUMCLN,1,-1
                 IF ((REC(I).NE.0).AND.(IMC(I).NE.0))THEN
                    MINII=AMAX0(NINT(CLLOC(I))-12*NINT(WIDTH),0)
                    MAXII=AMIN0(NINT(CLLOC(I))+12*NINT(WIDTH),KMAX)
                    DO 400 II = MINII,MAXII
                       DD1 = DEXP(((II - CLLOC(I))**2.)*CNST)
                       DD2 = DEXP(((II + CLLOC(I))**2.)*CNST)
                       DCOSTR(II)= DCOSTR(II)+(REC(I)*(DD1+DD2))
                       DSINTR(II)= DSINTR(II)+(IMC(I)*(DD1-DD2))
  400               CONTINUE
                 ENDIF
  500        CONTINUE
           ELSEIF (SMOOTH.EQ.1) THEN

              DO 502 I = 1,NUMCLN
                 DO 402
     +      II =NINT(CLLOC(I)-(WIDTH/2.)),NINT(CLLOC(I)+(WIDTH/2.))
                    DCOSTR(II)= DCOSTR(II)+REC(I)
                    DSINTR(II)= DSINTR(II)+IMC(I)

  402            CONTINUE
  502         CONTINUE
           ENDIF

*
*       Calculate now the CLEAN power, the phase and the window
*       and its phase for all frequencies.
*
           IF (NOPRT.EQ.1) THEN
              WRITE (*,*)
              WRITE (*,*)
              WRITE (*,38)
 38         FORMAT (1X,' Just to remind you. The ''power'' for strong
     +      noiseless sinecurves is equal to (0.5*ampl)**2.')


              WRITE (*,39)
 39         FORMAT (1X,'     FREQ       POWER       PPHASE       WINDOW
     +      WPHASE       AMPL OF SIN')

            ENDIF

            CALL MSG_PRNT('  CLEANed SPECTRUM  HAS BEEN CALCULATED')

            DO 110 K = KMIN,KMAX

               POWER(K) = (DSINTR(K)**2.+DCOSTR(K)**2.)/NRWSQ
               WINDOW(K)= (DSINWN(K)**2.+DCOSWN(K)**2.)/NRWSQ
               RPOWER(K)= (RSINTR(K)**2.+RCOSTR(K)**2.)/NRWSQ
*
*          In most cases the calculation of the phases of both the
*          power and the window is straightforward. If one of the
*          components (real or imaginary or both) goes to zero
*          the actual   atan equation will fall apart. Those cases
*          are all considered separately below. The order of testing
*          was chosen to keep the actual phase curves continuous in
*          each case.
*




               IF (POWER(K).GT.0) THEN
                IF (ABS(DCOSTR(K)/(NRW*DSQRT(POWER(K)))).GT..000001)THEN
                PPHASE(K)= (1.8D2/PI)*DATAN(DSINTR(K)/DCOSTR(K))
                 IF(DCOSTR(K).LT.0.) THEN
                  IF(DSINTR(K).GE.0.) PPHASE(K)=PPHASE(K)+1.8D2
                  IF(DSINTR(K).LT.0.) PPHASE(K)=PPHASE(K)-1.8D2
                 ENDIF
                ELSE
                IF(DSINTR(K).GT.0) PPHASE(K) = 9.D1
                IF(DCOSTR(K).LT.0) PPHASE(K) =-9.D1
               ENDIF
              ELSE
               IF(DSINTR(K+1).GT.0) PPHASE(K) = 9.D1
               IF(DSINTR(K+1).LT.0) PPHASE(K) =-9.D1
               IF((DSINTR(K+1).EQ.0).AND.(DCOSTR(K+1).GE.0))PPHASE(K)=
     +                                                        0.
               IF((DSINTR(K+1).EQ.0).AND.(DCOSTR(K+1).LT.0))PPHASE(K)=
     +                                                        1.8D2
              ENDIF

              IF(RPOWER(K).GT.0) THEN
               IF(ABS(RCOSTR(K)/(NRW*DSQRT(RPOWER(K)))).GT..000001)THEN
                RPHASE(K)= (1.8D2/PI)*DATAN(RSINTR(K)/RCOSTR(K))
                IF(RCOSTR(K).LT.0) THEN
                 IF(RSINTR(K).GE.0.) RPHASE(K)=RPHASE(K)+1.8D2
                 IF(RSINTR(K).LT.0.) RPHASE(K)=RPHASE(K)-1.8D2
                ENDIF
               ELSE
                IF(RSINTR(K).GT.0) RPHASE(K) = 9.D1
                IF(RCOSTR(K).LT.0) RPHASE(K) =-9.D1
               ENDIF
              ELSE
               IF(RSINTR(K+1).GT.0) RPHASE(K) = 9.D1
               IF(RSINTR(K+1).LT.0) RPHASE(K) =-9.D1
               IF((RSINTR(K+1).EQ.0).AND.(RCOSTR(K+1).GE.0))RPHASE(K)=
     +                                                        0.
               IF((RSINTR(K+1).EQ.0).AND.(RCOSTR(K+1).LT.0))RPHASE(K)=
     +                                                        1.8D2
              ENDIF


              IF(WINDOW(K).GT.0) THEN
               IF(ABS(DCOSWN(K)/(NRW*DSQRT(WINDOW(K)))).GT..000001)THEN
                WPHASE(K)= (1.8D2/PI)*DATAN(DSINWN(K)/DCOSWN(K))
                IF (DCOSWN(K).LT.0) THEN
                 IF(DSINWN(K).GE.0.) WPHASE(K)=WPHASE(K)+1.8D2
                 IF(DSINWN(K).LT.0.) WPHASE(K)=WPHASE(K)-1.8D2
                ENDIF
               ELSE
                IF(DSINWN(K).GT.0) WPHASE(K) = 9.D1
                IF(DCOSWN(K).LT.0) WPHASE(K) =-9.D1
               ENDIF
              ELSE
               IF(DCOSWN(K+1).GT.0) WPHASE(K)=0.0
               IF(DCOSWN(K+1).LT.0) WPHASE(K)=1.8D2
               IF((DCOSWN(K+1).EQ.0).AND.(DSINWN(K+1).GT.0))WPHASE(K)=
     +                                                              9.D1
               IF((DCOSWN(K+1).EQ.0).AND.(DSINWN(K+1).LT.0))WPHASE(K)=
     +                                                             -9.D1
               IF((DCOSWN(K+1).EQ.0).AND.(DSINWN(K+1).EQ.0))WPHASE(K)=0
              ENDIF

*
*         In case you want to obtain amplitude instead of power, it is
*         also calculated here.
*
              RK       = K
              AMPL     = 2.*(POWER(K)**.5)
              NUFREQ(K)= (MINFR+RK*FRSTEP)

              IF(NOPRT.EQ.1) THEN

                 WRITE (*,40) NUFREQ(K),POWER(K),PPHASE(K),
     +                    WINDOW(K),WPHASE(K),AMPL
 40              FORMAT (1X, 2E12.4, F8.1,6X, F9.6, F10.1,8X, E10.4 )
              ENDIF

 110       CONTINUE
 99     RETURN
        END
*
********************************************************************
       SUBROUTINE CLEANPOW_SMOMENTS(X, W, N, DEG, MOM, CENFLG, CENTER)

*      Subroutine MOMENTS calculates the moments of the distribution
*      function of the discrete variable X.

       INTEGER N, DEG, CENFLG

       REAL X(N), W(N), MOM, CENTER

       DOUBLEPRECISION SUMX, SUMW, CENTR
*
*      Inputs:  X(N)= Array of data points
*               W(N)= Array of weights
*               N   = Number of datapoints
*               DEG = Degree of the moment to be calculated
*
*      Outputs: MOM = Value of moment
*               CENFLG = 0 if central moment should be calculated
*                        1 if a non central moment in respect to CENTER
*                          is to be calculated
*
             SUMW=0
          IF (CENFLG.EQ.0) THEN

             SUMX=0
             DO 10 I = 1,N
                SUMX=SUMX+X(I)*W(I)
                SUMW=SUMW+W(I)
10           CONTINUE
             CENTR= SUMX/SUMW

          ELSE

             CENTR=CENTER
             DO 15 I = 1,N
               SUMW=SUMW+W(I)
15           CONTINUE

          ENDIF

          SUMX = 0
          DO 20 I = 1,N
             SUMX=SUMX+W(I)*(X(I)-CENTR)**DEG
20        CONTINUE

          MOM=SUMX/SUMW


          RETURN
          END
          SUBROUTINE PLTXYZ(X,Y,Z,N,XTITLE,YTITLE,ZTITLE,TITLE,
     +         LGFLGX,LGFLGY,LGFLGZ,
     +       SCLFLG,ABSMAX,ABSMIX,ABSMAY,ABSMIY,ABSMAZ,ABSMIZ)

          INTEGER N, LGFLGX, LGFLGY, LGFLGZ, SCLFLG

          REAL X(N), Y(N), Z(N),
     +         ABSMAX, ABSMIX, ABSMAY, ABSMIY, ABSMAZ, ABSMIZ,
     +         AXMAX, AXMIN, AYMAX, AYMIN, AZMAX, AZMIN


          CHARACTER*72 XTITLE, YTITLE, ZTITLE, TITLE



*         Plot your results with PGPLOT
           IF (SCLFLG.EQ.0) THEN
              XMAX = -1.E20
              XMIN = 1.E20
              YMAX = -1.E20
              YMIN = 1.E20
              ZMAX = -1.E20
              ZMIN = 1.E20
           ENDIF
              AXMAX = ABSMAX
              AXMIN = ABSMIX
              AYMAX = ABSMAY
              AYMIN = ABSMIY
              AZMAX = ABSMAZ
              AZMIN = ABSMIZ
           IF (LGFLGX.EQ.1) THEN
              DO 10 I = 1,N
                IF(X(I).LE.0.)THEN
                       CALL MSG_PRNT('*********WARNING***********')
                       CALL MSG_SETI('I',I)
                       CALL MSG_SETR('DATA',X(I))
                       CALL MSG_PRNT('X(^I)=^DATA')
                       X(I)=AXMIN
                ENDIF
                  X(I)=ALOG10(X(I))
 10           CONTINUE
           AXMIN=ALOG10(AXMIN)
           ENDIF
           IF (LGFLGY.EQ.1) THEN
              DO 20 I = 1,N
                IF(Y(I).LE.0.)THEN
                       CALL MSG_PRNT('*********WARNING***********')
                       CALL MSG_SETI('I',I)
                       CALL MSG_SETR('DATA',Y(I))
                       CALL MSG_PRNT('Y(^I)=^DATA')
                       Y(I)=AYMIN
                ENDIF
                  Y(I)=ALOG10(Y(I))
 20           CONTINUE
           AYMIN=ALOG10(AYMIN)
           ENDIF
           IF (LGFLGZ.EQ.1) THEN
              DO 30 I = 1,N
                IF(Z(I).LE.0.)THEN
                       CALL MSG_PRNT('*********WARNING***********')
                       CALL MSG_SETI('I',I)
                       CALL MSG_SETR('DATA',Z(I))
                       CALL MSG_PRNT('Z(^I)=^DATA')
                       Z(I)=AZMIN
                ENDIF
                  Z(I)=ALOG10(Z(I))
 30           CONTINUE
           AZMIN=ALOG10(AZMIN)
           ENDIF
           IF (SCLFLG.EQ.0)THEN
              DO 40 I = 1,N
                  XMAX = AMAX1(X(I),XMAX)
                  XMIN = AMIN1(X(I),XMIN)
                  YMAX = AMAX1(Y(I),YMAX)
                  YMIN = AMIN1(Y(I),YMIN)
                  ZMAX = AMAX1(Z(I),ZMAX)
                  ZMIN = AMIN1(Z(I),ZMIN)
 40           CONTINUE
              AXMAX = AMIN1(XMAX,AXMAX)
              AXMIN = AMAX1(XMIN,AXMIN)
              AYMAX = AMIN1(YMAX,AYMAX)
              AYMIN = AMAX1(YMIN,AYMIN)
              AZMAX = AMIN1(ZMAX,AZMAX)
              AZMIN = AMAX1(ZMIN,AZMIN)

           ENDIF

           XMAX = AXMAX+(AXMAX-AXMIX)*.002
           XMIN = AXMIN-(AXMAX-AXMIN)*.002
           YMAX = AYMAX+(AYMAX-AYMIN)*.002
           YMIN = AYMIN-(AYMAX-AYMIN)*.002
           ZMAX = AZMAX+(AZMAX-AZMIN)*.002
           ZMIN = AZMIN-(AZMAX-AZMIN)*.002
D             print *,xmax,xmin,ymax,ymin,zmax,zmin

           CALL PGBEGIN(0,'?',1,1)
           CALL PGPAPER(10,1.4142)

           CALL PGADVANCE
           CALL PGVPORT(0.1,0.9,.515,.9)
           IF((XMIN.NE.XMAX).AND.(ZMIN.NE.ZMAX)) THEN
              CALL PGWINDOW(XMIN, XMAX, ZMIN, ZMAX)
              CALL PGBOX('BCNST', 0.0, 0, 'BCNST', 0.0, 0)

              CALL PGLABEL(' ',ZTITLE,
     +               TITLE)
              CALL PGLINE(N, X, Z)
           ELSE
              CALL PGLABEL('ZTITLE',' ',' ')
           ENDIF
           CALL PGVPORT(0.1,0.9,.1,.485)
           IF((XMIN.NE.XMAX).AND.(YMIN.NE.YMAX)) THEN
              CALL PGWINDOW (XMIN, XMAX, YMIN, YMAX)
              CALL PGBOX('BCNST', 0.0, 0, 'BCNST', 0.0, 0)

              CALL PGLABEL(XTITLE,YTITLE,
     +              ' ')

              CALL PGPOINT(N, X, Y, 1)
          ELSE
              CALL PGLABEL(' ',' ',XTITLE)
           ENDIF
           CALL PGEND

             IF (XMIN.EQ.XMAX) THEN
                CALL MSG_SETR('X',XMIN)
                CALL MSG_PRNT('X NOT PLOTTED. XMIN=XMAX= ^X')
             ENDIF
             IF (YMIN.EQ.YMAX) THEN
                CALL MSG_SETR('Y',YMIN)
                CALL MSG_PRNT('Y NOT PLOTTED. YMIN=YMAX= ^Y')
             ENDIF
             IF (ZMIN.EQ.ZMAX) THEN
                CALL MSG_SETR('Z',ZMIN)
                CALL MSG_PRNT('Z NOT PLOTTED. ZMIN=ZMAX= ^Z')
             ENDIF

         RETURN
         END

          SUBROUTINE PLTCRS(N,XMI,XMA,X,YMI,YMA,Y,XTITLE,YTITLE,TITLE)

          INTEGER N

          REAL XMI(N), XMA(N),X(N), YMI(N), YMA(N), Y(N)

          CHARACTER*72 XTITLE, YTITLE, TITLE*80

*         Plot your results with PGPLOT
           IF (SCLFLG.EQ.0) THEN
              XMAX = -1.E20
              XMIN = 1.E20
              YMAX = -1.E20
              YMIN = 1.E20
           ENDIF
              AXMAX = 1E20
              AXMIN = -1E20
              AYMAX = 1E20
              AYMIN = -1E15
              DO 40 I = 1,N
                  IF(XMA(I).LT.AXMAX) XMAX = AMAX1(XMA(I),XMAX)
                  IF(XMI(I).GT.AXMIN) XMIN = AMIN1(XMI(I),XMIN)
                  IF(YMA(I).LT.AYMAX) YMAX = AMAX1(YMA(I),YMAX)
                  IF(YMI(I).GT.AYMIN) YMIN = AMIN1(YMI(I),YMIN)
 40           CONTINUE
              AXMAX = AMIN1(XMAX,AXMAX)
              AXMIN = AMAX1(XMIN,AXMIN)
              AYMAX = AMIN1(YMAX,AYMAX)
              AYMIN = AMAX1(YMIN,AYMIN)

           XMAX = AXMAX+(AXMAX-AXMIX)*.005+0.5*(ABS(X(2)-X(1)))
           XMIN = AXMIN-(AXMAX-AXMIN)*.005-0.5*(ABS(X(2)-X(1)))
           YMAX = AYMAX+(AYMAX-AYMIN)*.005
           YMIN = AYMIN-(AYMAX-AYMIN)*.005
           SCALEC=(XMAX-XMIN)/1.4860
           IF (SCALEC.GT.(YMAX-YMIN)) THEN
                 YCOR=(SCALEC-(YMAX-YMIN))/2.
                 YMAX=YMAX+YCOR
                 YMIN=YMIN-YCOR
           ELSE
                 XCOR=((YMAX-YMIN)*1.4860-(XMAX-XMIN))/2.
                 XMAX=XMAX+XCOR
                 XMIN=XMIN-XCOR
           ENDIF

D             print *,xmax,xmin,ymax,ymin

           CALL PGBEGIN(0,'?',1,1)
           CALL PGPAPER(10,1.4142)
           CALL PGVPORT(0.1,0.9,.515,.9)
           IF((XMIN.NE.XMAX).AND.(YMIN.NE.YMAX)) THEN
              CALL PGWINDOW (XMIN, XMAX, YMIN, YMAX)
              CALL PGBOX('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
              CALL PGLABEL(XTITLE,YTITLE,TITLE)
              CALL PGERRX(N,XMI,XMA,Y,0)
              CALL PGERRY(N,X,YMA,YMI,0)
          ELSE
              CALL PGLABEL(' ',' ',XTITLE)
           ENDIF
           CALL PGEND

             IF (XMIN.EQ.XMAX) THEN
                CALL MSG_SETR('X',XMIN)
                CALL MSG_PRNT('X NOT PLOTTED. XMIN=XMAX= ^X')
             ENDIF
             IF (YMIN.EQ.YMAX) THEN
                CALL MSG_SETR('Y',YMIN)
                CALL MSG_PRNT('Y NOT PLOTTED. YMIN=YMAX= ^Y')
             ENDIF

         RETURN
         END
*
*******************************************************************
*+CLEANPOW_WEIGHTS  -  Calculates weights for clean prog.
      SUBROUTINE CLEANPOW_WEIGHTS(NGOOD, VAR, VAR0, WEIGHT)
*    Description :
*     Calculates a weight for each data point using the recipe:
*
*           W(i) = 1.0 / (VAR(i) + VAR0)
*
*     If (var + var0) is zero or negative for any data point. All
*     weights are set to one.
*    History :
*     7-Jun-1991     original   (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER NGOOD                        ! Number of data points
      REAL VAR(NGOOD)                      ! Variances
      REAL VAR0                            ! Extra weighting factor
*    Import-Export :
      REAL WEIGHT(NGOOD)                   ! Weights
*    Export :
*    Local constants :
*    Local variables :
      REAL LP,LP2
*-
      DO LP=1,NGOOD
*
*  Test if the variance for this data point is ok.
         IF ( (VAR(LP) + VAR0) .GT. 0) THEN
*
            WEIGHT(LP) = 1.0 / ( VAR(LP) + VAR0 )
*
         ELSE
*
*  If one variance is bad set, issue a message and set all weights to
*  one.
            CALL MSG_SETI('LP', LP)
            CALL MSG_PRNT('Variance of datapoint ^LP is zero or '/
     &                   /'negative - setting all weights to 1.0')
*
            DO LP2=1,NGOOD
               WEIGHT(LP2) = 1.0
            ENDDO
*
*  Leave loop
            GOTO 100
*
         ENDIF

      ENDDO
*
100   CONTINUE
*
      END

*************************************************************************
*+CLEANPOW_REWRITE  -  Copies douvble prec. arrays into single precision arays
      SUBROUTINE CLEANPOW_REWRITE(N, D1, D2, D3, S1, S2, S3)
*    Description :
*     <description of what the subroutine does>
*    History :
*     date:  original (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Import :
      INTEGER N                           ! Number of elements
      DOUBLE PRECISION D1(N),D2(N),D3(N)  ! D.P. arrays
*    Import-Export :
*    Export :
      REAL S1(N),S2(N),S3(N)	          ! REAL arrays
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER LP
*-
      DO LP=1,N
*
         S1(LP) = D1(LP)
         S2(LP) = D2(LP)
         S3(LP) = D3(LP)
*
      ENDDO
*
      END
