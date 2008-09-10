#if HAVE_CONFIG_H
#  include "config.h"
#endif
C+
      SUBROUTINE GAUSS
C
C     G A U S S
C
C     Interactively fits Gaussians to an emission or absorption 
C     feature in a spectrum. A spectrum, whose length is prompted
C     for, is plotted on the selected 'SOFT' device together with a 
C     residual plot above. Regions of continuum are demarcated with 
C     the cursor. Continuum is fitted by a polynomial with rejection
C     of points a selected sigma from this continuum or a fraction
C     of the error bar ( if available ). Single or multiple Gaussian 
C     fits are made to the emission/absorption feature. In the case 
C     of multiple fits interactive fitting of the profile by Gaussians 
C     whose parameters are altered from the terminal occurs. The 
C     residuals on the fit ( observed - fitted ) are plotted in the 
C     residuals box above the main plot. If error bars are available 
C     for the spectrum they are plotted in the residuals box. The fit
C     may be optimized and any single Gaussian parameter can be
C     constrained or its value chained to another. The residuals on
C     the fit for the purposes of the optimization can be weighted
C     either by value or error. Data on the fit - central wavelength,
C     height and sigma - are reported for each Gaussian and r.m.s. and
C     mean fractional error (if errors available) on fit. The results
C     may be recorded in a data file. The final fit spectrum can be
C     saved as a file for subsequent analysis.
C 
C     Command parameters -
C
C     SPECTRUM    The data to be fitted. This should be a
C                 1d .dst file with a .z.data component.  If there
C                 is a .x.data component this will be used to
C                 give the x-axis.  If not, the x-axis will just
C                 have to be the numbers from 1 to n.
C     XSTART      The x-value at which plotting is to start.
C     XEND        The x-value at which plotting is to end.
C                 (XSTART and XEND are not required if the
C                 WHOLE keyword is specified.)
C     HIGH        The maximum value to be used for the plot.
C     LOW         The minimum value to be used for the plot.
C     BIAS        Value to be added to zero for the plot.
C     LABEL       A label for the plot and the data file.
C    
C     Command keywords -
C
C     AUTOSCALE   The program is to work out the values for HIGH 
C                 and LOW, using the maximum and minimum values
C                 in the data over the specified range.
C     WHOLE       The program is to display all of the spectrum.
C
C     User variables -    (">" input, "<" output)
C
C     (>) SOFT    Specifies the device and type to be used for soft
C                 plots.  See the SOFT command for more details.
C     (>) HARD    Specifies the device and type to be used for hard
C                 plots.  See the HARD command for more details.
C     (<) TVXST   is set to the starting x-value for the plot.
C     (<) TVXEN   Is set to the final x-value for the plot.
C     (<) TVHIGH  Is set to the same value as HIGH.
C     (<) TVLOW   Is set to the same value as LOW.
C     (<) TVFILE  Is set to the value of SPECTRUM.
C
C     Subroutines called:
C       MAPSPEC  -  unmaps the X and Z from virtual memory into arrays
C                   in the program
C       MAPSPECE  -  unmaps the X, Z and error data from virtual memory
C                    into arrays in the program
C       MAPSPECG  - maps the output fitting spectrum Z component into 
C                   virtual memory
C       GAUS_XZPLOT  -  handles all the PGPLOT plotting of spectrum,
C                       continuum fit, Gaussian fit, residuals and
C                       error bars
C       CONMENU  -  the continuum fitting menu for polynomial continuum
C                   fitting
C       GAUMENU  -  the Gaussian fitting menu for all Gaussian fitting
C
C                                          JRW / AAO March 1987
C
C     Modified:
C      1987 Mar 13 JRW / AAO 
C                  Highest order, highest iteration continuum fit 
C                  plotted in continuum fitting menu (FIT) to allow 
C                  visual check on goodness of fit. 
C                  Default values of ORD, ITN, SIG and ERR in continuum
C                  fitting menu are not reset at each entry to this
C                  subroutine. Default values printed to terminal on a 
C                  second entry to this subroutine are now the actual
C                  values used.  Can fit further new lines after 
C                  optimization in the Gaussian fitting menu.
C                  Data on all fits are recorded in results file.
C      1987 Apr 24 KS / AAO.
C                  'DEFAULTFILE' added to INQUIRE and OPEN statements 
C                  for the results file (prevents error if return 
C                  or blank pressed for name of file).
C      1987 Apr 26 JRW / AAO
C                  If OPT chosen in Gaussian fitting then residuals can 
C                  be weighted either by inverse square of error 
C                  ( if errors available ) or by value.
C                  Prompt for accepting manual fit changed to PAR_RDKEY
C      1987 Jun 10 JRW/AAO
C                  Second Gaussian has its flux correctly calculated 
C                  (factor 2.354 was omitted) in RECORD.
C                  Lower limits to line width corrected to be in terms
C                  of sigma and not FWHM (in AUTOFIT).
C                  In OPT of GAUMENU, a check is made for lines whose 
C                  peaks occur outside the range of GXVALS and any such
C                  are deleted.
C      1987 Jul 6  KS/AAO
C                  The appendage of .DAT to the file name, which 
C                  remained from the Apr 24 bug fix is deleted.
C      1987 Aug 6  JRW/AAO 
C                  In Gaussian fitting menu if full name of prompt given
C                  it would not be recognized. Full names now accepted
C                  by ICH_KEY.
C      1987 Sep 15 JRW/AAO
C                  Any single Gaussian parameter ( position, width or 
C                  height ) can be locked beofre the optimization or
C                  sets of values can be chained.
C      1987 Oct 22 JRW/AAO
C                  Bug fix. If no .X.DATA present X values are 
C                  assumed to be increasing channel number and 
C                  reduction proceeds. 
C                  PGASK added so that prompt for " Press <RETURN>
C                  when ready " only appears after fitting continuum
C                  or optimization of Gaussian fit ( to allow 
C                  assessment of results printed to terminal )
C      1987 Dec 21 JRW/AAO
C                  If dimension of spectrum greater than 4096 an error 
C                  message is issued and program stops. 
C                  Can now exit from Gaussian fitting menu without
C                  having fitted a line. This can be useful if only a
C                  continuum fit is required.
C                  In Gaussian fitting menu SIN now tests for a sensible
C                  extent for the line. If no left edge is available the
C                  left edge of the spectrum is used; similarly for
C                  right edge. Warning messages are issued in these
C                  cases.
C      1988 Mar 24 KS / AAO
C                  Modified to work with GKS version of PGPLOT.  Only
C                  minimal changes made - replacement of 
C                  now-discontinued GRPCKG routines and suchlike. 
C                  Proper modifications to introduce GKD_ dialogue etc, 
C                  are a bigger job - partly because of use of PAR_
C                  system for dialogue - e.g. use of PAR_CNPAR.
C      1991 Apr  5 JMS / AAO. Modified to use DSA_ routines.
C      1992 Sep 10 HME / UoE, Starlink.  INCLUDE changed. INQUIRE
C                  statement changed, to not use "DEFAULTFILE". Calls to
C                  WRUSER changed to PAR_WRUSER.
C                  Open help file with lowercase. Changed file extension
C                  for RECNAME to lowercase.
C                  PGASK is banned from ADAM, commented out.
C                  Calls to DSA_WRUSER changed to PAR_WRUSER.
C      1993 Jan 25 HME / UoE, Starlink.  Put PGASK back in.
C      1993 Jul 21 HME / UoE, Starlink.  Use DSA_GET_LU.
C      1993 Jul 27 HME / UoE, Starlink.  Disuse PAR_RDUSER, use
C                  PAR_ABORT. Added parameters CCMD and GCMD.
C                  Change use of PGASK, so that no prompting occurs.
C      1994 Dec 21 HME / UoE, Starlink.  Re-write the line-fit part. The
C                  new algorithm is taken from Specdre's FITGAUSS. The
C                  reason is that the old algorithm used routines
C                  removed from the NAG library at Mark 16. But the new
C                  algorithm is also more stable.
C                  At the same time prevent specifying more than 10
C                  lines at a time with the "NEXT" option. This is the
C                  internal limit and specifying an 11th line did mess
C                  up the guess parameters.
C      1995 Aug 23 HME / UoE, Starlink.  No longer use NAG.
C      2005 Jun 14 MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_LEN,PGBEGIN
C
C     Local variables
C
      INTEGER I,EPTR,ITEMP,IXEN,IXST,LLAB,NDIM,
     :NOPEN,ILEN,NX,NNX,XPTR,ZPTR,ICONO(4096),ORD,ITN,CX,ICST,
     :GPTR,STATUS,STAT,LU,SLOT
      REAL BIAS,HIGH,LOW,VALUE,WID,VMAX,VMIN,XMAX,XMIN,XVST,XVEN,
     :RMAX,RMIN,HIGHR,LOWR,SIG,ERR,XVALS(4096),ZVALS(4096),
     :ERRORS(4096),ZRESID(4096),CONVALS(4096),FITTOT(4096),
     :GAUFT(4096),DUM(1)
      DOUBLE PRECISION XTOLI
      CHARACTER DEVICES*32,DEVICEH*32,PLAB*64,
     :SPECT*32,XLAB*32,XLABEL*16,XUNITS*16,ZLAB*32,
     :ZLABEL*16,ZUNITS*16,FNAME*32,DATNAM*30
      LOGICAL AUTOSC,EMAP,ERASE,ERRUSE,HARD,WHOLE,LOPEN,
     :FAULT,XMAP,XIMAP,ZMAP,LCON,LREC,LSECT,CRES,GRES,GTOT,GALL,
     :SAVE,SAVEF,SOPEN,GOTVM,PGOPEN,SAVOPN,SAVMAP,GOTLU,FOPEN
      INTEGER DIMS(2)
      DOUBLE PRECISION NUM_ARRAY(1),DUMMY2
      CHARACTER*16 CHAR_ARRAY(2),DUMMY1
C
C     Floating point limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Get the hidden parameter TOL for the initial value
C     for XTOLI ( used by E04JBF as the tolerance on the
C     value of the optimised function )
C
      CALL PAR_RDVAL('TOL',1.E-6,1.E-1,1.E-3,' ',VALUE)
      XTOLI=DBLE(VALUE)
C
C     Initial values
C
      STATUS = 0
      PGOPEN=.TRUE.
      GOTVM=.FALSE.
      EMAP=.FALSE.
      ZMAP=.FALSE.
      XMAP=.FALSE.
      XIMAP=.FALSE.
      LREC=.FALSE.
      LSECT=.FALSE.
      SAVE=.FALSE.
      SOPEN=.FALSE.
      SAVEF=.FALSE.
      FAULT=.FALSE.
      SAVOPN=.FALSE.
      SAVMAP=.FALSE.
      FOPEN=.FALSE.
      GOTLU=.FALSE.
C
C     Open DSA routines
C
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Set initial values for continuum fitting parameters ( order,
C     sigma for point rejection and number of iterations for point 
C     rejection. These can be amended in CONMENU
C
      ORD=3
      SIG=2.
      ITN=1 
C
C     Get name of file for SPECTRUM, and open it.
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GOTO 500
      SOPEN=.TRUE.
C
C     Get dimensions of data
C
      CALL DSA_DATA_SIZE('SPECT',1,NDIM,DIMS,NX,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Test for value of NX greater than 4096. If so stop with a warning.
C
      IF (NX.GT.4096) THEN
        CALL PAR_WRUSER('Spectrum has more than 4096 channels.',
     :                  STATUS) 
        FAULT=.TRUE.
        GO TO 500
      END IF
C
C     Try for .X information
C
      CALL DSA_GET_AXIS_INFO('SPECT',1,2,CHAR_ARRAY,0,DUMMY2,STATUS)
      XUNITS=CHAR_ARRAY(1)
      XLABEL=CHAR_ARRAY(2)
C
C     Try to map the .X data array
C
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',XPTR,SLOT,STATUS)
      XIMAP=.TRUE.
C
C     Map the .Z data
C
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',ZPTR,SLOT,STATUS)
      ZMAP=.TRUE.
C
C     Get Z data information (units and label)
C
      CALL DSA_GET_DATA_INFO('SPECT',2,CHAR_ARRAY,0,DUMMY2,STATUS)
      ZUNITS=CHAR_ARRAY(1)
      ZLABEL=CHAR_ARRAY(2)
C
C     Find out if there are errors. If so then use in plotting. Set initial
C     value of factor for point rejection on basis of errors ( ERR )
C
      CALL DSA_SEEK_ERRORS('SPECT',ERRUSE,STATUS)
      IF (ERRUSE) THEN
         CALL DSA_MAP_ERRORS('SPECT','READ','FLOAT',EPTR,SLOT,STATUS)
         ERR=1
         EMAP=.TRUE.
      ELSE 
         CALL PAR_WRUSER('No errors available.',STATUS)
         ERR=0
         EMAP=.FALSE.
      END IF
C
C     Look for the value of the user variable 'HARD'
C
      CALL VAR_GETCHR('HARD',0,0,DEVICEH,STATUS)
      IF (STATUS.NE.0) THEN
        CALL PAR_WRUSER('No hardcopy device specified.',STATUS)
        CALL PAR_WRUSER(
     :   'Use "HARD" command eg "HARD VER" to rectify.',STATUS)
        FAULT=.TRUE.
        GO TO 500
      END IF
C
C     Look for the value of the user variable 'SOFT'
C
      CALL VAR_GETCHR('SOFT',0,0,DEVICES,STATUS)
      IF (STATUS.NE.0) THEN
        CALL PAR_WRUSER('No plotting device specified.',STATUS)
        CALL PAR_WRUSER(
     :   'Use "SOFT" command eg "SOFT /VT" to rectify.',STATUS)
        FAULT=.TRUE.
        GO TO 500
      END IF
C
C     Get the axis labels from the labels and units
C
      LLAB=ICH_LEN(XLABEL)
      IF (LLAB.GT.0) THEN
         XLAB=XLABEL(:LLAB)//'  '//XUNITS
      ELSE
         XLAB=XUNITS
      END IF
      LLAB=ICH_LEN(ZLABEL)
      IF (LLAB.GT.0) THEN
         ZLAB=ZLABEL(:LLAB)//'  '//ZUNITS
      ELSE
         ZLAB=ZUNITS
      END IF
C
C     Get the label for the plot - 
C
      CALL PAR_RDCHAR('LABEL',' ',PLAB)
      IF (PAR_ABORT()) GO TO 500
C
C     Determine if the results of the fit are to be preserved.
C     If so determine if appending to an existing file or
C     creating a new file
C
      CALL PAR_RDKEY('FILE',.TRUE.,LREC)
      IF (PAR_ABORT()) GO TO 500
      IF (LREC) THEN
        CALL DSA_GET_LU(LU,STATUS)
        IF (STATUS.NE.0) THEN
           CALL PAR_WRUSER('Unable to get logical unit for file',STATUS)
           FAULT=.TRUE.
           GO TO 500
        END IF
        GOTLU=.TRUE.
        CALL PAR_RDCHAR('FILNAM','gausfit.dat',DATNAM)
        IF (PAR_ABORT()) GO TO 500
        ILEN=ICH_LEN(DATNAM)
        IF (ILEN.LT.1) THEN
           FNAME='gausfit.dat'
        ELSE
           FNAME=DATNAM(:ILEN)
        END IF
C
C       See if the file already exists.
C
        INQUIRE (FILE=FNAME,EXIST=LOPEN)
C
C       Open the file in such a way that if it already exists
C       new records will be appended to it.
C
        OPEN(UNIT=LU,FILE=FNAME,STATUS='UNKNOWN',
#if HAVE_FC_OPEN_POSITIONAPPEND
     :       ACCESS='SEQUENTIAL',POSITION='APPEND',
#elif HAVE_FC_OPEN_ACCESSAPPEND
     :       ACCESS='APPEND',
#else
      Do not have ACCESS=APPEND or POSITION=APPEND !
#endif
     :          FORM='FORMATTED',IOSTAT=NOPEN)
        IF (NOPEN.NE.0) THEN
          CALL PAR_WRUSER('Failed to open file',STATUS)
          FAULT=.TRUE.
          GO TO 500
        END IF
        FOPEN=.TRUE.
C
C       Write some brief header information at the top of this file,
C       if it did not already exist.
C
        IF (.NOT.LOPEN) THEN
           WRITE(LU,211)
           WRITE(LU,212)
           WRITE(LU,215)
           WRITE(LU,213) SPECT
           WRITE(LU,214) PLAB
           WRITE(LU,215)
           WRITE(LU,216) XUNITS,ZUNITS
           WRITE(LU,217) XUNITS,ZUNITS
           WRITE(LU,215)
           WRITE(LU,218)
           WRITE(LU,219)  
           WRITE(LU,215)
211       FORMAT('     DATA ON GAUSSIAN FIT')
212       FORMAT('     --------------------')
213       FORMAT('  Spectrum file: ',A32) 
214       FORMAT(' Spectrum label: ',A64)
215       FORMAT('.')
216       FORMAT(' X and E.W. units are ',A16,'; Y units are '
     :           ,A16)
217       FORMAT(' Flux units are ',A16,'* ',A16)
218       FORMAT('                         G a u s s i a n   F i t     
     :                                       !     C o n t i n u u m
     :   F i t')
219       FORMAT('     Peak Posn.   Peak Height     Sigma     Flux  
     :         E.W.    R.m.s.    Mean frac err    Order 
     :  Sigma Rej.  Frac err Rej.')
         END IF
      END IF
C
C     Get XSTART and XEND, unless WHOLE was
C     specified, in which case use all of the spectrum.
C
      CALL GEN_RANGEF(%VAL(CNF_PVAL(XPTR)),1,NX,XMAX,XMIN)
      WID=ABS((XMAX-XMIN)/REAL(NX-1))
      CALL PAR_RDKEY('WHOLE',.FALSE.,WHOLE)
      IF (PAR_ABORT()) GO TO 500
C
101   CONTINUE
C      
      IF (WHOLE) THEN
         IXST=1
         IXEN=NX
         XVST=XMIN
         XVEN=XMAX
         CALL PAR_SDVAL('XSTART',XVST,STATUS)
         CALL PAR_SDVAL('XEND',XVEN,STATUS)
      ELSE
         CALL PAR_RDVAL('XSTART',XMIN,XMAX,XMIN,XUNITS,XVST)
         CALL PAR_RDVAL('XEND',XVST,XMAX,XMAX,XUNITS,XVEN)
         IF (PAR_ABORT()) GO TO 500
         IXST=NINT((XVST-XMIN)/WID) + 1
         IF (IXST.LE.0) THEN
           IXST=1
           XVST=XMIN
         END IF
         IXEN=NINT((XVEN-XMIN)/WID) + 1
         IF (IXEN.LE.0.OR.IXEN.GT.NX) THEN
           IXEN=NX
           XVEN=XMAX
         END IF
         IF (IXST.GT.IXEN) THEN
            ITEMP=IXST
            IXST=IXEN
            IXEN=ITEMP
         END IF
      END IF
C
C     Make sure there is a range of data to plot.
C
      IF (IXST.GE.IXEN) THEN
         IXST=1
         IXEN=NX
         XVST=XMIN
         XVEN=XMAX
      END IF
      NNX=IXEN-IXST+1
C
C     Specified or not, find out the scale range 
C
      IF (ERRUSE) THEN
         CALL GEN_RANGEF(%VAL(CNF_PVAL(EPTR)),IXST,IXEN,RMAX,RMIN)
      END IF
      CALL GEN_RANGEF(%VAL(CNF_PVAL(ZPTR)),IXST,IXEN,VMAX,VMIN)
C
C     Was AUTOSCALE specified?
C
      CALL PAR_RDKEY('AUTOSCALE',.FALSE.,AUTOSC)
      IF (PAR_ABORT()) GO TO 500
C
C     If AUTOSCALE not specified, get values of HIGH and LOW
C
      IF (AUTOSC) THEN
         HIGH=VMAX + ABS(VMAX-VMIN)*0.04
         LOW=VMIN - ABS(VMAX-VMIN)*0.04
         CALL PAR_SDVAL('HIGH',HIGH,STATUS)
         CALL PAR_SDVAL('LOW',LOW,STATUS)
         CALL PAR_SDVAL('BIAS',0.,STATUS)
      ELSE
         CALL PAR_RDVAL('HIGH',FMIN,FMAX,VMAX,ZUNITS,HIGH)
         CALL PAR_RDVAL('LOW',FMIN,HIGH-1.E-20,VMIN,ZUNITS,LOW)
         CALL PAR_RDVAL('BIAS',FMIN,FMAX,0.,ZUNITS,BIAS)
         IF (PAR_ABORT()) GO TO 500
         HIGH=HIGH-BIAS
         LOW=LOW-BIAS
      END IF
      IF (HIGH.EQ.LOW) THEN
         IF (LOW.EQ.0.0) THEN
            HIGH=0.01
         ELSE
            HIGH=ABS(LOW)*1.1
         END IF
      END IF
C
C     Get the maximum and minimum values of the error residuals
C     for setting the size of the residuals scale. If no errors
C     set to 1.0% of full scale
C
      IF (ERRUSE) THEN
        HIGHR=1.083*RMAX
        LOWR=-1.083*RMAX
      ELSE
        HIGHR=ABS(HIGH-LOW)/100.
        LOWR=-1.*HIGHR
      END IF
C
C     See if the data should be plotted in reverse (do this only if
C     the data is in magnitudes).
C
      CALL DSA_GET_DATA_INFO('SPECT',0,DUMMY1,1,NUM_ARRAY,STATUS)
      IF (STATUS.NE.0) GOTO 500
      IF (NUM_ARRAY(1).NE.0) THEN
         VALUE=HIGH
         HIGH=LOW
         LOW=VALUE
      END IF
C
C     Copy the mapped data into the local arrays - XVALS,ZVALS and ERRORS
C
      IF (XIMAP.AND.ZMAP) THEN
        IF (.NOT.ERRUSE) THEN
          CALL MAPSPEC(NX,%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(ZPTR)),
     :                 IXST,NNX,XVALS,ZVALS,STATUS)
        END IF
        IF (ERRUSE) THEN
          CALL MAPSPECE(NX,%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(ZPTR)),
     :                  %VAL(CNF_PVAL(EPTR)),IXST,NNX,XVALS,ZVALS,
     :                  ERRORS,STATUS)
        END IF
        IF (STATUS.NE.0) THEN
          CALL PAR_WRUSER('Error copying data',STAT)
          GO TO 500 
        END IF
      END IF
C
C     Open PGPLOT on the selected device. Check status. If OK, plot data.
C
      STATUS=PGBEGIN(0,DEVICES,1,1)
      IF (STATUS.NE.1) THEN
        CALL PAR_WRUSER('Error opening soft device',STATUS)
        FAULT=.TRUE.
        GO TO 500
      END IF
      PGOPEN=.TRUE.
      CALL PGASK(.FALSE.)
C
C     Perform the initial spectrum plot without any residuals
C
      ERASE=.TRUE.
      LCON=.FALSE.
      CRES=.FALSE.
      GRES=.FALSE.
      GTOT=.FALSE.
      GALL=.FALSE.
      HARD=.FALSE.
C
C     ( GAUS_XZPLOT is the only plotting routine. To avoid setting
C     arrays with variable dimensions to zero dimensions, set length 
C     of CONVALS to NNX; and  GXVALS, GAUSUM, GAUFS and GZRESID to 
C     useless array DUM, of dimension 1 )
C
      CALL GAUS_XZPLOT(XVALS,DUM,ZVALS,ERRORS,
     :    ZRESID,CONVALS,ICONO,DUM,DUM,DUM,NNX,1,1,1,
     :    ICST,0,HIGH,LOW,HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,
     :    HARD,DEVICEH,ERRUSE,LCON,CRES,GRES,GTOT,GALL,XVST,XVEN,
     :    STATUS)
      IF (STATUS.NE.0) THEN
        CALL PAR_WRUSER('Error in plotting',STAT)
      END IF
C
C     Perform the continuum fit through the continuum fitting options
C     in the menu CONMENU
C
C     Cursor introduced on lower spectrum plot so set PGPLOT window
C     to this viewport
C
      CALL PGVPORT(0.08,0.97,0.09,0.74)
      CALL PGWINDOW(XVST,XVEN,LOW,HIGH)
C
      CALL CONMENU(NX,XVALS,WID,ZVALS,ERRORS,ERRUSE,ICST,CX,
     :   ORD,ICONO,CONVALS,ZRESID,ITN,SIG,ERR,XVST,XVEN,HIGH,
     :   LOW,HIGHR,LOWR,XLAB,ZLAB,PLAB,ERASE,DEVICES,HARD,
     :   DEVICEH,LCON,CRES,GRES,GTOT,GALL,FAULT)
      IF (PAR_ABORT()) GO TO 500
      IF (FAULT) THEN
        GO TO 500
      END IF
C
C     Perform the Gaussian fit to the profile through the Gaussian
C     fitting options in the menu GAUMENU
C
C     Initial plotting done on lower spectrum plot. Set PGPLOT to this
C     viewport
C    
      CALL PGVPORT(0.08,0.97,0.09,0.74)
      CALL PGWINDOW(XVST,XVEN,LOW,HIGH)
C
      CALL GAUMENU(XVALS,WID,ZVALS,ERRORS,ZRESID,
     :       CONVALS,ICONO,FITTOT,NNX,CX,ICST,HIGH,LOW,HIGHR,
     :       LOWR,XLAB,ZLAB,PLAB,DEVICES,DEVICEH,ERRUSE,XVST,
     :       XVEN,ORD,SIG,ERR,XUNITS,ZUNITS,LREC,LU,XTOLI,LSECT,SAVE)
      IF (PAR_ABORT()) GO TO 500
C
C     If fit to be saved then copy the fitted section to the
C     holding array GAUFT
C
      IF (SAVE) THEN
        DO I=1,NNX
          GAUFT(IXST+I-1)=FITTOT(I)
        END DO
        SAVE=.FALSE.
        SAVEF=.TRUE.
      END IF
C
C     If another continuum section to analyse return to delineating
C     continuum. If PGPLOT not closed, then do so.
C
      IF (LSECT) THEN
        CALL PAR_CNPAR('XSTART')
        CALL PAR_CNPAR('XEND')
        CALL PAR_CNPAR('HIGH')        
        CALL PAR_CNPAR('LOW')
        CALL PAR_CNPAR('BIAS')        
        IF (.NOT.HARD) THEN
          CALL PGEND
          PGOPEN=.FALSE.
        END IF
        WHOLE=.FALSE.
        GO TO 101
      END IF
C
C     If fit complete and resultant fitting spectrum to be saved, then
C     prompt for name of output spectrum. Copy the complete input 
C     structure to this file and replace the Z values by the fitting
C     spectrum.
C
      IF (.NOT.LSECT.AND.SAVEF) THEN
         CALL DSA_OUTPUT('GAUFIT','GAUFIT','SPECT',0,0,STATUS)
         IF (STATUS.NE.0) GOTO 500
         CALL DSA_MAP_DATA('GAUFIT','UPDATE','FLOAT',GPTR,SLOT,STATUS)
         SAVMAP=.TRUE.
         CALL MAPSPECG(NX,GAUFT,%VAL(CNF_PVAL(GPTR)),STATUS)
      END IF
C
C     Close down the soft plot
C
      CALL PGEND
      PGOPEN=.FALSE.
      STATUS=0
C
C     Set the user variables describing the plot for any subsequent plotting
C
      CALL VAR_SETNUM('TVXST',0,0,XVST,STATUS)
      CALL VAR_SETNUM('TVXEN',0,0,XVEN,STATUS)
      CALL VAR_SETNUM('TVHIGH',0,0,HIGH,STATUS)
      CALL VAR_SETNUM('TVLOW',0,0,LOW,STATUS)
      CALL VAR_SETCHR('TVFILE',0,0,SPECT,STATUS)
C
C     Tidy up everything
C
  500 CONTINUE
C
C     Close down everything
C
      IF (PGOPEN) CALL PGEND
C
      CALL DSA_CLOSE(STATUS)
C
      IF (FAULT) CALL FIG_SETERR
C
      END
