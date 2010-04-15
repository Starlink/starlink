C+
      SUBROUTINE ECHARC
C
C     E C H A R C                    (Version 1.0, 18-NOV-1987 ff.)
C                                    (Version 1.5, 08-DEC-1987 ff.)
C
C     This substantially revised version of ECHARC0 performs the 1D
C     ARC process on 3-30 orders of a collapsed echelle image, and
C     then automatically detects lines and performs fits to all the
C     remaining orders. The output from the program is a complete
C     listing of all lines found (ARLINES.ECH) and an output image
C     containing the fitted wavelengths as image data. Alternatively the
C     fitted wavelengths can be stored as a new X axis array together
C     with the original image data.
C
C     Command parameters -
C
C     IMAGE        The arc data.  This should be a two-dimensional
C                  image. ECHARC assumes there is a y axis giving
C                  order numbers "m". If there is an x axis
C                  component the information it contains will be
C                  used during the program, although usually the
C                  x data will simply be pixel number.
C     ARCTYPE      The type of arc that was used - eg HELIUM,
C                  NEON, etc. ARC will look for a file called
C                  ARCTYPE.ARC which should hold the line list for
C                  the arc.
C     INTERACTIVE  The number of orders to be fit interactively.
C     ORDERS       The array of INTERACTIVE order numbers to be fit.
C     ORDERFIT     The initial order of the polynomial fit.
C     SIGMA        The initial value for the line width.
C     ARFILE       The name of the list file from which the previous
C                  fit is to be read. Only used if PREVIOUS is
C                  specified. Note that the output is always written
C                  to ARLINES.ECH. Default extension is .ECH
C     WAVES        An output image containing the fitted wavelengths
C                  from this ECHARC solution as image data (not as axis
C                  data). This is created only if DOWAVES is specified.
C     OUTPUT       The name of the output file that combines the input
C                  image data with the fitted wavelengths as axis
C                  data. This is created only if DOWAVES is false.
C     CONTINUE     Command after an order is done with.
C     ORDPPAG      The number of sub-orders to be plotted per page in
C                  the hard copy line atlas.
C     DISNCHAN     Length of displayed sections.
C     MOVETOX      New plot centre x value.
C     CMD          Command in main menu.
C     LINENO       Number of line to be edited.
C     WAVELEN      Wavelength specification.
C     CHFACT
C     SIGFACT
C
C     Command keywords -
C
C     PREVIOUS     If specified, ARC will read in the line list from
C                  the previous session as a starting point.
C     DOWAVES      If specified, the fitted wavelengths will be stored
C                  as image data in a separate file. Otherwise the
C                  fitted wavelengths will be stored as axis data along
C                  with the input image.
C     HLINEMAP     If specified, a map of line locations is plotted as
C                  a hard copy.
C     HATLAS       If specified, an atlas of lines is plotted as a
C                  hard copy.
C     ANALYSIS     If specified, a detailed line-by-line analysis of the
C                  arc fit is written to the file "echarc.lis".
C     HARDARC      If specified, the output spectrum is plotted in a
C                  hard copy.
C     HARDISP      If specified, the dispersion curve is plotted in a
C                  hard copy.
C     QUITSEL      Used to confirm quitting line selection.
C     LINEOK       Used to confirm a choice of line for deletion,
C                  editing etc.
C     RESOLVE      Used to decide what to do if a line is used twice.
C
C     User variables -
C
C     (>) SOFT     (Char) The device/type to be used for graphics
C                  soft plots.  See the SOFT command for details.
C                  The device must support a cursor.
C     (>) HARD     (Char) The device/type for graphics hard plots.
C
C     Input -
C
C     As named     May use the lines from a previous run.  If so
C     by ARFILE    these are read from the previous run's output
C                  file.  See below.
C
C     Output -
C
C     ARLINES.ECH  File containing the lines used in the final fit.
C                  Format is as follows -
C                  Number of lines used in fit (I5)
C                  1 blank record, then one header record.
C                  Then one record for each line, giving order, channel
C                  number, wavelength, calculated wavelength, wavelength
C                  discrepancy, line number and auto flag (I3,4F13.4,I7,A4)
C                  The auto flag is either " (A)" for a single order Auto
C                  fit, " (E)" for complete echelle order auto fit, or is
C                  a blank string for lines explicitly identified by user.
C                  Then one blank record, then a record giving the RMS
C                  error and the value of SIGMA used (12X,F10.2,19X,F5.2)
C                  Then one blank record, then one record giving the
C                  number of coefficients of the fit (15X,I3).
C     ECHARC.LIS   Detailed line-by-line analysis.
C
C     Functions / subroutines used -
C
C     ECH_ARINTR  (FIGARO)  Plots an order section by section and
C                           allows user to identify lines, fit a
C                           polynomial to those lines, and repeat.
C                           When an order is finished, the lines
C                           identified within it are written to
C                           the file ARLINES.ECH in case of problems
C                           during the fit to the next order.
C     ECH_ARGETL  (FIGARO)  Reads order, channel, wavelength, etc.,
C                           information from an existing ARLINES.ECH
C                           -format file to use as a starting point
C                           for fits to the current order.
C
C     Originally  ARC :                       KS / CIT  13th June 1984
C
C        Modified:
C
C        28th Nov 1984. KS/AAO. Test for only one line added before fit.
C        10th Dec 1984. KS/AAO. Number of arc lines allowed increased.
C        5th Sept 1985. KS/AAO. Dispersion plot facility added and menu
C                       operation adopted for the fit,edit,refit sequence.
C                       ARFILE parameter added.  Output file name now
C                       output. Line number added to output file format.
C                       RMS now output after 'C'.  Weights array now
C                       incorporated everywhere, but only really used in
C                       ARFIT for the 'RMS without this line' figure.
C                       Autofit added, together with class array.  Defaults
C                       for order and sigma now taken from previous file,
C                       if used.
C        12th Sept 1985 KS/AAO. Now checks dispersion and decides if a
C                       double precision X array has to be created.
C                       Error actions modified to follow later Figaro style
C                       using FAULT and FIG_DTAERR.  WRUSER calls changed
C                       to PAR_WRUSER.
C        30th Sept 1985 KS/AAO. 'Modify' added to menu options.  Order may
C                       now be specified as 0.
C        22nd Nov  1985 KS/AAO. Occasional bug causing access violation in
C                       final hardcopy plot traced to failure to remap Z
C                       array after creation of new X array.  Fixed.
C        30th June 1986 KS/AAO Initially requested order is now remembered
C                       until enough lines have been selected.
C
C     Stolen & Modified --> ECHARC0:               JKM / CIT 9 Dec 1986
C
C        Modified for use with 2D input data, one order at a time. "ORDER"
C        (of polynomial fit) renamed "NCOEFF" and all occurances of [real]
C        ORDER+1 ---> ORDER [NCOEFF] changed to a consistant NCOEFF def'n.
C
C        Output file renamed "ARLINES.ECH" and format of file changed to
C        include order number for each line identified.  All subroutines
C        affected by this revision renamed ECH_AR...
C
C     Modified:         --> ECHARC:  v. 1.0        JKM / ESO 18 Nov 1987
C
C        The highly interactive loop through all orders was replaced by
C        interactive fits required for only three orders, with the program
C        then identifying lines and performing fits automatically for all
C        the remaining orders in the input image.  WAVES image added.
C
C     Modified:         --> ECHARC:  v. 1.5        JKM / ESO 8. Dez 1987
C
C        The automatic search sequence was changed, so that instead of
C        starting with order 1 and going to order NY, it starts mid-way
C        between the first and last order interactively fit and proceeds
C        from the center outward in four stages.  It then makes three
C        additional attempts to fit orders previously unsatisfactory.
C
C     26 May 1988  WFL/AAO Replace GRETEXT calls with GKD_ calls.
C                  Increase maximum number of arc lines to 5000.
C     27 May 1988  WFL/AAO Replace GR* calls with PG* calls.
C     28 May 1988  WFL/AAO Sort out PGEND calls for hardcopy atlas.
C                  Correct formatting bug in ECHARC.LIS output.
C     29 May 1988  WFL/AAO Check that newly found lines during the auto
C                  order fit are not duplicates (they may have been
C                  imported in the ARLIST.ECH file). Allow binary arc
C                  line files. Prevent "Error releasing ARC workspace"
C                  message on premature quit. Permit further lines to
C                  be found in the interactively fitted orders during
C                  the automatic order fit. Prevent zero lines identified
C                  from causing fall over when writing ECHARC.LIS file.
C     30 May 1988  WFL/AAO Correct bug re deletion of lines so far ident-
C                  ified for an order. Sort lines before writing the
C                  ECHARC.LIS file. After reading the ARLINES.ECH file,
C                  delete all lines not in orders to be fit interactively.
C                  (because they are not used and to avoid problems when
C                  applying small shifts in wavelength). Change MONITOR
C                  to be a normal lockable keyword.
C     31 May 1988  WFL/AAO Add DOWAVES keyword. If TRUE write the WAVES
C                  file as at present. If FALSE write it to a 2D .X.DATA
C                  object. Don't use the .X.DATA to label the axes as
C                  to do so would confuse and in the reports, channel
C                  number would be wavelength! Correct bug that deleted
C                  lines from an order if they also occurred in another
C                  order. Allow search of interactively fitted orders
C                  in first pass. Prevent crashes due to calls to ARFITX
C                  with too few lines.
C     01 Jun 1988  Cut out error message in ECH_ARFILL (caused by changes
C                  to fitting algorithm and non-fatal!)
C     07 Sep 1988  Change NCOEFF to ORDERFIT to preserve some compatibility
C                  with ARC (undoing JKM's mod of 9th Dec 1986)
C     18 Sep 1988  To avoid conflicts with routines in ARC (when linked
C                  with Callable Figaro, for example) all subroutines
C                  with names still common to both - anything still
C                  beginning 'AR' instead of 'ECH_AR' have been given
C                  an ARC_ prefix.  Sometime, these and those used by
C                  ARC should be combined into a single set of utilities.
C                  KS/AAO.
C     08 May 1989  Increase maximum number of interactively fitted orders
C                  from 10 to 30; insert debug output explaining why lines
C                  are rejected in the automatic inter-order phase. WFL/AAO.
C     25 Sep 1992  HME / UoE, Starlink.  Lowercase file names. Don't
C                  fold the ARC type. Change declarations with
C                  MAX/MIN to *. Call PGPOINT with last argument
C                  integer ICHAR('X'), not 'X' etc. No longer
C                  PRINT/DELETE arctemp.lis. Remove test for ] and .
C                  in ARFILE. PGASK commented out. Fix conflicting
C                  use of PAR_QNUM: was called though declared
C                  logical, now assign to GIVEN. Use FIG_WXYFIT
C                  instead of WXYFIT.
C                  No longer support binary arc files (.arcbin);
C                  open .arc file with FIG_OPFILE rather than using
C                  FIGARO_PROG_? at this level in an OPEN statement.
C                  Treat status from LIB$sth as integer instead of
C                  logical.
C     26 Oct 1992  HME / UoE, Starlink.  Removed the "type *"
C                  statements.
C     09 Nov 1992  HME / UoE, Starlink.  Changed file extension .dst
C                  to lowercase. If MONITOR, use soft device, not
C                  'PGPLOT'.
C     25 Jan 1993  HME / UoE, Starlink.  Put PGASK back in.
C     18 May 1993  HME / UoE, Starlink.  There were rather long
C                  strings in some "type *" statements. So they
C                  cannot be written into STRING*78 but must be
C                  passed to PAR_WRUSER direct.
C                  Also write 'echelle correctly into plot title.
C                  And replace backslashes with CHAR(92).
C                  Also allow 4-digit integers for numbers of lines
C                  reported as found or rejected.
C     21 Jul 1993  HME / UoE, Starlink.  Use "echarcmenu" help file,
C                  since it differs slightly form "arcmenu".  Use
C                  DSA_*_LU to get free Fortran unit.
C     26 Jul 1993  HME / UoE, Starlink.  Disuse GKD_*.
C                  Use PAR_ABORT, disuse PAR_Q* and
C                  PAR_RDUSER. Added parameters CONTINUE, HLINEMAP,
C                  HATLAS, ORDPPAG, ANALYSIS, QUITSEL, DISNCHAN,
C                  MOVETOX, LINEOK, CMD, LINENO, WAVELEN, RESOLVE,
C                  CHFACT, SIGFACT, HARDARC, HARDISP.
C                  Disuse LIB$* and STR$*.
C     03 Aug 1993  HME / UoE, Starlink.  Convert to DSA.
C     14 Jan 1994  HME / UoE, Starlink.  If ARCTYPE folds to 'NONE',
C                  use the folded string. This is to make 'none',
C                  'None' etc. acceptable under Unix, where (F)PAR
C                  does not fold strings.
C     16 Feb 1995  HME / UoE, Starlink. In the big workspace move
C                  the DOUBLE workspace to the front. Otherwise the
C                  odd number of FLOAT workspaces combined with an
C                  odd number of channels in the input spectrum
C                  cause the DOUBLE workspace to be misaligned
C                  (memory address and odd multiple of 4).
C     18 Apr 1995  HME / UoE, Starlink. No longer use NAG for
C                  polynomial fit. No longer get one big workspace,
C                  but one for each array. Makes the code more
C                  comprehensible. Also get workspaces with
C                  DSA_GET_WORK_ARRAY (by type and size) rather than
C                  DSA_GET_WORKSPACE (by number of bytes). F2PTR
C                  workspace is now 4*NX+33 instead of 3*NX. This is
C                  for DPOLFIT (to replace E02ADF), which needs one
C                  workspace of NX to return fit evaluation and one
C                  workspace of 3*NX+3*(MAXDEG+1) to work with and
C                  return coefficients. (These two are acquired as
C                  one chunk of workspace at this level.) MAXDEG is
C                  10.
C     19 Jul 1995  Allow ECH_ARGETL to cause abortion if arlines.ech
C                  cannot be opened for output.
C     15 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Input is read-only.
C                  Add output parameter. The input image data were
C                  mapped for update, but used read-only, so the access
C                  mode could be changed. When it comes to putting out
C                  the fitted wavelengths, they go either in their own
C                  image, or a copy of input is made and given the
C                  fitted wavelengths.
C     18 Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                  file names to 132 chars.
C     2005 June 1  MJC / Starlink.  Use CNF_PVAL for pointers to mapped
C                  data.
C     2005 Aug 15  TIMJ / JACH. NAME= should be FILE= in Fortran standard
C     2008 Apr 21  MJC / Starlink.  Handle pointer arithmetic for 64-bit
C                  use.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters -  array sizes for arc lines and identified lines
C                   available logical unit number.
C
      INTEGER LU,NC,NLARCS,NLMAX,INTMAX,IREC
      PARAMETER (LU=2,IREC=3)
      PARAMETER (NC=11,NLARCS=5000,NLMAX=5000,INTMAX=30)
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_FOLD,ICH_LEN,ICH_VERIF
      INTEGER PGBEGIN
      REAL    GEN_ELEMF
C
C     Local variables
C
      LOGICAL QUIT,
     :GIVEN,PREV
      LOGICAL MONITOR,DOWAVES
      LOGICAL ISNEW
      INTEGER IBYTES,NFIT,FMPTR,FWPTR,FOPTR,NCFILL,NELM,NYSUB,I
      INTEGER INTERACT,NINTERACTIVE,IORDPTR,ISTAT
      INTEGER IWTSPTR,IMLDPTR,JORDPTR,FITSPTR,RCHAR,NARCS,ARCPTR
      INTEGER BYTES,FORDER,IGNORE,INVOKE,IOUT,DIMS(2)
      INTEGER LLAB,NDIM,NLID,NSTAT,NX,NCOEFF
      INTEGER WPTR,WPTR2,XPTR,YPTR,IPTR,ZPTR,I2ZPTR,CLASS(NLMAX)
      INTEGER CORDR,IORDR,FORDR,LORDR,DI,NORDERS,ORDER(NLMAX),STATUS
      INTEGER XDPTR,OPTR,O2PTR
      INTEGER SLOT
      INTEGER WXPTR,WZPTR,WWPTR,F1PTR,F2PTR,F3PTR,F4PTR,F5PTR
      REAL    CHANS(NLMAX),WAVES(NLMAX),FSIGMA,SIGMA,PARMS(2)
      REAL    ARC1(NLARCS),ARC2(NLARCS),ARC3(NLARCS),VALUE
      REAL    WEIGHTS(NLMAX)
      REAL    ORDMIN,ORDMAX,RMS,SVRMS,AP
      DOUBLE PRECISION COEFFS(NC)
      DOUBLE PRECISION DITEMS(1)
      CHARACTER REPLY*16,STR3*3
      CHARACTER ARCS*132,ARFILE*132,DEVICE*32,ARCTST*132
      CHARACTER XLAB*32,XUNITS*16,XLABEL*16
      CHARACTER ZLAB*32,ZUNITS*16,ZLABEL*16,YUNITS*16,YLABEL*16
      CHARACTER YLABELU*16
      CHARACTER*64 CITEMS(2)
C
C     Output logical unit
C
      DATA IOUT/1/
C
C     Open DSA
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
C
C     Get SOFT device name
C
      CALL VAR_GETCHR('SOFT',0,0,DEVICE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('No graphics device specified',STATUS)
         CALL PAR_WRUSER('Use "SOFT" command eg "SOFT VT" to rectify.'
     :                                                    ,STATUS)
         GO TO 500
      END IF
C
C     Get file for collapsed echelle arc image.
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get value of ARCTYPE and read the files
C     If ARCS folds to NONE, we should set it NONE.
C
      CALL PAR_RDCHAR('ARCTYPE',' ',ARCS)
      IF (PAR_ABORT()) GO TO 500
      ARCTST=ARCS
      IGNORE=ICH_FOLD(ARCTST)
      IF (ARCTST.EQ.'NONE') ARCS=ARCTST
      CALL ECH_AREAD(ARCS,NLARCS,LU,ARC1,ARC2,ARC3,NARCS,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of data
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('Input data file is not an image',STATUS)
         GO TO 500
      END IF
      NX=DIMS(1)
      NORDERS=DIMS(2)
C
C     Try for .X information
C
      CALL DSA_GET_AXIS_INFO('IMAGE',1,2,CITEMS,1,DITEMS,STATUS)
      IF (STATUS.NE.0) GO TO 500
      XLABEL=CITEMS(2)
      XUNITS=CITEMS(1)
C
C     Try for .Y information
C
      CALL DSA_GET_AXIS_INFO('IMAGE',2,2,CITEMS,1,DITEMS,STATUS)
      IF (STATUS.NE.0) GO TO 500
      YLABEL=CITEMS(2)
      YUNITS=CITEMS(1)
C
C     Make sure Y.DATA is order numbers before we map it
C
      YLABELU=YLABEL
      INVOKE=ICH_FOLD(YLABELU)
      IF (YLABELU(1:12).NE.'ORDER NUMBER') THEN
         CALL PAR_WRUSER(YLABEL,STATUS)
         CALL PAR_WRUSER('.Y.LABEL above not order number',STATUS)
         GO TO 500
      END IF
C
C     Map the .Y data
C
      CALL DSA_MAP_AXIS_DATA('IMAGE',2,'READ','FLOAT',
     :   YPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map the .Z data
C     Changed from update to read, wasn't actually updated.
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get Z data information (units and label)
C
      CALL DSA_GET_DATA_INFO('IMAGE',2,CITEMS,1,DITEMS,STATUS)
      IF (STATUS.NE.0) GO TO 500
      ZLABEL=CITEMS(2)
      ZUNITS=CITEMS(1)
C
C     Get the axis labels from the labels and units
C
      LLAB=ICH_LEN(XLABEL)
      IF (LLAB.GT.0) THEN
         XLAB=XLABEL(:LLAB)//' '//XUNITS
      ELSE
         XLAB=XUNITS
      END IF
      LLAB=ICH_LEN(ZLABEL)
      IF (LLAB.GT.0) THEN
         ZLAB=ZLABEL(:LLAB)//' '//ZUNITS
      ELSE
         ZLAB=ZUNITS
      END IF
C
C     Check the command keyword MONITOR. If MONITOR=.TRUE., intermediate
C     results are plotted on the device 'PGPLOT'.
C
      CALL PAR_RDKEY('MONITOR',.FALSE.,MONITOR)
      IF (PAR_ABORT()) GO TO 500
C
C     Get workspace for the dispersion plots (two real arrays, each NX long)
C     and for the X array and Z array (both real, NX long), and for a
C     double precision X array XDPTR (NX long).
C
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',  WPTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',  WPTR2,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',  XPTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',  ZPTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',XDPTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Initialise the channel number and wavelength arrays, and
C     open output ARLINES.ECH file (UNIT=IOUT)
C
      CALL PAR_RDKEY('PREVIOUS',.FALSE.,PREV)
      IF (PREV) THEN
         CALL PAR_SDCHAR('ARFILE','arlines.ech',STATUS)
         CALL PAR_RDCHAR('ARFILE','arlines.ech',ARFILE)
      END IF
      IF (PAR_ABORT()) GO TO 500
      CALL ECH_ARGETL(IOUT,ARFILE,NLMAX,PREV,
     :                ORDER,CHANS,WAVES,WEIGHTS,CLASS,
     :                   FSIGMA,FORDER,NLID,ISTAT)
      IF (ISTAT.NE.0) GO TO 500
C
C     Get the order numbers to be fit interactively ...
C
      CALL PAR_RDVAL('INTERACTIVE',3.,FLOAT(INTMAX),3.,' ',VALUE)
      IF (PAR_ABORT()) GO TO 500
      NINTERACTIVE=VALUE
      IBYTES=4*NINTERACTIVE*4+NORDERS*4
      CALL DSA_GET_WORKSPACE(IBYTES,IORDPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DYN_INCAD(IORDPTR,'FLOAT',NINTERACTIVE,IWTSPTR,ISNEW,STATUS)
      CALL DYN_INCAD(IWTSPTR,'FLOAT',NINTERACTIVE,IMLDPTR,ISNEW,STATUS)
      CALL DYN_INCAD(IMLDPTR,'FLOAT',NINTERACTIVE,JORDPTR,ISNEW,STATUS)
      CALL DYN_INCAD(JORDPTR,'INT',NINTERACTIVE,FITSPTR,ISNEW,STATUS)

      FORDR=GEN_ELEMF(%VAL(CNF_PVAL(YPTR)),1)
      LORDR=GEN_ELEMF(%VAL(CNF_PVAL(YPTR)),NORDERS)
      IF (FORDR.LE.LORDR) THEN
         DI=1
         ORDMIN=FLOAT(FORDR)
         ORDMAX=FLOAT(LORDR)
      ELSE
         DI=-1
         ORDMIN=FLOAT(LORDR)
         ORDMAX=FLOAT(FORDR)
      END IF

      CALL ECH_ORINIT(NINTERACTIVE,FORDR,LORDR,%VAL(CNF_PVAL(IORDPTR)))
      CALL PAR_RDARY('ORDERS',ORDMIN,ORDMAX,'None','Order #',
     :               NINTERACTIVE,INTMAX,%VAL(CNF_PVAL(IORDPTR)))
      IF (PAR_ABORT()) GO TO 500
C
C     Delete all lines that were read from the ARLINES.ECH file and which
C     are not in orders to be fit interactively. This is because they are
C     not at present used in the fit and to leave them there would cause
C     problems when bodily shifting an existing fit.
C
      DO I = 1,NORDERS
         CORDR=GEN_ELEMF(%VAL(CNF_PVAL(YPTR)),I)
         IORDR=CORDR+1
         INTERACT=1
         DO WHILE (IORDR.NE.CORDR.AND.INTERACT.LE.NINTERACTIVE)
            IORDR=GEN_ELEMF(%VAL(CNF_PVAL(IORDPTR)),INTERACT)
            INTERACT=INTERACT+1
         END DO
         IF (IORDR.NE.CORDR) THEN
            CALL ECH_ARDELE(CORDR,0,NLMAX,ORDER,CHANS,WAVES,WEIGHTS,
     :                                                   CLASS,NLID)
         END IF
      END DO
C
C     Get the initial values for the number of coefficients of the fit
C     & arc line width.
C
      IF (PREV) THEN
         CALL PAR_SDVAL('ORDERFIT',FLOAT(FORDER),STATUS)
         CALL PAR_SDVAL('SIGMA',FSIGMA,STATUS)
      END IF
      CALL PAR_RDVAL('ORDERFIT',0.,FLOAT(INTMAX),5.,' ',VALUE)
      NCOEFF=VALUE+1
      CALL PAR_RDVAL('SIGMA',0.01,100.,2.,' ',SIGMA)
C
C     Determine whether to create a WAVES file or to create a 2D .X.DATA
C     file.
C
      CALL PAR_RDKEY('DOWAVES',.FALSE.,DOWAVES)
      IF (PAR_ABORT()) GO TO 500
C
C     If requested, get output file to be created.
C
      IF (DOWAVES) THEN
         CALL PAR_SDCHAR('WAVES','waves',STATUS)
         CALL DSA_OUTPUT('WAVES','WAVES','IMAGE',0,0,STATUS)
         CALL DSA_COERCE_DATA_ARRAY('WAVES','DOUBLE',2,DIMS,STATUS)
         CALL DSA_MAP_DATA('WAVES','WRITE','DOUBLE',OPTR,SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
C
C     If a WAVES file was not requested, create an IMAGE.X.DATA structure
C     instead (delete it if it already existed).
C
      ELSE
         CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
         CALL DSA_DELETE_AXIS('OUTPUT',1,STATUS)
         CALL DSA_COERCE_AXIS_DATA('OUTPUT',1,'DOUBLE',2,DIMS,STATUS)
         CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'WRITE','DOUBLE',
     :      OPTR,SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
C
C        Create .X data structures .LABEL="Wavelength" and .UNITS="Angstroms"
C
         CITEMS(1)='Angstroms'
         CITEMS(2)='Wavelength'
         CALL DSA_SET_AXIS_INFO('OUTPUT',1,2,CITEMS,1,0D0,STATUS)
         IF (STATUS.NE.0) GO TO 500
      END IF
C
C     Initialise the autofit parameter array
C
      CALL ARC_ARAINT(PARMS)
C
C     Fill the single precision X array, which is the one used throughout
C     the main part of the program, with the numbers 1..NX
C
      CALL GEN_NFILLF(NX,%VAL(CNF_PVAL(XPTR)))
C
C     Number of BYTES in a single row (order) of image ...
C
      BYTES=NX*4
C
C     This is the main loop through INTMAX orders identifying lines
C         interactively
C
      REPLY='                '
      QUIT=.FALSE.
      INTERACT=1
      SVRMS=0.0
      ISNEW = .FALSE.
      DO WHILE ((INTERACT.LE.NINTERACTIVE).AND.(.NOT.QUIT))
C
C        Set integer variable IORDR equal to the next order number
C           chosen by the user ...
C
         IORDR=GEN_ELEMF(%VAL(CNF_PVAL(IORDPTR)),INTERACT)
C
C        Initialise the SOFT graphics device
C
         STATUS=PGBEGIN(LU,DEVICE,1,1)
         IF (STATUS.NE.1) THEN
            CALL PAR_WRUSER('Unable to open graphics device',STATUS)
            GO TO 500
         END IF
C
C        Set I2ZPTR to point to IORDR row of image, and copy row of
C           image beginning at I2ZPTR over to ZPTR workspace
C
         CALL DYN_INCAD(IPTR,'FLOAT',NX*DI*(IORDR-FORDR),I2ZPTR,ISNEW,
     :                  STATUS)
         CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(I2ZPTR)),
     :                 %VAL(CNF_PVAL(ZPTR)))
         IF (ISNEW) CALL CNF_UNREGP(I2ZPTR)
C
C        Identify all lines within this order using ARPLOT,ARSLCT,ARMENU
C
         CALL ECH_ARINTR(%VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(ZPTR)),
     :                   XLAB,ZLAB,NX,NC,COEFFS,NCOEFF,PARMS,SIGMA,
     :                   IORDR,NLMAX,ORDER,CHANS,WAVES,CLASS,NLID,
     :                   NLARCS,ARC1,ARC2,ARC3,ARCS,IOUT,AP,
     :                   WEIGHTS,%VAL(CNF_PVAL(WPTR)),
     :                   %VAL(CNF_PVAL(WPTR2)),RMS)


         IF (PAR_ABORT()) GO TO 500
C
C        Give user option to CONTINUE [default] on to next order,
C           repeat the order just finished, backup to the start and
C           begin again, or quit pre-maturely...
C
         CALL PAR_WRUSER(' ',STATUS)
         CALL PAR_WRUSER(
     :    '* DONE WITH INTERACTIVE FIT TO THAT ORDER - OPTIONS NOW:',
     :                                                      STATUS)
         CALL PAR_WRUSER(' ',NSTAT)
 111     REPLY='                '
         CALL PAR_CNPAR('CONTINUE')
         CALL PAR_RDCHAR('CONTINUE','Continue',REPLY)
         IF (PAR_ABORT()) GO TO 500
         CALL PAR_WRUSER(' ',NSTAT)
C
C        Test for null reply while converting reply found to upper case, then
C           identify first non-blank character of reply
C
         RCHAR=ICH_FOLD(REPLY)
         IF (RCHAR.EQ.0) THEN
            RCHAR=1
            REPLY(RCHAR:RCHAR)=' '
         ELSE
            RCHAR=ICH_VERIF(REPLY,1,' ')
         END IF
C
         IF ((REPLY(RCHAR:RCHAR).EQ.' ').OR.
     :       (REPLY(RCHAR:RCHAR).EQ.'C')) THEN
C
C           Compute the double precision X array, and copy it over to
C              the output image ....
C
            CALL ARC_ARSETXD(NX,COEFFS,NCOEFF,%VAL(CNF_PVAL(XDPTR)))
C
C           Set O2PTR to point to IORDR row of image, and copy XDPTR data
C              over to OPTR image beginning at I2ZPTR.
C
            CALL DYN_INCAD(OPTR,'FLOAT',NX*DI*(IORDR-FORDR)*2,O2PTR,
     :                     ISNEW,STATUS)
            CALL GEN_MOVE(BYTES*2,%VAL(CNF_PVAL(XDPTR)),
     :                    %VAL(CNF_PVAL(O2PTR)))
            IF (ISNEW) CALL CNF_UNREGP(O2PTR)
C
C           ...and now continue normally
C
            SVRMS=SVRMS+RMS/AP
            INTERACT=INTERACT+1
C
         ELSE IF (REPLY(RCHAR:RCHAR).EQ.'R') THEN
C
C           Loop will go back without incrementing
C
         ELSE IF (REPLY(RCHAR:RCHAR).EQ.'S') THEN
C
C           Start over again
C
            SVRMS=0.0
            INTERACT=1
C
         ELSE IF (REPLY(RCHAR:RCHAR).EQ.'Q') THEN
C
C           Set QUIT flag true
C
            QUIT=.TRUE.
C
         ELSE
C
C           Have user repeat input
C
            CALL PAR_WRUSER(' '//REPLY(1:1)//
     :                        ' *** Not matched to one of the choices',
     :                                                           NSTAT)
            CALL PAR_WRUSER(' ',NSTAT)
            CALL PAR_WRUSER(' ***** The choices are as follows: *****',
     :                                                           NSTAT)
            CALL PAR_WRUSER(' ',NSTAT)
            CALL PAR_WRUSER('       C(ontinue) on to next order, etc.',
     :                                                           NSTAT)
            CALL PAR_WRUSER('       R(epeat) order just completed    ',
     :                                                           NSTAT)
            CALL PAR_WRUSER('       S(tart) over again w/ first order',
     :                                                           NSTAT)
            CALL PAR_WRUSER('       Q(uit) pre-maturely without fits ',
     :                                                           NSTAT)
            CALL PAR_WRUSER(' ',NSTAT)
            GOTO 111
C
         END IF
C
      END DO
C
C     Did we get here pre-maturely ??
C
      IF (QUIT) THEN
         GO TO 500
      END IF
C
      SVRMS=SVRMS/FLOAT(NINTERACTIVE)
C
C     Now we have NINTERACTIVE rows of %VAL(OPTR) filled with wavelengths;
C        below we fill in the rest with rather good guesses ...
C
      CALL ECH_ARFLAG(%VAL(CNF_PVAL(IORDPTR)),NINTERACTIVE,NORDERS,
     :                          FORDR,LORDR,%VAL(CNF_PVAL(FITSPTR)))
C
C     Determine an appropriate number of coefficients to use for initial
C        %VAL(OPTR) filling ...
C
      IF (NINTERACTIVE.LE.3) THEN
         NCFILL=NINTERACTIVE
      ELSE IF (NINTERACTIVE.LE.6) THEN
         NCFILL=NINTERACTIVE-1
      ELSE IF (NINTERACTIVE.LE.10) THEN
         NCFILL=NINTERACTIVE-2
      ELSE
         NCFILL=8
      END IF
C
      CALL ECH_ARFILL(%VAL(CNF_PVAL(OPTR)),NX,NORDERS,FORDR,LORDR,
     :                %VAL(CNF_PVAL(FITSPTR)),NINTERACTIVE,NCFILL,
     :                %VAL(CNF_PVAL(IMLDPTR)),%VAL(CNF_PVAL(IWTSPTR)),
     :                %VAL(CNF_PVAL(JORDPTR)))

C    Deregister pointers within the workspace.
      CALL CNF_UNREGP(IMLDPTR)
      CALL CNF_UNREGP(IWTSPTR)
      CALL CNF_UNREGP(JORDPTR)

C
C     Below we call a utility routine to get a single, sorted ARC array
C        after first getting the workspace for it ...
C
      STATUS=0
C      ABYTES=NARCS*4+3*NX*4+NX*4+3*NX*8+3*NX*8+3*NORDERS*4
C      CALL DSA_GET_WORKSPACE(ABYTES,ARCPTR,SLOT,STATUS)
C      IF (STATUS.NE.0) THEN
C         GO TO 500
C      END IF
C      WXPTR=ARCPTR+NARCS*4
C      WZPTR=WXPTR+NX*4
C      WWPTR=WZPTR+NX*4
C      F1PTR=WWPTR+NX*4
C      F2PTR=F1PTR+NX*4
C      F3PTR=F2PTR+3*NX*8
C      F4PTR=F3PTR+NX*8
C      F5PTR=F4PTR+NX*8
C      FMPTR=F5PTR+NX*8
C      FWPTR=FMPTR+NORDERS*4
C      FOPTR=FWPTR+NORDERS*4
      CALL DSA_GET_WORK_ARRAY(NARCS,   'FLOAT',ARCPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX,      'FLOAT',WXPTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX,      'FLOAT',WZPTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX,      'FLOAT',WWPTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX,      'FLOAT',F1PTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(4*NX+33,'DOUBLE',F2PTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX,     'DOUBLE',F3PTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX,     'DOUBLE',F4PTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX,     'DOUBLE',F5PTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NORDERS, 'FLOAT',FMPTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NORDERS, 'FLOAT',FWPTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NORDERS, 'FLOAT',FOPTR, SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
      CALL ECH_ARCONE(NLARCS,ARC1,ARC2,ARC3,NARCS,
     :                %VAL(CNF_PVAL(ARCPTR)))
C
      NFIT=NINTERACTIVE
C
C     Now with %VAL(OPTR) as a starting point, we can go looking for lines
C        in array %VAL(ARCPTR) and so refine these guesses ...
C
      CALL ECH_ARFIND(%VAL(CNF_PVAL(IPTR)),NX,NORDERS,
     :                %VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(IORDPTR)),
     :                NINTERACTIVE,NC,COEFFS,NCOEFF,PARMS,SIGMA,SVRMS,
     :                NLMAX,ORDER,CHANS,WAVES,CLASS,NLID,NARCS,
     :                %VAL(CNF_PVAL(ARCPTR)),IOUT,FORDR,LORDR,
     :                %VAL(CNF_PVAL(WXPTR)),%VAL(CNF_PVAL(WZPTR)),
     :                %VAL(CNF_PVAL(WWPTR)),WEIGHTS,
     :                %VAL(CNF_PVAL(FITSPTR)),NFIT,FMPTR,FWPTR,FOPTR,
     :                %VAL(CNF_PVAL(F1PTR)),%VAL(CNF_PVAL(F2PTR)),
     :                %VAL(CNF_PVAL(F3PTR)),%VAL(CNF_PVAL(F4PTR)),
     :                %VAL(CNF_PVAL(F5PTR)),DEVICE,MONITOR)

C    Deregister pointer within the workspace.
      CALL CNF_UNREGP(FITSPTR)
C
C     List name of output list file
C
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER(' *** ECHARC autofit completed *** ',STATUS)
      INQUIRE (UNIT=IOUT,NAME=ARFILE)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('Details of fit output to '//
     :                            ARFILE(:ICH_LEN(ARFILE)),STATUS)
C
C     Plot line locations in order to judge whether bad columns and
C        saturated columns were detected as lines
C
      STATUS=PGBEGIN(LU,DEVICE,1,1)
      IF (STATUS.NE.1) THEN
         CALL PAR_WRUSER('Unable to open graphics device',STATUS)
         GO TO 500
      END IF
      CALL ECH_MXPLOT(CHANS,ORDER,NLMAX,NLID,NX,FORDR,LORDR)
C
C     Ask user if about a hardcopy of this arc line map ...
C
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_RDKEY('HLINEMAP',.FALSE.,GIVEN)
      IF (PAR_ABORT()) GO TO 500
      IF (GIVEN) THEN
         CALL ARC_AHOPEN(1,STATUS)
         IF (STATUS.EQ.0) THEN
            CALL ECH_MXPLOT(CHANS,ORDER,NLMAX,NLID,
     :                              NX,FORDR,LORDR)
            CALL PAR_WRUSER('Plot file created',STATUS)
         ELSE
            CALL PAR_WRUSER('Could not open hard copy device',
     :                                                   STATUS)
         END IF
      END IF
C
C     Ask user about a hardcopy atlas of the entire frame, and hope
C        he or she says no, they do not want one (!)
C
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_RDKEY('HATLAS',.FALSE.,GIVEN)
      IF (PAR_ABORT()) GO TO 500
      IF (GIVEN) THEN
         CALL PAR_RDVAL('ORDPPAG',1.,10.,2.,' ',VALUE)
         IF (PAR_ABORT()) GO TO 500
         NYSUB=VALUE
         CALL ARC_AHOPEN(NYSUB,STATUS)
         IF (STATUS.EQ.0) THEN
            DO I=1,NORDERS,1
               IORDR=GEN_ELEMF(%VAL(CNF_PVAL(YPTR)),I)
               CALL DYN_INCAD(IPTR,'FLOAT',NX*DI*(IORDR-FORDR),I2ZPTR,
     :                        ISNEW,STATUS)
               CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(I2ZPTR)),
     :                       %VAL(CNF_PVAL(ZPTR)))
               IF (ISNEW) CALL CNF_UNREGP(I2ZPTR)
               CALL ECH_ATLAS(%VAL(CNF_PVAL(XPTR)),
     :                        %VAL(CNF_PVAL(ZPTR)),XLAB,ZLAB,NX,
     :                        IORDR,NLMAX,ORDER,CHANS,WAVES,
     :                        WEIGHTS,CLASS,NLID)
               REPLY='* order     hard'
               WRITE (STR3,'(I3)',IOSTAT=IGNORE) IORDR
               REPLY(9:11)=STR3
               CALL PAR_WRUSER(REPLY//' plot(s) done * ',IGNORE)
            END DO
            CALL PGEND
            CALL PAR_WRUSER('Arc atlas hard copy file created ',
     :                                                   STATUS)
         ELSE
            CALL PAR_WRUSER('Could not open hard copy device',
     :                                                   STATUS)
         END IF
      END IF
C
C     Ask user about a line by line analysis of the fit
C
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_RDKEY('ANALYSIS',.FALSE.,GIVEN)
      IF (PAR_ABORT()) GO TO 500
      IF (GIVEN) THEN
         OPEN(UNIT=IREC,FILE='echarc.lis',STATUS='NEW',
     :                                    IOSTAT=STATUS)
         IF (STATUS.NE.0) THEN
            CALL PAR_WRUSER('Unable to open ECHARC.LIS file',
     :                                             STATUS)
            GO TO 500
         END IF
C
         CALL ECH_ARRECD(IREC,CHANS,WAVES,WEIGHTS,CLASS,ORDER,
     :                   NLMAX,NLID,FORDR,LORDR,NX,.TRUE.,NCOEFF)
         CLOSE(UNIT=IREC)
C
         CALL PAR_WRUSER(
     :               'Detailed analysis written to ECHARC.LIS',
     :                                                  STATUS)
      END IF
      CALL PAR_WRUSER(' ',STATUS)
C
C     Tidy up
C
  500 CONTINUE
      CLOSE (UNIT=IOUT,IOSTAT=IGNORE)
      CALL DSA_CLOSE(STATUS)
C
      END
