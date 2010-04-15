C+
      SUBROUTINE SPIED
C
C     S P I E D
C
C     SPIketrum EDitor.  Allows the values of a spiketrum to be
C     edited interactively, until the user is satisfied with the
C     data that results from interpolating between them.
C
C     Command Parameters -
C
C     SPIKETRUM    (Character) The name of the spiketrum to be
C                  edited.
C     OUTPUT       (Character) The name of the resulting edited
C                  spiketrum.
C     COLOUR       (Character) The colour to be used for the data
C                  when it is ploted.
C     HIGH         (Real) The maximum Y value for the plot.
C     LOW          (Real) The minimum Y value for the plot.
C     XSTART       (Real) The maximum X value for the plot.
C     XEND         (Real) The minimum X value for the plot.
C     ORDER        (Integer) The order for the polynomial fit.
C
C     Command keywords -
C
C     QUIT         Used to confirm quitting from application.
C
C     User variables used -    (">" input)
C
C     (>) SOFT     (Character) The device/type of the current soft
C                  graphics device.
C
C                                        KS / CIT 23rd May 1984
C     Modified:
C
C     26th Mar 1985  KS / AAO.  Call to FIG_EDSPI and workspace
C                    usage modified to comply with the NAG version
C                    of FIG_ISPIKE.  COLOUR parameter added.
C      6th Aug 1987  DJA/ AAO.  Revised DSA_ routines - some specs
C                    changed. Now uses DYN_ routines for dynamic
C                    memory handling.
C     20th Mar 1988  KS / AAO. Conversion to DSA_ routines completed,
C                    and modified for GKS version of PGPLOT.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     3rd  Sep 1992  HME / UoE, Starlink.  INCLUDE changed. Avoid
C                    GEN_SWOP.  PGASK is banned from ADAM, commented
C                    out.  Changed case of file SPIKETRUM.
C     25th Jan 1993  HME / UoE, Starlink.  Put PGASK back in.
C     27th Jul 1993  HME / UoE, Starlink.  Disuse GKD_* except
C                    GKD_WRITE_LINE. Disuse PAR_Q* and PAR_RDUSER, use
C                    PAR_ABORT. Added parameters QUIT, HIGH, LOW,
C                    XSTART, XEND, ORDER.
C     16th Feb 1995  HME / UoE, Starlink. In the big workspace move
C                    the DOUBLE workspace to the front. Otherwise the
C                    odd number of FLOAT workspaces combined with an
C                    odd number of channels in the input spectrum
C                    cause the DOUBLE workspace to be misaligned
C                    (memory address and odd multiple of 4).
C     18th May 1995  HME / UoE, Starlink. FIG_ISPIKE now needs a longer
C                    work space. Make DWPTR 11*NX+24.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_CLEAN, ICH_FOLD, ICH_KEY
      REAL    GEN_ELEMF
C
C     Local variables
C
      LOGICAL FAULT, REV, REVRL
      INTEGER CKEY, DIMS, DTA_STATUS, DWPTR, IGNORE
      INTEGER INVOKE, NDIM, NEXT
      INTEGER NX, OPTR, SLOT, STATUS, WPTR, WSLOT1, WSLOT2, XPTR
      REAL    ENDS(4), TEMP
      CHARACTER COLOUR*16, NAME*64, PGDEV*64
C
C     Initial values
C
      FAULT = .FALSE.
C
C     Get value of 'SOFT'
C
      CALL VAR_GETCHR('SOFT',0,0,PGDEV,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to get soft device name.',IGNORE)
         CALL PAR_WRUSER('Correct using SOFT command.',IGNORE)
         FAULT = .TRUE.
         GO TO 500           ! Error exit
      END IF
C
C     Initialise DSA_ system
C
      STATUS = 0
      CALL DSA_OPEN (STATUS)
      IF (STATUS.NE.0) GO TO 500     ! Error exit
C
C     Get name of SPIKETRUM and open the file
C
      CALL DSA_INPUT('SPIKE','SPIKETRUM',STATUS)
C
C     Get size of data
C
      CALL DSA_DATA_SIZE('SPIKE',1,NDIM,DIMS,NX,STATUS)
C
C     Get name of output file.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPIKE',0,0,STATUS)
C
C     Map data
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,SLOT,STATUS)
C
C     Map the X data
C
      CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'READ','FLOAT',XPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500       ! Error exit
C
C     Read the spiketrum structure definition
C
      CALL DSA_READ_STRUCT_DEF ('spiketrum',STATUS)
C
C     Look for any 'end' values
C
      CALL DSA_ELEMENT_NAME ('SPIKE','lambda_left',NAME,STATUS)
      CALL DTA_RDVARF(NAME,1,ENDS(1),DTA_STATUS)
      IF (DTA_STATUS.EQ.0) THEN
         CALL DSA_ELEMENT_NAME ('SPIKE','data_left',NAME,STATUS)
         CALL DTA_RDVARF(NAME,1,ENDS(2),DTA_STATUS)
      END IF
      IF (DTA_STATUS.NE.0) THEN
         ENDS(1)=0.
         ENDS(2)=0.
      END IF
      CALL DSA_ELEMENT_NAME ('SPIKE','lambda_right',NAME,STATUS)
      CALL DTA_RDVARF(NAME,1,ENDS(3),DTA_STATUS)
      IF (DTA_STATUS.EQ.0) THEN
         CALL DSA_ELEMENT_NAME ('SPIKE','data_right',NAME,STATUS)
         CALL DTA_RDVARF(NAME,1,ENDS(4),DTA_STATUS)
      END IF
      IF (DTA_STATUS.NE.0) THEN
         ENDS(3)=0.
         ENDS(4)=0.
      END IF
      STATUS = 0
C
C     The FIG_EDSPI algorithm requires that the data be in ascending
C     order of X.  The easiest way to do this is to swop it round if
C     necessary.
C
      REV=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),1).GT.
     :    GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),NX)
      IF (REV) THEN
         CALL GEN_REV2D(%VAL(CNF_PVAL(XPTR)),NX,1,.TRUE.,
     :                  %VAL(CNF_PVAL(XPTR)))
         CALL GEN_REV2D(%VAL(CNF_PVAL(OPTR)),NX,1,.TRUE.,
     :                  %VAL(CNF_PVAL(OPTR)))
      END IF
      REVRL=ENDS(1).GT.ENDS(3)
      IF (REVRL) THEN
         TEMP = ENDS(1)
         ENDS(1) = ENDS(3)
         ENDS(3) = TEMP
         TEMP = ENDS(2)
         ENDS(2) = ENDS(4)
         ENDS(4) = TEMP
      END IF
C
C     Get the colour for the plot (note that BLACK and BLUE are reversed
C     in the colour list, so BL will be taken as BLUE - the codes are
C     then reversed to give the correct PGPLOT colour code)
C
      CALL PAR_RDCHAR('COLOUR','White',COLOUR)
      IF (PAR_ABORT()) GO TO 500
      INVOKE=ICH_FOLD(COLOUR)
      INVOKE=ICH_CLEAN(COLOUR)
      CKEY=ICH_KEY(COLOUR,1,',; ',
     :      'BLUE:WHITE:RED:GREEN:BLACK:CYAN:MAGENTA:YELLOW:',
     :      'Abbr.',NEXT)-1
      IF (CKEY.EQ.0) THEN
         CKEY=4
      ELSE IF (CKEY.EQ.4) THEN
         CKEY=0
      END IF
      IF (CKEY.LT.0) THEN
         CALL PAR_WRUSER('Cannot colour the plot '//COLOUR,IGNORE)
         CKEY=1
      END IF
C
C     Get workspace (needed for the fitting routines)
C
      CALL DSA_GET_WORK_ARRAY(NX*11+24,'DOUBLE',DWPTR,WSLOT1,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',WPTR,WSLOT2,STATUS)
      IF (STATUS.NE.0) GO TO 500       ! Error exit
C
C     Edit the spiketrum
C
      CALL FIG_EDSPI(PGDEV,NX,%VAL(CNF_PVAL(XPTR)),CKEY,
     :               %VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(DWPTR)),
     :               ENDS,%VAL(CNF_PVAL(OPTR)))
      CALL DSA_FREE_WORKSPACE(WSLOT2,STATUS)
      CALL DSA_FREE_WORKSPACE(WSLOT1,STATUS)
      IF (PAR_ABORT()) GO TO 500
C
C     Reverse back if necessary
C
      IF (REV) THEN
         CALL GEN_REV2D(%VAL(CNF_PVAL(XPTR)),NX,1,.TRUE.,
     :                  %VAL(CNF_PVAL(XPTR)))
         CALL GEN_REV2D(%VAL(CNF_PVAL(OPTR)),NX,1,.TRUE.,
     :                  %VAL(CNF_PVAL(OPTR)))
      END IF
      IF (REVRL) THEN
         TEMP = ENDS(1)
         ENDS(1) = ENDS(3)
         ENDS(3) = TEMP
         TEMP = ENDS(2)
         ENDS(2) = ENDS(4)
         ENDS(4) = TEMP
      END IF
C
C     Now we have the edited spiketrum, we have to do some tidying up
C     in the .TABLE structure.  The end values held in ENDS may
C     be new, changed, or removed.  Any data in the .DATA.TABLES object
C     will be out-dated and may as well be deleted - this may introduce
C     a loss of precision, but since the data has been mucked about
C     with anyway, this shouldn't matter.
C
C     (Note in the following code, that the possibility of attempting
C     to delete objects that don't exist, or to create existing objects,
C     is covered simply by not testing the condition codes..)
C
      CALL DSA_ELEMENT_NAME ('OUTPUT','table_data',NAME,STATUS)
      CALL DTA_DLVAR(NAME,DTA_STATUS)
      IF (ENDS(1).EQ.0.) THEN
         CALL DSA_ELEMENT_NAME ('OUTPUT','lambda_left',NAME,STATUS)
         CALL DTA_DLVAR(NAME,DTA_STATUS)
         CALL DSA_ELEMENT_NAME ('OUTPUT','data_left',NAME,STATUS)
         CALL DTA_DLVAR(NAME,DTA_STATUS)
      ELSE
         CALL DSA_ELEMENT_NAME ('OUTPUT','lambda_left',NAME,STATUS)
         CALL DTA_CRVAR(NAME,'FLOAT',STATUS)
         CALL DTA_WRVARF(NAME,1,ENDS(1),STATUS)
         CALL DSA_ELEMENT_NAME ('OUTPUT','data_left',NAME,STATUS)
         CALL DTA_CRVAR(NAME,'FLOAT',STATUS)
         CALL DTA_WRVARF(NAME,1,ENDS(2),STATUS)
      END IF
      IF (ENDS(3).EQ.0.) THEN
         CALL DSA_ELEMENT_NAME ('OUTPUT','lambda_right',NAME,STATUS)
         CALL DTA_DLVAR(NAME,DTA_STATUS)
         CALL DSA_ELEMENT_NAME ('OUTPUT','data_right',NAME,STATUS)
         CALL DTA_DLVAR(NAME,DTA_STATUS)
      ELSE
         CALL DSA_ELEMENT_NAME ('OUTPUT','lambda_right',NAME,STATUS)
         CALL DTA_CRVAR(NAME,'FLOAT',STATUS)
         CALL DTA_WRVARF(NAME,1,ENDS(3),STATUS)
         CALL DSA_ELEMENT_NAME ('OUTPUT','data_right',NAME,STATUS)
         CALL DTA_CRVAR(NAME,'FLOAT',STATUS)
         CALL DTA_WRVARF(NAME,1,ENDS(4),STATUS)
      END IF
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      IF (FAULT) CALL FIG_SETERR
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_EDSPI (PGDEV,NX,XDATA,CKEY,WORK,DWORK,ENDS,DATA)
C
C     F I G _ E D S P I
C
C     Allows the user to interactively edit a spiketrum, so as to get
C     a satisfactory interpolation between its points.
C
C     Parameters -   (">" input, "!" modified, "W" workspace)
C
C     (>) PGDEV     (Character) The device/type spec for the current
C                   soft graphics device (PGPLOT format, eg TTA0:/VT)
C     (>) NX        (Integer) Number of elements in the spiketrum (not
C                   including the end elements in ENDS)
C     (>) XDATA     (Real array XDATA(NX)) X values for centers of data
C                   elements)
C     (>) CKEY      (Integer) The GRPCKG code for the colour to be used
C                   for the data.
C     (W) WORK      (Real array WORK(NX)) Workspace.
C     (W) DWORK     (Double precision array DWORK(11*NX+24)) Workspace.
C     (!) ENDS      (Real array ENDS(4)) In case there are values known
C                   to the left and right of the X values given in XDATA,
C                   these may be specified in ENDS.  ENDS(1) gives an X
C                   value that would precede XDATA(1), ENDS(3) an X value
C                   that would follow XDATA(NX), and ENDS(2) and (4) are
C                   the corresponding data values.  If such values are
C                   not available, the ENDS(1) and/or ENDS(3) should be
C                   set to zero.
C     (!) DATA      (Real array DATA(NX)) The spiketrum data.
C
C     Note that this routine assumes that ENDS(1)<ENDS(3), and that
C     XDATA is in ascending order.
C
C     Common variables used -  None
C
C     Functions / subroutines used -
C
C     FIG_ISPIKE   (FIG_ package)  Fit spline or polynomial to spiketrum
C     FIG_SLFIND   ( "     "    )  Find nearest spiketrum point to value
C     GEN_BSEARCH  (GEN_   "    )  Given value, get nearest array element
C     GEN_MOVE     ( "     "    )  Fast copy of bytes array -> array
C     GEN_RANGEF   ( "     "    )  Get range of values in array
C     ICH_FOLD     (ICH_   "    )  Convert string to upper case
C     GKD_WRITE_LINE  ( "     "    )  Output character string to user
C     PGBEGIN      (PGPLOT) Initialise plotting routines
C     PGBIN        (   "  ) Plot histogram of array
C     PGCURSE      (   "  ) Get cursor position and character
C     PGENV        (   "  ) Set plot environment (axes etc.)
C     PGLINE       (   "  ) Draw series of vectors between points
C     PGSCI        (   "  ) Set line colour
C     PGSLW        (   "  ) Set line intensity
C     PGSLS        (   "  ) Set line style
C
C                                             KS / CIT 24th May 1984
C     Modified:
C
C     26th March 1985.  KS / AAO.  Call to FIG_ISPIKE and workspace
C                       usage modified to comply with the NAG version
C                       of FIG_ISPIKE.  WORK1 removed from call to this
C                       routine, and size of DWORK modified.  CKEY
C                       parameter added, together with ability to
C                       display results of a linear fit.
C     20th March 1988.  KS / AAO.  Modified for GKS version of PGPLOT
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, CKEY
      REAL    XDATA(NX), WORK(NX), ENDS(4), DATA(NX)
      DOUBLE PRECISION DWORK(11*NX+24)
      CHARACTER*(*) PGDEV
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER GEN_BSEARCH, ICH_FOLD, PGBEGIN
C
C     Local variables
C
      LOGICAL GOOD, MORE, REPLOT, RETRY
      INTEGER CMPTAB(0:7), INVOKE, N1, N2, NELM, NORD, ORDER, STATUS
      REAL    HIGH, HORIG, HWAS, LORIG, LOW, LWAS, VALUE, X(2), XDMAX
      REAL    XDMIN, XMAORG, XMAWAS, XMAX, XMIORG, XMIWAS, XMIN, XPOSN
      REAL    Y(2), YPOSN
      CHARACTER CHR*1, ENDER1*64, ENDER2*64
C
C     Range limits (close to VAX real limits)
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)
C
C     PGPLOT codes for line intensities and types
C
      INTEGER BLACK, DOTTED, FULL, WHITE
      PARAMETER (BLACK=0, DOTTED=4, FULL=1, WHITE=1)
C
C     Sundry flags:  X values are for center of data, fits are not to
C                    be to log data, linear end interpolation not used.
C
      LOGICAL CENTER, LINEND, LOGFIT
      PARAMETER (CENTER=.TRUE., LINEND=.FALSE., LOGFIT=.FALSE.)
C
C     Complementary colour codes for the various PGPLOT colours
C
      DATA CMPTAB/1,1,5,6,7,2,3,4/
C
      DATA ENDER1
     :      /'You can only have one point in each of the end zones'/
      DATA ENDER2
     :      /'Delete the current end point and insert this one again'/
C
C     Open graphics device
C
      STATUS=PGBEGIN(0,PGDEV,1,1)
      IF (STATUS.NE.1)  GO TO 600
      CALL PGASK(.FALSE.)
C
C     Determine plot limits
C
      XDMIN=XDATA(1)
      XDMAX=XDATA(NX)
      XMIN=XDMIN-(XDMAX-XDMIN)*.1
      XMAX=XDMAX+(XDMAX-XDMIN)*.1
      CALL GEN_RANGEF(DATA,1,NX,HIGH,LOW)
      IF (ENDS(1).NE.0.) THEN
         LOW=MIN(LOW,ENDS(2))
         HIGH=MAX(HIGH,ENDS(2))
         XMIN=MIN(XMIN,ENDS(1))
      END IF
      IF (ENDS(3).NE.0.) THEN
         LOW=MIN(LOW,ENDS(4))
         HIGH=MAX(HIGH,ENDS(4))
         XMAX=MAX(XMAX,ENDS(3))
      END IF
C
C     Remember oriinal scale values (will be needed by 'W' command)
C
      HORIG=HIGH
      LORIG=LOW
      XMAORG=XMAX
      XMIORG=XMIN
C
C     Set flag to force plot to be made on first time through loop.
C
      REPLOT=.TRUE.
C
C     Initial order for polynomial fits, initial cursor position
C
      ORDER=10
      XPOSN=(XMIN+XMAX)*.5
      YPOSN=(HIGH+LOW)*.5
C
C     Following loop is executed until the user picks the Quit option
C
      MORE=.TRUE.
      DO WHILE (MORE)
C
C        Do we re-display the data?
C
         IF (REPLOT) THEN
C
C           Yes.  Set up the plot environment (axes, etc)
C
            CALL PGSCI(WHITE)
            CALL PGENV(XMIN,XMAX,LOW,HIGH,0,1)
            X(1)=XDMIN
            X(2)=XDMIN
            Y(1)=LOW
            Y(2)=HIGH
            CALL PGSLS(DOTTED)
            CALL PGLINE(2,X,Y)
            X(1)=XDMAX
            X(2)=XDMAX
            CALL PGLINE(2,X,Y)
            CALL PGSLS(FULL)
C
C           Display spiketrum and end values
C
            CALL PGSCI(CKEY)
            CALL PGBIN(NX,XDATA,DATA,.TRUE.)
            IF (ENDS(1).NE.0.) THEN
               X(1)=ENDS(1)
               X(2)=ENDS(1)
               Y(1)=0.
               Y(2)=ENDS(2)
               CALL PGLINE(2,X,Y)
            END IF
            IF (ENDS(3).NE.0.) THEN
               X(1)=ENDS(3)
               X(2)=ENDS(3)
               Y(1)=0.
               Y(2)=ENDS(4)
               CALL PGLINE(2,X,Y)
            END IF
C
            CALL PAR_WRUSER('"D" to delete, "I" to insert,'//
     :                       ' "Q" to quit, "?" or "H" for help',STATUS)
C
            REPLOT=.FALSE.
         END IF
C
C        Get cursor position and command letter
C
         CALL PGCURSE(XPOSN,YPOSN,CHR)
         INVOKE=ICH_FOLD(CHR)
C
C        Following code is a CASE construct, the cases being selected
C        on the character input by the user.
C
         IF (CHR.EQ.'Q') THEN
C
C           'Q' is for Quit
C
            CALL PAR_CNPAR('QUIT')
            CALL PAR_RDKEY('QUIT',.FALSE.,MORE)
            IF (PAR_ABORT()) GO TO 600
            MORE=.NOT.MORE
C
         ELSE IF ((CHR.EQ.'?').OR.(CHR.EQ.'H')) THEN
C
C           '?' and 'H' are both for 'Help'
C
            CALL FIG_SPEHLP
            REPLOT=.TRUE.
C
         ELSE IF (CHR.EQ.'Y') THEN
C
C           'Y' is for 'change the scaling in Y'
C
            HWAS=HIGH
            LWAS=LOW
            RETRY=.TRUE.
            DO WHILE(RETRY)
               CALL PAR_CNPAR('HIGH')
               CALL PAR_RDVAL('HIGH',FMIN,FMAX,HWAS,' ',HIGH)
               IF (PAR_ABORT()) GO TO 600
               CALL PAR_CNPAR('LOW')
               CALL PAR_RDVAL('LOW',FMIN,HIGH,LWAS,' ',LOW)
               IF (PAR_ABORT()) GO TO 600
               IF (HIGH.EQ.LOW) THEN
                  CALL GKD_WRITE_LINE(
     :                        'Cannot have both values the same')
               ELSE
                  RETRY=.FALSE.
               END IF
            END DO
            REPLOT=(HIGH.NE.HWAS).OR.(LOW.NE.LWAS)
C
         ELSE IF (CHR.EQ.'X') THEN
C
C           'X' is for 'change the scaling in X'
C
            XMIWAS=XMIN
            XMAWAS=XMAX
            RETRY=.TRUE.
            DO WHILE(RETRY)
               CALL PAR_CNPAR('XEND')
               CALL PAR_RDVAL('XEND',FMIN,FMAX,XMAWAS,' ',XMAX)
               IF (PAR_ABORT()) GO TO 600
               CALL PAR_CNPAR('XSTART')
               CALL PAR_RDVAL('XSTART',FMIN,XMAX,XMIWAS,' ',XMIN)
               IF (PAR_ABORT()) GO TO 600
               IF (XMIN.EQ.XMAX) THEN
                  CALL GKD_WRITE_LINE(
     :                        'Cannot have both values the same')
               ELSE
                  RETRY=.FALSE.
               END IF
            END DO
            REPLOT=(XMIN.NE.XMIWAS).OR.(XMAX.NE.XMAWAS)
C
         ELSE IF (CHR.EQ.'W') THEN
C
C           'W' is for 'display Whole spiketrum'
C
            IF ((HIGH.NE.HORIG).OR.(LOW.NE.LORIG).OR.(XMIN.NE.XMIORG)
     :                .OR.(XMAX.NE.XMAORG)) THEN
               HIGH=HORIG
               LOW=LORIG
               XMIN=XMIORG
               XMAX=XMAORG
               REPLOT=.TRUE.
            END IF
C
         ELSE IF (CHR.EQ.'D') THEN
C
C           'D' is for 'Delete nearest spike'
C
            CALL FIG_SLFIND(XPOSN,NX,XDATA,ENDS,DATA,NELM)
            IF (NELM.GT.0) THEN
               N1=MAX(1,NELM-2)
               N2=MIN(NX,NELM+2)
               CALL PGSCI(BLACK)
               CALL PGBIN(N2-N1+1,XDATA(N1),DATA(N1),CENTER)
               DATA(NELM)=0.
               CALL PGSCI(WHITE)
               CALL PGBIN(N2-N1+1,XDATA(N1),DATA(N1),CENTER)
            ELSE IF (NELM.LT.0) THEN
               Y(1)=0.
               IF (NELM.EQ.-1) THEN
                  X(1)=ENDS(1)
                  X(2)=ENDS(1)
                  Y(2)=ENDS(2)
                  ENDS(1)=0.
               ELSE
                  X(1)=ENDS(3)
                  X(2)=ENDS(3)
                  Y(2)=ENDS(4)
                  ENDS(3)=0.
               END IF
               CALL PGSCI(BLACK)
               CALL PGLINE(2,X,Y)
               CALL PGSCI(WHITE)
            END IF
C
         ELSE IF (CHR.EQ.'I') THEN
C
C           'I' is for 'Insert point at current position'
C
            GOOD=.FALSE.
            IF (XPOSN.LT.XDATA(1)) THEN
               IF (ENDS(1).NE.0.) THEN
                  CALL GKD_WRITE_LINE(ENDER1)
                  CALL GKD_WRITE_LINE(ENDER2)
               ELSE
                  ENDS(1)=XPOSN
                  ENDS(2)=YPOSN
                  GOOD=.TRUE.
               END IF
            ELSE IF (XPOSN.GT.XDATA(NX)) THEN
               IF (ENDS(3).NE.0) THEN
                  CALL GKD_WRITE_LINE(ENDER1)
                  CALL GKD_WRITE_LINE(ENDER2)
               ELSE
                  ENDS(3)=XPOSN
                  ENDS(4)=YPOSN
                  GOOD=.TRUE.
               END IF
            ELSE
               NELM=GEN_BSEARCH(XDATA,NX,XPOSN)
               IF (NELM.GT.0) THEN
                  DATA(NELM)=YPOSN
                  GOOD=.TRUE.
               END IF
            END IF
            IF (GOOD) THEN
               X(1)=XPOSN
               X(2)=XPOSN
               Y(1)=0.
               Y(2)=YPOSN
               CALL PGLINE(2,X,Y)
            END IF
C
         ELSE IF (CHR.EQ.'R') THEN
C
C           'R' is for Redraw spiketrum
C
            REPLOT=.TRUE.
C
         ELSE IF ((CHR.EQ.'L').OR.(CHR.EQ.'P').OR.(CHR.EQ.'S')) THEN
C
C           'L' is for 'interpolate Linearly and display result'
C
C           'P' is for 'fit Polynomial and display result'
C
C           'S' is for 'interpolate using Splines and display result'
C
            CALL GEN_MOVE(NX*4,DATA,WORK)
            IF (CHR.EQ.'L') THEN
               CALL GKD_WRITE_LINE('Linear interpolation')
               CALL FIG_LSPIKE (NX,XDATA,ENDS,WORK)
            ELSE
               IF (CHR.EQ.'S') THEN
                  NORD=-1
                  CALL GKD_WRITE_LINE('Spline fit')
               ELSE
                  CALL PAR_CNPAR('ORDER')
                  CALL PAR_RDVAL('ORDER',1.,10.,FLOAT(ORDER),' ',VALUE)
                  IF (PAR_ABORT()) GO TO 600
                  ORDER=VALUE
                  NORD=ORDER
               END IF
               CALL FIG_ISPIKE (NX,XDATA,ENDS,LOGFIT,LINEND,NORD,NX,
     :                                                DWORK,WORK,STATUS)
            END IF
            CALL PGSCI(CMPTAB(CKEY))
            CALL PGBIN(NX,XDATA,WORK,CENTER)
            CALL PGSCI(CKEY)
C
         ELSE
C
C           All other characters are ignored
C
         END IF
C
      END DO
      CALL PGSCI(WHITE)
C
  600 CONTINUE
      CALL PGEND
      END
C+
      SUBROUTINE FIG_SPEHLP
C
C     F I G _ S P E H L P
C
C     Outputs help information for the SPIED command
C
C     Parameters -  None
C
C     Common variables -  None
C
C     Functions / subroutines used -
C
C     PAR_WRUSER     (PAR_    "   ) Output string to user
C     GKD_CLEAR_ALPHA( "      "   ) Clear terminal screen
C     PGADVANCE    (PGPLOT) Erase screen
C
C                                            KS / CIT 24th May 1984
C     Modified:
C
C     26th March 1985.  KS / AAO.  'L' command added.  Minor mods to
C                       text of some lines.
C     20th March 1988.  KS / AAO.  Modified for GKS version of PGPLOT.
C+
      IMPLICIT NONE
C
C     Local variables
C
      INTEGER STATUS
      CHARACTER*16 STRING
C
C     Clear screen.
C
      CALL PGADVANCE
C
C     Output help info
C
      CALL PAR_WRUSER('Note that a spiketrum may have end points that'
     :                                                       ,STATUS)
      CALL PAR_WRUSER('are outside the normal range of the data.  The'
     :                                                       ,STATUS)
      CALL PAR_WRUSER('end of the data is shown by the dotted lines.'
     :                                                       ,STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('Use cursor to indicate points to be changed',
     :                                                       STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('Command letters recognised are - ',STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('?  - output this Help information',STATUS)
      CALL PAR_WRUSER('H  -    "    "     "       "',STATUS)
      CALL PAR_WRUSER('I  - Insert a new point as indicated',
     :                                                       STATUS)
      CALL PAR_WRUSER('D  - Delete point nearest cursor',STATUS)
      CALL PAR_WRUSER(
     :   'P  - show global Polynomial fit to points (cf SPIFIT)',
     :                                                       STATUS)
      CALL PAR_WRUSER('S  - show Spline fit to points (cf INTERP)'
     :                                                      ,STATUS)
      CALL PAR_WRUSER(
     :   'L  - show linear interpolation between points (cf LINTERP)',
     :                                                       STATUS)
      CALL PAR_WRUSER('X  - change plotted limits in X',STATUS)
      CALL PAR_WRUSER('Y  - change plotted limits in Y',STATUS)
      CALL PAR_WRUSER('W  - display Whole spiketrum range',STATUS)
      CALL PAR_WRUSER('R  - Redraw plot',STATUS)
      CALL PAR_WRUSER('Q  - Quit and write out modified points',
     :                                                       STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('Hit the return key to continue editing',
     :                                                       STATUS)
C
      END
C+
      SUBROUTINE FIG_SLFIND (XPOSN,NX,XDATA,ENDS,DATA,NELM)
C
C     F I G _ S L F I N D
C
C     Finds the nearest point in a spiketrum to a given X value.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) XPOSN    (Real) The given X value
C     (>) NX       (Integer) The number of elements in the spiketrum
C     (>) XDATA    (Real array XDATA(NX)) X values for the elements
C     (>) ENDS     (Real array ENDS(4)) In case there are values known
C                  to the left and right of the X values given in XDATA,
C                  these may be specified in ENDS.  ENDS(1) gives an X
C                  value that would precede XDATA(1), ENDS(3) an X value
C                  that would follow XDATA(NX), and ENDS(2) and (4) are
C                  the corresponding data values.  If such values are
C                  not available, the ENDS(1) and/or ENDS(3) should be
C                  set to zero.
C     (>) DATA     (Real array DATA(NX)) The spiketrum data
C     (<) NELM     (Integer) The number of the nearest element.  If the
C                  nearest element is actually that given by ENDS(1),
C                  NELM is set to -1.  If it is that given by ENDS(3),
C                  it is set to -2.  If for some reason no line can be
C                  found, it is set to 0.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     GEN_BSEARCH  (GEN_ package) Given value, find nearest array element
C
C     Note that this routine assumes that ENDS(1)<ENDS(3), and that
C     XDATA is in ascending order.
C
C                                               KS / CIT 24th May 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NELM
      REAL    XPOSN, XDATA(NX), ENDS(4), DATA(NX)
C
C     Functions
C
      INTEGER GEN_BSEARCH
C
C     Local variables
C
      INTEGER IPTR, ND, NP, NSTART
      REAL    DELD, DELP
C
C     Make rough guess, based on X value, for start of search
C
      NSTART=GEN_BSEARCH(XDATA,NX,XPOSN)
      IF (NSTART.EQ.0) THEN
         IF (XPOSN.LT.XDATA(1)) THEN
            NSTART=0
         ELSE
            NSTART=NX+1
         END IF
      END IF
C
C     Search up through array for first point  (Note that the WHILE
C     loop will not be executed at all if IPTR>NX, but if IPTR=0
C     IPTR will be set to 1 to prevent going outside array bounds.
C     These considerations are necessary if the selected point is in
C     the end zones.)
C
      IPTR=NSTART
      DO WHILE (IPTR.LE.NX)
         IPTR=MAX(1,MIN(NX,IPTR))
         IF (DATA(IPTR).NE.0.) THEN
            DELP=XDATA(IPTR)-XPOSN
            NP=IPTR
            GO TO 320
         END IF
         IPTR=IPTR+1
      END DO
      IF (ENDS(3).NE.0.) THEN
         NP=-2
         DELP=ENDS(3)-XPOSN
      ELSE
         NP=0
      END IF
  320 CONTINUE
C
C     And down
C
      IPTR=NSTART
      DO WHILE (IPTR.GE.1)
         IPTR=MAX(1,MIN(NX,IPTR))
         IF (DATA(IPTR).NE.0.) THEN
            DELD=XPOSN-XDATA(IPTR)
            ND=IPTR
            GO TO 330
         END IF
         IPTR=IPTR-1
      END DO
      IF (ENDS(1).NE.0.) THEN
         ND=-1
         DELD=XPOSN-ENDS(1)
      ELSE
         ND=0
      END IF
  330 CONTINUE
C
C     We now have up to 2 candidates, one from up and one from down
C     If we have only one, take that, else take nearest of the two.
C
      IF (ND.EQ.0) THEN
         NELM=NP
      ELSE
         IF (NP.EQ.0) THEN
            NELM=ND
         ELSE
            IF (DELD.LT.DELP) THEN
               NELM=ND
            ELSE
               NELM=NP
            END IF
         END IF
      END IF
C
      END
