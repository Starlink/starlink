C+
      SUBROUTINE SCLEAN
C
C     S C L E A N
C
C     Main routine for the Figaro 'SCLEAN' command.  Displays
C     a CCD image and then allows the user to move around it with
C     the cursor, selecting rows and columns to be corrected and
C     cosmic rays to be zapped.  The idea is that this routine can
C     be used to fix up any areas in an image that were not fixed
C     automatically by the non-interactive version ('BCLEAN').  It
C     may also give a better idea of the best settings for the
C     BCLEAN parameters.  A mode is provided especially suited for
C     examination of SCUBA data files.
C
C     The task works internally with a quality array, setting the
C     bit specified by the parameter BITNUM to mark pixels as bad,
C     but if the input NDF uses flagged bad values and no quality
C     array, the output NDF will represent quality information in
C     the same way.
C
C     Variances are propagated, but if any changes are made to the
C     data array, the corresponding element of the variance array
C     is set to zero.  This is intended to mark it as a bad value,
C     since Figaro does not support explicitly flagged bad values
C     in the variance array.  If this has been done, the user is
C     warned at the end of processing.
C
C     Command parameters -
C
C     IMAGE      (Character) The name of the image to be displayed.
C     OUTPUT     (Character) The name of the resulting cleaned image.
C     QUIT       (Logical) Used to confirm quitting the application.
C     DEG        (Integer) Degree of fit to use for interpolation.
C     XSIZE      (Integer) Size of smoothing box in X.
C     YSIZE      (Integer) Size of smoothing box in Y.
C     HIGH       (Real) Highest displayed data value.
C     LOW        (Real) Lowest displayed data value.
C     BITNUM     (Integer) Number of quality bit to modify.
C     ZOOM       (Integer) Minimum pixel zoom factor.
C     LOGFILE    (Character) The name of a file to log to.  If null,
C                no logging is performed.
C     BATCHFILE  (Character) The name of a file (in SCLEAN log file
C                format) to draw batch input from.  If null (default),
C                run in interactive mode.
C
C     User variables -  ("<" output, "!" modified)
C
C     (!) IMARRAY (Numeric array) Contains current image display
C                 parameters.
C     (<) IMFILE  (Character) Contains name of currently displayed
C                 image file.
C     (>) IDEV    (Character) Contains name of display device.
C
C                                        MBT / IoA 29th July 1998
C     Modified:
C
C     26th Aug  1985.  KS / AAO.  TVZOOM call commented out.  This
C                      seems to be able to introduce an offset when used
C                      with the ARGS.
C     10th Aug  1987.  DJA/ AAO.  Revised DSA_ routines - some specs
C                      changed. Now uses DYN_ package for dynamic memory
C                      handling.
C     17th Dec  1987.  KS / AAO.  'Position' option added, 'Undo' option
C                      finally implemented.
C     7th Sept  1988.  KS / AAO.  Modified for increased resolution
C                      displays. No longer assumes display is 512x512.
C                      Needs a version of TVPCKG that supports TVSIZE.
C     8th Jan   1992.  HME / UoE, Starlink. No longer map input data,
C                      but map output data for update. Otherwise we are
C                      working on an all-zero array.
C     6th Apr   1993.  HME / UoE, Starlink. Conversion to PGPLOT. REDRAW
C                      keyword removed. This would still assume old
C                      meaning of IMARRAY. Support bad values. Use IDEV
C                      device.
C     10th May  1993.  HME / UoE, Starlink. When background colour
C                      unavailable, use foreground colour for bad
C                      pixels.
C     26th Jul  1993.  HME / UoE, Starlink. Disuse PAR_Q*. Use PAR_ABORT.
C     18th Jul  1996.  MJCL / Starlink, UCL.  Set variables for storage
C                      of file names to 132 chars.
C     29th Jul  1998.  MBT / IoA, Starlink. Use quality internally
C                      instead of flags for bad pixels, and additional
C                      display mode for SCUBA data. Name changed from
C                      CLEAN to SCLEAN.
C     2005 May 31      MJC / Starlink Use CNF_PVAL for pointers to mapped
C                      data.
C+
      IMPLICIT NONE
C
C     Global Constants
C
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard HDS constants
      INCLUDE 'FIO_ERR'          ! Stati returned by FIO_
      INCLUDE 'PAR_ERR'          ! Stati returned by PAR_
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_LEN,PGBEG
C
C     Local variables
C
      REAL         ARRAY(12)    ! Display parameters
      LOGICAL      BATCH        ! Whether to run from a batch file
      INTEGER      BFD          ! FIO file descriptor of batch file
      INTEGER      BITNUM       ! Number of bit to set for bad quality
      LOGICAL      CHANGE       ! Data may have been modified since mapping
      INTEGER      COL1,COL2    ! First and last PGPLOT pen
      CHARACTER    DEVNAM*32    ! Name of display device
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      INTEGER      FSTAT        ! Figaro running status
      INTEGER      IGNORE       ! Ignored dummy argument
      CHARACTER    IMAGE*132    ! The actual name of the image to be cleaned
      INTEGER      INDF         ! Identifier for input NDF
      INTEGER      LENG         ! Length of STRING
      LOGICAL      LOG          ! Whether to log to file
      INTEGER      LFD          ! FIO file descriptor of log file
      INTEGER      MULT         ! Multiplier for size of work array
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      NX           ! Size of 1st dimension
      INTEGER      NY           ! Size of 2nd dimension (if present)
      INTEGER      DPTR         ! Dynamic-memory pointer to output data array
      INTEGER      ONDF         ! Identifier for output NDF
      INTEGER      PGSTAT       ! Status for PGPLOT call
      LOGICAL      PLOT         ! Whether to plot to PGPLOT device
      LOGICAL      QEXIST       ! Existence of quality array in input data
      BYTE         QMASK        ! Bad bit mask
      INTEGER      QPTR         ! Dynamic-memory pointer to quality array
      INTEGER      STATUS       ! Global status
      CHARACTER*70 STRING       ! Character buffer
      LOGICAL      VEXIST       ! Existence of variance array in input data
      INTEGER      VPTR         ! Dynamic-memory pointer to variance array
      CHARACTER    WLOC*(DAT__SZLOC) ! Locator for work temp array
      INTEGER      WPTR         ! Dynamic-memory pointer to workspace
      INTEGER      WSIZE        ! Number of elements in workspace

C
C     Initialisation of Figaro running status
C
      FSTAT=0
C
C     Initialisation of NDF routines
C
      STATUS=SAI__OK
      CALL NDF_BEGIN(STATUS)
C
C     Open input NDF
C
      CALL NDF_ASSOC('IMAGE','READ',INDF,STATUS)
C
C     Create a new NDF for output
C
      CALL NDF_PROP(INDF,'Data,Variance,Quality,Axis,Units',
     :              'OUTPUT',ONDF,STATUS)
C
C     Check if quality array exists and create one if necessary.
C     Note this has to be done before mapping the data array.
C
      CALL NDF_STATE(ONDF,'Quality',QEXIST,STATUS)
      IF (QEXIST) THEN
         CALL NDF_MAP(ONDF,'Quality','_UBYTE','UPDATE',QPTR,NELM,STATUS)
      ELSE
         CALL NDF_MAP(ONDF,'Quality','_UBYTE','WRITE/ZERO',QPTR,NELM,
     :                STATUS)
      END IF
C
C     Map data for output NDF, and assert that it starts off unchanged
C
      CALL NDF_MAP(ONDF,'Data','_REAL','UPDATE',DPTR,NELM,STATUS)
      CHANGE=.FALSE.
C
C     Check if variance array exists, and map it if it does.
C
      CALL NDF_STATE(ONDF,'Variance',VEXIST,STATUS)
      IF (VEXIST)
     :   CALL NDF_MAP(ONDF,'Variance','_REAL','UPDATE',VPTR,NELM,STATUS)
C
C     If there are bad bits, put that information into the quality array.
C     This way, the rest of the routines used only need to worry about
C     the quality array and not about magic values.
C
      CALL FIG_SCL_FLAG2QUAL(%VAL(CNF_PVAL(DPTR)),%VAL(CNF_PVAL(QPTR)),
     :                       NELM,STATUS)
C
C     Get dimensions of data
C
      CALL NDF_DIM(ONDF,2,DIMS,NDIM,STATUS)
      NX=DIMS(1)
      NY=DIMS(2)
C
C     The required workspace is only used for the Undo function, and may
C     be any size (must be more than 12 elements).  The size controls the
C     number of operations that may be Undone.  Here it is set at some
C     multiple of the larger of the image dimensions.
C
      IF (VEXIST) THEN
         MULT=3
      ELSE
         MULT=2
      END IF
      WSIZE=MULT*12*MAX(NX,NY)
      DIMS(1)=WSIZE
      CALL DAT_TEMP('_REAL',1,DIMS,WLOC,STATUS)
      CALL DAT_MAP(WLOC,'_REAL','WRITE',1,DIMS,WPTR,STATUS)
C
C     Open log file if LOGFILE parameter is not null
C     By opening for 'APPEND' and then (if all is well) rewinding we
C     effectively open for write but without generating an error if
C     the file exists.
C
      IF (STATUS.NE.SAI__OK) GO TO 500
      LOG=.FALSE.
      CALL FIO_ASSOC('LOGFILE','APPEND','LIST',0,LFD,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
         CALL ERR_ANNUL(STATUS)
      ELSE IF (STATUS.EQ.SAI__OK) THEN
         CALL FIO_RWIND(LFD,STATUS)
         LOG=.TRUE.
C
C        Write header containing input and output NDF names
C
         CALL NDF_MSG('IMAGE_NAME',INDF)
         CALL MSG_LOAD('IMAGE_LOG','# IMAGE:  ^IMAGE_NAME',
     :                 STRING,LENG,STATUS)
         CALL FIO_WRITE(LFD,STRING(:LENG),STATUS)
         CALL NDF_MSG('OUTPUT_NAME',ONDF)
         CALL MSG_LOAD('OUTPUT_LOG','# OUTPUT: ^OUTPUT_NAME',
     :                 STRING,LENG, STATUS)
         CALL FIO_WRITE(LFD,STRING(:LENG),STATUS)
      ELSE
         GO TO 500
      END IF
C
C     Open batch file if BATCHFILE parameter is not null
C
      IF (STATUS.NE.SAI__OK) GO TO 500
      BATCH=.FALSE.
      CALL FIO_ASSOC('BATCHFILE','READ','LIST',0,BFD,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
         CALL ERR_ANNUL(STATUS)
      ELSE IF (STATUS.EQ.SAI__OK) THEN
         BATCH=.TRUE.
      ELSE
         GO TO 500
      END IF
C
C     Open display device
C
      IF (STATUS.NE.SAI__OK) GO TO 500
      IF (BATCH) CALL PAR_DEF0C('IDEV',' ',STATUS)
      CALL PAR_GET0C('IDEV',DEVNAM,STATUS)
      IF (STATUS.EQ.PAR__NULL.OR.ICH_LEN(DEVNAM).EQ.0) THEN
         PLOT=.FALSE.
         CALL ERR_ANNUL(STATUS)
      ELSE
         PGSTAT=PGBEG(0,DEVNAM(:ICH_LEN(DEVNAM))//'/append',1,1)
         PLOT=PGSTAT.EQ.1
      END IF
      IF (.NOT.PLOT .AND. .NOT.BATCH) THEN
         CALL PAR_WRUSER('Failed to open image display device',FSTAT)
         GO TO 500
      END IF
      IF (PLOT) CALL PGASK(.FALSE.)
C
C     Find out the number of colours.
C
      IF (PLOT) THEN
         CALL PGQCOL(COL1,COL2)
         IF (COL1.NE.0) THEN
            CALL PAR_WRUSER( 'Cannot use background colour to display '
     :         // 'bad pixels, will use foreground colour instead.',
     :         FSTAT )
         ELSE IF (COL2.LE.17) THEN
            CALL PAR_WRUSER('SCLEAN: Error: Insufficient colours '//
     :         'available for display.', FSTAT)
            GO TO 500
         END IF
      END IF
C
C     Get number of bit to set in quality array for bad values.
C
      CALL PAR_GET0I('BITNUM',BITNUM,STATUS)
C
C     Make sure the bad bits mask is set appropriately in output NDF.
C
      CALL NDF_BB(ONDF,QMASK,STATUS)
      CALL FIG_SETBIT(QMASK,BITNUM,.TRUE.,STATUS)
      CALL NDF_SBB(QMASK,ONDF,STATUS)
C
C     Perform the interactive or batch cleaning.
C
      CALL FIG_CSCLEAN(%VAL(CNF_PVAL(DPTR)),%VAL(CNF_PVAL(QPTR)),
     :                      %VAL(CNF_PVAL(VPTR)),VEXIST,NX,NY,BITNUM,
     :                      .TRUE.,%VAL(CNF_PVAL(WPTR)),WSIZE,ARRAY,
     :                      COL1,COL2,BATCH,BFD,LOG,LFD,PLOT,CHANGE,
     :                      STATUS)
C
C     Set user variable IMARRAY to reflect current display parameters
C     (local error context is used so that an error here does not
C     inhibit the rest of the closedown sequence).
C
      IF (PLOT .AND. STATUS.EQ.SAI__OK) THEN
         CALL ERR_MARK
         CALL NDF_MSG('IMAGE_NAME',INDF)
         CALL MSG_LOAD('IMAGE_NAME','^IMAGE_NAME',IMAGE,IGNORE,STATUS)
         CALL VAR_SETCHR('IMFILE',0,0,IMAGE,STATUS)
         CALL VAR_SETARY('IMARRAY',12,ARRAY,STATUS)
         CALL ERR_RLSE
         STATUS=SAI__OK
      END IF
C
C     If the original NDF had no quality array (used flagged values
C     instead) then write the quality info back as flagged values and
C     remove the quality array.
C
      IF (.NOT.QEXIST) THEN
         CALL FIG_SCL_QUAL2FLAG(%VAL(CNF_PVAL(DPTR)),
     :                          %VAL(CNF_PVAL(QPTR)),NELM,STATUS)
         CALL NDF_UNMAP(ONDF,'Quality',STATUS)
         CALL NDF_RESET(ONDF,'Quality',STATUS)
      END IF
C
C     Since what we've done to the variance component (used 0.0 to flag
C     bad values) may be surprising to a non-Figaro user, inform the
C     user if we have done this.
C
      IF (VEXIST.AND.CHANGE) CALL PAR_WRUSER(
     :   'Zero has been used to flag bad values in the Variance array.',
     :   FSTAT)
C
C     Tidy up
C
  500 CONTINUE
C
C     Close down display
C
      IF (PLOT) CALL PGEND
C
C     Close log file
C
      IF (LOG) CALL FIO_ANNUL(LFD,STATUS)
C
C     Close batch file
C
      IF (BATCH) CALL FIO_ANNUL(BFD,STATUS)
C
C     End NDF context
C
      CALL NDF_END(STATUS)
C
      END
C+
      SUBROUTINE FIG_CSCLEAN (DATA,QUAL,VAR,VEXIST,NX,NY,BITNUM,REDRAW,
     :                        WORK,WSIZE,ARRAY,BADVAL,COLHI,
     :                        BATCH,BFD,LOG,LFD,PLOT,CHANGE,STATUS)
C
C     F I G _ C S C L E A N
C
C     This is the subroutine that does most of the work for SCLEAN.
C     It displays the image on the display device, and allows the
C     user to walk around it with the cursor, indicating bad pixels
C     and bad rows.
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (!) DATA      (Real array IMAGE(NX,NY)) The image data to be
C                   cleaned up.
C     (!) QUAL      (Byte array QUAL(NX,NY)) The quality array
C                   corresponding to DATA.
C     (!) VAR       (Real array VAR(NX,NY)) The variance array
C                   corresponding to DATA.
C     (>) VEXIST    (Logical) True if VAR contains values
C     (>) NX        (Integer) The first dimension of IMAGE
C     (>) NY        (Integer) The second dimension of IMAGE
C     (>) BITNUM    (Integer) Number of the bit to be set by the routine
C                   to mark a quality byte as bad.
C     (>) REDRAW    (Logical) True if default display parameters
C                   are to be determined and used.  If false, the
C                   values in ARRAY should be used.
C     (W) WORK      (Real array WORK(WSIZE)) Workspace.  This is used
C                   to remember operations for the Undo command, and
C                   may be any size - the larger it is, the more
C                   operations may be Undone.
C     (>) WSIZE     (Integer) The number of elements in WORK.
C     (!) ARRAY     (Real array ARRAY(12)) Returned with the display
C                   parameters for the final display.  These should
C                   be, in order, IYST(first y element), IYEN(last y
C                   element), IXST(1st x element), IXEN(last x element),
C                   NBLOCKX(x-blocking factor), NBLOCKY(y-blocking
C                   factor), IXWID(x-stretch factor), IYWID(y-stretch
C                   factor), IXORIG(image display x-start), IYORIG(image
C                   display y-start), HIGH (maximum displayed value),
C                   LOW (minimum displayed value).  If REDRAW is false,
C                   these should be passed as the current values.
C     (>) BADVAL    (Integer) The PGPLOT pen to be used for bad pixels.
C     (>) COLHI     (Integer) The last PGPLOT pen available.
C     (>) BATCH     (Logical) True if commands are from a batch file,
C                   otherwise interactive mode.
C     (>) BFD       (Integer) FIO file descriptor of batch file if BATCH.
C     (>) LOG       (Logical) True if logging commands to file.
C     (>) LFD       (Integer) FIO file descriptor of log file if LOG.
C     (>) PLOT      (Logical) True if there is a PGPLOT display device.
C     (!) CHANGE    (Logical) True if Data array may have been modified.
C                   Note *not* affected by modifying the Quality array.
C     (!) STATUS    (Integer) The inherited status.
C
C     Common variables used - None
C
C                                            KS / CIT 2nd July 1984
C     Modified:
C
C     19th July 1985.  KS / AAO.  Test for cursor outside data area added.
C     16th July 1986.  KS / AAO.  Test for same values after 'S' modified.
C     17th Dec  1987.  KS / AAO.  'P' option added, 'U' option implemented.
C     7th  Sept 1988.  KS / AAO.  Uses TVSIZE to get display dimensions.
C     8th  Mar  1993.  HME / UoE, Starlink. Conversion to PGPLOT.
C     26th Jul  1993.  HME / UoE, Starlink. Disuse PAR_Q*. Use PAR_ABORT.
C     16th Jan  1997.  BKM / RAL, Starlink. Initialise STATUS before use.
C     29th Jul  1998.  MBT / IoA, Starlink. Use quality array not flags,
C                                   new display mode 'B'.
C     2005 May 31      MJC / Starlink Use CNF_PVAL for pointers to mapped
C                      data.
C+
      IMPLICIT NONE
C
C     Global constants
C
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! Stati returned by FIO_
      INCLUDE 'PAR_ERR'          ! Stati returned by PAR_
      INCLUDE 'DAT_PAR'          ! Standard HDS constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      LOGICAL REDRAW,BATCH,LOG,PLOT,CHANGE,VEXIST
      INTEGER NX,NY,WSIZE,BITNUM,BFD,LFD
      REAL    ARRAY(12), DATA(NX,NY), VAR(NX,NY), WORK(WSIZE)
      BYTE    QUAL(NX,NY)
      INTEGER BADVAL
      INTEGER COLHI,STATUS
C
C     Functions
C
      LOGICAL GEN_SIMILAR,PAR_ABORT
      INTEGER ICH_ENCODE,ICH_LEN,ICH_FOLD,ICH_NUMBR
C
C     Maximum number of points for 'X' and 'Y' fits
C
      INTEGER MAXPT
      PARAMETER (MAXPT=35)
C
C     Pen for minimum value
C
      INTEGER MINVAL
      PARAMETER (MINVAL=16)
C
C     Local variables
C
      LOGICAL BADX, BADY, BADPOSN, DONE, REDISPLAY, BITVAL
      INTEGER IMX, IMY, INVOKE, IX, IX1, IX2, IXEN, CPOS
      INTEGER IXORIG, IXST, IXWID
      INTEGER IY, IY1, IY2, IYEN
      INTEGER IYORIG, IYST, IYWID, LIMX1, LIMX2, LIMY1, LIMY2, NBLOCKX
      INTEGER NBLOCKY, NBOXX, NBOXY, NCOEFF, NEXT, LENG
      INTEGER NXP, NYP, WFIRST, WLAST
      INTEGER XMAX, XMIN, X, Y, XCENT, YCENT, YMAX, YMIN
      INTEGER XCURR, XHALF, YHALF
      INTEGER DEVX, DEVY, PIXX, PIXY, NPT, NARG, DIMS(2)
      REAL AMAX, AMIN, HIGH, HWAS, LOW, LWAS, MEAN, SIGMA
      REAL SIZE, TOTAL, VALUE, XWRK(MAXPT), ZWRK(MAXPT)
      REAL XCH, YCH, BORD, BYTICK, ARG1, ARG2
      CHARACTER CURMESS*70, BATMESS*70, CHR, STRING*80, MODE
      CHARACTER*(DAT__SZLOC) ILOC,XLOC,YLOC
      INTEGER IPTR,XPTR,YPTR
      INTEGER ZOOM,FSTAT
      REAL RXPOSN,RYPOSN
      REAL WINDOW(4)
C
C     Check inherited status
C
      IF (STATUS.NE.SAI__OK) RETURN
C
C     Initialise some variables
C
      XCENT=0
      YCENT=0
      CURMESS='** Cursor outside data area'
C
C     Get initial values for degree of fit and box dimensions for
C     'X' and 'Y' cleaning.
C
      CALL PAR_RDVAL('DEG',0.,7.,2.,' ',VALUE)
      IF (PAR_ABORT()) GO TO 500
      NCOEFF=VALUE+1
      CALL PAR_RDVAL('XSIZE',1.,20.,5.,'Pixels',VALUE)
      IF (PAR_ABORT()) GO TO 500
      NBOXX=VALUE
      CALL PAR_RDVAL('YSIZE',1.,20.,5.,'Pixels',VALUE)
      IF (PAR_ABORT()) GO TO 500
      NBOXY=VALUE
      CALL PAR_RDVAL('ZOOM',1.,24.,6.,'Pixels',VALUE)
      IF (PAR_ABORT()) GO TO 500
      ZOOM=INT(VALUE)
C
C     Get an integer work space the same size as the input data,
C     and two real ones for doing the graph plot in 'B' mode.
C
      DIMS(1)=NX*NY
      CALL DAT_TEMP('_INTEGER',1,DIMS,ILOC,STATUS)
      CALL DAT_MAP(ILOC,'_INTEGER','WRITE',1,DIMS,IPTR,STATUS)
      DIMS(1)=NY
      CALL DAT_TEMP('_REAL',1,DIMS,XLOC,STATUS)
      CALL DAT_TEMP('_REAL',1,DIMS,YLOC,STATUS)
      CALL DAT_MAP(XLOC,'_INTEGER','WRITE',1,DIMS,XPTR,STATUS)
      CALL DAT_MAP(YLOC,'_INTEGER','WRITE',1,DIMS,YPTR,STATUS)
      IF (STATUS.NE.SAI__OK) GO TO 500
C
      IF (PLOT) THEN
C
C        Get display size
C
         CALL PGSVP(0.,1.,0.,1.)
         CALL PGQVP(3,WINDOW(1),WINDOW(2),WINDOW(3),WINDOW(4))
         DEVX=INT(WINDOW(2)-WINDOW(1))+1
         DEVY=INT(WINDOW(4)-WINDOW(3))+1
C
C        Set (sometimes-used) border size to two character heights
C
         CALL PGQCS(0,XCH,YCH)
         BORD=YCH*2.0
C
C        If image is to be redrawn, then get the range of the data,
C        for the first scaling guess.
C
         IF (REDRAW) THEN
            CALL GEN_ASTATQ(DATA,QUAL,NX,NY,1,NX,1,NY,TOTAL,AMAX,AMIN,
     :                       MEAN,XMAX,XMIN,YMAX,YMIN,SIGMA,SIZE)
            HIGH=MEAN+SIGMA
            LOW=MEAN-SIGMA
            MODE='W'
            REDISPLAY=.TRUE.
         ELSE
            IYST=ARRAY(1)
            IYEN=ARRAY(2)
            IXST=ARRAY(3)
            IXEN=ARRAY(4)
            NBLOCKX=ARRAY(5)
            NBLOCKY=ARRAY(6)
            IXWID=ARRAY(7)
            IYWID=ARRAY(8)
            IXORIG=ARRAY(9)
            IYORIG=ARRAY(10)
            HIGH=ARRAY(11)
            LOW=ARRAY(12)
            NXP=IXEN-IXST+1
            NYP=IYEN-IYST+1
            IF (IXWID.EQ.1) THEN
               IMX=NXP/NBLOCKX
            ELSE
               IMX=NXP*IXWID
            END IF
            IF (IYWID.EQ.1) THEN
               IMY=NYP/NBLOCKY
            ELSE
               IMY=NYP*IYWID
            END IF
            REDISPLAY=.FALSE.
            IF ((IXST.EQ.1).AND.(IXEN.EQ.NX)
     :         .AND.(IYST.EQ.1).AND.(IYEN.EQ.NY)) MODE='W'
         END IF
C
C        Repair scaling limits if equal.
C
         IF (LOW.EQ.HIGH) THEN
            HIGH=LOW+HIGH
            LOW=0.
         END IF
      END IF
C
C     Initial hints
C
      IF (.NOT.BATCH) THEN
         CALL PAR_WRUSER(' ',FSTAT)
         CALL PAR_WRUSER('Move cursor with mouse, and use '//
     :      'keys to initiate cleaning operations.  Hit the '//
     :      '"?" key or the "H" key to get help.',FSTAT)
      END IF
C
C     Initialise the circular list used for Undo in the workspace array
C
      WFIRST=0
      WLAST=0
C
C     The following loop continues until the user signals 'QUIT'
C
      DONE=.FALSE.
      DO WHILE (.NOT.DONE)
C
C        Handle graphics for current display mode
C
         IF (PLOT) THEN
C
C           Do we have to re-display the data?
C
            IF (REDISPLAY) THEN
               REDISPLAY=.FALSE.
C
C              Clear screen
C
               CALL PGPAGE
C
C              Set display parameters in preparation for drawing pixels
C
               IF (MODE.EQ.'W') THEN
C
C                 Mode 'W': Plot Whole image (all pixels)
C
                  CALL PGSVP(0.0, 1.0, 0.0, 1.0)
                  PIXX=DEVX
                  PIXY=DEVY
                  IXST=1
                  IXEN=NX
                  IYST=1
                  IYEN=NY
                  IXWID=MAX(1,PIXX/NX)
                  IYWID=MAX(1,PIXY/NY)
                  NBLOCKX=(NX+PIXX-1)/PIXX
                  NBLOCKY=(NY+PIXY-1)/PIXY
C
               ELSE IF (MODE.EQ.'E') THEN
C
C                 Mode 'E': Plot Expanded image (around XCENT,YCENT).
C                           Fixed pixel size.
C
                  CALL PGSVP(0.0, 1.0, 0.0, 1.0)
                  PIXX=DEVX
                  PIXY=DEVY
                  NBLOCKX=1
                  NBLOCKY=1
                  IXWID=ZOOM
                  IYWID=ZOOM
                  IF (NX.LE.PIXX/IXWID) THEN
                     IXST=1
                     IXEN=NX
                  ELSE
                     XHALF=(PIXX/2-IXWID/2)/IXWID
                     XCENT=MAX(XCENT,XHALF+1)
                     XCENT=MIN(XCENT,NX-XHALF)
                     IXST=XCENT-XHALF
                     IXEN=XCENT+XHALF
                  END IF
                  IF (NY.LE.PIXY/IYWID) THEN
                     IYST=1
                     IYEN=NY
                  ELSE
                     YHALF=(PIXY/2-IYWID/2)/IYWID
                     YCENT=MAX(YCENT,YHALF+1)
                     YCENT=MIN(YCENT,NY-YHALF)
                     IYST=YCENT-YHALF
                     IYEN=YCENT+YHALF
                  END IF
                  CALL PGSVP(0.5-(IXWID*(IXEN-IXST)*0.5)/DEVX,
     :                       0.5+(IXWID*(IXEN-IXST)*0.5)/DEVX,
     :                       0.5-(IYWID*(IYEN-IYST)*0.5)/DEVY,
     :                       0.5+(IYWID*(IYEN-IYST)*0.5)/DEVY)
C
               ELSE IF (MODE.EQ.'B') THEN
C
C                 Mode 'B': SCUBA mode (pixels around XCURR,YCENT and
C                           graph of column XCURR).  Expand pixels to
C                           fit viewport if possible.
C
                  PIXX=DEVX*(0.5-BORD)
                  PIXY=DEVY*(1.0-BORD)
                  NBLOCKX=1
                  NBLOCKY=1
                  IXWID=MAX(ZOOM,PIXX/NX)
                  IYWID=MAX(ZOOM,PIXY/NY)
                  IF (NX.LE.PIXX/IXWID) THEN
                     IXST=1
                     IXEN=NX
                  ELSE
                     XHALF=(PIXX/2-IXWID/2)/IXWID
                     XCENT=XCURR
                     XCENT=MAX(XCENT,XHALF+1)
                     XCENT=MIN(XCENT,NX-XHALF)
                     IXST=XCENT-XHALF
                     IXEN=XCENT+XHALF
                  END IF
                  IF (NY.LE.PIXY/IYWID) THEN
                     IYST=1
                     IYEN=NY
                  ELSE
                     YHALF=(PIXY/2-IYWID/2)/IYWID
                     YCENT=MAX(YCENT,YHALF+1)
                     YCENT=MIN(YCENT,NY-YHALF)
                     IYST=YCENT-YHALF
                     IYEN=YCENT+YHALF
                  END IF
                  IF (ZOOM.GE.4) THEN
                     BYTICK=16.0
                  ELSE
                     BYTICK=64.0
                  END IF
C
C                 First plot graph on one half of view surface:
C                 set abcissa (X) limits appropriately, set coordinates
C                 of plotting surface, plot axes, plot good values with
C                 lines, finally plot bad values with points.
C
                  AMAX=HIGH
                  AMIN=LOW
                  STRING='Column '
                  INVOKE=ICH_ENCODE(STRING,FLOAT(XCURR),8,0,NEXT)
                  CALL PGSVP(0.5+BORD,1.0,0.0+BORD,1.0-BORD)
                  CALL PGSCH(1.5)
                  CALL PGMTXT('T',0.5,0.5,0.5,STRING(:NEXT))
                  CALL PGSCH(1.0)
                  CALL PGSWIN(AMIN,AMAX,REAL(IYST)-.5,REAL(IYEN)+.5)
                  CALL PGBOX('NTBC',0.0,0,'NTABCS',BYTICK,4)
                  CALL FIG_SCLEAN_2(DATA,QUAL,NX,NY,XCURR,IYST,IYEN,
     :                              .TRUE.,%VAL(CNF_PVAL(XPTR)),
     :                               %VAL(CNF_PVAL(YPTR)),NPT)
                  IF (NPT.GE.1) CALL PGLINE(NPT,%VAL(CNF_PVAL(XPTR)),
     :                                      %VAL(CNF_PVAL(YPTR)))
                  CALL PGPT(NPT,%VAL(CNF_PVAL(XPTR)),
     :                      %VAL(CNF_PVAL(YPTR)),4)
                  CALL FIG_SCLEAN_2(DATA,QUAL,NX,NY,XCURR,IYST,IYEN,
     :                              .FALSE.,%VAL(CNF_PVAL(XPTR)),
     :                               %VAL(CNF_PVAL(YPTR)),NPT)
                  IF (NPT.GE.1) CALL PGPT(NPT,%VAL(CNF_PVAL(XPTR)),
     :                                    %VAL(CNF_PVAL(YPTR)),5)
C
C                 Then prepare to plot pixels on other half of view
C                 surface.
C
                  CALL PGSVP(0.0+BORD, 0.5, 0.0+BORD, 1.0-BORD)
C
               END IF
C
C              Set more image display parameter values
C
               NXP=IXEN-IXST+1
               NYP=IYEN-IYST+1
               IF (IXWID.EQ.1) THEN
                  IMX=NXP/NBLOCKX
               ELSE
                  IMX=NXP*IXWID
               END IF
               IF (IYWID.EQ.1) THEN
                  IMY=NYP/NBLOCKY
               ELSE
                  IMY=NYP*IYWID
               END IF
               IXORIG=(PIXX-IMX)/2
               IYORIG=(PIXY-IMY)/2
C
C              Set coordinates of the viewport to the pixel coordinates
C              of the data array
C
               CALL PGSWIN(REAL(IXST)-.5,REAL(IXEN)+.5,
     :                     REAL(IYST)-.5,REAL(IYEN)+.5)
C
C              Write axes for pixel plot if in 'B' mode
C
               IF (MODE.EQ.'B')
     :            CALL PGBOX('BCNMTIS',5.0,5,'BCNTIS',BYTICK,4)
C
C              Display the image.  First construct an integer array
C              corresponding to the selected part, then plot it in world
C              coordinates on the plotting surface, offset by a half pixel.
C
               CALL FIG_SCLEAN_1(.TRUE.,BADVAL,MINVAL,COLHI,
     :            NX,NY,IXST,IYST,IXEN-IXST+1,IYEN-IYST+1,
     :            DATA,QUAL,LOW,HIGH,%VAL(CNF_PVAL(IPTR)))
               CALL PGPIXL(%VAL(CNF_PVAL(IPTR)),IXEN-IXST+1,
     :                     IYEN-IYST+1,
     :                     1,IXEN-IXST+1,1,IYEN-IYST+1,
     :                     REAL(IXST)-.5,REAL(IXEN)+0.5,
     :                     REAL(IYST)-0.5,REAL(IYEN)+0.5)
            END IF
C
         END IF
C
C        Get the command character and (interactive mode) cursor position
C
   1     CONTINUE
         IF (BATCH) THEN
C
C           Batch mode: Read command line from batch file
C
    2       CONTINUE
C
C           Read next line or detect end of file
C
            IF (STATUS.NE.SAI__OK) GO TO 500
            CALL FIO_READ(BFD,STRING,LENG,STATUS)
            IF (STATUS.EQ.FIO__EOF) THEN
               CALL ERR_ANNUL(STATUS)
               STRING='Q'
               LENG=1
            ELSE IF (STATUS.NE.SAI__OK) THEN
               GO TO 500
            END IF
C
C           Discard any characters after a hash character
C
            CPOS=INDEX(STRING,'#')
            IF (CPOS.EQ.1) THEN
               LENG=0
            ELSE IF (CPOS.GT.0) THEN
               LENG=ICH_LEN(STRING(:CPOS-1))
               STRING=STRING(:LENG)
            END IF
C
C           Ignore blank lines
C
            IF (LENG.EQ.0) GO TO 2

            BATMESS='** Bad line in batch file: "'//STRING(:LENG)//'"'
            CURMESS=BATMESS
C
C           Parse into command character and 0, 1 or 2 arguments
C
            CHR=STRING(1:1)
            INVOKE=ICH_FOLD(CHR)
            NARG=0
            IF (LENG.GE.3) THEN
               INVOKE=ICH_NUMBR(STRING,3,' ',ARG1,NEXT)
               IF (INVOKE.EQ.0) THEN
                  NARG=1
                  IF (NEXT.GT.0) THEN
                     INVOKE=ICH_NUMBR(STRING,NEXT,' ',ARG2,NEXT)
                     IF (INVOKE.EQ.0) NARG=2
                     IF (NEXT.GT.0.AND.ICH_LEN(STRING(NEXT:)).GT.0) THEN
                        NARG=3
                        CALL PAR_WRUSER(BATMESS,FSTAT)
                        GO TO 2
                     END IF
                  END IF
               END IF
            END IF
C
C           If there are two arguments, interpret them as X and Y values.
C           This is not always their meaning, but doing this enables
C           us to make use of it easily in the cases where it is.
C
            IF (NARG.EQ.2 .AND.
     :          ARG1.GE.1.AND.ARG1.LE.NX .AND.
     :          ARG2.GE.1.AND.ARG2.LE.NY) THEN
               X=ARG1
               Y=ARG2
               BADPOSN=.FALSE.
            ELSE
               BADPOSN=.TRUE.
            END IF
C
         ELSE
C
C           Interactive mode: Get the cursor position and command key
C           from the user
C
            CALL PGCURSE(RXPOSN,RYPOSN,CHR)
            INVOKE=ICH_FOLD(CHR)
C
C           Obtain and validate cursor position.
C
            X=INT(RXPOSN+0.5)
            Y=INT(RYPOSN+0.5)
            BADX=((X.LT.IXST).OR.(X.GT.IXEN))
            BADY=((Y.LT.IYST).OR.(Y.GT.IYEN))
            IF (BADX.AND.MODE.EQ.'B') THEN
C
C              In mode 'B' if the cursor is out of the image region,
C              then the X coordinate is assumed to be that of the column
C              plotted in the graph region.
C
               BADX=.FALSE.
               X=XCURR
            END IF
            BADPOSN=BADX.OR.BADY
C
         END IF
C
C        What follows is a 'CASE' construct, switched on the
C        command character entered.
C
         IF (CHR.EQ.'Q') THEN
C
C           'Q' is for 'QUIT'
C
            IF (BATCH) THEN
               DONE=.TRUE.
               CALL FIG_SCLEAN_OUT(CHR,'Quit',
     :                             0.,0.,0,.FALSE.,LOG,LFD,STATUS)
            ELSE
               CALL PAR_CNPAR('QUIT')
               CALL PAR_RDKEY('QUIT',.FALSE.,DONE)
               IF (PAR_ABORT()) GO TO 500
            END IF
C
         ELSE IF (CHR.EQ.'W') THEN
C
C           'W' is for 'Whole' - ie display whole image
C
            IF (PLOT) THEN
               REDISPLAY=MODE.NE.'W'
               MODE='W'
               CALL FIG_SCLEAN_OUT(CHR,'Whole screen mode',
     :                             0.,0.,0,.FALSE.,LOG,LFD,STATUS)
            END IF
C
         ELSE IF (CHR.EQ.'E') THEN
C
C           'E' is for 'Expand' - ie display magnified image
C           around current cursor position.
C
            IF (PLOT) THEN
               IF (BADPOSN) THEN
                  CALL PAR_WRUSER(CURMESS,FSTAT)
               ELSE
                  REDISPLAY=.TRUE.
                  MODE='E'
                  XCENT=X
                  YCENT=Y
                  CALL FIG_SCLEAN_OUT(CHR,'Expanded mode',
     :                      FLOAT(X),FLOAT(Y),2,.TRUE.,LOG,LFD,STATUS)
               END IF
            END IF
C
         ELSE IF (CHR.EQ.'B') THEN
C
C           'B' is for 'SCUBA display' - ie display image in one half
C           of display and graph plot of one column in the other half.
C
            IF (PLOT) THEN
               IF (BADPOSN) THEN
                  CALL PAR_WRUSER(CURMESS,FSTAT)
               ELSE
                  REDISPLAY=.TRUE.
                  MODE='B'
                  XCURR=X
                  YCENT=Y
                  CALL FIG_SCLEAN_OUT(CHR,'SCUBA mode',
     :                      FLOAT(X),FLOAT(Y),2,.TRUE.,LOG,LFD,STATUS)
                  STRING='Plotting column '
                  INVOKE=ICH_ENCODE(STRING,FLOAT(XCURR),17,0,NEXT)
                  CALL PAR_WRUSER(STRING(:NEXT-1),FSTAT)
               END IF
            END IF
C
         ELSE IF (CHR.EQ.'D') THEN
C
C           'D' is for 'Degree' - set degree of fit in interpolation
C
            IF (BATCH) THEN
               IF (NARG.EQ.1 .AND. ARG1.GE.0. .AND. ARG1.LE.7.) THEN
                  VALUE=ARG1
               ELSE
                  CALL PAR_WRUSER(BATMESS,FSTAT)
                  GO TO 1
               END IF
            ELSE
               CALL PAR_CNPAR('DEG')
               CALL PAR_RDVAL('DEG',0.,7.,FLOAT(NCOEFF-1),' ',VALUE)
               IF (PAR_ABORT()) GO TO 500
            END IF
            NCOEFF=VALUE+1
            CALL FIG_SCLEAN_OUT(CHR,'Smoothing box fit degree set to',
     :                          VALUE,0.,1,.FALSE.,LOG,LFD,STATUS)
C
         ELSE IF (CHR.EQ.'N') THEN
C
C           'N' is for 'Number' of pixels on side of area deleted
C               by 'X' or 'Y'.
C
            IF (BATCH) THEN
               IF (NARG.EQ.2 .AND. ARG1.GE.1. .AND. ARG1.LE.20.
     :                       .AND. ARG2.GE.1. .AND. ARG2.LE.20.) THEN
                  NBOXX=ARG1
                  NBOXY=ARG2
               ELSE
                  CALL PAR_WRUSER(BATMESS,FSTAT)
                  GO TO 1
               END IF
            ELSE
               CALL PAR_CNPAR('XSIZE')
               CALL PAR_RDVAL('XSIZE',1.,20.,FLOAT(NBOXX),'Pixels',
     :                        VALUE)
               IF (PAR_ABORT()) GO TO 500
               NBOXX=VALUE
               CALL PAR_CNPAR('YSIZE')
               CALL PAR_RDVAL('YSIZE',1.,20.,FLOAT(NBOXY),'Pixels',
     :                        VALUE)
               IF (PAR_ABORT()) GO TO 500
               NBOXY=VALUE
            END IF
            CALL FIG_SCLEAN_OUT(CHR,'Smoothing box dimensions set to',
     :                          FLOAT(NBOXX),FLOAT(NBOXY),2,
     :                          .FALSE.,LOG,LFD,STATUS)
C
         ELSE IF (CHR.EQ.'S') THEN
C
C           'S' is for 'Stretch' - ie change display scale limits
C
            IF (PLOT) THEN
               HWAS=HIGH
               LWAS=LOW
               IF (BATCH) THEN
                  IF (NARG.EQ.2) THEN
                     LOW=ARG1
                     HIGH=ARG2
                  ELSE
                     CALL PAR_WRUSER(BATMESS,FSTAT)
                     GO TO 1
                  END IF
               ELSE
                  CALL PAR_WRUSER('Set stretch',FSTAT)
                  CALL PAR_CNPAR('HIGH')
                  CALL PAR_RDVAL('HIGH',-3E30,3E30,HIGH,' ',VALUE)
                  IF (PAR_ABORT()) GO TO 500
                  HIGH=VALUE
                  CALL PAR_CNPAR('LOW')
                  CALL PAR_RDVAL('LOW',-3E30,3E30,LOW,' ',VALUE)
                  IF (PAR_ABORT()) GO TO 500
                  LOW=VALUE
                  REDISPLAY=.NOT.(GEN_SIMILAR(HIGH,HWAS).AND.
     :                                           GEN_SIMILAR(LOW,LWAS))
               END IF
               IF (LOW.EQ.HIGH) THEN
                  CALL PAR_WRUSER('Cannot have the same value '
     :                      // 'for low and high',FSTAT)
                  HIGH=HWAS
                  LOW=LWAS
               ELSE
                  CALL FIG_SCLEAN_OUT(CHR,'Display limits set to',
     :                      LOW,HIGH,2,.FALSE.,LOG,LFD,STATUS)
               END IF
            END IF
C
         ELSE IF ((CHR.EQ.'?').OR.(CHR.EQ.'H')) THEN
C
C           '?' or 'H' is for 'HELP'
C
            CALL FIG_SCLHELP
C
         ELSE IF ((CHR.EQ.'R').OR.(CHR.EQ.'L')) THEN
C
C           'R' is for 'Row' - ie delete and fix the indicated bad row.
C           'L' is for 'Line' - delete the indicated row but don't fix.
C
            IF (BATCH .AND.
     :         (NARG.NE.1 .OR. ARG1.LT.1 .OR. ARG1.GT.NY)) THEN
               CALL PAR_WRUSER(BATMESS,FSTAT)
               GO TO 1
            ELSE IF (.NOT.BATCH .AND. BADPOSN) THEN
               CALL PAR_WRUSER(CURMESS,FSTAT)
            ELSE IF (.NOT.BATCH .AND. NBLOCKY.NE.1) THEN
               CALL PAR_WRUSER('Display cannot resolve a single line'
     :                                                       ,FSTAT)
               CALL PAR_WRUSER(
     :            'You will have to use a magnified display',FSTAT)
            ELSE
               IF (BATCH) Y=ARG1
               CALL FIG_SCLSAVE(DATA,QUAL,VAR,VEXIST,CHANGE,
     :                          NX,NY,1,NX,Y,Y,'R',
     :                          WORK,WSIZE,WFIRST,WLAST)
               IF (CHR.EQ.'R') THEN
                  CALL FIG_FIXROWSQ(DATA,QUAL,NX,NY,1,NX,1,NY,1,Y,
     :                              NCOEFF)
                  IF (VEXIST) CALL FIG_PUNCHOUT(VAR,NX,NY,1,Y,NX,Y,0.0)
                  CHANGE=.TRUE.
               ELSE
                  CALL FIG_PUNCHOUTQ(QUAL,NX,NY,1,Y,NX,Y,BITNUM,.TRUE.)
               END IF
               REDISPLAY=.TRUE.
               CALL FIG_SCLEAN_OUT(CHR,'Bad row number',
     :                             FLOAT(Y),0.,1,.FALSE.,LOG,LFD,STATUS)
            END IF
C
         ELSE IF ((CHR.EQ.'C').OR.(CHR.EQ.'K')) THEN
C
C           'C' is for 'Column' - ie delete and fix the indicated column
C           'K' is for 'Kolumn' - delete column but don't fix
C
            IF (BATCH .AND.
     :         (NARG.NE.1 .OR. ARG1.LT.1 .OR. ARG1.GT.NX)) THEN
               CALL PAR_WRUSER(BATMESS,FSTAT)
               GO TO 1
            ELSE IF (.NOT.BATCH .AND. BADPOSN) THEN
               CALL PAR_WRUSER(CURMESS,FSTAT)
            ELSE IF (.NOT.BATCH .AND. NBLOCKX.NE.1) THEN
               CALL PAR_WRUSER(
     :            'Display cannot resolve a single column',FSTAT)
               CALL PAR_WRUSER(
     :            'You will have to use a magnified display',FSTAT)
            ELSE
               IF (BATCH) X=ARG1
               CALL FIG_SCLSAVE(DATA,QUAL,VAR,VEXIST,CHANGE,
     :                          NX,NY,X,X,1,NY,'C',
     :                          WORK,WSIZE,WFIRST,WLAST)
               IF (CHR.EQ.'C') THEN
                  CALL FIG_FIXCOLSQ(DATA,QUAL,NX,NY,1,NX,1,NY,1,X,
     :                              NCOEFF)
                  IF (VEXIST) CALL FIG_PUNCHOUT(VAR,NX,NY,X,1,X,NY,0.0)
                  CHANGE=.TRUE.
               ELSE
                  CALL FIG_PUNCHOUTQ(QUAL,NX,NY,X,1,X,NY,BITNUM,.TRUE.)
               END IF
               REDISPLAY=.TRUE.
               CALL FIG_SCLEAN_OUT(CHR,'Bad column number',
     :                             FLOAT(X),0.,1,.FALSE.,LOG,LFD,STATUS)
            END IF
C
         ELSE IF (CHR.EQ.'A' .OR. CHR.EQ.'G') THEN
C
C           'A' is for 'Delete pixel At cursor position' (not fix)
C           'G' is for 'Mark as Good pixel at cursor position'
C
            IF (BADPOSN) THEN
               CALL PAR_WRUSER(CURMESS,FSTAT)
            ELSE IF (.NOT.BATCH .AND.
     :              (NBLOCKX.NE.1.OR.NBLOCKY.NE.1)) THEN
               CALL PAR_WRUSER(
     :            'Display cannot resolve a single pixel',FSTAT)
               CALL PAR_WRUSER(
     :            'You will have to use a magnified display',FSTAT)
            ELSE
               IF (CHR.EQ.'A') THEN
                  BITVAL=.TRUE.
                  STRING='Bad pixel at'
               ELSE
                  BITVAL=.FALSE.
                  STRING='Good pixel at'
               END IF
               CALL FIG_SCLSAVE(DATA,QUAL,VAR,VEXIST,CHANGE,
     :                          NX,NY,X,X,Y,Y,'A',
     :                          WORK,WSIZE,WFIRST,WLAST)
               CALL FIG_PUNCHOUTQ(QUAL,NX,NY,X,Y,X,Y,BITNUM,BITVAL)
               REDISPLAY=.TRUE.
               CALL FIG_SCLEAN_OUT(CHR,STRING,FLOAT(X),FLOAT(Y),
     :                             2,.TRUE.,LOG,LFD,STATUS)
            END IF
C
         ELSE IF (CHR.EQ.'P') THEN
C
C           'P' is for display current cursor Position
C
            IF (PLOT) THEN
               IF (BADPOSN) THEN
                  CALL PAR_WRUSER(CURMESS,FSTAT)
               ELSE
                  CALL FIG_SCLEAN_OUT(' ','Cursor is at',
     :                                FLOAT(X),FLOAT(Y),2,
     :                                .TRUE.,.FALSE.,0,STATUS)
                  IF ((NBLOCKX.GT.1).OR.(NBLOCKY.GT.1)) THEN
                     STRING='Note: one display pixel covers '
                     INVOKE=ICH_ENCODE(STRING,FLOAT(NBLOCKX),32,0,NEXT)
                     STRING(NEXT:)=' image pixel'
                     NEXT=NEXT+12
                     IF (NBLOCKX.GT.1) THEN
                        STRING(NEXT:NEXT)='s'
                        NEXT=NEXT+1
                     END IF
                     STRING(NEXT:)=' in X, and '
                     INVOKE=ICH_ENCODE(STRING,FLOAT(NBLOCKY),NEXT+11,
     :                                                        0,NEXT)
                     STRING(NEXT:)=' in Y'
                     CALL PAR_WRUSER(STRING(:NEXT+5),FSTAT)
                  END IF
               END IF
            END IF
C
         ELSE IF ((CHR.EQ.'X').OR.(CHR.EQ.'Y')) THEN
C
C           'X' is for fix by interpolation in the 'X' direction,
C           'Y' is for fix by interpolation in the 'Y' direction.
C
            IF (BADPOSN) THEN
               CALL PAR_WRUSER(CURMESS,FSTAT)
            ELSE
               IX1=MAX(1,X-NBOXX/2)
               IX2=MIN(NX,X+NBOXX/2)
               IY1=MAX(1,Y-NBOXY/2)
               IY2=MIN(NY,Y+NBOXY/2)
               CALL FIG_SCLSAVE(DATA,QUAL,VAR,VEXIST,CHANGE,NX,NY,
     :                          IX1,IX2,IY1,IY2,CHR,
     :                          WORK,WSIZE,WFIRST,WLAST)
               IF (CHR.EQ.'X') THEN
                  LIMX1=IX1-2*NCOEFF
                  LIMX2=IX2+2*NCOEFF
                  DO IY=IY1,IY2
                     CALL FIG_HORIZONTALQ(DATA,QUAL,NX,NY,IY,IX1,IX2,
     :                                    LIMX1,LIMX2,NCOEFF,MAXPT,
     :                                    XWRK,ZWRK,FSTAT)
                  END DO
               ELSE
                  LIMY1=IY1-2*NCOEFF
                  LIMY2=IY2+2*NCOEFF
                  DO IX=IX1,IX2
                     CALL FIG_VERTICALQ(DATA,QUAL,NX,NY,IX,IY1,IY2,
     :                                  LIMY1,LIMY2,NCOEFF,MAXPT,
     :                                  XWRK,ZWRK,FSTAT)
                  END DO
               END IF
               IF (FSTAT.EQ.0) THEN
                  IF (VEXIST)
     :               CALL FIG_PUNCHOUT(VAR,NX,NY,IX1,IY1,IX2,IY2,0.0)
                  CHANGE=.TRUE.
                  REDISPLAY=.TRUE.
                  CALL FIG_SCLEAN_OUT(CHR,'Interpolated around',
     :               FLOAT(X),FLOAT(Y),2,.TRUE.,LOG,LFD,STATUS)
               ELSE
                  FSTAT=0
                  CALL PAR_WRUSER('Interpolation failed - '
     :               // 'not enough good pixels',FSTAT)
                  CALL FIG_SCLRSTR(DATA,QUAL,VAR,VEXIST,CHANGE,
     :                             NX,NY,IX1,IX2,IY1,IY2,
     :                             STRING,WORK,WSIZE,WFIRST,WLAST)
               END IF
            END IF
C
         ELSE IF (CHR.EQ.'T') THEN
C
C           'T' is for 'Test' - see what BCLEAN settings would be
C               needed to pick up cosmic rays in the area round the
C               indicated pixel.
C
            IF (BADPOSN) THEN
               CALL PAR_WRUSER(CURMESS,FSTAT)
            ELSE
               IX1=MAX(1,X-NBOXX/2)
               IX2=MIN(NX,X+NBOXX/2)
               IY1=MAX(1,Y-NBOXY/2)
               IY2=MIN(NY,Y+NBOXY/2)
               CALL FIG_SGRSTAT(DATA,QUAL,NX,NY,IX1,IX2,IY1,IY2)
            END IF
C
         ELSE IF (CHR.EQ.'U') THEN
C
C           'U' is for Undo previous operation - only applies to
C           those operations that change data.  Note that the undoing
C           is done by resetting the data involved to that in the original
C           image, which is not STRICTLY an undoing of the previous
C           operation.
C
            CALL FIG_SCLRSTR(DATA,QUAL,VAR,VEXIST,CHANGE,
     :                       NX,NY,IX1,IX2,IY1,IY2,
     :                       STRING,WORK,WSIZE,WFIRST,WLAST)
            IF (IX1.GT.0) THEN
               CALL FIG_SCLEAN_OUT(CHR,STRING,0.,0.,0,.FALSE.,LOG,LFD,
     :                          STATUS)
               REDISPLAY=.TRUE.
            END IF
C
         ELSE
C
C           Unknown command
C
            IF (BATCH) THEN
               CALL PAR_WRUSER(BATMESS,FSTAT)
               GO TO 1
            ELSE
               STRING='Command '' '' is not defined.'
               STRING(10:10)=CHR
               CALL PAR_WRUSER(STRING,FSTAT)
            END IF
         END IF
      END DO
C
C     Set display values array
C
      IF (PLOT) THEN
         ARRAY(1)=IYST
         ARRAY(2)=IYEN
         ARRAY(3)=IXST
         ARRAY(4)=IXEN
         ARRAY(5)=NBLOCKX
         ARRAY(6)=NBLOCKY
         ARRAY(7)=IXWID
         ARRAY(8)=IYWID
         ARRAY(9)=IXORIG
         ARRAY(10)=IYORIG
         ARRAY(11)=HIGH
         ARRAY(12)=LOW
      END IF
C
C     Return
C
  500 CONTINUE
C
      IF (PAR_ABORT()) STATUS=PAR__ABORT
C
      END
C+

      SUBROUTINE FIG_SCLHELP
C
C     F I G _ S C L H E L P
C
C     Outputs help information for 'SCLEAN'.
C
C     Common variables - None
C
C                                       KS / CIT 2nd March 1984
C     Modified:
C
C     17th Dec 1987.  KS / AAO.  'P' option added.
C     20th Jul 1998.  MBT / IoA.  Changed 'joystick' for 'mouse', and
C                                 added commands 'B' and 'A'.
C+
      IMPLICIT NONE
C
C     Local variables
C
      INTEGER STATUS
C
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER(
     :   'Use the mouse or the numeric keypad to move the cursor.',
     :                                                        STATUS)
      CALL PAR_WRUSER(
     :   'The PF1..PF4 keys act as gears for the cursor speed',STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('The following keys are recognised -',STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('W - display Whole image',STATUS)
      CALL PAR_WRUSER('E - Expand image around cursor position',STATUS)
      CALL PAR_WRUSER('B - SCUBA type display for cursor column',STATUS)
      CALL PAR_WRUSER(
     :   'R - delete indicated Row (horizontal line) & fix it',STATUS)
      CALL PAR_WRUSER(
     :   'C - delete indicated Column (vertical line) & fix it',STATUS)
      CALL PAR_WRUSER(
     :   'X - delete indicated area and fix by interpolation in ' //
     :   'the x direction',STATUS)
      CALL PAR_WRUSER(
     :   'Y - like X, but uses vertical interpolation',STATUS)
      CALL PAR_WRUSER(
     :   'S - set stretch, ie High & low limits for display',STATUS)
      CALL PAR_WRUSER(
     :   'L - delete indicated line, but don''t fix',STATUS)
      CALL PAR_WRUSER(
     :   'K - delete indicated column, but don''t fix',STATUS)
      CALL PAR_WRUSER(
     :   'A - delete pixel at cursor, but don''t fix',STATUS)
      CALL PAR_WRUSER(
     :   'G - mark as Good pixel at cursor',STATUS)
      CALL PAR_WRUSER(
     :   'D - set degree of fit used for interpolation',STATUS)
      CALL PAR_WRUSER(
     :   'N - set size of deleted area for "X" and "Y"',STATUS)
      CALL PAR_WRUSER(
     :   'P - indicate current cursor position',STATUS)
      CALL PAR_WRUSER(
     :   'T - test area to see what BCLEAN might find there',STATUS)
      CALL PAR_WRUSER(
     :   'U - Undo last operation - replace with original image data',
     :                                                          STATUS)
      CALL PAR_WRUSER('Q - Quit program',STATUS)
      CALL PAR_WRUSER(' ',STATUS)
C
      END
C+
      SUBROUTINE FIG_SGRSTAT (DATA,QUAL,NX,NY,IX1,IX2,IY1,IY2)
C
C     F I G _ S G R S T A T
C
C     Given an area of the image to be fixed, calculates the
C     cosmic ray parameters that would be needed for BCLEAN to
C     have chosen this area.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) DATA    (Real array DATA(NX,NY)) The image data
C     (>) QUAL    (Byte array QUAL(NX,NY)) Quality array for DATA
C     (>) NX      (Integer) First dimension of DATA
C     (>) NY      (Integer) Second dimension of DATA
C     (>) IX1     (Integer) The area in question has its
C     (>) IX2     (Integer) bottom left corner at DATA(IX1,IY1)
C     (>) IY1     (Integer) and its top right corner at
C     (>) IY2     (Integer) DATA(IX2,IY2).
C
C     Common variables used - None
C
C                                         KS / CIT 2nd March 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,IX1,IX2,IY1,IY2
      REAL    DATA(NX,NY)
      BYTE    QUAL(NX,NY)
C
C     Functions
C
      INTEGER ICH_ENCODE
      REAL    FIG_CLOSESTQ
C
C     Local variables
C
      LOGICAL PEAK
      INTEGER INVOKE, IX, IY, NEXT, NRAYS, STATUS, LENG
      REAL    AVE, AVEX, AVEY, D0, D1, D2, D3, D4, EXCESS, FRAC
      REAL    NSIG, SIGMA
      CHARACTER STRING*80
      BYTE    GOOD
C
C     Constants
C
      PARAMETER (GOOD=0)             ! Value of quality array for valid data
C
C     Look at all pixels in the area
C
      NRAYS=0
      DO IY=IY1,IY2
         DO IX=IX1,IX2
C
C           Get the set of 5 cross pixels
C
            PEAK=.FALSE.
            D0=DATA(IX,IY)
            IF (D0.GT.0.) THEN
               D1=FIG_CLOSESTQ(DATA,QUAL,NX,NY,IX+1,IY)
               IF (D0.GT.D1) THEN
                  D2=FIG_CLOSESTQ(DATA,QUAL,NX,NY,IX,IY+1)
                  IF (D0.GT.D2) THEN
                     D3=FIG_CLOSESTQ(DATA,QUAL,NX,NY,IX-1,IY)
                     IF (D0.GT.D3) THEN
                        D4=FIG_CLOSESTQ(DATA,QUAL,NX,NY,IX,IY-1)
                        PEAK=D0.GT.D4
                     END IF
                  END IF
               END IF
            END IF
C
C           Are we looking at a peak?  If so, output parameters.
C
            IF (PEAK) THEN
               AVEX=.5*(D2+D4)
               AVEY=.5*(D1+D3)
               AVE=(AVEY+AVEX)/2
               SIGMA=SQRT(ABS(AVE))
               EXCESS=D0-AVE
               NSIG=EXCESS/SIGMA
               FRAC=EXCESS/AVE
               STRING='Possible cosmic ray at ('
               INVOKE=ICH_ENCODE(STRING,FLOAT(IX),25,0,NEXT)
               STRING(NEXT:NEXT)=','
               INVOKE=ICH_ENCODE(STRING,FLOAT(IY),NEXT+1,0,NEXT)
               STRING(NEXT:)='), would be detected by BCLEAN with'
               LENG=NEXT+2
               CALL PAR_WRUSER(STRING(:NEXT+34),STATUS)
               STRING='settings of Crsig = '
               INVOKE=ICH_ENCODE(STRING,NSIG,21,2,NEXT)
               STRING(NEXT:)=', Crfrac = '
               INVOKE=ICH_ENCODE(STRING,FRAC,NEXT+12,2,NEXT)
               STRING(NEXT:)=', Crminv = '
               INVOKE=ICH_ENCODE(STRING,EXCESS,NEXT+12,2,NEXT)
               CALL PAR_WRUSER(STRING(:NEXT-1),STATUS)
               IF (QUAL(IX,IY).NE.GOOD) THEN
                  STRING(1:LENG)=' '
                  STRING(LENG:)='- already marked bad'
                  CALL PAR_WRUSER(STRING(:LENG+19),STATUS)
               END IF
               NRAYS=NRAYS+1
            END IF
         END DO
      END DO
C
      IF (NRAYS.EQ.0) THEN
         CALL PAR_WRUSER('No cosmic ray candidates in test area',
     :                                                       STATUS)
      END IF
C
      END
C+
      SUBROUTINE FIG_SCLSAVE(DATA,QUAL,VAR,VEXIST,CHANGE,
     :                       NX,NY,IX1,IX2,IY1,IY2,CHR,
     :                       WORK,WSIZE,WFIRST,WLAST)
C
C     F I G _ S C L S A V E
C
C     Saves the data about to be modified so that the operation may be
C     undone.  The actual data, together with enough details about the
C     operation to allow its undoing, is stored in a circular work
C     array.
C
C     Parameters - (">" input, "<" output, "!" modified)
C
C     (>) DATA     (Real array DATA(NX,NY)) The image being cleaned.
C     (>) QUAL     (Byte array QUAL(NX,NY)) Quality array for DATA.
C     (>) VAR      (Real array VAR(NX,NY)) Variance array for DATA.
C     (>) VEXIST   (Logical) True if VAR contains values
C     (>) CHANGE   (Logical) True if data array has been changed
C     (>) NX       (Integer) The first image dimension
C     (>) NY       (Integer) The second image dimension
C     (>) IX1      (Integer) The x-coordinate of the first element
C                  in the block to be modified.
C     (>) IX2      (Integer) The x-coordinate of the last element
C                  in the block to be modified.
C     (>) IY1      (Integer) The y-coordinate of the first element
C                  in the block to be modified.
C     (>) IY2      (Integer) The y-coordinate of the last element
C                  in the block to be modified.
C     (>) CHR      (Character) A character indicating the SCLEAN command
C                  being remembered.
C     (!) WORK     (Real array WORK(WSIZE)) The work array used to
C                  hold the command details and modified data.
C     (>) WSIZE    (Integer) The number of elements in WORK.
C     (!) WFIRST   (Integer) The first of the currently used elements
C                  in WORK.
C     (!) WLAST    (Integer) The last of the currently used elements
C                  in WORK.
C
C                                        KS / AAO 18th Dec 1987
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,IX1,IX2,IY1,IY2,WSIZE,WFIRST,WLAST
      REAL    DATA(NX,NY), WORK(WSIZE), VAR(NX,NY)
      BYTE    QUAL(NX,NY)
      LOGICAL VEXIST,CHANGE
      CHARACTER CHR*1
C
C     Local variables
C
      LOGICAL CANCEL
      INTEGER IX, IY, NWORK, STATUS, WEND, WLIM, WPTR, WVAL, MULT
C
C     Note the structure of an entry in the circular buffer.  Taking
C     element 1 as the first for this entry, the elements contain:
C     1:          The number of the last element in the entry.
C     2:          IX1 (as a real number)
C     3:          IX2 (      "         )
C     4:          IY1 (      "         )
C     5:          IY2 (      "         )
C     6:          CHR (      "         )
C     7:          CHANGE (   "         )
C     8   ..7+N:  Data elements saved (IX1..IX2,IY1..IY2)
C     8+N ..7+2N: Quality elements saved (IX1..IX2,IY1..IY2) (as REALs)
C     8+2N..7+3N: Variance elements saved (IX1..IX2,IY1..IY2) (if VEXIST)
C     8+(2 or 3)N:The number of the first element in the entry.
C     - where N=(IX2-IX1+1)*(IY2-IY1+1), the number of elements to be saved.
C     If VEXIST is false the variances are not stored.
C
C     Storage of the BYTE quality array section in the REAL array WORK is
C     a bit wasteful, but pressure of space here is not anticipated to
C     be very great.
C
C     If the elements of the entry reach the end of the buffer, they
C     start immediately at the beginning.  The element at which the first
C     (earliest) entry in the buffer starts is WFIRST, the element at
C     which the last (latest) entry ends is WLAST.

C
C     Make sure that the work array is large enough for the
C     operation information.
C
      IF (VEXIST) THEN
         MULT=3
      ELSE
         MULT=2
      END IF
      NWORK=((IX2-IX1+1)*(IY2-IY1+1))*MULT+8
      IF (WSIZE.LT.NWORK) THEN
         CALL PAR_WRUSER(
     :         'Insufficient workspace allocated.  Will not be able to'
     :         //'"Undo" this operation.',STATUS)
      ELSE
C
C        Put the operation data into the workspace.  First of all a lot
C        of messing about is required with the circular buffer to see if
C        the new data is going to overwrite any of the data from earlier
C        operations.  In what follows, WPTR is the buffer element to be
C        used for the start of the new data, WEND is the last element
C        needed for the new data.  If WEND is such that data from earlier
C        entries will be overwritten, they are removed from the buffer
C        by moving WFIRST past them.  The circular nature of the buffer
C        complicates the whole business, although it is quite possible
C        that the following algorithm is not optimal.
C
         WPTR=WLAST+1
         WEND=WPTR+NWORK-1
         IF (WEND.GT.WSIZE) THEN
            CALL PAR_WRUSER(
     :       'Workspace buffer has overflowed. Information required to',
     :                                                           STATUS)
            CALL PAR_WRUSER('"Undo" earliest operations has been lost.',
     :                                                           STATUS)
         END IF
         IF (WLAST.GE.WFIRST) THEN
C
C           In this case, the earlier entries do not wrap round the
C           end of the buffer - they form a continuous string from
C           WFIRST to WLAST, and will only be overwritten if WEND is
C           larger than WSIZE (in which case the new data will wrap round
C           to the start of the buffer).
C
            IF (WEND.GT.WSIZE) THEN
               WEND=WEND-WSIZE
               CANCEL=(WEND.GE.WFIRST)
               DO WHILE (CANCEL)
                  WVAL=WORK(WFIRST)
                  WFIRST=WVAL+1
                  IF (WFIRST.GT.WSIZE) WFIRST=WFIRST-WSIZE
                  CANCEL=(WEND.GE.WFIRST).AND.(WVAL.NE.WLAST)
               END DO
            END IF
         ELSE
C
C           In this case, the entries already in the buffer wrap round
C           - running from WFIRST to the end of the buffer, then continuing
C           from the start of the buffer to WLAST.  They will be overwritten
C           if WEND reaches up as far as WFIRST, and we also have to allow
C           for the possibility that WEND is bigger than WSIZE, in which
C           case some of the data at the start may be overitten too.
C
            WLIM=WFIRST
            CANCEL=(WEND.GE.WLIM)
            DO WHILE (CANCEL)
               WVAL=WORK(WFIRST)
               WLIM=WVAL
               IF (WEND.GT.WSIZE) WLIM=WLIM+WSIZE
               WFIRST=WVAL+1
               IF (WFIRST.GT.WSIZE) WFIRST=WFIRST-WSIZE
               CANCEL=(WEND.GE.WLIM).AND.(WVAL.NE.WLAST)
            END DO
         END IF
         IF (WPTR.GT.WSIZE) WPTR=WPTR-WSIZE
         IF (WEND.GT.WSIZE) WEND=WEND-WSIZE
         WLAST=WEND
C
C        Now enter the data for the operation into the circular buffer.
C        Its a bit messy testing WPTR for wrap-round each time, but
C        so what?
C
C        A (statement) function for wrapping increment would be tidier;
C        but if it ain't broke...
C
         WORK(WPTR)=WLAST
         WORK(WEND)=WPTR
         IF (WFIRST.LE.0) WFIRST=1
         WPTR=WPTR+1
         IF (WPTR.GT.WSIZE) WPTR=1
         WORK(WPTR)=IX1
         WPTR=WPTR+1
         IF (WPTR.GT.WSIZE) WPTR=1
         WORK(WPTR)=IX2
         WPTR=WPTR+1
         IF (WPTR.GT.WSIZE) WPTR=1
         WORK(WPTR)=IY1
         WPTR=WPTR+1
         IF (WPTR.GT.WSIZE) WPTR=1
         WORK(WPTR)=IY2
         WPTR=WPTR+1
         IF (WPTR.GT.WSIZE) WPTR=1
         WORK(WPTR)=ICHAR(CHR)
         WPTR=WPTR+1
         IF (WPTR.GT.WSIZE) WPTR=1
         IF (CHANGE) THEN
            WORK(WPTR)=1.0
         ELSE
            WORK(WPTR)=0.0
         END IF
         DO IY=IY1,IY2
            DO IX=IX1,IX2
               WPTR=WPTR+1
               IF (WPTR.GT.WSIZE) WPTR=1
               WORK(WPTR)=DATA(IX,IY)
            END DO
         END DO
         DO IY=IY1,IY2
            DO IX=IX1,IX2
               WPTR=WPTR+1
               IF (WPTR.GT.WSIZE) WPTR=1
               WORK(WPTR)=QUAL(IX,IY)
            END DO
         END DO
         IF (VEXIST) THEN
            DO IY=IY1,IY2
               DO IX=IX1,IX2
                  WPTR=WPTR+1
                  IF (WPTR.GT.WSIZE) WPTR=1
                  WORK(WPTR)=VAR(IX,IY)
               END DO
            END DO
         END IF
      END IF
C
      END
C+
      SUBROUTINE FIG_SCLRSTR(DATA,QUAL,VAR,VEXIST,CHANGE,
     :                       NX,NY,IX1,IX2,IY1,IY2,
     :                       STRING,WORK,WSIZE,WFIRST,WLAST)
C
C     F I G _ S C L R S T R
C
C     Restores the last operation saved in the workspace.  This routine
C     performs the data restoration part of an Undo operation, and returns
C     the data limits that have been changed so the calling routine can
C     redisplay the relevant part of the image.
C
C     Parameters - (">" input, "<" output, "!" modified)
C
C     (!) DATA     (Real array DATA(NX,NY)) The image being cleaned.
C     (!) QUAL     (Byte array QUAL(NX,NY)) Quality array for DATA
C     (!) VAR      (Real array VAR(NX,NY)) Variance array for DATA
C     (>) VEXIST   (Logical) True if VAR contains values
C     (!) CHANGE   (Logical) True if the Data array has been changed
C     (>) NX       (Integer) The first image dimension
C     (>) NY       (Integer) The second image dimension
C     (<) IX1      (Integer) The x-coordinate of the first element
C                  in the block that was modified.
C     (<) IX2      (Integer) The x-coordinate of the last element
C                  in the block that was modified.
C     (<) IY1      (Integer) The y-coordinate of the first element
C                  in the block that was modified.
C     (<) IY2      (Integer) The y-coordinate of the last element
C                  in the block that was modified.
C                  If no operation was saved in the buffer, the limits
C                  are returned all set to -1.
C     (>) STRING   (Character*(*)) String describing undo operation.
C     (>) WORK     (Real array WORK(WSIZE)) The work array used to
C                  hold command details and modified data and quality.
C     (>) WSIZE    (Integer) The number of elements in WORK.
C     (!) WFIRST   (Integer) The first of the currently used elements
C                  in WORK.
C     (!) WLAST    (Integer) The last of the currently used elements
C                  in WORK.
C
C                                        KS / AAO 18th Dec 1987
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,IX1,IX2,IY1,IY2,WSIZE,WFIRST,WLAST
      REAL    DATA(NX,NY), VAR(NX,NY), WORK(WSIZE)
      BYTE    QUAL(NX,NY)
      LOGICAL VEXIST
      CHARACTER*(*) STRING
C
C     Functions
C
      INTEGER ICH_ENCODE
C
C     Local variables
C
      LOGICAL VALID,CHANGE
      INTEGER INVOKE, IX, IY, NEXT, STATUS, WPTR
      REAL XWAS, YWAS
      CHARACTER CHWAS*1
C
      IF (WLAST.EQ.0) THEN
         CALL PAR_WRUSER ('No operation to "Undo"',STATUS)
         IX1=-1
         IX2=-1
         IY1=-1
         IY2=-1
      ELSE
         WPTR=WORK(WLAST)
         IF (WPTR.EQ.WFIRST) THEN
C
C           This is the only entry in the buffer, so reset it.  This
C           makes things a bit neater.
C
            WFIRST=0
            WLAST=0
         ELSE
            WLAST=WPTR-1
            IF (WLAST.LE.0) WLAST=WSIZE
         END IF
C
C        Pick up the operation parameters from the circular buffer
C
         WPTR=WPTR+1
         IF (WPTR.GT.WSIZE) WPTR=1
         IX1=WORK(WPTR)
         WPTR=WPTR+1
         IF (WPTR.GT.WSIZE) WPTR=1
         IX2=WORK(WPTR)
         WPTR=WPTR+1
         IF (WPTR.GT.WSIZE) WPTR=1
         IY1=WORK(WPTR)
         WPTR=WPTR+1
         IF (WPTR.GT.WSIZE) WPTR=1
         IY2=WORK(WPTR)
         WPTR=WPTR+1
         IF (WPTR.GT.WSIZE) WPTR=1
         CHWAS=CHAR(INT(WORK(WPTR)))
         VALID=.TRUE.
         IF (CHWAS.EQ.'R') THEN
            STRING='Restoring row '
            INVOKE=ICH_ENCODE(STRING,FLOAT(IY1),15,0,NEXT)
         ELSE IF (CHWAS.EQ.'C') THEN
            STRING='Restoring column '
            INVOKE=ICH_ENCODE(STRING,FLOAT(IX1),18,0,NEXT)
         ELSE IF ((CHWAS.EQ.'X').OR.(CHWAS.EQ.'Y')) THEN
            STRING='Restoring area around ( '
            XWAS=NINT(FLOAT(IX1+IX2)*0.5)
            YWAS=NINT(FLOAT(IY1+IY2)*0.5)
            INVOKE=ICH_ENCODE(STRING,XWAS,25,0,NEXT)
            STRING(NEXT:NEXT+1)=', '
            INVOKE=ICH_ENCODE(STRING,YWAS,NEXT+2,0,NEXT)
            STRING(NEXT:NEXT+1)=' )'
         ELSE IF (CHWAS.EQ.'A') THEN
            STRING='Restoring pixel ( '
            INVOKE=ICH_ENCODE(STRING,FLOAT(IX1),19,0,NEXT)
            STRING(NEXT:NEXT+1)=', '
            INVOKE=ICH_ENCODE(STRING,FLOAT(IY1),NEXT+2,0,NEXT)
            STRING(NEXT:NEXT+1)=' )'
         ELSE
            STRING='Internal error: invalid operation saved.'
            VALID=.FALSE.
         END IF
         IF (VALID) THEN
            WPTR=WPTR+1
            IF (WPTR.GT.WSIZE) WPTR=1
            CHANGE=WORK(WPTR).NE.0.0
            DO IY=IY1,IY2
               DO IX=IX1,IX2
                  WPTR=WPTR+1
                  IF (WPTR.GT.WSIZE) WPTR=1
                  DATA(IX,IY)=WORK(WPTR)
               END DO
            END DO
            DO IY=IY1,IY2
               DO IX=IX1,IX2
                  WPTR=WPTR+1
                  IF (WPTR.GT.WSIZE) WPTR=1
                  QUAL(IX,IY)=WORK(WPTR)
               END DO
            END DO
            IF (VEXIST) THEN
               DO IY=IY1,IY2
                  DO IX=IX1,IX2
                     WPTR=WPTR+1
                     IF (WPTR.GT.WSIZE) WPTR=1
                     VAR(IX,IY)=WORK(WPTR)
                  END DO
               END DO
            END IF
         END IF
      END IF
C
      END

      SUBROUTINE FIG_SCLEAN_1( BAD, BADVAL, MINVAL, MAXVAL,
     :   IDIM1, IDIM2, IXST, IYST, ODIM1, ODIM2,
     :   IDATA, IQUAL, LOW, HIGH, ODATA )
*+
*  Name:
*     FIG_SCLEAN_1

*  Purpose:
*     First service routine for SCLEAN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_SCLEAN_1( BAD, BADVAL, MINVAL, MAXVAL,
*        IDIM1, IDIM2, IXST, IYST, ODIM1, ODIM2,
*        IDATA, IQUAL, LOW, HIGH, ODATA )

*  Description:
*     This routine picks a subset from a REAL array and scales it into
*     an INTEGER array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        True if this routine must look out for bad values in IQUAL.
*        This also tells whether bad values may be put into ODATA.
*     BADVAL = INTEGER (Given)
*        The output value to replace bad input values.
*     MINVAL = INTEGER (Given)
*        The output value to correspond to input LOW.
*     MAXVAL = INTEGER (Given)
*        The output value to correspond to input HIGH. MAXVAL must not
*        equal MINVAL.
*     IDIM1 = INTEGER (Given)
*        The X dimension of IDATA.
*     IDIM2 = INTEGER (Given)
*        The Y dimension of IDATA.
*     IXST = INTEGER (Given)
*        The first pixel in X to be picked from IDATA and to become
*        pixel 1 in ODATA.
*     IYST = INTEGER (Given)
*        The first pixel in Y to be picked from IDATA and to become
*        pixel 1 in ODATA.
*     ODIM1 = INTEGER (Given)
*        The X dimension of ODATA.
*     ODIM2 = INTEGER (Given)
*        The Y dimension of ODATA.
*     IDATA( IDIM1, IDIM2 ) = REAL (Given)
*        The input data array.
*     IQUAL( IDIM1, IDIM2 ) = BYTE (Given)
*        The quality array for the input data.
*     LOW = REAL (Given and Returned)
*        The original data value in IDATA corresponding to MINVAL in
*        ODATA.
*     HIGH = REAL (Given and Returned)
*        The original data value in IDATA corresponding to MAXVAL in
*        ODATA. HIGH must not equal LOW.
*     ODATA( ODIM1, ODIM2 ) = INTEGER (Returned)
*        The outut data array.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     MBT: Mark Taylor (IoA, Starlink)
*     {enter_new_authors_here}

*  History:
*     03 Mar 1993 (HME):
*        Original version.
*     20 Jul 1998 (MBT):
*        Now uses quality array instead of magic values for bad pixels.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      LOGICAL BAD
      INTEGER BADVAL
      INTEGER MINVAL
      INTEGER MAXVAL
      INTEGER IDIM1, IDIM2
      INTEGER IXST, IYST
      INTEGER ODIM1, ODIM2
      REAL IDATA( IDIM1, IDIM2 )
      BYTE IQUAL( IDIM1, IDIM2 )

*  Arguments Given and Returned:
      REAL LOW
      REAL HIGH

*  Arguments Returned:
      INTEGER ODATA( ODIM1, ODIM2 )

*  Local Variables:
      INTEGER I, J, K, L         ! Loop indices
      REAL HILO                  ! HIGH minus LOW
      REAL MAXMIN                ! MAXVAL minus MINVAL
      REAL TEMP                  ! Temporary number
      BYTE GOOD                  ! Quality value indicating good data

*  Constants:
      PARAMETER ( GOOD = 0 )

*.

*  Loop through ODATA, picking the element from IDATA.
      HILO = HIGH - LOW
      MAXMIN = MAXVAL - MINVAL
      L = 1
      IF ( BAD ) THEN
         DO 2 J = IYST, IYST+ODIM2-1
            K = 1
            DO 1 I = IXST, IXST+ODIM1-1
               IF ( IQUAL(I,J) .EQ. GOOD ) THEN
                  TEMP = ( IDATA(I,J) - LOW ) / HILO
                  TEMP = MAX( 0., TEMP )
                  TEMP = MIN( 1., TEMP )
                  ODATA(K,L) = MINVAL + INT( TEMP * MAXMIN + 0.5 )
               ELSE
                  ODATA(K,L) = BADVAL
               END IF
               K = K + 1
 1          CONTINUE
            L = L + 1
 2       CONTINUE
      ELSE
         DO 4 J = IYST, IYST+ODIM2-1
            K = 1
            DO 3 I = IXST, IXST+ODIM1-1
               TEMP = ( IDATA(I,J) - LOW ) / HILO
               TEMP = MAX( 0., TEMP )
               TEMP = MIN( 1., TEMP )
               ODATA(K,L) = MINVAL + INT( TEMP * MAXMIN + 0.5 )
               K = K + 1
 3          CONTINUE
            L = L + 1
 4       CONTINUE
      END IF

*  Return.
      END

      SUBROUTINE FIG_SCLEAN_2(DATA,QUAL,NX,NY,IX,IYST,IYEN,QGOOD,
     :                        XR,YR,NPT)
*+
*  Name:
*     FIG_SCLEAN_2

*  Purpose:
*     Second service routine for SCLEAN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_SCLEAN_2( DATA, QUAL, NX, NY, IX, IYST, IYEN, QGOOD,
*    :                   XR, YR, NPT )

*  Description:
*     This routine takes a column from an NDF Data array and returns
*     a pair of arrays (X and Y) suitable for passing to PGLINE or
*     PGPT to make a (sideways) X-Y plot.
*     According to the value of the parameter QGOOD, the routine will
*     pick out only the good (quality zero) or only the bad (quality
*     nonzero) points.  If the selected set contains no points,
*     NPT is simply returned as 0.

*  Arguments:
*     DATA( NX, NY ) = REAL (Given)
*        Data array.
*     QUAL( NX, NY ) = BYTE (Given)
*        Quality array corresponding to DATA.
*     NX = INTEGER (Given)
*        The X dimension of DATA.
*     NY = INTEGER (Given)
*        The Y dimension of DATA.
*     IX = INTEGER (Given)
*        The column number of DATA to be used.
*     IYST = INTEGER (Given)
*        The first Y coordinate to be used.
*     IYEN = INTEGER (Given)
*        The last Y coordinate to be used.
*     QGOOD = LOGICAL (Given)
*        Select good (True) or bad (False) pixels.
*     XR( NY ) = REAL (Returned)
*        The X values of the points to be plotted.
*     YR( NY ) = REAL (Returned)
*        The Y values of the points to be plotted.
*     NPT = INTEGER (Returned)
*        The number of elements in XR and YR.

*  Authors:
*     MBT: Mark Taylor (IoA, Starlink)

*  History:
*     24 Jul 1998 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! Define BAD constants

*  Arguments Given:
      INTEGER NX,NY
      REAL DATA(NX,NY)
      BYTE QUAL(NX,NY)
      INTEGER IX,IYST,IYEN
      LOGICAL QGOOD

*  Arguments Returned:
      REAL XR(NY)
      REAL YR(NY)
      INTEGER NPT

*  Local variables:
      INTEGER IY
      BYTE GOOD

*  Local Constants:
      PARAMETER ( GOOD = 0 )

*.

*  Initialise.
      NPT=0

*  Loop along column, copying to output arrays either all good or all
*  bad values.  Flagged values are skipped in any case.
      DO IY=IYST,IYEN
         IF (((QUAL(IX,IY).EQ.GOOD) .EQV. QGOOD) .AND.
     :       (DATA(IX,IY).NE.VAL__BADR)) THEN
            NPT=NPT+1
            XR(NPT)=DATA(IX,IY)
            YR(NPT)=IY
         END IF
      END DO

*  Return.
      END

      SUBROUTINE FIG_SCLEAN_OUT(CHR,STRING,VAL1,VAL2,NVAL,PAREN,LOG,LFD,
     :                          STATUS)
*+
*  Name:
*     FIG_SCLEAN_OUT

*  Purpose:
*     Output SCLEAN log message to screen and maybe log file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_SCLEAN_OUT(CHR,STRING,VAL1,VAL2,NVAL,PAREN,LOG,LFD,STATUS)
*
*  Description:
*     This routine takes zero, one or two numerical values and a
*     string and outputs them in two ways: firstly verbosely to
*     standard output (using PAR_WRUSER), and secondly in a terse
*     format to a log file (if the LOG argument is set).

*  Arguments:
*     CHR = CHARACTER*1 (Given)
*        Single character for terse output
*     STRING = CHARACTER*(*) (Given)
*        String for verbose output
*     VAL1 = REAL (Given)
*        First numerical value
*     VAL2 = REAL (Given)
*        Second numerical value
*     NVAL = INTEGER (Given)
*        Number of values to be printed (0 <= NVAL <= 2)
*     PAREN = LOGICAL (Given)
*        True if values are to be printed with parentheses and commas
*     LOG = LOGICAL (Given)
*        True if output is to be additionally to log file
*     LFD = INTEGER (Given)
*        FIO file descriptor if LOG is true.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Authors:
*     MBT: Mark Taylor (IoA, Starlink)

*  History:
*     06 Aug 1998 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER CHR*1, STRING*(*)
      REAL VAL1, VAL2
      INTEGER NVAL, LFD
      LOGICAL PAREN, LOG

*  Arguments Returned:

*  Global Status:
      INTEGER STATUS

*  Functions:
      INTEGER ICH_ENCODE, ICH_LEN

*  Local variables:
      INTEGER INVOKE            ! Dummy variable for function calls.
      INTEGER LNEXT             ! Next free character position in LOGOUT.
      CHARACTER*80 LOGOUT       ! Buffer for terse (logfile) output.
      INTEGER NEXT              ! Next free character position in OUT.
      CHARACTER*80 OUT          ! Buffer for verbose output.
      INTEGER FSTAT             ! Figaro running status.

*.

*  Check inherited status.
      IF (STATUS.NE.SAI__OK) RETURN

*  Initialise.
      OUT = STRING // ' '
      NEXT = ICH_LEN( STRING ) + 2
      LOGOUT = CHR
      LNEXT = 2

*  Prepare terse and verbose output strings.
      IF ( PAREN ) THEN
         OUT( NEXT : NEXT + 1 ) = '( '
         NEXT = NEXT + 2
      END IF
      IF ( NVAL .GE. 1 ) THEN
         INVOKE = ICH_ENCODE( OUT, VAL1, NEXT + 0, 0, NEXT )
         LOGOUT( LNEXT : LNEXT ) = ' '
         LNEXT = LNEXT + 1
         INVOKE = ICH_ENCODE( LOGOUT, VAL1, LNEXT + 0, 0, LNEXT )
         IF ( NVAL .GE. 2 ) THEN
            IF ( PAREN ) THEN
               OUT( NEXT : NEXT ) = ','
               NEXT = NEXT + 1
            END IF
            OUT( NEXT : NEXT ) = ' '
            NEXT = NEXT + 1
            INVOKE = ICH_ENCODE( OUT, VAL2, NEXT + 0, 0, NEXT )
            LOGOUT( LNEXT : LNEXT ) = ' '
            LNEXT = LNEXT + 1
            INVOKE = ICH_ENCODE( LOGOUT, VAL2, LNEXT + 0, 0, LNEXT )
         END IF
      END IF
      IF ( PAREN ) THEN
         OUT( NEXT : NEXT + 2 ) = ' )'
         NEXT = NEXT + 2
      END IF

*  Write verbose string to user.
      FSTAT = 0
      CALL PAR_WRUSER( OUT( : NEXT - 1 ), FSTAT )

*  Write terse string to log file (if log file is open).
      IF ( LOG ) CALL FIO_WRITE( LFD, LOGOUT( : LNEXT - 1), STATUS )

*  Return.
      END

      SUBROUTINE FIG_SCL_FLAG2QUAL(DATA,QUAL,EL,STATUS)
*+
*  Name:
*     FIG_SCL_FLAG2QUAL

*  Purpose:
*     Merge flagged bad pixels into quality array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_SCL_FLAG2QUAL( DATA, QUAL, EL, STATUS )
*
*  Description:
*     This routine examines a data array, and for every flagged bad
*     pixel in it, marks the corresponding quality array element as
*     bad (gives it a value of 255).

*  Arguments:
*     DATA = REAL(EL) (Given)
*        Data array.
*     QUAL = BYTE(EL) (Given and returned)
*        Quality array.
*     EL = INTEGER (Given)
*        Dimension of data and quality arrays.
*     STATUS = INTEGER (Given and returned)
*        Inherited status.

*  Authors:
*     MBT: Mark Taylor (IoA, Starlink)

*  History:
*     10 Aug 1998 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Define BAD constants

*  Arguments Given:
      INTEGER EL, STATUS
      REAL    DATA(EL)

*  Arguments Given and Returned:
      BYTE    QUAL(EL)

*  Local constants:
      BYTE    BADBYT
      PARAMETER ( BADBYT = -127 )

*  Local variables:
      INTEGER I

*.

*  Check global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Merge flagged values into quality array
      DO I = 1, EL
         IF ( DATA( I ) .EQ. VAL__BADR ) QUAL( I ) = BADBYT
      END DO

*  Return
      END

      SUBROUTINE FIG_SCL_QUAL2FLAG(DATA,QUAL,EL,STATUS)
*+
*  Name:
*     FIG_SCL_QUAL2FLAG

*  Purpose:
*     Merge quality array into flagged pixels in data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_SCL_QUAL2FLAG( DATA, QUAL, EL, STATUS )
*
*  Description:
*     This routine examines a quality array, and if any element has
*     a non-zero value, sets the corresponding element of the data
*     array to the flagged value.

*  Arguments:
*     DATA = REAL(EL) (Given and returned)
*        Data array.
*     QUAL = BYTE(EL) (Given)
*        Quality array.
*     EL = INTEGER (Given)
*        Dimension of data and quality arrays.
*     STATUS = INTEGER (Given and returned)
*        Inherited status.

*  Authors:
*     MBT: Mark Taylor (IoA, Starlink)

*  History:
*     10 Aug 1998 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Define BAD constants

*  Arguments Given:
      INTEGER EL, STATUS
      BYTE    QUAL(EL)

*  Arguments Given and Returned:
      REAL    DATA(EL)

*  Local constants:
      BYTE    GOOD
      PARAMETER ( GOOD = 0 )

*  Local variables:
      INTEGER I

*.

*  Check global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Merge flagged values into quality array
      DO I = 1, EL
         IF ( QUAL( I ) .NE. GOOD ) DATA( I ) = VAL__BADR
      END DO

*  Return
      END
