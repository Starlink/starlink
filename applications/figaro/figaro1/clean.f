C+
      SUBROUTINE CLEAN
C
C     C L E A N
C
C     Main routine for the Figaro 'CLEAN' command.  Displays
C     a CCD image and then allows the user to move around it with
C     the cursor, selecting rows and columns to be corrected and
C     cosmic rays to be zapped.  The idea is that this routine can
C     be used to fix up any areas in an image that were not fixed
C     automatically by the non-interactive version ('BCLEAN').  It
C     may also give a better idea of the best settings for the
C     BCLEAN parameters.
C
C     Command parameters -
C
C     IMAGE      (Character) The name of the image to be displayed.
C     OUTPUT     (Character) The name of the resulting cleaned image.
C                If the same as IMAGE, the image is cleaned in situ.
C     QUIT       (Logical) Used to confirm quitting the application.
C     DEG        (Integer) Degree of fit to use for interpolation.
C     XSIZE      (Integer) Size of deletion box in X.
C     YSIZE      (Integer) Size of deletion box in Y.
C     HIGH       (Real) Highest displayed data value.
C     LOW        (Real) Lowest displayed data value.
C
C     User variables -  ("<" output, "!" modified)
C
C     (!) IMARRAY (Numeric array) Contains current image display
C                 parameters.
C     (<) IMFILE  (Character) Contains name of currently displayed
C                 image file.
C     (>) IDEV    (Character) Contains name of display device.
C
C                                        KS / CIT 2nd July 1984
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
C     9th  Feb  1998.  ACD / UoE, Starlink. Move the cancelling of
C                      parameters HIGH and LOW to after obtaining their
C                      values rather than before (in subroutine
C                      FIG_CCLEAN).
C     8th  Nov  1998.  ACD / UoE, Starlink. Fixed a couple of instances
C                      where Y axis quantities were computed from variables
C                      pertaining to the X axis rather than the equivalent
C                      Y axis variables (spotted by Mark Taylor).
C     2005 May 31      MJC / Starlink Use CNF_PVAL for pointers to mapped
C                      data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER DSA_TYPESIZE,ICH_LEN,PGBEGIN
C
C     Local variables
C
      REAL         ARRAY(12)    ! Display parameters
      INTEGER      BYTES        ! Number of bytes of workspace required
      INTEGER      COL1,COL2    ! First and last PGPLOT pen
      CHARACTER    DEVNAM*32    ! Name of display device
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      CHARACTER    IMAGE*132    ! The actual name of the image to be cleaned
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      NX           ! Size of 1st dimension
      INTEGER      NY           ! Size of 2nd dimension (if present)
      INTEGER      OPTR         ! Dynamic-memory pointer to output data array
      INTEGER      OSLOT        ! Map slot number outputdata array
      INTEGER      STATUS       ! Running status for DSA_ routines
      INTEGER      WPTR         ! Dynamic-memory pointer to workspace
      INTEGER      WSIZE        ! Number of elements in workspace
      INTEGER      WSLOT        ! Map slot number of workspace
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input file
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
C
C     Get dimensions of data
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NY=DIMS(2)
      IF (STATUS.NE.0) GO TO 500
C
C     Get the name of the output image
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      CALL DSA_USE_FLAGGED_VALUES('OUTPUT',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map the data.
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
C
C     The workspace is only needed for the Undo function, and may be
C     any size (must be more than 3 elements).  The size controls the
C     number of operations that may be Undone.  Here it is set at some
C     multiple of the larger of the image dimensions.
C
      WSIZE=12*MAX(NX,NY)
      BYTES=WSIZE*DSA_TYPESIZE('FLOAT',STATUS)
      CALL DSA_GET_WORKSPACE(BYTES,WPTR,WSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Open display device
C
      CALL VAR_GETCHR('IDEV',0,0,DEVNAM,STATUS)
      STATUS=PGBEGIN(0,DEVNAM(:ICH_LEN(DEVNAM))//'/append',1,1)
      IF (STATUS.NE.1) THEN
         CALL PAR_WRUSER('Failed to open image display device',STATUS)
         GO TO 500
      END IF
      CALL PGASK(.FALSE.)
C
C     Find out the number of colours.
C
      CALL PGQCOL(COL1,COL2)
      IF (COL1.NE.0) THEN
         CALL PAR_WRUSER( 'Cannot use background colour to display ' //
     :      'bad pixels, will use foreground colour instead.', STATUS )
      ELSE IF (COL2.LE.17) THEN
         CALL PAR_WRUSER('CLEAN: Error: Insufficient colours '//
     :      'available for display.', STATUS)
         GO TO 500
      END IF
C
C     Perform the interactive cleaning.
C
      CALL FIG_CCLEAN(%VAL(CNF_PVAL(OPTR)),NX,NY,.TRUE.,
     :                     %VAL(CNF_PVAL(WPTR)),WSIZE,ARRAY,COL1,COL2)
      IF (PAR_ABORT()) GO TO 500
C
C     Set user variable IMARRAY to reflect current display parameters
C
      CALL DSA_GET_ACTUAL_NAME('IMAGE',IMAGE,STATUS)
      CALL VAR_SETARY('IMARRAY',12,ARRAY,STATUS)
      CALL VAR_SETCHR('IMFILE',0,0,IMAGE,STATUS)
C
C     Tidy up
C
  500 CONTINUE
C
C     Close down display
C
      CALL PGEND
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_CCLEAN (DATA,NX,NY,REDRAW,WORK,WSIZE,ARRAY,
     :   BADVAL,COLHI)
C
C     F I G _ C C L E A N
C
C     This is the subroutine that does most of the work for CLEAN.
C     It displays the image on the display device, and allows the
C     user to walk around it with the cursor, indicating bad pixels
C     and bad rows.
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (!) DATA      (Real array IMAGE(NX,NY)) The image data to be
C                   cleaned up.
C     (>) NX        (Integer) The first dimension of IMAGE
C     (>) NY        (Integer) The second dimension of IMAGE
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
C     9th  Feb  1998.  ACD / UoE, Starlink. Move the cancelling of
C                      parameters HIGH and LOW to after obtaining their
C                      values rather than before.
C     8th  Nov  1998.  ACD / UoE, Starlink. Fixed a couple of instances
C                      where Y axis quantities were computed from variables
C                      pertaining to the X axis rather than the equivalent
C                      Y axis variables (spotted by Mark Taylor).
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL REDRAW
      INTEGER NX, NY, WSIZE
      REAL    ARRAY(12), DATA(NX,NY), WORK(1)
      INTEGER BADVAL
      INTEGER COLHI
C
C     Functions
C
      LOGICAL GEN_SIMILAR,PAR_ABORT
      INTEGER ICH_ENCODE
C
C     PRIMDAT constants
C
      INCLUDE 'PRM_PAR'
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Display parameters for FIG_GIMAGE
C
      LOGICAL PLOG, PREFORM
      PARAMETER (PLOG=.FALSE., PREFORM=.FALSE.)
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
      LOGICAL BADPOSN, DONE, REDISPLAY, WHOLE
      INTEGER IMX, IMXP, IMY, IMYP, INVOKE, IX, IX1, IX2, IXEN
      INTEGER IXLIM, IXORG, IXORIG, IXST, IXW, IXWID
      INTEGER IY, IY1, IY2, IYEN, IYLIM, IYORG
      INTEGER IYORIG, IYST, IYWID, LIMX1, LIMX2, LIMY1, LIMY2, NBLOCKX
      INTEGER NBLOCKY, NBOXX, NBOXY, NCOEFF, NEXT
      INTEGER NIPIX, NXP, NYP, STATUS, WFIRST, WLAST, X, XCENT
      INTEGER XMAX, XMIN, XPOSN, XST, Y, YCENT, YMAX, YMIN
      INTEGER YPOSN, YST
      INTEGER DEVX, DEVY
      REAL AMAX, AMIN, FLAG, HIGH, HWAS, LOW, LWAS, MEAN, SIGMA
      REAL SIZE, TOTAL, VALUE, XWRK(MAXPT), ZWRK(MAXPT)
      CHARACTER CHARS*33, CURMESS*24, CHR, STRING*80
      INTEGER IPTR,ISLOT,IGNORE,ICH_FOLD
      REAL RXPOSN,RYPOSN
      REAL WINDOW(4)
C
C     Acceptable command characters
C
      DATA CHARS/'QqCcWwHh?EeXxYyRrNnSsUuLlKkDdTtPp'/
C
C     Default number of coefficients for interpolation, and size of
C     box for "X" and "Y" commands
C
      DATA NBOXX,NBOXY,NCOEFF/5,5,3/
C
C     Error message
C
      DATA CURMESS/'Cursor outside data area'/
C
C     Initialise XCENT, YCENT
C
      XCENT=0
      YCENT=0
C
C     Get an integer work space the same size as the input data.
C
      STATUS=0
      CALL DSA_GET_WORK_ARRAY(NX*NY,'INT',IPTR,ISLOT,STATUS)
C
C     Get display size
C
      CALL PGVPORT(0.,1.,0.,1.)
      CALL PGQVP(3,WINDOW(1),WINDOW(2),WINDOW(3),WINDOW(4))
      CALL PGWINDOW(WINDOW(1),WINDOW(2),WINDOW(3),WINDOW(4))
      DEVX=INT(WINDOW(2)-WINDOW(1))+1
      DEVY=INT(WINDOW(4)-WINDOW(3))+1
C
C     If image is to be redrawn, then get the range of the data,
C     for the first scaling guess.  Also use for value of 'invalid
C     pixel' flag.
C
      IF (REDRAW) THEN
         CALL GEN_ASTATB(DATA,NX,NY,1,NX,1,NY,TOTAL,AMAX,AMIN,
     :                    MEAN,XMAX,XMIN,YMAX,YMIN,SIGMA,SIZE)
         HIGH=MEAN+SIGMA
         LOW=MEAN-SIGMA
         FLAG=VAL__BADR
         WHOLE=.TRUE.
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
         FLAG=VAL__BADR
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
         IXLIM=IXORIG+IMX
         IYLIM=IYORIG+IMY
         REDISPLAY=.FALSE.
         WHOLE=(IXST.EQ.1).AND.(IXEN.EQ.NX).AND.(IYST.EQ.1)
     :                                       .AND.(IYEN.EQ.NY)
      END IF
C
C     Repair scaling limits if equal.
C
      IF (LOW.EQ.HIGH) THEN
         HIGH=LOW+HIGH
         LOW=0.
      END IF
C
C     Initial hints
C
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('Move cursor with mouse, and use '//
     :   'keys to initiate cleaning operations.  Hit the '//
     :   '"?" key or the "H" key to get help.',STATUS)
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
C        Do we have to re-display the data?
C
         IF (REDISPLAY) THEN
            REDISPLAY=.FALSE.
C
C           Set display parameters according to whether we want a
C           'whole' or a 'magnified' display.
C
            IF (WHOLE) THEN
               IXST=1
               IXEN=NX
               IYST=1
               IYEN=NY
               IXWID=MAX(1,DEVX/NX)
               IYWID=MAX(1,DEVY/NY)
               NBLOCKX=(NX+DEVX-1)/DEVX
               NBLOCKY=(NY+DEVY-1)/DEVY
            ELSE
               NBLOCKX=1
               NBLOCKY=1
               IXWID=4
               IYWID=4
               IXST=MAX(1,XCENT-(DEVX/2-IXWID/2)/IXWID)
               IXEN=MIN(NX,XCENT+(DEVX/2-IXWID/2)/IXWID)
               IYST=MAX(1,YCENT-(DEVY/2-IYWID/2)/IYWID)
               IYEN=MIN(NY,YCENT+(DEVY/2-IYWID/2)/IYWID)
            END IF
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
            IXORIG=(DEVX-IMX)/2
            IYORIG=(DEVY-IMY)/2
            IXLIM=IXORIG+IMX
            IYLIM=IYORIG+IMY
C
C           Display the image - or magnified part of it.
C           We want to erase and then display the data subset from
C           IX/YXT to IX/YEN. This is to go into a displaylet of size
C           IMX by IMY with origin at IX/YORIG.
C
            WINDOW(1)=FLOAT(IXORIG)
            WINDOW(2)=WINDOW(1)+FLOAT(IMX)
            WINDOW(3)=FLOAT(IYORIG)
            WINDOW(4)=WINDOW(3)+FLOAT(IMY)
            CALL PGPOINT(1,WINDOW(1),WINDOW(3),-1)
            CALL PGPAGE
            CALL FIG_CLEAN_1(.TRUE.,BADVAL,MINVAL,COLHI,
     :         NX,NY,IXST,IYST,IXEN-IXST+1,IYEN-IYST+1,
     :         DATA,LOW,HIGH,%VAL(CNF_PVAL(IPTR)))
            CALL PGPIXL(%VAL(CNF_PVAL(IPTR)),IXEN-IXST+1,IYEN-IYST+1,
     :         1,IXEN-IXST+1,1,IYEN-IYST+1,
     :         WINDOW(1),WINDOW(2),WINDOW(3),WINDOW(4))
         END IF
C
C        Get the cursor position and command key
C
         CALL PGCURSE( RXPOSN, RYPOSN, CHR )
         XPOSN=RXPOSN
         YPOSN=RYPOSN
         IGNORE=ICH_FOLD(CHR)
C        XPOSN=XPOSN-1
C        YPOSN=YPOSN-1
         IF (IXWID.EQ.1) THEN
            X=(XPOSN-IXORIG)*NBLOCKX+IXST
         ELSE
            X=((XPOSN-IXORIG)/IXWID)+IXST
         END IF
         IF (IYWID.EQ.1) THEN
            Y=(YPOSN-IYORIG)*NBLOCKY+IYST
         ELSE
            Y=((YPOSN-IYORIG)/IYWID)+IYST
         END IF
         BADPOSN=((X.LT.1).OR.(X.GT.NX).OR.(Y.LT.1).OR.(Y.GT.NY))
C
C        What follows is a 'CASE' construct, switched on the
C        command character entered.
C
         IF (CHR.EQ.'Q') THEN
C
C           'Q' is for 'QUIT'
C
            CALL PAR_CNPAR('QUIT')
            CALL PAR_RDKEY('QUIT',.FALSE.,DONE)
            IF (PAR_ABORT()) RETURN
C
         ELSE IF (CHR.EQ.'W') THEN
C
C           'W' is for 'Whole' - ie display whole image
C
            REDISPLAY=.NOT.WHOLE
            WHOLE=.TRUE.
C
         ELSE IF (CHR.EQ.'E') THEN
C
C           'E' is for 'Expand' - ie display magnified image
C           around current cursor position.
C
            IF (BADPOSN) THEN
               CALL PAR_WRUSER(CURMESS,STATUS)
            ELSE
               REDISPLAY=WHOLE
               WHOLE=.FALSE.
               XCENT=X
               YCENT=Y
            END IF
C
         ELSE IF (CHR.EQ.'D') THEN
C
C           'D' is for 'Degree' - set degree of fit in interpolation
C
            CALL PAR_CNPAR('DEG')
            CALL PAR_RDVAL('DEG',0.,7.,FLOAT(NCOEFF-1),' ',VALUE)
            IF (PAR_ABORT()) RETURN
            NCOEFF=VALUE+1
C
         ELSE IF (CHR.EQ.'N') THEN
C
C           'N' is for 'Number' of pixels on side of area deleted
C               by 'X' or 'Y'.
C
            CALL PAR_CNPAR('XSIZE')
            CALL PAR_RDVAL('XSIZE',1.,20.,FLOAT(NBOXX),'Pixels',VALUE)
            IF (PAR_ABORT()) RETURN
            NBOXX=VALUE
            CALL PAR_CNPAR('YSIZE')
            CALL PAR_RDVAL('YSIZE',1.,20.,FLOAT(NBOXY),'Pixels',VALUE)
            IF (PAR_ABORT()) RETURN
            NBOXY=VALUE
C
         ELSE IF (CHR.EQ.'S') THEN
C
C           'S' is for 'Stretch' - ie change display scale limits
C
            HWAS=HIGH
            LWAS=LOW
            CALL PAR_WRUSER('Set stretch',STATUS)
            CALL PAR_RDVAL('HIGH',-3E30,3E30,HIGH,' ',VALUE)
            CALL PAR_CNPAR('HIGH')
            IF (PAR_ABORT()) RETURN
            HIGH=VALUE
            CALL PAR_RDVAL('LOW',-3E30,3E30,LOW,' ',VALUE)
            CALL PAR_CNPAR('LOW')
            IF (PAR_ABORT()) RETURN
            LOW=VALUE
            IF (LOW.EQ.HIGH) THEN
               CALL PAR_WRUSER(
     :            'Cannot have the same value for low and high',STATUS)
               HIGH=HWAS
               LOW=LWAS
            END IF
            REDISPLAY=.NOT.(GEN_SIMILAR(HIGH,HWAS).AND.
     :                                        GEN_SIMILAR(LOW,LWAS))
C
         ELSE IF ((CHR.EQ.'?').OR.(CHR.EQ.'H')) THEN
C
C           '?' or 'H' is for 'HELP'
C
            CALL FIG_CLHELP
C
         ELSE IF ((CHR.EQ.'R').OR.(CHR.EQ.'L')) THEN
C
C           'R' is for 'Row' - ie delete and fix the indicated bad row.
C           'L' is for 'Line' - delete the indicated row but don't fix.
C
            IF (BADPOSN) THEN
               CALL PAR_WRUSER(CURMESS,STATUS)
            ELSE
               IF (NBLOCKY.NE.1) THEN
                  CALL PAR_WRUSER('Display cannot resolve a single line'
     :                                                          ,STATUS)
                  CALL PAR_WRUSER(
     :               'You will have to use a magnified display',STATUS)
               ELSE
                  CALL FIG_CLSAVE(DATA,NX,NY,1,NX,Y,Y,'R',WORK,WSIZE,
     :                                                   WFIRST,WLAST)
                  STRING='Bad row # '
                  INVOKE=ICH_ENCODE(STRING,FLOAT(Y),11,0,NEXT)
                  CALL PAR_WRUSER(STRING(:NEXT-1),STATUS)
                  CALL FIG_PUNCHOUT(DATA,NX,NY,1,Y,NX,Y,FLAG)
                  IYORG=IYORIG+(Y-IYST)*IYWID
                  IF (CHR.EQ.'R') THEN
                     CALL FIG_FIXROWS(DATA,NX,NY,1,NX,1,NY,1,Y,NCOEFF,
     :                                                            FLAG)
                  END IF
                  REDISPLAY=.TRUE.
               END IF
            END IF
C
         ELSE IF ((CHR.EQ.'C').OR.(CHR.EQ.'K')) THEN
C
C           'C' is for 'Column' - ie delete and fix the indicated column
C           'K' is for 'Kolumn' - delete column but don't fix
C
            IF (BADPOSN) THEN
               CALL PAR_WRUSER(CURMESS,STATUS)
            ELSE
               IF (NBLOCKX.NE.1) THEN
                  CALL PAR_WRUSER(
     :               'Display cannot resolve a single column',STATUS)
                  CALL PAR_WRUSER(
     :               'You will have to use a magnified display',STATUS)
               ELSE
                  CALL FIG_CLSAVE(DATA,NX,NY,X,X,1,NY,'C',WORK,WSIZE,
     :                                                   WFIRST,WLAST)
                  STRING='Bad column # '
                  INVOKE=ICH_ENCODE(STRING,FLOAT(X),14,0,NEXT)
                  CALL PAR_WRUSER(STRING(:NEXT-1),STATUS)
                  CALL FIG_PUNCHOUT(DATA,NX,NY,X,1,X,NY,FLAG)
                  IXORG=IXORIG+(X-IXST)*IXWID
                  IXW=MAX(IXWID,2)
                  IF (CHR.EQ.'C') THEN
                     CALL FIG_FIXCOLS(DATA,NX,NY,1,NX,1,NY,1,X,NCOEFF,
     :                                                             FLAG)
                  END IF
                  REDISPLAY=.TRUE.
               END IF
            END IF
C
         ELSE IF (CHR.EQ.'P') THEN
C
C           'P' is for display current cursor Position
C
            IF (BADPOSN) THEN
               CALL PAR_WRUSER(CURMESS,STATUS)
            ELSE
               STRING='Cursor is at ('
               INVOKE=ICH_ENCODE(STRING,FLOAT(X),15,0,NEXT)
               STRING(NEXT:NEXT)=','
               INVOKE=ICH_ENCODE(STRING,FLOAT(Y),NEXT+1,0,NEXT)
               STRING(NEXT:NEXT)=')'
               CALL PAR_WRUSER(STRING(:NEXT),STATUS)
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
                  CALL PAR_WRUSER(STRING(:NEXT+5),STATUS)
               END IF
            END IF
C
         ELSE IF ((CHR.EQ.'X').OR.(CHR.EQ.'Y')) THEN
C
C           'X' is for fix by interpolation in the 'X' direction,
C           'Y' is for fix by interpolation in the 'Y' direction.
C
            IF (BADPOSN) THEN
               CALL PAR_WRUSER(CURMESS,STATUS)
            ELSE
               IX1=MAX(1,X-NBOXX/2)
               IX2=MIN(NX,X+NBOXX/2)
               IY1=MAX(1,Y-NBOXY/2)
               IY2=MIN(NY,Y+NBOXY/2)
               CALL FIG_CLSAVE(DATA,NX,NY,IX1,IX2,IY1,IY2,CHR,WORK,
     :                                            WSIZE,WFIRST,WLAST)
               CALL FIG_PUNCHOUT(DATA,NX,NY,IX1,IY1,IX2,IY2,FLAG)
               IF (IXWID.EQ.1) THEN
                  NIPIX=(IX1-IXST)/NBLOCKX
                  IXORG=IXORIG+NIPIX
                  XST=IXST+NBLOCKX*NIPIX
                  IMXP=IXORIG+((IX2-IXST)/NBLOCKX)-IXORG
               ELSE
                  XST=IX1
                  IXORG=IXORIG+(IX1-IXST)*IXWID
                  IMXP=(IX2-IX1+1)*IXWID
               END IF
               IF (IYWID.EQ.1) THEN
                  NIPIX=(IY1-IYST)/NBLOCKY
                  IYORG=IYORIG+NIPIX
                  YST=IYST+NBLOCKY*NIPIX
                  IMYP=IYORIG+((IY2-IYST)/NBLOCKY)-IYORG
               ELSE
                  YST=IY1
                  IYORG=IYORIG+(IY1-IYST)*IYWID
                  IMYP=(IY2-IY1+1)*IYWID
               END IF
               IMYP=MAX(2,IMYP)
               IMXP=MAX(2,IMXP)
               IF (CHR.EQ.'X') THEN
                  LIMX1=IX1-2*NCOEFF
                  LIMX2=IX2+2*NCOEFF
                  DO IY=IY1,IY2
                     CALL FIG_HORIZONTAL(DATA,NX,NY,IY,IX1,IX2,LIMX1,
     :                        LIMX2,NCOEFF,FLAG,MAXPT,XWRK,ZWRK,STATUS)
                  END DO
               ELSE
                  LIMY1=IY1-2*NCOEFF
                  LIMY2=IY2+2*NCOEFF
                  DO IX=IX1,IX2
                     CALL FIG_VERTICAL(DATA,NX,NY,IX,IY1,IY2,LIMY1,
     :                        LIMY2,NCOEFF,FLAG,MAXPT,XWRK,ZWRK,STATUS)
                  END DO
               END IF
               REDISPLAY=.TRUE.
            END IF
         ELSE IF (CHR.EQ.'T') THEN
C
C           'T' is for 'Test' - see what BCLEAN settings would be
C               needed to pick up cosmic rays in the area round the
C               indicated pixel.
C
            IF (BADPOSN) THEN
               CALL PAR_WRUSER(CURMESS,STATUS)
            ELSE
               IX1=MAX(1,X-NBOXX/2)
               IX2=MIN(NX,X+NBOXX/2)
               IY1=MAX(1,Y-NBOXY/2)
               IY2=MIN(NY,Y+NBOXY/2)
               CALL FIG_GRSTAT(DATA,NX,NY,IX1,IX2,IY1,IY2,FLAG)
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
            CALL FIG_CLRSTR(DATA,NX,NY,IX1,IX2,IY1,IY2,WORK,WSIZE,
     :                                                   WFIRST,WLAST)
            IF (IX1.GT.0) THEN
               IF (IXWID.EQ.1) THEN
                  NIPIX=(IX1-IXST)/NBLOCKX
                  IXORG=IXORIG+NIPIX
                  XST=IXST+NBLOCKX*NIPIX
                  IMXP=IXORIG+((IX2-IXST)/NBLOCKX)-IXORG
               ELSE
                  XST=IX1
                  IXORG=IXORIG+(IX1-IXST)*IXWID
                  IMXP=(IX2-IX1+1)*IXWID
               END IF
               IF (IYWID.EQ.1) THEN
                  NIPIX=(IY1-IYST)/NBLOCKY
                  IYORG=IYORIG+NIPIX
                  YST=IYST+NBLOCKY*NIPIX
                  IMYP=IYORIG+((IY2-IYST)/NBLOCKY)-IYORG
               ELSE
                  YST=IY1
                  IYORG=IYORIG+(IY1-IYST)*IYWID
                  IMYP=(IY2-IY1+1)*IYWID
               END IF
               IMYP=MAX(2,IMYP)
               IMXP=MAX(2,IMXP)
               REDISPLAY=.TRUE.
            END IF
C
         ELSE
            CALL PAR_WRUSER('Sorry, not implemented yet',STATUS)
         END IF
      END DO
C
C     Set display values array
C
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
C
      END
C+
      SUBROUTINE FIG_CLHELP
C
C     F I G _ C L H E L P
C
C     Outputs help information for 'CLEAN'.
C
C     Common variables - None
C
C                                       KS / CIT 2nd March 1984
C     Modified:
C
C     17th Dec 1987.  KS / AAO.  'P' option added.
C+
      IMPLICIT NONE
C
C     Local variables
C
      INTEGER STATUS
C
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER(
     :   'Use the joystick or the numeric keypad to move the cursor.',
     :                                                        STATUS)
      CALL PAR_WRUSER(
     :   'The PF1..PF4 keys act as gears for the cursor speed',STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('The following keys are recognised -',STATUS)
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('W - display Whole image',STATUS)
      CALL PAR_WRUSER('E - Expand image around cursor position',STATUS)
      CALL PAR_WRUSER(
     :   'R - delete indicated Row (horizontal line) & fix it',STATUS)
      CALL PAR_WRUSER(
     :   'C - delete indicated Column (vertical line) & fix it',STATUS)
      CALL PAR_WRUSER(
     :   'X - delete indicated area and fix by interpolation in the'
     :                                                         ,STATUS)
      CALL PAR_WRUSER(
     :   '    x - ie horizontal - direction',STATUS)
      CALL PAR_WRUSER(
     :   'Y - like X, but uses vertical interpolation',STATUS)
      CALL PAR_WRUSER(
     :   'S - set stretch, ie High & low limits for display',STATUS)
      CALL PAR_WRUSER(
     :   'L - delete indicated line, but don''t fix',STATUS)
      CALL PAR_WRUSER(
     :   'K - delete indicated column, but don''t fix',STATUS)
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
      SUBROUTINE FIG_GRSTAT (DATA,NX,NY,IX1,IX2,IY1,IY2,FLAG)
C
C     F I G _ G R S T A T
C
C     Given an area of the image to be fixed, calculates the
C     cosmic ray parameters that would be needed for BCLEAN to
C     have chosen this area.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) DATA    (Real array DATA(NX,NY)) The image data
C     (>) NX      (Integer) First dimension of DATA
C     (>) NY      (Integer) Second dimension of DATA
C     (>) IX1     (Integer) The area in question has its
C     (>) IX2     (Integer) bottom left corner at DATA(IX1,IY1)
C     (>) IY1     (Integer) and its top right corner at
C     (>) IY2     (Integer) DATA(IX2,IY2).
C     (>) FLAG    (Real) Invalid pixel value.
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
      REAL    DATA(NX,NY),FLAG
C
C     Functions
C
      INTEGER ICH_ENCODE
      REAL    FIG_CLOSEST
C
C     Local variables
C
      LOGICAL PEAK
      INTEGER INVOKE, IX, IY, NEXT, NRAYS, STATUS
      REAL    AVE, AVEX, AVEY, D0, D1, D2, D3, D4, EXCESS, FRAC
      REAL    NSIG, SIGMA
      CHARACTER STRING*80
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
               D1=FIG_CLOSEST(DATA,NX,NY,IX+1,IY,FLAG)
               IF (D0.GT.D1) THEN
                  D2=FIG_CLOSEST(DATA,NX,NY,IX,IY+1,FLAG)
                  IF (D0.GT.D2) THEN
                     D3=FIG_CLOSEST(DATA,NX,NY,IX-1,IY,FLAG)
                     IF (D0.GT.D3) THEN
                        D4=FIG_CLOSEST(DATA,NX,NY,IX,IY-1,FLAG)
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
               NSIG=EXCESS/SQRT(ABS(AVE))
               FRAC=EXCESS/AVE
               STRING='Possible cosmic ray at ('
               INVOKE=ICH_ENCODE(STRING,FLOAT(IX),25,0,NEXT)
               STRING(NEXT:NEXT)=','
               INVOKE=ICH_ENCODE(STRING,FLOAT(IY),NEXT+1,0,NEXT)
               STRING(NEXT:)='), would be detected by BCLEAN with'
               CALL PAR_WRUSER(STRING(:NEXT+34),STATUS)
               STRING='settings of Crsig = '
               INVOKE=ICH_ENCODE(STRING,NSIG,21,2,NEXT)
               STRING(NEXT:)=', Crfrac = '
               INVOKE=ICH_ENCODE(STRING,FRAC,NEXT+12,2,NEXT)
               STRING(NEXT:)=', Crminv = '
               INVOKE=ICH_ENCODE(STRING,EXCESS,NEXT+12,2,NEXT)
               CALL PAR_WRUSER(STRING(:NEXT-1),STATUS)
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
      SUBROUTINE FIG_CLSAVE(DATA,NX,NY,IX1,IX2,IY1,IY2,CHR,WORK,WSIZE,
     :                                                   WFIRST,WLAST)
C
C     F I G _ C L S A V E
C
C     Saves the data about to be modified so that the operation may be
C     undone.  The actual data, together with enough details about the
C     operation to allow its undoing, is stored in a circular work
C     array.
C
C     Parameters - (">" input, "<" output, "!" modified)
C
C     (>) DATA     (Real array DATA(NX,NY)) The image being cleaned.
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
C     (>) CHR      (Character) A character indicating the CLEAN command
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
      REAL    DATA(NX,NY), WORK(1)
      CHARACTER CHR*1
C
C     Local variables
C
      LOGICAL CANCEL
      INTEGER IX, IY, NWORK, STATUS, WEND, WLIM, WPTR, WVAL
C
C     Note the structure of an entry in the circular buffer.  Taking
C     element 1 as the first for this entry, the elements contain:
C     1: The number of the last element in the entry.
C     2: IX1 (as a real number)
C     3: IX2 (      "         )
C     4: IY1 (      "         )
C     5: IY2 (      "         )
C     6: CHR (      "         )
C     7..penultimate element: The array data to be saved (IX1..IX2,IY1..IY2)
C     Final element: The number of the first element in the entry.
C
C     If the elements of the entry reach the end of the buffer, they
C     start immediately at the beginning.  The element at which the first
C     (earliest) entry in the buffer starts is WFIRST, the element at
C     which the last (latest) entry ends is WLAST.
C
C
C     Make sure that the work array is large enough for the
C     operation information.
C
      NWORK=((IX2-IX1+1)*(IY2-IY1+1))+7
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
         DO IY=IY1,IY2
            DO IX=IX1,IX2
               WPTR=WPTR+1
               IF (WPTR.GT.WSIZE) WPTR=1
               WORK(WPTR)=DATA(IX,IY)
            END DO
         END DO
      END IF
C
      END
C+
      SUBROUTINE FIG_CLRSTR(DATA,NX,NY,IX1,IX2,IY1,IY2,WORK,WSIZE,
     :                                                 WFIRST,WLAST)
C
C     F I G _ C L R S T R
C
C     Restores the last operation saved in the workspace.  This routine
C     performs the data restoration part of an Undo operation, and returns
C     the data limits that have been changed so the calling routine can
C     redisplay the relevant part of the image.
C
C     Parameters - (">" input, "<" output, "!" modified)
C
C     (!) DATA     (Real array DATA(NX,NY)) The image being cleaned.
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
C     (>) WORK     (Real array WORK(WSIZE)) The work array used to
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
      REAL    DATA(NX,NY), WORK(1)
C
C     Functions
C
      INTEGER ICH_ENCODE
C
C     Local variables
C
      LOGICAL VALID
      INTEGER INVOKE, IX, IY, NEXT, STATUS, WPTR
      REAL XWAS, YWAS
      CHARACTER CHWAS*1, STRING*80
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
            CALL PAR_WRUSER(STRING(:NEXT),STATUS)
         ELSE IF (CHWAS.EQ.'C') THEN
            STRING='Restoring column '
            INVOKE=ICH_ENCODE(STRING,FLOAT(IX1),18,0,NEXT)
            CALL PAR_WRUSER(STRING(:NEXT),STATUS)
         ELSE IF ((CHWAS.EQ.'X').OR.(CHWAS.EQ.'Y')) THEN
            STRING='Restoring area around ('
            XWAS=NINT(FLOAT(IX1+IX2)*0.5)
            YWAS=NINT(FLOAT(IY1+IY2)*0.5)
            INVOKE=ICH_ENCODE(STRING,XWAS,24,0,NEXT)
            STRING(NEXT:NEXT)=','
            INVOKE=ICH_ENCODE(STRING,YWAS,NEXT+1,0,NEXT)
            STRING(NEXT:NEXT)=')'
            CALL PAR_WRUSER(STRING(:NEXT),STATUS)
         ELSE
            CALL PAR_WRUSER('Internal error: invalid operation saved.',
     :                                                          STATUS)
            VALID=.FALSE.
         END IF
         IF (VALID) THEN
            DO IY=IY1,IY2
               DO IX=IX1,IX2
                  WPTR=WPTR+1
                  IF (WPTR.GT.WSIZE) WPTR=1
                  DATA(IX,IY)=WORK(WPTR)
               END DO
            END DO
         END IF
      END IF
C
      END
      SUBROUTINE FIG_CLEAN_1( BAD, BADVAL, MINVAL, MAXVAL,
     :   IDIM1, IDIM2, IXST, IYST, ODIM1, ODIM2,
     :   IDATA, LOW, HIGH, ODATA )
*+
*  Name:
*     FIG_CLEAN_1

*  Purpose:
*     First service routine for CLEAN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_CLEAN_1( BAD, BADVAL, MINVAL, MAXVAL,
*        IDIM1, IDIM2, IXST, IYST, ODIM1, ODIM2,
*        IDATA, LOW, HIGH, ODATA )

*  Description:
*     This routine picks a subset from a REAL array and scales it into
*     an INTEGER array.

*  Arguments:
*     BAD = LOGICAL (Given)
*        True if this routine must look out for bad values in IDATA.
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
*     {enter_new_authors_here}

*  History:
*     03 Mar 1993 (HME):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! Global PRIMDAT constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER BADVAL
      INTEGER MINVAL
      INTEGER MAXVAL
      INTEGER IDIM1, IDIM2
      INTEGER IXST, IYST
      INTEGER ODIM1, ODIM2
      REAL IDATA( IDIM1, IDIM2 )

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

*.

*  Loop through ODATA, picking the element from IDATA.
      HILO = HIGH - LOW
      MAXMIN = MAXVAL - MINVAL
      L = 1
      IF ( BAD ) THEN
         DO 2 J = IYST, IYST+ODIM2-1
            K = 1
            DO 1 I = IXST, IXST+ODIM1-1
               IF ( IDATA(I,J) .NE. VAL__BADR ) THEN
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

