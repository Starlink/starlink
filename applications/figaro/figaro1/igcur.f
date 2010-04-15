*+
      SUBROUTINE IGCUR
*
*     I G C U R
*
*     Services an 'IGCUR' command.   Displays position and
*     data value on the image display, depending on cursor position.
*
*     Command parameters, keywords - None
*
*     User variables used -   (">" input, "<" output)
*
*     (>) IMFILE    (Character) The name of the data currently
*                   displayed.  Set by IGREY or similar.
*     (>) PGENVARG  (Numeric array) The argument list for PGENV that was
*                   used in displaying the image.
*     (<) XPIXELS   (Numeric) The pixel numbers in X for the
*                   points indicated by the cursor.
*     (<) YPIXELS   (Numeric) The pixel numbers in Y for the
*                   points indicated by the cursor.
*     (<) NPIXELS   (Numeric) The number of points selected by
*                   the cursor.  Note: if no points are selected,
*                   the values of NPIXELS, XPIXELS and YPIXELS
*                   are left unchanged.
*
*                                          KS / CIT 20th March 1984
*     Modified:
*
*     Summer 1987  Converted to use DSA_ routines.  DJA/AAO.
*     18 Aug 1988  References to `Grinnell' removed from comments.
*                  Now uses TVSIZE and TVCSIZE to get image display
*                  characteristics (and so will needs a version of
*                  TVPCKG that supports these), so no longer assumes
*                  display is 512 by 512. KS/AAO.
*     05 Feb 1991  Now handles the display parameters as set in IMARRAY
*                  by the latest version of IMAGE, which is not as
*                  limited in the stretching it can apply to an image
*                  to fit it onto the display. KS/AAO.
*     16 Nov 1992  HME / UoE, Starlink.  Adapted and simplified from
*                  ICUR: Use PGCURSE for input and terminal for output.
*                  INCLUDE changed. TABs removed.
*     07 Apr 1993  HME / UoE, Starlink.  Change G format so as to not
*                  truncate numbers >1 to integer.
*     18 Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
*                  file names to 132 chars.
*     2005 June 8  MJC / Starlink  Use CNF_PVAL for pointers to
*                  mapped data.
*+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Functions
      INTEGER ICH_LEN,GEN_BSEARCH,PGBEGIN

*  Maximum number of points that can be selected
      INTEGER MAXPTS
      PARAMETER (MAXPTS=50)

*  Local variables
      REAL      ARRAY(6)         ! Display parameters recorded by IGREY
      INTEGER   DIMS(10)         ! Sizes of the dimensions of the data
      CHARACTER DEVICE*32        ! Plot device name
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      INTEGER   DSLOT            ! Map slot number used for data
      INTEGER   IGNORE           ! Used to ignore status codes
      CHARACTER IMAGE*132        ! Name of image file on current device
      INTEGER   IXEN             !
      INTEGER   IXST             !
      INTEGER   IYEN             !
      INTEGER   IYST             !
      INTEGER   NDELM            ! Total number of elements in the data
      INTEGER   NDIM             ! Dimensionality of input data
                                 ! structure
      INTEGER   NPIX             !
      INTEGER   STAT1            !
      INTEGER   STAT2            !
      INTEGER   STATUS           ! Status return from DSA_xxx routines
      INTEGER   XPTR             ! Dynamic-memory pointer to x-axis data
      REAL      XS(MAXPTS)       !
      INTEGER   XSLOT            ! Map slot number used for x-axis info
      INTEGER   YPTR             ! Dynamic-memory pointer to y-axis data
      REAL      YS(MAXPTS)       !
      INTEGER   YSLOT            ! Map slot number used for y-axis info

*  Initialisation of DSA_ routines
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500

*  Get image display variables
      CALL VAR_GETARY('PGENVARG',6,ARRAY,STAT1)
      CALL VAR_GETCHR('IMFILE',0,0,IMAGE,STAT2)
      IF ((STAT1.NE.0).OR.(STAT2.NE.0))  THEN
         CALL PAR_WRUSER('Unable to obtain display information',IGNORE)
         CALL PAR_WRUSER('Probably no image currently displayed',IGNORE)
         GO TO 500
      END IF

*  Map displayed data.
      CALL DSA_NAMED_INPUT ('IMAGE',IMAGE,STATUS)
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NDELM,STATUS)
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500

*  Get X-axis data
      CALL DSA_MAP_AXIS_DATA ('IMAGE',1,'READ','FLOAT',XPTR,XSLOT,
     :                        STATUS)

*  And the same for the Y-axis
      CALL DSA_MAP_AXIS_DATA ('IMAGE',2,'READ','FLOAT',YPTR,YSLOT,
     :                        STATUS)
      IF (STATUS.NE.0) GOTO 500

*  Work out the pixel range displayed.
      IXST=GEN_BSEARCH(%VAL(CNF_PVAL(XPTR)),DIMS(1),ARRAY(1))
      IXEN=GEN_BSEARCH(%VAL(CNF_PVAL(XPTR)),DIMS(1),ARRAY(2))
      IYST=GEN_BSEARCH(%VAL(CNF_PVAL(YPTR)),DIMS(2),ARRAY(3))
      IYEN=GEN_BSEARCH(%VAL(CNF_PVAL(YPTR)),DIMS(2),ARRAY(4))
      IF (IXEN.LE.IXST .OR. IYEN.LE.IYST) THEN
         CALL PAR_WRUSER('Invalid pixel bounds',STATUS)
         GOTO 500
      END IF

*  Open the image display device
      CALL VAR_GETCHR('SOFT',0,0,DEVICE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to obtain plot variables',STATUS)
         CALL PAR_WRUSER('Probably no plot has been made.',STATUS)
         GO TO 500
      END IF
      STATUS=PGBEGIN(0,DEVICE(:ICH_LEN(DEVICE))//'/APPEND',1,1)
      IF (STATUS.NE.1) THEN
         CALL PAR_WRUSER('Unable to open graphics device',STATUS)
         GO TO 500
      END IF
      STATUS=0

*  Reproduce the PGENV call used for the display.
*  Override last argument, because we want nothing new plotted.
      CALL PGENV(ARRAY(1),ARRAY(2),ARRAY(3),ARRAY(4),
     :   INT(ARRAY(5)+.5),-2)

*  Now do the real work
      CALL IGCUR_WORK(%VAL(CNF_PVAL(DPTR)),DIMS(1),DIMS(2),
     :                IXST,IXEN,IYST,IYEN,%VAL(CNF_PVAL(XPTR)),
     :                %VAL(CNF_PVAL(YPTR)),MAXPTS,XS,YS,NPIX,STATUS)

*  Close down display device
      CALL PGEND

*  Set the XPIXELS, YPIXELS and NPIXELS user variables
      IF (NPIX.GT.0) THEN
         CALL VAR_SETARY('XPIXELS',MAXPTS,XS,IGNORE)
         CALL VAR_SETARY('YPIXELS',MAXPTS,YS,IGNORE)
         CALL VAR_SETNUM('NPIXELS',0,0,FLOAT(NPIX),IGNORE)
      END IF

*  Close down everything
 500  CALL DSA_CLOSE (STATUS)

      END
*+
      SUBROUTINE IGCUR_WORK(IMAGE,IMX,IMY,IXST,IXEN,IYST,IYEN,
     :   XVALS,YVALS,MAXPTS,XS,YS,NPIX,STATUS)
*
*     I G C U R _ W O R K
*
*     Controls an image display cursor, reading the
*     cursor position and displaying the position (in data coordinates)
*     and data at that position.  Points may be enquired by using the
*     qustion mark and selected with the space bar.
*
*     Parameters -   (">" input, "<" output, "!" modified)
*
*     (>) IMAGE     (Real array) The data displayed.
*     (>) IMX       (Integer) The x-dimension of IMAGE
*     (>) IMY       (Integer) The y-dimension of IMAGE
*     (>) XVALS     (Real array) The values for the x-axis of the
*                   data
*     (>) YVALS     (Real array) The values for the y-axis of the
*                   data
*     (>) MAXPTS    (Integer) The maximum number of points that can
*                   be selected.
*     (<) XS        (Real array X(MAXPTS)) The pixel positions in X
*                   selected using the cursor.
*     (<) YS        (Real array X(MAXPTS)) The pixel positions in Y
*                   selected using the cursor.
*     (<) NPIX      (Integer) The number of points selected using the
*                   cursor.
*
*                                            KS / CIT 20th March 1984
*     Modifications:-
*
*     30 Oct 1984 KS/AAO CHRSZY value modified. Should be OK for
*                 ARGS and Grinnell now.  Cursor turned on and off
*                 explicitly.
*     27 Aug 1985 KS/AAO Delay when cursor not moving doubled to
*                 100 msec in attempt to reduce system loading.
*     18 Aug 1988 KS/AAO References to `Grinnell' removed from
*                 comments.  TVSIZE and TVCSIZE calls added to enquire
*                 (rather than assume) image display dimensions.
*     05 Feb 1991 KS/AAO. Now supports the new conventions used by
*                 IMAGE, where NBLOCKX=NBLOXKY=-1 implies that IXWID
*                 and IYWID are the width of the whole displayed image
*                 on the display.
*     16 Nov 1992 HME / UoE, Starlink.  Adapted from GCUR (in ICUR).
*     07 Apr 1993 HME / UoE, Starlink.  Change G format so as to not
*                 truncate numbers >1 to integer.
*+
      IMPLICIT NONE

*  Arguments Given:
      INTEGER IMX, IMY, IXST, IXEN, IYST, IYEN
      INTEGER MAXPTS
      REAL IMAGE( IMX, IMY ), XVALS( IMX ), YVALS( IMY )

*  Arguments Returned:
      INTEGER NPIX
      REAL XS( MAXPTS ), YS( MAXPTS )

*  Status Argument:
      INTEGER STATUS

*  Local Variables:
      INTEGER IX, IY            ! Pixel numbers for X, Y
      INTEGER PGSTAT            ! PGP status
      REAL X, Y                 ! Position returned by PGCURSE
      REAL TEMP                 ! Real buffer
      CHARACTER * ( 1 ) KEY     ! Key returned by PGCURSE
      CHARACTER * ( 78 ) STRING ! Report string

*  Internal References:
      INTEGER PGCURSE           ! PGP cursor input routine

*  Format for screen report:
*  Pixel:  iiii iiii  Coord:  gg.gggggEggg gg.gggggEggg  Data:  gg.gggggEggg
 101  FORMAT( ' Pixel: ', 2I5, '  Coord: ', 2G13.5,
     :   '  Data: ', G13.5 )

*  Check status.
      IF ( STATUS .NE. 0 ) RETURN

*  Tell user what to do.
      CALL PAR_WRUSER( ' ', STATUS )
      CALL PAR_WRUSER( '       Q - to quit', STATUS )
      CALL PAR_WRUSER( ' <Space> - to record position', STATUS )
      CALL PAR_WRUSER( ' Any key - to report data', STATUS )

*  Zero-initialise the array of pixels.
      DO 1 NPIX = 1, MAXPTS
         XS( NPIX ) = 0.
         YS( NPIX ) = 0.
 1    CONTINUE

*  Loop until quit requested (q or Q).
      NPIX = 0
      KEY = '?'
 2    IF ( KEY .NE. 'q' .AND. KEY .NE. 'Q' .AND. NPIX .LT. MAXPTS ) THEN
         PGSTAT = PGCURSE( X, Y, KEY )
         IF ( PGSTAT .NE. 1 ) KEY = 'q'

*     If not quitting, report and possibly record.
         IF ( KEY .NE. 'q' .AND. KEY .NE. 'Q' ) THEN

*        Work out the pixel number.
            TEMP = FLOAT( IXEN - IXST ) *
     :         ( X - XVALS(IXST) ) / ( XVALS(IXEN) - XVALS(IXST) )
            IX = IXST + INT( TEMP + SIGN( .5, TEMP ) )
            TEMP = FLOAT( IYEN - IYST ) *
     :         ( Y - YVALS(IYST) ) / ( YVALS(IYEN) - YVALS(IYST) )
            IY = IYST + INT( TEMP + SIGN( .5, TEMP ) )

*        Work out data value and report to screen.
            IF ( IX .GE. 1 .AND. IX .LE. IMX .AND.
     :           IY .GE. 1 .AND. IY .LE. IMY       ) THEN
               WRITE( STRING, 101 ) IX, IY, X, Y, IMAGE( IX, IY )
               CALL PAR_WRUSER( STRING, STATUS )
            ELSE
               CALL PAR_WRUSER( ' Pixel out of range.', STATUS )
            END IF

*        If pixel to be recorded, do so.
            IF ( KEY .EQ. ' ' ) THEN
               NPIX = NPIX + 1
               XS(NPIX) = FLOAT(IX)
               YS(NPIX) = FLOAT(IY)
               CALL PAR_WRUSER( ' ...   recorded.', STATUS )
            END IF
         END IF
         GO TO 2
      END IF

*  Return.
      END
