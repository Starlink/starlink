C+
      SUBROUTINE SDIST
C
C     S D I S T
C
C     Determines the s-distortion parameters for a star spectrum.
C     This program requires as input an image containing at least one
C     stellar spectrum.  For each spectrum it is to examine, it
C     assumes that a point has been indicated somewhere in the center
C     of the spectrum.  Starting at that point, it attempts to follow
C     the spectrum and so map out the distortion.
C
C     Command parameters -
C
C     IMAGE      (Character) the image containing the spectra.
C     COLUMNS    (Numeric) The number of columns to be added
C                together when tracing the spectra.
C     TRACE      (Character) Controls the algorithm used to follow
C                the spectra.  Only the first character is significant.
C                'E' (Edges) Indicates that the data profile is a
C                'top-hat'shape - as you might get from a continuum
C                source through a dekker.  If EDGES is specified, an
C                edge-locating algorithm is used, and the width of the
C                top hat is assumed to be approximately WIDTH*2.
C                'C' is the same as 'E', except that the center is
C                taken as the center of gravity of the data within the
C                edges, while 'E' takes it as the average of the edges.
C                'G' (Gaussian) Indicates that the profile is roughly
C                gaussian, of half width WIDTH.
C     WIDTH      (Numeric) The expected half-width of the spectra in
C                pixels.
C     MAXDEG     (Numeric) The maximum degree polynomial to be
C                fitted.
C
C     Command keywords -
C
C     SOFTD      Indicates that the fit results are to be plotted on
C                the current soft graphics device.
C     DIAGNOSE   Requests more detailed diagnostics on the tracing.
C     NEXT       Used to pause between plot pages.
C
C     User variables -  (">" input, "<" output)
C
C     (>) IMARRAY  (Numeric array) Contains the parameters that
C                  specify the current image display.  See IMAGE
C                  for full details.
C     (>) NPIXELS  (Numeric) Number of points selected using ICUR
C     (>) XPIXELS  (Numeric array) X positions of points selected
C     (>) YPIXELS  (Numeric array) Y      "    "     "      "
C
C     Output files -
C
C     SDIST.DAT contains the results of the fit(s), in a format that
C               can be treated as follows -
C
C               3 header lines, all beginning with '*'
C               One line giving the number of spectra traced, and the
C               dimensions of the image used, in the format
C               20X,I5,15X,I8,4X,I8.
C               Then, for each spectrum traced, one record giving
C               the spectrum number, and the leftmost and rightmost
C               pixels covered by the trace, in format
C               11X,I5,17X,I5,4X,I5, then 1 record giving the average
C               Y value in the spectrum, and the width of the
C               spectrum, in format 16X,F13.7,10X,F9.2,
C               which is followed by 3 records giving the 11
C               polynomial coefficients for the fit, in 3D23.16.
C               Coefficients are given constant first, with any unused
C               coefficients set to zero.
C
C                                             KS / CIT 5th Feb 1984
C     Modified:
C
C      2nd Apr 1985  KS/ AAO. Revised to use NAG version of FIG_DXYFIT.
C                    Workspace used, and call to FIG_GETDIS changed.
C      7th Aug 1987  DJA/AAO.  Rewritten to use DSA_ routines.
C                    Now uses DYN_ routines for dynamic memory handling
C     22nd Mar 1988  KS / AAO. Modified for GKS version of PGPLOT.
C                    Workspace usage corrected, use of STATUS made
C                    uniform.
C     26th May 1988  KS/AAO. Maximum number of spectra increased to 50.
C                    TRACE parameter added.
C     31st May 1988  KS/AAO. DIAG keyword added. Image dimensions and
C                    spectrum width added to output format.
C     16th Aug 1988  KS/AAO. WIDTH limit increased.  Number of records
C                    for coefficients (4) corrected in comments.
C     31st Oct 1992  HME / UoE, Starlink.  Lower case file name
C                    sdist.dat. INCLUDE changed, TABs removed. DISPLAY
C                    always false, no TVP calls. SOFT parameter becomes
C                    SOFTD.
C     27th Jul 1993  HME / UoE, Starlink.  Disuse PAR_Q*, use
C                    PAR_ABORT. Added parameter NEXT.
C     16th Feb 1995  HME / UoE, Starlink. In the big workspace move
C                    the DOUBLE workspace to the front. Otherwise the
C                    odd number of FLOAT workspaces combined with an
C                    odd number of channels in the input spectrum
C                    cause the DOUBLE workspace to be misaligned
C                    (memory address and odd multiple of 4).
C     18th Apr 1995  HME / UoE, Starlink.  No longer use NAG, need more
C                    workspace for FIG_DXYFIT. Get workspaces by
C                    separate calls and use DSA_GET_WORK_ARRAY.
C     26th Jul 1995  HME / UoE, Starlink.  Implement the changes
C                    contained in AAO's version 4.2:
C                    KS/AAO. Increased tolerance to allow for larger
C                    detectors (now varies with WIDTH, was just 2
C                    pixels).  Introduced the 'BALANCE' algorithm.
C                    KS/AAO. Changes as determined by SJM for the
C                    portable Figaro version made to this newer version
C                    of SDIST.
C     18th Jul 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                    file names to 132 chars.
C     9th  Jun 1999  TDCA / Starlink, RAL. Removed unsupported keywords
C                    from OPEN statements.
C     2005 June 15   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      LOGICAL PAR_ABORT
      INTEGER ICH_FOLD, ICH_LEN
C
C     Maximum number of spectra
C
      INTEGER MAXSPEC
      PARAMETER (MAXSPEC=50)
C
C     Local variables
C
      REAL      ARRAY(10)        !
      DOUBLE PRECISION COEFFS(11,MAXSPEC) !
      LOGICAL   DIAG             ! Flags diagnostics required
      INTEGER   DIMS(10)         ! Sizes of dimensions of data
      LOGICAL   DISPLY           ! See above
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      INTEGER   DSLOT            ! Map slot number of input data array
      INTEGER   FILE             ! Logical unit of output file
      LOGICAL   FOPEN            ! TRUE if the output file was opened
      INTEGER   FSTATUS          ! Status from I/O operations
      INTEGER   I                !
      INTEGER   IGNORE           ! Used to pass ignorable status value
      CHARACTER IMAGE*132        ! The actual name of the image
      INTEGER   INVOKE           ! Dummy function value
      INTEGER   IWPTR            ! Dynamic-memory pointer to workspace
      INTEGER   IX1(MAXSPEC)     !
      INTEGER   IX2(MAXSPEC)     !
      INTEGER   J                !
      LOGICAL   LSOFT            ! See above
      INTEGER   MAXD             ! See above
      INTEGER   NCOL             ! See above
      INTEGER   NDIM             ! Number of dimensions in data
      INTEGER   NELM             ! Total number of elements in data
      INTEGER   NX               ! Size of 1st dimension
      INTEGER   NY               ! Size of 2nd dimension
      INTEGER   NXYS             !
      CHARACTER SOFT*32          ! The name of the current soft device
      INTEGER   SPTR             ! Dynamic-memory pointer to workspace
      INTEGER   STATUS           ! Running status for DSA_ routines
      CHARACTER STRING*64        ! String used to hold output text
      CHARACTER TRACE*16         ! Trace mode.
      REAL      VALUE            ! Temporary real number
      INTEGER   VSTATUS          ! Status from VAR_ routines
      REAL      WIDTH            ! See above
      REAL      WIDTHS(MAXSPEC)  ! Widths of the spectra fitted
      INTEGER   WPTR             ! Dynamic-memory pointer to workspace
      INTEGER   WSLOT            ! Map slot number of workspace
      REAL      XVS(MAXSPEC)     !
      INTEGER   XWPTR            ! Dynamic-memory pointer to workspace
      REAL      YAVS(MAXSPEC)    !
      REAL      YVS(MAXSPEC)     !
      INTEGER   YWPTR            ! Dynamic-memory pointer to workspace
C
C     I/O Logical unit
C
      DATA FILE/1/
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of image containing spectra and open it.
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      CALL DSA_GET_ACTUAL_NAME('IMAGE',IMAGE,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get size of data and map it
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF (NDIM.NE.2) THEN
         CALL PAR_WRUSER('Data is not two-dimensional',IGNORE)
         GO TO 500
      END IF
      NX=DIMS(1)
      NY=DIMS(2)
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get X and Y positions.
C
      CALL VAR_GETNUM('NPIXELS',0,0,VALUE,VSTATUS)
      IF (VSTATUS.EQ.0) THEN
         NXYS=MIN(MAXSPEC,INT(VALUE))
         IF (NXYS.LE.0) THEN
            CALL PAR_WRUSER('Zero or -ve number of pixels selected',
     :                      IGNORE)
            VSTATUS=-1
         ELSE
            CALL VAR_GETARY('XPIXELS',NXYS,XVS,VSTATUS)
            IF (VSTATUS.EQ.0) THEN
               CALL VAR_GETARY('YPIXELS',NXYS,YVS,VSTATUS)
            END IF
         END IF
      END IF
      IF (VSTATUS.NE.0) THEN
         CALL PAR_WRUSER(
     :     'Unable to get x-y position(s) for central parts of spectra',
     :                                                           IGNORE)
         CALL PAR_WRUSER('These are held in the user variables',IGNORE)
         CALL PAR_WRUSER('NPIXELS - number of selected pixels',IGNORE)
         CALL PAR_WRUSER('XPIXELS and YPIXELS - the pixel positions',
     :                                                         IGNORE)
         CALL PAR_WRUSER(
     :     'These are usually set using the space bar in ICUR',IGNORE)
         GO TO 500
      END IF
C
C     Get fitting parameters
C
      CALL PAR_RDVAL('COLUMNS',1.,200.,3.,'Pixels',VALUE)
      IF (PAR_ABORT()) GO TO 500
      NCOL=NINT(VALUE)
      CALL PAR_RDCHAR('TRACE','Gaussian',TRACE)
      IF (PAR_ABORT()) GO TO 500
      INVOKE=ICH_FOLD(TRACE)
      IF ((TRACE(1:1).NE.'E').AND.(TRACE(1:1).NE.'G')
     :         .AND.(TRACE(1:1).NE.'C').AND.(TRACE(1:1).NE.'B')) THEN
         CALL PAR_WRUSER('Cannot understand trace mode "'//
     :                     TRACE(:ICH_LEN(TRACE))//'"',IGNORE)
         CALL PAR_WRUSER(
     :       'Possible trace modes are "Edge" and "Gaussian",',IGNORE)
         CALL PAR_WRUSER('and "Balance" and "Center".',IGNORE)
         CALL PAR_WRUSER('Will use a gaussian profile follower.',IGNORE)
         TRACE='Gaussian'
      END IF
      CALL PAR_RDVAL('WIDTH',0.05,FLOAT(NY),2.,'Pixels',WIDTH)
      IF (PAR_ABORT()) GO TO 500
      CALL PAR_RDVAL('MAXDEG',1.,10.,10.,' ',VALUE)
      IF (PAR_ABORT()) GO TO 500
      MAXD=NINT(VALUE)
C
C     Should we display the fits on the image display? NO.
C
      DISPLY = .FALSE.
C     CALL PAR_RDKEY('DISPLAY',.NOT.PAR_BATCH(),DISPLY)
C     IF (PAR_ABORT()) GO TO 500
C     IF (DISPLY) THEN
C
C        Yes, so we have to open the display, and also get
C        the current display parameters.
C
C        CALL VAR_GETARY('IMARRAY',10,ARRAY,VSTATUS)
C        IF (VSTATUS.NE.0) THEN
C           CALL PAR_WRUSER(
C    :         'Unable to obtain image display parameters',IGNORE)
C           CALL PAR_WRUSER(
C    :         'Probable cause - no image displayed',IGNORE)
C           DISPLY=.FALSE.
C        ELSE
C           CALL TVOPEN(1,TVSTATUS)
C           IF (TVSTATUS.NE.1) THEN
C              CALL PAR_WRUSER('Unable to open image display device',
C    :                                                        IGNORE)
C              DISPLY=.FALSE.
C           END IF
C           CALL TVSETFM(0,TVSTATUS)
C        END IF
C        IF (.NOT.DISPLY) THEN
C           CALL PAR_WRUSER(
C    :         'Will not be able to display results of fits',IGNORE)
C        END IF
C     END IF
C
C     Do we display the results on the soft plot device?
C
      SOFT=' '
      CALL PAR_RDKEY('SOFTD',.FALSE.,LSOFT)
      IF (PAR_ABORT()) GO TO 500
      IF (LSOFT) THEN
         CALL VAR_GETCHR('SOFT',0,0,SOFT,VSTATUS)
         IF (VSTATUS.NE.0) THEN
            CALL PAR_WRUSER(
     :          'Unable to get device/type for soft plots',IGNORE)
            CALL PAR_WRUSER(
     :          'Use SOFT command, eg "SOFT /VT" to correct this',
     :                                                     IGNORE)
            SOFT=' '
         END IF
      END IF
C
C     Detailed diagnostics?
C
      CALL PAR_RDKEY('DIAGNOSE',.FALSE.,DIAG)
      IF (PAR_ABORT()) GO TO 500
C
C     Get workspace for the polynomial fits (this is used as 2 double
C     precision arrays by the routine FIG_DXYFIT) and to collect the
C     X and Y values to be fitted.  Soft plots also require extra space
C     (under pointer SPTR). See FIG_GETDIS for details.
C
      CALL DSA_GET_WORK_ARRAY(5*NX+3*(MAXD+1),
     :                            'DOUBLE',WPTR,WSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX,    'INT',IWPTR,WSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX, 'DOUBLE',XWPTR,WSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(NX, 'DOUBLE',YWPTR,WSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_GET_WORK_ARRAY(2*NX,'FLOAT',SPTR,WSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Now perform the fitting
C
      CALL FIG_GETDIS(%VAL(CNF_PVAL(DPTR)),NX,NY,NXYS,XVS,YVS,NCOL,
     :                WIDTH,TRACE(1:1),MAXD,DISPLY,SOFT,ARRAY,DIAG,
     :                %VAL(CNF_PVAL(IWPTR)),%VAL(CNF_PVAL(WPTR)),
     :                %VAL(CNF_PVAL(XWPTR)),%VAL(CNF_PVAL(YWPTR)),
     :                %VAL(CNF_PVAL(SPTR)),IX1,IX2,YAVS,WIDTHS,COEFFS)
      IF (PAR_ABORT()) GO TO 500
C
C     Output results of fit
C
      OPEN (UNIT=FILE,FILE='sdist.dat',STATUS='NEW',IOSTAT=FSTATUS)
      IF (FSTATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to open output file SDIST.DAT',IGNORE)
         GO TO 500
      END IF
      FOPEN=.TRUE.
      WRITE (FILE,'(A/2A/A)',IOSTAT=FSTATUS)
     :                   '*','*  SDIST results for ',IMAGE, '*'
      IF (FSTATUS.NE.0) GO TO 450
      WRITE (FILE,'(A,I5,A,I8,A,I8)',IOSTAT=FSTATUS)
     :      'Number of spectra = ',NXYS,'     Data array',NX,' by ',NY
      IF (FSTATUS.NE.0) GO TO 450
      DO I=1,NXYS
         WRITE (FILE,'(3(A,I5))',IOSTAT=FSTATUS) 'Spectrum # ',I,
     :                      ' X coverage from ',IX1(I),' to ',IX2(I)
         IF (FSTATUS.NE.0) GO TO 450
         WRITE (FILE,'(A,F13.7,A,F9.2)',IOSTAT=FSTATUS)
     :            'Average Y value ',YAVS(I),'     Width',WIDTHS(I)
         IF (FSTATUS.NE.0) GO TO 450
         WRITE (FILE,'(3D23.16)',IOSTAT=FSTATUS) (COEFFS(J,I),J=1,11)
         IF (FSTATUS.NE.0) GO TO 450
      END DO
C
C     Reveal name of file created
C
      INQUIRE (UNIT=FILE,NAME=STRING)
      CALL PAR_WRUSER('Analysis results written to '//
     :                               STRING(:ICH_LEN(STRING)),IGNORE)
      GO TO 500
  450 CONTINUE
      CALL PAR_WRUSER('Error writing to output file SDIST.DAT',IGNORE)
C
C     Tidy up
C
  500 CONTINUE
      IF (FOPEN) THEN
         CLOSE (FILE,IOSTAT=FSTATUS)
      END IF
C     IF (DISPLY) THEN
C        CALL TVCLOSE(TVSTATUS)
C
C         Following section disabled pending investigation of a
C         possible TVPCKG bug.
C
C         IF (TVSTATUS.NE.1) THEN
C            CALL PAR_WRUSER('Error closing down image device',IGNORE)
C         END IF
C     END IF
C
C     Close everything down
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_GETDIS(DATA,NX,NY,NXYS,XVS,YVS,NCOL,WIDTH,
     :               TRACE,MAXD,DISPLY,SOFT,ARRAY,DIAG,IWORK,WORK,XWORK,
     :               YWORK,SWORK,IX1,IX2,YAVS,WIDTHS,COEFFS)
C
C     F I G _ G E T D I S
C
C     This routine traces the distortion for a number of spectra in
C     an image and fits them with polynomials.
C
C     Parameters -   (">" input, "<" output, "W" workspace)
C
C     (>) DATA    (Real array DATA(NX,NY)) The image data.
C     (>) NX      (Integer) The number of columns in DATA
C     (>) NY      (Integer) The number of rows in DATA
C     (>) NXYS    (Integer) The number of spectra to trace.
C     (>) XVS     (Real array XVS(NXYS)) The x-pixel positions near
C                 the centers of the spectra.
C     (>) YVS     (Real array YVS(NXYS)) The y-pixel positions near
C                 the centers of the spectra.
C     (>) NCOL    (Integer) The number of columns to add together
C                 when determining centroid positions.
C     (>) WIDTH   (Real) The expected half-width of the spectra in
C                 pixels.
C     (>) TRACE   (Character) Trace mode - 'E' => edge detection,
C                 'G' => gaussian following, 'C' => center of
C                 gravity between edges.
C     (>) MAXD    (Integer) Maximum degree polynomial to be used.
C     (>) DISPLY  (Logical) True if the image display is to be used
C                 to display the fitted spectra.
C     (>) SOFT    (Character) Blank if no soft displays are to be
C                 created.  If non-blank, should be the device/type
C                 string required by PGBEGIN.
C     (>) ARRAY   (Real array ARRAY(10)) Contains the display parameters
C                 for the current image.  See IMAGE for details.
C     (>) DIAG    (Logical) True if detailed diagnostics required.
C     (W) IWORK   (Integer array IWORK(NX)) Workspace.
C     (W) WORK    (Double precision array WORK(5*NX+3*(MAXD+1)))
C                  Workspace.
C     (W) XWORK   (Double precision array XWORK(NX)) Workspace.
C     (W) YWORK   (Double precision array YWORK(NX)) Workspace.
C     (W) SWORK   (Real array SWORK(2*NX)) Workspace.  May be a
C                 dummy if SOFT is blank, since it is only needed
C                 for graphics output.
C     (<) IX1     (Integer array IX1(NXYS)) The pixel numbers at
C                 which each spectrum vanished to the left.
C     (<) IX2     (Integer array IX1(NXYS)) The pixel numbers at
C                 which each spectrum vanished to the right.
C     (<) YAVS    (Real array YAVS(NXYS)) The average Y values for
C                 the traced spectra.
C     (<) WIDTHS  (Real array WIDTHS(NXYS)) The average widths for
C                 each spectrum.
C     (<) COEFFS  (Double precision array COEFFS(11,NXYS)) The
C                 polynomial coefficients for each spectrum.
C
C                                           KS / CIT 5th Feb 1984
C     Modified:
C
C     31st Jan 1985.  KS / AAO.  Test added to prevent crash when
C                     no points could be found in a spectrum.
C     2nd April 1985. KS / AAO. Modified to use NAG version of
C                     FIG_DXYFIT.  Calling sequence for this routine
C                     and FIG_DXYFIT changed.
C     26th May 1988.  KS/AAO. TRACE parameter added.
C     31st May 1988.  KS/AAO. Set initial limit values properly.
C                     DIAG parameter added.  WIDTHS parameter added.
C     18th Apr 1995.  HME / UoE, Starlink.  No longer use NAG, need more
C                     workspace for FIG_DXYFIT.
C     26th Jul 1995.  KS/AAO. Added Balance algorithm, made tolerance on
C                     closeness of centre vary with WIDTH.
C     26th Feb 1997.  MJCL / Starlink.  Changed zero-out of COEFFS to
C                     run over NXYS spectra, was previously
C                     (uninitialised) ISPECT.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL DISPLY, DIAG
      INTEGER NCOL,NX,NY,NXYS,MAXD,IX1(NXYS),IX2(NXYS),IWORK(NX)
      REAL DATA(NX,NY),ARRAY(10),WIDTH,XVS(NXYS),YVS(NXYS)
      REAL SWORK(2*NX),YAVS(NXYS),WIDTHS(NXYS)
      DOUBLE PRECISION WORK(5*NX+3*(MAXD+1)),XWORK(NX),YWORK(NX)
      DOUBLE PRECISION COEFFS(11,NXYS)
      CHARACTER*(*) TRACE,SOFT
C
C     Parameters controlling the fitting
C
      INTEGER MAXBAD, MAXCUT, MAXDEG
      PARAMETER (MAXBAD=10, MAXCUT=40, MAXDEG=10)
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER GEN_PMAX
      REAL FIG_EDGES,FIG_BALANCE
C
C     Local variables
C
      LOGICAL BAL, COG, EDGES, FIRST, GOLEFT, GORIGHT, LSOFT, REPLY
      LOGICAL SAME
      INTEGER BAD, DEGREE, I, ICOL, ICPT, IGNORE, IROW, ISPECT, IWID
      INTEGER IWPT, IXEND, IXLEFT, IXPTR, IXRIGHT, IYEND, IYPTR, IYST, J
      INTEGER NEXT, STATUS
      REAL AVERAGE, CENT, CENTWAS, CUT(MAXCUT), LEFT, LSTRENGTH
      REAL RANGE, RIGHT, TOTAL, TOTWID
      DOUBLE PRECISION RCOEFF(11)
      CHARACTER STRING*64
C
C     Soft plots wanted?
C
      LSOFT=SOFT.NE.' '
C
C     Trace mode
C
      BAL=.FALSE.
      EDGES=.FALSE.
      COG=.FALSE.
      IF (TRACE(1:1).EQ.'E') THEN
         EDGES=.TRUE.
      ELSE IF (TRACE(1:1).EQ.'C') THEN
         EDGES=.TRUE.
         COG=.TRUE.
      ELSE IF (TRACE(1:1).EQ.'B') THEN
         BAL=.TRUE.
      END IF
C
C     Clear out the coefficient array
C
      DO I=1,NXYS
         DO J=1,11
            COEFFS(J,I)=0.0
         END DO
      END DO
C
C     Get width of cut to take
C
      IF (EDGES) THEN
         IWID=MIN(MAXCUT/2,INT(WIDTH*2.+1.))
      ELSE
         IWID=MIN(MAXCUT/2,INT(WIDTH*4.+1.))
      END IF
C
C     Loop through each spectrum
C
      FIRST=.TRUE.
      DO ISPECT=1,NXYS
C
C        Get the starting positions
C
         IWPT=0
         IXPTR=XVS(ISPECT)
         IYPTR=YVS(ISPECT)
         IXLEFT=IXPTR
         IXRIGHT=IXPTR
C
C        Loop through each column of the spectrum, first to the left
C
         BAD=0
         GOLEFT=.TRUE.
         TOTWID=0.0
         DO WHILE (GOLEFT)
C
C           Fill up work array ('CUT')
C
            IXPTR=MAX(IXPTR-NCOL,1)
            IXEND=MIN(NX,IXPTR+NCOL-1)
            IYST=MAX(1,IYPTR-IWID)
            IYEND=MIN(NY,IYPTR+IWID-1)
            ICPT=0
            DO IROW=IYST,IYEND
               ICPT=ICPT+1
               TOTAL=0.
               DO ICOL=IXPTR,IXEND
                  TOTAL=TOTAL+DATA(ICOL,IROW)
               END DO
               CUT(ICPT)=TOTAL
            END DO
C
C           and find centroid
C
            CENTWAS=FLOAT(IYPTR-IYST+1)
            IF (EDGES) THEN
               CENT=FIG_EDGES(CUT,ICPT,CENTWAS,LEFT,RIGHT)
               IF (CENT.EQ.0.0) THEN
                  STATUS=1
               ELSE
                  IF (.NOT.COG) CENT=(LEFT+RIGHT)*0.5
                  RANGE=RIGHT-LEFT
                  STATUS=0
               END IF
            ELSE IF (BAL) THEN
               CENT=FIG_BALANCE(CUT,ICPT)
            ELSE
               CENT=GEN_PMAX(CUT,ICPT)
               CALL GEN_CENTROID(CUT,ICPT,WIDTH,CENT,LSTRENGTH,STATUS)
               RANGE=WIDTH*2.0
            END IF
C
C           See if we got a centroid, and if we did, do we believe it?
C
            BAD=BAD+1
            IF (STATUS.EQ.0) THEN
               IF (ABS(CENT-CENTWAS).LT.(WIDTH*0.5)) THEN
                  IWPT=IWPT+1
                  XWORK(IWPT)=FLOAT(IXPTR+IXEND)*.5
                  YWORK(IWPT)=CENT+FLOAT(IYST-1)
                  BAD=0
                  IYPTR=NINT(YWORK(IWPT))
                  IXLEFT=IXPTR
                  TOTWID=TOTWID+RANGE
               ELSE
                  IF (DIAG) THEN
                     WRITE (STRING,*,IOSTAT=IGNORE)
     :                  'At X= ',FLOAT(IXPTR+IXEND)*.5,' center = ',CENT
                     CALL PAR_WRUSER(STRING,IGNORE)
                     WRITE (STRING,*,IOSTAT=IGNORE)
     :                  '   rejected; too far from last center = ',
     :                                                           CENTWAS
                     CALL PAR_WRUSER(STRING,IGNORE)
                  END IF
               END IF
            ELSE
               IF (DIAG) THEN
                  WRITE (STRING,*,IOSTAT=IGNORE)
     :               'At X= ',FLOAT(IXPTR+IXEND)*.5,' no center found'
                  CALL PAR_WRUSER(STRING,IGNORE)
                  IF (EDGES) THEN
                     WRITE (STRING,*,IOSTAT=IGNORE)
     :                 '     left limit = ',LEFT,' right limit = ',RIGHT
                     CALL PAR_WRUSER(STRING,IGNORE)
                  END IF
               END IF
            END IF
C
C           Do we carry on ?
C
            IF (IXPTR.LE.1) GOLEFT=.FALSE.
            IF (BAD.GT.MAXBAD) GOLEFT=.FALSE.
C
         END DO
C
C        We have reached the left end of the spectrum.
C
         IX1(ISPECT)=IXLEFT
C
C        Now repeat heading to the right of the spectrum
C
         BAD=0
         IXPTR=XVS(ISPECT)
         IYPTR=YVS(ISPECT)
         GORIGHT=.TRUE.
         DO WHILE (GORIGHT)
C
C           Fill up work array ('CUT')
C
            IXEND=MIN(NX,IXPTR+NCOL-1)
            IYST=MAX(1,IYPTR-IWID)
            IYEND=MIN(NY,IYPTR+IWID-1)
            ICPT=0
            DO IROW=IYST,IYEND
               ICPT=ICPT+1
               TOTAL=0.
               DO ICOL=IXPTR,IXEND
                  TOTAL=TOTAL+DATA(ICOL,IROW)
               END DO
               CUT(ICPT)=TOTAL
            END DO
C
C           And find centroid
C
            CENTWAS=FLOAT(IYPTR-IYST+1)
            IF (EDGES) THEN
               CENT=FIG_EDGES(CUT,ICPT,CENTWAS,LEFT,RIGHT)
               IF (CENT.EQ.0.0) THEN
                  STATUS=1
               ELSE
                  IF (.NOT.COG) CENT=(LEFT+RIGHT)*0.5
                  RANGE=RIGHT-LEFT
                  STATUS=0
               END IF
            ELSE IF (BAL) THEN
               CENT=FIG_BALANCE(CUT,ICPT)
            ELSE
               CENT=GEN_PMAX(CUT,ICPT)
               CALL GEN_CENTROID(CUT,ICPT,WIDTH,CENT,LSTRENGTH,STATUS)
               RANGE=WIDTH*2.0
            END IF
C
C           See if we got a centroid, and if we did, do we believe it?
C
            BAD=BAD+1
            IF (STATUS.EQ.0) THEN
               IF (ABS(CENT-CENTWAS).LT.(WIDTH*0.5)) THEN
                  IWPT=IWPT+1
                  XWORK(IWPT)=FLOAT(IXPTR+IXEND)*.5
                  YWORK(IWPT)=CENT+FLOAT(IYST-1)
                  BAD=0
                  IYPTR=NINT(YWORK(IWPT))
                  IXRIGHT=IXEND
                  TOTWID=TOTWID+RANGE
               ELSE
                  IF (DIAG) THEN
                     WRITE (STRING,*,IOSTAT=IGNORE)
     :                  'At X= ',FLOAT(IXPTR+IXEND)*.5,' center = ',CENT
                     CALL PAR_WRUSER(STRING,IGNORE)
                     WRITE (STRING,*,IOSTAT=IGNORE)
     :                  '   rejected; too far from last center = ',
     :                                                           CENTWAS
                     CALL PAR_WRUSER(STRING,IGNORE)
                  END IF
               END IF
            ELSE
               IF (DIAG) THEN
                  WRITE (STRING,*,IOSTAT=IGNORE)
     :               'At X= ',FLOAT(IXPTR+IXEND)*.5,' no center found'
                  CALL PAR_WRUSER(STRING,IGNORE)
                  IF (EDGES) THEN
                     WRITE (STRING,*,IOSTAT=IGNORE)
     :                 '     left limit = ',LEFT,' right limit = ',RIGHT
                     CALL PAR_WRUSER(STRING,IGNORE)
                  END IF
               END IF
            END IF
C
C           Do we carry on ?
C
            IXPTR=IXEND+1
            IF (IXPTR.GT.NX) GORIGHT=.FALSE.
            IF (BAD.GT.MAXBAD) GORIGHT=.FALSE.
         END DO
C
C        We have now reached the right end of the spectrum
C
         IX2(ISPECT)=IXRIGHT
C
C        Report result, and test for the case where the spectrum
C        could not be traced at all.
C
         IF (IWPT.GT.0) THEN
            STRING='Spectrum traced from '
            CALL ICH_ENCODE(STRING,FLOAT(IXLEFT),22,0,NEXT)
            STRING(NEXT:)=' to '
            CALL ICH_ENCODE(STRING,FLOAT(IXRIGHT),NEXT+4,0,NEXT)
            CALL PAR_WRUSER(STRING(:NEXT-1),STATUS)
         ELSE
            CALL PAR_WRUSER('Unable to trace spectrum at all.',STATUS)
            CALL PAR_WRUSER(
     :         'Suggest you repeat with different parameters.',STATUS)
            YWORK(1)=YVS(ISPECT)
            XWORK(1)=XVS(ISPECT)
            IWPT=1
            IXLEFT=XVS(ISPECT)
            IXRIGHT=IXLEFT
         END IF
C
C        Reduce the Y values to zero average
C
         TOTAL=0.
         DO I=1,IWPT
            TOTAL=TOTAL+YWORK(I)
         END DO
         AVERAGE=TOTAL/FLOAT(IWPT)
         YAVS(ISPECT)=AVERAGE
         DO I=1,IWPT
            YWORK(I)=YWORK(I)-AVERAGE
         END DO
C
C        Calculate the average width
C
         WIDTHS(ISPECT)=TOTWID/FLOAT(IWPT)
C
C        If we are going to have to plot the data, convert them
C        to single precision.  (XWORK gets destroyed by the fitting
C        in any case)
C
         IF (LSOFT) THEN
            DO I=1,IWPT
               SWORK(I)=XWORK(I)
               SWORK(NX+I)=YWORK(I)
            END DO
         END IF
C
C        Now fit the spectrum.  Note the way WORK is split into
C        two separate work arrays.
C
         IF (IWPT.GT.1) THEN
            CALL FIG_DXYFIT(XWORK,YWORK,IWPT,MIN(MAXDEG,MAXD),IWORK,
     :                       WORK,WORK(IWPT+1),COEFFS(1,ISPECT),DEGREE)
C
         ELSE
            DEGREE=0
            COEFFS(1,ISPECT)=YWORK(1)
         END IF
         STRING='Spectrum fitted to polynomial of degree '
         CALL ICH_ENCODE(STRING,FLOAT(DEGREE),41,0,NEXT)
         CALL PAR_WRUSER(STRING(:NEXT-1),STATUS)
C
C        Reverse the coefficients, since they are the wrong way
C        round for GEN_EPOLY.
C
         SAME=.FALSE.
         CALL GEN_REVR8(COEFFS(1,ISPECT),DEGREE+1,1,SAME,RCOEFF)
C
C        Display the results on the image display, if required
C
C        IF (DISPLY) THEN
C           CALL FIG_FDIST(IXLEFT,IXRIGHT,RCOEFF,DEGREE,AVERAGE,
C    :                                                     ARRAY)
C        END IF
C
C        And on the graphics device, if required.  (SWORK is split
C        up into two arrays for this, one for X, one for Y.)
C
         IF (LSOFT) THEN
            IF (.NOT.FIRST) THEN
               REPLY=.FALSE.
               DO WHILE (.NOT.REPLY)
                  CALL PAR_CNPAR('NEXT')
                  CALL PAR_RDKEY('NEXT',.TRUE.,REPLY)
                  IF (PAR_ABORT()) RETURN
               END DO
            END IF
            FIRST=.FALSE.
            CALL FIG_PDIST(SWORK,SWORK(NX+1),IWPT,NX,IXLEFT,
     :                         IXRIGHT,SOFT,RCOEFF,DEGREE,STATUS)
            IF (STATUS.NE.0) LSOFT=.FALSE.
         END IF
      END DO
C
      END
C+
      SUBROUTINE FIG_PDIST (XPTS,YPTS,NPTS,NX,IXLEFT,IXRIGHT,
     :                                 SOFT,RCOEFF,DEGREE,STATUS)
C
C     F I G _ P D I S T
C
C     Displays the results of a distortion spectrum fit on the current
C     soft graphics device, first plotting the points located, then
C     the fit to them.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (!) XPTS    (Real array XPTS(NX)) Passed with the first NPTS
C                 elements giving the X values for the points located.
C                 Then used as workspace to generate the fitted points.
C     (!) YPTS    (Real array YPTS(NX)) Passed with the first NPTS
C                 elements giving the Y values for the points located.
C                 Then used as workspace to generate the fitted points.
C     (>) NPTS    (Integer) The number of points located.
C     (>) NX      (Integer) The number of elements in XPTS & YPTS.
C     (>) IXLEFT  (Integer) The leftmost extent of the traced spectrum
C     (>) IXRIGHT (Integer) The rightmost extent of the traced spectrum
C     (>) SOFT    (Character) The device/type for the plots, as required
C                 by PGBEGIN.
C     (>) RCOEFF  (Double precision array COEFFS(DEGREE+1)) The
C                 polynomial coefficients for the fitted spectrum.
C                 The constant term should be LAST.
C     (>) DEGREE  (Integer) The polynomial degree used.
C     (<) STATUS  (Integer) Status code. 0 => OK, other values are
C                 PGBEGIN error codes.
C
C     Common variables used - None
C
C                                            KS / CIT 5th Feb 1984
C     Modified:
C
C     22nd March 1988  KS / AAO.  Modified for GKS version of PGPLOT.
C                      GRSETLS calls changed to PGSLS calls.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NPTS,NX,IXLEFT,IXRIGHT,DEGREE,STATUS
      REAL XPTS(NX),YPTS(NX)
      DOUBLE PRECISION RCOEFF(DEGREE+1)
      CHARACTER*(*) SOFT
C
C     Functions
C
      INTEGER PGBEGIN
      DOUBLE PRECISION GEN_EPOLYD
C
C     Local variables
C
      INTEGER I
      REAL AMAX, AMIN, RANGE
C
C     Open soft graphics device.
C
      STATUS=PGBEGIN(0,SOFT,1,1)
      IF (STATUS.EQ.1) THEN
         STATUS=0
C
C        Get range of points
C
         CALL GEN_RANGEF(YPTS,1,NPTS,AMAX,AMIN)
         RANGE=AMAX-AMIN
         AMAX=AMAX+RANGE*.15
         AMIN=AMIN-RANGE*.15
C
C        Setup plot environment
C
         CALL PGENV(1.,FLOAT(NX),AMIN,AMAX,0,1)
C
C        Plot the initial points as 'X's
C
         CALL PGPOINT(NPTS,XPTS,YPTS,5)
C
C        Generate the fitted points over the whole range
C
         DO I=1,NX
            YPTS(I)=GEN_EPOLYD(DBLE(I),RCOEFF,DEGREE+1)
            XPTS(I)=FLOAT(I)
         END DO
C
C        Display, using a solid line for points inside the traced
C        range, and a dotted line outside.
C
         IF (IXLEFT.GT.1) THEN
            CALL PGSLS(4)
            CALL PGLINE(IXLEFT-1,XPTS,YPTS)
            CALL PGSLS(1)
         END IF
         CALL PGLINE(IXRIGHT-IXLEFT+1,XPTS(IXLEFT),YPTS(IXLEFT))
         IF (IXRIGHT.LT.NX) THEN
            CALL PGSLS(4)
            CALL PGLINE(NX-IXRIGHT,XPTS(IXRIGHT+1),YPTS(IXRIGHT+1))
            CALL PGSLS(1)
         END IF
C
C        Close down plotting routines
C
         CALL PGEND
C
      END IF
C
      END
C+
      REAL FUNCTION FIG_EDGES (ARRAY,NELM,CENT,LEFT,RIGHT)
C
C     F I G _ E D G E S
C
C     Attempts to find the center and right and left edges of a
C     square-ish line profile.  First a three pixel median filter
C     is run through the data, then the program starts to examine
C     the absolute values of the first derivative of the data.
C     This should have a peak at the left edge and a peak at the
C     right edge.  The program looks for the first peaks in the
C     derivative either side of the center in the range within
C     the first minima in the actual data either side of the
C     center.  The positions of the edges are more precisely
C     calculated by looking at the derivative of that absolute
C     first derivative - this should cross zero at the edge of
C     the peak.  The two points where this 2nd derivative crosses
C     zero are returned as the left and right edges of the data.
C     The function value is the position of the center of the data
C     calculated as the center of gravity of the data within
C     the left and right edges.
C
C     Call-
C
C     CENTER = FIG_EDGES(ARRAY,NELM,CENT,LEFT,RIGHT)
C
C     Parameters-
C
C     (!) ARRAY      (Real array,ref) The data for the profile.
C                    The median filtering takes place in situ, so
C                    this is returned as the filtered data.
C     (>) NELM       (Integer,ref) The number of data elements
C     (>) CENT       (Real,ref) An estimate of the profile center
C     (<) LEFT       (Real,ref) The left edge of the data
C     (<) RIGHT      (Real,ref) The right edge of the data
C
C     Returns -
C
C     (<) CENTER     (Real,function value) The calculated center
C
C                                      KS / AAO  26th March 1988
C+
      IMPLICIT NONE
C
      INTEGER NELM
      REAL ARRAY(*),CENT,LEFT,RIGHT
C
C     Local variables
C
      INTEGER I,ICENT,IMAX,IMIN
      REAL FIRST,LAST,WORK(3),MAXDIF,MINVAL,DIFF1,DIFF2,DIFF3
      REAL DDIF12,DDIF23,TOTAL,TOTXY
C
C     Center pixel
C
      ICENT=NINT(CENT)
C
C     Apply median filter
C
      FIRST=(ARRAY(1)+ARRAY(2))*0.5
      LAST=(ARRAY(NELM)+ARRAY(NELM-1))*0.5
      DO I=2,NELM-1
         WORK(1)=ARRAY(I-1)
         WORK(2)=ARRAY(I)
         WORK(3)=ARRAY(I+1)
         CALL GEN_SORTF(WORK,3)
         ARRAY(I-1)=WORK(2)
      END DO
      DO I=NELM-1,2,-1
         ARRAY(I)=ARRAY(I-1)
      END DO
      ARRAY(1)=FIRST
      ARRAY(NELM)=LAST
C
C     Look for abs diff peak to right within first minimum of data
C
      MINVAL=ARRAY(ICENT)
      IMIN=ICENT
      DO I=ICENT+1,NELM-1
         IF (ARRAY(I).LT.MINVAL) THEN
            MINVAL=ARRAY(I)
            IMIN=I
         END IF
      END DO
      IMAX=0
      MAXDIF=ABS(ARRAY(ICENT)-ARRAY(ICENT+1))
      DO I=ICENT+1,IMIN
         IF (ABS(ARRAY(I)-ARRAY(I+1)).GT.MAXDIF) THEN
            MAXDIF=ABS(ARRAY(I)-ARRAY(I+1))
            IMAX=I
         END IF
      END DO
      IF (IMAX+2.GT.NELM) THEN
         RIGHT=0.0
      ELSE
         DIFF1=ABS(ARRAY(IMAX-1)-ARRAY(IMAX))
         DIFF2=MAXDIF
         DIFF3=ABS(ARRAY(IMAX+1)-ARRAY(IMAX+2))
         DDIF12=DIFF2-DIFF1
         DDIF23=DIFF3-DIFF2
         RIGHT=FLOAT(IMAX)+DDIF12/(DDIF12-DDIF23)
      END IF
C
C     Look for abs diff peak to left within first minimum of data
C
      MINVAL=ARRAY(ICENT)
      IMIN=ICENT
      DO I=ICENT-1,1,-1
         IF (ARRAY(I).LT.MINVAL) THEN
            MINVAL=ARRAY(I)
            IMIN=I
         END IF
      END DO
      IMAX=0
      MAXDIF=ABS(ARRAY(ICENT)-ARRAY(ICENT-1))
      DO I=ICENT,IMIN+1,-1
         IF (ABS(ARRAY(I)-ARRAY(I-1)).GT.MAXDIF) THEN
            MAXDIF=ABS(ARRAY(I)-ARRAY(I-1))
            IMAX=I
         END IF
      END DO
      IF (IMAX-2.LT.1) THEN
         LEFT=0.0
      ELSE
         DIFF1=ABS(ARRAY(IMAX-2)-ARRAY(IMAX-1))
         DIFF2=MAXDIF
         DIFF3=ABS(ARRAY(IMAX)-ARRAY(IMAX+1))
         DDIF12=DIFF2-DIFF1
         DDIF23=DIFF3-DIFF2
         LEFT=FLOAT(IMAX)-DDIF12/(DDIF12-DDIF23)
      END IF
C
C     Calculate new center
C
      IF ((LEFT.EQ.0.0).OR.(RIGHT.EQ.0.0)) THEN
         FIG_EDGES=0.0
      ELSE
         TOTAL=0.0
         TOTXY=0.0
         DO I=INT(LEFT),INT(RIGHT)+1
            TOTAL=TOTAL+ARRAY(I)
            TOTXY=TOTXY+ARRAY(I)*FLOAT(I)
         END DO
         IF (TOTAL.NE.0.0) THEN
            FIG_EDGES=TOTXY/TOTAL
         ELSE
            FIG_EDGES=0.0
         END IF
      END IF
C
      END
C+
      REAL FUNCTION FIG_BALANCE (ARRAY,NELM)
C
C     F I G _ B A L A N C E
C
C     Returns the centre of gravity of the data in the array NELM.
C     (This is a real centre of gravity algorithm, but it's been called
C     'balance' because the COG actually means something much more
C     sophisticated to do with edge following in the SDIST context.)
C
C     Call-
C
C     CENTER = FIG_BALANCE(ARRAY,NELM)
C
C     Parameters-
C
C     (!) ARRAY      (Real array,ref) The data for the profile.
C     (>) NELM       (Integer,ref) The number of data elements
C
C     Returns -
C
C     (<) CENTER     (Real,function value) The calculated center
C
C                                      KS / AAO  12th Dec 1992.
C+
      IMPLICIT NONE
C
      INTEGER NELM
      REAL ARRAY(*)
C
C     Local variables
C
      INTEGER I
      REAL TOTAL,TOTXY
C
C     Center pixel
C
      TOTAL=0.0
      TOTXY=0.0
      DO I=1,NELM
         TOTAL=TOTAL+ARRAY(I)
         TOTXY=TOTXY+ARRAY(I)*FLOAT(I)
      END DO
      IF (TOTAL.NE.0.0) THEN
         FIG_BALANCE=TOTXY/TOTAL
      ELSE
         FIG_BALANCE=0.0
      END IF
C
      END
