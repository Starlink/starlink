C+
      SUBROUTINE CENTERS
C
C     C E N T E R S
C
C     Figaro function that takes a list of approximate X,Y positions
C     of objects in a two-dimensional direct image and calculates
C     centroids (that is, accurate positions) for these objects.
C
C     The approximate positions input are obtained from environment
C     variables.  These variables should be set up prior to running
C     CENTERS, usually by using Figaro functions IGCUR or ICUR.
C     Alternatively, you may enter the positions into a text file and
C     use IMPOS to read this file and copy the values into the
C     environment variables required by CENTERS.
C
C     The computed centroids are output to a new file called center.dat.
C
C     Command parameters -
C
C     IMAGE    (Character) The name of the image containing the
C              objects.  This need not be the image used to
C              generate the original list of points - it may be
C              a similar image in a different waveband, with
C              offsets in X and Y with respect to the original image.
C     APERTURE (Integer) The aperture used for the centroiding.
C              The aperture actually used is a box APERTURE*2+1
C              pixels square, which is a rough approximation to a
C              circle of radius APERTURE.
C     XOFF     (Real) The offset in X, in pixels.
C     YOFF     (Real) The offset in Y, in pixels.
C     PROFBOX  (Real) Size of profile box.
C     PROFMIN  (Real) Minimum displayed value.
C     PROFMAX  (Real) Maximum displayed value.
C
C     Command keywords -
C
C     PFILE    If specified, a formatted file giving the summed
C              radial profiles will be produced.
C     DISPROF  If specified, the profile is displayed.
C     CHGPROF  If specified, the profile display can be changed.
C
C     User variables - (">" input
C
C     (>) SOFT    (Character) The current device/type for soft plots.
C     (>) NPIXELS (Real) Number of objects for which positions are
C                 specified.
C     (>) XPIXELS (Real array) List of approximate X positions of the
C                 objects for which the centroids are to be computed
C                 (pixels).
C     (>) YPIXELS (Real array) List of approximate Y positions of the
C                 objects for which the centroids are to be computed
C                 (pixels).
C
C     Output -
C
C     center.dat contains one record for each point, giving
C                XCENT,YCENT,IX,IY,DX,DY,AP
C                in the format 2F8.2,2I5,2F8.2,I4 where
C                XCENT,YCENT give the position of the centroid
C                IX,IY are the original pixel position of the point.
C                DX,DY are the offsets in X and Y, and
C                AP is the value used for APERTURE.
C                If the centroid for a point cannot be determined, a
C                record is written giving
C                '*** No centroid ',IX,IY,DX,DY,AP
C                in the format A,2I5,2F8.2,I4.
C
C                                       KS / CIT 29th Sept 1983
C     Modified:
C
C     31st Aug 1987  DJA/ AAO. Revised DSA_ routines - some specs
C                    changed. Now uses DYN routines for dynamic-memory
C                    handling.
C     22nd Mar 1988  KS / AAO. Conversion completed.  Use of STATUS
C                    restricted to DSA routines.  Modified for use
C                    with GKS version of PGPLOT.
C     5th  Oct 1992  HME / UoE, Starlink.  United the third unit PFILE
C                    with the other two in a PARAMETER statement; it
C                    used to be uninitialised. TABs removed, INCLUDE
C                    changed. Lowercase build file name. Lowercase file
C                    names profiles.lis, cursor.dat, center.dat. Build
C                    file stuff commented out. Call FIG_SETERR rather
C                    than SETERR. PGASK is banned from ADAM, commented
C                    out.
C     18th Nov 1992  HME / UoE, Starlink.  Instead of cursor.dat, get
C                    input from the user variables set by IGCUR (or
C                    ICUR).
C     25th Jan 1993  HME / UoE, Starlink.  Put PGASK back in.
C     26th Jul 1993  HME / UoE, Starlink.  Disuse GKD_*, PAR_Q*. Added
C                    parameters DISPROF, CHGPROF, PROFBOX, PROFMIN,
C                    PROFMAX. Use PAR_ABORT.
C     18th JUl 1996  MJCL / Starlink, UCL.  Set variables for storage of
C                    file names to 132 chars.
C     9th  Jun 1999  TDCA / Starlink, RAL. Removed unsupported keywords
C                    from OPEN statements.
C     9th  Jun 2000  ACD / UoE, Starlink.  Changed the manipulation of
C                    parameter DISPROF so that it is cancelled after,
C                    rather than before, each value is obtained.
C     29th Oct 2001  ACD / UoE, Starlink.  Changed the prologue comments
C                    to correspond to the modifications made by HME on
C                    8th Nov 1992.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Maximum aperture size
C
      INTEGER NRMAX
      PARAMETER (NRMAX=40)
C
C     Fortran End-of-file code
C
      INTEGER EOF
      PARAMETER (EOF=-1)
C
C     I/O units
C
      INTEGER INPUT,OUTPUT,PUNIT
      PARAMETER (INPUT=1,OUTPUT=2,PUNIT=3)
C
C     Dimension of X/YPIXELS.
C
      INTEGER MAXPTS
      PARAMETER (MAXPTS=50)
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_LEN
C
C     Local variables
C
      LOGICAL      BUOPEN        ! True if build file opened
      LOGICAL      CENTOK        !
      LOGICAL      CEOPEN        ! True if center.dat opened
      LOGICAL      CUOPEN        ! True if CURSOR.DAT opened
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      INTEGER      DPTR          ! Dynamic-memory pointer to data array
      INTEGER      DSLOT         ! Map slot number of input data array
      LOGICAL      FAULT         ! TRUE if an error occurred
      CHARACTER    FILE*32       ! Name of the build file if required
      INTEGER      I             !
      INTEGER      IGNORE        ! Used to pass ignorable status
      CHARACTER    IMAGE*132     ! The actual name of the image file
      INTEGER      IPIX          !
      INTEGER      IREC          !
      INTEGER      ISTAT         ! Status variable for non-DSA routines
      INTEGER      IX            !
      INTEGER      IY            !
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NEXT          !
      INTEGER      NPIX          !
      REAL         NPIXELS       !
      INTEGER      NRAD          !
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      NY            ! Size of 2nd dimension (if present)
      LOGICAL      PFILE         ! Value of PFILE keyword
      LOGICAL      POPEN         !
      LOGICAL      PROFIL        !
      LOGICAL      REPLY         ! Reply from would-be PAR_Q*
      INTEGER      RADIUS        !
      INTEGER      SIZE          !
      CHARACTER    SOFT*32       ! The name of the soft device
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER    STRING*64     ! Output message text
      REAL       SUMX(NRMAX*2+1) !
      REAL       SUMY(NRMAX*2+1) !
      INTEGER      SZMAX         !
      REAL         VALUE         ! Temporary real number
      INTEGER      X             !
      REAL         XCENT         !
      REAL         XOFF          !
      INTEGER      Y             !
      REAL         YCENT         !
      REAL         YOFF          !
      REAL         XPIXELS(MAXPTS)
      REAL         YPIXELS(MAXPTS)
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Initial values
C
      FAULT=.FALSE.
      CUOPEN=.FALSE.
      CEOPEN=.FALSE.
      PROFIL=.TRUE.
      BUOPEN=.FALSE.
      POPEN=.FALSE.
C
C     Get value of SOFT
C
      CALL VAR_GETCHR('SOFT',0,0,SOFT,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL PAR_WRUSER('No device for soft plots defined',IGNORE)
         CALL PAR_WRUSER(
     :          'Will have to omit display of radial profile',IGNORE)
         PROFIL=.FALSE.
      END IF
C
C     Name of build file (if required)
C
      FILE='bplt.dat'
C
C     Get value of IMAGE and open file. Also get actual name of IMAGE.
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      CALL DSA_GET_ACTUAL_NAME('IMAGE',IMAGE,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map data
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF (NDIM.LT.2) THEN
         CALL PAR_WRUSER('Data is not an image',IGNORE)
         GO TO 500
      END IF
      NX=DIMS(1)
      NY=DIMS(2)
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get offsets and aperture size
C
      CALL PAR_RDVAL('APERTURE',1.,FLOAT(NRMAX),5.,'Pixels',VALUE)
      NRAD=NINT(VALUE)
      CALL PAR_RDVAL('XOFF',-FLOAT(NX),FLOAT(NX),0.,'Pixels',XOFF)
      CALL PAR_RDVAL('YOFF',-FLOAT(NY),FLOAT(NY),0.,'Pixels',YOFF)
      IF (PAR_ABORT()) GO TO 500
C
C     See if a formatted output file giving the summed radial profiles
C     is to be created.
C
      CALL PAR_RDKEY('PFILE',.FALSE.,PFILE)
      IF (PAR_ABORT()) GO TO 500
      IF (PFILE) THEN
         OPEN (UNIT=PUNIT,FILE='profiles.lis',STATUS='NEW',
     :                          IOSTAT=ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL PAR_WRUSER('Unable to create new file for profiles',
     :                                                        IGNORE)
            CALL PAR_WRUSER('Will have to omit profile listing',IGNORE)
            PFILE=.FALSE.
         ELSE
            POPEN=.TRUE.
         END IF
         WRITE (PUNIT,'(//,T30,A,/A/)',IOSTAT=ISTAT) IMAGE,
     :            ' X Cent  Y Cent  Profile values'
      END IF
C
C     Open input and output files
C
c     OPEN (UNIT=INPUT,FILE='cursor.dat',STATUS='OLD',IOSTAT=ISTAT)
c     IF (ISTAT.NE.0) THEN
c        CALL PAR_WRUSER('Unable to open file CURSOR.DAT',IGNORE)
c        GO TO 500
c     END IF
c     CUOPEN=.TRUE.
      OPEN (UNIT=OUTPUT,FILE='center.dat',STATUS='NEW',IOSTAT=ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL PAR_WRUSER('Unable to create file center.dat',ignore)
         GO TO 500
      END IF
      CEOPEN=.TRUE.
C
C     Output headers
C
      CALL PAR_WRUSER(' ',IGNORE)
      WRITE (STRING,'(A,I2,A)',IOSTAT=ISTAT) 'Using an aperture of '
     :                                                ,NRAD,' pixels'
      CALL PAR_WRUSER(STRING,IGNORE)
      CALL PAR_WRUSER(' ',IGNORE)
      CALL PAR_WRUSER('    X    Y  X-cent  Y-cent',IGNORE)
      CALL PAR_WRUSER(' ---- ---- ------- -------',IGNORE)
C
C     Initial value for size covered by radial profile display
C
      SZMAX=NRMAX+NRMAX+1
      SIZE=MIN(NRAD*8,SZMAX)
C
C     Get X and Y positions.
C
      CALL VAR_GETNUM('NPIXELS',0,0,NPIXELS,STATUS)
      NPIX=INT(NPIXELS+SIGN(0.5,NPIXELS))
      IF (STATUS.EQ.0) THEN
         NPIX=MIN(MAXPTS,NPIX)
         IF (NPIX.LE.0) THEN
            CALL PAR_WRUSER(
     :         'Zero or negative number of pixels selected',IGNORE)
            STATUS=-1
         ELSE
            CALL VAR_GETARY('XPIXELS',NPIX,XPIXELS,STATUS)
            IF (STATUS.EQ.0) THEN
               CALL VAR_GETARY('YPIXELS',NPIX,YPIXELS,STATUS)
            END IF
         END IF
      END IF
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to get x-y position(s)',IGNORE)
         CALL PAR_WRUSER('These are held in the user variables',IGNORE)
         CALL PAR_WRUSER('NPIXELS - number of selected pixels',IGNORE)
         CALL PAR_WRUSER('XPIXELS and YPIXELS - the pixel positions',
     :                                                         IGNORE)
         GO TO 500
      END IF
C
C     Now loop for each position read.
C
      IREC=0
      DO IPIX=1,NPIX
         IX=INT(XPIXELS(IPIX)+SIGN(0.5,XPIXELS(IPIX)))
         IY=INT(YPIXELS(IPIX)+SIGN(0.5,YPIXELS(IPIX)))
C
C        Read the record OK, calculate centroids and output result
C        to terminal.
C
         X=IX+XOFF
         Y=IY+YOFF
         CALL FIG_CPCENT(%VAL(CNF_PVAL(DPTR)),NX,NY,X,Y,NRAD,SUMX,
     :                   SUMY,XCENT,YCENT,ISTAT)
         IF (ISTAT.NE.0) THEN
            WRITE (STRING,'(2I5,A)',IOSTAT=IGNORE) X,Y,
     :                               ' Cannot calculate centroid: '
            NEXT=39
            IF (ISTAT.EQ.-1) THEN
               STRING(NEXT:)='Sums not positive'
            ELSE IF (ISTAT.EQ.-2) THEN
               STRING(NEXT:)='Failed to converge'
            ELSE IF (ISTAT.EQ.-3) THEN
               STRING(NEXT:)='Too close to edge of image'
            ELSE
               STRING(NEXT:)='Unexpected error code'
            END IF
            CENTOK=.FALSE.
         ELSE
            WRITE (STRING,'(2I5,2F8.2)',IOSTAT=IGNORE)
     :                                 X,Y,XCENT,YCENT
            CENTOK=.TRUE.
         END IF
         CALL PAR_WRUSER(STRING,IGNORE)
         IREC=IREC+1
C
C        Display radial profile if required.  Initially, profile
C        will be displayed by default as long as the soft device
C        was defined.  Subsequently, the default will be not to
C        display, but the question will be repeated until the user
C        chooses not to display.
C
         IF (PROFIL) THEN
            CALL PAR_RDKEY('DISPROF',IREC.EQ.1,REPLY)
            CALL PAR_CNPAR('DISPROF')
            IF (PAR_ABORT()) GO TO 500
            IF (REPLY) THEN
               CALL FIG_RPROF(SOFT,FILE,%VAL(CNF_PVAL(DPTR)),NX,NY,
     :                        XCENT,YCENT,SZMAX,BUOPEN,SIZE,SUMX,
     :                        SUMY,ISTAT)
               IF (PAR_ABORT()) GO TO 500
               PROFIL=ISTAT.EQ.0
            ELSE
               PROFIL=.FALSE.
            END IF
         END IF
C
C        If required, (ie if PFILE specified) calculate profile, in SUMX
C        using SUMY as workspace, and write to output file.
C
         IF (PFILE) THEN
            RADIUS=INT(1.4142*SIZE*.5+.5)
            CALL FIG_CPCALC(%VAL(CNF_PVAL(DPTR)),NX,NY,XCENT,YCENT,
     :                      RADIUS,SUMY,SUMX)
            WRITE (PUNIT,'(/2F8.2,10G10.3)',IOSTAT=ISTAT)
     :                     XCENT,YCENT,(SUMX(I),I=1,MIN(10,RADIUS))
            IF (RADIUS.GT.10) THEN
               WRITE (PUNIT,'(16X,10G10.3)',IOSTAT=ISTAT)
     :                                        (SUMX(I),I=11,RADIUS)
            END IF
         END IF
C
C        Now output the result to the center file
C
         IF (CENTOK) THEN
            WRITE (OUTPUT,'(2F8.2,2I5,2F8.2,I4)',IOSTAT=ISTAT)
     :                            XCENT,YCENT,X,Y,XOFF,YOFF,NRAD
         ELSE
            WRITE (OUTPUT,'(A,2I5,2F8.2,I4)',IOSTAT=ISTAT)
     :                     '*** No centroid ',X,Y,XOFF,YOFF,NRAD
         END IF
      END DO
C
C     Indicate result file(s)
C
      CALL PAR_WRUSER(' ',IGNORE)
      INQUIRE (UNIT=OUTPUT,NAME=STRING)
      CALL PAR_WRUSER('Results written to file '//
     :                            STRING(:ICH_LEN(STRING)),IGNORE)
C
      IF (POPEN) THEN
         CALL PAR_WRUSER(' ',IGNORE)
         INQUIRE (UNIT=PUNIT,NAME=STRING)
         CALL PAR_WRUSER('Profile data written to file '//
     :                            STRING(:ICH_LEN(STRING)),IGNORE)
      END IF
C
C     IF (BUOPEN) THEN
C        CALL PAR_WRUSER(' ',IGNORE)
C        CALL PAR_WRUSER('Hard copy output to '//FILE(:ICH_LEN(FILE)),
C    :                                                      IGNORE)
C        CALL PAR_WRUSER('Use BPLOT command to obtain plots',IGNORE)
C     END IF
C
C     Ended OK, go to tidy up
C
  500 CONTINUE
      IF (CEOPEN) CLOSE(UNIT=OUTPUT,IOSTAT=IGNORE)
c     IF (CUOPEN) CLOSE(UNIT=INPUT,IOSTAT=IGNORE)
      IF (POPEN)  CLOSE(UNIT=PUNIT,IOSTAT=IGNORE)
C     IF (BUOPEN) CALL DSK_CLOSE(IGNORE)
C
C     Closedown DSA routines
C
      CALL DSA_CLOSE(STATUS)
C
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE FIG_RPROF (SOFT,FILE,IMAGE,NX,NY,XCENT,YCENT,SZMAX,
     :                              BUOPEN,SIZE,SUMX,SUMY,STATUS)
C
C     F I G _ R P R O F
C
C     Controls the radial profile display for CENTERS.  Outputs one
C     soft plot using default parameters, then offers the user the
C     option of a hardcopy (which has to be a 'build' plot), or a
C     replot with different parameters.
C
C     Parameters  (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) SOFT    (Character) The name/type of the device for soft
C                 plots.
C     (>) FILE    (Character) The name to be used for the build file,
C                 if required.
C     (>) IMAGE   (Real array IMAGE(NX,NY)) The image data
C     (>) NX      (Integer) The number of x-pixels in IMAGE
C     (>) NY      (Integer) The number of y-pixels in IMAGE
C     (>) XCENT   (Real) The x-coordinate of the central pixel for the
C                 profile plot.
C     (>) YCENT   (Real) The y-coordinate of the central pixel for the
C                 profile plot.
C     (>) SZMAX   (Integer) The maximum value for SIZE.
C     (!) BUOPEN  (Logical) True if a 'build' file for hardcopy output
C                 has been opened.
C     (!) SIZE    (Integer) The profile data is gathered from a box of
C                 side SIZE centered at (XCENT,YCENT).  This may be
C                 changed by the user.
C     (W) SUMX    (Real array SUMX(SZMAX)) Workspace
C     (W) SUMY    (Real array SUMY(SZMAX)) Workspace
C     (<) STATUS  (Integer) Status code. 0 => OK  Non-zero=> some error
C                 opening soft plot device.
C
C                                               KS / CIT 29th Sept 1983
C     Modified:
C
C     22nd Mar 1988  KS / AAO  Modified for use with GKS version of
C                    PGPLOT.
C     5th  Oct 1992  HME / UoE, Starlink.  Build file stuff commented
C                    out.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL BUOPEN
      INTEGER NX,NY,SZMAX,SIZE,STATUS
      REAL IMAGE(NX,NY),XCENT,YCENT,SUMX(SZMAX),SUMY(SZMAX)
      CHARACTER*(*) SOFT,FILE
C
C     Functions used
C
      LOGICAL PAR_ABORT
      INTEGER PGBEGIN
C
C     Floating point limits (allows a fair bit of headroom)
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.E30,FMIN=-1.E30)
C
C     Local variables
C
      LOGICAL BUILD,MORE
      REAL AMAX,AMIN,VALUE
C
C     Open soft plot device
C
      STATUS=PGBEGIN(0,SOFT,1,1)
      IF (STATUS.NE.1) THEN
         STATUS=1
         GO TO 600
      END IF
      CALL PGASK(.FALSE.)
C
C     Set for autoscale of data and soft plot
C
      AMIN=0.
      AMAX=0.
      BUILD=.FALSE.
C
C     This loop continues until the user is happy with the display
C
      MORE=.TRUE.
      DO WHILE (MORE)
C
C        Display profile
C
         CALL FIG_CPRAD (IMAGE,NX,NY,XCENT,YCENT,SIZE,BUILD,
     :                                      AMIN,AMAX,SUMX,SUMY)
C
C        Produce hardcopy?  (Only if last plot was soft)
C
C        IF (BUILD) THEN
C           CALL DSK_CHECK(STATUS)
C           IF (STATUS.NE.0) THEN
C              CALL DSK_ERROR(STATUS,ERROR)
C              CALL GKD_WRUSER('Error producing plot',STATUS)
C              CALL GKD_WRUSER(ERROR,STATUS)
C           END IF
C           BUILD=.FALSE.
C        ELSE
C           BUILD=GKD_QUEST('Produce hardcopy of plot?',.FALSE.)
C        END IF
C        IF (BUILD) THEN
C           IF (.NOT.BUOPEN) THEN
C              APPEND=.FALSE.
C              WRITE=.TRUE.
C              ERRLOG=.FALSE.
C              CALL DSK_OPEN(FILE,'NEW',APPEND,WRITE,ERRLOG,
C    :                                                      STATUS)
C              IF (STATUS.NE.0) THEN
C                 CALL DSK_ERROR(STATUS,ERROR)
C                 CALL GKD_WRUSER('Unable to open output file',STATUS)
C                 CALL GKD_WRUSER(ERROR,STATUS)
C                 BUILD=.FALSE.
C              ELSE
C                 BUOPEN=.TRUE.
C              END IF
C           END IF
C        END IF
C
C        If no hardcopy wanted, (or if it failed) then ask about
C        a change of parameters.
C
         IF (.NOT.BUILD) THEN
            CALL PAR_CNPAR('CHGPROF')
            CALL PAR_RDKEY('CHGPROF',.FALSE.,MORE)
            IF (PAR_ABORT()) GO TO 600
            IF (MORE) THEN
               CALL PAR_CNPAR('PROFBOX')
               CALL PAR_RDVAL('PROFBOX',1.,FLOAT(SZMAX),FLOAT(SIZE),
     :            'Pixels',VALUE)
               IF (PAR_ABORT()) GO TO 600
               SIZE=VALUE
               CALL PAR_CNPAR('PROFMIN')
               CALL PAR_RDVAL('PROFMIN',FMIN,FMAX,AMIN,' ',AMIN)
               IF (PAR_ABORT()) GO TO 600
               CALL PAR_CNPAR('PROFMAX')
               CALL PAR_RDVAL('PROFMAX',FMIN,FMAX,AMAX,' ',AMAX)
               IF (PAR_ABORT()) GO TO 600
            END IF
         END IF
      END DO
C
C     Close down soft plots
C
      CALL PGEND
      STATUS=0
C
  600 CONTINUE
      END
C+
      SUBROUTINE FIG_CPRAD (IMAGE,NX,NY,X,Y,SIZE,BUILD,AMIN,AMAX,
     :                                                        R,Z)
C
C     F I G _ C P R A D
C
C     CPOS utility.  Plots the radial profile of a subset of an
C     image about a given center.  This is an extended version of
C     a routine written by JRM.
C
C     Parameters -   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) IMAGE    (Real array IMAGE(NX,NY)) The image data.
C     (>) NX       (Integer) The number of x-pixels in IMAGE.
C     (>) NY       (Integer) The number of y-pixels in IMAGE.
C     (>) X        (Real) The coordinates of the central pixel
C     (>) Y        (Real) for the profile plot.
C     (>) SIZE     (Integer) The profile data is gathered from a
C                  box of side SIZE pixels centered at (X,Y).
C     (>) BUILD    (Logical) If true, a plot 'build file' is being
C                  generated.
C     (!) AMIN     (Real) The minimum scale value for the display.
C     (!) AMAX     (Real) The maximum scale value for the display.
C                  If AMAX is passed equal to AMIN, the routine will
C                  autoscale the display and return the calculated
C                  limits in AMIN and AMAX.
C     (W) R        (Real array R(SIZE)) Workspace.
C     (W) Z        (Real array Z(SIZE)) Workspace.
C
C     Subroutines / functions used -
C
C     PGLABEL     (PGPLOT package) Label a graph
C     PGENV       ( "      "     ) Establish plotting environment
C     PGPOINT     ( "      "     ) Plot a sequence of points
C     ICH_ENCODE  (ICH_ package) Encode a real number into a char string
C     DSK_PGLABEL (DSK_    "   ) Build file version of PGLABEL
C     DSK_PGENV   ( "      "   )   "    "      "    "  PGENV
C     DSK_PGPOINT ( "      "   )   "    "      "    "  PGPOINT
C     DSK_PGSLW   ( "      "   )   "    "      "    "  PGSLW (set
C                                line width for plots)
C
C     Note: This routine assumes that the graphics device is
C     already open - ie PGBEGIN has already been called.  It does
C     not CALL PGEND to close the device; this is also up to the
C     caller.  Similarly, if BUILD is true, it leaves the calling
C     of DSK_OPEN and DSK_CLOSE to the user.
C                                                 KS/ CIT 29th Sept 1983
C     Modified:
C
C     22nd Mar 1988  KS / AAO  Modified for use with GKS version of
C                    PGPLOT.
C     5th  Oct 1992  HME / UoE, Starlink.  Build file stuff commented
C                    out.
C+
      IMPLICIT NONE
C
C     Functions -
C
      INTEGER ICH_ENCODE
C
C     Parameters -
C
      LOGICAL BUILD
      INTEGER NX,NY,SIZE
      REAL AMAX,AMIN,IMAGE(NX,NY),R(SIZE),X,Y,Z(SIZE)
C
C     Local variables -
C
      INTEGER INVOKE,IP,IX,IXEN,IXST,IY,IYEN,IYST,NEXT
      REAL VALUE,XDIST,YDIST
      CHARACTER STRING*64
C
C     Find limits of data, and range of data
C
      IXST=MAX(1,INT(X)-SIZE/2)
      IXEN=MIN(NX,IXST+SIZE-1)
      IYST=MAX(1,INT(Y)-SIZE/2)
      IYEN=MIN(NY,IYST+SIZE-1)
      IF (ABS(AMAX-AMIN).LT.1.0E-20) THEN
         AMIN=IMAGE(IXST,IYST)
         AMAX=AMIN
         DO IY=IYST,IYEN
            DO IX=IXST,IXEN
               VALUE=IMAGE(IX,IY)
               IF (AMIN.GT.VALUE) AMIN=VALUE
               IF (AMAX.LT.VALUE) AMAX=VALUE
            END DO
         END DO
         VALUE=(AMAX-AMIN)*.1
         AMAX=AMAX+VALUE
         AMIN=AMIN-VALUE
      END IF
C
C     Start the plot
C
C     IF (BUILD) THEN
C        CALL DSK_PGSLW(1)
C        CALL DSK_PGENV(0.,1.4142*FLOAT(SIZE/2),AMIN,AMAX,0,0)
C     ELSE
         CALL PGENV(0.,1.4142*FLOAT(SIZE/2),AMIN,AMAX,0,0)
C     END IF
C
C     Generate label
C
      STRING='Radial profile at ('
      INVOKE=ICH_ENCODE(STRING,X,20,2,NEXT)
      STRING(NEXT:NEXT)=','
      INVOKE=ICH_ENCODE(STRING,Y,NEXT+1,2,NEXT)
      STRING(NEXT:NEXT)=')'
C     IF (BUILD) THEN
C        CALL DSK_PGLABEL(' ',' ',STRING(:NEXT))
C        CALL DSK_PGSLW(3)
C     ELSE
         CALL PGLABEL(' ',' ',STRING(:NEXT))
C     END IF
C
C     Build up plot a row at a time
C
      DO IY=IYST,IYEN
         YDIST=(FLOAT(IY)-Y)*(FLOAT(IY)-Y)
         IP=1
         DO IX=IXST,IXEN
            XDIST=(FLOAT(IX)-X)*(FLOAT(IX)-X)
            R(IP)=SQRT(YDIST+XDIST)
            Z(IP)=IMAGE(IX,IY)
            IP=IP+1
         END DO
C        IF (BUILD) THEN
C           CALL DSK_PGPOINT(IXEN-IXST+1,R,Z,-1)
C        ELSE
            CALL PGPOINT(IXEN-IXST+1,R,Z,-1)
C        END IF
      END DO
C
      END
C+
      SUBROUTINE FIG_CPCALC (IMAGE,NX,NY,XCENT,YCENT,RADIUS,PIXELS,
     :                                                      PROFILE)
C
C     F I G _ C P C A L C
C
C     CENTERS utility routine.  Calculates (a litle roughly) a radial
C     profile given a central point in the image.
C
C     Parameters -   (">" input, "W" work "<" output)
C
C     (>) IMAGE     (Real array IMAGE(NX,NY)) The image data
C     (>) NX        (Integer) The first dimension of IMAGE
C     (>) NY        (Integer) The second dimension of IMAGE
C     (>) XCENT     (Real) The central point for the profile in X
C     (>) YCENT     (Real) The central point for the profile in Y
C     (>) RADIUS    (Integer) The maximum number of pixels to be
C                   included in the profile calculation
C     (W) PIXELS    (Real array PIXELS(RADIUS)) Workspace.
C     (<) PROFILE   (Real array PROFILE(RADIUS)) Workspace.
C
C                                                  KS / CIT 29th Sept 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,RADIUS
      REAL IMAGE(NX,NY),XCENT,YCENT,PROFILE(RADIUS),PIXELS(RADIUS)
C
C     Local variables
C
      INTEGER I,IDIST,IX,IXCEN,IXEN,IXST,IY,IYCEN,IYEN,IYST
      REAL XDIST,YDIST
C
C     Calculate the profile over a box of side RADIUS
C
      IXCEN=XCENT
      IYCEN=YCENT
      IXST=MAX(IXCEN-RADIUS+1,1)
      IXEN=MIN(IXCEN+RADIUS,NX)
      IYST=MAX(IYCEN-RADIUS+1,1)
      IYEN=MIN(IYCEN+RADIUS,NY)
C
C     Zero out the profile array
C
      DO I=1,RADIUS
         PROFILE(I)=0.
         PIXELS(I)=0.
      END DO
C
C     Loop through the box and fill the profile array
C
      DO IY=IYST,IYEN
         YDIST=(FLOAT(IY)-YCENT)*(FLOAT(IY)-YCENT)
         DO IX=IXST,IXEN
            XDIST=(FLOAT(IX)-XCENT)*(FLOAT(IX)-XCENT)
            IDIST=INT(SQRT(YDIST+XDIST))+1
            IF (IDIST.LE.RADIUS) THEN
               PROFILE(IDIST)=PROFILE(IDIST)+IMAGE(IX,IY)
               PIXELS(IDIST)=PIXELS(IDIST)+1.
            END IF
         END DO
      END DO
C
C     Scale down by number of pixels
C
      DO I=1,RADIUS
         IF (PIXELS(I).GT.0.) PROFILE(I)=PROFILE(I)/PIXELS(I)
      END DO
C
      END
