C+
      SUBROUTINE CFIT
C
C     C F I T
C
C     Figaro function to generate a spectrum by interpolation
C     between points selected interactively using a display
C     device cursor.  CFIT assumes that a spectrum has already
C     been displayed by SPLOT, and will generate a new data
C     structure based on the spectrum displayed, with only the
C     data changed.
C
C     Command parameters -
C
C     OUTPUT      (Character) The name of the output file to
C                 be created.  If this is the same as the displayed
C                 spectrum, the data in the displayed spectrum will
C                 be modified.
C
C     Command keywords -
C
C     CHANGE      Set true to mofify points.
C     REDRAW      Set true to redraw the original spectrum.
C
C     User variables used -  (">" input, "<" output)
C
C     (>) TVFILE  The name of the displayed spectrum
C     (>) TVXST   The first displayed x-value
C     (>) TVXEN   The last displayed x-value
C     (>) TVHIGH  The maximum displayed y-value
C     (>) TVLOW   The minimum displayed y-value
C     (>) TVCOLOR The GRPLOT code for the plot colour
C     (>) SOFT    The device/type string defining the display device
C
C                                              KS / CIT 17th May 1983
C     Modified:
C
C      5th Aug 1987  Rewritten to use the DSA_ and DYN_ routines.  DJA/AAO.
C     13th Aug 1987  Test for identical X values added before fit.  KS / AAO.
C     21st Mar 1988  Modified for GKS version of PGPLOT.  KS/AAO.
C     28th Aug 1992  Change INCLUDE. Remove TABs.  HME/UoE, Starlink.
C     26th Jul 1993  Disuse GKD_*, PAR_Q*. Added parameters CHANGE and
C                    REDRAW. Use PAR_ABORT.  HME / UoE, Starlink.
C     23rd Jan 1995  HME / UoE, Starlink. Increase TVFILE to *132.
C      5th Apr 1995  HME / UoE, Starlink. No longer use NAG.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER DSA_TYPESIZE
C
C     Local variables
C
      INTEGER      BYTES        ! Number of bytes of workspace required
      INTEGER      CKEY         ! GRPCKG code for plot colour
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      INTEGER      DPTR         ! Dynamic-memory pointer to data array
      INTEGER      DSLOT        ! Map slot number of input data array
      REAL         HIGH         ! Highest pixel value
      INTEGER      IGNORE       ! Used to pass an ignorable error
      REAL         LOW          ! Lowest pixel value
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      NX           ! Size of 1st dimension
      INTEGER      OPTR         ! Dynamic-memory pointer to output data array
      INTEGER      OSLOT        ! Map slot number outputdata array
      CHARACTER    PGDEV*32     ! The name of the plotting device
      CHARACTER    SPECT*132    ! The actual filename of the input data
      INTEGER      STATUS       ! Running status for DSA_ routines
      REAL         VALUE        ! Temporary real number
      INTEGER      WPTR         ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT        ! Map slot number of workspace
      REAL         XEN          ! Value of rightmost pixel on display
      INTEGER      XPTR         ! Dynamic-memory pointer to x-axis data
      INTEGER      XSLOT        ! Map slot number of x-axis data
      REAL         XST          ! Value of leftmost pixel on display
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get value of 'SOFT'
C
      CALL VAR_GETCHR('SOFT',0,0,PGDEV,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to get soft device name.',IGNORE)
         CALL PAR_WRUSER('Probably no plot has been made.',IGNORE)
         GO TO 500
      END IF
C
C     Get the user variables describing the plot.
C
      CALL VAR_GETNUM('TVXST',0,0,XST,STATUS)
      IF (STATUS.EQ.0) THEN
         CALL VAR_GETNUM('TVXEN',0,0,XEN,STATUS)
         IF (STATUS.EQ.0) THEN
            CALL VAR_GETNUM('TVLOW',0,0,LOW,STATUS)
            IF (STATUS.EQ.0) THEN
               CALL VAR_GETNUM('TVHIGH',0,0,HIGH,STATUS)
               IF (STATUS.EQ.0) THEN
                  CALL VAR_GETCHR('TVFILE',0,0,SPECT,STATUS)
                  IF (STATUS.EQ.0) THEN
                     CALL VAR_GETNUM('TVCOLOR',0,0,VALUE,STATUS)
                     CKEY=NINT(VALUE)
                  END IF
               END IF
            END IF
         END IF
      END IF
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to obtain plot variables',IGNORE)
         CALL PAR_WRUSER('Probably no plot has been made.',IGNORE)
         GO TO 500
      END IF
C
C     Open spectrum file
C
      CALL DSA_NAMED_INPUT('SPECT',SPECT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get size of data
C
      CALL DSA_DATA_SIZE('SPECT',10,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
C
C     Get name for output file
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map data arrays
C
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',DPTR,DSLOT,STATUS)
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',OPTR,OSLOT,STATUS)
      IF (DPTR.EQ.OPTR) THEN
         BYTES=DSA_TYPESIZE('FLOAT',STATUS)*NX
         CALL DSA_GET_WORKSPACE (BYTES,WPTR,WSLOT,STATUS)
         CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(DPTR)),%VAL(CNF_PVAL(WPTR)))
         DPTR=WPTR
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     Map the X-array (if there is one).  If there isn't, create
C     one, using the element numbers.
C
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',XPTR,XSLOT,
     :                       STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Now interactively generate the fitted spectrum.
C
      CALL FIG_CSFIT(PGDEV,XST,XEN,HIGH,LOW,CKEY,NX,
     :               %VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(DPTR)),
     :               %VAL(CNF_PVAL(OPTR)),STATUS)
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_CSFIT(PGDEV,XST,XEN,HIGH,LOW,CKEY,NX,XDATA,
     :                                        SDATA,DATA,STATUS)
C
C     F I G _ C S F I T
C
C     Allows the user to interactively select a set of points on the
C     display device and generates an interpolated spectrum from
C     these selected points.  Assumes a spectrum has already been
C     displayed.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) PGDEV     (Character) The device/type for the plots, as
C                   required by PGPLOT.
C     (>) XST       (Real) The initial displayed X value
C     (>) XEN       (Real) The final displayed X value
C     (>) HIGH      (Real) The maximum displayed Y value
C     (>) LOW       (Real) The minimum displayed Y value
C     (>) CKEY      (Integer) The GRPCKG code for the plot colour
C     (>) NX        (Integer) The number of points in the spectrum
C     (>) XDATA     (Real array XDATA(NX)) The X-values for each of
C                   the points in the spectrum.
C     (>) SDATA     (Real array SDATA(NX)) The original spectrum data
C                   - used if the spectrum has to be redrawn.
C     (<) DATA      (Real array DATA(NX)) Receives the generated spectrum
C     (<) STATUS    (Integer) Status return.  If unable to open the
C                   plotting device, this will be non-zero.
C
C
C                                          KS / CIT 26th July 1984
C     Modified:
C
C     21st Mar 1985  KS / AAO.  Re-written to use NAG rouitnes.
C     13th Aug 1987  KS / AAO.  Test for identical X values added.
C     21st Mar 1988  KS / AAO.  Modified for GKS version of PGPLOT.
C                    Use of GKD_ routines introduced.
C      5th Apr 1995  HME / UoE, Starlink. No longer use NAG.
C     18th May 1995  HME / UoE, Starlink.  DBINTK needs MAXPTS more
C                    workspace than I thought previously, increased
C                    WRK from 6*... to 7*...
C     24th Jul 1996  MJCL / Starlink, UCL.  Changed string catenation
C                    for Linux port.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CKEY,NX,STATUS
      REAL XST,XEN,HIGH,LOW,SDATA(NX),XDATA(NX),DATA(NX)
      CHARACTER*(*) PGDEV
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_LEN, PGBEGIN
      DOUBLE PRECISION PDA_DBVALU
C
C     Maximum number of points allowed, plotting symbol code, and
C     white colour code
C
      INTEGER MAXPTS,SYMB,WHITE
      PARAMETER (MAXPTS=100,SYMB=5,WHITE=1)
C
C     Local variables
C
      LOGICAL NAGERR,REPEAT,REPLY
      INTEGER CMPTAB(0:7),I,IFAIL,IFAIL2,IOFF,IR(MAXPTS)
      INTEGER IPT,MNPT,NPT,NSTAT,INVB,NDEG
      REAL X(MAXPTS),XMAX,XMIN,Y(MAXPTS),W(MAXPTS)
      DOUBLE PRECISION C(MAXPTS+4),DX(MAXPTS)
      DOUBLE PRECISION DY(MAXPTS),K(MAXPTS+4),WRK(7*MAXPTS+8),EPS
      DOUBLE PRECISION WTS(5)
      CHARACTER PGDEVL*128
C
C     Complementary colour codes for the various PGPLOT colours
C
      DATA CMPTAB/1,1,5,6,7,2,3,4/
C
C     Initiate PGPLOT - note use of '/APPEND' to suppress
C     screen erase.
C
      PGDEVL=PGDEV(:ICH_LEN(PGDEV))//'/APPEND'
      STATUS=PGBEGIN(1,PGDEVL,1,1)
      IF (STATUS.NE.1) THEN
         CALL PAR_WRUSER('Unable to open graphics device',NSTAT)
         GO TO 600
      END IF
C
C     Define window and viewport
C
      CALL PGWINDOW(XST,XEN,LOW,HIGH)
      CALL PGVSTAND
C
C     Set number of selected points to zero, initially
C
      NPT=0
C
C     This loop continues until a satisfactory
C     result is obtained (or until the user gives up)
C
      REPEAT=.TRUE.
      DO WHILE (REPEAT)
C
C        Select points
C
         CALL PAR_WRUSER(
     :      '"A" to add points, "D" to delete, "X" to finish',NSTAT)
         CALL PAR_WRUSER(' ',NSTAT)
         CALL PGSCI(WHITE)
         CALL PGNCURSE(MAXPTS-2,NPT,X,Y,SYMB)
C
C        Check that enough points have been selected
C
         IF (NPT.LT.4) THEN
            CALL PAR_WRUSER(
     :         'Not enough points selected - must be >= 4',NSTAT)
         ELSE
C
C           Sort points into ascending order in X and convert to
C           double precision for NAG routines.  (May have to leave
C           one slot spare at the start - see comments to next section)
C           Note that the order has to be strictly increasing, so we
C           also have to test for identical X values.  If we find any,
C           we replace the two with their mean (if this will take us
C           below the minimum 4 points, we have to leave in two the same).
C
            CALL GEN_QFISORT(X,NPT,IR)
            CALL GEN_FVSORT(IR,NPT,1,W,X)
            CALL GEN_FVSORT(IR,NPT,1,W,Y)
            XMIN=MIN(XDATA(1),XDATA(NX))
            XMAX=MAX(XDATA(1),XDATA(NX))
            IPT=2
            DO WHILE (IPT.LE.NPT)
               IF (X(IPT).EQ.X(IPT-1)) THEN
                  Y(IPT-1)=(Y(IPT)+Y(IPT-1))*0.5
                  IF (NPT.GT.4) THEN
                     DO I=IPT,NPT-1
                        X(I)=X(I+1)
                        Y(I)=Y(I+1)
                     END DO
                     NPT=NPT-1
                  ELSE
                     Y(IPT)=Y(IPT-1)
                     IPT=IPT+1
                  END IF
               ELSE
                  IPT=IPT+1
               END IF
            END DO
            IF (X(1).GT.XMIN) THEN
               IOFF=1
            ELSE
               IOFF=0
            END IF
            DO I=1,NPT
               DX(I+IOFF)=X(I)
               DY(I+IOFF)=Y(I)
            END DO
            MNPT=NPT+IOFF
C
C           This step is required because the NAG spline evaluator will
C           reject any point outside the X-range originally supplied.  So
C           we need to generate two extra points at the extreme ends of
C           the spectrum.  We do this by fitting a cubic to the end points
C           that we have, including one at zero weight at the actual end
C           point.  We then evaluate the fitted polynomial at that point,
C           doing this once for each end of the spectrum.
C
            NAGERR=.TRUE.
            WTS(1)=1.0D-6
            WTS(2)=1.
            WTS(3)=1.
            WTS(4)=1.
            WTS(5)=1.
            IF (X(1).GT.XMIN) THEN
               DX(1)=XMIN
               DY(1)=0.
               EPS=0D0
               IFAIL2=0
               CALL PDA_DPOLFT(5,DX,DY,WTS,3,NDEG,EPS,K,
     :            IFAIL,WRK,IFAIL2)
               IF (IFAIL.NE.1.OR.IFAIL2.NE.0) GO TO 400
               DY(1)=K(1)
            END IF
            IF (X(MNPT).LT.XMAX) THEN
               WTS(1)=1.
               WTS(5)=1.0D-6
               MNPT=MNPT+1
               DX(MNPT)=XMAX
               DY(MNPT)=0.
               EPS=0D0
               IFAIL2=0
               CALL PDA_DPOLFT(5,DX(MNPT-4),DY(MNPT-4),WTS,3,
     :            NDEG,EPS,K,IFAIL,WRK,IFAIL2)
               IF (IFAIL.NE.1.OR.IFAIL2.NE.0) GO TO 400
               DY(MNPT)=K(5)
            END IF
C
C           Spline fit to points
C
            DO I=1,4
               K(I)=DX(1)
               K(MNPT+I)=DX(MNPT)
            END DO
            DO I=5,MNPT
               K(I)=DX(I-2)
            END DO
            IFAIL2=0
            CALL PDA_DBINTK(DX,DY,K,MNPT,4,C,WRK(9),WRK,IFAIL2)
            IF (IFAIL2.NE.0) GO TO 400
C
C           Generate interpolated spectrum
C
            INVB=1
            IFAIL2=0
            DO I=1,NX
               DATA(I)=SNGL(PDA_DBVALU(K,C,MNPT,4,0,DBLE(XDATA(I)),
     :            INVB,WRK,IFAIL2))
               IF (IFAIL2.NE.0) GO TO 400
            END DO
            NAGERR=.FALSE.
C
C           Plot resulting spectrum, using complementary colour
C
            CALL PGSCI(CMPTAB(CKEY))
            CALL PGLINE(NX,XDATA,DATA)
         END IF
C
C        Any NAG errors cause a breakout to here
C
  400    CONTINUE
C
         CALL PAR_CNPAR('CHANGE')
         CALL PAR_RDKEY('CHANGE',.TRUE.,REPEAT)
         IF (PAR_ABORT()) GO TO 600
         IF (REPEAT) THEN
            CALL PAR_CNPAR('REDRAW')
            CALL PAR_RDKEY('REDRAW',.FALSE.,REPLY)
            IF (PAR_ABORT()) GO TO 600
            IF (REPLY) THEN
               CALL PGADVANCE
               CALL PGSCI(WHITE)
               CALL PGBOX('ABCNST',0.,0.,'ABCNST',0.,0.)
               CALL PGSCI(CKEY)
               CALL PGLINE(NX,XDATA,SDATA)
            END IF
         END IF
C
      END DO
C
C     Close down plotting device
C
      CALL PGSCI(WHITE)
      CALL PGEND

C
C     Indicate OK
C
      STATUS=0
C
C     Exit
C
  600 CONTINUE
C
      END
