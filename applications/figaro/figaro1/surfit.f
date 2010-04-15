C+
C                            S U R F I T
C
C  Function:
C     Figaro application that fits a surface to an image.
C
C  Description:
C     SURFIT takes an image and fits bi-cubic splines to a series
C     of knots calculated by averaging any valid data in the image
C     over a range of pixels centered on each knot.  (Optionally, it
C     can use the median value.) It then creates a new image from the
C     calculated fit.
C
C  Command parameters:
C     IMAGE    The image to be fitted.
C     XKNOTS   The number of interior knots along the X-axis of the
C              image.
C     YKNOTS   The number of interior knots along the Y-axis of the
C              image.
C     VMODE    'Average' or 'Median' (only first letter counts), to
C              indicate the way the values used are calculated.
C     MOSAIC   Indicates that a mosaic rather than a fit is to be
C              produced.
C     OUTPUT   The resulting image.
C
C  Treatment of errors:  Ignored.
C
C  Treatment of data quality:  By use of quality arrays.
C
C  Support:  Keith Shortridge, AAO.
C
C  Version date: 20th February 1995
C-
C  History:
C     28 Jul 1988   Original version. KS/AAO.
C     20 Feb 1995   NAG deprecated the routine E02DBF. Modified to use
C                   E02DEF instead. KS/AAO.
C     16 Feb 1996   HME / UoE, Starlink. Convert to FDA:
C                   Bad-pixel handling. Had to swap mapping input data
C                   before mapping input quality.
C     2005 June 10  MJC / Starlink  Use CNF_PVAL for pointers to
C                   mapped data.
C+
      SUBROUTINE SURFIT
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_FOLD
C
C     Local variables
C
      INTEGER   DIMS(2)          ! Data dimensions
      LOGICAL   FAULT            ! Indicates non-DSA detected error
      INTEGER   IGNORE           ! Dummy status argument
      INTEGER   INVOKE           ! Dummy function value
      INTEGER   IPTR             ! Input data dynamic array element
      LOGICAL   MOSAIC           ! Value of MOSAIC parameter
      INTEGER   NDIM             ! Number of data dimensions
      INTEGER   NELM             ! Number of data elements
      INTEGER   NX               ! First data dimension
      INTEGER   NY               ! Second data dimension
      LOGICAL   OK               ! Result of FIG_SURFIT call
      INTEGER   OPTR             ! Output data dynamic array element
      INTEGER   QPTR             ! Quality data dynamic array element
      LOGICAL   QUAL             ! True if quality data present
      INTEGER   SLOT             ! Map slot for mapped arrays
      INTEGER   STATUS           ! Inherited status for DSA routines
      REAL      VALUE            ! Temporary real variable
      CHARACTER VMODE*16         ! Value of VMODE parameter
      INTEGER   XKNOTS           ! Value of XKNOTS parameter
      INTEGER   YKNOTS           ! Value of YKNOTS parameter
C
C     Initialise system
C
      FAULT = .FALSE.
      STATUS = 0
      CALL DSA_OPEN (STATUS)
C
C     Open input image and get dimensions
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      NX = DIMS(1)
      IF (NDIM.GT.1) THEN
         NY = DIMS(2)
      ELSE
         NY = 1
      END IF
C
C     See if the input image has data quality information
C
      CALL DSA_SEEK_FLAGGED_VALUES ('IMAGE',QUAL,STATUS)
      IF (.NOT.QUAL) CALL DSA_SEEK_QUALITY ('IMAGE',QUAL,STATUS)
C
C     If it does, use and map the quality array
C     Map the input image
C
      IF (QUAL) THEN
         CALL DSA_USE_QUALITY ('IMAGE',STATUS)
         CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
         CALL DSA_MAP_QUALITY ('IMAGE','READ','BYTE',QPTR,SLOT,STATUS)
      ELSE
         CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      END IF
      IF (STATUS.NE.0) GO TO 500    ! Error exit
C
C     Get the number of knots to use in each dimension
C
      CALL PAR_RDVAL ('XKNOTS',2.,200.,10.,'Knots',VALUE)
      XKNOTS = VALUE
      CALL PAR_RDVAL ('YKNOTS',2.,200.,10.,'Knots',VALUE)
      YKNOTS = VALUE
C
C     Get and check the value mode parameter
C
      CALL PAR_RDCHAR('VMODE','Median',VMODE)
      INVOKE = ICH_FOLD (VMODE)
      IF ((VMODE(1:1).NE.'M').AND.(VMODE(1:1).NE.'A')) THEN
         CALL PAR_WRUSER(
     :      'Value mode should be either "Median" or "Average"',IGNORE)
         CALL PAR_WRUSER('Will assume "median"',IGNORE)
         VMODE = 'Median'
      END IF
C
C     See if all we want is the mosaic
C
      CALL PAR_RDKEY('MOSAIC',.FALSE.,MOSAIC)
C
C     Open the output image
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
C
C     Map the output image
C
      CALL DSA_MAP_DATA ('OUTPUT','WRITE','FLOAT',OPTR,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500    ! Error exit
C
C     Perform the fitting
C
      CALL FIG_SURFIT (%VAL(CNF_PVAL(IPTR)),NX,NY,QUAL,
     :                 %VAL(CNF_PVAL(QPTR)),XKNOTS,YKNOTS,VMODE,
     :                 MOSAIC,%VAL(CNF_PVAL(OPTR)),OK)
      IF (.NOT.OK) THEN
         FAULT = .TRUE.
         GO TO 500       ! Error exit
      END IF
C
C     Close down
C
  500 CONTINUE
      CALL DSA_CLOSE (STATUS)
      IF (FAULT) CALL FIG_SETERR
C
      END

C+
C                           F I G _ S U R F I T
C
C  Routine name:
C     FIG_SURFIT
C
C  Function:
C     Fits a set of bi-cubic splines to an image.
C
C  Description:
C     This routine divides an image up into a number of panels.  Within
C     each panel it calculates either the average value or the median of
C     the data and treats the data as just having that value at the
C     centre of the panel. It then performs a set of bi-cubic-spline
C     fits between these generated points, and then evaluates the fit
C     for each point of the original image.  The result is the output
C     image.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_SURFIT (INPUT,NX,NY,QUAL,QDATA,XPOINTS,
C                                  YPOINTS,VMODE,MOSAIC,OUTPUT,OK)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C     (>) INPUT    (Real array,ref) The input image. INPUT(NX,NY)
C     (>) NX       (Integer,ref) First dimension of INPUT and OUTPUT
C     (>) NY       (Integer,ref) Second dimension of INPUT and OUTPUT
C     (>) QUAL     (Logical,ref) True if there is a quality array
C                  associated with the data.
C     (>) QDATA    (Byte array,ref) The quality array associated with
C                  the data.  Dimensions QDATA(NX,NY).
C     (>) XPOINTS  (Integer,ref) The number of data points in
C                  X to be generated and fitted.
C     (>) YPOINTS  (Integer,ref) The number of data points in
C                  Y to be generated and fitted.
C     (>) VMODE    (Fixed string,descr) Indicates if the values used
C                  are to be average values, in which case it should
C                  begin with 'A', or median values, in which case it
C                  should begin with 'M'.
C     (>) MOSAIC   (Logical,ref) True if a mosaic rather than a fit is
C                  to be generated.  This is mainly a debugging tool for
C                  the application - it shows the data that gets fitted.
C     (<) OUTPUT   (Real array, ref) The resulting image.
C                  OUTPUT(NX,NY).  INPUT and OUTPUT may be the same
C                  array.
C     (<) OK       (Logical,ref) Indicates if the fitting was OK, or
C                  if an error occurred.
C
C  External variables used:   None.
C
C  External subroutines / functions used:
C     FIG_SURFIT_NAG, DSA_GET_WORK_ARRAY
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th July 1988
C-
C  History:
C     29th July 1988.  Original version.  KS / AAO.
C     June 89.         JOS/AAO.  Fixed bug in calculation of workspace.
C     6th Sept 1989.   KS/AAO.   Formalised previous fix, by changing
C                      so COVER is known here and passed down.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      SUBROUTINE FIG_SURFIT (INPUT,NX,NY,QUAL,QDATA,XPOINTS,YPOINTS,
     :                                         VMODE,MOSAIC,OUTPUT,OK)
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

      LOGICAL QUAL,OK,MOSAIC
      INTEGER NX,NY,XPOINTS,YPOINTS
      BYTE QDATA(NX,NY)
      REAL INPUT(NX,NY),OUTPUT(NX,NY)
      CHARACTER*(*) VMODE
C
C     Local variables
C
      INTEGER ADRES             ! Dynamic-memory pointer for ADRES array
      INTEGER C                 ! Dynamic-memory pointer for C array
      REAL    COVER             ! Panel overlap factor
      INTEGER D                 ! Dynamic-memory pointer for D array
      INTEGER DL                ! Dynamic-memory pointer for DL array
      INTEGER F                 ! Dynamic-memory pointer for F array
      INTEGER FW                ! Dynamic-memory pointer for FW array
      INTEGER IWRK              ! Dynamic-memory pointer for IWRK array
      INTEGER LAMDA             ! Dynamic-memory pointer for LAMDA array
      INTEGER M                 ! Number of points for fit
      INTEGER MU                ! Dynamic-memory pointer for MU array
      INTEGER NADRES            ! Number of elements in ADRES array
      INTEGER NC                ! Number of elements in C array
      INTEGER ND                ! Number of elements in D array
      INTEGER NIWRK             ! Number of points in IWRK array
      INTEGER NPOINT            ! Number of elements in POINT array
      INTEGER NWRK              ! Number of elements in WRK array
      INTEGER NWS               ! Number of elements in NWS array
      INTEGER P                 ! As specified for WS array by E02DAF
      INTEGER POINT             ! Dynamic memory address of POINT array
      INTEGER PX                ! Number of knots in X for spline fit
      INTEGER PY                ! Number of knots in Y for spline fit
      INTEGER SLOT              ! Slot entry for workspace array
      INTEGER STATUS            ! Status for DSA calls
      INTEGER W                 ! Dynamic-memory pointer for W array
      INTEGER WRK               ! Dynamic-memory pointer for WRK array
      INTEGER WS                ! Dynamic-memory pointer for WS array
      INTEGER X                 ! Dynamic-memory pointer for X array
      INTEGER XW                ! Dynamic-memory pointer for XW array
      INTEGER Y                 ! Dynamic-memory pointer for Y array
      INTEGER YW                ! Dynamic-memory pointer for YW array
C
C     This routine is nothing more than an interface between the main
C     routine and the routine FIG_SURFIT_NAG that actually calls the
C     NAG routines that do the real work.  The NAG routines (and
C     FIG_SURFIT_NAG itself) need a huge number of workspace arrays,
C     and this routine exists just to calculate their sizes and
C     allocate the dynamic memory needed for the workspace arrays.
C
C     COVER controls the overlap of panels when the central values
C     are calculated.  A value of 1.0 means there is no overlap.
C     It might be nice to make COVER a program parameter.
C
      COVER = 1.5
C
C     All these variables have the meaning used in the NAG documentation
C     Note that PX/PY are the total number of knots in the axis
C     directions, and so are 8 more than the number of internal knots.
C
      PX = XPOINTS + 8
      PY = YPOINTS + 8
      M = (XPOINTS + 2) * (YPOINTS + 2)
      NADRES = (PX-7) * (PY-7)
      NPOINT = MAX((M + NADRES),(NX + NADRES))
      P = 3 * (PY-4) + 4
      NC = (PX-4) * (PY-4)
      NWS = 2 * NC * (P + 2) + P
      ND = ((((NX - 1)/(XPOINTS - 1) + 2) * COVER) + 1) *
     :         ((((NY - 1)/(YPOINTS - 1) + 2) * COVER) + 1)
      NWRK = PY - 4
      NIWRK = PY - 4
C
      OK = .FALSE.
      STATUS = 0
      CALL DSA_GET_WORK_ARRAY(M,'DOUBLE',X,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(M,'DOUBLE',Y,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(M,'DOUBLE',F,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(M,'DOUBLE',W,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(PX,'DOUBLE',LAMDA,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(PY,'DOUBLE',MU,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NC,'DOUBLE',DL,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NC,'DOUBLE',C,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NWS,'DOUBLE',WS,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',XW,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',YW,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',FW,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NWRK,'DOUBLE',WRK,SLOT,STATUS)

      CALL DSA_GET_WORK_ARRAY(NPOINT,'INT',POINT,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NADRES,'INT',ADRES,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NIWRK,'INT',IWRK,SLOT,STATUS)

      CALL DSA_GET_WORK_ARRAY(ND,'FLOAT',D,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500      ! Error exit
C
      CALL FIG_SURFIT_NAG (INPUT,NX,NY,QUAL,QDATA,XPOINTS,YPOINTS,COVER,
     :                     VMODE,MOSAIC,M,PY,PX,NC,NWS,NPOINT,NADRES,
     :                     NWRK,NIWRK,ND,%VAL(CNF_PVAL(X)),
     :                     %VAL(CNF_PVAL(Y)),%VAL(CNF_PVAL(F)),
     :                     %VAL(CNF_PVAL(W)),%VAL(CNF_PVAL(LAMDA)),
     :                     %VAL(CNF_PVAL(MU)),%VAL(CNF_PVAL(POINT)),
     :                     %VAL(CNF_PVAL(ADRES),%VAL(CNF_PVAL(DL)),
     :                     %VAL(CNF_PVAL(C)),%VAL(CNF_PVAL(WS)),
     :                     %VAL(CNF_PVAL(XW)),%VAL(CNF_PVAL(YW)),
     :                     %VAL(CNF_PVAL(FW),%VAL(CNF_PVAL(WRK)),
     :                     %VAL(CNF_PVAL(IWRK)),%VAL(CNF_PVAL(D)),
     :                     OUTPUT,OK)
C
  500 CONTINUE
C
      END

C+
C                       F I G _ S U R F I T _ N A G
C
C  Routine name:
C     FIG_SURFIT_NAG
C
C  Function:
C     Performs the actual surface fitting for FIG_SURFIT.
C
C  Description:
C     This is the routine that actually does the work for the SURFIT
C     application.   It uses NAG, and like all NAG routines needs a
C     huge number of work arrays.  These are obtained dynamically by
C     FIG_SURFIT and then passed to this routine.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_SURFIT_NAG (INPUT,NX,NY,QUAL,QDATA,XPOINTS,YPOINTS,COVER,
C          VMODE,MOSAIC,M,PY,PX,NC,NWS,NPOINT,NADRES,NWRK,NIWRK,ND,X,Y,F,
C           W,LAMDA,MU,POINT,ADRES,DL,C,WS,XW,YW,FW,WRK,IWRK,D,OUTPUT,OK)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C     Most parameters are the same as those for FIG_SURFIT.  The additional
C     ones are the work arrays and their dimensions.  One day I'll flesh
C     out this documentation. Maybe.
C
C  External variables used: None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 7th Sept 1988
C-
C  History:
C     7th Sept 1988.  Original version.  KS / AAO.
C     6th Sept 1989.  COVER now a parameter.  KS/AAO.
C+
      SUBROUTINE FIG_SURFIT_NAG (INPUT,NX,NY,QUAL,QDATA,XPOINTS,YPOINTS,
     :                  COVER,VMODE,MOSAIC,M,PY,PX,NC,NWS,NPOINT,NADRES,
     :                    NWRK,NIWRK,ND,X,Y,F,W,LAMDA,MU,POINT,ADRES,DL,
     :                               C,WS,XW,YW,FW,WRK,IWRK,D,OUTPUT,OK)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL QUAL,OK,MOSAIC
      INTEGER M, PX, PY, NC, NWS,NPOINT, NADRES, NX, NY
      INTEGER NWRK, NIWRK, ND
      INTEGER XPOINTS, YPOINTS
      DOUBLE PRECISION X(M),Y(M),F(M),W(M),LAMDA(PX)
      DOUBLE PRECISION MU(PY),DL(NC),C(NC),WS(NWS)
      DOUBLE PRECISION XW(NX),YW(NX),FW(NX),WRK(NWRK)
      INTEGER POINT(NPOINT),ADRES(NADRES),IWRK(NIWRK)
      BYTE QDATA(NX,NY)
      REAL COVER, INPUT(NX,NY), D(ND), OUTPUT(NX,NY)
      CHARACTER*(*) VMODE
C
C     Functions
C
      REAL GEN_QFMED
C
C     Local variables
C
      DOUBLE PRECISION FMAX
      DOUBLE PRECISION FMIN
      DOUBLE PRECISION WMAX
      DOUBLE PRECISION WMIN
      DOUBLE PRECISION XMAX
      DOUBLE PRECISION XMIN
      DOUBLE PRECISION YMAX
      DOUBLE PRECISION YMIN
      DOUBLE PRECISION EPS
      DOUBLE PRECISION SIGMA
      INTEGER   RANK
      REAL      XRANGE
      REAL      YRANGE
      REAL      YVAL
      REAL      YVMIN
      REAL      XVMIN
      REAL      XVMAX
      REAL      YVMAX
      REAL      VALUE
      REAL      XVAL
      INTEGER   POINTS
      INTEGER   IFAIL
      INTEGER   IXMIN
      INTEGER   IYMIN
      INTEGER   IXMAX
      INTEGER   IYMAX
      INTEGER   XIKNOTS
      INTEGER   YIKNOTS
      INTEGER   IP
      INTEGER   IPOINT
      INTEGER   IPT
      INTEGER   IPX1
      INTEGER   IY
      INTEGER   IX
      INTEGER   IYP
      INTEGER   IXP
C
C     Go through the data building up the points to be fitted
C
      OK = .FALSE.
      XRANGE = FLOAT(NX-1)/FLOAT(XPOINTS-1)
      YRANGE = FLOAT(NY-1)/FLOAT(YPOINTS-1)
      IPOINT = 0
      YVAL = 1.0
      DO IY = 1,YPOINTS
         YVMIN = YVAL - YRANGE*COVER*0.5
         YVMAX = YVAL + YRANGE*COVER*0.5
         IYMIN = MAX(INT(YVMIN),1)
         IYMAX = MIN(INT(YVMAX),NY)
         IPX1 = IPOINT + 1
         XVAL = 1.0
         DO IX = 1,XPOINTS
            XVMIN = XVAL - XRANGE*COVER*0.5
            XVMAX = XVAL + XRANGE*COVER*0.5
            IXMIN = MAX(INT(XVMIN),1)
            IXMAX = MIN(INT(XVMAX),NX)
            VALUE = 0.0
            POINTS = 0
            IF (QUAL) THEN
               DO IYP = IYMIN,IYMAX
                  DO IXP = IXMIN,IXMAX
                     IF (QDATA(IXP,IYP).EQ.0) THEN
                        VALUE = VALUE + INPUT(IXP,IYP)
                        POINTS = POINTS + 1
                        D(POINTS) = INPUT(IXP,IYP)
                     END IF
                  END DO
               END DO
            ELSE
               DO IYP = IYMIN,IYMAX
                  DO IXP = IXMIN,IXMAX
                     VALUE = VALUE + INPUT(IXP,IYP)
                     POINTS = POINTS + 1
                     D(POINTS) = INPUT(IXP,IYP)
                  END DO
               END DO
            END IF
            IPOINT = IPOINT + 1
            X(IPOINT) = XVAL
            Y(IPOINT) = YVAL
            IF (POINTS.GT.0) THEN
               IF (VMODE(1:1).EQ.'A') THEN
                  F(IPOINT) = VALUE/FLOAT(POINTS)
               ELSE
                  F(IPOINT) = GEN_QFMED(D,POINTS)
               END IF
               W(IPOINT) = SQRT(FLOAT(POINTS))
            ELSE
               F(IPOINT) = 0.0
               W(IPOINT) = 0.0
            END IF
            IF (MOSAIC) THEN
               DO IYP = IYMIN,IYMAX
                  DO IXP = IXMIN,IXMAX
                     OUTPUT(IXP,IYP) = F(IPOINT)
                  END DO
               END DO
            END IF
            XVAL = XVAL + XRANGE
            IF (IX.EQ.XPOINTS-1) XVAL = DBLE(NX)
         END DO
         IPOINT = IPOINT + 1
         F(IPOINT) = F(IPOINT-1)
         X(IPOINT) = X(IPOINT-1) + XRANGE
         Y(IPOINT) = YVAL
         W(IPOINT) = W(IPOINT-1)
         IPOINT = IPOINT + 1
         F(IPOINT) = F(IPX1)
         X(IPOINT) = X(IPX1) - XRANGE
         Y(IPOINT) = YVAL
         W(IPOINT) = W(IPX1)
         IF (IY.EQ.1) THEN
            IPT = IPOINT
            DO IP = IPX1,IPT
               IPOINT = IPOINT + 1
               F(IPOINT) = F(IP)
               X(IPOINT) = X(IP)
               W(IPOINT) = W(IP)
               Y(IPOINT) = Y(IP) - YRANGE
            END DO
         ELSE IF (IY.EQ.YPOINTS) THEN
            IPT = IPOINT
            DO IP = IPX1,IPT
               IPOINT = IPOINT + 1
               F(IPOINT) = F(IP)
               X(IPOINT) = X(IP)
               W(IPOINT) = W(IP)
               Y(IPOINT) = Y(IP) + YRANGE
            END DO
         END IF
         YVAL = YVAL + YRANGE
         IF (IY.EQ.YPOINTS-1) YVAL = DBLE(NY)
      END DO
C
C     If all we wanted was the mosaic, quit now
C
      IF (MOSAIC) THEN
         OK = .TRUE.
         GO TO 500
      END IF
C
C     Rescale the axis values to a 0..1 range, the data points and
C     weights ditto.
C
      XMAX = X(1)
      XMIN = XMAX
      YMAX = Y(1)
      YMIN = YMAX
      WMAX = W(1)
      WMIN = WMAX
      FMAX = F(1)
      FMIN = FMAX
      DO IP = 1,M
         IF (XMAX.LT.X(IP)) XMAX = X(IP)
         IF (XMIN.GT.X(IP)) XMIN = X(IP)
         IF (YMAX.LT.Y(IP)) YMAX = Y(IP)
         IF (YMIN.GT.Y(IP)) YMIN = Y(IP)
         IF (WMAX.LT.W(IP)) WMAX = W(IP)
         IF (WMIN.GT.W(IP)) WMIN = W(IP)
         IF (FMAX.LT.F(IP)) FMAX = F(IP)
         IF (FMIN.GT.F(IP)) FMIN = F(IP)
      END DO
      DO IP = 1,M
         X(IP)=(X(IP) - XMIN)/(XMAX - XMIN)
         Y(IP)=(Y(IP) - YMIN)/(YMAX - YMIN)
         W(IP)=(W(IP) - WMIN)/(WMAX - WMIN)
         F(IP)=(F(IP) - FMIN)/(FMAX - FMIN)
      END DO
C
C     Fill up the interior knot arrays
C
      XIKNOTS = XPOINTS
      YIKNOTS = YPOINTS
      XVAL = 1.0
      DO IX=1,XIKNOTS
         LAMDA(IX+4) = (XVAL-XMIN)/(XMAX-XMIN)
         XVAL = XVAL + XRANGE
      END DO
      YVAL = 1.0
      DO IY=1,YIKNOTS
         MU(IY+4) = (YVAL-YMIN)/(YMAX-YMIN)
         YVAL = YVAL + YRANGE
      END DO
C
C     Sort points into panel order
C
      IFAIL = 1
      CALL E02ZAF (PX,PY,LAMDA,MU,M,X,Y,POINT,NPOINT,ADRES,
     :                                                NADRES,IFAIL)
      IF (IFAIL.NE.0) THEN
         CALL FIG_NAGERR(IFAIL,'E02ZAF')
         GO TO 500    ! Error exit
      END IF
C
C     Fit bicubic spline to data points
C
      EPS = 1.0D-6
      IFAIL = 1
      CALL E02DAF (M,PX,PY,X,Y,F,W,LAMDA,MU,POINT,NPOINT,DL,C,NC,
     :                               WS,NWS,EPS,SIGMA,RANK,IFAIL)
      IF (IFAIL.NE.0) THEN
         CALL FIG_NAGERR(IFAIL,'E02DAF')
         GO TO 500    ! Error exit
      END IF
C
C     Fill up the output array
C
      DO IY = 1,NY
C
C        We evaluate the spline a line at a time, filling up the
C        X and Y position arrays for each pixel of the image.  We
C        have to be careful with the extreme values, since rounding
C        error may have set the knot extrema to fractionally less than
C        the actual data range.  So we use the known values, just to
C        be safe.
C
         DO IX = 1,NX
            XW(IX)=(DBLE(IX)-XMIN)/(XMAX - XMIN)
            YW(IX)=(DBLE(IY)-YMIN)/(YMAX - YMIN)
         END DO
         IFAIL = 1
         CALL E02ZAF (PX,PY,LAMDA,MU,NX,XW,YW,POINT,
     :                                   NPOINT,ADRES,NADRES,IFAIL)
         IF (IFAIL.NE.0) THEN
            CALL FIG_NAGERR(IFAIL,'E02ZAF')
            GO TO 500    ! Error exit
         END IF
         IFAIL = 1
         CALL E02DEF (NX,PX,PY,XW,YW,
     :                            LAMDA,MU,C,FW,WRK,IWRK,IFAIL)
         IF (IFAIL.NE.0) THEN
            CALL FIG_NAGERR(IFAIL,'E02DBF')
            GO TO 500    ! Error exit
         END IF
         DO IX=1,NX
            OUTPUT(IX,IY) = FW(IX)*(FMAX-FMIN) + FMIN
         END DO
      END DO
      OK = .TRUE.
C
  500 CONTINUE
      END
