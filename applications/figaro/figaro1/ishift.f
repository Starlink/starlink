C+
      SUBROUTINE ISHIFT
C
C     I S H I F T
C
C     Shifts an image in both X and Y, by, in each direction,
C     a constant amount expressed in pixels.
C
C     The VARIANCE array, if present, is propagated in exactly the same
C     way as the DATA array.  This procedure it not formally correct if
C     re-sampling occurs (that is, if either of the shifts is non-integer)
C     and in this case the resulting variance will probably under-estimate
C     the true error.
C
C     Command parameters -
C
C     IMAGE  The name of the structure containing the image.
C
C     XSHIFT (Numeric) The number of pixels the image is to be
C            shifted in X.  A -ve number shifts towards lower
C            numbered pixels.
C
C     YSHIFT (Numeric) The number of pixels the image is to be
C            shifted in Y.  Sense is as for XSHIFT.
C
C     XSPLIT (Numeric) The number of sub-divisions to be made in
C            each of the original pixels in X.
C   
C            Note that if both YSHIFT and XSHIFT are integers, the
C            routine will just perform a fast data move, rather
C            than hit the problem with the big hammer of a general
C            rebinning.  YSPLIT and XSPLIT will be ignored.
C
C     YSPLIT (Numeric) The number of sub-divisions to be made in
C            each of the original pixels in Y.
C
C            Note that if XSPLIT or YSPLIT are greater than 1,
C            the data will be interpolated using a fit to a 2D 
C            parabola.  This increases the accuracy (sometimes)
C            of the rebinning, but results in increased CPU usage.
C
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the DATA and, if present,
C            the VARIANCE a direct copy of the input.
C
C     Command keywords -  None
C
C                                      KS / CIT 11th Sept 1983
C
C     Modified:
C
C     28th Jul 1987   DJA/AAO. Revised DSA_ routines - some specs changed.
C                     Now uses DYN_ dynamic memory handling routines.
C                     All WRUSERs changed to PAR_WRUSERs.
C     27th Oct 1987   KS / AAO.  Corrected code to handle case of 1D
C                     input data properly.
C     29th Sep 1992.  HME / UoE, Starlink.  INCLUDE changed, TABs
C                     removed.
C     26th Jul 1996.  MJCL / Starlink, UCL.  Added PAR_ABORT checking.
C     27th Sep 2001.  VGG / Starlink, RAL and ACD / Starlink, UoE.
C                     Error propagation added.
C+
      IMPLICIT NONE
C
C     Functions
C
      INTEGER DYN_ELEMENT,DSA_TYPESIZE
      LOGICAL PAR_ABORT         ! (F)PAR abort flag
C
C     Local variables
C
      INTEGER      ADDRESS      ! Address of dynamic memory element
      INTEGER      BYTES        ! Amount of workspace required
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      INTEGER      DPTR         ! Dynamic-memory pointer to data array
      INTEGER      DSLOT        ! Map slot number of input data array
      REAL         EXTRA        !
      INTEGER      IGNORE       ! Used to ignore status codes
      INTEGER      MODE         !
      INTEGER      NCXX         !
      INTEGER      NCXY         !
      INTEGER      NCYX         !
      INTEGER      NCYY         !
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NDX          !
      INTEGER      NDY          !
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      NPIX         ! Size of 1st dimension
      INTEGER      NLINE        ! Size of 2nd dimension (if present)
      INTEGER      OPTR         ! Dynamic-memory pointer to output data array
      INTEGER      OSLOT        ! Map slot number for output data array
      INTEGER      PSLOT        ! Map slot number for workspace
      INTEGER      PX           !
      INTEGER      PY           !
      LOGICAL      REALS        !
      INTEGER      STATUS       ! Running status for DSA_ routines
      REAL         VALS(20,20)  !
      REAL         VALUE        ! Temporary real number
      INTEGER      WBYTES       !
      INTEGER      WPTR         ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT        ! Map slot number of workspace
      DOUBLE PRECISION XCOEFF(1,2) !
      LOGICAL      XLOG         !
      REAL         XRESMIN      !
      REAL         XRESMAX      !
      REAL         XSHIFT       !
      REAL         XSPLIT       !
      DOUBLE PRECISION YCOEFF(2,1) !
      LOGICAL      YLOG         !
      REAL         YRESMIN      !
      REAL         YRESMAX      !
      REAL         YSHIFT       !
      REAL         YSPLIT       !
      LOGICAL      VEXIST       ! TRUE if a variance array exists
      INTEGER      VPTR         ! Dynamic-memory pointer to input variance 
      INTEGER      VSLOT        ! Map slot number for input variance array
      INTEGER      OVPTR        ! Dynamic-memory pointer to output variance 
      INTEGER      OVSLOT       ! Map slot number for output variance array
      CHARACTER    HIST(3)*50   ! History text.
C
C     Dynamic memory support - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
C
C     Parameter values for FIG_REBIN2D
C
      DATA XCOEFF,YCOEFF,XLOG,YLOG/1.,0.,1.,0.,.FALSE.,.FALSE./
      DATA NCXY,NCXX,NCYY,NCYX/1,2,2,1/
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      IF (NDIM.EQ.1) THEN
         NLINE=1
      ELSE
         NLINE=DIMS(2)
      END IF
      NPIX=DIMS(1)
C
C     Map input data
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',ADDRESS,DSLOT,STATUS)
      DPTR=DYN_ELEMENT(ADDRESS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get the various parameters, checking for the integer shift case
C
      CALL PAR_RDVAL('XSHIFT',-FLOAT(NPIX),FLOAT(NPIX),0.,'Pixels',
     :                                                      XSHIFT)
      CALL PAR_RDVAL('YSHIFT',-FLOAT(NLINE),FLOAT(NLINE),0.,'Pixels',
     :                                                      YSHIFT)
      REALS=(ABS(MOD(XSHIFT,1.)).GT.0.0001).OR.
     :                             (ABS(MOD(YSHIFT,1.)).GT.0.0001)
      IF (REALS) THEN
         CALL PAR_RDVAL('XSPLIT',1.,20.,1.,'sub-pixels',VALUE)
         NDX=NINT(VALUE)
         CALL PAR_RDVAL('YSPLIT',1.,20.,1.,'sub-pixels',VALUE)
         NDY=NINT(VALUE)
      END IF
      IF ( PAR_ABORT() ) GO TO 500
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map output data
C
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',ADDRESS,OSLOT,STATUS)
      OPTR=DYN_ELEMENT(ADDRESS)
      IF (STATUS.NE.0) GOTO 500
C
C     Single operand (result replaces original data). Get workspace to
C     rebin into.  This is not needed in the integer shift case.
C
      IF (OPTR.EQ.DPTR) THEN
         IF (REALS) THEN
            BYTES=NELM*DSA_TYPESIZE('FLOAT',STATUS)
            CALL DSA_GET_WORKSPACE (BYTES,ADDRESS,WSLOT,STATUS)
            IF (STATUS.NE.0) GO TO 500
            WPTR=DYN_ELEMENT(ADDRESS)
            CALL GEN_MOVE (BYTES,DYNAMIC_MEM(DPTR),DYNAMIC_MEM(WPTR))
            DPTR=WPTR
         END IF
      END IF
C
C     If either of the shifts are real, we have to do this the hard
C     way.  Get the rest of the workspace needed by FIG_REBIN2D
C
      IF (REALS) THEN
         WBYTES=16*(NPIX+3)
         CALL DSA_GET_WORKSPACE(WBYTES,ADDRESS,PSLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
         PX=DYN_ELEMENT(ADDRESS)
         PY=PX+WBYTES/2
C
C        Set the remaining parameters for FIG_REBIN2D and then let it get
C        on with the job.
C
         MODE=1
         XRESMIN=0.
         XRESMAX=FLOAT(NPIX)
         YRESMIN=0.
         YRESMAX=FLOAT(NLINE)
         CALL FIG_REBIN2D(DYNAMIC_MEM(DPTR),NPIX,NLINE,NPIX,NLINE,
     :             XRESMIN,XRESMAX,XLOG,YRESMIN,YRESMAX,YLOG,NCXY,NCXX,
     :             XCOEFF,NCYY,NCYX,YCOEFF,XSHIFT,YSHIFT,NDX,NDY,
     :             MODE,EXTRA,DYNAMIC_MEM(PX),DYNAMIC_MEM(PY),VALS,
     :                                           DYNAMIC_MEM(OPTR))
      ELSE
C
C        This is the simple integer shift case.  Note that this
C        operation can be performed in situ, if needed.
C
         CALL GEN_ISHIFT(DYNAMIC_MEM(DPTR),NPIX,NLINE,NINT(XSHIFT),
     :                              NINT(YSHIFT),DYNAMIC_MEM(OPTR))
      END IF
C
C     Check whether a variance array exists.  If so then process it
C     in exactly same way as the data array.
C
C     Note that the variance array is necessarily the same size as the
C     data array and hence the work arrays mapped for the data array are
C     re-used here.
C
      VEXIST = .FALSE.
      CALL DSA_SEEK_VARIANCE ('IMAGE',VEXIST,STATUS)
C
      IF (STATUS.EQ.0  .AND.  VEXIST) THEN
C
C        Map the input and output variance arrays.
C
         CALL DSA_MAP_VARIANCE('IMAGE','READ','FLOAT',ADDRESS,
     :     VSLOT,STATUS)
         VPTR=DYN_ELEMENT(ADDRESS)
         IF (STATUS.NE.0) GOTO 500
         CALL DSA_MAP_VARIANCE('OUTPUT','WRITE','FLOAT',ADDRESS,
     :     OVSLOT, STATUS)
         OVPTR=DYN_ELEMENT(ADDRESS)
         IF (STATUS.NE.0) GOTO 500
C
C        Check for the single operand case (the new variance replaces
C        the original one).
C
         IF (OVPTR.EQ.VPTR) THEN
            IF (REALS) THEN
               CALL GEN_MOVE (BYTES,DYNAMIC_MEM(VPTR),
     :           DYNAMIC_MEM(OVPTR))
               VPTR=OVPTR
            END IF
         END IF
C
C        If either of the shifts are real, we have to do this the hard
C        way.  Get the rest of the workspace needed by FIG_REBIN2D
C
         IF (REALS) THEN
            CALL FIG_REBIN2D(DYNAMIC_MEM(VPTR),NPIX,NLINE,NPIX,NLINE,
     :        XRESMIN,XRESMAX,XLOG,YRESMIN,YRESMAX,YLOG,NCXY,NCXX,
     :        XCOEFF,NCYY,NCYX,YCOEFF,XSHIFT,YSHIFT,NDX,NDY,
     :        MODE,EXTRA,DYNAMIC_MEM(PX),DYNAMIC_MEM(PY),VALS,
     :        DYNAMIC_MEM(OVPTR))
         ELSE
C
C           This is the simple integer shift case.  Note that this
C           operation can be performed in situ, if needed.
C
            CALL GEN_ISHIFT(DYNAMIC_MEM(VPTR),NPIX,NLINE,
     :        NINT(XSHIFT),NINT(YSHIFT),DYNAMIC_MEM(OVPTR))
         END IF
C
C        Add comments to the history section.  Comments are only
C        required if resampling has occurred.
C
         IF (REALS) THEN
            HIST(1) =
     :        'The variance array has been simply re-sampled.'
            HIST(2) =
     :        'The resulting values are probably under-estimates'
            HIST(3) = 'of the true errors.'
            CALL DSA_PUT_HIST (OSLOT, 3, HIST, STATUS)
         END IF
      END IF
C
C     Closedown everything
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
