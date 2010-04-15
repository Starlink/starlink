C+
      SUBROUTINE ISTRETCH
C
C     I S T R E T C H
C
C     Shifts an image in both X and Y, by, in each direction,
C     a constant amount expressed in pixels and stretches it
C     by a given factor - this is a linear stretch, combined
C     with a shift.  Note that this is functionally a superset
C     of ISHIFT, but ISHIFT should be used for cases where the
C     stretch is 1.0 in both X and Y, since it uses a simpler
C     algorithm for very simple cases.
C
C     Command parameters -
C
C     IMAGE    The name of the structure containing the image.
C
C     XSTRETCH (Numeric) The stretch factor to be applied in X.
C
C     YSTRECTH (Numeric) The stretch factor to be applied in Y.
C
C     XSHIFT   (Numeric) The number of pixels the image is to be
C              shifted in X.  A -ve number shifts towards lower
C              numbered pixels.
C
C     YSHIFT   (Numeric) The number of pixels the image is to be
C              shifted in Y.  Sense is as for XSHIFT.
C
C     XSPLIT   (Numeric) The number of sub-divisions to be made in
C              each of the original pixels in X.
C
C     YSPLIT   (Numeric) The number of sub-divisions to be made in
C              each of the original pixels in Y.
C
C              Note that if XSPLIT or YSPLIT are greater than 1,
C              the data will be interpolated using a fit to a 2D
C              parabola.  This increases the accuracy (sometimes)
C              of the rebinning, but results in increased CPU usage.
C
C     OUTPUT   The name of the result of the operation.  This can
C              be the same as for IMAGE.  If not, a new structure
C              is created, with everything but the data a direct
C              copy of the input.  Note that the size of the image
C              is the same as before - so either some pixels will
C              be stretched out of the image, or some pixels will
C              just be zero.
C
C     Command keywords -  None
C
C                                      KS / CIT 5th Dec 1983
C
C     Modified:
C
C     3rd  Aug 1987  DJA/AAO. Revised DSA_ routines - some specs
C                    changed. Dynamic memory handling now done using
C                    DYN_ routines
C     7th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed. Redundant second set of DATA statements
C                    removed.
C     26th Jul 1996  MJCL / Starlink, UCL.  Added PAR_ABORT checking.
C     2005 June 8    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER DSA_TYPESIZE
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
      INTEGER      BYTES         ! Amount of workspace required
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      INTEGER      DPTR          ! Dynamic-memory pointer to data array
      INTEGER      DSLOT         ! Map slot number of input data array
      REAL         EXTRA         !
      INTEGER      MODE          !
      INTEGER      NCXX          !
      INTEGER      NCXY          !
      INTEGER      NCYX          !
      INTEGER      NCYY          !
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NDX           !
      INTEGER      NDY           !
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NPIX          ! Size of 1st dimension
      INTEGER      NLINE         ! Size of 2nd dimension (if present)
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number for output data array
      INTEGER      PX            ! Workspace pointer
      INTEGER      PXSLOT        ! Map slot number for workspace
      INTEGER      PY            ! Workspace pointer
      INTEGER      PYSLOT        ! Map slot number for workspace
      INTEGER      STATUS        ! Running status for DSA_ routines
      REAL         VALS(20,20)   !
      REAL         VALUE         ! Temporary real number
      INTEGER      WPTR          ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT         ! Map slot number of workspace
      DOUBLE PRECISION XCOEFF(1,2) !
      LOGICAL      XLOG          !
      REAL         XRESMIN       !
      REAL         XRESMAX       !
      REAL         XSHIFT        !
      REAL         XSTRCH        !
      DOUBLE PRECISION YCOEFF(2,1) !
      LOGICAL      YLOG          !
      REAL         YRESMIN       !
      REAL         YRESMAX       !
      REAL         YSHIFT        !
      REAL         YSTRCH        !
C
C     Parameter values for FIG_REBIN2D
C
      DATA XCOEFF,YCOEFF,XLOG,YLOG/1.,0.,1.,0.,.FALSE.,.FALSE./
      DATA NCXY,NCXX,NCYY,NCYX/1,2,2,1/
C
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
      IF (STATUS.NE.0) GOTO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      IF (NDIM.EQ.1) THEN
         DIMS(2)=1
      ELSE IF (NDIM.EQ.0) THEN
         CALL PAR_WRUSER('This is neither an image nor a spectrum',
     :                                                      STATUS)
         GO TO 500
      END IF
      NLINE=DIMS(2)
      NPIX=DIMS(1)
C
C     Map input data
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get the various parameters.
C
      CALL PAR_RDVAL('XSTRETCH',0.,1000.,1.,' ',XSTRCH)
      CALL PAR_RDVAL('YSTRETCH',0.,1000.,1.,' ',YSTRCH)
      CALL PAR_RDVAL('XSHIFT',-FLOAT(NPIX),FLOAT(NPIX),0.,'Pixels',
     :               XSHIFT)
      CALL PAR_RDVAL('YSHIFT',-FLOAT(NLINE),FLOAT(NLINE),0.,'Pixels',
     :               YSHIFT)
      CALL PAR_RDVAL('XSPLIT',1.,20.,1.,'sub-pixels',VALUE)
      NDX=VALUE
      CALL PAR_RDVAL('YSPLIT',1.,20.,1.,'sub-pixels',VALUE)
      IF ( PAR_ABORT() ) GO TO 500
      NDY=VALUE
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
C
C     Map output data
C
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
      IF (DPTR.EQ.OPTR) THEN
C
C        Single operand (result replaces original data). Get
C        workspace to rebin into.
C
         BYTES=NPIX*NLINE*DSA_TYPESIZE('FLOAT',STATUS)
         CALL DSA_GET_WORKSPACE(BYTES,WPTR,WSLOT,STATUS)
         CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(DPTR)),%VAL(CNF_PVAL(WPTR)))
         DPTR=WPTR
      END IF
C
C     Get the rest of the workspace needed by FIG_REBIN2D
C
      CALL DSA_GET_WORK_ARRAY(2*(NPIX+3),'FLOAT',PX,PXSLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(2*(NPIX+3),'FLOAT',PY,PYSLOT,STATUS)
C
C     Set the parameters for FIG_REBIN2D and then let it get
C     on with the job.
C
      MODE=1
      XCOEFF(1,1)=XSTRCH
      XCOEFF(1,2)=0.
      YCOEFF(1,1)=YSTRCH
      YCOEFF(2,1)=0.
      XRESMIN=0.
      XRESMAX=FLOAT(NPIX)
      YRESMIN=0.
      YRESMAX=FLOAT(NLINE)
      CALL FIG_REBIN2D(%VAL(CNF_PVAL(DPTR)),NPIX,NLINE,NPIX,NLINE,
     :                 XRESMIN,XRESMAX,XLOG,YRESMIN,YRESMAX,YLOG,NCXY,
     :                 NCXX,XCOEFF,NCYY,NCYX,YCOEFF,XSHIFT,YSHIFT,
     :                 NDX,NDY,MODE,EXTRA,%VAL(CNF_PVAL(PX)),
     :                 %VAL(CNF_PVAL(PY)),VALS,%VAL(CNF_PVAL(OPTR)))
C
C     Tidy up
C
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
