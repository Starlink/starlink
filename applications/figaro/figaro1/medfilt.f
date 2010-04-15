C+
      SUBROUTINE MEDFILT
C
C     M E D F I L T   /    M E D F I L T R
C
C     Figaro routines to median filter an image.  The result of
C     this operation is an image in which the value of each pixel
C     is the median value of a rectangular box of pixels centered on the
C     corresponding pixel in the original array. MEDFILTR allows the
C     box to be specified with different X and Y dimensions, while the
C     original MEDFILT only supported a square box. MEDFILTR is
C     therefore a full superset of MEDFILT, but the older application
C     has to be retained for compatability reasons.
C
C     Command parameters for MEDFILT -
C
C     IMAGE    The name of the structure containing the image.
C
C     BOX      (Numeric) The size of the box (in pixels) to be
C              used in calculating the medians.  Should be odd;
C              if even, BOX-1 will be used.
C
C     OUTPUT   The name of the result of the operation.  This can
C              be the same as for IMAGE.  If not, a new structure
C              is created, with everything but the data a direct
C              copy of the input.
C
C     Command parameters for MEDFILTR -
C
C     IMAGE    The name of the structure containing the image.
C
C     XBOX     (Numeric) The X-dimension of the box (in pixels) to be
C              used in calculating the medians.  Should be odd;
C              if even, XBOX-1 will be used.
C
C     YBOX     (Numeric) The Y-dimension of the box (in pixels) to be
C              used in calculating the medians.  Should be odd;
C              if even, YBOX-1 will be used.
C
C     OUTPUT   The name of the result of the operation.  This can
C              be the same as for IMAGE.  If not, a new structure
C              is created, with everything but the data a direct
C              copy of the input.
C
C                                      KS / CIT 28th Jan 1984
C     Modified:
C
C     11th Aug 1987  Revised DSA_ routines - some specs changed. Now
C                    uses DYN_ routines for dynamic memory handling.
C     7th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed.
C     21st Mar 1994  KS / AAO. Added MEDFILTR code.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER DSA_TYPESIZE
C
C     Maximum size of median box
C
      INTEGER BOXMAX
      PARAMETER (BOXMAX=50)
C
C     Local variables
C
      INTEGER      BYTES         ! Number of bytes of workspace required
      CHARACTER    COMMAND*8     ! Command being serviced.
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      INTEGER      DPTR          ! Dynamic-memory pointer to data array
      INTEGER      DSLOT         ! Map slot number of input data array
      INTEGER      IXBOX         ! X-dimension of median box
      INTEGER      IYBOX         ! Y-dimension of median box
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      NY            ! Size of 2nd dimension (if present)
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number outputdata array
      INTEGER      STATUS        ! Running status for DSA_ routines
      REAL         VALUE         ! Temporary real number
      LOGICAL      WARN          ! Indicates we need to warn about box
                                 ! size
      REAL   WORK(BOXMAX*BOXMAX) ! Data over which median is calculated
      INTEGER      WPTR          ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT         ! Map slot number of workspace
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     See which command we are handling
C
      CALL PAR_COMMAND(COMMAND)
C
C     Get input name
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NY=NELM/NX
      BYTES=NELM*DSA_TYPESIZE('FLOAT',STATUS)
C
C     Get dimensions of median box, the parameters depending on the
C     command being handled.
C
      WARN=.FALSE.
      IF (COMMAND.EQ.'MEDFILTR') THEN
         CALL PAR_RDVAL('XBOX',1.,FLOAT(BOXMAX),5.,'Pixels',VALUE)
         IXBOX=NINT(VALUE)
         CALL PAR_RDVAL('YBOX',1.,FLOAT(BOXMAX),5.,'Pixels',VALUE)
         IYBOX=NINT(VALUE)
      ELSE
         CALL PAR_RDVAL('BOX',1.,FLOAT(BOXMAX),5.,'Pixels',VALUE)
         IXBOX=NINT(VALUE)
         IYBOX=IXBOX
      END IF
      IF (STATUS.NE.0) GOTO 500
      IF (MOD(IXBOX,2).NE.1) THEN
         WARN=.TRUE.
         IXBOX=IXBOX-1
      END IF
      IF (MOD(IYBOX,2).NE.1) THEN
         WARN=.TRUE.
         IYBOX=IYBOX-1
      END IF
      IF (WARN) THEN
         CALL PAR_WRUSER(
     :      'Note: Filter boxes should be of odd dimensions',STATUS)
         CALL PAR_WRUSER(
     :      'Even values have been taken as the lower odd value',STATUS)
      END IF
C
C     Check on what we do for non-2D data.
C
      IF ((NDIM.NE.2).AND.(IYBOX.GT.1)) THEN
         CALL PAR_WRUSER (
     :      'For anything other than 2D data, the Y size of the filter',
     :      STATUS)
         CALL PAR_WRUSER (
     :      'box must be 1. Y size is being set to 1.',STATUS)
         IYBOX=1
      END IF
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map data.  Note that GEN_MEDFLT cannot operate on data in situ,
C     so in the single operand case workspace must be obtained and
C     the data copied back from it later.
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',OPTR,OSLOT,STATUS)
      IF (DPTR.EQ.OPTR) THEN
         CALL DSA_GET_WORKSPACE (BYTES,WPTR,WSLOT,STATUS)
         CALL GEN_MOVE (BYTES,%VAL(CNF_PVAL(DPTR)),%VAL(CNF_PVAL(WPTR)))
         DPTR=WPTR
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     Perform filtering
C
      CALL GEN_MEDFLT(%VAL(CNF_PVAL(DPTR)),NX,NY,IXBOX,IYBOX,WORK,
     :                                     %VAL(CNF_PVAL(OPTR)))
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
