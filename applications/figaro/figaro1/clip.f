C+
      SUBROUTINE CLIP
C
C     C L I P
C
C     Clips an image (or spectrum, cube or whatever..).  Given a low
C     and a high threshold value, CLIP sets any elements above the
C     high value or below the low value to the appropriate value.
C
C     Command parameters -
C
C     IMAGE    (Character) The name of the structure containing the image.
C     LOWCLIP  (Numeric) The low threshold value
C     HIGHCLIP (Numeric) The high threshold value
C     OUTPUT   (Character) The name of the result of the operation.  This
C              can be the same as for IMAGE.  If not, a new structure
C              is created, with everything but the data a direct
C              copy of the input.
C
C                                      KS / AAO 22nd July 1985
C
C     Modified:
C
C     24th Jul 1987  DJA / AAO. Revised DSA_ routines - some specs changed.
C                    Modified dynamic memory handling - now uses DYN_
C                    routines.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     6th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed.
C     26th Jul 1996  MJCL / Starlink, UCL.  Changed to PAR_ABORT check
C                    rather than STATUS check for thresholds.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_ENCODE
      LOGICAL PAR_ABORT         ! (F)PAR abort flag
C
C     Local variables
C
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      REAL         HIGH         ! Value of high threshold
      INTEGER      IGNORE       ! Used to pass ignorable status
      INTEGER      INVOKE       ! Used to invoke functions
      REAL         LOW          ! Value of low threshold
      CHARACTER*64 MESSAGE      ! Output strings for user
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      NEXT         ! Index of a character in a string
      INTEGER      NHIGH        ! Number of points set to high threshold
      INTEGER      NLOW         !   "    "    "     "   "  low     "
      INTEGER      OPTR         ! Dynamic-memory pointer to output data array
      INTEGER      OSLOT        ! Map slot number outputdata array
      INTEGER      STATUS       ! Running status for DSA_ routines
C
C     Numeric parameter limits - close to VAX real limits
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get value of thresholds
C
      CALL PAR_RDVAL('LOWCLIP',FMIN,FMAX,FMIN,' ',LOW)
      CALL PAR_RDVAL('HIGHCLIP',LOW,FMAX,FMAX,' ',HIGH)
      IF ( PAR_ABORT() ) GO TO 500
C
C     Get output structure name
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map data
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Clip the image and report results. Note GEN_CLIPF can operate on its
C     data in situ.
C
      CALL GEN_CLIPF(%VAL(CNF_PVAL(OPTR)),NELM,LOW,HIGH,NLOW,NHIGH,
     :                                        %VAL(CNF_PVAL(OPTR)))
C
      MESSAGE='Number of points set to low  threshold ('
      INVOKE=ICH_ENCODE(MESSAGE,LOW,41,3,NEXT)
      MESSAGE(NEXT:)=') = '
      INVOKE=ICH_ENCODE(MESSAGE,REAL(NLOW),NEXT+4,0,NEXT)
      CALL PAR_WRUSER(MESSAGE(:NEXT-1),IGNORE)
      MESSAGE='Number of points set to high threshold ('
      INVOKE=ICH_ENCODE(MESSAGE,HIGH,41,3,NEXT)
      MESSAGE(NEXT:)=') = '
      INVOKE=ICH_ENCODE(MESSAGE,REAL(NHIGH),NEXT+4,0,NEXT)
      CALL PAR_WRUSER(MESSAGE(:NEXT-1),IGNORE)
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE (STATUS)
C
      END
