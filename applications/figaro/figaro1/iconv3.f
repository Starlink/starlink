C+
      SUBROUTINE ICONV3
C
C     I C O N V 3
C
C     Convolves an image with a 3x3 symmetric convolution kernel.
C     This allows a variety of spatial filters to be applied to
C     an image.  The 3 by 3 array convolved with the image is defined
C     by two values, the central value and the single value used
C     for the eight edge elements.  That is, if C is the central and
C     E the edge value, the kernel array looks like
C
C                       E  E  E
C                       E  C  E
C                       E  E  E
C
C     Command parameters -
C
C     IMAGE    The name of the structure containing the image.
C
C     CENTER   The value for the kernel central element.
C
C     EDGE     The value for the kernel edge element.
C
C     OUTPUT   The name of the result of the operation.  This can
C              be the same as for IMAGE.  If not, a new structure
C              is created, with everything but the data a direct
C              copy of the input.
C
C                                      KS / AAO 30th Oct 1987
C     Modified:
C
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     25th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. TABs
C                    removed.
C     16th Sep 1994  HME / UoE, Starlink.  Change call to GEN_FILT3,
C                    which now has one argument less.
C     2005 June 8    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      REAL         CENTER        ! Value of CENTER parameter
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      REAL         EDGE          ! Value of EDGE parameter
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      NY            ! Size of 2nd dimension (if present)
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number outputdata array
      INTEGER      STATUS        ! Running status for DSA_ routines
      INTEGER      WPTR          ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT         ! Map slot number of workspace
C
C     Limits for numeric parameters - close to VAX limits
C
      REAL FMAX, FMIN
      PARAMETER (FMAX = 1.7E38, FMIN = -1.7E38)
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
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      IF (NDIM.LT.2) THEN
         NY=1
      ELSE
         NY=DIMS(2)
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     Get values for CENTER and EDGE
C
      CALL PAR_RDVAL('CENTER',FMIN,FMAX,1.0,' ',CENTER)
      CALL PAR_RDVAL('EDGE',FMIN,FMAX,1.0,' ',EDGE)
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
C
C     Map data
C
      CALL DSA_MATCH_SIZES('IMAGE','OUTPUT',STATUS)
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,
     :                   STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get workspace for GEN_FILT3 (if data is 1D we can use a
C     dummy workspace - GEN_FILT3 will ignore it)
C
      IF ((NX.GT.1).AND.(NY.GT.1)) THEN
         CALL DSA_GET_WORK_ARRAY(NX*2,'FLOAT',WPTR,WSLOT,STATUS)
      ELSE
         WPTR=OPTR
      END IF
C
C     Pass the filter through the data.
C
      CALL GEN_FILT3(NX,NY,CENTER,EDGE,
     :                     %VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(OPTR)))
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
