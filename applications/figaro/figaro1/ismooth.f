C+
      SUBROUTINE ISMOOTH
C
C     I S M O O T H
C
C     This routine is the main body of ISMOOTH, which smooths
C     a 2D image using a nine-point 2D smoothing algorithm.  This
C     is a 2D extension of the 3-point spectrum smoothing algorithm
C     which replaces each pixel with 1/2 its own value plus 1/4 of
C     the value of each of its two neighbours.  The routine used here
C     (see GEN_ASMOTH for details) replaces each pixel with a fraction
C     of its own value plus a fraction of the value of its eight nearest
C     neighbours.  The fraction from a 'corner' neighbour is 1.414 times
C     less than from a 'side' neighbour.  Repeating the process
C     increases the amount of smothing applied, and the operation is
C     close in effect to a gaussian convolution.
C
C     Command parameters -
C
C     IMAGE    The name of the structure containing the image.
C
C     FRACTION (Numeric) The fraction of each pixel's value to be
C              distributed to its neighbours.  The higher this
C              value the greater the smoothing effect.
C
C     REPEAT   (Numeric) The number of times the operation is to
C              be repeated.  The higher this value, the greater
C              the smoothing effect.
C
C     OUTPUT   The name of the result of the operation.  This can
C              be the same as for IMAGE.  If not, a new structure
C              is created, with everything but the data a direct
C              copy of the input.
C
C                                      KS / CIT 21st June 1983
C
C     Modified:
C
C     28th Jul 1987  DJA / AAO. Revised DSA_ routines - some specs
C                    changed. Modified dynamic memory handling - now
C                    uses DYN rotuines
C     7th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed.
C     16th Sep 1994  HME / UoE, Starlink.  Change mapping so that only
C                    the output (copy of input) is mapped, for UPDATE.
C                    Thus GEN_ASMOTH will always work in situ and REPEAT
C                    > 1 will have an effect after all. GEN_ASMOTH has
C                    been changed to always work in situ and more
C                    importantly to use only one dummy argument for
C                    input and output.
C     26th Jul 1996  MJCL / Starlink, UCL.  Added PAR_ABORT checking.
C     2005 June 8    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
C
C     Local variables
C
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      REAL         FRACT         ! see above
      INTEGER      IRPT          !
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      NY            ! Size of 2nd dimension (if present)
      INTEGER      OPTR          ! Dynamic-memory pointer to data array
      INTEGER      OSLOT         ! Map slot number outputdata array
      INTEGER      REPEAT        ! see above
      INTEGER      STATUS        ! Running status for DSA_ routines
      REAL         VALUE         ! Temporary real number
      INTEGER      WPTR          ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT         ! Map slot number of workspace
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
C     Get values for FRACTION and REPEAT
C
      CALL PAR_RDVAL('FRACTION',0.0,1.0,0.5,' ',FRACT)
      CALL PAR_RDVAL('REPEAT',1.,1000.,1.,' ',VALUE)
      IF ( PAR_ABORT() ) GO TO 500
      REPEAT=NINT(VALUE)
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
C
C     Map data
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get workspace for GEN_ASMOTH (if data is 1D we can use a
C     dummy workspace - ASMOTH will ignore it)
C
      IF ((NX.GT.1).AND.(NY.GT.1)) THEN
         CALL DSA_GET_WORK_ARRAY(NX*2,'FLOAT',WPTR,WSLOT,STATUS)
      ELSE
         WPTR=OPTR
      END IF
C
C     Perform the smoothing - smooth the data into itself and
C     repeat as many times as required
C
      DO IRPT=1,REPEAT
         CALL GEN_ASMOTH(NX,NY,FRACT,%VAL(CNF_PVAL(WPTR)),
     :                   %VAL(CNF_PVAL(OPTR)))
      END DO
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
