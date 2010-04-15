C+
      SUBROUTINE XCOPY
C
C     X C O P Y
C
C     Copies the axis information from one Figaro data structure
C     (typically an arc) into another data structure (typically
C     some other spectrum).  The result is either a modified version
C     of the other data structure, or a new structure.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The file name of the spectrum whose
C                 data is to be combined with the axis data from the
C                 arc.
C
C     ARC         (Character) The file name of the arc spectrum - ie
C                 the spectrum whose axis information is to be used.
C
C     OUTPUT      (Character) The file name for the resulting data
C                 structure.  If this is the same as SPECTRUM, the
C                 axis data in SPECTRUM will be changed in situ.
C
C     Command keywords - None
C
C     User variables used - None
C
C                                             KS / CIT 7th June 1983
C     Modified:
C     21st Oct  1988.  JM / RAL. Modified to use DSA_ routines
C     23rd Sep  1992.  HME / UoE, Starlink.  Call PAR_WRUSER rather
C                      than DSA_WRUSER.
C+
      IMPLICIT NONE
C
C     Local variables
C
      INTEGER   DIMS(5)    ! Accomodates axis data dimensions
      LOGICAL   EXIST      ! Used to check for existence of axis data
      INTEGER   NDIM       ! Number of dimensions in axis data
      INTEGER   NELM       ! Number of elements in ARC axis data array
      INTEGER   STATUS     ! Running status for DSA_ routines
C
C     Initial values
C
      STATUS=0
      EXIST=.FALSE.
C
C     Open DSA
C
      CALL DSA_OPEN(STATUS)
C
C     Get value of SPECTRUM and open the file
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
C
C     Get value of ARC and open the file
C
      CALL DSA_INPUT('ARC','ARC',STATUS)
C
C     Check ARC axis data-array exists
C
      CALL DSA_SEEK_AXIS('ARC',1,EXIST,STATUS)
      IF(STATUS.NE.0)GOTO 500
      IF(.NOT.EXIST)THEN
         CALL PAR_WRUSER('Arc has no axis data-array',STATUS)
         GOTO 500
      ENDIF
C
C     Get size of axis data-array in Arc
C
      CALL DSA_AXIS_SIZE('ARC',1,5,NDIM,DIMS,NELM,STATUS)
C
C     Create a new data structure modelled on SPECT, but with
C     the axis structure modelled on ARC.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      CALL DSA_RESHAPE_AXIS('OUTPUT',1,'ARC',1,NDIM,DIMS,STATUS)

  500 CONTINUE
C
C     Close down everything
C
      CALL DSA_CLOSE(STATUS)

      END
