C+
      SUBROUTINE ICOR16
C
C     I C O R 1 6
C
C     It is possible to have an image written on tape in what looks
C     like unsigned 16 bit data, when what is intended is actually
C     signed 16 bit data.  This is often true of FITS tapes, where
C     16 bit data is always assumed to be unsigned.  ICOR16 corrects
C     such an image.
C
C     Command parameters -
C
C     IMAGE  The name of the structure containing the image.
C
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C     Command keywords  - None
C
C     User variables used - None
C                                      KS / AAO 6th Dec 1984
C
C     Modified:
C
C     31st Jul 1987  DJA / AAO. Revised DSA_ routines - some specs
C                    changed. Modified dynamic memory handling - now
C                    uses DYN_ routines.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed.
C     2005 June 8    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NELM         ! Total number of elements in data
      LOGICAL      OK           ! Status returned by conversion routine
      INTEGER      OPTR         ! Dynamic-memory pointer to output data array
      INTEGER      OSLOT        ! Map slot number outputdata array
      INTEGER      STATUS       ! Running status for DSA_ routines
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
      CALL DSA_DATA_SIZE('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map data.  Note that GEN_ICOR16 can operate on data in situ.
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
C
C     Operate on the data.
C
      CALL GEN_ICOR16(%VAL(CNF_PVAL(OPTR)),NELM,%VAL(CNF_PVAL(OPTR)),OK)
      IF (.NOT.OK) THEN
         CALL PAR_WRUSER(
     :          'WARNING - Input data were not integers'
     :                        //' in range -32768..32767',STATUS)
      END IF
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
