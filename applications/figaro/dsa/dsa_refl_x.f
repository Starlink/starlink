C+
C                       D S A _ R E F L A G _ x
C
C  Routine name:
C     DSA_REFLAG_x
C
C  Function:
C     Reinstates `flagged' data values from a data array of specified type
C
C  Description:
C     DSA_REFLAG_x covers the routines DSA_REFLAG_F, DSA_REFLAG_I,
C     DSA_REFLAG_D, DSA_REFLAG_S, DSA_REFLAG_B, DSA_REFLAG_U and
C     DSA_REFLAG_C.  This set of routines reverses the effect of the
C     DSA_UNFLAG_x set of routines, reinstating `flagged' values
C     removed and remembered by those other routines.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_REFLAG_x (NELM,DATA,FLAGS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NELM      (Integer,ref) The number of data elements in the array.
C     (!) DATA      (Array, ref) The data array to be processed.  Note that
C                   even a character array is passed by reference.
C     (>) FLAGS     (Byte array, ref) The quality array produced by the
C                   corresponding DSA_UNFLAG_x routine. Elements
C                   corresponding to flagged elements are set to 1, all
C                   others are set to zero.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DSA_GET_FLAG_VALUE
C
C  Prior requirements: None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_GET_FLAG_VALUE  Get flag value for a specific type
C
C  History:
C     20th July 1988  Original version.  KS / AAO.
C     25th Apr  1989  Support for USHORT type added.  KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_REFLAG_F (NELM,DATA,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      REAL    FLAG_VALUE                  ! Flag value for this data type
      INTEGER I                           ! Loop index through data elements
      INTEGER STATUS                      ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('FLOAT',FLAG_VALUE,STATUS)
C
C     Loop through data, reinstating flagged values. as we find them
C
      DO I=1,NELM
         IF (FLAGS(I).NE.0) DATA(I)=FLAG_VALUE
      END DO
C
      END
C
      SUBROUTINE DSA_REFLAG_D (NELM,DATA,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      DOUBLE PRECISION DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      DOUBLE PRECISION FLAG_VALUE         ! Flag value for this data type
      INTEGER I                           ! Loop index through data elements
      INTEGER STATUS                      ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('DOUBLE',FLAG_VALUE,STATUS)
C
C     Loop through data, reinstating flagged values. as we find them
C
      DO I=1,NELM
         IF (FLAGS(I).NE.0) DATA(I)=FLAG_VALUE
      END DO
C
      END
C
      SUBROUTINE DSA_REFLAG_S (NELM,DATA,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      INTEGER*2 DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      INTEGER*2 FLAG_VALUE                ! Flag value for this data type
      INTEGER I                           ! Loop index through data elements
      INTEGER STATUS                      ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('SHORT',FLAG_VALUE,STATUS)
C
C     Loop through data, reinstating flagged values. as we find them
C
      DO I=1,NELM
         IF (FLAGS(I).NE.0) DATA(I)=FLAG_VALUE
      END DO
C
      END
C
      SUBROUTINE DSA_REFLAG_I (NELM,DATA,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      INTEGER DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      INTEGER FLAG_VALUE                  ! Flag value for this data type
      INTEGER I                           ! Loop index through data elements
      INTEGER STATUS                      ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('INT',FLAG_VALUE,STATUS)
C
C     Loop through data, reinstating flagged values. as we find them
C
      DO I=1,NELM
         IF (FLAGS(I).NE.0) DATA(I)=FLAG_VALUE
      END DO
C
      END
C
      SUBROUTINE DSA_REFLAG_B (NELM,DATA,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      BYTE DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      BYTE    FLAG_VALUE                  ! Flag value for this data type
      INTEGER I                           ! Loop index through data elements
      INTEGER STATUS                      ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('BYTE',FLAG_VALUE,STATUS)
C
C     Loop through data, reinstating flagged values. as we find them
C
      DO I=1,NELM
         IF (FLAGS(I).NE.0) DATA(I)=FLAG_VALUE
      END DO
C
      END
C
      SUBROUTINE DSA_REFLAG_C (NELM,DATA,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      BYTE DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      CHARACTER FLAG_CHAR*1               ! Character flag value
      BYTE      FLAG_VALUE                ! Flag value for this data type
      INTEGER   I                         ! Loop index through data elements
      INTEGER   STATUS                    ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('CHAR',FLAG_CHAR,STATUS)
      FLAG_VALUE=ICHAR(FLAG_CHAR)
C
C     Loop through data, reinstating flagged values. as we find them
C
      DO I=1,NELM
         IF (FLAGS(I).NE.0) DATA(I)=FLAG_VALUE
      END DO
C
      END
C
      SUBROUTINE DSA_REFLAG_U (NELM,DATA,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      INTEGER*2 DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      INTEGER*2 FLAG_VALUE                ! Flag value for this data type
      INTEGER I                           ! Loop index through data elements
      INTEGER STATUS                      ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('USHORT',FLAG_VALUE,STATUS)
C
C     Loop through data, reinstating flagged values. as we find them
C
      DO I=1,NELM
         IF (FLAGS(I).NE.0) DATA(I)=FLAG_VALUE
      END DO
C
      END
