C+
C                       D S A _ U N F L A G _ x
C
C  Routine name:
C     DSA_UNFLAG_x
C
C  Function:
C     Removes `flagged' data values from a data array of specified type
C
C  Description:
C     DSA_UNFLAG_x covers the routines DSA_UNFLAG_F, DSA_UNFLAG_I,
C     DSA_UNFLAG_D, DSA_UNFLAG_S, DSA_UNFLAG_B, DSA_UNFLAG_U and
C     DSA_UNFLAG_C.  This set of routines take a data array of
C     specified type that may contain `flagged' data values.  These
C     flagged values are replaced by harmless values - since flag
C     values tend to be extreme values and liable to produce arithmetic
C     errors if processed - and the locations of the flagged data
C     elements are remembered in a quality array.  The replacement is
C     unsubtle: the flagged elements are replaced by the value of the
C     previous unflagged element in the array if the array is numeric,
C     and by blank if it is a character array.  (If a whole numeric
C     array is flagged, it is set to zero.)  The number of flagged values
C     found in the array is returned. Optionally, the data values can be
C     left flagged, in which case all that happens is that their locations
C     are recorded in the quality array. The elements of the quality array
C     that correspond to unflagged values in the main array are untouched.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_UNFLAG_x (NELM,LEAVE,DATA,NFLAGGED,FLAGS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NELM      (Integer,ref) The number of data elements in the array.
C     (>) LEAVE     (Logical,ref) True if the data is to be left flagged.
C     (!) DATA      (Array, ref) The data array to be processed.  Note that
C                   even a character array is passed by reference.
C     (<) NFLAGGED  (Integer,ref) The number of flagged values found.
C     (<) FLAGS     (Byte array, ref) The quality array produced. Elements
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
C  Version date: 6th February 1995
C-
C  Subroutine / function details:
C     DSA_GET_FLAG_VALUE  Get flag value for a specific type
C
C  History:
C     20th July 1988  Original version.  KS / AAO.
C     25th Apr  1989  Support for USHORT type added.  KS / AAO.
C     3rd  May  1990  NFLAGGED parameter added.  KS/AAO.
C     5th  Apr  1991  Modified to cope properly with the case where the
C                     1st array element is flagged. KS/AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C     6th  Feb 1995   LEAVE parameter added to calling sequence. Quality
C                     array elements now set only if corresponding data
C                     value is flagged (previously, the rest of the quality
C                     array was cleared). KS/AAO.
C+
      SUBROUTINE DSA_UNFLAG_F (NELM,LEAVE,DATA,NFLAGGED,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LEAVE
      INTEGER NELM, NFLAGGED
      REAL DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      REAL    FLAG_VALUE                  ! Flag value for this data type
      INTEGER I                           ! Loop index through data elements
      INTEGER J                           ! Loop index through flagged elements
      INTEGER LAST_GOOD                   ! Last non-flagged element number
      INTEGER STATUS                      ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('FLOAT',FLAG_VALUE,STATUS)
C
C     Loop through data, replacing flagged values as we find them
C
      NFLAGGED=0
      LAST_GOOD=0
      DO I=1,NELM
         IF (DATA(I).EQ.FLAG_VALUE) THEN
            FLAGS(I)=1
            NFLAGGED=NFLAGGED+1
            IF (.NOT.LEAVE) THEN
               IF (LAST_GOOD.GT.0) THEN
                  DO J=LAST_GOOD+1,I
                     DATA(J)=DATA(LAST_GOOD)
                  END DO
               ELSE
                  DATA(I)=0.0
               END IF
            END IF
         ELSE
            LAST_GOOD=I
         END IF
      END DO
C
      END
C
      SUBROUTINE DSA_UNFLAG_D (NELM,LEAVE,DATA,NFLAGGED,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LEAVE
      INTEGER NELM, NFLAGGED
      DOUBLE PRECISION DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      DOUBLE PRECISION FLAG_VALUE         ! Flag value for this data type
      INTEGER I                           ! Loop index through data elements
      INTEGER J                           ! Loop index through flagged elements
      INTEGER LAST_GOOD                   ! Last non-flagged element number
      INTEGER STATUS                      ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('DOUBLE',FLAG_VALUE,STATUS)
C
C     Loop through data, replacing flagged values as we find them
C
      NFLAGGED=0
      LAST_GOOD=0
      DO I=1,NELM
         IF (DATA(I).EQ.FLAG_VALUE) THEN
            FLAGS(I)=1
            NFLAGGED=NFLAGGED+1
            IF (.NOT.LEAVE) THEN
               IF (LAST_GOOD.GT.0) THEN
                  DO J=LAST_GOOD+1,I
                     DATA(J)=DATA(LAST_GOOD)
                  END DO
               ELSE
                  DATA(I)=0.0D0
               END IF
            END IF
         ELSE
            LAST_GOOD=I
         END IF
      END DO
C
      END
C
      SUBROUTINE DSA_UNFLAG_S (NELM,LEAVE,DATA,NFLAGGED,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LEAVE
      INTEGER NELM, NFLAGGED
      INTEGER*2 DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      INTEGER*2 FLAG_VALUE                ! Flag value for this data type
      INTEGER I                           ! Loop index through data elements
      INTEGER J                           ! Loop index through flagged elements
      INTEGER LAST_GOOD                   ! Last non-flagged element number
      INTEGER STATUS                      ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('SHORT',FLAG_VALUE,STATUS)
C
C     Loop through data, replacing flagged values as we find them
C
      NFLAGGED=0
      LAST_GOOD=0
      DO I=1,NELM
         IF (DATA(I).EQ.FLAG_VALUE) THEN
            FLAGS(I)=1
            NFLAGGED=NFLAGGED+1
            IF (.NOT.LEAVE) THEN
               IF (LAST_GOOD.GT.0) THEN
                  DO J=LAST_GOOD+1,I
                     DATA(J)=DATA(LAST_GOOD)
                  END DO
               ELSE
                  DATA(I)=0
               END IF
            END IF
         ELSE
            LAST_GOOD=I
         END IF
      END DO
C
      END
C
      SUBROUTINE DSA_UNFLAG_I (NELM,LEAVE,DATA,NFLAGGED,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LEAVE
      INTEGER NELM, NFLAGGED
      INTEGER DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      INTEGER FLAG_VALUE                  ! Flag value for this data type
      INTEGER I                           ! Loop index through data elements
      INTEGER J                           ! Loop index through flagged elements
      INTEGER LAST_GOOD                   ! Last non-flagged element number
      INTEGER STATUS                      ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('INT',FLAG_VALUE,STATUS)
C
C     Loop through data, replacing flagged values as we find them
C
      NFLAGGED=0
      LAST_GOOD=0
      DO I=1,NELM
         IF (DATA(I).EQ.FLAG_VALUE) THEN
            FLAGS(I)=1
            NFLAGGED=NFLAGGED+1
            IF (.NOT.LEAVE) THEN
               IF (LAST_GOOD.GT.0) THEN
                  DO J=LAST_GOOD+1,I
                     DATA(J)=DATA(LAST_GOOD)
                  END DO
               ELSE
                  DATA(I)=0
               END IF
            END IF
         ELSE
            LAST_GOOD=I
         END IF
      END DO
C
      END
C
      SUBROUTINE DSA_UNFLAG_B (NELM,LEAVE,DATA,NFLAGGED,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LEAVE
      INTEGER NELM, NFLAGGED
      BYTE DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      BYTE    FLAG_VALUE                  ! Flag value for this data type
      INTEGER I                           ! Loop index through data elements
      INTEGER J                           ! Loop index through flagged elements
      INTEGER LAST_GOOD                   ! Last non-flagged element number
      INTEGER STATUS                      ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('BYTE',FLAG_VALUE,STATUS)
C
C     Loop through data, replacing flagged values as we find them
C
      NFLAGGED=0
      LAST_GOOD=0
      DO I=1,NELM
         IF (DATA(I).EQ.FLAG_VALUE) THEN
            FLAGS(I)=1
            NFLAGGED=NFLAGGED+1
            IF (.NOT.LEAVE) THEN
               IF (LAST_GOOD.GT.0) THEN
                  DO J=LAST_GOOD+1,I
                     DATA(J)=DATA(LAST_GOOD)
                  END DO
               ELSE
                  DATA(I)=0
               END IF
            END IF
         ELSE
            LAST_GOOD=I
         END IF
      END DO
C
      END
C
      SUBROUTINE DSA_UNFLAG_C (NELM,LEAVE,DATA,NFLAGGED,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LEAVE
      INTEGER NELM, NFLAGGED
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
C     Loop through data, replacing flagged values as we find them
C
      NFLAGGED=0
      DO I=1,NELM
         IF (DATA(I).EQ.FLAG_VALUE) THEN
            NFLAGGED=NFLAGGED+1
            FLAGS(I)=1
            IF (.NOT.LEAVE) DATA(I)=ICHAR(' ')
         END IF
      END DO
C
      END
C
      SUBROUTINE DSA_UNFLAG_U (NELM,LEAVE,DATA,NFLAGGED,FLAGS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LEAVE
      INTEGER NELM, NFLAGGED
      INTEGER*2 DATA(NELM)
      BYTE FLAGS(NELM)
C
C     Local variables
C
      INTEGER*2 FLAG_VALUE                ! Flag value for this data type
      INTEGER I                           ! Loop index through data elements
      INTEGER J                           ! Loop index through flagged elements
      INTEGER LAST_GOOD                   ! Last non-flagged element number
      INTEGER STATUS                      ! Status for DSA call
C
C     Get bad value to look for
C
      STATUS=0
      CALL DSA_GET_FLAG_VALUE ('USHORT',FLAG_VALUE,STATUS)
C
C     Loop through data, replacing flagged values as we find them
C
      NFLAGGED=0
      LAST_GOOD=0
      DO I=1,NELM
         IF (DATA(I).EQ.FLAG_VALUE) THEN
            FLAGS(I)=1
            NFLAGGED=NFLAGGED+1
            IF (.NOT.LEAVE) THEN
               IF (LAST_GOOD.GT.0) THEN
                  DO J=LAST_GOOD+1,I
                     DATA(J)=DATA(LAST_GOOD)
                  END DO
               ELSE
                  DATA(I)=0
               END IF
            END IF
         ELSE
            LAST_GOOD=I
         END IF
      END DO
C
      END
