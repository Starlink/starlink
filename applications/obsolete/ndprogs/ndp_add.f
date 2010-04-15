      SUBROUTINE NDP_ADD(NELM,BADPIX,ARRAY1,ARRAY2,ARRAY3)
C+
C
C   -------------
C   N D P _ A D D
C   -------------
C
C   Description
C   -----------
C   Adds two REAL arrays together. If the bad data flag is set, a magic value
C   pixel is output when either input pixel has the magic value.
C
C
C   Parameters
C   ----------
C   NELM    (> integer). Number of array elements.
C   BADPIX  (> logical). Bad data flag.
C   ARRAY1  (> real array). First input array.
C   ARRAY2  (> real array). Second input array.
C   ARRAY3  (< real array). Output array.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   None.
C
C
C   INCLUDE statements
C   ------------------
C   INCLUDE 'MAGIC_VALUES'
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   END DO / IMPLICIT NONE / INCLUDE / Names > 6 characters
C
C
C   VAX-specific statements
C   -----------------------
C   None.
C
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C
C
C   History
C   -------
C   01-FEB-1989   - Original program
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      LOGICAL BADPIX
      REAL    ARRAY1(NELM),ARRAY2(NELM),ARRAY3(NELM)
C
C     Local variables
C
      INTEGER I
C
      INCLUDE 'MAGIC_VALUES'
C
      IF(.NOT.BADPIX)THEN
        DO I=1,NELM
          ARRAY3(I)=ARRAY1(I)+ARRAY2(I)
        END DO
      ELSE
        DO I=1,NELM
          IF(ARRAY1(I).GT.MAGIC_FLOAT .AND.
     &       ARRAY2(I).GT.MAGIC_FLOAT)THEN
            ARRAY3(I)=ARRAY1(I)+ARRAY2(I)
          ELSE
            ARRAY3(I)=MAGIC_FLOAT
          END IF
        END DO
      END IF
C
      END
