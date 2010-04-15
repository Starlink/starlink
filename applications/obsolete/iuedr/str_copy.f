      SUBROUTINE str_COPY( STR1, C1, C2, START, SIZE, STR2 )
*+
*   Name:
*      SUBROUTINE str_COPY
*
*   Description:
*      Copy part of one string into another.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      25-AUG-81
*         AT4 version.
*      Paul Rees          31-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          14-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*      If the string is extended by the plant operation, then it is
*      terminated correctly.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER ARB         ! arbitrary string length
      INTEGER SZEOS       ! size of string terminator

      PARAMETER (ARB=100, SZEOS=1)

*   Import:
      BYTE STR1(ARB)      ! string to be copied from

      INTEGER C1          ! start character in str1
      INTEGER C2          ! end character in str1
      INTEGER START       ! start character in str2
      INTEGER SIZE        ! maximum size of str2

*   Import/Export:
      BYTE STR2(SIZE)     ! string to be changed

*   External references:
      INTEGER str_LEN     ! string length

*   Local variables:
      INTEGER I1          ! index in str1
      INTEGER I2          ! index in str2
      INTEGER LEN1        ! length of str1
      INTEGER LEN2        ! length of str2

*.

      LEN1 = MIN( str_LEN(STR1), C2 )
      LEN2 = str_LEN( STR2 )
      I1 = MAX( C1, 1 )
      I2 = MAX( START, 1 )

      DO WHILE ( I2.LE.SIZE-SZEOS .AND. I1.LE.LEN1 )
         STR2( I2 ) = STR1( I1 )
         I1 = I1 + 1
         I2 = I2 + 1
      END DO

      IF ( I2-1 .GT. LEN2 ) THEN
         CALL str_TERM( I2-1, SIZE, STR2 )
      END IF

      END
