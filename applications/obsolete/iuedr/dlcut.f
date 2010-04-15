      SUBROUTINE DLCUT(ORD)

*+
*
*   Name:
*      SUBROUTINE DLCUT
*
*   Description:
*      Delete order from cutoff table.
*
*   History:
*      Jack Giddings      31-DEC-81     IUEDR Vn. 1.0
*      Paul Rees          13-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER ORD      ! echelle order number

*   CMCUT:
      INCLUDE 'CMCUT'

*   Local variables:
      INTEGER IORD     ! order index

*   Find any existing slot
      CALL FNCUT(ORD, IORD)

      IF (IORD.GT.0) THEN

         CUTORD(IORD) = 0

 50      CONTINUE

         IF (NCUT.GT.0) THEN

            IF (CUTORD(NCUT).GT.0) GO TO 100
            NCUT = NCUT - 1
            GO TO 50

         END IF

      END IF

 100  CONTINUE

      END
