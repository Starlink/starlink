      SUBROUTINE DLRIP(ORD)

*+
*
*   Name:
*      SUBROUTINE DLRIP
*
*   Description:
*      Delete order from ripple table.
*
*   History:
*      Jack Giddings      31-DEC-81     IUEDR Vn. 1.0
*      Paul Rees          19-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER ORD      ! echelle order number

*   CMRIP:
      INCLUDE 'CMRIP'

*   Local variables:
      INTEGER IORD     ! order index

*   Find any existing slot
      CALL FNRIP(ORD, IORD)

      IF (IORD.GT.0) THEN

         RIPOS(IORD) = 0

 50      CONTINUE

         IF (NRIPO.GT.0) THEN

            IF (RIPOS(NRIPO).GT.0) GO TO 100
            NRIPO = NRIPO - 1
            GO TO 50

         END IF

      END IF

 100  CONTINUE

      END
