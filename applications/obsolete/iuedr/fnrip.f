      SUBROUTINE FNRIP(ORD, IORD)

*+
*
*   Name:
*      SUBROUTINE FNRIP
*
*   Description:
*      Find order in ripple table.
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

*   Export:
      INTEGER IORD     ! order index in table

*   CMRIP:
      INCLUDE 'CMRIP'

*   Find any existing slot
      IF (NORIP) THEN

         IORD = 0

      ELSE

         IORD = NRIPO

 50      CONTINUE

         IF (IORD.GT.0) THEN

            IF (ORD.EQ.RIPOS(IORD)) GO TO 100
            IORD = IORD - 1
            GO TO 50

         END IF

      END IF

 100  CONTINUE

      END
