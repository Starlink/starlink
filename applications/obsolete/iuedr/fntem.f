      SUBROUTINE FNTEM(ORD, IORD)

*+
*
*   Name:
*      SUBROUTINE FNTEM
*
*   Description:
*      Find template slot.
*
*   History:
*      Jack Giddings      31-DEC-81     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      Find order in template table.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER ORD      ! echelle order number

*   Export:
      INTEGER IORD     ! order index in table

*   CMTEM:
      INCLUDE 'CMTEM'

*   Find any existing slot
      IF (NOTEM) THEN

         IORD = 0

      ELSE

         IORD = NTEMO

 50      CONTINUE

         IF (.NOT.(IORD.GT.0)) GO TO 100
         IF (ORD.EQ.TEMORD(IORD)) GO TO 100
         IORD = IORD - 1
         GO TO 50

      END IF

 100  CONTINUE

      END
