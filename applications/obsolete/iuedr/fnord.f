      SUBROUTINE FNORD( ORDER, IORDER )

*+
*
*   Name:
*      SUBROUTINE FNORD
*
*   Description:
*      ORDER is sought in the list of echelle orders.
*      If it is not found, a value of zero is returned.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          07-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     10-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER ORDER      ! echelle order

*   Export:
      INTEGER IORDER     ! order index

*   Global variables:
      INCLUDE 'CMSAVE'

      IORDER = NORDER
      DO WHILE ( IORDER .GT. 0 )
         IF ( ORDERS(IORDER) .EQ. ORDER ) GO TO 100
         IORDER = IORDER - 1
      END DO
 100  CONTINUE

      END
