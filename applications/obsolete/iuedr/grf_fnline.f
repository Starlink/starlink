      SUBROUTINE grf_FNLINE(STYLE, ISTYLE)

*+
*
*   Name:
*      SUBROUTINE grf_FNLINE
*
*   Description:
*      Find line style in list.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings     01-MAY-82
*         AT4 version.
*      Paul Rees         14-JAN-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees         09-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*
*   Method:
*      The index of STYLE in CMLINR, LINSTY is fetched.
*      If it is not found, a value of zero is returned.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE STYLE(*)       ! line style

*   Export:
      INTEGER ISTYLE      ! style index

*   External references:
      LOGICAL str_SIMLR   ! caseless string equality

*   CMLINR:
      INCLUDE 'CMLINR'

      ISTYLE = 4

      DO WHILE (ISTYLE.GT.0)

         IF (str_SIMLR(STYLE, LINSTY(1, ISTYLE))) GO TO 100

        ISTYLE = ISTYLE - 1
      END DO

 100  CONTINUE

      END
