      SUBROUTINE GETRIP(ORD, RIPK, RIPA, MAXCS, CS, NCS)

*+
*
*   Name:
*      SUBROUTINE GETRIP
*
*   Description:
*      Get the ripple parameters for an order (context independent).
*
*   History:
*      Jack Giddings      25-JUN-81     IUEDR Vn. 1.0
*      Paul Rees          19-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER ORD         ! echelle order
      INTEGER MAXCS       ! maximum number of polynomial terms

*   Export:
      REAL*8 RIPK           ! K value
      REAL*8 RIPA           ! Alpha
      REAL*8 CS(MAXCS)      ! polynomial terms

      INTEGER NCS         ! number of polynomial terms

*   External references:
      REAL*8 MSC_EVPOLY     ! polynomial evaluation

*   CMHEAD:
      INCLUDE 'CMHEAD'

*   CMRIP:
      INCLUDE 'CMRIP'

*   Local variables:
      INTEGER I           ! loop index
      INTEGER IORDR       ! ripple order index

*.

*   Force default global ripple
      IF (NORIP) CALL DEFRIP

*   Either individual or global
      CALL FNRIP(ORD, IORDR)

      IF (IORDR.GT.0) THEN
         RIPK = RIPKS(IORDR)
         RIPA = RIPAS(IORDR)
         NCS = NRIPCS(IORDR)

         IF (NCS.GT.0) THEN
            DO 20 I = 1, NCS
               CS(I) = RIPCS(I, IORDR)
 20         CONTINUE
         END IF

      ELSE
         RIPK = MSC_EVPOLY(NRIPM, RIPM, DBLE(ORD))
         RIPA = RIPALF
         NCS = 0
      END IF

      END
