      SUBROUTINE FNAPER(APR, IAPER)

*+
*
*   Name:
*      SUBROUTINE FNAPER
*
*   Description:
*      Provide aperture number from name.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          13-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      BYTE APR(16)          ! aper

*   Export:
      INTEGER IAPER         ! aper index

*   External references:
      LOGICAL STR_SIMLR     ! caseless string equality

*   CMHEAD:
      INCLUDE 'CMHEAD'

      IF (NOHEAD) THEN
         IAPER = 0
      ELSE
         IAPER = NAPER

 50      CONTINUE
         IF (IAPER.GT.0) THEN
            IF (STR_SIMLR(APR, APERS(1, IAPER))) GO TO 100
            IAPER = IAPER - 1
            GO TO 50
         END IF
      END IF

 100  CONTINUE
      END
