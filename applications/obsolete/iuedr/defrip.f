      SUBROUTINE DEFRIP

*+
*
*   Name:
*      SUBROUTINE DEFRIP
*
*   Description:
*     Fill the global ripple calibration, based on Camera etc.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     18-SEP-94     IUEDR Vn. 3.1-8
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   External references:
      LOGICAL STR_SIMLR     ! caseless string equality

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMRIP'

      IF ( NORIP .OR. NRIPM.LT.1 ) THEN
         IF ( STR_SIMLR( 'SWP\\', CAMERA ) ) THEN
            NRIPM = 1
            RIPM(1) = 137725.0

         ELSE IF ( STR_SIMLR( 'LWR\\', CAMERA ) ) THEN
            NRIPM = 1
            RIPM(1) = 231180.0

         ELSE IF ( STR_SIMLR( 'LWP\\', CAMERA ) ) THEN
            NRIPM = 1
            RIPM(1) = 231150.0

         ELSE IF ( STR_SIMLR( 'SWR\\', CAMERA ) ) THEN
            NRIPM = 1
            RIPM(1) = 137725.0

         ELSE
            NRIPM = 1
            RIPM(1) = 1.0
         END IF

         XRLIM(1) = -3.0
         XRLIM(2) = +3.0
         RIPALF = 1.0
         NRIPO = 0
         NORIP = .FALSE.
      END IF
      END
