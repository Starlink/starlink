      SUBROUTINE GRQDEV (DEVICE, L)
*+
*     - - - - - - - -
*       G R Q D E V     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Obtain the name of the current graphics device or file.
*
*   Returned
*      DEVICE     c  Receives the device name of the currently active device.
*      L          i  Number of characters in DEVICE, excluding trailing blanks.
*
*   D.L.Terrett  Starlink  Apr 1991
*+
      IMPLICIT NONE

      CHARACTER*(*) DEVICE
      INTEGER L

      INCLUDE 'grecom.inc'

      INCLUDE 'SAE_PAR'

      INTEGER STATUS

      IF (GRCIDE.LT.1) THEN
          DEVICE = '?'
          L = 1
      ELSE
          STATUS = SAI__OK
          CALL GNS_IDNG(GRWKID(GRCIDE),DEVICE,L,STATUS)
          IF (L.GT.LEN(DEVICE)) L = LEN(DEVICE)
      END IF
      END
