      SUBROUTINE FRSUBS(D_VM, Q_VM, R_VM, W_VM, STATUS)

*+
*
*   Name:
*      SUBROUTINE FRSUBS
*
*   Description:
*      Release VM for subset - release VM for the components of the image
*      subset used for extraction.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      Release in reverse order to which they were created so that the
*      VM manager has an easy time.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER D_VM       ! address of DATA array
      INTEGER Q_VM       ! address of QUAL array
      INTEGER R_VM       ! address of R array
      INTEGER W_VM       ! address of W array

*   Export:
      INTEGER STATUS     ! status return

      IF (W_VM.GT.0) CALL DLADR(W_VM, STATUS)
      IF (R_VM.GT.0) CALL DLADR(R_VM, STATUS)
      IF (Q_VM.GT.0) CALL DLADR(Q_VM, STATUS)
      IF (D_VM.GT.0) CALL DLADR(D_VM, STATUS)

      END
