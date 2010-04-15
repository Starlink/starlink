      SUBROUTINE FRQUAL(S_VM, L_VM, Q_VM, STATUS)


*+
*
*   Name:
*      SUBROUTINE FRQUAL
*
*   Description:
*      Release VM used for sparse data quality.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          22-SEP-88     IUEDR Vn. 2.0
*
*   Method:
*      Release in reverse order to which they were created so that the
*      VM manager has an easy time.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER S_VM       ! address of DATA array
      INTEGER L_VM       ! address of QUAL array
      INTEGER Q_VM       ! address of R array

*   Export:
      INTEGER STATUS     ! status return

      IF (Q_VM.GT.0) CALL DLADR(Q_VM, STATUS)
      IF (L_VM.GT.0) CALL DLADR(L_VM, STATUS)
      IF (S_VM.GT.0) CALL DLADR(S_VM, STATUS)

      END
