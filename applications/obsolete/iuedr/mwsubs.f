      SUBROUTINE MWSUBS(NSUB, D_VM, Q_VM, R_VM, W_VM, STATUS)

*+
*
*   Name:
*      SUBROUTINE MWSUBS
*
*   Description:
*      Provide VM for subset.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      Get VM for the components of the image subset used for extraction.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NSUB       ! number of subset pixels

*   Export:
      INTEGER D_VM       ! address of DATA array
      INTEGER Q_VM       ! address of QUAL array
      INTEGER R_VM       ! address of R array
      INTEGER STATUS     ! status return
      INTEGER W_VM       ! address of W array

*   Release existing D_VM
      IF (D_VM.GT.0) CALL DLADR(D_VM, STATUS)

*   Get new D_VM
      CALL ALADR('short\\', NSUB, D_VM, STATUS)

      IF (STATUS.NE.0) THEN

         CALL ERROUT('Error: getting VM\\', STATUS)
         RETURN

      END IF

*   Release existing Q_VM
      IF (Q_VM.GT.0) CALL DLADR(Q_VM, STATUS)

*   Get new Q_VM
      CALL ALADR('byte\\', NSUB, Q_VM, STATUS)

      IF (STATUS.NE.0) THEN

         CALL ERROUT('Error: getting VM\\', STATUS)
         RETURN

      END IF

*   Release existing R_VM
      IF (R_VM.GT.0) CALL DLADR(R_VM, STATUS)

*   Get new R_VM
      CALL ALADR('float\\', NSUB, R_VM, STATUS)

      IF (STATUS.NE.0) THEN

         CALL ERROUT('Error: getting VM\\', STATUS)
         RETURN

      END IF

*   Release existing W_VM
      IF (W_VM.GT.0) CALL DLADR(W_VM, STATUS)

*   Get new W_VM
      CALL ALADR('float\\', NSUB, W_VM, STATUS)

      IF (STATUS.NE.0) THEN

         CALL ERROUT('Error: getting VM\\', STATUS)
         RETURN

      END IF

      END
