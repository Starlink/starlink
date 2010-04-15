      SUBROUTINE MWDATA(NAXIS1, NAXIS2, D_VM, Q_VM, STATUS)

*+
*
*   Name:
*      SUBROUTINE MWDATA
*
*   Description:
*      Use RDHEAD to get current DATASET in, then copy virtual addresses
*      from CMFILE.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          22-SEP-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NAXIS1     ! axis1 size
      INTEGER NAXIS2     ! axis2 size

*   Export:
      INTEGER*4 D_VM     ! DATA VM
      INTEGER*4 Q_VM     ! QUAL VM
      INTEGER STATUS     ! status return

*   Release existing DATA_VM
      IF (D_VM.GT.0) CALL DLADR(D_VM, STATUS)

*   Release existing QUAL_VM
      IF (Q_VM.GT.0) CALL DLADR(Q_VM, STATUS)

*   Get new DATA_VM
      CALL ALADR('short\\', NAXIS1*NAXIS2, D_VM, STATUS)

      IF (STATUS.NE.0) THEN

         CALL ERROUT('Error: getting DATA VM\\', STATUS)
         RETURN

      END IF

*   Get new QUAL_VM
      CALL ALADR('byte\\', NAXIS1*NAXIS2, Q_VM, STATUS)

      IF (STATUS.NE.0) THEN

         CALL ERROUT('Error: getting QUAL VM\\', STATUS)
         RETURN

      END IF

      END
