      SUBROUTINE RDSPAR(FD, NAXIS1, NAXIS2, NBAD, QUAL, SBAD, LBAD,
     :                  QBAD, STATUS)

*+
*
*   Name:
*      SUBROUTINE RDSPAR
*
*   Description:
*      This reads the QUAL array from disk into the supplied program array.
*      The QUAL elements are stored as a SPARSE array.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          22-SEP-88     IUEDR Vn. 2.0
*
*    Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER FD                    ! file descriptor
      INTEGER NAXIS1                ! size of axis1
      INTEGER NAXIS2                ! size of axis2
      INTEGER NBAD                  ! number of bad pixels

*   Import-Export:
      BYTE QUAL(NAXIS1, NAXIS2)     ! QUAL array

*   Export:
      BYTE QBAD(NBAD)               ! data quality values

      INTEGER SBAD(NBAD)            ! s-coordinates
      INTEGER LBAD(NBAD)            ! l-coordinates
      INTEGER STATUS                ! status return

*   CMDATA:
      INCLUDE 'CMDATA'

*   Internal:
      INTEGER I                     ! loop index
      INTEGER IBAD                  ! loop index
      INTEGER IL                    ! temporary LBAD
      INTEGER IS                    ! temporary SBAD

      READ (FD, IOSTAT = STATUS) (SBAD(I), I = 1, NBAD)

      IF (STATUS.NE.0) THEN

         CALL ERROUT('Error: reading SBAD\\', STATUS)
         RETURN

      END IF

      READ (FD, IOSTAT = STATUS) (LBAD(I), I = 1, NBAD)

      IF (STATUS.NE.0) THEN

         CALL ERROUT('Error: reading LBAD\\', STATUS)
         RETURN

      END IF

      READ (FD, IOSTAT = STATUS) (QBAD(I), I = 1, NBAD)

      IF (STATUS.NE.0) THEN

         CALL ERROUT('Error: reading QBAD\\', STATUS)
         RETURN

      END IF

      DO 100 IBAD = 1, NBAD

         IS = SBAD(IBAD)
         IL = LBAD(IBAD)

         IF (IS.GE.1 .AND. IS.LE.NAXIS1) THEN

            IF (IL.GE.1 .AND. IL.LE.NAXIS2)
     :           CALL DQ_COPY(1, QBAD(IBAD), QUAL(IS, IL))

         END IF

 100  CONTINUE

      END
