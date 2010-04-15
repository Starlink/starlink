      SUBROUTINE WRSPAR(FD, NAXIS1, NAXIS2, MBAD, QUAL, SBAD, LBAD,
     :                  QBAD, STATUS)

*+
*
*   Name:
*      SUBROUTINE WRSPAR
*
*   Description:
*      This writes the QUAL array from disk into the supplied program array.
*      The QUAL elements are stored as a SPARSE array.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          22-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     17-AUG-94     IUEDR Vn. 3.1-2
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER FD                    ! file descriptor
      INTEGER NAXIS1                ! size of axis1
      INTEGER NAXIS2                ! size of axis2
      INTEGER MBAD                  ! number of bad pixels

      BYTE QUAL(NAXIS1, NAXIS2)     ! QUAL array

*   Export:
      INTEGER SBAD(MBAD)            ! s-coordinates
      INTEGER LBAD(MBAD)            ! l-coordinates

      BYTE QBAD(MBAD)               ! data quality values

      INTEGER STATUS                ! status return

*   CMDATA:
      INCLUDE 'CMDATA'

*   Internal:
      INTEGER I                     ! loop index
      INTEGER IL                    ! internal LBAD
      INTEGER IS                    ! internal SBAD
      INTEGER NBAD                  ! number of bad pixels

*   Determine number of bad pixels
      NBAD = 0
      DO IL = LMIN, LMAX
         DO IS = SMIN(IL), SMAX(IL)
            IF (QUAL(IS, IL).NE.0) THEN
               IF (NBAD.GE.MBAD) THEN
                  CALL ERROUT('Error: too many bad pixels\\',
     :                STATUS)
                  RETURN
               END IF

               NBAD = NBAD + 1
               SBAD(NBAD) = IS
               LBAD(NBAD) = IL
               CALL DQ_COPY(1, QUAL(IS, IL), QBAD(NBAD))

            END IF
         END DO
      END DO

*   Write to file
      WRITE (FD, IOSTAT = STATUS) (SBAD(I), I = 1, NBAD)
      IF (STATUS.NE.0) THEN
         CALL ERROUT('Error: writing SBAD\\', STATUS)
         RETURN
      END IF

      WRITE (FD, IOSTAT = STATUS) (LBAD(I), I = 1, NBAD)
      IF (STATUS.NE.0) THEN
         CALL ERROUT('Error: writing LBAD\\', STATUS)
         RETURN
      END IF

      WRITE (FD, IOSTAT = STATUS) (QBAD(I), I = 1, NBAD)
      IF (STATUS.NE.0) THEN
         CALL ERROUT('Error: writing QBAD\\', STATUS)
         RETURN
      END IF
      END
