      SUBROUTINE RDQUAL(FD, NAXIS1, NAXIS2, QUAL, STATUS)

*+
*
*   Name:
*      SUBROUTINE RDQUAL
*
*   Description:
*      This reads the QUAL array from disk into the supplied program array.
*      The QUAL elements are stored as a SPARSE array.
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
      INTEGER FD                    ! file descriptor
      INTEGER NAXIS1                ! size of axis1
      INTEGER NAXIS2                ! size of axis2

*   Export:
      BYTE QUAL(NAXIS1, NAXIS2)     ! QUAL array

      INTEGER STATUS                ! status return

*   External references:
      LOGICAL STR_SIMLR             ! caseless string equality

*   CMDATA:
      INCLUDE 'CMDATA'

*   Local variables:
      BYTE VNAME(16)                ! temporary name
      BYTE VTYPE(16)                ! temporary type

      LOGICAL NOQUAL                ! whether defined

      INTEGER SV_VM                 ! sparse data quality s-coordinates
      INTEGER LV_VM                 ! sparse data quality l-coordinates
      INTEGER QV_VM                 ! sparse data quality values
      INTEGER ISTAT                 ! internal status
      INTEGER NBAD                  ! number of bad pixels
      INTEGER NPIXEL                ! number of pixels

*   Zero output array first
      NPIXEL = NAXIS1*NAXIS2
      CALL DQ_ZERO(NPIXEL, QUAL)

*   DATA_QUALITY
      CALL RDPART(FD, VNAME, NOQUAL, VTYPE, STATUS)

      IF (STATUS.NE.0) THEN

         RETURN

      ELSE IF (.NOT.STR_SIMLR('DATA_QUALITY\\', VNAME)) THEN

         CALL ERRSTR('Error: \\')
         CALL ERRSTR(VNAME)
         CALL ERRSTR(' found instead of \\')
         CALL ERROUT('DATA_QUALITY\\', STATUS)
         RETURN

      ELSE IF (.NOT.NOQUAL) THEN

         IF (STR_SIMLR('SPARSE\\', VTYPE)) THEN

            READ (FD, IOSTAT = STATUS) NBAD

            IF (STATUS.NE.0) THEN

               CALL ERROUT('Error: reading NBAD\\', STATUS)
               RETURN

            ELSE IF (NBAD.GT.0) THEN

               CALL MEQUAL(NBAD, SV_VM, LV_VM, QV_VM, STATUS)

               IF (STATUS.NE.0) THEN

                  CALL ERROUT('Error: getting data quality VM\\',
     :                        STATUS)
                  RETURN

               END IF

               CALL RDSPAR(FD, NAXIS1, NAXIS2, NBAD, QUAL, %VAL(SV_VM),
     :                     %VAL(LV_VM), %VAL(QV_VM), STATUS)
               CALL FRQUAL(SV_VM, LV_VM, QV_VM, ISTAT)

               IF (STATUS.NE.0) THEN

                  CALL ERROUT('Error: reading data quality\\',
     :                        STATUS)
                  RETURN

               ELSE IF (ISTAT.NE.0) THEN

                  CALL ERROUT('Error: freeing data quality VM\\',
     :                        STATUS)
                  RETURN

               END IF

            END IF

         ELSE

            CALL ERROUT('Error: data quality has illegal type\\',
     :                  STATUS)
            RETURN

         END IF

      END IF

      END
