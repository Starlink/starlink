C History:
C     8-May-2000 (ajc):
C       Port to Linux
C       Replace 'TYPE *' with 'PRINT *'
C-----------------------------------------------------------------

      SUBROUTINE INDEX_GSD (IERR)

C  Routine to produce summary listing of GSD Scan files

      IMPLICIT   NONE

      INCLUDE    'GSD_FILE.INC'

      INTEGER*4  IERR

      INTEGER*4  I
      INTEGER*4  ISTAT
      INTEGER*4  SCANS(2)

      SCANS(1) = 1
      SCANS(2) = 1

      CALL GEN_GETI4A ('GSD scan numbers?', SCANS, 2, ' ', SCANS, ISTAT)

      IERR = 0
      CALL PUSH

      DO I = SCANS(1), MAX(SCANS(1),SCANS(2))
        CALL SPECX_GSD_OPEN   (I, IERR)
        IF (IERR .EQ. 0) THEN
          IF ((VERSION-3.9999) .LT. 0.01) THEN
            CALL SPECX_GSD_V4_HEADER (IERR)
          ELSE IF ((VERSION-4.9999) .LT. 0.01) THEN
            CALL SPECX_GSD_V5_HEADER (IERR)
          ELSE
            PRINT *,
     &        'File header version = ', VERSION, ' (read as V5.0)'
            CALL SPECX_GSD_V5_HEADER (IERR)
          END IF

          IF (IERR.NE.0) THEN
            PRINT *, ' -- index_gsd -- Failed to read GSD file header'
            PRINT *, '    Scan number:  ', I
            PRINT *, '    Error number: ', IERR
          ELSE
            CALL SPECX_GSD_LIST   (IERR)
          END IF

          CALL SPECX_GSD_CLOSE  (IERR)
          IF (IERR.NE.0) GO TO 99
        ELSE
          PRINT *, 'Error opening GSD scan ', I
          IERR = 0
        END IF
      END DO

   99 CALL POP

      RETURN
      END

C-------------------------------------------------------------------------
