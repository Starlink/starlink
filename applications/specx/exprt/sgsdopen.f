C-------------------------------------------------------------------------

      SUBROUTINE SPECX_GSD_OPEN (ISCAN, IERR)

C   Routine to open a GSD file containing a generalized observation

C History:
C    8-May-2000 (ajc):
C      Port to Linux
C      Replace TYPE with PRINT
C      Unused IEXIST, IOPEN
C-
      IMPLICIT  NONE

      INTEGER   ADAM__OK
      PARAMETER (ADAM__OK=0)

      INTEGER   ISCAN
      INTEGER   IERR
      INTEGER   STATUS
      INTEGER   GEN_ILEN
      CHARACTER OLDFILE*80
      CHARACTER *80 STOREFILE

      INTEGER*4 NEW_IFD
      INTEGER*4 OLD_IFD
      INTEGER*4 OLD_NITEM
      REAL*4    OLD_VERSION
      CHARACTER OLD_LABEL*40


      INCLUDE  'FLAGCOMM'
      INCLUDE  'GSD_FILE.INC'

      SAVE OLDFILE

C  Open the GSD file

      IF (IERR.NE.0) RETURN
      STATUS = ADAM__OK

      STOREFILE = OLDFILE

      WRITE (FILENAME,'(A,I4.4)')
     &           GSDNAME(:GEN_ILEN(GSDNAME)),ISCAN

      OLDFILE = FILENAME

      IF (FILENAME.eq.STOREFILE .and. GSD_OPEN) THEN
        PRINT *, 'GSD file already open!'
      ELSE

         IF (GSD_OPEN) THEN
*     Store the existing file information
            OLD_IFD      = IFD
            OLD_VERSION  = VERSION
            OLD_LABEL    = LABEL
            OLD_NITEM    = NITEM
         END IF

         CALL GSD_OPEN_READ (FILENAME,IFD,VERSION,LABEL,NITEM,STATUS)
         IF (STATUS.NE.ADAM__OK) THEN
            WRITE (ILOUT,*) 'Error opening GSD file - status',status
            IERR = 10
*     Reset OLD GSD info
            IF (GSD_OPEN) THEN
               FILENAME= STOREFILE
               OLDFILE = STOREFILE
               IFD     = OLD_IFD
               VERSION = OLD_VERSION
               LABEL   = OLD_LABEL
               NITEM   = OLD_NITEM
            ELSE
               OLDFILE = " "
            END IF
        ELSE

* Close the OLD file
           IF (GSD_OPEN) THEN
              NEW_IFD = IFD
              IFD = OLD_IFD
              CALL SPECX_GSD_CLOSE(IERR)
              IFD = NEW_IFD
           END IF

           GSD_OPEN = .TRUE.
        END IF
      END IF

C  Standard return

      RETURN
      END

C-------------------------------------------------------------------------

