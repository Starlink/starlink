*  History:
*     22 Nov 1993 (hme):
*        Replace LIB$FREE_LUN with FIO_PUNIT.
*     06 Dec 1993 (hme):
*        Do not touch the actual file (no INQUIRE or CLOSE).
*        With the expanded include file FILES, we need to INCLUDE
*        DAT_PAR as well.
C-----------------------------------------------------------------------

      SUBROUTINE LSTFIL

      INCLUDE 'DAT_PAR'
      INCLUDE 'FILES'

      DO IFILE = 1, MAX_DFILES
         IF ( FILELUNS(IFILE) .NE. 0 ) WRITE( 6, 1000 )
     :         IFILE, FILNAMS(IFILE), ACCESS(IFILE), FILELUNS(IFILE)
      END DO

 1000 FORMAT (1X, I1, 2X, A40, 2X, A2, 2X, I3)

      END


