*  History:
*     31 Jan 1994 (hme):
*        Disuse <> in formats.
C-----------------------------------------------------------------------

      SUBROUTINE STACKSTAT (LSCAN, TITLE)

C   routine to print current status of X-register on terminal

      INTEGER   LSCAN
      CHARACTER TITLE*(*)

      INCLUDE   'FLAGCOMM'

      INTEGER   GEN_ILEN

C  Ok, go..

      IF (LSCAN .GE. 0) THEN
        ILT = GEN_ILEN (TITLE)
        WRITE (ILOUT,'('' X-register now contains scan '',I4.1,'': '',
     &         A)') LSCAN, TITLE
      ELSE
        WRITE (ILOUT,*)'X-register is empty'
      END IF

      RETURN
      END

C-----------------------------------------------------------------------
