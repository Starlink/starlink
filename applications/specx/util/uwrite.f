*  History:
*     20 July 2000 (ajc):
*        Specify record number standardly
*-----------------------------------------------------------------------

      SUBROUTINE UWRITE (LUN,RECORD,IREC,IRECL,IERR)

C   Random access write on unit LUN.
C   IRECL bytes of data written to record IREC (first record = #1)
C   from array IADD.

      BYTE RECORD(*)

      IPOS = IREC
      WRITE(LUN, REC=IPOS, IOSTAT=IERR) (RECORD(I),I=1,IRECL)

      RETURN
      END

*-----------------------------------------------------------------------

