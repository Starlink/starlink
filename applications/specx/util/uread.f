*  History:
*     20 July 2000 (ajc):
*        Specify record number standardly
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      SUBROUTINE UREAD (LUN, RECORD, IREC, IRECL, IERR)

C   Random access read on unit LUN.
C   IRECL bytes of data read from record IREC (first record = #1)
C   Returned into array RECORD

      BYTE RECORD(*)

*     Print *,'Accessing record #', IREC, '  (', IRECL, ' bytes)'
      IPOS = IREC
      READ (LUN, REC=IPOS, IOSTAT=IERR) (RECORD(I),I=1,IRECL)

      RETURN
      END

*-----------------------------------------------------------------------
