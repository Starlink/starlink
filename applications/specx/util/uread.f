*-----------------------------------------------------------------------

      SUBROUTINE UREAD (LUN, RECORD, IREC, IRECL, IERR)

C   Random access read on unit LUN. 
C   IRECL bytes of data read from record IREC (first record = #1)
C   Returned into array RECORD

      BYTE RECORD(*)

*     Type *,'Accessing record #', IREC, '  (', IRECL, ' bytes)'
      IPOS = IREC
      READ (LUN'IPOS, IOSTAT=IERR) (RECORD(I),I=1,IRECL)

      RETURN
      END

*-----------------------------------------------------------------------
