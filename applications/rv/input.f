      SUBROUTINE INPUT (LUIN, LUECHO, INREC, J)
*+
*
*  INPUT:  subroutine of RV utility which reads and echoes input
*          records.  Input records are returned with TABs
*          translated to spaces and with lower case converted to
*          upper case.  Special records END and ? are recognised.
*
*  Given:
*     LUIN      i      I/O unit for input
*     LUECHO    i      I/O unit for echo of input
*
*  Returned:
*     INREC     c*(*)  record with TABs and lower case eliminated
*     J         i      status:  -1 = END record, or EOF
*                                  0 = record available
*                                 +1 = ? record
*
*  P.T.Wallace   Starlink   9 June 1992
*-

      IMPLICIT NONE

      INTEGER LUIN,LUECHO
      CHARACTER*(*) INREC
      INTEGER J

      CHARACTER INBUF*80



*  Read and echo records
      J=-1
      READ (LUIN,'(A)',END=900) INBUF
      WRITE (LUECHO,'(1X,A)') INBUF

*  Deal with TABs and lower case, and copy to output array
      CALL TRAN(INBUF,INREC)

*  Test for END and ?
      IF (INREC(:3).EQ.'?') THEN
         J=1
      ELSE IF (INREC(:3).NE.'END') THEN
         J=0
      END IF
      GO TO 999

*  End of file
 900  CONTINUE
      WRITE (LUECHO,'(1X,''<EOF>'')')

*  Exit
 999  CONTINUE

      END
