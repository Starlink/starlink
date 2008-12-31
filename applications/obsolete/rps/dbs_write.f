*+DBS_WRITE      Writes a record to a file
*- M.D.C.Harris ( R.A.L )                     26th March 1987.
*	9 apr 1992	M. Duesterhaus (GSFC)	port to unix
*	7/29/93		P. Brisco		Explicitly declared rectmp
*****************************************************************
      SUBROUTINE DBS_WRITE( REF_NO , RECN , IERROR )

*  Calling Arguments
      INTEGER RECN	! In	Number of record.
     & ,      REF_NO 	! 	Reference number of files.
      INTEGER IERROR	! Out	Error indicator.
      REAL rectmp
 
*  Global Variables
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_iof.inc'
      INCLUDE 'com_dbs_rec.inc'
 
*  Local Variables
      INTEGER I,J
      CHARACTER*80 BLANKSP

      I=1
      DO WHILE (I.LE.80)
        BLANKSP(I:I) = ' '
        I=I+1
      END DO
      RECTMP = ((RECN-1)*NRECORDS(REF_NO))+1
      I=1
      DO WHILE (I.LT.RECSIZE(REF_NO))
        IF (I+79.GT.RECSIZE(REF_NO)) THEN
          J=RECSIZE(REF_NO)
          WRITE ( LNDB(REF_NO), REC=RECTMP, IOSTAT = IERROR )
     &      RECORD( REF_NO )(I:J)//BLANKSP(1:I+79-J)
        ELSE
          J=I+79
          WRITE( LNDB( REF_NO ) ,REC=RECTMP, IOSTAT = IERROR )			! Write record to file -
     &     RECORD( REF_NO ) ( I:J) 			! for correct number of bytes.
        END IF
        I=J+1
        RECTMP=RECTMP+1
      END DO

      END									! End.
