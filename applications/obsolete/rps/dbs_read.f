*+DBS_READ       Reads record from file into d/b
*-  Author M.D.C.Harris ( R.A.L )                         20th March 1987.
*  20 July 1992	M. Duesterhaus	modify for portability
*	7/29/93	P. Brisco	Explicitly typed "J".
**********************************************************************
      SUBROUTINE DBS_READ( REF_NO , RECN , IERROR )
 
*  Calling Arguments

      INTEGER RECN	! In	Number of record wanted.
     & ,      REF_NO	! 	Reference of data set.
      INTEGER IERROR	! Out	Error indicator.
 
*  Global Variables
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_iof.inc'
      INCLUDE 'com_dbs_rec.inc'

*  Local Variable
      INTEGER I,J,RECTMP		
c------------------------------------------------------------------------------
      RECTMP = ((RECN-1)*NRECORDS(REF_NO))+1
      I = 1
      DO WHILE (I.LT.RECSIZE(REF_NO))
        IF (I+79.GT.RECSIZE(REF_NO)) THEN
          J=RECSIZE(REF_NO)
        ELSE
          J=I+79
        END IF
        READ( LNDB( REF_NO ) ,REC=RECTMP, IOSTAT = IERROR )			! Read in the record.
     &    RECORD(REF_NO)(I:J)			! Getting number of bytes from array.
        I=J+1
        RECTMP=RECTMP	+1
      END DO

      END									! Record read in?
