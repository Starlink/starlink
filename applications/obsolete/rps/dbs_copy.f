*+DBS_COPY       Read a record from a file and writes to another record
*- M Ricketts( R.A.L )                     1989 Jan.
*  20 July 1992	M. Duesterhaus	modify for portability
**************************************************************************
      SUBROUTINE DBS_COPY( REF_NO , RECREAD, RECWRITE , IERROR )
      IMPLICIT NONE

*  Calling Arguments
      INTEGER REF_NO 	! In	Reference number of files.
      INTEGER RECREAD	! 	Number of record to read
      INTEGER RECWRITE	! 	Number of record to write to
      INTEGER IERROR	! Out	Error indicator.
 
*  Global Variables
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_iof.inc'
      INCLUDE 'com_dbs_rec.inc'
 
*  Local Variables

      CALL DBS_READ ( REF_NO , RECREAD, IERROR )			! Read record 
      IF (IERROR .NE.0) GOTO 10
 
      CALL DBS_WRITE( REF_NO , RECWRITE, IERROR )

10    CONTINUE
      END									! End.
