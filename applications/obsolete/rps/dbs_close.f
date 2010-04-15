*+DBS_CLOSE       Closes file and deletes either original or the copy
*-	DATE		AUTHOR			DESCRIPTION
*-	17 Feb 1987	MDC Harris (RAL)	original
*-	8  Apr 1992	M. Duesterhaus (GSFC)	remove VAX RTL calls
**************************************************************************

      SUBROUTINE DBS_CLOSE( REF_NO , EXIT )

*  Calling Arguments
      CHARACTER*(*) EXIT	! Indicates if new file to be kept.
      INTEGER       REF_NO	! Reference number of data set.

*  Global Variables
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_iof.inc'		! Includes ARRAY

*  Local Variables
      CHARACTER*128 FILENAME	! Actual file name.
      CHARACTER*128 TEMPFILENAME
      INTEGER       LUNDB	! Logical unit number.
      INTEGER       FL
      INTEGER       NCHAR	! string length

*  Functions, Subroutines
      CHARACTER*128 DBS_INFOC		! Gets character information on the file.
      INTEGER       MDH_ENDWORD
      INTEGER       DBS_INFOI		! Gets integer information on the file.



      LUNDB = DBS_INFOI( REF_NO , 1 , 'UNIT NUMBER' )				! Get logical unit number.

      IF ( EXIT .EQ. 'R' ) THEN							! If read only then.

        CLOSE( LUNDB )								!  Close the file.

      ELSE IF ( EXIT .EQ. 'E' ) THEN						! Else if not quit then.

        FILENAME = DBS_INFOC( REF_NO , 1 , 'FILENAME' )				!  Get the backup file name.

        IF ( FILENAME .NE. ' ' ) THEN
	  NCHAR = MDH_ENDWORD(FILENAME)
          TEMPFILENAME = FILENAME(1:NCHAR)//'2'
	  CALL DELFILE(TEMPFILENAME(1:NCHAR+1))			!  If backup exists get rid of it.
	ENDIF
        CLOSE( LUNDB )								!  Close the file.

      ELSE IF ( EXIT .EQ. 'Q' ) THEN						! Else if quit then.

        CLOSE ( LUNDB , STATUS = 'DELETE' )					!  Remove altered file and keep the saved one.
        FILENAME = DBS_INFOC( REF_NO , 1 , 'FILENAME' )				!  Get the backup file name.
        FL = MDH_ENDWORD(FILENAME)
        TEMPFILENAME = FILENAME(1:FL)//'2'
        CALL RENAMEFILE(TEMPFILENAME(1:FL+1), FILENAME(1:FL) )

      END IF									! End if.

      CALL FRELUN( LUNDB )							! Free the logical unit number.
      ARRAY( REF_NO ) = 1							! Indicate no file opened for this reference number.

      END									! File is closed.
