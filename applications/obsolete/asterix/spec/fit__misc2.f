*+  FIT_INSGET_CCHECK - Checks response matrix channels against dataset channels
      SUBROUTINE FIT_INSGET_CCHECK(NDAT,DATCHAN,NRES,RESCHAN,STATUS)
*    Description :
*     Checks that channel numbers in response matrix match those in data.
*     If not then an error message is queued.
*    History :
*     30 Mar 87: Original (DATGET_$CCHECK) (TJP)
*      3 Mar 88: Renamed (TJP)
*    Type Definitions :
	IMPLICIT NONE
*    Global constants :
	INCLUDE 'SAE_PAR'
*    Import :
	INTEGER NDAT			! No of data channels
	REAL DATCHAN(NDAT)		! Data channel values
	INTEGER NRES			! No of response channels
	REAL RESCHAN(NRES)		! Response channel values
*    Import-Export :
*    Export :
*    Status :
	INTEGER STATUS
*    Local constants :
*    Local variables :
	LOGICAL BAD			! Mismatch detected
	INTEGER I
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Check channels
	BAD=.FALSE.
	IF(NDAT.EQ.NRES)THEN
	  DO I=1,NDAT
D	    print *,'dat,res: ',datchan(i),reschan(i)
	    IF(ABS(DATCHAN(I)-RESCHAN(I)) .GT.
     :      ABS(1.E-5*DATCHAN(I)) ) THEN
	      BAD=.TRUE.
	      GO TO 8000
	    ENDIF
	  ENDDO
	ELSE
	  BAD=.TRUE.
          I = 0
	ENDIF

* Error message
 8000	IF(BAD)THEN
	  STATUS=SAI__ERROR
          IF ( I .EQ. 0 ) THEN
	  CALL ERR_REP('BAD_CHAN','Mismatch between response and data'//
     :  ' channels, number of bins do not match ', STATUS )
          ELSE
          CALL MSG_SETI( 'CHAN', I )
          CALL MSG_SETR( 'BIN', DATCHAN(I) )
          CALL MSG_SETR( 'BND', RESCHAN(I) )

	  CALL ERR_REP('BAD_CHAN','Mismatch between response and data'//
     :    ' channels at bin ^CHAN, energy axis value = ^BIN vs. '/
     :     /'response bound of ^BND',STATUS)
          END IF
	ENDIF

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR',
     :  'from FIT_INSGET_CCHECK',STATUS)
	END
