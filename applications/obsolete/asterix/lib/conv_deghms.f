*+  CONV_DEGDMS - Converts decimal degrees to hours,minutes, seconds
	SUBROUTINE CONV_DEGHMS(DEGS, HMS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      REAL DEGS		      ! angle in degrees.
*    Export :
      CHARACTER*(*) HMS	      ! character string (>=10 chars normally).
*    Local constants :
*    Local variables :
        REAL HOURS,SECS,RSECS
        INTEGER NSECS,ISECS,DSECS
        INTEGER MINS,IMINS
        INTEGER IHOUR
        INTEGER L,K
*
*-
        L=LEN(HMS)
        HOURS = MOD(ABS(DEGS)/15.0,24.0)
        IHOUR = INT(HOURS)
        SECS  = HOURS*3600.0
	NSECS = INT(SECS)
        RSECS = MOD(SECS,60.0)
	ISECS = MOD(NSECS, 60)
        DSECS = NINT((RSECS-REAL(ISECS))*10.0)
	MINS  = NSECS / 60
	IMINS = MOD(MINS, 60)
        IF (DSECS.EQ.10) THEN
          DSECS=0
          ISECS=ISECS+1
        ENDIF
        IF (ISECS.EQ.60) THEN
          ISECS=0
          IMINS=IMINS+1
        ENDIF
        IF (IMINS.EQ.60) THEN
          IMINS=0
          IHOUR=IHOUR+1
        ENDIF

        IF (L.GE.10) THEN
          WRITE(HMS,'(I2.2,A1,I2.2,A1,I2.2,A1,I1.1)',IOSTAT=K)
     :                   IHOUR,'h',IMINS,'m',ISECS,'.',DSECS
        ELSE
          IF (DSECS.GE.5) THEN
            ISECS=ISECS+1
          ENDIF
          IF (ISECS.EQ.60) THEN
            ISECS=0
            IMINS=IMINS+1
          ENDIF
          IF (IMINS.EQ.60) THEN
            IMINS=0
            IHOUR=IHOUR+1
          ENDIF
          WRITE(HMS,'(I2.2,A1,I2.2,A1,I2.2)',IOSTAT=K)
     :                   IHOUR,'h',IMINS,'m',ISECS
        ENDIF

	END
