*+EXPOS_L_EXP - Calculate exposure factors for a list of locations
      SUBROUTINE EXPOS_L_EXP (ARR,NX,NY,SRA,SDEC,NSOU,EXF,STATUS)
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'CONSTANTS.INC'

* Input
      INTEGER	NSOU			! # source locations
      INTEGER	NX, NY
      REAL	ARR(NX,NY)
      DOUBLE PRECISION	SRA(NSOU), SDEC(NSOU)	! Source locations (degs)

* Output
      REAL	EXF(NSOU)			! Exposure factor
      INTEGER   STATUS

* Local
      INTEGER	N
      REAL	EXP
      DOUBLE PRECISION	ELON, ELAT
      REAL	ELOD, ELAD
* M Denby Sep-89
* P McGale Sept-94 , UNIX changes
*-

*    Local variables :
*
*   Check status
      IF (STATUS.NE.0) RETURN

      DO N = 1,NSOU
	CALL CEL2EC (SRA(N),SDEC(N),ELON,ELAT)
	ELOD = ELON*RTOD
	ELAD = ELAT*RTOD
	CALL GRIDINT (ELOD,ELAD,ARR,NX,NY,EXP)
	IF (EXP.GT.0.) THEN
	  EXF(N) = 1/EXP
	ELSE
	  EXF(N) = -1.
	ENDIF
      ENDDO

999   IF (STATUS .NE. 0) THEN
	 WRITE(*,*) '   Error in EXPOS_L_EXP'
      END IF

      END

