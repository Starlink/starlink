*+EXPOS_L_RGW - Calculate RGW efficiency factors for a list of locations
      SUBROUTINE EXPOS_L_RGW (ARRU,ARRC,NX,NY,SRA,SDEC,NSOU,EXF,STATUS)
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'CONSTANTS.INC'

* Input
      INTEGER	NSOU			! # source locations
      INTEGER	NX, NY
      REAL	ARRU(NX,NY), ARRC(NX,NY)
      DOUBLE PRECISION	SRA(NSOU), SDEC(NSOU)	! Source locations (degs)

* Output
      REAL	EXF(NSOU)			! Exposure factor
      INTEGER   STATUS

* Local
      INTEGER	N, mx, my
      REAL	EXP
      DOUBLE PRECISION	ELON, ELAT
      REAL	ELOD, ELAD
      real      xscale, yscale
      real 	u_corr, corr

* P McGale May 95
*-

*    Local variables :
*
*   Check status
      IF (STATUS.NE.0) RETURN

      xscale = real(nx)/360.0
      yscale = real(ny)/180.0
      DO N = 1,NSOU
	CALL CEL2EC (SRA(N),SDEC(N),ELON,ELAT)
	ELOD = real(ELON)*RTOD
	ELAD = real(ELAT)*RTOD
        mx=int(mod(int(elod*xscale+0.5)+real(nx), real(nx)))+1
        my=int((elad+90.0)*yscale+0.5)+1
        u_corr=arru(mx,my)
        corr=arrc(mx,my)
	exp = 1.0/exf(n)
	IF (EXP.GT.0. .and. u_corr .gt. 0 .and. corr .gt. 0) THEN
	  EXF(N) = exp*(corr/u_corr)
	  exf(n) = 1./exf(n)
	ELSE
	  EXF(N) = -1.
	ENDIF
      ENDDO

999   IF (STATUS .NE. 0) THEN
	 WRITE(*,*) '   Error in EXPOS_L_RGW'
      END IF

      END

