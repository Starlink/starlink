*+  SPEC_LG - Computes flux from a Gaussian spectral line
      SUBROUTINE SPEC_LG(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*
*    Description :
*
*     Returns flux in each energy channel delimited by bounds from a Gaussian
*     spectral line model:
*		A*exp[-(E-EL)**2/(2*W**2)]/W*SQRT(2*PI)  photons/(cm**2*s*keV)
*     where
*	A = PARAM(1)
*	EL = PARAM(2)
*	W = PARAM(3)
*
*    Method :
*
*     Integral under Gaussian is evaluated using NAG routine S15ABF. If W<=0
*     then a delta function line is adopted.
*     The leading constant A is the integrated photons/(cm**2*s) in the line.
*     Note that if EL is not >>W then some of the line flux will be lost to
*     negative energies.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*
*    History :
*
*     13 Sep 85 : Original (TJP)
*     27 Apr 88 : Lower and upper energy bounds passed in separate arrays (TJP)
*     13 Jan 93 : Converted to D.P. NAG routine (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
	INTEGER NEN			! No of energy channels
	REAL ELBOUND(NEN)		! Lower bin bounds (keV)
	REAL EUBOUND(NEN)		! Upper bin bounds (keV)
	REAL PARAM(3)			! Model parameters
*
*    Export :
*
	REAL FLUX(NEN)			! Photon flux (phot/cm**2/s)
*
*    Status :
*
	INTEGER STATUS
*
*    Function declarations :
*
	DOUBLE PRECISION S15ABF
*
*    Local constants :
*
	REAL SIGMAX			! Max. sigma deviation from line centre
	PARAMETER(SIGMAX=5.0)		! at which flux will be computed
*
*    Local variables :
*
	INTEGER I
	INTEGER IFAIL			! NAG fail flag
	REAL EL				! Line energy
	REAL W				! Line width (sigma)
	DOUBLE PRECISION Z		! Unit normal variate
	DOUBLE PRECISION CUM1,CUM2	! Cumulative unit normal distribution
					! at bottom & top of channel
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Check for spot value
	IF(ELBOUND(1).EQ.EUBOUND(1))THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('SPOT','Spot values not supported by SPEC_ routines',
     :    STATUS)
	  GO TO 9000
	ENDIF

* Parameters
	EL=PARAM(2)
	W=PARAM(3)

* Zero FLUX array
	DO I=1,NEN
	  FLUX(I)=0.0
	ENDDO

* Delta function line
	IF(W.LE.0.0)THEN
	  IF(ELBOUND(1).GT.EL) GO TO 9000
	  DO I=1,NEN
	    IF(EUBOUND(I).GT.EL)THEN		! Note that a delta fn. line
	      FLUX(I)=PARAM(1)			! falling on a boundary is put
	      GO TO 9000			! into the lower channel
	    ENDIF
	  ENDDO
	ENDIF

* Set up base value
      IFAIL=0
      Z=(ELBOUND(1)-EL)/W
      IF(Z.LT.-SIGMAX)THEN
	CUM1=0.0D0
      ELSE
	CUM1=S15ABF(Z,IFAIL)
      END IF
      IF(IFAIL.NE.0)THEN
	STATUS=SAI__ERROR
	CALL ERR_REP(' ','Bad IFAIL from NAG routine S15ABF',STATUS)
	GOTO 9000
      END IF

*    Integrate over each energy channel
      DO I = 1, NEN
	Z=(EUBOUND(I)-EL)/W
	IF(Z.LT.-SIGMAX)THEN
	  FLUX(I)=0.0
        ELSE
	  CUM2=S15ABF(Z,IFAIL)
	  IF(IFAIL.NE.0)THEN
	    STATUS=SAI__ERROR
	    CALL ERR_REP(' ','Bad IFAIL from NAG routine S15ABF',STATUS)
	    GOTO 9000
	  END IF
	  FLUX(I)=PARAM(1)*(CUM2-CUM1)
	  IF(Z.GT.SIGMAX) GO TO 9000
	  CUM1=CUM2
	END IF
      END DO

*    Exit
 9000 IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from SPEC_LG',STATUS)

      END
