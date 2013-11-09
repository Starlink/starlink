*+ FIT_FOLD - Convolves 1D data array with instrument response
	SUBROUTINE FIT_FOLD(NMDAT,NOBDAT,NRESP,MPRED,MIND,DIND,RESP,
     :  PRED,STATUS)
*
*    Description :
*     The 1D model space data array in the array MPRED is convolved with the
*     instrument response matrix stored in the arrays MIND, DIND & RESP.
*     The resulting data array is returned in PRED.
*
*    Method :
*     Simple. No longer requires any particular ordering of indices.
*
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*     Martin Watt  (BHVAD::MPW)
*
*    History :
*     31 Mar 87: Original - adapted from SPEC_FOLDI (BHVAD::TJP)
*     11 Aug 88: Logic simplified, index ordering no longer assumed (MPW)
*
*    Type definitions :
	IMPLICIT NONE

*    Global constants :
	INCLUDE 'SAE_PAR'

*    Import :
	INTEGER NMDAT			! Size of model data array
	INTEGER NOBDAT			! Size of observed data array
	INTEGER NRESP			! No of response elements
	REAL MPRED(NMDAT)		! Model space data
	INTEGER MIND(NRESP)		! Array of model channel indices
	INTEGER DIND(NRESP)		! Array of data channel indices
	REAL RESP(NRESP)		! Array of response elements

*    Export :
	REAL PRED(NOBDAT)		! Predicted data space array

*    Status :
	INTEGER STATUS

*    Local variables :
	INTEGER I,J

*-----------------------------------------------------------------------------

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Initialise output array
	DO I=1,NOBDAT
	   PRED(I)=0.0
	ENDDO

* Do the convolution
	DO J=1,NRESP
	  PRED(DIND(J))=PRED(DIND(J))+RESP(J)*MPRED(MIND(J))
	ENDDO

* Exit
	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EX','from FIT_FOLD',STATUS)
	END
