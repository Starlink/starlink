*+  TIM_XCOR - computes cross-correlation function
	SUBROUTINE TIM_XCOR(ND,Y,Z,VARY,VARZ,WEIGHT,DENOISE,NL,XC)
*    Description :
*     Computes the cross-correlation function of two series Y & Z of equal
*     length.
*    Environment parameters :
*    Method :
*     The estimator used is biased by roughly a factor (N-L)/N at lag L,
*     but has a lower variance than the unbiased estimator. Weighted and
*     unweighted functions are available, and the estimated contribution of
*     statistical noise may be removed from the zero lag autocovariances if
*     desired - this results in an estimate of the source cross-correlation
*     rather than the data cross-correlation.
*     See Weisskopf et al, Ap.J.199, L147.
*     Note that variance information is only needed if weighting or noise
*     correction is required.
*     NOTE - This routine (for efficiency reasons) is not safeguarded against
*     zero variances. This can be checked in the calling program if required.
*    Deficiencies :
*     The FFT is not used; this involves a time penalty for large data sets.
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*      6 Feb 84: Original
*     26 Sep 88: Recoded for ASTERIX88, arguments changed (TJP)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER ND		! No.of data points
      REAL Y(ND)		! Real array containing first data set
      REAL Z(ND)		! Real array containing second data set
      REAL VARY(*)		! Variance of Y. VARY(1)=0.0 =>	unweighted data
      REAL VARZ(*)		! Variance of Z. VARZ(1)=0.0 =>	unweighted data
      LOGICAL WEIGHT		! Weighted cross-correlation required?
      LOGICAL DENOISE		! Subtract estimated noise contributions from
				! denominator of cross-correlation?

*    Import-Export :
      INTEGER NL		! No.of lags to be computed. This will
				! comprise the zero lag value plus (NL-1)/2
				! lags in each direction. Must be <2*ND.
				! If NL is even on entry then NL-1 values are
				! computed and NL is reset to this value.
*    Export :
      REAL XC(NL)		! Cross-correlation of Y & Z, stored as XC(-M)
				! to XC(M), where M=(NL-1)/2. Negative lags
				! correspond to Y lagging Z
*    Local constants :
*    Local variables :
      INTEGER M			! Maximum lag in each direction
      INTEGER I,K		! Loop counters
      INTEGER L			! Lag value

      REAL YMN,ZMN		! Data means
      REAL ACY0,ACZ0		! Autocovariances
      REAL SUMYVAR,SUMZVAR	! Summed variances
      REAL DENOM		! Denominator in cross-correlation
      REAL C1,C2		! Cross-covariances for -ve & +ve lags
      REAL V1,V2,V12		! Sums of inverse variances

*    Local data :

*---------------------------------------------------------------------------

* Set up number of lags in each direction
	M=(NL-1)/2
	IF(M.GT.ND-1)THEN
	  M=ND-1
	ENDIF
	NL=2*M+1

* Set up default (unit) variances if necessary
	IF(WEIGHT.OR.DENOISE)THEN
	  IF(VARY(1).LE.0.0)THEN
	    DO I=1,ND
	      VARY(I)=1.0
	    ENDDO
	  ENDIF
	  IF(VARZ(1).LE.0.0)THEN
	    DO I=1,ND
	      VARZ(I)=1.0
	    ENDDO
	  ENDIF
	ENDIF

* Unweighted cross-correlation
	IF(.NOT.WEIGHT)THEN

*    Data means
	  YMN=0.0
	  ZMN=0.0
	  DO I=1,ND
	    YMN=YMN+Y(I)
	    ZMN=ZMN+Z(I)
	  ENDDO
	  YMN=YMN/ND				! Unweighted
	  ZMN=ZMN/ND				! means

*    Autocovariances
	  ACY0=0.0
	  ACZ0=0.0
	  DO I=1,ND
	    ACY0=ACY0+(Y(I)-YMN)**2
	    ACZ0=ACZ0+(Z(I)-ZMN)**2
	  ENDDO

*    Denominator of cross-correlation. Remove noise contribution if required.
	  IF(DENOISE)THEN
	    SUMYVAR=0.0
	    SUMZVAR=0.0
	    DO I=1,ND
	      SUMYVAR=SUMYVAR+VARY(I)	! Expected noise contributions
	      SUMZVAR=SUMZVAR+VARZ(I)	! to ACVs
	    ENDDO
	    DENOM=SQRT((ACY0-SUMYVAR)*(ACZ0-SUMZVAR))
	  ELSE
	    DENOM=SQRT(ACY0*ACZ0)
	  ENDIF

*    Cross-covariance
	  DO I=1,M+1
	    L=I-1
	    C1=0.0
	    C2=0.0
	    DO K=1,ND-L
	      C1=C1+(Y(K+L)-YMN)*(Z(K)-ZMN)
	      C2=C2+(Y(K)-YMN)*(Z(K+L)-ZMN)
	    ENDDO

*    Cross-correlation
	    XC(M+1-L)=C1/DENOM
	    XC(M+1+L)=C2/DENOM
	  ENDDO

* Weighted cross-correlation
	ELSE

*    Weighted data means
	  YMN=0.0
	  ZMN=0.0
	  V1=0.0
	  V2=0.0
	  DO I=1,ND
	    YMN=YMN+Y(I)/VARY(I)
	    ZMN=ZMN+Z(I)/VARZ(I)
	    V1=V1+1./VARY(I)
	    V2=V2+1./VARZ(I)
	  ENDDO
	  YMN=YMN/V1				! Weighted
	  ZMN=ZMN/V2				! means

*    Autocovariances (weighted)
	  ACY0=0.0
	  ACZ0=0.0
	  V12=0.0
	  DO I=1,ND
	    ACY0=ACY0+((Y(I)-YMN)/VARY(I))
	    ACZ0=ACZ0+((Z(I)-ZMN)/VARZ(I))
	    V12=V12+1./SQRT(VARY(I)*VARZ(I))
	  ENDDO
	  ACY0=ACY0/V1
	  ACZ0=ACZ0/V2

*    Denominator of cross-correlation. Remove noise contribution if required.
	  IF(DENOISE)THEN
	    DENOM=SQRT((ACY0-FLOAT(ND)/V1)*(ACZ0-FLOAT(ND)/V2))
	  ELSE
	    DENOM=SQRT(ACY0*ACZ0)
	  ENDIF
	  DENOM=DENOM*V12

*    Cross-covariance
	  DO I=1,M+1
	    L=I-1
	    C1=0.0
	    C2=0.0
	    DO K=1,ND-L
	      C1=C1+(Y(K+L)-YMN)*(Z(K)-ZMN)/SQRT(VARY(K+L)*VARZ(K))
	      C2=C2+(Y(K)-YMN)*(Z(K+L)-ZMN)/SQRT(VARY(K)*VARZ(K+L))
	    ENDDO

*    Cross-correlation
	    XC(M+1-L)=C1/DENOM
	    XC(M+1+L)=C2/DENOM
	  ENDDO
	ENDIF
	END
