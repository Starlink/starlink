*+NIPSUB	Searches for points in image
      SUBROUTINE NIPSUB(NX, NY, imager, rdata, THRESH, NDET,
     &   XPOS, YPOS, COUNTS, STATUS)

	integer nsize
	real imager(nx,ny)
      INTEGER NX, NY  	  !input	Dimensions of raw data array
      REAL RDATA(NX,NY)  !input	Raw data counts (non-negative)
      REAL THRESH     	  !input	detection threshold, -log10(pfa)
      INTEGER NDET	  !output	Cumulative number of detections
      REAL XPOS(*), YPOS(*) !output Pixel coordinates of detection
      REAL COUNTS(*)	  !output Size of detection
      INTEGER STATUS	  !in/out	Inherited status
*-Author	Clive Page	1991-NOV-1
*
      REAL SIGPXS, GETEL
      EXTERNAL SIGPXS, GETEL
      INTEGER LBLOCK, MAXAV, MAXNXY, MINGROUP
      REAL PIXSIZE, F2P5, F4P5, F6TO18
      PARAMETER (LBLOCK = 8, MAXAV = 16384, PIXSIZE = 1.0,
     &  MAXNXY = 512*512, MINGROUP = 10)
      PARAMETER (F2P5 = 0.56, F4P5 = 0.7825, F6TO18 = 0.11)
      INTEGER NBX, NBY, LOCX, LOCY, QBAD
      REAL BGRBLK(MAXAV), SIGMA(MAXNXY), SIZE, AVGBGR, SIG, STATMIN,
     & AB, CB, AS, CS, DENOM, SOURCE, SRCERR, BACKGD, BGRERR,
     & PFIT(7), PERR(7)
c
	do nbx=1,Nx
	  do nby=1,ny
	    rdata(nbx,nby)=imager(nbx,nby)
	  end do
	end do
*
      IF(STATUS .NE. 0) RETURN
      NDET = 0
*Find average of raw counts in block of 8x8 pixels
      NBX = (NX-1)/LBLOCK + 1
      NBY = (NY-1)/LBLOCK + 1
      IF(NX*NY .GT. MAXNXY .OR. NBX*NBY .GT. MAXAV) THEN
         PRINT *,'data array too big'
         STATUS = -1
         GO TO 1000
      END IF
c

*Compute raw mean and apply median filter
      CALL ESTBGR(NX, NY, RDATA, LBLOCK, NBX, NBY, BGRBLK)
*Convolve RDATA data with circle of radius 2.5 arcmins producing SIGMA
*Convert these levels to log10 likelihood levels per image
      CALL TOPHAT(NX, NY, RDATA, 2.5/PIXSIZE, LBLOCK, NBX, NBY,
     &   BGRBLK, SIGMA)
*
*Loop from here finding largest remaining element in SIGMA
*
100   CONTINUE
      STATUS = 0
      CALL FINDMAX(NX, NY, SIGMA, LOCX, LOCY, SIZE)
*
* Test with reduced threshold because approximations made above
*
      IF(SIZE .LE. MAX(0.1,THRESH-2.0)) GO TO 1000
*Find counts around this point and the local background
      QBAD = 0
      CALL SUMANN(NX, NY, RDATA, LOCX, LOCY, 6.0/PIXSIZE,
     &		18.0/PIXSIZE, .FALSE., SIGMA, QBAD, AVGBGR, AB)
      CB = AVGBGR * AB
      CALL SUMCIRC(NX, NY, RDATA, REAL(LOCX), REAL(LOCY),
     &			 2.5/PIXSIZE, AS, CS)
      DENOM  = F2P5*AB - F6TO18*AS
      SOURCE = (CS*AB - CB*AS)/DENOM
      SRCERR = SQRT(CS*AB**2 + CB*AS**2)/DENOM
      BACKGD = (CB*F2P5 - CS*F6TO18)/DENOM
      BGRERR = SQRT(CB*F2P5**2 + CS*F6TO18**2)/DENOM
*If corrected background is near negative, revert to raw uncorrected value
      IF(BACKGD .LT. 1.0E-3) BACKGD = AVGBGR
      SIG = SIGPXS(INT(CS), AS, BACKGD, BGRERR, REAL(NX*NY))
      CALL ZAP(NX, NY, SIGMA, LOCX, LOCY, 7.0/PIXSIZE, 0.0)
      IF(SIG .LT. THRESH) GO TO 100
*Fit to gaussian using max liklihood: ignore detection if bad fit
      CALL FITMAXL(NX, NY, RDATA, BACKGD, PIXSIZE, LOCX, LOCY,
     &		SOURCE, PFIT, PERR, STATMIN, STATUS)
      CALL SUMCIRC(NX, NY, RDATA, PFIT(2), PFIT(3), 4.5/PIXSIZE,
     &  AS, CS)
      SOURCE = (CS - AS*BACKGD)/F4P5
      IF(NDET .LT. 10 .AND. STATUS .EQ. 0) THEN
         NDET = NDET + 1
         XPOS(NDET) = PFIT(2)
         YPOS(NDET) = PFIT(3)
         COUNTS(NDET) = SOURCE
         GO TO 100
      END IF
1000  CONTINUE
      END

*+CGAUSS	Point response function - circular gaussian
	SUBROUTINE CGAUSS(PARAM, NHALF, ARRAY)
	REAL PARAM(7)   !input	Parameters: PARAM(1) = total flux,
			!	PARAM(2) = X-centre, PARAM(3) = Y-centre,
			!	PARAM(4) = width (FWHM),
			!	PARAM(5) = background level
	INTEGER NHALF   !input	Width of array is 2*NHALF+1
	REAL ARRAY(-NHALF:NHALF, -NHALF:NHALF)
			!output	Response sampled on grid.
*-Author	Clive Page	1990-Sept-10
	REAL FWHMSQ, EFACT, AMPL, YOFFSQ, RSQ, ARG, YOFF, XOFF
	INTEGER IX, IY
*
	XOFF = PARAM(6) - PARAM(2)
	YOFF = PARAM(7) - PARAM(3)
	FWHMSQ = MAX(1.0E-6, PARAM(4)**2)
*First constant below is 4*log(2), second one is 4*log(2)/pi
	EFACT = 2.7725887/FWHMSQ
	AMPL = 0.8825424*PARAM(1)/FWHMSQ
	DO IY = -NHALF, NHALF
	    YOFFSQ = (REAL(IY)+YOFF)**2
	    DO IX = -NHALF, NHALF
		RSQ = (REAL(IX)+XOFF)**2 + YOFFSQ
		ARG = MIN(20.0, EFACT*RSQ)
		ARRAY(IX, IY) = AMPL*EXP(-ARG) + PARAM(5)
	    END DO
	END DO
	END

*+CSTAT		Evaluates Cash statistic for given point-response and data
	REAL FUNCTION CSTAT(PARAM, PRFUNC, NX, NY, RDATA)
	REAL PARAM(*)           !input	Parameters of the function
	EXTERNAL PRFUNC         !input	Subroutine evaluating point-response
	INTEGER NX, NY          !input	Dimensions of data array
	REAL RDATA(NX, NY)      !input	Data array
*Returns statistic: 2 * sum{model - data*log(model)}
*-Author	Clive Page	1990 Sept 18
*local storage
	INTEGER NHALF
	PARAMETER (NHALF = 5)
	REAL C, RMODEL(-NHALF:NHALF, -NHALF:NHALF)
	INTEGER IX, IY, KX, KY, LOCX, LOCY
*
	LOCX = NINT(PARAM(6))
	LOCY = NINT(PARAM(7))
*Generate model with supplied parameters
	CALL PRFUNC(PARAM, NHALF, RMODEL)
*Compute the Cash statistic over array (with limits at edge of field)
	C = 0.0
	DO IY = MAX(LOCY-NHALF, 1), MIN(LOCY+NHALF, NY)
	    KY = IY - LOCY
	    DO IX = MAX(LOCX-NHALF, 1), MIN(LOCX+NHALF, NX)
		KX = IX - LOCX
		IF(RMODEL(KX,KY).GT.0.0)C = C + RMODEL(KX, KY)
     &		    - RDATA(IX, IY)*LOG(RMODEL(KX,KY))
	    END DO
	END DO
	CSTAT = 2.0*C
	END

*+ESTBGR	Estimates background in each block of size LBLOCK
	SUBROUTINE ESTBGR(NX, NY, RDATA, LBLOCK, NBX, NBY, BGRBLK)
	INTEGER NX, NY          !input	Dimension of raw data
	REAL RDATA(NX, NY)      !input	Raw data array
	INTEGER LBLOCK          !input	Dimension of square blocks
	INTEGER NBX, NBY        !input	Dimensions of BGRBLK
	REAL BGRBLK(NBX, NBY)   !output	Background in each block
*-Author	Clive Page	1990-Sept-10
	INTEGER NBYTES, IMEAN, DIMS(2)
*Get workspace for initial mean values
	NBYTES = 4*NBX*NBY
	DIMS(1)=2*NBX
	DIMS(2)=2*NBY

C	CALL LIB$GET_VM(NBYTES, IMEAN)
D	CALL DYN_MAP(1, NBYTES, IMEAN, STATUS)
	CALL DYN_MAPR(2, DIMS, IMEAN, STATUS)

	CALL RAWAVG(NX, NY, RDATA, LBLOCK, NBX, NBY, %VAL(IMEAN))
*Now apply median filter in 3x3 group of these values
	CALL MEDFILT(NBX, NBY, %VAL(IMEAN), BGRBLK)

C	CALL LIB$FREE_VM(NBYTES, IMEAN)
	CALL DYN_UNMAP( IMEAN, STATUS)

	END

*+FINDMAX	Searches for largest element in real array.
	SUBROUTINE FINDMAX(NX, NY, ARRAY, MX, MY, VMAX)
	INTEGER NX, NY          !input	Dimensions of array
	REAL ARRAY(NX, NY)      !input	Data
	INTEGER MX, MY          !output	Location of peak.
	REAL VMAX               !output	Peak value
*-Author	Clive Page	1990-Aug-15
	INTEGER IX, IY
	REAL AMAX
*
	AMAX = ARRAY(1, 1)
	MX = 1
	MY = 1
	DO IY = 1, NY
	    DO IX = 1, NX
		IF(ARRAY(IX,IY).GT.AMAX)THEN
		    AMAX = ARRAY(IX, IY)
		    MX = IX
		    MY = IY
		END IF
	    END DO
	END DO
	VMAX = AMAX
	END

*+FITMAXL	Maximum Liklihood fit to gaussian PSF
	SUBROUTINE FITMAXL(NX, NY, RDATA, BGR, PIXSIZE,
     &   LOCX, LOCY, COUNT, PBEST, PERR, STATMIN, STATUS)
	INTEGER NX, NY          !input	Dimensions of raw data
	REAL RDATA(NX, NY)      !input	Raw counts
	REAL BGR                !input	Estimated background level
	REAL PIXSIZE		!input	Pixel size, arcmins
	INTEGER LOCX, LOCY      !input	Approx location of peak in raw data
	REAL COUNT		!input	Approx count in circle near peak
	REAL PBEST(7)           !output	Returns: count, posx, posy
	REAL PERR(7)            !output	Returns standard errors in above.
	REAL STATMIN		!output	C-statistic minimum found
	INTEGER STATUS		!in/out	Inherited status
*-Author	Clive Page	1990-OCT-1
	INTEGER NPARAM, J
	PARAMETER (NPARAM=7)
	REAL PARAM(NPARAM), PLO(NPARAM), PHI(NPARAM), CONFL(NPARAM)
	EXTERNAL CGAUSS
*Use 90% confidence level for positions, 68% (1-sigma) for size, width
	DATA CONFL/ 0.68, 0.9, 0.9, 0.68, 3*0.0/
*
	IF(STATUS .NE. 0) RETURN
	PARAM(1) = MAX(COUNT, 10.0)
	PARAM(2) = LOCX
	PARAM(3) = LOCY
	PARAM(4) = 3.5 / PIXSIZE
	PARAM(5) = BGR
	PARAM(6) = LOCX
	PARAM(7) = LOCY
	PERR(1) = 0.1*PARAM(1)
	PERR(2) = 0.2
	PERR(3) = 0.2
	PERR(4) = 0.1
	PERR(5) = 0.0
	PERR(6) = 0.0
	PERR(7) = 0.0
	CALL SRCHMIN(CGAUSS, NX, NY, RDATA, NPARAM, PARAM,
     &	  PERR, CONFL, STATMIN, PBEST, PLO, PHI, STATUS)
*Return standard errors as half distance of PHI and PLO
	DO J = 1, 7
	    PERR(J) = 0.5*(PHI(J)-PLO(J))
	END DO
	END

*+GETEL		Extracts one element from a variable-shape array
	REAL FUNCTION GETEL(NX, NY, ARRAY, IX, IY)
	INTEGER NX, NY          !input	Dimensions of array
	REAL ARRAY(NX, NY)      !input	Data array
	INTEGER IX, IY          !input	Required element
*-Author	Clive Page	1990 Aug 24
	GETEL = ARRAY(IX, IY)
	END

*+LEFACT	Returns Logarithm to base e of factorial of integer N.
	REAL FUNCTION LEFACT(N)
	INTEGER N       !input	Argument must be non-negative.
*-Author	Clive Page	1990-Aug-15
*On its first call this routine stores the first MAXSTORE values in FLN
* to save time later, higher values computed by Stirling's approximation.
	REAL LRT2PI     !natural log of square-root of two pi.
	INTEGER MAXSTORE, I
	PARAMETER(MAXSTORE=100, LRT2PI=0.91893853)
	REAL FLN(0:MAXSTORE)
	LOGICAL SETUP
	SAVE SETUP, FLN
	DATA SETUP/.FALSE./
*
	IF(.NOT.SETUP)THEN
	    FLN(0) = 0.0
	    DO I = 1, MAXSTORE
		FLN(I) = FLN(I-1) + LOG(REAL(I))
	    END DO
	    SETUP = .TRUE.
	END IF
	IF(N.LT.0)THEN
	    PRINT *, 'LEFACT ERROR: -ve arg for factorial'
	    LEFACT = 0.0
	ELSE IF(N.LE.MAXSTORE)THEN
	    LEFACT = FLN(N)
	ELSE
*Compute from Stirling's approximation
	    LEFACT = (N+0.5)*LOG(REAL(N)) - N + LRT2PI
	END IF
	END

*+MEDFILT	Filters 2-d array with 3x3 median filter avoiding zeros
	SUBROUTINE MEDFILT(NX, NY, ARRAY, OUTPUT)
	INTEGER NX, NY          !input	Dimensions of ARRAY and OUTPUT
	REAL ARRAY(NX,NY)	!input	Data array
	REAL OUTPUT(NX,NY)	!output	Output (must not be same as input)
*-Author	Clive Page	1990-OCT-15
	INTEGER IX, IY, N, I, J, K, L, MID
	REAL TEMP(9), TMID, DELTA
	EXTERNAL SORTR
*
	DO IY = 1, NY
	    DO IX = 1, NX
		N = 0
		DO J = MAX(1,IY-1), MIN(NY,IY+1)
		    DO I = MAX(1,IX-1), MIN(NY,IX+1)
			IF(ARRAY(I,J) .GT. 0.0) THEN
			    N = N + 1
			    TEMP(N) = ARRAY(I,J)
			END IF
		    END DO
		END DO
		IF(N .GT. 0) THEN
		    CALL MEDIAN(N, TEMP, TMID, DELTA)
		    OUTPUT(IX,IY) = TMID
		ELSE
		    OUTPUT(IX,IY) = 0.0
		END IF
	    END DO
	END DO
	END

*+MEDIAN	Finds "middle" value of small array ignoring outliers
	SUBROUTINE MEDIAN(NPTS, ARRAY, TMID, DELTA)
	INTEGER NPTS		!input	No of data points
	REAL ARRAY(NPTS)	!in/out	Values, returned sorted
	REAL TMID		!output	Selected median value
	REAL DELTA		!output	Typical difference in sorted values
*-Author	Clive Page	1991-FEB-18
* altered thresholds for rejecting outliers 	cgp	1990-nov-5
	INTEGER MID, K, L
	REAL TMIN, TMAX
*
	CALL SORTR(NPTS, ARRAY)
*Simple choice of median can leave residual bias from strong sources or
* areas of zero counts.  If min value small, choose an initial value
* two-thirds of the way up the sorted list to avoid effects of near-zeros
        IF(ARRAY(1) .GE. 0.5) THEN
           MID = NINT(NPTS/2.0)
        ELSE
	   MID = NINT(NPTS/1.5)
        END IF
	TMID = ARRAY(MID)
*Now try to reduce sensitivity to areas of blank sky or to strong
* sources by excluding very low or high values from the range
	K = 1
	TMIN = 0.5 * TMID
10	CONTINUE
	IF(K .LT. MID .AND. ARRAY(K) .LT. TMIN) THEN
		K = K + 1
		GO TO 10
	END IF
	L = NPTS
	TMAX = 2.0 * TMID
20	CONTINUE
	IF(L .GT. MID .AND. ARRAY(L) .GT. TMAX) THEN
		L = L - 1
		GO TO 20
	END IF
*Now take mid-point of the values between points K and L in the sorted list
	MID = (K+L)/2
	IF(MOD(L-K+1,2) .EQ. 0) THEN
		TMID = 0.5 * (ARRAY(MID) + ARRAY(MID+1))
		DELTA = ARRAY(MID+1) - ARRAY(MID)
	ELSE
		TMID = ARRAY(MID)
		IF(MID .GE. 2 .AND. MID .LT. NPTS) THEN
			DELTA = 0.5*(ARRAY(MID+1)-ARRAY(MID-1))
		ELSE
			DELTA = 0.0
		END IF
	END IF
	END

*+MINIMIZE	Search for minimum of statistic
	SUBROUTINE MINIMIZE(NPARAM, P, CURV, PFIXED, SVAL, FUNC,
     &		NX, NY, RDATA, STATUS)
	INTEGER NPARAM          !input	Total number of parameters, max 100
	REAL P(NPARAM)          !in/out	Parameter values, returns best fit
	REAL CURV(NPARAM)       !in/out	Supplies parameter error estimates
				!	returns 2nd derivative wrt params
	LOGICAL PFIXED(NPARAM)  !input	.TRUE. for params to be kept fixed
	REAL SVAL               !output	Final minimum value of statistic
	EXTERNAL FUNC           !function to be minimized
	INTEGER NX, NY          !input	Dimensions of data
	REAL RDATA(NX, NY)      !input	Data array passed to function
	INTEGER STATUS		!in/out	Inherited status
*-
*This routine uses the following call to get current value of statistic
*	S = CSTAT(P,FUNC,NX,NY,RDATA)
*STAT must be returned value of statistic
* MINIMIZE always exits such that last
*model calculated was for best fit obtained.
*
*Search uses gradient and 2nd derivative
*
*Convergence test is as follows
*Fractional change in statistic < 1.E-4, normalised step < 1%
*-Author Dick Willingale 1986-Feb-12
* Fixed divide by near zero when checking 2nd derivative. CGP 1991-MAY-3
*
*The search only allows parameters to take positive values. If it is
*possible for a model parameter to be both negative and positive then it
*must be offset by a large positive constant so that as far as this routine
*is concerned it is always positive. If a model parameter takes only
*negative values then simply negate the parameter when using it in the model.
*This feature is useful since it allows physically impossible values of
*parameters to be excluded from the search for a minimum statistic.
*
*On entry array CURV() contains estimate of 2nd derivative of statistic
*wrt parameters. This can be estimated using guessed parameter errors,
*CURV(I)=1./ERROR(I)**2. On exit CURV() contains the derived curvature values.
*
*local storage
	INTEGER MAXPAR, NSTEPS
	PARAMETER(MAXPAR=100, NSTEPS=50)
	REAL P0(MAXPAR), PC(MAXPAR), B(MAXPAR), SCURV(MAXPAR)
	REAL STEP, DELTP, STAT0, STAT1, STAT2, STAT, PRN, DELC, CSTAT
	INTEGER IPASS, IGO, J
	EXTERNAL CSTAT
*
	IF(STATUS .NE. 0) RETURN
* Find initial value of statistic
	SVAL = CSTAT(P, FUNC, NX, NY, RDATA)
	STAT0 = SVAL
* Save initial curvature
	DO J = 1, NPARAM
	    SCURV(J) = CURV(J)
	END DO
* Set number of passes and convergence flag
	IPASS = 0
	STEP = 0.0
* Try another iteration
100	CONTINUE
	STEP = 0.
	DO J = 1, NPARAM
	    P0(J) = P(J)
* If fixed don't do it
	    IF(.NOT.(PFIXED(J)))THEN
* Set small step using 2nd derivative
		IF(CURV(J).NE.0.)THEN
		    DELTP = SQRT(ABS(STAT0*0.01/CURV(J)))
		ELSE IF(SCURV(J).NE.0.)THEN
		    DELTP = SQRT(ABS(STAT0*0.01/SCURV(J)))
		ELSE
		    DELTP = AMAX1(0.1*P(J), 1.)
		END IF
		IF(DELTP.GT.P(J))THEN
* Step forward twice
		    STAT2 = STAT0
		    P(J) = P(J) + DELTP
		    STAT = CSTAT(P, FUNC, NX, NY, RDATA)
		    P(J) = P(J) + DELTP
		    STAT1 = CSTAT(P, FUNC, NX, NY, RDATA)
		    P(J) = P(J) - 2.*DELTP
* Calculate coefficients of quadratic
		    CURV(J) = (STAT1+STAT2-2.*STAT)*0.5/DELTP**2
		    B(J) = (STAT1-STAT)/DELTP - CURV(J)
     &			   *(DELTP+2.*(P(J)+DELTP))
		ELSE
* Step forward and back
		    P(J) = P(J) + DELTP
		    STAT1 = CSTAT(P, FUNC, NX, NY, RDATA)
		    P(J) = P(J) - 2.*DELTP
		    STAT2 = CSTAT(P, FUNC, NX, NY, RDATA)
		    P(J) = P(J) + DELTP
* Calculate coefficients of quadratic
		    CURV(J) = (STAT1+STAT2-2.*STAT0)*0.5/DELTP**2
		    B(J) = (STAT1-STAT0)/DELTP - CURV(J)*(DELTP+2.*P(J))
		END IF
* Check 2nd derivative +ve or -ve
                IF(ABS(CURV(J)) .LT. 1E-20) THEN
		    DELTP = AMAX1(0.1*P(J), 1.)
		ELSE IF(CURV(J).LT.0.)THEN
* -ve 2nd derivative, move left or right a bit
		    DELTP = SQRT(ABS(STAT0*0.1/CURV(J)))
		    IF(STAT1.GT.STAT2)DELTP = -DELTP
* +ve 2nd derivative, calculate position of minimum
		ELSE
		    DELTP = -B(J)*0.5/CURV(J) - P(J)
		END IF
* Try out step
		IF(P(J)+DELTP.LT.0.)DELTP = -P(J)
		IF(DELTP.NE.0.)THEN
		    P(J) = P(J) + DELTP
		    STAT = CSTAT(P, FUNC, NX, NY, RDATA)
		    IF(STAT0.GT.STAT)THEN
			STAT0 = STAT
		    ELSE
			P(J) = P(J) - DELTP
		    END IF
		END IF
		PC(J) = P(J) - P0(J)
		STEP = STEP + ABS(PC(J))
	    END IF
	END DO
* Try steps in same direction until fails
	IF(STEP.NE.0.)THEN
	    IGO = 0
150	    CONTINUE
	    IGO = IGO + 1
	    IF(IGO.LE.30)THEN
		DO J = 1, NPARAM
		    IF(.NOT.(PFIXED(J)))THEN
			IF(P(J)+PC(J).LE.0.)PC(J) = -P(J)
			P(J) = P(J) + PC(J)
		    END IF
		END DO
		STAT = CSTAT(P, FUNC, NX, NY, RDATA)
		IF(STAT.LT.STAT0)THEN
		    STAT0 = STAT
		    GO TO 150
		ELSE
		    DO J = 1, NPARAM
			IF(.NOT.(PFIXED(J)))P(J) = P(J) - PC(J)
		    END DO
		END IF
	    END IF
	END IF
* Finished pass
	IPASS = IPASS + 1
* Calculate normalised step made in pass
	STEP = 0.
	PRN = 0.
	DO J = 1, NPARAM
	    IF(.NOT.(PFIXED(J)))THEN
		PRN = PRN + 1.
		IF(P0(J).NE.0.)THEN
		    STEP = STEP + ((P(J)-P0(J))/P0(J))**2
		ELSE
		    STEP = 0.
		END IF
	    END IF
	END DO
	PRN = MAX(PRN, 1.)
	STEP = SQRT(STEP/PRN)
* Calculate change in statistic in pass
	DELC = ABS(STAT0-SVAL)/SVAL
	SVAL = STAT0
* Check convergence
* Fractional change in statistic;  normalised step
	IF((DELC .GT. 1.0E-6 .OR. STEP .GT. 1.0E-3) .AND.
     &      IPASS .LT. NSTEPS) GO TO 100
	IF(IPASS.GE.NSTEPS) THEN
	    STATUS = 2
	    WRITE(9,*)'MINIMIZE failed to converge, PASS=',IPASS,
     &		' DELC=',DELC, ' STEP=',STEP, ' P =',(P(J),J=1,7)
	END IF
	END

*+PPLNAV	Returns log base e of Poisson prob of >= N when X uncertain.
	REAL FUNCTION PPLNAV(N, X, XERR)
	INTEGER N	!input	No of events to be equalled or exceeded.
	REAL X		!input	Mean expected level.
	REAL XERR	!input	Standard error in X.
*-Author	Clive Page	1990-NOV-5
*Possible bug fixed		1991-JUL-26
*This routine evaluates the Poisson probability of getting N or more events
* for X distributed from -3 sigma to +3 sigma around its central value,
* and weights each of these by the Gaussian probability of such a
* deviation from the central value.
	EXTERNAL PPLNGE
	REAL PPLNGE, PGAUSS(0:10), DELTAX, PMEAN, PSUM, XTEMP, PROB
	INTEGER JJ
	DATA PGAUSS/ 0.1192, 0.1140, 0.0998, 0.0797, 0.0584, 0.0390,
     &   0.0239, 0.01338, 0.00683, 0.00320, 0.00136/
*
	PMEAN = PPLNGE(N,X)
	IF(XERR .LT. 1.0E-6 * X .OR. ABS(PMEAN) .GE. 35.0) THEN
* Convolution will overflow or be meaningless: just return probability
* based on central value of X
	    PPLNAV = PMEAN
	ELSE
*Sum weighted probabilities for X from -3 sigma to + 3 sigma step 0.3 sigma
	    DELTAX = 0.3 * XERR
	    PSUM   = 0.0
	    DO JJ = -10,+10
		XTEMP = X + REAL(JJ) * DELTAX
                IF(XTEMP .GE. 0.1) THEN
		   PROB = PPLNGE(N,XTEMP)
		   IF(PROB .GE. -35.0)
     &		      PSUM  = PSUM + EXP(PROB) * PGAUSS(ABS(JJ))
                END IF
	    END DO
	    PPLNAV = LOG(MAX(PSUM,1E-30))
	END IF
	END

*+PPLNEQ	Returns log base e of Poisson probability of exactly N events
	REAL FUNCTION PPLNEQ(N, X)
	INTEGER N	!input	No of events to be equalled or exceeded.
	REAL X		!input	Expected level.
*Note: probability is always > 1 so result returned is always -ve.
*-Author	Clive Page	1990-NOV-5
	REAL LEFACT
	EXTERNAL LEFACT
*
	IF(X .GE. 0.01 .AND. N .GE. 0) THEN
	    PPLNEQ = REAL(N)*LOG(X) - X - LEFACT(N)
	ELSE
	    PPLNEQ = -1000.0
	END IF
	END

*+PPLNGE	Returns log base e of Poisson probability of N or more events.
	REAL FUNCTION PPLNGE(N, X)
	INTEGER N	!input	No of events to be equalled or exceeded.
	REAL X		!input	Expected level.
*Note: probability is always < 1 so result returned is always -ve.
*-Author	Clive Page	1990-NOV-5
*BUG FIXED IN CONDITION		1991-JUL-26 CGP
	INTEGER NMAX, JJ
	PARAMETER (NMAX = 10000)
	REAL SUM, TERM, PPLNEQ
	EXTERNAL PPLNEQ
*
	TERM = 1.0
	SUM  = 1.0
	DO JJ = N+1,N+NMAX
	    TERM = TERM * X / REAL(JJ)
	    SUM  = SUM + TERM
	    IF(TERM .LT. 1.0E-6 * SUM .OR. SUM .GE. 1.0E30) GO TO 90
	END DO
	PRINT *,'PPLNGE error: series summation failed', N,X,JJ,TERM,SUM
90	CONTINUE
	PPLNGE = PPLNEQ(N,X) + LOG(SUM)
	END

*+QUADFIT	Quadratic fitting at minimum
	SUBROUTINE QUADFIT(XMIN, YMIN, X, Y, YFIT, A, B, C, XLO, XHI,
     &			   STATUS)
	REAL XMIN, YMIN !input	position of minimum
	REAL X, Y       !input	other position on quadratic
	REAL YFIT       !input	requested Y value
	REAL A, B, C    !output	coefficients of quadratic
	REAL XLO, XHI   !output	the 2 intersection values
	INTEGER STATUS	!in/out	inherited status
*
* Internal subroutine used by SRCHMIN.
* Given two points on a quadratic, one of which is the minimum, returns
* the coefficients of the quadratic and the x values for a requested y.
* If no intersection for requested y then XLO and XHI both returned as 0
* and STATUS set to 1.
*
*-Author Dick Willingale 1986-Mar-14
* minor mods Clive Page	1990-Sept-7
* Calculate coefficients of quadratic
	REAL BTEMP
*
	IF(STATUS .NE. 0) RETURN
	IF(XMIN.NE.X)THEN
	    A = (Y-YMIN)/(X-XMIN)**2
	ELSE
	    A = 0.
	END IF
	B = -2.*A*XMIN
	C = YMIN + A*XMIN**2
* Now find intersection with YFIT
	BTEMP = B**2 - 4.*A*(C-YFIT)
	IF(A.NE.0. .AND. BTEMP.GE.0.)THEN
	    XLO = XMIN - SQRT(BTEMP)/(2.0*A)
	    XHI = XMIN + SQRT(BTEMP)/(2.0*A)
	ELSE
	    XLO = 0.
	    XHI = 0.
	    STATUS = 1
	END IF
	END

*+RAWAVG	Gets average by summing blocks
	SUBROUTINE RAWAVG(NX, NY, RDATA, LBLOCK, NBX, NBY, BGRMEAN)
	INTEGER NX, NY          !input	Dimensions of raw data
	REAL RDATA(NX, NY)      !input	Raw data array
	INTEGER LBLOCK          !input	Blocking factor
	INTEGER NBX, NBY        !input	Dimensions of BGRMEAN
	REAL BGRMEAN(NBX, NBY)  !output	Arithmetic mean levels
*-Author	Clive Page	1990-Sept-26
	INTEGER IX, IY, I, J, NPTS, KX, KY, LX, LY
	REAL SUM, AVG, SAVE
*
	KY = 0
	DO IY = 1, NY, LBLOCK
	    KX = 0
	    KY = KY + 1
	    LY = MIN(NY, IY+LBLOCK-1)
	    DO IX = 1, NX, LBLOCK
		KX = KX + 1
		LX = MIN(NX, IX+LBLOCK-1)
		SUM = 0.0
		NPTS = 0
		DO J = IY, LY
		    DO I = IX, LX
			SUM = SUM + RDATA(I, J)
			NPTS = NPTS + 1
		    END DO
		END DO
		AVG = SUM/REAL(MAX(1,NPTS))
		SAVE = AVG
		BGRMEAN(KX,KY) = AVG
	    END DO
	END DO
	END

*+SIGPXS	Computes significance of count excess by Poisson statistics
	REAL FUNCTION SIGPXS(NDET, AREA, AVG, AVGERR, AREAIM)
	INTEGER NDET    !input	No of events detected in area of NPIX pixels
	REAL AREA	!input	No of pixels summed to find NDET events
	REAL AVG        !input	Expected count/pixel, i.e. local average
	REAL AVGERR	!input	Standard error of AVG.
	REAL AREAIM	!input	Total number of pixels in the image.
*-Author	Clive Page	1990-Sept-29
*SIGPXS returns -logarithm to base 10 of the probability of false-alarm
* per whole image, using Poisson statistics, of NDET or more events in
* the area NPIX. i.e. a value of 3.0 means 1.0E-3 chance of exceeding the
* observed count by chance if the background is random and Poissonian.
	EXTERNAL PPLNAV
	REAL PPLNAV, E10, SUMEXP, SUMERR
	PARAMETER (E10=2.3025851)
*
	SUMEXP = AREA * AVG
	SUMERR = AREA * AVGERR
d	IF(SUMEXP .LE. 0.1)THEN
	IF(SUMEXP .LE. 0.01)THEN
	    SIGPXS = -100.0
	ELSE
	    SIGPXS = (LOG(AREA/AREAIM)-PPLNAV(NDET,SUMEXP,SUMERR))/E10
	END IF
	END

*+SORTR		Simple interchange ascending sort of real array.
	SUBROUTINE SORTR(NPTS, ARRAY)
	INTEGER NPTS            !input	No of real points
	REAL ARRAY(*)        	!in/out	Data, returned in ascending order.
*-Author	Clive Page	1990-Sept-29
	INTEGER I, J
	REAL A
*
	DO J = 2, NPTS
	    A = ARRAY(J)
	    DO I = J - 1, 1, -1
		IF(ARRAY(I).LE.A)GO TO 50
		ARRAY(I+1) = ARRAY(I)
	    END DO
	    I = 0
50	    CONTINUE
	    ARRAY(I+1) = A
	END DO
	END

*+SRCHMIN	Find parameter values and ranges at minimum of a statistic
	SUBROUTINE SRCHMIN(PRFUNC, NX, NY, RDATA, NPARAM, PINIT,
     &	  ESTERR, CONFL, STATMIN, PMIN, PLO, PHI, STATUS)
	EXTERNAL PRFUNC         !subroutine to be minimised
	INTEGER NX, NY          !input	Dimensions of raw data
	REAL RDATA(NX, NY)      !input	Raw counts
	INTEGER NPARAM          !input	total number of parameters
	REAL PINIT(NPARAM)      !input	initial parameter values, UNCHANGED ON OUTPUT
	REAL ESTERR(NPARAM)     !input	estimated error in parameter -
				! if 0.0 then parameter fixed during iteration.
				! if unknown try 0.1*PARAM in each case
	REAL CONFL(NPARAM)	!input	Confidence level, e.g. 0.68 = 1 sigma
	REAL STATMIN            !output	Minimum value of statistic found
	REAL PMIN(NPARAM)       !output	Values of parameters at minimum
	REAL PLO(NPARAM)        !Output	Lower limit of parameter values
	REAL PHI(NPARAM)        !output	upper limit of parameter values
	INTEGER STATUS		!in/out	Inherited status
*
* The user must supply the following call which calculates the value of
* statistic given a set of model parameters PARAM(NPARAM).
* 	s = cstat(param, prfunc, nx, ny, rdata)
* Using the routine CSTAT, SRCHMIN will search for the values of th
* parameters that minimise the statistic. When the minimum is located th
* changes in the parameters required to increase the statistic by DELS will
* then be found.  The convergence test in MINIMIZE is as follows:
* Fractional change in statistic < 1.E-4, normalised step length < 1%
*
* IMPORTANT NOTE: the search only allows parameters to take positive
* values. If it ispossible for a model parameter to be both negative and
* positive then i must be offset by a large positive constant so that it
* is always posit If a model parameter takes only negative values then
* simply negate the parameter when using it in the model. This feature is
* useful since it allows physically impossible values of parameters to be
* excluded from the search for the minimum.
*
*-Original author Dick Willingale 1986-Mar-27
* Tidied code: Clive Page	1990-OCT-10
* Fixed error in calling QUADFIT with coincident points CGP 1990-OCT-15
* Tried to prevent further rare error messages when parabolic extrapolation
* inadequate. CGP 1990-NOV-5
	INTEGER II, ILOOP, J, K, NFIXED, MAXPAR
	PARAMETER(MAXPAR=100)
	REAL A, B, C, CLO, CHI, DELS(MAXPAR), SFIT, SMIN
	REAL PARAM(MAXPAR), CURV(MAXPAR), BCURV(MAXPAR), PINC(MAXPAR)
	LOGICAL PFIXED(MAXPAR)
	REAL     SX_CHISQD, CSTAT
	EXTERNAL SX_CHISQD, CSTAT, MINIMIZE, QUADFIT
*
	IF(STATUS .NE. 0) RETURN
* Check that number of parameters is not too large
	IF(NPARAM.GT.MAXPAR)THEN
	    PRINT *,'SRCHMIN error: too many parameters'
	    STATUS = 9
	    GO TO 99999
	END IF
* Estimate curvature from errors and set up array PFIXED
* Set initial values
	NFIXED = 0
	DO J = 1, NPARAM
	    PMIN(J) = PINIT(J)
	    IF(ESTERR(J).GT.0.0)THEN
*One degree of freedom since estimating each parameter error separately
		DELS(J)   = SX_CHISQD(1.0-CONFL(J), 1)
		BCURV(J)  = DELS(J)/ESTERR(J)**2
		PFIXED(J) = .FALSE.
	    ELSE
		DELS(J)   = 0.0
		BCURV(J)  = 0.
		PFIXED(J) = .TRUE.
		NFIXED = NFIXED + 1
	    END IF
	END DO
* Search for minimum of statistic
	CALL MINIMIZE(NPARAM, PMIN, BCURV, PFIXED, STATMIN,
     &		      PRFUNC, NX, NY, RDATA, STATUS)
* Return if limits not required or if not converged or if all fixed
	IF(STATUS .EQ.0 .AND. NFIXED.LT.NPARAM)THEN
* Set steps using curvature and required change in statistic
	    DO J = 1, NPARAM
		IF(.NOT.PFIXED(J))THEN
		    IF(BCURV(J).GT.0.)THEN
			PINC(J) = SQRT(DELS(J)/BCURV(J))
		    ELSE
			PINC(J) = ESTERR(J)
		    END IF
		END IF
	    END DO
* Increment NFIXED since always 1 more fixed parameter when doing limits
* analysis.
* Now loop for each parameter in turn to find errors
	    NFIXED = NFIXED + 1
	    DO J = 1, NPARAM
* If parameter not fixed then fix it
		IF(.NOT.PFIXED(J))THEN
		    PFIXED(J) = .TRUE.
* Loop back if find new minimum or step not big enough
		    ILOOP = 0
* Set jth parameter value smaller than bestfit
* Set other parameters and 2nd derivatives to bestfit values
10		    CONTINUE
		    DO II = 1, NPARAM
			PARAM(II) = PMIN(II)
			CURV(II) = BCURV(II)
		    END DO
		    PARAM(J) = PARAM(J) - PINC(J)
* Trap parameters less than zero
		    IF(PARAM(J).LT.0.)PARAM(J) = 0.
* Search for values at minimum
		    CALL MINIMIZE(NPARAM, PARAM, CURV, PFIXED, SMIN,
     &				  PRFUNC, NX, NY, RDATA, STATUS)
* Check if new minimum found
		    IF(SMIN.LE.STATMIN .AND. PARAM(J).NE.0.)THEN
			DO K = 1, NPARAM
			    PMIN(K) = PARAM(K)
			END DO
			STATMIN = SMIN
			ILOOP = ILOOP + 1
			IF(ILOOP .LT. 10)GO TO 10
			PARAM(J) = PARAM(J) - ESTERR(J)
			CALL MINIMIZE(NPARAM, PARAM, CURV, PFIXED,
     &			  SMIN, PRFUNC, NX, NY, RDATA, STATUS)
		    END IF
* Use quadratic fit to find intersection of surface with requested value
		    SFIT = STATMIN+DELS(J)
		    CALL QUADFIT(PMIN(J), STATMIN, PARAM(J), SMIN, SFIT,
     &				 A, B, C, CLO, CHI, STATUS)
		    IF(STATUS .NE. 0) THEN
			WRITE(*,*)'SRCHMIN error fitting -ve bar',J
			WRITE(9,*)'SRCHMIN error fitting -ve bar',J
			STATUS = 0
		    END IF
* Don't allow parameter to go -ve
		    CLO = MAX(CLO, 0.0)
* If quadratic fitting involved large extrapolation then try again
		    IF(PARAM(J)-CLO.GT.0.5*PINC(J) .AND. STATUS.EQ.0)THEN
			ILOOP = ILOOP + 1
			PINC(J) = PMIN(J) - CLO
			IF(PINC(J).EQ.0.0)PINC(J) = ESTERR(J)
			IF(ILOOP .LT. 10)GO TO 10
		    END IF
25		    CONTINUE
		    PLO(J) = CLO
* Reset step length using value just found
		    PINC(J) = PMIN(J) - CLO
		    IF(PINC(J).EQ.0.0)PINC(J) = ESTERR(J)
* Now step in other direction
* Loop back if new minimum found or step not big enough
		    ILOOP = 0
* Set jth parameter value larger than bestfit
* Set other parameters and 2nd derivatives to bestfit values
30		    CONTINUE
		    DO II = 1, NPARAM
			PARAM(II) = PMIN(II)
			CURV(II) = BCURV(II)
		    END DO
		    PARAM(J) = PARAM(J) + PINC(J)
* Search for values at minimum
		    CALL MINIMIZE(NPARAM, PARAM, CURV, PFIXED, SMIN,
     &			PRFUNC, NX, NY, RDATA, STATUS)
* Check if new minimum
		    IF(SMIN.LE.STATMIN .AND. PARAM(J).NE.0.)THEN
			DO K = 1, NPARAM
			    PMIN(K) = PARAM(K)
			END DO
			STATMIN = SMIN
			ILOOP = ILOOP + 1
			IF(ILOOP .LT. 10)GO TO 30
			PARAM(J) = PARAM(J) + ESTERR(J)
			CALL MINIMIZE(NPARAM, PARAM, CURV, PFIXED,
     &			  SMIN, PRFUNC, NX, NY, RDATA, STATUS)
		    END IF
* Use quadratic fit to calculate intersection of surface with requested
		    SFIT = STATMIN+DELS(J)
		    CALL QUADFIT(PMIN(J), STATMIN, PARAM(J), SMIN, SFIT,
     &				 A, B, C, CLO, CHI, STATUS)
		    IF(STATUS .NE. 0) THEN
			WRITE(9,*)'SRCHMIN error fitting -ve bar',J
			WRITE(*,*)'SRCHMIN error fitting -ve bar',J
			STATUS = 0
		    END IF
* If quadratic fitting involved large extrapolation then try again
		    IF(CHI-PARAM(J).GT.0.5*PINC(J) .AND. STATUS.EQ.0)THEN
			PINC(J) = CHI - PMIN(J)
			IF(PINC(J).EQ.0.0)PINC(J) = ESTERR(J)
			ILOOP = ILOOP + 1
			IF(ILOOP .LT. 10)GO TO 30
		    END IF
		    PHI(J) = CHI
* Unfix parameter
45		    CONTINUE
		    PFIXED(J) = .FALSE.
		END IF
	    END DO
* Finally set model to best fit before returning
	    STATMIN = CSTAT(PARAM, PRFUNC, NX, NY, RDATA)
	END IF
99999	CONTINUE
	END

*+SUMANN	Finds background rate using median of octants
	SUBROUTINE SUMANN(NX, NY, ARRAY, MX, MY, RIN, ROUT,
     &	 NOZAP, SIGMA, QBAD, AVGBGR, AREA)
	INTEGER NX, NY          !input	Dimensions of ARRAY
	REAL ARRAY(NX, NY)      !input	Data array.
	INTEGER MX, MY          !input	Position of centre of circles.
	REAL RIN, ROUT          !input	Radii of boundaries of annulus (pixels)
	LOGICAL NOZAP		!input	If .true. uses all pixels, else...
	REAL SIGMA(NX,NY)	!input	avoids pixels zerod here
	INTEGER QBAD		!in/out	May set following bit values:
				!	1=low bgrd, 2=large err, 4=slope.
	REAL AVGBGR             !output	Average uncorrected background rate.
	REAL AREA		!output	No of pixels used for estimate
*-Author	Clive Page	1990-Oct-15
* Use all pixels, don't exclude those near earlier detections. CGP 1990-NOV-5
*
	INTEGER MINX, MAXX, MINY, MAXY, IY, IX, NINOCT(0:7), I, NTOTAL,
     &		NCRIT, NOCT, N, BIT2, ABSDY
	REAL RINSQ, ROUTSQ, DSQ, YSQ, SUMOCT(0:7), TEMP(8), ERR, DELTA
	LOGICAL USEALL
*
	USEALL = .TRUE.
100	CONTINUE
*Initialise octant sums
	DO I = 0, 7
	    NINOCT(I) = 0
	    SUMOCT(I) = 0.0
	END DO
*Compute size of square around the outer circle, limited to edge of ARRAY
	MINX = MAX(1.0, MX-ROUT)
	MAXX = MIN(NX, MX+INT(ROUT+1.0))
	MINY = MAX(1.0, MY-ROUT)
	MAXY = MIN(NY, MY+INT(ROUT+1.0))
	RINSQ = RIN**2
	ROUTSQ = ROUT**2
*Accumulate counts and no added in eight roughtly equal octants
	DO IY = MINY, MAXY
	    YSQ = (IY-MY)**2
	    BIT2 = 4*MAX(0, MIN(IY-MY,1))
	    ABSDY = ABS(IY-MY)
	    DO IX = MINX, MAXX
		IF(USEALL .OR. (SIGMA(IX,IY) .NE. 0)) THEN
		    DSQ = (IX-MX)**2 + YSQ
		    IF(DSQ.GE.RINSQ .AND. DSQ.LT.ROUTSQ)THEN
		        NOCT = MAX(0, MIN(ABS(IX-MX)-ABSDY,1))
     &			    + 2*MAX(0, MIN(IX-MX,1)) + BIT2
		        NINOCT(NOCT) = NINOCT(NOCT) + 1
		        SUMOCT(NOCT) = SUMOCT(NOCT) + ARRAY(IX,IY)
		    END IF
		END IF
	    END DO
	END DO
*Find total number of values
	NTOTAL = 0
	DO I = 0, 7
	    NTOTAL = NTOTAL + NINOCT(I)
	END DO
*Find median of all octants with at least one sixteenth of the total
*area and a count above zero
	NCRIT = MAX(1, NTOTAL/16)
	N = 0
	AREA = 0.0
	DO I = 0, 7
	    IF(NINOCT(I) .GE. NCRIT .AND. SUMOCT(I).GT. 0.0)THEN
		N = N + 1
		TEMP(N) = SUMOCT(I)/REAL(NINOCT(I))
		AREA    = AREA + REAL(NINOCT(I))
	    END IF
	END DO
*Sort into ascending order:
	IF(N .EQ. 0) THEN
	    AVGBGR = 1.0
	    AREA   = 1.0
	    QBAD   = IOR(QBAD,7)
	    GO TO 999
	END IF
	CALL MEDIAN(N, TEMP, AVGBGR, DELTA)
	ERR = AVGBGR/SQRT(AREA)
*Now set bad quality bits
	IF(AVGBGR .LT. 0.1)     QBAD = IOR(QBAD,1)
	IF(3.0*ERR .GT. AVGBGR) QBAD = IOR(QBAD,2)
	IF(DELTA .GT. 2.5*ERR)  QBAD = IOR(QBAD,4)
999	CONTINUE
	END

*+SUMBOX	Convolves raw data with circular top-hat
	SUBROUTINE SUMBOX(NX, NY, RDATA, RADIUS, SIGMA, AREA)
	INTEGER NX, NY          !input	Dimensions of RDATA and SIGMA
	REAL RDATA(NX,NY)      	!input	Data array
	REAL RADIUS		!input	Radius of box, pixels, max 100.
	REAL SIGMA(NX,NY)      	!output	Convolved result
	REAL AREA		!output	No of pixels summed (away from edges)
*Note SIGMA and RDATA must not share storage
*-Author	Clive Page	1990-Sept-13
	INTEGER MAXR
	PARAMETER (MAXR = 100)
	INTEGER MAXOFF(0:MAXR), I, IX, IY, INTR, JX, JY, NOFF
	REAL RSQ, R
*
	INTR = INT(RADIUS)
	IF(INTR .GT. MAXR) STOP 'SUMBOX error: radius too large'
*Initialise output array
	DO IY = 1, NY
	    DO IX = 1, NX
		SIGMA(IX,IY) = 0.0
	    END DO
	END DO
*Compute X offsets for each Y offset
	RSQ  = RADIUS**2
	AREA = 0.0
	DO I = 0,INTR
	    MAXOFF(I) = INT(SQRT(RSQ - REAL(I**2)))
	    AREA = AREA + 4*MAXOFF(I) + 2
	END DO
	AREA = AREA - 2*MAXOFF(0) - 1
*Spread the counts around only when non-zero to save time
	DO IY = 1, NY
	    DO IX = 1, NX
		IF(NINT(RDATA(IX,IY)) .NE. 0)THEN
		    R = RDATA(IX,IY)
		    DO JY = MAX(1,IY-INTR), MIN(NY,IY+INTR)
			NOFF = MAXOFF(ABS(JY-IY))
			DO JX = MAX(1,IX-NOFF), MIN(NX,IX+NOFF)
			    SIGMA(JX,JY) = SIGMA(JX,JY) + R
			END DO
		    END DO
		END IF
	    END DO
	END DO
	END

*+SUMCIRC	Sums counts in circle around given posn
	SUBROUTINE SUMCIRC(NX, NY, RDATA, CX, CY, RADIUS,
     &		AREA, COUNT)
	INTEGER NX,NY		!Input	Dimensions of data
	REAL RDATA(NX,NY)	!input	Data array
	REAL CX, CY 		!input	Pixel position of peak
	REAL RADIUS		!input	Radius of circle, pixels
	REAL AREA		!output	Pixels in circle
	REAL COUNT		!output	Total counts in circle
*-Author	Clive Page	1990-OCT-8
	INTEGER MINX, MAXX, MINY, MAXY, JX, JY
	REAL RSQ, YOFFSQ
*
	RSQ  = RADIUS**2
	MINX = MAX(1, INT(CX - RADIUS))
	MAXX = MIN(NX, NINT(CX + RADIUS))
	MINY = MAX(1, INT(CY - RADIUS))
	MAXY = MIN(NY, NINT(CY + RADIUS))
	COUNT = 0.0
	AREA  = 0.0
	DO JY = MINY, MAXY
	    YOFFSQ = (JY-CY)**2
	    DO JX = MINX, MAXX
		IF( (JX-CX)**2 + YOFFSQ .LE. RSQ) THEN
		    COUNT = COUNT + RDATA(JX,JY)
		    AREA = AREA + 1.0
		END IF
	    END DO
	END DO
	END

*+SUMPROB	Converts summed counts to log likelihood levels
	SUBROUTINE SUMPROB(NX, NY, LBLOCK, NBX, NBY, BGR, AREA, SIGMA)
	INTEGER NX, NY          !input	Dimensions of SIGMA
	INTEGER LBLOCK          !input	Background blocking factor
	INTEGER NBX, NBY	!input	Dimensions of background array
	REAL BGR(NBX,NBY)     	!input	Mean background levels per block
	REAL AREA		!input	Area of top-hat function
	REAL SIGMA(NX, NY)      !in/out	Inputs sum of counts in sliding box
				!	Returns with log10 likelihood levels
				!	per image
*-Author	Clive Page	1990-OCT-15
	INTEGER IY, JY, KY, LY, IX, JX, KX, LX, N
	REAL A, RLOGA, LEFACT, RLOG10, LOGTPI
	PARAMETER (RLOG10 = 0.43429448)
	EXTERNAL LEFACT
*
*LOGTPI is loge of approx no of independent trials per image
	LOGTPI = LOG(REAL(NX*NY)/AREA)
	KY = 0
	DO IY = 1, NY, LBLOCK
	    KX = 0
	    KY = KY + 1
	    LY = MIN(IY+LBLOCK-1, NY)
	    DO IX = 1, NX, LBLOCK
		KX = KX + 1
		LX = MIN(IX+LBLOCK-1, NX)
		A = MAX(1.0E-3,BGR(KX,KY)) * AREA
		RLOGA = LOG(A)
		DO JY = IY, LY
		    DO JX = IX, LX
			IF(SIGMA(JX,JY) .GT. A) THEN
			    N = NINT(SIGMA(JX,JY))
*Convert SIGMA to log10(likelihood-per-image)
			    SIGMA(JX,JY) = RLOG10 *
     &                        (LEFACT(N) + A - N*RLOGA - LOGTPI)
			ELSE
			    SIGMA(JX,JY) = -1.0
			END IF
		    END DO
		END DO
	    END DO
	END DO
	END

*+TOPHAT	Convolves raw data with top-hat function
	SUBROUTINE TOPHAT(NX, NY, RDATA, RADIUS, LBLOCK, NBX, NBY,
     &   BGRBLK, SIGMA)
	INTEGER NX, NY          !input	Dimensions of RDATA and SIGMA
	REAL RDATA(NX,NY)      	!input	Data array
	REAL RADIUS		!input	Radius of box, pixels, max 100.
	INTEGER LBLOCK          !input	Background blocking factor
	INTEGER NBX, NBY	!input	Dimensions of background array
	REAL BGRBLK(NBX,NBY)	!input	Mean background levels per block
	REAL SIGMA(NX, NY)      !output	log10 likelihood levels per image
*-Author	Clive Page	1990-OCT-15
	REAL AREA
*
	CALL SUMBOX(NX, NY, RDATA, RADIUS, SIGMA, AREA)
	CALL SUMPROB(NX, NY, LBLOCK, NBX, NBY, BGRBLK, AREA, SIGMA)
	END

*+ZAP		Clears likelihood array in region of detected source
	SUBROUTINE ZAP(NX, NY, ARRAY, LOCX, LOCY, RADIUS, ZAPVAL)
	INTEGER NX, NY          !Input	Dimensions of data
	REAL ARRAY(NX, NY)      !in/out	Data array.
	INTEGER LOCX, LOCY      !input	Location of peak
	REAL RADIUS             !input	Radius of circle, pixels
	REAL ZAPVAL		!input	Value to be inserted.
*-Author	Clive Page	1990 SEPT 13
	INTEGER MAXR
	PARAMETER (MAXR = 100)
	INTEGER MAXOFF(0:MAXR), I, INTR, JX, JY, NOFF
	REAL RSQ
*
	INTR = INT(RADIUS)
	IF(INTR .GT. MAXR) STOP 'ZAP error: radius too large'
*Compute X offsets for each Y offset
	RSQ  = RADIUS**2
	DO I = 0,INTR
	    MAXOFF(I) = INT(SQRT(RSQ - REAL(I**2)))
	END DO
	DO JY = MAX(1,LOCY-INTR), MIN(NY,LOCY+INTR)
	    NOFF = MAXOFF(ABS(JY-LOCY))
	    DO JX = MAX(1,LOCX-NOFF), MIN(NX,LOCX+NOFF)
		ARRAY(JX,JY) = ZAPVAL
	    END DO
	END DO
	END
*+SX_CHISQD	Computes chi-squared deviate for given probability and freedom.
	REAL FUNCTION SX_CHISQD(PROB,NFREE)
	REAL PROB
	INTEGER NFREE
*PROB	input	Probability level, e.g. 0.001 for 1 %.
*NFREE	input	Number of degrees of freedom, range 1 upwards.
*SX_CHISQD output	Returns deviate corresponding to the given upper
*			tail area (i.e. probability).
*Accuracy: better than 3 digits over entire range.
*-
*Author	Clive Page	1983-JULY-5
*Standard Fortran-77 version 1984 Sept 25, CGP.
*Algorithm of R.B.Goldstein, Comm. Ass. Comp. Mach. 16,483 (1973)
      REAL C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13,
     & C14, C15, C16, C17, C18, C19, C20, C21
      REAL A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13,
     & A14, A15, A16, A17, A18, A19
      REAL F1, T, F2, SX_GAUSSD
      PARAMETER (C1=1.565326E-3, C2=1.1060438E-3, C3=-6.950356E-3,
     &   C4 =-1.323293E-2,  C5 = 2.277679E-2,  C6 =-8.986007E-3,
     &   C7 =-1.51390E-2,   C8 = 2.530010E-3,  C9 =-1.450117E-3,
     &   C10= 5.169654E-3,  C11=-1.153761E-2,  C12= 1.128186E-2,
     &   C13= 2.607083E-2,  C14=-0.2237368,    C15= 9.780499E-5,
     &   C16=-8.426812E-4,  C17= 3.125580E-3,  C18=-8.553069E-3,
     &   C19= 1.348028E-4,  C20= 0.4713941,    C21= 1.0000886)
      PARAMETER (A1=1.264616E-2, A2=-1.425296E-2, A3=1.400483E-2,
     &   A4 =-5.886090E-3, A5 =-1.091214E-2, A6 =-2.304527E-2,
     &   A7 = 3.135411E-3, A8 =-2.728484E-4, A9 =-9.9699681E-3,
     &   A10= 1.316872E-2, A11= 2.618914E-2, A12=-0.2222222,
     &   A13= 5.406674E-5, A14= 3.483789E-5, A15=-7.274761E-4,
     &   A16= 3.292181E-3, A17=-8.729713E-3, A18= 0.4714045, A19=1.0)

      IF(NFREE .EQ. 1) THEN
	    SX_CHISQD = (SX_GAUSSD(0.5 * PROB))**2
      ELSE IF(NFREE .EQ. 2) THEN
	    SX_CHISQD = -2.0 * LOG(PROB)
      ELSE
	    F1 = 1.0 / NFREE
	    T  = SX_GAUSSD(PROB)
	    F2 = SQRT(F1) * T
	    IF(NFREE .LT. (2+INT(4.0*ABS(T)))) THEN
		SX_CHISQD =
     &   (((((((C1*F2+C2)*F2+C3)*F2+C4)*F2+C5)*F2+C6)*F2+C7)*F1+
     &   ((((((C8+C9*F2)*F2+C10)*F2+C11)*F2+C12)*F2+C13)*F2+C14))*F1+
     &   (((((C15*F2+C16)*F2+C17)*F2+C18)*F2+C19)*F2+C20)*F2+C21
	    ELSE
		SX_CHISQD =
     &   (((A1+A2*F2)*F1+(((A3+A4*F2)*F2+A5)*F2+A6))*F1+
     &   (((((A7+A8*F2)*F2+A9)*F2+A10)*F2+A11)*F2+A12))*F1+
     &   (((((A13*F2+A14)*F2+A15)*F2+A16)*F2+A17)*F2*F2+A18)*F2+A19
	    END IF
	    SX_CHISQD = NFREE * SX_CHISQD**3
      END IF
      END

*+SX_GAUSSD	Returns normal deviate from upper tail area of Gaussian.
	REAL FUNCTION SX_GAUSSD(PROB)
	REAL PROB
*PROB	   input	Upper tail probability, range 0.0 to 0.9999997
*SX_GAUSSD output	Returns normal deviate, +ve if P < 0.5, else -ve.
*-
* Author	Clive Page	1983 July 5
*Standard Fortran-77 version 1984 Sept 25, CGP.
* Algorithm AS111 from J.D.Beasley and S.G.Springer, Appl Stats 26,118 (1977)
      REAL A0, A1, A2, A3, B1, B2, B3, B4, C0, C1, C2, C3, D1, D2, Q,
     &  R
      PARAMETER (A0 =   2.50622823884E0, A1 = -18.61500062529E0,
     &		  A2 =  41.39119773534E0, A3 = -25.44106049637E0,
     &		  B1 =  -8.47351093090E0, B2 =  23.08336743743E0,
     &		  B3 = -21.06224101826E0, B4 =   3.13082909833E0,
     &		  C0 =  -2.78718931138E0, C1 =  -2.29796479134E0,
     &		  C2 =   4.85014127135E0, C3 =   2.32121276858E0,
     &		  D1 =   3.54388924762E0, D2 =   1.63706781897E0)

      Q = PROB - 0.5
      IF(ABS(Q) .LE. 0.42) THEN
	    R = Q**2
	    SX_GAUSSD = -Q * (((A3 * R + A2) * R + A1) * R + A0) /
     &             ((((B4 * R + B3) * R + B2) * R + B1) * R + 1.0)
      ELSE
	    R = PROB
	    IF(Q .GT. 0.0) R = 1.0 - PROB
	    R = SQRT(-LOG(R))
	    SX_GAUSSD = (((C3 * R + C2) * R + C1) * R + C0) /
     &        ((D2 * R + D1) * R + 1.0)
	    IF(Q .GT. 0.0) SX_GAUSSD = -SX_GAUSSD
      END IF
      END
