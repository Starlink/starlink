*+TIMEXP - Calculate exposure correction array
      SUBROUTINE TIMEXP(SOURCE,HKR,ASPECT,STINW,EXP,ATC,NEXP,N1,STATUS)
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'CONSTANTS.INC'
      include 'HKR_BUFFER.INC'
      INCLUDE 'ASR_EXPATT.INC'
      INCLUDE 'EXPOS_DEF.INC'

* Input:
      LOGICAL		SOURCE
      INTEGER		N1
      REAL		STINW(3)
      RECORD 		/HKR_BUFFER/    HKR
      RECORD 		/ATTSTRUC/      ASPECT
      RECORD 		/EXPOS_DEF/     EXP

* Output:
      INTEGER		NEXP(N1)
      INTEGER   	STATUS
      REAL    		ATC(N1)
* M Denby
* Modified Jan 92, P.McGale to take account of detector/source circle
* intersection area, and fraction of psf contained in source circle
* at any offset.
* Modified Jul 92, P. McGale not to use psf corrections if analysing
* a background region. (Employs cirovl to work out intersection of 'source'
* and detector FOV when 'source' at FOV edge).
* Modified Aug 92, P. McGale does psf fraction by linear interpolation,
* after NAG routine E01ACF deletion.
* Mods for S3 - P. McGale - Sept 92.
* Mods for config - P. McGale - Oct 92.
* P McGale May 95  UNIX mods
*-
*    Function declarations :
      REAL		HKR_DTCP		! Dead time factor at spot time
      real		cirovl

*    Local variables :
      INTEGER   	NW              ! counter of valid time windows
      INTEGER   	IASTAT          ! Status from aspect subroutine
      INTEGER		I, J		! Dummy counters
      INTEGER   	NT      	! counters over exposure array
      INTEGER		NC		! exposure step counter
      INTEGER		SSTEP, ESTEP	! Step range in a window
      INTEGER		IZ, IFMT	! Zoom, format flags
      LOGICAL           ZOOM		! WFC in zoom mode ?
      CHARACTER*80 	CFILT		! Filter string designator
      REAL		TRIG
      REAL	   	ENER		! Filter characteristic energy
      REAL	   	TFRAC		! Step after dead time corr
      REAL		GFRAC
      REAL		THETA
      REAL	   	DTC		! Dead Time factor
      REAL	   	VIG		! Vignetting at a point
      REAL	   	CAL_TVIGN	! WFC vignetting fn.
      REAL		S3, C3		! SIN COS of Roll Euler
      REAL		DSAZ, DSEL	! Euler corrections
      REAL		WAZ, WEL	! WFC coords
      REAL		S_WAZ, S_WEL	! WFC coords temporary save
      REAL		IRIS2, RAD2
      REAL		RDET, RBEAM, RC, ROV
      DOUBLE PRECISION	DELTA		! Step size (MJD)
      DOUBLE PRECISION	TSTART, TEND, TMEAN
      DOUBLE PRECISION	ATTST, ATTET	! Aspect rec windows
      DOUBLE PRECISION	TBASE		! Base MJD for the obs
      DOUBLE PRECISION	TSCAN		! Time from scan reference
      DOUBLE PRECISION	CELI(2)		! Cel coords of exp image centre
      DOUBLE PRECISION	SCANP(2)	! Scan coords of pointing
      DOUBLE PRECISION	IMGP(2)		! Exp image coords of pointing
      DOUBLE PRECISION	ST2C(3,3)	! DCM scan to Cel
      DOUBLE PRECISION	C2I(3,3)	! DCM celestials to exp image
      DOUBLE PRECISION	I2C(3,3)	! DCM exp image to celestials
      DOUBLE PRECISION	ST2I(3,3)	! DCM scan to exp image
      DOUBLE PRECISION	I2ST(3,3)	! DCM exp image to Scan
      DOUBLE PRECISION	XAZ, XEL	! Exposure pixel coords
      DOUBLE PRECISION	AZST, ELST
      DOUBLE PRECISION	V(3)
      integer		icstat		! Status of return from calpsf
      real		n_bins1,n_bins2	! Produce PSF array of n_bins x n_bins
        parameter (n_bins1=20.0,n_bins2=30.0)
      real	        a_frac		! Area intersect fraction
      real              res		! Resolution for PSF
      DOUBLE PRECISION 	dbmjd		! MJD for master cal file.
      DOUBLE PRECISION 	psf_frac       	! Interpolated val of psf.
      DOUBLE PRECISION	radius,offset	! Vaules to interpolate for.
*
* If the flux from a point in the sky would give rise to C cts/sec on axis
* then in a perfect (ie no dead time) system in the interval Tn to Tn + Dt
* the counts accumulated are Cn = C.Dt.An/Dn
* where
* An = Vignetting function (<= 1)
* Dn = Dimensionless dead time factor (>= 1)
*
* Thus
*      Sum{Cn}   =  C.Dt.Sum{An/Dn}
* and
*        C    	 =  Sum{Cn} / (Dt.Sum{An/Dn})


*   Check status
      IF (STATUS.NE.0) RETURN

*   Initialize cummulative arrays used in exp calculation
      DO NT = 1, N1
        ATC(NT) = 0
	NEXP(NT) = 0
      ENDDO

* Initialise time zero and step size
      DELTA = EXP.TDELT/86400.D0
      TBASE = DBLE(EXP.BMJD) + (EXP.BUTC/86400.D0)
      ATTST = 0.D0
      ATTET = 0.D0
      S3 = SIN(STINW(3))
      C3 = COS(STINW(3))
      DSAZ =  STINW(1)*C3 + STINW(2)*S3
      DSEL = -STINW(1)*S3 + STINW(2)*C3
      RDET  = EXP.IRIS
      RBEAM = EXP.DAZ
      radius=DBLE(rbeam*RTOD*60.0)

* Calculate DCM from celestials to locals (and vv) at ref point
      CELI(1) = EXP.FRA
      CELI(2) = EXP.FDEC
      CALL AX_DMAT (CELI,EXP.ROLL,C2I,I2C)

* Find energy of the filter
      CALL CAL_FILT_INFO (EXP.FILT,CFILT,ENER,STATUS)
      if (index(CFILT,'Al') .gt. 0 ) then
	write(*,*)
     &      '   WARNING: PSF matrix being used for non-survey filter.'
      endif

*   Loop through all time windows.
      DO NW = 1, EXP.NPAIR
	 ZOOM = BTEST(EXP.CONF(NW),5)
	 IFMT = IBITS(EXP.CONF(NW),3,2)
	 IZ   = IBITS(EXP.CONF(NW),5,1)

*      Loop through aspect file in coarse time steps searching for
* 	overlap with the exposure region

	 SSTEP = INT((EXP.SW(NW) - TBASE)/DELTA) + 1
	 ESTEP = INT((EXP.EW(NW) - TBASE)/DELTA) + 1

         DO NC = SSTEP, ESTEP
	    TSTART = MAX(EXP.SW(NW), (NC-1)*DELTA+TBASE)
	    TEND   = MIN(EXP.EW(NW), NC*DELTA+TBASE)
	    TMEAN  = (TSTART+TEND)/2.
	    TFRAC  = (TEND - TSTART)*86400.D0

*         If nec refresh aspect record
            IF (TSTART.LE.ATTST .OR. TSTART.GT.ATTET) THEN
	       CALL EXPOS_S_ATT(TSTART,ASPECT,ST2C,ATTST,ATTET,IASTAT)
            ENDIF

* If the aspect is bad skip this step (can't be any events for it)
	    IF (IASTAT .LE. 0) THEN

	       CALL AX_DONMXM (ST2C, C2I, ST2I)
	       DO J = 1,3
		 DO I = 1,3
		   I2ST(I,J) = ST2I(J,I)
		 ENDDO
	       ENDDO

* Get pointing direction in exp image local coords + test proximity
	      TSCAN = (TSTART - ATTST)*86400.D0
* First get the postn of the ST axis in the ST ref frame
	      SCANP(1) = TSCAN*ASPECT.SCAN_RATE_AZ
              SCANP(2) = TSCAN*ASPECT.SCAN_RATE_EL
* Then add on the position of the WFC axis in the ST frame
	      SCANP(1) = SCANP(1) + DSAZ
	      SCANP(2) = SCANP(2) + DSEL

              CALL DTRANSQ(SCANP(1),SCANP(2),ST2I,IMGP(1),IMGP(2),V)
              IF (IMGP(1).GT.PI) IMGP(1) = IMGP(1) - TWOPI

	      IF ((ABS(IMGP(1)) .LT. (EXP.DAZ+EXP.IRIS+.5*DTOR)) .AND.
     :		  (ABS(IMGP(2)) .LT. (EXP.DEL+EXP.IRIS+.5*DTOR))) THEN

*    Find the dead time correction to be applied to this time step
	        TFRAC = TFRAC*HKR_DTCP(HKR,IFMT,IZ,TSTART,STATUS)

*    Calculate which timebin we are in (TBASE = start MJD of obsvn.)
                NT = 1 + ((TMEAN - TBASE)*86400./EXP.DUR)*N1

*   Loop through the exposure grid - calculate the distance from
*   current pointing of the grid point
		IF (NT .LE. N1) THEN

*   Check if ref point is within the FOV
		  CALL DTRANSQ(0.D0,0.D0,I2ST,AZST,ELST,V)
	  	  IF (AZST.GT.PI) AZST = AZST - TWOPI
	  	  AZST = AZST - ASPECT.SCAN_RATE_AZ*TSCAN
		  ELST = ELST - ASPECT.SCAN_RATE_EL*TSCAN

		  WAZ = -(AZST*S3 + ELST*C3 - STINW(2))
		  WEL =  (AZST*C3 - ELST*S3 - STINW(1))
		  S_WAZ = WAZ
		  S_WEL = WEL

		  RAD2 = (WAZ**2 + WEL**2)
		  RC = SQRT(RAD2)

* Get fraction of psf included in source circle to correct exposure.
		  if (source) then
		    offset = DBLE(rc*RTOD)
		    call psf_sum(radius,offset,psf_frac,status)
		  else
		    psf_frac = 1.0
		  endif

* If source circle not wholly contained in detector FOV, then
* have to calculate area of overlap.

		  if (rc .gt. (rdet+rbeam)) then
		    a_frac=0.0
		  else if (rc .lt. (rdet-rbeam)) then
	            a_frac=1.0
	          else
		    if (source) then
		      if (rbeam .le. 10.0*DTOR/60.0) res=2.0*rbeam/n_bins1
		      if (rbeam .gt. 10.0*DTOR/60.0) res=2.0*rbeam/n_bins2
		      if ( status .eq. 0 ) icstat=0
	              dbmjd=tbase
		      if (dbmjd .lt. 48050.0) dbmjd=48050.0
		      call s2_cal_psft2d(dbmjd,EXP.FILT,ener,waz,wel,
     &                                 rdet,rbeam,res,0,0,a_frac,icstat)
		      a_frac=a_frac/psf_frac
		    else
		      a_frac = cirovl(rdet,rbeam,rc)
		    endif
		     if (a_frac .gt. 1.0) a_frac=1.0
		  endif

		  IF (RAD2 .LT. (RDET+RBEAM)**2) THEN
* If the source centre is outside the detector nudge it inside
		    IF (RAD2 .GT. RDET**2) THEN
		      CALL NUDGE(RBEAM,S_WAZ,S_WEL)
		    ENDIF
	  	    VIG = MAX(CAL_TVIGN(EXP.SW(NW),EXP.FILT,ZOOM,
     :					    ENER,S_WAZ,S_WEL,STATUS),0.)
		    ATC(NT) = ATC(NT) + VIG*TFRAC*a_frac*psf_frac
		    NEXP(NT) = NEXP(NT) + 1
		  ENDIF

	       ENDIF
	    ENDIF

	  ENDIF
        ENDDO
      ENDDO


999   IF (STATUS.NE.0) THEN
	 WRITE(*,*) '   Error in TIMEXP'
      END IF

      END
