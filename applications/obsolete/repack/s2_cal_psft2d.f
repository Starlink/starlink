*+S2_CAL_PSFT2D	integrated PSF over a set of defined regular rectangular bins
       SUBROUTINE S2_CAL_PSFT2D(DMJD,IFILT,ENERGY,AZ,EL,RDECT,RSOURCE,
     &                           RES,OAZ,OEL,ASUM,ISTAT)

       DOUBLE PRECISION DMJD
       INTEGER IFILT,ISTAT,N_AZ,N_EL
       REAL AZ,EL,DELTA_AZ,DELTA_EL,ENERGY,OAZ,OEL
       real rdect,rsource,asum,sep,rd,rs
*DMJD	    input	date required
*IFILT	    input	filter number (not used at present)
*ENERGY	    input	energy (eV)
*AZ	    input	azimuth of source/PSF centre (radians)
*EL	    input	elevation of source/PSF centre (radians)
*RDECT      input       Radius of detector FOV (radius)
*RSOURCE    input	Radius of circle around source (radius)
*RES        input	resolution of PSF grid array (rads)
*OAZ        input       offset of PSF centre from grid centre in az (rads)
*OEL        input       offset of PSF centre from grid centre in el (rads)
*ASUM	    output      PSF sum for area intersection
*ISTAT	    in/out	status return
*DELTA_AZ   local 	grid spacing in azimuth (radians)
*N_AZ       local	number of grid positions in azimuth
*DELTA_EL   local       grid spacing in elevation (radians)
*N_EL       local       number of grid positions in elevation
*DAZ        local	offset from PSF centre in azimuth
*DEL        local	offset from PSF centre in elevation
*
* Paul McGale Jan. 92
* Modified version of CAL_PSFT2D by AES that calculates the PSF for
* those bins that have their centres in the area formed by the intersection
* of the detector FOV and a circle around a source, for the case where
* the source circle is not wholly contained in the FOV.
*-
	PARAMETER (NCMAX=10)
	CHARACTER*32 FUNC_NAME
	REAL COEFF(NCMAX),CIN_PSF
	INTEGER NCOEFF
C
	IF(ISTAT.NE.0) RETURN
* Trap obvious cases.
	sep = sqrt(az**2+el**2)
	if (sep .gt. (rdect+rsource)) then
	   asum = 0.
	   return
	elseif (sep .lt. (rdect-rsource)) then
	   asum=1.0
	   return
	endif

	delta_az = res
	delta_el = res
	n_az     = int(2.0*rsource/delta_az + 0.5)
	n_el     = int(2.0*rsource/delta_el + 0.5)

C Get data from MCF
        CALL CIN_SET_PSF(DMJD,IFILT,
     +	ENERGY,AZ,EL,NCMAX,COEFF,NCOEFF,FUNC_NAME,ISTAT)
C
C Integration step within samples that overlap the central peak
C order 5 arc seconds
	RPEAK=0.5*SQRT(DELTA_AZ**2+DELTA_EL**2)+6.E-4
	PEAKSTEP=2.42E-5
C Integration step in wings order 1 arc minute
	WINGSTEP=2.91E-4
	asum=0.0
        DO I=1,N_EL
             DEL=(REAL(I)-1.0-(REAL(N_EL)-1.0)/2.0)*DELTA_EL-OEL
             DO J=1,N_AZ
                      DAZ=(REAL(J)-1.-(REAL(N_AZ)-1.)/2.)*DELTA_AZ-OAZ
* Only do calculation for those bin centres that lie within intersection
* area.
* RS  - distance of bin centre to centre of source circle.
* RD  - distance of bin centre to centre of detector.
                      DAZL=DAZ-0.5*DELTA_AZ
                      DELL=DEL-0.5*DELTA_EL
		      RD=SQRT((AZ+DAZL)**2+(EL+DELL)**2)
		      RS=SQRT(DAZL**2+DELL**2)
		      if ( rs.le. rsource .and. rd .le. rdect) then  ! Got one
		        R=SQRT(DAZ**2+DEL**2)
			IF(R.GT.RPEAK) THEN
				NX=DELTA_AZ/WINGSTEP
				NY=DELTA_EL/WINGSTEP
			ELSE
				NX=DELTA_AZ/PEAKSTEP
				NY=DELTA_EL/PEAKSTEP
			ENDIF
			NX=MAX(NX,1)
			NY=MAX(NY,1)
			DX=DELTA_AZ/NX
			DY=DELTA_EL/NY
                        TXYS=0.0
C
C      Integrate over specified area in y then x directions.
C
                        DO IX=1,NX
                          X=DAZL+(FLOAT(IX)-0.5)*DX
                          DO IY=1,NY
                            Y=DELL+(FLOAT(IY)-0.5)*DY
			    T=CIN_PSF(FUNC_NAME,NCOEFF,COEFF,AZ,EL,X,Y,ISTAT)
                            TXYS=TXYS+T*DX*DY
                          END DO
                        END DO
C Take the absolute value of the probability
	   	        asum = asum + ABS(TXYS)
	              endif
              END DO
        END DO
C
	IF(ISTAT.NE.0) THEN
		WRITE(*,*) '   error in S2_CAL_PSFT2D'
	ENDIF
	END

