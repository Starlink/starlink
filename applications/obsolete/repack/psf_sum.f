*+PSF_SUM - Sum up the fraction of psf for a radius at some offset
	SUBROUTINE PSF_SUM(IN_RAD,IN_OFF,PSF_FR,STATUS)
	implicit none

* Input:
      	double precision in_rad	    ! Radius (arcmin)
      	double precision in_off	    ! Offset from detector centre (degrees)

* Output:
	double precision psf_fr     ! Interpolated val of psf from psf_a
	integer status

* P McGale Aug 92.
* P McGale May 95 - UNIX mods
*-

*   Local variables:
	integer	rad_low,rad_upp		! Indicies to radius array.
	integer	off_low,off_upp		! Indicies to offset array.
	double precision  val1,val2		! Temporary variables.
	double precision  psf_a(7,5)		! Fraction in radius at offset
	double precision  off(5),rad(7)		! Pointers to psf_a
      	data psf_a/
     &	0.7125357,0.8633258,0.9305329,0.9620864,0.9778211,0.9927243,1.,
     &	0.7217039,0.8600705,0.9137912,0.9428869,0.9610870,0.9832906,1.,
     &	0.6687348,0.8408714,0.9052650,0.9388605,0.9592556,0.9833004,1.,
     &	0.5806848,0.7963527,0.8792177,0.9221230,0.9478834,0.9777881,1.,
     &	0.5397611,0.7731789,0.8652139,0.9130027,0.9416552,0.9747960,1.0/
	data off/0.0,1.0,1.5,2.0,2.25/
	data rad/3.0,6.0,9.0,12.0,15.0,21.0,24.0/

* Data hold the fraction of the psf contained within a circle at some
* offset point on the WFC.  The radii are 3(3)12,21,24 arcmin, and the offsets
* are 0,1,1.5,2,2.25 degrees from the centre of the detector.
* Matrix is good only for survey filters.  Linear intrepolation is used
* to return the appropriate psf fraction for a given radius and offset.


* Check status
	if (status.ne.0) return

* Get lower and upper bounds for offset.
	if (in_off .eq. 0.0)       then
	  off_low = 1
	  off_upp = 1
	else if (in_off .lt. 1.0)  then
	  off_low = 1
	  off_upp = 2
	else if (in_off .lt. 1.5)  then
	  off_low = 2
	  off_upp = 3
	else if (in_off .lt. 2.0)  then
	  off_low = 3
	  off_upp = 4
	else if (in_off .lt. 2.25) then
	  off_low = 4
	  off_upp = 5
	else if (in_off .ge. 2.25) then
	  off_low = 5
	  off_upp = 5
	else
	  write(*,*)'   Error in PSF_SUM: Cannot set source offset.'
	  status = 1
	  return
	endif

* Set lower and upper bounds for radius.
	if (in_rad .le. 3.0)       then
	  rad_low = 1
	  rad_upp = 1
	else if (in_rad .lt. 6.0)  then
	  rad_low = 1
	  rad_upp = 2
	else if (in_rad .lt. 9.0)  then
	  rad_low = 2
	  rad_upp = 3
	else if (in_rad .lt. 12.0)  then
	  rad_low = 3
	  rad_upp = 4
	else if (in_rad .lt. 15.0) then
	  rad_low = 4
	  rad_upp = 5
	else if (in_rad .lt. 21.0) then
	  rad_low = 5
	  rad_upp = 6
	else if (in_rad .lt. 24.0) then
	  rad_low = 6
	  rad_upp = 7
	else if (in_rad .ge. 24.0) then
	  rad_low = 7
	  rad_upp = 7
	else
	  write(*,*)'   Error in PSF_SUM: Cannot set circle radius.'
	  status = 1
	  return
	endif

* Do interpolation.
	call lin_interp(rad(rad_low),psf_a(rad_low,off_low),rad(rad_upp),
     &                 psf_a(rad_upp,off_low),in_rad,val1)
	call lin_interp(rad(rad_low),psf_a(rad_low,off_upp),rad(rad_upp),
     &                 psf_a(rad_upp,off_upp),in_rad,val2)
	call lin_interp(off(off_low),val1,off(off_upp),val2,
     &                 in_off,psf_fr)


      	END


