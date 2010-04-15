*+RE_EVSRCH event files touched by circle of arbitary centre and radius
	PROGRAM RE_EVSRCH
	implicit none

* Global constants
	include 'CONSTANTS.INC'

* P.McGale (pam@star.le.ac.uk) Mar. 95
* P.McGale Add par calls       Apr. 95
*-

* Local constants:
        integer   nlon, nlat                   	! # ecliptic maps in sky
          parameter ( nlon = 192, nlat = 90 )
        integer   nbins(nlat)   		! # maps per circumference
          data nbins /12,2*24,4*48,8*96,60*192,8*96,4*48,2*24,12/

* Local variables:
	double precision delo_c, dela_c
	double precision dsra, dsdec
*
	real sra				! Search RA   (rads)
	real sdec				! Search Dec  (rads)
	real rad				! Search radius (degs)
	real elo_c				! Ecliptic Long srch cent
	real ela_c				! Ecliptic Lat  srch cent
	real rlam_top, rlam_bot			! Top & bot lat. ec. maps
	real rlom_lh, rlom_rh			! L & RH long. ec. maps
	real relom
	real delta				! Ec. long steps
*
	integer elam				! Ec. lat. map #
	integer elam_top, elam_bot
	integer elom				! Ec. long. map #
	integer elom_lh, elam_rh
	integer nmaps				! # maps affected
	integer i
	integer status
*
	character*7 map(nlon*nlat)		! Map reference
*
	logical search				! Search for Long. maps
*
	data status/0/


	call par_cmdl(' ', status)

	write(*,*)
	write(*,*) '   RE_EVSRCH, version 010595'
	write(*,*)

* Get search RA, Dec and Radius
	call par_get0r('SRA search RA (deg/hms)', sra, status)
	call par_get0r('SDEC search Dec (deg/dms)', sdec, status)
	call par_get0r('RAD  radius (deg)', rad, status)

* Convert to ecliptic.
	dsra  = dble(sra  * dtor)
	dsdec = dble(sdec * dtor)
	call cel2ec(dsra, dsdec, delo_c, dela_c)
	elo_c = real(delo_c)
	ela_c = real(dela_c)
	elo_c = elo_c * rtod
	ela_c = ela_c * rtod

* Work out Denby sky grid boundary extremities affected by search circle
* Latitudes
	rlam_bot = ela_c - rad + 90.0
	if (rlam_bot .lt. 0.0) then
	  rlam_bot = 1.0
	else
	  if (mod(2.0,(rlam_bot/2.0)) .eq. 0.0) then   ! On a grid boundary
            rlam_bot = (rlam_bot / 2.0) + 1.0
	  else
	    rlam_bot = int((rlam_bot / 2.0)) + 1.0
	  endif
	endif
	rlam_top = ela_c + rad + 90.0
	if (rlam_top .gt. 180.0) then
	  rlam_top = 90
	else
	  if (mod(2.0,(rlam_top/2.0)) .eq. 0.0) then   ! On a grid boundary
            rlam_top = (rlam_top / 2.0) + 1.0
	  else
	    rlam_top = int((rlam_top / 2.0)) + 1.0
	  endif
	endif

	elam_bot = int(rlam_bot)
	elam_top = int(rlam_top)

* Longitudes
	rlom_lh = elo_c - rad
	rlom_rh = elo_c + rad

* For each latitude work out longitude maps.
	nmaps = 0
	do elam=elam_bot,elam_top
	  delta = 360.0 / nbins(elam)
	  search = .TRUE.
	  relom =rlom_lh
	  do while(search)
	    elom = int(relom / delta) + 1
	    if (relom .lt. 0.0 ) elom = elom - 1
	    if (elom .le. 0) elom = elom + nbins(elam)
	    if (elom .gt. nbins(elam)) elom = elom - nbins(elam)
	    nmaps = nmaps + 1
	    write(map(nmaps),'(a1,i3.3,a1,i2.2)') 'x',elom,'y',elam
* Any more maps for this latitude? (Have to reset elom before test.)
	    if (relom .lt. 0.0 ) elom = int(relom / delta) - 1
	    if ((int(rlom_rh / delta) + 1) .gt. elom) then
	      relom = relom + delta
	    else
	      search = .FALSE.
	    endif
	  enddo
	enddo

* Show which event files are needed to merge to cover area requested.
* Open file to hold list of event files found.
	open(10, file='re_evmrg.list', status='unknown')

	write(*,*)
	write(*,30) nmaps
	write(*,'(2x,a7)') (map(i),i=1,nmaps)
	write(10,'(2x,a7)') (map(i),i=1,nmaps)
 30	format('  # event files partially or totally enclosed: ',i5,//,
     +  '  List:')
	close (10)
	write(*,40)
 40	format(/,'List also stored in file re_evmrg.list.',/)


	END


