	program predict

	implicit none

	integer nco, nim
	real cntdn, cntph, rdnoise, elpdn, expt, zp, arcsecppix
	real t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12
	real t13, t14, t15
	real jcps, hcps, kcps, nblcps
	real zpj, zph, zpk, zpnbl
	real apsiz
	character*20 mode, filter, yesno, readmode

	parameter ( jcps = 120.0)
	parameter ( hcps = 300.0)
	parameter ( kcps = 450.0)
	parameter ( nblcps = 110000.0)
	parameter ( zpj = 21.4)
	parameter ( zph = 20.6)
	parameter ( zpk = 20.3)
	parameter ( zpnbl = 18.65)

  10	type *, ' '
	type *, 'IRCAM 1/2 PREDICT Program'
	type *, ' '
	type *, 'Direct imaging (I) or polarimetry (P) mode ?'
	read( 5, '(a)') mode
	call chr_ucase( mode)
	if( mode( 1:1) .eq. 'P') then
	  mode = 'P'
	else
	  mode = 'I'
	end if

	type *, ' '
	type *, 'Filter used in observation (J,H,K,nbL) ? '
	read( 5, '(a)') filter
	call chr_ucase( filter)
	type *, ' '
	type *, 'Readout mode for data (NDR,MNDR) ? '
	read( 5, '(a)') readmode
	call chr_ucase( readmode)
	if( readmode( 1:3) .eq. 'NDR') then
	  rdnoise = 12.0
	else if( readmode( 1:4) .eq. 'MNDR') then
	  rdnoise = 5.0
	else
	  type *, 'Assumed normal NDR readout mode...'
	  rdnoise = 12.0
	end if
	type *, ' '
	type *, 'On-chip exposure time per coadd in seconds ?'
	read( 5, *) expt
	type *, 'Number of coadds per image ?'
	read( 5, *) nco
	type *, 'Number of images in mosaic to be eventually combined ?'
	read( 5, *) nim
	type *, ' '
	type *, 'Pixel scale in arcsec/pixel ?'
	read( 5, *) arcsecppix
	type *, ' '
	type *, 'Aperture size in arcsec for stellar photometry ?'
	read( 5, *) apsiz
	type *, ' '
	if( filter( 1:1) .eq. 'J') then
	  zp = zpj
	  cntdn = jcps*(arcsecppix**2)/(0.62**2)
	else if( filter( 1:1) .eq. 'H') then
	  zp = zph
	  cntdn = hcps*(arcsecppix**2)/(0.62**2)
	else if( filter( 1:1) .eq. 'K') then
	  zp = zpk
	  cntdn = kcps*(arcsecppix**2)/(0.62**2)
	else if( filter( 1:3) .eq. 'NBL') then
	  zp = zpnbl
	  cntdn = nblcps*(arcsecppix**2)/(0.62**2)
	else
	  type *, ' '
	  type *, 'Enter zeropoint for unknown filter selected ? '
	  read( 5, *) zp
	end if
	type *, 'Filter chosen                  = ', filter
	type *, 'Zeropoint being used (1DN/sec) = ', zp
	type *, 'Readout noise/pixel (DN)       = ', rdnoise
	type *, 'DN/pixel/sec in chosen filter  = ', cntdn
	type *, 'DN/pixel/coadd in 1 image      = ', cntdn*expt
	type *, 'DN/pixel/image                 = ', cntdn*expt*nco
	type *, 'DN/pixel/image (post-mosaicing)= ', cntdn*expt*nco*nim
	type *, ' '

	elpdn = 30
	cntph = cntdn*elpdn
	t1 = ( cntph + ( ( rdnoise*elpdn)**2))*nco*nim
	t2 = ( ( sqrt( t1))/elpdn)*sqrt( 2.0)
	t3 = t2/( expt*nco*nim)
	t4 = zp-2.5*log10( 20.0*t3)
	t5 = zp-2.5*log10( 5.0*t3)
	t6 = zp-2.5*log10( 10.0*t3)
	t7 = t4+2.5*log10( arcsecppix**2)
	t8 = t5+2.5*log10( arcsecppix**2)
	t9 = t6+2.5*log10( arcsecppix**2)
	t10 = t7-2.5*log10( 3.1415926*( apsiz/2.0)**2)
	t11 = t8-2.5*log10( 3.1415926*( apsiz/2.0)**2)
	t12 = t9-2.5*log10( 3.1415926*( apsiz/2.0)**2)
	t13 = zp-2.5*log10( t3)
	t14 = t13+2.5*log10( arcsecppix**2)
	t15 = t14-2.5*log10( 3.1415926*( apsiz/2.0)**2)

	type *, 'Photon+Readout noise in final flat-fielded image (DN) = ', t2
	type *, 'Above but /second (DN)                                = ', t3
	type *, ' '
	type *, ' Detection limit (MAGS)'
	type *, '                  1-sigma    5-sigma    10-sigma   20-sigma'
	type *, ' /pixel     = ', t13, t5, t6, t4
	type *, ' /sq arcsec = ', t14, t8, t9, t7
	type *, ' stars      = ', t15, t11, t12, t10
	type *, ' '
	type *, 'Another go (Yes or No) ? '
	read( 5, '(a)') yesno
	call chr_ucase( yesno)
	if( yesno( 1:1) .eq. 'Y') goto 10

	end
