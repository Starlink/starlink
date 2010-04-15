	program rotoff

	implicit none

	integer lun, lun2, l1, chr_len

	real tilt, x, y, xval, yval

	character*80 infil

	logical more

        lun = 142
        lun2 = 143

	type *, 'Give name of offset file to be rotated : '
	read( 5, '(a)') infil
	l1 = chr_len( infil)
	open( unit=lun, file=infil( 1:l1), status='old')

	open( unit=lun2, file='rotoff.dat', status='new')

	type *, 'Current tile for IRCAM3 images is -2 deg.'
	type *, 'Enter tilt in degrees (clockwise) : '
	read( 5, *) tilt
	tilt = -1.0*tilt

	more = .true.
	do while ( more)
	  read( lun, *, end=20) x, y
	  xval = x*cosd( tilt) + y*sind( tilt)
	  yval = -x*sind( tilt) + y*cosd( tilt)
	  write( lun2, '(2f10.2)') xval, yval
	end do
  20	close( lun)
	close( lun2)

	end
