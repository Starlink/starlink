	program spiral

	implicit none

	integer lun, numoff, j, numside, status, l1, chr_len
	parameter ( numoff = 225)

	real xoffsets( numoff), yoffsets( numoff), distance
	real tilt, x, y, xval, yval

	character*80 outfil, act*80

        lun = 142

        call ab_translate_env( '$LIRCAMDIR', act, status)
        l1 = chr_len( act)
        outfil = act( 1:l1)//'/spiral.off'

	open( unit=lun, file=outfil, status='old', err=999)

	do j = 1, numoff
	  read( lun, *) xoffsets( j), yoffsets( j)
	end do

	close( lun)

	type *, 'Enter offset between images in arcseconds : '
	read( 5, *) distance
	type *, 'Enter tilt in degrees (clockwise) : '
	read( 5, *) tilt

  10	type *, 'Enter number of images (ODD) per side (1-15) : '
	read( 5, *) numside

	if( numside .eq. 1 .or.
     :	    numside .eq. 3 .or.
     :	    numside .eq. 5 .or.
     :	    numside .eq. 7 .or.
     :	    numside .eq. 9 .or.
     :	    numside .eq. 11 .or.
     :	    numside .eq. 13 .or.
     :	    numside .eq. 15) then
	  goto 15
	else
	  goto 10
	end if

  15	type *, 'Enter name for output offset file : '
	read( 5, '(A)') outfil

	open (unit=lun, file=outfil, status='new')

	do j = 1, numside*numside

	  x = xoffsets( j)*distance
	  y = -1*yoffsets( j)*distance
	  xval = x*cosd( tilt) + y*sind( tilt)
	  yval = x*sind( tilt) - y*cosd( tilt)

	  write( lun, '(2f10.2)') xval, yval

	end do

	close( lun)

	goto 20

  999	type *, 'Error opening offset file $LIRCAMDIR/spiral.off'

  20	continue

	end
