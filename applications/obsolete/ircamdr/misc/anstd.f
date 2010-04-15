	program anstd

	implicit none

	intege rl1, chr_len
	integer nlines, n, m, j, sta( 20), sto( 20), lstart
	integer nwrd, k, lstat, l, ipos1, ipos2, status
        integer mosflag( 2000), imos, gstart( 2000), gend( 2000)
	integer ngroups, numpix, n2, m2

	real ap1( 2000), ap2( 2000), ap3( 2000)
	real ps( 2000), zp( 2000), am( 2000), actmag( 2000)
	real objmsky( 2000), i3mag( 2000), calzp( 2000), calzpx( 2000)

	real*8 sum, sumsq, mean, variance, std
	real*8 sumx, sumsqx, meanx, variancex, stdx

	character file1*80, dline*132, lines( 2000)*132, file2*80
	character words( 20)*80, all( 20, 2000)*80, objnam( 2000)*30
	character objrest( 2000)*30, filt( 2000)*20, itype*1, pref*20

	logical exists, more

	include 'SAE_PAR'

	status = SAI__OK

	type *, 'Name of photometry file created by stdred : '
	read( 5, '(a)') file1
!	file1 = 'stdred_photometry.results'
	inquire(file=file1, exist=exists)
	if( exists) then
	  open( unit=142, file=file1, status='old')
	else
	  type *, file1
	  type *, 'That file does not exist, quiting'
	  goto 100
	end if
	type *, 'Input prefix for output files : '
	read( 5, '(a)') pref

	l1 = chr_len( pref)
	file2 = pref( 1:l1)//'anstd.results'
	open( unit=143, file=file2, status='unknown')
	open( unit=144, file=pref( 1:l1)//'j.am-zp', status='unknown')
	open( unit=145, file=pref( 1:l1)//'h.am-zp', status='unknown')
	open( unit=146, file=pref( 1:l1)//'k.am-zp', status='unknown')

	more = .true.
	nlines = 0
	do while ( more)
	  nlines = nlines + 1
	  read( 142, '(a)', end=200) dline
	  lines( nlines) = dline
	  call chr_ucase( dline)
	  call chr_fandl( dline, n, m)
	  if( dline( n:n+7) .eq. 'FILENAME') then
	    lstart = nlines + 1
	  end if
	end do
  200	continue
	nlines = nlines - lstart
	type *, 'Number of photometry lines of data in file = ',
     :	  nlines
	close( 142)

	l = 0
	do j = lstart, nlines+lstart-1
	  l = l + 1
	  call chr_dcwrd( lines( j), 20, nwrd, sta, sto, words, lstat)
	  do k = 1, nwrd
	    all( k, l) = words( k)
	  end do
	  call chr_fandl( all( 1, l), n, m)
	  ipos1 = index( all( 1, l), '_')
	  objnam( l) = all( 1, l)( n:ipos1-1)
	  call chr_ucase( objnam( l))
	  ipos2 = index( all( 1, l), '.')
	  objrest( l) = all( 1, l)( ipos1+1:ipos2-1)
	  call chr_ucase( objrest( l))
	  imos = index( objrest( l), '_MOS')
	  if( imos .ne. 0) then
	    mosflag( l) = 1
	  else
	    mosflag( l) = 0
	  end if
	  call chr_fandl( all( 3, l), n, m)
	  ipos1 = index( all( 3, l), '.')
	  filt( l) = all( 3, l)( n:ipos1-1)
	  call chr_ucase( filt( l))
	  call chr_ctor( all( 4, l), ap1( l), status)
	  call chr_ctor( all( 5, l), ap2( l), status)
	  call chr_ctor( all( 6, l), ap3( l), status)
	  call chr_ctor( all( 7, l), ps( l), status)
	  call chr_ctor( all( 8, l), zp( l), status)
	  call chr_ctor( all( 9, l), am( l), status)
	  call chr_ctor( all( 10, l), actmag( l), status)
	  call chr_ctor( all( 11, l), objmsky( l), status)
	  call chr_ctor( all( 12, l), i3mag( l), status)
	  call chr_ctor( all( 13, l), calzp( l), status)
	  call chr_ctor( all( 14, l), calzpx( l), status)
	end do
	nlines = l - 1

	j = 0
	ngroups = 1
	do while ( j .le. nlines)
	  j = j + 1
	  if( j .gt. nlines) goto 300
	  gstart( ngroups) = j
	  do while ( mosflag( j) .eq. 0)
	    j = j + 1
	  end do
	  gend( ngroups) = j - 1
!	type *, gstart( ngroups), gend( ngroups)
	  gstart( ngroups+1) = j
	  gend( ngroups+1) = j
!	type *, gstart( ngroups+1), gend( ngroups+1)
	  ngroups = ngroups + 2
	end do
  300	continue
	ngroups = ngroups - 1
	type *, 'Number groups = ', ngroups

	write( 143, '(a)') 'Known standard stars : FS or UKIRT'
	write( 143, '(a)') 'Source    Filter  Type     Airmass     '//
     :	  'ZP        Std            ZPX       Stdx'
	do j = 1, ngroups
	  if( actmag( gstart( j)) .ne. -99.99) then
	    sum = 0.0d0
	    sumsq = 0.0d0
	    sumx = 0.0d0
	    sumsqx = 0.0d0
	    numpix = gend( j)-gstart( j)+1
	    do k = gstart( j), gend( j)
	      sum = sum + dble(calzp( k))
	      sumsq = sumsq + dble(calzp( k))**2
	      sumx = sumx + dble(calzpx( k))
	      sumsqx = sumsqx + dble(calzpx( k))**2
	    end do
	    mean = sum/numpix
	    variance = ( sumsq - numpix*mean**2)
            if( numpix .eq. 1 .or. variance .lt. 0.0d0) THEN
              variance  =  0.0d0
            else
              variance  =  variance/( numpix-1.0d0)
            end if
            std = dsqrt( variance)
	    meanx = sumx/numpix
	    variancex = ( sumsqx - numpix*meanx**2)
            if( numpix .eq. 1 .or. variancex .lt. 0.0d0) THEN
              variancex  =  0.0d0
            else
              variancex  =  variancex/( numpix-1.0d0)
            end if
            stdx = dsqrt( variancex)
	    if( mosflag( gstart( j)) .eq. 0) then
	      itype = 'I'
	    else
	      itype = 'M'
	    end if
	    call chr_fandl( objnam( gstart( j)), n, m)
	    if( m .lt. 12) m = 12
	    call chr_fandl( filt( gstart( j)), n2, m2)
	    if( m2 .lt. 6) m2 = 6
	    write( 143, '(a,a,a,a,a,2f10.3,a,f7.3,a,f10.3,a,f7.3)')
     :	      objnam( gstart( j))( n:m),
     :	      filt( gstart( j))( n2:m2),'  ',itype,'  ',
     :	      am( gstart( j)), sngl( mean), '    ', sngl( std),'    ',
     :	      sngl( meanx), '    ',sngl( stdx)
	    if( filt( gstart( j)) .eq. 'J') then
	      if( itype .eq. 'I') then
	        if( sngl( std) .le. 0.1) then
	          write( 144, '(5f10.3)') am( gstart( j)),
     :	            sngl( mean), sngl( std), sngl( meanx), sngl( stdx)
	        else
	          write( 144, '(a, 5f10.3)') '**  ',am( gstart( j)),
     :	            sngl( mean), sngl( std), sngl( meanx), sngl( stdx)
	        end if
	      end if
	    end if
	    if( filt( gstart( j)) .eq. 'H') then
	      if( itype .eq. 'I') then
	        if( sngl( std) .le. 0.1) then
	          write( 145, '(5f10.3)') am( gstart( j)),
     :	            sngl( mean), sngl( std), sngl( meanx), sngl( stdx)
	        else
	          write( 145, '(a, 5f10.3)') '**  ',am( gstart( j)),
     :	            sngl( mean), sngl( std), sngl( meanx), sngl( stdx)
	        end if
	      end if
	    end if
	    if( filt( gstart( j)) .eq. 'K') then
	      if( itype .eq. 'I') then
	        if( sngl( std) .le. 0.1) then
	          write( 146, '(5f10.3)') am( gstart( j)),
     :	            sngl( mean), sngl( std), sngl( meanx), sngl( stdx)
	        else
	          write( 146, '(a, 5f10.3)') '**  ',am( gstart( j)),
     :	            sngl( mean), sngl( std), sngl( meanx), sngl( stdx)
	        end if
	      end if
	    end if
	  end if
 	end do

	close( 143)
	close( 144)
	close( 145)
	close( 146)

  100	continue
	end
