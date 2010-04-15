	program crestd

	implicit none

	integer k, n, m, n1, m1, chr_len, jj, kk
	integer iobs, ut1, ut2, ut3, icoadds, first
	integer icoadds1, inumb, iobs1, icoaddsd, iobsd
	integer n2, m2, idark
	integer nomit, nobj, iflag

	real exptime, airmass, pixscal, ra1, ra2, ra3
	real dec1, dec2, dec3
	real exptime1, exptimed

	character dline*132, header*132
	character objnam*12, objtyp*9, readout*11, filter*9
	character array*8, mag*5
	character objnam1*12, filter1*9, onam*12
	character objnamd*12, filterd*9
	character*10 dum1, dum2, dum3
	character*80 objects( 100), omit( 100)

	logical more, more2, exists

	open( unit=42, file='crestd.tmp', status='old')
	open( unit=43, file='crestd.icl', status='new')
	open( unit=44, file='header.txt', status='new')

	nobj = 0
	nomit = 0
	inquire (file='objects.list', exist=exists)
	if( exists .eq. .TRUE.) then
	  open( unit=45, file='objects.list', status='old')
	  more = .true.
	  do while ( more)
	    nobj = nobj + 1
	    read( 45, '(a)', end=101) objects( nobj)
	    call chr_ucase( objects( nobj))
	    if( objects( nobj)( 1:1) .eq. '-') then
	      nomit = nomit + 1
	      call chr_fandl( objects( nobj), n1, m1)
	      omit( nomit) = objects( nobj)( n1+1:m1)
	    end if
	  end do
  101	  continue
	  nobj = nobj - 1
	  type *, 'Number of object types scanned for = ', nobj
	  type *, 'Number of object types omitted     = ', nomit
	  do jj = 1, nobj
	    call chr_fandl( objects( jj), n, m)
	    type *, objects( jj)( n:m)
	  end do
	  do jj = 1, nomit
	    call chr_fandl( omit( jj), n, m)
	    type *, omit( jj)( n:m)
	  end do
	  type *, ' '
        else
	  type *, 'No file objects.list found...'
	  type *, 'Assuming reduction of FS, HD stars only'
          nobj = 2
	  objects( 1) = 'FS'
	  objects( 2) = 'HD'
  	end if
	close( 45)

	write( 43, '(a)') 'setstd 1'
	read( 42, '(a)', end=200) dline
	call chr_fandl( dline, n, m)
	header = dline( 1:m)
	write( 44, '(a)') header( 1:m)
	close( 44)
	read( 42, '(a)', end=200) dline
	read( 42, '(a)', end=200) dline
	read( 42, '(a)', end=200) dline
	read( 42, '(a)', end=200) dline
	call crestd_getpar( iobs, objnam, objtyp, ut1, ut2, ut3, exptime,
     :	                    icoadds, readout, filter, airmass, array,
     :	                    mag, pixscal, ra1, ra2, ra3, dec1, dec2,
     :	                    dec3, more)
	first = 1
	more = .true.
	do while ( more)
	  if( first .eq. 1) then
	    iobs1 = iobs
	    objnam1 = objnam
	    exptime1 = exptime
	    icoadds1 = icoadds
	    filter1 = filter
	    first = 0
	  end if
	  onam = objnam
	  call chr_ucase( onam)
	  if( onam( 1:4) .eq. 'DARK') then
	    iobsd = iobs
	    objnamd = objnam
	    exptimed = exptime
	    icoaddsd = icoadds
	    filterd = filter
	  end if
	  inumb = 0
	  more2 = .true.
	  do while ( more2)
	    call crestd_getpar( iobs, objnam, objtyp, ut1, ut2, ut3,
     :	                        exptime, icoadds, readout, filter,
     :	                        airmass, array, mag, pixscal, ra1, ra2,
     :	                        ra3, dec1, dec2, dec3, more)
	    if( more) then
	      if( onam( 1:4) .eq. 'DARK') then
	        iobsd = iobs-1
	        objnamd = objnam
	        exptimed = exptime
	        icoaddsd = icoadds
	        filterd = filter
	      end if
	      if( objnam .eq. objnam1 .and.
     :	        exptime1 .eq. exptime .and.
     :	        icoadds1 .eq. icoadds .and.
     :	        filter1 .eq. filter) then
	        inumb = inumb + 1
	        more2 = .true.
	      else
	        more2 = .false.
	        first = 1
	      end if
	    else
	      more2 = .false.
	    end if
	  end do
	  if( inumb .gt. 1) then
	    iflag = 0
	    do jj = 1, nobj
	      call chr_fandl( objects( jj), n1, m1)
	      if( onam( 1: m1-n1+1) .eq. objects( jj)( n1:m1)) then
	        iflag = 1
	        do kk = 1, nomit
	          call chr_fandl( omit( kk), n1, m1)
	          if( onam( 1: m1-n1+1) .eq. omit( kk)( n1:m1)) then
	            iflag = 0
	          end if
	        end do
	      end if
	    end do
	    if( iflag .eq. 1) then
	      write( dum1, '(i5)') iobs1
	      write( dum2, '(i5)') (inumb+1)
	      call chr_fandl( dum1, n, m)
	      call chr_fandl( dum2, n1, m1)
	      if( exptime1 .eq. exptimed) then
	        write( dum3, '(i5)') iobsd
	        call chr_fandl( dum3, n2, m2)
	        type *, 'Object found - no. ', iobs1, '  ', onam
	        dline = 'drb_stred '//dum1( n:m)//' '//dum2( n1:m1)//
     :	                ' '//dum3( n2:m2)
	      else
	        type *, 'Object found - no. ', iobs1, '  ', onam
	        type *, 'Not sure which DARK to use...'
	        type *, 'Please enter DARK obs. no. for above objects : '
	        read( 5, *) idark
	        write( dum3, '(i5)') idark
	        call chr_fandl( dum3, n2, m2)
	        dline = 'drb_stred '//dum1( n:m)//' '//dum2( n1:m1)//
     :	                ' '//dum3( n2:m2)
	      end if
	      k = chr_len( dline)
	      write( 43, '(a)') dline( 1:k)
	    end if
	  end if
	end do

  200	continue
	write( 43, '(a)') 'send plt2d set ffj NONE'
	write( 43, '(a)') 'send plt2d set ffh NONE'
	write( 43, '(a)') 'send plt2d set ffk NONE'
	write( 43, '(a)') 'send plt2d set ffnbl NONE'
	write( 43, '(a)') 'send plt2d set fflp NONE'
	write( 43, '(a)') 'send plt2d set ffnbm NONE'

	close( 42)
	close( 43)

  100	continue

	end

	subroutine crestd_getpar( iobs, objnam, objtyp, ut1, ut2, ut3,
     :	                          exptime,
     :	                          icoadds, readout, filter, airmass,
     :	                          array,
     :	                          mag, pixscal, ra1, ra2, ra3, dec1,
     :	                          dec2,
     :	                          dec3, more)

	implicit none

	integer n, m, n1, m1
	integer iobs, ut1, ut2, ut3, icoadds

	real exptime, airmass, pixscal
	real ra1, ra2, ra3, dec1, dec2, dec3

	character dline*132
	character*(*) objnam, objtyp, cut*8, readout, filter
	character array*8, mag*5, cra*11, cdec*11, cpix*10

	logical more

	iobs = 0
	objnam = ' '
	objtyp = ' '
	ut1 = 0
	ut2 = 0
	ut3 = 0
	exptime = 0
	icoadds = 0
	readout = ' '
	filter = ' '
	airmass = 0
	array = ' '
	mag = ' '
	pixscal = 0
	ra1 = 0
	ra2 = 0
	ra3 = 0
	dec1 = 0
	dec2 = 0
	dec3 = 0

	read( 42, '(a)', end=300) dline
	call chr_fandl( dline, n, m)
!	type *, dline( 1:m)
	if( dline .eq. ' ') goto 300
	if( dline( n:n) .eq. '#') goto 300
	read( dline( 1:6), *, err=999) iobs
!	type *, dline( 1:6), '  ', iobs
	if( iobs .ne. 0) then
	  objnam = dline( 7:18)
!	type *, dline( 7:18), '  ', objnam
	  call chr_fandl( objnam, n1, m1)
	  objnam = objnam( n1:m1)
	  objtyp = dline( 19:27)
!	type *, dline( 19:27), '  ', objtyp
	  call chr_fandl( objtyp, n1, m1)
	  objtyp = objtyp( n1:m1)
	  cut = dline( 28:35)
!	type *, dline( 28:35), '  ', cut
	  cut( 3:3) = ' '
	  cut( 6:6) = ' '
	  read( cut, *, err=998) ut1, ut2, ut3
	  read( dline( 36:44), *, err=997) exptime
	  read( dline( 45:52), *, err=996) icoadds
	  readout = dline( 53:63)
	  call chr_fandl( readout, n1, m1)
	  readout = readout( n1:m1)
	  filter = dline( 67:75)
	  call chr_fandl( filter, n1, m1)
	  filter = filter( n1:m1)
	  read( dline( 76:82), *, err=995) airmass
	  array = dline( 83:90)
	  call chr_fandl( array, n1, m1)
	  array = array( n1:m1)
	  mag = dline( 91:95)
	  call chr_fandl( mag, n1, m1)
	  mag = mag( n1:m1)
	  cpix = dline( 96:101)
	  read( cpix, *, err=994) pixscal
	  cra = dline( 103:113)
	  cra( 3:3) = ' '
	  cra( 6:6) = ' '
	  read( cra, *, err=993) ra1, ra2, ra3
	  cdec = dline( 116:126)
	  cdec( 4:4) = ' '
	  cdec( 7:7) = ' '
	  read( cdec, *, err=992) dec1, dec2, dec3
	end if

	goto 200
  300	continue
	more = .false.

  200	continue

	goto 100
  999	type *, 'Crestd: Error reading observation number'
	type *, dline( 1:6)
	more = .false.
	goto 100
  998	type *, 'Crestd: Error reading ut time - ', cut
	more = .false.
	goto 100
  997	type *, 'Crestd: Error reading exposure time'
	more = .false.
	goto 100
  996	type *, 'Crestd: Error reading number coadds'
	more = .false.
	goto 100
  995	type *, 'Crestd: Error reading airmass'
	more = .false.
	goto 100
  994	type *, 'Crestd: Error reading pixel scale'
	more = .false.
	goto 100
  993	type *, 'Crestd: Error reading ra'
	more = .false.
	goto 100
  992	type *, 'Crestd: Error reading dec'
	more = .false.

  100	continue

	end
