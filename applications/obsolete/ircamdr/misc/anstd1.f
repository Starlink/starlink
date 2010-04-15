	program anstd1

! This program does a number of different calculations:
!
! 1) It reads in the stdred_photometry.results file that is output from
!    the ircamdr task drb_stred. It read all the photometry lines into
!    the array `lines'.
!
! 2) For the old faint standards (FS stars) it calculates the J-H, H-K, and
!    K zeropoints, as well as the extinction curves for each night.
!    The program calculates the standard deviation of the instrumental
!    magnitudes of the individual frames (zeropoint for the K measurement)
! for each star and each filter. To get a sigma for the J-H and H-K measurments
!    it adds the two sigma values in quadature to get a final sigma. If this
!    final sigma is greater than 0.1 then that zeropoint is not used in the
!    calculation of the extinction curve, or the errors.
!
! 3) Residuals from the best fit extinction curve are calculated for ALL FS
!    stars, even the ones with sigma greater than 0.1. The results for all
!    stars are outputted to the file `prefix'anstd.results. This outputs
!    the star name, airmass of the observation (the H observation for the J-H
!    colour, and the K observation for the H-K colour), the type of the
!    observation (whether it is an `I'ndividual, or a `M'osaic), the zeropoint,
!    the sigma, and the residual.
!
! 4) There are seperate files for the J-H zeropoints (`prefix'j-h.zp), the
!    H-K zeropoint (`prefix'h-k.zp), and the K zeropoints (`prefix'k.zp).
!    These files are plottable, and include the source name, the airmass,
!    the zeropoint, the sigma, and the residuals, but only for those who
!    have a sigma less than 0.1. The other ones are flagged by having their
!    source name prefixed with `## ', and the residual is not displayed.
!
! 5) This program also derives colours and magnitudes for the new faint
!    standards. Using the same algorithm as to calculate the zeropoints, it
!    calculates the instramental colour and then applies the airmass to the
!    appropriate extinction curve in order to derive a zeropoint, and then
!    calculates the colour or magnitude. The results are outputted to the
!    file `prefix'new.results, which displayes the source name, the filter or
!    colour, the airmass, the derived colour or magniutde, and the uncertainty.
!    The uncertainties were calculated using the scatter of the instarmental
!    magnitudes in the individual measurements, and the stnadrd deviations of
!    both the slope and the intercept for the extinction curves. For the
!    linear relation y = mx + b, let the slope have an absolut unceratinty of
!    delm, and the intercept of an absolute uncertainty of delb. dely,
!    therefore, is given by dely = sqrt( (delm*x)**2 + delb**2). We then
!    have to add our y result to the inst. colour/magnitude, which is
!    z+/-delz. The final uncertainty, delf, is therefore
!    sqrt( dely**2 + delz**2).
!
!    I made no restrictions on the scatter of the points. That is, magnitudes
!    and colours were calculated no matter what the sigma of the observations
!    was.

	implicit none

	integer l1, chr_len
	integer nlines, n, m, j, p, sta( 20), sto( 20), lstart
	integer nwrd, k, lstat, l, ipos1, ipos2, status
        integer mosflag( 2000), imos, gstart( 2000), gend( 2000)
	integer ngroups, numpix, n2, m2, kcount
        integer c1, c2, c3, tallyj, tallyk, mark

! These arrays hopd the data from the stdred_photometry.results file.

	real ap1( 2000), ap2( 2000), ap3( 2000), nstdj( 2000), nstdk( 2000)
	real ps( 2000), zp( 2000), am( 2000), actmag( 2000), residj( 2000)
        real residk( 2000), kmag
	real objmsky( 2000), i3mag( 2000), calzp( 2000), zp1( 2000),zp2( 2000)

! These are the statistics variables

        real*8 jsum, jsumsq, jmean, jmeanam, variance, std, tempsum, tempsumsq
        real*8 jsumam, jsumamsq, jsumprod, slopeh, slopek, slopej, tempmean
        real*8 hint, jint, kint, resid, jresidsq, hresidsq, kresidsq
	real*8 hsum, hsumsq, hmean, hmeanam, hsumam, hsumamsq, hsumprod
        real*8 ksum, ksumsq, kmean, kmeanam, ksumam, ksumamsq, ksumprod
        real*8 kssq, jssq, hssq, jstanderr, kstanderr, hstanderr
        real*8 hstd, jstd, kstd, jintstd, hintstd, kintstd, tempstd
        real*8 jmh, actjmh, hmk, acthmk, actj, instj, acth, insth, actk, instk

	character file1*80, dline*132, lines( 2000)*132, file2*80, date*80
	character words( 20)*80, all( 20, 2000)*80, objnam( 2000)*30
	character objrest( 2000)*30, filt( 2000)*20, itype*1, pref*20

	logical exists, more, flag, high

	include 'SAE_PAR'

	status = SAI__OK

	type *, 'Name of photometry file created by drb_stred : '
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

! We need the date for the ledgering program

        type *, 'Input date as follows (950327) :'
        read( 5, '(a)') date

	l1 = chr_len( pref)
	file2 = pref( 1:l1)//'anstd.results'
	open( unit=143, file=file2, status='unknown')
	open( unit=144, file=pref( 1:l1)//'j-h.zp', status='unknown')
	open( unit=145, file=pref( 1:l1)//'h-k.zp', status='unknown')
	open( unit=146, file=pref( 1:l1)//'k.zp', status='unknown')
        open( unit=147, file=pref( 1:l1)//'new.results', status='unknown')
        do j = 143,147
           write(j, '(a,a)') '## ', date
        end do

! Read all the photometry lines, and put them into the array `lines'

	more = .true.
	nlines = 0
	do while ( more)
	  nlines = nlines + 1
	  read( 142, '(a)', end=200) dline
	  lines( nlines) = dline
! Turn them into uppercase
	  call chr_ucase( dline)
! Get the positions of the first and last characters in the line
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

! This is where we break each line into words, and read the `words' into
! their appropriate arrays.

	l = 0
! For all the photometry lines
	do j = lstart, nlines+lstart-1
	  l = l + 1
! Get the number of words and their broken up and put into the array `words'
	  call chr_dcwrd( lines( j), 20, nwrd, sta, sto, words, lstat)
	  do k = 1, nwrd
! Read the words for the current line into an array that will hold them for
! all the lines
	    all( k, l) = words( k)
	  end do
	  call chr_fandl( all( 1, l), n, m)
	  ipos1 = index( all( 1, l), '_')
! Make sure we find the object name
          if((all( 1, l)( ipos1+4:ipos1+4) .ne. 'f') .and.
     :       (all( 1, l)( ipos1+4:ipos1+4) .ne. 'm') .and.
     :       (all( 1, l)( ipos1+4:ipos1+4) .ne. 'd') .and.
     :       (all( 1, l)( ipos1+4:ipos1+4) .ne. '_')) then
             if( (ipos1+2) .eq. '_') then
                   ipos1 = ipos1+2
             else
                   ipos1 = ipos1+3
             end if
          end if
	  objnam( l) = all( 1, l)( n:ipos1-1)
	  call chr_ucase( objnam( l))
	  ipos2 = index( all( 1, l), '.')
	  objrest( l) = all( 1, l)( ipos1+1:ipos2-1)
	  call chr_ucase( objrest( l))
! Is the current line a mosaic?
	  imos = index( objrest( l), '_MOS')
	  if( imos .ne. 0) then
	    mosflag( l) = 1
	  else
	    mosflag( l) = 0
	  end if
	  call chr_fandl( all( 3, l), n, m)
	  ipos1 = index( all( 3, l), '.')
! Read the current filter into the filter array
	  filt( l) = all( 3, l)( n:ipos1-1)
          call chr_ucase( filt( l))
! Change the remaining data into real numbers and store them in their own
! arrays
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
	end do
	nlines = l - 1

! This counts the number of `groups'. One group being the individual frames
! of a mosaic. For example the five frames of a five point jitter in one
! filter is called a group, but the resulting mosaic is not considered part
! of that group.
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


! Initialize a whole bunch of variables

        jsum = 0.0d0
        jsumsq = 0.0d0
        jsumam = 0.0d0
        jsumamsq = 0.0d0
        jsumprod = 0.0d0
        jresidsq = 0.0d0
        hsum = 0.0d0
        hsumsq = 0.0d0
        hsumam = 0.0d0
        hsumamsq = 0.0d0
        hsumprod = 0.0d0
        hresidsq = 0.0d0
        tempsum = 0.0d0
	tempsumsq = 0.0d0
        numpix = 0
        c1 = 0
        c3 = 0
        j = 0
        mark = 0
        tallyj = 0
        tallyk = 0
        flag = .false.
        high = .false.
! Now we go through each line and calculate the J-H and H-K zeropoints for
! each FS star. This algorithm expects the photometry in a J, H, K order, but
! can handle problems such as no J observation, no K observation, etc.
        do while ( j .le. nlines)
          j = j + 1
          if( actmag( j) .ne. -99.99) then
! First we calculate the sigma of the instra. mags of the indivifual images.
            if( mosflag( j) .ne. 1) then
              numpix = numpix + 1
              tempsum = tempsum + dble(i3mag( j))
	      tempsumsq = tempsumsq + dble(i3mag( j))**2
! Now we get to a mosaic
            else
              tempmean = tempsum/numpix
	      variance = ( tempsumsq - numpix*tempmean**2)
              if( numpix .eq. 1 .or. variance .lt. 0.0d0) THEN
                variance  =  0.0d0
              else
                variance  =  variance/( numpix-1.0d0)
              end if
              std = dsqrt( variance)
              tempsum = 0.0d0
	      tempsumsq = 0.0d0
              numpix = 0
              if( filt( j) .eq. 'J') then
! Mark the line number of the J observatuion
                mark = j
                actj = actmag( j)
                instj = i3mag( j)
                tempstd = std
! If the sigma is high, flag it for later
                if( sngl( std) .le. 0.1) then
                  high = .true.
                end if
              else if( filt(j ) .eq. 'H') then
! If we've come across a J...
                  if( mark .ne. 0) then
! If the object names of this H and the previous J are the same...
                    if( lle( objnam( j), objnam( mark)) .and.
     :                     lge( objnam( j), objnam( mark))) then
                      acth = actmag( j)
                      insth = i3mag( j)
! We've got a match, iterate tallyj
                      tallyj = tallyj + 1
! Compute the combined sigma
                      tempstd = sqrt(tempstd**2 + std**2)
! Store that sigma
                      nstdj(tallyj) = tempstd
                      jmh = instj - insth
                      actjmh = actj - acth
! Computer the zp, and store it
                      zp1(tallyj) = actjmh - jmh
! If the sigmas are less then 0.1, get the paarametrs we need to calculate
! the best fit extinction curve (zp vs. airmass)
                      if( ( .not. high) .or. ( sngl( tempstd) .le. 0.1)) then
                        c1 = c1 + 1
                        jsum = jsum + dble( zp1( tallyj))
                        jsumsq = jsumsq + dble( zp1( tallyj))**2
                        jsumam = jsumam + dble( am( j)-1)
                        jsumamsq = jsumamsq + dble( am( j)-1)**2
                        jsumprod = jsumprod + dprod( ( am( j)-1),zp1( tallyj))
                      end if
! Mark our new line number
                      mark = j
                      tempstd = std
                    end if
                  else
! If we haven't come across a J, but the next one is a K...
                    if( ( lle( objnam( j+1), objnam( j)) .and.
     :                     lge( objnam( j+1), objnam( j))) .and.
     :                     ( filt(j+1) .eq. 'K')) then
                      acth = actmag( j)
                      insth = i3mag( j)
                      mark = j
                      tempstd = std
                    end if
                  end if
              else if( filt(j ) .eq. 'K') then
! If we've come across an H...
                  if( mark .ne. 0) then
! If that H and this K, have the same object names...
                    if ( lle( objnam( j), objnam( mark)) .and.
     :                     lge( objnam( j), objnam( mark))) then
                      actk = actmag( j)
                      instk = i3mag( j)
                      tallyk = tallyk + 1
! This just makes sure we get everything in order...
                      if( tallyk .lt. tallyj) then
                        zp2( tallyk) = -99.99
                        tallyk = tallyk + 1
                      end if
! Similarly to J-H...
                      tempstd = sqrt(tempstd**2 + std**2)
                      nstdk(tallyk) = tempstd
                      hmk = insth - instk
                      acthmk = acth - actk
                      zp2(tallyk) = acthmk - hmk
                      if( sngl( tempstd) .le. 0.1) then
                        c3 = c3 + 1
                        hsum = hsum + dble( zp2( tallyk))
                        hsumsq = hsumsq + dble( zp2( tallyk))**2
                        hsumam = hsumam + dble(am( j)-1)
                        hsumamsq = hsumamsq + dble( am( j)-1)**2
                        hsumprod = hsumprod + dprod( (am( j)-1), zp2( tallyk))
                      end if
! We're finished for this star...
                      flag = .true.
                    end if
                  end if
              end if
              if ( flag) then
                 mark = 0
                 flag = .false.
              end if
            end if
          end if
        end do


! More initializations for the K zeropoint variables
        ksum = 0.0d0
        ksumsq = 0.0d0
        ksumam = 0.0d0
        ksumamsq = 0.0d0
        ksumprod = 0.0d0
        kcount = 0.0d0
        kresidsq = 0.0d0
        c2 = 0
! This algorithim is a bit cleaner....we go through each group, taking
! note of only the K ones...
	do j = 1, ngroups
	  if( actmag( gstart( j)) .ne. -99.99) then
! The sigma is computed from the zeropoints this time
	    tempsum = 0.0d0
	    tempsumsq = 0.0d0
	    numpix = gend( j)-gstart( j)+1
	    do k = gstart( j), gend( j)
	      tempsum = tempsum + dble(calzp( k))
	      tempsumsq = tempsumsq + dble(calzp( k))**2
	    end do
	    tempmean = tempsum/numpix
	    variance = ( tempsumsq - numpix*tempmean**2)
            if( numpix .eq. 1 .or. variance .lt. 0.0d0) THEN
              variance  =  0.0d0
            else
              variance  =  variance/( numpix-1.0d0)
            end if
            std = dsqrt( variance)
	    if( mosflag( gstart( j)) .eq. 0) then
	      itype = 'I'
	    else
	      itype = 'M'
	    end if
! If the filter of the group is K....
	    if( filt( gstart( j)) .eq. 'K') then
! If its not a mosiac...
	      if( itype .eq. 'I') then
! If its sigma is less than 0.1...
	        if( sngl( std) .le. 0.1) then
! Compute the parameters needed to compute the extinction curve
                  do p = gstart( j), gend( j)
                     ksum = ksum + dble(calzp( p))
                     ksumsq = ksumsq + dble(calzp( p))**2
                     ksumam = ksumam + dble(am( p)-1)
                     ksumamsq = ksumamsq + dble(am( p)-1)**2
                     ksumprod = ksumprod + dprod((am( p)-1),calzp( p))
                     kcount = kcount+1
                  end do
                end if
	      end if
	    end if
	  end if
 	end do





! Now compute the slope, intercept, the sigma of the slope (eg, jstd), and
! the sigma of the points, (eg, jstanderr) for each extiction curve
        jmeanam = jsumam/c1
        jmean = jsum/c1
        slopej = (jsumprod - jsumam * jmean)/(jsumamsq - jsumam * jmeanam)
        jint = jmean - slopej * jmeanam
        jssq = (jsumsq - (jint * jsum) - (slopej * jsumprod))/c1
        jstanderr = dsqrt(jssq)
        jstd = dsqrt(jssq/(jsumamsq - (jsumam**2/c1)))
        hmeanam = hsumam/c3
        hmean = hsum/c3
        slopeh = (hsumprod - hsumam * hmean)/(hsumamsq - hsumam * hmeanam)
        hint = hmean - slopeh * hmeanam
        hssq = (hsumsq - hint * hsum - slopeh * hsumprod)/c3
        hstanderr = dsqrt(hssq)
        hstd = dsqrt(hssq/(hsumamsq - (hsumam**2/c3)))
        kmeanam = ksumam/kcount
        kmean = ksum/kcount
        slopek = (ksumprod - ksumam * kmean)/(ksumamsq - ksumam * kmeanam)
        kint = kmean - slopek * kmeanam
        kssq = (ksumsq - kint * ksum - slopek * ksumprod)/kcount
        kstanderr = dsqrt(kssq)
        kstd = dsqrt(kssq/(ksumamsq - (ksumam**2/kcount)))


! Using the same algorithm to find and compute the J-H and H-K zeropints, we
! compute the residuals from the line for each observation and output them
! to the plottable (`prefix'j-h.zp) files.
        tempsum = 0.0d0
	tempsumsq = 0.0d0
        numpix = 0
        tallyj = 0
        tallyk = 0
        j = 0
        mark = 0
        flag = .false.
        do while ( j .le. nlines)
          j = j + 1
          if( actmag( j) .ne. -99.99) then
            if( mosflag( j) .ne. 1) then
              numpix = numpix + 1
              tempsum = tempsum + dble(i3mag( j))
	      tempsumsq = tempsumsq + dble(i3mag( j))**2
            else
              tempmean = tempsum/numpix
	      variance = ( tempsumsq - numpix*tempmean**2)
              if( numpix .eq. 1 .or. variance .lt. 0.0d0) THEN
                variance  =  0.0d0
              else
                variance  =  variance/( numpix-1.0d0)
              end if
              std = dsqrt( variance)
              tempsum = 0.0d0
	      tempsumsq = 0.0d0
              numpix = 0
              if( filt( j) .eq. 'J') then
                tempstd = std
                mark = j
              end if
              if( filt( j) .eq. 'H') then
                if( mark .ne. 0) then
                  if ( lle( objnam( j), objnam( mark)) .and.
     :                   lge( objnam( j), objnam( mark))) then
                    tallyj = tallyj + 1
! Compute the residual
                    resid = zp1(tallyj) - (jint + slopej * (am( j) - 1))
                    residj(tallyj) = resid
                    call chr_fandl( objnam( j), n, m)
                    if( m .lt. 12) m = 12
                    tempstd = sqrt(tempstd**2 + std**2)
! If the sigma is less than 0.1, output with the residual included,
! and compute the parameter needed for the intercept's sigma...
                    if( sngl( tempstd) .le. 0.1) then
                      write(144, '(a5,4f10.3,f10.5)') objnam(j)(n:m),
     :                    am(j), sngl( zp1(tallyj)), sngl( tempstd), resid
                      jresidsq = jresidsq + resid**2
! otherwise, flag the observation, and don't output the residual
                    else
                      write(144, '(a,a,4f10.3)') '**  ', objnam( j)(n:m),
     :                  am( j), sngl(zp1(tallyj)), sngl( tempstd)
                    end if
                    tempstd = std
                    mark = j
                  end if
                else
                  if( ( lle( objnam( j+1), objnam( j)) .and.
     :                   lge( objnam( j+1), objnam( j))) .and.
     :                   ( filt(j+1) .eq. 'K')) then
                    mark = j
                    tempstd = std
                  end if
                end if
              end if
! similarly for H-K
              if( filt(j ) .eq. 'K') then
                if( mark .ne. 0) then
                  if ( lle( objnam( j), objnam( mark)) .and.
     :                   lge( objnam( j), objnam( mark))) then
                    tallyk = tallyk + 1
                    if( zp2(tallyk) .eq. -99.99) then
                       tallyk = tallyk + 1
                    end if
                    resid  = zp2(tallyk) - (hint + slopeh * (am( j) - 1))
                    residk(tallyk) = resid
                    call chr_fandl( objnam( j), n, m)
                    if( m .lt. 12) m = 12
                    tempstd = sqrt(tempstd**2 + std**2)
                    if( sngl( tempstd) .le. 0.1) then
                      write(145, '(a5,4f10.3,f10.5)') objnam(j)(n:m),
     :                    am(j), sngl( zp2(tallyk)), sngl( tempstd), resid
                      hresidsq = hresidsq + resid**2
                    else
                      write(145, '(a,a,4f10.3)') '**  ', objnam( j)(n:m),
     :                am( j), sngl(zp2(tallyk)), sngl( tempstd)
                    end if
                    flag = .true.
                  end if
                end if
              end if
              if ( flag) then
                 mark = 0
                 flag = .false.
              end if
            end if
          end if
        end do

! Here we output the `prefix'anstd.results file, as well as the `prefix'k.zp
! file, iterating by groups...
        tallyj = 0
        tallyk = 0
	write( 143, '(a)') 'Known standard stars : FS or UKIRT'
	write( 143, '(a)') 'Source    Fil/Col  Type     Airmass     '//
     :	  'ZP        Std         Residuals'
	do j = 1, ngroups
! Comput the sigma in zeropoints to use for the K observations..
	  if( actmag( gstart( j)) .ne. -99.99) then
	    tempsum = 0.0d0
	    tempsumsq = 0.0d0
	    numpix = gend( j)-gstart( j)+1
	    do k = gstart( j), gend( j)
	      tempsum = tempsum + dble(calzp( k))
	      tempsumsq = tempsumsq + dble(calzp( k))**2
	    end do
	    tempmean = tempsum/numpix
	    variance = ( tempsumsq - numpix*tempmean**2)
            if( numpix .eq. 1 .or. variance .lt. 0.0d0) THEN
              variance  =  0.0d0
            else
              variance  =  variance/( numpix-1.0d0)
            end if
            std = dsqrt( variance)
	    if( mosflag( gstart( j)) .eq. 0) then
	      itype = 'I'
	    else
	      itype = 'M'
	    end if
	    call chr_fandl( objnam( gstart( j)), n, m)
	    if( m .lt. 12) m = 12
	    call chr_fandl( filt( gstart( j)), n2, m2)
	    if( m2 .lt. 6) m2 = 6
            if( filt(gstart( j)) .eq. 'K') then
              if( itype .eq. 'I') then
                if( sngl( std) .le. 0.1) then
! Again if sigma is less than 0.1, compute the residual, and output it to
! the file.
                   resid = tempmean - (kint + slopek * (am( gstart( j))-1))
                   kresidsq = kresidsq + resid**2
                   c2 = c2 + 1
                   write( 146, '(a5,4f10.3,f10.5)') objnam( gstart( j))( n:m),
     :             am( gstart( j)),
     :	           sngl( tempmean), sngl( std),resid
! otherwise, don't output the residual
	        else
                   resid = tempmean - (kint + slopek * (am( gstart( j))-1))
	           write( 146, '(a,a,4f10.3)') '**  ',
     :             objnam( gstart( j))( n:m),
     :             am( gstart( j)),
     :	           sngl( tempmean), sngl( std)
                end if
              else
! compute the residual for the mosaic
                resid = tempmean - (kint + slopek * (am( gstart( j))-1))
              end if
           end if
           if( filt( gstart( j)) .eq. 'J') then
! Now output the J-H zp, residual, etc. to the anstd.results file
             if( itype .eq. 'M') then
               tallyj = tallyj + 1
               write( 143, '(a,a,a,a,a,2f10.3,a,f7.3,a,f10.3,a,f10.5)')
     :	         objnam( gstart( j))( n:m),
     :	         'J-H','     ',itype,'   ',
     :	         am( gstart( j)), zp1(tallyj), '   ', nstdj(tallyj),
     :           '     ', residj(tallyj)
             end if
           else if( filt( gstart( j)) .eq. 'H') then
! Similarly for H-K
             if( itype .eq. 'M') then
               tallyk = tallyk + 1
               if( zp2(tallyk) .ne. -99.99) then
                 write( 143, '(a,a,a,a,a,2f10.3,a,f7.3,a,f10.3,a,f10.5)')
     :	             objnam( gstart( j))( n:m),
     :	             'H-K','     ',itype,'   ',
     :               am( gstart( j)), zp2(tallyk), '   ', nstdk(tallyk),
     :               '     ', residk(tallyk)
               end if
             end if
           else if( filt( gstart( j)) .eq. 'K') then
! Finally for K
             write( 143, '(a,a,a,a,a,2f10.3,a,f7.3,a,f10.3,a,f10.5)')
     :       objnam( gstart( j))( n:m),
     :	     filt( gstart( j))( n2:m2),'  ',itype,'   ',
     :	     am( gstart( j)), sngl( tempmean), '   ', sngl( std),'     ',resid
           end if
          end if
        end do

! Compute the sigmas for the intercepts...
        jintstd = sqrt( (jresidsq/(c1*(c1-2))) + jmeanam * (jstd**2) )
        hintstd = sqrt( (hresidsq/(c3*(c3-2))) + hmeanam * (hstd**2) )
        kintstd = sqrt( (kresidsq/(c2*(c2-2))) + kmeanam * (kstd**2) )

! And output the statisitics to the end of the *.zp files
        write( 144, '(a,f10.5,2x,f6.3)') 'Best fit: slope and intercept = ',
     :  slopej,jint
        write( 144, '(a,f10.5,2x,f6.3)') 'Standard deviation of slope and '//
     :  'standard deviation of points: ',jstd, jstanderr
        write( 144, '(a, f10.5)') 'Standard deviation of intercept: ', jintstd

        write( 145, '(a,f10.5,2x,f6.3)') 'Best fit: slope and intercept = ',
     :  slopeh,hint
        write( 145, '(a,f10.5,2x,f6.3)') 'Standard deviation of slope and '//
     :  'standard deviation of points: ',hstd, hstanderr
        write( 145, '(a, f10.5)') 'Standard deviation of intercept: ', hintstd

        write( 146, '(a,f10.5,2x,f6.3)') 'Best fit: slope and intercept = ',
     :  slopek,kint
        write( 146, '(a,f10.5,2x,f6.3)') 'Standard deviation of slope and '//
     :  'standard deviation of points: ',kstd, kstanderr
        write( 146, '(a, f10.5)') 'Standard deviation of intercept: ', kintstd

! Now we start on computing the magnitudes and colours for the new faint
! standard stars. I reused alot of the same code and variables as above
        j = 0
        numpix = 0
        tempsum = 0
        tempsumsq = 0
        mark = 0
        tallyj = 0
        tallyk = 0
        flag = .false.
        high = .false.
        do while ( j .le. nlines)
          j = j + 1
! Notice that now this is EQUALS -99.99
          if( actmag( j) .eq. -99.99) then
            if( mosflag( j) .ne. 1) then
              numpix = numpix + 1
              tempsum = tempsum + dble(i3mag( j))
	      tempsumsq = tempsumsq + dble(i3mag( j))**2
            else
! If the line is a mosaic
              tempmean = tempsum/numpix
	      variance = ( tempsumsq - numpix*tempmean**2)
              if( numpix .eq. 1 .or. variance .lt. 0.0d0) THEN
                variance  =  0.0d0
              else
                variance  =  variance/( numpix-1.0d0)
              end if
              std = dsqrt( variance)
              tempsum = 0.0d0
	      tempsumsq = 0.0d0
              numpix = 0
              if( filt( j) .eq. 'J') then
                mark = j
                instj = i3mag( j)
                tempstd = std
              else if( filt(j ) .eq. 'H') then
                  if( mark .ne. 0) then
                    if( lle( objnam( j), objnam( mark)) .and.
     :                     lge( objnam( j), objnam( mark))) then
                      insth = i3mag( j)
                      tallyj = tallyj + 1
                      tempstd = sqrt(tempstd**2 + std**2)
! Compute the instramental J-H
                      jmh = instj - insth
! Apply the airmass to the extiction curve to get the zeropoint...
                      zp1(tallyj) = jint + slopej * dble( am(j)-1)
! Now get the actual J-H
                      zp1(tallyj) = zp1(tallyj) + jmh
! Compute and store the errors...
                      nstdj(tallyj) = sqrt( ((jstd*dble( am(j)-1))**2 +
     :                                    jintstd**2) + (tempstd**2))
                      mark = j
                      tempstd = std
                    end if
                  else
                    if( ( lle( objnam( j+1), objnam( j)) .and.
     :                     lge( objnam( j+1), objnam( j))) .and.
     :                     ( filt(j+1) .eq. 'K')) then
                      insth = i3mag( j)
                      mark = j
                      tempstd = std
                    end if
                  end if
              else if( filt(j ) .eq. 'K') then
! Similarly for H-K
                  if( mark .ne. 0) then
                    if ( lle( objnam( j), objnam( mark)) .and.
     :                     lge( objnam( j), objnam( mark))) then
                      instk = i3mag( j)
                      tallyk = tallyk + 1
                      if( tallyk .lt. tallyj) then
                        zp2( tallyk) = -99.99
                        tallyk = tallyk + 1
                      end if
                      tempstd = sqrt(tempstd**2 + std**2)
                      hmk = insth - instk
                      zp2(tallyk) = hint + slopeh * dble( am(j)-1)
                      zp2(tallyk) = zp2(tallyk) + hmk
                      nstdk(tallyk) = sqrt( ((hstd * dble( am(j)-1))**2 +
     :                        hintstd**2) + (tempstd**2))
                      flag = .true.
                    end if
                  end if
              end if
              if ( flag) then
                 mark = 0
                 flag = .false.
              end if
            end if
          end if
        end do

! Here, we go through by the number of groups and output the J-H and H-K
! data, and compute and output the K data...
        tallyj = 0
        tallyk = 0
	write( 147, '(a)') 'New UKIRT Faint Standards Stars'
	write( 147, '(a)') 'Source    Fil/Col    Airmass    '//
     :	  'Value    Uncert'
	do j = 1, ngroups
	  if( actmag( gstart( j)) .eq. -99.99) then
	    tempsum = 0.0d0
	    tempsumsq = 0.0d0
	    numpix = gend( j)-gstart( j)+1
	    do k = gstart( j), gend( j)
! again use the instramental mags. to comput the sigma...
	      tempsum = tempsum + dble(i3mag( k))
	      tempsumsq = tempsumsq + dble(i3mag( k))**2
	    end do
	    tempmean = tempsum/numpix
	    variance = ( tempsumsq - numpix*tempmean**2)
            if( numpix .eq. 1 .or. variance .lt. 0.0d0) THEN
              variance  =  0.0d0
            else
              variance  =  variance/( numpix-1.0d0)
            end if
            std = dsqrt( variance)
	    if( mosflag( gstart( j)) .eq. 0) then
	      itype = 'I'
	    else
	      itype = 'M'
	    end if
	    call chr_fandl( objnam( gstart( j)), n, m)
	    if( m .lt. 12) m = 12
	    call chr_fandl( filt( gstart( j)), n2, m2)
	    if( m2 .lt. 6) m2 = 6
            if( filt( gstart( j)) .eq. 'J') then
             if( itype .eq. 'M') then
! If we are on a J mosaic, output the appropriate J-H colour and error
               tallyj = tallyj + 1
               write( 147, '(a,a,a,f10.3,f10.3,a,f7.3)')
     :	         objnam( gstart( j))( n:m),
     :	         'J-H','  ',
     :	         am( gstart( j)), zp1(tallyj), '   ', nstdj(tallyj)
             end if
           else if( filt( gstart( j)) .eq. 'H') then
             if( itype .eq. 'M') then
! If we are on a H mosaic, output the appropriate H-K colour and error
               tallyk = tallyk + 1
               if( zp2(tallyk) .ne. -99.99) then
                 write( 147, '(a,a,a,f10.3,f10.3,a,f7.3)')
     :	             objnam( gstart( j))( n:m),
     :	             'H-K','  ',
     :               am( gstart( j)), zp2(tallyk), '   ', nstdk(tallyk)
               end if
             end if
           else if( filt( gstart( j)) .eq. 'K') then
            if( itype .eq. 'M') then
! If we are on a K mosaic, comput and out put the K magnitude and error
             kmag  = kint + slopek * (am( gstart( j))-1)
             kmag = kmag + tempmean
             std = sqrt( ((kstd * dble(am( gstart( j))-1))**2 +
     :               kintstd**2) + (std**2))
             write( 147, '(a,a,a,f7.3,a,f7.3,a,f7.3)')
     :       objnam( gstart( j))( n:m),
     :	     filt( gstart( j))( n2:m2),'  ',
     :	     am( gstart( j)),'   ', sngl( kmag), '   ', sngl( std)
            end if
           end if
          end if
        end do

! Close the output files...

	close( 143)
	close( 144)
	close( 145)
	close( 146)
        close( 147)

  100	continue
	end


