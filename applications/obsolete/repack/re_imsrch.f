*+RE_IMSRCH image whose centre is nearest specified coordinates
	PROGRAM RE_IMSRCH
	implicit none

* Global constants
        include 'CONSTANTS.INC'

* P.McGale (pam@star.le.ac.uk) Mar. 95
* P McGale Add par i/o routines - Apr 95.
* P McGale Add re_res variable - May 95.
* J.Ashley re_res now uses RECAL - Jan 96.
* J.Ashley RA/Dec input via file. - Jan 96.
*-

* Local variables:
	real sra				! Search RA   (rads)
	real sdec				! Search Dec  (rads)
	real im_ra				! Image RA    (rads)
	real im_dec				! Image Dec   (rads)
	real c_ra				! Store closest ra (rads)
	real c_dec				! Store closest dec (rads)
        real sep				! Separation (rads)
        real bsep				! Best separation (rads)
	integer fc, lc, lun1,lun2
	integer status
	character*80 rec			! Data record
	character*80 brec			! Choice record
	character*80 re_res			! Reserv. file directory
	character*11 ch_dec			! Char Dec
	character*10 ch_ra			! Char RA
	character*255 fnam			! Co-ords filename
	character*1 in				! use fnam for ra/dec
* Functions:
        real sla_sep				! Calculate Separation
*
	data status/0/


	call par_cmdl(' ', status)

	write(*,*)
	write(*,*) '   RE_IMSRCH, version 310196'
	write(*,*)

* Get input co-ordinate options
	call par_get0c('Get RA/Dec search pairs from file (Y/N)',in,status)
	lun1 = 0
	if (in.eq.'Y'.or.in.eq.'y') then
	    call par_get0c('Filename of co-ordinates pairs',fnam,status)
*	    call get_lun1(lun1)
	    lun1 = 11
	    call chr_fandl(fnam, fc, lc)
	    open(lun1,file=fnam(fc:lc),status='old',readonly,iostat=status)
	    if (status.ne.0) then
	      write(*,*) 'Error: can`t open file: ',fnam(fc:lc)
	      goto 300
	    endif
        else
* Get search RA and Dec.
	    call par_get0r('SRA search RA (deg/hms)', sra, status)
	    call par_get0r('SDEC search Dec (deg/dms)', sdec, status)
	endif
* Get input co-ordinate options
	call par_get0c('Write results into a file (Y/N)',in,status)
	lun2 = 0
	if (in.eq.'Y'.or.in.eq.'y') then
	    call par_get0c('Output filename',fnam,status)
*	    call get_lun(lun2)
	    lun2 = 12
	    call chr_fandl(fnam, fc, lc)
	    open(lun2,file=fnam(fc:lc),status='unknown',iostat=status)
	    if (status.ne.0) then
	      write(*,*) 'Error: can`t open file: ',fnam(fc:lc)
	      goto 300
	    endif
	    write(12,*) ' Image        RA (degrees) Dec (degrees)'
        endif
* read RA/DEC from file if open
 50     continue
	if (lun1.ne.0) then
	  read(lun1,*,end=299,iostat=status) sra,sdec
	  if (status.ne.0) then
	     write(*,*) 'Error: invalid co-ordinate pairs ignored'
	     goto 50
	  endif
        endif

* Open file with image names and centres
	call getenv("RECAL", re_res)
	if (re_res .eq. ' ') then
	    write(*,*)'   Can''t get RECAL environment variable.'
	endif
	call chr_fandl(re_res, fc, lc)
	open(10, file=re_res(fc:lc)//'/re_imge.dat',
     &                          status='old', readonly)

* Search for image with centre closest to input RA & Dec.
	write(*,*)
	write(*,*) ' Image with centre nearest search RA & Dec: '
	write(*,*) ' RA: (',sra,') Dec: (',sdec,')'
	write(*,*)
	sra  = sra  * dtor
	sdec = sdec * dtor
        brec  = 'None found!'
        bsep  = 999999.0
	c_ra  = 360.0 * dtor
	c_dec = 90.0  * dtor
* Loop through image list.
 100	read(10, '(a)',end=199) rec
	  read(rec(36:57), *) im_ra, im_dec
	  im_ra  = im_ra  * dtor
	  im_dec = im_dec * dtor
	  sep = sla_sep(sra, sdec, im_ra, im_dec)
	  if (sep .lt. bsep) then
            brec = rec
            bsep = sep
	    c_ra  = im_ra
	    c_dec = im_dec
	  endif
	  goto 100
 199	continue
	close(10)

	call ax_ra2hms(c_ra, 'hm', ch_ra)
	call ax_dec2dms(c_dec, 'dm', ch_dec)
        brec(36:57) = ch_ra//'   '//ch_dec
	write(*,30)
	write(*,*)  brec(8:80)
	write(*,35)  c_ra*rtod, c_dec*rtod
	write(*,*)
 30	format('  Archive      Image           RA-cent    Dec-cent
     +  Cnts F1  Cnts F2',/,
     +         '  ------------------------------------------------
     +------------------')
 35 	format(28x,'(',f9.2,')  (',f9.2,')')

	if (lun2.ne.0) then
	  write(12,37) brec(22:34),c_ra*rtod, c_dec*rtod
 37       format(1x,A12,1x,f9.2,1x,f9.2)
	endif

* For MID region images, separation should be no greater than 92 arcmin
* if co-ord is to lie within image area that was searched during 2RE
* production.

	write(*,40) bsep*60.0/dtor
	write(*,*)
 40	format('  Separation: ',f5.1, ' arcmin')
* loop for next set of ra & dec in file
	if (lun1.ne.0) goto 50
 299    continue
* close input file
        if (lun1.ne.0) close(lun1)
* close output file
	if (lun2.ne.0) close(lun2)

 300    continue
	END


