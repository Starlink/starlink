*+ BGD_GOOD  Decides if background is good at particular time.
	LOGICAL FUNCTION BGD_GOOD(FIL,EVT)

	implicit  none
	include 'SMAPDEF.INC'

* Input
	integer	 fil				! Filter number
	integer  evt				! Event time

* P. McGale Jun 92.
* P. McGale Apr 95. - read FITS files instead.
*-

* Local constants
	integer maxslots			!  LEVS array size.
 	 parameter (maxslots=250000*32)
* Local vars
	double precision b_mjd			! Base MJD of LEVS string
	double precision d_tslot		! No of tslots in a day
	double precision mjd			! Date to test.
	double precision conv
	  parameter ( conv = 32.d0*86400.d0)
*
	integer levs_w(maxslots/32)    		! LEVS words
	integer nels				! No of LEVS words
	integer tslot				! Time length of LEVS bit
	integer levs(maxslots)			! LEVS good or bad
	integer wordno				! Current LEVS word
	integer slot				! Current LEVS bit
	integer bitno				! Current bit in LEVS word
 	integer status
	integer fc, lc
	integer blksz
	integer lunit
	integer hdutyp
*
	character*80	c_dum
	character*80    re_res			! Path to reserv. files
	character*2     filter			! Filter using
*
	logical		anyf
* Function
	character*10    cal_filt_n2s		! Filter no. to string
*
	logical		first			! Decide if func already called
* Save some values
	save		first,b_mjd,nels,levs_w,tslot
*
	data	status/0/
	data	first/.TRUE./

* If first call of function then open up relevant background discrimination
* file and read in value.

	if (first) then
* Decide what filter using, and read in relevant mask array.
	  call getenv("RECAL", re_res)
	  if (re_res .eq. ' ') then
	    write(*,*)
	    write(*,*)'   BGD_GOOD.'
	    write(*,*)'   Can''t get RECAL environment variable.'
	    write(*,*)
	    bgd_good =.FALSE.
	    status = 1
	    return
	  endif
	  call chr_fandl(re_res, fc, lc)
          call ftgiou(lunit, status)
	  call ftopen(lunit, re_res(fc:lc)//'/re_slots.fit', 0,
     &                                             blksz, status)
	  if (status .ne. 0) then
	    write(*,*) '   Error in BGD_GOOD - opening S3_SLOTS file'
	    bgd_good = .FALSE.
	    return
	  endif

	  filter = cal_filt_n2s(fil)
	  if (filter .eq. 'S1' ) then
* Read in scalars and vector.
	    call ftmahd(lunit, 6, hdutyp, status)
	    CALL ftgkyd(lunit, 'MJD1MASK', b_mjd, c_dum, status)
	    CALL ftgkyj(lunit, 'NAXIS2',    nels, c_dum,  status)
	    if (nels .gt. maxslots/32) then
	       write(*,*) '   BGD_GOOD can''t handle array size.'
	       bgd_good = .FALSE.
	       return
	    endif
	    CALL ftgcvj(lunit, 1, 1, 1, nels, -1, levs_w, anyf, status)
	    CALL ftgkyj(lunit, 'T1MASK',  tslot, c_dum,  status)
	  else if (filter .eq. 'S2' ) then
	    call ftmahd(lunit, 6, hdutyp, status)
	    CALL ftgkyd(lunit, 'MJD2MASK', b_mjd, c_dum, status)
	    CALL ftgkyj(lunit, 'NAXIS2',    nels, c_dum,  status)
	    if (nels .gt. maxslots/32) then
	       write(*,*) '   BGD_GOOD can''t handle array size.'
	       bgd_good = .FALSE.
	       return
	    endif
	    CALL ftgcvj(lunit, 2, 1, 1, nels, -1, levs_w, anyf, status)
	    CALL ftgkyj(lunit, 'T2MASK',  tslot, c_dum,  status)
	  else
	     status=1
	     write(*,*)  '   No valid mask array available!'
	  endif

	  if (status .ne. 0 ) then
	       write(*,*) '   Problem with FITS in BGD_GOOD.'
	       bgd_good = .FALSE.
	       return
	  endif
	  CALL ftclos (lunit, status)
	  CALL ftfiou (lunit, status)


* Note that condition has been run.
	  d_tslot = 86400.D0/real(tslot)
	  first = .FALSE.
	endif

* See if background good or bad at time MJD.
	mjd    = (evt/conv) + s2_ref_mjd
	slot   = int((mjd-b_mjd)*d_tslot)
	if (slot .lt. 0) then
	  write(*,*)'   WARNING: Event predates mask file.  Event rejected.'
	  bgd_good = .FALSE.
	else
	  bitno  = mod(slot,32)
	  wordno = int(slot/32)+1
	  levs(slot) = ibits(levs_w(wordno),bitno,1)
	  if (levs(slot) .eq. 1) then
	       bgd_good = .TRUE.
	  else
	       bgd_good = .FALSE.
          endif
	endif

 	end
