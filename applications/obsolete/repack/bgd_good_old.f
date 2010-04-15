*+ BGD_GOOD  Decides if background is good at particular time.
	LOGICAL FUNCTION BGD_GOOD(FIL,EVT)

	implicit  none
	include 'SMAPDEF.INC'
	include 'DAT_PAR'

* Input
	integer	 fil				! Filter number
	integer  evt				! Event time

* P. McGale Jun 92.
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
*
	character*(DAT__SZLOC) locin		! HDS locator
	character*2     filter			! Filter using
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
	  call hds_open('re_slots','read',locin,status)
	  if (status .ne. 0) then
	    write(*,*) '   Error in BGD_GOOD - opening S3_SLOTS file'
	    bgd_good = .FALSE.
	    return
	  endif

	  filter = cal_filt_n2s(fil)
	  if (filter .eq. 'S1' ) then
* Read in HDS scalars and vector.
	    CALL CMP_GET0D(locin, 'mjd1mask', b_mjd, status)
	    CALL CMP_GET0I(locin, 'n1mask',    nels,  status)
	    if (nels .gt. maxslots/32) then
	       write(*,*) '   BGD_GOOD can''t handle array size.'
	       bgd_good = .FALSE.
	       return
	    endif
	    CALL CMP_GET1I(locin, 's1mask',  nels,  levs_w, nels, status)
	    CALL CMP_GET0I(locin, 't1mask',   tslot, status)
	  else if (filter .eq. 'S2' ) then
	    CALL CMP_GET0D(locin, 'mjd2mask', b_mjd, status)
	    CALL CMP_GET0I(locin, 'n2mask',    nels,  status)
	    if (nels .gt. maxslots/32) then
	       write(*,*) '   BGD_GOOD can''t handle array size.'
	       bgd_good = .FALSE.
	       return
	    endif
	    CALL CMP_GET1I(locin, 's2mask',  nels,  levs_w, nels, status)
	    CALL CMP_GET0I(locin, 't2mask',   tslot, status)
	  else
	     status=1
	     write(*,*)  '   No valid mask array available!'
	  endif

	  if (status .ne. 0 ) then
	       write(*,*) '   Problem with HDS in BGD_GOOD.'
	       bgd_good = .FALSE.
	       return
	  endif
	  CALL HDS_CLOSE (locin,status)


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
