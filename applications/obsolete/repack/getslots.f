*+GETSLOTS returns set of good times from slots reservoir file
	SUBROUTINE GETSLOTS(lunit,SMJD,EMJD,MREJ,BREJ,FILT,SW,EW,
     :							CF,NW,STATUS)
	IMPLICIT NONE

* Input
	double precision		smjd,emjd	! Window to sort on
	integer		filt				! Filter number
	integer		lunit				! FITS file
	logical		mrej				! Reject moon times
	logical		brej				! Reject high bgnd

* Output
	double precision sw(*),ew(*)	    		! Start/end times
	integer		cf(*)
	integer		nw,ns,nc			! # windows
	integer 	status

* P. McGale Sept 92.
* P. McGale May  93. - take account of non-monotonic time sequences.
* P. McGale Apr 95 - UNIX mods.
*-
* Local variables.
	integer		maxel
	    parameter (maxel=50000)
	double precision s_se(maxel),e_se(maxel)        ! Stnd evnts strt/end
	double precision s_mn(maxel),e_mn(maxel)	! Moon out FOV times
	double precision s_bk(maxel),e_bk(maxel)	! Good bkgnd times
	double precision s_flt(maxel),e_flt(maxel)	! Filter times
	integer		conf(maxel)			! Config
	integer		work(maxel)			! Temp. work array
	integer		indx(maxel)			! Indx to unsort. lists
	integer 	nse				! # stnd evnts winds
	integer 	nmn				! # moon winds
	integer 	nbk				! # good bkgnd winds
	integer 	nflt				! # filter winds
	integer		i
	integer hdutyp
	character*80	c_dum
	logical		anyf

*
	if (status .ne. 0) return


* Read in Standard Event times and merge.
        call ftmahd(lunit, 4, hdutyp, status)
	CALL ftgkyj(lunit, 'NAXIS2',    nse, c_dum,  status)
	CALL ftgcvd(lunit, 1, 1, 1, nse, -1, s_se, anyf, status)
	CALL ftgcvd(lunit, 2, 1, 1, nse, -1, e_se, anyf, status)

* Ensure times are sorted in ascending order.
	call bsortd(s_se,nse,indx)
	call bsortd(e_se,nse,indx)
	nw = nse
	do i=1,nw
	  sw(i) = s_se(i)
	  ew(i) = e_se(i)
	enddo
	if (status .ne. 0 ) then
	  write(*,*)
	  write(*,*) '   Error in GETSLOTS.'
	  write(*,*) '   Problem at standard events times merge.'
	  return
	endif
	call wmerge(smjd,emjd,1,sw,ew,nw)

* If want to reject moon in FOV times merge.
	if (mrej) then
          call ftmahd(lunit, 5, hdutyp, status)
	  CALL ftgkyj(lunit, 'NAXIS2',    nmn, c_dum,  status)
	  CALL ftgcvd(lunit, 1, 1, 1, nmn, -1, s_mn, anyf, status)
	  CALL ftgcvd(lunit, 2, 1, 1, nmn, -1, e_mn, anyf, status)
	  if (status .ne. 0 ) then
	    write(*,*)
	    write(*,*) '   Error in GETSLOTS.'
	    write(*,*) '   Problem at moon times merge.'
	    return
	  endif
	  call wmerge(s_mn,e_mn,nmn,sw,ew,nw)
	endif

* If want to reject high background times merge.
	if (brej) then
	  if (filt .eq. 8) then
            call ftmahd(lunit, 7, hdutyp, status)
	    CALL ftgkyj(lunit, 'NAXIS2',    nbk, c_dum,  status)
	    CALL ftgcvd(lunit, 1, 1, 1, nbk, -1, s_bk, anyf, status)
	    CALL ftgcvd(lunit, 2, 1, 1, nbk, -1, e_bk, anyf, status)
	  elseif (filt .eq. 6) then
            call ftmahd(lunit, 8, hdutyp, status)
	    CALL ftgkyj(lunit, 'NAXIS2',    nbk, c_dum,  status)
	    CALL ftgcvd(lunit, 1, 1, 1, nbk, -1, s_bk, anyf, status)
	    CALL ftgcvd(lunit, 2, 1, 1, nbk, -1, e_bk, anyf, status)
	  endif
	  if (status .ne. 0 ) then
	    write(*,*)
	    write(*,*) '   Error in GETSLOTS.'
	    write(*,*) '   Problem at background times merge.'
	    return
	  endif
	  call wmerge(s_bk,e_bk,nbk,sw,ew,nw)
	endif

* Merge with appropriate filter times.
	if (filt .eq. 8) then
            call ftmahd(lunit, 2, hdutyp, status)
	    CALL ftgkyj(lunit, 'NAXIS2',    nflt, c_dum,  status)
	    CALL ftgcvd(lunit, 1, 1, 1, nflt, -1, s_flt, anyf, status)
	    CALL ftgcvd(lunit, 2, 1, 1, nflt, -1, e_flt, anyf, status)
	    CALL ftgcvj(lunit, 3, 1, 1, nflt, -1, conf, anyf, status)
	elseif (filt .eq. 6) then
            call ftmahd(lunit, 3, hdutyp, status)
	    CALL ftgkyj(lunit, 'NAXIS2',    nflt, c_dum,  status)
	    CALL ftgcvd(lunit, 1, 1, 1, nflt, -1, s_flt, anyf, status)
	    CALL ftgcvd(lunit, 2, 1, 1, nflt, -1, e_flt, anyf, status)
	    CALL ftgcvj(lunit, 3, 1, 1, nflt, -1, conf, anyf, status)
	endif
* Ensure filt times are sorted in ascending order and re-index conf to match
* the new order.  (indx should be the same for s_flt and e_flt!).
	call bsortd(s_flt,nflt,indx)
	call bsortd(e_flt,nflt,indx)
	do i=1,nflt
	  work(i) = conf(i)
	enddo
	do i=1,nflt
	  conf(i) = work(indx(i))
	enddo
	if (status .ne. 0 ) then
	  write(*,*)
	  write(*,*) '   Error in GETSLOTS.'
	  write(*,*) '   Problem at filter times merge.'
	  return
	endif
	call wmerge(s_flt,e_flt,nflt,sw,ew,nw)

* Reset the config vector
	do ns = 1,nw
	  do nc = 1,nflt
	    if (sw(ns) .lt. e_flt(nc)) then
	      cf(ns) = conf(nc)
	      goto 100
	    endif
	  enddo
100	enddo



	end
