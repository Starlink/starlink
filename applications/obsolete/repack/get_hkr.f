*+GET_HKR Get a record from the HKR file for specified UT
        SUBROUTINE GET_HKR(UT2, HKR, STATUS)
	implicit none
	include 'HKR_BUFFER.INC'

* Input
	integer UT2			! UT 1/2 secs
* Output
	record /hkr_buffer/ hkr
	integer status


* P McGale May 95
*-
* Local
	integer hdutyp
	integer nrecs
	integer blksz
	character*80	c_dum
	logical		anyf
*

	if (status .ne. 0) return

        call ftmahd(hkr.ihkr, 2, hdutyp, status)
	call ftgkyj(hkr.ihkr, 'NAXIS2',  nrecs, c_dum,  status)

* Read first record.
	if (hkr.c_rec .eq. 0)  then
	  hkr.c_rec = 1
	  call ftgcvj(hkr.ihkr, 1, hkr.c_rec, 1, 1, -1,
     &                  hkr.end_ut, anyf, status)
	endif


* Find first record that has end_ut >= UT2
	if (hkr.end_ut .lt. UT2) then
	  do while (hkr.end_ut .lt. UT2)
	    call ftgcvj(hkr.ihkr, 1, hkr.c_rec, 1, 1, -1,
     &                  hkr.end_ut, anyf, status)
	    hkr.c_rec = hkr.c_rec + 1
	    if (status .ne. 0) then
	      write(*,*) '   Error in GET_HKR.'
	      call printerror(status)
	      return
	    endif
	  enddo
	  hkr.c_rec = hkr.c_rec - 1
	endif

* Read rest of record.
        call ftgcvj(hkr.ihkr, 2, hkr.c_rec, 1, 1, -1,
     &                             hkr.start_ut, anyf, status)
        call ftgcvj(hkr.ihkr, 3, hkr.c_rec, 1, 2, -1,
     &                             hkr.flag, anyf, status)
        call ftgcvj(hkr.ihkr, 4, hkr.c_rec, 1, 64, -1,
     &                             hkr.tevs, anyf, status)
        call ftgcvj(hkr.ihkr, 5, hkr.c_rec, 1, 64, -1,
     &                             hkr.vevs, anyf, status)
        call ftgcvj(hkr.ihkr, 6, hkr.c_rec, 1, 64, -1,
     &                             hkr.levs, anyf, status)
        call ftgcvj(hkr.ihkr, 7, hkr.c_rec, 1, 64, -1,
     &                             hkr.aevs, anyf, status)
        call ftgcvj(hkr.ihkr, 8, hkr.c_rec, 1, 64, -1,
     &                             hkr.xtra, anyf, status)
        call ftgcvj(hkr.ihkr, 9, hkr.c_rec, 1, 64, -1,
     &                             hkr.fevs, anyf, status)

	if (status .ne. 0) then
	  write(*,*) '   Error in GET_HKR.'
	  call printerror(status)
	endif

	end
