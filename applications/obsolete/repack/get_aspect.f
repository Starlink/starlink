*+GET_ASPECT Get a record from the aspect file for specified MJD
        SUBROUTINE GET_ASPECT(MJD, ASPECT, STATUS)
	implicit none
	include 'ASR_EXPATT.INC'

* Input
	double precision MJD
* Output
	record /attstruc/ aspect
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

        call ftmahd(aspect.iatt, 2, hdutyp, status)
	call ftgkyj(aspect.iatt, 'NAXIS2',  nrecs, c_dum,  status)

* Read first record.
	if (aspect.c_rec .eq. 0)  then
	  aspect.c_rec = 1
	  call ftgcvd(aspect.iatt, 2, aspect.c_rec, 1, 1, -1,
     &                  aspect.rec_end_mjd, anyf, status)
	endif


* Find first record that has end_mjd >= MJD
	if (aspect.rec_end_mjd .lt. mjd) then
	  do while (aspect.rec_end_mjd .lt. mjd)
	    call ftgcvd(aspect.iatt, 2, aspect.c_rec, 1, 1, -1,
     &                  aspect.rec_end_mjd, anyf, status)
	    aspect.c_rec = aspect.c_rec + 1
	    if (status .ne. 0) then
	      call printerror(status)
	      status = 1
	      return
	    endif
	  enddo
	  aspect.c_rec = aspect.c_rec - 1
	endif

* Read rest of record.
        call ftgcvd(aspect.iatt, 1, aspect.c_rec, 1, 1, -1,
     &                             aspect.rec_start_mjd, anyf, status)
        call ftgcvj(aspect.iatt, 3, aspect.c_rec, 1, 1, -1,
     &                             aspect.end_sclock, anyf, status)
        call ftgcvj(aspect.iatt, 4, aspect.c_rec, 1, 1, -1,
     &                             aspect.start_sclock, anyf, status)
        call ftgcvj(aspect.iatt, 5, aspect.c_rec, 1, 1, -1,
     &                             aspect.ref_sclock, anyf, status)
	call ftgcve(aspect.iatt, 6, aspect.c_rec, 1, 9, -1,
     &                             aspect.fov2sky, anyf, status)
	call ftgcve(aspect.iatt, 7, aspect.c_rec, 1, 1, -1,
     &                             aspect.scan_rate_az, anyf, status)
	call ftgcve(aspect.iatt, 8, aspect.c_rec, 1, 1, -1,
     &                             aspect.scan_rate_el, anyf, status)
	call ftgcve(aspect.iatt, 9, aspect.c_rec, 1, 1, -1,
     &                             aspect.durn_days, anyf, status)


	end
