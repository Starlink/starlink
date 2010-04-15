*+GETSHED Read the header records from a small map file
	SUBROUTINE GETSHED (IMAP, HEAD, STATUS)
	IMPLICIT NONE
	INCLUDE 'SMAPDEF.INC'
* Input
	INTEGER		IMAP		! Small map unit #
* output
	RECORD /HEADEF/ HEAD		! Header rec struct
	INTEGER		STATUS		! Status return
* M. Denby Jan-88
* P McGale Apr 95 Unix mods
*-
* Local
	integer		hdu		! Header Data unit.
	integer		hdutyp
	integer		idum
	character*80	cdum


	IF (STATUS .NE. 0) RETURN

* Read some keywords from the primary header.
	hdu = 1
	call ftmahd(imap, hdu, hdutyp, status)
	if (hdutyp .ne. 0) then
	  WRITE(*,*) '   Error in GETSHED.'
	  write(*,*) '   Not reading primary header.'
          status = 1
	  return
	endif

	call ftgkys(imap, 'DATE', head.cre_date, cdum, status)
	call ftgkys(imap, 'TELESCOP', head.observatory, cdum, status)
	call ftgkys(imap, 'INSTRUME', head.instrument, cdum, status)
	call ftgkys(imap, 'TARGET',   head.target, cdum, status)
* See if it is a merged event file.
	call ftgkyj(imap, 'NMAPS',    head.nmap, cdum, status)
        if (status .eq. 202) then
	  head.nmap = 1
	  status = 0
	endif

* Goto bintable extension and get start/end times etc.
	hdu = 2
	call ftmahd(imap, hdu, hdutyp, status)
	if (hdutyp .ne. 2) then
	  WRITE(*,*) '   Error in GETSHED.'
	  write(*,*) '   Not reading bintable header.'
          status = 1
	  return
	endif
	call ftgkyj(imap, 'NAXIS2', head.nevent, cdum, status)
	call ftgkyj(imap, 'DETNAM', head.detector, cdum, status)
	call ftgkyd(imap, 'REF_MJD', head.ref_mjd, cdum, status)
	call ftgkys(imap, 'REVISION', head.revision, cdum, status)
	call ftgkyj(imap, 'TDMIN1', idum, cdum, status)
	head.base_mjd = (dble(idum) / (32.d0 * 86400.d0)) + head.ref_mjd
	call ftgkyj(imap, 'TDMAX1', idum, cdum, status)
	head.end_mjd = (dble(idum) / (32.d0 * 86400.d0)) + head.ref_mjd

* Fill out some other info, assuming true.
	head.mode     = 'S'
	head.observer = 'ROS-UKSC'
	head.ref_date = s2_ref_date

999	IF (STATUS .NE. 0) THEN
	  WRITE(*,*) '   Error in GETSHED.'
	ENDIF

	END
