*+MX_ROOT Extracts 'root' of a filename, i.e. strips off any dev/dir etc.
	SUBROUTINE MX_ROOT(INFIL, ROOT)
	CHARACTER*(*) INFIL, ROOT
*INFILE	input	filename with or without extension
*ROOT	output	filename with any node,dev,dir,version specifications removed
*Restrictions : intended for VAX/VMS filename formats only
*-Author	Clive Page	1986 Jan 26.
* P McGale Apr 95, UNIX version
	integer i, l, istrt, iend

C search for last /
	istrt = 1
	do i=1,len(infil)
	  if (infil(i:i) .eq. '/') istrt = i + 1
	enddo

C find posn of extension or last sig. char if no extension.
	iend = 0
	DO L=LEN(INFIL),1,-1
	  IF(INFIL(L:L).EQ.'.') THEN
	    IEND = L - 1
	    GOTO 10
	  END IF
	  IF(iend .eq. 0 .and. INFIL(L:L).NE.' ') THEN
	    IEND = L
	  END IF
	END DO

10	CONTINUE

	if (istrt .ge. iend) then
	  root = infil(1:iend)
	else
 	  root = infil(istrt:iend)
	endif

	END
