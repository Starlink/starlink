*+DBM_OPMAP Gets the unit number for a small map.
	SUBROUTINE DBM_OPMAP (PARAM, FNAME, LUNIT, WRITE, STATUS)
	IMPLICIT NONE
* Input
	CHARACTER*(*) PARAM		! File name parameter
	integer	      WRITE		! File is READONLY
* Output
	CHARACTER*(*) FNAME		! File name
	INTEGER       LUNIT		! Unit number
	INTEGER	      STATUS		! Status return
* P. McGale Apr 95.
*-
* Local
	integer	      blksz

	IF (STATUS .NE. 0) RETURN

	CALL PAR_GETLC (PARAM, FNAME, STATUS)
	CALL PAR_CANCL (PARAM, STATUS)
	CALL MX_DEFEXT (FNAME, '.fit', FNAME)

* Open file ...
	CALL ftgiou(LUNIT, status)
        call ftopen(lunit, fname, write, blksz, status)

	if (status .ne. 0) call printerror(status)

	END

