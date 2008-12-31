	subroutine delfile(file)
	implicit none
	character*(*) file

* Local
	integer status, l
	integer mdh_endword

	l = mdh_endword(file)
	call sys_docommand('rm '//file(:l), status)
	if (status .ne.0) write(*,*) ' Error deleting '//file(:L)

	end
