	subroutine file_copy(file1, file2, status)
	implicit none
	character*(*) file1, file2

* Local
	integer l1, l2, status
	integer mdh_endword

	l1 = mdh_endword(file1)
	l2 = mdh_endword(file2)
	call sys_docommand('cp '//file1(:l1)//' '//file2(:l2), status)
	if (status .ne. 0) write(*,*) ' Error, filecopy '//
     &	file1(:l1)//' to '//file2(:l2)

	end
