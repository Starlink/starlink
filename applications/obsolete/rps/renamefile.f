        subroutine renamefile( file1, file2)
	implicit none
	character*(*) file1, file2

	integer l1, l2, status
	integer mdh_endword

	l1 = mdh_endword(file1)
	l2 = mdh_endword(file2)
	call sys_docommand( 'mv '//file1(:l1)//' '//file2(:l2), status) 
	if (status .ne.0) write(*,*) ' Rename error ', 
     &		file1(:l1)//' to '//file2(:l2)

 	end
