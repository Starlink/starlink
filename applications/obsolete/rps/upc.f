	subroutine upc(string)
	implicit none
	character*(*) string
	integer i

	do i=1,len(string)
	  if (string(i:i) .ge. 'a' .and. string(i:i) .le. 'z')
     &	  string(i:i) = char( ichar(string(i:i)) .and. 95)
	end do

	end
