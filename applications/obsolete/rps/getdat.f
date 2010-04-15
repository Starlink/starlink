	subroutine getdat(tstring)
	implicit none

	character*(*) tstring

* Local variables
	integer nticks, status, secs, mins, hours, day, month, year
	integer wday, yday, isdst, tstrct(2)
	character*18 xstring

* Executable code

	call psx_time(nticks, status)
	call psx_localtime(nticks, secs, mins, hours, day, month,
     *		year, wday, yday, isdst, tstrct, status)

	write(xstring,'(i3,a1,i2.2,a1,i2.2,a1,i2,a1,i2.2,a1,i2)')
     &  hours, ':', mins, ':', secs, ' ', day, '/', month+1,'/',year

	tstring = xstring

	end
