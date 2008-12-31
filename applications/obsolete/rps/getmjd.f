	subroutine getmjd(dmjd)
	implicit none
        double precision dmjd

* Local variables
	integer nticks, status, secs, mins, hours, day, month, year
	integer wday, yday, isdst, tstrct, istat

* Executable code
	call psx_time(nticks, status)
	call psx_localtime(nticks, secs, mins, hours, day, month,
     *		year, wday, yday, isdst, tstrct, status)

* month = 0 for jan, year is last 2 digits

	call sla_caldj( year, month+1,day, dmjd, istat)
	if (istat.ne.0) write(*,*) ' Bad status, sla_caldj with ',year, month+1,day

	end
