	subroutine rhelp(file, topic, istat)
	implicit none

* Calling Arguments
	character*(*) file	! Library
	character*(*) topic	! topic
	integer istat		! status

* Local variables
	integer glun, lp, lf, jflags/1/
	character*1 dum
	character*132 path

	integer mdh_endword
	integer hlp_help
	external hlp_nametr, hlp_insub, hlp_outsub

* Executable code

	if (istat .ne. 0) goto 99
	call getlun(glun)
	call sys_getenv('RPS_AUX',path, lp, istat)
	lf = mdh_endword(file)
*	write(*,*) ' RPS_AUX: '//path(:lp)
	call hlp_nametr(1, path(:lp), dum, istat)
	if (istat .ne. 0) goto 99
	call hlp_nametr(2, '.shl', dum, istat)
	if (istat .ne. 0) goto 99

*	write(*,*) ' help lib ',file
*	write(*,*) ' topic ',topic
	istat = hlp_help(hlp_outsub, 132, topic, glun,
     &		file(:lf), jflags,
     &		hlp_insub, hlp_nametr)

99	continue
	end
