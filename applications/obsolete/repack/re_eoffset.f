*+ RE_EOFFSET   Offset new coords from old coords along ecliptic latitude
	PROGRAM RE_EOFFSET
	implicit none
	include 'CONSTANTS.INC'
* Input
	double precision s_ra,s_dec	! Source RA and DEC
	real offset			! Required offset (arcmin)

* Output
	double precision o_ra,o_dec		! Offset RA and DEC

* P.McGale Jan 92.
* P McGale May 95 - UNIX mods
*-
	double precision s_lo,s_la,o_lo,o_la	! Ecliptic coords.
	integer pstat
	character*255 str
	integer   len
	data pstat/0/

	call par_cmdl(' ',pstat)
	if (pstat.ne.0) stop ' Problem with PAR_DCL startup.'

* Get input data
	call par_get0d('S_RA source RA',s_ra,pstat)
	call par_get0d('S_DEC source DEC',s_dec,pstat)
	call par_get0r('OFFSET offset requires (arcmin)',offset,pstat)
	if (pstat.ne.0) stop ' Problem with PAR_GET.'

* Do offset, work in radians, final output in degrees.
	s_ra = s_ra*dble(DTOR)
	s_dec = s_dec*dble(DTOR)
	call cel2ec(s_ra,s_dec,s_lo,s_la)
	o_lo = s_lo
	o_la = s_la + dble((DTOR*(offset/60.0)))
	call ec2cel(o_lo,o_la,o_ra,o_dec)
	o_ra = o_ra*dble(RTOD)
	o_dec = o_dec*dble(RTOD)

* Output values to current process.
	write(*,*) o_ra, o_dec


	END
