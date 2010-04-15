*+EV2MJD Convert event time to MJD
	double precision function ev2mjd(evt)
	include		'SMAPDEF.INC'
	double precision		conv
	parameter	(conv=32.d0*86400.d0)
	integer		evt

	ev2mjd = (evt/conv) + s2_ref_mjd

	end
