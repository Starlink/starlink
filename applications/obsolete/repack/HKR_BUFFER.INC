*+HKR_BUFFER Structure definition of reduced HK rates record
	STRUCTURE /HKR_BUFFER/
	  INTEGER	END_UT		! ISAM record key (1/2 sec after
	  INTEGER	START_UT	! BASE_MJD = 47892.0)
	  INTEGER	FLAG (2)	! 64 8 second Slot flags (1 = valid)
	  INTEGER	TEVS(64)	! Instrument count rates :
	  INTEGER	VEVS(64)
	  INTEGER	LEVS(64)
	  INTEGER	AEVS(64)
	  INTEGER	XTRA(64)	! Retained for compat. with S2
	  INTEGER	FEVS(64)
	  integer	c_rec		! Pointer to current record
	  integer	ihkr		! FITS i/o #
	END STRUCTURE

