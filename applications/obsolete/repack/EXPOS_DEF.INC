*+EXPOS_DEF - Definition for structure used in EXPOS
      STRUCTURE /EXPOS_DEF/
	 CHARACTER*255	EVE		! EVE event file
 
         INTEGER        NPAIR           ! Number of time windows
	 INTEGER	BMJD		!
	 INTEGER	FILT
	 INTEGER	DET
	 INTEGER	CONF(50000)
 
         REAL		XPIX		! Exposure grid resolution
         REAL		DUR             ! duration fo observation (secs)
         REAL           IRIS		! Radius of iris (Rads)
	 REAL		TDELT		! Time step
 
         DOUBLE PRECISION    	SW(50000)        ! start time windows
         DOUBLE PRECISION    	EW(50000)        ! end time windows
	 DOUBLE PRECISION    	BUTC
         DOUBLE PRECISION    	DAZ             ! Az extent of image (Rads)
         DOUBLE PRECISION    	DEL             ! El extent of image (Rads)
         DOUBLE PRECISION    	FRA		! RA of field center (Rads)
         DOUBLE PRECISION    	FDEC		! DEC of field center (Rads)
	 DOUBLE PRECISION	ARA		! RA of axis centre (Rads)
	 DOUBLE PRECISION	ADEC		! DEC of axis centre (rads)
         DOUBLE PRECISION    	ROLL		! Roll exp image to N (Rads)
      END STRUCTURE
 
 
