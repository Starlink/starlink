*+SORT_DEF Sort control definition
      STRUCTURE /SORT_DEF/
         CHARACTER*80	MODE			! Mode 'S' or 'P'
	 CHARACTER*80	DTYPE			! 'IMDS' or 'EVDS'
	 CHARACTER*40	TARG			! target name (or pos)
	 CHARACTER*40	OBSR			! Oberver name
	 CHARACTER*40	OBSY			! 'ROSAT'
	 CHARACTER*40	INST			! eg 'WFC' or 'XRT'
	 CHARACTER*255	EVE			! Eve event file
	 CHARACTER*6	MDR			! Original MDR file

	 INTEGER	NBINS			! Time series num bins
	 INTEGER	NXPIX, NYPIX		! Image dimensions
         INTEGER        NMAPS             	! # of maps to process
         INTEGER        MLO(3000), MLA(3000)	! Indices of small maps
	 INTEGER	FILT			! WFC filter #
	 INTEGER	DET			! Detector #

         REAL		ARA, ADEC 		! Axis Centre (rads)
	 REAL		FRA, FDEC		! Field centre (rads)
         REAL		DAZ, DEL           	! local extents (rads)
	 REAL		INR, OUTR		! Inner/ outer radii (rads)
         REAL		ROLL			! Cel N to image N (rad)
	 REAL		XPIXEL, YPIXEL		! Pixel size (rads)
	 REAL		XCMIN, YCMIN		! Image bottom left (rad
         REAL           IRIS                    ! Iris radius (rads)

	 LOGICAL	BREJ			! Backgd reject flag
	 LOGICAL	IGBGD			! Ignore backgnd mask
	 LOGICAL	MREJ			! Moon reject flag
	 LOGICAL  	IGMOON			! Ignore Moon mask

         REAL*8      	SMJD, EMJD		! Overall sort time
	 REAL*8		RMJD			! Event time tag base
      ENDSTRUCTURE

