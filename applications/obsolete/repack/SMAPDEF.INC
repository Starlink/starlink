*+SMAPDEF Include file defines structure of a small map record
*-
	INCLUDE		'CONSTANTS.INC'  	      ! Pull in Pi etc etc

	INTEGER			NLON, NLAT    ! # ecliptic maps in sky
	  PARAMETER   	      ( NLON = 192, NLAT = 90 )
	INTEGER			MAXEV	      ! # events per map rec.
	  PARAMETER   	      ( MAXEV = 83 )
	INTEGER			NBINS(NLAT)   ! # maps per circumference
	  DATA NBINS /12,2*24,4*48,8*96,60*192,8*96,4*48,2*24,12/
	  SAVE NBINS
	INTEGER			LENREC	      ! record length (reals)
	  PARAMETER	      ( LENREC = 256 )
	INTEGER			SLOTBUF	      ! Max # slots
	  PARAMETER	      ( SLOTBUF = 10200 )
	INTEGER			SLOTREC	      ! # slots / record
	  PARAMETER	      ( SLOTREC = 51 )

	DOUBLE PRECISION  LONPIX		      ! Long Pixel size (secs)
	  PARAMETER	      ( LONPIX  = 20.0D0 )
	DOUBLE PRECISION  LATPIX	              ! Lat Pixel size (secs)
	  PARAMETER	      ( LATPIX  = 10.0D0 )
	DOUBLE PRECISION  LINPIX		      ! Lin pixel size (secs)
	  PARAMETER	      ( LINPIX  = 72.D0 )
	DOUBLE PRECISION  R2LONP
	  PARAMETER	      ( R2LONP  = 3600.D0/(DTOR*LONPIX) )
	DOUBLE PRECISION  R2LATP
	  PARAMETER	      ( R2LATP  = 3600.D0/(DTOR*LATPIX) )
	DOUBLE PRECISION  R2LINP
	  PARAMETER	      ( R2LINP  = 3600.D0/(DTOR*LINPIX) )
	DOUBLE PRECISION  S2_REF_MJD		      ! Event time tag base
	  PARAMETER	      ( S2_REF_MJD = 47892.D0 )
	CHARACTER*11		S2_REF_DATE   ! Date of time tag base
	  PARAMETER	      ( S2_REF_DATE = '01-JAN-1990' )


	STRUCTURE /EBLOCK/		! structure of small map record
	  INTEGER	LINK		! Pointer to next rec in linked list
	  BYTE		NEV		! No. events in record
	  INTEGER	MAPX		! Map Az. index
	  INTEGER	MAPY		! Map El. index
	  INTEGER	STIME		! UT (1/32 sec) of first event in rec
	  INTEGER	ETIME		! UT (1/32 sec) of last event in rec
	  BYTE		FILT
	  BYTE		%FILL(6)
	  STRUCTURE /EVENTS/ EV(MAXEV)
	    BYTE	LINX		! Lin. det. Az
	    BYTE	LINY		! Lin. det. El
	    INTEGER*2	ELON		! Ecliptic Long.
	    INTEGER*2	ELAT		! Ecliptic Lat.
	    INTEGER	TIME		! Event UT from REF_MJD (1/32 sec)
	    INTEGER*2	XTRA		! FEVS (12 bits) + 4 flags
	  ENDSTRUCTURE
	ENDSTRUCTURE


	STRUCTURE /MAPDEF/		! Structure of index info
	  INTEGER	HEAD		! Head rec of linked list
	  INTEGER	TAIL		! Tail rec of linked list
	  INTEGER	NBLOC		! No. recs in linked list
	ENDSTRUCTURE



	STRUCTURE /HEADEF/		! small map header records
	  UNION
	    MAP
	      INTEGER		NLON		! No. of Az small maps
	      INTEGER		NLAT		! No. of El small maps
	      INTEGER		LENREC		! Rec length (bytes)
	      INTEGER		MAXEV		! No. events / rec
	      INTEGER		ENDREC		! Last used rec in file
	      INTEGER		NMAP		! # active maps in file
	      INTEGER		NEVENT		! #events in file
	      CHARACTER*4	REVISION	! File revision
	      REAL		PIXEL		! Map pixel size (sec)
	      REAL		LPIXEL		! Linearised pixel (sec)

	      DOUBLE PRECISION	REF_MJD		! Zero for event time
	      CHARACTER*11	REF_DATE	! Date of 0:0 on REF_MJD
	      DOUBLE PRECISION	BASE_MJD	! MDR start time
	      CHARACTER*11	BASE_DATE	! Date of 0:0 on BASE
	      DOUBLE PRECISION	END_MJD		! MDR end time
	      INTEGER		BASE_ORBIT	! MDR start orbit
	      INTEGER		END_ORBIT	! MDR end orbit
	      CHARACTER*10	MDR_SEQ		! MDR ident + Spare
	      CHARACTER*9	CRE_DATE	! File creation date

	      CHARACTER*1	MODE		! 'S' or 'P'
	      CHARACTER*40	TARGET		! Target name/position
	      CHARACTER*40	OBSERVATORY	! 'ROSAT'
	      CHARACTER*40	INSTRUMENT	! Eg 'WFC'
	      CHARACTER*40	OBSERVER	! PI name or 'SURVEY'
	      REAL		NOM_RA		! scan axis (degs)
	      REAL		NOM_DEC		! scan axis (degs)
	      LOGICAL		ZOOM
	      INTEGER		DETECTOR
	      INTEGER		NSLOT
	      INTEGER		NCREC

	      DOUBLE PRECISION	STIME(SLOTBUF)	! Start windows (MJDs)
	      DOUBLE PRECISION	ETIME(SLOTBUF)	! End windows (MJDs)
	      INTEGER		CONFIG(SLOTBUF)	! 32 bits config info
	    END MAP

	    MAP
	      BYTE		PACK(318)
	    ENDMAP
	  ENDUNION
	ENDSTRUCTURE

* Define bit fields in the config word(s)
	INTEGER		CW_FIL
	  PARAMETER    (CW_FIL = '07'O)
					! Filter # Bits 0-2
	INTEGER		CW_FMT
	  PARAMETER    (CW_FMT = '30'O)
					! Format  Bit 3-4
	INTEGER		CW_ZOM
	  PARAMETER    (CW_ZOM = '40'O)
					! Zoom flag Bit 5
	INTEGER	        CW_WIN
	  PARAMETER    (CW_WIN = '100'O)
					! Window flag Bit 6
	INTEGER	     	CW_INV
	  PARAMETER    (CW_INV = '200'O)
					! Invert flag Bit 7
	INTEGER		CW_XLO
	  PARAMETER    (CW_XLO = '7400'O)
					! DSP X lo Bits 8-11
	INTEGER		CW_XHI
	  PARAMETER    (CW_XHI = '170000'O)
					! DSP X hi Bits 12-15
	INTEGER		CW_YLO
	  PARAMETER    (CW_YLO = '3600000'O)
					! DSP Y lo Bits 16-19
	INTEGER		CW_YHI
	  PARAMETER    (CW_YHI = '74000000'O)
					! DSP Y hi Bits 20-23
	INTEGER		CW_FHT
	  PARAMETER    (CW_FHT = '37700000000'O)
					! FPA HT setting Bits 24-31
