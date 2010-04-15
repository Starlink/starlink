*+SORT_S_SETUP  Prepare for sort (survey)
      SUBROUTINE SORT_S_SETUP (ISM, SRT, STATUS)
      IMPLICIT 		NONE

* Include files:
      INCLUDE   	'SMAPDEF.INC'
      INCLUDE   	'SLIST.INC'
      INCLUDE   	'SORT_DEF.INC'

* Input
      RECORD /SORT_DEF/ SRT
      INTEGER		ISM

* Output:
      INTEGER		STATUS		! HDS status flag

* M Denby
* P McGale Apr 95
*-

*    Local variables :
        CHARACTER*80	ALIGN		! Cel or Ecl image alignment
	CHARACTER*80	MODE
	LOGICAL		USE
	LOGICAL		KEEP
	LOGICAL		POLE
	LOGICAL 	ANTI
	REAL		SCANAZ, SCANEL
	REAL		RADTMP		! Temp iris radius
	REAL		ELON, ELAT
	REAL		DELT
	REAL		RAOFF, DECOFF
	REAL*8		DELON, DELAT
	REAL*8		DELO, DELA, DRA, DDEC
	REAL*8		NEPRA, NEPDEC	! North Ecl pole
	REAL*8		TEMP(2)
	REAL*8		CTOS(3,3)
	REAL*8		STOC(3,3)
	REAL*8		RAS, DECS
	REAL*8		RAE, DECE
	REAL*8		PAZ, PEL
	INTEGER		I

	PARAMETER  	(DELT = 0.1)

* Check status
	IF (STATUS .NE. 0) RETURN

	INQUIRE (UNIT=ISM, NAME=SRT.EVE)

	SRT.MODE = IHEAD.MODE
	SRT.TARG = ihead.target
	SRT.OBSR = IHEAD.OBSERVER
	SRT.OBSY = IHEAD.OBSERVATORY
	SRT.INST = IHEAD.INSTRUMENT
	SRT.RMJD = IHEAD.REF_MJD
	SRT.MDR	 = IHEAD.MDR_SEQ
	SRT.ARA	 = IHEAD.NOM_RA*DTOR
	SRT.ADEC = IHEAD.NOM_DEC*DTOR
	SRT.DET	 = IHEAD.DETECTOR

* Get data set type and mode (COORDINATE or MAP)
	CALL PAR_GETUC ('DTYPE I,T,E or L', SRT.DTYPE, STATUS)
	IF (SRT.DTYPE(1:1) .EQ. 'T') THEN
	  MODE(1:1) = 'C'
	  ALIGN(1:1) = 'C'
	ELSE
* For moment don't give choice of mode and align.
          MODE(1:1) = 'C'
	  ALIGN(1:1) = 'C'
*	  CALL PAR_GETUC ('MODE (C)oordinate or (M)ap', MODE, STATUS)
*	  IF (MODE(1:1) .EQ. 'C') THEN
*	    CALL PAR_GETUC ('ALIGN to (C)el, (E)cl or (S)can pole',
*    :							ALIGN, STATUS)
*	  ENDIF
	ENDIF

* Spatial region specified as pointing + extents
	IF (MODE(1:1) .EQ. 'C') THEN

* Field is aligned with Celestial North
	  IF (ALIGN(1:1) .EQ. 'C') THEN
	    CALL PAR_GET0R ('RA of Field centre (deg/hms)',
     &                                      SRT.FRA,  STATUS)
	    CALL PAR_GET0R ('DEC of Field centre (deg/hms)',
     &                                       SRT.FDEC, STATUS)

	    SRT.ROLL = 0.0
	    SRT.FRA = SRT.FRA*DTOR
	    SRT.FDEC  = SRT.FDEC*DTOR

* Field is aligned with Ecliptic pole
	  ELSEIF (ALIGN(1:1) .EQ. 'E') THEN
	    CALL CEL2EC (DBLE(IHEAD.NOM_RA*DTOR),
     :		DBLE(IHEAD.NOM_DEC*DTOR), DELON, DELAT)
	    ELON = REAL(DELON)/DTOR
	    ELAT = REAL(DELAT)/DTOR
	    CALL PAR_GET0R ('ELON of Field centre', ELON, STATUS)
	    CALL PAR_GET0R ('ELAT of Field centre', ELAT, STATUS)
	    CALL EC2CEL (DBLE(ELON*DTOR), DBLE(ELAT*DTOR), RAE, DECE)

* Get the roll from Ecl N to Cel N
	    CALL EC2CEL (0.D0, DBLE(PIBY2), NEPRA, NEPDEC)
	    CALL BEARING (REAL(RAE), REAL(DECE), REAL(NEPRA),
     :					    REAL(NEPDEC), SRT.ROLL)
	    SRT.FRA  = REAL(RAE)
	    SRT.FDEC = REAL(DECE)

* Field is aligned with Scan Pole (orbit vector pole)
	  ELSEIF (ALIGN(1:1) .EQ. 'S') THEN

* Get angle around scan (0-360) from user, check if at pole and side of scan
	    CALL PAR_GET0R ('AZ Angle around scan (degs)',  SCANAZ,
     :								STATUS)
	    SCANAZ = MOD(SCANAZ, 360.)
	    POLE = (SCANAZ .EQ. 90. .OR. SCANAZ .EQ. 270.)
	    IF (POLE) THEN
* Nudge the image off the pole
	      SCANAZ = SCANAZ - DELT
	    ENDIF
	    ANTI = (SCANAZ .GT. 90. .AND. SCANAZ .LT. 270.)
	    SCANAZ = SCANAZ*DTOR

* Find Elon Elat of requested point
	    CALL CEL2EC(DBLE(SRT.ARA), DBLE(SRT.ADEC), DELON, DELAT)
	    DELAT = ASIN(SIN(SCANAZ))
	    IF (ANTI) THEN
	      DELON = MOD (DELON + DBLE(PI), DBLE(TWOPI))
	    ENDIF

* Find Ra, Dec of requested point and get roll to Cel N
	    CALL EC2CEL (DELON, DELAT, RAS, DECS)
	    CALL EC2CEL (0.D0, DBLE(PIBY2), NEPRA, NEPDEC)
	    CALL BEARING (REAL(RAS), REAL(DECS),
     :				REAL(NEPRA), REAL(NEPDEC), SRT.ROLL)
	    IF (ANTI) THEN
	      SRT.ROLL = SRT.ROLL - PIBY2
	    ELSE
	      SRT.ROLL = SRT.ROLL + PIBY2
	    ENDIF

	    SRT.FRA  = REAL(RAS)
	    SRT.FDEC = REAL(DECS)
	  ENDIF

* Map mode - used to perform S3
	ELSEIF (MODE(1:1) .EQ. 'M')THEN
	  CALL PAR_GET0I ('MLO Long Index 1-192', SRT.MLO(1), STATUS)

* If the user gives a -ve map #, get one for him
	  IF (SRT.MLO(1) .LT. 0) THEN
	    CALL SORT_S_NEX(SRT.MLO(1), SRT.MLA(1), STATUS)
	    CALL PAR_PUT0I ('MLO', SRT.MLO(1), STATUS)
	    CALL PAR_PUT0I ('MLA', SRT.MLA(1), STATUS)

	    IF (SRT.MLO(1) .GT. 0) THEN
	      WRITE(*,*) '   Selecting map ',SRT.MLO(1), SRT.MLA(1)
	    ELSE
	      WRITE(*,*) '   File has no active maps'
	    ENDIF

	  ELSE
	    CALL PAR_GET0I ('MLA Lat index 1-90', SRT.MLA(1), STATUS)
	  ENDIF

	  CALL MAPTOC2(SRT.MLO(1), SRT.MLA(1), SRT.FRA,
     :					SRT.FDEC, SRT.ROLL)
	  CALL CEL2EC (DBLE(SRT.FRA), DBLE(SRT.FDEC),
     :						       DELON, DELAT)
	ENDIF

	SRT.ARA  = SRT.FRA
	SRT.ADEC = SRT.FDEC

* Sort to Image data set
	IF (SRT.DTYPE(1:1) .EQ. 'I') THEN

	  CALL PAR_GET0R ('DAZ Field 1/2 width (degs)', SRT.DAZ, STATUS)
	  CALL PAR_GET0R ('DEL Field 1/2 Height (degs)', SRT.DEL,STATUS)
	  SRT.DAZ = SRT.DAZ*DTOR
	  SRT.DEL = SRT.DEL*DTOR

	  CALL PAR_GET0I ('NXPIX Azimuth Pixels',   SRT.NXPIX, STATUS)
	  CALL PAR_GET0I ('NYPIX Elevation Pixels', SRT.NYPIX, STATUS)

	  SRT.XPIXEL = -2.0*(SRT.DAZ)/REAL(SRT.NXPIX)
	  SRT.YPIXEL =  2.0*(SRT.DEL)/REAL(SRT.NYPIX)

	  SRT.XCMIN = (SRT.XPIXEL/2.) + SRT.DAZ
	  SRT.YCMIN = (SRT.YPIXEL/2.) - SRT.DEL

* Sort to Event data set
	ELSEIF (SRT.DTYPE(1:1) .EQ. 'E') THEN

	  CALL PAR_GET0R ('DAZ Field 1/2 Width (degs)', SRT.DAZ, STATUS)
	  CALL PAR_GET0R ('DEL Field 1/2 Height (degs)', SRT.DEL,STATUS)
	  SRT.DAZ = SRT.DAZ*DTOR
	  SRT.DEL = SRT.DEL*DTOR

* Sort to raw or linearised image data set
	ELSEIF (SRT.DTYPE(1:1).EQ.'L') THEN

	  SRT.NXPIX = 512
	  SRT.NYPIX = 512

	  CALL PAR_GET0R ('DAZ Field 1/2 Width (degs)', SRT.DAZ, STATUS)
	  CALL PAR_GET0R ('DEL Field 1/2 Height (degs)', SRT.DEL,STATUS)
	  SRT.DAZ = SRT.DAZ*DTOR
	  SRT.DEL = SRT.DEL*DTOR

	  SRT.XPIXEL = -2.0*(3.*DTOR)/REAL(SRT.NXPIX)
	  SRT.YPIXEL =  2.0*(3.*DTOR)/REAL(SRT.NYPIX)

	  SRT.XCMIN = (SRT.XPIXEL/2.) + (3.*DTOR)
	  SRT.YCMIN = (SRT.YPIXEL/2.) - (3.*DTOR)

* Sort to a time series data set
	ELSEIF (SRT.DTYPE(1:1) .EQ. 'T') THEN

	  CALL PAR_GET0R ('INRAD  Inner radius (amins)', SRT.INR, STATUS)
	  CALL PAR_GET0R ('OUTRAD Outer radius (amins)', SRT.OUTR, STATUS)
	  SRT.INR  = (SRT.INR/60.)*DTOR
	  SRT.OUTR = (SRT.OUTR/60.)*DTOR
	  SRT.DAZ  = SRT.OUTR
	  SRT.DEL  = SRT.DAZ
	  CALL CEL2EC (DBLE(SRT.FRA),DBLE(SRT.FDEC),DELO,DELA)
	  DELA = DELA + 2.*SRT.OUTR
	  IF (DELA .GE. PIBY2) THEN
	    DELA = PI - DELA
	    DELO = MOD((DELO+PI),DBLE(TWOPI))
	  ENDIF
	  CALL EC2CEL(DELO,DELA,DRA,DDEC)
	  RAOFF = REAL(DRA/DTOR)
	  DECOFF = REAL(DDEC/DTOR)
	  CALL PAR_PUT0R ('RAOFF',RAOFF,STATUS)
	  CALL PAR_PUT0R ('DECOFF',DECOFF,STATUS)
	ENDIF

* Locate the maps overlapping the field
*	CALL ACTMAPS (SRT.FRA, SRT.FDEC, SRT.DAZ,
*     :		  SRT.DEL, SRT.ROLL, SRT.MLO, SRT.MLA, SRT.NMAPS)
*	IF (SRT.NMAPS .GT. 3000) THEN
*	  WRITE(*,*) '   Error in SORT_S_SETUP - Map buffer overflow'
*	  STATUS = 1
*	  RETURN
*	ENDIF


* Now get the time definition of the sort.  Aid user.
	IF (SRT.DTYPE(1:1) .EQ. 'T') THEN
	  CALL PAR_PUT0L ('BREJ', .FALSE., STATUS)
	  CALL PAR_PUT0L ('MREJ', .TRUE., STATUS)
	else
	  CALL PAR_PUT0L ('BREJ', .TRUE., STATUS)
	  CALL PAR_PUT0L ('MREJ', .TRUE., STATUS)
	endif
	SRT.IGBGD  = .TRUE.
	CALL PAR_GET0L('BREJ Background rejection',SRT.BREJ,STATUS)
	IF (SRT.BREJ) SRT.IGBGD  = .FALSE.
	SRT.IGMOON = .TRUE.
	CALL PAR_GET0L('MREJ Moon in field rejection',SRT.MREJ,STATUS)
	IF (SRT.MREJ) SRT.IGMOON = .FALSE.
	CALL SORT_S_TDEF(ISM,SRT,STATUS)
	IF (STATUS.NE.0) GOTO 999

999	IF (STATUS .NE. 0) THEN
	  WRITE(*,*) '   Error in SORT_SETUP_S'
	ENDIF

	END
