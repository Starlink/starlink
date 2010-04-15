*+HKR_DTCP Dead time correction for a spot time
	REAL FUNCTION HKR_DTCP (HKR, IFMT, IZ, SMJD, STATUS)
	IMPLICIT NONE
* Input
	INCLUDE		'HKR_BUFFER.INC'
	RECORD		/HKR_BUFFER/ HKR
	INTEGER		IFMT			! Format normal or high speed
	INTEGER		IZ			! Zoom bit
	DOUBLE PRECISION SMJD			! Spot MJD
* Output
	INTEGER		STATUS			! Status return
* M. Denby Sep-89
* Modified to use HK rates directly 1990-Oct-3 Dick Willingale
* P McGale May 95 UNIX mods
*-
* local
	INCLUDE		'SMAPDEF.INC'
	CHARACTER*40	TEXT
	INTEGER		SUT2			! SMJD in UT 1/2 secs
	INTEGER		NS, NT			! Loop counter
	INTEGER		NWORD,NBIT		! Pointers
	INTEGER		ISLT			! Slot #
	INTEGER		MJD2HK
	REAL		FEVS(64)		! Can't SAVE HKR !
	REAL		LEVS(64)
	REAL		CAL_FRADEAD
	LOGICAL		IFLAG(64)
	DOUBLE PRECISION BUFST, BUFET		! Cur buff start/end MJD
	DOUBLE PRECISION TSLOT
	DOUBLE PRECISION HK2MJD

	PARAMETER	(TSLOT = 8.D0 / 86400.D0)
	DATA 		BUFST, BUFET /0.D0, 0.D0/
	SAVE 		FEVS, LEVS, BUFST, BUFET

* Note this routine returns the effective on-time fraction expected
* from all data loses which can been observed in the house keeping.
* No allowance is made for data loses from the on-board event queue through
* the telemetry stream right through to the small map file.

	if (status .ne. 0) then
	  hkr_dtcp = 1.0
	  return
	endif

* Check if current buffer has expired - if so refresh
* Note offset of 1 slot in times because rates refer to previous slot
	IF (SMJD .LT. BUFST .OR. SMJD .GT. BUFET) THEN
	  SUT2 = MJD2HK (SMJD-TSLOT)
	  call GET_HKR(SUT2, HKR, STATUS)
	  if (status .ne. 0) goto 999
	  DO NS = 1, 64
	    LEVS(NS) = HKR.LEVS(NS)
	    FEVS(NS) = HKR.FEVS(NS)
	  ENDDO
	  BUFST = HK2MJD (HKR.START_UT)-TSLOT
	  BUFET = HK2MJD (HKR.END_UT)-TSLOT
	ENDIF

* If the spot time is in a gap then we can't do anything
	IF (SMJD.GT.BUFET .OR. SMJD.LT.BUFST) THEN
	  HKR_DTCP = 1.0
	ELSE

* Convert time to slot number (1-64) in buffer
	  ISLT = INT((SMJD-BUFST)/TSLOT) + 1

* If we have hit a bad slot shuffle around for a good one
	  IF (LEVS(ISLT).LE.0.) THEN
	    IF (ISLT.GT.1) THEN
	      ISLT = ISLT-1
	    ELSE
	      ISLT = ISLT+1
	    ENDIF
	  ENDIF

* If slot still bad return unity
	  IF (LEVS(ISLT).LE.0.) THEN
	    HKR_DTCP = 1.0
	  ELSE
	    HKR_DTCP=1.-CAL_FRADEAD(IFMT,IZ,LEVS(ISLT))
	  ENDIF
	ENDIF

 999	IF (STATUS .NE. 0) THEN
	  WRITE(*,*) '   Error in HKR_DTCP '
	  HKR_DTCP = 1.0
	ENDIF

	END

