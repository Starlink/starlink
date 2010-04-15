*+SORT_S_TDEF Define time windows for sort (survey)
      SUBROUTINE SORT_S_TDEF (ISM, SRT, STATUS)
        IMPLICIT NONE

* Include files:
        INCLUDE   'SMAPDEF.INC'
	INCLUDE   'SLIST.INC'
        INCLUDE   'SORT_DEF.INC'

* Input
	INTEGER		ISM
	RECORD /SORT_DEF/    SRT

* Output
        INTEGER		STATUS		! HDS status flag
* M. Denby
* P McGale Apr 95
* P McGale May 95 - write out start and end times for light curve ref.
*-

* Local variables :
	CHARACTER*10	TEXT
	CHARACTER*13	CTIM

	INTEGER		N, NM
	INTEGER		NTMP		! Temp # combined window pairs
	INTEGER		MLO, MLA
	INTEGER		NS, NC, NR, NXR
	INTEGER		NACT		! # non-empty maps
	INTEGER		ILEN
	INTEGER		CAL_FILT_S2N

	REAL		TOT
	REAL		TBIN
	REAL		DEFRAD		! Temp radius
	  PARAMETER    (DEFRAD = 2.378898)

	REAL*8		TMIN, TMAX

	RECORD /EBLOCK/ EBUF

* Check status
	IF (STATUS .NE. 0) RETURN

* check for correct filter
	CALL PAR_GETUC ('FILTER eg S1A', TEXT, STATUS)
	CALL CHR_UCASE (TEXT)
	SRT.FILT = CAL_FILT_S2N (TEXT)

* Get the time limits of the active maps + check if events in field
*	TMAX = 0.D0
*	TMIN = 1.D32
*	NACT = 0
*	DO NM = 1, SRT.NMAPS
*	  MLO = SRT.MLO(NM)
*	  MLA = SRT.MLA(NM)
*	  IF (ILIST(MLO,MLA).NBLOC.NE.0) THEN
*	    NACT = NACT + 1
*	    READ(ISM'ILIST(MLO,MLA).HEAD) EBUF
*	    TMIN = MIN(DBLE(EBUF.STIME),TMIN)
*	    READ(ISM'ILIST(MLO,MLA).TAIL) EBUF
*	    TMAX = MAX(DBLE(EBUF.ETIME),TMAX)
*	  ENDIF
*	ENDDO

*	IF (NACT .EQ. 0) THEN
*	  WRITE(*,*) '   Error in SORT_S_TDEF - selected field is empty'
*	  STATUS = 1
*	  RETURN
*	ENDIF

*	TMIN = TMIN/32.D0
*	TMAX = TMAX/32.D0

* Now get the time definition of the sort
	WRITE(*,*) '   BASE_MJD is ',IHEAD.BASE_MJD

*	SRT.SMJD = (TMIN/86400.D0)+S2_REF_MJD
*	SRT.EMJD = (TMAX/86400.D0)+S2_REF_MJD
	SRT.SMJD = ihead.base_mjd
	SRT.EMJD = ihead.end_mjd
	CALL PAR_PUT0D('TLO',SRT.SMJD,STATUS)
	CALL PAR_GET0D('TLO Start MJD',SRT.SMJD,STATUS)
	CALL PAR_PUT0D('THI',SRT.EMJD,STATUS)
	CALL PAR_GET0D('THI End MJD',SRT.EMJD,STATUS)

* Write out strt & end times for use in lcurve script.
	open(10, file='~/strt_end.lcurve', status='unknown')
	WRITE(10,*) SRT.SMJD, SRT.EMJD
 	close (10)

	IF (SRT.DTYPE(1:1) .EQ. 'T') THEN
 	  CALL PAR_PUT0I('NBINS', 0, STATUS)
	  CALL PAR_GET0I ('NBINS (or 0 to set TBIN)', SRT.NBINS, STATUS)
	  IF (SRT.NBINS .LE. 0) THEN
 	    CALL PAR_PUT0R('TBIN', 5760.0, STATUS)
	    CALL PAR_GET0R ('TBIN (secs)', TBIN, STATUS)
	    SRT.NBINS = INT((SRT.EMJD - SRT.SMJD)*86400.D0/TBIN)
	    SRT.EMJD = SRT.SMJD + SRT.NBINS*TBIN/86400.D0
	  ENDIF
	ENDIF

* Set default iris to filter radius then prompt
	IF (SRT.DTYPE(1:1) .EQ. 'L') THEN
	  SRT.IRIS = DEFRAD
	ELSE
	  CALL PAR_DEF0R('IRAD', DEFRAD, STATUS)
	  CALL PAR_GET0R('IRAD Iris Radius (degs)', SRT.IRIS, STATUS)
	ENDIF
	SRT.IRIS = MIN(SRT.IRIS, DEFRAD)*DTOR
*	WRITE(*,*) '   Iris set to ',SRT.IRIS/DTOR

999	IF (STATUS .NE. 0) THEN
	  WRITE(*,*) '   Error in SORT_S_TDEF'
	ENDIF

	END
