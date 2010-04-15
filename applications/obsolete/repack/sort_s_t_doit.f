*+SORT_S_T_DOIT - converts mapped data into array and continues sort.
      SUBROUTINE SORT_S_T_DOIT (SMF,SRT,NBI,DAT,STATUS)
      IMPLICIT NONE

*   Include statements:
      INCLUDE 'SMAPDEF.INC'		! Small map linked list info
      INCLUDE 'SLIST.INC'
      INCLUDE 'SORT_DEF.INC'

* Input
      RECORD /SORT_DEF/	      SRT
      INTEGER		NBI
      INTEGER		SMF		! Unit for small map file

*   Export
      REAL              DAT(NBI)
      INTEGER           STATUS          ! Status flag

* P McGale Apr 95
*-
*   local constants

*   Local
      INTEGER		NP
      INTEGER           IEV           	! Event loop counter
      INTEGER		NS, NE
      INTEGER           EVT             ! Event time (SCLK ticks)
      INTEGER           I, ITIM
      INTEGER           NTOTAL          ! Total events in image
      INTEGER           NXREC          	! Next record to read in smap
      INTEGER           SMREC           ! Record number in small map
      INTEGER		LINX, LINY
      INTEGER		ILON,ILAT
      INTEGER		FEVS,FIL
      INTEGER		ASPF
      INTEGER		NBIN
      INTEGER		I_PID, MSG_LEN
      integer           hdutyp
      integer		itmp

      LOGICAL		MOONF
      logical		anynull
      CHARACTER*11	TMODE

      REAL		IRIS2		! Iris radius **2
      REAL		IN2, OUT2, RAD2 ! radii squared (rads**2)

      DOUBLE PRECISION  ETOL(3,3)       ! Dcm map local to image local
      DOUBLE PRECISION	CTOL(3,3),LTOC(3,3)
      DOUBLE PRECISION	CEL(2)
      DOUBLE PRECISION  RX, RY          ! Event coords in image (rads)
      DOUBLE PRECISION	VX, VY
      DOUBLE PRECISION	XLCL, YLCL
      DOUBLE PRECISION  XLIN, YLIN      ! Map local coords (rads)
      DOUBLE PRECISION	BINSIZ, ETIM
      double precision	evmjd

      CHARACTER*10      TEXT            ! Work text

      RECORD /EBLOCK/ EBUF

*   External functions
      DOUBLE PRECISION	ELOMAP, ELAMAP, ev2mjd
      REAL 		AX_GTCIRC
      LOGICAL		BGD_GOOD

      IF (STATUS .NE. 0) RETURN

*   Initialize the DATA_ARRAY
      DO NP = 1, NBI
        DAT(NP) = 0.0
      END DO
      NTOTAL = 0

* Find bin duration, set iris**2
      BINSIZ = (SRT.EMJD - SRT.SMJD)*86400.D0/DBLE(NBI)
      IRIS2  = (SRT.IRIS)**2

* Get radii squared for annulus check
      IN2  = SRT.INR**2
      OUT2 = SRT.OUTR**2

      CEL(1) = SRT.FRA
      CEL(2) = SRT.FDEC
      CALL AX_DMAT(CEL,DBLE(SRT.ROLL),CTOL,LTOC)
      CALL AX_DONMXM(ETOC,CTOL,ETOL)

* Make sure reading correct extension in FITS file.
      call ftmahd(smf, 2, hdutyp, status)
      if (hdutyp .ne. 2) then
	write(*,*)
	write(*,*)'   Error in SORT_S_T_DOIT.'
	write(*,*)'   Trying to read BINTABLE extn.'
	write(*,*)
      endif

*   Loop over events in event file.
      iev = 1
      call fttscl(smf, 4, 1.d0, 0.d0, status)		! Reset scaling for
      call fttscl(smf, 5, 1.d0, 0.d0, status)           ! LON & LAT.
      DO I = 1, ihead.nevent

* Get aspect moon and filter flags for row
	call ftgcvj(smf, 2, i, 1, 1, 0, itmp, anynull, status)
	aspf = ibits(itmp, 0, 3)
	moonf = btest(itmp, 3)
	ebuf.filt = ibits(itmp, 4, 1)
	if (ebuf.filt .eq. 0) ebuf.filt = 8		! S1a filt
	if (ebuf.filt .eq. 1) ebuf.filt = 6 		! S2a filt

*  Check for overlap of the record with valid time windows
	IF (EBUF.FILT.EQ.SRT.FILT) THEN
	  call ftgcvj(smf, 1, i, 1, 1, 0, ebuf.ev(iev).time,
     &                                              anynull, status)
          evt = ebuf.ev(iev).time
	  evmjd = ev2mjd(evt)
	  if (evmjd.ge.srt.smjd .and. evmjd.le.srt.emjd) then

	    IF (ASPF .EQ. 0) THEN
	      IF (SRT.IGBGD.OR.BGD_GOOD(SRT.FILT,EVT)) THEN
	        IF (SRT.IGMOON.OR..NOT.MOONF) THEN

* Get linearized detector coords of this event
	          call ftgcvb(smf, 6, i, 1, 1, 0,
     &              ebuf.ev(iev).linx, anynull, status)
	          call ftgcvb(smf, 7, i, 1, 1, 0,
     &              ebuf.ev(iev).liny, anynull, status)
	          linx = ebuf.ev(iev).linx
	          liny = ebuf.ev(iev).liny
		  if (linx .gt. 128) linx = linx - 256
		  if (liny .gt. 128) liny = liny - 256
                  XLIN = DBLE(LINX)/R2LINP
                  YLIN = DBLE(LINY)/R2LINP

*  Check if event is in required iris (Note coords are really RA & Dec)
	          call ftgcvi(smf, 4, i, 1, 1, 0,
     &              ebuf.ev(iev).elon, anynull, status)
	          call ftgcvi(smf, 5, i, 1, 1, 0,
     &              ebuf.ev(iev).elat, anynull, status)
		  ilon = ebuf.ev(iev).elon
		  ilat = ebuf.ev(iev).elat
                  IF ((XLIN*XLIN + YLIN*YLIN).LT.IRIS2) THEN
                    XLCL = DBLE(ILON)/R2LONP
                    YLCL = DBLE(ILAT)/R2LATP
* 		    CALL AX_DTRANS(XLCL,YLCL,ETOL,.TRUE.,RX,RY)
 		    CALL AX_DTRANS(XLCL,YLCL,CTOL,.TRUE.,RX,RY)
		    IF (RX.GT.PI) RX = RX-TWOPI
		    RAD2 = RX*RX + RY*RY

* Ignore variance - it gets calculated by the exposure correction prog.
		    IF (RAD2.LE.OUT2 .AND. RAD2.GE.IN2) THEN
		      ETIM = DBLE(EVT)/32.D0
		      NBIN = INT ((ETIM - (SRT.SMJD -
     :			            SRT.RMJD)*86400.D0)/BINSIZ) + 1
		      IF (NBIN.GE.1 .AND. NBIN.LE.NBI) THEN
                        DAT(NBIN) = DAT(NBIN) + 1.0
                        NTOTAL = NTOTAL + 1
		      ENDIF
                    ENDIF

                  ENDIF
	        ENDIF 				! Moon rej
	      ENDIF				! Bgnd rej
	    ENDIF				! Aspect flag
	  endif					! Time
        ENDIF					! Filter flag
      ENDDO

      WRITE(*,*) '   Events in time-series : ',NTOTAL

999   IF (STATUS .NE. 0) THEN
	WRITE(*,*) '   Error in SORT_S_T_DOIT'
      ENDIF

      END
