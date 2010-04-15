*+SORT_S_I_DOIT - converts mapped data into arrays, and continues sort.
      SUBROUTINE SORT_S_I_DOIT (SMF,SRT,NXP,NYP,DAT,STATUS)
      IMPLICIT NONE

*   Include statements:
      INCLUDE 'SMAPDEF.INC'
      INCLUDE 'SLIST.INC'
      INCLUDE 'SORT_DEF.INC'

* Input
      RECORD /SORT_DEF/	      	SRT
      INTEGER			NXP, NYP
      INTEGER			SMF	! Unit for small map file

* Output
      REAL              DAT(NXP,NYP)	! Image array
      INTEGER           STATUS          ! Status flag

* M. Denby
* P. McGale - Sept. 93 - Use user defined start and end times.
* P McGale Apr 95 - UNIX mods
*-

*   Local
      CHARACTER*10      TEXT            ! Work text
      CHARACTER*11	TMODE

      INTEGER		NP
      INTEGER           IEV           ! Event loop counter
      INTEGER		NS, NE
      INTEGER           EVT             ! Event time (SCLK ticks)
      INTEGER           I, ITIM
      INTEGER		ISEED
      INTEGER           NTOTAL          ! Total events in image
      INTEGER           NX, NY          ! Pixel pointers
      INTEGER           NXREC          ! Next record to read in smap
      INTEGER           SMREC           ! Record number in small map
      INTEGER		METHOD
      INTEGER		LINX,LINY
      INTEGER		ILON,ILAT
      INTEGER		I_PID,MSG_LEN
      INTEGER		FEVS,FIL
      INTEGER		ASPF
      integer		hdutyp
      integer		itmp

      LOGICAL		MOONF
      logical           anynull

      REAL		IRIS2		! Iris radius **2
      REAL		DXPIX, DYPIX

      double precision        	ETOL(3,3)       ! Dcm map local to image local
      double precision		CTOL(3,3),LTOC(3,3)
      double precision		CEL(2)
      double precision        	RX, RY          ! Event coords in image (rads)
      double precision	      	VX, VY
      double precision	      	XLCL, YLCL
      double precision        	XLIN, YLIN      ! Map local coords (rads)
      double precision		EVMJD

      RECORD /EBLOCK/ EBUF

*   External functions
      double precision	      	ELOMAP,ELAMAP,EV2MJD
      REAL 		AX_GTCIRC
      LOGICAL		BGD_GOOD
      REAL              RAN

      DATA  		ISEED/12789531/

      IF (STATUS .NE. 0) RETURN

*   Initialize array
      DO NY = 1, NYP
         DO NX = 1, NXP
            DAT(NX,NY) = 0.0
         END DO
      END DO
      NTOTAL = 0
      IRIS2  = (SRT.IRIS)**2

* set pixel size
      DXPIX = ABS(SRT.XPIXEL)
      DYPIX = ABS(SRT.YPIXEL)

      CEL(1)=SRT.FRA
      CEL(2)=SRT.FDEC

      CALL AX_DMAT(CEL,DBLE(SRT.ROLL),CTOL,LTOC)
      CALL AX_DONMXM(ETOC,CTOL,ETOL)

      call ftmahd(smf, 2, hdutyp, status)
      if (hdutyp .ne. 2) then
         write(*,*)
	 WRITE(*,*) '   Error in SORT_S_I_DOIT'
         write(*,*) '   Trying to read BINTABLE extn.'
         write(*,*)
      endif

* Loop over all events in event file.
      IEV = 1
      call fttscl(smf, 4, 1.d0, 0.d0, status)         ! Reset scaling for
      call fttscl(smf, 5, 1.d0, 0.d0, status)         ! LON & LAT
      DO I = 1, ihead.nevent

* Get aspect, moon and filter flags for row.
	call ftgcvj(smf, 2, i, 1, 1, 0, itmp, anynull, status)
        aspf  = ibits(itmp, 0, 3)
	moonf = btest(itmp, 3)
        ebuf.filt = ibits(itmp, 4, 1)
        if (ebuf.filt .eq. 0) ebuf.filt = 8
        if (ebuf.filt .eq. 1) ebuf.filt = 6

* Check for overlap of the record with valid time windows
        IF (EBUF.FILT.EQ.SRT.FILT) THEN

	  call ftgcvj(smf, 1, i, 1, 1, 0, EBUF.EV(IEV).TIME,
     &                                         anynull, status)
 	  EVT = EBUF.EV(IEV).TIME

* See if event is in user time window.
	  EVMJD = EV2MJD(EVT)
	  IF(EVMJD.GE.SRT.SMJD .AND. EVMJD.LE.SRT.EMJD) THEN

	     IF (ASPF .EQ. 0) THEN
	       IF (SRT.IGBGD.OR.BGD_GOOD(SRT.FILT,EVT)) THEN
		 IF (SRT.IGMOON.OR..NOT.MOONF) THEN

*  Get linearized detector coords of this event
                    call ftgcvb(smf, 6, i, 1, 1, 0,
     &                    EBUF.EV(IEV).LINX, anynull, status)
                    call ftgcvb(smf, 7, i, 1, 1, 0,
     &                    EBUF.EV(IEV).LINY, anynull, status)
                    linx = ebuf.ev(iev).linx
                    liny = ebuf.ev(iev).liny
	            if (linx .gt. 128) linx  = linx - 256
	            if (liny .gt. 128) linx  = liny - 256
                    XLIN = DBLE(LINX)/R2LINP
                    YLIN = DBLE(LINY)/R2LINP

*  Check if event is in required iris (Note coords are really RA & Dec)
                    call ftgcvi(smf, 4, i, 1, 1, 0,
     &                 EBUF.EV(IEV).ELON, anynull, status)
                    call ftgcvi(smf, 5, i, 1, 1, 0,
     &                 EBUF.EV(IEV).ELAT, anynull, status)

	            ilon = EBUF.EV(IEV).ELON
                    ilat = EBUF.EV(IEV).ELAT
                    IF ((XLIN*XLIN+YLIN*YLIN) .LT. IRIS2) THEN
                      XLCL = (ILON + RAN(ISEED) - 0.5)/R2LONP
                      YLCL = (ILAT + RAN(ISEED) - 0.5)/R2LATP
*                      CALL AX_DTRANS(XLCL,YLCL,ETOL,.TRUE.,RX,RY)
                      CALL AX_DTRANS(XLCL,YLCL,CTOL,.TRUE.,RX,RY)
	  	      IF (RX.GT.PI) RX = RX - TWOPI
	 	    ENDIF

		    IF (ABS(RX) .LT. SRT.DAZ .AND.
     :		                      ABS(RY) .LT. SRT.DEL) THEN
		       NX = 1 + INT((SRT.DAZ-RX)/DXPIX)
		       NY = 1 + INT((RY+SRT.DEL)/DYPIX)
                       DAT(NX, NY) = DAT(NX, NY) + 1.0
                       NTOTAL = NTOTAL + 1
                    ENDIF			! RX & RY
		  ENDIF			        ! Moon reject
		ENDIF   			! Bgnd reject
	      ENDIF				! Aspect flag
	    ENDIF  				! Event time
	  ENDIF
      ENDDO

      WRITE(*,*) '   Events in image : ',NTOTAL

999   IF (STATUS .NE. 0) THEN
	WRITE(*,*) '   Error in SORT_S_I_DOIT'
      ENDIF

      END
