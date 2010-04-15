*+SORT_S_D_DOIT - converts mapped data into arrays, and continues sort.
      SUBROUTINE SORT_S_D_DOIT (SMF, SRT, NXP, NYP, DAT, STATUS)
      IMPLICIT NONE

*   Include statements:
      INCLUDE 'SMAPDEF.INC'		! Small map linked list info
      INCLUDE 'SLIST.INC'
      INCLUDE 'SORT_DEF.INC'

* Input
      RECORD /SORT_DEF/	      	SRT
      INTEGER			SMF	! Unit for small map file
      INTEGER			NXP, NYP

* Output
      REAL              DAT(NXP, NYP)
      INTEGER           STATUS          ! Status flag

* M. Denby
* P McGale May 95 - UNIX mods
*-

*   Local
      CHARACTER*10      TEXT            ! Work text
      CHARACTER*11	TMODE

      INTEGER		NP
      INTEGER           IEV             ! Event loop counter
      INTEGER		NS, NE
      INTEGER           EVT             ! Event time (SCLK ticks)
      INTEGER           I, ITIM
      INTEGER           NTOTAL          ! Total events in image
      INTEGER           NX, NY          ! Pixel pointers
      INTEGER           SMREC           ! Record number in small map
      INTEGER		NXREC
      INTEGER		FEVS,FIL
      INTEGER		ASPF
      INTEGER		LINX,LINY
      INTEGER		ILON,ILAT
      INTEGER		I_PID,MSG_LEN
      INTEGER		ISEED
      integer		hdutyp
      integer		itmp

      LOGICAL		MOONF
      logical		anynull

      double precision 	XLIN, YLIN      ! Map local coords (rads)
      double precision	evmjd

      REAL		DXPIX, DYPIX
      REAL		SIZ
      PARAMETER		(SIZ = 3.*DTOR)

      RECORD 		/EBLOCK/ EBUF

*   External Functions
      double precision 	ev2mjd
      real              ran
      LOGICAL 		BGD_GOOD

      DATA		ISEED/1768953/

      IF (STATUS .NE. 0) RETURN

*   Initialize array
      DO NY = 1, NYP
         DO NX = 1, NXP
            DAT(NX, NY) = 0.0
         END DO
      END DO
      NTOTAL = 0

* Set pixel size
      DXPIX = ABS(SRT.XPIXEL)
      DYPIX = ABS(SRT.YPIXEL)

* Get correct FITS extension.
      call ftmahd(smf, 2, hdutyp, status)
      if (hdutyp .ne. 2) then
	write(*,*)'   Error in SORT_S_D_DOIT'
	write(*,*)'   Trying to read BINTABLE extn.'
      endif

*   Loop over events in this file
      iev = 1
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
	  evmjd = ev2mjd(evt)
	  if (evmjd.ge.srt.smjd .and. evmjd.le.srt.emjd) then

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
                  XLIN=dble((LINX+RAN(ISEED)-0.5))/R2LINP
                  YLIN=dble((LINY+RAN(ISEED)-0.5))/R2LINP

		  IF (ABS(XLIN) .LT. SIZ .AND.
     :					ABS(YLIN) .LT. SIZ) THEN
		    NX = 1 + INT((SIZ-XLIN)/DXPIX)
	 	    NY = 1 + INT((SIZ+YLIN)/DYPIX)
		    DAT(NX,NY) = DAT(NX,NY) + 1.
		    NTOTAL = NTOTAL + 1
	  	  ENDIF
	        ENDIF			        ! Moon reject
	      ENDIF   				! Bgnd reject
	    ENDIF				! Aspect flag
	  endif					! Time
	ENDIF					! Filter
      ENDDO

      WRITE(*,*) '   Events in image : ',NTOTAL

999   IF (STATUS .NE. 0) THEN
	WRITE(*,*) '   Error in SORT_S_D_DOIT'
      ENDIF

      END
