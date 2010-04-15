*+SORT_S_E_DOIT - Convert pointers to arrays, and continue the sort.
      SUBROUTINE SORT_S_E_DOIT(SMF, SRT, LLOC, DLOC, DPTR, MLIM, STATUS)
      IMPLICIT NONE

*   Include files:
      INCLUDE 'SMAPDEF.INC'	         ! Small map linked list info
      INCLUDE 'SLIST.INC'
      INCLUDE 'SORT_DEF.INC'
      include 'DAT_PAR'

* Input
      RECORD /SORT_DEF/	SRT
      CHARACTER*(DAT__SZLOC)  	LLOC(5)   	! Locators to list objects
      CHARACTER*(DAT__SZLOC)  	DLOC(5)		! Locators to list DATA_ARRAY
      INTEGER           	DPTR(5)      	! PtR To mapped DATA_ARRAYS
      INTEGER           	SMF             ! Unit number of small map file
      INTEGER           	MLIM          	! mapping extent of lists

* Output
      INTEGER		STATUS		! Status
* M. denby
* P McGale May 95 - UNIX mods
*-

*   Local constants:
      INTEGER           MINC          	! List size mapping increment
         PARAMETER      (MINC = 10000)

*   Local variables:
      INTEGER           I               ! Loop counter
      INTEGER           NBL             ! position in small map list
      INTEGER           NM, NP		! Loop counter
      INTEGER           NTOTEV          ! Total number events in evds
      INTEGER		METHOD
      INTEGER		I_PID, MSG_LEN
      integer		IEV
      integer		EVT
      integer		aspf
      integer		hdutyp
      integer		itmp

      logical		moonf
      logical 		anynull

      CHARACTER*10      TEXT            ! Work text
      CHARACTER*11   	TMODE

      DOUBLE PRECISION  ETOL(3,3)       ! Dcm map locals to image locals
      DOUBLE PRECISION	CTOL(3,3),LTOC(3,3)
      DOUBLE PRECISION	CEL(2)
      double precision  EVMJD

      RECORD /EBLOCK/ EBUF

*   External functions:
      DOUBLE PRECISION	ELOMAP          ! function map to ecl long
      DOUBLE PRECISION	ELAMAP          ! function map to ecl lat
      double precision  EV2MJD
      REAL		AX_GTCIRC
      LOGICAL		TMATCHI, BGD_GOOD

      EXTERNAL ELOMAP, ELAMAP, AX_GTCIRC, TMATCHI

*   Check status - return if bad
      IF (STATUS .NE. 0) RETURN


      CEL(1) = SRT.FRA
      CEL(2) = SRT.FDEC
      CALL AX_DMAT(CEL,DBLE(SRT.ROLL),CTOL,LTOC)
      CALL AX_DONMXM(ETOC,CTOL,ETOL)

* Point to right extension.
      call ftmahd(smf, 2, hdutyp, status)
      if (hdutyp .ne. 2) then
         write(*,*)
	 WRITE(*,*) '   Error in SORT_S_I_DOIT'
         write(*,*) '   Trying to read BINTABLE extn.'
         write(*,*)
      endif


*   Loop through all active small maps ...
      IEV = 1
      call fttscl(smf, 4, 1.d0, 0.d0, status)         ! Reset scaling for
      call fttscl(smf, 5, 1.d0, 0.d0, status)         ! LON & LAT
      NTOTEV = 0
      DO NM = 1, ihead.nevent

* Get aspect, moon and filter flags for row.
	call ftgcvj(smf, 2, nm, 1, 1, 0, itmp, anynull, status)
        aspf  = ibits(itmp, 0, 3)
	moonf = btest(itmp, 3)
        ebuf.filt = ibits(itmp, 4, 1)
        if (ebuf.filt .eq. 0) ebuf.filt = 8
        if (ebuf.filt .eq. 1) ebuf.filt = 6

* Check for overlap of the record with valid time windows
        IF (EBUF.FILT.EQ.SRT.FILT) THEN
	  call ftgcvj(smf, 1, nm, 1, 1, 0, EBUF.EV(IEV).TIME,
     &                                         anynull, status)
 	  EVT = EBUF.EV(IEV).TIME

* See if event is in user time window.
	  EVMJD = EV2MJD(EVT)
	  IF(EVMJD.GE.SRT.SMJD .AND. EVMJD.LE.SRT.EMJD) THEN
 	     IF (ASPF .EQ. 0) THEN
	       IF (SRT.IGBGD.OR.BGD_GOOD(SRT.FILT,EVT)) THEN
		 IF (SRT.IGMOON.OR..NOT.MOONF) THEN

*  Get linearized detector coords of this event
                    call ftgcvb(smf, 6, nm, 1, 1, 0,
     &                    EBUF.EV(IEV).LINX, anynull, status)
                    call ftgcvb(smf, 7, nm, 1, 1, 0,
     &                    EBUF.EV(IEV).LINY, anynull, status)
                    call ftgcvi(smf, 4, nm, 1, 1, 0,
     &                 EBUF.EV(IEV).ELON, anynull, status)
                    call ftgcvi(smf, 5, nm, 1, 1, 0,
     &                 EBUF.EV(IEV).ELAT, anynull, status)
		    CALL SORT_S_E_WRTDAT(EBUF,EVT,CTOL,%VAL(DPTR(1)),
     :		            %VAL(DPTR(2)),%VAL(DPTR(3)),%VAL(DPTR(4)),
     :		            %VAL(DPTR(5)),SRT,MLIM,NTOTEV,STATUS)
	            IF (STATUS .NE. 0) GOTO 999
 		 ENDIF				! Moon
	       ENDIF				! Bgnd
	     ENDIF				! Aspect
	   ENDIF				! Time

*         Check if we need to re-map
           IF ((NTOTEV + 100) .GT. MLIM) THEN
             MLIM = MLIM + MINC

             DO I = 1, 5
                CALL SORT_E_REMAP(DLOC(I), '_REAL', MLIM, DPTR(I),
     :	  					         	.TRUE.)
             END DO
           END IF

	ENDIF 						! Filter
      ENDDO  						! Next event

      WRITE(*,*) '   Events in EVDS : ',NTOTEV

*   Shrink the data objects to size.
      DO I = 1,5
         CALL SORT_E_REMAP(DLOC(I), '_REAL', NTOTEV, DPTR(I), .FALSE.)
      END DO

999   IF (STATUS .NE. 0) THEN
	WRITE(*,*) '   Error in SORT_S_E_DOIT'
      ENDIF

      END

