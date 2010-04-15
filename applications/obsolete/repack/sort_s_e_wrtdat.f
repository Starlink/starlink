*+SORT_S_E_WRTDAT Write event data to the mapped list data-arrays
	SUBROUTINE SORT_S_E_WRTDAT(EBUF,EVT,CTOL,XRW,YRW,XCO,YCO,RWT,
     :						SRT,MLIM,NTOT,STATUS)
	IMPLICIT NONE

*   Include files:
	INCLUDE	'SMAPDEF.INC'
	INCLUDE 'SORT_DEF.INC'

* Input:
	RECORD		/EBLOCK/ EBUF
	RECORD 		/SORT_DEF/ SRT
	INTEGER		MLIM		! mapping extent of lists

	REAL		RWT(MLIM)	! Raw time tags
	REAL		XCO(MLIM), YCO(MLIM)
					! Image offsets (arcsec)
	REAL		XRW(MLIM), YRW(MLIM)
					! Linearised det coords

	INTEGER		EVT		! Event time
*	DOUBLE PRECISION  ETOL(3,3)	! Dcm map locals to image locals
	DOUBLE PRECISION  CTOL(3,3)	! Dcm map locals to image locals

* Input/Output
	INTEGER		NTOT		! Total events in evds
	INTEGER		STATUS		! status flag

	DOUBLE PRECISION XLIN, YLIN	! Detector coords of event

* M. Denby
* P McGale May 95 UNIX mods
*-
*   Local constants

*   Local variables :
	INTEGER		IXPIX, IYPIX
	INTEGER		IEV, ITIM
	INTEGER		NS, NE
	INTEGER		LINX, LINY
	INTEGER		ILON,ILAT
	INTEGER		FEVS,FIL
	INTEGER		ASPF
	INTEGER		ISEED
 	integer		hdutyp
	integer		itmp

	LOGICAL		MOONF
	logical 	anynull

	REAL		IRIS2		! Iris radius **2

	DOUBLE PRECISION  XPIX, YPIX
	DOUBLE PRECISION  DXLOC, DYLOC
	DOUBLE PRECISION  VX, VY

*   External functions :
	LOGICAL		CAL_HIT_MASK
	REAL            RAN

	DATA		ISEED/12789531/
* Check status
	IF (STATUS .NE. 0) RETURN

	IRIS2 = (SRT.IRIS)**2
	IEV = 1
        linx = ebuf.ev(iev).linx
        liny = ebuf.ev(iev).liny
        if (linx .gt. 128) linx  = linx - 256
        if (liny .gt. 128) linx  = liny - 256
        XLIN = DBLE(real(LINX)+RAN(ISEED)-0.5)/R2LINP
        YLIN = DBLE(real(LINY)+RAN(ISEED)-0.5)/R2LINP


* Event in IRIS?
        IF ((XLIN*XLIN+YLIN*YLIN) .LT. IRIS2) THEN
          ilon = EBUF.EV(IEV).ELON
          ilat = EBUF.EV(IEV).ELAT
          XPIX = DBLE(real(ILON) + RAN(ISEED) - 0.5)/R2LONP
          YPIX = DBLE(real(ILAT) + RAN(ISEED) - 0.5)/R2LATP
          CALL AX_DTRANS(XPIX,YPIX,CTOL,.TRUE.,DXLOC,DYLOC)
          IF (DXLOC.GT.PI) DXLOC = DXLOC - TWOPI
        ENDIF

* Within selected region?
        IF (ABS(DXLOC) .LT. SRT.DAZ .AND.
     :                                ABS(DYLOC) .LT. SRT.DEL) THEN
	  NTOT = NTOT + 1
          IF (NTOT .GT. MLIM) THEN
	     WRITE(*,*) '   Error in SORT_S_E_WRTDAT',
     :			         ' - Internal buffer overflow'
	     GOTO 999
          END IF


          XRW(NTOT) = REAL(XLIN/DTOR)*60.
          YRW(NTOT) = REAL(YLIN/DTOR)*60.
          XCO(NTOT) = REAL(DXLOC/DTOR)*60.
          YCO(NTOT) = REAL(DYLOC/DTOR)*60.
          RWT(NTOT) = REAL(DBLE(EVT)/
     :	           	     32.D0 - (SRT.SMJD - SRT.RMJD)*86400.D0)
        ENDIF


999	IF (STATUS .NE. 0) THEN
	  WRITE(*,*) '   Error in SORT_S_E_WRTDAT'
	ENDIF

	END
