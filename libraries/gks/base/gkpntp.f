      SUBROUTINE GKPNTP (NSEG, NPKID, NPRIM, XNDC, YNDC)
*
*     Author: KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE
*  -------
*      Select a point within a specified segment part
*
*
*  MAINTENANCE LOG
*  ---------------
*     09/01/89  KEVP  Changed name from GKPKLC to GKPNTP
*                     removed segment read routine argument
*                     added primitive number argument
*                     Changed pick point from DC to NDC
*     16/06/89  KEVP  Made it accept non-positive segment name,
*                     quitting in such a case.
*     16/11/89  RMK   Removed unused local variables.
*
*  ARGUMENTS
*  ---------
*     INP  NSEG    The Segment Name
*     INP  NPKID   The Pick ID of the picked primitive
*     INP  NPRIM   Primitive number
*     INP  XNDC,YNDC  Value taken by pick pt, if no primitive
*                  is found in the specified segment and Pick ID
*                  within the clipping rectangle (NDC).
*     OUT  XNDC,YNDC  The Pick Point in NDC
*
      INTEGER NSEG, NPKID, NPRIM
      REAL XNDC,YNDC
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /STK/    Arrays from CSS return on the stack. This routine
*                     deallocates it.
*     Read   /CCA/    Individual variables from CSS return in this
*                     CSS Communication Area.
*     Modify /ERR/    KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkcca.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IHOLD  Local copy of KERROR. Needed because more than one error
*            condition may occur and we want the first.
*     ITYPE  Type of item returned from CSS
*     NR,NI,NC Number of REALs, INTEGERs, CHARACTER*80's returned from CSS
*     IPR    Stack pointers for array of reals (x & y)
*     IPI    Stack pointer for integers
*     IPC    Stack pointer for CHARACTER*80 information
*     J      Loop pointer
*     JPT    Point in primitive
*     MORE   Indicates whether there is more of the current item not yet
*            returned by CSS (values GMORE and GNMORE)
*
*     DCX,DCY    Used to store a small number of Device Coords
*     FOUND      True, if suitable point has been found
*     IDPK   Current Pick ID
*     IPRIM  Current primitive number
*     ISTAT  Status of segment find
*     IWA,RWA Local arrays saving Workstation Communication Area
*     NWD    Size of arrays WCX,WCY,DCX & DCY
*     PX,PY  Prospective point - needs to be tested for clipping DC
*     PXN,PYN  Prospective point in NDC
*     WCX,WCY     Used to store a small number of World Coords
*
      INTEGER IHOLD,ITYPE, NR,NI,NC, IPR,IPI,IPC
      INTEGER IDPK, IPRIM, NWD, J, JPT, MORE
      PARAMETER (NWD=2)
      INTEGER IWA(KNKWI), IATTS(KSGISZ), ISTAT
      REAL    RWA(KNQWR), RATTS(KSGRSZ), PX,PY, PXN,PYN
      LOGICAL FOUND
      REAL    WCX(NWD),WCY(NWD), DCX(NWD),DCY(NWD)

*  STACK USAGE
*  -----------
*     1*NR     REAL     (twice) used by CSS to return 2 arrays (x & y) of
*                       REAL info
*     1*NI     INTEGER  used by CSS to return 1 array of INTEGER info
*     1*NC     CHARACTER*80  Currently unused .... no CHARACTER stack yet
*
*  ERRORS
*  ------
*      301   Overflow of segment storage ... in this case because pick
*            cannot cope with big segment item
*    -2004   Bug in parameters of internal routine
*
*  COMMENTS
*  --------
*      This routine is built in the structure provided by the
*      routine GKSGCN which plays back a segment.
*
*---------------------------------------------------------------------
*
*      Quit if segment name is non-positive
       IF(NSEG .LE. 0) GOTO 9999
*
*   Set stack pointers for error free deallocation (if error)
       IPC = KNIL
       IPI = KNIL
       IPR = KNIL

*   Save Workstation Communication area in local arrays
      DO 2 J=1,KNKWI
    2   IWA(J)=KWIA(J)
      DO 3 J=1,KNQWR
    3   RWA(J)=QWRA(J)

*   Find segment in CSS.
      CALL GKCSSL(NSEG,ISTAT)
      IF( ISTAT.EQ.KRFUSE ) THEN
        GOTO 9900
      ENDIF

*   Here, segment NSEG exists in CSS.
*   Now get the segment attributes.
      IF( KSGLST.NE.KNIL ) THEN
        CALL GKDRGE(KSGLST,NSEG, KSGISZ,KSGRSZ, IATTS,RATTS)
      ELSE
      KERROR = -2007
      ENDIF

      IF( KERROR.NE.0 ) GOTO 9700

*   Initialise primitive count and pick ID and set as not found
      IDPK = 1
      IPRIM = 1
      FOUND = .FALSE.
*
*   --------------
*   Start of loop.   Read CSS once for each iteration.
*   --------------
    5 CONTINUE

*   Read CSS
      CALL GKCSRD(ITYPE, NR,NI,NC, IPR,IPI,IPC, MORE)
      IF( KERROR.NE.0 ) GOTO 6000

*   Currently we allow polyline and polymarker items that require
*   more than one call. Eventually the workstation interface should
*   be changed to allow all output primitives.
      IF( MORE.EQ.GMORE .AND.
     :   .NOT. ( ITYPE.EQ.KPL .OR. ITYPE.EQ.KPM .OR. ITYPE.EQ.KCA
     :                                                  )) THEN
        KERROR=301
        GOTO 9999
      ENDIF

*  If point is found, skip all further, primitives
      IF(FOUND)GOTO 6000
*   If current Pick ID is not the required Pick ID
*   Skip all primitives.
      IF((KPL .LE. ITYPE) .AND. (ITYPE .LE. KGDP))THEN
        IF((IDPK .NE. NPKID) .AND. (NPKID .GT. 0))THEN
          GOTO 6000
*     If current primitive is not the required one skip it
        ELSEIF((IPRIM .NE. NPRIM) .AND. (NPRIM .GT. 0))THEN
          GOTO 6000
        ENDIF
      ENDIF

*     CASE ITYPE of

      GOTO (     9700,9700,9700,9700,9700,9700,9700,9700,9700,
     :      9700,9700, 120, 120, 140, 120, 160, 120,6000,6000,
     :      6000,6000, 220,9700,9700,9700,9700,9700,9700,9700,
     :      9700, 310,9700,9700,9700,9700,9700,9700,9700,9700,
     :      9700,9700,9700,9700,9700,9700,6000) ITYPE


*     Otherwise
      GOTO 9700

*     --------------------------------------
*     Coord Primitives  KPL, KPM, KFA & KGDP
*     --------------------------------------
  120 CONTINUE
*     Data received and used:
*     NR       :  Number of points in primitive
*     IPR      :  Coordinates - Stack pointer
*
      IF(NR .LT. 1) GOTO 9700
      JPT = 0
*
*     Convert point to DC
  125 CONTINUE
      CALL GKTWD (1,QSTACK(IPR+JPT),QSTACK(IPR+NR+JPT),PX,PY)
*     Test it for Clipping
      JPT = JPT+1
      IF(JPT .GE. NR) GOTO 6000
      CALL GKTDN (1,PX,PY,PXN,PYN)
*     If point is clipped, go onto next one.
*     if no points left, ignore primitive.
      IF(KERROR .EQ. 152)THEN
         KERROR = 0
         GOTO 125
      ENDIF
*     Point Survives Clipping
      XNDC = PXN
      YNDC = PYN
      FOUND = .TRUE.
      GOTO 6000

*     ---------
*     Text  KTX
*     ---------
  140 CONTINUE
      IF(NI .LT. 1) GOTO 9700

*     Data received and used:
*     (from CSS)
*     QSS1    : x-text position
*     QSS2    : y-text position

*     Put data into WCA for use by Text Extent Utilities
      WCX(1) = QSS1
      WCY(1) = QSS2
      CALL GKTWD (1,WCX,WCY,PX,PY)
      CALL GKTDN (1,PX,PY,PXN,PYN)
      IF(KERROR .EQ. 152)THEN
         KERROR = 0
      ELSE
        XNDC = PXN
        YNDC = PYN
        FOUND = .TRUE.
      ENDIF
      GOTO 6000

*     --------------
*     Cell Array KCA
*     --------------
  160 CONTINUE
*     Data received and used:
*     QSS1   : X coordinate of point P
*     QSS2   : Y coordinate of point P
*     QSS3   : X coordinate of point Q
*     QSS4   : Y coordinate of point Q
      IF(NI .LT. 1) GOTO 9700

*     Define cell array rectangle
      WCX(1) = QSS1
      WCY(1) = QSS2
      WCX(2) = QSS3
      WCY(2) = QSS4
*     Convert to DC
      CALL GKTWD (2,WCX,WCY,DCX,DCY)
*     Clip it
      IF(DCX(1) .LT. QWCLXL(KWKIX))THEN
         DCX(1) = QWCLXL(KWKIX)
      ELSEIF(DCX(1) .GT. QWCLXR(KWKIX))THEN
         DCX(1) = QWCLXR(KWKIX)
      ENDIF
      IF(DCX(2) .LT. QWCLXL(KWKIX))THEN
         DCX(2) = QWCLXL(KWKIX)
      ELSEIF(DCX(2) .GT. QWCLXR(KWKIX))THEN
         DCX(2) = QWCLXR(KWKIX)
      ENDIF
      IF(ABS(DCX(1)-DCX(2)) .LT. 0.5)GOTO 6000
      IF(DCY(1) .LT. QWCLYB(KWKIX))THEN
         DCY(1) = QWCLYB(KWKIX)
      ELSEIF(DCY(1) .GT. QWCLYT(KWKIX))THEN
         DCY(1) = QWCLYT(KWKIX)
      ENDIF
      IF(DCY(2) .LT. QWCLYB(KWKIX))THEN
         DCY(2) = QWCLYB(KWKIX)
      ELSEIF(DCY(2) .GT. QWCLYT(KWKIX))THEN
         DCY(2) = QWCLYT(KWKIX)
      ENDIF
      IF(ABS(DCY(1)-DCY(2)) .LT. 0.5)GOTO 6000
*     Take Centre
      PX = (DCX(1) + DCX(2))/2.0
      PY = (DCY(1) + DCY(2))/2.0
*     Convert to NDC
      CALL GKTDN (1,PX,PY,PXN,PYN)
      IF(KERROR .NE. 0)THEN
         KERROR = 0
      ELSE
         XNDC = PXN
         YNDC = PYN
         FOUND = .TRUE.
      ENDIF
      GOTO 6000


*     ----------------------
*     Pick identifier KSPKID
*     ----------------------
  220 CONTINUE
      IDPK=KSS1
      IPRIM = 1
      GOTO 6000

*     -------------------------------------------------------
*     Normalization (C2) and Segment (C3) transformations KNT
*     -------------------------------------------------------
  310 CONTINUE
*     Make GKTWD and GKTND appropiate
      DO 315 J=1,6
        QWRA(J)=0.0
        QWRA(10+J)=RATTS(KSGTRN+J-1)
  315 CONTINUE
      QWR1 = 1.0
      QWR5 = 1.0
      QWR7 = QSS7
      QWR8 = QSS8
      QWR9 = QSS9
      QWR10= QSS10
      CALL GKWKC4
      GOTO 6000

 6000 CONTINUE
*   Increment primitive count
      IPRIM = IPRIM + 1

*   Save KERROR
      IHOLD=KERROR

*   Here to deallocate stack. We pass through here whether there was
*   was error or not.
*   Deallocate stack in reverse order to the parameters returning
*   from GKCSRD.
      CALL GKSTDA(KCHARS,IPC)
      CALL GKSTDA(KINTGS,IPI)
      CALL GKSTDA(KREALS,IPR)
      IF( IHOLD.NE.0 ) KERROR=IHOLD

*   Do another item if error-free and not at the end
      IF( KERROR.EQ.0 .AND. ITYPE.NE.KENSG ) GOTO 5
*
*   -----------
*   End of loop
*   -----------

      GOTO 9999

 9700 CONTINUE
      CALL GKBUG (-2004,'GKPNTP')

*   Restore W.C.A. ... first restore general purpose variables
 9900 CONTINUE
      DO 9910 J=1,KNKWI
 9910   KWIA(J)=IWA(J)
      DO 9920 J=1,KNQWR
 9920   QWRA(J)=RWA(J)


 9999 CONTINUE
      END
