C# IL>=a, OL>=1
      SUBROUTINE GKPPSG(XP,YP,TRNC2,TRNC3,XFTSUB,XPPSUB,PREHIT,
     :                  BOXDEF,DIST,NPKID,NPRIM,IPTYPE,DIRHIT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To read the contents of a segment
*     as directed by the input parameters
*     to determine whether any primitive is picked
*     and if so, what the its Pick ID is.
*
*     Subroutine terminates when end of segment
*     is reached or error occurs.
*
*  MAINTENANCE LOG
*  ---------------
*     03/02/88  KEVP  Stabilised
*     06/01/89  KEVP  Name changed from GKLPSG to GKPPSG
*                     Segment Read Routine deleted from arguments
*                     DIST,NPRIM and DIRHIT added to arguments
*     16/01/89  KEVP  Added primitive type and PREHIT to arguments
*     16/01/89  KEVP  Changed bounding boxes from WC to DC
*
*  ARGUMENTS
*  ---------
*     INP  XP,YP   The Pick Point
*     INP  TRNC2   Transformation C2 - combined insert transformation
*     INP  TRNC3   Transformation C3 - combined segment transformation
*                  (segment itself is in KRPSG (in COMMON)).
*     INP  XPPSUB  Device primitive pick routine
*     INP  PREHIT  True, if hit in previous (ie, higher) segment
*     INP  BOXDEF  True, if bounding box needs defining
*     I/O  DIST    As input, the Pick Aperture
*                  As output the distance from the nearest primitive
*                  so far, if less.
*     OUT  NPKID   The Pick ID of the picked primitive or
*                  KNIL if no pick has occurred
*     OUT  NPRIM   The ordinal number of the picked primitive
*     OUT  IPTYPE  Primitive Type
*     OUT  DIRHIT  True, if direct hit on picked primitive
*
*
      REAL XP,YP, TRNC2(6),TRNC3(6), DIST
      EXTERNAL  XFTSUB,  XPPSUB
      INTEGER NPKID, NPRIM, IPTYPE
      LOGICAL PREHIT, BOXDEF, DIRHIT
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
      INCLUDE '../include/gkwdt.cmn'
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
*     ISCTYP Type of item to be scanned
*     NR,NI,NC Number of REALs, INTEGERs, CHARACTER*80's returned from CSS
*     IPR    Stack pointer for array of reals (x & y) WC
*     IPI    Stack pointer for integers
*     IPC    Stack pointer for CHARACTER*80 information
*     J      Loop pointer
*     MINPTS Minimum number of points for a primitive
*     MORE   Indicates whether there is more of the current item not yet
*            returned by CSS (values GMORE and GNMORE)
*
*     DCX,DCY    Used to store a small number of Device Coords
*     DIN    Distance within clipping rectangle from an edge
*     IDC    Stack pointer for array of reals (x & y) DC
*     IDPK   Current Pick ID
*     INSIDE True, if pick point is inside current primitive
*     NCOUNT Primitive Count
*     NWD    Size of arrays WCX,WCY,DCX & DCY
*     OUTCLP True if pick point is outside of clipping rectangle
*     PKAP   Local copy of pick apperture - This is reduced,
*            if the pick point is close to the clipping boundary
*            Negative value indicates that the
*            pick pt is ouside the clipping rectangle.
*     SQCD   Squared Distance to edge of current primitive
*     SQDIST Squared Distance to edge of nearest primitive
*     SQPKAP Square of pick aperture
*     UP     Used to normalise character up vector
*     WCX,WCY     Used to store a small number of World Coords
*
      INTEGER IHOLD,ITYPE,ISCTYP, NR,NI,NC, IPR,IPI,IPC, MORE
      INTEGER IDPK, NCOUNT, NWD, IDC, J, MINPTS(4)
      PARAMETER (NWD=4)
      LOGICAL INSIDE, OUTCLP
      REAL    DIN, PKAP, SQPKAP, UP, SQDIST, SQCD
      REAL    WCX(NWD),WCY(NWD), DCX(NWD),DCY(NWD)

      DATA MINPTS /2,1,0,3/
*
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
*      It is assumed that the Workstation Communication Area
*      has been saved locally, allowing it
*      to be overwritten for the segment being read.
*
*      The name of the segment to be read is determined
*      by the latest successful execution of GKCSSL.
*---------------------------------------------------------------------

*   Set Output Arguments to their NOPICK values
      NPKID = KNIL
      NPRIM = KNIL
      IPTYPE = KNIL
      DIRHIT = .FALSE.
*   Initialise Primitive Count
      NCOUNT = 0
*   Set Squared Pick Aperture and Initial Squared Distance
      SQPKAP = DIST*DIST
      SQDIST = SQPKAP + 1.0
*   Set Stack pointers for tidy deallocation, if error
*     IDC = KNIL
      IPC = KNIL
      IPI = KNIL
      IPR = KNIL

*   --------------
*   Start of loop.   Read CSS once for each iteration.
*   --------------
    5 CONTINUE

*   Read CSS
      CALL GKCSRD(ITYPE, NR,NI,NC, IPR,IPI,IPC, MORE)
      IF( KERROR.NE.0 ) GOTO 6000

*     If pick point is outside clip rectangle, ignore any primitives
      IF(PKAP .LE. 0.0) THEN
         IF((KPL .LE. ITYPE) .AND. (ITYPE .LE. KGDP)) GOTO 6000
      ENDIF

*     Currently we allow polyline and polymarker items that require
*     more than one call. Eventually the workstation interface should
*     be changed to allow all output primitives.
      IF( MORE.EQ.GMORE .AND.
     :   .NOT. ( ITYPE.EQ.KPL .OR. ITYPE.EQ.KPM .OR. ITYPE.EQ.KCA
     :                                                    )) THEN
        KERROR=301
        GOTO 9999
      ENDIF

*     CASE ITYPE of

      GOTO (     9700,9700,9700,9700,9700,9700,9700,9700,9700,
     :      9700,9700, 120, 120, 140, 120, 160, 170,6000, 190,
     :       200,6000, 220,9700,9700,9700,9700,9700,9700,9700,
     :      9700, 310,9700,9700,9700,9700,9700,9700,9700,9700,
     :      9700,9700,9700,9700,9700,9700, 460) ITYPE


*     Otherwise
      GOTO 9700

*     ---------------------------
*     Primitives KPL, KPM and KFA
*     ---------------------------
  120 CONTINUE
*     Data received and used:
*     NR       :  Number of points in primitive
*     IPR      :  Primitive's Coordinates - Stack pointer
*
*     Check for minimum number of points
      IF (NR .LT. MINPTS(ITYPE - KPL + 1))GOTO 9700
*
*     Obtain Stack Space for DC
      CALL GKSTAL(KREALS,NR*2,IDC)
      IF(KERROR .NE. 0)GOTO 9999
*     Convert to DC
      CALL GKTWD (NR,QSTACK(IPR),QSTACK(IPR+NR),
     :               QSTACK(IDC),QSTACK(IDC+NR))
*     Set Scantype
      IF((ITYPE .EQ. KFA) .AND. PREHIT)THEN
*       No need to scan areas if there was a hit in a previous segment
        ISCTYP = KPL
      ELSE
        ISCTYP = ITYPE
      ENDIF
*     Scan primitive for pick (in DC)
      CALL XPPSUB (ISCTYP,XP,YP,NR,QSTACK(IDC),QSTACK(IDC+NR),
     :             BOXDEF,SQPKAP,SQCD,INSIDE)

*     Release Stack
      CALL GKSTDA (KREALS,IDC)

*     Update bounding box, if being defined
      IF(BOXDEF) CALL GKPPBB (NR,QSTACK(IDC),QSTACK(IDC+NR))

      GOTO 5000

*     ---------
*     Text  KTX
*     ---------
  140 CONTINUE
      IF(NI .LT. 1) GOTO 9700

*     Data received and used:
*     (from CSS)
*     NI      : Length of string
*     IPI     : Integer character codes for string - Stack pointer
*     QSS1    : x-text position
*     QSS2    : y-text position
*     (from text-extent utilities)
*     KERROR  : error indicator
*     WCX(1-4): x-text extent
*     WCY(1-4): y-text extent

*     Put data into WCA for use by Text Extent Utilities
      QWR1 = QSS1
      QWR2 = QSS2
*     calculate height and width vectors
*     QWR3:wd(x), QWR4:wd(y), QWR5:ht(x), QWR6:ht(y)
      UP = SQRT(QCCHUX*QCCHUX + QCCHUY*QCCHUY)
      QWR3 = (QCCHUY/UP)*QCCHH
      QWR5 = (QCCHUX/UP)*QCCHH
      QWR4 = -QWR5
      QWR6 = QWR3
*     QWCHRX(KWKIX),QWCHRY(KWKIX) : baseline vector
*
*     stroke precision
      IF (KWTXPR(KWKIX) .EQ. GSTRKP) THEN
        CALL GKXQXO(NI,KSTACK(IPI),WCX,WCY)
*     string and char precision
      ELSE
*       baseline vector from ws Set text attributes entry
        CALL GKXQXC (NI,KSTACK(IPI),QWCHRX(KWKIX),QWCHRY(KWKIX),
     :                  WCX,WCY,XFTSUB)

      ENDIF
      IF(KERROR .NE. 0) GOTO 9700

*     Convert text extent to DC
      CALL GKTWD (4,WCX,WCY,DCX,DCY)

*     Scan text-extent for pick point
      IF(PREHIT)THEN
        CALL XPPSUB (KPL,XP,YP,4,DCX,DCY,BOXDEF,SQPKAP,SQCD,INSIDE)
      ELSE
        CALL XPPSUB (KFA,XP,YP,4,DCX,DCY,BOXDEF,SQPKAP,SQCD,INSIDE)
      ENDIF

*     Update bounding box, if being defined
      IF(BOXDEF) CALL GKPPBB (4,DCX,DCY)
      GOTO 5000

*     --------------
*     Cell Array KCA
*     --------------
  160 CONTINUE
*     Data received and used:
*     NI     : Number of cells
*     QSS1   : X coordinate of point P
*     QSS2   : Y coordinate of point P
*     QSS3   : X coordinate of point Q
*     QSS4   : Y coordinate of point Q
*     QSS5   : X coordinate of point R
*     QSS6   : Y coordinate of point R
      IF(NI .LT. 1) GOTO 9700

*     Define cell array rectangle
      WCX(1) = QSS1
      WCY(1) = QSS2
      WCX(2) = QSS5
      WCY(2) = QSS6
      WCX(3) = QSS3
      WCY(3) = QSS4
*     Convert to DC
      CALL GKTWD (3,WCX,WCY,DCX,DCY)
*     Define fourth point in rectangle
      DCX(4) = DCX(1) + DCX(3) - DCX(2)
      DCY(4) = DCY(1) + DCY(3) - DCY(2)
*
*     Scan cell-array rectangle for pick point
      IF(PREHIT)THEN
        CALL XPPSUB (KPL,XP,YP,4,DCX,DCY,BOXDEF,SQPKAP,SQCD,INSIDE)
      ELSE
        CALL XPPSUB (KFA,XP,YP,4,DCX,DCY,BOXDEF,SQPKAP,SQCD,INSIDE)
      ENDIF

*     Update bounding box, if being defined
      IF(BOXDEF) CALL GKPPBB (4,DCX,DCY)

      GOTO 5000

*     ----------------------------------
*     Generalised Drawing Primitive KGDP
*     ----------------------------------
  170 CONTINUE

*     Data received and used:
*     NR     : Number of points
*     IPR    : Coordinates - Stack pointer
*     KSS1   : GDP identifier
*     KSS3   : Indicates whether there was a hit in a previous segment
*     QSS1   : X coordinate of point P
*     QSS2   : X coordinate of point Q
*     QSS3   : X coordinate of point R
*     QSS4   : Y coordinate of point P
*     QSS5   : Y coordinate of point Q
*     QSS6   : Y coordinate of point R

      IF(NR .LT. 2) GOTO 9700
*     Temporary instruction
      IF(NR .GT. 3) GOTO 9700
*     Set PREHIT indicator in CSS communication area
      IF(PREHIT)THEN
         KSS3 = 1
      ELSE
         KSS3 = 0
      ENDIF
      CALL XPPSUB (KGDP,XP,YP,NR,QSTACK(IPR),QSTACK(IPR+NR),
     :             BOXDEF,SQPKAP,SQCD,INSIDE)
*     Bounding box defining is done inside XPPSUB for GDP.

      GOTO 5000


*     ---------------------------
*     Polymarker attributes KSPMA
*     ---------------------------
  190 CONTINUE
      KIPMI=KSPMI
      KIMKTY=KSMKTY
      QIMKSZ=QSMKSZ
      DO 195 J=1,3
  195   KIPMAF(J)=KSPMAF(J)
      KSPMWK=KHANGE
      CALL GKDPMB
* Need to check because individual settings won't have been checked.
      IF (KWMKTY(KWKIX).GT.5) KWMKTY(KWKIX) = 3
      GOTO 6000

*     -----------------------
*     Text attributes   KSTXA
*     -----------------------
  200 CONTINUE
      KITXI=KSTXI
      KITXFN=KSTXFN
      KITXPR=KSTXPR
      KITXP =KSTXP
      KIHTXA=KSHTXA
      KIVTXA=KSVTXA
      QICHXP=QSCHXP
      QICHSP=QSCHSP
      QICHHX=QSCHHX
      QICHHY=QSCHHY
      QICHWX=QSCHWX
      QICHWY=QSCHWY
      DO 205 J=1,4
  205   KITXAF(J)=KSTXAF(J)
      KSTXWK=KHANGE
      CALL GKDTXB
      GOTO 6000

*
*     ----------------------
*     Pick identifier KSPKID
*     ----------------------
  220 CONTINUE
      IDPK=KSS1
      NCOUNT = 0
      GOTO 6000

*     -------------------------------------------------------
*     Normalization (C2) and Segment (C3) transformations KNT
*     -------------------------------------------------------
  310 CONTINUE
*     Change world coordinates to CSS WC which are NDC
*     Make GKTWD apply the appropriate segment transformation in NDC
      DO 315 J=1,6
        QWRA(J)=TRNC2(J)
        QWRA(10+J)=TRNC3(J)
  315 CONTINUE
      QWR7=QSS7
      QWR8=QSS8
      QWR9=QSS9
      QWR10=QSS10
      CALL GKWKC4
*     Check if pick point is within the clipping rectangle
*        if not set PKAP negative
*        else
*          if necessary reduce PKAP so that nothing is picked
*             from outside the clipping rectangle
      PKAP = DIST
      DIN = XP - QWCLXL(KWKIX)
      IF(DIN .LT. PKAP) PKAP = DIN
      DIN = QWCLXR(KWKIX) - XP
      IF(DIN .LT. PKAP) PKAP = DIN
      DIN = YP - QWCLYB(KWKIX)
      IF(DIN .LT. PKAP) PKAP = DIN
      DIN = QWCLYT(KWKIX) - YP
      IF(DIN .LT. PKAP) PKAP = DIN
      OUTCLP = (PKAP .LT. 0.0)
*     Update squared copy
      SQPKAP = PKAP*PKAP
      GOTO 6000


*     ------------------
*     End Segment  KENSG
*     ------------------
  460 CONTINUE
*     In case the segment has no primitives in it
*     and its bounding box is being defined,
*       make sure that it is defined,
*       but empty in such a case.
      IF(BOXDEF) CALL GKPPBE
      GOTO 6000

 5000 CONTINUE

*     ----------------
*     End of primitive
*     ----------------
*     Increment Primitive Count
      NCOUNT = NCOUNT + 1
*     Update distance, pick ID and Primitive Number
      IF(INSIDE .OR.
     :   ((SQCD .LE. SQPKAP) .AND. (SQCD .LE. SQDIST)))THEN
        SQDIST = SQCD
        NPKID = IDPK
        NPRIM = NCOUNT
        IPTYPE = ITYPE
        DIRHIT = INSIDE
      ENDIF

 6000 CONTINUE

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

*   Set Distance and quit
      IF(NPKID .NE. KNIL)THEN
         IF(DIRHIT)THEN
           DIST = 0.0
         ELSEIF(SQDIST .LT. SQPKAP)THEN
           DIST = SQRT(SQDIST)
         ENDIF
      ENDIF
      GOTO 9999

 9700 CONTINUE
      CALL GKBUG (-2004,'GKPPSG')

 9999 CONTINUE
      END
