C# IL>=a, OL>=0
      SUBROUTINE GIITM ( ITYPE, IDRL, NCD, STR )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    USER CALLABLE
*  GKS Function name:  INTERPRET ITEM
*  Author:             DSG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Interpret the item read in by READ ITEM FROM GKSM.
*
*  MAINTENANCE LOG
*  ---------------
*     24/12/83  DSG   Original version stabilized
*     10/01/84  DSG   Array bounds corrected
*     12/01/84  DSG   Improvement in error reporting
*     12/01/84  DSG   Code for MESSAGE put in
*     12/01/84  DSG   Colour intensities put in (COLOUR REP.)
*     17/01/84  DSG   Local character string replaced by CSTR
*     18/01/84  DSG   Bug fixes
*     23/01/84  DSG   Call to GKMPAR introduced
*     17/04/84  DSG   Fix of Bug S44 - missing argument in GSTXR call
*     17/04/84  DSG   Fix of Bug S45 - Vertical text alignment
*     17/04/84  DSG   Fix of Bug S46 - Character expansion factor
*     30/04/84  DSG   S49: Now clips to the viewport boundary
*     30/04/84  DSG   S50: Set Segment transformation bug fixed
*     02/05/84  DSG   S51: Polyline representation bug fixed
*     19/01/87  CJC   IS conversion. Add new argument IDRL and clarify
*                     argument NCD.
*     20/01/87  DCS   IS conversion. Remove metafile index, Metafile
*                     Input Attribute List and set GKS State List.
*                     Remove test on value of primitive last source
*                     flags (KSxxWK) since now only two possible values.
*                     Set last source flags for all attributes. Align
*                     setting of aspect source flags with GSASF.
*     01/04/87  RMK   Error changes for IS version.
*     01/10/87  PJWR  Segment transformation matrix is now transposed
*                     from row major order (from metafile) to column
*                     major order (for GSSGT) using array TRLA.  Fixes
*                     bug S279.
*     02/10/87  DSG   Bug fix S280: to allow the interpretation of
*                     negative integers from the GKSM (GSTXR and GSFAR).
*     27/10/87  DSG   Bug fix S288: as for S280 - GSTXFP and GSFASI.
*     09/11/87  RMK   Removed unused label 7100.
*     19/06/89  RTP   Change call to GMSG to use substring (S376).
*     21/03/90  RTP   Change linetype, markertype and escape to unpack
*                     possible negative integers (S387).
*
*  ARGUMENTS
*  ---------
*     INP   ITYPE  Item type
*     INP   IDRL    Item data record length
*     INP   NCD    Item data record dimension
*     INP   STR     Item data record
*
      INTEGER ITYPE, IDRL, NCD
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYERR/    KERROR
*     Read   /GKYWCB/    KWKID, KNACWK, KACPT
*     Modify /GKYSL/     Attributes, clipping rectangle, last source
*                        flags
*     Modify /GKYWCA/    QWR1-4
*                           Character workspace CSTR
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gaspct.par'
      INCLUDE '../include/gkpid.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*
*  Loop variable
      INTEGER J
*
*  Workstation identifier
      INTEGER IWKID
*
*  Packing routine pointer
      INTEGER IAT
*
*  Length of character array that the data record is unpacked into
      INTEGER LENGTH
      PARAMETER (LENGTH = 1)
*
*  Offsets to aspect source list for primitives
      INTEGER    IPL,   IPM,   ITX,   IFA
      PARAMETER (IPL=0, IPM=3, ITX=6, IFA=10)
*
*  Local integer array
      INTEGER ILA(13)
*
*  Local real arrays
      REAL RLA(6), TRLA(6)
*
*  Map the Metafile item types onto Error handler routine names
*
      INTEGER NAME(100)
      DATA NAME/ ECLRWK, ERSGWK, EUWK  , ESDS  , EMSG  ,
     :           EESC  , 0     , 0     , 0     , 0     ,
     :           EPL   , EPM   , ETX   , EFA   , ECA   ,
     :           EGDP  , 0     , 0     , 0     , 0     ,
     :           ESPLI , ESLN  , ESLWSC, ESPLCI, ESPMI ,
     :           ESMK  , ESMKSC, ESPMCI, ESTXI , ESTXFP,
     :           ESCHXP, ESCHSP, ESTXCI, ESCHH , ESCHUP,
     :           ESTXP , ESTXAL, ESFAI , ESFAIS, ESFASI,
     :           ESFACI, ESPA  , ESPARF, ESASF , ESPKID,
     :           0     , 0     , 0     , 0     , 0     ,
     :           ESPLR , ESPMR , ESTXR , ESFAR , ESPAR ,
     :           ESCR  , 0     , 0     , 0     , 0     ,
     :           ESVP  , 0     , 0     , 0     , 0     ,
     :           0     , 0     , 0     , 0     , 0     ,
     :           ESWKWN, ESWKVP, 0     , 0     , 0     ,
     :           0     , 0     , 0     , 0     , 0     ,
     :           ECRSG , ECLSG , ERENSG, EDSG  , 0     ,
     :           0     , 0     , 0     , 0     , 0     ,
     :           ESSGT , ESVIS , ESHLIT, ESSGP , ESDTEC,
     :           0     , 0     , 0     , 0     , EWITM /
*
*
*  ERRORS
*  ------
*        7   GKS not in proper state: GKS shall be in one of
*             states WSOP, WSAC, or SGOP
*      161   Item length is invalid
*      163   Metafile item is invalid
*      164   Item type is not a valid GKS item
*      167   User item cannot be interpreted
*      300   Storage overflow has occurred in GKS
*    -1048   Error detected while interpreting a metafile item
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------



      CALL GKPRLG(EIITM,GGKOP,GSGOP)
      IF(KERROR.NE.0) GOTO 9950

* Make crude check on item data record length

      IF(IDRL.LT.0 .OR. IDRL.GT.NCD*80) THEN
        KERROR = 161
        GOTO 9950
      ENDIF

* Check the item type: a "User Item" will not be interpreted

      IF(ITYPE.GT.100) THEN
        KERROR = 167
        GOTO 9950
      ENDIF

      IAT=1


* Group items by class

      IF (ITYPE.GE.0.AND.ITYPE.LE.6) GOTO 1
      IF (ITYPE.GE.11.AND.ITYPE.LE.16) GOTO 1000
      IF (ITYPE.GE.21.AND.ITYPE.LE.44) GOTO 2000
      IF (ITYPE.GE.51.AND.ITYPE.LE.56) GOTO 5000
      IF (ITYPE.EQ.61.OR.ITYPE.EQ.71.OR.ITYPE.EQ.72) GOTO 6000
      IF (ITYPE.GE.81.AND.ITYPE.LE.84) GOTO 8000
      IF (ITYPE.GE.91.AND.ITYPE.LE.95) GOTO 9000

* Any other item type is invalid

      KERROR = 164
      GOTO 9950

* CONTROL ITEMS
* -------------
    1 GOTO (50,100,200,300,400,500,600) ITYPE+1

* End item
* --------
* Last item of every GKS metafle. No action necessary.

   50 CONTINUE
      GOTO 9925

* Clear Workstation
* -----------------
* Requests CLEAR WORKSTATION on all active workstations

  100 CONTINUE
* The effect of this is obtainable by calling the user-callable
* Clear Workstation routine for each active workstation in turn.

      CALL GKUPSI(IAT,STR,1,ILA)

      DO 110 J=1,KNACWK
        IWKID = KWKID(KACPT(J))
        CALL GCLRWK(IWKID,ILA(1))
  110 CONTINUE

      GOTO 9925

* Redraw all segments on workstation
* ----------------------------------
* Requests REDRAW ALL SEGMENTS ON WORKSTATION on all active
* workstations

  200 CONTINUE

      DO 210 J=1,KNACWK
        IWKID = KWKID(KACPT(J))
        CALL GRSGWK(IWKID)
  210 CONTINUE
      GOTO 9925

* Update workstation
* ------------------
* Requests UPDATE WORKSTATION on all active workstations

  300 CONTINUE

      CALL GKUPSI(IAT,STR,1,ILA)

      DO 310 J=1,KNACWK
        IWKID = KWKID(KACPT(J))
        CALL GUWK(IWKID,ILA(1))
  310 CONTINUE

      GOTO 9925

* Deferral state
* --------------
* Requests SET DEFERRAL STATE on all active workstations

  400 CONTINUE

      CALL GKUPSI(IAT,STR,2,ILA)

      DO 410 J=1,KNACWK
        IWKID = KWKID(KACPT(J))
        CALL GSDS(IWKID,ILA(1),ILA(2))
  410 CONTINUE

      GOTO 9925

* Message
* -------
* Requests MESSAGE on all active workstations

  500 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      IAT=IAT+KPDSSZ

* Allow for possible truncation having occurred at input
      IF(ILA(1).GT.(NCD*80-KPDSSZ)/KPDCSZ) THEN
        KERROR=165
        GOTO 9950
      ENDIF

* Is the message too long?
      IF(ILA(1).GT.KWCST) THEN
        KERROR = 300
        GOTO 9950
      ENDIF

* Unpack into character string
      CALL GKUPC(IAT,STR,LENGTH,ILA(1),CSTR)

* Send to all active workstations
      DO 520 J=1,KNACWK
        IWKID =KWKID(KACPT(J))
        CALL GMSG(IWKID,CSTR(1)(1:ILA(1)))
  520 CONTINUE

      GOTO 9925

* Escape
* ------
* Requests ESCAPE

  600 CONTINUE
      GOTO 9925


* OUTPUT PRIMITIVES
* -----------------
* Interpretation of items in this class generates output
* corresponding to the primative functions, except that
* coordinates of points are expressed in NDC.

 1000 CONTINUE
      CALL GKPRLG(NAME(ITYPE),GWSAC,GSGOP)
      IF(KERROR.NE.0) GOTO 9900

      GOTO (1100,1200,1300,1400,1500,1600) ITYPE-10

* Polyline
 1100 CALL GKMPL(NCD,STR)
      GOTO 9900

* Polymarker
 1200 CALL GKMPM(NCD,STR)
      GOTO 9900

* Text
 1300 CALL GKMTX(NCD,STR)
      GOTO 9900

* Fill area
 1400 CALL GKMFA(NCD,STR)
      GOTO 9900

* Cell array
 1500 CALL GKMCA(NCD,STR)
      GOTO 9900

* GDP
 1600 CALL GKMGDP(NCD,STR)
      GOTO 9900


* OUTPUT PRIMITIVE ATTRIBUTES
* ---------------------------
* Interpretation of items in this class sets values in the
* GKS state list.

 2000 CONTINUE
      CALL GKPRLG(NAME(ITYPE),GGKOP,GSGOP)
      IF(KERROR.NE.0) GOTO 9900

      GOTO (2100,2200,2300,2400,2500,2600,2700,2800,2900,3000,
     :      3100,3200,3300,3400,3500,3600,3700,3800,3900,4000,
     :      4100,4200,4300,4400) ITYPE-20

* Set polyline index
 2100 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      KCPLI=ILA(1)
      KSPLWK=KHANGE
      GOTO 9900

* Set linetype
 2200 CONTINUE
      CALL GKUPI(IAT,STR,1,ILA)
      KCLNTY=ILA(1)
      IF(KCPLAF(KLNTYA).EQ.GINDIV) KSPLWK=KHANGE
      GOTO 9900

* Set linewidth scale factor
 2300 CONTINUE
      CALL GKUPR(IAT,STR,1,RLA)
      QCLNWD=RLA(1)
      IF(KCPLAF(KLNWDA).EQ.GINDIV) KSPLWK=KHANGE
      GOTO 9900

* Set polyline colour index
 2400 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      KCPLCI=ILA(1)
      IF(KCPLAF(KPLCIA).EQ.GINDIV) KSPLWK=KHANGE
      GOTO 9900

* Set polymarker index
 2500 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      KCPMI=ILA(1)
      KSPMWK=KHANGE
      GOTO 9900

* Set marker type
 2600 CONTINUE
      CALL GKUPI(IAT,STR,1,ILA)
      KCMKTY=ILA(1)
      IF(KCPMAF(KMKTYA).EQ.GINDIV) KSPMWK=KHANGE
      GOTO 9900

* Set marker size scale factor
 2700 CONTINUE
      CALL GKUPR(IAT,STR,1,RLA)
      QCMKSZ=RLA(1)
      IF(KCPMAF(KMKSZA).EQ.GINDIV) KSPMWK=KHANGE
      GOTO 9900

* Set polymarker colour index
 2800 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      KCPMCI=ILA(1)
      IF(KCPMAF(KPMCIA).EQ.GINDIV) KSPMWK=KHANGE
      GOTO 9900

* Set text index
 2900 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      KCTXI=ILA(1)
      KSTXWK=KHANGE
      GOTO 9900

* Set text font and precision
 3000 CONTINUE
      CALL GKUPI(IAT,STR,1,ILA)
      KCTXFN=ILA(1)
      IAT=IAT+KPDISZ
      CALL GKUPSI(IAT,STR,1,ILA)
      KCTXPR=ILA(1)
      IF(KCTXAF(KTXFNA).EQ.GINDIV) KSTXWK=KHANGE
      GOTO 9900

* Set character expansion factor
 3100 CONTINUE
      CALL GKUPR(IAT,STR,1,RLA)
      QCCHXP=RLA(1)
      IF(KCTXAF(KCHXPA).EQ.GINDIV) KSTXWK=KHANGE
      GOTO 9900

* Set character spacing
 3200 CONTINUE
      CALL GKUPR(IAT,STR,1,RLA)
      QCCHSP=RLA(1)
      IF(KCTXAF(KCHSPA).EQ.GINDIV) KSTXWK=KHANGE
      GOTO 9900

* Set text colour index
 3300 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      KCTXCI=ILA(1)
      IF(KCTXAF(KTXCIA).EQ.GINDIV) KSTXWK=KHANGE
      GOTO 9900

* Set charactor vectors
 3400 CONTINUE
      CALL GKUPR(IAT,STR,4,RLA)
      CALL GKTNWV(RLA(1),RLA(2),QCCHUX,QCCHUY)
      QCCHH=SQRT(QCCHUX**2+QCCHUY**2)
      CALL GKTNWV(RLA(3),RLA(4),QCCHBX,QCCHBY)
      QCCHW=SQRT(QCCHBX**2+QCCHBY**2)
      KSTXWK=KHANGE
      GOTO 9900

* Set text path
 3500 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      KCCHP=ILA(1)
      KSTXWK=KHANGE
      GOTO 9900

* Set text alignment
 3600 CONTINUE
      CALL GKUPSI(IAT,STR,2,ILA)
      KCHZCH=ILA(1)
      KCVTCH=ILA(2)
      KSTXWK=KHANGE
      GOTO 9900

* Set fill area index
 3700 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      KCFAI=ILA(1)
      KSFAWK=KHANGE
      GOTO 9900

* Set fill area interior style
 3800 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      KCFAIS=ILA(1)
      IF(KCFAAF(KFAISA).EQ.GINDIV) KSFAWK=KHANGE
      GOTO 9900

* Set fill area style index
 3900 CONTINUE
      CALL GKUPI(IAT,STR,1,ILA)
      KCFASI=ILA(1)
      IF(KCFAAF(KFASIA).EQ.GINDIV) KSFAWK=KHANGE
      GOTO 9900

* Set fill area colour index
 4000 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      KCFACI=ILA(1)
      IF(KCFAAF(KFACIA).EQ.GINDIV) KSFAWK=KHANGE
      GOTO 9900

* Set pattern size
 4100 CONTINUE
      CALL GKUPR(IAT,STR,4,RLA)
      CALL GKTNWV(RLA(1),RLA(2),QCPAWX,QCPAWY)
      CALL GKTNWV(RLA(3),RLA(4),QCPAHX,QCPAHY)
      KSFAWK=KHANGE
      GOTO 9900

* Set pattern reference point
 4200 CONTINUE
      CALL GKUPR(IAT,STR,2,RLA)
      QCPAX=RLA(1)
      QCPAY=RLA(2)
      KSFAWK=KHANGE
      GOTO 9900

* Set aspect source flags
 4300 CALL GKUPSI(IAT,STR,13,ILA)
      DO 4330 J=KLNTYA,KPLCIA
        IF(ILA(J+IPL).NE.KCPLAF(J)) THEN
          KCPLAF(J)=ILA(J+IPL)
          KSPLWK=KHANGE
        ENDIF
 4330 CONTINUE

      DO 4340 J=KMKTYA,KPMCIA
        IF(ILA(J+IPM).NE.KCPMAF(J)) THEN
          KCPMAF(J)=ILA(J+IPM)
          KSPMWK=KHANGE
        ENDIF
 4340 CONTINUE

      DO 4350 J=KTXFNA,KTXCIA
        IF(ILA(J+ITX).NE.KCTXAF(J)) THEN
          KCTXAF(J)=ILA(J+ITX)
          KSTXWK=KHANGE
        ENDIF
 4350 CONTINUE

      DO 4360 J=KFAISA,KFACIA
        IF(ILA(J+IFA).NE.KCFAAF(J)) THEN
          KCFAAF(J)=ILA(J+IFA)
          KSFAWK=KHANGE
        ENDIF
 4360 CONTINUE

      GOTO 9900
* Pick identifier
 4400 CONTINUE
      GOTO 9900

* WORKSTATION ATTRIBUTES
* ----------------------
* Interpretation of items in this class has the same effect as
* invocation of ther corresponding GKS functions. The GKS functions
* are performed on all active workstations.

 5000 CONTINUE

      GOTO(5100,5200,5300,5400,5500,5600) ITYPE-50

* Polyline representation
*     Unpack name record
 5100 CALL GKUPSI(IAT,STR,2,ILA)
      IAT=IAT+KPDSSZ*2
      ILA(3)=ILA(1)
      CALL GKUPR(IAT,STR,1,RLA)
      IAT=IAT+KPDRSZ
      CALL GKUPSI(IAT,STR,1,ILA)

*     Send to each active workstation in turn
      DO 5110 J=1,KNACWK
        IWKID=KWKID(KACPT(J))
        CALL GSPLR(IWKID,ILA(3),ILA(2),RLA(1),ILA(1))
 5110 CONTINUE
      GOTO 9925

* Polymarker representation
 5200 CALL GKUPSI(IAT,STR,2,ILA)
      IAT=IAT+KPDSSZ*2
      ILA(3)=ILA(1)
      CALL GKUPR(IAT,STR,1,RLA)
      IAT=IAT+KPDRSZ
      CALL GKUPSI(IAT,STR,1,ILA)

      DO 5210 J=1,KNACWK
        IWKID=KWKID(KACPT(J))
        CALL GSPMR(IWKID,ILA(3),ILA(2),RLA(1),ILA(1))
 5210 CONTINUE
      GOTO 9925

* Text representation
 5300 CALL GKUPSI(IAT,STR,1,ILA)
      IAT=IAT+KPDSSZ
      ILA(4)=ILA(1)
      CALL GKUPI(IAT,STR,1,ILA)
      IAT=IAT+KPDISZ
      ILA(2)=ILA(1)
      CALL GKUPSI(IAT,STR,1,ILA)
      ILA(3)=ILA(1)
      IAT=IAT+KPDSSZ
      CALL GKUPR(IAT,STR,2,RLA)
      IAT=IAT+KPDRSZ*2
      CALL GKUPSI(IAT,STR,1,ILA)

      DO 5310 J=1,KNACWK
        IWKID=KWKID(KACPT(J))
        CALL GSTXR(IWKID,ILA(4),ILA(2),ILA(3),RLA(1),RLA(2),ILA(1))
 5310 CONTINUE
      GOTO 9925

* Fill area representation
 5400 CALL GKUPSI(IAT,STR,2,ILA)
      IAT=IAT+2*KPDSSZ
      ILA(3)=ILA(1)
      CALL GKUPI(IAT,STR,1,ILA)
      IAT=IAT+KPDISZ
      ILA(4)=ILA(1)
      CALL GKUPSI(IAT,STR,1,ILA)

      DO 5410 J=1,KNACWK
        IWKID=KWKID(KACPT(J))
        CALL GSFAR(IWKID,ILA(3),ILA(2),ILA(4),ILA(1))
 5410 CONTINUE
      GOTO 9925

* Pattern representation
 5500 CALL GKMPAR(NCD,STR)
      GOTO 9900

* Colour representation
 5600 CALL GKUPSI(IAT,STR,1,ILA)
      IAT=IAT+KPDSSZ
      CALL GKUPR(IAT,STR,3,RLA)

      DO 5610 J=1,KNACWK
        IWKID=KWKID(KACPT(J))
        CALL GSCR(IWKID,ILA(1),RLA(1),RLA(2),RLA(3))
 5610 CONTINUE
      GOTO 9925

* TRANSFORMATIONS
* ---------------
* Interpretation of a clipping rectangle item sets values in the GKS
* State List. Interpretation of other items in this class (WORKSTATION
* WINDOW and WORKSTATION VIEWPORT) causes the invocation of the
* corresponding GKS functions on all active workstations.

 6000 CONTINUE
      IF(ITYPE.NE.61) GOTO 7000

* Clipping rectangle

      CALL GKPRLG(NAME(ITYPE),GGKOP,GSGOP)
      IF(KERROR.NE.0) GOTO 9900

      CALL GKUPR(IAT,STR,4,RLA)

      QCCLXL=RLA(1)
      QCCLXR=RLA(2)
      QCCLYB=RLA(3)
      QCCLYT=RLA(4)

      CALL GKTOLD

      GOTO 9900

 7000 CALL GKPRLG(NAME(ITYPE),GWSOP,GSGOP)
      IF(KERROR.NE.0) GOTO 9900

      IF(ITYPE.EQ.72) GOTO 7200

* Workstation viewport
      CALL GKUPR(IAT,STR,4,RLA)
      QWR1=RLA(1)
      QWR2=RLA(2)
      QWR3=RLA(3)
      QWR4=RLA(4)

      CALL GKSACW(KSWKWN,1,KDAT,1,QDAT,QDAT,1,CH)
      IF(KERROR.NE.0) GOTO 9900

      CALL GKRGN
      IF(KERROR.NE.0) GOTO 9900

      CALL GKTOLD

      GOTO 9900

* Workstation window
 7200 CONTINUE
      CALL GKUPR(IAT,STR,4,RLA)
      QWR1=RLA(1)
      QWR2=RLA(2)
      QWR3=RLA(3)
      QWR4=RLA(4)

      CALL GKSACW(KSWKVP,1,KDAT,1,QDAT,QDAT,1,CH)
      IF(KERROR.NE.0) GOTO 9950

      CALL GKRGN
      IF(KERROR.NE.0) GOTO 9900

      CALL GKTOLD

      GOTO 9900

* SEGMENT MANIPULATION
* --------------------
* Interpretation of items in this class has the same effect as
* invocation of the corresponding GKS functions. (Item 84 causes
* Invocation of DELETE SEGMENT).

 8000 CONTINUE
      GOTO(8100,8200,8300,8400)ITYPE-80

* Create segment
 8100 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      CALL GCRSG(ILA(1))
      GOTO 9925

* Close segment
 8200 CONTINUE
      CALL GCLSG
      GOTO 9925

* Rename segment
 8300 CONTINUE
      CALL GKUPSI(IAT,STR,2,ILA)
      CALL GRENSG(ILA(1),ILA(2))
      GOTO 9925

* Delete segment
 8400 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      CALL GDSG(ILA(1))
      GOTO 9925

* SEGMENT ATTRIBUTES
* ------------------
* Interpretation of items in this class has the same effect as
* invocation of the corresponding GKS functions.

 9000 CONTINUE

      GOTO(9100,9200,9300,9400,9500)ITYPE-90

* Set segment transformation
 9100 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      IAT=IAT+KPDSSZ
      CALL GKUPR(IAT,STR,6,RLA)
*     The matrix is returned from the metafile in row major order, so
*     we transpose it into column major order using TRLA.
      TRLA(1) = RLA(1)
      TRLA(2) = RLA(4)
      TRLA(3) = RLA(2)
      TRLA(4) = RLA(5)
      TRLA(5) = RLA(3)
      TRLA(6) = RLA(6)
      CALL GSSGT(ILA(1),TRLA)
      GOTO 9925

* Set visibility
 9200 CONTINUE
      CALL GKUPSI(IAT,STR,2,ILA)
      CALL GSVIS(ILA(1),ILA(2))
      GOTO 9925

* Set highlighting
 9300 CONTINUE
      CALL GKUPSI(IAT,STR,2,ILA)
      CALL GSHLIT(ILA(1),ILA(2))
      GOTO 9925

* Set segment priority
 9400 CONTINUE
      CALL GKUPSI(IAT,STR,1,ILA)
      IAT=IAT+KPDSSZ
      CALL GKUPR(IAT,STR,1,RLA)
      CALL GSSGP(ILA(1),RLA(1))
      GOTO 9925

* Set detectability
 9500 CONTINUE
      CALL GKUPSI(IAT,STR,2,ILA)

C     CALL GSDTEC(ILA(1),ILA(2))
      GOTO 9925

*  Error check for 'internal' functions
 9900 IF(KERROR.NE.0) CALL GKERR(KERROR)

*  If an error is detected while interpreting an item, the message
*  is reported using the corresponding error handler routine name.
*  Here send a second message to indicate that the error occurred
*  during metafile interpretation.
 9925 CONTINUE
      IF(KERROR.NE.0) THEN
        CALL GKPRLG(EIITM,GGKOP,GSGOP)
        KERROR = -1048
        CALL GKERR(KERROR)
      ENDIF
      GOTO 9999

 9950 CALL GKERR(KERROR)

 9999 RETURN
      END
