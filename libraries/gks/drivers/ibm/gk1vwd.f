C# IL>=a, OL>=0
      SUBROUTINE GK1VWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION DRIVER
*  Author:             MJD
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     VERSATEC and XEROX Metafile output workstation driver.
*     The alterations needed to convert this from VERSATEC
*     driver to VERSATEC and XEROX driver, were a couple
*     of alterarions to Inquiries.
*
*  MAINTENANCE LOG
*  ---------------
*     11/11/86  MJD  Original version stabilized
*     26/04/87  MJD  added code to entrypoint 210 to
*                    only write Fill AREA ASF's to the metafile
*                    if they have been changed - thus reducing the
*                    size of the metafile (especially in large ones).
*     06/05/87  MJD  Add IF statement to enrtypoint 1850, to
*                    decipher colour facilities.
*     15/08/87  RTP  cos version all machine
*     18/01/88  DSG  IWKS set but not used (KCLWK). Line removed.
*     18/01/88  DSG  IS binding change: GxxBDN changed to GxxATT.
*     18/01/88  DSG  GKYWDT included, as KWDT and KNONE are used.
*     04/08/88  KEVP Put in temporary instructions to obtain correct
*                    raster display dimensions (marked with +++)
*     05/08/88  KEVP Relabelled line label 1300 to 1111 (GKQWK).
*                    Put in new utility GKPXAD toinquire pixel
*                    array dimensions (entry point KQPXAD=120) (S339).
*     12/12/88  NMH  Fix inquire fill area facilities - label 1810 - to return
*                    the correct element of list of available fill area hatch
*                    style indices (S399).
*     12/12/88  NMH  Fix inquire colour facilities to include versatec
*                    driver type 1183 (S398).
*     27/02/89  DBG  Modified Set Workstation Viewport (label 330), to check
*                    for error condition 54.
*     09/05/89  DBG  Added KSSGPT(KWKIX)=KNIL to Clear Workstation (label 30)
*                    to clear up a bug related to using the same segment name
*                    more than once, deleting all segments by calling GCLRWK,
*                    RAL GKS Bug Report S400.  Error report issued
*                    because KWI2 was being returned as GABSNT when
*                    segment was actually present and GPRSNT when it was
*                    not.
*     11/05/89  DBG  Modified Delete Segment (label 440) to remove miss
*                    leading error report [123] when calling GDSGWK (S401).
*     02/10/89  RTP  Output GDP integer values before points
*     02/10/89  DSG  Set Pattern Rep. converted to 7.4
*     03/10/89  DSG  De-allocate heap space at Close Workstation (S260)
*     03/10/89  DSG  Error numbers 2002 changed to -2002
*     03/10/89  DSG  Begin and End Segment entry points removed - not
*                    relevant for this driver.
*     03/10/89  DSG   Set Representation code altered. Item written out
*                     and no errors returned, unless an addressing
*                     problem could occur.
*     03/10/89  DSG  Changed open wkstn entry to give up if GKIWSL
*                    returns a non-zero value of KERROR (S260).
*     03/10/89  DSG  Default deferral mode set to ASTI (Bug 283).
*     03/10/89  DSG  Workstation viewport limits raised (Bug 290).
*     03/10/89  DSG  Use enumerated types rather than integers for
*                    absent/present in delete segment entry (S299).
*     05/10/89  DSG  Removed the temporary code of 04/8/88, as
*                    the wdt files have been modified.
*     23/01/90  DSG  Error numbers changed 3/10/89 - revert back to
*                    2002 (list element or set member not available).
*     22/03/90  DSG  GDP, Pattern Size, Pattern Representation and
*                    User Item changed to conform to GKS 7.4 (!).
*     10/05/90  PLP  Removed the unused variables.
*     21/05/90  RMK  Added bug report numbers to earlier entries by
*                    NMH and DBG.
*     24/10/90  KEVP Rewrote inQuire Fill Area Facilities entry point
*                    to take account of the fact that some workstations
*                    do not support patterned fill (C34) and to ensure
*                    data is passed even if there is an error (C55,C29).
*     24/10/90  KEVP Ensured that inQuire TeXt Facilities entry point
*                    passes data even if there is an error (C55).
*     07/11/90  KEVP Removed temporary code, no longer needed,
*                    because of the fix to bug C18.
*     28/03/91  KEVP In inQuire Colour Facilities return 257 colours as
*                    available (not 256) including background (C12).
*
*  ARGUMENTS
*  ---------
*     INP   IENT   Entrypoint code
*     INP   NID    Size of array IDAT
*     INP   IDAT   Integer data passed to workstation
*     INP   NRD    Size of arrays RX and RY
*     INP   RX     Real X-coordinate data passed to workstation
*     INP   RY     Real Y-coordinate data passed to workstation
*
      INTEGER IENT,NID,IDAT(NID),NRD,NCD
      REAL RX(NRD), RY(NRD)
*
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/    Workstation index and type
*     Read   /ASPCT/     KLNTYA,KPLCIA,KMKTYA,KPMCIA,KTXFNA,
*                        KTXCIA,KFAISA,KFACIA..(PAR)
*     Modify /GKYWCA/    Set error and workstation category
*     Modify /GKYWSL/    Set up KCID in the state list
*     Modify /GKYWKD/    Derive workstation 'total' transform
*                        In KWKDAT(n,KWKIX) keep local copy of AFSs in elements
*                        1 to 13 and store buffer pointer in 14.
*     Modify /GKYERR/    KERROR
*     Modify /GKYWDT/    KWDT and KNONE.
*
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gwksgl.par'
      INCLUDE '../../include/gaspct.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkfls.cmn'
      INCLUDE '../../include/gkxfd.cmn'
      INCLUDE '../../include/gksl.cmn'
      INCLUDE '../../include/gkplb.cmn'
      INCLUDE '../../include/gkpmb.cmn'
      INCLUDE '../../include/gktxb.cmn'
      INCLUDE '../../include/gkfab.cmn'

*
*  EXTERNALS
*  ---------
      EXTERNAL GK1VXF

*  LOCALS
*  ------
*
      INTEGER ITEM
* Do Loop Variable
      INTEGER J, JENT, I
*
      INTEGER ISGNO
      REAL SGPRI
*
*     IPL   offset to aspect list for polyline
*     IPM   offset to aspect list for polymarker
*     ITX   offset to aspect list for text
*     IFA   offset to aspect list for fill area
*
*     RCDC  Conversion factor for DC to Raster Coords (temporary)
*
      INTEGER    IPL,  IPM,  ITX, IFA
      PARAMETER (IPL=0,IPM=3,ITX=6,IFA=10)
*
      INTEGER INTA(19)
      REAL REALA(12)
*
      REAL       RCDC
      PARAMETER (RCDC = 200.0/25.4)
*
*  ERRORS
*  ------

*    34   Workstation is not of category MI
*    37   Workstation is not of category OUTIN
*    38   Workstation is neither of category INPUT nor of
*         category OUTIN
*    54   Workstation viewport is not within the display space
*
*  COMMENTS
*  --------

* This part of the MO driver maps the GKS functionality, but
* has no knowledge of the metafile encoding except for the ITEM
* TYPE values.

*
*---------------------------------------------------------------------



* GOTO conditional on the entrypoint

      GOTO(  10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     :      110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     :      210, 220, 230, 240, 250, 260, 270, 280,9999,9999,
     :      310, 320, 330,9999,9999,9999,9999,9999,9999,9999,
     :      410, 420, 430, 440,9999,9999, 470, 480, 490, 500,
     :      510,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :     9938,9938,9938,9938,9937,9938,9938,9938,9938,9938,
     :     9938,9938,9937,9938,9938,9938,9938,9938,9937,9938,
     :     9938,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      910,9934,9934) IENT

      GOTO(1200,1111,9999,9999,9999,9999,9999,9999,9999,9999,
     :     1111,1111,1111,1111,1111,1111,1111,1370,1380,1111,
     :     1111,1111,1111,1111,1111,1111,1460,9938,9938,9938,
     :     9938,9937,9938,9999,9999,9999,9999,9999,9999,9999,
     :     9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :     1700,1710,1111,1111,1111,1111,1111,1111,1111,1790,
     :     1111,1810,1111,1111,1111,1850,1111,1870,1880,1111,
     :     1111,1111,9938,9938,9938,9938,9938,9938,9938) IENT-119

      GOTO 9999
*
* OPEN WORKSTATION
* ----------------
   10 CONTINUE
* Set up WSL and WDT
      IF (KERROR.NE.0) GOTO 9999
      CALL GKIWSL(KWKIX, KWKTYP)
      IF(KERROR.EQ.0) THEN
        KCID(KWKIX) = KWI1
        KDFM(KWKIX) = GASTI
        KWIO(KWKIX) = GNO
        KIMRGM(KWKIX) = GSUPPD
      END IF
      QWCHXP(KWKIX) = 1.0
      KWTXFN(KWKIX) = 1

* Ask operating system to make a connection.
      CALL GKIOOP(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
*
      IF(KERROR.EQ.0) THEN

* Initialise the output buffering
        CALL GK1VBU(1,' ')

* Write the GKSM file header
        CALL GK1VFH

C??????
* Output the control item 'clear workstation (conditionally)'
        ITEM=1
        KWI2=0
        CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
C??????

*
        KSSGPT(KWKIX)=KNIL

* Set derived ASF's to bundled, and output ASF item
      DO 15 J=1,13
        KWKDAT(J,KWKIX) = GBUNDL
   15 CONTINUE
      ITEM = 43
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF

* Workstation category (WDT) embedded
      KWI1=GOUTPT

      GOTO 9999

*
* CLOSE WORKSTATION
* -----------------
   20 CONTINUE
      IF(KWI1.EQ.1) THEN
        KWDONE=KRFUSE
      ELSE
        ITEM=0
        CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
        CALL GK1VBU(3,' ')

* Ask the operating system to break the connection.
        CALL GKIOCL(KFWKFW,KCID(KWKIX),KWCID(KWKIX))
* Close GKS data structures for this workstation
        CALL GKCWSL(KWKIX)
      ENDIF
*                                                                             *
*******************************************************************************
      GOTO 9999
*
* CLEAR WORKSTATION
* -----------------
   30 CONTINUE
      IF(KWI1.EQ.1) GOTO 9999
      KWDONE = KACEPT
      KSSGPT(KWKIX) = KNIL
      ITEM = 1
      GOTO 9994
*
* REDRAW ALL SEGMENTS
* -------------------
   40 CONTINUE
      KWDONE=KACEPT
      ITEM=2
      GOTO 9994
*
* UPDATE WORKSTATION
* ------------------
   50 CONTINUE
      KWDONE=KACEPT
      ITEM=3
      GOTO 9994
*
* SET DEFERRAL STATE
* ------------------
   60 CONTINUE
      KWDONE=KACEPT
      ITEM=4
      GOTO 9994
*
* DO DEFERRED OUTPUT ACTIONS
* --------------------------
   70 CONTINUE
      GOTO 9999
*
* CLEAR DISPLAY SURFACE
* ---------------------
   80 CONTINUE
      GOTO 9999
*
* REDRAW ONE SEGMENT
* ------------------
   90 CONTINUE
      GOTO 9999
*
* MESSAGE
* -------
  100 CONTINUE
      ITEM=5
      CALL GK1VIT(ITEM,NID,IDAT,1,QDAT,QDAT,NCD,STR)
      GOTO 9999
*
* ESCAPE
* ------
  110 CONTINUE
      ITEM=6
      CALL GK1VIT(ITEM,NID,IDAT,1,QDAT,QDAT,NCD,STR)
      GOTO 9999
*
* POLYLINE
* --------
  120 CONTINUE
      ITEM=11
      CALL GK1VIT(ITEM,1,KDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999
*
* POLYMARKER
* ----------
  130 CONTINUE
      ITEM=12
      CALL GK1VIT(ITEM,1,KDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999
*
* TEXT
* ----
  140 CONTINUE
      ITEM=13
      CALL GK1VIT(ITEM,NID,IDAT,1,QDAT,QDAT,NCD,STR)
      GOTO 9999
*
* FILL AREA
* ---------
  150 CONTINUE
      ITEM=14
      CALL GK1VIT(ITEM,1,KDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999
*
* CELL ARRAY
* ----------
  160 CONTINUE
      ITEM=15
      CALL GK1VIT(ITEM,NID,IDAT,1,QDAT,QDAT,NCD,STR)
      GOTO 9999
*
* GDP
* ---
  170 CONTINUE
      ITEM=16
      CALL GK1VIT(ITEM,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999
*
* SET POLYLINE ATTRIBUTES
* -----------------------
  180 CONTINUE
* Aspect source flags
* Data expected: KIPLAF(1) -> (3)     Polyline ASF's
*
      CALL GKDPLB
      DO 182 J = KLNTYA,KPLCIA
        IF(KWKDAT(J+IPL,KWKIX).NE.KIPLAF(J)) GOTO 183
  182 CONTINUE
      GOTO 187
  183 DO 185 J=KLNTYA,KPLCIA
        KWKDAT(J+IPL,KWKIX)=KIPLAF(J)
  185 CONTINUE
      ITEM=43
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Polyline index
  187 ITEM=21
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Linetype
      IF(KIPLAF(KLNTYA).EQ.GINDIV) THEN
      ITEM=22
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Linewidth scale factor
      IF(KIPLAF(KLNWDA).EQ.GINDIV) THEN
      ITEM=23
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Polyline colour index
      IF (KIPLAF(KPLCIA) .NE. GINDIV) GOTO 9999
      ITEM = 24
      GOTO 9994
*
* SET POLYMARKER ATTRIBUTES
* -------------------------
  190 CONTINUE
* Aspect source flags
* Data expected: KIPMAF(1) -> (3)     Polymarker ASF's
*
      CALL GKDPMB
      DO 192 J = KMKTYA,KPMCIA
        IF(KWKDAT(J+IPM,KWKIX).NE.KIPMAF(J)) GOTO 193
  192 CONTINUE
      GOTO 197
  193 DO 195 J=KMKTYA,KPMCIA
        KWKDAT(J+IPM,KWKIX)=KIPMAF(J)
  195 CONTINUE
      ITEM=43
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Polymarker index
  197 ITEM=25
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Marker type
      IF(KIPMAF(KMKTYA).EQ.GINDIV) THEN
      ITEM=26
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Marker size scale factor
      IF(KIPMAF(KMKSZA).EQ.GINDIV) THEN
      ITEM=27
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Polymarker colour index
      IF(KIPMAF(KPMCIA).NE.GINDIV) GOTO 9999
      ITEM=28
      GOTO 9994
*
* SET TEXT ATTRIBUTES
* -------------------
  200 CONTINUE
* Aspect source flags
* Data expected: KITXAF(1) -> (4)     Text ASF's
*
      CALL GKDTXB
      DO 202 J = KTXFNA,KTXCIA
        IF(KWKDAT(J+ITX,KWKIX).NE.KITXAF(J)) GOTO 203
  202 CONTINUE
      GOTO 207
  203 DO 205 J=KTXFNA,KTXCIA
        KWKDAT(J+ITX,KWKIX)=KITXAF(J)
  205 CONTINUE
      ITEM=43
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Text index
  207 ITEM=29
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Text font and precision
      IF(KITXAF(KTXFNA).EQ.GINDIV) THEN
      ITEM=30
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Character expansion factor
      IF(KITXAF(KCHXPA).EQ.GINDIV) THEN
      ITEM=31
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Character spacing
      IF(KITXAF(KCHSPA).EQ.GINDIV) THEN
      ITEM=32
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Text colour index
      IF(KITXAF(KTXCIA).EQ.GINDIV) THEN
      ITEM=33
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Charactor vectors
      ITEM=34
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Text path
      ITEM=35
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Text alignment
      ITEM=36
      GOTO 9994
*
* SET FILL AREA ATTRIBUTES
* ------------------------
  210 CONTINUE
* Aspect source flags
* Data expected: KIFAAF(1) -> (3)     Fill Area ASF's
*
      CALL GKDFAB
      DO 212 J = KFAISA,KFACIA
        IF(KWKDAT(J+IFA,KWKIX).NE.KIFAAF(J)) GOTO 213
  212 CONTINUE
      GOTO 217
  213 DO 215 J=KFAISA,KFACIA
        KWKDAT(J+IFA,KWKIX)=KIFAAF(J)
  215 CONTINUE
      ITEM=43
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Fill area index
  217 CONTINUE
      ITEM=37
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Fill area interior style
      IF (KIFAAF(KFAISA).EQ.GINDIV) THEN
      ITEM=38
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Fill area style index
      IF(KIFAAF(KFASIA).EQ.GINDIV)THEN
      ITEM=39
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Fill area colour index
      IF(KIFAAF(KFACIA).EQ.GINDIV)THEN
      ITEM=40
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
      END IF
* Pattern size
      ITEM=41
      CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)
* Pattern reference point
      ITEM=42
      GOTO 9994
*
* SET PICK IDENTIFIER
* -------------------
  220 CONTINUE
      ITEM=44
      GOTO 9994
*
* SET POLYLINE REPRESENTATION
* ---------------------------
  230 CONTINUE
* Find out if index exists or there is room for another bundle
      DO 231 JENT=1,KMXPLB
        IF(KWI1.EQ.KPLI(JENT,KWKIX) .OR. KPLI(JENT,KWKIX).EQ.KNIL)
     :  GOTO 232
  231 CONTINUE
* No room
      KERROR = -1004
      GOTO 9999
  232 CONTINUE
* Now set representation
      KPLI(JENT,KWKIX) = KWI1
      KLNTY(JENT,KWKIX) = KWI2
      QLNWD(JENT,KWKIX) = QWR1
      KPLCI(JENT,KWKIX) = KWI3

      ITEM=51
      GOTO 9994
*
* SET POLYMARKER REPRESENTATION
* -----------------------------
  240 CONTINUE
* Find out if index exists or there is room for another bundle
      DO 241 JENT=1,KMXPMB
        IF(KWI1.EQ.KPMI(JENT,KWKIX) .OR. KPMI(JENT,KWKIX).EQ.KNIL)
     :    GOTO 242
  241 CONTINUE
* No room
      KERROR = -1004
      GOTO 9999
* Now set representation
  242 CONTINUE
      KPMI(JENT,KWKIX) = KWI1
      KMKTY(JENT,KWKIX) = KWI2
      QMKSZ(JENT,KWKIX) = QWR1
      KPMCI(JENT,KWKIX) = KWI3

      GOTO 9994
*
* SET TEXT REPRESENTATION
* -----------------------
  250 CONTINUE
* Find out if index exists or there is room for another bundle
      DO 251 JENT=1,KMXTXB
        IF(KWI1.EQ.KTXI(JENT,KWKIX) .OR. KTXI(JENT,KWKIX).EQ.KNIL)
     :    GOTO 252
  251 CONTINUE
* No room
      KERROR = -1004
      GOTO 9999
* Now set representation
  252 CONTINUE
      KTXI(JENT,KWKIX) = KWI1
      KTXFN(JENT,KWKIX) = KWI2
      KTXPR(JENT,KWKIX) = KWI3
      QCHXP(JENT,KWKIX) = QWR1
      QCHSP(JENT,KWKIX) = QWR2
      KTXCI(JENT,KWKIX) = KWI4

      ITEM=53
      GOTO 9994
*
* SET FILL AREA REPRESENTATION
* ----------------------------
  260 CONTINUE
* Find out if index exists or there is room for another bundle
      DO 261 JENT=1,KMXFAB
        IF(KWI1.EQ.KFAI(JENT,KWKIX) .OR. KFAI(JENT,KWKIX).EQ.KNIL)
     :    GOTO 262
  261 CONTINUE
* No room
      KERROR = -1004
      GOTO 9999
* Now set representation
  262 CONTINUE
      KFAI(JENT,KWKIX) = KWI1
      KIS(JENT,KWKIX) = KWI2
      KSI(JENT,KWKIX) = KWI3
      KFACI(JENT,KWKIX) = KWI4

      ITEM=54
      GOTO 9994
*
* SET PATTERN REPRESENTATION
* --------------------------
  270 CONTINUE
* Check if colour indices valid
      DO 272 J=KWI5, KWI5+KWI7-1
        DO 271 I=KWI4, KWI4+KWI6-1
          IF (IDAT((J-1)*KWI2+I).LT.0 .OR.
     :        IDAT((J-1)*KWI2+I).GT.KPCI(KWKIX)-1) THEN
            KERROR = 93
            GOTO 9999
          ENDIF
  271   CONTINUE
  272 CONTINUE

* Find out if index exists
      CALL GKDRGE(KPABPT(KWKIX),KWI1,3,0,INTA,REALA)
      IF (KERROR.EQ.0) THEN
* If it does, deallocate heap for pattern
        CALL GKHPDA(INTA(3),KINTGS)
      ELSE
* If it doesn't, directory will automatically extend
        KERROR = 0
      ENDIF

      INTA(1) = KWI6
      INTA(2) = KWI7
* Grab heap space for new pattern
      CALL GKHPAL(KWI6*KWI7,KINTGS,INTA(3))
      IF (KERROR.NE.0) GOTO 9999

* Fill directory entry for this pattern
      CALL GKDRPU(KPABPT(KWKIX),KWI1,3,0,INTA,REALA)
      IF (KERROR.NE.0) GOTO 9999

* Copy pattern into heap
      DO 274 I=KWI5, KWI5+KWI7-1
        CALL GKHPPI(INTA(3),(I-KWI5)*KWI6,KWI6,IDAT((I-1)*KWI2+KWI4))
  274 CONTINUE

      ITEM=55
      CALL GK1VIT(ITEM,NID,IDAT,1,QDAT,QDAT,NCD,STR)
      GOTO 9999
*
* SET COLOUR REPRESENTATION
* -------------------------
  280 CONTINUE
* See if colour index valid
      IF (KWI1.GE.0 .AND. KWI1.LT.KPCI(KWKIX)) THEN
        QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1) = QWR1
        QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1) = QWR2
        QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1) = QWR3
      ELSE
        KERROR = 93
      ENDIF
      ITEM=56
      GOTO 9994
*
* NORM TRANS
* ----------
*Data expected: QWR1,...,6     Transformation C2

  310 CONTINUE

* Derive 'workstation total transformation'. (For workstations
* of category MO this is the same as the normalisation
* transformation).
      QWTOTT(1,KWKIX)=QWR1
      QWTOTT(2,KWKIX)=QWR2
      QWTOTT(3,KWKIX)=QWR3
      QWTOTT(4,KWKIX)=QWR4
      QWTOTT(5,KWKIX)=QWR5
      QWTOTT(6,KWKIX)=QWR6

*Send clipping rectangle
      ITEM=61
      GOTO 9994
*
* SET WORKSTATION WINDOW
* ----------------------
  320 CONTINUE
* Store the requested and current values in the WSL
      QRWWXL(KWKIX) = QWR1
      QRWWXR(KWKIX) = QWR2
      QRWWYB(KWKIX) = QWR3
      QRWWYT(KWKIX) = QWR4
      QCWWXL(KWKIX) = QWR1
      QCWWXR(KWKIX) = QWR2
      QCWWYB(KWKIX) = QWR3
      QCWWYT(KWKIX) = QWR4
      ITEM=71
      GOTO 9994
*
* SET WORKSATION VIEWPORT
* -----------------------
  330 CONTINUE

      IF (QWR1.LT.0.0 .OR. QWR2.GT.QDSDX(KWKIX).OR.
     :    QWR3.LT.0.0 .OR. QWR4.GT.QDSDY(KWKIX)) THEN
             KERROR = 54
             GOTO 9999
      ENDIF

* Store the requested and current values in the WSL
      QRWVXL(KWKIX) = QWR1
      QRWVXR(KWKIX) = QWR2
      QRWVYB(KWKIX) = QWR3
      QRWVYT(KWKIX) = QWR4
      QCWVXL(KWKIX) = QWR1
      QCWVXR(KWKIX) = QWR2
      QCWVYB(KWKIX) = QWR3
      QCWVYT(KWKIX) = QWR4
      ITEM=72
      GOTO 9994
*
* CREATE SEGMENT
* --------------
  410 CONTINUE
      CALL GKSGWK(IENT,.FALSE.)
      KSGRQ=KSGRQ-1
      ITEM=81
      GOTO 9994
*
* CLOSE SEGMENT
* -------------
  420 CONTINUE
      ITEM=82
      GOTO 9994
*
* RENAME SEGMENT
* --------------
  430 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF(KWI1.EQ.ISGNO) THEN
        CALL GKSLNM(KSSGPT(KWKIX),KWI1,KWI2)
      ENDIF

      ITEM=83
      GOTO 9994
*
* DELETE SEGMENT
* --------------
  440 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF(KWI1.EQ.ISGNO) THEN
        KWI2=GPRSNT
        CALL GKSLDS(KSSGPT(KWKIX),KCURR)
      ELSE
        KWI2=GABSNT
      ENDIF

      ITEM=84
      GOTO 9994
*
* SET SEGMENT TRANSFORMATION
* --------------------------
  470 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF((KWI1.EQ.ISGNO).AND.(KWI5.EQ.2)) THEN
        ITEM = 91
        GOTO 9994
      ELSE
        GOTO 9999
      ENDIF
*
* SET VISIBILITY
* --------------
  480 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF(KWI1.NE.ISGNO) GOTO 9999
      ITEM = 92
      GOTO 9994
*
* SET HIGHLIGHTING
* ----------------
  490 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF(KWI1.NE.ISGNO) GOTO 9999
      ITEM = 93
      GOTO 9994
*
* SET SEGMENT PRIORITY
* --------------------
  500 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF(KWI1.NE.ISGNO) GOTO 9999
      ITEM = 94
      GOTO 9994
*
* SET DETECTABILITY
* -----------------
  510 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SGPRI,QDAT)
      IF(KWI1.NE.ISGNO) GOTO 9999
      ITEM = 95
      GOTO 9994
*
* WRITE ITEM TO GKSM
* ------------------
* The item type supplied is in KWI1, and it is be tested in
* GK1VIT to see if it has a value that is valid for a user
* item in this metafile. In order that GK1VIT knows that it
* has a user item, ITEM will be set here temporarily.
  910 CONTINUE
      ITEM=101
      CALL GK1VIT(ITEM,NID,IDAT,1,QDAT,QDAT,NCD,STR)
      GOTO 9999
*
* INQUIRE EVERYTHING
* ------------------
 1111 CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999
*
* --------------------------------------------------------------
* Inquire pixel array dimensions
* --------------------------------------------------------------
 1200 CONTINUE
*     The utilitity GKQWK can't be used because GKTWD doesnot
*     convert World Coordinates to Raster Coordinates (but to NDC)
      CALL GKPXAD
      GOTO 9999
*
* --------------------------------------------------------------
* Inquire text representation
* --------------------------------------------------------------
 1370 CONTINUE
*     Input data :
*     KWI1   : text index
*     KWI2   : type of returned value ( GSET or GREALIzed )
*
*     Data returned:
*     KERROR : error indicator
*     KWI1   : text font
*     KWI2   : text precision
*     KWI3   : text colour index
*     QWR1   : character expansion factor
*     QWR2   : character spacing


      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      IF(KWI3.GE.KPCI(KWKIX)) KWI3 = 1
      GOTO 9999

* --------------------------------------------------------------
* Inquire text extent
* --------------------------------------------------------------
 1380 CONTINUE
*     Input data :
*     NID    : length of string
*     IDAT   : integer array character codes for string
*     QWR1   : x-text position
*     QWR2   : y-text position
*     QWR3   : x-width vector
*     QWR4   : y-width vector
*     QWR5   : x-height vector
*     QWR6   : y-height vector
*     QWCHRX(KWKIX),QWCHRY(KWKIX) : baseline vector
*
*     Data returned:
*     KERROR : error indicator
*     RX(1-4): x-text extent
*     RY(1-4): y-text extent
*     QWR7   : x-concatenation point
*     QWR8   : y-concatenation point

* the Xeox & Versatec only use the Hershey font's from the font
* data base GKSFNT RANFNT12 - but the 4250 uses it's own hardware
* font to replace the Hershey Font number 1. So, with the Xerox &
* Versatec all tesxxt is drawn using Strokr precision no matter
* what precision is specified, whereas the 4250 uses CHAR precision
* for font 1 .... so GKXQXC has to be called if an 4250 w/s is
* being driven. GKXQXC calls GK1VXF whix holds the Width & height
* of the actual 4250 Hardware font ! The 4250 driver at HARDCOPY end
* is an awkward driver !
*
*     stroke precision
      IF (KWTXPR(KWKIX) .EQ. GSTRKP) THEN
        CALL GKXQXO(NID,IDAT,RX,RY)
*     string and char precision
      ELSE
         IF (KWKTYP .LE. 1484 .AND. KWKTYP .GE. 1480) THEN
*
* The font index is set to the Rogue value of 999 because in
* GKXQXC  if the font index is one of the Hershey font indexes
* eg 1, 102, 103 etc .. then the font details are taken from the
* font database OTHERWISE the font details are taken from the user
* written routine GK**XF which supplies the HARDWARE font details.
* As we need the OTHERWISE part a rogue value is set and then reset
* after the call to GKXQXC.
* The y con-catenation point is incremented because the 4250's font
* has a lower base line than other fonts ... yeah what a machine !
*
            KWTXFN (KWKIX) = 999
            CALL GKXQXC (NID, IDAT, QWCHRX(KWKIX), QWCHRY (KWKIX),
     :                      RX, RY, GK1VXF)
            QWR8 = QWR8 + 0.0097
            KWTXFN (KWKIX) = 1
         ELSE
            CALL GKXQXO (NID, IDAT, RX, RY)
         END IF

      ENDIF
      GOTO 9999

* --------------------------------------------------------------
* Inquire Set Members of Segment Names on Workstation
* --------------------------------------------------------------
 1460 CONTINUE
*     KWI1    : list element requested
*
*     Data returned:
*     KERROR : error indicator
*     KWI1   : no of segment names
*     KWI2   : Nth member of set of stored segments for this
*                workstation
*
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*     KSGRQ = KSGRQ + 1
      GOTO 9999

* ---------------------------------------------------------------[D
* INQUIRE WORKSTATION CATEGORY
* -------------------------------------------------------------
 1700 CONTINUE
      KWI1=GOUTPT
      GOTO 9999

* --------------------------------------------------------------
* Inquire workstation classification
* --------------------------------------------------------------
 1710 CONTINUE
*     Data returned:
*     KWI1   : Workstation Classification

      KWI1 = GVECTR
      GOTO 9999


* Inquire text facilities ... on entry KWI1 specifies list element reque
* Allow for string and char precision font (number 1) explicitly
 1790 CONTINUE
      KWI6 = 6
      IF(( KWI1.LT.0) .OR. ( KWI1.GT.KFNTMX+2 )) THEN
        KERROR= 2002
      ELSEIF(KWI1.GT.KFNTMX) THEN

*       String or Char precision font
          IF( KWI1.EQ.KFNTMX+1 ) KWI3 = GSTRP
          IF( KWI1.EQ.KFNTMX+2 ) KWI3 = GCHARP
          KWI2 = 1
      ELSEIF(KWI1 .NE. 0) THEN

*       Stroke precision font
*       Make sure that fonts are available
        IF( KDBFLS.EQ.KFLNA ) THEN
           KERROR=-1009
           GOTO 9999
        ENDIF
        IF( KDBFLS.EQ.KFLCL ) CALL GKXON
        IF( KERROR.NE.0 ) GOTO 9999
        KWI2 = KHFONT(KWI1)
        KWI3 = 2
      ENDIF
*
      KWI1=KFNTMX+2
      IF (KWKIX.NE.KNIL) THEN
        KWI4 = KCHH(KWKIX)
        KWI5 = KCHXPF(KWKIX)
        KWI6 = KPTXI(KWKIX)
        QWR1 = QMNCHH(KWKIX)
        QWR2 = QMXCHH(KWKIX)
        QWR3 = QMNCHX(KWKIX)
        QWR4 = QMXCHX(KWKIX)
      ELSE
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI4 = INTA(5)
          KWI5 = INTA(6)
          KWI6 = INTA(10)
          QWR1 = REALA(9)
          QWR2 = REALA(10)
          QWR3 = REALA(11)
          QWR4 = REALA(12)
        ENDIF
      ENDIF
      KWI6 = 6
      GOTO 9999

* ---------------------------------------------------------------
* Inquire Fill Area facilities
* ---------------------------------------------------------------
 1810 CONTINUE
*     Data sent:
*     KWI1   : list element of interior styles requested
*     KWI2   : list element of hatch styles requested
*     Data returned:
*     KERROR : error indicator
*     KWI1   : number of available fill area interior styles
*     KWI2   : Nth element of list of available fill area interior styles
*     KWI3   : number of available hatch styles
*     KWI4   : Nth element of list of available hatch styles
*     KWI5   : number of predefined fill area indices

*     If Versatec call standard utility
      IF((1100 .LE. KWKTYP) .AND. (KWKTYP .LE. 1199))THEN
         GOTO 1111
      ELSE
*     Workstation does not support patterned fill
*       Call standard inquiry with list element shifted,
*       if necessary,  to exclude patterned fill.
        IF(KWI1 .GE. 3) KWI1 = KWI1+1
        CALL GKQWK (IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*       Reset number of avaible fill-styles to exclude patterned fill.
        KWI1 = 3
      ENDIF
      GOTO 9999

* ---------------------------------------------------------------
* Inquire Colour facilities
* ---------------------------------------------------------------
 1850 CONTINUE
*     Data returned:
*     KERROR : error indicator
*     KWI1   : number of available colours
*     KWI2   : colour available
*     KWI3   : number of predefined colour indices

*  If being accessed as Versatec driver then GCOLOR else
*  Xerox is GMONOC.
      IF (KWKTYP .LE. 1183  .AND. KWKTYP .GE. 1160) THEN
         KWI1 = 257
         KWI2 = GCOLOR
         KWI3 = KWI1
      ELSE
         KWI1 = 2
         KWI2 = GMONOC
         KWI3 = KWI1
      END IF
      GOTO 9999
* ---------------------------------------------------------------
* Inquire List Element of Available GDPs
* ---------------------------------------------------------------
 1870 CONTINUE
*     Input data :
*     KWI1    : list element requested
*
*     Data returned:
*     KERROR : error indicator
*     KWI1   : no of available Generalised Drawing Primitives
*     KWI2   : Nth element of list of available GDP's

      IF ( KWI1.GE.1 .AND. KWI1.LE.4 ) THEN
        KWI2 = -KWI1
      ELSE
        KERROR = 2002
        KWI2 = KNIL
      ENDIF
      KWI1 = 4
      GOTO 9999

* ---------------------------------------------------------------
* Inquire Generalised Drawing Primitive
* ---------------------------------------------------------------
 1880 CONTINUE
*     Input data :
*     KWI1    : GDP identifier
*
*     Data returned ;
*     KERROR  : error indicator
*     IDAT    : number of sets of attributes used

      KNIR = 0
      IDAT(1) = KNIL
      IDAT(2) = KNIL
      IDAT(3) = KNIL
      IDAT(4) = KNIL
      IF (KWI1 .LE. -1 .AND. KWI1 .GE. -4) THEN
         KNIR = 1
         IDAT(1) = GFAATT
*                 Arc
         IF (KWI1 .EQ. -1) IDAT(1) = GPLATT
      ELSE
         KERROR = 41
      ENDIF
      GOTO 9999


* Workstation is not of category MI
 9934 KERROR=34
      GOTO 9999

* Workstation is not of category OUTIN
 9937 KERROR=37
      GOTO 9999

* Workstation is neither of category INPUT nor of category
* OUTIN
 9938 KERROR=38
      GOTO 9999


 9994 CALL GK1VIT(ITEM,1,KDAT,1,QDAT,QDAT,NCD,STR)

 9999 RETURN
      END
*
*
*
*
*
      SUBROUTINE GK1VFH
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION
*  Author:             DSG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Constructs SGKSM file header.
*
*  MAINTENANCE LOG
*  ---------------
*     11/03/83  DSG   Original version stabilized
*     19/06/86  RMK   Maintenance log incorporated into main driver routine.
*
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKMC/    CVERS
*
      INCLUDE '../../include/gkmc.par'
*
*  LOCALS
*  ------
      INTEGER IHFLD,ITFLD,LFIELD,IRFLD,IFIELD,IFORMT,IRI,
     :        IZERO,IONE,IFUNC,IDAY,MONTH,IYEAR
      CHARACTER STRING*90, SGKSM*5, SNAME*39, SDATE*8, SVERSN*2,
     : SLASH
      PARAMETER (SGKSM='GKSM ', SVERSN='01', SLASH='/')
      PARAMETER (IHFLD=1,ITFLD=3,LFIELD=7,IFIELD=4,IRFLD=9)
      PARAMETER (IFORMT=1,IRI=1,IZERO=0,IONE=1)
*
*  COMMENTS
*  --------
*     The format of the file header described in Annexe E is
*     adopted. GIN 66 gives the reasons for the values adopted
*     for the field lengths and number representation.
*     The field lengths must be the same as those used in
*     routine GK1VIT, so should they be passed in COMMON?
*
*---------------------------------------------------------------------

* Pick up system name title from GKMC PARLIB - only want 39 characters
      SNAME = CVERS

* Obtain the date from the system
      CALL GKDATE(IYEAR,MONTH,IDAY)

* Format it for the metafile header
      WRITE(SDATE,'(I2,2(A1,I2))') IYEAR,SLASH,MONTH,SLASH,IDAY

* Format the whole header
      WRITE(STRING,1000) SGKSM,SNAME,SDATE,SVERSN,IHFLD,ITFLD,
     :  LFIELD,IFIELD,IRFLD,IFORMT,IRI,IZERO,IONE
 1000 FORMAT(A5,A39,A8,A2,5I2,2I2,2I11)

* Send it
      IFUNC=2
      CALL GK1VBU(IFUNC,STRING)

      RETURN
      END
*
*
*
*
*
      SUBROUTINE GK1VIT(ITEM,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION
*  Author:             DSG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Constructs GKSM data items
*
*  MAINTENANCE LOG
*  ---------------
*     01/03/83  DSG  Original version stabilized
*     19/06/86  RMK  Maintenance log incorporated into main driver routine
*
*  ARGUMENTS
*  ---------
*     INP   ITEM   Item identification number
*     INP   NID    Size of array IDAT
*     INP   IDAT   Integer data passed to or from workstation
*     INP   NRD    Size of arrays RX and RY
*     INP   RX     Real X-coordinate data passed to workstation
*     INP   RY     Real Y-coordinate data passed to workstation
*     INP   NCD    Length of character array
*     INP   STR    Character array
*
      INTEGER ITEM,NID,IDAT(NID),NRD,NCD
      REAL RX(NRD),RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/  Interface variables
*     Modify /GKYWKD/  Real workspace used for NDC values
*     Modify /GKYSTK/  Stack
*     Modify /GKYERR/  Report stack overflow
*
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
* Size of integer or real stack space requested
      INTEGER ICHUNK
      PARAMETER (ICHUNK=200)
*
* Metafile field lengths
      INTEGER IHFLD,ITFLD,LFIELD,IFIELD,IRFLD
      PARAMETER (IHFLD=1,ITFLD=3,LFIELD=7,IFIELD=4,IRFLD=9)
*
******************************************************
* Arrays to hold world x- and y- coordinates before transformation
      REAL WCX(3), WCY(3)
*
      CHARACTER SGKSM*4
*
      CHARACTER*17 SFMT(21)
*
      INTEGER LENDAT,IFUNC,NPTS,NTOK,
     :        I,J,K,M,N,ISETR,IL,IRL
*
* STACK USAGE
* -----------
*
*  ERRORS
*  ------
*
*      160   Item type is not allowed for user items
*      161   Item length is invalid
*
*  COMMENTS
*  --------

* This part of the MO driver deals with the actual encoding of
* the metafile.
*
* The ESCAPE and GDP data records contain no data.
* This decision was taken after the code was written
* so the relevant lines have simply been comment out
* and the lengths of the integer and real data in these
* data records have been set to zero.
*
*---------------------------------------------------------------------


* Check user item data record length supplied
      IF(ITEM.EQ.101.AND.NCD.LT.1) THEN
        KERROR=161
        GOTO 999
      ENDIF

* Check user item type supplied
      IF(ITEM.EQ.101.AND.(KWI1.LE.100.OR.KWI1.GT.999)) THEN
        KERROR=160
        GOTO 999
      ELSE IF(ITEM.EQ.101) THEN
        ITEM=KWI1
      ENDIF

* End of error checking
* ---------------------

* Output the GKSM field and the Item Type field of the item
* header. The GKSM field can contain from 0 to 4 of the
* characters 'GKSM'.

      SGKSM = 'GKSM'

      IF(IHFLD.EQ.0) THEN
        WRITE(CSTR(1),'(I3)') ITEM
      ELSE
        WRITE(CSTR(1),'(A,I3)') SGKSM(:IHFLD),ITEM
      ENDIF
      IFUNC=2
      CALL GK1VBU(IFUNC,CSTR(1)(:IHFLD+ITFLD))

* Construct every type of format specification which will be
*  used in the internal WRITE statements.
      SFMT(1)='(I7)'
      SFMT(2)='(I7,I4)'
      SFMT(3)='(I7,2I4)'
      SFMT(4)='(I7,3I4)'
      SFMT(5)='(I7,4I4)'
      SFMT(6)='(I7,13I4)'
      SFMT(7)='(I7,F9.6)'
      SFMT(8)='(I7,2F9.6)'
      SFMT(9)='(I7,4F9.6)'
      SFMT(10)='(I7,I4,F9.6)'
      SFMT(11)='(I7,I4,3F9.6)'
      SFMT(12)='(I7,I4,6F9.6)'
      SFMT(13)='(I7,2F9.6,I4)'
      SFMT(14)='(I7,6F9.6,2I4)'
      SFMT(15)='(I7,2I4,F9.6,I4)'
      SFMT(16)='(I7,3I4,2F9.6,I4)'
      SFMT(19)='(112F9.6)'
      SFMT(18)='(2I4)'
      SFMT(20)='(16(16I4))'
      SFMT(21)='(113F9.6)'

* Conditional goto on item code

      IF(ITEM.EQ.0) GOTO 5
      GOTO (10,20,30,40,50,60,998,998,998,998,110,110,130,
     :      110,150,160,998,998,998,998,210,220,230,240,250,
     :      260,270,280,290,300,310,320,330,340,350,360,370,
     :      380,390,400,410,420,430,440,998,998,998,998,998,
     :      998,510,520,530,540,550,560,998,998,998,998,610,
     :      998,998,998,998,998,998,998,998,998,710,720,998,
     :      998,998,998,998,998,998,998,810,820,830,840,998,
     :      998,998,998,998,998,910,920,930,940,950) ITEM
      IF (ITEM.GT.100) GOTO 1010

* END ITEM
* ---------
* Data expected: NONE

    5 LENDAT=0
      WRITE(CSTR(1),SFMT(1)) LENDAT
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD))
      GOTO 999

* CLEAR WORKSTATION
* -----------------
* Data expected:
* KWI2 - CLEARING CONTROL FLAG

   10 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KWI2
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* REDRAW ALL SEGMENTS ON WORKSTATION
* ----------------------------------
* (LEVEL 1a)
* Data expected: NONE

   20 LENDAT=0
      WRITE(CSTR(1),SFMT(1)) LENDAT
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD))
      GOTO 999

* UPDATE WORKSTATION
* ------------------
* Data expected:
* KWI1 - REGENERATION FLAG

   30 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KWI1
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* DEFERRAL STATE
* --------------
* (LEVEL1a)
* Data expected:
* KWI1 - DEFFERAL MODE
* KWI2 - IMPLICIT REGENERATION FLAG

   40 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KWI1,KWI2
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* MESSAGE
* -------
* (LEVEL 1a)
* Data expected:
* IDAT(NID) - MESSAGE

   50 LENDAT=IFIELD+NID
      WRITE(CSTR(1),SFMT(2)) LENDAT,NID
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))

* Convert from Ascii integers to characters and send the string
      N=KWCST
      DO 55 I=1,NID,KWCST
        IF(NID-I.LT.KWCST) N=NID-I+1
        CALL GKATON (N, IDAT(I), CSTR(1))
        CALL GK1VBU(IFUNC,CSTR(1)(:N))
   55 CONTINUE

      GOTO 999

* ESCAPE
* ------
* Data expected:
* KWI1 - ESCAPE FUNCTION IDENTIFIER
* STR(NCD) - ESCAPE DATA RECORD

   60 CONTINUE

* Call the UNPACK utility with all array lengths set to unity,
* and it returns the actual lengths in the packed record.
* Try and get enough space in the stack for the integer and
* real data to be unpacked. Then recall UNPACK, this
* time supplying the actual lengths required.

*     CALL GUREC(NCD,STR,1,1,1,IL,KDAT,IRL,QDAT,
*    :  ICC,SDAT)
*     CALL GKSTAL(KINTGS,IL,ISETI)
*     IF(KERROR.NE.0) GOTO 999
*     CALL GKSTAL(KREALS,IRL,ISETR)
*     IF(KERROR.NE.0) GOTO 999
*     IIL=IL
*     IRIL=IRL
*     CALL GUREC(NCD,STR,IIL,IRIL,1,IL,KSTACK(ISETI),
*    :  IRL,QSTACK(ISETR),ICC,SDAT)

      IL=0
      IRL=0
      LENDAT=(IL+3)*IFIELD+IRL*IRFLD
      WRITE(CSTR(1),SFMT(4)) LENDAT,KWI1,IL,IRL
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+3*IFIELD))

* Send the integer array
*     NTOK=KWCST/IFIELD
*     DO 62 I=1,IL,NTOK
*       IF(IL-I+1.LT.NTOK) N=IL-I+1
*       WRITE(CSTR(1),SFMT(20)) (KSTACK(ISETI+J-1),J=1,N)
*       CALL GK1VBU(IFUNC,CSTR(1)(:N*IFIELD))
*  62 CONTINUE

* Send the real array
*     NTOK=KWCST/IRFLD
*     N=NTOK
*     DO 64 I=1,IRL,NTOK
*       IF(IRL-I.LT.NTOK) N=IRL-I+1
*       WRITE(CSTR(1),SFMT(21)) (QSTACK(ISETR+J-1),J=1,N)
*       CALL GK1VBU(IFUNC,CSTR(1)(:N*IRFLD))
*  64 CONTINUE

*     CALL GKSTDA(KREALS,ISETR)
*     CALL GKSTDA(KINTGS,ISETI)


      GOTO 999

* POLYLINE, POLYMARKER, FILL AREA
* -------------------------------
* Data expected:
* RX(NRD), RY(NRD) - POINTS IN WORLD COORDINATES

  110 LENDAT=IFIELD + 2*NRD*IRFLD
      WRITE(CSTR(1),SFMT(2)) LENDAT,NRD
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
*
* Transform points to NDC, then format and send
      CALL GKSTAL(KREALS,ICHUNK*2,ISETR)
      IF(KERROR.NE.0) GOTO 999
      N=ICHUNK
      DO 114 I=1,NRD,ICHUNK
        IF(NRD-I.LT.ICHUNK) N=NRD-I+1
        CALL GKTWD(N,RX(I),RY(I),QSTACK(ISETR),
     :    QSTACK(ISETR+ICHUNK))
        NPTS=KWCST/(2*IRFLD)
        M=NPTS
        DO 117 J=1,N,NPTS
          IF(N-J.LT.NPTS) M=N-J+1
          WRITE(CSTR(1),SFMT(19)) (QSTACK(ISETR+J+K-2),
     :      QSTACK(ISETR+ICHUNK+J+K-2),K=1,M)
          CALL GK1VBU(IFUNC,CSTR(1)(:2*M*IRFLD))
  117   CONTINUE
  114 CONTINUE
      CALL GKSTDA(KREALS,ISETR)
*
      GOTO 999

* TEXT
* ----
* Data expected:
* IDAT(NID) - STRING OF CHARACTERS IN ASCII
* QWR1,QWR2 - STARTING POINT OF CHARACTER STRING

* Transform starting point from WC to NDC

  130 CONTINUE

      CALL GKTWD(1,QWRA(1),QWRA(2),QWKDAT(1,KWKIX),QWKDAT(2,KWKIX))

      LENDAT=2*IRFLD+IFIELD+NID
      WRITE(CSTR(1),SFMT(13)) LENDAT,QWKDAT(1,KWKIX),
     :  QWKDAT(2,KWKIX),NID
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+2*IRFLD+IFIELD))
*
* Convert from Ascii integers to characters and send the string
      N=KWCST
      DO 135 I=1,NID,KWCST
        IF(NID-I.LT.KWCST) N=NID-I+1
         CALL GKATON( N,IDAT(I),CSTR(1) )
        CALL GK1VBU(IFUNC,CSTR(1)(:N))
  135 CONTINUE
      GOTO 999

* CELL ARRAY
* ----------
* Data expected:
* QWR1,..,6 Coordinates of corner points of pixel array
* KWI1      Dimension (in X)
* KWI2      Dimension (in Y)
* KWI3      Start column
* KWI4      Start row
* KWI5      Number of columns
* KWI6      Number of rows
* IDAT(NID) Array of colour indices stored row by row

* Transform points from WC to NDC

  150 CONTINUE

* [PX,PY]
      WCX(1)=QWR1
      WCY(1)=QWR2
* [QX,QY]
      WCX(2)=QWR3
      WCY(2)=QWR4
* [RX,RY]
      WCX(3)=QWR5
      WCY(3)=QWR6

      CALL GKTWD(3,WCX,WCY,QWKDAT(1,KWKIX),QWKDAT(4,KWKIX))

      LENDAT=6*IRFLD+(2+KWI5*KWI6)*IFIELD
      WRITE(CSTR(1),SFMT(14)) LENDAT,(QWKDAT(I,KWKIX),
     :  QWKDAT(I+3,KWKIX),I=1,3),KWI5,KWI6
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+6*IRFLD+2*IFIELD))
*
* Send the array in I-Format
      NTOK=KWCST/IFIELD
      DO 152 K=KWI4,KWI4+KWI6-1
        N=NTOK
        DO 155 I=KWI3,KWI3+KWI5-1,NTOK
          IF(KWI3+KWI5-I.LT.NTOK) N=KWI3+KWI5-I
          WRITE(CSTR(1),SFMT(20)) (IDAT(KWI1*(K-1)+I+J-1),J=1,N)
          CALL GK1VBU(IFUNC,CSTR(1)(:N*IFIELD))
  155   CONTINUE
  152 CONTINUE

      GOTO 999

* GENERALISED DRAWING PRIMITIVE
* -----------------------------
* Data expected:
* STR(NCD) - GDP DATA RECORD
* RX(NRD), RY(NRD) - LIST OF POINTS
* KWI1 - GDP IDENTIFIER
* QWR1,...QWR6  Three corners of unit square in WC

  160 CONTINUE

* Call the UNPACK utility with all array lengths set to unity,
* and it returns the actual lengths in the packed record.

*     CALL GUREC(NCD,STR,1,1,1,IL,KDAT,IRL,QDAT,
*    :  ICC,SDAT)

      IL=0
      IRL=0

      LENDAT=(4+IL)*IFIELD + ((NRD*2)+IRL+6)*IRFLD
      WRITE(CSTR(1),SFMT(5)) LENDAT,KWI1,NRD+3,IL,IRL
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+4*IFIELD))

* Transform first three points from WC to NDC, reformat, and send

      CALL GKTWD(3,QWRA(1),QWRA(4),QWKDAT(1,KWKIX),QWKDAT(4,KWKIX))

      WRITE(CSTR(1),SFMT(19)) (QWKDAT(J,KWKIX),J=1,6)
      CALL GK1VBU(IFUNC,CSTR(1) (:6*IRFLD))

* Transform points to NDC, then format and send
      CALL GKSTAL(KREALS,ICHUNK*2,ISETR)
      IF(KERROR.NE.0) GOTO 999
      N=ICHUNK
      DO 164 I=1,NRD,ICHUNK
        IF(NRD-I.LT.ICHUNK) N=NRD-I+1
        CALL GKTWD(N,RX(I),RY(I),QSTACK(ISETR),
     :    QSTACK(ISETR+ICHUNK))
        NPTS=KWCST/(2*IRFLD)
        M=NPTS
        DO 167 J=1,N,NPTS
          IF(N-J.LT.NPTS) M=N-J+1
          WRITE(CSTR(1),SFMT(19)) (QSTACK(ISETR+J+K-2),
     :      QSTACK(ISETR+ICHUNK+J+K-2),K=1,M)
          CALL GK1VBU(IFUNC,CSTR(1)(:2*M*IRFLD))
  167   CONTINUE
  164 CONTINUE
      CALL GKSTDA(KREALS,ISETR)

* Try and get enough space in the stack for the integer and
* real data to be unpacked. Then recall UNPACK, this
* time supplying the actual lengths required.

*     CALL GKSTAL(KINTGS,IL,ISETI)
*     IF(KERROR.NE.0) GOTO 999
*     CALL GKSTAL(KREALS,IRL,ISETR)
*     IF(KERROR.NE.0) GOTO 999
*     IIL=IL
*     IRIL=IRL
*     CALL GUREC(NCD,STR,IIL,IRIL,1,IL,KSTACK(ISETI),
*    :  IRL,QSTACK(ISETR),ICC,SDAT)

* Send the data record array lengths
*     WRITE(CSTR(1),SFMT(18)) IL,IRL
*     CALL GK1VBU(IFUNC,CSTR(1)(:2*IFIELD))

* Send the integer array
*     NTOK=KWCST/IFIELD
*     DO 166 I=1,IL,NTOK
*       IF(IL-I+1.LT.NTOK) N=IL-I+1
*       WRITE(CSTR(1),SFMT(20)) (KSTACK(ISETI+J-1),J=1,N)
*       CALL GK1VBU(IFUNC,CSTR(1)(:N*IFIELD))
* 166 CONTINUE

* Send the real array
*     NTOK=KWCST/IRFLD
*     N=NTOK
*     DO 168 I=1,IRL,NTOK
*       IF(IL-I.LT.NTOK) N=IL-I+1
*       WRITE(CSTR(1),SFMT(21)) (QSTACK(ISETR+J-1),J=1,N)
*       CALL GK1VBU(IFUNC,CSTR(1)(:N*IRFLD))
* 168 CONTINUE

*     CALL GKSTDA(KREALS,ISETR)
*     CALL GKSTDA(KINTGS,ISETI)

      GOTO 999

* POLYLINE INDEX
* --------------
* Data expected:
* KIPLI - POLYLINE INDEX

  210 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIPLI
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* LINETYPE
* --------
* Data expected:
* KILNTY - LINETYPE

  220 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KILNTY
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* LINEWIDTH SCALE FACTOR
* ----------------------
* Data expected
* QILNWD - LINEWIDTH SCALE FACTOR

  230 LENDAT=IRFLD
      WRITE(CSTR(1),SFMT(7)) LENDAT,QILNWD
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IRFLD))
      GOTO 999

* POLYLINE COLOUR INDEX
* ----------------------
* Data expected:
* KIPLCI - POLYLINE COLOUR INDEX

  240 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIPLCI
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* POLYMARKER INDEX
* ----------------
* Data expected:
* KIPMI - POLYMARKER INDEX

  250 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIPMI
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* MARKER TYPE
* -----------
* Data expected:
* KIMKTY - MARKER TYPE

  260 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIMKTY
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* MARKER SIZE SCALE FACTOR
* ------------------------
* Data expected:
* QIMKSZ - MARKER SIZE SCALE FACTOR

  270 LENDAT=IRFLD
      WRITE(CSTR(1),SFMT(7)) LENDAT,QIMKSZ
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IRFLD))
      GOTO 999

* POLYMARKER COLOUR INDEX
* -----------------------
* Data expected:
* KIPMCI - POLYMARKER COLOUR INDEX

  280 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIPMCI
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* TEXT INDEX
* ----------
* Data expected:
* KITXI -TEXT INDEX

  290 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KITXI
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* TEXT FONT AND PRECISION
* -----------------------
*Data expected:
* KITXFN - TEXT FONT
* KITXPR - TEXT PRECISION

  300 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KITXFN,KITXPR
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* CHARACTER EXPANSION FACTOR
* --------------------------
* (LEVEL 1a)
* Data expected:
* QICHXP - CHARACTER EXPANSION FACTOR

  310 LENDAT=IRFLD
      WRITE(CSTR(1),SFMT(7)) LENDAT,QICHXP
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IRFLD))
      GOTO 999

* CHARACTER SPACING
* -----------------
* (LEVEL 1a)
* Data expected:
* QICHSP - CHARACTER SPACING

  320 LENDAT=IRFLD
      WRITE(CSTR(1),SFMT(7)) LENDAT,QICHSP
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IRFLD))
      GOTO 999

* TEXT COLOUR INDEX
* -----------------
* Data expected:
* KITXCI - TEXT COLOUR INDEX

  330 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KITXCI
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* CHARACTER VECTORS
* -----------------
* Data expected:
* QICHHX,QICHHY - CHARACTER HEIGHT VECTOR
* QICHWX,QICHWY - CHARACTER WIDTH VECTOR

  340 CONTINUE

* Transform vectors from WC to NDC
      CALL GKTWDV(QICHHX,QICHHY,QWCHHX(KWKIX),QWCHHY(KWKIX))
      CALL GKTWDV(QICHWX,QICHWY,QWCHWX(KWKIX),QWCHWY(KWKIX))

      LENDAT=4*IRFLD
      WRITE(CSTR(1),SFMT(9)) LENDAT,QWCHHX(KWKIX),QWCHHY(KWKIX),
     :  QWCHWX(KWKIX),QWCHWY(KWKIX)
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* TEXT PATH
* ---------
*Data expected:
* KITXP- TEXT PATH

  350 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KITXP
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* TEXT ALIGNMENT
* --------------
* Data expected:
* KIHTXA,KIVTXA - TEXT ALIGNMENT

  360 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KIHTXA,KIVTXA
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* FILL AREA INDEX
* --------------
*Data expected:
* KIFAI- FILL AREA INDEX

  370 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIFAI
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* FILL AREA INTERIOR STYLE
* ------------------------
* Data expected:
* KIFAIS - FILL AREA INTERIOR STYLE

  380 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIFAIS
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* FILL AREA STYLE INDEX
* ---------------------
* Data expected:
* KIFASI - FILL AREA STYLE INDEX

  390 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIFASI
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

*FILL AREA COLOUR INDEX
* ---------------------
* Data expected:
* KIFACI - FILL AREA COLOUR INDEX

  400 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIFACI
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* PATTERN SIZE
* ------------
* (LEVEL 1a)
* Data expected:
* QIPAHX,QIPAHY,QIPAWX,QIPAWY - PATTERN HEIGHT AND WIDTH VECTORS

  410 CONTINUE

* Transform vectors from WC to NDC
      CALL GKTWDV(QIPAWX,QIPAWY,QWPAWX(KWKIX),QWPAWY(KWKIX))
      CALL GKTWDV(QIPAHX,QIPAHY,QWPAHX(KWKIX),QWPAHY(KWKIX))

      LENDAT=4*IRFLD
      WRITE(CSTR(1),SFMT(9)) LENDAT,QWPAWX(KWKIX),QWPAWY(KWKIX),
     :  QWPAHX(KWKIX),QWPAHY(KWKIX)
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* PATTERN REFERENCE POINT
* -----------------------
* (LEVEL 1a)
* Data expected:
* QIPAX,QIPAY - REFERENCE POINT

  420 CONTINUE

* Transform point from WC to NDC
      QWRA(1) = QIPAX
      QWRA(2) = QIPAY
      CALL GKTWD(1,QWRA(1),QWRA(2),QWPAX(KWKIX),QWPAY(KWKIX))

      LENDAT=2*IRFLD
      WRITE(CSTR(1),SFMT(8)) LENDAT,QWPAX(KWKIX),QWPAY(KWKIX)
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* ASPECT SOURCE FLAGS
* -------------------
* Data expected:
* KWKDAT(1)->(13)  -  ASF's

  430 CONTINUE
      LENDAT=13*IFIELD
      WRITE(CSTR(1),SFMT(6)) LENDAT,(KWKDAT(I,KWKIX),I=1,13)
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* PICK IDENTIFIER
* ---------------
* (LEVEL 1a)
* Data expected:
* KWI1 - PICK IDENTIFIER

  440 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KWI1
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* POLYLINE REPRESENTATION
* -----------------------
* (LEVEL 1a)
* Data expected:
* KWI1 - POLYLINE INDEX
* KWI2 - LINETYPE
* QWR1 - LINEWIDTH SCALE FACTOR
* KWI3 - POLYLINE COLOUR INDEX

  510 LENDAT=3*IFIELD+IRFLD
      WRITE(CSTR(1),SFMT(15)) LENDAT,KWI1,KWI2,QWR1,
     : KWI3
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* POLYMARKER REPRESENTATION
* -------------------------
* (LEVEL 1a)
* Data expected:
* KWI1 - POLYMARKER INDEX
* KWI2 - MARKER TYPE
* QWR1 - MARKER SIZE SCALE FACTOR
* KWI3 - POLYMARKER COLOUR INDEX

  520 LENDAT=3*IFIELD+IRFLD
      WRITE(CSTR(1),SFMT(15)) LENDAT,KWI1,KWI2,
     :  QWR1,KWI3
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* TEXT REPRESENTATION
* -------------------
* (LEVEL 1a)
* Data expected:
* KWI1 - TEXT INDEX
* KWI2 - TEXT FONT
* KWI3 - TEXT PRECISION
* QWR1 - CHARACTER EXPANSION FACTOR
* QWR2 - CHARACTER SPACING
* KWI4 - TEXT COLOUR INDEX

  530 LENDAT=4*IFIELD+2*IRFLD
      WRITE(CSTR(1),SFMT(16)) LENDAT,KWI1,KWI2,KWI3,
     : QWR1,QWR2,KWI4
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* FILL AREA REPRESENTATION
* ------------------------
* (LEVEL 1a)
* Data expected:
* KWI1 - FILL AREA INDEX
* KWI2 - FILL AREA INTERIOR STYLE
* KWI3 - FILL AREA STYLE INDEX
* KWI4 - FILL AREA COLOUR INDEX

  540 LENDAT=4*IFIELD
      WRITE(CSTR(1),SFMT(5)) LENDAT,KWI1,KWI2,KWI3,KWI4
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* PATTERN REPRESENTATION
* ----------------------
* (LEVEL 1a)
* Data expected:
* KWI1 - PATTERN INDEX
* KWI2,KWI3 - DIMENSIONS OF PATTERN ARRAY
* KWI4,KWI5 - START COLUMN AND START ROW OF PATTERN ARRAY
* KWI6,KWI7 - NUMBER OF COLUMNS AND ROWS TO BE OUTPUT FROM PATTERN ARRAY
* PATTERN ARRAY

  550 LENDAT=(3+KWI6*KWI7)*IFIELD
      WRITE(CSTR(1),SFMT(4)) LENDAT,KWI1,KWI6,KWI7
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+3*IFIELD))
*
* Send the array in I-Format
      NTOK=KWCST/IFIELD
      DO 552 K=KWI5,KWI5+KWI7-1
        N=NTOK
        DO 555 I=KWI4,KWI4+KWI6-1,NTOK
          IF(KWI6+KWI4-I.LT.NTOK) N=KWI6+KWI4-I
          WRITE(CSTR(1),SFMT(20)) (IDAT(KWI2*(K-1)+I+J-1),J=1,N)
          CALL GK1VBU(IFUNC,CSTR(1)(:N*IFIELD))
  555   CONTINUE
  552 CONTINUE
*
      GOTO 999

* COLOUR REPRESENTATION
* ---------------------
* Data expected:
* KWI1 - COLOUR INDEX
* QWR1,QWR2,QWR3 - COLOUR (RED,GREEN,BLUE INTENSITIES)

  560 LENDAT=IFIELD+3*IRFLD
      WRITE(CSTR(1),SFMT(11)) LENDAT,KWI1,QWR1,QWR2,
     : QWR3
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* CLIPPING RECTANGLE
* ------------------
* Data expected:
* QWR7,QWR8,QWR9,QWR10 - LIMITS OF CLIPPING RECTANGLE (XMIN,
*  XMAX,YMIN,YMAX)

* These values were copied from the GKS state list by the
* front-end utitity GKCCTG if clipping was ON, and set to 0.0,
* 1.0,0.0,1.0 by GKCCTG if clipping was OFF.

  610 LENDAT=4*IRFLD
      WRITE(CSTR(1),SFMT(9)) LENDAT,QWR7,QWR8,QWR9,QWR10
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* WORKSTATION WINDOW
* ------------------
* Data expected:
* QWR1,QWR2,QWR3,QWR4 - WORKSTATION WINDOW LIMITS (XMIN,XMAX,
*  YMIN,YMAX)

  710 LENDAT=4*IRFLD
      WRITE(CSTR(1),SFMT(9)) LENDAT,QWR1,QWR2,QWR3,QWR4
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* WORKSTATION VIEWPORT
* --------------------
* Data expected:
* QWR1,QWR2,QWR3,QWR4 - WORKSTATION VIEWPORT LIMITS (XMIN,XMAX,
*  YMIN,YMAX)

  720 LENDAT=4*IRFLD
      IF(QWR1.GE.100.0.OR.QWR2.GE.100.0.OR.QWR3.GE.100.0.OR.
     : QWR4.GE.100.0) THEN
         WRITE(CSTR(1),'(I7,4F9.4)') LENDAT,QWR1,QWR2,QWR3,QWR4
      ELSE
         WRITE(CSTR(1),SFMT(9)) LENDAT,QWR1,QWR2,QWR3,QWR4
      ENDIF
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* CREATE SEGMENT
* --------------
* Data expected:
* KWI1 - SEGMENT NAME

  810 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KWI1
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* CLOSE SEGMENT
* --------------
* Data expected: NONE

  820 LENDAT=0
      WRITE(CSTR(1),SFMT(1)) LENDAT
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD))
      GOTO 999

* RENAME SEGMENT
* --------------
* Data expected:
* KWI1 - OLD SEGMENT NAME
* KWI2 - NEW SEGMENT NAME

  830 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KWI1,KWI2
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* DELETE SEGMENT
* --------------
* Data expected:
* KWI1 - SEGMENT NAME

  840 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KWI1
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* SET SEGMENT TRANSFORMATION
* --------------------------
* Data expected:
* KWI1 - SEGMENT NAME
* KWI5 - 1 OR 2 INDICATING 1ST OR 2ND ENTRY
* QWR1,..,.QWR6 - TRANSFORMATION MATRIX

  910 LENDAT=IFIELD+6*IRFLD
      WRITE(CSTR(1),SFMT(12)) LENDAT,KWI1,QWR1,QWR2,QWR3,QWR4,
     :  QWR5,QWR6
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* SET VISIBILITY
* --------------
* Data expected:
* KWI1 - SEGMENT NAME
* KWI4 - VISIBILITY

  920 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KWI1,KWI4
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* SET HIGHLIGHTING
* ----------------
* Data expected:
* KWI1 - SEGMENT NAME
* KWI5 - HIGHLIGHTING

  930 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KWI1,KWI5
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* SET SEGMENT PRIORITY
* --------------------
* Data expected:
* KWI1 - SEGMENT NAME
* QWR1 - SEGMENT PRIORITY

  940 LENDAT=IFIELD+IRFLD
      WRITE(CSTR(1),SFMT(10)) LENDAT,KWI1,QWR1
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* SET DETECTABILITY
* -----------------
* Data expected:
* KWI1 - SEGMENT NAME
* KWI2 - DETECTABILITY

  950 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KWI1,KWI2
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* USER ITEM
* ----------
* Data expected:
* STR(NCD) - ITEM DATA RECORD
* KWI2 - LENGTH OF ITEM DATA RECORD (CHARACTERS)

 1010 LENDAT=KWI2
      IF(LENDAT.GT.NCD*80 .OR. LENDAT.LT.0) THEN
        KERROR=161
        GOTO 999
      ENDIF
      WRITE(CSTR(1),SFMT(1)) LENDAT
      CALL GK1VBU(IFUNC,CSTR(1)(:LFIELD))

      DO 1015 I=1,(LENDAT-1)/80+1
        J=MIN(80,LENDAT-80*(I-1))
        CALL GK1VBU(IFUNC,STR(I)(1:J))
 1015 CONTINUE

      GOTO 999

* Could have a call to GKBUG here to trap invalid metafile
* item type, but all that is needed is for GK1VWD to be
* consistent with this routine.

  998 CONTINUE

  999 RETURN
      END
C# IL>=a, OL>=0
*
*
*
*
*
      SUBROUTINE GK1VBU(IFUNC,STRING)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation
*  Author:             DSG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Metafile output buffer handler
*
*  MAINTENANCE LOG
*  ---------------
*     01/03/83  DSG  Original version stabilized
*     19/06/86  RMK  Maintenance log incorporated into main driver routine
*
*  ARGUMENTS
*  ---------
*     INP   IFUNC  Function code
*     INP   STRING Character string
*
      INTEGER IFUNC
      CHARACTER*(*) STRING
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKZIO/  CMBUFF - main buffer
*     Read   /GKYWCA/ KWKIX  - workstation index
*     Read   /GKYWKD/ KWCID  - channel number
*                     KWKDAT(14,KWKIX) - buffer pointer
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     MAXBUF   Fixed length of the output buffer
*     NCH      Number of characters in the string to be output
*     I        Loop  variable
*     IP       Buffer pointer
*
      INTEGER MAXBUF,NCH,I,IP
      PARAMETER (MAXBUF=80)
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


* Pick up value of buffer pointer
      IP=KWKDAT(14,KWKIX)

      GOTO(10,20,30) IFUNC

* Initialise buffer

   10 IP=1
      GOTO 99

   20 NCH=LEN(STRING)
      DO 25 I= 1,NCH
      CMBUFF(KWKIX)(IP:IP)=STRING(I:I)
        IP=IP+1
        IF(IP.GT.MAXBUF) THEN
          WRITE(KWCID(KWKIX),'(A80)') CMBUFF(KWKIX)(1:80)
          IP=1
        ENDIF
   25 CONTINUE
      GOTO 99

* Pad buffer with blanks, and force out

   30 DO 35 I=IP,MAXBUF
        CMBUFF(KWKIX)(I:I) = ' '
   35 CONTINUE
      WRITE(KWCID(KWKIX),'(A80)') CMBUFF(KWKIX)(1:80)

* Save buffer pointer
   99 KWKDAT(14,KWKIX)=IP

      END

*----------------------------------------------------------------

      SUBROUTINE GK1VXF(IFID,RHT,RMAXWD,RBOT,RTOP,RWD)
*
* (C) COPYRIGHT ICL & SERC  1986
*

*----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Part of workstation driver
*  Author:             MJD
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To supply details for IBM4250 Hardware Font.
*
*  MAINTENANCE LOG
*  ---------------
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier (ignored)
*     OUT RHT      Height from base to cap line
*     OUT RMAXWD   Width of widest character
*     OUT RBOT     Distance from base to bottom line
*     OUT RTOP     Distance from cap to top line
*     OUT RWD      Character widths array
*
      INTEGER IFID
      REAL RHT,RMAXWD,RBOT,RTOP,RWD(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER II
*
*----------------------------------------------------------------------


      RHT = 0.044
      RMAXWD = 0.030
      RBOT = 0.0
      RTOP=0.0
*     widths for chars
      DO 10 II=1,95
        RWD(II)=RMAXWD
 10   CONTINUE
      RETURN
      END
