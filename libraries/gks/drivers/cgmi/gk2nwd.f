C# IL>=a, OL>=0
      SUBROUTINE GK2NWD (IENT,NID,IDAT,NRD,RX,RY,NCD,SDR)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION DRIVER
*  Author:             CIA
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CGM Input driver interface
*
*  MAINTENANCE LOG
*  ---------------
*     **/11/87  CIA   Original version
*      8/01/88  CIA   Changed GK2NCH to call routine GKATON
*     11/01/88  DSG   Put in jump to statement 4550
*     13/01/88  CIA   Fixed bug in reading the char up vector
*     19/01/88  CIA   Took out ILEN as unnecessary
*     21/01/88  CIA   Changed name of CGM include file
*     25/01/88  CIA   Altered logic of Style Index
*     26/01/88  CIA   Added default for KINTPR & added comments
*                     on Routine Calls
*     **/05/88  CIA   Optimised code; fixed bug in GDP; ensured that
*        to           attributes were reset at each picture; added
*                     workstation index to CGM state list variables;
*     **/06/88        added code for workstation window
*     01/08/88  CIA   Improved code for reading reals without explicit
*                     exponents.
*     17/11/88  NMH   Allow for use of inquire functions when no CGM
*                     input workstation currently open
*     12/12/88  CIA   Now handles defaults replacements correctly
*     13/01/89  CIA   Added code to handle CGM rectangle, arcs & pseudo
*                     ASFs
*     31/01/89  CIA   Corrected the way metafile defaults are read
*     **/03/89  CIA   Changed driver so that character substitution
*                     is now handled by GK2NBU - needed an extra flag,
*                     but makes things tidier in the end. Also changed
*                     it to handle integer VDCs.
*     26/09/89  RTP   Insert new GKS fonts
*      1/11/89  RTP   Change Error codes for some invalid parameters
*                     Use Error code 162 to abort for serious errors
*      4/12/89  RTP   Remove unused local variables
*     19/03/90  RTP   Change settings in GK2NRV to KHANGE (S410).
*                     Change character width on change to Char height
*                     Use Hex parameters for some constants
*                     Add entry points for readability
*     20/03/90  RTP   Allow Marker type and Line type to have negative
*                     values by using GKPKI not GKPKSI (S387).
*                     Introduce Real Normalisation factor for real VDCs
*     27/3/90   RTP   Change VDC handling and points transformations
*                     to handle any VDC Extent
*     30/3/90   RTP   Add flags for VDC dependent defaults
*     04/4/90   RTP   Change points calculation for GDPs
*     12/4/90   RTP   Change GK2NHI to read data from input so that
*                     Cell Array size can be calculated.
*                     Add KWKDAT(1) to store CGM Opcode and reassign
*                     KWKDAT(2) and KWKDAT(3)
*     17/4/90   RTP   Add routine GK2NCL to get Bitstream colours
*                     Remove Common variable TEXFON, CHIGHT and KXTRA
*                     and use CGM opcode instead.
*     18/4/90   RTP   Use routine GKIOFI to read file
*     03/5/90   RTP   Tidy up headers for inclusion in master source
*     04/5/90   RTP   In GK2NBU check for character substitution before
*                     Writing to buffer
*                     Use KVXALL flag for real VDCs, not KEXALL
*     15/5/90   RTP   Change Aux colour and Transparency check to AND
*                     Check for Zero colour range in GK2NRG
*     02/7/90   RTP   Change entry for GDSG(44) to ignore instead of
*                     w/s is MI (see GKS BUG - S406)
*     09/07/90  PLP   Removed unused locals SFMT and ISET.
*     17/07/90  PLP   RMK added GKS bug numbers to some of the logs.
*     27/07/90  RTP   Change scaling algorithm for character orientation
*                     to adjust for current character height
*                     (see GKS BUG C42)
*                     Separate entry point 71 (Workstation Viewport)
*     16/01/91  RTP   Add Application Data (User Item) for NCC Tests

*    Last Modified on  17 Jan 1991  at  12:43:01


*   SUBROUTINE CALLS IN WORKSTATION DRIVER:
*      GK2NIL  : Called at GTITM entrypoint to calculate the
*                internal packed length of the RDITM data record.

*      GK2NBU : Gets the next ASCII character, converts it to
*               an integer code, and stores in internal buffer.
*               Handles character substitution.

*      GK2NOP : Returns a GKS itemtype from a CGM opcode.

*      GK2NHI : Gets an integer from the CGM.

*      GK2NHR : Gets a real or VDC from the CGM.

*      GK2NCH : Gets a character string from the CGM.

*      GK2NCL : Gets a bitstream Colour from the CGM

*      GK2NRG : Gets an RGB value from the CGM, returning it
*               as three real values.

*      GK2NNC : Gets the next ASCII character from the buffer
*               & converts it to integer code.

*      GK2NIC : Initialises the common block.

*      GK2NRV : Resets the GKS State List

*      GK2NMP : Finds a point in the centre of an arc, given end, radius
*               & centre points.


*
*  ARGUMENTS
*  ---------
*     INP   IENT   Entrypoint code
*     INP   NID    Size of array IDAT
*     INP   IDAT   Integer data passed to workstation
*     INP   NRD    Size of arrays RX and RY
*     INP   RX     Real X-coordinate data passed to workstation
*     INP   RY     Real Y-coordinate data passed to workstation
*     INP   NCD    Dimension of character array
*     OUT   SDR    Character*80 array Data Record
*
      INTEGER IENT,NID,IDAT(NID),NRD,NCD
      REAL RX(NRD), RY(NRD)
      CHARACTER*80 SDR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/    Workstation communication area
*     Read   /GKYWCB/    Metafile input workstation index
*     Read   /GKYERR/    KERROR
*     Modify /GKYWKD/    Workstation derived data - KWKDAT usage:
*                        1 - CGM Character Opcode
*                        2 - ITYPE, GKS metafile item type
*                        3 - GOTTYP, 0 if item type and length need to
*                                       be got
*                                     1 if already got (Nb this is
*                                       different from other GKS
*                                       metafile drivers)
*     Modify /GKYWSL/    Workstation state list
*     Modify /GKYSTK/    Stack
*
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkpid.par'
      INCLUDE '../../include/gkwke.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwcb.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkerr.cmn'

*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgn.cmn'

      INCLUDE '../../include/gksl.cmn'
*
*  LOCALS
*  ------
*     GOTTYP   Item flag used to identify the current item
*              ( 0 if item type and length is next to be input,
*                1 if item data is next)
*
*     IPL   offset to aspect list for polyline
*     IPM   offset to aspect list for polymarker
*     ITX   offset to aspect list for text
*     IFA   offset to aspect list for fill area
*     REALPT,VDC,VDCX,VDCY,VDCXL,VDCYL
*           flags telling real utilities the type of
*           reals being processed
*     NFONTS Number of Fonts in RAL-GKS
*     FACTOR Conversion factor for Character vectors
*
      INTEGER    IPL,  IPM,  ITX, IFA
      PARAMETER (IPL=0,IPM=3,ITX=6,IFA=10)
      INTEGER    REALPT, VDC, VDCX, VDCY, VDCXL, VDCYL
      PARAMETER (REALPT=1,VDC=2,VDCX=3,VDCY=4,VDCXL=5,VDCYL=6)
*
      INTEGER NFONTS
      PARAMETER (NFONTS=16)
*
      INTEGER RESTX, RECT, CIRCLE, CHHT, TEXFON, POLYSET
      PARAMETER (RESTX=36, RECT=42, POLYSET = 39)
      PARAMETER (CIRCLE = 13344, TEXFON=13617, CHHT=13622)
*
      INTEGER GOTTYP
*
*  Integer local array
      INTEGER ILA(13)
*
*  Real local array
      REAL RLA(7)
*
      INTEGER KRALFN(NFONTS)
*
      INTEGER IAT
      INTEGER  TEMP,ITYPE,NCHARS,I,J,N,M,
     :         IDGDP,LENGTH,IBIT,IBYTE,
     :         MNCD,NPTS,COLPRE
      REAL FACTOR

      CHARACTER*80 CDUMMY

      DATA KRALFN /1, -101, -102, -103, -104, -105, -106, -107, -110,
     +            -115, -108, -109, -111, -112, -113, -114/

*  ERRORS
*  ------
*
*    32   Specified workstation is not of category MO
*    33   Specified workstation is of category MI
*    37   Workstation is not of category OUTIN
*    38   Workstation is neither of category INPUT nor of
*         category OUTIN
*    39   Workstation is neither of category OUTPUT nor of
*         category OUTIN
*   162   No item is left in metafile
*   163   Metafile item is invalid
*   164   Item type is not a valid GKS item
*   166   Maximum item data length record is invalid
*
*  COMMENTS
*  --------
*
*  Implementation decision (ref GIN 76):
*    Data records will not be packed using the PACK utility, but
*  will be returned in internal binary format in the same order
*  as in the external metafile.
*
*---------------------------------------------------------------------


* Restore local variables from COMMON
      IF((IENT.NE.KOPWK).AND.(IENT.LT.KQWKDU)) THEN
        ITYPE = KWKDAT(2,KWKIX)
        GOTTYP = KWKDAT(3,KWKIX)
      ENDIF

* GOTO conditional on the entrypoint

      GOTO(  10,  20,9933,9933,9933,9933,9933,9933,9999,9999,
     :      110,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :     9999,9999,9933,9933,9933,9933,9933,9933,9999,9999,
     :     9999,9933,9933,9999,9999,9999,9999,9999,9999,9999,
     :     9999,9999,9999,9999,9933,9933,9999,9999,9999,9999,
     :     9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :     9938,9938,9938,9938,9937,9938,9938,9938,9938,9938,
     :     9938,9938,9937,9938,9938,9938,9938,9938,9937,9938,
     :     9938,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :     9932, 920, 930 )IENT

      IF(IENT.EQ.KQPXAD .OR. IENT .EQ. KQPXA) GOTO 9939

      GOTO(3000,9933,9933,9933,9933,9933,9933,9933,9939,9933,
     :     9933,9933,9933,9933,9933,9933,9933,9938,9938,9938,
     :     9938,9937,9938) IENT-129

      IF(IENT.EQ.KQWKCA) GOTO 4000
      IF(IENT.EQ.KQWKCL) GOTO 9939
      IF(IENT.EQ.KQMDS) GOTO 9933
      IF(IENT.GE.KQDWKA.AND.IENT.LE.KQDSGA) GOTO 9939
      IF(IENT.GE.KQLI.AND.IENT.LE.KQDST) GOTO 9938


* Invalid entrypoint
      GOTO 9999

* OPEN WORKSTATION  -  KOPWK ( 1 )
* ----------------
   10 CONTINUE
      KCID(KWKIX) = KWI1

      CALL GKIOOP(KFWKFR,KCID(KWKIX),KWCID(KWKIX))

      IF(KERROR.EQ.0) THEN
* Initialise Input buffer
         CALL GKIOFI( KIOIT, KLENRC, I, CDUMMY)
         CALL GK2NIC
* Initialise item type flag to 'not yet known'
         GOTTYP = 0
      ENDIF

*  Workstation category
      KWI1 = GMI

      GOTO 9999

* CLOSE WORKSTATION  -  KCLWK (2)
* ----------------
   20 CONTINUE
      CALL GKIOCL(KFWKFR,KCID(KWKIX),KWCID(KWKIX))
      GOTO 9999

* ESCAPE  -  KESC (11)
* ------
*  No action
  110 CONTINUE
      GOTO 9999

* GET ITEM TYPE FROM GKSM  -  KGTITM (92)
* -----------------------
* Data expected:   None
* Data returned:   KWI1   item type
*                  KWI2   item data record length

  920 CONTINUE

*   If Item type not known then get it
      IF (GOTTYP.EQ.0) THEN
         CALL GK2NOP (ITYPE)
      ENDIF
      IF(KERROR.NE.0) GOTO 9999

      GOTTYP = 1
      KWI1=ITYPE

* Calculate the item Data Length in the internal form
* ( N.B. - GDP not fully implemented yet )
      CALL GK2NIL (ITYPE,LENGTH)
      KWI2=LENGTH
      GO TO 9999

* READ ITEM FROM GKSM  -  KRRITM (93)
* -------------------
* Data expected:
*  KWI1 - Maximum length of data record

  930 CONTINUE
      MNCD = KWI1

      IF(MNCD.LT.0 .OR. MNCD.GT.NCD*KLENRC)THEN
*  Maximum item data record length is invalid
        KERROR=166
        GOTO 9999
      ENDIF

      IF(MNCD.EQ.0 .OR. ITYPE .LT. 0) THEN
* Skip item data record
        GOTTYP = 0
        GOTO 9999
      ENDIF

      IAT=1

      IF(ITYPE.EQ.0) GOTO 1020
      GOTO (1000,1020,1000,1040,1050,1060,9463,9463,9463,9463,
     :      1110,1110,1130,1140,1150,1160,9463,9463,9463,9463,
     :      1010,1030,1230,1010,1010,1030,1270,1010,1010,1300,
     :      1310,1310,1010,1340,1010,1040,1010,1010,1390,1010,
     :      1410,1420,1430,1010,9463,9463,9463,9463,9463,9463,
     :      9463,9463,9463,9463,1550,1560,9463,9463,9463,9463,
     :      1350,9463,9463,9463,9463,9463,9463,9463,9463,9463,
     :      1710,1720,9463,9463,9463,9463,9463,9463,9463,9463,
     :      9463,9463,9463,9463,9463,9463,9463,9463,9463,9463,
     :      9463,9463,9463,9463,9463,9463,9463,9463,9463,9463),
     :      ITYPE

      GOTO 2010


* Clear and Update Workstation -  GKSM 1, 3
* ----------------------------
 1000 CONTINUE
      ILA(1)=0
      CALL GKPKSI(1,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ

*   Initialise Hatch Flag
      HATCH(KWKIX) = .FALSE.

      GOTO 9990

* Metafile items whose data record consists of one positive integer
* -----------------------------------------------------------------
* value only - GKSM 21,24,25,28,29,33,35,37,38,40,44
* ----------
 1010 CONTINUE
      CALL GK2NHI(ILA(1),.TRUE.)
      IF(KERROR.NE.0) GOTO 9999

      CALL GKPKSI(1,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ

*   Set hatch flag
      IF(ITYPE.EQ.38) THEN

         IF (ILA(1).EQ.3) THEN
            HATCH(KWKIX)=.TRUE.
            IF(KHTPAT(KWKIX).EQ.2)THEN
               KCFASI=KHTCHI(KWKIX)
            ENDIF
         ELSE
            IF((KHTPAT(KWKIX).EQ.3).AND.(ILA(1).EQ.2))THEN
               KCFASI=KPATTI(KWKIX)
            ENDIF
            HATCH(KWKIX)=.FALSE.
         ENDIF
      ENDIF
      GOTO 9990

* Metafile items having a zero length data record  -  GKSM 0,2
* -----------------------------------------------
 1020 CONTINUE
      GOTO 9990

* Metafile items whose data record consists of one integer
* --------------------------------------------------------
* value only - GKSM 22 (line type), 26 (marker type)
* ----------
 1030 CONTINUE
      CALL GK2NHI(ILA(1),.TRUE.)
      IF(KERROR.NE.0) GOTO 9999

      CALL GKPKI(1,ILA,IAT,SDR)
      IAT=IAT+KPDISZ
      GOTO 9990


* Metafile items whose data record consists of two integer
* -------------------------------------------------------
* values only  -  GKSM 4 (deferral state), 36 (Text Align)
* -----------
 1040 CONTINUE
      CALL GK2NHI(ILA(1),.TRUE.)
      CALL GK2NHI(ILA(2),.TRUE.)
      IF(KERROR.NE.0) GOTO 9999
      CALL GKPKSI(2,ILA,IAT,SDR)
      IAT=IAT+2*KPDSSZ
      GOTO 9990

* Message  -  GKSM 5
* -------
 1050 CONTINUE

*   Get Action flag - Ignore
      CALL GK2NHI(ILA(1),.TRUE.)

*   Pack the number of characters into the array
      CALL GKPKSI(1,KXCHA,IAT,SDR)
      IAT=IAT+KPDSSZ

*   Advance pointer to allow for open character string code
      KCDPTR(KWKIX)=KCDPTR(KWKIX)+2
 1055 CONTINUE

*   Pack the characters into the array
      IF (KXCHA.GT.KLENRC) THEN
         NCHARS=KLENRC
      ELSE
         NCHARS=KXCHA
      ENDIF

      CALL GK2NCH(NCHARS,CSTR)
      IF(KERROR.NE.0) GOTO 9999
      CALL GKPKC(1,NCHARS,CSTR,IAT,SDR)
      IAT=IAT+NCHARS*KPDCSZ
      KXCHA=KXCHA-NCHARS
      IF(KXCHA.GT.0) GOTO 1055
      GOTO 9990

* Escape  -  GKSM 6
* ------
 1060 CONTINUE
*   Get Escape Identifier
      CALL GK2NHI(ILA(1),.TRUE.)

*   Ignore data record

      GOTO 9990

* Polyline, Polymarker  -  GKSM 11,12
* --------------------
 1110 CONTINUE

*   Pack number of points
      ILA(1)=KNOPTS
      CALL GKPKSI(1,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ

* Input pairs of X,Y values, convert to internal binary
*  format, and put into the array supplied

      RLA(1) = -QVDCOX(KWKIX)*QVDCSX(KWKIX)
      RLA(2) = -QVDCOY(KWKIX)*QVDCSY(KWKIX)
      KXEXP(KWKIX)=KVDEFX(KWKIX)
      KYEXP(KWKIX)=KVDEFX(KWKIX)
      DO 1112 J=1,KNOPTS
        CALL GK2NHR(RLA(3),VDCXL)
        CALL GK2NHR(RLA(4),VDCYL)
        IF(KERROR.NE.0) GOTO 9999
        RLA(1)=RLA(1)+RLA(3)
        RLA(2)=RLA(2)+RLA(4)
        CALL GKPKR(2,RLA,IAT,SDR)
        IAT=IAT+2*KPDRSZ
 1112 CONTINUE

      GOTO 9990

* Text  -  GKSM 13
* ----
 1130 CONTINUE

*   Get the start point & pack into the array
      CALL GK2NHR(RLA(1),VDCX)
      CALL GK2NHR(RLA(2),VDCY)
      IF (KWKDAT(1,KWKIX).EQ.RESTX) THEN
*  Retricted Text get next two points as text origin
         CALL GK2NHR(RLA(1),VDCX)
         CALL GK2NHR(RLA(2),VDCY)
      ENDIF

*   Get final/not final flag (GKS doesnt use it:- maybe do something
*   with it later)
      CALL GK2NHI(TEMP,.TRUE.)
      IF(KERROR.NE.0) GOTO 9999
      CALL GKPKR(2,RLA,IAT,SDR)
      IAT=IAT+2*KPDRSZ
      IF(KERROR.NE.0) GOTO 9999

*   Pack the number of characters into the array
      CALL GKPKSI(1,KXCHA,IAT,SDR)
      IAT=IAT+KPDSSZ
      KCDPTR(KWKIX)=KCDPTR(KWKIX)+2

 1135 CONTINUE

*   Pack the characters into the array
      IF (KXCHA.GT.KLENRC) THEN
         NCHARS=KLENRC
      ELSE
         NCHARS=KXCHA
      ENDIF

      CALL GK2NCH(NCHARS,CSTR)
      IF(KERROR.NE.0) GOTO 9999
      CALL GKPKC(1,NCHARS,CSTR,IAT,SDR)
      IAT=IAT+NCHARS*KPDCSZ
      KXCHA=KXCHA-NCHARS
      IF(KXCHA.GT.0) GOTO 1135

      GOTO 9990

* Fill Area  -  GKSM 14
*-----------

 1140 CONTINUE

*   If Item is CGM Rectangle, then handle it accordingly
      IF (KWKDAT(1,KWKIX).EQ.RECT) THEN

*   Pack number of points
         ILA(1)=4
         CALL GKPKSI(1,ILA,IAT,SDR)
         IAT=IAT+KPDSSZ
         CALL GK2NHR(RLA(1),VDCX)
         CALL GK2NHR(RLA(2),VDCY)
         CALL GK2NHR(RLA(3),VDCX)
         CALL GK2NHR(RLA(4),VDCY)

*   Map the corner points onto four points for Fill Area
         CALL GKPKR(2,RLA,IAT,SDR)
         IAT=IAT+2*KPDRSZ
         CALL GKPKR(1,RLA,IAT,SDR)
         IAT=IAT+KPDRSZ
         CALL GKPKR(1,RLA(4),IAT,SDR)
         IAT=IAT+KPDRSZ
         CALL GKPKR(2,RLA(3),IAT,SDR)
         IAT=IAT+2*KPDRSZ
         CALL GKPKR(1,RLA(3),IAT,SDR)
         IAT=IAT+KPDRSZ
         CALL GKPKR(1,RLA(2),IAT,SDR)
         IAT=IAT+KPDRSZ
         CALL GKPKR(2,RLA,IAT,SDR)
         IAT=IAT+2*KPDRSZ
         IF(KERROR.NE.0) GOTO 9999
         GOTO 9990
      ELSE

*   Ordinary Fill Area
         ILA(1)=KNOPTS
         CALL GKPKSI(1,ILA,IAT,SDR)
         IAT=IAT+KPDSSZ
      ENDIF

* Set first point to transformation of NDC value (0,0)
      RLA(1) = -QVDCOX(KWKIX)*QVDCSX(KWKIX)
      RLA(2) = -QVDCOY(KWKIX)*QVDCSY(KWKIX)
      KXEXP(KWKIX)=KVDEFX(KWKIX)
      KYEXP(KWKIX)=KVDEFX(KWKIX)
* Input pairs of X,Y values, convert to internal binary
*  format, and put into the array supplied
      DO 1142 J=1,KNOPTS
        CALL GK2NHR(RLA(3),VDCXL)
        CALL GK2NHR(RLA(4),VDCYL)
        IF ( KWKDAT(1,KWKIX) .EQ. POLYSET ) THEN
* For Polygon set - ignore close/vis flag
           CALL GK2NHI(TEMP,.TRUE.)
        ENDIF
        IF(KERROR.NE.0) GOTO 9999
        RLA(1)=RLA(1)+RLA(3)
        RLA(2)=RLA(2)+RLA(4)
        CALL GKPKR(2,RLA,IAT,SDR)
        IAT=IAT+2*KPDRSZ
 1142 CONTINUE

      GOTO 9990


* Cell Array  -  GKSM 15
* ----------
 1150 CONTINUE
      DO 1152 J=1,6
         CALL GK2NHR(RLA(J),VDCX+MOD(J+1,2))
         IF(KERROR.NE.0) GOTO 9999
 1152 CONTINUE
      CALL GKPKR(6,RLA,IAT,SDR)
      IAT=IAT+6*KPDRSZ
      CALL GK2NHI(N,.TRUE.)
      CALL GK2NHI(M,.TRUE.)
      IF(KERROR.NE.0) GOTO 9999
      ILA(1)=N
      ILA(2)=M
      CALL GKPKSI(2,ILA,IAT,SDR)
      IAT=IAT+2*KPDSSZ

*   Local colour precision
      CALL GK2NHI(COLPRE,.TRUE.)
      IF (KERROR.NE.0) GOTO 9999
      IF (COLPRE.EQ.0) COLPRE = KCOLPR(KWKIX)

*   Get type of list
      CALL GK2NHI(ITYPE,.TRUE.)
      IBYTE = 0
      IBIT = 0

*   Indexed colour Normal or bitstream
      IF(ITYPE.EQ.0.OR.ITYPE.EQ.1)THEN
         NPTS=N*M
         DO 1154 J=1,NPTS
            IF (ITYPE .EQ. 0) THEN
               CALL GK2NHI(ILA(1),.TRUE.)
            ELSE
               CALL GK2NCL(ILA(1),COLPRE,IBYTE,IBIT)
            ENDIF
            IF ( ILA(1).LT.0 .OR. KERROR.NE.0) GOTO 9999
            CALL GKPKSI(1,ILA,IAT,SDR)
            IAT=IAT+KPDSSZ
 1154    CONTINUE

*   Normal Runlength or Bitstream runlength
      ELSEIF(ITYPE.EQ.2.OR.ITYPE.EQ.3)THEN
         NPTS=N*M
         I = 0

 1155    CONTINUE

*   Get index and number of repetitions
         IF (ITYPE .EQ. 2) THEN
            CALL GK2NHI(ILA(1),.TRUE.)
         ELSE
            CALL GK2NCL(ILA(1),COLPRE,IBYTE,IBIT)
         ENDIF
         IF ( ILA(1).LT.0 .OR. KERROR.NE.0) GOTO 9999
         CALL GK2NHI(ILA(2),.TRUE.)
         IF(KERROR.NE.0) GOTO 9999

         DO 1157 J=1,ILA(2)
            CALL GKPKSI(1,ILA,IAT,SDR)
            IAT=IAT+KPDSSZ
            I=I+1
 1157    CONTINUE
         IF(I.LT.NPTS)GOTO 1155
      ELSE
*  Invalid Type code
         KERROR=163
      ENDIF

      IF(KERROR.NE.0) GOTO 9999

      GOTO 9990

* Generalised Drawing Primitive  -  GKSM 16
* -----------------------------
 1160 CONTINUE
      ITYPE = KWKDAT(1,KWKIX) - CIRCLE

*   Check whether CGM opcodes not catered for in GKS
      IF (ITYPE.GE.0) THEN

*   CGM Circle
         IF (ITYPE.EQ.0) THEN
            ILA(1)=-4
            ILA(2)=5
*   Pack GDP identifier & Number of points
            CALL GKPKI(1,ILA,IAT,SDR)
            IAT=IAT+KPDISZ
            CALL GKPKSI(1,ILA(2),IAT,SDR)
            IAT=IAT+KPDSSZ

*   put out the registration points
            RLA(1)=1.0
            RLA(2)=0.0
            RLA(3)=1.0
            RLA(4)=0.0
            RLA(5)=1.0
            RLA(6)=1.0
            CALL GKPKR(6,RLA,IAT,SDR)
            IAT=IAT+6*KPDRSZ

*   Centre
            CALL GK2NHR(RLA(3),VDCX)
            CALL GK2NHR(RLA(4),VDCY)
*   Radius
            CALL GK2NHR(RLA(1),VDC)
*   Work out point on circumference
            RLA(1)=RLA(3) + RLA(1)*QVDCSC(KWKIX)
            RLA(2)=RLA(4)
            CALL GKPKR(4,RLA,IAT,SDR)
            IAT=IAT+4*KPDRSZ
         ELSE

            DO 1163 J=1,6
               CALL GK2NHR(RLA(J),VDCX+MOD(J+1,2))
               IF(KERROR.NE.0) GOTO 9999
 1163       CONTINUE
            IF (ITYPE.EQ.1) THEN
*   CGM Arc 3 point
               ILA(1)=-1
            ELSEIF (ITYPE.EQ.2) THEN
*   CGM Arc 3 point close
               CALL GK2NHI(ITYPE,.TRUE.)
               IF(KERROR.NE.0) GOTO 9999
               IF (ITYPE.EQ.0) THEN
*   Pie
                  ILA(1)=-3
               ELSE
*   Chord
                  ILA(1)=-2
               ENDIF
            ELSEIF (ITYPE.EQ.3) THEN
*   CGM Arc Centre
               CALL GK2NHR(RLA(7),VDC)
               RLA(7) = RLA(7) * QVDCSC(KWKIX)
               CALL GK2NMP(RLA)
               ILA(1)=-1
            ELSEIF (ITYPE.EQ.4) THEN
*   CGM Arc Centre close
               CALL GK2NHR(RLA(7),VDC)
               RLA(7) = RLA(7) * QVDCSC(KWKIX)
               CALL GK2NHI(ITYPE,.TRUE.)
               IF(KERROR.NE.0) GOTO 9999
               IF (ITYPE.EQ.0)THEN
*   CGM Pie
                  CALL GK2NMP(RLA)
                  ILA(1)=-3
               ELSE
*   CGM Chord
                  CALL GK2NMP(RLA)
                  ILA(1)=-2
               ENDIF
            ENDIF

*   Pack GDP identifier & Number of points
            ILA(2)=6
            CALL GKPKI(1,ILA,IAT,SDR)
            IAT=IAT+KPDISZ
            CALL GKPKSI(1,ILA(2),IAT,SDR)
            IAT=IAT+KPDSSZ

*   put out the registration points
            CALL GKPKR(1,1.0,IAT,SDR)
            IAT=IAT+KPDRSZ
            CALL GKPKR(1,0.0,IAT,SDR)
            IAT=IAT+KPDRSZ
            CALL GKPKR(1,1.0,IAT,SDR)
            IAT=IAT+KPDRSZ
            CALL GKPKR(1,0.0,IAT,SDR)
            IAT=IAT+KPDRSZ
            CALL GKPKR(1,1.0,IAT,SDR)
            IAT=IAT+KPDRSZ
            CALL GKPKR(1,1.0,IAT,SDR)
            IAT=IAT+KPDRSZ

*   GDP points
            CALL GKPKR(6,RLA,IAT,SDR)
            IAT=IAT+6*KPDRSZ
            IF(KERROR.NE.0) GOTO 9999

         ENDIF
      ELSE

* -------------------
*   Normal GKS GDP
* -------------------
         ILA(2)=KNOPTS
         CALL GK2NHI(IDGDP,.TRUE.)
         IF(KERROR.NE.0) GOTO 9999
         ILA(1)=IDGDP

*   Pack GDP identifier & Number of points
         CALL GKPKI(1,ILA,IAT,SDR)
         IAT=IAT+KPDISZ
         CALL GKPKSI(1,ILA(2),IAT,SDR)
         IAT=IAT+KPDSSZ

* Input pairs of X,Y values, convert to internal binary
*  format, and put into the array supplied

         RLA(1) = -QVDCOX(KWKIX)*QVDCSX(KWKIX)
         RLA(2) = -QVDCOY(KWKIX)*QVDCSY(KWKIX)
         KXEXP(KWKIX)=KVDEFX(KWKIX)
         KYEXP(KWKIX)=KVDEFX(KWKIX)
         DO 1168 J=1,KNOPTS
           CALL GK2NHR(RLA(3),VDCXL)
           CALL GK2NHR(RLA(4),VDCYL)
           IF(KERROR.NE.0) GOTO 9999
           RLA(1)=RLA(1)+RLA(3)
           RLA(2)=RLA(2)+RLA(4)
           CALL GKPKR(2,RLA,IAT,SDR)
           IAT=IAT+2*KPDRSZ
 1168    CONTINUE
      ENDIF

*   Set data record lengths to zero cos the data record hasnt been
*   defined yet
      ILA(1)=0
      ILA(2)=0
      CALL GKPKSI(2,ILA,IAT,SDR)
      IAT=IAT+2*KPDSSZ

      GOTO 9990

* Line width scaling factor -  GKSM 23
* -------------------------
 1230 CONTINUE
      IF ( KLWSMD(KWKIX).EQ.0 ) THEN
*  If scaling mode is absolute get a VDC and adjust to standard nominal
        CALL GK2NHR(RLA,VDC)
        RLA(1) = RLA(1)*QVDCSC(KWKIX)*1000.0
      ELSE
        CALL GK2NHR(RLA,REALPT)
      ENDIF
      IF(KERROR.NE.0) GOTO 9999
      CALL GKPKR(1,RLA,IAT,SDR)
      IAT=IAT+KPDRSZ
      GOTO 9990

* Marker size scaling factor -  GKSM 27
* --------------------------
 1270 CONTINUE
      IF ( KMSSMD(KWKIX).EQ.0 ) THEN
*  If scaling mode is absolute get a VDC and adjust to standard nominal
        CALL GK2NHR(RLA,VDC)
        RLA(1) = RLA(1)*QVDCSC(KWKIX)*100.0
      ELSE
        CALL GK2NHR(RLA,REALPT)
      ENDIF
      IF(KERROR.NE.0) GOTO 9999
      CALL GKPKR(1,RLA,IAT,SDR)
      IAT=IAT+KPDRSZ
      GOTO 9990

* Text Font & Precision  -  GKSM 30
* ----------------------
 1300 CONTINUE
      CALL GK2NHI(ILA(1),.TRUE.)
      IF(KERROR.NE.0) GOTO 9999

* Pad GKS item so that the CGM element of either Font or Precision
* is translated to the GKS item Text Font and Precision.(It is padded
* with the current Font or Precision, depending on CGM code)
      IF (KWKDAT(1,KWKIX).EQ.TEXFON) THEN

*   Convert font index to relevant GKS Font
         IF ( ILA(1) .GE. 1 .AND. ILA(1) .LE. NFONTS ) THEN
            KITXFN=KRALFN(ILA(1))
            ILA(1)=KITXFN
         ELSE
            KITXFN=ILA(1)
         ENDIF
         ILA(2)=KITXPR
      ELSE
         KITXPR=ILA(1)
         ILA(2)=KITXPR
         ILA(1)=KITXFN
      ENDIF

      CALL GKPKI(1,ILA(1),IAT,SDR)
      IAT=IAT+KPDISZ
      CALL GKPKSI(1,ILA(2),IAT,SDR)
      IAT=IAT+KPDSSZ
      GOTO 9990

* Character spacing and Character expansion  -  GKSM 31, 32
* -----------------------------------------
 1310 CONTINUE
      CALL GK2NHR(RLA,REALPT)
      IF(KERROR.NE.0) GOTO 9999
      CALL GKPKR(1,RLA,IAT,SDR)
      IAT=IAT+KPDRSZ
      GOTO 9990

* Character Vector  -  GKSM 34
* ----------------
*  Can be entered from CHARHEIGHT or CHARORI in CGM
*
 1340 CONTINUE

*   If CGM Character height has been used, translate this to
*   the GKS up and base vectors from the current vector
      IF ( KWKDAT(1,KWKIX).EQ. CHHT ) THEN
*  Get Character height in unscaled World Coords
         CALL GK2NHR(FACTOR,VDC)
*  Convert to NDC
         FACTOR = FACTOR * QVDCSC(KWKIX) /
     :            SQRT( QCCHUX*QCCHUX + QCCHUY*QCCHUY )

*  Scale vector by new height / old height
         RLA(1) = QCCHUX * FACTOR
         RLA(2) = QCCHUY * FACTOR
         RLA(3) = QCCHBX * FACTOR
         RLA(4) = QCCHBY * FACTOR

         DEFCHH(KWKIX) = .FALSE.
*   Otherwise, read & convert the vector using the current height
      ELSE
         CALL GK2NHR(RLA(1),VDCX)
         CALL GK2NHR(RLA(2),VDCY)
         CALL GK2NHR(RLA(3),VDCX)
         CALL GK2NHR(RLA(4),VDCY)
         IF(KERROR.NE.0) GOTO 9999
         FACTOR = SQRT( QCCHUX*QCCHUX + QCCHUY*QCCHUY ) /
     :            SQRT( RLA(1)*RLA(1) + RLA(2)*RLA(2) )
         RLA(1) = RLA(1) * FACTOR
         RLA(2) = RLA(2) * FACTOR
         FACTOR = SQRT( QCCHBX*QCCHBX + QCCHBY*QCCHBY ) /
     :            SQRT( RLA(3)*RLA(3) + RLA(4)*RLA(4) )
         RLA(3) = RLA(3) * FACTOR
         RLA(4) = RLA(4) * FACTOR
      ENDIF
      CALL GKPKR(4,RLA,IAT,SDR)
      IAT=IAT+4*KPDRSZ
      GOTO 9990

* Clip Rectangle  -  GKSM 61
* --------------
 1350 CONTINUE

*   Change the order - in the CGM it is stored as 2 points
*   in GKS it is passed as xmin,xmax,ymin,ymax
      CALL GK2NHR(RLA(1),VDCX)
      CALL GK2NHR(RLA(3),VDCY)
      CALL GK2NHR(RLA(2),VDCX)
      CALL GK2NHR(RLA(4),VDCY)
      IF(KERROR.NE.0) GOTO 9999

      DEFCLP(KWKIX) = .FALSE.
      CALL GKPKR(4,RLA,IAT,SDR)
      IAT=IAT+4*KPDRSZ
      GOTO 9990

* Set Fill Area Style index  - GKSM 39
* -------------------------
 1390 CONTINUE
      CALL GK2NHI(ILA(1),.TRUE.)
      IF(KERROR.NE.0) GOTO 9999

      IF(HATCH(KWKIX)) THEN
*   Juggle the CGM Hatches to fit the GKS ones
         IF(ILA(1).EQ.4) THEN
            ILA(1)=3
         ELSEIF(ILA(1).EQ.3) THEN
            ILA(1)=4
         ENDIF
         IF(ILA(1).GT.0) THEN
            ILA(1)=-ILA(1)
         ENDIF
         KHTCHI(KWKIX)=ILA(1)
      ELSE
         KPATTI(KWKIX)=ILA(1)
      ENDIF

*   Pack it!
 1395 CONTINUE
      CALL GKPKI(1,ILA,IAT,SDR)
      IAT=IAT+KPDISZ
      GOTO 9990

* Pattern size  -  GKSM 41
* ------------
 1410 CONTINUE
      CALL GK2NHR(RLA(1),VDCX)
      CALL GK2NHR(RLA(2),VDCY)
      CALL GK2NHR(RLA(3),VDCX)
      CALL GK2NHR(RLA(4),VDCY)
      IF(KERROR.NE.0) GOTO 9999
      CALL GKPKR(4,RLA,IAT,SDR)
      IAT=IAT+4*KPDRSZ
      DEFPAT(KWKIX) = .FALSE.
      GOTO 9990

* Pattern Reference Point  -  GKSM 42
* -----------------------
 1420 CONTINUE
      CALL GK2NHR(RLA(1),VDCX)
      CALL GK2NHR(RLA(2),VDCY)
      CALL GKPKR(2,RLA,IAT,SDR)
      IAT=IAT+2*KPDRSZ
      DEFREF(KWKIX) = .FALSE.
      GOTO 9990

* Aspect source flags  -  GKSM 43
* -------------------
 1430 CONTINUE

*   Set asfs with the flags that have been changed
      CALL GK2NHI(J,.TRUE.)
      CALL GK2NHI(TEMP,.TRUE.)

*   Change asf from 1 to 0 or 0 to 1 since the CGM has the flags
*   meaning the opposite things
      TEMP=ABS(TEMP-1)

*   See if its a normal asf
      IF (J.LT.506) THEN

*   Do a bit of juggling
         IF (J.EQ.12) THEN
            J=13
         ELSEIF ((J.GT.12).AND.(J.LT.15)) THEN
            J=12
         ENDIF

         IF (J.LT.7) THEN
            J=J+1
         ENDIF
         KSETAS(J,KWKIX)=TEMP

*  Must be a pseudo asf
*  All
      ELSEIF (J.EQ.511) THEN
         DO 1433 I=1,13
            KSETAS(I,KWKIX)=TEMP
 1433    CONTINUE

*  All line
      ELSEIF (J.EQ.510) THEN
         DO 1436 I=1,3
            KSETAS(I,KWKIX)=TEMP
 1436    CONTINUE

*  All marker
      ELSEIF (J.EQ.509) THEN
         DO 1438 I=4,6
            KSETAS(I,KWKIX)=TEMP
 1438    CONTINUE

*  All text
      ELSEIF (J.EQ.508) THEN
         DO 1440 I=7,10
            KSETAS(I,KWKIX)=TEMP
 1440    CONTINUE

*  All fill
      ELSEIF (J.EQ.507) THEN
         DO 1442 I=11,13
            KSETAS(I,KWKIX)=TEMP
 1442    CONTINUE
      ENDIF

*  Checkout whether there are more asfs to come (without moving pointer)
      CALL GK2NNC(TEMP,.FALSE.)
      IF (TEMP.GE.58) THEN
         GOTO 1430
      ENDIF
      DO 1450 J=1,13
         CALL GKPKSI(1,KSETAS(J,KWKIX),IAT,SDR)
         IAT=IAT+KPDSSZ
 1450 CONTINUE
      GOTO 9990

* Pattern Representation  -  GKSM 55
* ----------------------
 1550 CONTINUE
      DO 1552 J=1,5
         CALL GK2NHI(ILA(J),.TRUE.)
 1552 CONTINUE
      IF(KERROR.NE.0) GOTO 9999
      CALL GKPKSI(3,ILA,IAT,SDR)
      IAT=IAT+3*KPDSSZ

*   Check type of list
      IF(ILA(5).NE.0) GOTO 9990

*   Local colour precision
      COLPRE=ILA(4)
      IF (COLPRE.EQ.0) COLPRE = KCOLPR(KWKIX)

      M=ILA(2)
      N=ILA(3)
      NPTS=N*M
      DO 1555 J=1,NPTS
         CALL GK2NHI(ILA(1),.TRUE.)
         CALL GKPKSI(1,ILA,IAT,SDR)
         IAT=IAT+KPDSSZ
 1555 CONTINUE

      GOTO 9990

* Colour representation  -  GKSM 56
* ---------------------
 1560 CONTINUE

*   Index
      CALL GK2NHI(ILA(1),.TRUE.)

*   Find Format
      CALL GK2NHI(ILA(2),.TRUE.)
      IF(ILA(2).GT.1) THEN
         KERROR = 163
         GOTO 9999
      ENDIF
      CALL GK2NRG(RLA,.TRUE.)
      IF(KERROR.NE.0) GOTO 9999
      CALL GKPKSI(1,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ
      CALL GKPKR(3,RLA,IAT,SDR)
      IAT=IAT+3*KPDRSZ
      GOTO 9990

* Workstation Window  -  GKSM 71
* ------------------
 1710 CONTINUE

*   Check if VDC Type is integer, if so, read the points as integers
      IF (KVDCTY(KWKIX).EQ.0) THEN
         DO 1712 J=3,6
            CALL GK2NHI(ILA(J),.TRUE.)
 1712    CONTINUE
*   Convert integers to reals
         DO 1714 J=3,6
            RLA(J) = REAL(ILA(J))
 1714    CONTINUE
      ELSE

*   Read the two points as unscaled VDCs
         DO 1716 J=3,6
            CALL GK2NHR(RLA(J),VDC)
 1716    CONTINUE
      ENDIF
      QVDCOX(KWKIX) = RLA(3)
      QVDCOY(KWKIX) = RLA(4)
      QVDCSX(KWKIX) = 1.0/(RLA(5) - RLA(3))
      QVDCSY(KWKIX) = 1.0/(RLA(6) - RLA(4))
*  Work out scaling factor
      IF ( ABS(QVDCSY(KWKIX)) .LT. ABS(QVDCSX(KWKIX)) ) THEN
         QVDCSC(KWKIX) = ABS(QVDCSY(KWKIX))
         QVDCSX(KWKIX) = SIGN(QVDCSC(KWKIX),QVDCSX(KWKIX))
      ELSE
         QVDCSC(KWKIX) = ABS(QVDCSX(KWKIX))
         QVDCSY(KWKIX) = SIGN(QVDCSC(KWKIX),QVDCSY(KWKIX))
      ENDIF
*   Normalise coodinates
      RLA(1) = (RLA(3)-QVDCOX(KWKIX)) * QVDCSX(KWKIX)
      RLA(2) = (RLA(5)-QVDCOX(KWKIX)) * QVDCSX(KWKIX)
      RLA(3) = (RLA(4)-QVDCOY(KWKIX)) * QVDCSY(KWKIX)
      RLA(4) = (RLA(6)-QVDCOY(KWKIX)) * QVDCSY(KWKIX)

      CALL GKPKR(4,RLA,IAT,SDR)
      IAT=IAT+4*KPDRSZ
      IF(KERROR.NE.0) GOTO 9999

*   If defaults replacement
      IF (DOREP(KWKIX)) THEN

*   Then if the default clipping rectangle
*   hasn't already been set, then set it to the VDC Extent
         IF( DEFCLP(KWKIX) ) THEN
            QCCLXL=RLA(1)
            QCCLXR=RLA(2)
            QCCLYB=RLA(3)
            QCCLYT=RLA(4)
         ENDIF

*   If character height hasn't been set, then set it to 1/100th of
*   the largest side of the VDC rectangle
         IF( DEFCHH(KWKIX) ) THEN
            QCCHH=0.01
            QCCHW=QCCHH*QCCHXP
         ENDIF

*   If pattern size hasn't been set, then set it to the height & width
*   of the VDC rectangle
         IF( DEFPAT(KWKIX) ) THEN
            QCPAWX=RLA(2)-RLA(1)
            QCPAHY=RLA(4)-RLA(3)
         ENDIF

*   If fill reference point hasn't been set, then set it to the lower
*   left corner of the VDC rectangle
         IF( DEFREF(KWKIX) ) THEN
            QCPAX=RLA(1)
            QCPAY=RLA(3)
         ENDIF

      ENDIF

      GOTO 9990

* Workstation Viewport  -  GKSM 72
* --------------------
 1720 CONTINUE

      CALL GK2NHR(RLA(1),VDCX)
      CALL GK2NHR(RLA(2),VDCY)
      CALL GK2NHR(RLA(3),VDCX)
      CALL GK2NHR(RLA(4),VDCY)
      IF(KERROR.NE.0) GOTO 9999
      CALL GKPKR(4,RLA,IAT,SDR)
      IAT=IAT+4*KPDRSZ
      GOTO 9990

* User item  -  GKSM 100+
* ---------
 2010 CONTINUE

*   Advance pointer to allow for open character string code
      KCDPTR(KWKIX)=KCDPTR(KWKIX)+2

*   Pack the data record into the character array
      J = 0
      DO 2012 I = 0,KXCHA,80
         J = J + 1
         N = MIN(80,KXCHA-I)
         IF ( N .GT. 0 ) THEN
*   Get up to 80 bytes of the Data record
            CALL GK2NCH(N,CSTR(1))
            IF(KERROR.NE.0) GOTO 9999
            SDR(J)(1:N) = CSTR(1)(1:N)
         ENDIF
 2012 CONTINUE
      GOTO 9990

* INQUIRE WORSTATION CONNECTION AND TYPE  -  KQWKC (130)
* --------------------------------------

 3000 CONTINUE
      KWI1=KCID(KWKIX)
      KWI2=KWKTYP
      GOTO 9999

* INQUIRE WORKSTATION CATEGORY  -  KQWKCA (170)
* ----------------------------

 4000 CONTINUE
      KWI1=GMI
      GOTO 9999

* Workstation is not of category MO
 9932 KERROR=32
      GOTO 9999

* Workstation is of category MI
 9933 KERROR=33
      GOTO 9999

* Workstation is not of category OUTIN
 9937 KERROR=37
      GOTO 9999

* Workstation is neither of category INPUT nor of category OUTIN
 9938 KERROR=38
      GOTO 9999

* Workstation is neither of category OUTPUT nor of category OUTIN
 9939 KERROR=39
      GOTO 9999

* Error: invalid metafile item
 9463 CONTINUE
      KERROR = 163
      GOTO 9999

 9990 GOTTYP = 0
      GOTO 9999

 9999 CONTINUE

* Save local variables in COMMON
      IF(IENT.LT.KQWKDU) THEN
         KWKDAT(2,KWKIX) = ITYPE
         KWKDAT(3,KWKIX) = GOTTYP
      END IF

      RETURN
      END
