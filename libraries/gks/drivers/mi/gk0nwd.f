C# IL>=a, OL>=0
      SUBROUTINE GK0NWD (IENT,NID,IDAT,NRD,RX,RY,NCD,SDR)
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
*     Input Workstation Driver for Annexe E type metafiles.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  DSG   Original version stabilized
*     30/06/83  DSG   Extensively changed GK0NIL to avoid using PACK utility
*     09/11/83  DSG   Binding change to Character Array Data Records.
*     10/11/83  DSG   In GK0NIL, binding change for the data record's
*                     character array
*     13/01/84  DSG   Pattern representation stack allocation
*     17/01/84  DSG   Local character string replaced by CSTR in GK0NFH
*     19/01/84  DSG   Bug in Polyline code fixed
*     24/01/84  DSG   Metafile index removed from user item
*     15/02/84  DSG   Bug fix I75
*     20/03/84  DSG   Bug fix S27
*     21/03/84  DSG   Bug fix S28
*     27/03/84  DSG   Bug fix I162 - Internal READ format for GDP .
*                     Code for GDP and ESCAPE data records skipped;
*                     (See Bug fix S34).
*     17/04/84  RSK   Bug fix ??? - Set Pattern Representation
*     25/04/84  PGLS  Bug fix I189: ignore (rather than 20001) entry points
*                     47-51, segment entries.
*     02/05/84  PGLS  Set colour repn bug, label 1560, found by DSG.
*     16/01/86  DSG   S176: Change in integer format used in Cell Array
*                     (CMS compiler restriction)
*     06/03/86  DSG   Local string in GK0NBU replaced by CMBUFF
*     13/03/86  DSG   Metafile item field lengths passed to GK0NIL in common
*                     S142: Some local variables put into COMMON-
*                     item type, field lengths, Escape data lengths.
*                     In GK0NFH, metafile header field lengths put into common
*     19/03/86  DSG   All calls to GKBUG commented out (corrects problem with
*                     GQTXX causing error 2001), and lower case text removed.
*     02/05/86  RMK   Changed to use new interface to GKIOOP/GKIOCL.
*     02/06/86  RMK   Changed to use lower entries in KWKDAT.
*     19/06/86  RMK   Incorporated maintenance logs from other GK0Nxx
*                     routines. Removed unused local variable SNAME.
*                     Removed GKBUG code.
*     20/01/87  CJC   IS conversion. New specification of data record
*                     length by character count rather than dimension.
*                     GDP record amended with   all counts at the front
*                     required changed use of KWKDAT.
*     20/01/87  CJC   IS conversion. Metafile workstation identifier
*                     removed from front of item data record.
*     08/04/87  RMK   Added GKWKE.PAR to GK0NWD. Added GKERR.CMN to
*                     GK0NIL and changed IFD to IFLD.
*     18/05/87  DCS   Corrected error number for INQ WK CLASSN (S265).
*     28/05/87  DCS   Error numbers changed for IS.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     02/10/87  DSG   S280: to allow the input of negative
*                     integers from the GKSM (GSTXR, GSFAR, and GGDP).
*     16/10/87  DSG   S278: (Set Pattern Reference Point) fixed.
*     27/10/87  DSG   S288: As for S280 - GSTXFP and GSFASI.
*     28/10/87  DSG   S287: GK0NIL - fix of errors in the calculation
*                     of the packed data record for some items.
*     30/10/87  DSG   S285: GK0NWD - message error chunking item fixed;
*                     test for error 2001 in Read Item entrypoint.
*                     S286: GK0NWD -  conversion of MNCD*80 to MNCD
*                     everywhere. Change in the test for binding error
*                     2001. LENGTH has to go into common.
*     06/11/87  DSG   Generalized to handle any I or F formats for
*                     integers and reals.
*     10/11/87  DSG   S289: GK0NWD - large item problem fixed (effects
*                     GESC, GPL, GPM, GFA, GCA, GDP, GSPLR entrypoints).
*     17/11/87  DSG   Polyline - number of points for truncated item
*     17/11/87  RMK   Removed extra ENDIF in GK0NWD and changed
*                     local character declaration in GK0NIL.
*     01/12/87  DSG   S326: Stack deallocated after any error in
*                     external READ (several entrypoints).
*                     S303: Inequality changed at Text and Message
*                     entrypoints to allow single character strings.
*     03/12/87  DSG   S325: Message - code reworked to remove faults.
*     23/12/87  DSG   Generalised to handle a GKSM with variable length
*                     records (but padded with trailing blanks).
*     10/03/88  DSG   Generic driver to read Annexe E metafiles:
*                     RAL GKS (type 10), UNIRAS GKS (type 15) and
*                     SUN Microsystems GKS (type 16).
*     15/03/88  DSG   S317: Error 33 no longer returned when
*                     interpreting a Delete Segment metafile item.
*     21/03/88  RMK   Changed to use enumerated names for entrypoints
*                     at start-up (S312).
*                     Moved initialisation of locals from KWKDAT into
*                     get item type and read item entries (S313).
*     05/08/88  DSG   Changes to allow input of current UNIGKS GKSM
*                     (version 4.0).
*     17/11/88  NMH   Allow for use of inquire functions when no input
*                     workstation currently open.
*     17/05/89  DSG   Cell array modified to skip rest of item if there
*                     is more than the expected number of colour
*                     indices (allows the input of GTS-GRAL GKSMs).
*     17/05/89  RTP   Changes to allow input of current UNIRAS GKSM
*                     (version 5.4).
*     23/05/89  RTP   Change Sun Message format - length omitted
*     16/06/89  RTP   Determine type of metafile in GK0NFH so that only
*                     workstation type 10 is needed.
*                     Change internal writes to use (I2) instead of (I1)
*     19/06/89  RTP   In GK0NFH, check for valid workstations and set flags
*                     In GK0NIL, check for Sun GKSM and set message
*                     length correctly.
*     28/07/89  RTP   Check for end of input buffer before READ. Stops
*                     GKS error 162 if last buffer is full (GK0NBU).
*     22/08/89  RMK   Removed NMH change, as this was an
*                     alternative fix for S313.
*                     Remove unused local variables.
*     12/10/89  RMK   Changed user Item entry so that it doesn't pack
*                     the user data before returning it (S365).
*     21/03/90  RTP   Use GKPKI for linetype and Markertype which may
*                     be negative (S387).
*     10/10/90  RTP   Add check for GTS-GRAL metafiles for cell arrays
*                     and set representations which not as per Annex E
*     26/10/90  PLP   Commented out decaration of IOVER - now not used
*                     because of commented out section of code after label
*                     1156.
*
*    Last Modified on  10 Oct 1990  at  15:10:58
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
*  DRIVER SUBROUTINES
*  ------------------
*
*     GK0NFH  - Decodes the metafile header
*     GK0NIL  - Called at GTITM entrypoint to calculate the internal
*               packed length of the RDITM data record.
*     GK0NBU  - Buffer handler for metafile input
*     GK0NIR  - Converts Integer values to real if conversion applies
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/    Workstation communication area
*     Read   /GKYWCB/    Metafile input workstation index
*     Read   /GKYERR/    KERROR
*     Modify /GKYWKD/    Workstation derived data - KWKDAT usage:
*                        1 - "GKSM" field length
*                        2 - length of item type indicator field
*                        3 - length of item length indicator field
*                        4 - IFLD, length of integer fields
*                        5 - IRFLD, length of real fields
*                        (Entries 1 to 5 are read from the metafile header
*                        by GK0NIL.)
*                        6 - Escape/GDP function indentifier
*                        7 - Escape - number of integers in data record
*                            GDP - number of points
*                        8 - Escape - number of reals in data record
*                            GDP - number of integers in data record
*                        9 - GDP - number of reals in data record
*                        10 - ITYPE, metafile item type
*                        11 - GOTTYP, 0 if item type and length need to
*                                        be got
*                                      1 if get done and item type and
*                                        length (and additional data in
*                                        the case of escape/GDP) got and
*                                        read comes next
*                        12 - ILEN, length of GKSM item
*                        13 - unused
*                        14 - buffer pointer used in GK0NBU
*                        15 - LENGTH, item data record length
*                        16 - Flag used to indicate type of Annexe E
*                             metafile being input.
*                        17 - Integer equivalent of 0.0
*                        18 - Integer equivalent of 1.0 - 0.0
*                             0 if reals input as real
*     Modify /GKYWSL/    Workstation state list
*     Modify /GKYSTK/    Stack
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkfls.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkpid.par'
      INCLUDE '../../include/gkwke.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwcb.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkstk.cmn'
*
*  LOCALS
*  ------
*     IFLD   Length of integer field in item data
*     IRFLD   Length or real field in item data
*     IFUNC    =1, Initialise input buffer; =2, Read from
*                  buffer; =3, Skip over item data record
*     NCH      Number of characters wanted from input buffer
*     GOTTYP   Item flag used to identify the current item
*              ( 0 if item type and length is next to be input,
*                1 if item data is next)
*     IOVER    Used to record any extra parameters in item for skipping
*
*
      CHARACTER*17 SFMT(18)
*
      INTEGER IFLD, IRFLD, NCH, IFUNC, GOTTYP
*      INTEGER IOVER
*
*  Integer local array
      INTEGER ILA(13)
*
*  Real local array
      REAL RLA(6)
*
* Stack variables
      INTEGER ISET
*
* Position variable in packing routines
      INTEGER IAT

* Setting of KWKDAT(16,KWKIX):
*     IRAL     Indicates RAL GKS Annex E metafile and GKS-GRAL
*     ISUN     Indicates SUN Microsystems metafile
*     IUNI     Indicates UNIRAS GKS metafile or UNIGKS
*     IGRAL    Indicates GTS-GRAL GKS2D Metafile
*
      INTEGER LENDAT,ITYPE,ILEN,IESC(3),NCHARS,I,J,N,M,
     :         NUMPTS,IDGDP,LINT,LREAL,LENGTH,JUNK,
     :         NINT,MNCD,NN,NP,NPTS,IRSZ,IISZ,ICSZ,K,L
      CHARACTER SFMTH,SFMTT,SFMTL,SGTFMT*10,SGKSM*4
      CHARACTER SFMTIC*3, SFMTRC*5

      CHARACTER SJUNK(1)

* Different types of metafile to handle
      INTEGER IRAL, ISUN, IUNI, IGRAL
      PARAMETER (IRAL = 1, ISUN = 2, IUNI = 3, IGRAL = 4)

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


      IFUNC = 2

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

      IF(IENT.EQ.KQPXAD.OR.IENT.EQ.KQPXA) GOTO 9939

      GOTO(3000,9933,9933,9933,9933,9933,9933,9933,9939,9933,
     :     9933,9933,9933,9933,9933,9933,9933,9938,9938,9938,
     :     9938,9937,9938) IENT-129

      IF(IENT.EQ.KQWKCA) GOTO 4000
      IF(IENT.EQ.KQWKCL) GOTO 9939
      IF(IENT.EQ.KQMDS) GOTO 9933
      IF(IENT.GE.173.AND.IENT.LE.191) GOTO 9939
      IF(IENT.GE.192.AND.IENT.LE.198) GOTO 9938

* Invalid entrypoint
      GOTO 9999

* OPEN WORKSTATION
* ----------------
   10 CONTINUE
      KCID(KWKIX) = KWI1

      CALL GKIOOP(KFWKFR,KCID(KWKIX),KWCID(KWKIX))

      IF(KERROR.EQ.0) THEN

* Initialise the input buffering
        IFUNC = 1
        CALL GK0NBU(IFUNC,JUNK,SJUNK)

* Initialise item type flag to 'not yet known'
        GOTTYP = 0

* Deal with the file header.
        CALL GK0NFH


* Initialise other locals
        ITYPE = KNIL
        ILEN = KNIL

      ENDIF

*  Workstation category
      KWI1 = GMI

      GOTO 9995

* CLOSE WORKSTATION
* ----------------
   20 CONTINUE
      CALL GKIOCL(KFWKFR,KCID(KWKIX),KWCID(KWKIX))
      GOTO 9999

* ESCAPE
* ------
  110 CONTINUE
      GOTO 9999

* GET ITEM TYPE FROM GKSM
* -----------------------
* Data expected:   None
* Data returned:   KWI1   item type
*                  KWI2   item data record length

  920 CONTINUE
*
* Initialise local variables from KWKDAT
      ITYPE  = KWKDAT(10,KWKIX)
      GOTTYP = KWKDAT(11,KWKIX)
      ILEN   = KWKDAT(12,KWKIX)

* Test flag To see if current item Type has been got previously
      IF(GOTTYP.EQ.0) THEN
         WRITE(SFMTH,'(I1)') KWKDAT(1,KWKIX)
         WRITE(SFMTT,'(I1)') KWKDAT(2,KWKIX)
         WRITE(SFMTL,'(I1)') KWKDAT(3,KWKIX)
         SGTFMT='(A'//SFMTH//',I'//SFMTT//',I'//SFMTL//')'
         LENDAT = KWKDAT(1,KWKIX)+KWKDAT(2,KWKIX)+KWKDAT(3,KWKIX)
         IF (KWKDAT(16,KWKIX).EQ.IUNI .OR.
     :       KWKDAT(16,KWKIX).EQ.ISUN) IFUNC = 4
         CALL GK0NBU (IFUNC,LENDAT,CSTR)
         IF(KERROR.NE.0) GOTO 9995
         READ (CSTR(1) (:LENDAT),SGTFMT) SGKSM, ITYPE, ILEN
         GOTTYP = 1
      ENDIF

      KWI1=ITYPE

* Calculate the item Data Length in the internal form
* ( N.B. - GDP not fully implemented yet )
      CALL GK0NIL (ITYPE,ILEN,LENGTH)
      KWI2=LENGTH
      KWKDAT(15,KWKIX) = LENGTH
      GO TO 9995

* READ ITEM FROM GKSM
* -------------------
* Data expected:
*  KWI1 - Length of data record in characters
*   SDR - data record

  930 CONTINUE
      IFLD   = KWKDAT(4,KWKIX)
      IRFLD  = KWKDAT(5,KWKIX)
      ITYPE  = KWKDAT(10,KWKIX)
      GOTTYP = KWKDAT(11,KWKIX)
      ILEN   = KWKDAT(12,KWKIX)
      LENGTH = KWKDAT(15,KWKIX)

      MNCD = KWI1

      IF(MNCD.LT.0) THEN
*  Item data record length is invalid
      KERROR=166
      GOTO 9995
      ENDIF

      MNCD = MIN0(LENGTH, MNCD)

      IF(MNCD.GT.NCD*80) THEN
*  Not enough space has been made available
      KERROR=2001
      GOTO 9995
      ENDIF

      IF(GOTTYP.EQ.0) THEN
* Get item data type and record length
        WRITE(SFMTH,'(I1)') KWKDAT(1,KWKIX)
        WRITE(SFMTT,'(I1)') KWKDAT(2,KWKIX)
        WRITE(SFMTL,'(I1)') KWKDAT(3,KWKIX)
        SGTFMT='(A'//SFMTH//',I'//SFMTT//',I'//SFMTL//')'
        LENDAT = KWKDAT(1,KWKIX)+KWKDAT(2,KWKIX)+KWKDAT(3,KWKIX)
        IF (KWKDAT(16,KWKIX).EQ.IUNI .OR.
     :      KWKDAT(16,KWKIX).EQ.ISUN) IFUNC = 4
        CALL GK0NBU (IFUNC,LENDAT,CSTR)
        IF(KERROR.NE.0) GOTO 9995
        READ (CSTR(1)(:LENDAT),SGTFMT) SGKSM, ITYPE, ILEN
        GOTTYP = 1
        IF (KWKDAT(16,KWKIX).EQ.IUNI .OR.
     :      KWKDAT(16,KWKIX).EQ.ISUN) IFUNC = 2
      ENDIF

      IF(MNCD.EQ.0) THEN
* Skip item data record
        IFUNC=3
        N=KWCST
        DO 935 I=1,ILEN,KWCST
          IF(ILEN-I.LT.N) N=ILEN+I-1
          CALL GK0NBU(IFUNC,N,CSTR)
          IF(KERROR.NE.0) GOTO 9995
  935   CONTINUE
        GOTTYP = 0
        GOTO 9995
      ENDIF

      IAT=1

* Here construct every type of format specification that will
* be used in the internal READ statements.

         WRITE(SFMTIC,'(A1,I2)') 'I',IFLD

         IF (KWKDAT(16,KWKIX).EQ.IUNI.AND.KWKDAT(18,KWKIX).EQ.0) THEN
            WRITE(SFMTRC,'(A1,I2,A2)') 'F',IRFLD,'.7'
         ELSE
            WRITE(SFMTRC,'(A1,I2,A2)') 'F',IRFLD,'.0'
         ENDIF

         SFMT(1)='('//SFMTIC//')'
         SFMT(2)='(2'//SFMTIC//')'
         SFMT(3)='(3'//SFMTIC//')'
         SFMT(4)='(4'//SFMTIC//')'
         SFMT(5)='(13'//SFMTIC//')'
         SFMT(6)='('//SFMTRC//')'
         SFMT(7)='(2'//SFMTRC//')'
         SFMT(8)='(4'//SFMTRC//')'
         SFMT(9)='('//SFMTIC//','//SFMTRC//')'
         SFMT(10)='('//SFMTIC//',3'//SFMTRC//')'
         SFMT(11)='('//SFMTIC//',6'//SFMTRC//')'
         SFMT(12)='(2'//SFMTRC//','//SFMTIC//')'
         SFMT(13)='(6'//SFMTRC//',2'//SFMTIC//')'
         SFMT(14)='(2'//SFMTIC//','//SFMTRC//','//SFMTIC//')'
         SFMT(15)='(3'//SFMTIC//',2'//SFMTRC//','//SFMTIC//')'
         SFMT(16)='(16(16'//SFMTIC//'))'
         SFMT(17)='(112'//SFMTRC//')'
         SFMT(18)='(113'//SFMTRC//')'

* Conditional GOTO over item code. Items of similar format are
* grouped.

      IF(ITYPE.EQ.0) GOTO 1020
      GOTO (1010,1020,1010,1040,1050,1060,9463,9463,9463,9463,
     :      1110,1110,1130,1110,1150,1160,9463,9463,9463,9463,
     :      1010,1030,1230,1010,1010,1030,1230,1010,1010,1300,
     :      1230,1230,1010,1340,1010,1040,1010,1010,1390,1010,
     :      1340,1420,1430,1010,9463,9463,9463,9463,9463,9463,
     :      1510,1510,1530,1540,1550,1560,9463,9463,9463,9463,
     :      1340,9463,9463,9463,9463,9463,9463,9463,9463,9463,
     :      1340,1340,9463,9463,9463,9463,9463,9463,9463,9463,
     :      1010,1020,1040,1010,9463,9463,9463,9463,9463,9463,
     :      1910,1040,1040,1940,1040,9463,9463,9463,9463,9463),
     :      ITYPE
      IF (ITYPE.GT.100) GOTO 2010


* Metafile items whose data record consists of one positive integer
* -----------------------------------------------------------------
* value only
* ----------
 1010 CONTINUE
      NCH=IFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ (CSTR,SFMT(1)) ILA(1)
      CALL GKPKSI(1,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ
      GOTO 9990

* Metafile items having a zero length data record
* -----------------------------------------------
 1020 CONTINUE
      GOTO 9990

* Metafile items whose data record consists of one integer
* --------------------------------------------------------
* value only
* ----------
 1030 CONTINUE
      NCH=IFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ (CSTR,SFMT(1)) ILA(1)
      CALL GKPKI(1,ILA,IAT,SDR)
      IAT=IAT+KPDISZ
      GOTO 9990

* Metafile items whose data record consists of two integer
* -------------------------------------------------------
* values only
* -----------
 1040 CONTINUE
      NCH=2*IFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ(CSTR,SFMT(2)) ILA(1),ILA(2)
      CALL GKPKSI(2,ILA,IAT,SDR)
      IAT=IAT+2*KPDSSZ
      GOTO 9990

* Message
* -------
 1050 CONTINUE
C
C  Don't read Number of characters for Sun, They've got it wrong
C
      IF ( KWKDAT(16,KWKIX) .EQ. ISUN ) THEN
         ILA(1) = KWKDAT(12,KWKIX)
      ELSE
         NCH=IFLD
         CALL GK0NBU(IFUNC,NCH,CSTR)
         IF(KERROR.NE.0) GOTO 9995
         READ (CSTR,SFMT(1)) ILA(1)
      ENDIF

      CALL GKPKSI(1,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ
      NCHARS = ILA(1)

      NN = KWCST
      N  = NN
      DO 1052 I=1,NCHARS,NN
         IF((NCHARS-I).LT.NN) N=NCHARS-I+1
         CALL GK0NBU(IFUNC,N,CSTR)
         IF(KERROR.NE.0) GOTO 9995
         IF(MNCD-IAT+1.GE.KPDCSZ) THEN
           N=MIN0(N,KPDCSZ*((MNCD-IAT+1)/KPDCSZ))
           CALL GKPKC(1,N,CSTR,IAT,SDR)
           IAT=IAT+N*KPDCSZ
         ENDIF
 1052 CONTINUE

      GOTO 9990

* Escape
* ------
 1060 CONTINUE
      IESC(2) = KWKDAT(7,KWKIX)
      IESC(3) = KWKDAT(8,KWKIX)

      ILA(1) = KWKDAT(6,KWKIX)
      CALL GKPKI(1,ILA,IAT,SDR)
      IAT=IAT+KPDISZ
      ILA(1) = KWKDAT(7,KWKIX)
      ILA(2) = KWKDAT(8,KWKIX)
      CALL GKPKI(2,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ

      IF(IESC(2).EQ.0) GOTO 1063

      NPTS=IESC(2)
      CALL GKSTQ(IRSZ,IISZ,ICSZ)
      IF(IISZ.LT.IESC(2)) NPTS=IISZ
      CALL GKSTAL(KINTGS,NPTS,ISET)

      NN=KWCST/IFLD
      NP=NPTS

      DO 1064 J=1,IESC(2),NPTS
      N = NN
        IF(IESC(2)-J.LT.NPTS) NP=IESC(2)-J+1
        DO 1065 K=1,NP,NN
          IF(NP-K.LT.NN) N=NP-K+1
          CALL GK0NBU(IFUNC,N*IFLD,CSTR)
          IF(KERROR.NE.0) GOTO 1066
          READ(CSTR,SFMT(16)) (KSTACK(ISET+(K-1)+L-1), L=1,N)
 1065   CONTINUE
        IF(MNCD-IAT+1.GE.KPDSSZ) THEN
          NP=MIN0(NP,(MNCD-IAT+1)/KPDSSZ)
          CALL GKPKSI(NP,KSTACK(ISET),IAT,SDR)
          IAT=IAT+NP*KPDSSZ
        ENDIF
 1064 CONTINUE

 1066 CALL GKSTDA(KINTGS,ISET)
      IF(KERROR.NE.0) GOTO 9995

 1063 IF(IESC(3).EQ.0) GOTO 1069

      NPTS=IESC(3)
      CALL GKSTQ(IRSZ,IISZ,ICSZ)
      IF(IRSZ.LT.IESC(3)) NPTS=IRSZ
      CALL GKSTAL(KREALS,NPTS,ISET)

      NN=KWCST/IRFLD
      NP=NPTS

      DO 1067 J=1,IESC(3),NPTS
        N=NN
        IF(IESC(3)-J.LT.NPTS) NP=IESC(3)-J+1
        DO 1068 K=1,NP,NN
          IF(NP-K.LT.NN) N=NP-K+1
          CALL GK0NBU(IFUNC,N*IRFLD,CSTR)
          IF(KERROR.NE.0) GOTO 1070
          READ(CSTR,SFMT(18)) (QSTACK(ISET+(K-1)+L-1), L=1,N)
 1068   CONTINUE
        IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(NP, QSTACK(ISET) )
        IF(MNCD-IAT+1.GE.KPDRSZ) THEN
          NP=MIN0(NP,(MNCD-IAT+1)/KPDRSZ)
          CALL GKPKR(NP,QSTACK(ISET),IAT,SDR)
          IAT=IAT+NP*KPDRSZ
        ENDIF
 1067 CONTINUE

 1070 CALL GKSTDA(KREALS,ISET)
      IF(KERROR.NE.0) GOTO 9995

 1069 GOTO 9990

* Polyline, Polymarker, Fill Area
* -------------------------------
 1110 CONTINUE
      NCH=IFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      READ (CSTR,SFMT(1)) NUMPTS
      ILA(1)=NUMPTS
      CALL GKPKSI(1,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ
      NPTS=NUMPTS
      CALL GKSTQ(IRSZ,IISZ,ICSZ)
      IF(IRSZ.LT.NUMPTS*2) NPTS=IRSZ/2
      CALL GKSTAL(KREALS,NPTS*2,ISET)

      NN=KWCST/(2*IRFLD)
      NP=NPTS

* Input pairs of X,Y values, convert to internal binary
*  format, and put into the array supplied

      DO 1112 J=1,NUMPTS,NPTS
        N=NN
        IF(NUMPTS-J.LT.NPTS) NP=NUMPTS-J+1
        DO 1114 K=1,NP,NN
          IF(NP-K.LT.NN) N=NP-K+1
          CALL GK0NBU(IFUNC,2*N*IRFLD,CSTR)
          IF(KERROR.NE.0) GOTO 1116
          READ(CSTR,SFMT(17)) (QSTACK(ISET+(K-1)*2+L-1), L=1,N*2)
 1114   CONTINUE
        IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(2*NP, QSTACK(ISET) )
        IF(MNCD-IAT+1.GE.KPDRSZ*2) THEN
          NP=MIN0(NP,(MNCD-IAT+1)/(KPDRSZ*2))
          CALL GKPKR(NP*2,QSTACK(ISET),IAT,SDR)
          IAT=IAT+NP*2*KPDRSZ
        ENDIF
 1112 CONTINUE

 1116 CALL GKSTDA(KREALS,ISET)
      IF(KERROR.NE.0) GOTO 9995

      GOTO 9990

* Text
* ----
 1130 CONTINUE
      NCH=2*IRFLD+IFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ(CSTR,SFMT(12)) RLA(1),RLA(2),ILA(1)
      IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(2, RLA(1) )
      CALL GKPKR(2,RLA,IAT,SDR)
      IAT=IAT+2*KPDRSZ
      CALL GKPKSI(1,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ
      NCHARS=ILA(1)

        NN=KWCST
        N=NN
        DO 1132 I=1,NCHARS,NN
          IF((NCHARS-I).LT.NN) N=NCHARS-I+1
          CALL GK0NBU(IFUNC,N,CSTR)
          IF(KERROR.NE.0) GOTO 9995
          IF(MNCD-IAT+1.GE.KPDCSZ) THEN
            N=MIN0(N,KPDCSZ*((MNCD-IAT+1)/KPDCSZ))
            CALL GKPKC(1,N,CSTR,IAT,SDR)
            IAT=IAT+N*KPDCSZ
          ENDIF
 1132   CONTINUE

      GOTO 9990

* Cell Array
* ----------
 1150 CONTINUE
      NCH=6*IRFLD+2*IFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ (CSTR,SFMT(13)) (RLA(J),J=1,6),M,N
      IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(6, RLA(1) )
      ILA(1)=M
      ILA(2)=N
      CALL GKPKR(6,RLA,IAT,SDR)
      IAT=IAT+6*KPDRSZ
      CALL GKPKSI(2,ILA,IAT,SDR)
      IAT=IAT+2*KPDSSZ
*  Skip 4 integers for GTS-GRAL metafiles
      IF( KWKDAT(16,KWKIX).EQ.IGRAL ) THEN
         NCH=4*IFLD
         CALL GK0NBU(IFUNC,NCH,CSTR)
         READ(CSTR,SFMT(4)) (ILA(J), J=1,4)
      ENDIF

      NPTS=M*N
      CALL GKSTQ(IRSZ,IISZ,ICSZ)
      IF(IISZ.LT.M*N) NPTS=IISZ
      CALL GKSTAL(KINTGS,NPTS,ISET)

      NN=KWCST/IFLD
      NP=NPTS

      DO 1152 J=1,M*N,NPTS
        NINT=NN
        IF(M*N-J.LT.NPTS) NP=M*N-J+1
        DO 1154 K=1,NP,NN
          IF(NP-K.LT.NN) NINT=NP-K+1
          CALL GK0NBU(IFUNC,NINT*IFLD,CSTR)
          IF(KERROR.NE.0) GOTO 1156
          READ(CSTR,SFMT(16)) (KSTACK(ISET+(K-1)+L-1), L=1,NINT)
 1154   CONTINUE
        IF(MNCD-IAT+1.GE.KPDSSZ) THEN
          NP=MIN0(NP,(MNCD-IAT+1)/KPDSSZ)
          CALL GKPKSI(NP,KSTACK(ISET),IAT,SDR)
          IAT=IAT+NP*KPDSSZ
        ENDIF

 1152 CONTINUE

 1156 CALL GKSTDA(KINTGS,ISET)

* If any of the item remains, skip over for GTS-GRAL problem
*     IOVER=IOVER-6*IRFLD-(2+M*N)*IFLD
*     IF ( KWKDAT(16,KWKIX) .EQ. IGRAL ) IOVER = IOVER - 4*IFLD
*     IF(IOVER.GT.0) THEN
*        IFUNC = 3
*        CALL GK0NBU(IFUNC,IOVER,CSTR)
*     ENDIF
      IF(KERROR.NE.0) GOTO 9995

      GOTO 9990

* Generalised Drawing Primitive
* -----------------------------
 1160 CONTINUE
      IDGDP = KWKDAT(6,KWKIX)
      NUMPTS = KWKDAT(7,KWKIX)
      LINT = KWKDAT(8,KWKIX)
      LREAL = KWKDAT(9,KWKIX)

      ILA(1)=IDGDP
      ILA(2)=NUMPTS
      CALL GKPKI(1,ILA,IAT,SDR)
      IAT=IAT+KPDISZ
      CALL GKPKSI(1,ILA(2),IAT,SDR)
      IAT=IAT+KPDSSZ

      NPTS=NUMPTS
      CALL GKSTQ(IRSZ,IISZ,ICSZ)
      IF(IRSZ.LT.NUMPTS*2) NPTS=IRSZ/2
      CALL GKSTAL(KREALS,NPTS*2,ISET)

      NN=KWCST/(2*IRFLD)
      NP=NPTS

* Input pairs of X,Y values, convert to internal binary
*  format, and put into the array supplied

      DO 1162 J=1,NUMPTS,NPTS
      N=NN
        IF(NUMPTS-J.LT.NPTS) NP=NUMPTS-J+1
        DO 1164 K=1,NP,NN
          IF(NP-K.LT.NN) N=NP-K+1
          CALL GK0NBU(IFUNC,2*N*IRFLD,CSTR)
          IF(KERROR.NE.0) GOTO 1169
          READ(CSTR,SFMT(17)) (QSTACK(ISET+(K-1)*2+L-1), L=1,N*2)
 1164   CONTINUE
        IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(2*NP, QSTACK(ISET) )
        IF(MNCD-IAT+1.GE.KPDRSZ*2) THEN
          NP=MIN0(NP,(MNCD-IAT+1)/(KPDRSZ*2))
          CALL GKPKR(NP*2,QSTACK(ISET),IAT,SDR)
          IAT=IAT+NP*2*KPDRSZ
        ENDIF
 1162 CONTINUE

 1169 CALL GKSTDA(KREALS,ISET)
      IF(KERROR.NE.0) GOTO 9995

      ILA(1)=LINT
      ILA(2)=LREAL
      CALL GKPKSI(2,ILA,IAT,SDR)
      IAT=IAT+2*KPDSSZ

      IF(LINT.EQ.0) GOTO 1161

      NPTS=LINT
      CALL GKSTQ(IRSZ,IISZ,ICSZ)
      IF(IISZ.LT.LINT) NPTS=IISZ
      CALL GKSTAL(KINTGS,NPTS,ISET)

      NN=KWCST/IFLD
      NP=NPTS

      DO 1165 J=1,LINT,NPTS
      N=NN
        IF(LINT-J.LT.NPTS) NP=LINT-J+1
        DO 1166 K=1,NP,NN
          IF(NP-K.LT.NN) N=NP-K+1
          CALL GK0NBU(IFUNC,N*IFLD,CSTR)
          IF(KERROR.NE.0) GOTO 1172
          READ(CSTR,SFMT(16)) (KSTACK(ISET+(K-1)+L-1), L=1,N)
 1166   CONTINUE
        IF(MNCD-IAT+1.GE.KPDSSZ) THEN
          NP=MIN0(NP,(MNCD-IAT+1)*KPDSSZ)
          CALL GKPKSI(NP,KSTACK(ISET),IAT,SDR)
          IAT=IAT+NP*KPDSSZ
        ENDIF
 1165 CONTINUE

 1172 CALL GKSTDA(KINTGS,ISET)
      IF(KERROR.NE.0) GOTO 9995

 1161 IF(LREAL.EQ.0) GOTO 1163

      NPTS=LREAL
      CALL GKSTQ(IRSZ,IISZ,ICSZ)
      IF(IRSZ.LT.LREAL) NPTS=IRSZ
      CALL GKSTAL(KREALS,NPTS,ISET)

      NN=KWCST/IRFLD
      NP=NPTS

      DO 1167 J=1,LREAL,NPTS
        N=NN
        IF(LREAL-J.LT.NPTS) NP=LREAL-J+1
        DO 1168 K=1,NP,NN
          IF(NP-K.LT.NN) N=NP-K+1
          CALL GK0NBU(IFUNC,N*IRFLD,CSTR)
          IF(KERROR.NE.0) GOTO 1174
          READ(CSTR,SFMT(18)) (QSTACK(ISET+(K-1)*N+L-1), L=1,N)
 1168   CONTINUE
        IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(NP, QSTACK(ISET) )
        IF(MNCD-IAT+1.GE.KPDRSZ) THEN
          NP=MIN0(NP,(MNCD-IAT+1)/KPDRSZ)
          CALL GKPKR(NP,QSTACK(ISET),IAT,SDR)
          IAT=IAT+NP*KPDRSZ
        ENDIF
 1167 CONTINUE

 1174 CALL GKSTDA(KREALS,ISET)
      IF(KERROR.NE.0) GOTO 9995

 1163 GOTO 9990

* Metafile items whose data record consists of one real
* -----------------------------------------------------
* value only
* ----------
 1230 CONTINUE
      NCH=IRFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ (CSTR,SFMT(6)) RLA(1)
      IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(1, RLA(1) )
      CALL GKPKR(1,RLA,IAT,SDR)
      IAT=IAT+KPDRSZ
      GOTO 9990

* Set text font and precision
* ---------------------------
 1300 CONTINUE
      NCH=2*IFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ(CSTR,SFMT(2)) ILA(1),ILA(2)
      CALL GKPKI(1,ILA,IAT,SDR)
      IAT=IAT+KPDISZ
      CALL GKPKSI(1,ILA(2),IAT,SDR)
      IAT=IAT+KPDSSZ
      GOTO 9990

* Metafile items whose data record consists of four real
* ------------------------------------------------------
*  values only
*  -----------
 1340 CONTINUE
      NCH=4*IRFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ (CSTR,SFMT(8)) (RLA(I),I=1,4)
      IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(4, RLA(1) )
      CALL GKPKR(4,RLA,IAT,SDR)
      IAT=IAT+4*KPDRSZ
      GOTO 9990

* Set fill area style index
* -------------------------
 1390 CONTINUE
      NCH=IFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ (CSTR,SFMT(1)) ILA(1)
      CALL GKPKI(1,ILA,IAT,SDR)
      IAT=IAT+KPDISZ
      GOTO 9990

* Pattern Reference Point
* -----------------------
 1420 CONTINUE
      NCH=2*IRFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ (CSTR,SFMT(7)) RLA(1),RLA(2)
      IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(2, RLA(1) )
      CALL GKPKR(2,RLA,IAT,SDR)
      IAT=IAT+2*KPDRSZ
      GOTO 9990

* Aspect source flags
* -------------------
 1430 CONTINUE
      NCH=13*IFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ (CSTR,SFMT(5)) (ILA(I),I=1,13)
      CALL GKPKSI(13,ILA,IAT,SDR)
      IAT=IAT+13*KPDSSZ
      GOTO 9990

* Polyline representation; Polymarker representation.
* ---------------------------------------------------
 1510 CONTINUE
      IF( KWKDAT(16,KWKIX).EQ.IGRAL ) THEN
         NCH=3*IFLD
         CALL GK0NBU(IFUNC,NCH,CSTR)
         IF(KERROR.NE.0) GOTO 9995
         READ(CSTR,SFMT(3)) ILA(1),ILA(3),ILA(2)
         NCH=IRFLD
         CALL GK0NBU(IFUNC,NCH,CSTR)
         IF(KERROR.NE.0) GOTO 9995
         READ(CSTR,SFMT(6)) RLA(1)
      ELSE
         NCH=3*IFLD+IRFLD
         CALL GK0NBU(IFUNC,NCH,CSTR)
         IF(KERROR.NE.0) GOTO 9995
         READ(CSTR,SFMT(14)) ILA(1),ILA(2),RLA(1),ILA(3)
      ENDIF
      IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(1, RLA(1) )
      CALL GKPKSI(2,ILA,IAT,SDR)
      IAT=IAT+2*KPDSSZ
      CALL GKPKR(1,RLA,IAT,SDR)
      IAT=IAT+KPDRSZ
      CALL GKPKSI(1,ILA(3),IAT,SDR)
      IAT=IAT+KPDSSZ
      GOTO 9990

* Text representation
* -------------------
 1530 CONTINUE
      IF( KWKDAT(16,KWKIX).EQ.IGRAL ) THEN
         NCH=4*IFLD
         CALL GK0NBU(IFUNC,NCH,CSTR)
         IF(KERROR.NE.0) GOTO 9995
         READ(CSTR,SFMT(4)) ILA(1),ILA(4),ILA(2),ILA(3)
         NCH=2*IRFLD
         CALL GK0NBU(IFUNC,NCH,CSTR)
         IF(KERROR.NE.0) GOTO 9995
         READ(CSTR,SFMT(7)) RLA(1),RLA(2)
      ELSE
         NCH=4*IFLD+2*IRFLD
         CALL GK0NBU(IFUNC,NCH,CSTR)
         IF(KERROR.NE.0) GOTO 9995
         READ (CSTR,SFMT(15)) (ILA(I),I=1,3),RLA(1),RLA(2),ILA(4)
      ENDIF
      IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(2, RLA(1) )
      CALL GKPKSI(1,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ
      CALL GKPKI(1,ILA(2),IAT,SDR)
      IAT=IAT+KPDISZ
      CALL GKPKSI(1,ILA(3),IAT,SDR)
      IAT=IAT+KPDSSZ
      CALL GKPKR(2,RLA,IAT,SDR)
      IAT=IAT+2*KPDRSZ
      CALL GKPKSI(1,ILA(4),IAT,SDR)
      IAT=IAT+KPDSSZ
      GOTO 9990

* Fill area representation
* ------------------------
 1540 CONTINUE
      NCH=4*IFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      IF( KWKDAT(16,KWKIX).EQ.IGRAL ) THEN
         READ(CSTR,SFMT(4)) ILA(1),ILA(4),ILA(2),ILA(3)
      ELSE
         READ(CSTR,SFMT(4)) ILA(1),ILA(2),ILA(3),ILA(4)
      ENDIF
      CALL GKPKSI(2,ILA,IAT,SDR)
      IAT=IAT+2*KPDSSZ
      CALL GKPKI(1,ILA(3),IAT,SDR)
      IAT=IAT+KPDISZ
      CALL GKPKSI(1,ILA(4),IAT,SDR)
      IAT=IAT+KPDSSZ
      GOTO 9990

* Pattern Representation
* ----------------------
 1550 CONTINUE
      NCH=3*IFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ (CSTR,SFMT(3)) (ILA(I),I=1,3)
      CALL GKPKSI(3,ILA,IAT,SDR)
      IAT=IAT+3*KPDSSZ

      M=ILA(2)
      N=ILA(3)
      NPTS=M*N
      CALL GKSTQ(IRSZ,IISZ,ICSZ)
      IF(IISZ.LT.M*N) NPTS=IISZ
      CALL GKSTAL(KINTGS,NPTS,ISET)

      NN=KWCST/IFLD
      NP=NPTS

      DO 1552 J=1,M*N,NPTS
      NINT=NN
        IF(M*N-J.LT.NPTS) NP=M*N-J+1
        DO 1554 K=1,NP,NN
          IF(NP-K.LT.NN) NINT=NP-K+1
          CALL GK0NBU(IFUNC,NINT*IFLD,CSTR)
          IF(KERROR.NE.0) GOTO 1556
          READ(CSTR,SFMT(16)) (KSTACK(ISET+(K-1)+L-1), L=1,NINT)
 1554   CONTINUE
        IF(MNCD-IAT+1.GE.KPDSSZ) THEN
          NP=MIN0(NP,(MNCD-IAT+1)/KPDSSZ)
          CALL GKPKSI(NP,KSTACK(ISET),IAT,SDR)
          IAT=IAT+NP*KPDSSZ
        ENDIF
 1552 CONTINUE

 1556 CALL GKSTDA(KINTGS,ISET)
      IF(KERROR.NE.0) GOTO 9995

      GOTO 9990

* Colour representation
* ---------------------
 1560 CONTINUE
      NCH=IFLD+3*IRFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ (CSTR,SFMT(10)) ILA(1),(RLA(I),I=1,3)
      IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(3, RLA(1) )
      CALL GKPKSI(1,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ
      CALL GKPKR(3,RLA,IAT,SDR)
      IAT=IAT+3*KPDRSZ
      GOTO 9990

* Set segment transformation
* --------------------------
 1910 CONTINUE
      NCH=IFLD+6*IRFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ (CSTR,SFMT(11)) ILA(1),(RLA(I),I=1,6)
      IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(6, RLA(1) )
      CALL GKPKSI(1,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ
      CALL GKPKR(6,RLA,IAT,SDR)
      IAT=IAT+6*KPDRSZ
      GOTO 9990

* Set Segment Priority
* --------------------
 1940 CONTINUE
      NCH=IFLD+IRFLD
      CALL GK0NBU(IFUNC,NCH,CSTR)
      IF(KERROR.NE.0) GOTO 9995
      READ (CSTR,SFMT(9)) ILA(1),RLA(1)
      IF (KWKDAT(18,KWKIX).NE.0) CALL GK0NIR(1, RLA(1) )
      CALL GKPKSI(1,ILA,IAT,SDR)
      IAT=IAT+KPDSSZ
      CALL GKPKR(1,RLA,IAT,SDR)
      IAT=IAT+KPDRSZ
      GOTO 9990

* User item
* ---------
 2010 CONTINUE

      N=80
      J=0
      DO 2012 I=1,ILEN,80
        IF(ILEN-I.LT.80) N=ILEN-I+1
        J=J+1
        CALL GK0NBU(IFUNC,N,SDR(J))
        IF(KERROR.NE.0) GOTO 9995
 2012 CONTINUE

      GOTO 9990

* INQUIRE WORSTATION CONNECTION AND TYPE
* --------------------------------------

 3000 CONTINUE
      KWI1=KCID(KWKIX)
      KWI2=KWKTYP
      GOTO 9999

* INQUIRE WORKSTATION CATEGORY
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

* Workstation is neither of category INPUT nor of category
* OUTIN
 9938 KERROR=38
      GOTO 9999

* Workstation is neither of category OUTPUT nor of category
* OUTIN
 9939 KERROR=39
      GOTO 9999

* Error: invalid metafile item
 9463 CONTINUE
      KERROR = 163
      GOTO 9999

 9990 GOTTYP = 0

 9995 CONTINUE

* Save local variables in COMMON
      KWKDAT(10,KWKIX) = ITYPE
      KWKDAT(11,KWKIX) = GOTTYP
      KWKDAT(12,KWKIX) = ILEN

 9999 CONTINUE

      RETURN
      END
