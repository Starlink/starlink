C# IL>=a, OL>=0
      SUBROUTINE GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Answer workstation inquiries.
*     WARNING ----- where action is workstation dependent and embedded
*     this routine will set all returned variables to a default
*     (hopefully sensible). If the default is not what the workstation
*     requires then it will have to reset the required variables in the
*     workstation entrypoint. eg INQUIRE COLOUR FACILITIES sets KWI1
*     (number of available colours or intensities) to 2. Obviously this
*     has to be reset for a full colour workstation.
*
*  MAINTENANCE LOG
*  ---------------
*     20/12/83  AS    Original version stabilized
*     10/01/84  AS    Add to list of errors
*     12/01/84  AS    Fix polymarker and fill area facilities
*     16/01/84  JRG   Added INQ SET OF SEG NAMES at 1460
*     30/01/84  JRG   For INQ TEXT FACIL, use KWI1 as index; also
*                     initialises fonts if not already done
*     30/01/84  JRG   Start search for segment names correctly
*     01/02/84  JL    Change INQ NUM of LOGICAL INPUT DEVs (1920)
*                     to access WDT
*     02/02/84  JRG   Inquiry of colour indices now starts at zero not one
*     13/02/84  JL    Make Inquire Default Device state work (I117)
*     23/02/84  JL    Inquire Default Device state -
*                     if Nth PET doesn't exist don't inquire WDT
*                     and do return KERROR = 902 (I141)
*     27/02/84  JL    Inquire Default Device state -
*                     insert workstation category checking.(I143)
*     28/02/84  JL    Inquire Pattern Representation - call GKHPGI
*                     instead of GKHPPI  (I150)
*     27/03/84  RSK   Put in fix for bug (S29) Inquire Pattern
*                     Facilities when workstation not open.
*     29/03/84  CJW   Inquire Pattern rep. - correct call to GKHPGI
*     01/04/84  RSK   Merged ICL and SERC versions
*     26/04/84  CJW   Inq. Seg wk (KREL) incorrect (I193)
*     09/05/84  NGB   enhance comments to 1940 & 1980
*     23/05/84  JRG   remove redundant include for GKWCB
*     10/12/84  RMK   Corrected tests on colour index in Inquire Colour
*                     Repres. and Inquire Predefined Colour Repres. (S90)
*     19/08/85  RMK   Set style index to 1 for solid or hollow in Inquire
*                     Fill Area Representation entry (S84).
*                     Generate error 902 if get invalid interior style and
*                     hatch style in Inquire Fill Area Facilities entry (S134).
*     25/06/86  RMK   Tightened up error checking in Inquire Fill Area Facils
*                     entry (additional code for fix S134).
*     03/07/86  RMK   Removed change made to Inq Fill Area Repres entry on
*                     19/08/85 - shouldn't alter style index here.
*     22/01/87  JCS   IS conversion. Error number changes.
*     22/01/87  RMK   IS conversion. Change inquire FA facilities to allow
*                     -ve hatch styles. In inquire choice device state
*                     entry pick-up initial status.
*     28/04/87  RMK   Changed inquire dynamic modification of segment
*                     attributes to return segment invisible->visible as
*                     immediate (S257).
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     30/07/87  PJWR  Corrected array indices when inquiring input device
*                     data as these changed when PIDs were replaced by
*                     FORTRAN binding style data records in the WDT.  Also
*                     replaced inappropriate use of WSL parameters in these
*                     inquiries and removed comments saying that using them
*                     was wrong!
*                     For Inquire Default Choice Device Data, increased
*                     maximum number of choices from 4 to 9,  as all single
*                     PET workstations offer this at present.
*     12/08/87  PJWR  Fixed bug S174 - The inquire input device default
*                     data entrypoints were attempting to cope with some
*                     devices/PETs other than 1.
*     14/08/87  PJWR  Corrected number of integers GKDRGE called with
*                     when inquiring pick device state.
*     13/11/87  RMK   Changed Inquire Predefined xx Repres entries to use
*                     new GKS error numbers (S270).
*     05/08/88  KEVP  In Inquire Pixel array dimensions, removed the
*                     added 1 to make it conform to GKS standard (C19).
*     21/10/88  KEVP  Fixed bug S332 in Inquire Pixel Array Dimensions
*                     by converting WC to DC using GKTWN then GKTND.
*     08/12/88  NMH   Added setting of error 2001 if input array size too
*                     small in inquire predefined pattern repres (S360).
*     26/05/89  NMH   Fixed inquire default string device data entry. Return
*                     buffer values in correct variables (S356).
*     26/05/89  NMH   Added handling of inquire default pick device data for
*                     PET 1 (S357).
*     24/08/89  RMK   Changed inquire predefined pattern repres to give
*                     error 89 if pattern is not predefined (S363).
*     21/03/90  PLP   Changed the Inquire Fill Area Facilities Entry
*                     to behave in accordance with the standard when
*                     list elements outside the supported range are
*                     requested (S345).
*     11/04/90  PLP   Corrected the String Device State Entry. Initial
*                     string now returned only if it has a non-zero length.
*                     (S327).
*     29/05/90  KEVP  Fixed bug S295 in Inquire predefined pattern
*                     representation, by using the Number of Predefined
*                     Pattern Indices instead of the Maximum Number of
*                     Pattern Bundle Table Entries.
*     30/05/90  KEVP  Fixed bug S274 in Inquire Dynamic Modification of
*                     Segment Attributes, so the "adding primitives to
*                     the open segment" is immediate.
*     04/07/90  KEVP  In inQuire list Element of PAttern Indices, check
*                     maximum number of patterns is positive, before
*                     looking up directory (S395).
*     27/09/90  KEVP  For inQuire Default input device state, moved
*                     checking of error 2002 to front end. Made sure
*                     that number of PETs is always output as 1 (C41).
*     02/10/90  KEVP  In inQuire list Element of PAttern indices,
*                     ensured that number of pattern indices is output
*                     even if error 2002 is reported (C41 & C35).
*     25/10/90  PLP   Changed the inQuire TeXt Facilities Entry
*                     to behave in accordance with the standard when
*                     list elements outside the supported range are
*                     requested (C55).
*     21/02/91  KEVP  Dynamic modification of segment highlighting set
*                     to immediate (S470).
*
*  ARGUMENTS
*  ---------
*     INP IENT  - Entrypoint code
*     INP NID   - Size of array IDAT
*     I/O IDAT  - Integer data passed to or from workstation
*     INP NRD   - Size of arrays RX and RY
*     I/O RX    - Real X-coordinate data passed to or from workstation
*     I/O RY    - Real Y-coordinate data passed to or from workstation
*     INP NCD   - Length of character array
*     I/O STR   - Character array
*
      INTEGER IENT, NID, IDAT(NID), NRD, NCD
      REAL RX(NRD), RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkinp.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkplb.cmn'
      INCLUDE '../include/gkpmb.cmn'
      INCLUDE '../include/gktxb.cmn'
      INCLUDE '../include/gkfab.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkhp.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gksl.cmn'
*
*  LOCALS
*  ------
*
*     ISSGPT  Local copy of pointer to segment list in workstation
*     ISPEC   Specifying where to start search for segment
*     IER     Error number to use if no segments returned
*     NEXT    Next segment to be found
*     NTH1    Temporary storage variable
*     NTH2    Temporary storage variable
*     PRI     Segment priority (dummy argument)
*     BOUND   Segment bounding box (dummy argument)
*
      INTEGER INTA(19),ITEMP,I,J,NI,NR,NTH,IOFF,ISSGPT,ISPEC,IER,NEXT,
     :        NTH1, NTH2
      REAL REALA(12),WCX(2),WCY(2),DCX(2),DCY(2),PRI,BOUND(4)
*
*  ERRORS
*  ------
*     40  No pixel store readback capability
*     41  Does not have specified GDP
*     61  No representation for polyline index
*     62  No predefined representation for polyline index
*     67  No representation for polymarker index
*     68  No predefined representation for polymarker index
*     73  No representation for text index
*     74  No predefined representation for text index
*     81  No representation for fill area index
*     82  No predefined representation for fill area index
*     88  No representation for pattern index
*     89  No predefined representation for pattern index
*     94  No representation for colour index
*     95  No predefined representation for colour index
*    140  Specified input device is not present
*   2002  List or set element is not available
*
*---------------------------------------------------------------------



* Conditional GOTO on entrypoint code

      ITEMP = IENT-119
      GOTO (1200,1210,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1300,1310,1320,1330,1340,1350,1360,1370,1380,1390,
     :      1400,1410,1420,1430,1440,1450,1460,1470,1480,1490,
     :      1500,1510,1520,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1700,1710,1720,1730,1740,1750,1760,1770,1780,1790,
     :      1800,1810,1820,1830,1840,1850,1860,1870,1880,1890,
     :      1900,1910,1920,1111,1111,1111,1111,1111,1111) ITEMP

*  Fix I143
*  Insert preliminary check to ensure that workstation has at least
*  one input device.     If it has not return error 38 -
*  workstation is not of category GINPUT nor GOUTIN
 1111 CONTINUE
      CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
      IF (KERROR.NE.0) GOTO 9999
      I = INTA(14)+INTA(15)+INTA(16)+INTA(17)+INTA(18)+INTA(19)
      IF (I.LE.0) THEN
         KERROR = 38
         GOTO 9999
      ENDIF
      GOTO (1930,1940,1950,1960,1970,1980) IENT - 192
* Security measure only - probably un-necessary
      CALL GKBUG(-1016,'GKQWK')
      GOTO 9999

* Inquire pixel array dimensions (NB: DC are assumed to be Raster Coords)
 1200 CONTINUE
      WCX(1)=QWR1
      WCY(1)=QWR2
      WCX(2)=QWR3
      WCY(2)=QWR4
* Transform to DC indirectly to avoid QWTOTT (to get True DC)
      CALL GKTWN (KCNTN,2,WCX,WCY,DCX,DCY)
      IF(KERROR .EQ. 152)KERROR = 0
      CALL GKTND (2,DCX,DCY,DCX,DCY)
      IF(KERROR .EQ. 152)KERROR = 0
* Round to integers, so as to include a pixel, if and only if
* its centre is inside the rectangle. This is done by just using NINT.
      KWI1=ABS(NINT(DCX(2)) - NINT(DCX(1)))
      KWI2=ABS(NINT(DCY(2)) - NINT(DCY(1)))
      GOTO 9999



* Inquire pixel array
 1210 CONTINUE
      KERROR = 40
      GOTO 9999



* Inquire workstation connection and type
 1300 CONTINUE
      KWI1 = KCID(KWKIX)
      KWI2 = KWKTYP
      GOTO 9999



* Inquire workstation deferral and update states
 1310 CONTINUE
      KWI1 = KDFM(KWKIX)
      KWI2 = KIMRGM(KWKIX)
      KWI3 = KDSMT(KWKIX)
      KWI4 = KNFAUP(KWKIX)
      GOTO 9999



* Inquire list element of polyline indices
 1320 CONTINUE
      NTH = KWI1
      KWI1 = KMXPLB
      DO 1322 I=1,KMXPLB
        IF (KPLI(I,KWKIX).EQ.KNIL) THEN
          KWI1 = I - 1
          GOTO 1324
        ENDIF
 1322 CONTINUE
 1324 CONTINUE
      IF (NTH.GT.0 .AND. NTH.LE.KWI1) THEN
        KWI2 = KPLI(NTH,KWKIX)
      ELSE
      KERROR = 2002
      ENDIF
      GOTO 9999



* Inquire polyline representation
 1330 CONTINUE
      DO 1332 I=1,KMXPLB
        IF (KPLI(I,KWKIX).EQ.KWI1) GOTO 1334
 1332 CONTINUE
      KERROR = 61
      GOTO 9999
 1334 CONTINUE
      KWI1 = KLNTY(I,KWKIX)
      KWI2 = KPLCI(I,KWKIX)
      QWR1 = QLNWD(I,KWKIX)
      GOTO 9999



* Inquire list element of polymarker indices
 1340 CONTINUE
      NTH = KWI1
      KWI1 = KMXPMB
      DO 1342 I=1,KMXPMB
        IF (KPMI(I,KWKIX).EQ.KNIL) THEN
          KWI1 = I - 1
          GOTO 1344
        ENDIF
 1342 CONTINUE
 1344 CONTINUE
      IF (NTH.GT.0 .AND. NTH.LE.KWI1) THEN
        KWI2 = KPMI(NTH,KWKIX)
      ELSE
        KERROR = 2002
      ENDIF
      GOTO 9999



* Inquire polymarker representation
 1350 CONTINUE
      DO 1352 I=1,KMXPMB
        IF (KPMI(I,KWKIX).EQ.KWI1) GOTO 1354
 1352 CONTINUE
      KERROR = 67
      GOTO 9999
 1354 CONTINUE
      KWI1 = KMKTY(I,KWKIX)
      KWI2 = KPMCI(I,KWKIX)
      QWR1 = QMKSZ(I,KWKIX)
      GOTO 9999



* Inquire list element of text indices
 1360 CONTINUE
      NTH = KWI1
      KWI1 = KMXTXB
      DO 1362 I=1,KMXTXB
        IF (KTXI(I,KWKIX).EQ.KNIL) THEN
          KWI1 = I - 1
          GOTO 1364
        ENDIF
 1362 CONTINUE
 1364 CONTINUE
      IF (NTH.GT.0 .AND. NTH.LE.KWI1) THEN
        KWI2 = KTXI(NTH,KWKIX)
      ELSE
        KERROR = 2002
      ENDIF
      GOTO 9999



* Inquire text representation
 1370 CONTINUE
      DO 1372 I=1,KMXTXB
        IF (KTXI(I,KWKIX).EQ.KWI1) GOTO 1374
 1372 CONTINUE
      KERROR = 73
      GOTO 9999
 1374 CONTINUE
      KWI1 = KTXFN(I,KWKIX)
      KWI2 = KTXPR(I,KWKIX)
      KWI3 = KTXCI(I,KWKIX)
      QWR1 = QCHXP(I,KWKIX)
      QWR2 = QCHSP(I,KWKIX)
      GOTO 9999



* Inquire text extent
 1380 CONTINUE
      CALL GKXQXO(NID,IDAT,RX,RY)
      GOTO 9999



* Inquire list element of fill area indices
 1390 CONTINUE
      NTH = KWI1
      KWI1 = KMXFAB
      DO 1392 I=1,KMXFAB
        IF (KFAI(I,KWKIX).EQ.KNIL) THEN
          KWI1 = I - 1
          GOTO 1394
        ENDIF
 1392 CONTINUE
 1394 CONTINUE
      IF (NTH.GT.0 .AND. NTH.LE.KWI1) THEN
        KWI2 = KFAI(NTH,KWKIX)
      ELSE
        KERROR = 2002
      ENDIF
      GOTO 9999



* Inquire fill area representation
 1400 CONTINUE
      DO 1402 I=1,KMXFAB
        IF (KFAI(I,KWKIX).EQ.KWI1) GOTO 1404
 1402 CONTINUE
      KERROR = 81
      GOTO 9999
 1404 CONTINUE
      KWI1 = KIS(I,KWKIX)
      KWI2 = KSI(I,KWKIX)
      KWI3 = KFACI(I,KWKIX)
      GOTO 9999



* Inquire list element of pattern indices
 1410 CONTINUE
      NTH = KWI1
      IF (KMXPAB(KWKIX) .GT. 0)THEN
         CALL GKDRQ (KPABPT(KWKIX),KWI1,NI,NR)
         IF((0 .LT. NTH) .AND. (NTH .LE. KWI1))THEN
           CALL GKDRQN(KPABPT(KWKIX),NTH,KWI2)
         ELSE
           KWI2 = KNIL
           KERROR = 2002
         ENDIF
       ELSE
*      No patterns
         KWI1 = 0
         KWI2 = KNIL
       ENDIF
       GOTO 9999



* Inquire pattern representation
 1420 CONTINUE
      CALL GKDRGE(KPABPT(KWKIX),KWI1,3,0,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        KWI1 = INTA(1)
        KWI2 = INTA(2)
        IF (KWI1.LE.KWI3 .AND. KWI2.LE.KWI4) THEN
          DO 1422 I=1,KWI2
            CALL GKHPGI(INTA(3),(I-1)*KWI1,KWI1,IDAT((I-1)*KWI3+1))
 1422     CONTINUE
        ENDIF
      ELSE
        KERROR = 88
      ENDIF
      GOTO 9999



* Inquire list element of colour indices
 1430 CONTINUE
      NTH = KWI1
      KWI1 = KPCI(KWKIX)
      IF (NTH.GT.0 .AND. NTH.LE.KWI1) THEN
        KWI2 = NTH-1
      ELSE
        KERROR = 2002
      ENDIF
      GOTO 9999



* Inquire colour representation
 1440 CONTINUE
      IF (KWI1.GE.KPCI(KWKIX)) THEN
        KERROR = 94
      ELSE
        QWR1 = QHP(KHPXR(KCTBPT(1,KWKIX))+KWI1)
        QWR2 = QHP(KHPXR(KCTBPT(2,KWKIX))+KWI1)
        QWR3 = QHP(KHPXR(KCTBPT(3,KWKIX))+KWI1)
      ENDIF
      GOTO 9999



* Inquire workstation transformation
 1450 CONTINUE
      KWI1 = KWKTUP(KWKIX)
      RX(1) = QRWWXL(KWKIX)
      RY(1) = QRWWYB(KWKIX)
      RX(2) = QRWWXR(KWKIX)
      RY(2) = QRWWYT(KWKIX)
      RX(3) = QCWWXL(KWKIX)
      RY(3) = QCWWYB(KWKIX)
      RX(4) = QCWWXR(KWKIX)
      RY(4) = QCWWYT(KWKIX)
      RX(5) = QRWVXL(KWKIX)
      RY(5) = QRWVYB(KWKIX)
      RX(6) = QRWVXR(KWKIX)
      RY(6) = QRWVYT(KWKIX)
      RX(7) = QCWVXL(KWKIX)
      RY(7) = QCWVYB(KWKIX)
      RX(8) = QCWVXR(KWKIX)
      RY(8) = QCWVYT(KWKIX)
      GOTO 9999



* Inquire (set of) segment names on workstation
 1460 CONTINUE
      ISSGPT=KSSGPT(KWKIX)
      IF( KWI1.EQ.KNUM ) THEN

*       'Starting segment' (KWI2) is a sequence number. This is
*       required by the GKS inquiry
        IER = 2002

*       First find the number of segments - this is the slow way! - would
*       be better to inquire of the GKSLxx package.
          ISPEC=KLOEST
          KWI3=0

*       Loop: one cycle for each segment name found.
*       Counting in KWI3
 1462     CONTINUE
            CALL GKSLGE(ISSGPT,ISPEC,NEXT,PRI,BOUND)
            IF( KERROR.NE.0 ) GOTO 9999
            IF( NEXT.EQ.KNIL ) GOTO 1464
            KWI3=KWI3+1
            ISPEC=KHIER
            GOTO 1462
 1464     CONTINUE
*       End loop ... KWI3 contains number of segments

*       Get the segment name if available.
          IF( KWI3.GT.0 .AND. KWI2.LE.KWI3 ) THEN
            CALL GKSLQN(ISSGPT,KWI2,IDAT(1))
            KNIR=1
          ELSE
            KNIR=0
         KERROR = 2002
          ENDIF

      ELSEIF( KWI1.EQ.KREL ) THEN

*       Here, 'starting segment' is a segment name or KLOEST or KHIER.
*       Currently, number of segments is not returned in KWI3 (in this
*       branch of the IF.
*       First set up the maximum number of segment names to be
*       picked up (NID) and the error number to be used (IER) if there
*       are no segments left.
          IF( KWI2.EQ.KLOEST .OR. KWI2.EQ.KHIER ) THEN
              NI=NID
              IER=0
          ELSE
              NI=1
              IER=123
          ENDIF
          ISPEC=KWI2
          DO 1466 J=1,NI
             CALL GKSLGE(ISSGPT,ISPEC,IDAT(J),PRI,BOUND)
             IF( KERROR.NE.0 ) GOTO 9999
             IF( IDAT(J).EQ.KNIL ) GOTO 1468
             ISPEC=KHIER
 1466     CONTINUE
          J = NI + 1

*   Here: J is 1 beyond the end of any segment names present
 1468     KNIR=J-1
      ELSE
         CALL GKBUG(-2004,'GKQWK')
      ENDIF
      IF( KNIR.EQ.0 ) KERROR=IER
      GOTO 9999



* Inquire locator device state
 1470 CONTINUE
      IF (KIPDPT(1,KWKIX).NE.KNIL) THEN
        CALL GKDRGE(KIPDPT(1,KWKIX),KWI1,5,6,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI1 = INTA(KIPOPM)
          KWI2 = INTA(KIPE)
          KWI3 = INTA(KIPPRT)
          KWI4 = INTA(KLCINN)
          QWR1 = REALA(KIPEXL)
          QWR2 = REALA(KIPEXR)
          QWR3 = REALA(KIPEYB)
          QWR4 = REALA(KIPEYT)
          RX(1) = REALA(KLCINX)
          RY(1) = REALA(KLCINY)
          KNRR=1
          CALL GKPIPD(INTA(KIPD),NCD,KNCR,STR)
        ELSE
          KERROR = 140
        ENDIF
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999



* Inquire stroke device state
 1480 CONTINUE
      IF (KIPDPT(2,KWKIX).NE.KNIL) THEN
        CALL GKDRGE(KIPDPT(2,KWKIX),KWI1,10,4,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI1 = INTA(KIPOPM)
          KWI2 = INTA(KIPE)
          KWI3 = INTA(KIPPRT)
          KWI4 = INTA(KSKINN)
          KWI5 = INTA(KSKINB)
          KNRR = INTA(KSKINP)
          QWR1 = REALA(KIPEXL)
          QWR2 = REALA(KIPEXR)
          QWR3 = REALA(KIPEYB)
          QWR4 = REALA(KIPEYT)
          IF (KNRR.GT.0) THEN
            CALL GKHPGR(INTA(KSKINX),0,KNRR,RX)
            CALL GKHPGR(INTA(KSKINY),0,KNRR,RY)
          ENDIF
          CALL GKPIPD(INTA(KIPD),NCD,KNCR,STR)
        ELSE
          KERROR = 140
        ENDIF
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999



* Inquire valuator device state
 1490 CONTINUE
      IF (KIPDPT(3,KWKIX).NE.KNIL) THEN
        CALL GKDRGE(KIPDPT(3,KWKIX),KWI1,4,7,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI1 = INTA(KIPOPM)
          KWI2 = INTA(KIPE)
          KWI3 = INTA(KIPPRT)
          QWR1 = REALA(KIPEXL)
          QWR2 = REALA(KIPEXR)
          QWR3 = REALA(KIPEYB)
          QWR4 = REALA(KIPEYT)
          QWR5 = REALA(KVLINV)
          QWR6 = REALA(KVLMNV)
          QWR7 = REALA(KVLMXV)
          CALL GKPIPD(INTA(KIPD),NCD,KNCR,STR)
        ELSE
          KERROR = 140
        ENDIF
      ELSE
       KERROR=140
      ENDIF
      GOTO 9999



* Inquire choice device state
 1500 CONTINUE
      IF (KIPDPT(4,KWKIX).NE.KNIL) THEN
        CALL GKDRGE(KIPDPT(4,KWKIX),KWI1,6,4,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI1 = INTA(KIPOPM)
          KWI2 = INTA(KIPE)
          KWI3 = INTA(KIPPRT)
          KWI4 = INTA(KCHINN)
          KWI5 = INTA(KCHINS)
          QWR1 = REALA(KIPEXL)
          QWR2 = REALA(KIPEXR)
          QWR3 = REALA(KIPEYB)
          QWR4 = REALA(KIPEYT)
          CALL GKPIPD(INTA(KIPD),NCD,KNCR,STR)
        ELSE
         KERROR=140
        ENDIF
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999



* Inquire pick device state
 1510 CONTINUE
      IF (KIPDPT(5,KWKIX).NE.KNIL) THEN
        CALL GKDRGE(KIPDPT(5,KWKIX),KWI1,9,4,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI1 = INTA(KIPOPM)
          KWI2 = INTA(KIPE)
          KWI3 = INTA(KIPPRT)
          KWI4 = INTA(KPCINS)
          KWI5 = INTA(KPCISG)
          KWI6 = INTA(KPCINI)
          QWR1 = REALA(KIPEXL)
          QWR2 = REALA(KIPEXR)
          QWR3 = REALA(KIPEYB)
          QWR4 = REALA(KIPEYT)
          CALL GKPIPD(INTA(KIPD),NCD,KNCR,STR)
        ELSE
          KERROR = 140
        ENDIF
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999



* Inquire string device state
 1520 CONTINUE
      IF (KIPDPT(6,KWKIX).NE.KNIL) THEN
        CALL GKDRGE(KIPDPT(6,KWKIX),KWI1,9,4,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI1 = INTA(KIPOPM)
          KWI2 = INTA(KIPE)
          KWI3 = INTA(KIPPRT)
          KWI4 = INTA(KSTINB)
          KWI5 = INTA(KSTICP)
          QWR1 = REALA(KIPEXL)
          QWR2 = REALA(KIPEXR)
          QWR3 = REALA(KIPEYB)
          QWR4 = REALA(KIPEYT)
          KNIR = INTA(KSTINL)
          IF (KNIR.GT.0) THEN
             CALL GKHPGI(INTA(KSTINS),0,KNIR,IDAT)
          ENDIF
          CALL GKPIPD(INTA(KIPD),NCD,KNCR,STR)
        ELSE
         KERROR=140
        ENDIF
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999



* Inquire workstation category
 1700 CONTINUE
      KWI1 = GOUTIN
      GOTO 9999



* Inquire workstation classification
 1710 CONTINUE
      KWI1 = GRASTR
      GOTO 9999



* Inquire maximum display surface size
 1720 CONTINUE
      KWI1 = GOTHU
      IF (KWKIX.NE.KNIL) THEN
        KWI2 = KDSRX(KWKIX)
        KWI3 = KDSRY(KWKIX)
        QWR1 = QDSDX(KWKIX)
        QWR2 = QDSDY(KWKIX)
      ELSE
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI2 = INTA(1)
          KWI3 = INTA(2)
          QWR1 = REALA(1)
          QWR2 = REALA(2)
        ENDIF
      ENDIF
      GOTO 9999



* Inquire dynamic modification of workstation attributes
 1730 CONTINUE
      KWI1 = GIRG
      KWI2 = GIRG
      KWI3 = GIRG
      KWI4 = GIRG
      KWI5 = GIRG
      KWI6 = GIRG
      KWI7 = GIRG
      GOTO 9999



* Inquire default deferral state values
 1740 CONTINUE
      KWI1 = GBNIG
      KWI2 = GSUPPD
      GOTO 9999



* Inquire polyline facilities
 1750 CONTINUE
      NTH = KWI1
      KWI1 = 5
      IF (NTH.GT.0 .AND. NTH.LE.5) THEN
        KWI2 = NTH
      ELSE
        KERROR = 2002
      ENDIF
      IF (KWKIX.NE.KNIL) THEN
        KWI3 = KLNWD(KWKIX)
        KWI4 = KPPLI(KWKIX)
        QWR1 = QNMLNW(KWKIX)
        QWR2 = QMNLNW(KWKIX)
        QWR3 = QMXLNW(KWKIX)
      ELSE
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI3 = INTA(3)
          KWI4 = INTA(8)
          QWR1 = REALA(3)
          QWR2 = REALA(4)
          QWR3 = REALA(5)
        ENDIF
      ENDIF
      GOTO 9999



* Inquire predefined polyline representation
 1760 CONTINUE
      CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        IF (KWI1.LE.INTA(8)) THEN
         CALL GKQWDT(KWKTYP,KPPLB,KWI1,3,1,INTA,REALA)
          IF (KERROR.EQ.0) THEN
            KWI1 = INTA(2)
            KWI2 = INTA(3)
            QWR1 = REALA(1)
          ENDIF
        ELSE
          KERROR = 62
        ENDIF
      ENDIF
      GOTO 9999



* Inquire polymarker facilities
 1770 CONTINUE
      NTH = KWI1
      KWI1 = 5
      IF (NTH.GT.0 .AND. NTH.LE.5) THEN
        KWI2 = NTH
      ELSE
        KERROR = 2002
      ENDIF
      IF (KWKIX.NE.KNIL) THEN
        KWI3 = KMKSZ(KWKIX)
        KWI4 = KPPMI(KWKIX)
        QWR1 = QNMMKS(KWKIX)
        QWR2 = QMNMKS(KWKIX)
        QWR3 = QMXMKS(KWKIX)
      ELSE
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI3 = INTA(4)
          KWI4 = INTA(9)
          QWR1 = REALA(6)
          QWR2 = REALA(7)
          QWR3 = REALA(8)
        ENDIF
      ENDIF
      GOTO 9999



* Inquire predefined polymarker representation
 1780 CONTINUE
      CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        IF (KWI1.LE.INTA(9)) THEN
         CALL GKQWDT(KWKTYP,KPPMB,KWI1,3,1,INTA,REALA)
          IF (KERROR.EQ.0) THEN
            KWI1 = INTA(2)
            KWI2 = INTA(3)
            QWR1 = REALA(1)
          ENDIF
        ELSE
           KERROR = 68
        ENDIF
      ENDIF
      GOTO 9999



* Inquire text facilities ... on entry KWI1 specifies list element requested
 1790 CONTINUE
      IF(( KWI1 .LT. 0) .OR. ( KWI1.GT.KFNTMX )) THEN
        KERROR = 2002
      ELSEIF(KWI1 .NE. 0)THEN
*   Make sure that fonts are available
       IF( KDBFLS.EQ.KFLNA ) THEN
          KERROR = -1042
          GOTO 9999
        ENDIF
        IF( KDBFLS.EQ.KFLCL ) CALL GKXON
        IF( KERROR.NE.0 ) GOTO 9999
        KWI2 = KHFONT(KWI1)
        KWI3 = 2
      ENDIF
      KWI1=KFNTMX
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
      GOTO 9999



* Inquire predefined text representation
 1800 CONTINUE
      CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        IF (KWI1.LE.INTA(10)) THEN
          CALL GKQWDT(KWKTYP,KPTXB,KWI1,4,2,INTA,REALA)
          IF (KERROR.EQ.0) THEN
            KWI1 = INTA(2)
            KWI2 = INTA(3)
            KWI3 = INTA(4)
            QWR1 = REALA(1)
            QWR2 = REALA(2)
          ENDIF
        ELSE
         KERROR = 74
        ENDIF
      ENDIF
      GOTO 9999



* Inquire fill area facilities
 1810 CONTINUE
*     NOTE: This entry assumes that all 4 interior styles are available on the
*            workstation.

*     Store the values of list elements requested.
      NTH1 = KWI1
      NTH2 = KWI2

*     Supply the standard answer and modify later if necessary.
      KWI4 = -KWI2
      KWI2 = KWI1 - 1
      KWI1 = 4
      KWI3 = 10
      IF (KWKIX.NE.KNIL) THEN
         KWI5 = KPFAI(KWKIX)
      ELSE
         CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
         IF (KERROR.EQ.0) THEN
            KWI5 = INTA(11)
         ENDIF
      ENDIF

*     Check the input list elements and modify the returned values
*     where necessary.
      IF (NTH1.LT.0.OR.NTH1.GT.4) THEN
         KERROR = 2002
         KWI2 = KNIL
      ELSEIF (NTH1.EQ.0) THEN
         KWI2 = KNIL
      ENDIF

      IF (NTH2.LT.0.OR.NTH2.GT.10) THEN
         KERROR = 2002
         KWI4 = KNIL
      ELSEIF (NTH2.EQ.0)THEN
         KWI4 = KNIL
      ENDIF

      GOTO 9999

* Inquire predefined fill area representation
 1820 CONTINUE
      CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        IF (KWI1.LE.INTA(11)) THEN
          CALL GKQWDT(KWKTYP,KPFAB,KWI1,4,0,INTA,REALA)
          IF (KERROR.EQ.0) THEN
            KWI1 = INTA(2)
            KWI2 = INTA(3)
            KWI3 = INTA(4)
          ENDIF
        ELSE
          KERROR = 82
        ENDIF
      ENDIF
      GOTO 9999



* Inquire pattern facilities
 1830 CONTINUE
      IF (KWKIX.NE.KNIL) THEN
        KWI1 = KPPAI(KWKIX)
      ELSE
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI1 = INTA(12)
        ENDIF
      ENDIF
      GOTO 9999



* Inquire predefined pattern representation
 1840 CONTINUE
      IOFF = KNIL
      IF (KWKIX.NE.KNIL) THEN
        ITEMP = KPPAI(KWKIX)
      ELSE
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.NE.0) GOTO 9999
        ITEMP = INTA(12)
      ENDIF
      IF (KWI1.LE.ITEMP) THEN
*       Read pattern array dimensions.  If error, pattern isn't predefined
        CALL GKQWDT(KWKTYP,KPPASZ,KWI1,3,0,INTA,REALA)
        IF (KERROR.NE.0) THEN
           KERROR = 89
           GOTO 9999
        ENDIF
        IF (INTA(2).LE.KWI2 .AND. INTA(3).LE.KWI3) THEN
          ITEMP = INTA(2)*INTA(3)
          CALL GKSTAL(KINTGS,ITEMP,IOFF)
          IF (KERROR.NE.0) GOTO 1843
          CALL GKQWDT(KWKTYP,KPPAB,KWI1,ITEMP,0,KSTACK(IOFF),REALA)
          IF (KERROR.NE.0) GOTO 1843
          DO 1842 I=1,INTA(3)
            DO 1841 J=1,INTA(2)
              IDAT((I-1)*KWI2+J) = KSTACK(IOFF+(I-1)*INTA(2)+J-1)
 1841       CONTINUE
 1842     CONTINUE
          KWI1 = INTA(2)
          KWI2 = INTA(3)
        ELSE
          KERROR = 2001
        ENDIF
      ELSE
        KERROR = 89
      ENDIF
 1843 CALL GKSTDA(KINTGS,IOFF)
      GOTO 9999



* Inquire colour facilities
 1850 CONTINUE
      KWI1 = 2
* This should in future come from WDT file
      KWI2 = GMONOC
      IF (KWKIX.NE.KNIL) THEN
        KWI3 = KPCI(KWKIX)
      ELSE
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI3 = INTA(13)
        ENDIF
      ENDIF
      GOTO 9999



* Inquire predefined colour representation
 1860 CONTINUE
      CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        IF (KWI1.LT.INTA(13)) THEN
          CALL GKQWDT(KWKTYP,KPCTB,KWI1,1,3,INTA,REALA)
          IF (KERROR.EQ.0) THEN
            QWR1 = REALA(1)
            QWR2 = REALA(2)
            QWR3 = REALA(3)
          ENDIF
        ELSE
          KERROR = 95
        ENDIF
      ENDIF
      GOTO 9999



* Inquire list element of available generalized drawing primitives
 1870 CONTINUE
      KNIR = 0
      GOTO 9999



* Inquire generalized drawing primitive
 1880 CONTINUE
      KERROR = 41
      GOTO 9999



* Inquire maximum length of workstation state tables
 1890 CONTINUE
      KWI1 = KMXPLB
      KWI2 = KMXPMB
      KWI3 = KMXTXB
      KWI4 = KMXFAB
      IF (KWKIX.NE.KNIL) THEN
        KWI5 = KMXPAB(KWKIX)
        KWI6 = KPCI(KWKIX)
      ELSE
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI5 = INTA(7)
          KWI6 = INTA(13)
        ENDIF
      ENDIF
      GOTO 9999



* Inquire number of segment priorities supported
 1900 CONTINUE
      KWI1 = 1
      GOTO 9999



* Inquire dynamic modification of segment attributes
 1910 CONTINUE
      KWI1 = GIRG
      KWI2 = GIRG
      KWI3 = GIMM
      KWI4 = GIMM
      KWI5 = GIRG
      KWI6 = GIMM
      KWI7 = GIRG
      GOTO 9999



* Inquire number of available logical input devices
 1920 CONTINUE
      CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        KWI1 = INTA(14)
        KWI2 = INTA(15)
        KWI3 = INTA(16)
        KWI4 = INTA(17)
        KWI5 = INTA(18)
        KWI6 = INTA(19)
      ENDIF
      GOTO 9999



* Inquire default locator device data
 1930 CONTINUE
      IF (KWI2.NE.1) THEN
        KWI2 = 1
        KWI3 = KNIL
      ELSE
        KWI3 = 1
      ENDIF
      CALL GKQWDT(KWKTYP,KLC,KWI1,6,6,INTA,REALA)
      IF (KERROR.EQ.0)THEN
        QWR1 = REALA(1)
        QWR2 = REALA(2)
        QWR3 = REALA(3)
        QWR4 = REALA(4)
        QWR5 = REALA(5)
        QWR6 = REALA(6)
        CALL GKPIDD(GLOCAT,NCD,KNCR,STR)
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999



* Inquire default stroke device data
 1940 CONTINUE
      IF (KWI2.EQ.1) THEN
        KWI3 = 1
      ELSE
        KWI2 = 1
        KWI3 = KNIL
      ENDIF
      CALL GKQWDT(KWKTYP,KSK,KWI1,8,6,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        QWR1 = REALA(1)
        QWR2 = REALA(2)
        QWR3 = REALA(3)
        QWR4 = REALA(4)
        KWI4 = INTA(7)
        KWI5 = INTA(8)
        CALL GKPIDD(GSTROK,NCD,KNCR,STR)
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999



* Inquire default valuator device data
 1950 CONTINUE
      IF (KWI2.EQ.1) THEN
        KWI3 = 1
      ELSE
        KWI2 = 1
        KWI3 = KNIL
      ENDIF
      CALL GKQWDT(KWKTYP,KVL,KWI1,6,7,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        QWR1 = REALA(1)
        QWR2 = REALA(2)
        QWR3 = REALA(3)
        QWR4 = REALA(4)
        QWR5 = REALA(5)
        QWR6 = REALA(6)
        QWR7 = REALA(7)
        CALL GKPIDD(GVALUA,NCD,KNCR,STR)
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999



* Inquire default choice device data
 1960 CONTINUE
      IF (KWI2.EQ.1) THEN
        KWI3 = 1
      ELSE
        KWI2 = 1
        KWI3 = KNIL
      ENDIF
      CALL GKQWDT(KWKTYP,KCH,KWI1,6,4,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        QWR1 = REALA(1)
        QWR2 = REALA(2)
        QWR3 = REALA(3)
        QWR4 = REALA(4)
        KWI4 = 9
        CALL GKPIDD(GCHOIC,NCD,KNCR,STR)
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999



* Inquire default pick device data
 1970 CONTINUE
      IF (KWI2.EQ.1) THEN
        KWI3 = 1
      ELSE
        KWI2 = 1
        KWI3 = KNIL
      ENDIF
      CALL GKQWDT(KWKTYP,KPC,KWI1,6,4,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        QWR1 = REALA(1)
        QWR2 = REALA(2)
        QWR3 = REALA(3)
        QWR4 = REALA(4)
        CALL GKPIDD(GPICK,NCD,KNCR,STR)
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999



* Inquire default string device data
 1980 CONTINUE
      IF (KWI2.EQ.1) THEN
        KWI3 = 1
      ELSE
        KWI2 = 1
        KWI3 = KNIL
      ENDIF
      CALL GKQWDT(KWKTYP,KST,KWI1,8,4,INTA,REALA)
      IF (KERROR.EQ.0) THEN
        QWR1 = REALA(1)
        QWR2 = REALA(2)
        QWR3 = REALA(3)
        QWR4 = REALA(4)
        KWI4 = INTA (7)
        KWI5 = INTA (8)
        CALL GKPIDD(GSTRIN,NCD,KNCR,STR)
      ELSE
        KERROR = 140
      ENDIF
      GOTO 9999



 9999 CONTINUE
      END
