C# IL>=a, OL>=2
      SUBROUTINE GK0WWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             MGC
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     WISS - Workstation Independent Segment Storage
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilized
*     18/05/87  DCS   Removed 'stabilized' maintenance log entries from
*                     other GK0Wxx routines. Tidied layout.
*     18/05/87  DCS   Corrected error messsage for INQ WK CLASSN (S265).
*     01/05/90  RMK   Added check on con. id. to Open Wkstn entry.
*
*  ARGUMENTS
*  ---------
*     INP IENT  - Entrypoint code
*     INP NID   - Size of array IDAT
*     I/0 IDAT  - Integer data passed to or from workstation
*     INP NRD   - Size of arrays RX and RY
*     I/O RX    - Real X-coordinate data passed to or from workstation
*     I/O RY    - Real Y-coordinate data passed to or from workstation
*     INP NCD   - Size of character array
*     I/O STR   - Character array
*
      INTEGER IENT, NID, IDAT(NID), NRD, NCD
      REAL RX(NRD), RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*     None
*
*  EXTERNALS
*  ---------
      EXTERNAL GKIWSL, GKCWSL, GKSLDL, GKQWK
*
*
*  STACK USAGE
*  -----------
*     None
*
*  ERRORS
*  ------
*   32  Specified workstation is not of category MO
*   34  Specified workstation is not of category MI
*   36  Specified workstation is Workstation Independent Segment Storage
*   37  Specified workstation is not of category OUTIN
*   38  Specified workstation is not of category INPUT nor OUTIN
*   39  Specified workstation is not of category OUTPUT nor OUTIN
*
*  COMMENTS
*  --------
*
*
* --------------------------------------------------------------------



* Conditional GOTO on entrypoint code : 0<IENT<94 < 120

      GOTO (       10,  20,  30,7736,7736,7736,  70,8888,9999,
     :      7736,8888,8888,8888,8888,8888,8888,8888,8888,8888,
     :      8888,8888,8888,7736,7736,7736,7736,7736,7736,9999,
     :      9999,8888,7736,7736,9999,9999,9999,9999,9999,9999,
     :      9999, 410, 410, 410, 410, 410, 410, 410, 410, 410,
     :       410, 410,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999,7738,7738,7738,7738,7738,7738,8888,7738,7738,
     :      7738,7738,7738,7738,7738,7738,7738,7738,7738,7738,
     :      7738,8888,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 910, 920, 930) IENT


* Conditional GOTO on entrypoint code : 119<IENT<199

      GOTO (7739,7739,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1300,7736,7736,7736,7736,7736,7736,7736,7739,7736,
     :      7736,7736,7736,7736,7736,7736,1460,7738,7738,7738,
     :      7738,7737,7738,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1700,7739,7736,7739,7739,7739,7739,7739,7739,7739,
     :      7739,7739,7739,7739,7739,7739,7739,7739,7739,7739,
     :      7739,7739,7738,7738,7738,7738,7738,7738,7738) IENT-119
      GOTO 9999



*     [1..11] : CONTROL FUNCTIONS
*
*
* Open workstation
* ----------------
 10   CONTINUE
*     data expected
*     KWI1   : connection identifier
*     KWKTYP : workstation type
*     data returned
*     KWI1   workstation category (WISS)
*     KERROR error response or zero

*     set up workstation state list and WDT
      CALL GKIWSL(KWKIX,KWKTYP)
      IF (KERROR.NE.0) GOTO 9999
*     check connection identifier
      IF (KWI1.LT.KMNIO .OR. KWI1.GT.KMXIO) KERROR = 21
      IF (KERROR.NE.0) GOTO 9999
*     save connection identifier
      KCID(KWKIX) = KWI1
*     implicit regeneration - suppressed
      KIMRGM(KWKIX) = GSUPPD
*     set segment list pointer
      KSSGPT(KWKIX) = KNIL
*     workstation category
      KWI1 = GWISS
      GOTO 9999

* Close workstation
* ----------------
 20   CONTINUE
*     Data expected
*     KWI1 : first/second pass

*     first pass
      IF(KWI1.EQ.1) THEN
*       indicate second pass expected
        KWDONE=KRFUSE
*     second pass
      ELSE
*       delete ws segments
        CALL GKSLDL(KSSGPT(KWKIX))
*       deallocate ws state list heaps
        CALL GKCWSL(KWKIX)
      ENDIF
      GOTO 9999

* Clear workstation (F/E announcement before clr surface)
* -------------------------------------------------------
 30   CONTINUE
*     Data expected:
*     KWI1 : first/second pass
*     KWI2 : conditional - Clear Control Flag (ignored)

*     second pass
      IF(KWI1.EQ.2) THEN
*       delete ws segments
        CALL GKSLDL(KSSGPT(KWKIX))
*       indicate clear surface entry not required
        KWDONE = KACEPT
      ENDIF
      GOTO 9999


* Do deferred output actions
* --------------------------
 70   CONTINUE
*     Data returned:
*     KWI1 : new frame action necessary at update

      KWI1 = KNFAUP(KWKIX)
      GOTO 9999

*     [12..17] : OUTPUT FUNCTIONS
*     [18..22] : WORKSTATION ATTRIBUTES (REALISED)
*     [23..28] : WORKSTATION ATTRIBUTES (REPRESENTATIONS)
*     [31..33] : TRANSFORMATIONS
*
* Refer to entrypoint jump table


*     [41..51] : SEGMENTS
*
 410  CONTINUE
      CALL GKSGWK(IENT,.FALSE.)
      GOTO 9999


*     [61..66] : INPUT FUNCTIONS - INITIALISATION
*     [67..68] : INPUT FUNCTIONS - SET MODE
*     [69..74] : INPUT FUNCTIONS - REQUEST
*     [75..80] : INPUT FUNCTIONS - SAMPLE
*     [81..81] : INPUT FUNCTIONS - EVENT
*
* Refer to entrypoint jump table


*     [91..93] : METAFILE FUNCTIONS


* Write item to GKSM
* ------------------
 910  CONTINUE
      KERROR = 32
      GOTO 9999

* Get item type from GKSM
* -----------------------
  920 CONTINUE
      KERROR = 34
      GOTO 9999

* Read item from GKSM
* -------------------
 930  CONTINUE
      KERROR = 34
      GOTO 9999


*     [120..121] : INQUIRY FUNCTIONS - PIXEL ARRAY
*
* Refer to entrypoint jump table


*     [130..152] : INQUIRY FUNCTIONS - WS STATE LIST


* Inquire workstation connection and type
* ---------------------------------------
 1300 CONTINUE
*     Data returned:
*     KERROR : error indicator
*     KWI1   : connection identifier
*     KWI2   : workstation type

      CALL GKQWK (IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999

* Inquire Set Members of Segment Names on Workstation
* ---------------------------------------------------
 1460 CONTINUE
*     data expected
*     KWI1    : list element requested
*     Data returned:
*     KERROR : error indicator
*     KWI1   : no of segment names
*     KWI2   : Nth member of set of stored segments for this
*                workstation
*
      CALL GKQWK (IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KSGRQ = KSGRQ + 1
      GOTO 9999

*     [170..198] : INQUIRY FUNCTIONS - WS DESC TABLE


* Inquire Workstation Category
* ----------------------------
 1700 CONTINUE
*     Data returned:
*     KWI1   : Workstation Category

      KWI1 = GWISS
      GOTO 9999


*     ERRORS - WORKSTATION CATEGORY


* Specified workstation is Workstation Independent Segment Storage
* ----------------------------------------------------------------
 7736 CONTINUE
      KERROR=36
      GOTO 9999

* Specified workstation is not of category OUTIN
* ----------------------------------------------
 7737 CONTINUE
      KERROR=37
      GOTO 9999

* Specified workstation is not of category INPUT nor OUTIN
* --------------------------------------------------------
 7738 CONTINUE
      KERROR=38
      GOTO 9999

* Specified workstation is not of category OUTPUT nor OUTIN
* ---------------------------------------------------------
 7739 CONTINUE
      KERROR=39
      GOTO 9999


*     EXITS


* Do nothing
* ----------
 8888 CONTINUE

* Return
* ------
 9999 CONTINUE
      RETURN
      END
