C# IL>=a, OL>=0
      SUBROUTINE GOPKS(IERRF,IBUFA)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  OPEN GKS
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To Open GKS.  Certain items of data must be already initialised
*     even before OPEN GKS, so that inquiries and erroneous calls are
*     handled properly.
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  JRG   First version
*     16/03/83  JRG   More commenting
*     030/3/83  JRG   Commenting out initialisation of ASF's until
*                     data structure (GKYSL) corrected.
*                     CHECK.INC included
*     11/04/83  JRG   Initialise ASF's in groups
*     29/04/83  JRG   Initialise ASCII marker codes,   set KDBFLS to indicate
*                     file closed,   KOFF from INCLUDE file,   initialise
*                     workstation types from WDT file.
*     03/05/83  JRG   Initialise the regen flags KRGN and KWRGN(1:KWK)
*     09/05/83  JRG   Initial text precision corrected to be 'strngp'
*     24/05/83  JRG   Reordering of initialisation of stack and file indicators,
*                     change name GCONST to GKYCON
*     10/06/83  JRG   Colour table pointer in W.C.B. no longer initialised
*                     (does not exist)
*     13/06/83  JRG   Open Heap.
*                     Initialise metafile index table.
*     20/06/83  JRG   Pattern initialisation removed.
*     23/06/83  JRG   Changes to use KERROR for error reporting
*     17/08/83  JRG   Some tidying up
*     09/11/83  JRG   Initialise text font storage
*     22/11/83  JRG   Comment changes
*     25/11/83  JRG   New variables (KCVIS,KCHLT,KNUMSG)
*     30/11/83  JRG   Check error after calling GKXON
*     10/12/83  JRG   Remove stack INCLUDE (unused)
*     12/12/83  JRG   Remove call to GKXON (not needed)
*     20/12/83  AS    Take out KERRWK
*     21/12/83  JRG   Remove all references to event reports
*     04/01/84  JL    Add call to GKVERS - print version number
*                     on error file
*     06/06/84  CJW   Use KMNIO, KMXIO to validate error file (I222)
*     09/05/85  RMK   Set current font to undefined (S129).
*     01/08/85  MGC   Set control flag for pick (include GKPCA.CMN)
*     03/09/85  RMK   Initialised QWOLDX, QWOLDY (S158).
*     20/01/87  DCS   IS conversion. Initialise new and changed entries
*                     in GKS State List (character width and base
*                     vector, pattern vectors, clipping rectangle).
*                     Delete entries removed from GKS Derived Data
*                     (GCWKIT and clipping rectangle). Reorder for
*                     ISO 7942.
*                     Remove metafile index from Workstation Control
*                     Block.
*     21/01/87  PKY   IS conversion. Add argument IBUFA (not used in
*                     this implementation).
*     22/01/87  JCS   IS conversion. Remove parameter for setting error
*                     number. Error numbers changed.
*     03/06/87  RMK   Merged ICL and RAL versions of this routine.
*                     Added call to GKIOOP and moved GKVERS call to
*                     beginning of routine (S204).
*     10/07/89  RMK   Initial value of pick identifier KCPCID should
*                     be 0 (S349).
*
*
*  ARGUMENTS
*  ---------
*     Inp  IERRF   Error stream number. Has machine dependent range.
*     Inp  IBUFA   Amount of memory units
*
      INTEGER IERRF,IBUFA
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYOPS/  GKS Operating State
*     Modify /GKYDT/   Setting up workstation types
*     Modify /GKYWKD/  Initialise variables for linetype simulation
*     Modify /GKYCON/  Set up ASCII codes for marker types
*     Modify /GKYSL/   Whole of GKS State List initialised
*     Modify /GKYDD/   Whole of GKS Derived Data initialised
*     Modify /GKYWCA/  Initialise regen flags in W.C.A.
*     Modify /GKYWCB/  Initialise Workstation Control Block to indicate
*                      that no workstations are open
*     Modify /GKYFLS/  Initialise file indicators to say all files are shut
*     Modify /GKYERR/  Initialise Error State List
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkdt.cmn'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkcon.cmn'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkdd.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkerr.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkpca.cmn'
*
*  LOCALS
*  ------
*     J       Loop counter
*
      INTEGER J
*
*  ERRORS
*  ------
*        1   Wrong GKS state
*      200   Error file
*      307   Error reading Workstation Description Table
*
*---------------------------------------------------------------------




*   GKS Prologue
      CALL GKPRLG (EOPKS,GGKCL,GGKCL)
      IF( KERROR.NE.0 ) GOTO 990

*   Open error stream and store the stream number in KERRFL.
      CALL GKIOOP(KFERR, IERRF, KERRFL)
      IF (KERROR.NE.0) GOTO 990
*   Output the system message on the error stream.
      CALL GKVERS(KERRFL)

*   Initialise GKYCON COMMON. Entries which are constant throughout
*   an execution of GKS.
*   ASCII codes for marker types 1 to 5.
      KMKASC(1)=46
      KMKASC(2)=43
      KMKASC(3)=42
      KMKASC(4)=79
      KMKASC(5)=88

*   Initialise stack
      CALL GKSTOP

*   Open heap
      CALL GKHPOP

*   Read list of workstation types from WDT file. Expect a list of
*   positive numbers followed by KNIL's. The dummies KDAT & QDAT are from
*   the Workstation Commumication Area (it so happens that this is
*   already INCLUDEd in this source).
      CALL GKQWDT(KDAT(1), KWKLST,KNONE,KMXWKT,1,KLAWKT(1),QDAT)
      IF( KERROR.NE.0 ) GOTO 990
      DO 30 J=1,KMXWKT
        IF( KLAWKT(J).EQ.KNIL ) GOTO 35
   30 CONTINUE
      J=KMXWKT+1
   35 KAWKT=J-1
      IF( KAWKT.LE.0 ) THEN
        KERROR=307
        GOTO 990
      ENDIF
*   Element 0 is CSS. Put in there an arbitrary value different from
*   other workstation types.
      KLAWKT(0)=-2

*   Initialise GKS State List: sequence is as in ISO 7942, except
*   that ASFs are taken before everything else.

*   Attributes.
      DO 50 J=1,3
   50   KCPLAF(J)=GBUNDL

      DO 60 J=1,3
   60   KCPMAF(J)=GBUNDL

      DO 70 J=1,4
   70   KCTXAF(J)=GBUNDL

      DO 80 J=1,3
   80   KCFAAF(J)=GBUNDL

      KCPLI=1
      KCLNTY=1
      QCLNWD=1.0
      KCPLCI=1

      KCPMI=1
      KCMKTY=3
      QCMKSZ=1.0
      KCPMCI=1

      KCTXI=1
      KCTXFN=1
      KCTXPR=GSTRP
      QCCHXP=1.0
      QCCHSP=0.0
      KCTXCI=1
      QCCHH=0.01
      QCCHUX=0.0
      QCCHUY=1.0
      QCCHW=0.01
      QCCHBX=1.0
      QCCHBY=0.0
      KCCHP=GRIGHT
      KCHZCH=GAHNOR
      KCVTCH=GAVNOR

      KCFAI=1
      KCFAIS=GHOLLO
      KCFASI=1
      KCFACI=1
      QCPAWX=1.0
      QCPAWY=0.0
      QCPAHX=0.0
      QCPAHY=1.0
      QCPAX=0.0
      QCPAY=0.0

      KCPCID=0

*   Transformations and clipping
      KCNTN=0
*                              (maxm norm trans is KT)
      DO 150 J=0,KT
        KTNOVP(J)=J
        QLWXL(J)=0.0
        QLWXR(J)=1.0
        QLWYB(J)=0.0
        QLWYT(J)=1.0
        QLVPXL(J)=0.0
        QLVPXR(J)=1.0
        QLVPYB(J)=0.0
        QLVPYT(J)=1.0
  150 CONTINUE

      KCLIN=GCLIP

*   Current clipping rectangle
      QCCLXL=0.0
      QCCLXR=1.0
      QCCLYB=0.0
      QCCLYT=1.0
*   Segments
      KOPSG=KNIL
      KSGLST=KNIL

*   Initialise source flags for transformations and attributes
      KSTRWK=KGKSFN
      KSPLWK=KGKSFN
      KSPMWK=KGKSFN
      KSTXWK=KGKSFN
      KSFAWK=KGKSFN

*   Miscellaneous
      KNUMSG=0

*   Initialise GKS Derived Data
      KSSG=.FALSE.
      KAIPR=.FALSE.

*   Set current font to undefined
      KURFON = 0

*   Initialise variables for linetype simulation
      DO 160 J=1,KWK
        QWOLDX(J) = -99.0
        QWOLDY(J) = -99.0
  160 CONTINUE

*   Initialise those entries in Workstation Communication Area that
*   are not set up each time i.e. the 'regen' and segment playback entries
      DO 180 J=1,KWK
  180   KWRGN(J)=.FALSE.
      KRGN=.FALSE.
      KCVIS=GVISI
      KCHLT=GNORML

*   And the control flag for pick
      KPKECO=KNIL

*   Initialise Workstation Control Block (max open wkstns = KWK)
*   First fill workstation type index for CSS
      KWTYIX(0)=0
      DO 200 J=1,KWK
        KWKID(J)=KWKINV
        KOPPT(J)=KWKINV
        KACPT(J)=KWKINV
        KWTYIX(J)=KWKINV
  200 CONTINUE
      KNOPWK=0
      KNACWK=0

*   ?????? free space pointers for input devices ???????

*   Set GKS Operating State
      KOPS=GGKOP

      GOTO 999

*   Report error
  990 CALL GKERR(KERROR)

*   Finally ...
  999 CONTINUE
      END
