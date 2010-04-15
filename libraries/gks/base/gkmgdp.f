C# IL>=a, OL>=0
      SUBROUTINE GKMGDP(NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             DSG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Interprets a GDP item from a metafile.
*
*  MAINTENANCE LOG
*  ---------------
*     30/11/83  DSG   Original version stabilized
*     23/03/84  DSG   Fix of bug S33 - Code for GDP points updated
*     02/05/84  PGLS  Bring FA attributes up to date: bug I202
*     21/02/86  DCS   Replace call to GKSCTM by its contents.
*                     Bring PM and TX attributes up to date for
*                     consistency with GGDP.
*                     Remove unused locals I, ISETY.
*     19/01/87  DCS   IS conversion. Remove metafile index and get
*                     primitive attributes from GKS State List.
*     03/04/87  RMK   Corrected GKSFN to KGKSFN.
*     02/10/87  DSG   Bug fix S280: to allow the interpretation of
*                     negative integers from the GKSM.
*     13/08/90  KEVP  Put error checks in for stack allocation (S311).
*
*  ARGUMENTS
*  ---------
*     INP   NCD   Item data record length
*     INP   STR   CHARACTER*80 Item data record
*
      INTEGER NCD
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKS/    GWSAC,GSGOP
*     Read   /GKWKE/  KGDP
*     Read   /GKWCA/  KDAT,KRGN
*     Modify   /GKYSL/  KSTRWK,KSPLWK,KSPMWK,KSTXWK,KSFAWK
*     Update /GKSTK/  KSTACK, QSTACK
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkpid.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkstk.cmn'
*
*  LOCALS
*  ------
      INTEGER J,K,NCHUNK,NUMPTS,NK,ISET,ISETX,IAT,
     :        IRSZ,IISZ,ICSZ

*  Local arrays
      INTEGER ILA(2)
      REAL RLA(6)

*  GDP identifier
      INTEGER IDGDP
*
* Fraction of stack in use at workstation call time
      REAL FRAC
      PARAMETER (FRAC = 0.49)
*
*  STACK USAGE
*  -----------
*  The data record is unpacked into the stack, and then repacked into
*  the X and Y arrays using further stack space. Not more than half
*  the stack is used in total.
*
*  ERRORS
*  ------
*     100  number of points invalid
*
*---------------------------------------------------------------------


      IAT=1
      CALL GKUPI(IAT,STR,1,ILA)
      IAT=IAT+KPDISZ
      IDGDP=ILA(1)
      KWI1=IDGDP
      CALL GKUPSI(IAT,STR,1,ILA)
      IAT=IAT+KPDSSZ
      NUMPTS = ILA(1)
      IF ( NUMPTS.LT.3 ) THEN
         KERROR = 100
      ELSE

* Bring transformation and clip information up-to-date.


* Bring polyline attributes up-to-date
        IF (KSPLWK.NE.KGKSFN) THEN
*          Setting transformation and clipping information as in GKS state
*          list not required for Polyline because it has no geometric
*          attributes.
           CALL GKCPLG
           IF(KERROR.NE.0) GOTO 999
           CALL GKSACW(KSPLA,1,KDAT,1,QDAT,QDAT,1,CH)
           IF (KERROR.NE.0) GOTO 999
*          Set flag
           KSPLWK=KGKSFN
        ENDIF

* Bring polymarker attributes up-to-date
        IF (KSPMWK.NE.KGKSFN) THEN
*          Setting transformation and clipping information as in GKS state
*          list not required for polyline because it has no geometric
*          attributes.
           CALL GKCPMG
           IF(KERROR.NE.0) GOTO 999
           CALL GKSACW(KSPMA,1,KDAT,1,QDAT,QDAT,1,CH)
           IF (KERROR.NE.0) GOTO 999
*          Set flag
           KSPMWK=KGKSFN
        ENDIF

* Bring text attributes up-to-date
        IF (KSTXWK.NE.KGKSFN) THEN
*          Set transformation and clipping information as in GKS state list.
           IF(KSTRWK.NE.KGKSFN)THEN
             CALL GKCCTG
             IF(KERROR.NE.0) GOTO 990
             CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
             IF(KERROR.NE.0) GOTO 990
*            Set flag
             KSTRWK=KGKSFN
           ENDIF
*          Update attributes.
           CALL GKCTXG
           IF(KERROR.NE.0) GOTO 999
           CALL GKSACW(KSTXA,1,KDAT,1,QDAT,QDAT,1,CH)
           IF (KERROR.NE.0) GOTO 999
*          Set flag
           KSTXWK=KGKSFN
        ENDIF

* Bring fill area attributes up-to-date
        IF (KSFAWK.NE.KGKSFN) THEN
*          Set transformation and clipping information as in GKS state list.
           IF(KSTRWK.NE.KGKSFN)THEN
             CALL GKCCTG
             IF(KERROR.NE.0) GOTO 990
             CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
             IF(KERROR.NE.0) GOTO 990
*            Set flag
             KSTRWK=KGKSFN
           ENDIF
*          Update attributes.
           CALL GKCFAG
           IF(KERROR.NE.0) GOTO 999
           CALL GKSACW(KSFAA,1,KDAT,1,QDAT,QDAT,1,CH)
           IF (KERROR.NE.0) GOTO 999
*          Set flag
           KSFAWK=KGKSFN
        ENDIF
      ENDIF

*
*       Bring transformation and clip info. up-to-date
*
        IF (KSTRWK.NE.KMI) THEN
          CALL GKCCTM
          IF(KERROR.NE.0) GOTO 999
          CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
          IF(KERROR.NE.0) GOTO 999
*         Set flag
          KSTRWK=KMI
        ENDIF


*  Send points of the polyline to all active workstations.
*  If the amount of local storage space available is too small
*  to hold all of the points,
*  then more than one call will have to to be made to the
*  workstation interface. The last point of one sub-polyline
*  will have to be repeated at the beginning of the next, and
*  the broken line utility will have to continue correctly.

* Strip off the first three reserved points
      CALL GKUPR(IAT,STR,6,RLA)
      IAT=IAT+6*KPDRSZ
      NUMPTS=NUMPTS-3
      QWR1=RLA(1)
      QWR2=RLA(2)
      QWR3=RLA(3)
      QWR4=RLA(4)
      QWR5=RLA(5)
      QWR6=RLA(6)

* Allow for the possibility that truncation occurred during metafile
* input:
      IF(NUMPTS.GT.(NCD*80-KPDSSZ)/(2*KPDRSZ)) THEN
        KERROR=10
        GOTO 999
      ENDIF
      NCHUNK=NUMPTS
* Don't take more than half of the stack space
      CALL GKSTQ(IRSZ,IISZ,ICSZ)
      NCHUNK=MIN0(NCHUNK,(INT(FRAC*IRSZ))/2)
      CALL GKSTAL(KREALS,NCHUNK*2,ISETX)
      IF(KERROR .NE. 0) GOTO 999
      NK=NCHUNK

      DO 20 J=1,NUMPTS,NCHUNK
        CALL GKSTAL(KREALS,NCHUNK*2,ISET)
        IF(KERROR .NE. 0)GOTO 990
        IF(NUMPTS-J.LT.NCHUNK) NK=NUMPTS-J+1
        CALL GKUPR(IAT,STR,NK*2,QSTACK(ISET))
        IAT=IAT+2*NK*KPDRSZ

* Repack into an X array and a Y array
        DO 30 K=1,NK
          QSTACK(ISETX+K-1)=QSTACK(ISET+(K-1)*2)
          QSTACK(ISETX+NK+K-1)=QSTACK(ISET+(K-1)*2+1)
   30   CONTINUE

      CALL GKSTDA(KREALS,ISET)

* Send to all active worksataions
        CALL GKSACW(KGDP,1,KDAT,NK,QSTACK(ISETX),QSTACK(ISETX+NK),
     :    1,CH)
        IF(KERROR.NE.0) THEN
          CALL GKSTDA(KREALS,ISETX)
          GOTO 999
        ENDIF
   20 CONTINUE

  990 CALL GKSTDA(KREALS,ISETX)

*--------------------------------------------------------------
*
*  N.B. The GDP Data Record is not used in this implementation
*
*--------------------------------------------------------------

      IF (KRGN) THEN
        CALL GKRGN
        IF (KERROR.NE.0) GOTO 999
      ENDIF

  999 CONTINUE

      CALL GKUPSI(IAT,STR,2,ILA)
      IF(ILA(1)+ILA(2).GT.0) KERROR = 103

      END
