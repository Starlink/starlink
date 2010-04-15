C# IL>=a, OL>=0
      SUBROUTINE GKMPL(NCD,STR)
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
*  Interprets a Polyline item from a metafile.
*
*  MAINTENANCE LOG
*  ---------------
*     20/07/83  DSG   Original version stabilized
*     29/11/83  DSG   FORTRAN binding changes
*     21/02/86  DCS   Replace call to GKSCTM by its contents.
*                     Remove unused locals I, ISETY.
*     19/01/87  DCS   IS conversion. Remove metafile index and get
*                     polyline attributes from GKS State List.
*     13/08/90  KEVP  Put in error checking for stack (S311).
*     08/03/91  DCS   Corrected updating of the normalisation
*                     transformation and clipping information (S475).
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
*     Read   /GKWKE/  KPL
*     Read   /GKWCA/  KDAT,KRGN
*     Modify /GKYSL/  KSTRWK,KSPLWK
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
*
*  Local integer array
      INTEGER ILA(1)
*
*  Fraction of stack in use at workstaion call time
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
      CALL GKUPSI(IAT,STR,1,ILA)
      IAT=IAT+KPDSSZ
      NUMPTS = ILA(1)
      IF ( NUMPTS.LT.2 ) THEN
         KERROR = 100
         GOTO 999
      ELSE

*
*      Bring polyline attributes up-to-date
*
        IF (KSPLWK.NE.KGKSFN) THEN
*         Setting transformation and clipping information as in GKS state
*         list is not required for Polyline since it has no geometric
*         attributes. Therefore we go straight to updating the attributes.
          CALL GKCPLG
          IF(KERROR.NE.0) GOTO 999
          CALL GKSACW(KSPLA,1,KDAT,1,QDAT,QDAT,1,CH)
          IF (KERROR.NE.0) GOTO 999
*         Set flag
          KSPLWK=KGKSFN
        ENDIF

*
*       Bring transformation and clip information up-to-date.
*
        IF (KSTRWK.NE.KMI) THEN
          CALL GKCCTM
          IF(KERROR.NE.0) GOTO 999
          CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
          IF(KERROR.NE.0) GOTO 999
*         Set flag
          KSTRWK=KMI
        ENDIF

      ENDIF

*  Send points of the polyline to all active workstations.
*  If the amount of local storage space available is too small
*  to hold all of the points,
*  then more than one call will have to to be made to the
*  workstation interface. The last point of one sub-polyline
*  will have to be repeated at the beginning of the next, and
*  the broken line utility will have to continue correctly.

* Allow for the possibility that truncation occurred during metafile
* input:
      IF(NUMPTS.GT.(NCD*80-KPDSSZ)/(2*KPDRSZ)) THEN
        KERROR=165
        GOTO 999
      ENDIF
      NCHUNK=NUMPTS

      CALL GKSTQ(IRSZ,IISZ,ICSZ)
      NCHUNK=MIN0(NCHUNK,(INT(FRAC*IRSZ))/2)
      CALL GKSTAL(KREALS,NCHUNK*2,ISETX)
      IF(KERROR .NE. 0)GOTO 999
      NK=NCHUNK

* Unpack data record
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
        CALL GKSACW(KPL,1,KDAT,NK,QSTACK(ISETX),
     :                 QSTACK(ISETX+NK),1,CH)
        IF(KERROR.NE.0) THEN
          CALL GKSTDA(KREALS,ISETX)
          GOTO 999
        ENDIF
   20 CONTINUE

  990 CALL GKSTDA(KREALS,ISETX)
      IF (KRGN) THEN
        CALL GKRGN
        IF (KERROR.NE.0) GOTO 999
      ENDIF

  999 CONTINUE

      END
