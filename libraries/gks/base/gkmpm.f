C# IL>=a, OL>=0
      SUBROUTINE GKMPM(NCD,STR)
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
* Metafile interpreter polymarker output primative routine
*
*  MAINTENANCE LOG
*  ---------------
*     21/07/83  DSG   Original version stabilized
*     29/11/83  DSG   FORTRAN binding changes
*     21/02/86  DCS   Replace call to GKSCTM by its contents.
*                     Remove unused locals I, ISETY.
*     19/01/87  DCS   IS conversion. Remove metafile index and get
*                     polymarker attributes from GKS State List.
*     13/08/90  KEVP  Put in stack error checking (S311).
*     08/03/91  DCS   Corrected updating of the normalisation
*                     transformation and clipping information (S475).
*
*  ARGUMENTS
*  ---------
*     INP  NCD     Length of data record
*     INP  STR     Data record
*
      INTEGER NCD

      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKS/     GWSAC,GSGOP
*     Read   /GKYWKE/  KPM
*     Read   /GKYWCA/  KDAT,KRGN
*     Modify /GKYSL/   KSTRWK,KSPMWK
*     Update /GKYSTK/  Stack
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
     :        ILA(1),IRSZ,IISZ,ICSZ
*
*  Fraction of stack in use at workstation call time
      REAL FRAC
      PARAMETER (FRAC = 0.49)
*
*  STACK USAGE
*  -----------
*
*
*  ERRORS
*  ------
*     100   number of points invalid
*
*---------------------------------------------------------------------


      IAT=1
      CALL GKUPSI(IAT,STR,1,ILA)
      IAT=IAT+KPDSSZ
      NUMPTS=ILA(1)

      IF ( NUMPTS.LT.1 ) THEN
        KERROR = 100
        GOTO 999
      ELSE

*
*       Bring polymarker atrributes up-to-date
*
        IF (KSPMWK.NE.KGKSFN) THEN
*         Setting transformation and clipping information as in GKS state
*         list is not required for Polymarker since it has no geometric
*         attributes. Therefore we go straight to updating the attributes.
          CALL GKCPMG
          IF(KERROR.NE.0) GOTO 999
          CALL GKSACW(KSPMA,1,KDAT,1,QDAT,QDAT,1,CH)
          IF(KERROR.NE.0) GOTO 999
*         Set flag
          KSPMWK=KGKSFN
        ENDIF

*
*       Bring transformation and clip info. up-to-date
*

        IF (KSTRWK.NE.KMI) THEN
          CALL GKCCTM
          IF(KERROR.NE.0) GOTO 999
          CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
          IF(KERROR.NE.0) GOTO 999
* Set flag
          KSTRWK=KMI
        ENDIF

      ENDIF
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
        CALL GKSACW(KPM,1,KDAT,NK,QSTACK(ISETX),
     :    QSTACK(ISETX+NK),1,CH)
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
