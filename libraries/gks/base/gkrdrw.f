C# IL>=a, OL>=1
      SUBROUTINE GKRDRW(IWKID)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To regenerate all (visible) segments on the specified workstation.
*
*  MAINTENANCE LOG
*  ---------------
*     1/11/83   JRG   Dummy version created
*     8/12/83   JRG   Full version created .... calls playback
*    21/12/83   JRG   Find the lowest priority segment
*    13/02/84   JRG   Correct bug which drew first segment even if
*                     was invisible
*
*  ARGUMENTS
*  ---------
*     INP  IWKID   Identifier of Workstation on which segments are to
*                  be regenerated.
*
      INTEGER IWKID
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WCA/    Set "original entrypoint" to be REDRAW ALL
*                     SEGMENTS ON WORKSTATION.
*     Read   /ERR/    Inspect KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*-----------------------------------------------------------------------


      KINENT=KRSGWK
      KWI1=KREL
      KWI2=KLOEST
      CALL GKSONW(IWKID,KQSGWK, 1,KDAT,1,QDAT,QDAT, 1,CH)
      IF( KERROR.EQ.0 .AND. KDAT(1).NE.KNIL ) THEN

*       Playback
          KRPCC=KRPVIS
          KRPTYP=KRPDRW
          KRPSG=KDAT(1)
          CALL GKSGPB(IWKID)
      ENDIF
*
      END
