C# IL>=a, OL>=0
      SUBROUTINE GQGDP (IWTYPE,IDGDP,IER,NATTR,IATTR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE GENERALIZED DRAWING PRIMITIVE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns GDP
*
*  MAINTENANCE LOG
*  ---------------
*     10/10/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     INP IDGDP  - GDP identifier
*     OUT IER    - error indicator
*     OUT NATTR  - number of sets of attributes used
*     OUT IATTR  - list of sets of attributes used
*
      INTEGER IWTYPE, IDGDP, IER, NATTR, IATTR(4)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        KWI1 = IDGDP
        CALL GKSONW(IWTYPE,KQGDP,4,IATTR,1,QDAT,QDAT,1,CH)
        IER = KERROR
        IF (KERROR.EQ.0) NATTR = KNIR
      ELSE
        IER = KERROR
      ENDIF

      END
