C# IL>=a, OL>=0
      SUBROUTINE GGTITM(IWKID,ITYPE,IL)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    USER-CALLABLE
*  GKS Function name:  GET ITEM TYPE FROM GKSM
*  Author:             D.S.GREENAWAY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
* Inspects the type of the current item and the length of
*  its data record in the GKSM and returns type and length
*  back to the application program.
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  DSG   Original version stabilized.
*     28/03/83  DSG   Change in error reporting.
*     11/06/86  RMK   Removed commenting from CHECK.INC.
*     20/01/87  CJC   Remove unused locals IHFLD, IT, LFIELD.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID   Workstation identifier
*     OUT   ITYPE   Item type
*     OUT   IL      Item data record length
*
      INTEGER IWKID, ITYPE, IL
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYERR/  KERROR
*     Modify /GKYWCA/  KWI1,KWI2
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
*---------------------------------------------------------------------


      CALL GKPRLG(EGTITM,GWSOP,GSGOP)

      IF(KERROR.NE.0) THEN
        CALL GKERR(KERROR)
      ELSE
        CALL GKSONW (IWKID,KGTITM,1,KDAT,1,QDAT,QDAT,1,CH)
        IF(KERROR.EQ.0) THEN
          ITYPE = KWI1
          IL = KWI2
        ELSE
          CALL GKERR(KERROR)
        ENDIF
      ENDIF

      RETURN
      END
