C# IL>=a, OL>=0
      SUBROUTINE GWITM (IWKID, ITYPE, IDRL, NCD, STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    USER-CALLABLE
*  GKS Function name:  WRITE ITEM TO GKSM
*  Author:             DSG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
* An item containing non-graphical information provided by the
* the application program is written to the GKSM. The
* parameters 'item data record' and 'item data record
* length' define the data to be output whilst 'item type'
* specifies its type.
*
* Note: This function can only be used to transfer non-
* graphical information to GKSM. Graphical data is sent
* automatically after a workstation of category MO has been
* activated.
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  DSG   Original version stabilized
*     28/06/83  DSG   Change in error reporting
*     20/01/87  CJC   IS conversion. Extra argument - IDRL.
*     14/07/87  RMK   Added declaration for argument IDRL.
*     06/08/90  KEVP  Removed unused variable (S342).
*
*  ARGUMENTS
*  ---------
*     INP   IWKID   Workstation identifier
*     INP   ITYPE   Item type
*     INP   IDRL    Item data record length in characters
*     INP   NCD     Dimension of item data record
*     INP   STR     Item data record
*
      INTEGER  IWKID, ITYPE, IDRL, NCD
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYERR/    KERROR
*     Modify /GKYWCA/    KWI1   Metafile item type
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'

*
*---------------------------------------------------------------------



      CALL GKPRLG(EWITM,GWSAC,GSGOP)
      IF(KERROR.NE.0) THEN
        CALL GKERR(KERROR)
      ELSE
        KWI1=ITYPE
        KWI2 = IDRL
        CALL GKSONW (IWKID,KWITM,1,KDAT,1,QDAT,QDAT,
     :   NCD,STR)
        IF(KERROR.NE.0) CALL GKERR(KERROR)
      ENDIF

      RETURN
      END
