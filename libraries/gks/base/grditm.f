C# IL>=a, OL>=0
      SUBROUTINE GRDITM (IWKID,MNCD,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    USER-CALLABLE
*  GKS Function name:  READ ITEM FROM GKSM
*  Author:             DSG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
* GKS returns the current item on the GKSM back to the application
* program then makes the next item in the metafile the current item.
* If the item data record length is greater than 'maximum item data
* record length', the excess parts of the item are lost. (Note: by
* specifying 'maximum item data record length'=0, the current item
* can be skipped).
* Any program which makes use of the access that this function
* provides to the content of GKSM items is using information
* that is not part of the standard, viz. the format and
* content of metafile items.
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  DSG   Original version stabilized
*     28/06/83  DSG   Change in error reporting
*     17/11/83  DSG   Binding change: CHARACTER*80 data records
*     02/10/85  JRG   Remove illegal character (bug S96)
*     20/01/87  CJC   IS conversion. MNCD and NCD have changed meanings.
*
*  ARGUMENTS
*  ---------
*    INP   IKWID  Workstation identifier
*     INP   MNCD     Length of item data record in characters
*     INP   NCD      Dimension of item data record
*    OUT   STR    Item data record (CHARACTER*80 array)
*
      INTEGER IWKID, MNCD, NCD
      CHARACTER*80 STR(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYERR/    KERROR
*     Modify /GKYWCA/    KWI1   maximum item data record length
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'

*   COMMENTS
*   --------
*
*   Implementation decision: data records were not packed by
*    the packing utility
*
*
*---------------------------------------------------------------------



      CALL GKPRLG(ERDITM,GWSOP,GSGOP)

      IF(KERROR.NE.0) THEN
        CALL GKERR(KERROR)
      ELSE
        KWI1=MNCD
        CALL GKSONW (IWKID,KRDITM,1,KDAT,1,QDAT,QDAT,NCD,
     :   STR)

        IF(KERROR.NE.0) CALL GKERR(KERROR)
      ENDIF

      RETURN
      END
