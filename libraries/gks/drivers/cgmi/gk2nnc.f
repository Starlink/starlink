      SUBROUTINE GK2NNC(ICODE,ADVANC)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) Workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine gets the next ASCII character from
*  the buffer & converts it into an integer code.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver GK2NWD
*
*  ARGUMENTS
*  ---------
*   out  ICODE  : ASCII Character as an integer (Its code)
*   in   ADVANC : Flag for whether to advance pointer or not
*
      INTEGER ICODE
      LOGICAL ADVANC
*
*  COMMON BLOCKS
*  -------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'

*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgn.cmn'

*---------------------------------------------------------------------

      IF (KCDPTR(KWKIX).GT.KLENRC) THEN
         KCDPTR(KWKIX)=KCDPTR(KWKIX)-KLENRC
         KCDREC(KWKIX)=KCDREC(KWKIX)+1
      ENDIF
      ICODE=ICHAR(CMFDR(KCDREC(KWKIX))(KCDPTR(KWKIX):KCDPTR(KWKIX)))

*   Advance pointer if necessary
      IF(ADVANC) THEN
         KCDPTR(KWKIX)=KCDPTR(KWKIX)+1
      ENDIF
      RETURN
      END
