      SUBROUTINE GK2MIW (NID,IDAT)
*---------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  This routine is given a list of integers,
*  converts them to hex, and writes them to the CGM
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*        NID    : No of integers to be written to CGM
*        IDAT   : Array containing integers
*
      INTEGER NID, IDAT(NID)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwdt.par'

      INCLUDE '../../include/gkwca.cmn'
*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgm.cmn'


*  LOCALS
*  ------
*        I      : Loop counter
*        INCHAR : Used as a temporary hex variable

      INTEGER I
      CHARACTER INCHAR*81

*---------------------------------------------------------------

*   Convert integers to hex & write to CGM
      DO 100 I=1,NID
         CALL GK2MIH(IDAT(I),INCHAR,1,.FALSE.)
         CALL GK2MBU(INCHAR)
  100 CONTINUE
      RETURN
      END
