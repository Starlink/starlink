      SUBROUTINE GK2NCH(NCHARS,CASTR)
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
*  This routine gets a character string from the CGM
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver GK2NWD
*
*  ARGUMENTS
*  ---------
*
*     in   NCHARS  : Number of chars to be converted
*     out  CASTR   : Output Character String

      INTEGER NCHARS
      CHARACTER*(*) CASTR

*  COMMON BLOCKS
*  -------------
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'

*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgn.cmn'

*  LOCALS
*  ------
*        N      : Loop counter
*        TEMP   : Temporary variable

      INTEGER  N, TEMP

      CHARACTER*1 AN1
*
*---------------------------------------------------------------------

      DO 10 N=1,NCHARS
         CALL GK2NNC(TEMP,.TRUE.)

*  Get ASCII character from number TEMP
         CALL GKATON(1,TEMP,AN1)
         CASTR(N:N)=AN1
  10  CONTINUE

      RETURN
      END
