      SUBROUTINE GK0NIR (NVAL, RVAL)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION
*  Author:             RTP
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Converts Real values input as Integer using conversion factor
*
*  MAINTENANCE LOG
*  ---------------
*
*      14/06/89  RTP  Created
*
*  ARGUMENTS
*  ---------
*     INP    NVAL  Number of Values to convert
*     IN/OUT RVAL  First REAL Value to convert
*
      INTEGER NVAL
      REAL RVAL(NVAL)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/    Workstation index
*     Read   /GKYWKD/    Derive workstation I/O channel i.d.
*                        KWKDAT usage is described in GK0NWD.
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     I        Do loop variable
*
      INTEGER I
*
*  ERRORS
*  ------
*     None.
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      DO 100 I = 1, NVAL
         RVAL(I) = ( RVAL(I) - KWKDAT(17,KWKIX) ) / KWKDAT(18,KWKIX)
 100  CONTINUE

      RETURN
      END
