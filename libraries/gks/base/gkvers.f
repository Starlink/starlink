      SUBROUTINE GKVERS(IERFIL)
*
* (C) COPYRIGHT ICL & SERC
*

*----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             SHS/CJC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE  OF THE ROUTINE
*  -----------------------
*     Identify the current version number of GKS if system requires it
*
*  MAINTENANCE LOG
*  ---------------
*     09/01/86  DRJF  Added subroutine header
*     24/06/86  RMK   New version number for master source release.
*     31/03/87  RMK   Changed to pick up message from GKMC.PAR.
*
*  STARLINK MODIFICATION
*  ---------------------
*     Printing of message suppressed.
*
*  ARGUMENTS
*  ---------
*     INP  IERFIL - Unit number of error file
*
      INTEGER IERFIL
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkmc.par'
*
*---------------------------------------------------------------------------


*      WRITE(IERFIL,100) CVERS
*  100 FORMAT(A40)
      END
