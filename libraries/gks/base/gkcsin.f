C# IL>=a, OL>=0
      SUBROUTINE GKCSIN(INDEX)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CSS : Read a record from CSS
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP INDEX  Page to input
*
      INTEGER INDEX
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkcss.cmn'
      INCLUDE '../include/gkfls.cmn'
*
*  LOCAL
*  -----
*     I      Loop index
*
      INTEGER I
*
*  COMMENTS
*  --------
*     Page is input and LOCKed ( FREE --> UNLOCK )
*
*---------------------------------------------------------------------



*     Inputs a record from CSS

      READ(KCSFLU,REC=KCSREC(INDEX)) KCSNXT(INDEX),
     :                               (KCSDAT(I,INDEX), I=1,KCSMXD)
      CALL GKCSLK(INDEX)
      END
