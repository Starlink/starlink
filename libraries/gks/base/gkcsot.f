C# IL>=a, OL>=0
      SUBROUTINE GKCSOT(INDEX)
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
*     CSS : Output a record
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP INDEX  Page to output
*
      INTEGER INDEX
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkdt.par'
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
*
*     Page is output and then FREEd
*
*---------------------------------------------------------------------



*     Outputs a record to CSS

      WRITE(KCSFLU,REC=KCSREC(INDEX)) KCSNXT(INDEX),
     :                               (KCSDAT(I,INDEX), I=1,KCSMXD)
      CALL GKCSUL(INDEX)
      KCSREC(INDEX) = KNIL
      KCSNXT(INDEX) = KNIL
      END
