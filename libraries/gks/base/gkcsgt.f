C# IL>=a, OL>=0
      SUBROUTINE GKCSGT(IREC,INDEX)
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
*     CSS : Get a record and return its INDEX
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IREC   Record number required
*     OUT INDEX  Page index of requested record
*
      INTEGER IREC, INDEX
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkcss.cmn'
*
*
*  COMMENTS
*  --------
*     A search is made to see if the record is already in memory.
*     It read if necessary.
*
*---------------------------------------------------------------------


      DO 1 INDEX = 1, KCSPAG
         IF ((KCSREC(INDEX).EQ.IREC) .AND.
     :       (KCSTYP(INDEX).NE.KCSFRE)) GO TO 2
    1 CONTINUE

      CALL GKCSFP(INDEX)
      KCSREC(INDEX) = IREC
      CALL GKCSIN(INDEX)

    2 CONTINUE
      CALL GKCSLK(INDEX)
      END
