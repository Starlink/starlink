C# IL>=a, OL>=0
      SUBROUTINE GKWDDL(IROOT)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Delete input device data record directory
*
*  MAINTENANCE LOG
*  ---------------
*     15/11/83  AS    Original version stabilized
*     10/12/83  AS    Change arguments so that JL can use it, not have a
*                     copy!
*     18/03/86  DSG   I246 - Error check done after GKSTAL call
*     30/04/86  RMK   Added include file GKERR.CMN.
*     29/07/87  PJWR  Rewritten to use FORTRAN data records because
*                     PIDs no longer in implementation.  Extended
*                     documentation.
*
*  ARGUMENTS
*  ---------
*     INP     IROOT   Heap pointer to pid directory
*
      INTEGER IROOT
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read    /GKYERR/  KERROR
*             /GKYHP/   KHP
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkhp.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     ITYPE   Data type,  which is also the directory entry key.  Used
*             as a loop index.
*     ISTR    String number in data record string table,  used as a loop
*             index
*     IDATA   Array for return of data type entry from data record
*             directory.
*     RDATA   Dummy real array for call to GKDRGE.
*
      INTEGER ITYPE, ISTR, IDATA(2)

      REAL RDATA(1)
*
*-----------------------------------------------------------------------

*     Check that there is a data record to delete.
      IF (IROOT.NE.KNIL) THEN
*       For each data type,  get the entry and deallocate heap structure
*       whose offset is in the entry.
        DO 20, ITYPE=MIN(KINTGS,KREALS,KCHARS),MAX(KINTGS,KREALS,KCHARS)
        CALL GKDRGE(IROOT,ITYPE,2,1,IDATA,RDATA)
        IF (KERROR.NE.0) GO TO 999
        IF (IDATA(1).NE.0) THEN
          IF (ITYPE.EQ.KCHARS) THEN
*             The strings themselves must be freed.
            DO 10, ISTR = 1, IDATA(1)*2, 2
            CALL GKHPDA(KHP(KHPXI(IDATA(2))+ISTR), KINTGS)
            IF (KERROR.NE.0) GO TO 999
   10         CONTINUE
            CALL GKHPDA(IDATA(2), KINTGS)
            IF (KERROR.NE.0) GO TO 999
          ELSE
            CALL GKHPDA(IDATA(2), ITYPE)
            IF (KERROR.NE.0) GO TO 999
          END IF
        END IF
   20   CONTINUE
*       Delete data record directory
      CALL GKDRDL(IROOT)
      END IF

*     All errors trap to here.
  999 CONTINUE

      RETURN

      END
