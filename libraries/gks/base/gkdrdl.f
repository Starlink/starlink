C# IL>=a, OL>=0
      SUBROUTINE GKDRDL( IROOT )
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
*     Delete a directory in the heap
*
*  MAINTENANCE LOG
*  ---------------
*     10/08/83  CJW   Original version stabilized
*     15/12/83  CJW   Changed meaning of NITEM
*     16/12/83  CJW   Provide more detailed bug numbers
*     19/01/87  PKY   IS conversion. Error number changes.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     10/10/89  KEVP  Made exit for heap deallocation error (S379).
*
*  ARGUMENTS
*  ---------
*     INP IROOT   Heap index  ( = Directory Index)
*
      INTEGER IROOT
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read    /GKHP/      Access heap
*
      INCLUDE '../include/gkdir.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     INEXT   Temporary heap pointer used to loop over allocations
*     IREAL   Temporary heap pointer used to point to real allocation
*
      INTEGER INEXT, IREAL
*
*  HEAP USAGE
*  ----------
*     Deallocate directory
*
*  ERRORS
*  ------
*   -2016 Invalid Heap pointer
*
*  COMMENTS
*  --------
*     Space allocated from the heap is returned.
*
*     Any of the following are detected as bugs -
*
*    (-2016)   IROOT < 0
*              IROOT > KHPXSI
*              KHPXI(IROOT) = KNIL
*
*---------------------------------------------------------------------


*     Check Arguments

      IF (      (IROOT .LT. 0)          .OR.
     :          (IROOT .GT. KHPXSI)     .OR.
     :          (KHPXI(IROOT) .EQ. KNIL)  ) THEN
         CALL GKBUG(-2016, 'GKDRDL')
      ELSE

         INEXT = IROOT

*        While INEXT <> KNIL do
    1    CONTINUE
         IF (INEXT .EQ. KNIL) GO TO 2

            IREAL = KHP(KHPXI(IROOT)+KDRRPT)
            INEXT = KHP(KHPXI(IROOT)+KDRNXT)
            CALL GKHPDA(IREAL, KREALS)
            CALL GKHPDA(IROOT, KINTGS)
            IF(KERROR .NE. 0)GOTO 2
            IROOT = INEXT

         GO TO 1
    2    CONTINUE
*        End While

      END IF

      END
