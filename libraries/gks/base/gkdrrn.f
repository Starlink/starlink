C# IL>=a, OL>=0
      SUBROUTINE GKDRRN (IROOT, IOLD, INEW)
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
*     Rename Directory Entry
*
*  MAINTENANCE LOG
*  ---------------
*     10/08/83  CJW   Original version stabilized
*     15/12/83  CJW   Changed meaning of NITEM
*     16/12/83  CJW   Provide more detailed bug numbers
*     02/05/84  CJW   Wrong "block" used in rename (I203)
*     19/01/87  PKY   IS conversion. Error number changes.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP IROOT   Heap Index - Points to Directory
*     INP IOLD    Directory item index (>=0)
*     INP INEW    New name
*
      INTEGER IROOT, IOLD, INEW
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKHP/    Access directory
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkdir.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     IDIR    Addresses in the integer heap - derived index at various levels
*     ISIZ    Number of directory entries
*     INTS    Number of integers per entry
*     IREA    Number of reals per entry
*     IHEAP   Temporary heap pointer - block containing entry
*     IOFF    Offset of required entry
*     IPRIOR  Pointer to previous block
*
      INTEGER IDIR, ISIZ, INTS, IREA, IHEAP, IOFF, IPRIOR
*
*  HEAP USAGE
*  ----------
*     Access heap directly
*
*  ERRORS
*  ------
*    -1017 Entry not found in list
*    -1040 Entry already exists
*    -2004 Documented condition to be satisfied by parameter(s) of int.
*          routine is not satisfied
*    -2016 Invalid Heap pointer
*
*  COMMENTS
*  --------
*
*     Any of the following are detected as bugs -
*
*    (-2004)   IOLD < 0
*              INEW < 0
*    (-2016)   IROOT <0
*              IROOT > KHPXSI
*              KHPXI(IROOT) = KNIL
*
*     Internal inconsistances in either the heap or the directory may
*     also be detected as bug -2004
*
*     Error -1017 if old name does not exist
*     Error -1040 if new name  already exists
*
*---------------------------------------------------------------------



*     Check Arguments

      IF (      (IROOT .LT. 0)          .OR.
     :          (IROOT .GT. KHPXSI)     .OR.
     :          (KHPXI(IROOT) .EQ. KNIL)  ) THEN
         CALL GKBUG(-2016, 'GKDRRN')
      ELSE IF ( (IOLD .LT. 0)           .OR.
     :          (INEW .LT. 0)  ) THEN
         CALL GKBUG(-2004, 'GKDRRN')
      ELSE
         IDIR  = KHPXI(IROOT)
         ISIZ  = KHP(IDIR + KDRINC)
         IREA  = KHP(IDIR + KDRREA)
         INTS  = KHP(IDIR + KDRINT)

*        Search Directory for INEW

         CALL GKDRS(IROOT, INEW, IHEAP, IOFF, IPRIOR)

         IF (IOFF .NE. KNIL) THEN
            KERROR = -1040
         ELSE
*           Search Directory for IOLD

            CALL GKDRS(IROOT, IOLD, IHEAP, IOFF, IPRIOR)

            IF (IOFF .EQ. KNIL) THEN
               KERROR = -1017
            ELSE
               IDIR  = KHPXI(IHEAP) + KDRX
               KHP(IDIR + IOFF) = INEW
            END IF
         END IF
      END IF
      END
