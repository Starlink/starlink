C# IL>=a, OL>=0
      SUBROUTINE GKDRDE (IROOT, INDEX)
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
*     Delete Directory Entry
*
*  MAINTENANCE LOG
*  ---------------
*     10/08/83  CJW   Original version stabilized
*     15/12/83  CJW   Changed meaning of NITEM
*     16/12/83  CJW   Provide more detailed bug numbers
*     19/01/87  PKY   IS conversion. Error number changes.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP IROOT   Heap Index - Points to Directory
*     INP INDEX   Directory item index (>=0)
*
      INTEGER IROOT, INDEX
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKHP/    Access directory
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkdir.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     IDIR    Addresses in the integer heap - derived index at various levels
*     ISIZ    Number of entries in directory block
*     INTS    Number of integers per entry
*     INUM    Actual number of entries in the current block
*     IREA    Number of reals per entry
*     ICURR   Temporary heap pointer - block containing entry
*     IRPT    Temporary heap pointer - points to reals
*     IOFF    Offset of required entry
*     IPRIOR  Pointer to previous block
*
      INTEGER IDIR, ISIZ, INTS, IREA, INUM, ICURR, IRPT,
     :        IOFF, IPRIOR
*
*  HEAP USAGE
*  ----------
*     Access heap directly
*
*  ERRORS
*  ------
*    -2004 Documented condition to be satisfied by parameter(s) of
*          internal routine is not satisfied.
*    -2016 Invalid Heap Pointer
*
*  COMMENTS
*  --------
*
*     Any of the following are detected as bugs -
*
*    (-2004)   INDEX < 0
*              Non existent entry
*    (-2016)   IROOT < 0
*              IROOT > KHPXSI
*              KHPXI(IROOT) = KNIL
*
*     Internal inconsistances in either the heap or the directory may
*     also be detected as bug -2004
*
*---------------------------------------------------------------------



*     Check Arguments

      IF (      (IROOT .LT. 0)          .OR.
     :          (IROOT .GT. KHPXSI)     .OR.
     :          (KHPXI(IROOT) .EQ. KNIL)  ) THEN
         CALL GKBUG(-2016, 'GKDRDE')
      ELSE IF (INDEX .LT. 0) THEN
         CALL GKBUG(-2004, 'GKDRDE')
      ELSE
         IDIR  = KHPXI(IROOT)
         INUM  = KHP(IDIR + KDRITM)
         ISIZ  = KHP(IDIR + KDRINC)
         IREA  = KHP(IDIR + KDRREA)
         INTS  = KHP(IDIR + KDRINT)

*           Search Directory for INDEX

            CALL GKDRS(IROOT, INDEX, ICURR, IOFF, IPRIOR)

            IF (IOFF .EQ. KNIL) THEN
               KERROR = -1017
            ELSE
               IDIR  = KHPXI(ICURR)
               KHP(IDIR + KDRX + IOFF) = KNIL
               INUM  = KHP(IDIR + KDRITM) - 1
               KHP(IDIR + KDRITM) = INUM

*              Delete the block its empty and if its not the first

               IF ((INUM.EQ.0) .AND. (IPRIOR.NE.KNIL)) THEN
                  KHP(KHPXI(IPRIOR)+KDRNXT) = KHP(KHPXI(ICURR)+KDRNXT)
                  IDIR = KHPXI(IPRIOR)
                  IRPT = KHP(KHPXI(ICURR) + KDRRPT)
                  IF (IRPT .NE. KNIL) CALL GKHPDA(IRPT, KREALS)
                  CALL GKHPDA(ICURR, KINTGS)
               END IF
            END IF
      END IF

      END
