C# IL>=a, OL>=0
      SUBROUTINE GKDRS (IROOT, INDEX, IHEAP, IOFF, IPRIOR)
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
*     Search for entry (internal routine to directory package)
*
*  MAINTENANCE LOG
*  ---------------
*     10/08/83  CJW   Original version stabilized
*     15/08/83  CJW   Add IPRIOR
*     15/12/83  CJW   Changed meaning of NITEM
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP IROOT   Heap Index - Points to Directory
*     INP INDEX   Directory item index (>=0)
*     OUT IHEAP   Heap index of found entry
*     OUT IOFF    Offset of required entry
*     OUT IPRIOR  Heap index of previous block
*
      INTEGER IROOT, INDEX, IHEAP, IOFF, IPRIOR
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKHP/    Access directory
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkdir.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     INEXT   Temporary heap pointer used to loop over allocations
*     IDIR    Addresses in the integer heap - derived index at various levels
*     ISIZ    Number of directory entries
*
      INTEGER INEXT, IDIR, ISIZ
*
*  HEAP USAGE
*  ----------
*     Access heap directly
*
*  COMMENTS
*  --------
*     If the entry is not found IHEAP is set to the last directory
*     scanned and IOFF to KNIL.
*---------------------------------------------------------------------



      IHEAP = KNIL
      INEXT = IROOT
      IOFF = KNIL

*     While INEXT <> KNIL and IOFF = KNIL do
    1 CONTINUE
      IF ((INEXT .EQ. KNIL) .OR. (IOFF .NE. KNIL)) GO TO 4

         IPRIOR = IHEAP
         IHEAP = INEXT
         IDIR  = KHPXI(IHEAP)
         ISIZ  = KHP(IDIR + KDRINC)
         INEXT = KHP(IDIR + KDRNXT)
         IDIR = IDIR + KDRX

         DO 2 IOFF = 0, ISIZ-1
            IF (KHP(IDIR+IOFF) .EQ. INDEX) GO TO 3
    2    CONTINUE
         IOFF = KNIL
    3    CONTINUE

      GO TO 1
    4 CONTINUE
*     End While

      END
