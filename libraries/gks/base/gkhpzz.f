C# IL>=a, OL>=0
      SUBROUTINE GKHPZZ(ITYPE)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End / Utility
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Performs garbage collection for GKHPAL
*
*  MAINTENANCE LOG
*  ---------------
*     17/05/83  CJW  Original version stabilized
*     27/06/83  CJW  Implement revised error handling precedure
*                    (No change required)
*     21/07/83  CJW  Name changed so that it does not clash with possible
*                    Heap Get Chars routine
*     22/04/86  RMK  Removed PRINT statement.
*     13/02/87  PLP  Character heap now equivalenced
*                    to Integer/Real. Altered code and
*                    comments accordingly.
*
*  ARGUMENTS
*  ---------
*     INP   ITYPE  Type of unit (integer, real, string)
*
      INTEGER ITYPE

*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYHP/  Pointers, counts and heap changed
*     Modify /GKZHP/  Heap changed
*
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     IFROM   Place to move item from
*     ITO     Place to move item to
*     ISIZE   Total size of item
*     I       Loop index
*     LUMP    Loop index - over LUMPS
*     NXTLMP  Next available lump
*
      INTEGER IFROM, ITO, ISIZE, I, LUMP, NXTLMP
*
*  HEAP USAGE
*  ----------
*     Heap manager - Garbage Collect
*
*  COMMENTS
*  --------
*     Called by GKHPAL to do a garbage collect.
*
*---------------------------------------------------------------------

      IF ((ITYPE.EQ.KINTGS) .OR. (ITYPE.EQ.KREALS)) THEN

         ITO = 1
         NXTLMP = 1

*        Loop over "Lumps"

         DO 2 LUMP = 1, KHPLMI
            IF (KHPXPI(LUMP) .GT. 0) THEN
*              its active
               IFROM = KHPXI(KHPXPI(LUMP))
               ISIZE = KHPESI(LUMP)
               IF (LUMP .NE. NXTLMP) THEN
*                 move it back
                  KHPXPI(NXTLMP) = KHPXPI(LUMP)
                  KHPESI(NXTLMP) = KHPESI(LUMP)
*                 change lump index
                  KHPLXI(KHPXPI(NXTLMP)) = NXTLMP
               END IF
               IF (IFROM .NE. ITO) THEN
*                 move it back
                  DO 1 I = 0, ISIZE-1
                     KHP(I+ITO) = KHP(I+IFROM)
    1             CONTINUE
               END IF
*              change the directory entry
               KHPXI(KHPXPI(NXTLMP)) = ITO
               ITO = ITO + ISIZE
               NXTLMP = NXTLMP + 1
            END IF

    2    CONTINUE

*        Update the next pointer and lump count
         KHPNXI = ITO
         KHPLMI = NXTLMP - 1

      ELSE IF (ITYPE .EQ. KCHARS) THEN

         ITO = 1
         NXTLMP = 1

*        Treat as Integer/Real heap: loop over "Lumps"

         DO 4 LUMP = 1, KHPLMI
            IF (KHPXPI(LUMP) .GT. 0) THEN
*              its active
               IFROM = KHPXI(KHPXPI(LUMP))
               ISIZE = KHPESI(LUMP)
               IF (LUMP .NE. NXTLMP) THEN
*                 move it back
                  KHPXPI(NXTLMP) = KHPXPI(LUMP)
                  KHPESI(NXTLMP) = KHPESI(LUMP)
*                 change lump index
                  KHPLXI(KHPXPI(NXTLMP)) = NXTLMP
               END IF
               IF (IFROM .NE. ITO) THEN
*                 move it back
                  DO 3 I = 0, ISIZE-1
                     KHP(I+ITO) = KHP(I+IFROM)
    3             CONTINUE
               END IF
*              change both directory's entries
               KHPXI(KHPXPI(NXTLMP)) = ITO
               KHPXC(KHPXPI(NXTLMP)) = ITO*KNIBYT
               ITO = ITO + ISIZE
               NXTLMP = NXTLMP + 1
            END IF

    4    CONTINUE

*        Update the next pointer and lump count
         KHPNXI = ITO
         KHPLMI = NXTLMP - 1
      END IF
      END
