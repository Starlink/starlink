C# IL>=a, OL>=0
      SUBROUTINE GKHPDA ( INDEX, ITYPE )
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
*     Deallocate heap storage
*
*  MAINTENANCE LOG
*  ---------------
*     17/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     21/07/83  CJW   INDEX = KNIL ignored rather than error
*     22/04/86  RMK   Removed unused local variables ISIZE and NEXT.
*     19/01/87  PKY   IS conversion. Error number changes.
*     13/02/87  PLP   Character heap now equivalenced
*                     to Integer/Real. Altered code and
*                     comments accordingly.
*     06/07/87  PLP   To signal the end of the directory when
*                     concatenating unused lumps, IPTR set to KNIL
*                     rather than to -1.
*     06/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP   INDEX  INDEX of item to deallocate
*     INP   ITYPE  Type of unit (integer, real, string)
*
      INTEGER INDEX, ITYPE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYERR/    Set error status
*     Modify /GKYHP/  Pointers, counts and heap changed
*     Modify /GKZHP/  Heap changed
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     ISIZE   Size of deallocated item
*     LUMP    Current lump
*     IPTR    Pointer to heap storage
*
      INTEGER LUMP, IPTR
*
*  HEAP USAGE
*  ----------
*     Heap manager - deallocates storage
*
*  ERRORS
*  ------
*   -2004 Documented condition to be satisfied by parameter(s) of
*         internal routine is not satisfied - INDEX must refer to
*         previously allocated heap
*
*   -2006 Value of internal enumerated type is invalid - ITYPE
*
*  COMMENTS
*  --------
*     Does a consistance check to prevent deallocation of unallocated
*     heap space.
*
*---------------------------------------------------------------------

      IF (INDEX .EQ. KNIL) RETURN
      IF ((ITYPE.EQ.KINTGS) .OR. (ITYPE.EQ.KREALS)) THEN

*        Check index is valid

         IF (   (INDEX .GT. 0)                    .AND.
     :          (INDEX .LE. KHPXSI)               .AND.
     :          (KHPXI(INDEX) .NE. 0)             .AND.
     :          (KHPXPI(KHPLXI(INDEX)) .EQ. INDEX) )       THEN

            LUMP = KHPLXI(INDEX)
            IPTR = KHPXI(INDEX)
            IF (LUMP .EQ. KHPLMI) THEN

*              Repeat

    1          CONTINUE
*                 Recover the storage
                  KHPNXI = IPTR
                  KHPLMI = KHPLMI - 1
                  IF (KHPLMI .GT. 0) THEN
                     IPTR = - KHPXPI(KHPLMI)
                  ELSE
                     IPTR = KNIL
                  END IF

*              Until we dont find an unused lump
               IF (IPTR .GT. 0) GO TO 1

            ELSE

*              Mark the storage as not active
               KHPXPI(LUMP) = - KHPXI(INDEX)

            END IF

*           Mark the directory entry as not active
            KHPXI(INDEX) = 0
*           Tot up the available storage
            KHPAVI = KHPAVI + KHPESI(LUMP)

         ELSE

*           Invalid index
            CALL GKBUG(-2004, 'GKHPDA')

         END IF

      ELSE IF (ITYPE .EQ. KCHARS) THEN

*        Treat as Integer/Real Heap: check index is valid

         IF (   (INDEX .GT. 0)                    .AND.
     :          (INDEX .LE. KHPXSI)               .AND.
     :          (KHPXI(INDEX) .NE. 0)             .AND.
     :          (KHPXPI(KHPLXI(INDEX)) .EQ. INDEX) )       THEN

            LUMP = KHPLXI(INDEX)
            IPTR = KHPXI(INDEX)
            IF (LUMP .EQ. KHPLMI) THEN

*              Repeat

    2          CONTINUE
*                 Recover the storage
                  KHPNXI = IPTR
                  KHPLMI = KHPLMI - 1
                  IF (KHPLMI .GT. 0) THEN
                     IPTR = - KHPXPI(KHPLMI)
                  ELSE
                     IPTR = KNIL
                  END IF

*              Until we dont find an unused lump
               IF (IPTR .GT. 0) GO TO 2

            ELSE

*              Mark the storage as not active
               KHPXPI(LUMP) = - KHPXI(INDEX)

            END IF

*           Mark both directory's entries as not active
            KHPXC(INDEX) = 0
            KHPXI(INDEX) = 0
*           Tot up the available storage
            KHPAVI = KHPAVI + KHPESI(LUMP)

         ELSE

*           Invalid index
            CALL GKBUG(-2004, 'GKHPDA')

         END IF

      ELSE

*        Invalid ITYPE
         CALL GKBUG(-2006, 'GKHPDA')
      END IF


      END
