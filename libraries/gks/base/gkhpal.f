C# IL>=a, OL>=0
      SUBROUTINE GKHPAL ( ISIZE, ITYPE, INDEX )
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
*     Allocate storage from the heap
*
*  MAINTENANCE LOG
*  ---------------
*     17/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     05/03/84  CJW   Return INDEX of KNIL on error
*                     Put in check on lump index full
*     19/01/87  PKY   IS conversion. Error number changes.
*     13/02/87  PLP   Character heap now equivalenced
*                     to Integer/Real. Altered code and
*                     comments accordingly.
*     06/07/87  PLP   To aid the portability, Character Heap
*                     initialised to spaces a character at a time,
*                     rather than doing it efficiently via Integer
*                     Heap and appropriate Integer masks.
*
*  ARGUMENTS
*  ---------
*     INP   ISIZE  Number of units to be allocated
*     INP   ITYPE  Type of unit (integer, real, string)
*     OUT   INDEX  Directory position where the offset is stored
*
      INTEGER ISIZE, ITYPE, INDEX

*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYERR/ Set error status
*     Modify /GKYHP/  Pointers, counts and heap changed
*     Modify /GKZHP/  Heap changed
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkhp.cmn'
*
*     Intrinsic functions declaration
*
      INTRINSIC MOD
*
*  LOCALS
*  ------
*     I       Loop Index
*     LEFT    Amount of room LEFT on the stack
*     LUMP    Current "lump"
*     ISMALL  Size of smallest free lump that is large enough
*
      INTEGER   I, LEFT, LUMP, ISMALL
*
*  HEAP USAGE
*  ----------
*     Heap manager - allocates storage
*
*  COMMENTS
*  --------
*     The heap must be initialised by calling GKHPOP, and terminated
*     after its last usage by calling GKHPCL.
*
*     After calling this routine you can access the Jth data item by
*
*      INTEGER HEAP     :  KHP(KHPXI(INDEX)+J-1)
*      REAL HEAP        :  QHP(KHPXR(INDEX)+J-1)
*      CHARACTER HEAP   :  CHP(KHPXC(INDEX)+J-1)
*
*     Deallocate using GKHPDA.
*
*  ERRORS
*  ------
*     300 Storage overflow has occurred in GKS
*
*   -2004 Documented condition to be satisfied by parameter(s) of
*         internal routine is not satisfied - ISIZE must be > 0
*
*   -2006 Value of internal enumerated type is invalid - ITYPE
*
*---------------------------------------------------------------------

      INDEX = 0

      IF (ISIZE .LT. 1) THEN
*        ISIZE invalid
         CALL GKBUG(-2004, 'GKHPAL')

      ELSE IF ((ITYPE.EQ.KINTGS) .OR. (ITYPE.EQ.KREALS)) THEN

*        Is there enough room?

         IF (ISIZE .GT. KHPAVI) THEN

            KERROR = 300

         ELSE

*           Find directory entry available

            DO 3 I = 1, KHPXSI
               INDEX = I
               IF (KHPXI(I) .EQ. 0) GO TO 4
    3       CONTINUE

*           If we get here then there is no directory room

            INDEX = KNIL
            KERROR = 300

    4       CONTINUE
            IF (KERROR .EQ. 0) THEN

*              Have a directory entry - Calculate room available

               LEFT = KHPSZI - KHPNXI + 1

*              Possible Garbage Collect
               IF ((ISIZE .GT. LEFT) .OR.
     :             (KHPLMI .GE. KHPXSI)) THEN
*                 Try to find room
                  LUMP = 0
                  ISMALL = KHPSZI + 1
                  DO 5 I = 1, KHPLMI
                     IF (       (KHPXPI(I).LT.0)        .AND.
     :                          (KHPESI(I).GE.ISIZE)    .AND.
     :                          (KHPESI(I).LT.ISMALL) ) THEN

                        ISMALL = KHPESI(I)
                        LUMP = I
                     END IF
    5             CONTINUE

                  IF (LUMP .EQ. 0) THEN
                     CALL GKHPZZ(ITYPE)
                     KHPLMI = KHPLMI + 1
                     KHPXI(INDEX) = KHPNXI
                     KHPNXI = KHPNXI + ISIZE
                     LUMP = KHPLMI
                  ELSE
                     KHPXI(INDEX) = - KHPXPI(LUMP)
                  END IF
               ELSE
                  KHPLMI = KHPLMI + 1
                  KHPXI(INDEX) = KHPNXI
                  KHPNXI = KHPNXI + ISIZE
                  LUMP = KHPLMI
               END IF


*              All is ok - do allocation

               KHPLXI(INDEX) = LUMP
               KHPXPI(LUMP) = INDEX
               KHPESI(LUMP) = ISIZE
               KHPAVI = KHPAVI - ISIZE
            END IF
         END IF

      ELSE IF (ITYPE .EQ. KCHARS) THEN

*
*        Adjust ISIZE so that KCHARS can be treated as KINTGS:
*        Character allocation must be a multiple of KNIBYT.
*
         IF(MOD(ISIZE,KNIBYT).NE.0)ISIZE=ISIZE+KNIBYT-MOD(ISIZE,KNIBYT)
         ISIZE=ISIZE/KNIBYT
*
*        Is there enough room?

         IF (ISIZE .GT. KHPAVI) THEN

            KERROR = 300

         ELSE

*           Find directory entry available

            DO 6 I = 1, KHPXSI
               INDEX = I
               IF (KHPXI(I) .EQ. 0) GO TO 7
    6       CONTINUE

*           If we get here then there is no directory room

            INDEX = KNIL
            KERROR = 300

    7       CONTINUE
*
            IF (KERROR .EQ. 0) THEN

*              Have a directory entry - Calculate room available

               LEFT = KHPSZI - KHPNXI + 1
*              Possible Garbage Collect
               IF ((ISIZE .GT. LEFT) .OR.
     :             (KHPLMI .GE. KHPXSI)) THEN
*                 Try to find room
                  LUMP = 0
                  ISMALL = KHPSZI + 1
                  DO 8 I = 1, KHPLMI
                     IF (       (KHPXPI(I).LT.0)        .AND.
     :                          (KHPESI(I).GE.ISIZE)    .AND.
     :                          (KHPESI(I).LT.ISMALL) ) THEN

                        ISMALL = KHPESI(I)
                        LUMP = I
                     END IF
    8             CONTINUE

                  IF (LUMP .EQ. 0) THEN
                     CALL GKHPZZ(ITYPE)
                     KHPLMI = KHPLMI + 1
*                    2 directory entries per each Character
*                    allocation are made
                     KHPXC(INDEX) = KHPNXI*KNIBYT
                     KHPXI(INDEX) = KHPNXI
                     KHPNXI = KHPNXI + ISIZE
                     LUMP = KHPLMI
                  ELSE
*                    Mark both directory entries as unused
                     KHPXC(INDEX) = - KHPXPI(LUMP)*KNIBYT
                     KHPXI(INDEX) = - KHPXPI(LUMP)
                  END IF
               ELSE
                  KHPLMI = KHPLMI + 1
*                 Character heap directory's entry is allocation
*                 address in bytes; Integer heap directory's
*                 entry is the same address in long Integers
                  KHPXC(INDEX) = KHPNXI*KNIBYT
                  KHPXI(INDEX) = KHPNXI
                  KHPNXI = KHPNXI + ISIZE
                  LUMP = KHPLMI
               END IF


*              All is ok - do allocation

               KHPLXI(INDEX) = LUMP
               KHPXPI(LUMP) = INDEX
               KHPESI(LUMP) = ISIZE
               KHPAVI = KHPAVI - ISIZE

*              Clear the newly allocated "Character" heap

                  DO 10 I=1,ISIZE*KNIBYT
                     CHP(KHPXC(INDEX)+I-1)=' '
   10             CONTINUE

            END IF
         END IF

      ELSE

*        ITYPE invalid
         CALL GKBUG(-2006, 'GKHPAL')

      END IF

      END
