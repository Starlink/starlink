C# IL>=a, OL>=0
      SUBROUTINE GKDRQN ( IROOT, N, IKEY )
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
*     Inquire Nth Entry
*
*  MAINTENANCE LOG
*  ---------------
*     15/12/83  CJW   Original version stabilized
*     15/12/83  CJW   Changed meaning of NITEM
*     16/12/83  CJW   Provide more detailed bug numbers
*     22/01/87  JCS   IS conversion. Error number changes.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP IROOT   Heap Index  ( = Directory Index)
*     INP N       Nth entry required
*     OUT IKEY    Key of Nth entry
*
      INTEGER IROOT, N, IKEY
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKHP/     Directory constants
*
      INCLUDE '../include/gkdir.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
*
*  LOCALS
*  ------
*     IHEAP   Current directory block
*     INEXT   Next directory block
*     ISIZ    Size of directory block
*     INUM    Number of items in current directory block
*     IADI    Addresses in the integer heap - derived Index at various levels
*     I       Loop Index
*     J       Array Index
*
      INTEGER IHEAP, INEXT, ISIZ, INUM, IADI, I, J
*
*  HEAP USAGE
*  ----------
*     Allocate directory
*
*  ERRORS
*  ------
*    -2004 Documented condition to be satisfied by parameter(s) of int.
*          routine is not satisfied
*    -2016 Invalid Heap pointer
*
*  COMMENTS
*  --------
*     The directory is scanned until the Nth non null entry is found.
*     Its KEY is returned. If the Nth entry does not exist, KNIL is
*     returned
*
*     Any of the following are detected as bugs -
*
*    (-2004)   N < 1
*    (-2016)   IROOT < 0
*              IROOT > KHPXSI
*              KHPXI(IROOT) = KNIL
*
*     If the Nth entry does not exist KNIL is returned
*
*---------------------------------------------------------------------


*     Check Arguments

      IF (      (IROOT .LT. 0)          .OR.
     :          (IROOT .GT. KHPXSI)     .OR.
     :          (KHPXI(IROOT) .EQ. KNIL)  ) THEN
        CALL GKBUG(-2016,'GKDRQN')
      ELSE IF (N .LE. 0) THEN
        CALL GKBUG(-2004,'GKDRQN')
      ELSE

*        Get Item

         INEXT = IROOT
         J = 0

*        While INEXT <> KNIL do
    1    CONTINUE
         IF (INEXT .EQ. KNIL) GO TO 3

            IHEAP = INEXT
            IADI = KHPXI(IHEAP)
            ISIZ = KHP(IADI + KDRINC)
            INUM = KHP(IADI + KDRITM)
            INEXT = KHP(IADI + KDRNXT)
            IADI = IADI + KDRX
            IF ((J+INUM) .LT. N) THEN
               J = J + INUM
            ELSE
               DO 2 I = 1, ISIZ
                  IF (KHP(IADI) .NE. KNIL) THEN
                     J = J + 1
                     IF (J .EQ. N) THEN
                        IKEY = KHP(IADI)
                        GO TO 999
                     END IF
                  END IF
                  IADI = IADI + 1
    2          CONTINUE
            END IF

         GO TO 1
    3    CONTINUE
*        End While

      END IF

      IKEY = KNIL

  999 CONTINUE

      END
