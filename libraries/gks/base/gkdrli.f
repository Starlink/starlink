C# IL>=a, OL>=0
      SUBROUTINE GKDRLI ( IROOT, ISIZE, IDATA )
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
*     Inquire List of Directory Entries
*
*  MAINTENANCE LOG
*  ---------------
*     10/08/83  CJW   Original version stabilized
*     15/12/83  CJW   Changed meaning of NITEM
*     16/12/83  CJW   Provide more detailed bug numbers
*     22/01/87  JCS   IS conversion. Error number changes.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP IROOT   Heap index  ( = Directory Index)
*     INP ISIZE   Dimension of IDATA
*     OUT IDATA   List of directory entries
*
      INTEGER IROOT, ISIZE, IDATA(*)
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
*     ISIZ    Number of items in directory
*     IADI    Addresses in the integer heap - derived index at various levels
*     I       Loop index
*     J       Array index
*
      INTEGER IHEAP, INEXT, ISIZ, IADI, I, J
*
*  HEAP USAGE
*  ----------
*     Allocate directory
*
*  ERRORS
*  ------
*    -2016 Invalid Heap pointer
*    -2017 Argument to internal GKS routine specifies an array that is
*          too small
*
*  COMMENTS
*  --------
*
*     Any of the following are detected as bugs -
*
*    (-2016)   IROOT < 0
*              IROOT > KHPXSI
*              KHPXI(IROOT) = KNIL
*    (-2017)   ISIZE < 1
*
*---------------------------------------------------------------------



*     Check Arguments

      IF (      (IROOT .LT. 0)          .OR.
     :          (IROOT .GT. KHPXSI)     .OR.
     :          (KHPXI(IROOT) .EQ. KNIL)  ) THEN
            CALL GKBUG(-2016, 'GKDRLI')
      ELSE IF (ISIZE .LE. 0) THEN
            CALL GKBUG(-2017, 'GKDRLI')
      ELSE


*        Get list

         INEXT = IROOT
         J = 0

*        While INEXT <> KNIL do
    1    CONTINUE
         IF (INEXT .EQ. KNIL) GO TO 3

            IHEAP = INEXT
            IADI = KHPXI(IHEAP)
            ISIZ = KHP(IADI + KDRINC)
            INEXT = KHP(IADI + KDRNXT)
            IADI = IADI + KDRX
            DO 2 I = 1, ISIZ
               IF (KHP(IADI) .NE. KNIL) THEN
                  J = J + 1
                  IF (J .GT. ISIZE) GO TO 999
                  IDATA(J) = KHP(IADI)
               END IF
               IADI = IADI + 1
    2       CONTINUE

         GO TO 1
    3    CONTINUE
*        End While

*        Fill the rest with KNIL

         DO 4 I = J+1, ISIZE
            IDATA(I) = KNIL
    4    CONTINUE

      END IF

  999 CONTINUE

      END
