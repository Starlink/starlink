C# IL>=a, OL>=0
      SUBROUTINE GKDRQ ( IROOT, NITEM, NINT, NREAL )
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
*     Inquire Directory parameters
*
*  MAINTENANCE LOG
*  ---------------
*     10/08/83  CJW   Original version stabilized
*     15/12/83  CJW   Changed meaning of NITEM
*     16/12/83  CJW   Provide more detailed bug numbers
*     20/01/87  ARG   IS conversion. Error number changed.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP IROOT   Heap index  ( = Directory Index)
*     OUT NITEM   Number of entries in the directory
*     OUT NINT    Number of integers per entry
*     OUT NREAL   Number of reals per entry
*
      INTEGER NITEM, NINT, NREAL, IROOT
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
*     IADI    Addresses in the integer heap - derived index at various levels
*     IHEAP   Heap pointer
*
      INTEGER IADI, IHEAP
*
*  HEAP USAGE
*  ----------
*     Allocate directory
*
*  ERRORS
*  ------
*     -2016  Invalid heap pointer
*
*  COMMENTS
*  --------
*
*     Any of the following are detected as bugs -
*
*     -2016    IROOT < 0
*              IROOT > KHPXSI
*              KHPXI(IROOT) = KNIL
*
*---------------------------------------------------------------------



*     Check Arguments

      IF (      (IROOT .LT. 0)          .OR.
     :          (IROOT .GT. KHPXSI)     .OR.
     :          (KHPXI(IROOT) .EQ. KNIL) ) THEN

         CALL GKBUG (-2016, 'GKDRQ')

      ELSE


*        Inquire Directory Numbers

         IADI = KHPXI(IROOT)

         NITEM = 0
         NREAL = KHP(IADI+KDRREA)
         NINT = KHP(IADI+KDRINT)
         IHEAP = IROOT

*        While IHEAP <> KNIL do
    1    CONTINUE
         IF (IHEAP .EQ. KNIL) GO TO 2

            NITEM = NITEM + KHP(KHPXI(IHEAP)+KDRITM)
            IHEAP = KHP(KHPXI(IHEAP)+KDRNXT)

         GO TO 1
    2    CONTINUE
*        End While

      END IF

      END
