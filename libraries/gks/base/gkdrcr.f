C# IL>=a, OL>=0
      SUBROUTINE GKDRCR( NINC, NINT, NREAL, IROOT )
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
*     Create a directory using the heap
*
*  MAINTENANCE LOG
*  ---------------
*     10/08/83  CJW   Original version stabilized
*     15/12/83  CJW   Changed meaning of NITEM
*     19/01/87  PKY   IS conversion. Error number changes.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP NINC    Number of entries per directory block
*     INP NINT    Number of integers per entry
*     INP NREAL   Number of reals per entry
*     OUT IROOT   Heap index  ( = Directory Index)
*
      INTEGER NINC, NINT, NREAL, IROOT
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKHP/     Put directory into heap
*     Modify /GKERR/    KERROR
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
*     I       Loop index
*     ISIZEI  Amount of integer heap required
*     ISIZER  Amount of real heap required
*     IREAL   Temporary heap pointer - points to reals
*     IADI    Addresses in the integer heap - derived index at various levels
*
      INTEGER I, ISIZEI, ISIZER, IREAL, IADI
*
*  HEAP USAGE
*  ----------
*     Allocate directory
*
*  ERRORS
*  ------
*   -2004 Documented condition to be satisfied by parameter(s) of
*         internal routine is not satisfied.
*
*  COMMENTS
*  --------
*     Space is grabbed from the integer and real heaps. The real heap
*     index is stored in the integer heap - only the integer heap index
*     is returned.
*
*     The structure of the directory is -
*
*
*
*     +-----+
*     |IROOT|
*     +-----+
*        |
*        |  |              |
*        |  |==============|
*        +->| NITEM       0|
*           |--------------|
*           | NINC        1|
*           |--------------|
*           | NREAL       2|
*           |--------------|
*           | NINT        3|
*           |--------------|
*        +--| NEXT        4|
*        |  |--------------|
*        |  | RealPtr     5|------------------------------+
*        |  |==============|=====]                        |
*        |  | Index       6|     ]                        |
*        |  |--------------|     ]                        V
*        V  | Index     ...|     ]             [====|===========|
*           |--------------|     ]             [    | Real Data |
*           | Index        |     ]             [    |-----------|
*           |==============|     ]- NINC times-[    | Real Data |
*           | Integer Data |     ]             [    |-----------|
*           |--------------|     ]             [    | Real Data |
*           | Integer Data |     ]             [====|===========|
*           |--------------|     ]
*           | Integer Data |     ]                    REAL HEAP
*           |==============|=====]
*
*             INTEGER HEAP
*
*     Where -
*
*     NITEM : Number of entries in the current directory block
*     NINC  : Size of directory block
*
*     The following conditions are considered to be bugs (-2004) -
*
*     NINC  < 1
*     NINT  < 0
*     NREAL < 0
*     (NREAL + NINT) = 0
*
*---------------------------------------------------------------------



*     Check Arguments

      IF (      (NINC .LT. 1)          .OR.
     :          (NINT  .LT. 0)          .OR.
     :          (NREAL .LT. 0)          .OR.
     :      ((NREAL + NINT) .EQ. 0)     ) THEN

         CALL GKBUG(-2004, 'GKDRCR')

      ELSE

*        Calculate heap space required

         ISIZEI = KDRX + (1 + NINT) * NINC
         ISIZER = NREAL * NINC

*        Obtain heap space

         CALL GKHPAL(ISIZEI, KINTGS, IROOT)

         IF (KERROR .EQ. 0) THEN

*           Cannot use KHP(KHPXI(IROOT)+KDRPTR) as an argument - Heap manager
*           might move my data!


            IF (NREAL .GT. 0) THEN
               CALL GKHPAL(ISIZER, KREALS,  IREAL)
            ELSE
               IREAL = KNIL
            END IF


            IF (KERROR .EQ. 0) THEN

*              Set up directory

               IADI = KHPXI(IROOT)

               KHP(IADI+KDRITM) = 0
               KHP(IADI+KDRINC) = NINC
               KHP(IADI+KDRREA) = NREAL
               KHP(IADI+KDRINT) = NINT
               KHP(IADI+KDRNXT) = KNIL
               KHP(IADI+KDRRPT) = IREAL

               IADI = IADI+KDRX

               DO 1 I = 0, NINC-1
                  KHP(IADI+I) = KNIL
    1          CONTINUE

            ELSE

               CALL GKHPDA(IROOT, KINTGS)

            END IF

         END IF

      END IF

      END
