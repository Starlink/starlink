C# IL>=a, OL>=0
      SUBROUTINE GKCETR(IREFL,NIC,NIR,ICAIN,NOC,NOR,ICAOUT)
*
* (C) COPYRIGHT ICL & SERC  1988
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To reflect and trim a cell array, sothat it can be
*     used by a hardware cell array routine, that does not
*     trim or reflect,
*
*  MAINTENANCE LOG
*  ---------------
*     09/05/88  KEVP  Created
*     10/12/91  KEVP  Initialized IFL (C91).
*
*  ARGUMENTS
*  ---------
*     INP IREFL      Reflection Indicator (0 to 7)
*                     1,3,5 or 7 (bit 0) Swap X and Y
*                     2,3,6 or 7 (bit 1) Reflect on X axis
*                     4,5,6 or 7 (bit 2) Reflect on Y axis
*                    Swapping done before reflection
*
*     INP NIC,NIR    Dimensions of input array
*     INP ICAIN      Input Cell Array
*     INP NOC,NOR    Dimensions of output array
*     OUT ICAOUT     Output Cell Array

      INTEGER  IREFL, NIC,NIR, ICAIN(NIC,NIR),
     :                NOC,NOR, ICAOUT(NOC,NOR)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*  Do loop variables for filling ICAOUT
*     IRS  Start of row
*     IRE  End of row
*     JR   Jump to next element of row
*     ICS  Start of column
*     ICE  End of column
*     JC   Jump to next element of column
*
      INTEGER IRS,IRE,JR, ICS,ICE,JC

*     IFL    Used for reflection data
*     IR,IC  Do Loop indices for filling ICAOUT
*     IX,IY  Scanning indices for ICAIN

      INTEGER IFL, IC,IR, IX,IY

*  ALGORITHM
*  ---------
*     The relevent portion of the input array
*     is scanned straight forward and
*     put in the output array in the appropiate order.
*
*  WARNING
*  -------
*     The output array must be different from the input array
*     for the correct result to occur.
*---------------------------------------------------------------------
*
*     Do anything, only if reflection indicator is valid
      IF((IREFL .GE. 0) .OR. (IREFL .LT. 8))THEN
*
*        Set DO-Loop variables
*
         IFL = IREFL
*        Rows
         IF(IFL .LT. 4)THEN
            IRS = 1
            IRE = NOR
            JR  = 1
         ELSE
            IRS = NOR
            IRE = 1
            JR  = -1
         ENDIF
         IFL =  2*(IFL - IFL/4)
*        Columns
         IF(IFL .LT. 4)THEN
            ICS = 1
            ICE = NOC
            JC  = 1
         ELSE
            ICS = NOC
            ICE = 1
            JC  = -1
         ENDIF

*        Use DO-Loop according to whether the X and Y axes are swapped
         IF(IREFL .EQ. 2*(IREFL/2))THEN
*           No - Swap
            IY = KWI4
            DO 1020 IR=IRS,IRE,JR
               IX = KWI3
               DO 1010 IC=ICS,ICE,JC
                  ICAOUT(IC,IR) = ICAIN(IX,IY)
                  IX = IX + 1
 1010          CONTINUE
               IY = IY + 1
 1020       CONTINUE
         ELSE
*           Swap
            IY = KWI4
            DO 1120 IC=ICS,ICE,JC
               IX = KWI3
               DO 1110 IR=IRS,IRE,JR
                  ICAOUT(IC,IR) = ICAIN(IX,IY)
                  IX = IX + 1
 1110          CONTINUE
               IY = IY + 1
 1120       CONTINUE
         ENDIF
      ENDIF
      END
