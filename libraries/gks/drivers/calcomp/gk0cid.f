

*---------------------------------------------------------------
      SUBROUTINE GK0CID
*---------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CC81 Initialise Device
*
*  MAINTENANCE LOG
*  ---------------
*     00/99/83  MGC  Original version stabilized
*     15/06/84  RMK  Changed values used for I/O parameters.
*     16/05/85  RMK  Changed RBL,RBF characters from C,D to A,G as they
*                    were causing the character size to be reset to the plotter
*                    default between buffers.
*
*  ARGUMENTS
*  ---------
*      None
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER NLEFT
      INTEGER IXPEN, IYPEN
      PARAMETER (IXPEN=2, IYPEN=3)
      INTEGER ILID
      PARAMETER (ILID=45)
      INTEGER ICID(ILID)
*             SOH  P SP &  SP  1  7 SP  1  3 SP  2  5  5 SP  0
      DATA ICID/1,80,32,38,32,49,55,32,49,51,32,50,53,53,32,48,
*              SP  1  3 SP  0 SP  0 SP  6  5 SP  7  1 SP
     :          32,49,51,32,48,32,48,32,54,53,32,55,49,32,
*                L  0  ,  5 SP  H SP  F  0 SP  0  /  0  K SP
     :          76,48,44,53,32,72,32,70,48,32,48,47,48,75,32/
*
*  ALGORITHM
*  ---------
*     .SELECT PLOTTER      : <SOH>P
*     .END                 : <SP>
*     .SET IO PARAMETERS       BS OT  TD 02 01 ABL ABF RBL RBF
*                          : & 17 13 255  0 13  0   0   65  71
*     .END                 : <SP>
*     .SET LINE TYPE SOLID : L0,5
*     .END                 : <SP>
*     .PEN UP              : H
*     .END                 : <SP>
*     .UNLOAD PEN          : F0
*     .END                 : <SP>
*     .MOVE TO (0,0)       : 0/0K
*     .END                 : <SP>
*
*
*  COMMENTS
*  --------
*     Buffer Status Request       BS == <DC1>
*     Output Trigger              OT == <CR>
*     Turnaround Delay            TD == 255 msec
*     Terminator Chars          0,13 == none,<CR>
*     Auto Buf Low,Full      ABL,ABF == not enabled
*     Req Buf Low,Full       RBL,RBF == A,G
*
* --------------------------------------------------------------

      CALL GKIOBO(KIOIT,1,KDAT,NLEFT)
      CALL GK0CPA(KIOPB,ILID,ICID)
      CALL GK0CPA(KIOSN,1,KDAT)
      KWKDAT(IXPEN,KWKIX)=KNIL
      KWKDAT(IYPEN,KWKIX)=KNIL
      END
