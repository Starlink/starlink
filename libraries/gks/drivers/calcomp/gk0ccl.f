
****************************************************************






*===============================================================
*                CALCOMP 81 UTILITY ROUTINES
*===============================================================
*
* -- CC81 Numeric Separators
*     .plus(43),minus(45),space(32),comma(44)
*
* -- CC81 Instruction Delimeters:
*     .space(32),period(46),comma(44),semicolon(49)
*     .carriage return(13)
*     .new command character
*


*---------------------------------------------------------------
      SUBROUTINE GK0CCL(LCLOSE)
*---------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*      CC81 Picture clear
*
*  MAINTENANCE LOG
*  ---------------
*     00/99/83  MGC  Original version stabilized
*     19/03/84  MGC  Revise rollpaper code
*     18/09/84  RMK  Made choice of sheet or roll paper dependent on
*                    workstation type.
*
*  ARGUMENTS
*  ---------
*
      LOGICAL LCLOSE
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
      INTEGER IXPEN, IYPEN
      PARAMETER (IXPEN=2, IYPEN=3)
      INTEGER IA4S, IA3S, IWNR
      PARAMETER (IA4S=700, IA3S=701, IWNR=704)
      INTEGER    ILCL
      PARAMETER (ILCL=13)
      INTEGER ICCL(ILCL)
C     INTEGER IPCS(12)
      INTEGER ICM
      CHARACTER*1 AKEY
*                H SP  F  0 SP  F  0 SP  0  /  0  K SP
      DATA ICCL/72,32,70,48,32,70,48,32,48,47,48,75,32/
*
*  ALGORITHM
*  ---------
*     .PEN UP         : H
*     .END            : <SP>
*     .UNLOAD PEN     : F0
*     .END            : <SP>
*     .UNLOAD PEN     : F0
*     .END            : <SP>
*     .MOVE TO (0,0)  : 0/0K
*     .END            : <SP>
*     .IF SHEET FEED THEN
*       .IF NOT CLOSING WS THEN
*         .WAIT FOR OPERATOR
*       .ENDIF
*     .ELSE
*       .ADVANCE PAPER          : U<ICM>
*       .END                    : <SP>
*     .ENDIF
*
* --------------------------------------------------------------
      CALL GK0CPA(KIOPB,ILCL,ICCL)
      CALL GK0CPA(KIOSN,1,KDAT)
      IF (KWKTYP.EQ.IA4S.OR.KWKTYP.EQ.IA3S) THEN
*     sheet paper
        IF( .NOT. LCLOSE ) THEN
*       workstation is open
** Can use digitise to wait for finish before WRITE
          WRITE(*,100)
          READ(*,'(A)') AKEY
        ENDIF
      ELSE
        IF (KWKTYP.EQ.IWNR) THEN
*       roll paper to end of workstation window
          ICM=NINT((QCWVXR(KWKIX)-QCWVXL(KWKIX))/100.0)
        ELSE
*       workstation type is IA4R or IA3R
*       roll paper to end of A4/A3 frame
          ICM=NINT(QDSDX(KWKIX)/100.0)+3
        ENDIF
        CALL GK0CPB(85)
        CALL GK0CPN(ICM)
        CALL GK0CPB(32)
        CALL GK0CPA(KIOSN,1,KDAT)
      ENDIF

      KWKDAT(IXPEN,KWKIX)=KNIL
      KWKDAT(IYPEN,KWKIX)=KNIL
      RETURN
 100  FORMAT
     :(1H ,'When plotting ends, change paper and RETURN to continue')
      END
