
      SUBROUTINE GK1TCL

*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Clears screen
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read    /WKD/   For KWKDAT
*     Read    /WCA/   For KWKIX
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     IBAUD   Offset of baud rate within KWKDAT
*     IWORK   Array containing clear screen orders
*     NLEFT   Number of bytes left in buffer
*
      INTEGER IBAUD
      PARAMETER (IBAUD=1)
      INTEGER IWORK(3), NLEFT
*      ASCII codes  US  ESC  FF
      DATA IWORK/   31, 27, 12/
*
*---------------------------------------------------------------------


      CALL GKIOBO(KIOPB,3,IWORK,NLEFT)
      CALL GKIOBO(KIOSN,1,IWORK,NLEFT)
      CALL GKSYN(KWKDAT(IBAUD,KWKIX), 0.8)

      END
