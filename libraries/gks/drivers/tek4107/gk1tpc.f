


      SUBROUTINE GK1TPC(IND,RED,GREEN,BLUE)
*---------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     TEK 4107 Set colour for a linetype index
*
*  MAINTENANCE LOG
*  ---------------
*     08/01/85  GGT  Original version stabilized
*
*  ARGUMENTS
*  ---------
*
*       IND     INP     Pen index
*       RED     INP     Red component
*       GREEN   INP     Green "
*       BLUE    INP     Blue  "

      INTEGER IND,RED,GREEN,BLUE
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*
      INTEGER IBUFF(20),NLEFT,I
      DATA IBUFF /27,84,71,33,52,15*0/
*
*  ALGORITHM
*  ---------
*
*       Command is
*        esc t g -1 4 index R G B
*
* --------------------------------------------------------------

      IBUFF(6)=IND+48
      I=7
      CALL GK1TTI(RED,IBUFF,I)
      CALL GK1TTI(GREEN,IBUFF,I)
      CALL GK1TTI(BLUE,IBUFF,I)
      CALL GKIOBO(KIOPB,I-1,IBUFF,NLEFT)
*
      RETURN
      END
