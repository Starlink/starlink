

      SUBROUTINE GK1TMV(PX,PY)
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) workstation driver
*  Author:             JRG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To move Tek cursor to specified position (D.C.). Switches
*     terminal out of graphics mode afterwards.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP  PX,PY   Coordinates of point (in D.C.) to which cursor is to
*                  be moved
*
      INTEGER PX,PY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*    IGS,IUS  ASCII codes for switching into graphics and normal mode
*     IB    Holds bytes that are to be sent to ternminal
*     NLEFT Number of bytes left in buffer (value not used)
*
      INTEGER ISYN,IGS,IUS,I,J,LOX,LOY,IEB,HIX,HIY
      PARAMETER (ISYN=22,IGS=29,IUS=31)
      INTEGER NLEFT, IB(9)
      INTEGER IX,IY
      REAL PTORX,PTORY
      PARAMETER (PTORX=6.0,PTORY=6.0)
*


*   Fill buffer with: graphics mode, high y, low y, high x, low x,
*   normal mode, 2 sync bytes (ought to be dependent on speed)
      IX=INT(FLOAT(PX)*PTORX + 0.5)
      IY=INT(FLOAT(PY)*PTORY + 0.5)

      HIX=IX/128
      HIY=IY/128
      LOX=IX-HIX*128
      LOY=IY-HIY*128
      I=LOX
      J=LOY
      LOX=LOX/4
      LOY=LOY/4
      IEB=4*(J-LOY*4)+(I-LOX*4)
      IB(1)=IGS
      IB(2)=HIY + 32
      IB(3)=IEB + 96
      IB(4)=LOY + 96
      IB(5)=HIX + 32
      IB(6)=LOX + 64
      IB(7)=IUS
      IB(8)=ISYN
      IB(9)=ISYN

      CALL GKIOBO(KIOPB,9,IB,NLEFT)

      END
