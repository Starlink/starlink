




      SUBROUTINE GK1TID
*---------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     TEK 4107 Initialise Device
*
*  MAINTENANCE LOG
*  ---------------
*     08/01/85  GGT  Original version stabilized
*
*  ARGUMENTS
*  ---------
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
*     ICOL    Offset in KWKDAT for current device colour index
*
      INTEGER    ICOL
      PARAMETER (ICOL=2)
      INTEGER IB1(4),IB2(4),IB3(4),IB4(5),IB5(4)
      INTEGER IB6(6),IB7(5),IB8(5),IB9(4),IB10(7)
      INTEGER IB11(4),IB12(6),IB13(4),IB14(4)
      INTEGER RED(16),GREEN(16),BLUE(16),INDEV,NLEFT,I
*
      DATA IB1 /27,37,33,48/
      DATA IB2 /27,83,75,33/
      DATA IB3 /27,75,84,50/
      DATA IB4 /27,73,67,0,48/
      DATA IB5 /27,73,76,48/
      DATA IB6 /27,73,83,33,48,48/
      DATA IB7 /27,78,84,49,61/
      DATA IB8 /27,78,67,61,48/
      DATA IB9 /27,73,77,49/
      DATA IB10/27,83,76,49,68,48,48/
      DATA IB11/27,82,70,54/
      DATA IB12/27,84,77,49,49,49/
      DATA IB13/27,75,65,48/
      DATA IB14/27,76,86,48/
*
      DATA RED
     :   /0,100,100,  0,  0,100,  0,100,  0, 20, 33, 47, 60, 73, 87,100/
      DATA GREEN
     :   /0,100,  0,100,  0,100,100,  0,  0, 20, 33, 47, 60, 73, 87,100/
      DATA BLUE
     :   /0,100,  0,  0,100,  0,100,100,  0, 20, 33, 47, 60, 73, 87,100/
*
      DATA INDEV /0/
CC      DATA INDEV /1/  Set this for tablet locator input
*
*  ALGORITHM
*  ---------
*
*       Escape sequences:-
*
*       IB1     esc % ! 0       Tek mode
*       IB2     esc s k -1      Delete all segments
*       IB3     esc k t 2       Set error threshold
*       IB4     esc i c dev func code 0  Gin cursor
*       IB5     esc i l 0       Report max line length
*       IB6     esc i s -1 0 0  Report size characters
*       IB7     esc n t 1 13    eol string
*       IB8     esc n c 13 0    eom characters
*       IB9     esc i m 1       Report eom frequency
*       IB10    esc s l 1 64 0  Current matching class
*       IB11    esc r f 6       Fixup level 6
*       IB12    esc t m 1 1 1   Colour mode RGB
*       IB13    esc k a 0       Disable dialog area
*       IB14    esc l v 0       Dialog area invisible
*
* --------------------------------------------------------------

      CALL GKIOBO(KIOPB,4,IB1,NLEFT)
      CALL GKIOBO(KIOPB,4,IB2,NLEFT)
      CALL GKIOBO(KIOPB,4,IB3,NLEFT)
      IB4(4)=INDEV*8 + 48
      CALL GKIOBO(KIOPB,5,IB4,NLEFT)
      CALL GKIOBO(KIOPB,4,IB5,NLEFT)
      CALL GKIOBO(KIOPB,6,IB6,NLEFT)
      CALL GKIOBO(KIOPB,5,IB7,NLEFT)
      CALL GKIOBO(KIOPB,5,IB8,NLEFT)
      CALL GKIOBO(KIOPB,4,IB9,NLEFT)
      CALL GKIOBO(KIOPB,7,IB10,NLEFT)
      CALL GKIOBO(KIOPB,4,IB11,NLEFT)
      CALL GKIOBO(KIOPB,6,IB12,NLEFT)
      CALL GKIOBO(KIOPB,4,IB13,NLEFT)
      CALL GKIOBO(KIOPB,4,IB14,NLEFT)
*
* Set up the default colour table
*
      DO 10 I=1,16
      CALL GK1TPC(I-1,RED(I),GREEN(I),BLUE(I))
   10 CONTINUE
*
* Set device current colour index to undefined
      KWKDAT(ICOL,KWKIX) = KNIL
*
      RETURN
      END
