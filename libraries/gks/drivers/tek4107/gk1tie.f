

      SUBROUTINE GK1TIE
*---------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     TEK4107 Finish with device
*
*  MAINTENANCE LOG
*  ---------------
*     08/01/85  GGT  Original version stabilized
*
*  ARGUMENTS
*  ---------
*      None
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*
      INTEGER IB1(4),IB2(4),IB3(4)
      INTEGER NLEFT
*
      DATA IB1 /27,75,65,49/
      DATA IB2 /27,76,86,49/
      DATA IB3 /27,37,33,49/

*
*  ALGORITHM
*  ---------
*
*       Escape sequences:-
*
*       IB1     esc k a 1       Enable dialog area
*       IB2     esc l v 1       Dialog area visible
*       IB3     esc % ! 1       Set ANSI mode
*
* --------------------------------------------------------------

      CALL GKIOBO(KIOPB,4,IB1,NLEFT)
      CALL GKIOBO(KIOPB,4,IB2,NLEFT)
      CALL GKIOBO(KIOPB,4,IB3,NLEFT)
*
      RETURN
      END
