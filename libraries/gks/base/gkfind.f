C# IL>=a, OL>=0
      SUBROUTINE GKFIND(IARRAY,MX, IVAL, IERR, IX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front end
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To find an occurrence of a particular value IVAL in an array IARRAY.
*     If IVAL has the same value as one of the elements
*     IARRAY(1) ... IARRAY(MX)
*     then
*         IX is set to satisfy 1 =< IX =< MX  and  IARRAY(IX)=IVAL
*         KERROR is unchanged
*     else
*         IX is set to be KNIL
*         KERROR is set to be IERR
*     endif
*
*  MAINTENANCE LOG
*  ---------------
*       6/7/83   JRG  Original version stabilized
*       7/7/83   JRG  Comment change
*
*  ARGUMENTS
*  ---------
*     Inp  IARRAY(MX)  Array to be searched
*     Inp  MX      Dimension of IARRAY
*     Inp  IVAL    Value to be sought
*     Inp  IERR    GKS error number to be used if entry not found
*     Out  IX      Index, in array, of found entry. If not found, IX=KNIL
*
      INTEGER MX,IARRAY(MX),IVAL,IERR,IX
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYERR/ Set KERROR to IERR if IVAL not found
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     J     Loop counter
*
      INTEGER J
*
*  ERRORS
*  ------
*     IERR   Entry not found
*
*---------------------------------------------------------------------


      DO 100 J=1,MX
        IF( IARRAY(J).EQ.IVAL ) GOTO 150
  100 CONTINUE

*   Here if not found
      IX=KNIL
      KERROR=IERR
      GOTO 999

*   Here if found
  150 IX=J

*   Finish
  999 CONTINUE
      END
