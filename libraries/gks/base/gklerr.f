C# IL>=a, OL>=0
      SUBROUTINE GKLERR (NTH,NEL,IEL,IELOUT,IER)
*
* (C) COPYRIGHT ICL & SERC  1990
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Check output of list element inquiry for error 2002
*     and set list element value to KNIL if not defined.
*
*  MAINTENANCE LOG
*  ---------------
*     24/10/90  KEVP  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP NTH     list element requested
*     INP NEL     number of list elements
*     INP IEL     value of list element if defined
*     OUT IELOUT  value of list element to be output
*     OUT IER     error indicator
*
      INTEGER NTH, NEL, IEL, IELOUT, IER
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
*
*  COMMENT
*  -------
*     This routine was written to fix bug C55, which concerns the
*     reporting of error 2002.
*     It ensures that it is done according to the FORTRAN GKS BINDING.
*
*  ERRORS
*  ------
*     2002  List element requested is out of range
*
*---------------------------------------------------------------------


       IF((NTH .EQ. 0).OR.(NEL.EQ.0))THEN
*        If either the list element requested is zero or
*        or the number of list elements is zero
*        report no error,
*        even though the value of the list element is undefined.
         IELOUT  = KNIL
         IER = 0
       ELSEIF((0.LT.NTH).AND.(NTH.LE.NEL))THEN
*        Value of list element is defined
         IELOUT  = IEL
         IER = 0
       ELSE
*        List element requested is out of range - report error
         IELOUT = KNIL
         IER = 2002
       ENDIF

      END
