      subroutine wtf(icl,chars)
*+
* Name:
*    WTF

* Invocation:
*    CALL WTF(ICL,CHARS)
*
* Purpose:
*    To write the command to the file.
*
* Description:
*    To write the command to the file.
*
* Arguments:
*      ICL = LOGICAL (Given)
*        If using ICL
*      CHARS = CHARACTER*(*) (Given)
*        Characters to write
*    Subroutines/functions referenced:
*
* Author:
*   T.N.Wilkins, Cambridge, 30-JAN-1990
*-
      implicit none
      logical icl
      character*(*) chars

*

      if(icl) then
        write(17,'(a)')chars
      else
        write(17,'(''$ '',a)')chars
      end if
      end
