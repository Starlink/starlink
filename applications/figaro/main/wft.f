      subroutine wft(string,lu,status)
*+
* Name:
*    WFT

* Invocation:
*    CALL WFT(STRING,LU,STATUS)

* Purpose:
*  To output a character string to a file, or the terminal.

* Description:
*  To output a character string to a file, or the terminal.
*
* Arguments:
*      STRING = CHARACTER*(*) (Given)
*        Character string
*      LU = INTEGER (Given)
*        Logical unit of file, if less than zero then
*                         written to terminal
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
* Subroutines/functions referenced:
*      CHR_LEN = INTEGER (Workspace)
*        Get non-blank length of string
*      PAR_WRUSER       : Write string to terminal
* Author:
*   T.N.Wilkins, Cambridge,  1-AUG-1989
*-
      implicit none
      character*(*) string
      integer lu
      integer status
      integer lst,chr_len
*
      lst = max(chr_len(string),1)

      if(lu.lt.0) then
        call par_wruser(string(:lst),status)
      else
        write(lu,'(a)',iostat=status) string(:lst)
      end if

      end
