      subroutine encode_range(chars1,chars2,start,end,string,len1)
*+
* Name:
*    ENCODE_RANGE

* Invocation:
*    CALL ENCODE_RANGE(CHARS1,CHARS2,START,END,STRING,LEN1)
*
* Description:
*    To encode a range into a string.
*
* Purpose:
*    To encode a range into a string.
*
* Arguments:
*      CHARS1 = CHARACTER*(*) (Given)
*        Starting string, start=end
*      CHARS2 = CHARACTER*(*) (Given)
*        Starting string, start < end
*      START = INTEGER (Given)
*        Start of range
*      END = INTEGER (Given)
*        End of range
*      STRING = CHARACTER*(*) (Given and returned)
*        String to encode into
*      LEN1 = INTEGER (Given and returned)
*        Position last written to
*  Subroutines/functions referenced:
*     CHR_ routines
* Author:
*    T.N.Wilkins, Cambridge, 16-17-APR-1991
* History:
*-
      implicit none
      character*(*) chars1
      character*(*) chars2
      integer start
      integer end
      character*(*) string,test*1
      integer len1

*

      if(len1.eq.0) call chr_fill(' ',string)
      if(end.eq.start) then
        call chr_appnd(chars1,string,len1)
      else
        call chr_appnd(chars2,string,len1)
      end if
      test = string(len1:len1)
      if((test.ne.'[').and.(test.ne.'(')) then
        call chr_putc(' ',string,len1)
      end if
      call chr_puti(start,string,len1)
      if(end.ne.start) then
        call chr_putc(' to ',string,len1)
        call chr_puti(end,string,len1)
      end if
      end
