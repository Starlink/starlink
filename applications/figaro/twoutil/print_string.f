      subroutine print_string(string,terminater)
*+
* Name:
*    PRINT_STRING

* Invocation:
*    CALL PRINT_STRING(STRING,TERMINATER)

* Purpose:
*   To output a string to the terminal

* Description:
*   To output a string to the terminal

* Arguments:
*    Given:
*      STRING    (c*(*)): String to print
*      TERMINATER (c*(*)): String to terminate with (if blank then ignored)
*    Altered:
*    Returned:
*    Workspace:
*    Subroutines/functions referenced:
* History:
*   T.N.Wilkins, Cambridge,  2-JAN-1990
*-
      implicit none
      character*(*) string
      character*(*) terminater

*

      integer ilen,chr_len,ilen2,temp,temp2

      ilen = chr_len(string)
      temp = ilen
      temp2 = 1
      do while(temp.gt.78)
        write(*,'(1x,a)')string(temp2:temp2+77)
        temp2 = temp2 + 78
        temp = temp - 78
      end do
      ilen2 = chr_len(terminater)
      if(ilen2.gt.0) then
        if((temp+ilen2).gt.79) then
          write(*,'(1x,a,/,1x,a)')string(temp2:ilen),terminater(:ilen2)
        else
          write(*,'(1x,2a)')string(temp2:ilen),terminater(:ilen2)
        end if
      else
        write(*,'(1x,a)')string(temp2:ilen)
      end if
      end
