      subroutine type_file
*+
* Name:
*    TYPE_FILE

* Invocation:
*    CALL TYPE_FILE
*
* Description:
*   To print out the dictionary of commands available in batch and to
*   give their parameters.
* Purpose:
*   To print out the dictionary of commands available in batch and to
*   give their parameters.
* Author:
*    T.N.Wilkins Manchester
* History:
*    TNW 20/7/88 to allow one continuation line in dictionary.
*    Bug fix 7/8/90 TNW
*-
      implicit none
      integer ndict,chr_len,i,nforbid,ilen
      integer itt
      character*150 dict(400),char*10,forbid(30)*20,scr*510
      common/bat_dict/dict,forbid
      common/bat_dicti/ndict,nforbid
      itt=0
      i=0
      do while(i.lt.ndict)
        i=i+1
        itt=itt+1
        scr=dict(i)
        ilen = chr_len(scr)
        if(dict(i+1)(1:1).eq.'-') then
          scr = dict(i)(:ilen)//dict(i+1)(2:)
          ilen = chr_len(scr)
          i=i+1
        end if
        call print_string(scr(:ilen),' ')
        if(itt.eq.20) then
          itt=0
          write(*,'('' <Return> to continue, E to end'')')
          read(*,'(a10)')char
          call chr_ucase(char)
          if (char(:1).eq.'E') i=ndict
        end if
      end do
      end
