      subroutine list_lines(line_count,waves,line_name)
*+
* Name:
*    LIST_LINES

* Invocation:
*    CALL LIST_LINES(LINE_COUNT,WAVES,LINE_NAME)
*
* Description:
*    To list the lines identified in a file to the terminal.
*
* Purpose:
*    To list the lines identified in a file to the terminal.
*
* Arguments:
*      LINE_COUNT = INTEGER (Given)
*        Number of lines
*      WAVES(LINE_COUNT) = REAL ARRAY (Given)
*        Wavelengths of lines
*      LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Names of lines
*  Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge, 17-APR-1991
* History:
*-
      implicit none
      integer line_count
      real waves(line_count)
      character*10 line_name(line_count)
      integer len1,line,lenst,pstat
      character*80 chars
*

      call par_wruser(' ',pstat)
      call par_wruser('Lines identified:',pstat)

      call chr_fill(' ',chars)
      lenst = 0
      do line = 1, line_count
        len1 = lenst
        call chr_appnd(line_name(line),chars,len1)
        len1 = len1 + 1
        call chr_putc('(',chars,len1)
        call chr_putr(waves(line),chars,len1)
        call chr_putc(')',chars,len1)
        lenst = lenst + 25
        if((lenst.eq.75).or.(line.eq.line_count)) then
          call par_wruser(chars(:len1),pstat)
          lenst = 0
          call chr_fill(' ',chars)
        end if
      end do
      call par_wruser(' ',pstat)
      end
