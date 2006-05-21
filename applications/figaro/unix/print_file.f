      subroutine print_file(filespec,queue,out_routine)
*+
* Name:
*    PRINT_FILE

* Invocation:
*    CALL PRINT_FILE(FILESPEC,QUEUE,OUT_ROUTINE)
*
* Purpose:
*    Print a graphics file.
*
* Description:
*    Print a graphics file-UNIX version.

* Arguments:
*      FILESPEC = CHARACTER*(*) (Given)
*        File name
*      QUEUE = CHARACTER*(*) (Given)
*        Queue name
*      OUT_ROUTINE = SUBROUTINE (Given)
*        Output subroutine
*  Subroutines/functions referenced:
*
* Authors:
*    TNW: T.N.Wilkins, Cambridge until 9/92, then Durham
* History:
*    TNW: 22-JUL-1991 Original version
*    TNW: 8-JAN-1993 Bug fix
*-
      implicit none
      character*(*) filespec
      character*(*) queue
      character*80 quenam,chars
      external out_routine
      integer pstat,len1,len2,istat,system,access,count
      logical loop

*

      call getenv(queue,quenam)
      call chr_fill(' ',chars)
      len1 = 0
      call chr_putc('lpr -P',chars,len1)
      len2 = len1
      call chr_appnd(quenam,chars,len1)
      if(len1.gt.len2) then
        len1 = len1 + 1
        call chr_appnd(filespec,chars,len1)
        len2 = len1
        istat = system(chars)
        if(istat.eq.0) then
          call chr_appnd(' (job submitted)',chars,len2)
        else
          call chr_appnd(' (job failed)',chars,len2)
        endif
        call out_routine(chars(:len2),pstat)
      else
        istat = 1
      endif

* If the job has not been submitted, then rename it to prevent
* over-writting

      if(istat.ne.0) then
        len1 = 0
        call chr_appnd(filespec,chars,len1)
        call chr_putc(',',chars,len1)
        len2 = len1
        loop = .true.
        count = 0
        do while(loop)
          len1 = len2
          count = count + 1
          call chr_puti(count,chars,len1)
          loop = access(chars(:len1),' ').eq.0
        enddo
*        istat = rename(filespec,chars(:len1))
      end if
      end
