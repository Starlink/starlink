      subroutine par_get0r(param,value,status)

      integer MAXTOKS
      parameter (MAXTOKS = 20)

      character*(*) param
      real          value
      character*80  line,key,prompt,upline,defread,default,fname,echdir
      integer       status,ios,lun,ntoks,start,end1,end2
      integer       lenkey,lendefault,lenprompt,i
      character*40  toks(MAXTOKS)
      logical       found
      integer       str_len
      external       str_len

      call getenv("ECHWIND_HOME",echdir)
      fname = echdir(:str_len(echdir))//'echwind.ifl'
      lun = 29
      open(unit = lun, file = fname(:str_len(fname)),status = 'old')

      lendefault = 0
      found  = .false.
      read(lun, '(a)', iostat=ios) line
      do while (ios .eq. 0)
         call str_upcase(upline,line)
         ntoks = 0
         start = 1
         do i = 1,MAXTOKS
            toks(i)(1:40) = '                                       '
         end do
         if ((found) .and. index(upline,'ENDPARAMETER') .eq. 1)
     +     found = .false.
         if ((index(upline,'PARAMETER') .eq. 1) .and.
     +       (index(upline,param) .ne. 0)) found = .true.
         if (found) then
            do while (start.le.len(line) .and. line(start:start).eq.' ')
               start = start + 1
            end do

            end1 = index(line(start:),' ') + start - 1
            end2 = index(line(1:),'       ')

            toks(1)(1:end1-start) = line(start:end1-1)
            if (toks(1)(1:7) .eq. 'default') then
               toks(2)(1:end2-end1-1) = line(end1+1:end2-1)
            else
               toks(2)(1:end2-end1-3) = line(end1+2:end2-2)
            end if

            if (index(toks(1),'keyword') .eq. 1) then
               key = toks(2)
               lenkey = end2 - end1 - 3
            endif
            if (index(toks(1),'prompt') .eq. 1) then
               prompt = toks(2)
               lenprompt = end2 - end1 - 3
            end if
            if (index(toks(1),'default') .eq. 1) then
               default = toks(2)
               lendefault = end2 - end1 - 1
            end if
         end if

         read(lun, '(a)', iostat=ios) line
      end do

      call str_upcase(key,key)
      write(6,6500) key(:lenkey)//' - '//prompt(:lenprompt)//' '
 6500 format(a,$)
      write(6,6500) '> '
      read(5,5500) defread
 5500 format(a)

      if (defread(1:5) .eq. '     ') then
         value = 0.0
      else
         read(defread,*) value
      endif

      status = 0

      close(unit = lun)

      return
      end
