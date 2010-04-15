      subroutine par_get0c(param,value,status)

      integer MAXTOKS
      parameter (MAXTOKS = 20)

      integer str_len
      external str_len

      character*(*) param,value
      character*80  line,key,prompt,default,upline,fname,echdir
      integer       status,ios,lun,ntoks,start,end1,end2
      integer       lenkey,lendefault,lenprompt,i
      character*40  toks(MAXTOKS)
      logical       found,doesit

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
            toks(2)(1:end2-end1-3) = line(end1+2:end2-2)

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
               lendefault = end2 - end1 - 3
            end if
         end if

         read(lun, '(a)', iostat=ios) line
      end do

      call str_upcase(key,key)
      doesit = .false.
      if (lendefault .gt. 0) then
         call getenv(default(:lendefault),value)
         if (value(1:5) .eq. '     ') then
            doesit = .false.
         else
            inquire(file = value(:str_len(value)),exist = doesit)
         end if
      end if
      if (.not. doesit) then
         if (param(1:8) .ne. 'DETECTOR') then
            write(6,6500) key(:lenkey)//' - '//prompt(:lenprompt)//' '
 6500       format(a,$)
            if (lendefault .gt. 0) then
               if (default(1:1) .ne. ' ')
     +              write(6,6500) '/'//default(:lendefault)//'/ '
            end if
         end if
         write(6,6500) '> '
         read(5,5300) value
 5300    format(a)
         if (value(1:5) .eq. '     ')
     +     value(1:lendefault) = default(1:lendefault)
      endif

      status = 0

      close(unit = lun)

      return
      end
