
         program hello

         character  line*132, prompt*80
         integer    llen, itrm

         integer    get_aline
         external   get_aline

         itrm = 0

         llen = get_aline(line, 'Hello World (over): ', itrm)

         if (llen .eq. 0) then
           write(6,'('' ....Nobody home I guess.'')')
         else
           write(6,'('' World replied:'')')
           write(6,'('' -'',A,''-'')') line(1:llen)
           if (llen .eq. -1)
     &       write(6,'('' ....*choke* remainder truncated.'')')
         endif

         write
     &    (6,'(/,''Try history (up-arrow) and some editing next!\n'')')
         write(6,'('' '')')
         write(prompt,'(''Hello Moon too (over): '',$)')

         llen = get_aline(line, prompt, itrm)

         if (llen .eq. 0) then
           write(6,'('' ....Still a dusty and lonely place.'')')
         else
           write(6,'('' Moon replied:'')')
         write(6,'('' -'',A,''-'')') line(1:llen)
           if (llen .eq. -1)
     &        write(6,'('' ....*choke* remainder truncated.'')')
         endif
         write(6,'('' '')')

         stop
         end
