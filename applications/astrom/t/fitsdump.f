      program fitsdump
      
      implicit none
      integer ftstat,ftunit,blocksize,i
      character*(80) s
      logical keepreading
      
      integer iargc
      
      if (iargc().ne.1) then
         write (*,'("Usage: fitsdump fitsfile")')
         goto 999
      endif
      call getarg (1, s)

      ftstat = 0
      call ftgiou (ftunit, ftstat)
      call ftopen (ftunit, s, 0, blocksize, ftstat)
      
      keepreading = .true.
      i = 1
      do while (keepreading)
         call ftgrec (ftunit, i, s, ftstat)
         write (*, '(a)'), s
         i = i+1
         if (s(1:3).eq.'END') keepreading = .false.
      enddo

      call ftclos (ftunit, ftstat)

 999  continue
      end
