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
      
      if (ftstat.gt.0) then
         write (*,'("Can''t open file ",a)') s
         goto 998
      endif

      keepreading = .true.
      i = 1
      do while (keepreading .and. ftstat.eq.0)
         call ftgrec (ftunit, i, s, ftstat)
*      Omit date record (to make diffing reasonable)
         if (s(1:4).ne.'DATE') write (*, '(a)'), s
         i = i+1
         if (s(1:3).eq.'END') keepreading = .false.
      enddo

 998  continue

      call ftclos (ftunit, ftstat)

      call ftrprt ('STDERR', ftstat)

 999  continue
      end
