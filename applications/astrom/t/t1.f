      program deldate
      
      implicit none
      integer ftstat,ftunit,blocksize
      character*(80) fn
      
      integer iargc
      
      if (iargc().ne.1) then
         write (*,'("Usage: deldate fitsfile")')
         goto 999
      endif
      call getarg (1, fn)

      ftstat = 0
      call ftgiou (ftunit, ftstat)
      call ftopen (ftunit, fn, 1, blocksize, ftstat)
      call ftdkey (ftunit, 'DATE', ftstat)
      call ftclos (ftunit, ftstat)

 999  continue
      end
