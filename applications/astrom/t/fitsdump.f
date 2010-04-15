      program fitsdump

      implicit none
      integer ftstat,ftunit,blocksize,i
      character*(80) s
      logical keepreading
      logical printline

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
      call ftgrec (ftunit, i, s, ftstat)
      do while (keepreading .and. ftstat.eq.0)

*      Suppress printing of lines with certain keywords, to make diffing
*      the dumped files reasonable.  The file creation DATE will
*      obviously change from file to file; HISTORY and COMMENT lines are
*      suppressed, too (a) because different versions of the (c)fitsio
*      library seem to have different ideas about how many blanks to
*      include after these comments, and (b) the post-2001 version of
*      the library is going to change the `this is FITS' comment cards
*      (see thread
*      <http://groups.google.com/groups?threadm=3BC7232C.C4E3A9C1%40tetra.gsfc.nasa.gov>)
         if (s(1:4).eq.'DATE') then
            printline = .false.
         else if (s(1:7).eq.'HISTORY') then
            printline = .false.
         else if (s(1:7).eq.'COMMENT') then
            printline = .false.
         else
*         It's OK -- print this line
            printline = .true.
         endif
         if (printline) write (*, '(a)'), s
         i = i+1
         if (s(1:3).eq.'END') then
            keepreading = .false.
         else
*         Next card
            call ftgrec (ftunit, i, s, ftstat)
         endif
      enddo

 998  continue

      call ftclos (ftunit, ftstat)

      call ftrprt ('STDERR', ftstat)

 999  continue
      end
