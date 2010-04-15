      program fitsdump

      implicit none
      integer ftstat,ftunit,blocksize,i
      character*(80) s
      character*(80) ffname
      logical keepreading
      logical printline
      logical trimlines

      integer iargc

      trimlines = .true.
      ffname = ''

      do i=1,iargc()
         call getarg(i, s)
         if (s(1:5).eq.'-trim') then
            trimlines = .true.
         else if (s(1:7).eq.'-notrim') then
            trimlines = .false.
         else if (s(1:1).eq.'-') then
            goto 991
         else
            ffname = s
         endif
      enddo

      if (ffname.eq.'') goto 991

      ftstat = 0
      call ftgiou (ftunit, ftstat)
      call ftopen (ftunit, ffname, 0, blocksize, ftstat)

      if (ftstat.gt.0) then
         write (*,'("Can''t open file ",a)') ffname
         goto 990
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
         if (trimlines) then
            if (s(1:4).eq.'DATE') then
               printline = .false.
            else if (s(1:7).eq.'HISTORY') then
               printline = .false.
            else if (s(1:7).eq.'COMMENT') then
               printline = .false.
            else
*            It's OK -- print this line
               printline = .true.
            endif
         else
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

 990  continue

      call ftclos (ftunit, ftstat)
      call ftrprt ('STDERR', ftstat)
      goto 999

 991  continue
      write(*,'("Usage: fitsdump [-[no]trim] fitsfile")')
      goto 999

 999  continue
      end
