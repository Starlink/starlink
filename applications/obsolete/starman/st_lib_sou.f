CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is STARFLIB.FOR
C
C    It contains the general s/rs:-
C
C BELL         Sound the terminal bell
C CHARLN       Position of last non-blank character in a character string
C ENDSTRIP     Return start and end of non-blank string in a string
C FINDHEAD     Find column header with a character string
C GETTHEH      Get header order in the input list
C GTHEAD       Get a character string from a file column header
C IDENT        Fill identifiers columns of a table with #1 - #n
C LBGONE       Remove leading blanks from string
C LENS         (FN) Find position of last non-blank character in a string
C LOWCASE      Convert a character string to lower case
C NAMECHR      See if two 20 character 'names' are the same
C NAMEGT       Get the 20 character 'name' identifier from a line in a table
C NAMETR       Translate a 20 character 'name' into 5 Reals
C NUMEL        Get no and location of elements in a text string
C PARGBEG      Start text values text loading
C PARG(CILR)   Load (string:int:logical:real) value for text loading
C PRINTD       Output text string to CL with embedded constants
C PTHEAD       Put a character string as a file column header
C RCHECK       Read a real number; return true if bit n is set, false if not
C RCHZERO      Check a 2-d sub-image in a 3-d real array, and returns
C RVTEXT       Put a real number into a character string
C TRUNC        (Function) Truncate no of digits before decimal point
C TRUNC_E      (Function) Truncate no of digits after decimal point
C TYHELP       Type out help
C UPPCASE      Convert a character string to upper case
C VALFORM      Formats a real number into a 12 character text string
C WCHECK       Write 1/0 into bit n of a real number if flag true/false

C ISCHAR       (Fn) Is this a character?
C ISNUMB       (Fn) Is this a number?
C ISSIGN       (Fn) Is this a sign?


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C BELL -- Sound the terminal bell
C
C    a j penny                ral               1991 June

      subroutine bell ( )

      implicit none
      include 'STARMAN_INC'
C--
Cbegin

      if ( ST_FAILED ) return

      write ( 6, '(''+'',a1)' ) char(7)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHARLN -- Position of last non-blank character in a character string
C
C  alan penny                 ral                      1990-02-19

      subroutine charln ( text, klen )

      implicit none
      include 'STARMAN_INC'

      character*(*)  text	!i: Input character string
      integer        klen	!o: Position of last non-blank character
				!   (if all blank, returned as 0)
C--
      integer k
Cbegin

      if ( ST_FAILED ) return

      klen = len(text)
      k = 1
      do while ( k.eq.1 .and. klen.gt.0 )
         if ( text(klen:klen).eq.' ' ) then
            klen = klen - 1
         else
            k = 0
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ENDSTRIP -- Return start and end of non-blank string in a string
C     (non-blank string may have imbedded blanks)
C
C   alan penny         ral                   1988 Nov

      subroutine endstrip ( string, i1, i2 )

      implicit none
      include 'STARMAN_INC'

      character string*(*)	!i: Input string
      integer   i1              !o: Posn in input string of start
      integer   i2              !o: Posn in input string of end
C--
      logical found
      integer length, i, j
Cbegin


      if ( ST_FAILED ) return

      length = len(string)

      i = 1
      found = .false.
      do while ( .not.found .and. i.le.length )
        if ( string(i:i) .ne. ' ' ) then
           found = .true.
           j = i
        else
           i = i + 1
        endif
      enddo
      i1 = 0
      if ( found ) i1 = j

      i2 = 0
      i = length
      found = .false.
      do while ( .not.found .and. i.ge.1 )
        if ( string(i:i) .ne. ' ' ) then
           found = .true.
           j = i
        else
           i = i - 1
        endif
      enddo
      i2 = 0
      if ( found ) i2 = j


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FINDHEAD -- Find column header with a character string
C             (ignores leading blanks)
C  a j penny                          ral               1995 Aug

      subroutine findhead ( file, nx, head, case, ncol )

      implicit none
      include 'STARMAN_INC'

      character*(*) file	!i: Name of input file
      integer nx		!i: No of columns (not including 5 of names)
      character*(*) head	!i: Header to search for
      logical case		!i: Case import? (.true. = yes)
      integer ncol		!o: Column header is in (0=not found)
C--
      integer j, kla, klf, istat
      character*132 heada, headf, headfa
      logical found
Cbegin


      ncol = 0

      if ( case ) then
         heada = head
      else
         call lowcase ( head, heada )
      endif
      call lbgone ( heada )
      call charln ( heada, kla )

      j = 0
      found = .false.
      do while ( .not.found .and. j.lt.nx )
         j = j + 1
         call gthead ( file, j, headf, istat )
         if ( ST_FAILED ) return
         if ( case ) then
            headfa = headf
         else
            call lowcase ( headf, headfa )
         endif
         call lbgone ( headfa )
         call charln ( headfa, klf )
         if ( heada(1:kla).eq.headfa(1:klf) ) then
            found = .true.
            ncol = j
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETTHEH -- Get header order in the input list
C
C  a j penny                          dao              1988-04-19

      subroutine gettheh ( file, nxfile, khead )

      implicit none
      include 'STARMAN_INC'

      character*(*) file	!i: Name of input file
      integer nxfile		!i: No of columns (+5)
      integer khead(16)		!o: Column positions of descripotors
C--
      integer j, k, ierr
      character*20 header(16), ahead
      data header / 'X', 'Y', 'HEIGHT', 'RX', 'RY', 'P', 'PRX', 'PRY',
     +              'THETA', 'QH', 'QR', 'QP', 'MAPNUM', 'MAPX',
     +              'MAPY', 'MAGNIF' /
Cbegin


      if ( ST_FAILED ) return

      call azeroi ( khead, 16 )

      do k = 1, nxfile - 5
         call gthead ( file, k, ahead, ierr )
         if ( ierr.eq.0 ) then
            do j = 1, 16
               if ( ahead.eq.header(j) ) khead(j) = k
            enddo
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GTHEAD -- Get a character string from a file column header
C   It assumes the file is an XYlist and finds how many parameters
C   it has and then reads the decsriptor 'HEAD00n' , where 'n'
C   is the number of the wanted Header.
C
C   If the descriptor is missing it is set to ' ' and an error flagged.
C
C   alan penny                       ral                    1990 Jan

      subroutine gthead ( file, num, text, ierr )

      implicit none
      include 'STARMAN_INC'

      character*(*) file	!i: File to read from
      integer       num		!i: Number of the descriptor to be read
      character*(*) text	!o: Descriptor
      integer       ierr	!o: Error flag. ok=0
C--
      character*7 texth
      integer istat, iv
Cbegin


      if ( ST_FAILED ) return

      ierr = 0						!Set error flag

      write ( texth, '(''HEAD'',i3.3)' ) num		!Get header
      call gtdesc ( file, texth, text, ' ', iv, istat )
      if ( text.eq.' ' .or. istat.ne.0 ) ierr = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IDENT -- Fill identifier columns of a table with #1 - #n
C   Load the identifier characters one by one into ID, and then
C   transfer them byte by byte into the correct place in the table.
C
C   The identifiers take up the first 20 bytes (1st 5 real words)
C   of each row.
C
C
C
C   alan penny                       ral                   1990-02-16

      subroutine ident ( data, nx, ny )

      implicit none
      include 'STARMAN_INC'

      integer    nx		!i: No of columns (inc identifier columns)
      integer    ny		!i: No of rows
      byte       data(nx*4,ny)	!o: Table
C--
      character id*20
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, ny
         write ( id, '(i20)' ) k
         id(1:1) = '#'
         call lbgone(id(2:))
         do j = 1, 20
            data(j,k) = ichar(id(j:j))
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C LBGONE -- Remove leading blanks from a character string
C
C   alan penny               ral         1990 Jan

      subroutine lbgone ( str )

      implicit none
      include 'STARMAN_INC'

      character*(*)  str	!i/o: Input string, modified on output
C--
      integer start, stop, lencpy, curorg, curshf
Cbegin


      if ( ST_FAILED ) return

      stop = len(str)

      do start = 1, stop
         if ( str(start:start).ne.' ' ) go to 1
      enddo
      start = stop + 1
    1 continue

      if ( start.gt.1 .and. start.le.stop ) then
         lencpy = stop - start + 1
         do curshf = 1, lencpy
            curorg = curshf + start - 1
            str(curshf:curshf) = str(curorg:curorg)
         enddo
         str(lencpy+1:) = ' '
      endif


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C LENS -- (FN) Find position of last non-blank character in a string
C  If all blank, then 0 is returned
C
C  alan penny                     ral             1990 Jan

      integer function lens ( ch )

      implicit none
      include 'STARMAN_INC'

      character*(*)   ch  	!i: Input string
C--
      integer ip
Cbegin


      lens = 0

      if ( ST_FAILED ) return

      ip = len ( ch )
      if ( ip.eq.0 ) return

      do while ( ip.ne.0 .and. ch(ip:ip).eq.' ' )
         ip = ip - 1
      enddo
      lens = ip


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C LOWCASE -- Convert a character string to lower case
C
C a j penny             ral                          1991 March

      subroutine lowcase ( a, b )

      implicit none
      include 'STARMAN_INC'

      character*(*)  a		!i: Input string
      character*(*)  b		!o: Output string
C--
      integer j, k, kl, lens
      external lens
Cbegin


      if ( ST_FAILED ) return

      b = a
      kl = lens(b)
      do k = 1, kl
         j = ichar(b(k:k))
         if ( j.ge.65 .and.  j.le.90 ) b(k:k) = char(j+32)
      enddo

      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C NAMECHR -- See if two 20 character 'names' are the same
C
C a j penny             ral                          1990-06-06

      subroutine namechr ( na, nb, ka )

      implicit none
      include 'STARMAN_INC'

      byte	na(20)			!i: First name
      byte	nb(20)			!i: Second name
      integer	ka			!o: Match flag (0=same;1=different)
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      ka = 0
      k = 1
      do while ( k.le.20 .and. ka.eq.0 )
         if ( na(k).ne.nb(k) ) ka = 1
         k = k + 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C NAMEGT -- Get the 20 character 'name' identifier from a line in a table
C
C a j penny             stsci                  1987-03-22

      subroutine namegt ( tb, tbvx, tby, kl, name )

      implicit none
      include 'STARMAN_INC'

      integer	tbvx			!i: No of columns in table
      integer	tby			!i: No of rows in table
      integer	tb(tbvx,tby)		!i: The table
      integer	kl			!i: Line to access
      character name*20			!o: Name found there
C--
      integer buf(20)
      byte asc(20)
      equivalence (buf(1),asc(1))
      integer i, nchar
Cbegin


      if ( ST_FAILED ) return

      call amovz ( tb(1,kl), buf(1), 20 )		!Load name

      do i = 1, 20					!Convert identifier bytes
         nchar = asc(i)					! to characters
         name(i:i) = char(nchar)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C NAMETR -- Translate a 20 character 'name' into 5 Reals
C
C a j penny             ral                     1990-06-06

      subroutine nametr ( na, rna )

      implicit none
      include 'STARMAN_INC'

      character na*20			!i: Input name
      real	rna(5)			!o: Output name
C--
      real rid(5)
      byte bid(20)
      equivalence (bid(1),rid(1))
      integer k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, 20
         bid(k) = ichar(na(k:k))
      enddo

      call amovr ( rid, rna, 5 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C NUMEL -- Get no and location of elements in a text string
C If an element is null (i.e. blanks or two commas or comma
C at start or end of text), then this is marked by the end
C position being before the start position.
C
C  alan penny                 ral                1990-06-15

      subroutine numel ( text, num, ks, ke )

      implicit none
      include 'STARMAN_INC'

      character*(*)   text	!i: Text string
      integer         num	!o: Number of elements
      integer         ks(1000)	!o: Start locations of elements
      integer         ke(1000)	!o: End locations of elements
C--
      integer k, klen, lens
      external lens
      logical gap, iscomma
      character*1 cv
Cbegin


      if ( ST_FAILED ) return

      klen = lens(text)

      if ( klen.lt.1 ) then
         num = 0
         ks(1) = 1
         ke(1) = 0
         return
      endif

      if ( klen.eq.1 ) then
         if ( text(1:1).eq.',' ) then
            num = 2
            ks(1) = 1
            ke(1) = 0
            ks(1) = 2
            ke(1) = 1
         else
            num = 1
            ks(1) = 1
            ke(1) = 1
         endif
         return
      endif

      cv = text(1:1)
      if ( cv.eq.',' ) then
         gap = .true.
         iscomma = .true.
         num = 1
         ks(1) = 1
         ke(1) = 0
      elseif ( cv.eq.' ' ) then
         gap = .true.
         iscomma = .false.
         num = 0
      else
         gap = .false.
         iscomma = .false.
         num = 1
         ks(1) = 1
      endif

      do k = 2, klen
         cv = text(k:k)
         if ( gap ) then
            if ( cv.ne.' ' .and. cv.ne.',' ) then
               iscomma = .false.
               gap = .false.
               num = num + 1
               ks(num) = k
            elseif ( cv.eq.',' ) then
               if ( iscomma ) then
                  num = num + 1
                  ks(num) = k - 1
                  ke(num) = k - 2
               endif
               iscomma = .true.
            endif
         else
            if ( cv.eq.' ' .or. cv.eq.',' ) then
               gap = .true.
               ke(num) = k - 1
               if ( cv.eq.',' ) iscomma = .true.
            endif
         endif
      enddo

      cv = text(klen:klen)
      if ( cv.eq.',' ) then
         num = num + 1
         ks(num) = klen
         ke(num) = klen - 1
      else
         ke(num) = klen
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PARGBEG -- Start text values text loading
C
C alan penny                  ral          1990-06-24

      subroutine pargbeg ( )

      implicit none
      include 'STARMAN_INC'

C--
      integer nint, nreal, aint(1000), nlog, nch, nach(100)
      real areal(1000)
      logical alog(1000)
      common / pargaa / aint, nint, areal, nreal, alog, nlog, nch, nach
Cbegin


      if ( ST_FAILED ) return

      NINT = 0
      NREAL = 0
      NLOG = 0
      NCH = 0


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PARGC -- Load character string for text loading with constant value
C
C alan penny                  ral          1990-06-24

      subroutine pargc ( ch )

      implicit none
      include 'STARMAN_INC'

      character*(*)	ch		!i: Input character string
C--
      integer nint, nreal, aint(1000), nlog, nch, nach(100)
      real areal(1000)
      logical alog(1000)
      common / pargaa / aint, nint, areal, nreal, alog, nlog, nch, nach
      character*200 ach(100)
      common / pargcc / ach

      character*200 bch
      integer kl, lens
      external lens
Cbegin


      if ( ST_FAILED ) return

      NCH = min(100,NCH+1)

      if ( NCH.lt.1 ) then
         call printo ( 'ERROR: In s/r PARGC - no text' )
         NCH = 1
      endif

      kl = min(lens(ch),200)
      if ( kl.eq.0 ) then
         NACH(NCH) = 0
      else
         bch = ch(1:kl)
         call lbgone ( bch )
         kl = lens(bch)
         ACH(NCH) = bch(1:kl)
         NACH(NCH) = kl
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PARGI -- Load integer for text loading with constant value
C
C alan penny                  ral          1990-06-24

      subroutine pargi ( n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: Input integer
C--
      integer nint, nreal, aint(1000), nlog, nch, nach(100)
      real areal(1000)
      logical alog(1000)
      common / pargaa / aint, nint, areal, nreal, alog, nlog, nch, nach
Cbegin


      if ( ST_FAILED ) return

      NINT = min(1000,NINT+1)
      if ( NINT.lt.1 ) then
         call printo ( 'ERROR: In s/r PARGI' )
         NINT = 1
      endif

      AINT(NINT) = n


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PARGL -- Load boolean for text loading with constant value
C
C alan penny                  ral          1990-06-24

      subroutine pargl ( l )

      implicit none
      include 'STARMAN_INC'

      logical	l		!i: Input integer
C--
      integer nint, nreal, aint(1000), nlog, nch, nach(100)
      real areal(1000)
      logical alog(1000)
      common / pargaa / aint, nint, areal, nreal, alog, nlog, nch, nach
Cbegin


      if ( ST_FAILED ) return

      NLOG = min(1000,NLOG+1)
      if ( NLOG.lt.1 ) then
         call printo ( 'ERROR: In s/r PARGL' )
         NLOG = 1
      endif

      ALOG(NLOG) = l


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PARGR -- Load real for text loading with constant value
C
C alan penny                  ral          1990-06-24

      subroutine pargr ( r )

      implicit none
      include 'STARMAN_INC'

      real	r		!i: Input real
C--
      integer nint, nreal, aint(1000), nlog, nch, nach(100)
      real areal(1000)
      logical alog(1000)
      common / pargaa / aint, nint, areal, nreal, alog, nlog, nch, nach
Cbegin


      if ( ST_FAILED ) return

      NREAL = max(1,min(1000,NREAL+1))
      if ( NREAL.lt.1 ) then
         call printo ( 'ERROR: In s/r PARGR' )
         NREAL = 1
      endif

      AREAL(NREAL) = r


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PRINTD -- Output text string to CL with embedded constants
C  %c = text; %d = integer; %f = real; %l = logical
C
C alan penny                  ral          1990-06-24

      subroutine printd ( ch )

      implicit none
      include 'STARMAN_INC'

      character*(*)	ch		!i: Input text string
C--
      integer nint, nreal, aint(1000), nlog, nch, nach(100)
      real areal(1000)
      logical alog(1000)
      character*200 ach(100)
      common / pargaa / aint, nint, areal, nreal, alog, nlog, nch, nach
      common / pargcc / ach

      integer lens, io, j, kl, kout, kin, km, kd, ka, nlen, ktext
      real ro
      logical more, lo
      character texta*100, textt*2000, texto*100, co*200
      external lens
Cbegin


      if ( ST_FAILED ) return

      kout = 1
      kin = 1
      textt = ' '

      km = lens(ch)
      more = .true.
      do while ( more .and. kin.lt.km )
         ka = index(ch(kin:),'%')
         if ( ka.eq.0 ) then
            more = .false.
         else

            if ( ka.ne.1 ) then
               textt(kout:kout+ka-2) = ch(kin:kin+ka-2)
               kout = kout + ka - 1
            endif
            kin = kin + ka

            ktext = 0
            if ( ch(kin:kin).eq.'d' ) then
               io = AINT(1)
               if ( NINT.ge.2 ) then
                  do j = 1, NINT-1
                     AINT(j) = AINT(j+1)
                  enddo
               endif
               NINT = NINT - 1
               write ( texta, '(i30)' ) io
               kin = kin + 1
            elseif ( ch(kin:kin).eq.'f' ) then
               ro = AREAL(1)
               if ( NREAL.ge.2 ) then
                  do j = 1, NREAL-1
                     AREAL(j) = AREAL(j+1)
                  enddo
               endif
               NREAL = NREAL - 1
               if ( abs(ro).gt.1.0e10 .or. abs(ro).lt.1.0e-3 ) then
                  write ( texta, '(g17.5)' ) ro
               else
                  write ( texta, '(f20.7)' ) ro
                  j = 15 + max(0.0,alog10(1000000.0/abs(ro)))
                  if ( j.lt.20 ) texta(j:) = ' '
               endif
               kin = kin + 1
            elseif ( ch(kin:kin).eq.'l' ) then
               lo = ALOG(1)
               if ( NLOG.ge.2 ) then
                  do j = 1, NLOG-1
                     ALOG(j) = ALOG(j+1)
                  enddo
               endif
               NLOG = NLOG - 1
               write ( texta, '(l1)' ) lo
               kin = kin + 1
            elseif ( ch(kin:kin).eq.'c' ) then
               co = ACH(1)
               nlen = NACH(1)
               if ( NCH.ge.2 ) then
                  do j = 1, NCH-1
                     ACH(j) = ACH(j+1)
                     NACH(j) = NACH(j+1)
                  enddo
               endif
               NCH = NCH - 1
               if ( nlen.eq.0 ) then
                  texta = ' '
               else
                  texta = co(1:nlen)
               endif
               ktext = 1
               kin = kin + 1
            endif

            if ( ktext.eq.1 .and. nlen.ne.0 ) then
               kl = nlen
            else
               call lbgone ( texta )
               kl = lens(texta)
            endif

            textt(kout:kout+kl-1) = texta(1:kl)
            kout = kout + kl

         endif
      enddo

      km = lens(ch)
      if ( km.gt.kin ) then
         kd = km - kin
         textt(kout+1:kout+kd) = ch(kin+1:km)
      endif

      kl = min(lens(textt),99)
      texto = textt(1:kl)
      call printo ( texto(1:kl) )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PTHEAD -- Put a character string as a file column header
C     It is put as the contents of a descriptor onto a file where the
C     descriptor is of the form 'HEADn' where 'n' is the desired
C     number 001 or 002 thru 099 to 999.
C     They should be at least 20 characters long and must be 72 or less.
C
C   alan penny            ral           1990 jan

      subroutine pthead ( file, num, text, ierr )

      implicit none
      include 'STARMAN_INC'

      character*(*) file	!i: File to write to
      integer       num		!i: Number of header to write to
      character*(*) text	!i: Header to write
      integer       ierr	!o: Error flag (always ok )
C--
      character*72 texta
      character texth*7
Cbegin


      if ( ST_FAILED ) return

      ierr = 0						!Set error flag

      write ( texth, '(''HEAD'',i3.3)' ) num		!Put header
      texta = text
      call ptdesc ( file, texth, texta )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C RCHECK -- Read a real number and returns true if bit n is set, false if not
C           (i.e.2**(n-1) is set.)
C
C     a j penny                stsci                   1987-02-24


      logical function rcheck ( val, num )
      implicit none

      real val		!i: Number to get result from
      integer num	!i: Bit in number to access
C--
      integer j, kval
Cbegin


      if ( num.lt.1 .or. num .gt.18 ) then
         rcheck = .false.
         call printo ( 'ERROR: Function RCHECK out of bounds' )
         return
      endif

      kval = nint(mod(val,100000000.0))
      if ( num.gt.1 ) then
         do j = 1, num - 1
            kval = kval/2
         enddo
      endif
      rcheck = .false.
      kval = mod(kval,2)
      if ( kval.eq.1 ) rcheck = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C RCHZERO -- Check a 2-d sub-image in a 3-d real array, and returns
C            .FALSE. if it is ALL zero, .TRUE.  if NOT ALL zero
C
C  a j penny                stsci                    1987-02-23

      subroutine rchzero ( data, nx, ny, nz, num, nxa, nya, check )

      implicit none
      include 'STARMAN_INC'

      integer nx		!i: X size of input array
      integer ny		!i: Y size of input array
      integer nz		!i: Z size of input array
      real    data(nx,ny,nz)	!i: Input array
      integer num		!i: Z plane in input array to use
      integer nxa		!i: X extent of valid area in Z-plane used
      integer nya		!i: Y extent of valid area in Z-plane used
      logical check		!o: Result. (True if sub-image zero
				!            False if non-zero)
C--
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      check = .false.
      do k = 1, nya
         do j = 1, nxa
            if ( data(j,k,num).ne.0.0 ) check = .true.
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C RVTEXT -- Put a real number into a character string
C
C alan penny                  ral          1993 Feb

      subroutine rvtext ( rv, text )

      implicit none
      include 'STARMAN_INC'

      real              rv              !i: Input number
      character*(*)	text		!o: Output text string
					!    Must be >= 11 characters long
C--
      integer j, kl
      character*50 texta
Cbegin


      if ( ST_FAILED ) return

      if ( abs(rv).gt.1.0e10 .or. abs(rv).lt.1.0e-3 ) then
         write ( texta, '(g17.5)' ) rv
      else
         write ( texta, '(f20.7)' ) rv
         j = 15 + max(0.0,alog10(1000000.0/abs(rv)))
         if ( j.lt.20 ) texta(j:) = ' '
      endif

      call lbgone ( texta )
      call charln ( texta, kl )
      kl = min(kl,11)
      text = texta(1:kl)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TRUNC -- (Function) Truncate no of digits before decimal point
C  Thus if you want a number to be limited between
C                +99*99.0 and -9*99.0
C  where * can be any number of nines, but the -ve one has one less.
C  Useful in listing numbers to ensure they do not overflow bounds.
C
C    alan penny             ral               1989-08-13

      real function trunc ( a, n )

      implicit none

      real     a      !i: input
      integer  n      !i: Max no of characters before decimal point
C--
      real rmin, rmax
Cbegin


      rmax = 10.0**max(1.0,real(n))
      rmin = -1.0*(rmax/10.0)
      trunc = max((rmin+1.0),min((rmax-1.0),a))


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TRUNC_E -- (Function) Truncate no of digits after decimal point
C  Thus if you want a number to be rounded
C      123.4567 can go to  123.45
C     -123.4567 can go to -123.4567
C
C    alan penny             ral               1989-08-13

      real function trunc_e ( a, n )

      implicit none

      real     a      !i: input
      integer  n      !i: Max no of characters after decimal point
C--
      real ab, aat, sv
      integer k
Cbegin


      sv = sign(1.0,a)
      ab = abs(a)
      k = int(ab)
      aat = real(10**n)
      trunc_e = sv*(real(k)+(real(int(aat*(ab-real(k))))/aat))


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TYHELP -- Type out help
C
C     a j penny                stsci                   1987-02-24

      subroutine tyhelp ( thelp, nh, cmdlst, ksw, klw, jcon, num )

      implicit none
      include 'STARMAN_INC'

      integer		nh		!i: No of help lines
      character*68	thelp(*)	!i: Help text
      character*(*)	cmdlst		!i: Command list
      integer		ksw(200)	!i: Start posn of commands in list
      integer		klw(200)	!i: Length of commands in list
      integer		jcon(200)	!i: Order of commands in list
      integer		num		!i: No of commands in list
C--
      integer k, kl, j, ja, ks, ke
      logical more
Cbegin


      if ( ST_FAILED ) return

      call printo ( 'For more help (when responding to :=) ' )
      call printo ( '     - on parameters, type "?" ' )
      call printo ( '     - on the program, type "??" ')
      call printo ( '     - to exit, type "!!" ' )
      call printo ( ' ' )
      kl = 4

      k = 0
      if ( nh.ge.1 ) then
         more = .true.
         do while ( k.lt.nh .and. more )
            k = k + 1
            kl = kl + 1
            call printo ( thelp(k) )
            if ( k.ne.nh .and. mod(kl,20).eq.0 ) then
               call morehelp ( more )
            endif
         enddo
      else
         call printo (
     +           'No help available. Try "?" or "$help PROGRAM_NAME"' )
         call printo ( 'Options are:-' )
         call printo ( '--------------------------------' )
         kl = kl + 3
         more = .true.
         do while ( k.lt.num .and. more )
            k = k + 1
            kl = kl + 1
            j = 1
            do ja = 1, num
               if ( jcon(ja).eq.k ) j = ja
            enddo
            ks = ksw(j)
            ke = ks + klw(j) - 1
            if ( (ke-ks).gt.49 ) ke = ks + 49
            call printo ( cmdlst(ks:ke) )
            if ( k.ne.num .and. mod(kl,20).eq.0 ) then
                call morehelp ( more )
            endif
          enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C UPPCASE -- Convert a character string to upper case
C
C a j penny             ral                          1991 June

      subroutine uppcase ( a, b )

      implicit none
      include 'STARMAN_INC'

      character*(*)  a		!i: Input string
      character*(*)  b		!o: Output string
C--
      integer j, k, kl, lens
      external lens
Cbegin


      if ( ST_FAILED ) return

      b = a
      kl = lens(b)
      do k = 1, kl
         j = ichar(b(k:k))
         if ( j.ge.97 .and.  j.le.122 ) b(k:k) = char(j-32)
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C VALFORM -- Formats a real number into a 12 character text string
C
C   alan penny                   RAL            1991 May

      subroutine valform ( val, text )

      implicit none
      include 'STARMAN_INC'

      real            val	!i: Input number
      character*12   text	!o: Output text string
C--
      character*40 texta
      integer j, k, lens
      external lens
Cbegin


      if ( ST_FAILED ) return

      if ( val.eq.0.0 ) then
         texta = '0.0'
      elseif ( abs(val).gt.1.0e10 .or. abs(val).lt.1.0e-3 ) then
         write ( texta, '(g17.5)' ) val
      else
         write ( texta, '(f20.7)' ) val
         j = 15 + max(0.0,alog10(1000000.0/abs(val)))
         if ( val.lt.10.0 ) j = j - 1
         if ( j.lt.20 ) texta(j:) = ' '
      endif

      call lbgone ( texta )
      k = min(12,lens(texta))
      text = texta(1:k)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C WCHECK -- Write 1/0 into bit n of a real number if flag true/false
C           (i.e.2**(n-1)) if true or false flag.
C
C     a j penny                stsci                   1987-02-24


      subroutine wcheck ( val, num, test )

      implicit none
      include 'STARMAN_INC'

      real val			!i/o: Storage number
      integer num		!i:   Bit in number to write to
      logical test		!i:   if true, write 1, else 0
C--
      integer j, kval, jval, ival, mval
Cbegin


      if ( ST_FAILED ) return

C  Check not too far to left

      if ( num.lt.1 .or. num .gt.18 ) then
         call printo ( 'ERROR: Subroutine WCHECK out of bounds' )
         return
      endif

C  Get vals to left (jval) and from the bit to left (ival)

      kval = nint(mod(val,1000000.0))
      ival = kval
      if ( num.gt.1 ) then
         do j = 1, num - 1
            ival = ival/2
         enddo
      endif
      jval = ival/2

C  Shift left part back one step, and set bit

      mval = jval*2
      if ( test ) mval = mval + 1

C  Shift new left part back to original position

      if ( num.gt.1 ) then
         do j = 1, num-1
            mval = mval*2
         enddo
      endif

C  Restore right part

      if ( num.gt.1 ) then
         do j = 1, num-1
            ival = ival*2
         enddo
      endif
      ival = kval - ival

C  Get new number

      mval = mval + ival

C Return value

      val = mval


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ISCHAR -- (Fn) Is this a character?
C
C  alan Penny               ral             1991 Dec

      logical function ischar ( arg )
      implicit none
      character*1      arg	!i: Input character
C--
Cbegin
      ischar = (arg.ge.'A') .and. (arg.le.'Z')
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ISNUMB -- (Fn) Is this a number?
C
C  alan Penny               ral             1991 Dec

      logical function isnumb ( arg )
      implicit none
      character*1      arg	!i: Input character
C--
Cbegin
      isnumb = ((arg.ge.'0') .and. (arg.le.'9')).or.(arg.eq.'.' )
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ISSIGN -- (Fn) Is this a sign?
C
C  alan Penny               ral             1991 Dec

      logical function issign ( arg )
      implicit none
      character*1      arg	!i: Input character
C--
Cbegin
      issign = (arg.eq.'+') .or. (arg.eq.'-')
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  This is  STARFLIB1.FOR
C
C It contains:-
C
C ACUBINT    Cubic interpolates value/slope at a point in a set of 4 values
C CUBINT     Cubic interpolates value/slope at a point in a set of 4 values
C CUBINTX    Cubic interpolates value at a point in a set of 4 values



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CUBINT -- Cubic interpolates value/slope at a point in a set of 4 values
C  Given values for a function F(x) at positions x=-1,0,1,2, this
C  function estimates a value for the function at a position X,
C  using a cubic interpolating polynomial.  The specific polynomial used
C  has the properties CUBINT(0)=F(0), CUBINT(1)=F(1), and the first
C  derivative of CUBINT with respect to x is continuous at x=0 and x=1.
C
C  P B STETSON                  DAO                1986
C  A j penny                        ral             1991

      real function  cubint ( f, x, dfdx )

      implicit none
      real f(0:0)	!i: Data values (-1),(0),(1),(2)
      real x		!i: Position to interpolate at (0<=x<1)
      real dfdx		!o: Fitted slope df/dx at x
C--
      real c1, c2, c3
Cbegin


      c1 = 0.5*(f(1)-f(-1))
      c2 = 2.0*f(1)+f(-1)-0.5*(5.0*f(0)+f(2))
      c3 = 0.5*(3.0*(f(0)-f(1))+f(2)-f(-1))
      cubint = x*(x*(x*c3+c2)+c1) + f(0)
      dfdx = x*(x*c3*3.0+2.0*c2) + c1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ACUBINT -- Cubic interpolates value/slope at a point in a set of 4 values
C  Given values for a function F(x) at positions x=-1,0,1,2, this
C  function estimates a value for the function at a position X,
C  using acubic interpolating polynomial.  The specific polynomial used
C  has the properties ACUBINT(0)=F(0), ACUBINT(1)=F(1), and the first
C  derivative of ACUBINT with respect to x is continuous at x=0 and x=1.
C
C  P B STETSON                  DAO                1986
C  A j penny                        ral             1991

      real function  acubint ( f, x, dfdx )

      implicit none
      real f(0:0)	!i: Data values (-1),(0),(1),(2)
      real x		!i: Position to interpolate at (0<=x<1)
      real dfdx		!o: Fitted slope df/dx at x
C--
      real c1, c2, c3
Cbegin


      c1 = 0.5*(f(1)-f(-1))
      c2 = 2.0*f(1)+f(-1)-0.5*(5.0*f(0)+f(2))
      c3 = 0.5*(3.0*(f(0)-f(1))+f(2)-f(-1))
      acubint = x*(x*(x*c3+c2)+c1) + f(0)
      dfdx = x*(x*c3*3.0+2.0*c2) + c1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CUBINTX -- Cubic interpolates value at a point in a set of 4 values
C  Given values for a function F(x) at positions x=-1,0,1,2, this
C  function estimates a value for the function at a position X,
C  using a cubic interpolating polynomial.  The specific polynomial used
C  has the properties CUBINTX(0)=F(0), CUBINTX(1)=F(1), and the first
C  derivative of CUBINTX with respect to x is continuous at x=0 and x=1.
C
C  P B STETSON                      DAO 1989
C  A j penny                        ral             1991

      real function  cubintx ( f, x )

      implicit none
      real f(0:0)	!i: Data values (-1),(0),(1),(2)
      real x		!i: Position to interpolate at (0<=x<1)
C--
      real c1, c2, c3
Cbegin


      c1 = 0.5*(f(1)-f(-1))
      c2 = 2.0*f(1)+f(-1)-0.5*(5.0*f(0)+f(2))
      c3 = 0.5*(3.0*(f(0)-f(1))+f(2)-f(-1))
      cubintx = x*(x*(x*c3+c2)+c1) + f(0)


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is STARFLIBG.FOR
C
C     Contains:-
C GD_SETUP    Set up defaults before using graphics device
C GD_OPEN     Open graphics device
C GD_CLOSE    Close graphics device
C GD_DOBOX    Clear a graphics plot and make a graph box
C GD_OPTS     Plot points with optional ommitting and numbering
C GD_TEXT     Put a text string at a posn on graphic device
C GD_SETDEV   Set graph device not open
C GD_BBUF     Start plot buffer
C GD_EBUF     End plot buffer
C GD_UPDT     Flush pending commands

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GD_SETUP -- Set up defaults before using graphics device
C
C   a j penny                 dao           1988-04-25

      subroutine gd_setup ()

      implicit none
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      GD_DISPOP = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GD_OPEN -- Open graphics device
C
C   a j penny                 dao           1988-04-25

      subroutine gd_open ( istat )

      implicit none
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'

      integer    istat		!o: Error flag (1=ok;0=bad)
C--
      character*50 text
      integer n, k
Cbegin


      if ( ST_FAILED ) return

      istat = 0
      if ( .not.GD_DISPOP ) then
         call printo ( 'Input GKS name of device for graphs'//
     +                 ' (-ask- if dont know)' )
         call get1c ( 'GTYPE', text, 'xwindows', .true. )
         if ( ST_FAILED ) return
         if ( text.eq.'ask' ) text = '?'
         call pgbegin ( 0, text, 1, 1 )
         call pgqinf ( 'STATE', text, n )
         if ( n.eq.6 .and. text(1:6).eq.'CLOSED' ) then
            call printo ( 'ERROR: Cant open graphical display' )
            istat = 1
         else
            call pgask ( .false. )
            call get_job ( 'LINECOL', 'black:white', k, 1, 0, ' ' )
            GD_LINECOL = k
            if ( k.eq.1 ) call pgscr ( 1, 0.0, 0.0, 0.0 )
            if ( k.eq.2 ) call pgscr ( 1, 1.0, 1.0, 1.0 )
            GD_DISPOP = .true.
         endif
      else
         call pgpage
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GD_CLOSE -- Close graphics device
C
C   a j penny                 dao           1988-04-25

      subroutine gd_close ()

      implicit none
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( GD_DISPOP ) call pgend
      GD_DISPOP = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GD_DOBOX -- Clear a graphics plot and make a graph box
C
C   a j penny                 dao           1988-04-25

      subroutine gd_dobox ( xs, xe, xt, ys, ye, yt, capt, kj )

      implicit none
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'

      real		xs	!i: User X start
      real		xe	!i: User X end
      character*(*)	xt	!i: Text for X axis
      real		ys	!i: User Y start
      real		ye	!i: User Y end
      character*(*)	yt	!i: Text for Y axis
      character*(*)     capt    !i: Caption
      integer           kj      !i: 1=X/Y scaled equal;0=scaled indep
C--
Cbegin


      if ( ST_FAILED ) return

      GD_XS = xs
      GD_XE = ye
      GD_YS = ys
      GD_YE = ye

      call pgbbuf
      call pgenv ( xs, xe, ys, ye, kj, 0 )        !Draw axes

      call pgsch ( 1.0 )			 !Draw labels
      call pglabel ( xt, yt, capt )
      call pgebuf


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GD_OPTS -- Plot points with optional ommitting and numbering
C
C   a j penny                 dao           1988-04-25

      subroutine gd_opts ( x, y, z, n, doz, dotnum, isymb )

      implicit none
      include 'STARMAN_INC'

      integer	n	!i: no of points
      real	x(n)	!i: X values
      real	y(n)	!i: Y values
      logical	z(n)	!i: Flags as to plot a point
      logical	doz	!i: Use the flags?
      logical	dotnum	!i: Number the points?
      integer	isymb	!i: PGPLOT code for plot symbol (1=small dot)
C--
      integer k
      character*7 text
Cbegin


      if ( ST_FAILED ) return

      call pgbbuf

      if ( .not.doz .and. .not.dotnum ) then
         call pgpoint ( n, x, y, isymb )
      elseif ( doz .and. .not.dotnum ) then
         do k = 1, n
            if ( z(k) ) call pgpoint ( 1, x(k), y(k), isymb )
         enddo
      elseif ( .not.doz .and. dotnum ) then
         call pgpoint ( n, x, y, isymb )
         do k = 1, n
            if ( k.ge.100000) then
               write ( text, '(1x,i6)' ) k
            elseif ( k.ge.10000) then
               write ( text, '(1x,i5)' ) k
            elseif ( k.ge.1000) then
               write ( text, '(1x,i4)' ) k
            elseif ( k.ge.100) then
               write ( text, '(1x,i3)' ) k
            elseif ( k.ge.10) then
               write ( text, '(1x,i2)' ) k
            else
               write ( text, '(1x,i1)' ) k
            endif
            call pgtext ( x(k), y(k), text )
         enddo
      else
         do k = 1, n
            if ( z(k) ) then
               call pgpoint ( 1, x(k), y(k), isymb )
               if ( k.ge.100000) then
                  write ( text, '(1x,i6)' ) k
               elseif ( k.ge.10000) then
                  write ( text, '(1x,i5)' ) k
               elseif ( k.ge.1000) then
                  write ( text, '(1x,i4)' ) k
               elseif ( k.ge.100) then
                  write ( text, '(1x,i3)' ) k
               elseif ( k.ge.10) then
                  write ( text, '(1x,i2)' ) k
               else
                  write ( text, '(1x,i1)' ) k
               endif
               call pgtext ( x(k), y(k), text )
            endif
         enddo
      endif

      call pgebuf


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GD_TEXT -- Put a text string at a posn on graphic device

C   a j penny                 dao           1988-04-25

      subroutine gd_text ( x, y, text )

      implicit none
      include 'STARMAN_INC'

      real	x		!i: X position
      real	y		!i: Y position
      character*(*) text	!i: Text string
C--
Cbegin


      if ( ST_FAILED ) return

      call pgtext ( x, y, text )
      call pgebuf


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GD_SETDEV -- Set graph device not open
C
C   a j penny                 dao           1988-04-25

      subroutine gd_setdev ()

      implicit none
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      GD_DISPOP = .false.
      GD_BAD = 1


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GD_BBUF -- Start command buffer
C
C   a j penny                 dao           1988-04-25

      subroutine gd_bbuf ()

      implicit none
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call pgbbuf


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GD_EBUF -- End command buffer
C
C   a j penny                 dao           1988-04-25

      subroutine gd_ebuf ()

      implicit none
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call pgebuf


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GD_UPDT -- Flush commands
C
C   a j penny                 dao           1988-04-25

      subroutine gd_updt ()

      implicit none
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call pgupdt


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    This is STARFLIBM.FOR
C
C    It contains general array and maths subroutines:-
C
C ADDCOL(IRS)  Add a constant to a column in a (int:real:short) array
C ADIFFR       See if two real arrays are different
C ARRSC        Scale a real array (Out = in*BS + BZ)
C COP(ab)      Copy array part into another array part (to:fm int:int*2:real)
C COPFR(IR)    Copy vertical column of a real 2-D array to (int:real) vector
C COPRR(R)     Copy real array ([scaled]; invalid flags) to real array
C COPSS(IRS)   Copy int*2 array ([scaled]; invalid flags) to int*2:int:real array
C COPT(IR)R    Copy an (int:real) row part into a column of a real 2-D array
C COPV(RS)(IRS)Extract part of a (real:int*2) array to (int*2:integer:real) array
C COP(1R:R1)   Copy a single real value (to:from) a real array location
C COP(1S:S1)   Copy a single short value (to:from) a short array location
C CSWOP(IR)    Swop two integers/reals if second smaller than first
C DETERM       (function) Calc the determinant of a square matrix
C FOURC        Calc Fourier Transform
C FOURN        Calc Fourier Transform in N dimensions
C GASDEV       Make normally (Gauss) distributed random number
C GAUSS1R      Solve for a 1-D (modified) Gaussian for real data
C GAUSS2RA     Fit a 2-D gaussian to a real array and bad pixels
C GAUSS2SA     Fit a 2-D gaussian to an int*2 array and bad pixels
C GAUSS2R      Fit a 2-D Gaussian (fast) to real array with centered star
C GAUSS_CN     Update s/r GAUSS2R estimates and see if converged
C GAUSS_PR     Sets up terms for use in GAUSS2R
C GPROB        Calc prob of a one-tailed Normal (Gauss) distribution
C HGRAM(RS)    Load histogram from values in a real/integer*2 array
C LINFIT       Take set of X,Y points and fits a straight line to them.
C LINFITA      Find mean and slope of a vector (simple and fast)
C LUDECMAT     Decompose matrix by LU decomposition
C LUSOLMAT     Solve a matrix by LU decompsoition
C MANNWHIT     Perform a 1-tailed mann-whitney (Wilcoxon) test to see
C MATINV       Invert a matrix
C MEANSTD(IR)  Calc mean and std deviation of an (integer:real) vector.
C POIDEV       Make a poisson randomised integer output of a real input number
C POLFIT       Least squares fit to data with a polynomial curve,
C PROD(CR/CR)  Sum products of an array row/column with row/column of another
C MEDIAN(RS)   Find median of real:int*2 bad pixel flagged image section
C RANGE(IRS)   Find mean, std dev of int:real:int*2 bad pixeled image section
C RANO         Make random number between 0 and 1.
C RINTER       (function) Interpolate in a two-dimensional look-up table.
C RSSCALE      Scale a real array into an integer*2 array in range
C SIMULX       Solve the simultaneous equations   Y = A.X  for X.
C SMOOTHR      Smooth a real array with a rectangular 'local mean' filter
C SMOOTHB(RS)  Smooth real:int*2 flagged array with rectangular 'local mean' filter
C SORT1(IR)    Sort of an (integer:real) vector
C SORT2(IR)    Sort of an (integer:real) vector, with alignment of another
C SRT1H(IR)    Heapsort to sort (integers:reals) into ascending order
C SRT1P(IR)    Straight insertion sort (ints:reals) to ascending order
C SRT1S(IR)    Shell's method insertion sort (ints:reals) to ascending order
C SRT2H(IR)    Heapsort to sort (integers:reals) with alignment of another
C SRT2P(IR)    Straight insertion sort (ints:reals) with alignment of another
C SRT2S(IR)    Shell's method sort (ints:reals) with alignment of another
C ST_MINMAX    Get max and min of flagged area of array
C ST_MINMAX(RS) Get max and min of flagged area of (real:int*2) array
C TRAN_DOIT    Transfromation between two sets of x,y posns - set up
C TRAN_LINTRAN Transformation between points - calculate



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ADDCOLI -- Adds constant to a column of an integer array
C   a j penny                 ral           1994-09-01
      subroutine addcoli ( a, nx, ny, kcol, k )
      implicit none
      integer   nx		!i: no of columns
      integer   ny		!i: no of rows
      integer   a(nx,ny)	!i/o: 1st input vector
      integer   kcol	 	!i: column to add to
      integer   k 		!i: constant to add
C--
      integer j
Cbegin
      do j = 1, ny
         a(kcol,j) = a(kcol,j) + k
      enddo
      end
C ADDCOLR -- Adds constant to a column of a real array
C   a j penny                 ral           1994-09-01
      subroutine addcolr ( a, nx, ny, kcol, k )
      implicit none
      integer   nx		!i: no of columns
      integer   ny		!i: no of rows
      real      a(nx,ny)	!i/o: 1st input vector
      integer   kcol	 	!i: column to add to
      real      k 		!i: constant to add
C--
      integer j
Cbegin
      do j = 1, ny
         a(kcol,j) = a(kcol,j) + k
      enddo
      end
C ADDCOLS -- Adds constant to a column of a short array
C   a j penny                 ral           1994-09-01
      subroutine addcols ( a, nx, ny, kcol, k )
      implicit none
      integer    nx		!i: no of columns
      integer    ny		!i: no of rows
      integer*2  a(nx,ny)	!i/o: 1st input vector
      integer    kcol	 	!i: column to add to
      integer*2  k 		!i: constant to add
C--
      integer j
Cbegin
      do j = 1, ny
         a(kcol,j) = a(kcol,j) + k
      enddo
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ADIFFR -- See if two real arrays are different
C
C   a j penny                    dao	         1988-05-16

      subroutine adiffr ( r1, r2, n, flag )

      implicit none
      include 'STARMAN_INC'

      integer	n	!i: No of points
      real	r1(n)	!i: frst input array
      real	r2(n)	!i: frst input array
      logical	flag	!o: true if diff, false if same
C--
      integer j
Cbegin


      if ( ST_FAILED ) return

      flag = .false.
      j = 0
      do while ( j.lt.n .and. .not.flag )
         j = j + 1
         if ( r1(j).ne.r2(j) ) flag = .true.
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ARRSC -- Scale a real array  (Out = in*BS + BZ)
C
C  a j penny                      stsci                   1987-01-25


      subroutine arrsc ( data, nx, ny, bs, bz )

      implicit none
      include 'STARMAN_INC'

      integer nx		!i: X size of array
      integer ny		!i: Y Size of array
      real    data(nx,ny)	!i/o: Array to scale
      real    bs		!i: Scaling factor
      real    bz		!i: Zero factor
C--
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, ny
         do j = 1, nx
            data(j,k) = bs*data(j,k) + bz
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPII -- Copy part of an integer array into an area of an integer array
C
C    a j penny                ral                 88-08-12

      subroutine copii ( in, n, m, ixs, ixe, iys, iye, out, n1, m1,
     +                   oxs, oys )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: X size of input image
      integer	m		!i: Y size of input image
      integer	in(n,m)		!i: Input image
      integer	ixs     	!i: X start of input area to be copied
      integer	ixe		!i: X end of input area to be copied
      integer	iys     	!i: Y start of input area to be copied
      integer	iye		!i: Y end of input area to be copied
      integer	n1		!i: X size of output image
      integer	m1		!i: Y size of output image
      integer   out(n1,m1)	!i/o: Output image
      integer	oxs     	!i: X start of output area to be copied into
      integer	oys		!i: Y start of output area to be copied into
C--
      integer i, j, ox, oy
Cbegin


      if ( ST_FAILED ) return

      do j = iys, iye
         do i = ixs, ixe
            if ( i.ge.1 .and. i.le.n .and. j.ge.1 .and. j.le.m ) then
               ox = oxs + i - ixs
               oy = oys + j - iys
               if ( ox.ge.1 .and. ox.le.n1 .and. oy.ge.1 .and.
     +              oy.le.m1 ) out(ox,oy) = real(in(i,j))
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPIR -- Copy part of an integer array into an area of a real array
C
C    a j penny                ral                 88-08-12

      subroutine copir ( in, n, m, ixs, ixe, iys, iye, out, n1, m1,
     +                   oxs, oys )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: X size of input image
      integer	m		!i: Y size of input image
      integer	in(n,m)		!i: Input image
      integer	ixs     	!i: X start of input area to be copied
      integer	ixe		!i: X end of input area to be copied
      integer	iys     	!i: Y start of input area to be copied
      integer	iye		!i: Y end of input area to be copied
      integer	n1		!i: X size of output image
      integer	m1		!i: Y size of output image
      real	out(n1,m1)	!i/o: Output image
      integer	oxs     	!i: X start of output area to be copied into
      integer	oys		!i: Y start of output area to be copied into
C--
      integer i, j, ox, oy
Cbegin


      if ( ST_FAILED ) return

      do j = iys, iye
         do i = ixs, ixe
            if ( i.ge.1 .and. i.le.n .and. j.ge.1 .and. j.le.m ) then
               ox = oxs + i - ixs
               oy = oys + j - iys
               if ( ox.ge.1 .and. ox.le.n1 .and. oy.ge.1 .and.
     +              oy.le.m1 ) out(ox,oy) = real(in(i,j))
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPRR -- Copy part of a real array into an area of a real array
C
C    a j penny                ral                 88-08-12

      subroutine coprr ( in, n, m, ixs, ixe, iys, iye, out, n1, m1,
     +                   oxs, oys )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: X size of input image
      integer	m		!i: Y size of input image
      real	in(n,m)		!i: Input image
      integer	ixs     	!i: X start of input area to be copied
      integer	ixe		!i: X end of input area to be copied
      integer	iys     	!i: Y start of input area to be copied
      integer	iye		!i: Y end of input area to be copied
      integer	n1		!i: X size of output image
      integer	m1		!i: Y size of output image
      real	out(n1,m1)	!i/o: Output image
      integer	oxs     	!i: X start of output area to be copied into
      integer	oys		!i: Y start of output area to be copied into
C--
      integer i, j, ox, oy
Cbegin


      if ( ST_FAILED ) return

      do j = iys, iye
         do i = ixs, ixe
            if ( i.ge.1 .and. i.le.n .and. j.ge.1 .and. j.le.m ) then
               ox = oxs + i - ixs
               oy = oys + j - iys
               if ( ox.ge.1 .and. ox.le.n1 .and. oy.ge.1 .and.
     +              oy.le.m1 ) out(ox,oy) = in(i,j)
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPRS -- Copy part of a real array into an area of a short array
C
C    a j penny                ral                 88-08-12

      subroutine coprs ( in, n, m, ixs, ixe, iys, iye, out, n1, m1,
     +                   oxs, oys )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: X size of input image
      integer	m		!i: Y size of input image
      real	in(n,m)		!i: Input image
      integer	ixs     	!i: X start of input area to be copied
      integer	ixe		!i: X end of input area to be copied
      integer	iys     	!i: Y start of input area to be copied
      integer	iye		!i: Y end of input area to be copied
      integer	n1		!i: X size of output image
      integer	m1		!i: Y size of output image
      integer*2	out(n1,m1)	!i/o: Output image
      integer	oxs     	!i: X start of output area to be copied into
      integer	oys		!i: Y start of output area to be copied into
C--
      integer i, j, ox, oy
Cbegin


      if ( ST_FAILED ) return

      do j = iys, iye
         do i = ixs, ixe
            if ( i.ge.1 .and. i.le.n .and. j.ge.1 .and. j.le.m ) then
               ox = oxs + i - ixs
               oy = oys + j - iys
               if ( ox.ge.1 .and. ox.le.n1 .and. oy.ge.1 .and.
     +              oy.le.m1 ) out(ox,oy) =
     +                           int(max(-32768.0,min(32767.0,in(i,j))))
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPSR -- Copy part of a short array into an area of a real array
C
C    a j penny                ral                 88-08-12

      subroutine copsr ( in, n, m, ixs, ixe, iys, iye, out, n1, m1,
     +                   oxs, oys )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: X size of input image
      integer	m		!i: Y size of input image
      integer*2	in(n,m)		!i: Input image
      integer	ixs     	!i: X start of input area to be copied
      integer	ixe		!i: X end of input area to be copied
      integer	iys     	!i: Y start of input area to be copied
      integer	iye		!i: Y end of input area to be copied
      integer	n1		!i: X size of output image
      integer	m1		!i: Y size of output image
      real	out(n1,m1)	!i/o: Output image
      integer	oxs     	!i: X start of output area to be copied into
      integer	oys		!i: Y start of output area to be copied into
C--
      integer i, j, ox, oy
Cbegin


      if ( ST_FAILED ) return

      do j = iys, iye
         do i = ixs, ixe
            if ( i.ge.1 .and. i.le.n .and. j.ge.1 .and. j.le.m ) then
               ox = oxs + i - ixs
               oy = oys + j - iys
               if ( ox.ge.1 .and. ox.le.n1 .and. oy.ge.1 .and.
     +              oy.le.m1 ) out(ox,oy) = real(in(i,j))
            endif
         enddo
      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPSS -- Copy part of a short array into an area of a short array
C
C    a j penny                ral                 88-08-12

      subroutine copss ( in, n, m, ixs, ixe, iys, iye, out, n1, m1,
     +                   oxs, oys )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: X size of input image
      integer	m		!i: Y size of input image
      integer*2	in(n,m)		!i: Input image
      integer	ixs     	!i: X start of input area to be copied
      integer	ixe		!i: X end of input area to be copied
      integer	iys     	!i: Y start of input area to be copied
      integer	iye		!i: Y end of input area to be copied
      integer	n1		!i: X size of output image
      integer	m1		!i: Y size of output image
      integer*2	out(n1,m1)	!i/o: Output image
      integer	oxs     	!i: X start of output area to be copied into
      integer	oys		!i: Y start of output area to be copied into
C--
      integer i, j, ox, oy
Cbegin


      if ( ST_FAILED ) return

      do j = iys, iye
         do i = ixs, ixe
            if ( i.ge.1 .and. i.le.n .and. j.ge.1 .and. j.le.m ) then
               ox = oxs + i - ixs
               oy = oys + j - iys
               if ( ox.ge.1 .and. ox.le.n1 .and. oy.ge.1 .and.
     +              oy.le.m1 ) out(ox,oy) = in(i,j)
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPFRI -- Copy vertical column of a real 2-D array to an integer vector
C
C    a j penny                ral                 1990 Jan

      subroutine copfri ( in, nxi, nyi, ix, iy, out, n )

      implicit none
      include 'STARMAN_INC'

      integer	nxi		!i: X size of input array
      integer	nyi		!i: Y size of input array
      real	in(nxi,nyi)	!i: Input array
      integer	ix		!i: X start in input array
      integer	iy		!i: Y start in input array
      integer	out(*)		!o: Output array
      integer	n	     	!i: Length to copy
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, n
         out(k) = in(ix,k+iy-1)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPFRR -- Copy vertical column of a real 2-D array to a real vector
C
C    a j penny                ral                 1990 Jan

      subroutine copfrr ( in, nxi, nyi, ix, iy, out, n )

      implicit none
      include 'STARMAN_INC'

      integer	nxi		!i: X size of input array
      integer	nyi		!i: Y size of input array
      real	in(nxi,nyi)	!i: Input array
      integer	ix		!i: X start in input array
      integer	iy		!i: Y start in input array
      real	out(*)		!o: Output array
      integer	n	     	!i: Length to copy
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, n
         out(k) = in(ix,k+iy-1)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPSSI -- Copy a integer*2 array (with invalid flags) to an integer one.
C  Integer = integer*2. Invalid points replaced with last (in fortran
C  sense ) pixel value, or 0 if it is the first pixel
C
C  Deals with case in an X row where contiguous pixels are invalid coming
C  in either from the left or right edges. In this case, pixels given
C  edge value of previous X row.
C
C     a j penny                  ral          1991 apr

      subroutine copssi ( in, nx, ny, inval, out )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of array
      integer	ny		!i: X size of array
      integer*2 in(nx,ny)	!i: Input array
      integer	inval		!i: Input array invalid flag value
      integer	out(nx,ny)	!o: Output array
C--
      integer jx, jy, iv, ilast, iclast, iclaste, kk
Cbegin


      if ( ST_FAILED ) return

      ilast = 0
      iclast = 0
      iclaste = 0
      do jy = 1, ny

         do jx = 1, nx
            iv = in(jx,jy)
            if ( iv.eq.inval ) then
               if ( jx.eq.1 ) then
                  out(jx,jy) = iclast
                  ilast = iclast
               else
                  out(jx,jy) = ilast
               endif
            else
               out(jx,jy) = iv
               ilast = iv
               if ( jx.eq.1 ) iclast = iv
            endif
         enddo

         if ( in(nx,jy).eq.inval ) then
            out(nx,jy) = iclaste
            kk = nx - 1
            do while ( kk.ge.1 )
               if ( in(kk,jy).eq.inval ) then
                  out(kk,jy) = iclaste
                  kk = kk - 1
               else
                  kk = 0
               endif
            enddo
         else
            iclaste = iv
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPSSR -- Copy a integer*2 array (scaled with invalid flags) to a real one.
C  Real = scale.integer*2 + zero. Invalid points replaced with last (in
C  fortran sense ) pixel value, or 0.0 if it is the first pixel
C
C  Deals with case in an X row where contiguous pixels are invalid coming
C  in either from the left or right edges. In this case, pixels given
C  edge value of previous X row.
C
C     a j penny                  ral          1991 apr

      subroutine copssr ( in, nx, ny, bs, bz, inval, out )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of array
      integer	ny		!i: X size of array
      integer*2 in(nx,ny)	!i: Input array
      real	bs		!i: Scale of input array
      real	bz		!i: Zero of invalid array
      integer	inval		!i: Input array invalid flag value
      real	out(nx,ny)	!o: Output array
C--
      real alast, aclast, aclaste, rv
      integer jx, jy, iv, kk
Cbegin


      if ( ST_FAILED ) return

      alast = 0.0
      aclast = 0.0
      aclaste = 0.0
      do jy = 1, ny

         do jx = 1, nx
            iv = in(jx,jy)
            if ( iv.eq.inval ) then
               if ( jx.eq.1 ) then
                  out(jx,jy) = aclast
                  alast = aclast
               else
                  out(jx,jy) = alast
               endif
            else
               rv = real(iv)*bs + bz
               out(jx,jy) = rv
               alast = rv
               if ( jx.eq.1 ) aclast = alast
            endif
         enddo

         if ( in(nx,jy).eq.inval ) then
            out(nx,jy) = aclaste
            kk = nx - 1
            do while ( kk.ge.1 )
               if ( in(kk,jy).eq.inval ) then
                  out(kk,jy) = aclaste
                  kk = kk - 1
               else
                  kk = 0
               endif
            enddo
         else
            aclaste = rv
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPRRR -- Copy a real array (scaled with invalid flags) to a real one.
C  Real = scale.real + zero. Invalid points replaced with last (in
C  fortran sense ) pixel value, or 0.0 if it is the first pixel
C
C  Deals with case in an X row where contiguous pixels are invalid coming
C  in either from the left or right edges. In this case, pixels given
C  edge value of previous X row.
C
C     a j penny                  ral          1991 apr

      subroutine coprrr ( in, nx, ny, bs, bz, rinval, out )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of array
      integer	ny		!i: X size of array
      real      in(nx,ny)	!i: Input array
      real	bs		!i: Scale of input array
      real	bz		!i: Zero of invalid array
      real      rinval		!i: Input array invalid flag value
      real	out(nx,ny)	!o: Output array
C--
      real alast, aclast, aclaste, rv
      integer jx, jy, kk
Cbegin


      if ( ST_FAILED ) return

      alast = 0.0
      aclast = 0.0
      aclaste = 0.0
      do jy = 1, ny

         do jx = 1, nx
            rv = in(jx,jy)
            if ( rv.eq.rinval ) then
               if ( jx.eq.1 ) then
                  out(jx,jy) = aclast
                  alast = aclast
               else
                  out(jx,jy) = alast
               endif
            else
               rv = rv*bs + bz
               out(jx,jy) = rv
               alast = rv
               if ( jx.eq.1 ) aclast = alast
            endif
         enddo

         if ( in(nx,jy).eq.rinval ) then
            out(nx,jy) = aclaste
            kk = nx - 1
            do while ( kk.ge.1 )
               if ( in(kk,jy).eq.rinval ) then
                  out(kk,jy) = aclaste
                  kk = kk - 1
               else
                  kk = 0
               endif
            enddo
         else
            aclaste = rv
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPSSS -- Copy a int*2 array to another one, NO scaling, INVAL deal
C   Invalid replaced by 0 (or last actual pixel )
C
C  Deals with case in an X row where contiguous pixels are invalid coming
C  in either from the left or right edges. In this case, pixels given
C  edge value of previous X row.
C
C     a j penny                   ral               1991 apr

      subroutine copsss ( in, nx, ny, inval, out )

      implicit none
      include 'STARMAN_INC'

      integer	nx			!i: X size of arrays
      integer	ny			!i: Y size of arrays
      integer*2	in(nx,ny)		!i: Input array
      integer	inval			!i: Invalid value
      integer*2	out(nx,ny)		!i: Output array
C--
      integer jx, jy, iv, ilast, iclast, iclaste, kk
Cbegin


      if ( ST_FAILED ) return

      ilast = 0
      iclast = 0
      iclaste = 0
      do jy = 1, ny

         do jx = 1, nx
            iv = in(jx,jy)
            if ( iv.eq.inval ) then
               if ( jx.eq.1 ) then
                  out(jx,jy) = iclast
                  ilast = iclast
               else
                  out(jx,jy) = ilast
               endif
            else
               out(jx,jy) = iv
               ilast = iv
               if ( jx.eq.1 ) iclast = iv
            endif
         enddo

         if ( in(nx,jy).eq.inval ) then
            out(nx,jy) = iclaste
            kk = nx - 1
            do while ( kk.ge.1 )
               if ( in(kk,jy).eq.inval ) then
                  out(kk,jy) = iclaste
                  kk = kk - 1
               else
                  kk = 0
               endif
            enddo
         else
            iclaste = iv
         endif

      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPTIR -- Copy part of a real vector into a vertical part of a real array
C
C    a j penny                ral                 88-08-12

      subroutine coptir ( in, nxi, out, nxo, nyo, oxs, oys )

      implicit none
      include 'STARMAN_INC'

      integer	nxi		!i: X size of input vector
      integer	in(nxi)		!i: Input vector
      integer	nxo		!i: X size of output array
      integer	nyo		!i: Y size of output array
      real	out(nxo,nyo)	!i/o: Output array
      integer	oxs     	!i: X start of output array to be copied into
      integer	oys		!i: Y start of output array to be copied into
C--
      integer i, oy
Cbegin


      if ( ST_FAILED ) return

      if ( oxs.ge.1 .and. oxs.le.nxo ) then
         do i = 1, nxi
            oy = oys + i - 1
            if ( oy.ge.1 .and. oy.le.nyo ) out(oxs,oy) = in(i)
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPTRR -- Copy part of a real vector into a vertical part of a real array
C
C    a j penny                ral                 88-08-12

      subroutine coptrr ( in, nxi, out, nxo, nyo, oxs, oys )

      implicit none
      include 'STARMAN_INC'

      integer	nxi		!i: X size of input vector
      real	in(nxi)		!i: Input vector
      integer	nxo		!i: X size of output array
      integer	nyo		!i: Y size of output array
      real	out(nxo,nyo)	!i/o: Output array
      integer	oxs     	!i: X start of output array to be copied into
      integer	oys		!i: Y start of output array to be copied into
C--
      integer i, oy
Cbegin


      if ( ST_FAILED ) return

      if ( oxs.ge.1 .and. oxs.le.nxo ) then
         do i = 1, nxi
            oy = oys + i - 1
            if ( oy.ge.1 .and. oy.le.nyo ) out(oxs,oy) = in(i)
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPR1 -- Copy a single real value from a real array into a value
C
C    a j penny                 oxvad                88-08-14

      subroutine copr1 ( arr, nx, ny, ix, iy, rv )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of intput array
      integer	ny		!i: Y size of intput array
      real	arr(nx,ny)	!i: array
      integer	ix		!i: X position in array to get value from
      integer	iy		!i: Y position in array to get value from
      real	rv		!i: Value to put into
C--
Cbegin


      if ( ST_FAILED ) return

      rv = arr(ix,iy)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPS1 -- Copy a single short value from a short array into a value
C
C    a j penny                 oxvad                88-08-14

      subroutine cops1 ( iarr, nx, ny, ix, iy, iv )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of intput array
      integer	ny		!i: Y size of intput array
      integer*2	iarr(nx,ny)	!i: array
      integer	ix		!i: X position in array to get value from
      integer	iy		!i: Y position in array to get value from
      integer*2	iv		!i: Value to put into
C--
Cbegin


      if ( ST_FAILED ) return

      iv = iarr(ix,iy)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COP1R -- Copy a single real value into a real array
C
C    a j penny                 oxvad                88-08-14

      subroutine cop1r ( rv, arr, nx, ny, ix, iy )

      implicit none
      include 'STARMAN_INC'

      real	rv		!i: Value to put in
      integer	nx		!i: X size of output array
      integer	ny		!i: Y size of output array
      real	arr(nx,ny)	!i/o: output array
      integer	ix		!i: X position in array to put value in
      integer	iy		!i: Y position in array to put value in
C--
Cbegin


      if ( ST_FAILED ) return

      arr(ix,iy) = rv


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COP1S -- Copy a single short value into a short array
C
C    a j penny                 oxvad                88-08-14

      subroutine cop1s ( iv, iarr, nx, ny, ix, iy )

      implicit none
      include 'STARMAN_INC'

      integer*2 iv		!i: Value to put in
      integer	nx		!i: X size of output array
      integer	ny		!i: Y size of output array
      integer*2	iarr(nx,ny)	!i/o: output array
      integer	ix		!i: X position in array to put value in
      integer	iy		!i: Y position in array to put value in
C--
Cbegin


      if ( ST_FAILED ) return

      iarr(ix,iy) = iv


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPVSI -- Extract part of an integer*2 array to an integer array.
C  INVALID pixels are copied over. The copying can either add to the input
C  values or replace them. It deals with areas outside of input and output
C  areas.
C
C    a j penny                      stsci                     87-12-30

      subroutine copvsi ( kwhole, n, m, part, no, mo, kxs, kxe, kys,
     +                    kye, inval, ninval, kadd )

      implicit none
      include 'STARMAN_INC'

      integer n			!i: X size of input image
      integer m			!i: Y size of input image
      integer*2 kwhole(n,m)	!i: Input image
      integer no		!i: X size of output image
      integer mo		!i: Y size of output image
      integer part(no,mo)	!i/o: Output image
      integer kxs     		!i: The first column to be stored
      integer kxe     		!i: The last column to be stored
      integer kys     		!i: The first row to be stored
      integer kye     		!i: The last row to be stored.
      integer inval   		!i: The flag value for an INVALID pixel
      integer ninval		!o: The number of INVALID pixels found
      integer kadd	    	!i: The flag for adding (1) into PART
				!   or overwriting (0)
C--
      integer j, ja, k, ka, kv
Cbegin


      if ( ST_FAILED ) return

      ninval = 0
      ka = 1
      do k = kys, kye
         ja = 1
         do j = kxs, kxe
            if ( j.lt.1.or.j.gt.n .or. k.lt.1.or.k.gt.m ) then
               kv = inval
            else
               kv = kwhole(j,k)
            endif
            if ( kv.eq.inval ) then
               ninval = ninval + 1
            endif
            if ( ja.le.no .and. ka.le.mo ) then
               if ( kv.eq.inval ) then
                  part(ka,ka) = inval
               else
                  if ( kadd.eq.0 ) then
                     part(ja,ka) = kv
                  else
                     part(ja,ka) = part(ja,ka) + kv
                  endif
               endif
            endif
            ja = ja + 1
         enddo
         ka = ka + 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPVRR -- Extract part of a real array to a real array.
C  INVALID pixels are copied over. The copying can either add to the input
C  values or replace them. It deals with areas outside of input and output
C  areas.
C
C
C    a j penny                      stsci                     87-12-30

      subroutine copvrr ( whole, n, m, part, no, mo, kxs, kxe, kys,
     +                    kye, bs, bz, rinval, ninval, kadd )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: X size of input image
      integer	m		!i: Y size of input image
      real      whole(n,m)	!i: Input image
      integer	no		!i: X size of output image
      integer	mo		!i: Y size of output image
      real	part(no,mo)	!i/o: Output image
      integer	kxs     	!i: The first column to be stored
      integer   kxe    		!i: The last column to be stored
      integer   kys    		!i: The first row to be stored
      integer	kye     	!i: The last row to be stored.
      real	bs		!i: Image pixel value scale
      real	bz		!i: Image pixel value zero
      real	rinval   	!i: The flag value for an INVALID real pixel
      integer	ninval		!o: The number of INVALID pixels found
      integer	kadd    	!i: The flag for adding (1) into PART
				!   or overwriting (0)
C--
      integer j, ja, k, ka
      real rv
Cbegin


      if ( ST_FAILED ) return

      ninval = 0
      ka = 1
      do k = kys, kye
         ja = 1
         do j = kxs, kxe
            if ( j.lt.1.or.j.gt.n .or. k.lt.1.or.k.gt.m ) then
               rv = rinval
            else
               rv = whole(j,k)
            endif
            if ( rv.eq.rinval ) then
               ninval = ninval + 1
            endif
            if ( ja.le.no .and. ka.le.mo ) then
               if ( rv.eq.rinval ) then
                  part(ja,ka) = rinval
               else
                  if ( kadd.eq.0 ) then
                     part(ja,ka) = bs*rv + bz
                  else
                     part(ja,ka) = part(ja,ka) + bs*rv + bz
                  endif
               endif
            endif
            ja = ja + 1
         enddo
         ka = ka + 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPVSR -- Extract part of an integer*2 array to a real array.
C  INVALID pixels are copied over. The copying can either add to the input
C  values or replace them. It deals with areas outside of input and output
C  areas.
C
C
C    a j penny                      stsci                     87-12-30

      subroutine copvsr ( kwhole, n, m, part, no, mo, kxs, kxe, kys,
     +                    kye, bs, bz, inval, rinval, ninval, kadd )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: X size of input image
      integer	m		!i: Y size of input image
      integer*2	kwhole(n,m)	!i: Input image
      integer	no		!i: X size of output image
      integer	mo		!i: Y size of output image
      real	part(no,mo)	!i/o: Output image
      integer	kxs     	!i: The first column to be stored
      integer   kxe    		!i: The last column to be stored
      integer   kys    		!i: The first row to be stored
      integer	kye     	!i: The last row to be stored.
      real	bs		!i: Image pixel value scale
      real	bz		!i: Image pixel value zero
      integer	inval   	!i: The flag value for a INVALID int*2 pixel
      real	rinval   	!i: The flag value for an INVALID real pixel
      integer	ninval		!o: The number of INVALID pixels found
      integer	kadd    	!i: The flag for adding (1) into PART
				!   or overwriting (0)
C--
      integer j, ja, k, ka, kv
Cbegin


      if ( ST_FAILED ) return

      ninval = 0
      ka = 1
      do k = kys, kye
         ja = 1
         do j = kxs, kxe
            if ( j.lt.1.or.j.gt.n .or. k.lt.1.or.k.gt.m ) then
               kv = inval
            else
               kv = kwhole(j,k)
            endif
            if ( kv.eq.inval ) then
               ninval = ninval + 1
            endif
            if ( ja.le.no.and.ka.le.mo ) then
               if ( kv.eq.inval ) then
                  part(ja,ka) = rinval
               else
                  if ( kadd.eq.0 ) then
                     part(ja,ka) = bs*real(kv) + bz
                  else
                     part(ja,ka) = part(ja,ka) + bs*real(kv) + bz
                  endif
               endif
            endif
            ja = ja + 1
         enddo
         ka = ka + 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C COPVSS -- Extract part of an integer*2 array to an integer*2 array.
C  INVALID pixels are copied over. The copying can either add to the input
C  values or replace them. It deals with areas outside of input and output
C  areas.
C
C    a j penny                      stsci                     87-12-30

      subroutine copvss ( kwhole, n, m, part, no, mo, kxs, kxe,
     +                    kys, kye, inval, ninval, kadd )

      implicit none
      include 'STARMAN_INC'

      integer n			!i: X size of input image
      integer m			!i: Y size of input image
      integer*2 kwhole(n,m)	!i: Input image
      integer no		!i: X size of output image
      integer mo		!i: Y size of output image
      integer*2 part(no,mo)	!i/o: Output image
      integer	kxs     	!i: The first column to be stored
      integer   kxe    		!i: The last column to be stored
      integer   kys    		!i: The first row to be stored
      integer	kye     	!i: The last row to be stored.
      integer	inval   	!i: The flag value for an INVALID pixel
      integer	ninval		!o: The number of INVALID pixels found
      integer	kadd    	!i: The flag for adding (1) into PART
				!   or overwriting (0)
C--
      integer j, ja, k, ka, kv
Cbegin


      if ( ST_FAILED ) return

      ninval = 0
      ka = 1
      do k = kys, kye
         ja = 1
         do j = kxs, kxe
            if ( j.lt.1.or.j.gt.n .or. k.lt.1.or.k.gt.m ) then
               kv = inval
            else
               kv = kwhole(j,k)
            endif
            if ( kv.eq.inval ) then
               ninval = ninval + 1
            endif
            if ( ja.le.no .and. ka.le.mo ) then
               if ( kv.eq.inval ) then
                  part(ja,ka) = inval
               else
                  if ( kadd.eq.0 ) then
                     part(ja,ka) = kv
                  else
                     part(ja,ka) = part(ja,ka) + kv
                  endif
               endif
            endif
            ja = ja + 1
         enddo
         ka = ka + 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CSWOPI -- Swop two integers if second smaller than first
C
C   alan penny                 ral                   1988-12-2

      subroutine cswopi ( ia, ib )

      implicit none
      include 'STARMAN_INC'

      integer    ia      !i/o: 1st number
      integer    ib      !i/o: 2nd number
C--
      integer iv
Cbegin


      if ( ST_FAILED ) return

      if ( ib.lt.ia ) then
         iv = ia
         ia = ib
         ib = iv
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CSWOPR -- Swop two reals if second smaller than first
C
C   alan penny                 ral                   1988-12-2

      subroutine cswopr ( a, b )

      implicit none
      include 'STARMAN_INC'

      real      a      !i/o: 1st number
      real      b      !i/o: 2nd number
C--
      real rv
Cbegin


      if ( ST_FAILED ) return

      if ( b.lt.a ) then
         rv = a
         a  = b
         b  = rv
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DETERM -- (function) Calc the determinant of a square matrix
C
C     Destroys the input matrix array
C     Dimension valid for norder up to 10
C
C
C      a j penny             rgo                      83-2-16

      double precision function determ ( array, norder )

      implicit none

      double precision	array(10,10)	!i/o: Matrix
      integer		norder		!i: Order of det (degree of matrix)
C--
      double precision save
      integer i, j, k, k1
Cbegin


      determ = 1.0d0
      do k = 1, norder

C  Interchange columns if diagonal element is zero

         if ( array(k,k).eq.0.0d0 ) then
            j = k
            do while ( array(k,j).eq.0.0d0 .and. j.lt.norder )
               j = j + 1
            enddo
            if ( j.eq.norder ) then
               determ = 0.0d0
               return
            endif
            do i = k, norder
               save = array(i,j)
               array(i,j) = array(i,k)
               array(i,k) = save
            enddo
            determ = -1.0d0*determ
         endif

C   Subtract row k from lower rows to get diagonal matrix

         determ = determ*array(k,k)
         if ( k.lt.norder ) then
            k1 = k + 1
            do i = k1, norder
               do j = k1, norder
                 array(i,j) = array(i,j) -
     +                        array(i,k)*array(k,j)/array(k,k)
               enddo
            enddo
         endif

      enddo



      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FOURC -- Calc Fourier Transform
C  Takes complex data and calculates its fourier transform
C  The input data is stored as 'DATA(1),DATA(2),DATA(3),DATA(4)..'
C  as 'real part of 1st point, complex part of 1st point, real part
C  of 2nd point, complex part of 2nd point..'. The number of
C  data points must be a power of 2.
C
C     a j penny                 ral                   1990-1-2

      subroutine fourc ( data, nn, isign )

      implicit none

      integer	nn		!i: no of data points (must be power of 2)
      real	data(2*nn)	!i/o: data/fft (both in complex form)
      integer	isign		!i: 1=transform wanted;-1=inverse wanted
C--
      real*8 wr, wi, wpr, wpi, wtemp, theta, dtemp
      integer i, j, m, n, mmax, istep
      real tempr, tempi
Cbegin


      n = 2*nn
      j = 1
      do i = 1, n, 2
         if ( j.gt.i ) then
            tempr     = data(j)
            tempi     = data(j+1)
            data(j)   = data(i)
            data(j+1) = data(i+1)
            data(i)   = tempr
            data(i+1) = tempi
         endif
         m = n/2
         do while ( m.ge.2 .and. j.gt.m )
            j = j - m
            m = m/2
         enddo
         j = j + m
      enddo

      mmax = 2
      do while ( n.gt.mmax )
         istep = 2*mmax
         theta = 6.28318530717959d0/(isign*mmax)
         dtemp =  dsin(0.5d0*theta)
         wpr = -2.0d0*dtemp*dtemp
         wpi = dsin(theta)
         wr = 1.0d0
         wi = 0.0d0
         do m = 1, mmax, 2
            do i = m, n, istep
               j = i + mmax
               tempr = sngl(wr)*data(j)   - sngl(wi)*data(j+1)
               tempi = sngl(wr)*data(j+1) + sngl(wi)*data(j)
               data(j)   = data(i)   - tempr
               data(j+1) = data(i+1) - tempi
               data(i)   = data(i)   + tempr
               data(i+1) = data(i+1) + tempi
            enddo
            wtemp = wr
            wr = wr*wpr - wi*wpi + wr
            wi = wi*wpr + wtemp*wpi + wi
         enddo
         mmax = istep
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FOURN -- Calc Fourier Transform in N dimensions
C  Does a N-dimensional FFT. Calculates either the FFT, or its inverse
C  times the product of the lengths of all dimensions.
C
C   a j penny                 dao           1988-04-25

      subroutine fourn ( data, nn, ndim, isign )

      implicit none
      include 'STARMAN_INC'

      integer	ndim		!i: no of dimensions
      integer	nn(ndim)	!i: size of each dimension (power of 2)
      real	data(*)		!i/o: data/fft (both in complex form)
      integer	isign		!i: 1=transform wanted;-1=inverse wanted
C--
      real*8 wr, wi, wpr, wpi, wtemp, theta
      integer ntot, idim, n, nrem, nprev, ip1, ip2, ip3, i2rev,
     +        i3rev, i1, i2, i3, ibit, ifp1, ifp2, k1, k2
      real tempr, tempi
Cbegin


      if ( ST_FAILED ) return

      ntot = 1
      do idim = 1, ndim
         ntot = ntot*nn(idim)
      enddo

      nprev = 1
      do idim = 1, ndim

         n = nn(idim)
         nrem = ntot/(n*nprev)
         ip1 = 2*nprev
         ip2 = ip1*n
         ip3 = ip2*nrem
         i2rev = 1
         do i2 = 1, ip2, ip1

            if ( i2.lt.i2rev ) then
               do i1 = i2, i2+ip1-2, 2
                  do i3 = i1, ip3, ip2
                     i3rev = i2rev + i3 - i2
                     tempr = data(i3)
                     tempi = data(i3+1)
                     data(i3) = data(i3rev)
                     data(i3+1) = data(i3rev+1)
                     data(i3rev) = tempr
                     data(i3rev+1) = tempi
                  enddo
               enddo
            endif

            ibit = ip2/2
            do while ( (ibit.ge.ip1) .and. (i2rev.gt.ibit) )
               i2rev = i2rev - ibit
               ibit = ibit/2
            enddo
            i2rev = i2rev + ibit

         enddo

         ifp1 = ip1
         do while ( ifp1.lt.ip2 )
            ifp2 = 2*ifp1
            theta = isign*6.28318530717959d0/(ifp2/ip1)
            wpr = -2.0d0*dsin(0.5d0*theta)**2
            wpi = dsin(theta)
            wr = 1.0d0
            wi = 0.0d0
            do i3 = 1, ifp1, ip1
               do i1 = i3, i3+ip1-2, 2
                  do i2 = i1, ip3, ifp2
                     k1 = i2
                     k2 = k1 + ifp1
                     tempr = sngl(wr)*data(k2) - sngl(wi)*data(k2+1)
                     tempi = sngl(wr)*data(k2+1) + sngl(wi)*data(k2)
                     data(k2) = data(k1) - tempr
                     data(k2+1) = data(k1+1) - tempi
                     data(k1) = data(k1) + tempr
                     data(k1+1) = data(k1+1) + tempi
                  enddo
               enddo
               wtemp = wr
               wr = wr*wpr - wi*wpi + wr
               wi = wi*wpr + wtemp*wpi + wi
            enddo
            ifp1 = ifp2
         enddo

         nprev = n*nprev

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GASDEV -- Make normally (Gauss) distributed random number
C   zero mean, unit standard deviation. Uses call to uniform
C   distribution RANO(IDUM).
C
C   a j penny                ral         88-07-04

      subroutine gasdev ( gas, idum )

      implicit none
      include 'STARMAN_INC'

      real      gas             !o:   random number
      integer	idum		!i/o: random number seed
C--
      real rv, r, gset, v1, v2, fac
      integer iset
      data iset / 0 /
Cbegin


      if ( ST_FAILED ) return

      if ( iset.eq.0 ) then
         r = 2.0
         do while ( r.ge.1.0 )
            call rano ( rv, idum )
            v1 = 2.0*rv - 1.0
            call rano ( rv, idum )
            v2 = 2.0*rv - 1.0
            r = v1*v1 + v2*v2
         enddo
         if ( r.gt.1.0e-20 ) then
            fac = sqrt(-2.0*log(r)/r)
            gset = v1*fac
            gas = v2*fac
         else
            gset = 0.0
            gas = 0.0
         endif
         iset = 1
      else
         gas = gset
         iset = 0
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GAUSS1R -- Solve for a 1-D (modified) Gaussian for real data
C
C   The data are input as (y,x) pairs of (height,x distance), or
C   just as data, when the x positions are taken as 1,2,3,...
C   Any of the parameters can be fixed.
C
C       EQUATION:
C                                                   P
C                               -1.0*(abs((x-x0)/R))
C                I  =   B + HT*e
C
C  On entry the height must be non-negative, the radius greater than 0.25
C  and the power between 0.1 and 6.0.
C
C  If during the fit, the height, radius or power go beyond those limits,
C  the subroutine stops and the input values are returned, with the base
C  and height set to 0.0.
C
C     a j penny                stsci             1987-01-31

      subroutine gauss1r ( data, dist, nx, isdist, nxa, nxb, b, ht, r,
     +                    p, xo, jf, htlim, ftlim, itlim, damp, iter,
     +                    kprint )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: No of points
      real	data(nx)	!i: Data values
      real	dist(nx)	!i: X posns of data
      logical	isdist		!i: if true, DIST => x vals; if not, not
      integer	nxa		!i: Start analyzing data from this pixel
      integer	nxb		!i: End analyzing data at this pixel
      real	b		!i/o: Input/output estimate of base
      real	ht		!i/o: Input/output estimate of height
      real	r		!i/o: Input/output estimate of radius
      real	p		!i/o: Input/output estimate of power
      real	xo		!i/o: Input/output estimate of posn
      integer	jf(5)		!i: jf(n)=0 =>param n fixed; =+1 => var
				!   in order b, ht, r, p, xo
      real	htlim		!i: min fract diff in height to stop (0.001)
      real	ftlim		!i: min fract diff in other pars to stop (0.001)
      integer	itlim		!i: Max no of iterations allowed  (20 normal)
      real	damp		!i: Damping factor in iterations (0.5 normal)
      integer	iter		!o: No of iterations done
      integer   kprint		!i: Progress printinmg flag (0=no;1=yes)
C--
      real cmat(25), cvec(5), rvec(5), cc(5), ccs(5), cclast(5), erf(5),
     +     x, dr, dd, f, z, res, factor, diff, fdiff
      integer j, k, kk, jk, kj, n, npf, npfa, nc
      logical loop, again
      character*80 text
Cbegin


      if ( ST_FAILED ) return

      cc(1) = b								!Set up on entry
      cc(2) = ht								! Translate to CC from outside
      cc(3) = r
      cc(4) = p
      cc(5) = xo

      if ( cc(2).lt.0.0 .or. cc(3).lt.0.25 .or. 			!Do not do if input too strange
     +     cc(4).lt.0.1 .or. cc(4).gt.6.0 ) return

      call asumi ( jf, 5, f )						!No free parameters
      if ( nint(f).eq.0 ) return

      npfa = 5								!Max poss no of free parameters

      call amovr ( cc, ccs, npfa )					!Store input, initial paramters
      call amovr ( cc, cclast, npfa )

      iter = 0								!Start of iteration loop
      loop = .true.
      do while ( loop )

         call azeror ( cmat, npfa*npfa )				!Zero norm eqns
         call azeror ( cvec, npfa  )

         do n = nxa, nxb						!Loop through the data, setting
									! up the simultaenous equations
            x = n - cc(5)
            if ( isdist ) x = dist(n) - cc(5)
            if ( abs(x).lt.1.0e-6) x = sign(1.0e-6,x)
            dr = abs(min((x/cc(3)),1.0e8))
            dd = min(max(dr**cc(4),1.0e-6),1.0e6)
            f = exp(-1.0*dd)

            npf = 0
            if ( jf(1).eq.1 ) then
               npf = npf + 1
               erf(npf) = 1.0						!di/db
            endif
            if ( jf(2).eq.1 ) then
               npf = npf + 1
               erf(npf) = f						!di/dht
            endif
            if ( jf(3).eq.1 ) then
               npf = npf + 1
               erf(npf) = cc(4)*cc(2)*f*dd/cc(3)			!di/dr
            endif
            if ( jf(4).eq.1 ) then
               npf = npf + 1
               erf(npf) = -1.0*cc(2)*f*dd*alog(dr)			!di/dp
            endif
            if ( jf(5).eq.1 ) then
               npf = npf + 1
               erf(npf) = cc(4)*cc(2)*f*dd/x				!di/dxo
            endif

            z = cc(1) + cc(2)*f						!height of fit here

            res = data(n) - z						!residual from data
            if ( abs(res).lt.1e-30 ) res = 0.0

            if ( npf.ge.1 ) then					!Accumulate vector and matrix
               do k = 1, npf						! contributions at this pixel
                  if ( abs(erf(k)).lt.1.0e-5 ) erf(k) = 0.0
               enddo
               do k = 1, npf
                  cvec(k) = cvec(k) + erf(k)*res
                  do j = k, npf
                     jk =  (k-1)*npf + j
                     cmat(jk) = cmat(jk) + erf(j)*erf(k)
                  enddo
               enddo
            endif

         enddo

         if ( npf.gt.1 ) then
            do k = 2, npf						!Complete matrix
               do j = 1, k-1
                  jk = (k-1)*npf + j
                  kj = (j-1)*npf + k
                  cmat(jk) = cmat(kj)
               enddo
            enddo
         endif

         factor = 1.0 + damp*damp					!Apply damping factor
         do k = 1, npf
            kk = (k-1)*npf + k
            cmat(kk) = cmat(kk)*factor
         enddo

         call simulx ( cvec, cmat, rvec, npf )				!Solve the normal equations

         nc = 0								!Update the solution. check
         do k = 1, npfa							! for radius, power, posn
            if ( jf(k).eq.1 ) then					! changing too rapidly
               nc = nc + 1
               if ( k.eq.3 .or. k.eq.4 .or. k.eq.5 ) then
                  diff = rvec(nc)/cc(k)
                  if ( abs(diff).gt.0.1 ) rvec(nc) =
     +                                    sign((0.1*cc(k)),rvec(nc))
               endif
               cc(k) = cc(k) + rvec(nc)
            endif
         enddo
         cc(3) = abs(cc(3))

         again = .false.						!Compare new parameters with the old ones.
         do k = 1, npfa
            if ( abs(cclast(k)-cc(k)).gt.1.0e-6 ) then
               if ( abs(cclast(k)).lt.1.0e-10 ) cclast(k) =
     +                                       sign(1.0e-10,cclast(k))
               diff = min(abs(cc(k)-cclast(k)),1.0e10)
               fdiff = diff/cclast(k)
               if ( k.eq.2 .and. fdiff.gt.htlim   ) again = .true.
               if ( k.ne.2 .and. fdiff.gt.ftlim   ) again = .true.
               cclast(k) = cc(k)
            endif
         enddo

         if ( cc(2).lt.0.0 .or. cc(3).lt.0.25 .or. 			!Fit too strange?
     +        cc(4).lt.0.1 .or. cc(4).gt.30.0 ) again = .false.

         iter = iter + 1

         if ( kprint.eq.1 ) then					!Print out progress?
            write ( text, '(''  Iter '',i5, ''  Base '', g12.2,
     +                      '' Ht '',g12.2, '' Rad '',g12.3)' )
     +                      iter, cc(1), cc(2), cc(3)
            call printo ( text )
            write ( text, '(12x, ''  Pow '',g12.3, '' Posn '',g12.3)' )
     +                     cc(4), cc(5)
            call printo ( text )
         endif

         if ( .not.again .or. iter.ge.itlim ) loop = .false.		!Loop again unless settled, or
									! bad profile, or too many loops
      enddo

      if ( cc(2).lt.0.0 .or. cc(3).lt.0.25 .or. 			!Reset result if fit too strange
     +        cc(4).lt.0.1 .or. cc(4).gt.30.0 ) then
         cc(1) = 0.0
         cc(2) = 0.0
         cc(3) = r
         cc(4) = p
         cc(5) = xo
      endif

      b  = cc(1)							!Translate back from CC to outside
      ht = cc(2)
      r  = cc(3)
      p  = cc(4)
      xo = cc(5)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GAUSS2RA -- Fit a 2-D gaussian to a real array and bad pixels
C  This takes a sub-area from an array, and fits a single
C  gaussian to the data, assuming a single star at the centre of
C  the sub-area.
C  It deals with bad pixels by assuming that they have the value
C  of the neighbouring one. Maximum size of sub-area is 100x100.
C
C          The profile fitted is :-
C
C                                    2
C                                -1.d
C                   I  = HEIGHT.e      + BASE
C
C          where d = sqrt(((x-XO)/RX)**2+((y-YO)/RY)**2)
C
C It assumes a single star near the centre and either using fixed
C orthogonal radii, or floating ones with a first estimate of
C boxsize/6, the s/r fits the data by means of an iterative
C linearised fully 2-d approximation method using least squares.
C
C      The Output mag is = 30 - 2.5*log  (HEIGHT.RX.RY)
C                                      10
C
C      But in the event of failure it is set to:-
C                 50   Fitted height less than 0.00001
C                 60   Star too near edge (all of box must be in image)
C                 61   Box size greater than 70
C                 70   No valid pixels in box
C
C
C   alan penny                        ral          1990-05-22

      subroutine gauss2ra ( pt, kx, ky, ax, ay, nx, ny, kw, orix,
     +                      oriy, rinval, itlim, amag, height, base,
     +                      dxo, dyo, anx, any, rx, ry, rms, iter,
     +                      ninval )

      implicit none
      include 'STARMAN_INC'

      integer   kx		!i: X length of the image
      integer   ky		!i: Y length of image
      real      pt(kx,ky)	!i: The input image
      real      ax		!i: X position of star in image
      real      ay		!i: Y position of star in image
      integer   nx		!i: X size of box round star
      integer   ny		!i: Y size of box round star
      integer   kw		!i: Flag- radii fixed/to be found (1/0)
      real      orix		!i: Value of X radius (if fixed to be used)
      real      oriy		!i: Value of Y radius (if fixed to be used)
      real      rinval		!i: Flag value of a pixel if Invalid
      integer   itlim		!i: Limit of no of iterations to try
      real      amag		!o: Output magnitude (may be false if failed)
      real	height		!o: Fitted Gaussian height
      real	base		!o: Fitted Gaussian base
      real	dxo		!o: X diff of fitted posn from input posn
      real	dyo		!o: Y diff of fitted posn from input posn
      real	anx		!o: X fitted position
      real	any		!o: Y fitted position
      real	rx		!o: X fitted radius (=input if fixed)
      real	ry		!o: Y fitted radius (=input if fixed)
      real	rms		!o: RMS error between fit and data in box
      integer   iter		!o: No of iterations done
      integer   ninval		!o: No of Invalid pixels in box
C--
      real data(100*100), xo, yo, rlast, rd
      integer jx, jy, js, je, ks, ke, j, ja, jj, k, ka
      logical found
Cbegin


      if ( ST_FAILED ) return

      jx = int(ax)							!Get x,y coord at star
      jy = int(ay)

      amag = 0.0							!Set up default output values
      height = 0.001
      base = 0.001
      dxo = 0.0
      dyo = 0.0
      anx = ax
      any = ay
      rms = 0.0
      iter = 20
      ninval = 0

      js = jx - nx/2							!Check if box lies totally in image
      je = js + nx - 1
      ks = jy - ny/2
      ke = ks + ny - 1
      if ( js.lt.1 .or. je.gt.kx .or. ks.lt.1 .or. ke.gt.ky ) then
         amag = 60.0							!Return if not
         return
      endif

      if ( nx.lt.2 .or. nx.gt.100 .or. ny.lt.2 .or. ny.gt.100 ) then	!Check if box size ok
         amag = 61.0							!Return if not
         return
      endif

									!Extract data.
									!If any Invalid pixels, set to pixel before

      js = js - 1							!Find first valid value
      ks = ks - 1
      found = .false.
      do k = 1, ny
         ka = ks + k
         do j = 1, nx
            ja = js + j
            rd = pt(ja,ka)
            if ( .not.found .and. rd.ne.rinval ) then
               rlast = rd
               found = .true.
            endif
         enddo
      enddo
      if ( .not.found ) then
         amag = 70.0
         return
      endif

      ninval = 0							!Move data over to real
      do k = 1, ny							!Replacing bad points
         ka = ks + k
         jj = (k-1)*nx
         do j = 1, nx
            ja = js + j
            rd = pt(ja,ka)
            if ( rd.eq.rinval ) then
               rd = rlast
               ninval = ninval + 1
            endif
            rlast = rd
            data(jj+j) = rd
         enddo
      enddo

      call gauss2r ( data, nx, ny, kw, orix, oriy, itlim, amag,		!Solve for Gaussian
     +               height, base, xo, yo, rx, ry, rms, iter )

      anx = xo + js 							!Calc position in main image
      any = yo + ks
      dxo = anx - ax
      dyo = any - ay


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GAUSS2SA -- Fit a 2-D gaussian to an int*2 array and bad pixels
C  This takes a sub-area from an array, and fits a single
C  gaussian to the data, assuming a single star at the centre of
C  the sub-area.
C  It deals with bad pixels by assuming that they have the value
C  of the neighbouring one. Maximum size of sub-area is 100x100.
C
C          The profile fitted is :-
C
C                                    2
C                                -1.d
C                   I  = HEIGHT.e      + BASE
C
C          where d = sqrt(((x-XO)/RX)**2+((y-YO)/RY)**2)
C
C It assumes a single star near the centre and either using fixed
C orthogonal radii, or floating ones with a first estimate of
C boxsize/6, the s/r fits the data by means of an iterative
C linearised fully 2-d approximation method using least squares.
C
C      The Output mag is = 30 - 2.5*log  (HEIGHT.RX.RY)
C                                      10
C
C      But in the event of failure it is set to:-
C                 50   Fitted height less than 0.00001
C                 60   Star too near edge (all of box must be in image)
C                 61   Box size greater than 70
C                 70   No valid pixels in box
C
C
C   alan penny                        ral          1990-05-22

      subroutine gauss2sa ( kpt, kx, ky, ax, ay, nx, ny, kw, orix,
     +                      oriy, inval, itlim, amag, height, base,
     +                      dxo, dyo, anx, any, rx, ry, rms, iter,
     +                      ninval )

      implicit none
      include 'STARMAN_INC'

      integer   kx		!i: X length of the image
      integer   ky		!i: Y length of image
      integer*2 kpt(kx,ky)	!i: The input image
      real      ax		!i: X position of star in image
      real      ay		!i: Y position of star in image
      integer   nx		!i: X size of box round star
      integer   ny		!i: Y size of box round star
      integer   kw		!i: Flag- radii fixed/to be found (1/0)
      real      orix		!i: Value of X radius (if fixed to be used)
      real      oriy		!i: Value of Y radius (if fixed to be used)
      integer   inval		!i: Flag value of a pixel if Invalid
      integer   itlim		!i: Limit of no of iterations to try
      real      amag		!o: Output magnitude (may be false if failed)
      real	height		!o: Fitted Gaussian height
      real	base		!o: Fitted Gaussian base
      real	dxo		!o: X diff of fitted posn from input posn
      real	dyo		!o: Y diff of fitted posn from input posn
      real	anx		!o: X fitted position
      real	any		!o: Y fitted position
      real	rx		!o: X fitted radius (=input if fixed)
      real	ry		!o: Y fitted radius (=input if fixed)
      real	rms		!o: RMS error between fit and data in box
      integer   iter		!o: No of iterations done
      integer   ninval		!o: No of Invalid pixels in box
C--
      real data(100*100), xo, yo
      integer jx, jy, js, je, ks, ke, j, ja, jj, k, ka, kd, last
      logical found
Cbegin


      if ( ST_FAILED ) return

      jx = int(ax)							!Get x,y coord at star
      jy = int(ay)

      amag = 0.0							!Set up default output values
      height = 0.001
      base = 0.001
      dxo = 0.0
      dyo = 0.0
      anx = ax
      any = ay
      rms = 0.0
      iter = 20
      ninval = 0

      js = jx - nx/2							!Check if box lies totally in image
      je = js + nx - 1
      ks = jy - ny/2
      ke = ks + ny - 1
      if ( js.lt.1 .or. je.gt.kx .or. ks.lt.1 .or. ke.gt.ky ) then
         amag = 60.0							!Return if not
         return
      endif

      if ( nx.lt.2 .or. nx.gt.100 .or. ny.lt.2 .or. ny.gt.100 ) then	!Check if box size ok
         amag = 61.0							!Return if not
         return
      endif

									!Extract data.
									!If any Invalid pixels, set to pixel before

      js = js - 1							!Find first valid value
      ks = ks - 1
      found = .false.
      do k = 1, ny
         ka = ks + k
         do j = 1, nx
            ja = js + j
            kd = kpt(ja,ka)
            if ( .not.found .and. kd.ne.inval ) then
               last = kd
               found = .true.
            endif
         enddo
      enddo
      if ( .not.found ) then
         amag = 70.0
         return
      endif

      ninval = 0							!Move data over to real
      do k = 1, ny							!Replacing bad points
         ka = ks + k
         jj = (k-1)*nx
         do j = 1, nx
            ja = js + j
            kd = kpt(ja,ka)
            if ( kd.eq.inval ) then
               kd = last
               ninval = ninval + 1
            endif
            last = kd
            data(jj+j) = kd
         enddo
      enddo

      call gauss2r ( data, nx, ny, kw, orix, oriy, itlim, amag,		!Solve for Gaussian
     +               height, base, xo, yo, rx, ry, rms, iter )

      anx = xo + js 							!Calc position in main image
      any = yo + ks
      dxo = anx - ax
      dyo = any - ay


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GAUSS2R -- Fit a 2-D Gaussian (fast) to real array with centered star
C It assumes a single star near the centre of the data and either using
C fixed orthogonal radii, or floating ones with a first estimate of
C boxsize/6.0, the s/r fits the data by means of an iterative linearised
C fully 2-d approximation method using least squares.
C
C          The profile fitted is :-
C
C                                2
C                            -1.d
C                I = HEIGHT.e     + BASE
C
C          where d = sqrt(((x-XO)/RX)**2+((y-YO)/RY)**2)
C
C
C          Maximum size of array = 100
C
C
C      The Output mag is = 30 - 2.5*log  (HEIGHT.RX.RY)
C                                      10
C
C      But in the event of failure it is set to:-
C                MAG        Reason
C                 50   Fitted height less than 0.00001
C
C  alan penny                ral             1990-05-22

      subroutine gauss2r ( apt, nx, ny, kw, orix, oriy, itlim, amag,
     +                     height, base, xo, yo, rx, ry, rms, iter )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: X size of the array
      integer	ny		!i: Y size of the array
      real	apt(nx,ny)	!i: Data array
      integer	kw		!i: Flag for radii (1=fixed;0=variable)
      real	orix		!i: Value of X radius (if fixed to be used)
      real	oriy		!i: Value of Y radius (if fixed to be used)
      integer	itlim		!i: Max no of iterations to do
      real	amag		!o: Output magnitude (may be false if failed)
      real	height		!o: Fitted Gaussian height
      real	base		!o: Fitted Gaussian base
      real	xo		!o: X fitted position
      real	yo		!o: Y fitted position
      real	rx		!o: X fitted radius (=input if fixed)
      real	ry		!o: Y fitted radius (=input if fixed)
      real	rms		!o: RMS error between fit and data in box
      integer	iter		!o: No of iterations done
C--
      real ise(6), isf(6), isg(6)
      data ise/6,1,2,4,3,5/,isf/4,1,2,1,3,1/,isg/4,1,1,2,1,3/
      equivalence ( d(1,1), dd(1) )

      real s(7), d(6,6), dd(36), e(6), xe(6), f(100,4), g(100,4), ra(3),
     +     rb(3), sf(4,4), sg(4,4), pr, sp, a, b
      integer ne, nf, ng, itera, j, k, jk, ks, kst, ksu, ksv, ksx
Cbegin


      if ( ST_FAILED ) return

      if ( nx.gt.100 .or. ny.gt.100 ) then				!Check size
         call printo ( 'ERROR: Array too large for s/r GAUSS2R' )
         return
      endif

      xo = real(nx)/2.0 + 0.5						!Assume star is at centre of data
      yo = real(ny)/2.0 + 0.5

      if ( kw.eq.0 ) then						!Set to fixed or floating radii
         ne = 6
         nf = 3
         ng = 3
         rx = real(nx)/6.0
         ry = real(ny)/6.0
      else
         ne = 4
         nf = 2
         ng = 2
         rx = orix
         ry = oriy
      endif

      amag = 0.0							!Set up default output values
      height = 0.001
      base = 0.001
      rms = 0.0

      call asumr ( apt, nx*ny, sp )					!Get sum of data

      itera = 0								!Iterate until solved or too many iterations
      iter = 0
      do while ( iter.lt.itlim .and. itera.eq.0 )

         call gauss_pr ( nx, rx, xo, f, sf, ra, nf )			!Set up the simultaneous equations
         call gauss_pr ( ny, ry, yo, g, sg, rb, ng )
         s(6) = sp
         do j = 1, nx
            call prodcr ( apt, nx, ny, j, g, 100, 4, 1, 1, ny, pr )
            f(j,4) = pr
         enddo
         do j = 1, ny
            call prodrr ( apt, nx, ny, j, f, 100, 4, 1, 1, nx, pr )
            g(j,4) = pr
         enddo
         do j = 1, nf
            call prodrr ( f, 100, 4, 4, f, 100, 4, j, 1, nx, pr )
            s(j) = ra(j)*pr
         enddo
         do j = 2, ng
            call prodrr ( g, 100, 4, 4, g, 100, 4, j, 1, ny, pr )
            s(j+2) = rb(j)*pr
         enddo
         do k = 1, ne
            do j = k, ne
               ks =  isf(k)
               kst = isf(j)
               ksu = isg(k)
               ksv = isg(j)
               d(j,k) = sf(ks,kst)*sg(ksu,ksv)
               d(k,j) = d(j,k)
            enddo
            ksx = ise(k)
            e(k) = s(ksx)
         enddo
         if ( kw.eq.1 ) then
            do k = 2, ne
               jk = (k-1)*ne
               do j = 1, ne
                  dd(jk+j) = d(j,k)
               enddo
            enddo
         endif

         call simulx ( e, d, xe, ne )					!Solve the simultaneous equations

         call gauss_cn ( xe, a, b, xo, yo, rx, ry, itera, nx, ny, kw )	!Update the estimates as a result of this iteration and
									! decide if converged
         iter = iter + 1
      enddo

      rms = 0.0								!Calc the RMS
      do k = 1, ny
         do j = 1, nx
            rms = rms + abs(apt(j,k)-(a*f(j,1)*g(k,1)+b))**2.0
         enddo
      enddo
      rms = sqrt(rms/real(nx*ny-1))

      amag = 50.0							!Calc the magnitude
      if ( a.gt.1.0e-5 ) amag = 30.0 - 2.5*alog10(a*rx*ry)
      height = a
      base = b

      if ( a.le.1.0e-5 .or. rx.gt.500.0 .or. ry.gt.500.0 ) then		!Check for bad fit
         if ( kw.eq.0 ) then
            rx = 0.0
            ry = 0.0
         endif
         base = 0.0
         height = 0.0
         rms = 0.0
         amag = 50.0
         xo = real(nx)/2.0 + 0.5
         yo = real(ny)/2.0 + 0.5
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GAUSS_CN -- Update Gauss estimates and see if converged
C  It makes new estimates of the height and base and corrects the
C  position and radii. Does not allow centre to go too near edge.
C
C  alan penny                ral                      1990-05-22

      subroutine gauss_cn ( e, a, b, xo, yo, rx, ry, itera, nx, ny, kw )

      implicit none
      include 'STARMAN_INC'

      real	e(6)		!i: Update factors
      real	a		!o: Gaussian height
      real	b		!o: Gaussian base
      real	xo		!i/o: X posn of Gauss centre
      real	yo		!i/o: Y posn of Gauss centre
      real	rx		!i/o: X Gauss radius
      real	ry		!i/o: Y Gauss radius
      integer	itera		!o: Convergence flag (no=0;yes=1)
      integer   nx		!i: Array X size
      integer   ny		!i: Array Y size
      integer   kw		!i: Flag for keeping radii fixed/var (1/0)
C--
      real rxo, ryo, hrx, hry, dx, dy, drx, dry
Cbegin


      if ( ST_FAILED ) return

      rxo = abs(rx)
      ryo = abs(ry)
      hrx = rxo/2.0
      hry = ryo/2.0
      a = e(2)
      if ( abs(a).lt.1.0e-6 ) a = sign(1.0e-6,a)
      dx = e(3)/a
      dy = e(4)/a
      drx = 0.0
      dry = 0.0
      if ( a.gt.0.0 ) then
         if ( kw.eq.0 ) drx = e(5)/a
         if ( kw.eq.0 ) dry = e(6)/a
      else
         dx = -1.0*dx
         dy = -1.0*dy
      endif
      b = e(1)

      if ( abs(dx).gt.hrx ) dx = sign(hrx,dx)
      if ( abs(dy).gt.hry ) dy = sign(hry,dy)
      xo = xo + dx
      xo = max(3.0,xo)
      xo = min(real(nx-2),xo)
      yo = yo + dy
      yo = max(3.0,yo)
      yo = min(real(ny-2),yo)
      rx = rxo + drx
      ry = ryo + dry
      rx = max(0.3,rx)
      ry = max(0.3,ry)

      itera = 1
      if ( abs(dx).gt.0.1 .or. abs(dy).gt.0.1 .or. abs(drx).gt.0.005
     +   .or. abs(dry).gt.0.005 ) itera = 0


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GAUSS_PR -- Sets up terms for use in GAUSS2(R)
C
C  alan penny                   ral                  1990-05-022

      subroutine gauss_pr ( n, r, xo, f, sf, ra, nf )

      implicit none
      include 'STARMAN_INC'

      integer   n		!i: No of points wanted
      real	r		!i: Gaussian radius
      real	xo		!i: Posn of Gaussian centre
      real      f(100,4)	!o:
      real      sf(4,4)		!o:
      real      ra(3)		!o:
      integer	nf		!i: Flag (fixed radii=2;variable=3)
C--
      real rr, z, zr, w, xj, s
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      if ( n.gt.100 ) then
          call printo ( 'ERROR: Array too large in GAUSS_PR' )
          return
      endif

      rr = 1.0/r
      ra(1) = 1.0
      ra(2) = 2.0*rr*rr
      ra(3) = ra(2)*rr

      do j = 1, n
         xj = j
         z = xj - xo
         zr = z*rr
         w = 0.0
         if ( abs(zr).lt.6.0 ) w = exp(-1.0*zr*zr)
         f(j,1) = w
         w = z*w
         f(j,2) = w
         f(j,3) = z*w
      enddo

      do k = 1, nf
         do j = k, nf
            call prodrr ( f, 100, 4, k, f, 100, 4, j, 1, n, s )
            sf(k,j) = s*ra(k)*ra(j)
            sf(j,k) = sf(k,j)
         enddo
         call asumr ( f(1,k), n, s )
         sf(4,k) = s*ra(k)
         sf(k,4) = s*ra(k)
      enddo

      sf(4,4) = n

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GPROB -- Calc prob of a one-tailed Normal (Gauss) distribution
C             If input value >4.3; prob = 0.0
C             If input value <1.28 prob = 0.1003
C             In between, a good approximation
C
C   a j penny                 dao           1988-04-25

      subroutine gprob ( rv, prob )

      implicit none
      include 'STARMAN_INC'

      real	rv		!i: Value
      real	prob		!o: Probability
C--
      real av
      integer j, kv
      real cubintx
      external cubintx

      real	f(17), g(176)
      data	f / .0025600, .0018658, .0013499, .0009676, .0006871,
     +              .0004834, .0003369, .0002326, .0001591, .0001078,
     +              .0000723, .0000481, .0000317, .0000207, .0000133,
     +              .000082,  .000055 /
      data (g(j),j=1,60)/ .1038, .1020, .1003, .0985, .0968, .0951,
     +              .0934, .0918, .0901, .0885, .0869, .0853,
     +              .0838, .0823, .0808, .0793, .0778, .0764,
     +              .0749, .0735, .0721, .0708, .0694, .0681,
     +              .0668, .0655, .0643, .0630, .0618, .0606,
     +              .0594, .0582, .0571, .0559, .0548, .0537,
     +              .0526, .0516, .0505, .0495, .0485, .0475,
     +              .0465, .0455, .0446, .0436, .0427, .0418,
     +              .0409, .0401, .0392, .0384, .0375, .0367,
     +              .0359, .0351, .0344, .0336, .0329, .0322 /
      data (g(j),j=61,74)/ .0314, .0307, .0301, .0294, .0287, .0281,
     +              .0274, .0268, .0262, .0256, .0250, .0244,
     +              .0239, .0233 /
      data (g(j),j=75,124)/ .02275, .02222, .02169, .02118, .02068,
     +              .02018, .01970, .01923, .01876, .01831,
     +              .01786, .01743, .01700, .01659, .01618,
     +              .01578, .01539, .01500, .01463, .01426,
     +              .01390, .01355, .01321, .01287, .01255,
     +              .01222, .01191, .01160, .01130, .01101,
     +              .01072, .01044, .01017, .00990, .00964,
     +              .00939, .00914, .00889, .00866, .00842,
     +              .00820, .00798, .00776, .00755, .00734,
     +              .00714, .00695, .00676, .00657, .00639 /
      data (g(j),j=125,174)/ .00621, .00604, .00587, .00570, .00554,
     +              .00539, .00523, .00508, .00494, .00480,
     +              .00466, .00453, .00440, .00427, .00415,
     +              .00402, .00391, .00379, .00368, .00357,
     +              .00347, .00336, .00326, .00317, .00307,
     +              .00298, .00289, .00280, .00272, .00264,
     +              .00256, .00248, .00240, .00233, .00226,
     +              .00219, .00212, .00205, .00199, .00193,
     +              .00187, .00181, .00175, .00169, .00164,
     +              .00159, .00154, .00149, .00144, .00139 /
      data (g(j),j=175,176)/ .00134, .00129 /
Cbegin


      if ( ST_FAILED ) return

      if ( rv.gt.4.3 ) then
         prob = 0.0
      elseif ( rv.ge.3.0 ) then
         av = (rv-2.7)*10.0
         kv = av
         av = av - kv
         prob = cubintx ( f(kv), av )
      elseif ( rv.ge.1.28 ) then
         av = (rv-1.25)*100.0
         kv = av
         av = av - kv
         prob = cubintx ( g(kv), av )
      else
         prob = 0.1003
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C HGRAMR -- Load histogram from values in a real array
C
C a j penny                         dao           1988-04-19

      subroutine hgramr ( rdata, kx, ky, lxa, lxb, lya, lyb, rmin,
     +                    kgram, numbin, rbin, rinval )

      implicit none
      include 'STARMAN_INC'

      integer kx		!i: X size of array
      integer ky		!i: Y size of array
      real    rdata(kx,ky)	!i: Array
      integer lxa		!i: X start of array area to use
      integer lya		!i: Y start of array area to use
      integer lxb		!i: X end of array area to use
      integer lyb		!i: Y end start of array area to use
      real    rmin		!i: Pixel value of histogram 1st bin
      integer numbin		!i: Number of bins in histogram
      integer kgram(numbin)	!o: Histogram
      real    rbin		!i: Pixel value step per bin
      real    rinval		!i: INVAL bad pixel flag value
C--
      integer j, k, l
Cbegin


      if ( ST_FAILED ) return

      call azeroi ( kgram, numbin )

      do k = lya, lyb
         do j = lxa, lxb
            if ( rdata(j,k).ne.rinval ) then
               l = int((rdata(j,k)-rmin)/rbin) + 1
               if ( l.ge.1 .and. l.le.numbin ) kgram(l) = kgram(l) + 1
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C HGRAMS -- Load histogram from values in an integer*2 array
C
C a j penny                         dao           1988-04-19

      subroutine hgrams ( kdata, kx, ky, lxa, lxb, lya, lyb, kmin,
     +                    kgram, numbin, kbin, inval )

      implicit none
      include 'STARMAN_INC'

      integer kx		!i: X size of array
      integer ky		!i: Y size of array
      integer*2 kdata(kx,ky)	!i: Array
      integer lxa		!i: X start of array area to use
      integer lya		!i: Y start of array area to use
      integer lxb		!i: X end of array area to use
      integer lyb		!i: Y end start of array area to use
      integer kmin		!i: Pixel value of histogram 1st bin
      integer numbin		!i: Number of bins in histogram
      integer kgram(numbin)	!o: Histogram
      integer kbin		!i: Pixel value step per bin
      integer inval		!i: INVAL bad pixel flag value
C--
      integer j, k, l
Cbegin


      if ( ST_FAILED ) return

      call azeroi ( kgram, numbin )

      do k = lya, lyb
         do j = lxa, lxb
            if ( kdata(j,k).ne.inval ) then
               l = (kdata(j,k)-kmin)/kbin + 1
               if ( l.ge.1 .and. l.le.numbin ) kgram(l) = kgram(l) + 1
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C LINFIT -- Take N  X,Y points and fits a straight line to them.
C       Y = ZERO + SL.X
C
C   a.j.penny                   rgo                    83-8-14


      subroutine linfit ( x, y, n, sl, zero, dx, dy, dsd, chi, rms )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: no of points
      real	x(n)		!i: X value of points
      real	y(n)		!i: Y value of points
      real	sl		!o: Slope of line fit
      real	zero		!o: zero of line fit
      double precision dx(n)	!o: Work space 1
      double precision dy(n)	!o: Work space 2
      double precision dsd(n)	!o: Work space 3
      real	chi		!o: Chi-sq of fit
      real	rms		!o: RMS of Y data from fit
C--
      double precision da(3), dchi, drms
      real xmax, ymax, xmin, ymin, xr, yr
      integer k
Cbegin


      if ( ST_FAILED ) return

C Scale data

      xmax = x(1)
      ymax = y(1)
      xmin = x(1)
      ymin = y(1)
      do k = 2, n
         xmax = max(xmax,x(k))
         ymax = max(ymax,y(k))
         xmin = min(xmin,x(k))
         ymin = min(ymin,y(k))
      enddo
      xr = xmax - xmin
      yr = ymax - ymin
      do k = 1, n
         dx(k) = dble((x(k)-xmin)/xr)
         dy(k) = dble((y(k)-ymin)/yr)
         dsd(k) = 1.0d0
      enddo

C  Do the fit

      call polfit ( dx, dy, dsd, n, 3, 0, da, dchi, drms )

C Rescale answer back

      sl   = da(2)*yr/xr
      zero = da(1)*yr + ymin - da(2)*yr*xmin/xr
      rms  = drms*yr
      chi  = dchi


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C LINFITA -- Find mean and slope of a vector (simple and fast)
C  This takes the vector as values of Y, and assumes X values of
C  1,2,3,... It then calculates the mean of the Y values and
C  the slope of a line Y = a.X + b
C
C alan penny            ral               nov 89

      subroutine linfita ( gx, n, am, sl )

      implicit none
      include 'STARMAN_INC'

      integer n			!i: Number of input points
      real    gx(n)		!i: Input data
      real    am		!o: Mean
      real    sl		!o: Slope
C--
      real e, ak, g, rh
      integer j
Cbegin


      if ( ST_FAILED ) return

      call asumr ( gx, n, e )				!Mean
      am = e/real(n)

      ak = 1 + real(n-1)/2.0				!Slope
      g = 0.0
      do j = 1, n
         g = g + (real(j)-ak)*(gx(j)-am)
      enddo
      rh = real(n)/2.0
      sl = g/(2.0*rh*(rh+1.0)*(2.0*rh+1.0)/6.0)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C LUDECMAT -- Decompose matrix by LU decomposition
C
C    alan penny             ral          1990 apr 14

      subroutine ludecmat ( a, n, np, indx, d, ierr )

      implicit none
      include 'STARMAN_INC'

      integer   np		!i:   Size of matrix
      real      a(np,np)	!i/o: Matrix and solution
      integer   n		!i:   Size of matrix contents
      integer   indx(n)		!o:   Row permutation
      real      d		!o:   =+1 if even no of interchanges,
				!     =-1 if odd number
      integer   ierr		!o:   Error flag 0=ok;1=bad
C--
      real  vv(100), aamax, sum, dum, tiny
      integer i, j, k, imax
      parameter ( tiny=1.0e-18 )
Cbegin


      if ( ST_FAILED ) then
         ierr = 1
         return
      endif

      ierr = 0

      d = 1.0
      do i = 1, n
         aamax = 0.0
         do j = 1, n
            if ( abs(a(i,j)).gt.aamax ) aamax = abs(a(i,j))
         enddo
         if ( abs(aamax).lt.tiny ) then
            ierr = 1
            return
         endif
         vv(i) = 1.0/aamax
      enddo

      do j = 1, n

         if ( j.gt.1 ) then
            do i = 1, j-1
               sum = a(i,j)
               if ( i.gt.1 ) then
                  do k = 1, i-1
                     sum = sum - a(i,k)*a(k,j)
                  enddo
                  a(i,j) = sum
               endif
            enddo
        endif

        aamax = 0.0
        do i = j, n
           sum = a(i,j)
           if ( j.gt.1 ) then
              do k = 1, j-1
                 sum = sum - a(i,k)*a(k,j)
              enddo
              a(i,j) = sum
           endif
           dum = vv(i)*abs(sum)
           if ( dum.ge.aamax ) then
              imax = i
              aamax = dum
           endif
        enddo

        if ( j.ne.imax ) then
           do k = 1, n
              dum = a(imax,k)
              a(imax,k) = a(j,k)
              a(j,k) = dum
           enddo
           d = -d
           vv(imax) = vv(j)
        endif

        indx(j) = imax
        if ( j.ne.n ) then
           if ( a(j,j).eq.0.0 ) then
              a(j,j) = tiny
           else
              if ( abs(a(j,j)).lt.tiny ) a(j,j) = sign(tiny,a(j,j))
           endif
           dum = 1.0/a(j,j)
           do i = j+1, n
              a(i,j) = a(i,j)*dum
           enddo
        endif

      enddo

      if ( a(n,n).eq.0.0 ) then
         a(n,n) = tiny
      else
         if ( abs(a(n,n)).lt.tiny ) a(n,n) = sign(tiny,a(n,n))
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C LUSOLMAT -- Solve a matrix by LU decompsoition
C
C  alan penny                 ral              1990 Jan

      subroutine lusolmat ( a, n, np, indx, b )

      implicit none
      include 'STARMAN_INC'

      integer  np
      real     a(np,np)
      integer  n
      integer  indx(n)
      real     b(n)
C--
      integer i, j, ii, ll
      real sum
Cbegin


      if ( ST_FAILED ) return

      ii = 0
      do i = 1, n
         ll = indx(i)
         sum = b(ll)
         b(ll) = b(i)
         if ( ii.ne.0 ) then
            do j = ii, i-1
               sum = sum - a(i,j)*b(j)
            enddo
         elseif ( sum.ne.0.0 ) then
            ii=i
         endif
         b(i)=sum
      enddo

      do i = n, 1, -1
         sum = b(i)
         if ( i.lt.n ) then
            do j = i+1, n
               sum = sum - a(i,j)*b(j)
            enddo
         endif
         b(i)=sum/a(i,i)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MANNWHITI -- Perform a 1-tailed mann-whitney (Wilcoxon) test on integers
C   To see if two groups of numbers come from the same population.
C   Approximation is valid provided n1 and n2 both > 8.
C   If probability is >0.1003, set at 0.1003. The total number of
C   points in the two groups can be up to 2000.
C
C   a j penny             stsci     86-11-26

      subroutine mannwhiti ( ix1, n1, ix2, n2, sig, larger, ierr )

      implicit none
      include 'STARMAN_INC'

      integer	n1		!i: No of points in 1st group
      integer	ix1(n1)		!i: 1st group data
      integer	n2		!i: No of points in 2nd group
      integer	ix2(n2)		!i: 2nd group data
      real	sig		!o: prob groups are same
      logical	larger		!o: True if group2 median>group1
      integer   ierr		!o: Failure flag: 0=ok;1= bad (total no of
				!    values > 2000)
C--
      integer   ix(2000), iw(2000)
      integer	j, k, kk, n, ivi
      real	sum, r1, r2, r3, r4, rv, diff, var, sumt, t
      logical	loop, allsame
Cbegin


      if ( ST_FAILED ) then
         ierr = 1
         return
      endif

      ierr = 0								!Failure flag

      larger = .false.							!Default
      sig = 1.0
									!Load data
      n = n1 + n2							!Total number of points

      if ( n.gt.2000 ) then						!Size check
         print *,'Mann-Whitney U test failed. Over 2000 points'
         ierr = 1
         return
      endif

      allsame = .true.
      ivi = ix1(1)
      do k = 1, n1							!Load and set up where come from
         if ( ix1(k).ne.ivi ) allsame = .false.
         ix(k) = ix1(k)
         iw(k) = 0
      enddo
      do k = 1, n2
         if ( ix2(k).ne.ivi ) allsame = .false.
         j = k + n1
         ix(j) = ix2(k)
         iw(j) = 1
      enddo

      if ( allsame ) return						!Check if all same, and if so, return

      call sort2i ( ix, iw, n )						!Order ix and iw

      sum = 0.0								!Calc rank sum of first set
      sumt = 0.0
      k = 1
      do while ( k.le.n )
         loop = .true.
         j = k
         do while ( loop )
            j = j + 1
            if ( j.gt.n .or. ix(k).ne.ix(j) ) loop = .false.
         enddo
         j = j - 1
         if ( j.ne.k ) then
            t = j - k + 1
            sumt = sumt + t*t*t - t
            rv = real(k+j)/2.0
            do kk = k, j
               if ( iw(kk).eq.1 ) sum = sum + rv
            enddo
            k = j
         else
           if ( iw(k).eq.1 ) sum = sum + real(k)
         endif
         k = k + 1
      enddo

      r1 = n1								!Median diff and significance
      r2 = n2
      r4 = n1 + n2
      r3 = r4 + 1
      diff = sum - (r2*r3)/2.0
      larger = .false.
      if ( diff.gt.0.0 ) larger = .true.
      sig = 1.0
      var = (r1*r2/(r4*(r4-1.0)))*((r4*r4*r4-r4-sumt)/12.0)
      rv = abs(diff)/sqrt(var)
      call gprob ( rv, sig )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MANNWHITR -- Perform a 1-tailed mann-whitney (Wilcoxon) test on reals
C   To see if two groups of numbers come from the same population.
C   Approximation is valid provided n1 and n2 both > 8.
C   If probability is >0.1003, set at 0.1003. The total number of
C   points in the two groups can be up to 2000.
C
C   a j penny             stsci     86-11-26

      subroutine mannwhitr ( ix1, n1, ix2, n2, sig, larger, ierr )

      implicit none
      include 'STARMAN_INC'

      integer	n1		!i: No of points in 1st group
      real      ix1(n1)		!i: 1st group data
      integer	n2		!i: No of points in 2nd group
      real      ix2(n2)		!i: 2nd group data
      real	sig		!o: prob groups are same
      logical	larger		!o: True if group2 median>group1
      integer   ierr		!o: Failure flag: 0=ok;1= bad (total no of
				!    values > 2000)
C--
      real      ix(2000), iw(2000)
      integer	j, k, kk, n
      real	rvi, sum, r1, r2, r3, r4, rv, diff, var, sumt, t
      logical	loop, allsame
Cbegin


      if ( ST_FAILED ) then
         ierr = 1
         return
      endif

      ierr = 0								!Failure flag

      larger = .false.							!Default
      sig = 1.0
									!Load data
      n = n1 + n2							!Total number of points

      if ( n.gt.2000 ) then						!Size check
         print *,'Mann-Whitney U test failed. Over 2000 points'
         ierr = 1
         return
      endif

      allsame = .true.
      rvi = ix1(1)
      do k = 1, n1							!Load and set up where come from
         if ( ix1(k).ne.rvi ) allsame = .false.
         ix(k) = ix1(k)
         iw(k) = 0.0
      enddo
      do k = 1, n2
         if ( ix2(k).ne.rvi ) allsame = .false.
         j = k + n1
         ix(j) = ix2(k)
         iw(j) = 1.0
      enddo

      if ( allsame ) return						!Check if all same, and if so, return

      call sort2r ( ix, iw, n )						!Order ix and iw

      sum = 0.0								!Calc rank sum of first set
      sumt = 0.0
      k = 1
      do while ( k.le.n )
         loop = .true.
         j = k
         do while ( loop )
            j = j + 1
            if ( j.gt.n .or. ix(k).ne.ix(j) ) loop = .false.
         enddo
         j = j - 1
         if ( j.ne.k ) then
            t = j - k + 1
            sumt = sumt + t*t*t - t
            rv = real(k+j)/2.0
            do kk = k, j
               if ( iw(kk).eq.1.0 ) sum = sum + rv
            enddo
            k = j
         else
           if ( iw(k).eq.1.0 ) sum = sum + real(k)
         endif
         k = k + 1
      enddo

      r1 = n1								!Median diff and significance
      r2 = n2
      r4 = n1 + n2
      r3 = r4 + 1
      diff = sum - (r2*r3)/2.0
      larger = .false.
      if ( diff.gt.0.0 ) larger = .true.
      sig = 1.0
      var = (r1*r2/(r4*(r4-1.0)))*((r4*r4*r4-r4-sumt)/12.0)
      rv = abs(diff)/sqrt(var)
      call gprob ( rv, sig )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MATINV -- Invert a matrix
C
C   alan penny                 ral          1990 apr 14

      subroutine matinv ( ain, w, n, na, ierr )

      implicit none
      include 'STARMAN_INC'

      integer n			!i:   Matrix occupies n by n
      real    ain(n,n)		!i/o: Input matrix. Replaced by inverse
      real    w(n,n)		!o:   Work space
      integer na		!i:   Matrix contents in na by na
      integer ierr		!o:   Error flag 0=ok;1=bad
C--
      integer  j, k, ka(100)
      real rv
Cbegin


      if ( ST_FAILED ) then
         ierr = 1
         return
      endif

      do j = 1, na							!Load identity matrix
         do k = 1, na
            w(j,k) = 0.0
         enddo
         w(j,1) = 1.0
      enddo

      call ludecmat ( ain, na, n, ka, rv, ierr )			!Decompose matrix

      if ( ierr.eq.0 ) then
         do j = 1, n							!Get inverse by columns
            call lusolmat ( ain, na, n, ka, w(1,j) )
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MEANSTDI -- Calc mean and std deviation of an integer vector.
C    This uses weights and elements of the vector can be ignored.
C    After calculating the mean and std dev, then those input
C    values which were more than some number of std devs from the
C    mean are rejected. Also rejected are those more than some
C    value from the mean. The mean and standard deviation are then
C    recalculated and the process repeated until no more values
C    are rejected. (The process stops if less than 5 values are left.)
C
C  a j penny             stsci             1988-03-27

      subroutine meanstdi ( v, w, use, n, dosdlim, rsd, doalim, rab,
     +                      av, sd )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: No of elements
      integer	v(n)		!i: Vector
      real	w(n)		!i: Weights
      logical	use(n)		!i:/o Flag to use element (can be set to
				!     to false if rejected by RSD or RAB)
      logical   dosdlim		!i: Use sd dev lim to reject?
      real	rsd		!i: Reject elements more than RSD sd devs from mean
      logical   doalim		!i: Use absolute lim to reject?
      real	rab		!i: Reject elements more than RAB from  mean
      real	av		!o: Mean
      real	sd		!o: Standard deviation
C--
      integer k, ng, knote
      real rmax, abdiff
      logical loop
      double precision dav, dsw, dsvw, dsvvw, dv, dw
Cbegin


      if ( ST_FAILED ) return

      av = 0.0								!Default and Checks
      sd = 0.0

      if ( n.lt.1 ) return
      ng = 0
      do k = 1, n
         if ( use(k) ) ng = ng + 1
      enddo
      if ( ng.eq.0 ) return

      dsw = 0.0d0							!Calculate mean and std dev
      dsvw = 0.0d0
      dsvvw = 0.0d0
      do k = 1, n
         if ( use(k) ) then
            dw = w(k)
            dsw = dsw + dw
            dsvw = dsvw + v(k)*dw
            dsvvw = dsvvw + v(k)*v(k)*dw
         endif
      enddo

      if ( dsw.eq.0.0d0 ) return
      dav = dsvw/dsw
      av = dav
      if ( ng.lt.2 ) return
      dv = max (1.0d-20,(dsvvw-dsw*dav*dav))
      sd = sqrt(dv/(dsw-1.0d0))

C  If wanted, recalculate mean, by throwing out largest diff and
C  recalculating until none thrown out as too far off mean

      if ( .not.dosdlim .and. .not.doalim ) return

      loop = .true.
      do while ( loop )							!Loop till no more rejected
         rmax = 0.0							!Find max out of limits
         do k = 1, n
            if ( use(k) ) then
               abdiff = abs(v(k)-av)
               if ( abdiff.gt.rmax ) then
                  rmax = abdiff
                  knote = k
               endif
            endif
         enddo
         if ( ( doalim .and. rmax.gt.rab )     .or.			!if,note or exit
     +        ( dosdlim .and. rmax.gt.rsd*sd ) ) then
            use(knote) = .false.
            ng = ng - 1
         else
            loop = .false.
         endif

         if ( loop ) then						!If redo, recalc mean, sd
            dsw = 0.0d0
            dsvw = 0.0d0
            dsvvw = 0.0d0
            do k = 1, n
               if ( use(k) ) then
                  dw = w(k)
                  dsw = dsw + dw
                  dsvw = dsvw + v(k)*dw
                  dsvvw = dsvvw + v(k)*v(k)*dw
               endif
            enddo

            if ( dsw.eq.0.0d0 ) return
            dav = dsvw/dsw
            av = dav
            if ( ng.lt.2 ) return
            dv = max (1.0d-20,(dsvvw-dsw*dav*dav))
            sd = sqrt(dv/(dsw-1.0d0))
         endif

         if ( loop .and. ng.lt.5 ) loop = .false.			!Must be >4 elements

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MEANSTDR -- Calc mean and std deviation of a real vector.
C    This uses weights and elements of the vector can be ignored.
C    After calculating the mean and std dev, then those input
C    values which were more than some number of std devs from the
C    mean are rejected. Also rejected are those more than some
C    value from the mean. The mean and standard deviation are then
C    recalculated and the process repeated until no more values
C    are rejected. (The process stops if less than 5 values are left.)
C
C  a j penny             stsci             1988-03-27


      subroutine meanstdr ( v, w, use, n, dosdlim, rsd, doalim, rab,
     +                      av, sd )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: No of elements
      real	v(n)		!i: Vector
      real	w(n)		!i: Weights
      logical	use(n)		!i:/o Flag to use element (can be set to
				!     to false if rejected by RSD or RAB)
      logical   dosdlim		!i: Use sd dev lim to reject?
      real	rsd		!i: Reject elements more than RSD sd devs from mean
      logical   doalim		!i: Use absolute lim to reject?
      real	rab		!i: Reject elements more than RAB from  mean
      real	av		!o: Mean
      real	sd		!o: Standard deviation
C--
      integer k, ng, knote
      real rmax, abdiff
      logical loop
      double precision dav, dsw, dsvw, dsvvw, dv, dw
Cbegin


      if ( ST_FAILED ) return

      av = 0.0								!Default and Checks
      sd = 0.0

      if ( n.lt.1 ) return
      ng = 0
      do k = 1, n
         if ( use(k) ) ng = ng + 1
      enddo
      if ( ng.eq.0 ) return

      dsw = 0.0d0							!Calculate mean and std dev
      dsvw = 0.0d0
      dsvvw = 0.0d0
      do k = 1, n
         if ( use(k) ) then
            dw = w(k)
            dsw = dsw + dw
            dsvw = dsvw + v(k)*dw
            dsvvw = dsvvw + v(k)*v(k)*dw
         endif
      enddo

      if ( dsw.eq.0.0d0 ) return
      dav = dsvw/dsw
      av = dav
      if ( ng.lt.2 ) return
      dv = max (1.0d-20,(dsvvw-dsw*dav*dav))
      sd = sqrt(dv/(dsw-1.0d0))

C  If wanted, recalculate mean, by throwing out largest diff and
C  recalculating until none thrown out as too far off mean

      if ( .not.doalim .and. .not.dosdlim ) return

      loop = .true.
      do while ( loop )							!Loop till no more rejected
         rmax = 0.0							!Find max out of limits
         do k = 1, n
            if ( use(k) ) then
               abdiff = abs(v(k)-av)
               if ( abdiff.gt.rmax ) then
                  rmax = abdiff
                  knote = k
               endif
            endif
         enddo
         if ( ( doalim .and. rmax.gt.rab )     .or.			!if,note or exit
     +        ( dosdlim .and. rmax.gt.rsd*sd ) ) then
            use(knote) = .false.
            ng = ng - 1
         else
            loop = .false.
         endif

         if ( loop ) then						!if redo, recalc mean, sd
            dsw = 0.0d0
            dsvw = 0.0d0
            dsvvw = 0.0d0
            do k = 1, n
               if ( use(k) ) then
                  dw = w(k)
                  dsw = dsw + dw
                  dsvw = dsvw + v(k)*dw
                  dsvvw = dsvvw + v(k)*v(k)*dw
               endif
            enddo

            if ( dsw.eq.0.0d0 ) return
            dav = dsvw/dsw
            av = dav
            if ( ng.lt.2 ) return
            dv = max (1.0d-20,(dsvvw-dsw*dav*dav))
            sd = sqrt(dv/(dsw-1.0d0))
         endif

         if ( loop .and. ng.lt.5 ) loop = .false.			!Must be >4 elements

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POIDEV -- Make a poisson randomised integer output of a real input number
C 1)Numbers < 20
C        choose a random number between 0 and 1. then calc the probability
C        p0 of 0 as exp(-s). if nr is less than p0 exit with answer 0. else
C        calc prob p1 of 1 as s*exp(-s)/1. if nr less than p0+p1, exit with
C        ans 1. else calc prob p2 of 2 as s*s*exp(-s)/2.1  if nr less than
C        p0+p1+p2, exit with ans 2. and so on
C 2)Numbers >=20
C        a gaussian scatter times the square root of the number, which is
C        very nearly a poissonian scatter.
C
C 3)Numbers < 1.0e-10
C        These get a return of 0.0
C
C   The seed number is changed after each call, but can be set to any number
C   to start the random number pattern
C
C     a.j. penny               stsci                        1989-04-21

      subroutine poidev ( s, ss, nran )

      implicit none
      include 'STARMAN_INC'

      real    s		!i:   input number
      real    ss	!o:   output number (integral value)
      integer nran	!i/o: Seed number for the random number
			!     generator
C--
      real b, r, a, c
      integer k
Cbegin


      if ( ST_FAILED ) return

      if ( s.lt.1.0e-10 ) then				!If -ve or very near zero
         ss = 0.0					! output 0. (random gen
         call rano ( b, nran )				! called anyway)
         return
      endif

      if ( s.lt.20.0 ) then				!Do full calc
         call rano (b, nran )
         k = 0
         r = 0.0
         a = exp(-1.0*s)
         c = a
         do while ( b.ge.c )
            k = k + 1
            r = r + 1.0
            a = a*s/r
            c = c + a
         enddo
         ss = real(k)
      else						!Approx gauss
         call gasdev ( a, nran )
         ss = a*sqrt(s)
         ss = aint(s+ss+0.5)
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POLFIT -- Least squares fit to data with a polynomial curve,
C               Y = A(1) + A(2)*X + A(3)*X**2 + ...
C
C     Valid up to nterms = 10
C     Needs input x,y values not much above 1
C
C     Got from m j currie by a j penny     rgo      7-sep-82

      subroutine polfit ( dx, dy, dsd, np, nfp, mode, da, dchisq, drms )

      implicit none
      include 'STARMAN_INC'

      integer	np		!i: No of points
      double precision dx(np)	!i: data of independent variable
      double precision dy(np)	!i: data of dependent variable
      double precision dsd(np)	!i: Std dev for y data points
      integer	nfp		!i: No of coefficents (polynomial degree +1)
      integer	mode		!i: 'mode' => weighting of least squares fit
				! +1=(instrumental) weight(i)=1.0/sdy(i)**2
				!  0=(no weighting) weight(i)=1.0
				! -1=(statistical)  weight(i)=1.0/y(i)
      double precision da(nfp)	!o: coeffs of polynomial
      double precision dchisq	!o: Reduced chi square for fit
      double precision drms	!o: Std deviation of fit
C--
      double precision dsumx(19), dsumy(10), darray(10,10), dxi, dyi,
     +                 dweight, dxterm, dyterm, ddelta, dcalc
      logical same
      integer i, j, k, l, n, nmax, nfree

      double precision determ
      external determ
Cbegin


      if ( ST_FAILED ) return

C  Set up default

      da(1) = dy(1)
      do k = 2, nfp
         da(k) = 0.0d0
      enddo
      dchisq = 0.0
      drms = 0.0

C  Check for all values same. If so, set answer, return.

      same = .true.
      do k = 1, np
         if ( dy(k).ne.dy(1) ) same = .false.
      enddo
      if ( same ) return

C   Accumulate weighted sums

      nmax = 2*nfp - 1
      do n = 1, nmax
         dsumx(n) = 0.0d0
      enddo
      do j = 1, nfp
         dsumy(j) = 0.0d0
      enddo

      do i = 1, np

         dxi = dx(i)
         dyi = dy(i)

         if ( mode.eq.1 ) then
            dweight = 1.0d0/(dsd(i)*dsd(i))
         elseif ( mode.eq.0 ) then
            dweight = 1.0d0
         else
            if ( dyi.gt.0.0d0 ) then
               dweight = 1.0d0/dyi
            elseif ( dyi.eq.0.0d0 ) then
               dweight = 1.0d0
            else
               dweight = -1.0d0/dyi
            endif
         endif
         dxterm = dweight

         do n = 1, nmax
            dsumx(n) = dsumx(n) + dxterm
            dxterm = dxterm*dxi
         enddo
         dyterm = dweight*dyi
         do n = 1, nfp
            dsumy(n) = dsumy(n) + dyterm
            dyterm = dyterm*dxi
         enddo
         dchisq = dchisq + dweight*dyi*dyi
      enddo

C   Construct matrices and calculate coefficients

      do j = 1, nfp
         do k = 1, nfp
            n = j + k - 1
            darray(j,k) = dsumx(n)
         enddo
      enddo
      ddelta = determ ( darray, nfp )

C  If determ is zero, exit

      if ( ddelta.eq.0.0d0 ) then
         dchisq = 0.0
         do j = 1, nfp
            da(j) = 0.0d0
         enddo
         return
      endif

      do l = 1, nfp
         do j = 1, nfp
            do k = 1, nfp
               n = j + k - 1
               darray(j,k) = dsumx(n)
            enddo
            darray(j,l) = dsumy(j)
         enddo
         da(l) = determ(darray,nfp)/ddelta
      enddo

C   Calculate chi square

      nfree = np - nfp
      if ( nfree.le.0 ) return
      do  j = 1, nfp
         dchisq = dchisq - 2.0d0*da(j)*dsumy(j)
         do k = 1, nfp
            n = j + k - 1
            dchisq = dchisq + da(j)*da(k)*dsumx(n)
         enddo
      enddo
      dchisq = dchisq/dble(nfree)

C   Calculate standard deviation

      drms = 0.0
      do k = 1, np
         dcalc = da(1)
         if ( nfp.ne.1 ) then
            do j = 2, nfp
               dcalc = dcalc + da(j)*(dx(k)**(j-1))
            enddo
            drms = drms + (dy(k)-dcalc)*(dy(k)-dcalc)
         endif
      enddo
      drms = sqrt(drms/dble(nfree))


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PRODCC -- Sum the products of columns in two arrays
C    It takes any column in each of the arrays and multiplies and sums
C    It takes any column in each of the arrays and multiplies and sums
C    the elements of the same section in those columns.
C
C   alan penny                ral              1990-05-22

      subroutine prodcc ( a, nxa, nya, ja, b, nxb, nyb, jb, ns, ne, s )

      implicit none
      include 'STARMAN_INC'

      integer	nxa		!i: X size of array A
      integer	nya		!i: Y size of array A
      real	a(nxa,nya)	!i: Array A
      integer	ja		!i: Column of array A to multiply
      integer	nxb		!i: X size of array B
      integer	nyb		!i: Y size of array B
      real	b(nxb,nyb)	!i: Array B
      integer	jb		!i: Column of array B to multiply
      integer	ns		!i: Start of section in columns
      integer	ne		!i: End of section in columns
      real	s		!o: Output sum
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      s = 0.0
      do k = ns, ne
         s = s + a(ja,k)*b(jb,k)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PRODCR -- Sum the products of a column of one array with the row of another
C    It takes any column in each of the arrays and multiplies and sums
C    the elements of the same section in those columns.
C
C   alan penny                ral              1990-05-22

      subroutine prodcr ( a, nxa, nya, ja, b, nxb, nyb, jb, ns, ne, s )

      implicit none
      include 'STARMAN_INC'

      integer	nxa		!i: X size of array A
      integer	nya		!i: Y size of array A
      real	a(nxa,nya)	!i: Array A
      integer	ja		!i: Column of array A to multiply
      integer	nxb		!i: X size of array B
      integer	nyb		!i: Y size of array B
      real	b(nxb,nyb)	!i: Array B
      integer	jb		!i: Row of array B to multiply
      integer	ns		!i: Start of section in row/column
      integer	ne		!i: End of section in row/column
      real	s		!o: Output sum
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      s = 0.0
      do k = ns, ne
         s = s + a(ja,k)*b(k,jb)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PRODRC -- Sum the products of a row of one array with the column of another
C    It takes any column in each of the arrays and multiplies and sums
C    the elements of the same section in those columns.
C
C   alan penny                ral              1990-05-22

      subroutine prodrc ( a, nxa, nya, ja, b, nxb, nyb, jb, ns, ne, s )

      implicit none
      include 'STARMAN_INC'

      integer	nxa		!i: X size of array A
      integer	nya		!i: Y size of array A
      real	a(nxa,nya)	!i: Array A
      integer	ja		!i: Row of array A to multiply
      integer	nxb		!i: X size of array B
      integer	nyb		!i: Y size of array B
      real	b(nxb,nyb)	!i: Array B
      integer	jb		!i: Column of array B to multiply
      integer	ns		!i: Start of section in row/column
      integer	ne		!i: End of section in row/column
      real	s		!o: Output sum
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      s = 0.0
      do k = ns, ne
         s = s + a(k,ja)*b(jb,k)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C PRODRR -- Sum the products of rows in two arrays
C    It takes any row in each of the arrays and multiplies and sums
C    the elements of the same section in those rows.
C
C
C   alan penny                ral              1990-05-22

      subroutine prodrr ( a, nxa, nya, ja, b, nxb, nyb, jb, ns, ne, s )

      implicit none
      include 'STARMAN_INC'

      integer	nxa		!i: X size of array A
      integer	nya		!i: Y size of array A
      real	a(nxa,nya)	!i: Array A
      integer	ja		!i: Row of array A to multiply
      integer	nxb		!i: X size of array B
      integer	nyb		!i: Y size of array B
      real	b(nxb,nyb)	!i: Array B
      integer	jb		!i: Row of array B to multiply
      integer	ns		!i: Start of section in rows
      integer	ne		!i: End of section in rows
      real	s		!o: Output sum
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      s = 0.0
      do k = ns, ne
         s = s + a(k,ja)*b(k,jb)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C RANGEI -- Find mean, std dev of an integer bad pixel flagged image section
C  It calcs the mean and std dev. Then recalcs (2 more times) using only
C  those points within 3 std dev of the calculated mean.
C  If the section is more than 80 on either side, a grid of points
C  is taken so that between 40 and 80 on a side is sampled.
C
C   alan penny             ral         1990 jan

      subroutine rangei ( data, nx, ny, kx, ky, inval, am, std, ierr )

      implicit none
      include 'STARMAN_INC'

      integer   nx		!i: X image size
      integer   ny		!i: Y image size
      integer	data(nx,ny)	!i: Image
      integer   kx(2)		!i: X range of image to use
      integer   ky(2)		!i: Y range of image to use
      integer	inval		!i: Image bad pixel value flag
      real      am		!o: Mean of image
      real      std		!o: Std Dev of image
      integer   ierr		!o: Error flag. 0=ok;1=all same;2=only
				!   one good point; 3=no good points
				!   (in sampling grid)
C--
      double precision s, ss, sn, an
      integer nxa, nya, j, k, iter, kn, iv
      real amax, amin, rmin, rmax
Cbegin


      if ( ST_FAILED ) then
         ierr = 1
         return
      endif

      nxa = 1 + (kx(2)-kx(1))/80			!Get sampling step
      nya = 1 + (ky(2)-ky(1))/80

      rmin = 1.0e10					!Get max and min
      rmax = -1.0e10					! (these are later used
      kn = 0						! as limits)
      do k = ky(1), ky(2), nya
         do j = kx(1), kx(2), nxa
            iv = data(j,k)
            if ( iv.ne.inval ) then
               kn = kn + 1
               rmin = min(real(iv),rmin)
               rmax = max(real(iv),rmax)
            endif
         enddo
      enddo

      ierr = 0
      am = 0.0
      std = 0.0
      if ( kn.eq.0 ) then				!Check none good
         am = rmin
         ierr = 3
      elseif ( kn.eq.1 ) then				!Check more than 1
         am = rmin
         ierr = 2
      elseif ( rmin.ge.rmax ) then			!Check that not all same
         am = rmin
         ierr = 1
      endif
      if ( ierr.ne.0 ) return

      do iter = 1,3					!Do the calcs 3 times

         s = 0.0					!get mean and std dev
         sn = 0.0					! of good data within
         ss = 0.0					! the limits
         do k = ky(1), ky(2), nya
            do j = kx(1), kx(2), nxa
               iv = data(j,k)
               if ( iv.ne.inval .and. real(iv).ge.rmin
     +              .and. real(iv).le.rmax ) then
                  an = dble(real(iv))
                  s = s + an
                  ss = ss + an*an
                  sn = sn + 1.0d0
               endif
            enddo
         enddo
         if ( sn.gt.0.1 ) am = s/sn
         if ( sn.gt.1.1 ) then
            ss = (ss-s*s/sn)/(sn-1.0)
            if ( ss.gt.1.0e-20 ) std = sqrt(ss)
         endif

         amin = am - 3.0*std				!Get new limits
         amax = am + 3.0*std
         rmin = int(max(amin,real(rmin)))
         rmax = int(min(amax,real(rmax)))

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C RANGER -- Find mean, std dev of a real bad pixel flagged image section
C  It calcs the mean and std dev. Then recalcs (2 more times) using only
C  those points within 3 std dev of the calculated mean.
C  If the section is more than 80 on either side, a grid of points
C  is taken so that between 40 and 80 on a side is sampled.
C
C   alan penny             ral         1990 jan

      subroutine ranger ( data, nx, ny, kx, ky, rinval, am, std, ierr )

      implicit none
      include 'STARMAN_INC'

      integer   nx		!i: X image size
      integer   ny		!i: Y image size
      real      data(nx,ny)	!i: Image
      integer   kx(2)		!i: X range of image to use
      integer   ky(2)		!i: Y range of image to use
      real      rinval		!i: Image bad pixel value flag
      real      am		!o: Mean of image
      real      std		!o: Std Dev of image
      integer   ierr		!o: Error flag. 0=ok;1=all same;2=only
				!   one good point; 3=no good points
				!   (in sampling grid)
C--
      double precision s, ss, sn, an
      integer nxa, nya, j, k, iter, kn,
     +        kxs, kxe, kys, kye
      real amax, amin, rv, rmin, rmax
Cbegin


      if ( ST_FAILED ) then
         ierr = 1
         return
      endif

      am = 0.0						!Defaults
      std = 0.0
      ierr = 0

      kxs = min(kx(1),kx(2))
      kxe = max(kx(1),kx(2))
      kys = min(ky(1),ky(2))
      kye = max(ky(1),ky(2))

      if ( kxs.gt.nx .or. kxe.lt.1 .or. kys.gt.ny
     +     .or. kye.lt.1 ) then
          ierr = 3
          return
      endif

      kxs = max(1,kxs)
      kxe = min(nx,kxe)
      kys = max(1,kys)
      kye = min(ny,kye)

      nxa = 1 + (kxe-kxs)/80				!Get sampling step
      nya = 1 + (kye-kys)/80

      rmin = 1.0e10					!Get default max and min
      rmax = -1.0e10					! (these are later used as limits)
      kn = 0
      do k = kys, kye, nya
         do j = kxs, kxe, nxa
            rv = data(j,k)
            if ( rv.ne.rinval ) then
               kn = kn + 1
               rmin = min(rv,rmin)
               rmax = max(rv,rmax)
            endif
         enddo
      enddo

      if ( kn.eq.0 ) then				!Check none good
         am = rmin
         ierr = 3
      elseif ( kn.eq.1 ) then				!Check more than 1
         am = rmin
         ierr = 2
      elseif ( rmin.ge.rmax ) then			!Check that not all same
         am = rmin
         ierr = 1
      endif
      if ( ierr.ne.0 ) return

      do iter = 1,3					!Do the calcs 3 times

         s = 0.0					!get mean and std dev
         sn = 0.0					! of good data within
         ss = 0.0					! the limits
         do k = kys, kye, nya
            do j = kxs, kxe, nxa
               rv = data(j,k)
               if ( rv.ne.rinval .and.
     +              rv.ge.rmin .and. rv.le.rmax ) then
                  an = dble(rv)
                  s = s + an
                  ss = ss + an*an
                  sn = sn + 1.0d0
               endif
            enddo
         enddo
         if ( sn.gt.0.1 ) am = s/sn
         if ( sn.gt.1.1 ) then
            ss = (ss-s*s/sn)/(sn-1.0)
            if ( ss.gt.1.0e-20 ) std = sqrt(ss)
         endif

         amin = am - 3.0*std				!Get new limits
         amax = am + 3.0*std
         rmin = max(amin,rmin)
         rmax = min(amax,rmax)

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MEDIANR -- Find median of real bad pixel flagged image section
C
C   alan penny             ral         1990 jan

      subroutine medianr ( data, nx, ny, rw, kx, ky, rinval, rm, ierr )

      implicit none
      include 'STARMAN_INC'

      integer   nx		!i: X image size
      integer   ny		!i: Y image size
      real      data(nx,ny)	!i: Image
      real      rw(*)		!o: Work space (box size)
      integer   kx(2)		!i: X range of image to use
      integer   ky(2)		!i: Y range of image to use
      real      rinval		!i: Image bad pixel value flag
      real      rm		!o: Mean of image
      integer   ierr		!o: Error flag. 0=ok;1=all same;2=only
				!   one good point; 3=no good points
				!   (in sampling grid)
C--
      integer j, k, kn, ka, kxs, kxe, kys, kye
      real rv
Cbegin


      if ( ST_FAILED ) then
         ierr = 1
         return
      endif

      rm = 0.0						!Defaults
      ierr = 0

      kxs = min(kx(1),kx(2))
      kxe = max(kx(1),kx(2))
      kys = min(ky(1),ky(2))
      kye = max(ky(1),ky(2))

      if ( kxs.gt.nx .or. kxe.lt.1 .or. kys.gt.ny
     +     .or. kye.lt.1 ) then
          ierr = 3
          return
      endif

      kxs = max(1,kxs)
      kxe = min(nx,kxe)
      kys = max(1,kys)
      kye = min(ny,kye)

      kn = (kxe-kxs+1)*(kye-kys+1)

      if ( kn.eq.1 ) then
         rv = data(kxs,kys)
         if ( rv.eq.rinval ) then
            ierr = 3
            rm = 0.0
         else
            ierr = 2
            rm = rv
         endif
         return
      endif

      rm = 0.0

      ka = 0
      do k = kys, kye
         do j = kxs, kxe
            if ( data(j,k).ne.rinval ) then
               ka = ka + 1
               rw(ka) = data(j,k)
            endif
         enddo
      enddo

      if ( ka.eq.0 ) then
         ierr = 3
      elseif ( ka.eq.1 ) then
         ierr = 2
         rm = rw(1)
      else
         call sort1r ( rw, ka )
         if ( (2*(ka/2)).eq.ka ) then
            k = (ka/2)
            rm = (rw(k)+rw(k+1))/2.0
         else
            k = (ka/2) + 1
            rm = rw(k)
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MEDIANS -- Find median int*2 bad pixel flagged image section
C
C   alan penny             ral         1990 jan

      subroutine medians ( kdata, nx, ny, iw, kx, ky, inval, rm, ierr )

      implicit none
      include 'STARMAN_INC'

      integer   nx		!i: X image size
      integer   ny		!i: Y image size
      integer*2 kdata(nx,ny)	!i: Image
      integer   iw(*)       	!o: Work space (box size)
      integer   kx(2)		!i: X range of image to use
      integer   ky(2)		!i: Y range of image to use
      integer   inval		!i: Image bad pixel value flag
      real      rm		!o: Mean of image
      integer   ierr		!o: Error flag. 0=ok;1=all same;2=only
				!   one good point; 3=no good points
				!   (in sampling grid)
C--
      integer j, k, ka, kn, kv, kxs, kxe, kys, kye
Cbegin


      if ( ST_FAILED ) then
         ierr = 1
         return
      endif

      rm = 0.0						!Defaults
      ierr = 0

      kxs = min(kx(1),kx(2))
      kxe = max(kx(1),kx(2))
      kys = min(ky(1),ky(2))
      kye = max(ky(1),ky(2))

      if ( kxs.gt.nx .or. kxe.lt.1 .or. kys.gt.ny
     +     .or. kye.lt.1 ) then
          ierr = 3
          return
      endif

      kxs = max(1,kxs)
      kxe = min(nx,kxe)
      kys = max(1,kys)
      kye = min(ny,kye)

      kn = (kxe-kxs+1)*(kye-kys+1)

      if ( kn.eq.1 ) then
         kv = kdata(kxs,kys)
         if ( kv.eq.inval ) then
            ierr = 3
            rm = 0.0
         else
            ierr = 2
            rm = kv
         endif
         return
      endif

      rm = 0.0

      ka = 0
      do k = kys, kye
         do j = kxs, kxe
            if ( kdata(j,k).ne.inval ) then
               ka = ka + 1
               iw(ka) = kdata(j,k)
            endif
         enddo
      enddo

      if ( ka.eq.0 ) then
         ierr = 3
      elseif ( ka.eq.1 ) then
         ierr = 2
         rm = iw(1)
      else
         call sort1i ( iw, ka )
         if ( (2*(ka/2)).eq.ka ) then
            k = (ka/2)
            rm = (real(iw(k)+iw(k+1)))/2.0
         else
            k = (ka/2) + 1
            rm = iw(k)
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C RANGES -- Find mean, std dev of int*2 bad pixel flagged image section
C  It calcs the mean and std dev. Then recalcs (2 more times) using only
C  those points within 3 std dev of the calculated mean.
C  If the section is more than 80 on either side, a grid of points
C  is taken so that between 40 and 80 on a side is sampled.
C
C   alan penny             ral         1990 jan

      subroutine ranges ( kdata, nx, ny, kx, ky, inval, am, std, ierr )

      implicit none
      include 'STARMAN_INC'

      integer   nx		!i: X image size
      integer   ny		!i: Y image size
      integer*2 kdata(nx,ny)	!i: Image
      integer   kx(2)		!i: X range of image to use
      integer   ky(2)		!i: Y range of image to use
      integer   inval		!i: Image bad pixel value flag
      real      am		!o: Mean of image
      real      std		!o: Std Dev of image
      integer   ierr		!o: Error flag. 0=ok;1=all same;2=only
				!   one good point; 3=no good points
				!   (in sampling grid)
C--
      double precision s, ss, sn, an
      integer nxa, nya, kmin, kmax, kv, j, k, iter, kn,
     +        kxs, kxe, kys, kye
      real amax, amin
Cbegin


      if ( ST_FAILED ) then
         ierr = 1
         return
      endif

      am = 0.0						!Defaults
      std = 0.0
      ierr = 0

      kxs = min(kx(1),kx(2))
      kxe = max(kx(1),kx(2))
      kys = min(ky(1),ky(2))
      kye = max(ky(1),ky(2))

      if ( kxs.gt.nx .or. kxe.lt.1 .or. kys.gt.ny
     +     .or. kye.lt.1 ) then
          ierr = 3
          return
      endif

      kxs = max(1,kxs)
      kxe = min(nx,kxe)
      kys = max(1,kys)
      kye = min(ny,kye)

      nxa = 1 + (kxe-kxs)/80				!Get sampling step
      nya = 1 + (kye-kys)/80

      kmin = 32767					!Get max and min
      kmax = -32768					! (these are later used as limits)
      kn = 0
      do k = kys, kye, nya
         do j = kxs, kxe, nxa
            kv = kdata(j,k)
            if ( kv.ne.inval ) then
               kn = kn + 1
               kmin = min(kv,kmin)
               kmax = max(kv,kmax)
            endif
         enddo
      enddo

      ierr = 0
      am = 0.0
      std = 0.0
      if ( kn.eq.0 ) then				!Check none good
         am = kmin
         ierr = 3
      elseif ( kn.eq.1 ) then				!Check more than 1
         am = kmin
         ierr = 2
      elseif ( kmin.ge.kmax ) then			!Check that not all same
         am = kmin
         ierr = 1
      endif
      if ( ierr.ne.0 ) return

      do iter = 1,3					!Do the calcs 3 times

         s = 0.0					!get mean and std dev
         sn = 0.0					! of good data within
         ss = 0.0					! the limits
         do k = kys, kye, nya
            do j = kxs, kxe, nxa
               kv = kdata(j,k)
               if ( kv.ne.inval .and. kv.ge.kmin .and.
     +              kv.le.kmax ) then
                  an = dble(kv)
                  s = s + an
                  ss = ss + an*an
                  sn = sn + 1.0d0
               endif
            enddo
         enddo
         if ( sn.gt.0.1 ) am = s/sn
         if ( sn.gt.1.1 ) then
            ss = (ss-s*s/sn)/(sn-1.0)
            if ( ss.gt.1.0e-20 ) std = sqrt(ss)
         endif

         amin = am - 3.0*std				!Get new limits
         amax = am + 3.0*std
         kmin = int(max(amin,real(kmin)))
         kmax = int(min(amax,real(kmax)))

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C RANO -- Make random number between 0 and 1.
C   Takes machine supplied generator (RAN) and randomises it a bit more.
C
C   AJSEED should be called before this. If not treated as
C   call with seed set to 1.
C
C   On 1st use, the routine sets itself up, using many calls to the
C   machine random generator. After that, it calls machine generator
C   only once per call.
C
C     a j penny               ral                      88-07-04

      subroutine rano ( rv, iseed )

      implicit none
      include 'STARMAN_INC'

      real      rv		!o:   Random number
      integer	iseed		!i/o: Random number seed
C--
      real v(85), y, dum
      integer iff, j
      data iff / 0 /
      real ajran
      external ajran
Cbegin


      if ( ST_FAILED ) return

      rv = 1.0
      if ( iff.eq.0 ) then
         do j = 1, 85
            dum = ajran ( iseed )
            iff = 1
         enddo
         do j = 1, 85
            v(j) = ajran ( iseed )
         enddo
         y = ajran ( iseed )
      endif
      j = 1 + int(85.0*y)
      if ( j.gt.85 .or. j.lt.1 ) pause
      y = v(j)
      rv = y
      v(j) = ajran ( iseed )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C RINTER -- (function) Interpolate in a two-dimensional look-up table.
C
C Input arguments
C
C        F  is an NX by NY array, where X=1.0, Y=1.0 refers to the center
C           of the first pixel and X=FLOAT(NX), Y=FLOAT(NY) refers to the
C           center of the last pixel.
C
C      X,Y  are the real coordinates of the point, relative to the corner
C           of the array, to which the table is to be interpolated.
C
C
C Output arguments
C
C   RINTER  is the numerical obtained by the interpolation.
C
C   DFDX,   are the estimated first spatial derivatives of F with
C   DFDY    respect to x and y, evaluated at (X, Y).
C
C The method used is cubic interpolation: first, at four integral
C values of y, the values of F at integral values of x are interpolated
C to the point x=X, yielding four values for the function and its first
C derivative with respect to x.  Then these four are interpolated to
C the point x,y=X,Y to give one functional value and one first
C derivative with respect to y.  Finally, the four first derivatives
C of F with respect to x are interpolated to x,y=X,Y.  It so happens
C that the final value obtained for the function and its derivatives
C would have come out the same if we had interpolated in y first,
C rather than in x.
C
C This has been modified to work on a 2-D (XY) plane in a 3-D (XYZ)
C cube. In the Z plane chosen, only a (sub)area is used.
C
C  P B STETSON                         DAO    1986
C  Modified by  A J PENNY          STScI              1987 jan 20
C


      real function rinter ( f, nxa, nya, nza, num, nx, ny, x, y, dfdx,
     +                       dfdy )


      implicit none

      integer nxa		!i: X size of input array
      integer nya		!i: Y size of input array
      integer nza		!i: Z size of input array
      real    f(nxa,nya,nza)	!i: Input array
      integer num		!i: Z plane of array to use
      integer nx		!i: X size useful data in that Z plane
      integer ny		!i: Y size useful data in that Z plane
      real    x			!i: X position
      real    y			!i: Y position
      real    dfdx		!o: Spatial derivative wrt X
      real    dfdy		!o: Spatial derivative wrt Y
C--
      integer ix, iy, i, j, k, ixn
      real dx, dy, val(-1:2), g(-1:2), dgdx(-1:2), dummy

      real cubint
      external cubint
      real acubint
      external acubint
Cbegin


C  Location

      ix = x
      iy = y
      dx = x - ix
      dy = y - iy

C  If outside box by 1 or more

      if ( iy.le.0 .or. iy.ge.ny+1 .or. ix.le.0. .or. ix.ge.nx+1 ) then
         rinter = 0.0
         dfdx = 0.0
         dfdy = 0.0
         return
      endif

C  If in body or not

      if ( ix.ge.2 .and. ix.le.nx-2 .and. iy.ge.2 .and. iy.le.ny-2 )then
         do i = -1, 2
            j = iy + i
            g(i) = cubint ( f(ix,j,num), dx, dgdx(i) )
         enddo
      else
         do i = -1, 2
            j = iy + i
            do k = -1, 2
               if ( j.lt.1 .or. j.gt.ny ) then
                  val(k) = 0.0
               else
                  ixn = ix + k
                  if ( ixn.lt.1 .or. ixn.gt.nx ) then
                     val(k) = 0.0
                  else
                     val(k) = f(ixn,j,num)
                  endif
               endif
            enddo
            g(i) = cubint ( val(0), dx, dgdx(i) )
         enddo
      endif

C  Calculate values

      rinter = cubint ( g(0), dy, dfdy )
      dfdx = cubint ( dgdx(0), dy, dummy )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C RSSCALE -- Scale a real array into an integer*2 array in range
C             0 - 255, from amin to amax
C
C   a j penny                 dao           1988-04-25

      subroutine rsscale ( rv, iv, n, rinval, amin, amax )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: No of points to do
      real	rv(n)		!i: Input array
      integer*2	iv(n)		!o: Output scaled array
      real	rinval		!i: Invalid pixel flag
      real	amin		!i: Bottom level to go to 0
      real	amax		!i: Top level to go to 255
C--
      real am, val, d
      integer k
Cbegin


      if ( ST_FAILED ) return

      am = amax
      if ( abs(amax-amin).lt.1.0e-3 ) am = amin + 1.0e-3
      d = am - amin
      do k = 1, n
         val = 127.0
         if ( rv(k).ne.rinval ) val = 255.0*(am-rv(k))/d
         iv(k) = min(max(val,1.0),255.0)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SIMULX -- Solve the simultaneous equations   Y = A.X  for X
C This is taken from the AAO routine SIMULT, except that the
C arrays have been extended to allow for 100 unknowns and there
C is no separate work space for the coefficent matrix. There is
C also more out-of-range checking. The method is Gaussian
C back-substitution.
C
C      K F HARTLEY, A J PENNY  RGO    STAFF AAO            83-2-23

      subroutine simulx ( y, a, x, n )

      implicit none
      include 'STARMAN_INC'

      integer n        		!i:Size of vector arrays; matrix is N*N
      real    y(n)     		!i:Array of function values
      real    a(n,n) 		!i:Array of coefficents (destroyed in calcs)
      real    x(n)     		!o:Vectorix
C--
      integer row(100), col(100)
      integer ip, iq, i, j, ia, ja, ik, jk, np2, n50, k
      real amx, aminv, amaxv, afactor, factor
Cbegin


      if ( ST_FAILED ) return

      if ( n.gt.100 ) then
         call printo ( 'ERROR: Too many eqns for s/r SIMULX' )
         return
      endif

      do i = 1, n							!Preparation
         row(i) = i
         col(i) = i
         x(i) = y(i)
      enddo

      np2 = n + 2							!Successive reduction from n equations in n unknowns
      if ( n.ge.2 ) then							! to n-1 equations in n-1 unknowns

         do n50 = 2, n							!Already one equation in one unknown
            k = np2 - n50

            aminv = 1.0e-15						!Reduce equation set from k equations in k unknowns
            amaxv = 1.0e10						!to k-1 equations in k-1 unknowns

            ik = row(k)							!First find largest element for pivot
            jk = col(k)
            amx = a(ik,jk)
            ip = k
            iq = k
            do i = 1, k
               ik = row(i)
               if ( abs(x(ik)).gt.amaxv ) then
                  amaxv = abs(x(ik))
                  aminv = max(1.0e-15,1.0e-25*amaxv)
               endif
               do j = 1, k
                  jk = col(j)
                  if ( amx.lt.a(ik,jk) ) then
                     ip = i
                     iq = j
                     amx = a(ik,jk)
                  endif
               enddo
            enddo
            if ( abs(amx).lt.aminv ) amx = sign(aminv,amx)

            ik = row(ip)						!Now have largest element a(ip,iq)
            row(ip) = row(k)						! "Interchange" rows and columns
            row(k) = ik
            jk = col(iq)
            col(iq) = col(k)
            col(k) = jk

            do i = 1, k							!"Normalise" current equations in variable to be eliminated
               ia = row(i)
               x(ia) = x(ia)/amx
               do j = 1, k
                  ja = col(j)
                  a(ia,ja) = a(ia,ja)/amx
               enddo
            enddo

            do i = 1, k - 1						!Eliminate kth unknown
               ia = row(i)
               factor = a(ia,jk)
	       afactor = abs(factor)
               if ( afactor.lt.1.0e-8 ) factor = sign ( 1.0e-8, factor )
               if ( afactor.gt.1.0e8 )  factor = sign ( 1.0e8, factor )
               x(ia) = x(ia) - factor*x(ik)
               do j = 1, k - 1
                  ja = col(j)
                  a(ia,ja) = a(ia,ja) - factor*a(ik,ja)
               enddo
            enddo

         enddo

      endif

      ia = row(1)							!Find first solution
      ja = col(1)
      if ( abs(a(ia,ja)).lt.1.0e-20 ) a(ia,ja) = sign(1.0e-20,a(ia,ja))
      if ( abs(x(ia)).gt.1.0e10 ) x(ia) = sign(1.0e10,x(ia))
      x(ia) = x(ia)/a(ia,ja)

      if ( n.ge.2 ) then						!Substitute to find other solutions
         do i = 2, n
            ia = row(i)
            do j = 1, i-1
               ja = col(j)
               ik = row(j)
               if ( abs(x(ik)).gt.1.0e12 ) x(ik) = sign(1.0e12,x(ik))
               if ( abs(a(ia,ja)).gt.1.0e12 ) a(ia,ja) =
     +                                        sign(1.0e12,a(ia,ja))
               x(ia) = x(ia) - a(ia,ja)*x(ik)
            enddo
         enddo
      endif

      do i = 1, n							!Put results in correct order
         a(i,1) = x(i)
      enddo
      do i = 1, n
         ia = row(i)
         ja = col(i)
         x(ja) = a(ia,1)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMOOTHR -- Smooth a real array with a rectangular 'local mean' filter
C  Smooth each line by running a local mean filter through it. Store the
C  result in workspace, then repeat the process down the image columns.
C
C  Taken from EDRS of R Warren-Smith
C  alan penny                  ral                  1990-05-022

      subroutine smoothr ( ia, nx, ny, ix, iy, nmin,
     +                     istor, nstor, iline, nline )

      implicit none
      include 'STARMAN_INC'

      integer   nx			!i: X size of array
      integer   ny			!i: Y size of array
      real	ia(nx,ny)		!i/o: Array
      integer   ix			!i: X size of smoothing rectangle
      integer   iy			!i: Y size of smoothing rectangle
      integer   nmin			!i: Min no of valid pixels required in
					!   smoothing area
      real	istor(nx,ny)		!w: Workspace
      integer	nstor(nx,ny)		!w: Workspace
      real	iline(nx)		!w: Workspace
      integer   nline(nx)		!w: Workspace
C--
      integer oldx, newx, oldy, newy, thresh, nx2, ny2, idx, idy,
     +        iside, jside, i, ii, j, jj, nsum
      real sum
Cbegin


      if ( ST_FAILED ) return

      nx2 = 2*nx
      ny2 = 2*ny

      idx = max(0,ix/2)							!Make box sizes next largest odd number and positive
      iside = 2*idx+1
      idy = max(0,iy/2)
      jside = 2*idy+1

      thresh = min(max(1,nmin),iside*jside)				!Set threshold for no. of good pixels
									! in box between 1 and max possible number

      do j = 1, ny							!First smooth along each line
									!----------------------------

	 sum = 0.0							!initiallise running sum of data (isum) and no. of
	 nsum = 0 							! good pixels (nsum)

	 do ii = -idx, idx						!Start with box size iside*1 centred on pixel (0,j)
	    i = ii

            if ( i.lt.1 ) i = 2 - i					!Box will project off ends of lines,
	    if ( i.gt.nx )  i = nx2 - i					! so reflect to keep it inside


            i = min(max(1,i),nx)					!Protect against extreme cases where box is so big it
									! goes off opposite end after reflection

            sum = sum + ia(i,j)					! it to sum of pixels within box
            nsum = nsum + 1

         enddo

         do i = 1, nx							!Now step the box along the line

	    oldx = i - idx - 1						!Find position of old pixel to be removed from left
	    newx = i + idx						! and new pixel to be added at right

	    if ( oldx.lt.1 ) oldx = 2 - oldx				!Reflect at ends of line
            if ( oldx.gt.nx ) oldx = nx2 - oldx
            oldx = min(max(1,oldx),nx)
            if ( newx.lt.1 ) newx = 2-newx
            if ( newx.gt.nx ) newx = nx2-newx
            newx = min(max(1,newx),nx)

            sum = sum - ia(oldx,j)
            nsum = nsum - 1

            sum = sum + ia(newx,j)
            nsum = nsum + 1

            istor(i,j) = sum						!store sums along line in workspace
            nstor(i,j) = nsum

         enddo

      enddo
									!Now smooth down columns
									!-----------------------

      do i = 1, nx							!Initiallise sums.. this time
         iline(i) = 0.0							! processing a whole line at once
         nline(i) = 0
      enddo

      do jj = -idy, idy							!Sum over a box of size 1*jside centred on pixel (i,0),
         j = jj								! where i scans along whole line

         if ( j.lt.1 ) j = 2 - j					!Reflect at top and bottom of image
         if ( j.gt.ny ) j = ny2 - j
         j = min(max(1,j),ny)

         do i = 1, nx							!Form a sum for each pixel in the line from the
            iline(i) = iline(i) + istor(i,j)				! data now stored in the workspace
            nline(i) = nline(i) + nstor(i,j)
         enddo

      enddo

      do j = 1, ny							!Now step down the image

         oldy = j- idy - 1						!Find location of old line to subtract at top
         newy = j + idy							! and new line to add at bottom

         if ( oldy.lt.1 ) oldy = 2 - oldy				!Reflect at top and bottom of image
         if ( oldy.gt.ny ) oldy = ny2 - oldy
         oldy = min(max(1,oldy),ny)
         if ( newy.lt.1 ) newy = 2 - newy
         if ( newy.gt.ny ) newy = ny2 - newy
         newy = min(max(1,newy),ny)

         do i = 1, nx							!Take off old line
            iline(i) = iline(i) - istor(i,oldy)
            nline(i) = nline(i) - nstor(i,oldy)

            iline(i) = iline(i) + istor(i,newy)				!Add new line
            nline(i) = nline(i) + nstor(i,newy)

            if ( nline(i).ge.thresh ) then				!If sufficient pixels present, form
               ia(i,j) = iline(i)/real(nline(i))			! output, otherwise output is not valid
            else
               ia(i,j) = 0.0
            endif
         enddo

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMOOTHBR -- Smooth a real flagged array with rectangular 'local mean' filter
C  Smooth each line by running a local mean filter through it. Store the
C  result in workspace, then repeat the process down the image columns. Take
C  account of invalid pixels.
C
C  Taken from EDRS of R Warren-Smith
C  alan penny                  ral                  1990-05-022

      subroutine smoothbr ( ra, nx, ny, rinval, ix, iy, nmin,
     +			    ristor, rnstor, riline, rnline )

      implicit none
      include 'STARMAN_INC'

      integer   nx			!i: X size of array
      integer   ny			!i: Y size of array
      real      ra(nx,ny)		!i/o: Array
      real      rinval			!i: Invalid pixel magic value flag
      integer   ix			!i: X size of smoothing rectangle
      integer   iy			!i: Y size of smoothing rectangle
      integer   nmin			!i: Min no of valid pixels required
					!   in smoothing area
      real      ristor(nx,ny)		!w: Workspace
      real      rnstor(nx,ny)		!w: Workspace
      real      riline(nx)		!w: Workspace
      real      rnline(nx)		!w: Workspace
C--
      integer oldx, newx, oldy, newy, thresh, nx2, ny2, idx, idy,
     +        iside, jside, i, ii, j, jj
      real sum, rnsum
Cbegin


      if ( ST_FAILED ) return

      nx2 = 2*nx
      ny2 = 2*ny

      idx = max(0,ix/2)							!Make box sizes next largest odd number and positive
      iside = 2*idx+1
      idy = max(0,iy/2)
      jside = 2*idy+1

      thresh = min(max(1,nmin),iside*jside)				!Set threshold for no. of good pixels
									! in box between 1 and max possible number

      do j = 1, ny							!First smooth along each line
									!----------------------------

	 sum = 0.0							!initiallise running sum of data (isum) and no. of
	 rnsum = 0.0 							! good pixels (nsum)

	 do ii = -idx, idx						!Start with box size iside*1 centred on pixel (0,j)
	    i = ii

            if ( i.lt.1 ) i = 2 - i					!Box will project off ends of lines,
	    if ( i.gt.nx )  i = nx2 - i					! so reflect to keep it inside


            i = min(max(1,i),nx)					!Protect against extreme cases where box is so big it
									! goes off opposite end after reflection

            if ( ra(i,j).ne.rinval ) then				!If pixel found is valid, add
               sum = sum + ra(i,j)					! it to sum of pixels within box
               rnsum = rnsum + 1.0
            endif

         enddo

         do i = 1, nx							!Now step the box along the line

	    oldx = i - idx - 1						!Find position of old pixel to be removed from left
	    newx = i + idx						! and new pixel to be added at right

	    if ( oldx.lt.1 ) oldx = 2 - oldx				!Reflect at ends of line
            if ( oldx.gt.nx ) oldx = nx2 - oldx
            oldx = min(max(1,oldx),nx)
            if ( newx.lt.1 ) newx = 2-newx
            if ( newx.gt.nx ) newx = nx2-newx
            newx = min(max(1,newx),nx)

            if ( ra(oldx,j).ne.rinval ) then				!If old pixel is valid, subtract from sums
               sum = sum - ra(oldx,j)
               rnsum = rnsum - 1.0
            endif

	    if ( ra(newx,j).ne.rinval ) then				!If new pixel is valid, add to sums
	       sum = sum + ra(newx,j)
	       rnsum = rnsum + 1.0
	    endif

            ristor(i,j) = sum						!store sums along line in workspace
            rnstor(i,j) = rnsum

         enddo

      enddo
									!Now smooth down columns
									!-----------------------

      do i = 1, nx							!Initiallise sums.. this time
         riline(i) = 0.0						! processing a whole line at once
         rnline(i) = 0.0
      enddo

      do jj = -idy, idy							!Sum over a box of size 1*jside centred on pixel (i,0),
         j = jj								! where i scans along whole line

         if ( j.lt.1 ) j = 2 - j					!Reflect at top and bottom of image
         if ( j.gt.ny ) j = ny2 - j
         j = min(max(1,j),ny)

         do i = 1, nx							!Form a sum for each pixel in the line from the
            riline(i) = riline(i) + ristor(i,j)				! data now stored in the workspace
            rnline(i) = rnline(i) + rnstor(i,j)
         enddo

      enddo

      do j = 1, ny							!Now step down the image

         oldy = j - idy - 1						!Find location of old line to subtract at top
         newy = j + idy							! and new line to add at bottom

         if ( oldy.lt.1 ) oldy = 2 - oldy				!Reflect at top and bottom of image
         if ( oldy.gt.ny ) oldy = ny2 - oldy
         oldy = min(max(1,oldy),ny)
         if ( newy.lt.1 ) newy = 2 - newy
         if ( newy.gt.ny ) newy = ny2 - newy
         newy = min(max(1,newy),ny)

         do i = 1, nx							!Take off old line
            riline(i) = riline(i) - ristor(i,oldy)
            rnline(i) = rnline(i) - rnstor(i,oldy)

            riline(i) = riline(i) + ristor(i,newy)			!Add new line
            rnline(i) = rnline(i) + rnstor(i,newy)

            if ( rnline(i).ge.thresh ) then				!If sufficient pixels present, form
               ra(i,j) = riline(i)/rnline(i)				! output, otherwise output is not valid
            else
               ra(i,j) = rinval
            endif
         enddo

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SMOOTHBS -- Smooth a int*2 flagged array with rectangular 'local mean' filter
C  Smooth each line by running a local mean filter through it. Store the
C  result in workspace, then repeat the process down the image columns. Take
C  account of invalid pixels.
C
C  Taken from EDRS of R Warren-Smith
C  alan penny                  ral                  1990-05-022

      subroutine smoothbs ( ia, nx, ny, inval, ix, iy, nmin,
     +			    ristor, rnstor, riline, rnline )

      implicit none
      include 'STARMAN_INC'

      integer   nx			!i: X size of array
      integer   ny			!i: Y size of array
      integer*2 ia(nx,ny)		!i/o: Array
      integer   inval			!i: Invalid pixel magic value flag
      integer   ix			!i: X size of smoothing rectangle
      integer   iy			!i: Y size of smoothing rectangle
      integer   nmin			!i: Min no of valid pixels required
					!   in smoothing area
      real      ristor(nx,ny)		!w: Workspace
      real      rnstor(nx,ny)		!w: Workspace
      real      riline(nx)		!w: Workspace
      real      rnline(nx)		!w: Workspace
C--
      integer oldx, newx, oldy, newy, thresh, nx2, ny2, idx, idy,
     +        iside, jside, i, ii, j, jj
      real sum, rnsum
Cbegin


      if ( ST_FAILED ) return

      nx2 = 2*nx
      ny2 = 2*ny

      idx = max(0,ix/2)							!Make box sizes next largest odd number and positive
      iside = 2*idx+1
      idy = max(0,iy/2)
      jside = 2*idy+1

      thresh = min(max(1,nmin),iside*jside)				!Set threshold for no. of good pixels
									! in box between 1 and max possible number

      do j = 1, ny							!First smooth along each line
									!----------------------------

	 sum = 0.0							!initiallise running sum of data (isum) and no. of
	 rnsum = 0.0 							! good pixels (nsum)

	 do ii = -idx, idx						!Start with box size iside*1 centred on pixel (0,j)
	    i = ii

            if ( i.lt.1 ) i = 2 - i					!Box will project off ends of lines,
	    if ( i.gt.nx )  i = nx2 - i					! so reflect to keep it inside


            i = min(max(1,i),nx)					!Protect against extreme cases where box is so big it
									! goes off opposite end after reflection

            if ( ia(i,j).ne.inval ) then				!If pixel found is valid, add
               sum = sum + ia(i,j)					! it to sum of pixels within box
               rnsum = rnsum + 1.0
            endif

         enddo

         do i = 1, nx							!Now step the box along the line

	    oldx = i - idx - 1						!Find position of old pixel to be removed from left
	    newx = i + idx						! and new pixel to be added at right

	    if ( oldx.lt.1 ) oldx = 2 - oldx				!Reflect at ends of line
            if ( oldx.gt.nx ) oldx = nx2 - oldx
            oldx = min(max(1,oldx),nx)
            if ( newx.lt.1 ) newx = 2-newx
            if ( newx.gt.nx ) newx = nx2-newx
            newx = min(max(1,newx),nx)

            if ( ia(oldx,j).ne.inval ) then				!If old pixel is valid, subtract from sums
               sum = sum - ia(oldx,j)
               rnsum = rnsum - 1.0
            endif

	    if ( ia(newx,j).ne.inval ) then				!If new pixel is valid, add to sums
	       sum = sum + ia(newx,j)
	       rnsum = rnsum + 1.0
	    endif

            ristor(i,j) = sum						!store sums along line in workspace
            rnstor(i,j) = rnsum

         enddo

      enddo
									!Now smooth down columns
									!-----------------------

      do i = 1, nx							!Initiallise sums.. this time
         riline(i) = 0.0						! processing a whole line at once
         rnline(i) = 0.0
      enddo

      do jj = -idy, idy							!Sum over a box of size 1*jside centred on pixel (i,0),
         j = jj								! where i scans along whole line

         if ( j.lt.1 ) j = 2 - j					!Reflect at top and bottom of image
         if ( j.gt.ny ) j = ny2 - j
         j = min(max(1,j),ny)

         do i = 1, nx							!Form a sum for each pixel in the line from the
            riline(i) = riline(i) + ristor(i,j)				! data now stored in the workspace
            rnline(i) = rnline(i) + rnstor(i,j)
         enddo

      enddo

      do j = 1, ny							!Now step down the image

         oldy = j - idy - 1						!Find location of old line to subtract at top
         newy = j + idy							! and new line to add at bottom

         if ( oldy.lt.1 ) oldy = 2 - oldy				!Reflect at top and bottom of image
         if ( oldy.gt.ny ) oldy = ny2 - oldy
         oldy = min(max(1,oldy),ny)
         if ( newy.lt.1 ) newy = 2 - newy
         if ( newy.gt.ny ) newy = ny2 - newy
         newy = min(max(1,newy),ny)

         do i = 1, nx							!Take off old line
            riline(i) = riline(i) - ristor(i,oldy)
            rnline(i) = rnline(i) - rnstor(i,oldy)

            riline(i) = riline(i) + ristor(i,newy)			!Add new line
            rnline(i) = rnline(i) + rnstor(i,newy)

            if ( rnline(i).ge.thresh ) then				!If sufficient pixels present, form
               ia(i,j) = riline(i)/rnline(i)				! output, otherwise output is not valid
            else
               ia(i,j) = inval
            endif
         enddo

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SORT1I -- Sort of an integer vector
C  Depending on the number of values, calls the fastest
C  routine. Works on integers, into ascending order.
C
C    a j penny      ral                     1988-07-01

      subroutine sort1i ( ia, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      integer	ia(n)		!i/o: Numbers to be sorted
C--
Cbegin


      if ( ST_FAILED ) return

      if ( n.lt.50 ) then
         call srt1pi ( ia, n )		! Straight insertion
      else if ( n.lt.1000 ) then
         call srt1si ( ia, n )		! Shell's method
      else
         call srt1hi ( ia, n )		! Heap sort
      endif

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SORT1R -- Sort of a real vector
C  Depending on the number of values, calls the fastest
C  routine. Works on reals, into ascending order.
C
C    a j penny      ral                     1988-07-01

      subroutine sort1r ( a, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      real 	a(n)		!i/o: Numbers to be sorted
C--
Cbegin


      if ( ST_FAILED ) return

      if ( n.lt.50 ) then
         call srt1pr ( a, n )		! Straight insertion
      else if ( n.lt.1000 ) then
         call srt1sr ( a, n )		! Shell's method
      else
         call srt1hr ( a, n )		! Heap sort
      endif

      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SORT2I -- Sort of one integer array, with alignment of another
C          Depending on the number of values, calls the fastest
C          routine. Works on integers, into ascending order.
C
C    a j penny      ral                     1988-07-01

      subroutine sort2i ( ia, ib, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      integer	ia(n)		!i/o: Numbers to be sorted
      integer	ib(n)		!i/o: Numbers to follow ia
C--
Cbegin


      if ( ST_FAILED ) return

      if ( n.lt.50 ) then
         call srt2pi ( ia, ib, n )		! Straight insertion
      else if ( n.lt.1000 ) then
         call srt2si ( ia, ib, n )		! Shell's method
      else
         call srt2hi ( ia, ib, n )		! Heap sort
      endif

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SORT2R -- Sort of one real array, with alignment of another
C           Depending on the number of values, calls the fastest
C           routine. Works on reals, into ascending order.
C
C    a j penny      ral                     1988-07-01

      subroutine sort2r ( a, b, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      real 	a(n)		!i/o: Numbers to be sorted
      real	b(n)		!i/o: Numbers to follow ia
C--
Cbegin


      if ( ST_FAILED ) return

      if ( n.lt.50 ) then
         call srt2pr ( a, b, n )		! Straight insertion
      else if ( n.lt.1000 ) then
         call srt2sr ( a, b, n )		! Shell's method
      else
         call srt2hr ( a, b, n )		! Heap sort
      endif

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SRT1HI -- Heapsort to sort integers into ascending order
C
C   a j penny                 dao           1988-04-25

      subroutine srt1hi ( ia, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      integer	ia(n)		!i/o: Numbers to be sorted
C--
      integer l, ir, iia, i, j
      logical loop
Cbegin


      if ( ST_FAILED ) return

      l = n/2 + 1
      ir = n
      do while ( .true. )
         if ( l.gt.1 ) then
            l = l - 1
            iia = ia(l)
         else
            iia = ia(ir)
            ia(ir) = ia(1)
            ir =ir - 1
            if ( ir.eq.1 ) then
               ia(1) = iia
               return
            endif
         endif
         i = l
         j = l + l
         loop = .true.
         do while ( loop )
            if ( j.le.ir ) then
               if ( j.lt.ir ) then
                  if ( ia(j).lt.ia(j+1) ) j = j + 1
               endif
               if ( iia.lt.ia(j) ) then
                  ia(i) = ia(j)
                  i = j
                  j = j + j
               else
                  j = ir + 1
               endif
            else
               loop = .false.
               ia(i) = iia
            endif
         enddo
      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SRT1HR -- Heapsort to sort reals into ascending order
C
C   a j penny                 dao           1988-04-25

      subroutine srt1hr ( a, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      real	a(n)		!i/o: Numbers to be sorted
C--
      integer l, ir, i, j
      real ra
      logical loop
Cbegin


      if ( ST_FAILED ) return

      l = n/2 + 1
      ir = n
      do while ( .true. )
         if ( l.gt.1 ) then
            l = l - 1
            ra = a(l)
         else
            ra = a(ir)
            a(ir) = a(1)
            ir =ir - 1
            if ( ir.eq.1 ) then
               a(1) = ra
               return
            endif
         endif
         i = l
         j = l + l
         loop = .true.
         do while ( loop )
            if ( j.le.ir ) then
               if ( j.lt.ir ) then
                  if ( a(j).lt.a(j+1) ) j = j + 1
               endif
               if ( ra.lt.a(j) ) then
                  a(i) = a(j)
                  i = j
                  j = j + j
               else
                  j = ir + 1
               endif
            else
               loop = .false.
               a(i) = ra
            endif
         enddo
      enddo


      end




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SRT1PI -- Straight insertion sort to sort integers into ascending order
C
C   a j penny                 dao           1988-04-25

      subroutine srt1pi ( ia, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      integer	ia(n)		!i/o: Numbers to be sorted
C--
      integer i, j
      integer ja
Cbegin


      if ( ST_FAILED ) return

      do j = 2, n
         ja = ia(j)
         i = j - 1
         do while ( i.ge.1 .and. ia(i).gt.ja )
            ia(i+1) = ia(i)
            i = i - 1
         enddo
         ia(i+1) = ja
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SRT1PR -- Straight insertion sort to sort reals into ascending order
C
C   a j penny                 dao           1988-04-25

      subroutine srt1pr ( a, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      real	a(n)		!i/o: Numbers to be sorted
C--
      integer i, j
      real ra
Cbegin


      if ( ST_FAILED ) return

      do j = 2, n
         ra = a(j)
         i = j - 1
         do while ( i.ge.1 .and. a(i).gt.ra )
            a(i+1) = a(i)
            i = i - 1
         enddo
         a(i+1) = ra
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SRT1SI -- Shell's method insertion to sort integers into ascending order
C
C   a j penny                 dao           1988-04-25

      subroutine srt1si ( ia, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      integer	ia(n)		!i/o: Numbers to be sorted
C--
      integer i, j, k, lognb2, m, l, nn
      real it, aln2i, tiny
      parameter ( aln2i=1.0/0.69314718 )
      parameter ( tiny=1.0e-5 )
Cbegin


      if ( ST_FAILED ) return

      lognb2 = int(alog(real(n)*aln2i+tiny))
      m = n
      do nn = 1, lognb2
         m = m/2
         k = n - m
         do j = 1, k
            i = j
            l = i + m
            do while ( i.ge.1 .and. ia(l).lt.ia(i) )
               it = ia(i)
               ia(i) = ia(l)
               ia(l) = it
               i = i - m
               l = i + m
            enddo
         enddo
      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SRT1SR -- Shell's method insertion to sort reals into ascending order
C
C   a j penny                 dao           1988-04-25

      subroutine srt1sr ( a, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      real	a(n)		!i/o: Numbers to be sorted
C--
      integer i, j, k, lognb2, m, l, nn
      real t, aln2i, tiny
      parameter ( aln2i=1.0/0.69314718 )
      parameter ( tiny=1.0e-5 )
Cbegin


      if ( ST_FAILED ) return

      lognb2 = int(alog(real(n)*aln2i+tiny))
      m = n
      do nn = 1, lognb2
         m = m/2
         k = n - m
         do j = 1, k
            i = j
            l = i + m
            do while ( i.ge.1 .and. a(l).lt.a(i) )
               t = a(i)
               a(i) = a(l)
               a(l) = t
               i = i - m
               l = i + m
            enddo
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SRT2HI -- Heapsort to sort integers with alignment of another array
C (ascending order)
C
C   a j penny                 dao           1988-04-25

      subroutine srt2hi ( ia, ib, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      integer	ia(n)		!i/o: Numbers to be sorted
      integer	ib(n)		!i/o: Numbers to follow ia
C--
      integer l, ir, iia, iib, i, j
      logical loop
Cbegin


      if ( ST_FAILED ) return

      l = n/2 + 1
      ir = n
      do while ( .true. )
         if ( l.gt.1 ) then
            l = l - 1
            iia = ia(l)
            iib = ib(l)
         else
            iia = ia(ir)
            iib = ib(ir)
            ia(ir) = ia(1)
            ib(ir) = ib(1)
            ir =ir - 1
            if ( ir.eq.1 ) then
               ia(1) = iia
               ib(1) = iib
               return
            endif
         endif
         i = l
         j = l + l
         loop = .true.
         do while ( loop )
            if ( j.le.ir ) then
               if ( j.lt.ir ) then
                  if ( ia(j).lt.ia(j+1) ) j = j + 1
               endif
               if ( iia.lt.ia(j) ) then
                  ia(i) = ia(j)
                  ib(i) = ib(j)
                  i = j
                  j = j + j
               else
                  j = ir + 1
               endif
            else
               loop = .false.
               ia(i) = iia
               ib(i) = iib
            endif
         enddo
      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SRT2HR -- Heapsort to sort real array with alignment of another array
C   (ascending order)
C
C   a j penny                 dao           1988-04-25

      subroutine srt2hr ( a, b, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      real	a(n)		!i/o: Numbers to be sorted
      real	b(n)		!i/o: Numbers to follow a
C--
      integer l, ir, i, j
      real ra, rb
      logical loop
Cbegin


      if ( ST_FAILED ) return

      l = n/2 + 1
      ir = n
      do while ( .true. )
         if ( l.gt.1 ) then
            l = l - 1
            ra = a(l)
            rb = b(l)
         else
            ra = a(ir)
            rb = b(ir)
            a(ir) = a(1)
            b(ir) = b(1)
            ir =ir - 1
            if ( ir.eq.1 ) then
               a(1) = ra
               b(1) = rb
               return
            endif
         endif
         i = l
         j = l + l
         loop = .true.
         do while ( loop )
            if ( j.le.ir ) then
               if ( j.lt.ir ) then
                  if ( a(j).lt.a(j+1) ) j = j + 1
               endif
               if ( ra.lt.a(j) ) then
                  a(i) = a(j)
                  b(i) = b(j)
                  i = j
                  j = j + j
               else
                  j = ir + 1
               endif
            else
               loop = .false.
               a(i) = ra
               b(i) = rb
            endif
         enddo
      enddo


      end




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SRT2PI -- Straight insertion sort of int array with alignment of another
C    (ascending order)
C
C   a j penny                 dao           1988-04-25

      subroutine srt2pi ( ia, ib, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      integer	ia(n)		!i/o: Numbers to be sorted
      integer	ib(n)		!i/o: Numbers to follow a
C--
      integer i, j
      integer ja, jb
Cbegin


      if ( ST_FAILED ) return

      do j = 2, n
         ja = ia(j)
         jb = ib(j)
         i = j - 1
         do while ( i.ge.1 .and. ia(i).gt.ja )
            ia(i+1) = ia(i)
            ib(i+1) = ib(i)
            i = i - 1
         enddo
         ia(i+1) = ja
         ib(i+1) = jb
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SRT2PR -- Straight insertion sort of real array  with alignment of another
C    (ascending order)
C
C   a j penny                 dao           1988-04-25

      subroutine srt2pr ( a, b, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      real	a(n)		!i/o: Numbers to be sorted
      real	b(n)		!i/o: Numbers to follow a
C--
      integer i, j
      real ra, rb
Cbegin


      if ( ST_FAILED ) return

      do j = 2, n
         ra = a(j)
         rb = b(j)
         i = j - 1
         do while ( i.ge.1 .and. a(i).gt.ra )
            a(i+1) = a(i)
            b(i+1) = b(i)
            i = i - 1
         enddo
         a(i+1) = ra
         b(i+1) = rb
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SRT2SI -- Shell's method insertion sort to sort integer array
C     (ascending order) with alignment of another array
C
C   a j penny                 dao           1988-04-25

      subroutine srt2si ( ia, ib, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      integer	ia(n)		!i/o: Numbers to be sorted
      integer	ib(n)		!i/o: Numbers to follow a
C--
      integer i, j, k, lognb2, m, l, nn
      real it, aln2i, tiny
      parameter ( aln2i=1.0/0.69314718 )
      parameter ( tiny=1.0e-5 )
Cbegin


      if ( ST_FAILED ) return

      lognb2 = int(alog(real(n))*aln2i+tiny)
      m = n
      do nn = 1, lognb2
         m = m/2
         k = n - m
         do j = 1, k
            i = j
            l = i + m
            do while ( i.ge.1 .and. ia(l).lt.ia(i) )
               it = ia(i)
               ia(i) = ia(l)
               ia(l) = it
               it = ib(i)
               ib(i) = ib(l)
               ib(l) = it
               i = i - m
               l = i + m
            enddo
         enddo
      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SRT2SR -- Shell's method insertion to sort real array
C    (ascending order) with alignment of another array
C
C   a j penny                 dao           1988-04-25

      subroutine srt2sr ( a, b, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i:   No of numbers to do
      real	a(n)		!i/o: Numbers to be sorted
      real	b(n)		!i/o: Numbers to follow a
C--
      integer i, j, k, lognb2, m, l, nn
      real t, aln2i, tiny
      parameter ( aln2i=1.0/0.69314718 )
      parameter ( tiny=1.0e-5 )
Cbegin


      if ( ST_FAILED ) return

      lognb2 = int(alog(real(n))*aln2i+tiny)
      m = n
      do nn = 1, lognb2
         m = m/2
         k = n - m
         do j = 1, k
            i = j
            l = i + m
            do while ( i.ge.1 .and. a(l).lt.a(i) )
               t = a(i)
               a(i) = a(l)
               a(l) = t
               t = b(i)
               b(i) = b(l)
               b(l) = t
               i = i - m
               l = i + m
            enddo
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ST_MINMAX -- Get max and min of flagged area of array
C
C  alan penny                ral                      1990-06-15

      subroutine st_minmax ( im, nx, ny, bs, bz, gtype, inval, rinval,
     +               nxs, nxe, nys, nye, amin, amax, kp, ngood, nbad )

      implicit none

      integer	nx		!i: Image X size
      integer	ny		!i: Image Y size
      integer*2 im(*)		!i: Image
      real	bs		!i: Image pixel value scale
      real	bz		!i: Image pixel value zero
      character*(*) gtype	!i: Image type ('REAL', 'SHORT' )
      integer	inval		!i: Integer*2 Image pixel value invalid flag
      real      rinval		!i: Real Image pixel value invalid flag
      integer	nxs		!i: Area X start
      integer	nxe		!i: Area Y end
      integer	nys		!i: Area X start
      integer	nye		!i: Area Y end
      real	amin		!o: Minimum value (scaled,zeroed)
      real	amax		!o: Maximum value (scaled,zeroed)
      integer   kp(4)		!o: Locations of min, max (x,y;x,y)
      integer	ngood		!o: Number of valid pixels
      integer	nbad		!o: Number of invalid pixels
C--
Cbegin


      if ( gtype.eq.'SHORT' ) then
         call st_minmaxs ( im, nx, ny, bs, bz, inval, nxs, nxe,
     +                     nys, nye, amin, amax, kp, ngood, nbad )
      else
         call st_minmaxr ( im, nx, ny, bs, bz, rinval, nxs, nxe,
     +                     nys, nye, amin, amax, kp, ngood, nbad )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ST_MINMAXR -- Get max and min of flagged area of real array
C
C  alan penny                ral                      1990-06-15

      subroutine st_minmaxr ( im, nx, ny, bs, bz, rinval,
     +               nxs, nxe, nys, nye, amin, amax, kp, ngood, nbad )

      implicit none

      integer	nx		!i: Image X size
      integer	ny		!i: Image Y size
      real      im(nx,ny)	!i: Image
      real	bs		!i: Image pixel value scale
      real	bz		!i: Image pixel value zero
      real      rinval		!i: Image pixel value invalid flag
      integer	nxs		!i: Area X start
      integer	nxe		!i: Area Y end
      integer	nys		!i: Area X start
      integer	nye		!i: Area Y end
      real	amin		!o: Minimum value (scaled,zeroed)
      real	amax		!o: Maximum value (scaled,zeroed)
      integer   kp(4)		!o: Locations of min, max (x,y;x,y)
      integer	ngood		!o: Number of valid pixels
      integer	nbad		!o: Number of invalid pixels
C--
      integer j, k, knxs, knxe, knys, knye, iv
      real    rv
Cbegin


      amax = -1.0e20
      amin = 1.0e20
      call azeroi ( kp, 4 )
      ngood = 0
      nbad = 0

      knxs = max(1,min(nx,nxs))
      knxe = max(1,min(nx,nxe))
      knys = max(1,min(ny,nys))
      knye = max(1,min(ny,nye))
      call cswopi ( knxs, knxe )
      call cswopi ( knys, knye )

      do k = knys, knye
         do j = knxs, knxe
            rv = im(j,k)
            if ( rv.ne.rinval ) then
               ngood = ngood + 1
               if ( rv.gt.amax ) then
                  amax = rv
                  kp(3) = j
                  kp(4) = k
               endif
               if ( rv.lt.amin ) then
                  amin = rv
                  kp(1) = j
                  kp(2) = k
               endif
            else
               nbad = nbad + 1
            endif
         enddo
      enddo

      amax = amax*bs + bz
      amin = amin*bs + bz
      if ( amin.gt.amax ) then
         rv = amin
         amin = amax
         amax = rv
         iv = kp(3)
         kp(3) = kp(1)
         kp(1) = iv
         iv = kp(4)
         kp(4) = kp(2)
         kp(2) = iv
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ST_MINMAXS -- Get max and min of flagged area of array
C
C  alan penny                ral                      1990-06-15

      subroutine st_minmaxs ( im, nx, ny, bs, bz, inval,
     +               nxs, nxe, nys, nye, amin, amax, kp, ngood, nbad )

      implicit none

      integer	nx		!i: Image X size
      integer	ny		!i: Image Y size
      integer*2 im(nx,ny)	!i: Image
      real	bs		!i: Image pixel value scale
      real	bz		!i: Image pixel value zero
      integer	inval		!i: Image pixel value invalid flag
      integer	nxs		!i: Area X start
      integer	nxe		!i: Area Y end
      integer	nys		!i: Area X start
      integer	nye		!i: Area Y end
      real	amin		!o: Minimum value (scaled,zeroed)
      real	amax		!o: Maximum value (scaled,zeroed)
      integer   kp(4)		!o: Locations of min, max (x,y;x,y)
      integer	ngood		!o: Number of valid pixels
      integer	nbad		!o: Number of invalid pixels
C--
      integer j, k, iv, knxs, knxe, knys, knye
      real    rv
Cbegin


      amax = -1.0e20
      amin = 1.0e20
      call azeroi ( kp, 4 )
      ngood = 0
      nbad = 0

      knxs = max(1,min(nx,nxs))
      knxe = max(1,min(nx,nxe))
      knys = max(1,min(ny,nys))
      knye = max(1,min(ny,nye))
      call cswopi ( knxs, knxe )
      call cswopi ( knys, knye )

      do k = knys, knye
         do j = knxs, knxe
            iv = im(j,k)
            if ( iv.ne.inval ) then
               ngood = ngood + 1
               if ( real(iv).gt.amax ) then
                  amax = real(iv)
                  kp(3) = j
                  kp(4) = k
               endif
               if ( real(iv).lt.amin ) then
                  amin = real(iv)
                  kp(1) = j
                  kp(2) = k
               endif
            else
               nbad = nbad + 1
            endif
         enddo
      enddo

      amax = amax*bs + bz
      amin = amin*bs + bz
      if ( amin.gt.amax ) then
         rv = amin
         amin = amax
         amax = rv
         iv = kp(3)
         kp(3) = kp(1)
         kp(1) = iv
         iv = kp(4)
         kp(4) = kp(2)
         kp(2) = iv
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TRAN_DOIT -- Transfromation between two sets of x,y posns - set up
C
C    a j penny               rgo                      82-11-4

      subroutine tran_doit ( xy1, xy2, num, numa, ktype, siglim, ok,
     +                       trc, ngood, sd )

      implicit none
      include 'STARMAN_INC'

      integer   num             !i: Size of table files
      real	xy1(num,2)	!i: 1st input table positions
      real	xy2(num,2)	!i: 2nd input table positions
      integer   numa            !i: Number of points to look at, starting from top
      integer	ktype		!i: Tranformation type (1=shift;2=shift+rotation;
                                !   3=shift+rotn+mag;4=full )
      real      siglim		!i: Limit to misfit allowed
      logical   ok(num)		!o: Work space for use
      real	trc(6)		!o: Calculated transformation
      integer   ngood		!o: Number of good points
      real      sd		!o: Std dev of points from fit
C--
      integer k, ierr, maxk
      logical more
      double precision xd, yd, dx, dy, errmax, errsq, varlim,
     +                 s, ss, sn, dss
Cbegin


      if ( ST_FAILED ) return

      do k = 1, numa
         ok(k) = .true.
      enddo

      more = .true.
      do while ( more )

         call tran_lintran ( xy1(1,1), xy1(1,2), xy2(1,1),
     +                       xy2(1,2), ok, num, numa, ktype, trc,
     +                       ierr )

         if ( ierr.ne.0 ) then
            more = .false.
         else
            errmax = 0.0d0
            maxk = 0
            s = 0.0d0
            ss = 0.0d0
            sn = 0.0d0
            do k = 1, numa
               if ( ok(k) ) then
                  xd = trc(1) + trc(2)*xy1(k,1) + trc(3)*xy1(k,2)
                  yd = trc(4) + trc(5)*xy1(k,1) + trc(6)*xy1(k,2)
                  dx = dble(xy2(k,1)) - xd
                  dy = dble(xy2(k,2)) - yd
                  errsq = dx*dx + dy*dy
                  s = s + dsqrt(errsq)
                  ss = ss + errsq
                  sn = sn + 1.0d0
                  if ( errsq.gt.errmax ) then
                     errmax = errsq
                     maxk = k
                  endif
               endif
            enddo

            sd = 0.0
            if ( sn.gt.1.0d0 ) then
               dss = ( ss-(s*s/sn))/(sn-1.0d0)
               if ( dss.gt.1.0d-20 ) sd = dsqrt(dss)
            endif

            ngood = nint(sn)
            if ( ngood.gt.0 ) then
               varlim = siglim*siglim
               if ( ss.gt.0.0d0 .and. errmax.gt.((ss/sn)*varlim)) then
                  ok(maxk) = .false.
                  if ( ngood.le.2 ) more = .false.
               else
                  more = .false.
               endif
            endif

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TRAN_LINTRAN -- Transformation between points - calculate
C
C    a j penny               ral                      1991 may

      subroutine tran_lintran ( xa, ya, xb, yb, ok, na, n, jfit, c,
     +                          ierr )

      implicit none
      include 'STARMAN_INC'

      integer    na		!i: Size of vectors
      real       xa(na)		!i: 1st table X positions
      real       ya(na)		!i: 1st table Y positions
      real       xb(na)		!i: 2nd table X positions
      real       yb(na)		!i: 2nd table Y positions
      logical    ok(na)		!i: Use this pair?
      integer    n		!i: Number of positions
      integer    jfit		!i: Type of fit (1=shift;2=shift+rotation;
				!   3=shift+rotn+mag;4=full 6 coords)
      real       c(6)		!o: Transformation coordinates
      integer    ierr		!o: Error flag (0=ok;1=no points;2=no good points)
C--
      double precision a(4,4), b(4), sw, swx, swy, swxy, swx2, swy2,
     +                 swxd, swyd, swxxd, swyyd, swxyd, swyxd, wx,
     +                 wy, x0, y0, xd0, yd0, swxxd0, swyyd0, swxyd0,
     +                 swyxd0, bot, top, theta
      real  rra(16), rrb(4)
      logical solved
      integer j, k, ka, ifit, npts
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      ifit = jfit
      call azeror ( c, 6 )

      if ( n.lt.1 ) then						!No points
         ierr = 1
         return
      endif

      sw = 0.0d0							!Number of good points
      do k = 1, n
         if ( ok(k) ) sw = sw + 1.0d0
      enddo
      npts = nint(sw)

      if ( npts.le.0 ) then						!No good points
         ierr = 2
         return
      endif

      if ( npts.le.2 ) ifit = min(ifit,3)				!check degrees of freedom
      if ( npts.le.1 ) ifit = 1

      swx   = 0.0d0							!Initialise for normal eqns
      swy   = 0.0d0
      swxy  = 0.0d0
      swx2  = 0.0d0
      swy2  = 0.0d0
      swxd  = 0.0d0
      swyd  = 0.0d0
      swxxd = 0.0d0
      swyyd = 0.0d0
      swxyd = 0.0d0
      swyxd = 0.0d0

      do k = 1, n							!Form sums
         if ( ok(k) ) then

            swx  = swx + xa(k)
            swy  = swy + ya(k)
            swxd = swxd + xb(k)
            swyd = swyd + yb(k)

            if ( ifit.ne.1 ) then
               wx = xa(k)
               wy = ya(k)
               swxy  = swxy  + wx*ya(k)
               swx2  = swx2  + wx*xa(k)
               swy2  = swy2  + wy*ya(k)
               swxxd = swxxd + wx*xb(k)
               swxyd = swxyd + wx*yb(k)
               swyxd = swyxd + wy*xb(k)
               swyyd = swyyd + wy*yb(k)
            endif

         endif
      enddo

      solved = .false.							!Solve: If cant try a simpler fit
      do while ( ifit.ge.1 .and. .not.solved )

         if ( ifit.eq.1 ) then						!Shift only

            c(1) = (swxd-swx)/sw
            c(2) = 1.0
            c(3) = 0.0
            c(4) = (swyd-swy)/sw
            c(5) = 0.0
            c(6) = 1.0
            solved = .true.

         elseif ( ifit.eq.2 ) then					!Shift and rotation

            xd0 = swxd/sw						!Centroids
            yd0 = swyd/sw
            x0  = swx/sw
            y0  = swy/sw

            swyxd0 = 0.0d0						!Deviations from centroids
            swxyd0 = 0.0d0
            swxxd0 = 0.0d0
            swyyd0 = 0.0d0
            do k = 1, n
               if ( ok(k) ) then
                  swyxd0 = swyxd0 + (ya(k)-y0)*(xb(k)-xd0)
                  swxyd0 = swxyd0 + (xa(k)-x0)*(yb(k)-yd0)
                  swxxd0 = swxxd0 + (xa(k)-x0)*(xb(k)-xd0)
                  swyyd0 = swyyd0 + (ya(k)-y0)*(yb(k)-yd0)
               endif
            enddo

            top = swyxd0 - swxyd0					!Rotation angle not defined
            bot = swyyd0 + swxxd0
            if ( top.ne.0.0d0 .or. bot.ne.0.0d0 ) then
               theta = atan2(top,bot)					!Calc rotation angle
               c(1) = xd0 - (x0*cos(theta)+y0*sin(theta))
               c(2) = cos(theta)
               c(3) = sin(theta)
               c(4) = yd0 - (-x0*sin(theta)+y0*cos(theta))
               c(5) = -sin(theta)
               c(6) = cos(theta)
               solved = .true.
            endif

         elseif ( ifit.eq.3 ) then					!Shift, rotation and magnification

            a(1,1) = sw
            a(1,2) = swx
            a(1,3) = swy
            a(1,4) = 0.0d0
            a(2,1) = swx
            a(2,2) = swx2 + swy2
            a(2,3) = 0.0d0
            a(2,4) = swy
            a(3,1) = swy
            a(3,2) = 0.0d0
            a(3,3) = swx2 + swy2
            a(3,4) = -swx
            a(4,1) = 0.0d0
            a(4,2) = swy
            a(4,3) = -swx
            a(4,4) = sw

            b(1) = swxd
            b(2) = swxxd + swyyd
            b(3) = swyxd - swxyd
            b(4) = swyd

            call achtdr ( a, rra, 16 )
            call achtdr ( b, rrb, 4 )
            call simulx ( rrb, rra, c, 4 )
            c(5) = -1.0*c(3)
            c(6) = c(2)

            solved = .true.

         elseif ( ifit.eq.4 ) then					!Full fit

            a(1,1) = sw
            a(1,2) = swx
            a(1,3) = swy
            a(2,1) = swx
            a(2,2) = swx2
            a(2,3) = swxy
            a(3,1) = swy
            a(3,2) = swxy
            a(3,3) = swy2

            b(1) = swxd
            b(2) = swxxd
            b(3) = swyxd

            ka = 0
            do k = 1, 3
               do j = 1, 3
                  ka = ka + 1
                  rra(ka) = a(j,k)
               enddo
            enddo
            call achtdr ( b, rrb, 3 )
            call simulx ( rrb, rra, c, 3 )

            b(1) = swyd
            b(2) = swxyd
            b(3) = swyyd

            ka = 0
            do k = 1, 3
               do j = 1, 3
                  ka = ka + 1
                  rra(ka) = a(j,k)
               enddo
            enddo
            call achtdr ( b, rrb, 3 )
            call simulx ( rrb, rra, c(4), 3 )

            solved = .true.

         endif

         ifit = ifit - 1

      enddo


      end
