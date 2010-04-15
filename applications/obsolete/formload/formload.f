CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C FORMLOAD -- Loads entries into a LaTeX file.
C
C  a j penny              ral                   1993 Feb

      program formload

      implicit none
C--
Cbegin


      call defaul

      call option

      call templa

      call input

      call dopage

      call output

      call work

      call chclos


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DEFAUL -- Set up defaults
C
C  a j penny              ral                   1992 Oct

      subroutine defaul ()

      implicit none
      include 'formload.inc'
C--
Cbegin

      FAILED = .false.

      OPEN1  = .false.
      OPEN2  = .false.
      OPEN3  = .false.
      OPEN4  = .false.

      NQUEST = 0
      NSPEC = 0
      NPAGE = 0
      TTFILE = ' '
      INFILE = ' '
      PXSC = 1.0
      PYSC = 1.0
      PXZE = 0.0
      PYZE = 0.0

      TOPOFF = 0.0
      ODDOFF = 0.0
      EVEOFF = 0.0
      ETOPOF = 0.0
      EODDOF = 0.0
      EEVEOF = 0.0
      USEOFF = .false.

      PEROUT = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OPTION -- Get and Type out list of options
C
C  a j penny              ral                   1992 Oct

      subroutine option ()

      implicit none
      include 'formload.inc'
C--
      integer j, k, kk, ks, ksi, kse, ksl, kn, kl, klo, istat, nline
      logical there
      character*132 text, texta, ctype, optfil
      character*50 cntype(MAXFOR), ootype, idef
Cbegin


      if ( FAILED ) return

C  Get if VMS or UNIX

      call whatsy ( k )
      ISVMS = .false.
      if ( k.eq.1 ) ISVMS = .true.

C  Get directory where file are kept

      call genv ( 'FORMLOAD_DIR', text, k )
      if ( k.ne.0 ) then
         write ( 6, '(''  ERROR: Cant get FORMLOAD_DIR directory'')' )
         if ( ISVMS ) then
            write ( 6, '(''         Have you done the: ASSIGN '',
     +                ''dirname FORMLOAD_DIR ? '')' )
            write ( 6, '(''         Example:  ASSIGN [ajp.formload] '',
     +                   ''FORMLOAD_DIR '')' )
         else
            write ( 6, '(''         Have you done the: setenv '',
     +                ''FORMLOAD_DIR dirname? '')' )
            write ( 6, '(''         Example:  setenv FORMLOAD_DIR '',
     +                   ''/user1/ajp/formload '')' )
         endif
         FAILED = .true.
         return
      endif
      call charln ( text, klo )
      if ( klo.gt.0 ) then
         OPTDIR = text(1:klo)
      endif

C  Open options list

      optfil = 'options.dat'
      if ( ISVMS ) then
         if ( klo.ne.0 ) optfil = OPTDIR(1:klo)//'options.dat'
      else
         if ( klo.ne.0 ) optfil = OPTDIR(1:klo)//TEXTXA//'options.dat'
      endif
      call charln ( optfil, kl )
      call filopn ( 1, 4, ISVMS, optfil(1:kl), istat )
      if ( istat.eq.1 ) goto 998
      OPEN4 = .true.

C  Get and type list of options

      nline = 1
      read ( 4, '(a)', err=999, end=999 ) text
      ksi = index(text,'a')
      ks = index(text,'c')
      call charln ( text, kse )
      ksl = kse - ks + 1
      if ( ksl.gt.50 ) goto 992

      write ( 6, '(''  '')' )
      write ( 6, '(''   Choose which form to make:-'')' )
      write ( 6, '(''   Type                              Code '')' )
      write ( 6, '(''   ____                              ____ '')' )
      write ( 6, '(''   None                              None '')' )
      kn = 0
      idef = ' '
    1 continue
         nline = nline + 1
         read ( 4, '(a)', err=999, end=2 ) text
         call charln ( text, kl )
         if ( kl.eq.0 ) goto 2
         if ( text(1:1).eq.'y' ) then
            idef = text(ksi:ksi+ksl-1)
         else
            write ( 6, '(1x,a)' ) text(1:kl)
            kn = kn + 1
            cntype(kn) = text(ks:kse)
         endif
         goto 1
    2 continue
      NUMFOR = kn + 1
      cntype(NUMFOR) = 'OWN  '
      write ( 6, '(''   Users own template                Own'')' )
      write ( 6, '(''  '')' )

      close ( 4 )
      OPEN4 = .false.

C  Option list error

      if ( NUMFOR.gt.MAXFOR ) then
         write ( 6, '('' ERROR: Too many options in option list'',
     +                '' - Maximum = '', i5)' ) MAXFOR
         call wrerrf ( optfil, 0 )
         FAILED = .true.
         return
      endif

C  Get input option

      texta = 'Input code (Default - '//idef
      call charln ( texta, kl )
      text  = texta(1:kl)//') ? '
      call charln ( text, kl )
      call qwrite ( text(1:kl) )
      ctype = '     '
      read ( 5, '(a)' ) ctype
      call lbgone ( ctype )
      if ( ctype(1:ksl).eq.'     ' ) ctype = idef

C  Upper case input options

      do k = 1, ksl
         j = ichar(ctype(k:k))
         if ( j.ge.97 .and.  j.le.122 ) ctype(k:k) = char(j-32)
      enddo
      do kk = 1, NUMFOR
         ootype = cntype(kk)
         do k = 1, ksl
            j = ichar(ootype(k:k))
            if ( j.ge.97 .and.  j.le.122 ) ootype(k:k) = char(j-32)
         enddo
         cntype(kk) = ootype
      enddo

C  Get 'permitted' input option

      KTYPE = 'BADIN'
      if ( ctype(1:ksl).eq.'NONE ' ) then
         KTYPE = 'NONE '
      else
         do k = 1, NUMFOR
            if ( ctype(1:ksl).eq.cntype(k) ) KTYPE = cntype(k)
         enddo
      endif

C  No input

      if ( KTYPE.eq.'NONE ' ) then
         FAILED = .true.
         return
      endif

C  Bad input

      if ( KTYPE.eq.'BADIN' ) then
         write ( 6, '('' ERROR: Invalid Code '')' )
         FAILED = .true.
         return
      endif

C  Turn file name into lower case

      call lowerc ( ctype )

C  Get template file name

      if ( KTYPE.eq.'OWN  ' ) then

         call qwrite ( 'Template file name (Default - ) ?' )
         text = ' '
         read ( 5, '(a)' ) text
         call charln ( text, kl )
         if ( kl.eq.0 ) then
            write ( 6, '('' ERROR: Blank template file name'')' )
            FAILED = .true.
            return
         endif

         there = .false.
         k = 1
         if ( ISVMS ) then
            k = 1 + index(text(1:kl),']')
            if ( index(text(k:kl),'.').ne.0 ) there = .true.
         else
            do j = kl, 1, -1
               if ( .not.there .and. text(j:j).eq.'.' ) then
                  if ( j.eq.kl ) then
                     there = .true.
                  else
                     if ( text(j+1:j+1).ne.TEXTXA .and.
     +                    text(j+1:j+1).ne.'.' ) there = .true.
                  endif
               endif
            enddo
         endif
         if ( there ) then
            TTFILE = text(1:kl)
         else
            TTFILE = text(1:kl)//'.tex'
         endif

      else

         call charln ( ctype, kl )
         text = ctype(1:kl)//'_template.tex'
         call charln ( text, kl )
         TTFILE = text(1:kl)
         if ( klo.ne.0 ) then
            TTFILE = OPTDIR(1:klo)//TEXTXA//text(1:kl)
            if ( ISVMS ) TTFILE = OPTDIR(1:klo)//text(1:kl)
         endif

      endif

C  Get default input file

      TIFILE = ' '
      TPFILE = ' '
      if ( KTYPE.ne.'OWN  ' ) then
         call charln ( ctype, kl )
         text = ctype(1:kl)//'_sample'
         call charln ( text, kl )

         TPFILE = '$FORMLOAD_DIR'//TEXTXA//text(1:kl)
         if ( ISVMS ) then
            do k = 1, kl
               j = ichar(text(k:k))
               if ( j.ge.97 .and.  j.le.122 ) text(k:k) = char(j-32)
            enddo
            TPFILE = 'FORMLOAD_DIR:'//text(1:kl)
         endif

         TIFILE = text(1:kl)
         if ( klo.ne.0 ) then
            TIFILE = OPTDIR(1:klo)//TEXTXA//text(1:kl)
            if ( ISVMS ) TIFILE = OPTDIR(1:klo)//text(1:kl)
         endif

      endif

C  Return

      goto 996
  992 write ( 6, '(1x, ''ERROR: Code names more than 50'',
     +                 '' characters long in list of options'')' )
      call wrerrf ( optfil, nline )
      FAILED = .true.

      goto 996
  999 write ( 6, '('' ERROR: in list of options'')' )
      call wrerrf ( optfil, nline )
      FAILED = .true.

      goto 996
  998 write ( 6, '('' ERROR: Cant find the file with the'',
     +             '' list of options'')' )
      call wrerrf ( optfil, 0 )
      FAILED = .true.

  996 continue


      end




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C LOWERC -- Turn text into lower case
C
C  a j penny              ral                   1992 Feb

      subroutine lowerc ( text )

      implicit none
      include 'formload.inc'

C!i/o:   Input/output text string
      character*(*) text
C--
      integer j, k, kl
Cbegin


      call charln ( text, kl )

      do k = 1, kl
         j = ichar(text(k:k))
         if ( j.ge.65 .and.  j.le.90 ) text(k:k) = char(j+32)
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TEMPLA -- Loads the .tex files needed for the LaTeX forms
C
C  a j penny              ral                   1992 Feb

      subroutine templa ( )

      implicit none
      include 'formload.inc'
C--
      integer istat, kl
Cbegin


      if ( FAILED ) return

      OPEN3 = .true.
      call charln ( TTFILE, kl )
      call filopn ( 1, 3, ISVMS, TTFILE(1:kl), istat )
      if ( istat.eq.1 ) goto 996

      call getpag

      call getofa

      call getofb

      call getque

      call getspe

      call messag

      call getscl

      goto 999
  996 write ( 6, '('' ERROR: Cant open that template file'')' )
      call wrerrf ( TTFILE, 0 )
      FAILED = .true.
  999 continue


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETPAG -- Get page information
C
C  a j penny              ral                   1992 Oct

      subroutine getpag ( )

      implicit none
      include 'formload.inc'
C--
      character*132 text*132, flag*1
      integer k, ka, ks, ke, j, nline
Cbegin


      if ( FAILED ) return

      NPAGE = 0

C Find pages section in template

    3 continue
      nline  = 1
      read ( 3, '(a)', err=998, end=999 ) text
      k = index(text,'?#$&pages')
      if ( k.eq.0 ) goto 3
      nline = nline + 1
      read ( 3, '(a)', err=998, end=998 ) text

C  Set blank entries

      do k = 1, MAXPAG
         TPAGE(k) = '  '
      enddo

C Get number and names of pages

      ka = 0
    4 ka = ka + 1
      ks = 1 + (ka-1)*5
      ke = ks + 4
      nline = nline + 1
      read ( 3, '(2x,a1,1x,5(a2,1x))', err=997, end=997 ) flag,
     +                      (TPAGE(j),j=ks,ke)
      if ( flag.ne.'a' ) goto 5
      do k = ks, ke
         if ( TPAGE(k).eq.'  ' ) then
            goto 5
         else
            NPAGE = NPAGE + 1
         endif
      enddo
      if ( NPAGE.le.(MAXPAG-5) ) goto 4
    5 continue

      if ( NPAGE.eq.0 ) then
         write (6, '('' ERROR: Cant get number of pages from'',
     +               '' template file'')')
         call wrerrf ( TTFILE, 0 )
         FAILED = .true.
      endif

      if ( NPAGE.gt.(MAXPAG-5) ) then
         write ( 6, '('' ERROR: Too many pages in template file'',
     +                '' - Maximum = '', i5)' ) MAXPAG
         call wrerrf ( TTFILE, 0 )
         FAILED = .true.
      endif

      goto 996
  999 write ( 6,'('' ERROR: Cant find pages list in template file'')')
      call wrerrf ( TTFILE, 0 )
      FAILED = .true.
      goto 996
  998 write ( 6,'('' ERROR: in reaching pages list in template file'')')
      call wrerrf ( TTFILE, nline )
      FAILED = .true.
      goto 996
  997 write ( 6, '('' ERROR: in reading page codes in template file'')')
      call wrerrf ( TTFILE, nline )
      FAILED = .true.

C  Rewind

  996 rewind ( 3 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETOFA -- Get printing offset information from offsets file
C
C  a j penny              ral                   1992 Oct

*  BLY: M.J. Bly  (Starlink, RAL)               24-MAR-1997
*     Correct illegal use of '(f)' format specifier on internal
*     read - modified to '(f10.0')'.

      subroutine getofa ( )

      implicit none
      include 'formload.inc'
C--
      character text*132, texta*132, offfil*132, ctype*50
      character*5 otype(100), ootype, idef
      integer j, k, kk, kl, klo, kn, ktot, koff, ksi, kci, kti,
     +        koi, kei, istat, nline
      real topa(100), evea(100), odda(100)
Cbegin


      if ( FAILED ) return

      USEOFF = .false.

      ETOPOF = 0.0
      EODDOF = 0.0
      EEVEOF = 0.0

C  Find offsets to add

      call charln ( OPTDIR, klo )
      offfil = 'offsets.dat'
      if ( ISVMS ) then
         if ( klo.ne.0 ) offfil = OPTDIR(1:klo)//'offsets.dat'
      else
         if ( klo.ne.0 ) offfil = OPTDIR(1:klo)//TEXTXA//'offsets.dat'
      endif
      call charln ( offfil, kl )
      call filopn ( 1, 4, ISVMS, offfil(1:kl), istat )
      if ( istat.eq.1 ) goto 994
      OPEN4 = .true.

C  Get and type list of offsets

      nline = 1
      read ( 4, '(a)', err=999, end=999 ) text
      ksi = index(text,'a')
      kci = index(text,'c')
      kti = index(text,'d')
      koi = index(text,'e')
      kei = index(text,'f')

C  Get default offsets

      nline = 2
      read ( 4, '(a)', err=999, end=999 ) text
      if ( index(text(1:6),'single').eq.1 ) then
         read ( text(kti:kti+5), '(f6.2)', err=999 ) ETOPOF
         read ( text(koi:koi+5), '(f6.2)', err=999 ) EODDOF
         read ( text(kei:kei+5), '(f6.2)', err=999 ) EEVEOF
         USEOFF = .true.
      endif

C  Check that they are not all defaulted

      nline = 3
      read ( 4, '(a)', err=999, end=999 ) text
      if ( index(text(1:2),'no').eq.1 ) then
         close ( 4 )
         OPEN4 = .false.
         return
      endif

C  Get alternative options

      write ( 6, '(''  '')' )
      write ( 6,
     +     '(''   Choose which page offsets to add in printing :-'')' )
      write ( 6, '(''   Type                              Code '')' )
      write ( 6, '(''   ____                              ____ '')' )
      write ( 6, '(''   None                              None '')' )

      kn = 1
      otype(1) = 'NONE '
      idef = ' '
    1    nline = nline + 1
         read ( 4, '(a)', err=999, end=2 ) text
         call charln ( text, kl )
         if ( kl.eq.0 ) goto 2
         if ( text(1:1).eq.'y' ) then
            idef = text(ksi:ksi+4)
         else
            write ( 6, '(1x,a)' ) text(1:kci+4)
            kn = kn + 1
            if ( kn.gt.99 ) goto 992
            otype(kn) = text(kci:kci+4)
            read ( text(kti:kti+5), '(f6.2)', err=999 ) topa(kn)
            read ( text(koi:koi+5), '(f6.2)', err=999 ) odda(kn)
            read ( text(kei:kei+5), '(f6.2)', err=999 ) evea(kn)
         endif
         goto 1
    2 continue
      ktot = kn + 1
      otype(ktot) = 'OWN  '
      write ( 6, '(''   Users own values                  Own'')' )
      write ( 6, '(''  '')' )

      close ( 4 )
      OPEN4 = .false.

C  Get input option

      texta = 'Offset code (Default - '//idef
      call charln ( texta, kl )
      text  = texta(1:kl)//') ? '
      call charln ( text, kl )
      call qwrite ( text(1:kl) )
      ctype = '     '
      read ( 5, '(a)' ) ctype
      if ( ctype(1:5).eq.'     ' ) ctype = idef

C  Upper case options

      do k = 1, 5
         j = ichar(ctype(k:k))
         if ( j.ge.97 .and.  j.le.122 ) ctype(k:k) = char(j-32)
      enddo
      do kk = 1, ktot
         ootype = otype(kk)
         do k = 1, 5
            j = ichar(ootype(k:k))
            if ( j.ge.97 .and.  j.le.122 ) ootype(k:k) = char(j-32)
         enddo
         otype(kk) = ootype
      enddo

C  Get 'permitted' offset option

      if ( ctype(1:5).eq.'NONE ' ) then
         USEOFF = .false.
      elseif ( ctype(1:5).eq.'OWN ' ) then
         write ( 6, '(1x,'' Extra margin offsets in cm to add'')' )
         text = 'To Top margin (Default - 00.00) ? '
         call charln ( text, kl )
         call qwrite ( text(1:kl) )
         read ( 5, '(f10.0)', err=993 ) ETOPOF
         text = 'To Odd side margin (Default - 00.00) ? '
         call charln ( text, kl )
         call qwrite ( text(1:kl) )
         read ( 5, '(f10.0)', err=993 ) EODDOF
         text = 'To Even side margin cm (Default - 00.00) ? '
         call charln ( text, kl )
         call qwrite ( text(1:kl) )
         read ( 5, '(f10.0)', err=993 ) EEVEOF
         USEOFF = .true.
      else
         koff = 0
         do k = 1, ktot
            if ( ctype(1:5).eq.otype(k) ) koff = k
         enddo
         if ( koff.eq.0 ) then
            goto 991
         else
            ETOPOF = topa(koff)
            EODDOF = odda(koff)
            EEVEOF = evea(koff)
         endif
         USEOFF = .true.
      endif

C  Return

      goto 996
  991 write ( 6, '('' ERROR: Code not recognized '')' )
      call wrerrf ( offfil, 0 )
      FAILED = .true.
      goto 996
  992 write ( 6, '('' ERROR: Too many page printing options'')' )
      call wrerrf ( offfil, 0 )
      write ( 6, '(1x,''       Limit is 100'')' )
      FAILED = .true.
      goto 996
  993 write ( 6, '('' ERROR: Bad length - must be a number '')' )
      FAILED = .true.
      goto 996
  994 write ( 6, '('' ERROR: Cant find the file with the'',
     +             '' list of page printing offsets'')' )
      call wrerrf ( offfil, 0 )
      FAILED = .true.
      goto 996
  999 write ( 6, '('' ERROR: in list of page printing offsets'')' )
      call wrerrf ( offfil, nline )
      FAILED = .true.
      goto 996

  996 continue


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C WRERRF -- Write name of file in which error is
C
C  a j penny              ral                   1992 Oct

      subroutine wrerrf ( name, nline )

      implicit none
      include 'formload.inc'

C!i: File name (must be 132 characters space)
      character*(132) name
C!i: Line number (0=no line number info output)
      integer       nline
C--
      integer kl
Cbegin


      call charln  ( name, kl )
      if ( kl.lt.1 ) then
         write ( 6, '(1x,''       File Name: - is blank'')' )
      else
         write ( 6, '(1x,''       File Name: '',a60)' ) name(1:60)
      endif
      if ( kl.gt.60 ) then
         write ( 6, '(1x,''                  '',a60)' ) name(61:120)
      endif
      if ( kl.gt.120 ) then
         write ( 6, '(1x,''                  '',a12)' ) name(121:132)
      endif

      if ( nline.ne.0 ) write ( 6, '(1x,''       File line: '',i6)' )
     +                                                      nline


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETOFB -- Get printing offset information
C
C  a j penny              ral                   1992 Oct

*  BLY: M.J. Bly  (Starlink, RAL)               24-MAR-1997
*     Correct illegal use of '(f)' format specifier on internal
*     read - modified to '(f10.0')'.

      subroutine getofb ( )

      implicit none
      include 'formload.inc'
C--
      character text*132, texta*132, ltype*2, offfil*132
      integer j, k, kl, kla, nline
Cbegin


      if ( FAILED ) return

      if ( .not.USEOFF ) return

      TOPOFF = 0.0
      ODDOFF = 0.0
      EVEOFF = 0.0

C Find offset section in template

    3 continue
      nline  = 1
      read ( 3, '(a)', err=998, end=999 ) text
      if ( index(text,'?#$&offset').eq.0 ) goto 3

C Get offsets

    4 nline = nline + 1
      read ( 3, '(a))', err=997, end=997 ) text
      if ( index(text,'?#$&endoffset').ne.0 ) goto 5

      call lbgone ( text )
      call charln ( text, kl )
      do k = 1, kl
         j = ichar(text(k:k))
         if ( j.ge.65 .and. j.le.90 ) text(k:k) = char(j+32)
      enddo
      k = index(text,' ')
      if ( k.eq.1 .or. k.eq.kl ) goto 997
      texta = text(k:)
      call lbgone ( texta )
      call charln ( texta, kla )
      if ( kla.lt.3 ) goto 995

      if ( text(1:k-1).eq.(TEXTX//'topmargin') ) then
         read ( texta(1:kla-2), '(f10.0)' ) TOPOFF
         read ( texta(kla-1:kla), '(a)' ) ltype
         if ( ltype.eq.'in' ) then
            TOPOFF = TOPOFF*2.54
         elseif ( ltype.eq.'pt' ) then
            TOPOFF = TOPOFF*2.54/72.27
         elseif ( ltype.ne.'cm' ) then
            goto 995
         endif
         TOPOFF = TOPOFF + ETOPOF
         if ( TOPOFF.gt.9999.99 .or. TOPOFF.lt.-999.99 ) goto 991
      elseif ( text(1:k-1).eq.(TEXTX//'oddsidemargin') ) then
         read ( texta(1:kla-2), '(f10.0)' ) ODDOFF
         read ( texta(kla-1:kla), '(a)' ) ltype
         if ( ltype.eq.'in' ) then
            ODDOFF = ODDOFF*2.54
         elseif ( ltype.eq.'pt' ) then
            ODDOFF = ODDOFF*2.54/72.27
         elseif ( ltype.ne.'cm' ) then
            goto 995
         endif
         ODDOFF = ODDOFF + EODDOF
         if ( ODDOFF.gt.9999.99 .or. ODDOFF.lt.-999.99 ) goto 991
      elseif ( text(1:k-1).eq.(TEXTX//'evensidemargin') ) then
         read ( texta(1:kla-2), '(f10.0)' ) EVEOFF
         read ( texta(kla-1:kla), '(a)' ) ltype
         if ( ltype.eq.'in' ) then
            EVEOFF = EVEOFF*2.54
         elseif ( ltype.eq.'pt' ) then
            EVEOFF = EVEOFF*2.54/72.27
         elseif ( ltype.ne.'cm' ) then
            goto 995
         endif
         EVEOFF = EVEOFF + EEVEOF
         if ( EVEOFF.gt.9999.99 .or. EVEOFF.lt.-999.99 ) goto 991
      endif
      goto 4

    5 continue

      goto 996
  991 write ( 6,'('' ERROR: Size of page offset(s) used is/are too '',
     +            ''large'')')
      write ( 6, '(1x,''       Values in cm (top/oddside/evenside): '',
     +            3f)' ) TOPOFF, ODDOFF, EVEOFF
      write ( 6, '(1x,''       Limits are -999.99 to +9999.99cm'')' )
      call wrerrf ( offfil, 0 )
      FAILED = .true.
      goto 996
  995 write ( 6, '('' ERROR: bad length in page margins in template '',
     +             ''file'')')
      call wrerrf ( TTFILE, nline )
      FAILED = .true.
      goto 996
  997 write (6,'('' ERROR: in reading page margins in template file'')')
      call wrerrf ( TTFILE, nline )
      FAILED = .true.
      goto 996
  998 write ( 6, '('' ERROR: in reaching flag - %?#$&offset- for page'',
     +             '' margins in template file'')')
      call wrerrf ( TTFILE, nline )
      FAILED = .true.
      goto 996
  999 write ( 6, '('' ERROR: cant find flag - %?#$&offset- for page'',
     +             '' margins in template file'')')
      call wrerrf ( TTFILE, 0 )
      FAILED = .true.
      goto 996

C  Rewind

  996 rewind ( 3 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETQUE -- Get number and codes of questions
C
C a j penny              ral               92 Oct

      subroutine getque ( )

      implicit none
      include 'formload.inc'
C--
      character text*132, flag*1
      integer k, kl, ks, ke, j, nline
Cbegin


      if ( FAILED ) return

      NQUEST = 0

C Find questions section in template

      nline = 0
    3 nline = nline + 1
      read ( 3, '(a)', err=999, end=999 ) text
      k = index(text,'?#$&questions')
      if ( k.eq.0 ) goto 3
      nline = nline + 1
      read ( 3, '(a)', err=999, end=999 ) text

C  Set blank entries

      do k = 1, MAXQUE
         TQUEST(k)  = '      '
      enddo

C Get number and names of questions

      kl = 0
    4 kl = kl + 1
      ks = 1 + (kl-1)*5
      ke = ks + 4
      nline = nline + 1
      read ( 3, '(2x,a1,1x,5(a7,1x))', err=997, end=997 ) flag,
     +                      (TQUEST(j),j=ks,ke)
      if ( flag.ne.'a' ) goto 5
      do k = ks, ke
         if ( TQUEST(k).eq.'       ' ) then
            goto 5
         else
            NQUEST = NQUEST + 1
         endif
      enddo
      if ( NQUEST.le.(MAXQUE-5) ) goto 4
    5 continue

      if ( NQUEST.eq.0 ) then
         write ( 6, '('' ERROR: Cant get number of '',
     +                ''questions from template file'')')
         call wrerrf ( TTFILE, 0 )
         FAILED = .true.
      endif

      if ( NQUEST.gt.(MAXQUE-5) ) then
         write ( 6, '('' ERROR: Too many questions in template file'',
     +                '' - Maximum = '', i5)' ) MAXQUE
         call wrerrf ( TTFILE, nline )
         FAILED = .true.
      endif

      goto 996
  999 write ( 6, '('' ERROR: in reaching questions list'',
     +             '' in template file'')' )
      call wrerrf ( TTFILE, 0 )
      FAILED = .true.
      goto 996
  997 write ( 6, '('' ERROR: in reading question codes'',
     +             '' in template file'')' )
      call wrerrf ( TTFILE, nline )
      FAILED = .true.

C  Rewind

  996 rewind ( 3 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETSPE -- Get number and codes of special questions
C
C a j penny              ral               92 Oct

      subroutine getspe ( )

      implicit none
      include 'formload.inc'
C--
      character text*132
      integer j, k, kk, kl, ks, ke, kn, nline, nchar
      logical bad1, bad2, bad3, bad4, bad5, bad6, ok
Cbegin


      if ( FAILED ) return

      NSPEC = 0

C Find questions section in template

      nline = 0
    3 nline = nline + 1
      read ( 3, '(a)', err=997, end=996 ) text
      k = index(text,'?#$&special')
      if ( k.eq.0 ) goto 3
      nline = nline + 1
      read ( 3, '(a)', err=997, end=996 ) text

C Clear number and names of special questions

      do k = 1, MAXQSP
         QSPEC(k)  = '      '
         NQSPEC(k) = 0
         do j = 1, MAXCSP
            CSPEC(k,j) = ' '
            ASPEC(k,j) = ' '
         enddo
      enddo

C Get number and names of special questions

      bad1 = .false.
      bad2 = .false.
      bad3 = .false.
      bad4 = .false.
      bad5 = .false.
      bad6 = .false.

    4 nline = nline + 1
      read ( 3, '(a)', err=997, end=997 ) text
         call charln ( text, kl )
         if ( text(3:3).eq.'x' ) goto 7

C  Check for repeated name

         if ( NSPEC.ne.0 ) then
            do k = 1, NSPEC
               if ( text(5:11).eq.QSPEC(k) ) then
                  bad3 = .false.
                  goto 7
               endif
            enddo
         endif

C  If new, add in, if not too many

         if ( text(5:11).ne.' ' ) then
            NSPEC = NSPEC + 1
            if ( NSPEC.gt.MAXQSP ) then
               bad2 = .true.
               goto 7
            endif
            QSPEC(NSPEC) = text(5:11)
         endif

C Check special question exists in normal questions

         ok = .false.
         do j = 1, NQUEST
            if ( TQUEST(j).eq.QSPEC(NSPEC) ) ok = .true.
         enddo
         if ( .not.ok ) then
            bad6 = .true.
            goto 7
         endif

C  Check for correct number of entries (sets of [code/answer])

         kk = 0
         k = 1
    5    j = index(text(k:),'"')
         if ( j.ne.0 ) then
            kk = kk + 1
            k = k + j + 1
            if ( k.le.kl ) goto 5
         endif
         j = kk - 4*(kk/4)
         if ( kk.eq.0 .or. j.ne.0 ) then
            bad5 = .true.
            goto 7
         endif

C  Store codes and answers

         ke = 11
    6    ks = ke + index(text(ke+1:),'"')
         ke = ks + index(text(ks+1:),'"')
         nchar = ke - ks + 1
         if ( ke.gt.kl ) goto 997
         if ( ks.eq.0 .or. nchar.lt.1 ) goto 4
         if ( nchar.gt.20 ) then
            bad4 = .true.
            goto 7
         endif
         NQSPEC(NSPEC) = NQSPEC(NSPEC) + 1
         kn = NQSPEC(NSPEC)
         if ( kn.gt.MAXCSP ) then
            bad1 = .true.
            goto 7
         endif
         read ( text(ks+1:ke-1), '(a)' ) CSPEC(NSPEC,kn)
         ks = ke + index(text(ke+1:),'"')
         ke = ks + index(text(ks+1:),'"')
         nchar = ke - ks + 1
         if ( ke.gt.kl ) goto 997
         if ( nchar.lt.1 ) goto 997
         if ( nchar.gt.20 ) then
            bad4 = .true.
            goto 7
         endif
         read ( text(ks+1:ke-1), '(a)' ) ASPEC(NSPEC,kn)
         if ( index(text(ke+1:),'"').eq.0 ) goto 4
         goto 6

C  Error messages

    7 continue

      if ( bad1 ) write ( 6, '('' ERROR: Too many codes for special '',
     +      ''questions in template file  - Maximum = '', i5)' ) MAXCSP
      if ( bad2 ) write ( 6, '('' ERROR: Too many special questions '',
     +      '' in template file - Maximum = '', i5)' ) MAXQSP
      if ( bad3 ) write ( 6, '('' ERROR: repeated special question'',
     +             '' in template file'')' )
      if ( bad4 ) write ( 6, '('' ERROR: codes more than 20 '',
     +     ''characters for special questions in template file'')' )
      if ( bad5 ) write ( 6, '('' ERROR: code/answer quotation marks'',
     +     '' wrong in the special questions in template file'')' )
      if ( bad6 ) write ( 6, '('' ERROR: Special question '',a7,
     +      '' not in question list in template file'')' ) QSPEC(NSPEC)
      if ( bad1 .or. bad2. or. bad3 .or. bad4 .or. bad5 .or. bad6 )then
         call wrerrf ( TTFILE, nline )
         FAILED = .true.
      endif

C  Error message

      goto 996
  997 write ( 6, '('' ERROR: in reading special question codes'',
     +             '' in template file'')' )
      call wrerrf ( TTFILE, nline )
      FAILED = .true.
  996 continue

C  Rewind file

      rewind ( 3 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C MESSAG -- Type out message in template file
C
C a j penny              ral               92 Oct

      subroutine messag ( )

      implicit none
      include 'formload.inc'
C--
      character flag*1, text*90
      integer k, nline
      logical first
Cbegin


      if ( FAILED ) return

      nline = 0
    3 nline = nline + 1
      read ( 3, '(a)', err=998, end=999 ) text
      k = index(text,'?#$&message')
      if ( k.eq.0 ) goto 3

      nline = nline + 1
      read ( 3, '(a)', err=997, end=997 ) text
      first = .true.
    4 nline = nline + 1
      read ( 3, '(2x,a1,1x,a65)', err=997, end=997 ) flag, text
      if ( flag.eq.'x' ) goto 5
      if ( first ) write ( 6, '(''  '')' )
      first = .false.
      call charln ( text, k )
      write ( 6, '(2x,a)' ) text(1:k)
      goto 4
    5 if ( .not.first ) write ( 6, '(''  '')' )

      goto 999
  998 write ( 6, '('' ERROR: in reaching message in template file'')' )
      call wrerrf ( TTFILE, 0 )
      FAILED = .true.
      goto 999
  997 write ( 6, '('' ERROR: in reading message in template file'')' )
      call wrerrf ( TTFILE, nline )
      FAILED = .true.

C  Rewind

  999 rewind ( 3 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GETSCL -- Find print scale and zero positions for the pages
C
C a j penny              ral               89 feb

      subroutine getscl ( )

      implicit none
      include 'formload.inc'
C--
      character*132 text, atext
      integer k, nline
Cbegin


      if ( FAILED ) return

      PXSC = 1.0
      PYSC = 1.0
      PXZE = 0.0
      PYZE = 0.0

C Get scale and offset

      nline = 0
    3 nline = nline + 1
      read ( 3, '(a)', err=998, end=998 ) text
      k = index(text,'?#$&scale')
      if ( k.eq.0 ) goto 3
      atext = text(k+8:)
      read ( atext, '(4(2x,f7.3))', err=997, end=997  ) PXSC, PYSC,
     +                                                  PXZE, PYZE

      goto 999
  998 write ( 6, '('' ERROR: in reaching scale in template file'')' )
      call wrerrf ( TTFILE, 0 )
      FAILED = .true.
      goto 999
  997 write ( 6, '('' ERROR: in reading scale in template file'')' )
      call wrerrf ( TTFILE, nline )
      FAILED = .true.

C  Rewind

  999 rewind ( 3 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INPUT -- Loads the .tex files needed for the LaTeX forms
C
C  a j penny              ral                   1992 Feb

      subroutine input ( )

      implicit none
      include 'formload.inc'
C--
      character*132 text, texta
      integer j, k, kl, istat
      logical there
Cbegin


      if ( FAILED ) return

      if ( TIFILE.ne.' ' ) then
         write ( text, '(''Input file name (Default - '',a70)' )
     +                 TPFILE(1:70)
         call charln ( text, kl )
      else
         text = 'Input file name (Default -  '
         call charln ( text, kl )
         kl = kl + 2
      endif
      texta = text(1:kl)//') ?'
      call qwrite ( texta(1:kl+4) )
      INFILE = ' '
      read ( 5, '(a)' ) INFILE
      if ( INFILE.eq.' ' ) then
         if ( TIFILE.eq.' ' ) then
            write ( 6, '('' ERROR: Invalid input'')' )
            FAILED = .true.
            return
         else
            INFILE = TIFILE
            call lowerc ( INFILE )
         endif
      endif

      call charln ( INFILE, kl )
      there = .false.
      k = 1
      if ( ISVMS ) then
         k = 1 + index(INFILE(1:kl),']')
         if ( index(INFILE(k:kl),'.').ne.0 ) there = .true.
      else
         do j = kl, 1, -1
            if ( .not.there .and. INFILE(j:j).eq.'.' ) then
               if ( j.eq.kl ) then
                  there = .true.
               else
                  if ( INFILE(j+1:j+1).ne.TEXTXA .and.
     +                 INFILE(j+1:j+1).ne.'.' ) there = .true.
               endif
            endif
         enddo
      endif
      if ( .not.there ) then
         text = INFILE(1:kl)//'.dat'
         INFILE = text
      endif

      OPEN1 = .true.
      call charln ( INFILE, kl )
      call filopn ( 1, 1, ISVMS, INFILE(1:kl), istat )
      if ( istat.eq.1 ) goto 997

      go to 999
  997 write ( 6, '('' ERROR: Cant get that input file'')' )
      call wrerrf ( INFILE, 0 )
      FAILED = .true.
  999 continue


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DOPAGE -- Get which pages to do
C
C  a j penny              ral                   1992 Feb

      subroutine dopage ( )

      implicit none
      include 'formload.inc'
C--
      character qtext*80, textb*80, texta*132, textc*2
      logical good
      integer k, kl, knum, ks, ke
Cbegin


      if ( FAILED ) return

C  Load question

      textb = ' '
      ke = 0
      do k = 1, NPAGE
         ks = ke + 1
         textc = TPAGE(k)
         call charln ( textc, kl )
         ke = ks + kl
         textb(ks:ke) = textc(1:kl)//','
      enddo
      call charln ( textb, kl )
      texta = 'Which pages to do (all,'//textb(1:kl)//'none)'//
     +        ' (Default - all) ?'
      call charln ( texta, kl )

C  Get which pages

      knum = 1
      good = .false.

   11 if ( knum.gt.3 .or. good ) goto 12

         call qwrite ( texta(1:kl) )
         qtext = ' '
         read ( 5, '(a)' ) qtext

         do k = 1, MAXPAG
            JPAGE(k) = .false.
         enddo

         if ( index(qtext,'none').ne.0 .or.
     +        index(qtext,'NONE').ne.0 ) then
            good = .true.
         elseif ( qtext.eq.' ' .or. index(qtext,'ALL').ne.0 .or.
     +         index(qtext,'all').ne.0 ) then
            do k = 1, NPAGE
               JPAGE(k) = .true.
            enddo
            good = .true.
         else
            do k = 1, 80
               if ( qtext(k:k).eq.',' ) qtext(k:k) = ' '
            enddo
            do k = 1, NPAGE
               if ( index(qtext,TPAGE(k)).ne.0 ) then
                  JPAGE(k) = .true.
                  good = .true.
               endif
            enddo
         endif

         knum = knum  + 1
         if ( .not.good ) then
            if ( knum.le.3 ) then
               write( 6, '('' ERROR: Cant understand. Try again'')')
            else
               write ( 6, '('' ERROR: Give up. No pages output'')' )
            endif
         endif

      goto 11
   12 continue

      DOANY = .false.
      do k = 1, NPAGE
         if ( JPAGE(k) ) DOANY = .true.
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C OUTPUT -- Get output file
C
C  a j penny              ral                   1992 Oct

      subroutine output ( )

      implicit none
      include 'formload.inc'
C--
      integer j, k, kl, kp, istat
      logical there
      character*132 otext
Cbegin


      if ( FAILED ) return

      if ( .not.DOANY ) return

      otext = INFILE

C  !Remove VAX or UNIX directory prefix

      if ( ISVMS ) then
         k = index(otext,']')
         if ( k.ne.0 ) otext(1:k) = ' '
         k = index(otext,':')
         if ( k.ne.0 ) otext(1:k) = ' '
      else
    1    k = index(otext,TEXTXA)
         if ( k.ne.0 ) otext(1:k) = ' '
         if ( index(otext,TEXTXA).ne.0 ) goto 1
      endif
      call lbgone ( otext )

C   !Remove type suffix

      if ( ISVMS ) then
         k = index(otext,'.')
      else
         k = 0
         call charln ( otext, kl )
         there = .false.
         do j = kl, 1, -1
            if ( .not.there .and. otext(j:j).eq.'.' ) then
               if ( j.eq.kl ) then
                  there = .true.
                  k = j
               else
                  if ( otext(j+1:j+1).ne.TEXTXA .and.
     +                 otext(j+1:j+1).ne.'.' ) then
                     there = .true.
                     k = j
                  endif
               endif
            endif
         enddo
      endif
      if ( k.ne.0 ) otext(k:) = ' '

C  !Add `.tex' suffix

      call charln ( otext, kp )
      if ( kp.gt.0 .and. kp.lt.127 ) otext = otext(1:kp)//'.tex'
      call charln ( otext, kp )
      OUTFILE = otext(1:kp)

C  !Open output file

      OPEN2 = .true.
      call filopn ( 2, 2, ISVMS, otext(1:kp), istat )
      if ( istat.eq.1 ) goto 996

      goto 999
  996 write ( 6, '('' ERROR: Cant open that output file'' )' )
      call wrerrf ( otext, 0 )
      FAILED = .true.
  999 continue

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C WORK -- Load the template and input to output
C
C  a j penny              ral                   1992 Feb

      subroutine work ( )

      implicit none
      include 'formload.inc'
C--
      logical lmore, gotoff
      integer k, kk, kl, ks, ke, nline
      character*132 text
      character texta*30
      character nepage*11
Cbegin


      if ( FAILED ) return

      if ( .not.DOANY ) return

C  Load comments

      text = '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      call twrite ( text )
      text = '% Output from program FORMLOAD                   %'
      call twrite ( text )
      text = '% Program written by:  Alan Penny                %'
      call twrite ( text )
      text = '%                      RAL      1992 Nov         %'
      call twrite ( text )
      text = '%                                                %'
      call twrite ( text )
      call charln ( INFILE, kl )
      text = '%    Input file: '//INFILE(1:30)//'  %'
      call twrite ( text )
      do k = 2, 4
         if ( kl.gt.(30*(k-1)) ) then
            ks = 1 + (30*(k-1))
            ke = ks + 29
            text = '%                '//INFILE(ks:ke)//'  %'
            call twrite ( text )
         endif
      enddo
      text = '% Template file: '//TTFILE(1:30)//'  %'
      call twrite ( text )
      call charln ( TTFILE, kl )
      do k = 2, 4
         if ( kl.gt.(30*(k-1)) ) then
            ks = 1 + (30*(k-1))
            ke = ks + 29
            text = '%                '//TTFILE(ks:ke)//'  %'
            call twrite ( text )
         endif
      enddo
      text = '%   Output file: '//OUTFILE(1:30)//'  %'
      call twrite ( text )
      call charln ( OUTFILE, kl )
      do k = 2, 4
         if ( kl.gt.(30*(k-1)) ) then
            ks = 1 + (30*(k-1))
            ke = ks + 29
            text = '%                '//OUTFILE(ks:ke)//'  %'
            call twrite ( text )
         endif
      enddo
      text = '%                                                %'
      call twrite ( text )
      text = '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      call twrite ( text )
      text = '    '
      call twrite ( text )
      text = '    '
      call twrite ( text )

C  Load starter lines

      nline = 0
    1 nline = nline + 1
      read ( 3, '(a)', end=998, err=999 ) text
      if ( index(text,'%?#$&startbeg').eq.0 ) goto 1

      lmore = .true.
      gotoff = .false.
    3 if ( .not.lmore ) goto 4
         nline = nline + 1
         read ( 3, '(a)', end=998, err=999 ) text

         if ( .not.gotoff .and. index(text,'%?#$&endoffset').eq.0 ) then

            if ( index(text,'%?#$&startend').ne.0 ) then
               lmore = .false.
            elseif ( index(text,'%?#$&offset').ne.0 ) then
               if ( USEOFF ) then
                  gotoff = .true.
                  write ( text, '(a1,''topmargin '',f7.2,
     +                                  ''cm'')' ) TEXTX, TOPOFF
                  call twrite ( text )
                  write ( text, '(a1,''oddsidemargin '',f7.2,
     +                                  ''cm'')' ) TEXTX, ODDOFF
                  call twrite ( text )
                  write ( text, '(a1,''evensidemargin '',f7.2,
     +                                  ''cm'')' ) TEXTX, EVEOFF
                  call twrite ( text )
               endif
            elseif ( index(text,'%?#$&startmid').ne.0 ) then
               call docomm
            else
               k = index(text,'%?#$&input')
               if ( k.ne.0 ) then
                  call insert ( text(k+11:k+17) )
               else
                  call twrite ( text )
               endif
            endif

         endif

         if ( index(text,'%?#$&endoffset').ne.0 ) gotoff = .false.
      goto 3
    4 continue

C  Do the work

      do kk = 1, NPAGE

         if ( kk.eq.NPAGE ) then
            nepage = '%?#$&end'
         else
            text = '%?#$&page'//TPAGE(kk+1)
            call charln ( text, kl )
            nepage = text(1:kl)
         endif

         lmore = .true.
   11    if ( .not. ( lmore ) ) goto 12

            nline = nline + 1
            read ( 3, '(a)', end=998, err=999 ) text
            if ( index(text,nepage).ne.0 ) then
               lmore = .false.
            else
               if ( JPAGE(kk) ) then
                  k = index(text,'%?#$&input')
                  if ( k.ne.0 ) then
                     call insert ( text(k+11:k+17) )
                  else
                     k = index(text,'%?#$&'//TEXTX//'bjpos')
                     if ( k.ne.0 ) then
                        call posins ( text(k+12:), nline )
                        if ( FAILED ) return
                     else
                        call twrite ( text )
                     endif
                  endif
               endif
            endif

         goto 11
   12    continue

      enddo

C  Write 'end of document' with the backslash done correctly

      texta = TEXTX//'end{document}'
      write ( 2, '(1x,a)' ) texta

C  Message to user

      call charln ( OUTFILE, kl )
      write ( 6, '(1x,''  Output file:  '',a60)' ) OUTFILE(1:60)
      if ( kl.gt.60 ) write ( 6, '(1x,14x,a62)') OUTFILE(61:122)

C  Errors

      goto 996
  999 write ( 6, '('' ERROR: in reading body of template file'')' )
      call wrerrf ( TTFILE, nline )
      FAILED = .true.
      goto 996
  998 write ( 6, '('' ERROR: reached end of body of template file'')' )
      call wrerrf ( TTFILE, 0 )
      FAILED = .true.
  996 continue


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DOCOMM -- Write the 'Formload' LateX commands
C
C  a j penny              ral                   1992 Oct

      subroutine docomm ()

      implicit none
      include 'formload.inc'
C--
      integer k, kla, klb, ncomm
      parameter ( ncomm=24 )
      character*132 text, texta, textb, tcomm(ncomm), utcomm(ncomm)

      data (tcomm(k),k=1,10) /
     + ' \newcommand{\ajinp}[3]{\vspace*{-0.13in} ' ,
     + '                  \rule[0.0in]{7.2in}{0in}\hspace*{-7.2in}' ,
     + '                  \rule[#1]{0in}{#2} ' ,
     + '                  \begin{minipage}[t]{7.2in} ' ,
     + '                       #3 ' ,
     + '                  \end{minipage}} ',
     + '  ' ,
     + ' \newcommand{\ajpos}[8]{ ',
     + '   \vspace*{#2}\hspace*{#1}\rule[#5]{0.0cm}{#4}' ,
     + '                           \rule[0.0cm]{0.0cm}{1.0cm} ' /
      data (tcomm(k),k=11,20) /
     + '   {\ajinpfont\begin{minipage}[t]{#3} ' ,
     + '   #8 \ ' ,
     + '   \end{minipage} ' ,
     + '   }\vspace*{-1.3165cm}\vspace*{#6}\hspace*{-1.0em}' ,
     + '                                    \hspace*{#7}} ' ,
     + ' ' ,
     + ' \newcommand{\cjpos}[8]{ ' ,
     + ' \vspace*{#2cm}\hspace*{#1cm}\rule[#5cm]{0.0cm}{#4cm}' ,
     + '                             \rule[0.0cm]{0.0cm}{1.0cm} ' ,
     + '   {\ajinpfont\begin{minipage}[t]{#3cm} ' /
      data (tcomm(k),k=21,ncomm) /
     + '   #8 \ ' ,
     + '   \end{minipage} ' ,
     + '   }\vspace*{-1.3165cm}\vspace*{#6cm}\hspace*{-1.0em}' ,
     + '                                    \hspace*{#7cm}} ' /

      data (utcomm(k),k=1,10) /
     + ' \\newcommand{\\ajinp}[3]{\\vspace*{-0.13in} ' ,
     + '                  \\rule[0.0in]{7.2in}{0in}\\hspace*{-7.2in}' ,
     + '                  \\rule[#1]{0in}{#2} ' ,
     + '                  \\begin{minipage}[t]{7.2in} ' ,
     + '                       #3 ' ,
     + '                  \\end{minipage}} ',
     + '  ' ,
     + ' \\newcommand{\\ajpos}[8]{ ',
     + '   \\vspace*{#2}\\hspace*{#1}\\rule[#5]{0.0cm}{#4}' ,
     + '                           \\rule[0.0cm]{0.0cm}{1.0cm} ' /
      data (utcomm(k),k=11,20) /
     + '   {\\ajinpfont\\begin{minipage}[t]{#3} ' ,
     + '   #8 \\ ' ,
     + '   \\end{minipage} ' ,
     + '   }\\vspace*{-1.3165cm}\\vspace*{#6}\\hspace*{-1.0em}' ,
     + '                                    \\hspace*{#7}} ' ,
     + ' ' ,
     + ' \\newcommand{\\cjpos}[8]{ ' ,
     + ' \\vspace*{#2cm}\\hspace*{#1cm}\\rule[#5cm]{0.0cm}{#4cm}' ,
     + '                             \\rule[0.0cm]{0.0cm}{1.0cm} ' ,
     + '   {\\ajinpfont\\begin{minipage}[t]{#3cm} ' /
      data (utcomm(k),k=21,ncomm) /
     + '   #8 \\ ' ,
     + '   \\end{minipage} ' ,
     + '   }\\vspace*{-1.3165cm}\\vspace*{#6cm}\\hspace*{-1.0em}' ,
     + '                                    \\hspace*{#7cm}} ' /

Cbegin


      k = 0
    1 if ( k.eq.ncomm ) goto 2
      k = k + 1

         if ( k.eq.9 .or. k.eq.14 .or. k.eq.17 .or. k.eq.23 ) then
            texta = utcomm(k)
            if ( ISVMS ) texta = tcomm(k)
            call charln ( texta, kla )
            k = k + 1
            textb = utcomm(k)
            if ( ISVMS ) textb = tcomm(k)
            call lbgone ( textb )
            call charln ( textb, klb )
            text = texta(1:kla)//textb(1:klb)
         else
            text = utcomm(k)
            if ( ISVMS ) text = tcomm(k)
         endif

         call twrite ( text )

      goto 1
    2 continue


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHCLOS -- Check to close files
C
C  a j penny              ral                   1992 Oct

      subroutine chclos ()

      implicit none
      include 'formload.inc'
C--
Cbegin


      if ( OPEN1 ) close ( 1 )
      if ( OPEN2 ) close ( 2 )
      if ( OPEN3 ) close ( 3 )
      if ( OPEN4 ) close ( 4 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INSERT -- Insert question in output file
C
C  a j penny              ral                   1992 Feb

      subroutine insert ( ftext )

      implicit none

C   !i: question to insert
      character*7 ftext
C--
      logical nend, more, blank, okfile
      character*132 itext, ntext, text, otext
      character*11 rrtext
      integer nline
Cbegin


      rrtext = ftext//'#?$&'

      rewind 1
      nline = 0
      read ( 1, '(a)', end=1 ) itext

      nend = .false.
      more = .true.
      ntext = itext
   11 if ( .not. ( .not.nend .and. more ) ) goto 12
         text = ntext
         nline = nline + 1
         read ( 1, '(a)', end=1 ) ntext
         goto 2
    1    nend = .true.
    2    continue
         if ( index(text,rrtext).ne.0 ) then
            call gline ( text, blank, otext )
            if ( blank ) then
               call lblank ( nend, ntext, ftext, otext )
               if ( otext.ne.' ' ) then
                  call twrite ( otext )
                  call chekpr ( otext, ftext, nline )
               endif
            else
               call spec ( otext, ftext )
               call twrite ( otext )
               call chekpr ( otext, ftext, nline )
            endif
            call isques ( ntext, okfile )
            if ( nend .or. okfile ) then
               more = .false.
            else
               call twrite ( ntext )
               call chekpr ( ntext, ftext, nline )
               call cpage ( ftext, nline, ntext, nend )
               more = .false.
            endif
         endif

      goto 11
   12 continue


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHEKPR -- Warn if input file line has '%', if not '\%'
C
C  a j penny              ral                   1992 Feb

      subroutine chekpr ( text, ftext, nline )

      implicit none
      include 'formload.inc'

C  !i: Input file line
      character*132   text
C  !i: Question number
      character*7     ftext
C  !i: line number
      integer         nline
C--
C   ! % and ' characters
      character*1 textp, texth
      parameter ( textp=char(37) )
      parameter ( texth=char(39) )

      integer kl, k, ka, kla
      logical more, flag, found, flaga
      character*72 texta, textb, textc, textd, texte, textf
Cbegin


      call charln ( text, kl )
      if ( kl.lt.1 ) return

      if ( ftext.eq.'XE_0_00' ) return

      more = .true.
      k = 1
      found = .false.
      flaga = .false.
    1 continue
         flag = .false.
         ka = index(text(k:kl),textp)
         k = k + ka - 1
         if ( ka.eq.0 ) then
            more  = .false.
         else
            if ( k.eq.1 ) then
               flag = .true.
            else
               if ( text(k-1:k).ne.(TEXTX//textp) ) flag = .true.
            endif
         endif

         if ( flag ) found = .true.

         if ( flag ) then

            if ( flaga ) then
               texte = '                               Line'
            else
               write ( texte, '('' WARNING: A '',a1,a1,a1,'' in '',
     +                     ''input file: Line'')' )texth, textp, texth
               flaga = .true.
            endif

            call charln ( texte, kl )
            write ( textb, '(i6)' ) nline
            call lbgone ( textb )
            textc = texte(1:kl)//' '//textb

            write ( texta, '('': Question '',a7)' ) ftext
            call charln ( textc, kl )
            textf = textc(1:kl)//texta

            call charln ( textf, kl )
            write ( texta, '('': Char '',i3)' ) k
            call lbgone ( texta(8:) )
            textd = textf(1:kl)//texta

            call charln ( textd, kl )
            write ( 6, '(1x,a)' ) textd(1:kl)

            if ( .not.PEROUT ) then
               write ( 6, '(1x,''          '',
     +                   ''LaTeX ignores it, and rest of line.'')' )
               PEROUT = .true.
            endif

         endif

         k = k + 1
         if ( k.ge.kl ) more = .false.

      if ( more ) goto 1

      if ( found ) then
         call charln ( text, kl )
         kla = min(kl,50)
         write ( 6, '(1x,''          Text:  '',a)' ) text(1:kla)
         if ( kl.gt.50 ) then
            kla = min(110,kl)
            write ( 6, '(1x,''           '',a)' ) text(51:kla)
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C LBLANK -- Loads right blank, if a 1st blank line
C
C  a j penny              ral                   1992 Feb

      subroutine lblank ( nend, ntext, ftext, otext )

      implicit none
      include 'formload.inc'

C  !i: is next line, input file end?
      logical         nend
C  !i: Input file next line
      character*132   ntext
C  !i: output file signifier
      character*7     ftext
C  !o: output line correct blank
      character*132   otext
C--
      logical nques
Cbegin


C Blank line is all response

      otext = ' '//TEXTX//'ajnull'
      if ( ftext.eq.'XE_0_00' ) otext = ' '

C  Blank line in body of text

      call isques ( ntext, nques )
      if ( .not.nend .and. .not.nques ) otext = ' '


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ISQUES -- Is this line a new question ?
C
C  a j penny              ral                   1992 Feb

      subroutine isques ( text, ok )

      implicit none
      include 'formload.inc'

C  !i: Input line
      character*132  text
C  !o: True, if a new file
      logical        ok
C--
      integer k, kno
Cbegin


      ok = .false.

      k = 0
      kno = 0
   11 if ( .not. (kno.eq.0 .and. k.lt.NQUEST) ) goto 12
         k = k + 1
         if ( text(8:11).eq.'#?$&' .and.
     +        text(1:7).eq.TQUEST(k) ) kno = k
      goto 11
   12 continue

      if ( kno.ne.0 ) ok = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C GLINE -- Gets a first line
C
C  a j penny              ral                   1992 Feb

      subroutine gline ( text, blank, otext )

      implicit none

C  !i: Input line
      character*132  text
C  !o: True if line is blank
      logical        blank
C  !o: Line after '!'
      character*132  otext
C--
      integer k, kex
Cbegin

      k = 11
      kex = 0
   11 if ( .not. (kex.eq.0 .and. k.lt.132)  ) goto 12
         k = k + 1
         if ( text(k:k).eq.'!' ) kex = k + 1
      goto 11
   12 continue

      otext = ' '
      blank = .true.
      if ( kex.ne.0 .and. kex.ne.133 ) then
         do k = kex, 132
            if ( text(k:k).ne.' ' ) blank = .false.
         enddo
         if ( .not.blank ) otext = text(kex:)
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SPEC -- For certain forms, some questions have special forms
C
C  a j penny              ral                   1992 Feb

      subroutine spec ( otext, ftext )

      implicit none
      include 'formload.inc'

C  !i/o: input/output line
      character*132   otext
C  !i: question name
      character*7     ftext
C--
      character*132 text
      integer k, kn, kl

Cbegin


C  Are there special questions?

      if ( NSPEC.eq.0 ) return

C  Is line blank?

      if ( otext.eq.' ' ) return

C  Is THIS a special question?

      kn = 0
      do k = 1, NSPEC
         if ( ftext.eq.QSPEC(k) ) kn = k
      enddo
      if ( kn.eq.0 ) return
      if ( NQSPEC(kn).eq.0 ) return

C What is the answer?

      do k = 1, NQSPEC(kn)
         text = CSPEC(kn,k)
         call charln ( text, kl )
         if ( index(otext,text(1:kl)).ne.0 ) then
            text = ASPEC(kn,k)
            call charln ( text, kl )
            otext = text(1:kl)
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CPAGE -- Copies following lines into file, until 'end'
C  'end' is shown by a non-null 1st character or end of input file
C  Closes output.
C
C  a j penny              ral                   1992 Feb

      subroutine cpage ( ftext, nline, text, nend )

      implicit none

C  !i: Question code
      character*7     ftext
C  !i: Line of input file reached already
      integer         nline
C  !o: next Input line
      character*132   text
C  !o: True if next line is input file end
      logical         nend
C--
      logical fmore, ok
      integer nlinea
Cbegin


      nlinea = nline + 1
      fmore = .true.
   11 if ( .not. (fmore) ) goto 12

         read ( 1, '(a)', end=1 ) text
         nlinea = nlinea + 1
         call isques ( text, ok )
         if ( ok ) then
            fmore = .false.
         else
            call twrite ( text )
            call chekpr ( text, ftext, nlinea )
         endif
         goto 2
    1    fmore = .false.
         nend = .true.
    2    continue

      goto 11
   12 continue


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TWRITE -- Write a line to device 2
C
C  a j penny              ral                   1992 Feb

      subroutine twrite ( text )

      implicit none

C  !i: Line to write
      character*132    text
C--
      integer j, k
Cbegin


      j = 0
      do k = 132, 1, -1
         if ( j.eq.0 .and. text(k:k).ne.' ' ) j = k
      enddo

      if ( j.eq.0 ) then
         write ( 2, '(1x,a79)' ) text(1:1)
      else
         write ( 2, '(1x,a)' ) text(1:j)
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C POSINS -- Insert question position setup in output file
C
C    a j penny          ral         1988 oct

      subroutine posins ( text, nline )

      implicit none
      include 'formload.inc'

C!i: question position and size
      character*132 text
C!i: Line count
      integer nline
C--
      real x, y, xp, yp
      integer kla, klb
      character taform*45, tbform*31, oform*132, ooform*132, tcform*55
      data taform / 'ajpos{000.00cm}{000.00cm}{000.00cm}{000.00cm}' /
      data tbform / '{000.00cm}{000.00cm}{000.00cm}{' /
Cbegin


      read ( text, '(f5.2,3(2x,f5.2),1x,a55)', err=999  ) x, y, xp, yp,
     +                                                    tcform

C  Turn numbers into full \ajpos line

      oform = TEXTX//taform//tbform
      call ldpos ( x, y, xp, yp, oform )

C Add any text at end of \bjpos input line

      call charln ( oform, kla )
      call charln ( tcform, klb )
      if ( klb.ne.0 ) then
         ooform = oform(1:kla)//tcform(1:klb)
      else
         ooform = oform(1:kla)
      endif

C  Write line

      call twrite ( ooform )

C Errors

      goto 996
  999 write ( 6, '('' ERROR: in reading body of template file'')' )
      write ( 6, '(''        error is in a '',a1,''bjpos line'')')TEXTX
      call wrerrf ( TTFILE, nline )
      FAILED = .true.
  996 continue


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C LDPOS -- Take a position, scale it and load into ajpos form
C
      subroutine ldpos ( x, y, xp, yp, text )

      implicit none
      include 'formload.inc'

C!i: X position
      real x
C!i: Y position
      real y
C!i: X para size
      real xp
C!i: Y para size
      real yp
C!i/o: Text to load
      character*132 text
C--
      real ypm, bx, by
Cbegin


      ypm = -1.0*yp
      bx = -1.0*x - xp
      by = -1.0*y - yp

      x   = PXSC*x   + PXZE
      y   = PYSC*y   + PYZE
      ypm = PYSC*ypm + PYZE
      bx  = PXSC*bx  + PXZE
      by  = PYSC*by  + PYZE

      write ( text(8:13),  '(f6.2)' ) x
      write ( text(18:23), '(f6.2)' ) y
      write ( text(28:33), '(f6.2)' ) xp
      write ( text(38:43), '(f6.2)' ) yp
      write ( text(48:53), '(f6.2)' ) ypm
      write ( text(58:63), '(f6.2)' ) by
      write ( text(68:73), '(f6.2)' ) bx


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C CHARLN -- Position of last non-blank character in a character string
C
C  alan penny                 ral                      1990-02-19

      subroutine charln ( text, kl )

      implicit none

      character*(*)  text
C	!i: Input character string
      integer        kl
C	!o: Position of last non-blank character
C	!   (if all blank, returned as 0)
C--
      integer k
Cbegin

      kl = len(text)
      k = 1
    1 continue
         if ( k.ne.1 .or. kl.le.0 ) goto 2
         if ( text(kl:kl).eq.' ' ) then
            kl = kl - 1
         else
            k = 0
         endif
      goto 1
    2 continue


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C LBGONE -- Remove leading blanks from a character string
C
C   alan penny               ral         1990 Jan

      subroutine lbgone ( str )

      implicit none

C	!i/o: Input string, modified on output
      character*(*)  str
C--
      integer j, k, kl
Cbegin


C  String length

      kl = len(str)

C  Scan string, looking for first non-blank character

      k = 1
    1 if ( k.gt.kl .or. str(k:k).ne.' ' ) goto 2
         k = k + 1
      goto 1
    2 continue
      k = k - 1

C  If some non-blank and 1st non-blank not 1st character, shift
C  remaining characters to left by length of gap at start

      if ( k.ne.0 .and. k.ne.kl ) then
          do j = 1, kl-k
             str(j:j) = str(j+k:j+k)
         enddo
         str(j:) = ' '
      endif


      end
