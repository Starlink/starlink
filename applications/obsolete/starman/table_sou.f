CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBCALC.F
C
C  Contains:-
C
C T_TBCALC      Do table maths
C TBCA_GCL      Get input and output info from the command line
C TBCA_BACk     Load background
C TBCA_POLISH   Decode equation and sort into reverse Polish
C TBCA_DOIT     Combine the tables
C TBCA_TBDIFF   Get number of diff input tables and point to stack
C TBCA_VARDIFF  Get number of diff input variables and point to stack
C TBCA_LOAD     Copy table into a 3-D stack
C TBCA_SORT     Sort identifiers to alphabetical order


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBCALC -- Do table calculations
C
C  alan penny                  ral              1991 Dec

      subroutine t_tbcalc ()

      implicit none
      include 'tbcalc.inc'
      include 'STARMAN_INC'
C--
      integer k, ipstk, ipd, ierr, ierr1, ierr2
Cbegin


      call azeroi ( IMP, 70*20 )					!Zero locators for tables and varaibles
      call azeroi ( IMPV, 70*20 )

      call tbca_gcl							!Get input/output info
      if ( ST_FAILED .or. NUMEQN.eq.0 ) return

      if ( TBZ.eq.0 ) then						!Get work space
         call gtwrki ( 'STACK', 1, ipstk, ierr1 )
      else
         call gtwrki ( 'STACK', TBXM*TBY*TBZ, ipstk, ierr1 )
      endif
      call gtwrkr ( 'COL', TBY, ipd, ierr2 )
      if ( max(ierr1,ierr2).ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      if ( TBZ.ne.0 ) then						!Load input tables into stack
         do k = 1, TBZ
            call tbca_load ( %val(IP(k)), TBVX(k), TBY, k, %val(ipstk),
     +                       TBXM, TBY )
            if ( ST_FAILED ) return
         enddo
      endif

      call tbca_back							!Load background

      do k = 1, NUMEQN							!Do the calculations
         call tbca_doit ( %val(ipstk), TBXM, TBY, OPCODE(1,k),
     +                    NOPCODE(k), IMP(1,k), IMPC(1,k), IMPV(1,k),
     +                    VAR, CON(1,k), %val(ipd), KSEED, NCOL(k),
     +                    ierr )
         if ( ST_FAILED ) return
         if ( ierr.ne.0 ) then
            ST_FAILED = .true.
            return
         endif
         call coprr ( %val(ipd), 1, TBY, 1, 1, 1, TBY,
     +                %val(IPO), TBVXO, TBY, (NCOL(k)+5), 1 )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCA_GCL -- Get input and output info from the command line
C
C  alan penny                         RAL                1991 Dec

      subroutine tbca_gcl ( )

      implicit none
      include 'tbcalc.inc'
      include 'STARMAN_INC'
C--
      logical more, gotit
      integer j, k, kv, ierr, tbxmax, ny, kopt, kn1, kn2
      character ntexta*7, ntextb*8, title*50, ctemp*2, texta*132,
     +          textb*70, ineqna*4, ineqnb*5, intab*3, thelp*68,
     +          inheada*5, inheadb*6, head*20, tempname*1
      character*2 chnum(27)
      character*1000 choice
      data choice / 'NONE:TA:TB:TC:TD:TE:TF:TG:TH:TI:TJ:TK:TL:TM:
     +                    TN:TO:TP:TQ:TR:TS:TT:TU:TV:TW:TX:TY:TZ' /
      data chnum / 'NO', 'TA', 'TB', 'TC', 'TD', 'TE', 'TF', 'TG',
     +             'TH', 'TI', 'TJ', 'TK', 'TL', 'TM', 'TN', 'TO',
     +             'TP', 'TQ', 'TR', 'TS', 'TT', 'TU', 'TV', 'TW',
     +             'TX', 'TY', 'TZ' /

Cbegin


      if ( ST_FAILED ) return

      more = .true.
      k = 0
      do while ( more )
         k = k + 1
         if ( k.lt.10 ) then
            write ( ntexta, '(''COLUMN'',i1)' ) k
            call get1i ( ntexta, NCOL(k), k, 0, 100000 )
         else
            write ( ntextb, '(''COLUMN'',i2)' ) k
            call get1i ( ntextb, NCOL(k), k, 0, 100000 )
         endif
         if ( ST_FAILED ) return
         if ( NCOL(k).ne.0 ) then
            gotit = .false.						!Get equation
            do while ( .not.gotit )
               gotit = .true.
               if ( k.lt.10 ) then
                  write ( ineqna, '(''EQN'',i1)' ) k
                  call get1c ( ineqna, texta, ' ', .false. )
               else
                  write ( ineqnb, '(''EQN'',i2)' ) k
                  call get1c ( ineqnb, texta, ' ', .false. )
               endif
               if ( ST_FAILED ) return
               textb = texta(1:70)
               call uppcase ( textb, EXPRS(k) )
               call tbca_polish ( EXPRS(k), OPCODE(1,k), NOPCODE(k),	!Translate it into reverse polish notation
     +                            TBID(1,k), NTB(k), IMP(1,k),
     +                            IMPC(1,k), VARID(1,k), NVAR(k),
     +                            IMPV(1,k), CON(1,k), NCON(k), ierr )
               if ( ST_FAILED ) return
               if ( ierr.ne.0 ) gotit = .false.

            enddo
         endif

         if ( NCOL(k).eq.0 ) then
            k = k - 1
            more = .false.
         elseif ( k.eq.20 ) then
            call printo ( 'No more columns accepted' )
            more = .false.
         endif

      enddo
      NUMEQN = k
      if ( NUMEQN.eq.0 ) return

      tbxmax = 0
      do k = 1, NUMEQN
         tbxmax = max(tbxmax,NCOL(k))
      enddo

      call tbca_tbdiff							!How many diff tables?
      if ( ST_FAILED ) return

      TBXM = 1								!Get tables
      if ( TBZ.ne.0 ) then
         TBXM = 0
         TBY = 0
         do k = 1, TBZ
            intab = 'IN '
            intab(3:3) = TBTOT(k)(2:2)
            call optabr ( intab, IP(k), TBVX(k), ny, .false., ierr )
            if ( ierr.ne.0 ) then
               if ( ierr.eq.3 ) then
                  call printo ( 'All input tables must come from'//
     +                          'different files' )
                  call printo ( 'To input a table more than once'//
     +                         '- refer to it by only one parameter')
               endif
               ST_FAILED = .true.
               return
            endif
            if ( (TBVX(k)-5).gt.999 ) then
               call printo ('ERROR: Cannot have more than 999 columns')
               ST_FAILED = .true.
               return
            endif
            if ( k.eq.1 ) then
               TBY = ny
            elseif ( ny.ne.TBY ) then
               call printo (
     +                'ERROR: Tables must have same number of rows' )
               ST_FAILED = .true.
               return
            endif
            TBXM = max(TBXM,(TBVX(k)-5))
         enddo
      endif

      call tbca_vardiff							!How many diff variables?
      if ( ST_FAILED ) return

      if ( NVARTOT.gt.0 ) then						!Get input variables
         do k = 1, NVARTOT
            call get1r ( VARTOT(k), VAR(k), 0.0, -1.0e37, +1.0e37 )
            if ( ST_FAILED ) return
         enddo
      endif

      kv = 0								!Random generator seed
      do k = 1, NUMEQN
         if ( index(EXPRS(k),'RAN').ne.0 ) kv = 1
         if ( index(EXPRS(k),'GAUSS').ne.0 ) kv = 1
      enddo
      KSEED = 1
      if ( kv.eq.1 ) call get1i ( 'SEED', KSEED, 1234567891,
     +                            1200000001, 1400000001 )
      if ( ST_FAILED ) return

      if ( TBZ.eq.0 ) then
         call get_job ( 'USE_TEMPLATE', 'yes:no', kopt, 2, ' ', 0 )
         if ( ST_FAILED ) return
         if ( kopt.eq.1 ) then
            call optabr ( 'INA', IP(1), TBVX(1), TBY, .false., ierr )
            if ( ST_FAILED ) return
            if ( ierr.ne.0 ) then
               ST_FAILED = .true.
               return
            endif
            if ( (TBVX(1)-5).gt.999 ) then
               call printo ('ERROR: Cannot have more than 999 columns')
               ST_FAILED = .true.
               return
            endif
            tempname = 'A'
            TBZ = 1
            TBXM = TBVX(1) - 5
            TBXO = TBVX(1) - 5
            TBVXO = TBVX(1)
            TEMPLATE = 1
            call get1b ( 'CLEAR', CLEAR, .false. )
            if ( ST_FAILED ) return
         else
            CLEAR = .true.
            TEMPLATE = 0
            call get1i ( 'NUMCOLS', TBXO, tbxmax, tbxmax, 999 )
            if ( ST_FAILED ) return
            TBVXO = TBXO + 5
            call get1i ( 'NUMROWS', TBY, 1, 1, 100000 )
            if ( ST_FAILED ) return
         endif
      else

         call get_job ( 'TEMPLATE', choice, kopt, 2, thelp, 0 )
         if ( ST_FAILED ) return
         if ( kopt.eq.1 ) then
            call get1i ( 'NUMCOLS', TBXO, tbxmax, tbxmax, 999 )
            if ( ST_FAILED ) return
            TBVXO = TBXO + 5
            TEMPLATE = 0
            CLEAR = .true.
         else
            ctemp = chnum(kopt)
            tempname = ctemp(2:2)
            kv = 0
            do k = 1, TBZ
               if ( ctemp.eq.TBTOT(k) ) kv = k
            enddo
            if ( kv.eq.0 ) then
               call printo ( 'ERROR: That table input not found' )
               ST_FAILED = .true.
               return
            endif
            TBXO = TBVX(kv) - 5
            TBVXO = TBVX(kv)
            if ( tbxmax.gt.TBXO ) then
               TBXO = tbxmax
               TBVXO = TBXO + 5
            endif
            TEMPLATE = kv
            CLEAR = .true.
            kn1 = 1
            k = 0
            do while ( k.lt.TBXO .and. kn1.eq.1 )
               k = k + 1
               kn2 = 0
               do j = 1, NUMEQN
                  if ( k.eq.NCOL(j) ) kn2 = 1
               enddo
               if ( kn2.eq.0 ) kn1 = 0
            enddo
            if ( kn1.eq.0 ) call get1b ( 'CLEAR', CLEAR, .false. )
            if ( ST_FAILED ) return
         endif
      endif

      call optabw ( 'OUT', IPO, TBVXO, TBY, .true., ierr )		!Get output table
      if ( ST_FAILED ) return
      if ( ierr.eq.1 ) then
         ST_FAILED = .true.
         return
      endif

      if ( TEMPLATE.ne.0 ) intab = 'IN'//tempname			!Input table for headers, descriptors

      if ( TEMPLATE.ne.0 ) then						!Copy descriptors
         call tcopdes ( intab, 'OUT', ierr )
         if ( ierr.eq.1 ) then
            ST_FAILED = .true.
            return
         endif
      endif

      title = 'Output from TBCALC' 					!Output table title
      if ( TEMPLATE.ne.0 ) call gtdesc ( intab, 'TITLE', title, title,
     +                                   kv, ierr )
      call get1c ( 'TITLE', title, title, .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUT', 'TITLE', title )

      if ( TEMPLATE.eq.0 ) then						!Put blank headers
         do k = 1, TBXO
            call pthead ( 'OUT', k, ' ', ierr )
         enddo
      else
         if ( TBVX(TEMPLATE).lt.TBVXO ) then
            do k = TBVX(TEMPLATE)+1-5, TBXO
               call pthead ( 'OUT', k, ' ', ierr )
            enddo
         endif
      endif

      do k = 1, NUMEQN							!New headers for working columns
         kv = NCOL(k)
         head = ' '
         if ( TEMPLATE.ne.0 ) call gthead ( intab, kv, head, ierr )
         if ( k.le.9 ) then
            write ( inheada, '(''HEAD'',i1)' ) k
            call get1c ( inheada, head, head, .true. )
         else
            write ( inheadb, '(''HEAD'',i2)' ) k
            call get1c ( inheada, head, head, .true. )
         endif
         if ( ST_FAILED ) return
         call pthead ( 'OUT', kv, head, ierr )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCA_BACK -- Load background
C
C  alan penny                RAL                1991 Dec

      subroutine tbca_back ()

      implicit none
      include 'tbcalc.inc'
      include 'STARMAN_INC'
C--
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      if ( TEMPLATE.eq.0 ) then						!Load background of output
         call azeror ( %val(IPO), TBVXO*TBY )
         call ident ( %val(IPO), TBVXO, TBY )
      else
         if ( CLEAR ) then
            call azeror ( %val(IPO), TBVXO*TBY )
            call coprr ( %val(IP(TEMPLATE)), TBVX(TEMPLATE), TBY,
     +                   1, 5, 1, TBY, %val(IPO), TBVXO, TBY, 1, 1 )
         else
            call coprr ( %val(IP(TEMPLATE)), TBVX(TEMPLATE), TBY,
     +                   1, TBVX(TEMPLATE), 1, TBY, %val(IPO), TBVXO,
     +                   TBY, 1, 1 )
            if ( TBVXO.gt.TBVX(TEMPLATE) ) then
               do k = 1, TBY
                  do j = TBVX(TEMPLATE)+1-5, TBXO
                     call cop1r ( 0.0, %val(IPO), TBVXO, TBY, (j+5), k )
                  enddo
               enddo
            endif
         endif
      endif


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCA_POLISH -- Decode equation and sort into reverse Polish
C
C  alan penny                RAL                1991 Dec

      subroutine tbca_polish ( exprs, opcode, noper, tbid, ntb, imp,
     +                         impc, varid, nvar, impv, con, ncon, ierr)

      implicit none
      include 'STARMAN_INC'

      character*70     exprs		!i: Equation to decode
      character*6      opcode(70)	!o: Polish stack of commands
      integer          noper		!o: No of operations
      character*2      tbid(70)		!o: Tables identifiers
      integer          ntb		!o: Number of different tables
      integer          imp(70)		!o: Pointers to tables
      integer          impc(70)		!o: Pointers to columns
      character*1      varid(70)	!o: Variables identifiers
      integer          nvar		!o: Number of variable
      integer          impv(70)		!o: Pointers to variables
      real             con(70)		!o: Constants
      integer          ncon		!o: Number of constants
      integer          ierr		!o: Error flag (0=ok;bad 2=;3=;4=)
C--

      character in*81, numchr*80, test*7, output*7, colid*3
      integer j, k, tos, stk(0:80), symb(80), ll, ncin, nnum,
     +        nsymb, iok, isymb, kextra, ndiff, istat
      real    const
      logical opnext, found, ischar, isnumb, issign, lmore, atend
      external ischar, isnumb, issign

      integer maxsym
      parameter ( maxsym=38 )

      character*7 oper(-3:maxsym), opsymb(-3:maxsym)
      integer opl(-3:maxsym), opr(-3:maxsym), l(-3:maxsym),
     +        prl(-3:maxsym), prr(-3:maxsym)

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),		!Set up table of operators,
     +        prr(j),j=-3,10 ) /					! symbols and their priorities
     +  '       ', 'LDCON  ', 1, 0, 0, 10, 10,
     +  '       ', 'LDVAR  ', 1, 0, 0, 10, 10,
     +  '       ', 'LDTB   ', 2, 0, 0, 10, 10,
     +  '=      ', '=      ', 1, 1, 1,  0,  0,
     +  ')      ', ')      ', 1, 1, 0,  2, 10,
     +  '(      ', '(      ', 1, 0, 1, 10,  1,
     +  '-      ', '-      ', 1, 1, 1,  4,  4,
     +  '+      ', '+      ', 1, 1, 1,  4,  4,
     +  '**     ', '**     ', 2, 1, 1,  9,  6,
     +  '*      ', '*      ', 1, 1, 1,  5,  5,
     +  '/      ', '/      ', 1, 1, 1,  5,  5,
     +  ':      ', ':      ', 1, 1, 1,  2,  2,
     +  '-      ', 'NEG    ', 1, 0, 1,  8,  7,
     +  '+      ', 'POS    ', 1, 0, 1,  8,  7 /

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),
     +        prr(j),j=11,20 ) /
     +  'SQRT(  ', 'SQRT(  ', 5, 0, 1, 10,  1,
     +  'EXP(   ', 'EXP(   ', 4, 0, 1, 10,  1,
     +  'LOG10( ', 'LOG10( ', 6, 0, 1, 10,  1,
     +  'LOG(   ', 'LOG(   ', 4, 0, 1, 10,  1,
     +  'SIN(   ', 'SIN(   ', 4, 0, 1, 10,  1,
     +  'COS(   ', 'COS(   ', 4, 0, 1, 10,  1,
     +  'TAN(   ', 'TAN(   ', 4, 0, 1, 10,  1,
     +  'ASIN(  ', 'ASIN(  ', 5, 0, 1, 10,  1,
     +  'ACOS(  ', 'ACOS(  ', 5, 0, 1, 10,  1,
     +  'ATAN(  ', 'ATAN(  ', 5, 0, 1, 10,  1 /

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),
     +        prr(j),j=21,30 ) /
     +  'ATAN2( ', 'ATAN2( ', 6, 0, 1, 10,  1,
     +  'SINH(  ', 'SINH(  ', 5, 0, 1, 10,  1,
     +  'COSH(  ', 'COSH(  ', 5, 0, 1, 10,  1,
     +  'TANH(  ', 'TANH(  ', 5, 0, 1, 10,  1,
     +  'ABS(   ', 'ABS(   ', 4, 0, 1, 10,  1,
     +  'AINT(  ', 'AINT(  ', 5, 0, 1, 10,  1,
     +  'ANINT( ', 'ANINT( ', 6, 0, 1, 10,  1,
     +  'MOD(   ', 'MOD(   ', 4, 0, 1, 10,  1,
     +  'SIGN(  ', 'SIGN(  ', 5, 0, 1, 10,  1,
     +  'DIM(   ', 'DIM(   ', 4, 0, 1, 10,  1 /

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),
     +       prr(j),j=31,maxsym ) /
     +  'MIN(   ', 'MIN(   ', 4, 0, 1, 10,  1,
     +  'MAX(   ', 'MAX(   ', 4, 0, 1, 10,  1,
     +  'CLIP(  ', 'CLIP(  ', 5, 0, 1, 10,  1,
     +  'GAUSS( ', 'GAUSS( ', 6, 0, 1, 10,  1,
     +  'RAN(   ', 'RAN(   ', 4, 0, 1, 10,  1,
     +  'RR     ', 'RR     ', 2, 0, 0, 10, 10,
     +  'CC     ', 'CC     ', 2, 0, 0, 10, 10,
     +  '%      ', '/      ', 1, 1, 1,  5,  5 /
Cbegin


      if ( ST_FAILED ) return

      ierr = 0
      ncin = 0

      do k = 1, len(exprs)						!Remove embedded blanks
         if ( exprs(k:k).ne.' ' .and. ncin.lt.80  ) then		!Count no of characters
            ncin = ncin + 1
            in(ncin:ncin) = exprs(k:k)
         endif
      enddo

      exprs = in(1:ncin)						!Return the expression with blanks removed

      ncin = ncin+1							!Append an '= ' operator to terminate the expression
      in(ncin:) = '='

      ntb = 0								!Initiallise counters
      nvar = 0
      ncon = 0
      k = 1
      j = 0

      opnext = .false.							!Indicates if an operator is expected next
									! first entity must not look like an operator

      lmore = .true.							!Loop thru OPLs until '='found
      do while ( lmore )

         found = .false.						!Search through the list of symbols to
         nsymb = -1							! identify which comes next
         do while ( nsymb.lt.maxsym .and. .not.found )
            nsymb = nsymb + 1
            if ( opnext.eqv.(opl(nsymb).eq.1) ) then			!Symbol is only valid if it looks like
               test = in(k:min(k+l(nsymb)-1,81))			! an operator or operand from the left, as appropriate
               if ( test.eq.oper(nsymb) ) found = .true.
             endif
         enddo

         if ( .not.found ) then						!If symbol was not found: -
            if ( opnext ) then						!Error if an operator expected
               ierr = 3
               call printo ( 'ERROR: Cant understand Equation - ' )
               call printo ( 'ERROR: '//exprs )
               return
            else if ( in(k:k).eq.'T' .and. ischar(in(k+1:k+1)) .and.	!If an operand was expected, it may be
     +               isnumb(in(k+2:k+2)) ) then				! a table, variable or const.
               nsymb = -1						!If it is a table, add name to table stack
               ntb = ntb + 1
               tbid(ntb) = in(k:k+1)
               colid = '000'
               if ( .not.isnumb(in(k+3:k+3)) ) then
                  colid(3:3) = in(k+2:k+2)
                  kextra = 1
               elseif ( .not.isnumb(in(k+4:k+4)) ) then
                  colid(2:3) = in(k+2:k+3)
                  kextra = 2
               else
                  colid = in(k+2:k+4)
                  kextra = 3
               endif
               call chartoi ( colid, impc(ntb), istat )			!Add column to column stack
               if ( istat.ne.0 .or. impc(ntb).eq.0 ) then
                  ierr = 1
                  call printo (
     +            'ERROR: Bad column ID - '//tbid(ntb)//colid )
                  call printo ( 'ERROR: '//exprs )
                  return
               endif
            else if ( ischar(in(k:k)) ) then
               nsymb = -2						!If it is a variable, add name to variable stack
               nvar = nvar + 1
               varid(nvar) = in(k:k)
            else

               nnum = 0							!Otherwise it may be a constant...
               numchr = ' '						! extract contiguous numerical characters

               atend = .false.						!Character may be part of a numerical constant
               ll = k - 1						! if it is 0..9 or '.'
               do while ( ll.lt.ncin .and. .not.atend )			! or if it is an 'e' following one of the above
                  ll = ll + 1						! or if it is a sign following an 'e'
                  if ( isnumb(in(ll:ll)) .or. (in(ll:ll).eq.'E') .or.
     +           (issign(in(ll:ll)).and.(in(ll-1:ll-1).eq.'E')) ) then
                     nnum = nnum + 1
                     numchr(nnum:nnum) = in(ll:ll)
                  else
                     atend = .true.					!End of number as soon as one of the above tests fails
                  endif
               enddo
               call chartor ( numchr(:nnum), const, iok )		!Try to read these characters as a constant

               if ( iok.eq.0 .and. nnum.ne.0 ) then			!If successful, add constant to stack
                  ncon = ncon + 1
                  con(ncon) = const
                  nsymb = -3
                  l(nsymb) = nnum
               else
                  ierr = 2						!Otherwise there is a bad operand error
                  call printo ( 'ERROR: Cant understand Equation -' )
                  call printo ( 'ERROR: '//exprs )
                  return
               endif

            endif
         endif

         j = j + 1							!Put the identified symbol into the output
         symb(j) = nsymb						! array and move the input pointer to the
         k = k + l(nsymb)						! next symbol
         if ( nsymb.eq.-1 ) k = k + kextra

         opnext = opr(nsymb).ne.1					!Decide whether an operator or operand follows
         if ( opsymb(nsymb).eq.'=' ) lmore = .false.

      enddo

      call azeroi ( stk(0), j+1 )					!Zero operator stack for converting to reverse polish
      tos = 0
      isymb = 1
      noper = 0

      lmore = .true.
      do while ( lmore )
         lmore = .false.

         if ( index(oper(stk(tos)),'(').ne.0 .and.			!If the top of stack and input stream have
     +        oper(symb(isymb)).eq.')' ) then				! matching parentheses, cancel them

            if ( oper(stk(tos)).ne.'(' ) then				!If there is a function asssociated with the
 									! opening parenthesis then send it to the output stream

               output = opsymb(stk(tos))				!Remove the enclosed '(' from functions first
               output(index(output,'('):index(output,'(')) = ' '
               tos = tos - 1
               isymb = isymb + 1
             else
               tos = tos - 1
               isymb = isymb + 1
               lmore = .true.
             endif

         else if ( prr(stk(tos)).ge.prl(symb(isymb)) ) then		!If the symbol on the top of the stack has
            output = opsymb(stk(tos))					! a high enough priority, transfer it to
            tos = tos - 1						! the output stream
         else
            tos = tos + 1						!Otherwise, transfer the next symbol to the stack
            stk(tos) = symb(isymb)
            isymb = isymb + 1
            lmore = .true. 						!Return for next test
         endif

         if ( .not.lmore ) then

            if ( index(output,'(').ne.0 ) then				!If a bracket appears in the output,
               ierr = 4							! it results from unpaired parentheses
               call printo ( 'ERROR: ) missing from Equation -' )	! in the input expression...quit qith error
               call printo ( 'ERROR: '//exprs )
               return
            elseif ( index(output,')').ne.0 ) then
               ierr = 4
               call printo ( 'ERROR: (  missing from Equation -' )
               call printo ( 'ERROR: '//exprs )
               return
            endif

            if ( output.ne.'POS'.and.output.ne.':' ) then		!If there is some output, disregard it
               noper = noper + 1					! if it is unary + or a comma
               opcode(noper) = output
            endif

            if ( output.ne.'=' ) lmore = .true.				!Return for next symbol if not the end

         endif

      enddo

      if ( ntb.ge.1 ) then						!If tables or variables are referenced, sort
         call tbca_sort ( tbid, ntb, imp, ndiff, symb )			! their names into alphabetical order and obtain
         ntb = ndiff							! pointers to allow them to be accessed in their
      endif								! original order
      if ( nvar.ge.1 ) then
         call tbca_sort ( varid, nvar, impv, ndiff, symb )
         nvar = ndiff
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCA_DOIT -- Combine the tables
C
C  alan penny                     RAL            1991 Dec

      subroutine tbca_doit ( tb, tbx, tby, opcode, nopcode, imp, impc,
     +                       impv, var, con, tbcol, kseed, kx, ierr )

      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'

      integer      tbx			!i: Max X size of input tables
      integer      tby			!i: Max Y size of input tables
      real         tb(tbx,tby,*)	!i: Input tables
      integer      nopcode		!i: Number of operations
      character*6  opcode(nopcode)	!i: Code for operations
      integer      imp(70)		!i: Number of table in (i)th posn
      integer      impc(70)		!i: Number of column in (i)th posn
      integer      impv(70)		!i: Number variable in (i)th posn
      real         var(26)		!i: Variables
      real         con(70)		!i: Constants
      real         tbcol(tby)		!o: Output column
      integer      kseed		!i/o: Random number seed
      integer      kx                   !i: Column number
      integer      ierr			!o: Error flag (0=ok;1=bad)
C--
      logical found, bad, more
      integer j, k, tos, numitab, nvar, ncon, ib, nop
      real    rv
      double precision s(0:80), a, b, c, dib, dvmin, dvmax
      integer opwhich(70)

      integer nsymb
      parameter ( nsymb=37 )

      character opsymb(nsymb)*6						!Recognised operations
      data opsymb /
     + 'LDCON ','LDVAR ','LDTB  ','=     ',
     + '-     ','+     ','**    ','*     ','/     ','NEG   ','SQRT  ',
     + 'EXP   ','LOG10 ','LOG   ','SIN   ','COS   ','TAN   ','ASIN  ',
     + 'ACOS  ','ATAN  ','ATAN2 ','SINH  ','COSH  ','TANH  ','ABS   ',
     + 'AINT  ','ANINT ','MOD   ','SIGN  ','DIM   ','MIN   ','MAX   ',
     + 'CLIP  ','GAUSS ','RAN   ','RR    ','CC    ' /
Cbegin


      if ( ST_FAILED ) return

      ierr = 0								!Failure flag

      dvmax = INT_MAXRR							!Set (dble precision) max and min vals
      dvmin = INT_MINRR

      do k = 1, nopcode							!Find `opcode' for each input operator
         found = .false.
         j = 0
         do while ( j.lt.nsymb .and. .not.found )
            j = j + 1
            if ( opcode(k).eq.opsymb(j) ) then
               opwhich(k) = j
               found = .true.
            endif
         enddo
         if ( .not.found ) then
            ierr = 1
            return
         endif
      enddo

      do j = 1, tby							!Do for all rows in a column

         tos = 0							!Initialise the arith, table,
         numitab = 0							! variable, constant stack pointers
         nvar = 0
         ncon = 0

         nop = 0							!Do the operations
         more = .true.
         bad = .false.

         do while ( nop.lt.nopcode .and. more )
            nop = nop + 1

            go to ( 101,102,103,104,105,106,107,108,109,110,111,112,
     +              113,114,115,116,117,118,119,120,121,122,123,124,
     +              125,126,127,128,129,130,131,132,133,134,135,
     +              136,137 ),
     +            opwhich(nop)

 101        continue
               tos = tos + 1						!Load constant on to stack
               ncon = ncon + 1
               s(tos) = con(ncon)
            go to 100
 102        continue
               tos = tos + 1						!Load variable onto stack
               nvar = nvar + 1
               s(tos) = var(impv(nvar))
            go to 100
 103        continue
               tos = tos + 1						!Load table on to stack
               numitab = numitab + 1
               s(tos) = tb(impc(numitab),j,imp(numitab))
            go to 100
 104        continue
               tbcol(j) = min(dvmax,max(dvmin,s(tos)))			!=  : end of calculation
               more = .false.
            go to 100
 105        continue
               s(tos-1) = s(tos-1) - s(tos)				!- : subtract tos
               tos = tos - 1
            go to 100
 106        continue
               s(tos-1) = s(tos-1) + s(tos)				!+ : add tos
               tos = tos - 1
            go to 100
 107        continue
               a = s(tos-1)						!** : raise to power
               b = s(tos)
               if ( a.ge.0.0 ) then
                  c = a**b
               else
                  ib = nint(b)
                  dib = ib
                  if ( abs(dib-b).le.1.0d-7 ) then
                     c = a**ib
                  else
                     bad = .true.
                  endif
               endif
               tos = tos - 1
               s(tos) = c
            go to 100
 108        continue
               s(tos-1) = s(tos-1)*s(tos)				!* : multiply tos
               tos = tos - 1
            go to 100
 109        continue
               a = s(tos)						!/ : Divide tos
               if ( a.ne.0.0d0 ) then
                  s(tos-1) = s(tos-1)/a
                  tos = tos - 1
               else
                  bad = .true.
               endif
            go to 100
 110        continue
               s(tos) = -s(tos)						!NEG : Negate tos
            go to 100
 111        continue
              a = s(tos)						!SQRT : Square root tos
               if ( a.ge.0.0d0 ) then
                  s(tos) = sqrt(a)
               else
                  bad = .true.
               endif
            go to 100
 112        continue
               a = s(tos)						!EXP : E to power tos
               if ( a.le.100.0d0 ) then
                  s(tos) = exp(a)
               else
                  bad = .true.
               endif
            go to 100
 113        continue
               a = s(tos)						!LOG10 : Log tos base 10
               if ( a.gt.0.0d0 ) then
                  s(tos) = log10(a)
               else
                  bad = .true.
               endif
            go to 100
 114        continue
               a = s(tos)						!LOG : Natural log of tos
               if ( a.gt.0.0d0 ) then
                  s(tos) = log(a)
               else
                  bad = .true.
               endif
            go to 100
 115        continue
               s(tos) = sin(s(tos))					!SIN : Sine of tos
            go to 100
 116        continue
               s(tos) = cos(s(tos))					!COS : Cosine of tos
            go to 100
 117        continue
               a = s(tos)						!TAN : Tangent of tos
               s(tos) = tan(a)
            go to 100
 118        continue
            a = s(tos)							!ASIN : Arcsine of tos
               if ( abs(a).le.1.0d0 ) then
                  s(tos) = asin(a)
               else
                  bad = .true.
               endif
            go to 100
 119        continue
               a = s(tos)						!ACOS : Arccosine of tos
               if ( abs(a).le.1.0d0 ) then
                  s(tos) = acos(a)
               else
                  bad = .true.
               endif
            go to 100
 120        continue
               s(tos) = atan(s(tos))					!ATAN : Arctangent of tos
            go to 100
 121        continue
               a = s(tos-1)						!ATAN2 : Arctangent of tos ratio
               b = s(tos)
               if ( a.ne.0.0d0 .or. b.ne.0.0d0 ) then
                  tos = tos - 1
                  s(tos) = atan2(a,b)
               else
                  bad = .true.
               endif
            go to 100
 122        continue
               a = s(tos)						!SINH : Hyperbolic sine of tos
               if ( abs(a).le.100.0d0 ) then
                  s(tos) = sinh(a)
               else
                  bad = .true.
               endif
            go to 100
 123        continue
               a = s(tos)						!COSH : Hyperbolic cosine of tos
               if ( abs(a).le.100.0d0 ) then
                  s(tos) = cosh(a)
               else
                  bad = .true.
               endif
            go to 100
 124        continue
               s(tos) = tanh(s(tos))					!TANH : Hyperbolic tangent of tos
            go to 100
 125        continue
               s(tos) = abs(s(tos)) 					!ABS : Absolute value of tos
            go to 100
 126        continue
               s(tos) = aint(s(tos))					!AINT : Truncate tos
            go to 100
 127        continue
               s(tos) = anint(s(tos))					!ANINT : Nearest integer to tos
            go to 100
 128        continue
               a = s(tos-1)						!MOD : Modulo on two tos entries
               b = s(tos)
               if ( b.ne.0.0d0 ) then
                  tos = tos - 1
                  s(tos) = mod(a,b)
               else
                  bad = .true.
               endif
            go to 100
 129        continue
               s(tos-1) = sign(s(tos-1),s(tos))				!SIGN : Transfer of sign between two tos entries
               tos = tos - 1
            go to 100
 130        continue
               s(tos-1) = dim(s(tos-1),s(tos))				!DIM : Positive difference of two tos entries
               tos = tos - 1
            go to 100
 131        continue
              s(tos-1) = min(s(tos-1),s(tos))				!MIN : Minimum of two tos entries
               tos = tos - 1
            go to 100
 132        continue
               s(tos-1) = max(s(tos-1),s(tos))				!MAX : Maximum of two tos entries
               tos = tos - 1
            go to 100
 133        continue
               a = s(tos-2)						!CLIP : Abort if tos lies outside window
               b = s(tos-1)
               c = s(tos)
               if ( a.le.b .and. b.le.c ) then
                  tos = tos - 2
                  s(tos) = b
               else
                  bad = .true.
               endif
            go to 100
 134        continue
               call gasdev ( rv, kseed )				!GAUSS : Put gaussian noise on tos
               s(tos) = s(tos)*dble(rv)
            go to 100
 135        continue
               call rano ( rv, kseed )					!RAN : Uniform random noise
               s(tos) = s(tos)*dble(rv)
            go to 100
 136        continue
               tos = tos + 1                                            !RR : Row number
               s(tos) = j
            go to 100
 137        continue
               tos = tos + 1                                            !CC : Column number
               s(tos) = kx
            go to 100

 100        continue

            if ( bad ) then						!Invalid arithmetic operation done
               tbcol(j) = 0.0
               more = .false.
            endif

         enddo

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCA_TBDIFF -- Get number of diff input tables and point to stack
C
C  alan penny                         RAL                1991 Dec

      subroutine tbca_tbdiff ( )

      implicit none
      include 'tbcalc.inc'
      include 'STARMAN_INC'
C--
      integer j, k, kk
      character ch*2
      logical found
Cbegin


      if ( ST_FAILED ) return

      TBZ = 0
      if ( NUMEQN.eq.0 ) return

      do k = 1, NUMEQN
         if ( NTB(k).ne.0 ) then
            do j = 1, NTB(k)
               if ( TBZ.eq.0 ) then
                  TBZ = 1
                  TBTOT(TBZ) = TBID(j,k)(1:2)
               else
                  found = .false.
                  ch = TBID(j,k)(1:2)
                  do kk = 1, TBZ
                     if ( ch.eq.TBTOT(kk) ) found = .true.
                  enddo
                  if ( .not.found ) then
                     TBZ = TBZ + 1
                     TBTOT(TBZ) = TBID(j,k)(1:2)
                  endif
               endif
            enddo
         endif
      enddo

      do k = 1, NUMEQN
         do j = 1, 70
            if ( IMP(j,k).ne.0 ) then
               ch = TBID(IMP(j,k),k)(1:2)
               do kk = 1, TBZ
                  if ( ch.eq.TBTOT(kk) ) IMP(j,k) = kk
               enddo
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCA_VARDIFF -- Get number of diff input variables and point to stack
C
C  alan penny                         RAL                1991 Dec

      subroutine tbca_vardiff ( )

      implicit none
      include 'tbcalc.inc'
      include 'STARMAN_INC'
C--
      integer j, k, kk
      character ch*1
      logical found
Cbegin


      if ( ST_FAILED ) return

      NVARTOT = 0
      if ( NUMEQN.eq.0 ) return

      do k = 1, NUMEQN
         if ( NVAR(k).ne.0 ) then
            do j = 1, NVAR(k)
               if ( NVARTOT.eq.0 ) then
                  NVARTOT = 1
                  VARTOT(NVARTOT) = VARID(j,k)(1:1)
               else
                  found = .false.
                  ch = VARID(j,k)(1:1)
                  do kk = 1, NVARTOT
                     if ( ch.eq.VARTOT(kk) ) found = .true.
                  enddo
                  if ( .not.found ) then
                     NVARTOT = NVARTOT + 1
                     VARTOT(NVARTOT) = VARID(j,k)(1:1)
                  endif
               endif
            enddo
         endif
      enddo

      do k = 1, NUMEQN
         do j = 1, 70
            if ( IMPV(j,k).ne.0 ) then
               ch = VARID(IMPV(j,k),k)(1:1)
               do kk = 1, NVARTOT
                  if ( ch.eq.VARTOT(kk) ) IMPV(j,k) = kk
               enddo
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCA_LOAD -- Copy table into a 3-D stack
C
C  alan penny                 ral                     1991 Dec

      subroutine tbca_load ( tb, tbvx, tby, nin, tbs, tbxs, tbys )

      implicit none

      integer     tbvx			!i: X size (inc 5 for name) of input table
      integer     tby			!i: Y size of input table
      real        tb(tbvx,tby)		!i: Input table
      integer     nin			!i: Plane to put it in
      integer     tbxs			!i: X size of output table
      integer     tbys			!i: Y size of output table
      real        tbs(tbxs,tbys,*)	!i/o: Output table
C--
      integer j, k
Cbegin


      do k = 1, tbys
         if ( k.le.tby ) then
            do j = 1, tbxs
               if (j.le.(tbvx-5) ) then
                  tbs(j,k,nin) = tb((j+5),k)
               else
                  tbs(j,k,nin) = 0.0
               endif
            enddo
         else
            do j = 1, tbys
               tbs((j+5),k,nin) = 0.0
            enddo
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCA_SORT -- Sort identifiers to alphabetical order
C
C  alan penny            ral                     1991 Dec

      subroutine tbca_sort ( c, nc, imp, ndiff, iw )

      implicit none

      integer        nc		!i: Number of identifiers
      character*(*)  c(nc)	!i/o: Identiers
      integer        imp(nc)	!o: pointer to identifier place in stack
      integer        ndiff	!o: Number of different identifiers
      integer        iw(nc)	!o: Work space
C--
      integer k, nn, it
      character*80 text
      logical repeat
Cbegin


      ndiff = 1								!Only 1?
      iw(1) = 1
      imp(1) = 1
      if ( nc.le.1 ) return

      nn = min(80,len(c(1)))						!Length of input strings

      do k = 1, nc							!Initiallise pointers in workspace
         iw(k) = k
      enddo

      repeat = .true.							!Perform a bubble sort to put
      do while ( repeat ) 						! character strings into alphabetical order
         repeat = .false.
         do k = 1, nc-1
            if ( c(k+1).lt.c(k) ) then
               repeat = .true.
               text(:nn) = c(k+1)					!Swap pairs which are in the wrong order
               c(k+1) = c(k)
               c(k) = text(:nn)
               it = iw(k+1)						!Permute the pointers in the same way
               iw(k+1) = iw(k)
               iw(k) = it
            endif
         enddo
      enddo

      ndiff = 1								!Pointers now point to original positions. scan list to
      imp(iw(1)) = 1							! remove repeated entries and reverse the pointing direction
      do k = 2, nc
         if ( c(k).ne.c(ndiff) ) then					!If a different character string is found, count it and
            ndiff = ndiff + 1						! put it in the correct place in the list
            c(ndiff) = c(k)
         endif
         imp(iw(k)) = ndiff						!Set the appropriate output pointer to its new location
      enddo


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C This is T_TBCHART.FOR
C
C It contains:-
C
C T_TBCHART      Plot out realistic-looking star map
C TBCH_OPTION_SETUP  Set up option choices
C TBCH_SETUP     Load the default parameters
C TBCH_PRANGE    Get plot range
C TBCH_PSIZE     Get plot size
C TBCH_MRANGE    Get magnitude range
C TBCH_MSCALE    Get magnitude circle size scale
C TBCH_LSIZE     Get Labels size
C TBCH_GETDATA   Get Table
C TBCH_CLEAR     Clear display
C TBCH_OPEN      Open device
C TBCH_CLOSE     Close device
C TBCH_PLOT      Set up and start plotting
C TBCH_DOPLOT    Plot points
C TBCH_CIRCLE    Write/overwrite circles round stars
C TBCH_DOLABEL   Plot labels
C TBCH_GCURSE    Get positions with cursor
C TBCH_DATLOAD   Load input data
C TBCH_FINDCOL   Find columns (x:y:mag:ht) is in



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBCHART -- Plot out realistic-looking star map
C
C         A J Penny            RAL            1991 May

      subroutine t_tbchart ( )

      implicit none
      include 'tbchart.inc'
      include 'ST_DS_PANEL_INC'
      include 'STARMAN_INC'
C--
      integer istat
      logical loop
      character*12 ktopt
Cbegin


      if ( ST_FAILED ) return

      call tbch_setup							!Set up parameters

      call type_hchoice
      call tbch_option_setup ( ktopt, 1, .true. )			!Do the work
      loop = .true.
      do while ( loop )

         call tbch_option_setup ( ktopt, 1, .false. )			!Do the work
         call get_choice ( ktopt, 1 )					!Get choice

         if ( ktopt.eq.'getdata' ) call tbch_getdata			!Open a set of files

         if ( ktopt.eq.'plot' ) call tbch_plot ( 1 )			!Display points

         if ( ktopt.eq.'clear' ) call tbch_clear			!Clear display

         if ( ktopt.eq.'open' ) call tbch_open				!Open device

         if ( ktopt.eq.'mrange' ) call tbch_mrange ( %val(IPTAB) )	!Change magnitude limits

         if ( ktopt.eq.'prange' ) call tbch_prange ( %val(IPTAB) )	!Allowed plot range

         if ( ktopt.eq.'panel' ) call choice_panel_sw 			!Change panel/keyboard input choice

         if ( ktopt.eq.'close' ) call tbch_close			!Close plot

         if ( ktopt.eq.'lsize' ) call tbch_lsize 			!Set labels size

         if ( ktopt.eq.'mscale' ) call tbch_mscale ( %val(IPTAB) )	!Labels and their size

         if ( ktopt.eq.'label' ) call tbch_plot ( 2 ) 			!Plot labels

         if ( ktopt.eq.'psize' ) call tbch_psize ( %val(IPTAB) )	!Labels and their size

         if ( ktopt.eq.'cursor' ) call tbch_gcurse ( %val(IPTAB) )	!Get positions

         if ( ktopt.eq.'ring' ) call tbch_circle ( %val(IPTAB), 1 )	!Write circles

         if ( ktopt.eq.'ring_rem' ) call tbch_circle ( %val(IPTAB), 2 )	!Overwrite circles

         if ( ktopt.eq.'exit' ) loop = .false. 				!Exit from program

         if ( ST_FAILED ) return

      enddo

      if ( DOPANEL ) call ds_p_close ( istat )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_OPTION_SETUP -- Set up option choices
C
C   alan penny                        ral              1990-01-31

      subroutine tbch_option_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'
      include 'ST_CHOICE_INC'

      character*12   ktopt              !i: Chosen option
      integer set_num                   !i: Code for set of options
      logical  koutside                 !i: Is this called from outside loop?
C--
      integer j, k

      integer opt_num
      parameter ( opt_num=16 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1), opt_head(1), (opt_help(j,1),j=1,6) /
     + 'clear', 'Clear plot',
     + 'Clear the plotted chart (the programme does not forget about',
     + 'the input table).',
     + ' ', ' ', ' ', ' '/

      data opt_text(2), opt_head(2), (opt_help(j,2),j=1,6) /
     + 'open', 'Open the plot device ',
     + 'Open a device for the plot. This uses the PGPLOT package. You',
     + 'put in the GKS name of the device. A standard device is ',
     + '-xwindows- for opening a window on an X-terminal. To find the',
     + 'GKS name of a device, type -ask-.',
     + 'For a device which makes a file, you must press the close',
     + 'button to close the device, before plotting the file.' /

      data opt_text(3), opt_head(3), (opt_help(j,3),j=1,6) /
     + 'close', 'Close the graphical device ',
     + 'One may need to close the plot output display. It is a',
     + 'PGPLOT -device-, and one might want to swop between screen ',
     + '(xwindows) and printer, or one might want to get the ',
     + 'output from printer (this file only becomes available when',
     + 'the device is -closed-). ',
     + '[All plot options open a device, if it is closed.]'/

      data opt_text(4), opt_head(4), (opt_help(j,4),j=1,6) /
     + 'panel', 'Switch between panel and keyboard option selection',
     + 'This returns you to using the keyboard for option choice.',
     + '(In keyboard option entry mode, you can get back to -panel-',
     + '  option entry mode, by choosing the -panel- option.',
     + ' ', ' ', ' '/

      data opt_text(5), opt_head(5), (opt_help(j,5),j=1,6) /
     + 'exit', 'Exit from this program',
     + 'Exit from this program. Any windows open are closed, and any',
     + 'files open are closed.',
     + ' ', ' ', ' ', ' '/

      data opt_text(6), opt_head(6), (opt_help(j,6),j=1,6) /
     + 'ring', 'Write rings round stars',
     + ' ', 'Put a large ring round each solid circle.',
     + ' ', 'This helps you to sort out stars which overlap each',
     + 'other.', ' '/

      data opt_text(7), opt_head(7), (opt_help(j,7),j=1,6) /
     + 'cursor', 'Use cursor to get position and nearest star',
     + 'Place the cursor at any point in the plot and press',
     + 'any button. The XY position will be typed out, together',
     + 'with (number, name, XY posn, mag) of the nearest star',
     + 'Place the cursor outside the area of the plot (but still',
     + 'in the window) and press for return. [Only give a quick',
     + 'press on return, for you may work the panel again.]'/

      data opt_text(8), opt_head(8), (opt_help(j,8),j=1,6) /
     + 'label', 'Plot by each star its name from input table',
     + 'The (up to) 20 character name for each star kept in the ',
     + 'the first input table are plotted by the stars. If there',
     + 'is no room by them, the names are plotted at the nearest',
     + 'clear space and a line drawn connecting name and star.',
     + ' ',
     + 'Name sizes are set via the LSIZE option and parameter.'/

      data opt_text(9), opt_head(9), (opt_help(j,9),j=1,6) /
     + 'plot', 'Plot the stars as filled-in circles' ,
     + 'The X,Y,Mag values of the stars are taken from input',
     + 'table columns 6,7,8. These are plotted as filled circles',
     + 'whose radii are - (magmin-magnitude)*magscale - . Thus',
     + 'brighter stars are plotted bigger. The - magmin and  ',
     + 'magscale factors are input via the MSCALE option. MRANGE',
     + 'and PRANGE control the range in mag and posn plotted. '/

      data opt_text(10), opt_head(10), (opt_help(j,10),j=1,6) /
     + 'ring_rem', 'Remove rings written round stars',
     + ' ', 'Remove the large ring round each solid circle.',
     + ' ', 'This removes the rings which you have written to',
     + 'help you to sort out stars which overlap each other.',
     + ' '/

      data opt_text(11), opt_head(11), (opt_help(j,11),j=1,6) /
     + 'lsize', 'Set the size of the names written by the stars',
     + ' ',
     + 'If the star names are written by the stars, this controls',
     + 'how large the type of the names is. The character size',
     + 'is proportional to the number input via the LSIZE ',
     + 'parameter. The default is 1.0.',
     + ' '/

      data opt_text(12), opt_head(12), (opt_help(j,12),j=1,6) /
     + 'mrange', 'Set the magnitude range of stars to be plotted',
     + ' ',
     + 'This sets the range in magnitude that stars are can have',
     + 'and be plotted. Stars falling outside the permitted',
     + 'magnitude range will not be plotted.',
     + ' ',
     + 'The defaults are the minimum and maximum of all the data. '/

      data opt_text(13), opt_head(13), (opt_help(j,13),j=1,6) /
     + 'mscale', 'Set rate which stars get bigger with magnitude',
     + 'The radii of the filled circles stars are plotted as is',
     + 'set by the equation - radius=(magmin-magnitude)*magscale -',
     + ' ',
     + 'This option lets you change the values MAGMIN and ',
     + 'MAGSCALE via the MMIN and MSCALE parameters. ',
     + ' '/

      data opt_text(14), opt_head(14), (opt_help(j,14),j=1,6) /
     + 'prange', 'Set the range in X an Y of stars to be plotted',
     + ' ',
     + 'This sets the range in X and Y that stars are allowed to be',
     + 'plotted in. Data points falling outside the permitted ',
     + 'XY rectangle will not be plotted.',
     + ' ',
     + 'The defaults are the minimum and maximum of all the data. '/

      data opt_text(15), opt_head(15), (opt_help(j,15),j=1,6) /
     + 'psize', 'Set the range of the graph axes in X and Y',
     + ' ',
     + 'This sets the range of the scales on the X and Y axes of',
     + 'the plot. ',
     + ' ',
     + 'The defaults are from a bit below the minimum of all the',
     + 'stars positions to a bit above the maximum. '/

      data opt_text(16), opt_head(16), (opt_help(j,16),j=1,6) /
     + 'getdata', 'Get star posns and magns from an input table' ,
     + ' ',
     + 'The data are taken from a Starman table in a file. The',
     + 'X, Y, and Magnitude are assumed to be in columns 6, 7, and',
     + '8 of the table (the parameter IN is used). ',
     + ' ',
     + 'If a new table is input, the previous data are lost.'/

      character*50 title, option
      integer ncode
      data title, option, ncode / 'Tbchart', 'OPTION', 1 /

      integer def_x, def_y
      parameter ( def_x=9 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'getdata', 'prange', 'mrange', 'psize',
     +                'open', 'plot', 'panel', 'cursor' /

      integer sect_num
      parameter ( sect_num=5 )
      character*10 sect_head(sect_num)
      data sect_head / 'ACTIONS', 'SETUP', 'FILE', 'GRAPH',
     +                 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1) / 'plot:label:cursor:ring:ring_rem' /
      data sect_text(2) / 'mscale:lsize:mrange:prange:psize' /
      data sect_text(3) / 'getdata' /
      data sect_text(4) / 'clear:close:open' /
      data sect_text(5) / 'panel:exit' /

      integer help_num
      parameter ( help_num=3 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,help_num) /
     + '             ' ,
     + 'To input cursor positions, position cursor at desired point',
     + 'and press any key. End by locating cursor outside graph' /
Cbegin


      if ( ST_FAILED ) return

      call setup_option ( ktopt, set_num, koutside,
     +                    sect_num, sect_text, sect_head,
     +                    title, option, ncode,
     +                    1, opt_num, opt_text,
     +                    1, opt_head,
     +                    1, opt_help,
     +                    1, help_num, help_text,
     +                    1, def_x, def_y, def_text )


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_SETUP -- Load the default parameters
C
C     a j penny                 ral               1991 May

      subroutine tbch_setup ( )

      implicit none
      include 'tbchart.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call ds_sdef ( 2, 14 )

      LSIZE = 1.0
      GOTDATA = .false.
      IPTAB = 1
      IPWK = 1
      GOTWORK = .false.
      NEWWORK = .false.

      PRX(1) = -1.0e10
      PRX(2) = 1.0e10
      PRY(1) = -1.0e10
      PRY(2) = 1.0e10
      GOTPLIMS  = .false.
      PSX(1) = -1.0e10
      PSX(2) = 1.0e10
      PSY(1) = -1.0e10
      PSY(2) = 1.0e10
      GOTPSIZE = .false.

      DML(1) = -1.0e10
      DML(2) = 1.0e10
      GOTMLIMS = .false.

      MAGMIN = 25.0
      MAGSC = 1.0
      GOTMSCALE = .false.

      GROPEN    = .false.
      DONEAXIS = .false.
      DONECIRCLE = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_PRANGE -- Get plot range
C
C     a j penny                 ral               1991 May

      subroutine tbch_prange ( table )

      implicit none
      include 'tbchart.inc'
      include 'STARMAN_INC'

      real    table(TBVX,TBY)		!i: Input table
C--
      integer k
      real  trunc_e
      external trunc_e
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTDATA ) then
         call printo ( 'WARNING: Need to have got table' )
         return
      endif

      if ( .not.GOTPLIMS ) then
         PRX(1) = table(6,1)
         PRX(2) = table(6,1)
         PRY(1) = table(7,1)
         PRY(2) = table(7,1)
         do k = 1, TBY
            PRX(1) = min(PRX(1),table(6,k))
            PRX(2) = max(PRX(2),table(6,k))
            PRY(1) = min(PRY(1),table(7,k))
            PRY(2) = max(PRY(2),table(7,k))
         enddo
         PRX(1) = trunc_e(PRX(1),1)
         PRX(2) = trunc_e(PRX(2),1)
         PRY(1) = trunc_e(PRY(1),1)
         PRY(2) = trunc_e(PRY(2),1)
      endif

      if ( .not.GOTPSIZE ) then
         PSX(1) = PRX(1)
         PSX(2) = PRX(2)
         PSY(1) = PRY(1)
         PSY(2) = PRY(2)
      endif

      call get2r ( 'XRANGE', PRX(1), PRX(2), .true., -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return
      call cswopr ( PRX(1), PRX(2) )

      call get2r ( 'YRANGE', PRY(1), PRY(2), .true., -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return
      call cswopr ( PRY(1), PRY(2) )

      GOTPLIMS = .true.


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_PSIZE -- Get plot size
C
C     a j penny                 ral               1991 May

      subroutine tbch_psize ( table )

      implicit none
      include 'tbchart.inc'
      include 'STARMAN_INC'

      real    table(TBVX,TBY)		!i: Input table
C--
      real rv, oldx, oldy
      integer k
      real  trunc_e
      external trunc_e
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTDATA ) then
         call printo ( 'WARNING: Need to have got table' )
         return
      endif

      if ( .not.GOTPSIZE ) then
         PSX(1) = table(6,1)
         PSX(2) = table(6,1)
         PSY(1) = table(7,1)
         PSY(2) = table(7,1)
         do k = 1, TBY
            PSX(1) = min(PSX(1),table(6,k))
            PSX(2) = max(PSX(2),table(6,k))
            PSY(1) = min(PSY(1),table(7,k))
            PSY(2) = max(PSY(2),table(7,k))
         enddo
         rv = (PSX(2)-PSX(1))/20.0
         PSX(1) = PSX(1) - rv
         PSX(2) = PSX(2) + rv
         rv = (PSY(2)-PSY(1))/20.0
         PSY(1) = PSY(1) - rv
         PSY(2) = PSY(2) + rv
         PSX(1) = trunc_e(PSX(1),1)
         PSX(2) = trunc_e(PSX(2),1)
         PSY(1) = trunc_e(PSY(1),1)
         PSY(2) = trunc_e(PSY(2),1)
      endif

      oldx = PSX(2) - PSX(1)
      call get2r ( 'DEVLIMX', PSX(1), PSX(2), .true., -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return
      call cswopr ( PSX(1), PSX(2) )

      oldy = PSY(2) - PSY(1)
      call get2r ( 'DEVLIMY', PSY(1), PSY(2), .true., -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return
      call cswopr ( PSY(1), PSY(2) )

      GOTPSIZE = .true.
      if ( (PSX(2)-PSX(1)).ne.oldx .or.
     +     (PSY(2)-PSY(1)).ne.oldy ) NEWWORK = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_MRANGE -- Get magnitude range
C
C     a j penny                 ral               1991 May

      subroutine tbch_mrange ( table )

      implicit none
      include 'tbchart.inc'
      include 'STARMAN_INC'

      real    table(TBVX,TBY)		!i: Input table
C--
      integer k
      real am, htmin, htmax
      real trunc_e
      external trunc_e
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTDATA ) then
         call printo ( 'WARNING: Need to have got table' )
         return
      endif

      if ( IN_MAG ) then
         if ( .not.GOTMLIMS ) then
            DML(1) = table(8,1)
            DML(2) = table(8,1)
            do k = 1, TBY
               am = table(8,k)
               if ( am.lt.49.0 ) then
                  DML(1) = min(DML(1),am)
                  DML(2) = max(DML(2),am)
               endif
            enddo
            DML(1) = DML(1) - 1.0
            DML(2) = DML(2) + 1.0
            DML(1) = trunc_e(DML(1),2)
            DML(2) = trunc_e(DML(2),2)
         endif
         call get2r ( 'MRANGE', DML(1), DML(2), .true.,-1.0e10,1.0e10 )
         if ( ST_FAILED ) return
      else
         if ( .not.GOTMLIMS ) then
            DML(1) = table(8,1)
            DML(2) = table(8,1)
            do k = 1, TBY
               am = table(8,k)
               if ( am.lt.49.0 ) then
                  DML(1) = min(DML(1),am)
                  DML(2) = max(DML(2),am)
               endif
            enddo
         endif
         htmax = 10.0**((30.0-DML(1))/2.5)
         htmin = 10.0**((30.0-DML(2))/2.5)
         call get2r ( 'HRANGE', htmin, htmax, .true., 1.0e-10, 1.0e20 )
         if ( ST_FAILED ) return
         DML(1) = 30.0 - 2.5*alog10(htmax)
         DML(2) = 30.0 - 2.5*alog10(htmin)
      endif

      call cswopr ( DML(1), DML(2) )

      GOTMLIMS = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_MSCALE -- Get magnitude circle size scale
C
C     a j penny                 ral               1991 May

      subroutine tbch_mscale ( table )

      implicit none
      include 'tbchart.inc'
      include 'STARMAN_INC'

      real    table(TBVX,TBY)		!i: Input table
C--
      integer k
      real rv, ht
      real trunc_e
      external trunc_e
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTDATA ) then
         call printo ( 'WARNING: Need to have got table' )
         return
      endif

      if ( IN_MAG ) then
         if ( .not.GOTMSCALE ) then
            MAGMIN = -100.0
            do k = 1, TBY
               rv = table(8,k)
               if ( rv.lt.49.0 ) MAGMIN = max(rv,MAGMIN)
            enddo
            MAGMIN = MAGMIN + 1.0
            MAGMIN = trunc_e(MAGMIN,2)
         endif
         call get1r ( 'MMIN', MAGMIN, MAGMIN, -1.0e10, 1.0e10 )
         if ( ST_FAILED ) return
      else
         if ( .not.GOTMSCALE ) then
            MAGMIN = -1000.0
            do k = 1, TBY
               rv = table(8,k)
               if ( rv.lt.49.0 ) MAGMIN = max(rv,MAGMIN)
            enddo
         endif
         ht = 10.0**((30.0-MAGMIN)/2.5)
         call get1r ( 'HMIN', ht, ht, 1.0e-10, 1.0e20 )
         if ( ST_FAILED ) return
         MAGMIN = 30.0 - 2.5*alog10(ht)
      endif

      call get1r ( 'MSCALE', MAGSC, MAGSC, -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return

      GOTMSCALE = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_LSIZE -- Get Labels size
C
C     a j penny                 ral               1991 May

      subroutine tbch_lsize ( )

      implicit none
      include 'tbchart.inc'
      include 'STARMAN_INC'
C--
      real old
Cbegin


      if ( ST_FAILED ) return

      old = LSIZE
      call get1r ( 'LSIZE', LSIZE, LSIZE, 0.0, 1.0e10 )
      if ( ST_FAILED ) return
      if ( LSIZE.ne.old ) NEWWORK = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_GETDATA -- Get Table
C
C     a j penny                 ral               1991 May

      subroutine tbch_getdata ( )

      implicit none
      include 'tbchart.inc'
      include 'STARMAN_INC'
C--
      integer ierr, isok, istat, iptemp, kdo(4), khead(4), kwarn(4),
     +        kbad(4)
Cbegin


      if ( ST_FAILED ) return

      if ( GOTDATA ) then
         call wrkcan ( 'WORK' )
         GOTDATA = .false.
      endif

      call optabr ( 'IN', iptemp, TBVX, TBY, .true., istat )
      if ( ST_FAILED ) return
      isok = 0
      if ( istat.ne.0 ) then
         isok = 1
      elseif ( TBVX.lt.8 ) then
         call printo ( 'ERROR: Table does not have 3 columns' )
         isok = 1
      elseif ( TBY.lt.1 ) then
         call printo ( 'ERROR: Table has no entries' )
         isok = 1
      endif
      if ( isok.eq.1 ) then
         call canpar ( 'IN' )
         return
      endif

      GOTDATA = .true.
      kdo(1) = 1
      kdo(2) = 1
      kdo(3) = 1
      kdo(4) = 0
      kwarn(1) = 1
      kwarn(1) = 1
      kwarn(3) = 0
      kwarn(4) = 0

      call tbch_findcol ( 'IN', kdo, kwarn, TBVX-5, khead, kbad )
      if ( kbad(3).eq.1 ) then
         kdo(1) = 0
         kdo(2) = 0
         kdo(3) = 0
         kdo(4) = 1
         call tbch_findcol ( 'IN', kdo, kwarn, TBVX-5, khead, kbad )
      endif
      call pargi ( khead(1) )
      call pargi ( khead(2) )
      if ( kbad(3).eq.0 ) then
         IN_MAG = .true.
         call pargi ( khead(3) )
         call printd ( '   X, Y, Magn are in columns %d , %d , %d ' )
      else
         if ( kbad(4).eq.0 ) then
            IN_MAG = .false.
            call pargi ( khead(4) )
            call printd (
     +                '   X, Y, Height are in columns %d , %d , %d ' )
         else
            IN_MAG = .true.
            call printd (
     +  '   X,Y are in columns %d , %d ; Column 3 is taken as magns' )
            khead(3) = 3
            kbad(3) = 0
         endif
      endif

      call gtwrkr ( 'WORK', TBY*8, IPTAB, ierr )
      call tbch_datload ( %val(iptemp), TBVX, TBY, khead, kbad,
     +                    %val(IPTAB) )
      TBVX = 8
      call canpar ( 'IN' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_CLEAR -- Clear display
C
C    a j penny                      ral                  1991 may

      subroutine tbch_clear ( )

      implicit none
      include 'tbchart.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GROPEN ) then						!Is device open?
         call printo ( 'ERROR: Device not open' )
         return
      endif

      NEWWORK = .true.
      DONEAXIS = .false.
      call pgpage


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_OPEN -- Open device
C
C   a j penny                       ral         1991 may

      subroutine tbch_open ( )

      implicit none
      include 'tbchart.inc'
      include 'STARMAN_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      if ( GROPEN ) then
         call printo ( 'ERROR: Device already open' )
      else
         call gd_open ( istat )
         if ( istat.eq.0 ) then
            GROPEN = .true.
            DONEAXIS = .false.
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_CLOSE -- Close device
C
C   a j penny                  ral         1991 may

      subroutine tbch_close ( )

      implicit none
      include 'tbchart.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GROPEN ) then
         call printo ( 'ERROR: Device not open' )
      else
         call gd_close
         GROPEN = .false.
         DONEAXIS = .false.
         NEWWORK = .true.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_PLOT -- Set up and start plotting
C
C   a j penny                 ral                  1991 may

      subroutine tbch_plot ( kopt )

      implicit none
      include 'tbchart.inc'
      include 'STARMAN_INC'

      integer   kopt		!i: 1=plot points;2=plot labels
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GROPEN ) call tbch_open					!Is device open?
      if ( .not.GROPEN ) return

      if ( .not.GOTDATA ) call tbch_getdata				!Loaded data?

      if ( .not.GOTPLIMS ) call tbch_prange ( %val(IPTAB) )		!Get posn limits

      if ( .not.GOTMLIMS ) call tbch_mrange ( %val(IPTAB) )		!Get mag limits

      if ( .not.GOTMSCALE ) call tbch_mscale ( %val(IPTAB) )		!Get mag scale

      if ( .not.GOTPSIZE ) call tbch_psize ( %val(IPTAB) )		!Get plot size

      if ( .not.DONEAXIS ) call gd_dobox ( PSX(1), PSX(2), 'X', 	!Draw axes
     +                             PSY(1), PSY(2), 'Y', ' ', 1 )
      DONEAXIS = .true.

      if ( kopt.eq.1 ) then
         call tbch_doplot ( %val(IPTAB) )				!Plot the data
      else
         if ( GOTWORK .and. NEWWORK ) then				!Get work space for
            call wrkcan ( 'WORKA' )					! modelling picture
            GOTWORK = .false.
         endif
         if ( .not.GOTWORK ) then
            NY = 100.0/max(0.01,LSIZE) + 1.0
            NX = NY*abs(PSX(2)-PSX(1))/abs(PSY(2)-PSY(1)) + 1.0
            call gtwrks ( 'WORKA', NX*NY, IPWK, istat )
            call azeros ( %val(IPWK), NX*NY )
            if ( istat.ne.0 ) then
               call printo ( 'ERROR: Cant open work space WORKA' )
               return
            endif
            GOTWORK = .true.
            NEWWORK = .false.
         endif
         call tbch_dolabel ( %val(IPTAB), %val(IPWK) )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_DOPLOT -- Plot points
C
C   a j penny               ral                     1991 may

      subroutine tbch_doplot ( table )

      implicit none

      include 'tbchart.inc'
      include 'STARMAN_INC'

      real        table(TBVX,TBY)	!i: Input data
C--
      real xd(32), yd(32), xp(32), yp(32), x, y, am, rv, r
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      do j = 1, 32
         rv = real(j-1)*2.0*3.141593/32.0
         xd(j) = cos(rv)
         yd(j) = sin(rv)
      enddo

      do k = 1, TBY							!Plot stars
         x = table(6,k)
         y = table(7,k)
         am = table(8,k)
         if ( am.lt.MAGMIN .and. x.ge.PRX(1) .and. x.le.PRX(2) .and.
     +        y.ge.PRY(1) .and. y.le.PRY(2) .and.
     +        am.ge.DML(1) .and. am.le.DML(2) ) then

            r = (MAGMIN-am)*MAGSC
            do j = 1, 32
               xp(j) = x + r*xd(j)
               yp(j) = y + r*yd(j)
            enddo
            call pgpoly ( 32, xp, yp )

         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_CIRCLE -- Write/overwrite circles round stars
C This is to be able to distinguish overlapping stars
C
C   a j penny              ral                       1991 may

      subroutine tbch_circle ( table, kopt )

      implicit none

      include 'tbchart.inc'
      include 'STARMAN_INC'
      include 'ST_GRAPH_INC'

      real        table(TBVX,TBY)	!i: Input data
      integer     kopt			!i: Control flag (1=write/2=overwrite)
C--
      real xd(32), yd(32), xp(33), yp(33), x, y, am, rv, r
      integer j, k
Cbegin


      if ( ST_FAILED ) return

      if ( kopt.eq.2 .and. .not.DONECIRCLE ) then
         call printo ( 'ERROR: No circles written to overwrite' )
         return
      endif
      if ( kopt.eq.1 ) then
         DONECIRCLE = .true.
      else
         DONECIRCLE = .false.
      endif

      do j = 1, 32
         rv = real(j-1)*2.0*3.141593/32.0
         xd(j) = cos(rv)
         yd(j) = sin(rv)
      enddo

      call gd_bbuf
      if ( kopt.eq.2 ) call pgsci ( 0 )

      do k = 1, TBY
         x = table(6,k)
         y = table(7,k)
         am = table(8,k)
         if ( am.lt.MAGMIN .and. x.ge.PRX(1) .and. x.le.PRX(2) .and.
     +        y.ge.PRY(1) .and. y.le.PRY(2) .and.
     +        am.ge.DML(1) .and. am.le.DML(2) ) then

            r = 1.0 + ((MAGMIN-am)*MAGSC)
            do j = 1, 32
               xp(j) = x + r*xd(j)
               yp(j) = y + r*yd(j)
            enddo
            xp(33) = xp(1)
            yp(33) = yp(1)
            call pgline ( 33, xp, yp )

         endif
      enddo

      call pgsci ( 1 )
      call gd_ebuf
      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_DOLABEL -- Plot labels
C
C   a j penny               ral                       1991 may

      subroutine tbch_dolabel ( table, kdum )

      implicit none

      include 'tbchart.inc'
      include 'STARMAN_INC'

      real        table(TBVX,TBY)	!i: Input data
      integer*2   kdum(NX,NY)		!i: Dummy map of plot
C--
      character*20 text
      real x, y, am, r, rr, radl, xa, ya, ttsize, dx, dy, dd, plen,
     +     clen, dda, ddb, ddc, ddd, xs, ys, ang, xd, yd,
     +     oldsize, px, py
      integer k, jx, jy, kxa, kxb, kya, kyb, len, kturn, kangle, kok
      logical more
      real xsh(8), ysh(8), fxsh(8), fysh(8)
      data  xsh / 0.707, 1.0,  0.707,  0.0, -0.707, -1.0, -0.707, 0.0 /
      data  ysh / 0.707, 0.0, -0.707, -1.0, -0.707,  0.0,  0.707, 1.0 /
      data fxsh / 0.0,   0.0,  0.0,   -0.5, -1.0,   -1.0, -1.0,  -0.5 /
      data fysh / 0.0,  -0.5, -1.0,   -1.0, -1.0,   -0.5,  0.0,   0.0 /
Cbegin


      if ( ST_FAILED ) return

      call pgqch ( oldsize )
      call pgsch ( 0.6*LSIZE )

      ttsize = abs(PSY(2)-PSY(1))/real(NY)

      do k = 1, TBY							!Fill stars into dummy array
         x = table(6,k)
         y = table(7,k)
         am = table(8,k)
         if ( am.lt.MAGMIN .and. 					!Select only 'good' stars
     +        x.ge.PRX(1) .and. x.le.PRX(2) .and.
     +        y.ge.PRY(1) .and. y.le.PRY(2) .and.
     +        x.ge.PSX(1) .and. x.le.PSX(2) .and.
     +        y.ge.PSY(1) .and. y.le.PSY(2) .and.
     +        am.ge.DML(1) .and. am.le.DML(2) ) then

            xa = 1.0 + ((x-PSX(1))/ttsize)				!Posn in dummy array
            ya = 1.0 + ((y-PSY(1))/ttsize)
            r  = (MAGMIN-am)*MAGSC/ttsize

            kxa = max(1,min(NX,nint(xa-r)))				!Load into dummy
            kxb = max(1,min(NX,nint(xa+r)))
            kya = max(1,min(NY,nint(ya-r)))
            kyb = max(1,min(NY,nint(ya+r)))
            rr = r*r
            do jy = kya, kyb
               do jx = kxa, kxb
                  dx = xa - real(jx)
                  dy = ya - real(jy)
                  dd = dx*dx + dy*dy
                  if ( dd.le.rr ) kdum(jx,jy) = 1
               enddo
            enddo

         endif
      enddo

      do k = 1, TBY							!Plot labels

         x = table(6,k)							!Only for 'good' stars
         y = table(7,k)
         am = table(8,k)
         if ( am.lt.MAGMIN .and. x.ge.PRX(1) .and. x.le.PRX(2) .and.
     +        y.ge.PRY(1) .and. y.le.PRY(2) .and.
     +        am.ge.DML(1) .and. am.le.DML(2) ) then

            xa  = 1.0 + ((x-PSX(1))/ttsize)
            ya  = 1.0 + ((y-PSY(1))/ttsize)
            r = (MAGMIN-am)*MAGSC/ttsize + 2.0
            call namegt ( table, TBVX, TBY, k, text )
            call charln ( text, len )

            if ( len.ne.0 .and.						!Is label just '#',' ', or was this star too faint to plot?
     +           .not.(len.eq.1.and.(text(1:1).eq.'#')) ) then
               if (text(1:1).eq.'#') text(1:1)= ' '
               call lbgone ( text )
               call charln ( text, len )
               if ( len.lt.20 ) text(len+1:20) = ' '
               plen = real(len)*2.0*5.0/7.0
               clen = anint((real(len)*2.0*5.0/7.0) + 0.5)

               kturn = -1						!Set for 1st position
               more = .true.
               do while ( more )

                  kturn = kturn + 1					!Find next position in dummy array coords
                  kangle = mod(kturn,8) + 1
                  radl = r + 2.0*real(int(kturn/8))
                  xd = xa + radl*xsh(kangle) + clen*fxsh(kangle)
                  yd = ya + radl*ysh(kangle) + 2.0*fysh(kangle)
                  xd = anint(xd)
                  yd = anint(yd)

                  dda = (xa-1.0)**2.0      + (ya-1.0)**2.0		!See if tried all box
                  ddb = (xa-1.0)**2.0      + (ya-real(NY))**2.0
                  ddc = (xa-real(NX))**2.0 + (ya-1.0)**2.0
                  ddd = (xa-real(NX))**2.0 + (ya-real(NY))**2.0
                  dd = max(dda,ddb,ddc,ddd)
                  if ( (radl*radl).gt.dd ) more = .false.

                  if ( more ) then

                     kxa = xd						!See if any of this
                     kxb = xd + clen 					! position already occupied
                     kya = yd
                     kyb = yd + 2.0
                     kok = 1
                     do jy = kya, kyb
                        do jx = kxa, kxb
                           if ( jx.ge.2 .and. jx.le.(NX-1) .and.
     +                          jy.ge.2 .and. jy.le.(NY-1) ) then
                              if ( kdum(jx,jy).eq.1 ) then
                                 kok = 0
                              endif
                           else
                              kok = 0
                           endif
                        enddo
                     enddo

                     if ( kok.eq.1 ) then                               !If not and not stopped trying, put label

                        px = xd*ttsize + PSX(1) - 1.0			!Put label
                        py = yd*ttsize + PSY(1) - 1.0
                        call pgtext ( px, py, text(1:len) )

                        if ( kturn.ne.0 ) then				!Draw line if not in 1st posn
                           xs = px - ttsize*plen*fxsh(kangle)
                           ys = py - 2.0*ttsize*fysh(kangle)
                           ang = atan2((ys-y),(xs-x))
                           px = xs - 0.3*ttsize*cos(ang)
                           py = ys - 0.3*ttsize*sin(ang)
                           call pgmove ( px, py )
                           px = x + (r+0.5)*ttsize*cos(ang)
                           py = y + (r+0.5)*ttsize*sin(ang)
                           call pgdraw ( px, py )
                        endif

                        do jy = kya, kyb				!Fill in dummy array area
                           do jx = kxa, kxb
                              kdum(jx,jy) = 1
                           enddo
                        enddo

                        more = .false.

                     endif

                  endif

               enddo
            endif
         endif
      enddo

      call pgsch ( oldsize )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_GCURSE -- Get positions with cursor
C
C     a j penny                 ral               1991 May

      subroutine tbch_gcurse ( table )

      implicit none
      include 'tbchart.inc'
      include 'STARMAN_INC'

      real    table(TBVX,TBY)		!i: Input table
C--
      real x, y, am, xa, ya, dx, dy, dd, ddmin, ax, ay, axa, aya,
     +     ama
      integer k, kmin, len
      logical more
      character ch*1, text*20, texta*79
      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      if ( .not.DONEAXIS ) then
         call printo ( 'WARNING: Need to have plotted area' )
         return
      endif

      call printo ( '       X        Y   Number  Name      '//
     +              '                  X        Y     Mag' )
      more = .true.
      do while ( more )

         call pgcurse ( x, y, ch )

         if ( x.gt.PSX(2) .or. x.lt.PSX(1) .or.
     +        y.gt.PSY(2) .or. y.lt.PSY(1) ) then
            more = .false.
         else

            kmin = 1
            ddmin = 1.0e20
            do k = 1, TBY
               xa = table(6,k)						!Only for 'good' stars
               ya = table(7,k)
               am = table(8,k)
               if ( am.lt.MAGMIN .and. x.ge.PRX(1) .and. x.le.PRX(2)
     +              .and. y.ge.PRY(1) .and. y.le.PRY(2) .and.
     +              am.ge.DML(1) .and. am.le.DML(2) ) then
                  dx = x - xa
                  dy = y - ya
                  dd = dx*dx + dy*dy
                  if ( dd.lt.ddmin ) then
                     ddmin = dd
                     kmin = k
                  endif
               endif
            enddo

            ax = trunc(x,5)
            ay = trunc(y,5)
            axa = trunc(table(6,kmin),5)
            aya = trunc(table(7,kmin),5)
            ama = trunc(table(8,kmin),2)
            call namegt ( table, TBVX, TBY, kmin, text )
            call charln ( text, len )
            if ( len.ne.0 .and.						!Is label just '#',' ', or was this star too faint to plot?
     +           .not.(len.eq.1.and.(text(1:1).eq.'#')) ) then
               if (text(1:1).eq.'#') text(1:1)= ' '
               call lbgone ( text )
               call charln ( text, len )
            else
               len = 1
               text = ' '
            endif
            write ( texta, '(1x,2f9.2,2x,i5,2x,a20,2x,2f9.2,f7.3)' )
     +                     ax, ay, kmin, text, axa, aya, ama
            call printo ( texta )
         endif

      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_DATLOAD -- Load input data
C
C   alan penny          ral               1995 March

      subroutine tbch_datload ( in, tbvx, tby, khead, kbad, table )

      implicit none
      include 'STARMAN_INC'

      integer    tbvx           !i: No of columns (inc 5 for names)
      integer    tby            !i: No of rows
      real       in(tbvx,tby)   !i:Input table
      integer    khead(4)       !i: Column numbers for (x:y:mag:ht)
      integer    kbad(4)        !i: Flags for columns (x:y:mag:ht) (0=ok;1=bad)
      real       table(8,tby)   !o: Work table

C--
      integer j, ja, k
      real ht
Cbegin


      if ( ST_FAILED ) return

      do k = 1, tby

         do j = 1, 5
            table(j,k) = in(j,k)
         enddo

         ja = 5 + khead(1)
         table(6,k) = in(ja,k)
         ja = 5 + khead(2)
         table(7,k) = in(ja,k)
         if ( kbad(3).eq.0 ) then
            ja = 5 + khead(3)
            table(8,k) = in(ja,k)
         elseif ( kbad(4).eq.0 ) then
            ja = 5 + khead(4)
            ht = in(ja,k)
            ht = max(1.0e-8,ht)
            table(8,k) = 30.0 - 2.5*alog10(ht)
         else
            table(8,k) = in(8,k)
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCH_FINDCOL -- Find columns (x:y:mag:ht) is in
C
C   alan penny          ral               1995 March

      subroutine tbch_findcol ( tfile, kdo, kwarn, ktbx, khead, kbad )

      implicit none
      include 'STARMAN_INC'

      character*(*) tfile       !i: Name of parameter with input file
      integer       kdo(4)	!i: Do (x:y:mag:ht) search flag (0=no;1=yes)
      integer       kwarn(4)	!i: Warn (x:y:mag:ht) error flag (0=no;1=yes)
      integer       ktbx	!i: No of columns
      integer       khead(4)	!o: Column (x:y:mag:ht) is in
      integer       kbad(4)	!o: Error (x:y:mag:ht) flag (0=no;1=yes)
C--
      integer    k, ierr
      character  ahead*20, thead*20
Cbegin


      if ( ST_FAILED ) return

      if ( kdo(1).eq.1 ) then
         khead(1) = 0
         kbad(1) = 0
         do k = ktbx, 1, -1
            call gthead ( tfile, k, thead, ierr )
            if ( ierr.eq.0 ) then
               call lowcase ( thead, ahead )
               call lbgone ( ahead )
               if ( ahead.eq.'x' ) khead(1) = k
            endif
         enddo
         if ( khead(1).eq.0 ) then
            khead(1) = 1
            kbad(2) = 1
            if ( kwarn(1).eq.1 ) then
               call printo (
     +                ' WARNING: Header for X positions not found')
               call printo ( '          Must be x or X')
               call printo ( '          Assumed to be in column 1')
               call gthead ( tfile, 1, thead, ierr )
               call pargc ( thead )
               call printd ( '          Header in column 1 is %c' )
            endif
         endif
      endif

      if ( kdo(2).eq.1 ) then
         khead(2) = 0
         kbad(2) = 0
         do k = ktbx, 1, -1
            call gthead ( tfile, k, thead, ierr )
            if ( ierr.eq.0 ) then
               call lowcase ( thead, ahead )
               call lbgone ( ahead )
               if ( ahead.eq.'y' ) khead(2) = k
            endif
         enddo
         if ( khead(2).eq.0 ) then
            khead(2) = 2
            kbad(2) = 1
            if ( kwarn(2).eq.1 ) then
               call printo (
     +               ' WARNING: Header for Y positions not found')
               call printo ( '          Must be y or Y')
               call printo ( '          Assumed to be in column 2')
               call gthead ( tfile, 2, thead, ierr )
               if ( ierr.eq.0 ) then
                  call pargc ( thead )
                  call printd ( '          Header in column 2 is %c' )
               endif
            endif
         endif
      endif

      if ( kdo(3).eq.1 ) then
         khead(3) = 0
         kbad(3) = 0
         do k = ktbx, 1, -1
            call gthead ( tfile, k, thead, ierr )
            if ( ierr.eq.0 ) then
               call lowcase ( thead, ahead )
               call lbgone ( ahead )
               if ( ahead.eq.'mag' .or. ahead.eq.'mags' .or.
     +              ahead.eq.'magn' .or. ahead.eq.'magns' .or.
     +              ahead.eq.'magnitudes' .or. ahead.eq.'magnitudes' )
     +              khead(3) = k
             endif
         enddo
         if ( khead(3).eq.0 ) then
            khead(3) = 3
            kbad(3) = 1
            if ( kwarn(3).eq.1 ) then
               call printo (
     +              ' WARNING: Header for magnitudes not found')
               call printo (
     +         '          Must be mag/mags/magn/magns/magnitude/s')
               call printo ( '          (Case not important)')
               call printo ( '          Assumed to be in column 3')
               call gthead ( tfile, 3, thead, ierr )
               if ( ierr.eq.0 ) then
                  call pargc ( thead )
                  call printd ( '          Header in column 3 is %c' )
               endif
            endif
         endif
      endif

      if ( kdo(4).eq.1 ) then
         khead(4) = 0
         kbad(4) = 0
         do k = ktbx, 1, -1
            call gthead ( tfile, k, thead, ierr )
            if ( ierr.eq.0 ) then
               call lowcase ( thead, ahead )
               call lbgone ( ahead )
               if ( ahead.eq.'ht' .or. ahead.eq.'hts' .or.
     +              ahead.eq.'height' .or. ahead.eq.'heights' )
     +              khead(4) = k
            endif
         enddo
         if ( khead(4).eq.0 ) then
            khead(4) = 3
            kbad(4) = 1
            if ( kwarn(4).eq.1 ) then
               call printo (
     +                     ' WARNING: Header for heights not found')
               call printo ( '          Must be ht/hts/height/heights')
               call printo ( '          (Case not important)')
               call printo ( '          Assumed to be in column 3')
               call gthead ( tfile, 3, thead, ierr )
               if ( ierr.eq.0 ) then
                  call pargc ( thead )
                  call printd ( '          Header in column 3 is %c' )
               endif
            endif
         endif
      endif


      end









CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    TBCOMPS.FOR
C
C    Contains:-
C
C T_TBCOMPS     Find stars within/not within a given annulus of other stars
C TBCO_CHECK    Find number and locations of rows to output
C TBCO_TRANS    Load selected rows into output


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBCOMPS -- Find stars within/not within a given annulus of other stars
C
C         A J Penny            RAL			1991 May

      subroutine t_tbcomps ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      character title*50
      integer ierr, ipin, ipo, iv, ipk, kmeth, k
      real topl, botl

      integer nthelp
      parameter ( nthelp=5 )
      character*68 thelp(nthelp)
      data (thelp(k),k=1,nthelp) /
     + 'Option     Function' ,
     + '------      --------' ,
     + 'In          Find stars within annulus round any other star' ,
     + 'Not_in      Find stars not within annulus round any other ' ,
     + '             star' /

Cbegin


      call optabr ( 'IN', ipin, TBVX, TBY, .false., ierr )		!Input tbale
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return
      TBX = TBVX - 5

      call pargi ( TBY )						!Tell user table size
      call pargi ( TBX )
      call printd ( 'No of rows =  %d   No of columns = %d' )

      call get_job ( 'OPTION', 'in:not_in', kmeth, 1, thelp, nthelp  )	!Get selection method
      if ( ST_FAILED ) return

      botl = 0.0							!Get annulus
      topl = 20.0
      call get2r ( 'LIMITS', botl, topl, .true., 0.0, 1.0e10 )
      if ( ST_FAILED ) return

      call gtwrki ( 'WORK', TBY, ipk, ierr )				!Open work space
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call tbco_check ( %val(ipin), botl, topl, kmeth, %val(ipk) )	!Find no of entries passed
      if ( TBYO.eq.0 ) then
         call printo ( 'No positions found' )
         return
      endif

      call pargi ( TBYO )
      call printd ( 'No of positions output = %d ' )

      call optabw ( 'OUT', ipo, TBVX, TBYO, .false., ierr )		!Open output list
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call tcopdes ( 'IN', 'OUT', ierr )				!title to output list and store it and the descriptors
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call gtdesc ( 'IN', 'TITLE', title, 'Output from Tbcomps', iv,
     +              ierr )
      call get1c  ( 'TITLE', title, title, .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUT', 'TITLE', title )

      call tbco_trans ( %val(ipin), %val(ipo), %val(ipk) )	 	!Do the loading of the Output from the Input


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCO_CHECK -- Find number and locations of rows to output
C
C    a j penny               rgo                      82-11-4


      subroutine tbco_check ( tb, botl, topl, kmeth, klist )

      implicit none
      include 'ST_TABLE_INC'

      real	tb(TBVX,TBY)	!i: Input table
      real	botl		!i: Bottom limit
      real	topl		!i: Top limit
      integer   kmeth		!i: Flag for close (1) or far (2)
      integer	klist(TBY)	!o: Flags for chosen (1) or not (0)		!i: Find numer (1) or load (2)
C--
      integer k, j
      real rmin, rmax, d, xa, ya, xb, yb, rv
Cbegin


      rmin = botl*botl
      rmax = topl*topl

      if ( kmeth.eq.1 ) then

         call azeroi ( klist, TBY )
         do k = 1, TBY-1
            xa = tb(6,k)
            ya = tb(7,k)
            do j = k+1, TBY
               xb = tb(6,j)
               yb = tb(7,j)
               d = (xb-xa)*(xb-xa) + (yb-ya)*(yb-ya)
               if ( d.ge.rmin .and. d.le.rmax ) then
                  klist(j) = 1
                  klist(k) = 1
               endif
            enddo
         enddo

      else

         call amovki ( 1, klist, TBY )
         do k = 1, TBY
            xa = tb(6,k)
            ya = tb(7,k)
            do j = 1, TBY
               if ( j.ne.k .and. klist(j).eq.1 ) then
                  xb = tb(6,j)
                  yb = tb(7,j)
                  d = (xb-xa)*(xb-xa) + (yb-ya)*(yb-ya)
                  if ( d.ge.rmin .and. d.le.rmax ) klist(j) = 0
               endif
            enddo
         enddo

      endif

      call asumi ( klist, TBY, rv )
      TBYO = nint(rv)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCO_TRANS -- Load selected rows into output
C
C    a j penny               rgo                      82-11-4

      subroutine tbco_trans ( tb, tbo, klist )

      implicit none
      include 'ST_TABLE_INC'

      real	tb(TBVX,TBY)	!i: Input table
      real	tbo(TBVX,TBYO)	!o: Output table
      integer	klist(TBY)	!o: Flags for chosen (1) or not (0)
C--
      integer k, j
Cbegin

      j = 0
      do k = 1, TBY
         if ( klist(k).eq.1 ) then
            j = j + 1
            call amovr ( tb(1,k), tbo(1,j), TBVX )
         endif
      enddo


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   TBCUT.FOR
C
C   Contains:-
C
C T_TBCUT      Cut out and output parts of a table
C TBCU_CHECK   Get output size
C TBCU_DOIT    Load ouput from cut input


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBCUT -- Cut out and output parts of a table
C
C  alan penny                ral                      1990-06-15

      subroutine t_tbcut ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      character title*50, tc*10
      integer kc(2,20), kr(2,20), ipin, iv, i, ierr, k, ipo, k1, k2,
     +        numr, numc, ipwr, ipwc, ipwv, ierr1
      logical more
Cbegin


      call optabr ( 'IN', ipin, TBVX, TBY, .false., ierr )		!Obtain input table
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return
      TBX = TBVX - 5

      call pargi ( TBX )						!Print out size
      call pargi ( TBY )
      call printd ( 'No of columns = %d : No of rows = %d' )
      call printo ( ' ' )

      more = .true.							!Columns to cut
      numc = 0
      do while ( more )
         k = numc + 1
         call pargi ( k )
         call printd ( 'Column section number: %d ' )
         k1 = 0
         k2 = 0
         if ( k.eq.1 ) then
            k1 = 1
            k2 = TBX
         endif
         if ( k.ge.10 ) then
            write ( tc, '(''COL_'',i2)' ) k
         else
            write ( tc, '(''COL_'',i1)' ) k
         endif
         call get2i ( tc, k1, k2, .true., 0, TBX )
         if ( ST_FAILED ) return
         call cswopi ( k1, k2 )
         if ( k1.eq.0 ) then
            more = .false.
         else
            numc = numc + 1
            kc(1,numc) = k1
            kc(2,numc) = k2
         endif
         if ( numc.eq.10 ) more = .false.
      enddo
      if ( numc.eq.0 ) then
         ST_FAILED = .true.
         return
      endif

      call printo ( ' ' )
      more = .true.							!Rows to cut
      numr = 0
      do while ( more )
         k = numr + 1
         call pargi ( k )
         call printd ( 'Row section number: %d ' )
         k1 = 0
         k2 = 0
         if ( k.eq.1 ) then
            k1 = 1
            k2 = TBY
         endif
         if ( k.ge.10 ) then
            write ( tc, '(''ROW_'',i2)' ) k
         else
            write ( tc, '(''ROW_'',i1)' ) k
         endif
         call get2i ( tc, k1, k2, .true., 0, TBY )
         if ( ST_FAILED ) return
         call cswopi ( k1, k2 )
         if ( k1.eq.0 ) then
            more = .false.
         else
            numr = numr + 1
            kr(1,numr) = k1
            kr(2,numr) = k2
         endif
         if ( numr.eq.10 ) more = .false.
      enddo
      if ( numr.eq.0 ) then
         ST_FAILED = .true.
         return
      endif

      call gtwrki ( 'WORKR', TBY, ipwr, ierr )
      call gtwrki ( 'WORKC', TBX, ipwc, ierr1 )
      if ( ierr.ne.0 .or. ierr1.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call tbcu_check ( kc, numc, %val(ipwc), kr, numr, %val(ipwr) )	!Set up for output

      if ( TBXO.lt.1 .or. TBYO.lt.1 ) then				!Check output size
         if ( TBXO.lt.1 ) call printo (
     +      'WARNING: Output table has no columns - no file made' )
         if ( TBYO.lt.1 ) call printo (
     +      'WARNING: Output table has no rows - no file made' )
         return
      endif

      call pargi ( TBXO )						!Inform size
      call pargi ( TBYO )
      call printd ( 'Output: columns = %d : rows = %d ' )

      call optabw ( 'OUT', ipo, TBVXO, TBYO, .false., ierr )		!Open output list
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         call printo ( 'ERROR: Cant store output list' )
         return
      endif

      call tcopdes ( 'IN', 'OUT', ierr )				!Get title to output list and store it and the descriptors
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call gtdesc ( 'IN', 'TITLE', title, 'Output from Tbcut', iv, i )
      call get1c ( 'TITLE', title, title, .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUT', 'TITLE', title )

      call gtwrkr ( 'WORKV', TBXO, ipwv, ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call tbcu_doit ( %val(ipwr), %val(ipwc), %val(ipin), %val(ipo),	!Load the output
     +                 %val(ipwv) )

      call wrkcan ( 'WORKC' )
      call wrkcan ( 'WORKR' )
      call wrkcan ( 'WORKV' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCU_CHECK -- Get output size
C
C  alan penny                ral                      1990-06-15

      subroutine tbcu_check ( kc, numc, colc, kr, numr, rowc )

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      integer     kc(2,20)		!i: Column excisions
      integer     numc			!i: Number of column excisions
      integer     colc(TBX)		!o: Use a column? (0=no;1=yes)
      integer     kr(2,20)		!i: Row excisions
      integer     numr			!i: Number of column excisions
      integer     rowc(TBY)		!o: Use a row? (0=no;1=yes)
C--
      integer k, j
Cbegin


      if ( ST_FAILED ) return

      call azeroi ( colc, TBX )
      if ( numc.gt.0 ) then
         do k = 1, numc
            do j = kc(1,k), kc(2,k)
               colc(j) = 1
            enddo
         enddo
      endif

      TBXO = 0
      do k = 1, TBX
         if ( colc(k).eq.1 ) TBXO = TBXO + 1
      enddo
      TBVXO = TBXO + 5

      call azeroi ( rowc, TBY )
      if ( numr.gt.0 ) then
         do k = 1, numr
            do j = kr(1,k), kr(2,k)
               rowc(j) = 1
            enddo
         enddo
      endif

      TBYO = 0
      do k = 1, TBY
         if ( rowc(k).eq.1 ) TBYO = TBYO + 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCU_DOIT -- Load ouput from cut input
C
C  alan penny                ral                      1990-06-15

      subroutine  tbcu_doit ( rowc, colc, tb, tbo, rv )

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      integer   rowc(TBY)		!i: Use a row? (0=no;1=yes)
      integer   colc(TBX)		!i: Use a column? (0=no;1=yes)
      real      tb(TBVX,TBY)		!i: Input table
      real      tbo(TBVXO,TBYO)		!o: Output table
      real      rv(TBXO)		!o: Work space for output rows
C--
      integer j, ja, k, ky, ierr
      character head*20, ahead*7
Cbegin


      if ( ST_FAILED ) return

      ky = 0
      do k = 1, TBY
         if ( rowc(k).eq.1 ) then

            ky = ky + 1							!Move name
            call amovr ( tb(1,k), tbo(1,ky), 5 )

            ja = 0							!Move values
            do j = 1, TBX
               if ( colc(j).eq.1 ) then
                  ja = ja + 1
                  rv(ja) = tb((5+j),k)
               endif
            enddo
            call amovr ( rv, tbo(6,ky), TBXO )

         endif
      enddo

      ky = 0								!Load headers
      do k = 1, TBX
         if ( colc(k).eq.1 ) then
            ky = ky + 1
            head = ' '
            call gthead ( 'IN', k, head, ierr )
            call pthead ( 'OUT', ky, head, ierr )
         endif
      enddo

      if ( TBX.gt.TBXO ) then						!Delete extra headers
         do k =  TBXO+1, TBX
            call gthead ( 'IN', k, head, ierr )
            if ( ierr.eq.0 ) then
               write ( ahead, '(''HEAD'',i3.3)') k
               call dldes ( 'OUT', ahead, ierr )
            endif
         enddo
      endif


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  T_TBDES.FOR
C
C    Contains:-
C
C T_TBDES      List, write, delete descriptors of a table
C TBD_LIST     List descriptors
C TBD_SINGLE   Put out wanted descriptors
C TBD_ALL      Put out all descriptors
C TBD_WRITE    Write descriptors to table
C TBD_DELETE   Delete descriptors from a table
C TBD_COPY     Copy descriptors from one table to another
C TBD_GETEM    Get existing descriptors in output table


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBDES -- List, write, delete, and/or copy descriptors of a table
C   For a fuller description see TBDES.HLP
C
C   alan penny                     ral           1991 May

      subroutine t_tbdes ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer kout

      character*1000 topt
      data topt / 'list:write:delete:copy:' /
      integer nth
      parameter ( nth=6 )
      character*68 th(nth)
      data th /
     + 'Option    Function',
     + '------    --------',
     + 'Copy      Copy descriptors from one table to next',
     + 'Delete    Delete descriptors from table',
     + 'List      List descriptors of table',
     + 'Write     Write descriptors to table' /
Cbegin


      call get_job ( 'OPTION', topt, kout, 1, th, nth )
      if ( ST_FAILED ) return

      if ( kout.eq.1 ) call tbd_list

      if ( kout.eq.2 ) call tbd_write

      if ( kout.eq.3 ) call tbd_delete

      if ( kout.eq.4 ) call tbd_copy

      call canpar ( 'IN' )
      if ( kout.eq.4 ) call canpar ( 'OUT' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBD_LIST -- List descriptors of a table
C
C   alan penny                     ral           1991 May

      subroutine tbd_list ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer ip, ierr
      logical all, onlyone
Cbegin


      if ( ST_FAILED ) return

      call optabr ( 'IN', ip, TBVX, TBY, .false., ierr )		!Obtain input table
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call get1b ( 'ALL_LIST', all, .true. )
      if ( ST_FAILED ) return
      if ( all ) then
         call tbd_all
      else
         call get1b ( 'ONLYONE', onlyone, .false. )
         if ( ST_FAILED ) return
         call tbd_single ( onlyone )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBD_SINGLE -- Put out wanted descriptors
C
C  alan penny                 ral                1990-06-15

      subroutine tbd_single ( onlyone )

      implicit none
      include 'tbdes.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      logical   onlyone		!i:Do only one descriptor
C--
      logical more, amore, paging
      integer k, nval, ierr, kout, iv
      character descr*80, textb*80, name*20, blank*8
      data blank / '        ' /
Cbegin


      if ( ST_FAILED ) return

      call get1b ( 'PAGING', paging, .false. )
      if ( ST_FAILED ) return

      call pargi ( TBVX-5 )
      call pargi ( TBY )
      call printo ( ' ' )
      call printd ( 'No of columns = %d :  No of rows = %d ' )
      call gtdesn ( 'IN', 1, descr, ierr )
      if ( ierr.ne.0 ) then
         call printo ( 'No Headers or Descriptors' )
         return
      endif
      call gtdesc ( 'IN', 'TITLE', textb, ' ', iv, ierr )
      if ( ierr.ne.0 ) then
         call printo ( 'No Title' )
      else
         call pargc ( textb )
         call printd ( 'Title: %c ' )
      endif
      call printo ( ' ' )

      amore = .true.
      do while ( amore )
         call get1c ( 'NAME', name, ' ', .true. )
         if ( ST_FAILED ) return
         if ( name.eq.' ' ) then
            amore = .false.
         else
            call gtdesc ( 'IN', name, TEXTA, ' ', nval, ierr )
            if ( TEXTA(1).eq.' ' .or. ierr.ne.0 ) then
               call printo ( 'No such descriptor' )
            else
               k = 0
               kout = 0
               more = .true.
               do while ( k.lt.nval .and. more )
                  k = k + 1
                  if ( k.eq.1 ) then
                     textb = name(1:8)//'  '//TEXTA(k)(1:68)
                  else
                     textb = blank//'  '//TEXTA(k)(1:68)
                  endif
                  call printo ( textb )
                  kout = kout + 1
                  if ( .not.onlyone .and. kout.eq.21 ) then
                      kout = 0
                      if ( paging) call get1b ( 'MORE', more, .true. )
                      if ( ST_FAILED ) return
                  endif
               enddo
            endif
         endif
         if ( onlyone ) amore = .false.
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBD_ALL -- Put out all descriptors
C
C  alan penny                 ral                1990-06-15

      subroutine tbd_all ()

      implicit none
      include 'tbdes.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      logical more, first, paging
      integer k, ncount, nval, ierr, kout, iv
      character descr*80, text*80
Cbegin


      if ( ST_FAILED ) return

      call get1b ( 'PAGING', paging, .false. )
      if ( ST_FAILED ) return

      call pargi ( TBVX-5 )
      call pargi ( TBY )
      call printo ( ' ' )
      call printd ( 'No of columns = %d :  No of rows = %d ' )
      call gtdesn ( 'IN', 1, descr, ierr )
      if ( ierr.ne.0 ) then
         call printo ( 'No Headers or Descriptors' )
         return
      endif
      call gtdesc ( 'IN', 'TITLE', text, ' ', iv, ierr )
      if ( ierr.ne.0 ) then
         call printo ( 'No Title' )
      else
         call pargc ( text )
         call printd ( 'Title: %c ' )
      endif

      more = .true.							!Put out descriptors
      first = .true.
      kout = 0
      ncount = 0
      nval = 1
      do while ( more )
         ncount = ncount + nval
         call gtdesn ( 'IN', ncount, descr, ierr )
         if ( ierr.ne.0 ) then
            more = .false.
         else
            if ( descr(1:4).eq.'HEAD' .and.
     +            descr(5:5).ge.'0' .and. descr(5:5).le.'9' .and.
     +            descr(6:6).ge.'0' .and. descr(6:6).le.'9' .and.
     +            descr(7:7).ge.'0' .and. descr(7:7).le.'9'  ) then
               call gtdesc ( 'IN', descr, TEXTA, ' ', nval, ierr )
               if ( first ) then
                  call printo ( ' ' )
                  call printo ( 'Column Headers' )
                  call printo ( '--------------' )
                  call printo ( ' ' )
                  first = .false.
               endif
               if ( ierr.eq.0 ) then
                  k = 0
                  more = .true.
                  do while ( k.lt.nval .and. more )
                     k = k + 1
                     text = descr(1:8)//'  '//TEXTA(k)(1:68)
                     call printo ( text )
                     kout = kout + 1
                     if ( kout.eq.21 ) then
                        kout = 0
                        if ( paging) call get1b ( 'MORE', more, .true. )
                        if ( ST_FAILED ) return
                      endif
                  enddo
               endif
            endif
         endif
      enddo
      if ( first ) then
         call printo ( ' ' )
         call printo ( 'No Column Headers' )
         call printo ( ' ' )
      endif

      more = .true.							!Put out descriptors
      first = .true.
      kout = 0
      ncount = 0
      nval = 1
      do while ( more )
         ncount = ncount + nval
         call gtdesn ( 'IN', ncount, descr, ierr )
         if ( ierr.ne.0 ) then
            more = .false.
         else
            if ( descr(1:4).ne.'HEAD' .or.
     +            descr(5:5).lt.'0' .or. descr(5:5).gt.'9' .or.
     +            descr(6:6).lt.'0' .or. descr(6:6).gt.'9' .or.
     +            descr(7:7).lt.'0' .or. descr(7:7).gt.'9'  ) then
               if ( first ) then
                  call printo ( ' ' )
                  call printo (
     +                  'Descriptors that are not Column Headers' )
                  call printo (
     +                  '---------------------------------------' )
                  call printo ( ' ' )
                  first = .false.
               endif
               call gtdesc ( 'IN', descr, TEXTA, ' ', nval, ierr )
               if ( ierr.eq.0 ) then
                  k = 0
                  more = .true.
                  do while ( k.lt.nval .and. more )
                     k = k + 1
                     text = descr(1:8)//'  '//TEXTA(k)(1:68)
                     call printo ( text )
                     kout = kout + 1
                     if ( kout.eq.21 ) then
                        kout = 0
                        if ( paging) call get1b ( 'MORE', more, .true. )
                        if ( ST_FAILED ) return
                      endif
                  enddo
               endif
            endif
         endif
      enddo
      if ( first ) then
         call printo ( ' ' )
                  call printo (
     +               'No descriptors that are not Column Headers' )
         call printo ( ' ' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBD_WRITE -- Write descriptors to table
C
C  p morris      leeds                   Jun 1992

      subroutine tbd_write ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      include 'ST_TABLE_INC'
C--
      integer ierr, iv, ip
      real rv
      character desnam*20, desna*20, val*72, type*30, atype*30,
     +          utype*30
      logical bv, loop, first
Cbegin


      if ( ST_FAILED ) return

      call optabr ( 'IN', ip, TBVX, TBY, .false., ierr )		!Obtain input table
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call get1b ( 'ONLYONE', bv, .false. )				!Loop needed?
      if ( ST_FAILED ) return
      loop = .not.bv

      first = .true.
      type = 'char'
      do while ( loop .or. first )					!Get and write descriptor(s)
         call get1c ( 'NAME', desna, ' ', .true. )
         if ( ST_FAILED ) return
         call uppcase ( desna, desnam )
         if ( desnam.eq.' ' ) then
            loop = .false.
         else
            atype = type
            call get_option ( 'FORMAT', 'char:real:integer', 1, type,
     +                        atype, ' ', 0 )
            if ( ST_FAILED ) return
            call uppcase ( type, utype )
            if ( utype .eq. 'CHAR' ) then
               call gtdesc ( 'IN', desnam, val, ' ', iv, ierr )
               call get1c ( 'CVALUE', val, val, .true. )
               if ( ST_FAILED ) return
               call ptdesc ( 'IN', desnam, val )
            elseif ( utype .eq. 'REAL' ) then
               call gtdesr ( 'IN', desnam, rv, 0.0, ierr )
               call get1r ( 'RVALUE', rv, rv, INT_MINRR, INT_MAXRR )
               if ( ST_FAILED ) return
               call ptdesr ( 'IN', desnam, rv  )
            elseif ( utype .eq. 'INTEGER' ) then
               call gtdesi ( 'IN', desnam, iv, 0, ierr )
               call get1i ( 'IVALUE', iv, iv, INT_MINII, INT_MAXII )
               if ( ST_FAILED ) return
               call ptdesi ( 'IN', desnam, iv )
            else
               call printo ( 'Invalid type' )
               ierr = 1
            endif
            if ( ierr.ne.0 ) return
         endif
         first = .false.
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBD_DELETE -- Delete descriptors from a table/table
C
C  p morris             leeds       jun 1992

      subroutine tbd_delete ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer ierr, ip
      character desnam*20
      logical bv, loop, first
Cbegin


      if ( ST_FAILED ) return

      call optabr ( 'IN', ip, TBVX, TBY, .false., ierr )		!Obtain input table
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call get1b ( 'ONLYONE', bv, .false. )				!Loop needed?
      if ( ST_FAILED ) return
      loop = .not.bv

      first = .true.
      do while ( loop .or. first )					!Get and write descriptor(s)
         call get1c ( 'NAME', desnam, ' ', .true. )
         if ( ST_FAILED ) return
         if ( desnam.eq.' ' ) then
            loop = .false.
         else
            call dldes ( 'IN', desnam, ierr )
         endif
         if ( ierr .ne. 0 ) then
            ST_FAILED = .false.
            loop = .false.
         endif
         first = .false.
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBD_COPY -- Copy descriptors from one table to another
C  Optionally you can choose to overwrite or not descriptors that
C  already occur in the target file.
C
C   alan penny                ral      1990 jan

      subroutine tbd_copy ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      character*80 text(16384)
      character*60 atext(10000)
      character*8  adescr(10000)
      integer numdes(10000), ierr, j, k, ip1, ip2, nval, ntot, ncount
      logical over, dele
Cbegin


      if ( ST_FAILED ) return

      call optabr ( 'IN', ip1, TBVX1, TBY1, .false., ierr )		!Get input
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call optabr ( 'OUT', ip2, TBVX2, TBY2, .false., ierr )		!Open output
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call get1b ( 'OVER', over, .true. )				!Get wether to overwrite
      if ( ST_FAILED ) return

      if ( over ) call get1b ('DELETE', dele, .true. )			!Get wether to delete before overwriting
      if ( ST_FAILED ) return

      if ( over ) then
         if ( dele ) then
            call tcopdes ( 'IN', 'OUT', ierr )
            if ( ierr.ne.0 ) then
               ST_FAILED = .true.
               return
            endif
         else
            call tbd_getem ( adescr, numdes, atext, ncount )
            call tcopdes ( 'IN', 'OUT', ierr )
            if ( ierr.ne.0 ) then
               ST_FAILED = .true.
               return
            endif
            ntot = 0
            do k = 1, ncount
               call gtdesc ( 'OUT', adescr(k), text, ' ', nval,
     +                              ierr )
               if ( ierr.ne.0 ) then
                  do j = 1, numdes(k)
                     text(j) = atext(ntot+j)
                  enddo
                  call ptdescn ( 'OUT', adescr(k), text, numdes(k) )
               endif
               ntot = ntot + numdes(k)
            enddo
         endif
      else
         call tbd_getem ( adescr, numdes, atext, ncount )
         call tcopdes ( 'IN', 'OUT' ,ierr )
         if ( ierr.ne.0 ) then
            ST_FAILED = .true.
            return
         endif
         ntot = 0
         do k = 1, ncount
            do j = 1, numdes(k)
               text(j) = atext(ntot+j)
            enddo
            ntot = ntot + numdes(k)
            call ptdescn ( 'OUT', adescr(k), text, numdes(k) )
         enddo
      endif

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBD_GETEM -- Get existing descriptors in output table
C
C   alan penny                ral         1990 jan

      subroutine tbd_getem ( adescr, numdes, atext, ncount )

      implicit none
      include 'STARMAN_INC'

      character*8  adescr(10000)	!o: Names of descriptors
      integer      numdes(10000)	!o: Number of values in descriptors
      character*60 atext(10000)		!o: Contents of descriptors
      integer      ncount		!o: Number of descriptors
C--
      character*80 text(16384)
      character btext*60, descr*80
      integer ierr, k, numtot, nval
      logical more
Cbegin


      if ( ST_FAILED ) return

      more = .true.
      ncount = 0
      numtot = 0
      do while ( more )
         ncount = ncount + 1
         call gtdesn ( 'OUT', ncount, descr, ierr )
         if ( ierr.eq.1 ) then
            more = .false.
         else
            call gtdesc ( 'OUT', descr, text,' ', nval,ierr )
            numtot = numtot + nval
            if ( numtot.gt.10000 ) then
               more = .false.
              call printo('WARNING: Too much - only 10000 lines copied')
            else
               adescr(ncount) = descr(1:8)
               numdes(ncount) = nval
               do k = 1, nval
                 btext = text(k)
                 atext(ncount+k-1) = btext(1:60)
               enddo
            endif
         endif
      enddo
      ncount = ncount - 1


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBJOIN.FOR
C
C  Contains:-
C
C T_TBJOIN   Join together up to 9 tables


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBJOIN -- Join together up to nine tables
C  For a full help see TBJOIN.HLP
C
C  alan penny                   RAL                         1991 May

      subroutine t_tbjoin

      implicit none
      include 'STARMAN_INC'
C--
      character text*70, texth*30, itext*3, iitext*7
      logical more, ok
      integer tbx(9), tby(9), tbvx(9), tbxo, tbyo, tbvxo, ipin(9),
     +        ipout, kshx(9), kshy(9), j, ja, i, ierr, iv,
     +        tbxst, tbxen, tbyst, tbyen, kn, kxd, kyd
Cbegin


      tbyen = 0								!Get input tables
      j = 0
      more = .true.
      do while ( more )

         j = j + 1
         itext = 'IN'
         write ( itext(3:3), '(i1)' ) j

         ok = .true.
         if ( j.eq.1 ) ok = .false.
         call optabr ( itext, ipin(j), tbvx(j), tby(j), ok, i )
         if ( ST_FAILED ) return
         if ( j.eq.1 .and. i.eq.2 ) then
            call printo ( 'ERROR: First file must be present' )
            ST_FAILED = .true.
            return
         endif

         tbx(j) = tbvx(j) - 5
         if ( i.eq.2 ) then
            j = j - 1
            more = .false.
         else

            if ( i.ne.0 .or. tbx(j).lt.1 .or. tby(j).lt.1 ) then
               call printo ( 'ERROR: Input file wrong' )
               ST_FAILED = .true.
               return
            endif

            if ( j.eq.1 ) then
               tbyen = tby(1)
               kshx(1) = 0
               kshy(1) = 0
            else

               iitext = 'COLOFF'					!Get X table displacement of file from 1st
               write ( iitext(7:7), '(i1)' ) j
               call get1i ( iitext, kshx(j), 0, -1000000, 1000000 )
               if ( ST_FAILED ) return

               iitext = 'ROWOFF'					!Get Y table displacement of file from start of 1st
               write ( iitext(7:7), '(i1)' ) j
               call get1i ( iitext, kshy(j), tbyen, -1000000, 1000000 )
               if ( ST_FAILED ) return
               tbyen = max(tbyen,(tby(j)+kshy(j)))

            endif
         endif

         if ( j.eq.9 ) then
            call printo ( 'Only 9 input files allowed' )
            more = .false.
         endif

      enddo
      kn = j

      tbxst = 1								!Open Output list
      tbyst = 1
      tbxen = tbx(1)
      do j = 1, kn
         tbxst = min(tbxst,(kshx(j)+1))
         tbyst = min(tbyst,(kshy(j)+1))
         tbxen = max(tbxen,(tbx(j)+kshx(j)))
      enddo
      tbxo = tbxen - tbxst + 1
      tbvxo = tbxo + 5
      tbyo = tbyen - tbyst + 1
      call optabw ( 'OUT', ipout, tbvxo, tbyo, .false., ierr )
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         call printo ( 'ERROR: Error in opening output file' )
         ST_FAILED = .true.
         return
      endif

      call azeror ( %val(ipout), tbvxo*tbyo )				!Clear output list and add default identifiers
      call ident ( %val(ipout), tbvxo, tbyo )

      do j = 1, kn							!Put in lists
         kxd = 2 + kshx(j) - tbxst
         kyd = 2 + kshy(j) - tbyst
         call coprr ( %val(ipin(j)), tbvx(j), tby(j), 1, 5, 1, tby(j),
     +                %val(ipout), tbvxo, tbyo, 1, kyd )
         call coprr ( %val(ipin(j)), tbvx(j), tby(j), 6, tbvx(j),
     +                1, tby(j), %val(ipout), tbvxo, tbyo, kxd+5, kyd )
      enddo

      more = .true.							!Put Title
      j = 0
      do while ( more .and. j.lt.kn )
         j = j + 1
         itext = 'IN'
         write ( itext(3:3), '(i1)' ) j
         call gtdesc ( itext, 'TITLE', text, ' ', iv, ierr )
         if ( text.eq.' ' .or. ierr.ne.0 ) more = .false.
      enddo
      if ( ierr.ne.0 ) text = 'Output from Tbjoin'
      call get1c ( 'TITLE', text, text, .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUT', 'TITLE', text )

      do j = 1, tbxo							!Load Output Descriptors
         call pthead ( 'OUT', j, ' ', ierr )
      enddo
      do j = 1, kn
         itext = 'IN'
         write ( itext(3:3), '(i1)' ) j
         do ja = 1, tbx(j)
            call gthead ( itext, ja, texth, i )
            kxd = ja + 1 + kshx(j) - tbxst
            if ( i.eq.0 ) call pthead ( 'OUT', kxd, texth, i )
         enddo
      enddo


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   TBKEY.FOR
C
C   Contains:-
C
C T_TBKEY      Input a table from the keyboard
C TBKE_GCL     Get command line info
C TBKE_DOIT    Input all the lines of data
C TBKE_LINE    Input a line of data (with optional name)
C TBKE_GETOUT  Get output file
C TBKE_LOAD    Load data into Get ouput file


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBKEY -- Input a table from the keyboard
C  For a full description see TBKEY.HLP
C
C
C         A J Penny            RAL                  1991 May

      subroutine t_tbkey ()

      implicit none
      include 'tbkey.inc'
C--
Cbegin


      call tbke_gcl

      call tbke_doit ( %val(IPWKA), %val(IPWKB) )

      call tbke_getout

      call tbke_load ( %val(IPWKA), %val(IPWKB), %val(IPO) )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBKE_GCL -- Get command line info
C
C  alan penny                      ral              1991 May

      subroutine tbke_gcl ( )

      implicit none
      include 'tbkey.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      call get1i ( 'NUMCOL', TBX, 2, 1, 20 )			!No of output columns
      if ( ST_FAILED ) return
      TBVX = TBX + 5

      TBYMAX = 1000
      call get1i ( 'MAXROW', TBYMAX, TBYMAX, 1, 1000000 )
      if ( ST_FAILED ) return

      call gtwrkr ( 'WORKA',    5*TBYMAX, IPWKA, istat )
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call gtwrkr ( 'WORKB', TBX*TBYMAX, IPWKB, istat )
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBKE_DOIT -- Input all the lines of data
C
C  alan penny                      ral              1991 May

      subroutine tbke_doit ( names, table )

      implicit none
      include 'tbkey.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      byte    names(20,TBYMAX)		!o: Names
      real    table(TBX,TBYMAX)		!o: Data
C--
      real data(201)
      character*20 idbuf
      integer j, k, ierr
      logical more
Cbegin


      if ( ST_FAILED ) return

      k = 0
      more = .true.
      do while ( more )
         k = k + 1

         call tbke_line ( idbuf, data, k, ierr )
         if ( ST_FAILED ) return

         if ( ierr.ne.0 ) then
            k = k - 1
            more = .false.
         else
            do j = 1, 20
               names(j,k) = ichar(idbuf(j:j))
            enddo
            call amovr ( data, table(1,k), TBX )
         endif

         if ( k.ge.TBYMAX ) more = .false.

      enddo

      TBY = k


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBKE_LINE -- Input a line of data (with optional name)
C
C  alan penny                      ral              1991 May

      subroutine tbke_line ( idbuf, data, ndef, ierr )

      implicit none
      include 'tbkey.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      character*20   idbuf		!o: Name
      real           data(201)		!o: Data
      integer        ndef		!i: Default number in name
      integer        ierr		!o: Error flag (0=ok:1=end of data)
C--
      character*80  inbuf(101)
      character*4 text
      integer j, k, istat, num, numval
      logical more, allblank
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      num = TBX + 1							!Expected number of entries
      more = .true.
      k = 0
      do while ( more .and. k.lt.3 )					!Try 3 times for correct data
         k = k + 1
         more = .false.

         do j = 1, num							!Get data
            inbuf(j) = ' '
         enddo
         call pargi ( ndef )
         call printd ( 'Row Number: %d' )
         call getnc ( 'DATA', inbuf, num, numval, .true. )

         do j = 1, num							!Remove leading blanks
            call lbgone ( inbuf(j) )
         enddo

         allblank = .true. 						!All blank entry?
         if ( numval.gt.0 ) then					! If yes, flag and return
            do j = 1, numval
               if ( inbuf(j).ne.' ' ) allblank = .false.
            enddo
         endif
         if ( allblank ) then
            ierr = 1
            return
         endif

         do j = 1, TBX							!Load numbers
            call chartor ( inbuf(j), data(j), istat )
            if ( istat.ne.0 ) then
               write ( text, '(1x,i3)' ) j
               call printd ( 'ERROR: Number '//
     +                       text//' is not a real number' )
               data(j) = 0.0
               more = .true.
            endif
         enddo
         if ( numval.ge.num ) then					!Load name or default
            idbuf = inbuf(num)
         else
            idbuf(1:1) = '#'
            write ( idbuf(2:), '(1x,i18)' ) ndef
            call lbgone ( idbuf(2:) )
         endif

         if ( more ) then
            if ( k.lt.3 ) call printo ( 'ERROR: Row repeated' )
            if ( k.ge.3 ) call printo ( 'ERROR: Row set to zeros' )
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBKE_GETOUT -- Get ouput files
C
C  alan penny                      ral              1991 May

      subroutine tbke_getout ( )

      implicit none
      include 'tbkey.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer ierr, k
      character head*20, cv*50, ihead*20
Cbegin


      if ( ST_FAILED ) return

      call optabw ( 'OUT', IPO, TBVX, TBY, .false., ierr )
      if ( ST_FAILED ) return

      call get1c ( 'TITLE', cv, 'Output from TBKEY', .true. )
      if ( ST_FAILED ) return

      do k = 1, TBX							!Add new column headers
         ihead = ' '
         if ( k.eq.1 ) ihead = 'X'
         if ( k.eq.2 ) ihead = 'Y'
         if ( k.eq.3 ) ihead = 'MAGNITUDE'
         call pargi ( k )
         call printd ( 'Header for column %d ' )
         call get1c ( 'HEADER', head, ihead, .true. )
         if ( ST_FAILED ) return
         call pthead ( 'OUT', k, head, ierr )
      enddo

      call ptdesc ( 'OUT', 'TITLE', cv )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBKE_LOAD -- Load data into Get ouput file
C
C  alan penny                      ral              1991 May

      subroutine tbke_load ( names, table, out )

      implicit none
      include 'tbkey.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      byte    names(20,TBYMAX)		!i: Names
      real    table(TBX,TBYMAX)		!i: Data
      real    out(TBVX,TBY)		!o: Output table
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, TBY
         call amovz ( names(1,k), out(1,k), 20 )
         call amovr ( table(1,k), out(6,k), TBX )
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    TBLIST.FOR
C
C    Contains:-
C
C T_TBLIST       Make an ASCII file of a table
C TBLI_SHORT     Short output of table
C TBLI_ALL       All columns floating formatted output of table
C TBLI_ALLHEAD   Put out all columns floating format header
C TBLI_FORM      Floating formatted output of table
C TBLI_WRI       Write to output channel number 1
C TBLI_FORMHEAD  Put out floating format header
C TBLI_DOHEAD    Put header out in standard format
C TBLI_FIXFORM   Fixed formatted output of table
C TBLI_FIXHEAD   Put out fixed format header
C TBLI_LINTIDY   Tidy a line


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBLIST -- Make an ASCII file of a table
C   For a fuller description see TBLIST.HLP
C
C   alan penny                     ral           1991 May

      subroutine t_tblist ()

      implicit none
      include 'tblist.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer ip, ierr, iv, k, kmode
      character file*80, text*80
Cbegin


      call optabr ( 'IN', ip, TBVX, TBY, .false., ierr )		!Obtain input table
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      TBX = TBVX - 5

      call gtdesc ( 'IN', 'TITLE', text, ' ', iv, ierr ) 		!Type summary
      call pargc ( text )
      call printd ( 'Title is:  %c' )
      call pargi ( TBX )
      call pargi ( TBY )
      call printd ( 'No of Columns - %d  ; No of Rows - %d' )
      call printo ( ' ' )

      call get1c ( 'OUT', file, ' ', .true. )				!Get output file name
      if ( ST_FAILED ) return
      DOFILE = .true.
      if ( file.eq.' ' ) DOFILE = .false.

      call get1b ( 'TYPING', DOTYPE, .true. )				!Get if typing as well
      if ( ST_FAILED ) return

      if ( .not.DOFILE .and. .not.DOTYPE ) return

      call get_job ( 'OUTMODE', 'short:full:fixform:form:header', 	!Get mode
     +               kmode, 1, text, 0 )
      if ( ST_FAILED ) return

      k = 2
      if ( kmode.eq.2 ) k = 1
      if ( kmode.ne.5 ) call get_job ( 'HEADER', 'full:short:none', 	!Get header mode
     +                                 DOHEAD, k, text, 0 )
      if ( ST_FAILED ) return

      if ( DOFILE ) open ( unit=1, file=file, status='NEW', 		!Open file
     +                     form='FORMATTED' )

      if ( kmode.eq.1 ) then						!Do the work
         call tbli_short ( %val(ip) )
      elseif ( kmode.eq.2 ) then
         call tbli_all ( %val(ip) )
      elseif ( kmode.eq.3 ) then
         call tbli_fixform ( %val(ip) )
      elseif ( kmode.eq.4 ) then
         call tbli_form ( %val(ip) )
      else
         call tbli_dohead ( TBX, 2 )
      endif

      if ( DOFILE ) close ( 1 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLI_SHORT -- Short output of table
C
C  alan penny                 ral                1990-06-15

      subroutine tbli_short ( tb )

      implicit none
      include 'tblist.inc'
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real         tb(TBVX,TBY)		!i: Input table
C--
      real   rv(3)
      integer j, k, n, iv, ierr, klen, lens, kn
      character title*50, text*75, texta*75, name*20, textd*70,
     +          head*20, textb*75
      character*12 textc(3)
      character*1 dash(20)
      data dash / 20*'-' /
      external lens
Cbegin


      if ( ST_FAILED ) return

      n = min(3,TBX)

      if ( DOHEAD.eq.1 ) then						!Get headers
         NCOL(1) = 1
         NCOL(2) = 2
         NCOL(3) = 3
         call tbli_dohead ( n, 1 )
      elseif ( DOHEAD.eq.2 ) then

         call gtdesc ( 'IN', 'TITLE', title, ' ', iv, ierr ) 		!Get title
         text = 'Title is:  '//title

         texta = '      Name             '
         textb = '      ----             '
         kn = 24
         do k = 1, n
            call gthead ( 'IN', k, head, ierr )
            call lbgone ( head )
            write ( texta(kn:), '(a20)' ) head
            klen = lens(head)
            write ( textb(kn:), '(20a1)' ) (dash(j),j=1,klen)
            kn = kn + 16
         enddo

         if ( DOTYPE ) then
            call printo ( text )
            call printo ( ' ' )
            call printo ( texta )
            call printo ( textb )
         endif
         if ( DOFILE ) then
            call tbli_wri ( text )
            write ( 1, '(1x,'' '')' )
            call tbli_wri ( texta )
            call tbli_wri ( textb)
         endif

      endif

      do k = 1, TBY							!Type out lines
         call namegt ( tb, TBVX, TBY, k, name )				! with names and up to 3 values
         do j = 1, n
            rv(j)= tb((5+j),k)
            call valform ( rv(j), textc(j) )
         enddo
         write ( textd, '(1x,a20,3(2x,a12,2x))' ) name,(textc(j),j=1,n)
         if ( DOTYPE ) call printo ( textd )
         if ( DOFILE ) write ( 1, '(1x,a70)' ) textd
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLI_ALL -- All columns floating formatted output of table
C
C  alan penny                 ral                1990-06-15

      subroutine tbli_all ( tb )

      implicit none
      include 'tblist.inc'
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real	   tb(TBVX,TBY)		!i: Input table
C--
      integer k, j, jj, ka, kn
      real    rv
      character name*20, text*80, textc*12
Cbegin


      if ( ST_FAILED ) return

      call get1i ( 'NUMNAME', NUMID, 12, 0, 20 )			!Get no of characters in name
      if ( ST_FAILED ) return

      if ( DOHEAD.eq.1 ) then						!Get headers
         call tbli_dohead ( TBX, 2 )
      elseif ( DOHEAD.eq.2 ) then
         call tbli_allhead
      endif

      do k = 1, TBY							!Print entries

         call namegt ( tb, TBVX, TBY, k, name )				!Load name
         text  = ' '
         if ( NUMID.ne.0 ) text = name(1:NUMID)
         kn = 1 + NUMID + 2

         do j = 1, min(4,TBX)						!Load values
            rv = tb((5+j),k)
            call valform ( rv, textc )
            text(kn:) = textc
            kn = kn + 14
         enddo

         if ( DOTYPE ) call printo ( text )				!Print line
         if ( DOFILE ) call tbli_wri ( text )

         text = ' '							!Other rows of values
         jj = TBX - 4
         j = 1
         do while ( jj.gt.0 )
            kn = 1 + NUMID + 2
            do ka = 1, min(jj,4)
               rv = tb((5+ka+j*4),k)
               call valform ( rv, textc )
               text(kn:) = textc
               kn = kn + 14
            enddo
            if ( DOTYPE ) call printo ( text )
            if ( DOFILE ) call tbli_wri ( text )

            jj = jj - 4
            j = j + 1
         enddo

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLI_ALLHEAD -- Put out all columns floating format header
C
C  alan penny                 ral                1990-06-15

      subroutine tbli_allhead ( )

      implicit none
      include 'tblist.inc'
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

C--
      integer k, iv, ierr, j, jj, jk, ka, kn, klen, klena(4)
      character head*20, title*70, text*80, atext*80, btext*80
      character*1 dash(20)
      data dash / 20*'-' /
      integer lens
      external lens
Cbegin


      if ( ST_FAILED ) return

      call gtdesc ( 'IN', 'TITLE', title, ' ', iv, ierr )		!Get title
      text = 'Title is:  '//title

      atext = ' '							!Type headers
      if ( NUMID.ne.0 ) atext = 'Name'

      kn = 1 + NUMID + 2						!1st row of headers
      do ka = 1, min(TBX,4)
         call gthead ( 'IN', ka, head, ierr )
         call lbgone ( head )
         write ( atext(kn:), '(a12)' ) head(1:12)
         klena(ka) = lens(head(1:12))
         kn = kn + 14
      enddo
      if ( DOTYPE ) then
         call printo ( text )
         call printo ( ' ' )
         call printo ( atext )
      endif
      if ( DOFILE ) then
         call tbli_wri ( text )
         write ( 1, '(1x,'' '')' )
         call tbli_wri ( atext )
      endif

      atext = ' '							!Other rows of headers
      jj = TBX - 4
      j = 1
      do while ( jj.gt.0 )
         kn = 1 + NUMID + 2
         do ka = 1, min(jj,4)
            k = ka + 4*j
            call gthead ( 'IN', k, head, ierr )
            call lbgone ( head )
            write ( atext(kn:), '(a12)' ) head(1:12)
            klen = lens(head(1:12))
            klena(ka) = max(klena(ka),klen)
            kn = kn + 14
         enddo
         if ( DOTYPE ) call printo ( atext )
         if ( DOFILE ) call tbli_wri ( atext )

         jj = jj - 4
         j = j + 1
      enddo

      btext = ' '						!dashes delow headers
      if ( NUMID.ne.0 ) btext = '----'

      kn = 1 + NUMID + 2
      do k = 1, min(TBX,4)
         jk = klena(k)
         write ( btext(kn:), '(12a1)' ) (dash(j),j=1,jk)
         kn = kn + 14
      enddo

      if ( DOTYPE ) call printo ( btext )
      if ( DOFILE ) call tbli_wri ( btext )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLI_WRI -- Write to output channel number 1
C
C  alan penny                 ral                1990-06-15

      subroutine tbli_wri ( text )

      implicit none
      include 'tblist.inc'
      include 'STARMAN_INC'

      character*80    text		!i: Text to output
C--
Cbegin


      if ( ST_FAILED ) return

      if ( NUMID.le.12 ) then
         write ( 1, '(1x,a72)' ) text(1:72)
      else
         write ( 1, '(1x,a80)' ) text
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLI_FORM -- Floating formatted output of table
C
C  alan penny                 ral                1990-06-15

      subroutine tbli_form ( tb )

      implicit none
      include 'tblist.inc'
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real	   tb(TBVX,TBY)		!i: Input table
C--
      integer      nmax
      parameter    ( nmax=15 )

      integer k, kx, j, jj, ka, kn
      real    rv
      logical more
      character name*20, texta*1, textb*2, text*80, textc*12, ntext*10
Cbegin


      if ( ST_FAILED ) return

      call get1i ( 'NUMNAME', NUMID, 12, 0, 20 )			!Get no of characters in name
      if ( ST_FAILED ) return

      kx = 0								!Get which columns and format of numbers
      more = .true.
      do while ( kx.lt.nmax .and. more )
         kx = kx + 1

         if ( kx.lt.10 ) then
            write ( texta, '(i1)' ) kx
            ntext = 'NCOL'//texta
         else
            write ( textb, '(i2)' ) kx
            ntext = 'NCOL'//textb
         endif
         j = min(kx,TBX)
         call get1i ( ntext, NCOL(kx), j, 0, TBX )
         if ( ST_FAILED ) return
         if ( NCOL(kx).eq.0 ) then
            more = .false.
            kx = kx - 1
         else
            call printo ( ' ' )
         endif

      enddo
      if ( kx.lt.1 ) return

      if ( DOHEAD.eq.1 ) then						!Get headers
         call tbli_dohead ( kx, 1 )
      elseif ( DOHEAD.eq.2 ) then
         call tbli_formhead ( kx )
      endif

      do k = 1, TBY							!Print entries

         call namegt ( tb, TBVX, TBY, k, name )				!Load name
         text  = ' '
         if ( NUMID.ne.0 ) text = name(1:NUMID)
         kn = 1 + NUMID

         do j = 1, min(4,kx)						!Load values
            rv = tb((5+NCOL(j)),k)
            call valform ( rv, textc )
            text(kn:) = textc
            kn = kn + 14
         enddo

         if ( DOTYPE ) call printo ( text )				!Print line
         if ( DOFILE ) call tbli_wri ( text )

         text = ' '							!Other rows of values
         jj = kx - 4
         j = 1
         do while ( jj.gt.0 )
            kn = 1 + NUMID + 2
            do ka = 1, min(jj,4)
               rv = tb((5+NCOL(ka+j*4)),k)
               call valform ( rv, textc )
               text(kn:) = textc
               kn = kn + 14
            enddo
            if ( DOTYPE ) call printo ( text )
            if ( DOFILE ) call tbli_wri ( text )

            jj = jj - 4
            j = j + 1
         enddo

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLI_FORMHEAD -- Put out floating format header
C
C  alan penny                 ral                1990-06-15

      subroutine tbli_formhead ( kx )

      implicit none
      include 'tblist.inc'
      include 'STARMAN_INC'

      integer     kx		!i: Number of columns to output

C--
      integer k, iv, ierr, j, jj, jk, ka, kn, klen, klena(4)
      character title*70, head*20, text*80, atext*80, btext*80
      character*1 dash(20)
      data dash / 20*'-' /
      integer lens
      external lens
Cbegin


      if ( ST_FAILED ) return

      call gtdesc ( 'IN', 'TITLE', title, ' ', iv, ierr )		!Get title
      text = 'Title is:  '//title

      atext = ' '							!Type headers
      if ( NUMID.ne.0 ) atext = 'Name'

      kn = 1 + NUMID + 2						!1st row of headers
      do ka = 1, min(kx,4)
         k = NCOL(ka)
         call gthead ( 'IN', k, head, ierr )
         call lbgone ( head )
         write ( atext(kn:), '(a12)' ) head(1:12)
         klena(ka) = lens(head(1:12))
         kn = kn + 14
      enddo
      if ( DOTYPE ) then
         call printo ( text )
         call printo ( ' ' )
         call printo ( atext )
      endif
      if ( DOFILE ) then
         call tbli_wri ( text )
         write ( 1, '(1x,'' '')' )
         call tbli_wri ( atext )
      endif

      atext = ' '							!Other rows of headers
      jj = kx - 4
      j = 1
      do while ( jj.gt.0 )
         kn = 1 + NUMID + 2
         do ka = 1, min(jj,4)
            k = NCOL(ka+4*j)
            call gthead ( 'IN', k, head, ierr )
            call lbgone ( head )
            write ( atext(kn:), '(a12)' ) head(1:12)
            klen = lens(head(1:12))
            klena(ka) = max(klena(ka),klen)
            kn = kn + 14
         enddo
         if ( DOTYPE ) call printo ( atext )
         if ( DOFILE ) call tbli_wri ( atext )

         jj = jj - 4
         j = j + 1
      enddo

      btext = ' '							!Dashes delow headers
      if ( NUMID.ne.0 ) btext = '----'

      kn = 1 + NUMID + 2
      do k = 1, min(kx,4)
         jk = klena(k)
         write ( btext(kn:), '(12a1)' ) (dash(j),j=1,jk)
         kn = kn + 14
      enddo

      if ( DOTYPE ) call printo ( btext )
      if ( DOFILE ) call tbli_wri ( btext )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLI_DOHEAD -- Put header out in standard format
C
C  alan penny                 ral                1990-06-15

      subroutine tbli_dohead ( kx, kopt )

      implicit none
      include 'tblist.inc'
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      integer      kx		!i: No of columns to output
      integer      kopt		!i: Selected (1) or all (2) columns flag
C--
      integer iv, ierr, k, count, nval, nline
      logical more
      character text*80, title*50, head*20, descr*80
      character*80 texta(16384)
Cbegin


      if ( ST_FAILED ) return

      call gtdesc ( 'IN', 'TITLE', title, ' ', iv, ierr )		!Get title
      text = 'Title is: '//title
      if ( DOTYPE ) call printo ( text )
      if ( DOFILE ) call tbli_wri ( text )

      write ( text, '(''Columns = '',i7)' ) kx
      if ( DOTYPE ) call printo ( text )
      if ( DOFILE ) call tbli_wri ( text )

      write ( text, '(''Rows =    '',i7)' ) TBY
      if ( DOTYPE ) call printo ( text )
      if ( DOFILE ) call tbli_wri ( text )

      if ( kopt.eq.1 ) then
         do k = 1, kx
            call gthead ( 'IN', NCOL(k), head, ierr )
            write ( text, '(''HEAD'',i3.3,''   '',a)' ) k, head
            if ( DOTYPE ) call printo ( text )
            if ( DOFILE ) call tbli_wri ( text )
         enddo
      else
         do k = 1, TBX
            call gthead ( 'IN', k, head, ierr )
            write ( text, '(''HEAD'',i3.3,''   '',a)' ) k, head
            if ( DOTYPE ) call printo ( text )
            if ( DOFILE ) call tbli_wri ( text )
         enddo
      endif

      more = .true.							!Get number of other descriptors
      count = 0
      nval = 1
      nline = 0
      do while ( more )
         count = count + nval
         call gtdesn ( 'IN', count, descr, ierr )
         if ( ierr.ne.0 ) then
            more = .false.
         else
            call chartoi ( descr(5:7), iv, ierr )
            if ( (descr(1:5).ne.'NITEM') .and.
     +           (descr(1:6).ne.'LSTLEN') .and.
     +           (descr(1:5).ne.'TITLE') .and.
     +           (descr(1:4).ne.'HEAD' .or. ierr.ne.0) ) then
               call gtdesc ( 'IN', descr, texta, ' ', nval, ierr )
               nline = nline + nval
            endif
         endif
      enddo

      write ( text, '(''DesNum  = '',i7)' ) nline			!Say how many descriptors
      if ( DOTYPE ) call printo ( text )
      if ( DOFILE ) call tbli_wri ( text )

      more = .true.							!Put out descriptors
      count = 0
      nval = 1
      do while ( more )
         count = count + nval
         call gtdesn ( 'IN', count, descr, ierr )
         if ( ierr.ne.0 ) then
            more = .false.
         else
            call chartoi ( descr(5:7), iv, ierr )
            if ( (descr(1:5).ne.'NITEM') .and.
     +           (descr(1:6).ne.'LSTLEN') .and.
     +           (descr(1:5).ne.'TITLE') .and.
     +           (descr(1:4).ne.'HEAD' .or. ierr.ne.0) ) then
               call gtdesc ( 'IN', descr, texta, ' ', nval, ierr )
               do k = 1, nval
                  if ( k.eq.1 ) then
                     text = descr(1:8)//'  '//texta(k)(1:68)
                  else
                     text = '        '//'  '//texta(k)(1:68)
                  endif
                  if ( DOTYPE ) call printo ( text )
                  if ( DOFILE ) call tbli_wri ( text )
               enddo
            endif
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLI_FIXFORM -- Fixed formatted output of table
C
C  alan penny                 ral                1990-06-15

      subroutine tbli_fixform ( tb )

      implicit none
      include 'tblist.inc'
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real	   tb(TBVX,TBY)		!i: Input table
C--
      integer k, kx, j, ka, klen, kk, knum, ks, ke, kl
      real    rv
      logical more
      character*7 forcha(16), forchb(16)
      character btext*10, atext*10, ntext*10, name*20, forin*7,
     +          forinb*13, texta*4000, textb*200, text*80

      integer      nmax
      parameter    ( nmax=15 )
      real         val(nmax)

      real trunc
      external trunc
Cbegin


      if ( ST_FAILED ) return

      call get1i ( 'NUMNAME', NUMID, 12, 0, 20 )			!Get no of characters in name
      if ( ST_FAILED ) return

      kx = 0								!Get which columns and format of numbers
      more = .true.
      do while ( kx.lt.nmax .and. more )
         kx = kx + 1

         if ( kx.lt.10 ) then
            write ( text, '(i1)' ) kx
            ntext = 'NCOL'//text
            btext = 'NBEF'//text
            atext = 'NAFT'//text
         else
            write ( text, '(i2)' ) kx
            ntext = 'NCOL'//text
            btext = 'NBEF'//text
            atext = 'NAFT'//text
         endif
         j = min(kx,TBX)
         call get1i ( ntext, NCOL(kx), j, 0, TBX )
         if ( ST_FAILED ) return
         if ( NCOL(kx).eq.0 ) then
            more = .false.
            kx = kx - 1
         else
            call get1i ( btext, nbef(kx), 4, 0, 100 )
            if ( ST_FAILED ) return
            call get1i ( atext, naft(kx), 2, 0, 100 )
            if ( ST_FAILED ) return
            call printo ( ' ' )
         endif

      enddo
      if ( kx.lt.1 ) return

      if ( DOHEAD.eq.1 ) then						!Get headers
         call tbli_dohead ( kx, 1 )
      elseif ( DOHEAD.eq.2 ) then
         call tbli_fixhead ( kx )
      endif


      forcha(1) = '(20a1,'						!Load format

      do k = 1, kx
         ka = nbef(k) + naft(k) + 2
         write ( forin, '(''f'',i2,''.'',i2,'','')' ) ka, naft(k)
         forcha(k+1) = forin
      enddo
      forcha(kx+1)(7:7) = ')'

      forchb(1) = '(20a1,'
      do k = 1, kx
         ka = nbef(k) + naft(k) + 2
         write ( forin, '(''f'',i2,''.'',i2,'','')' ) ka, naft(k)
         forchb(k+1) = forin
      enddo
      forchb(kx+1)(7:7) = ')'

      do k = 1, TBY							!Print entries

         call namegt ( tb, TBVX, TBY, k, name )

         do j = 1, kx
            rv = tb((5+NCOL(j)),k)
            val(j) = trunc(rv,nbef(j))
         enddo

         if ( dotype ) then
            write ( texta, forcha ) (name(j:j),j=1,20), (val(j),j=1,kx)
            call tbli_lintidy ( texta, 200, NUMID, naft, 1, textb, klen)
            knum = 1 + ((klen-1)/70)
            do kk = 1, knum
               ks = 1 + (kk-1)*70
               ke = min(klen,(ks+69))
               kl = ke - ks + 1
               write ( forinb, '(''(1h ,'',i3.3,''a1)'')' ) kl
               write ( text, forinb ) (textb(j:j),j=ks,ke)
               call printo ( text )
            enddo
         endif
         if ( dofile ) then
            write ( texta, forchb ) (name(j:j),j=1,20), (val(j),j=1,kx)
            call tbli_lintidy ( texta, 200, NUMID, naft, 1, textb, klen)
            write ( forinb, '(''(1h ,'',i3.3,''a1)'')' ) klen
            write ( 1, forinb ) (textb(j:j),j=1,klen)
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLI_FIXHEAD -- Put out fixed format header
C
C  alan penny                 ral                1990-06-15

      subroutine tbli_fixhead ( kx )

      implicit none
      include 'tblist.inc'
      include 'STARMAN_INC'

      integer     kx		!i: Number of columns to output

C--
      integer nmax
      parameter ( nmax=15 )
      integer j, k, ka, kb, kc, ke, kcc(nmax), kcd(nmax), klen, iv,
     +        ierr, kk, knum, ks, kl
      character*13 forchc(18), forchd(18), forina, forinb
      character text*80, title*50, texta*4000, textb*200
      character*20 head(nmax)
Cbegin


      if ( ST_FAILED ) return

      forchc(1) = '(5hIdent,'						!Load Header Formats
      forchc(2) = '8hifier   ,'
      if ( NUMID.lt.12 ) then
         forchc(1) = '(5hName ,'
         forchc(2) = '8h        ,'
      endif
      forchc(3) = '7h       ,'

      do k = 1, kx
         kc = nbef(k) + naft(k) + 1
         if ( naft(k).eq.0 ) kc = kc -1
         call gthead ( 'IN', k, head(k), ierr )
         call lbgone ( head(k) )
         call charln ( head(k), klen )
         klen = max(1,klen)
         kc = min(klen,kc)
         kcc(k) = kc
         kb = nbef(k) - kc + 1
         kb = max(1,kb)
         ke = nbef(k) + naft(k) + 2 - kb - kc
         if ( naft(k).eq.0 ) ke = ke - 1

         if ( ke.gt.0 ) then
            write ( forina, '(i2,''x,'',i2,''a1,'',i2,''x,'')' ) kb,
     +                                                        kc, ke
         else
            write ( forina, '(i2,''x,'',i2,''a1,'')' ) kb, kc
         endif
         forchc(k+3) = forina
      enddo
      ka = kx + 3
      forchc(ka)(13:13) = ')'

      forchd(1) = '(5hIdent,'
      forchd(2) = '8hifier   ,'
      if ( NUMID.lt.12 ) then
         forchd(1) = '(5hName ,'
         forchd(2) = '8h        ,'
      endif
      forchd(3) = '7h       ,'

      do k = 1, kx
         kc = nbef(k) + naft(k) + 1
         if ( naft(k).eq.0 ) kc = kc - 1
         call gthead ( 'IN', k, head(k), ierr )
         call lbgone ( head(k) )
         call charln ( head(k), klen )
         klen = max(1,klen)
         kc = min(klen,kc)
         kcd(k) = kc
         kb = nbef(k) - kc + 1
         kb = max(1,kb)
         ke = nbef(k) + naft(k) + 2 - kb - kc
         if ( naft(k).eq.0 ) ke = ke - 1
         if ( ke.gt.0 ) then
            write ( forina, '(i2,''x,'',i2,''a1,'',i2,''x,'')' ) kb,
     +                                                         kc, ke
         else
            write ( forina, '(i2,''x,'',i2,''a1,'')' ) kb, kc
         endif
         forchd(k+3) = forina
      enddo
      ka = kx + 3
      forchd(ka)(13:13) = ')'

      call gtdesc ( 'IN', 'TITLE', title, ' ', iv, ierr )		!Get title

      if ( dotype ) then						!Write headers
         call printo ( ' ' )
         text = 'Title is:-   '//title
         call printo ( text )
         call printo ( ' ' )
         write ( texta, forchc ) ((head(k)(j:j),j=1,kcc(k)),k=1,kx)
         call tbli_lintidy ( texta, 200, NUMID, naft, 0, textb, klen )
         knum = 1 + ((klen-1)/70)
         do kk = 1, knum
            ks = 1 + (kk-1)*70
            ke = min(klen,(ks+69))
            kl = ke - ks + 1
            write ( forinb, '(''(1h ,'',i3.3,''a1)'')' ) kl
            write ( text, forinb ) (textb(j:j),j=ks,ke)
            call printo ( text )
         enddo
      endif

      if ( dofile ) then
         text = ' Title is:-   '//title
         write ( 1, '(a)' ) text
         write ( 1, '(a)' ) ' '
         write ( texta, forchd ) ((head(k)(j:j),j=1,kcd(k)),k=1,kx)
         call tbli_lintidy ( texta, 200, NUMID, naft, 0, textb, klen)
         write ( forinb, '(''(1h ,'',i3.3,''a1)'')'  ) klen
         write ( 1, forinb ) (textb(j:j),j=1,klen)
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLI_LINTIDY -- Tidy a line
C
C  alan penny                 ral                1990-06-15

      subroutine tbli_lintidy ( texta, n, numid, naft, kw, textb,klen)

      implicit none

      character*(*) texta	!i: Input string
      integer       n		!i:
      integer       numid	!i:
      integer       naft(15)	!i:
      integer       kw		!i:
      character*(*) textb	!o:
      integer       klen	!
C--
      integer j, ja, k, kk, kpos(15)
Cbegin


      do j = 1, n
         textb(j:j) = texta(j:j)
      enddo

      if ( kw.eq.1 ) then						!Strip redundant decimals

         kk = 0
         do k = 21, n
            if ( kk.lt.15 .and. textb(k:k).eq.'.' ) then
               kk = kk + 1
               kpos(kk) = k
            endif
         enddo
         if ( kk.ne.0 ) then
            do k = kk, 1, -1
               if ( naft(k).eq.0 ) then
                  do j = kpos(k), n-1
                     ja = j + 1
                     textb(j:j) = textb(ja:ja)
                  enddo
               endif
            enddo
         endif

      endif

      if ( numid.ne.20 ) then						!Strip unwanted Identifier characters
         kk = 20 - numid
         do j = numid+1, n-kk
            ja = j + kk
            textb(j:j) = textb(ja:ja)
         enddo
         do j = n-kk+1, n
            textb(j:j) = ' '
         enddo
      endif

      call charln ( textb, klen )					!Find length


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    TBLOAD.FOR
C
C    Contains:-
C
C T_TBLOAD     Input an ASCII file to a table
C TBLO_CHECK    Get size of output table
C TBLO_DOIT     Load output table


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBLOAD -- Input an ASCII file to a table
C   For a fuller description see TBLOAD.HLP
C
C   alan penny                     ral           1991 May

      subroutine t_tbload ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer ip, nlines, ierr, hlines
      character file*80
      logical dohead, doname
Cbegin


      if ( ST_FAILED ) return

      call get1c ( 'IN', file, ' ', .true. )				!Get output file name
      if ( ST_FAILED ) return
      if ( file.eq.' ' ) return

      call get1b ( 'HEADER', dohead, .false. )				!Header in input file
      if ( ST_FAILED ) return

      if ( .not.dohead ) call get1i ( 'HIGNORE', hlines, 0, 0, 10000 )	!No of lines at start of file to ignore?
      if ( ST_FAILED ) return

      call get1b ( 'NAMES', doname, .false. )				!Names in input file?
      if ( ST_FAILED ) return

      call get1i ( 'NLINES', nlines, 1, 1, 1000 )			!Number of input lines per row
      if ( ST_FAILED ) return

      open ( unit=1, file=file, status='OLD', form='FORMATTED' )	!Open file

      call tblo_check ( dohead, doname, nlines, hlines  )		!Get size of file

      call optabw ( 'OUT', ip, TBVX, TBY, .false., ierr )		!Obtain input table
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) return

      call tblo_doit ( %val(ip), dohead, doname, nlines, hlines )	!Load the output table

      close ( 1 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLO_CHECK -- Get size of output table
C
C  alan penny                 ral                1990-06-15

      subroutine tblo_check ( dohead, doname, nlines, hlines )

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      logical	     dohead		!i: Input file has file header?
      logical	     doname		!i: Input file has names?
      integer        nlines		!i: No of lines in input file per output row
      integer        hlines		!i: No of lines at start of input file to ignore
C--
      integer j, k, ky, ks(1000), ke(1000), ndes
      character text*1000, texta*50
Cbegin


      if ( ST_FAILED ) return

      if ( dohead ) then

         read ( 1, '(a1)', end=1, err=1 ) text				!Skip title
         texta = text(1:50)
         call pargc ( texta )
         call printd ( '%c' )

         read ( 1, '(a)', end=1, err=1 ) text				!Get no of columns and rows
         call numel ( text, j, ks, ke )
         if ( j.lt.3 ) goto 1
         read ( text(ks(3):ke(3)), '(i)', end=1, err=1 ) TBX
         read ( 1, '(a)', end=1, err=1 ) text
         call numel ( text, j, ks, ke )
         if ( j.lt.3 ) goto 1
         read ( text(ks(3):ke(3)), '(i)', end=1, err=1 ) TBY
         TBVX = TBX + 5

         do k = 1, TBX							!Read over headers and descriptors
            read ( 1, '(a1)', end=1, err=1 ) text
         enddo
         read ( 1, '(10x,i7)', end=1, err=1 ) ndes
         if ( ndes.ne.0 ) then
            do k = 1, ndes
               read ( 1, '(a1)', end=1, err=1 )
            enddo
         endif

      else

         if ( hlines.ne.0 ) then
            do k = 1, hlines
               read ( 1, '(a1)', end=1,err=1 ) text
            enddo
         endif

      endif

      read ( 1, '(a)', end=1, err=1 ) text				!Get no of columns
      call numel ( text, j, ks, ke )
      TBX = j
      if ( doname ) TBX = TBX - 1
      if ( nlines.gt.1 ) then
         do k = 2, nlines
            read ( 1, '(a)', end=1, err=1 ) text
            call numel ( text, j, ks, ke )
            TBX = TBX + j
         enddo
      endif
      TBVX = TBX + 5

      ky = nlines							!Get number of rows
      do while ( .true. )
         read ( 1, '(a1)', end=2, err=1 ) text
         ky = ky + 1
      enddo
    2 TBY = ky/nlines
      if ( TBY*nlines.ne.ky ) then
         call printo ( 'ERROR: Not correct number of lines' )
         ST_FAILED = .true.
         return
      endif

      rewind ( 1 )

      call pargi ( TBX )						!Put out size
      call pargi ( TBY )
      call printd ( 'Output: No of columns = %d : No of rows = %d ' )

      if ( TBX.lt.1 .or. TBY.lt.1 ) then
         call printo ( 'ERROR: Invalid table size' )
         ST_FAILED = .true.
         return
      endif

      return

    1 call printo (
     +        'ERROR: Error reading input file - getting table size' )
      ST_FAILED = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLO_DOIT -- Load output table
C
C  alan penny                 ral                1990-06-15

      subroutine tblo_doit ( tb, dohead, doname, nlines, hlines )

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real        tb(TBVX,TBY)		!o: Output table
      logical	  dohead		!i: Input file has file header?
      logical	  doname		!i: Input file has names?
      integer     nlines		!i: No of lines in input file per output row
      integer     hlines		!i: No of lines at start of input file to ignore
C--
      integer j, jj, k, kk, ierr, ks(1000), ke(1000), numc, ja, jb,
     +        kd, kee, ndes, ka, iv, iverr
      logical more
      character text*1000, title*50, name*20, head*20, thead*7, tc*100,
     +          descr*8, descrl*8, textb*68
      character*68 texta(16384)
      real   rva(5), rv(1000)
Cbegin


      if ( ST_FAILED ) return

      call ident ( tb, TBVX, TBY )					!Load default empty output file
      do k = 1, TBY
         call amovkr ( 0.0, tb(6,k), TBX )
      enddo

      call printo ( ' ' )
      if ( dohead ) then						!Put header

         read ( 1, '(10x,a)', end=1, err=1 ) text
         j = index(text,'is:')
         title = text(j+4:j+53)
         read ( 1, '(a1)', end=1,err=1 )
         read ( 1, '(a1)',end=1, err=1 )
         call printo ( '[Title; column number; row number] - read' )

         do k = 1, TBX							!Read headers
            read ( 1, '(10x,a)',end=1, err=1 ) text
            j = index(text,'HEAD')
            head = text(j+10:j+29)
            call pthead ( 'OUT', k, head, ierr )
         enddo
         call printo ( 'Headers - read' )

         read ( 1, '(a)',end=1, err=1 ) text			!Read descriptors
         call numel ( text, k, ks, ke )
         read ( text(ks(3):ke(3)), '(i)', end=1, err=1 ) ndes
         if ( ndes.ne.0 ) then
            k = 0
            descrl = ' '
            do while ( k.lt.ndes )
               more = .true.
               ka = 0
               do while ( more )
                  k = k + 1
                  ka = ka + 1
                  read ( 1, '(a8,2x,a)', end=1, err=1 ) descr, textb
                  if ( k.eq.1 ) descrl = descr
                  if ( k.ne.1 .and. descr.ne.descrl ) then
                     more = .false.
                  else
                     texta(ka) = textb
                  endif
                  if ( k.eq.ndes ) more = .false.
               enddo
               if ( k.ne.ndes .or. descrl.ne.descr ) ka = ka - 1
               call ptdescn ( 'OUT', descrl, texta, ka )
               if ( k.eq.ndes .and. descr.ne.descrl ) call ptdescn
     +                      ( 'OUT', descr, textb, 1 )
               descrl = descr
               texta(1) = textb
            enddo
         endif
         call printo ( 'Descriptors - read' )

      else
         call get1c ( 'TITLE', title, ' ', .true. )
         if ( ST_FAILED ) return
         do k = 1, TBX
            call pargi ( k )
            call printd ( 'Header for column %d' )
            if ( k.gt.20 ) then
               call get1c ( 'HEAD', head, ' ', .true. )
            else
               write ( thead, '(''HEAD'',i3.3)' ) k
               call get1c ( thead, head, ' ', .true. )
            endif
            if ( ST_FAILED ) return
            call pthead ( 'OUT', k, head, ierr )
         enddo
         if ( hlines.ne.0 ) then
            do k = 1, hlines
               read ( 1, '(a1)', end=1,err=1 )
            enddo
            call printo ( 'Starter lines - read' )
         endif
      endif

      call ptdesc ( 'OUT', 'TITLE', title )

      do kk = 1, TBY

         call amovkr ( 0.0, rv, TBX )
         jj = 6
         do k = 1, nlines						!Get info

            iverr = 1
            read ( 1, '(a)', end=2, err=2 ) text			!Read line
            call numel ( text, numc, ks, ke )

            if ( numc.eq.0 ) then
               call printo ( 'ERROR: Blank line' )
               iverr = 1
               goto 2
            endif

            if ( k.eq.1 .and. numc.eq.1 .and. doname ) then
               call printo ( 'ERROR: Line has only name' )
               iverr = 1
               goto 2
            endif

            ja = 1							!Get name
            jb = 0
            if ( k.eq.1. and. doname ) then
                if ( ke(1).lt.ks(1) ) then
                   name = ' '
                else
                   kd = ke(1) - ks(1) + 1
                   kee = ks(1) + min(20,kd) - 1
                   name = text(ks(1):kee)
                endif
                ja = 2
                jb = 1
            endif

            do j = ja, numc						!Get values
               if ( ke(j).lt.ks(j) ) then
                  rv(j-jb) = 0.0
               else
                  tc = text(ks(j):ke(j))
                  if ( ks(j).eq.ke(j) ) then
                     if ( tc.eq.'.' ) then
                        rv(j-jb) = 0.0
                     else
                        read ( tc, '(i1)', end=2, err=2 ) iv
                        rv(j-jb) = iv
                     endif
                  else
                     iv = ke(j) - ks(j) + 2
                     if ( index(tc,'.').eq.0 ) tc(iv:iv) = '.'
                     read ( tc, '(f100.1)', end=2, err=2 ) rv(j-jb)
                  endif
               endif
            enddo

            call amovr ( rv, tb(jj,kk), (numc-jb) )			!Put data
            jj = jj + numc - jb

            if ( k.eq.1 ) then						!Put name
               call nametr ( name, rva )
               call amovr ( rva, tb(1,kk), 5 )
            endif

         enddo
      enddo
      call printo ( 'Data - read' )


      return								!Put out the input format
    1 call printo ( 'ERROR: Error reading input file' )			! error messages
      ST_FAILED = .true.
      return

    2 iv = (kk-1)*nlines + k
      call pargi ( iv )
      if ( iverr.eq.1 ) call printd (
     +               'ERROR: Error in input: in data line no %d')
      if ( iverr.eq.2 ) call printd (
     +         'ERROR: Error in input: in format of data line no %d' )
      call charln ( text, iv )
      iv = min(70,iv)
      if ( iv.lt.1 ) then
         call printo ( ' ' )
      else
         call printo ( text(1:iv) )
      endif
      ST_FAILED = .true.
      return


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     TBMATCH.FOR
C
C     Contains:-
C T_TBMATCH     Extract entries with matching names in two tables
C TBMA_CHECKA   Find number of output rows in output tables and posns
C TBMA_CHECKB   Find number of output rows in output tables and posns (same order)
C TBMA_TRANSA   Move entries from input tables to output tables
C TBMA_TRANSB   Move entries from input tables to output tables (same)


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBMATCH -- Extract entries with matching names in two tables
C  Take two tables lists and makes two new ones which are copies of the old ones,
C  but only contain the entries which do (or optionally do not) have
C  identifiers which occur in the other file. The new file entries are in
C  the same order as in their old files, except if choosing the matching
C  entries, the 2nd file entries are in the order they occur in the 1st file.
C
C         A J Penny            RAL                       1991 May

      subroutine t_tbmatch ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      character title*50, text*72
      logical same
      integer ierr, ip1, ip2, kacc, ipo1, ipo2, iv, ipw1, ipw2, ipw3
Cbegin


      call optabr ( 'IN1', ip1, TBVX1, TBY1, .false., ierr )		!Get 1st input table
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call optabr ( 'IN2', ip2, TBVX2, TBY2, .false., ierr )		!Get 2nd input table
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call get_job ( 'OPTION', 'accept:reject', kacc, 1, text, 0 )	!Get whether accept or reject common stars
      if ( ST_FAILED ) return

      if ( kacc.eq.1 ) call get1b ( 'SAME', same, .true. )		!Get same output order?
      if ( ST_FAILED ) return

      call pargi ( TBY1 )						!Type numbers
      call pargi ( TBY2 )
      call printd ( 'Input:   No in 1st = %d ; No in 2nd = %d' )

      call gtwrki ( 'WORK1', TBY1, ipw1, ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call gtwrki ( 'WORK2', TBY2, ipw2, ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      if ( kacc.eq.1 .and. same )  call gtwrkr ( 'WORK3', 5*(TBY1+TBY2),
     +                                           ipw3, ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      if ( kacc.eq.2 .or. .not.same ) then
         call tbma_checka ( %val(ip1), %val(ip2), %val(ipw1), 		!Find no of entries passed
     +                     %val(ipw2), kacc )
      else
         call tbma_checkb ( %val(ip1), %val(ip2), %val(ipw1),
     +                     %val(ipw2), %val(ipw3) )
      endif

      if ( TBYO1.eq.0 .and. TBYO2.eq.0 ) then
         if ( kacc.eq.1 ) call printo ( 'No matching entries found' )
         if ( kacc.eq.2 ) call printo ('No non-matching entries found')
         return
      endif

      call pargi ( TBYO1 )
      call pargi ( TBYO2 )
      call printd ( 'Output:  No in 1st = %d ; No in 2nd = %d' )

      ipo1 = 1
      if ( TBYO1.ne.0 ) then						!Open output table 1
         call optabw ( 'OUT1', ipo1, TBVX1, TBYO1, .false., ierr )
         if ( ST_FAILED ) return
         if ( ierr.ne.0 ) then
            ST_FAILED = .true.
            return
         endif

         call tcopdes ( 'IN1', 'OUT1', ierr )				!Get title to output list
         call gtdesc ( 'IN1', 'TITLE', title, 'Output from Tbmatch',	! and store it and the descriptors
     +                 iv, ierr )
         if ( ierr.ne.0 ) title = 'Output from Tbmatch'
         call get1c ( 'TITLE1', title, title, .true. )
         if ( ST_FAILED ) return
         call ptdesc ( 'OUT1', 'TITLE', title )
      endif

      ipo2 = 1
      if ( TBYO2.ne.0 ) then						!Open output table 2
         call optabw ( 'OUT2', ipo2, TBVX2, TBYO2, .false., ierr )
         if ( ST_FAILED ) return
         if ( ierr.ne.0 ) then
            ST_FAILED = .true.
            return
         endif

         call tcopdes ( 'IN2', 'OUT2', ierr )				!Get title to output list
         call gtdesc ( 'IN2', 'TITLE', title, 'Output from Tbmatch',	! and store it and the descriptors
     +                 iv, ierr )
         if ( ierr.ne.0 ) title = 'Output from Tbmatch'
         call get1c ( 'TITLE2', title, title, .true. )
         if ( ST_FAILED ) return
         call ptdesc ( 'OUT2', 'TITLE', title )
      endif

      if ( kacc.eq.1 .and. same ) then					!Do the loading of the
         if ( TBYO1.ne.0 ) then						! the input to the output
            call tbma_transb ( %val(ip1), TBVX1, TBY1, %val(ipo1),
     +                        TBYO1, %val(ipw1) )
            call tbma_transb ( %val(ip2), TBVX2, TBY2, %val(ipo2),
     +                        TBYO2, %val(ipw2) )
         endif
      else
         if ( TBYO1.ne.0 ) call tbma_transa ( %val(ip1), TBVX1, TBY1,
     +                          %val(ipo1), TBYO1, %val(ipw1), kacc )
         if ( TBYO2.ne.0 ) call tbma_transa ( %val(ip2), TBVX2, TBY2,
     +                          %val(ipo2), TBYO2, %val(ipw2), kacc )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBMA_CHECKA -- Find number of output rows in output tables and posns
C
C    A J PENNY               RGO                      82-11-4

      subroutine tbma_checka ( tb1, tb2, nd1, nd2, kacc )

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      byte	tb1(TBVX1*4,TBY1)	!i: Table 1
      byte	tb2(TBVX2*4,TBY2)	!i: Table 2
      integer   nd1(TBY1)		!o: List of posns of 1 in 2
      integer   nd2(TBY2)		!o: List of posns of 2 in 1
      integer	kacc			!i: Flag (1=accept;2=reject)
C--
      byte btemp(20)
      integer j, k, kd, mk
Cbegin


      if ( ST_FAILED ) return

      TBYO1 = 0
      do k = 1, TBY1
         call amovz ( tb1(1,k), btemp, 20 )
         kd = 1
         j = 0
         do while ( j.lt.TBY2 .and. kd.eq.1 )
            j = j + 1
            kd = 0
            mk = 1
            do while ( mk.le.20 .and. kd.eq.0 )
               if ( btemp(mk).ne.tb2(mk,j) ) kd = 1
               mk = mk + 1
            enddo
         enddo
         nd1(k) = 0
         if ( kacc.eq.1 ) then
            if ( kd.eq.0 ) then
               TBYO1 = TBYO1 + 1
               nd1(k) = TBYO1
            endif
         elseif ( kacc.eq.2 ) then
            if ( kd.eq.1 ) then
               TBYO1 = TBYO1 + 1
            else
               nd1(k) = 1
            endif
         endif
      enddo

      TBYO2 = 0
      do k = 1, TBY2
         call amovz ( tb2(1,k), btemp, 20 )
         kd = 1
         j = 0
         do while ( j.lt.TBY1 .and. kd.eq.1 )
            j = j + 1
            kd = 0
            mk = 1
            do while ( mk.le.20 .and. kd.eq.0 )
               if ( btemp(mk).ne.tb1(mk,j) ) kd = 1
               mk = mk + 1
            enddo
         enddo
         nd2(k) = 0
         if ( kacc.eq.1 ) then
            if ( kd.eq.0 ) then
               TBYO2 = TBYO2 + 1
               nd2(k) = TBYO2
            endif
         elseif ( kacc.eq.2 ) then
            if ( kd.eq.1 ) then
               TBYO2 = TBYO2 + 1
            else
               nd2(k) = 1
            endif
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBMA_CHECKB -- Find number of output rows in output tables and posns (same order)
C
C    A J PENNY               RGO                      82-11-4

      subroutine tbma_checkb ( tb1, tb2, nd1, nd2, names )

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      byte	tb1(TBVX1*4,TBY1)	!i: Table 1
      byte	tb2(TBVX2*4,TBY2)	!i: Table 2
      integer   nd1(TBY1)		!o: List of posns of 1 in 2
      integer   nd2(TBY2)		!o: List of posns of 2 in 1
      byte      names(20,(TBY1+TBY2))	!o: Output table names
C--
      byte btemp(20)
      integer k, mk, j1, j2, kd1, kd2
Cbegin


      if ( ST_FAILED ) return

      TBYO1 = 0
      do k = 1, TBY1

         call amovz ( tb1(1,k), btemp, 20 )				!For each name in 1st table

         kd1 = 1							!Check if already in output names table
         if ( TBYO1.ne.0 ) then
            j1 = 0
            kd1 = 1
            do while ( kd1.eq.1 .and. j1.lt.TBYO1 )
               j1 = j1 + 1
               kd1 = 0
               mk = 1
               do while ( mk.le.20 .and. kd1.eq.0 )
                  if ( btemp(mk).ne.names(mk,j1) ) kd1 = 1
                  mk = mk + 1
               enddo
            enddo
         endif

         j2 = 0								!Check if in 2nd table
         kd2 = 1
         do while ( kd2.eq.1 .and. j2.lt.TBY2 )
            j2 = j2 + 1
            kd2 = 0
            mk = 1
            do while ( mk.le.20 .and. kd2.eq.0 )
               if ( btemp(mk).ne.tb2(mk,j2) ) kd2 = 1
               mk = mk + 1
            enddo
         enddo

         if ( kd1.eq.1 .and. kd2.eq.0 ) then				!If not already in names
            TBYO1 = TBYO1 + 1						! and is in 2nd table, add to output list
            nd1(TBYO1) = k						! and remember location in the two tables
            nd2(TBYO1) = j2
            call amovz ( tb1(1,k), names(1,TBYO1), 20 )
         endif

      enddo

      TBYO2 = TBYO1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBMA_TRANSA -- Move entries from input tables to output tables
C
C  alan penny            ral        1990-06-06

      subroutine tbma_transa ( tbi, tbvx, tby, tbo, tbyo, nd, kacc )

      implicit none
      include 'STARMAN_INC'

      integer   tbvx			!i: No of input table columns + 5
      integer   tby			!i: No of input table rows
      real	tbi(tbvx,tby)		!i: Input table
      integer   tbyo			!i: No of input table rows
      real	tbo(tbvx,tbyo)		!o: Output table
      integer   nd(tby)			!i: List of posns of input in output
      integer	kacc			!i: Flag (1=accept; 2=reject)
C--
      integer k, kout
Cbegin


      if ( ST_FAILED ) return

      if ( kacc.eq.1 ) then

         do k = 1, tby
            kout = nd(k)
            if ( kout.ne.0 ) call amovr ( tbi(1,k), tbo(1,kout), tbvx )
         enddo

      elseif ( kacc.eq.2 ) then

         kout = 0
         do k = 1, tby
            if ( nd(k).eq.0 ) then
               kout = kout + 1
               call amovr ( tbi(1,k), tbo(1,kout), tbvx )
            endif
         enddo

      endif


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBMA_TRANSB -- Move entries from input tables to output tables (same)
C
C  alan penny            ral        1990-06-06

      subroutine tbma_transb ( tbi, tbvx, tby, tbo, tbyo, nd )

      implicit none
      include 'STARMAN_INC'

      integer   tbvx			!i: No of input table columns + 5
      integer   tby			!i: No of input table rows
      real	tbi(tbvx,tby)		!i: Input table
      integer   tbyo			!i: No of input table rows
      real	tbo(tbvx,tbyo)		!o: Output table
      integer   nd(tby)			!i: List of posns of input in output
C--
      integer k, ka
Cbegin


      if ( ST_FAILED ) return

      do k = 1, tbyo
         ka = nd(k)
         call amovr ( tbi(1,ka), tbo(1,k), tbvx )
      enddo


       end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBNATIVE.F -- Converts Starman table between different machines.
C   Table mapped as real, then as basic (bytes). This preserves the name
C   info in which is held in the first five reals of each row, and which
C   is thus corrupted if mapped as real on different machines
C
C   pat morris    leeds     May 1992


      subroutine t_tbnative ( )

      implicit none
      include 'STARMAN_INC'
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'NDF_PAR'
C--
      integer indf, outdf, num, iptr, optr, status,
     +        ndim(2), num2, nx, ny
      character*(DAT__SZLOC) loc, dloc, nloc
      character*(NDF__SZFRM) form
      character*60 frtype
Cbegin


      status = SAI__OK

      call ndf_assoc ( 'IN', 'READ', indf, status )        		!Input NDF
      call ndf_loc ( indf, 'READ', loc, status )           		!Get hds locator

      call ndf_form ( indf, 'DATA', form, status )
      if ( form.eq.'SIMPLE' ) then
         call dat_find ( loc, 'DATA_ARRAY', nloc, status )		!find data array
         call dat_find ( nloc, 'DATA', dloc, status )
      else
         call dat_find ( loc, 'DATA_ARRAY', dloc, status )
      endif

      call dat_basic ( dloc, 'READ', iptr, num2, status )		!Map as basic

      call ndf_dim ( indf, 2, ndim, num, status )
      nx = ndim(1)
      ny = ndim(2)

      call ndf_cget ( indf, 'LABEL', frtype, status )			!Check is a table
      if ( frtype.ne.'XYLIST' .or. nx.lt.6 .or. ny.lt.1 ) then
         call printo ('ERROR: Not a proper table - size wrong '//
     +                    ' - Table could not be accessed' )
         status = SAI__ERROR
         ST_FAILED = .true.
         return
      endif

      call ndf_prop ( indf, 'DATA', 'OUT', outdf, status )		!Propagate NDF

      call ndf_map ( outdf, 'DATA', '_REAL', 'UPDATE', optr,
     +               num, status )

      if ( status.ne.SAI__OK ) then                       		!Exit if not okay
         ST_FAILED = .true.
         return
      endif

      call pargi ( ny )                                 		!Informational output
      call pargi ( nx-5 )
      call printd ( 'Table size:  %d rows  ; %d columns' )

      call pargi ( nx*4 )	                             		!Check on conversion
      call pargi ( ny )
      call pargi ( num2 )
      call printd ( 'Contents Check:  %d x %d bytes should'//
     +              ' be = %d no. of byte elements' )

      call copzz ( %val(iptr), nx*4, ny, 1, 20, 1, ny,			!Copy first 5 reals for each column across
     +             %val(optr), nx*4, ny, 1, 1 )				! as bytes

      call dat_annul ( dloc, status )
      call dat_annul ( loc, status )
      if ( form.eq.'SIMPLE' ) call dat_annul ( nloc, status )

      call ndf_annul ( outdf, status )
      call ndf_annul ( indf, status )


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     TBNMATCH.FOR
C
C     Contains:-
C T_TBNMATCH    Extract entries with matching names in a number of tables


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBNMATCH -- Extract entries with matching names in a number of tables
C
C         A J Penny            RAL                       1991 May

      subroutine t_tbnmatch ()

      implicit none
C--
Cbegin


      call tbnm_gtin							!Get the input MEASURE files

      call tbnm_names							!Get locations of stars in files

      call tbnm_gtout							!Get output files

      call tbnm_move							!Load output


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBNM_GTIN -- Get input files
C
C  a j penny              ral                  1991 March

      subroutine tbnm_gtin ( )

      implicit none
      include 'tbnmatch.inc'
      include 'STARMAN_INC'
C--
      integer k, ierr, istat
      logical loop, ok
      character*4 itext
Cbegin


      if ( ST_FAILED ) return

      call get1i ( 'MAXIMUM', KMAXR, 10000, 1, 1000000 )		!Maximum no of output rows
      if ( ST_FAILED ) return

      call gtwrki ( 'WORKN', KNUMF*KMAXR, IPNUMS, istat )		!Open array with file row posns
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call azeroi ( %val(IPNUMS), KNUMF*KMAXR )				!Set default to not found for all

      call gtwrkr ( 'WORKR', 5*KMAXR, IPNAMES, istat )			!Open array with file row names
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call get1i ( 'FLAGCOL', NCOLO, 0, 0, 1000000 )			!Get flag columns and values
      if ( ST_FAILED ) return
      if ( NCOLO.ne.0 ) then
         call get1r ( 'FLAGVAL', VALCOL, 50.0, -1.0e20, 1.0e20 )
         if ( ST_FAILED ) return
      endif

      call printo ( ' ' )
      k = 0
      loop = .true.
      do while ( loop )							!Loop getting files
         k = k + 1
         ok = .true.

         write ( itext, '(''IN'',i2)' ) k
         call lbgone ( itext(3:) )
         call optabr ( itext, IPI(k), TBVXI(k), TBYI(k), .true., ierr )	!Get next file
         if ( ST_FAILED ) return
         if ( ierr.eq.2 ) then						!ok?
            k = k - 1
            ok = .false.
            if ( k.ne.0 ) then						!Must have 1 file
               loop = .false.
            else
               call printo ( 'ERROR: Must have at least one file' )
            endif
         elseif ( ierr.eq.1 ) then
            ST_FAILED = .true.
            return
         endif

         if ( NCOLO.ne.0 ) then
            if ( TBVXI(k).lt.(NCOLO+5) ) then				!Too narrow
               call printo (
     +             'ERROR: Table too narrow for use of flag column' )
               ST_FAILED = .true.
               return
            endif
         endif

         if ( ok .and. k.eq.KNUMF ) then				!Not too many
            call printo ( 'No more input files allowed' )
            loop = .false.
         endif

      enddo

      KTOT = k								!Total no of input tables


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBNM_NAMES -- Get locations of stars in files
C
C   a j penny                      ral              1991 March

      subroutine tbnm_names ( )

      implicit none
      include 'tbnmatch.inc'
      include 'STARMAN_INC'
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      call printo ( 'Making sorting tables for files to match names' )

      TBYO = 0
      do k = 1, KTOT							!Load names from file 1 and
         call tbnm_loadn1 ( %val(IPI(k)), TBVXI(k), TBYI(k), k, 	! add any extra names from other files
     +                      %val(IPNAMES) )
         if ( k.eq.1 ) then
            call pargi ( TBYO )
            call printd ( 'Loaded output names from file 1'//
     +                    ' - total rows = %d' )
         else
            call pargi ( k )
            call pargi ( TBYO )
            call printd (
     +      'Added extra output names from file %d - total rows = %d' )
         endif
      enddo

      call pargi ( TBYO )
      call printd ( 'Output files have %d rows' )

      do k = 1, KTOT							!Load actual positions for those there
         call tbnm_loadn2 ( %val(IPI(k)), TBVXI(k), TBYI(k),
     +                      k, %val(IPNAMES), %val(IPNUMS)  )
         call pargi ( k )
         call printd ( 'Loaded sorting table for file %d' )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBNM_LOADN1 -- Get number of output rows and load output names
C
C   a j penny                      ral              1991 March

      subroutine tbnm_loadn1 ( tb, tbvx, tby, kf, tbnames )

      implicit none
      include 'tbnmatch.inc'
      include 'STARMAN_INC'

      integer   tbvx			!i: Input file no of cols
      integer   tby			!i: Input file no of rows
      real	tb(tbvx,tby)		!i: Input file
      integer   kf			!i: File number
      real      tbnames(5,KMAXR)	!o: Output names
C--
      integer k, kk, kd
      logical found, more
Cbegin


      if ( ST_FAILED ) return

      k = 1
      more = .true.
      do while ( more )

         kk = 1
         found = .false.
         do while ( kk.le.TBYO .and. .not.found .and. TBYO.gt.0 )
            call namechr ( tbnames(1,kk), tb(1,k), kd )
            if ( kd.eq.0 ) found = .true.
            kk = kk + 1
         enddo

         if ( .not.found ) then
            if ( TBYO.eq.KMAXR ) then
               call pargi ( TBYO )
               call printd (
     +              'ERROR: Too many star names: there are over %d' )
               ST_FAILED  = .true.
            else
               TBYO = TBYO + 1
               call amovr ( tb(1,k), tbnames(1,TBYO), 5 )
            endif
         endif

         if ( ST_FAILED .or. k.eq.tby ) more = .false.
         k = k + 1

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBNM_LOADN2 -- Load positions of names in file
C
C   a j penny                      ral              1991 March

      subroutine tbnm_loadn2 ( tb, tbvx, tby, kf, tbnames, nums )

      implicit none
      include 'tbnmatch.inc'

      integer    tbvx			!i: Input file no of cols
      integer    tby			!i: Input file no of rows
      real	 tb(tbvx,tby)		!i: Input file
      integer    kf			!i: File number
      real       tbnames(5,KMAXR)	!i: Output names
      integer    nums(KNUMF,KMAXR)	!o: Positions array
C--
      integer k, kk, kd
      logical found
Cbegin


      do kk = 1, TBYO

         k = 1
         found = .false.
         do while ( k.le.tby .and. .not.found )
            call namechr ( tbnames(1,kk), tb(1,k), kd )
            if ( kd.eq.0 ) then
               nums(kf,kk) = k
               found = .true.
            endif
            k = k + 1
         enddo

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBNM_GTOUT -- Get output files
C
C   a j penny                      ral              1991 March

      subroutine tbnm_gtout ( )

      implicit none
      include 'tbnmatch.inc'
      include 'STARMAN_INC'
C--
      integer k, ierr, iv
      logical loop, ok
      character otext*5, itext*4, title*50
Cbegin


      if ( ST_FAILED ) return

      call printo ( ' ' )
      k = 0
      loop = .true.
      do while ( loop ) 						!Loop getting output files

         k = k + 1
         ok = .true.

         write ( otext, '(''OUT'',i2)' ) k
         call lbgone ( otext(4:) )
         call optabw ( otext, IPO(k), TBVXI(k), TBYO, .false., ierr )	!Get next file
         if ( ST_FAILED ) return
         if ( ierr.eq.1 ) then						!ok?
            k = k - 1
            ok = .false.
            call canpar ( otext )
         endif

         if ( ok ) then
            write ( itext, '(''IN'',i2)' ) k
            call lbgone ( itext(3:) )
            call gtdesc ( itext, 'TITLE', title, 			!Lad title and descriptors
     +                    'Output from Tbnmatch', iv, ierr )
            call get1c ( 'TITLE', title, title, .true. )
            if ( ST_FAILED ) return
            call tcopdes ( itext, otext, ierr )
            call ptdesc ( otext, 'TITLE', title )
         endif

         if ( k.eq.KTOT ) loop = .false.

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBNM_MOVE -- Move entries from input tables to output tables
C
C  alan penny            ral        1990-06-06

      subroutine tbnm_move ( )

      implicit none
      include 'tbnmatch.inc'
C--
      integer k
Cbegin


      do k = 1, KTOT
         call tbnm_doit ( %val(IPI(k)), TBVXI(k), TBYI(k), %val(IPO(k)),
     +                    k, %val(IPNUMS) )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBNM_DOIT -- Move entries from input tables to output tables
C
C  alan penny            ral        1990-06-06

      subroutine tbnm_doit ( tbi, tbvx, tby, tbo, kf, nums )

      implicit none
      include 'tbnmatch.inc'
      include 'STARMAN_INC'

      integer   tbvx			!i: No of input table columns + 5
      integer   tby			!i: No of input table rows
      real	tbi(tbvx,tby)		!i: Input table
      real	tbo(tbvx,TBYO)		!o: Output table
      integer   kf			!i: File number
      integer   nums(KNUMF,KMAXR)	!i: List of posns of input in output
C--
      integer k, nk
Cbegin


      if ( ST_FAILED ) return

      call coprr ( %val(IPNAMES), 5, KMAXR, 1, 5, 1, TBYO, tbo, tbvx, 	!Copy names
     +             TBYO, 1, 1 )

      do k = 1, TBYO							!Load values
         if ( nums(kf,k).eq.0 ) then
            call azeror ( tbo(6,k), (tbvx-5) )
            if ( NCOLO.ne.0 ) tbo((5+NCOLO),k) = VALCOL
         else
            nk = nums(kf,k)
            call amovr ( tbi(6,nk), tbo(6,k), (tbvx-5) )
         endif
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C This is T_TBPLOT.F
C
C It contains:-
C
C T_TBPLOT       Plot out a graph/histogram
C TBPL_SETUP     Load the default parameters
C TBPL_OPTION_SETUP Set up option choices
C TBPL_PRANGE    Get plot range
C TBPL_HRANGE    Get histogram range
C TBPL_PSIZE     Get plot size
C TBPL_GETTB     Get Table
C TBPL_GETIM     Get Image
C TBPL_CLEAR     Clear display
C TBPL_OPEN      Open device
C TBPL_CLOSE     Close device
C TBPL_HIST      Set up and start histogram
C TBPL_PLOT      Set up and start plotting
C TBPL_DOPLOT    Plot - points or line
C TBPL_GCURSE    Get positions with cursor


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBPLOT -- Plot out graph
C
C         A J Penny            RAL            1991 May

      subroutine t_tbplot ( )

      implicit none
      include 'tbplot.inc'
      include 'STARMAN_INC'
      include 'ST_DS_PANEL_INC'
C--
      integer k, ka, ktype, istat
      logical loop, kab, kas
      character*12 ktopt
Cbegin


      call tbpl_setup							!Set up parameters

      call get_job ( 'USE', 'auto:panel:key', ktype, 2, ' ', 0 )
      KMODE = 1
      if ( ktype.eq.1 ) then
         call get_job ( 'ATYPE', 'points:line:hist:disp:cont', ka, 1,
     +                  ' ', 0 )
         if ( ka.eq.1 ) KMODE = 2
         if ( ka.eq.2 ) KMODE = 3
         if ( ka.eq.3 ) then
            KMODE = 4
            ISHIST = .true.
         endif
         if ( ka.eq.4 ) KMODE = 5
         if ( ka.eq.5 ) KMODE = 6
      endif

      if ( KMODE.ne.1 ) then
         if ( KMODE.eq.2 .or. KMODE.eq.3 ) then
            call tbpl_gettb
            call tbpl_prange ( %val(IPTAB1), %val(IPTAB2), 1 )
            call tbpl_psize  ( %val(IPTAB1), %val(IPTAB2), 1 )
            if ( KMODE.eq.2 ) call tbpl_plot ( 1 )
            if ( KMODE.eq.3 ) call tbpl_plot ( 2 )
         elseif ( KMODE.eq.4 ) then
            call tbpl_gettb
            call tbpl_hist
         elseif ( KMODE.eq.5 ) then
            call tbpl_getim
            call tbpl_image
         elseif ( KMODE.eq.6 ) then
            call tbpl_getim
            call tbpl_contour
         endif
         return
      endif


      call type_hchoice							!Type option help
      call tbpl_option_setup ( ktopt, 1, .true. )			!Do the work
      if ( ktype.eq.2 ) call choice_panel_sw

      loop = .true.
      do while ( loop )

         call tbpl_option_setup ( ktopt, 1, .false.)			!Get choice
         call get_choice ( ktopt, 1 )
         if ( ST_FAILED ) return

         if ( ktopt.eq.'get_table' ) call tbpl_gettb			!Open a set of files

         if ( ktopt.eq.'get_image' ) call tbpl_getim			!Open an image

         if ( ktopt.eq.'points' ) call tbpl_plot ( 1 )			!Display points

         if ( ktopt.eq.'line' ) call tbpl_plot ( 2 )			!Display line

         if ( ktopt.eq.'histogram' ) call tbpl_hist			!Display histogram

         if ( ktopt.eq.'contour' ) call tbpl_contour			!Display contours

         if ( ktopt.eq.'display' ) call tbpl_image			!Display image

         if ( ktopt.eq.'text' ) call tbpl_text				!Display text

         if ( ktopt.eq.'label' ) call tbpl_label			!Display graph labels

         if ( ktopt.eq.'clear' ) call tbpl_clear ( 1 )			!Clear data in graph

         if ( ktopt.eq.'all_clear' ) call tbpl_clear ( 2 )		!Clear display

         if ( ktopt.eq.'open' ) call tbpl_open				!Open device

         if ( ktopt.eq.'prange' ) call tbpl_prange ( %val(IPTAB1), 	!Allowed plot range
     +                                               %val(IPTAB2), 1 )

         if ( ktopt.eq.'panel' ) call choice_panel_sw			!Change panel/keyboard input choice

         if ( ktopt.eq.'close' ) call tbpl_close			!Close plot

         if ( ktopt.eq.'psize' ) call tbpl_psize ( %val(IPTAB1), 	!Plot size
     +                                             %val(IPTAB2), 1 )

         if ( ktopt.eq.'cursor' ) call tbpl_gcurse ( %val(IPTAB1),	!Get positions
     +                                               %val(IPTAB2) )

         if ( ktopt.eq.'aspect' ) then					!Change graph aspect ratio
                                     kab = .true.
                                     if ( KASPECT.eq.0 ) kab = .false.
                                     call get1b ( 'ASPECT', kas, kab )
                                     KASPECT = 1
                                     if ( .not.kas ) KASPECT = 0
                                     GOTASPECT = .true.
                                  endif

         if ( ktopt.eq.'exit' ) loop = .false. 				!Exit from program

      enddo

      if ( DOPANEL ) call ds_p_close ( istat )


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_OPTION_SETUP -- Set up option choices
C
C   alan penny                        ral              1990-01-31

      subroutine tbpl_option_setup ( ktopt, set_num, koutside )

      implicit none
      include 'STARMAN_INC'
      include 'tbplot.inc'

      character*12   ktopt              !i: Chosen option
      integer set_num                   !i: Code for set of options
      logical  koutside                 !i: Is this called from outside loop?
C--
      integer j, k

      integer opt_num
      parameter ( opt_num=19 )

      character*12 opt_text(opt_num)
      character*68 opt_head(opt_num)
      character*68 opt_help(6,opt_num)

      data opt_text(1),opt_head(1),(opt_help(j,1),j=1,6) /
     + 'all_clear', 'Clear plot and axes',
     + 'Clear the plotted data and axes.',
     + 'The size and aspect of the graph are forgtten.',
     + 'The input table(s) and/or image are not forgotten.',
     + ' ', ' ', ' '/

      data opt_text(2),opt_head(2),(opt_help(j,2),j=1,6) /
     + 'open', 'Open the plot device ',
     + 'Open a device for the plot. This uses the PGPLOT package. You',
     + 'put in the GKS name of the device. A standard device is ',
     + '-xwindows- for opening a window on an X-terminal. To find the',
     + 'GKS name of a device, type -ask-.',
     + 'For a device which makes a file, you must press the close',
     + 'button to close the device, before plotting the file.' /

      data opt_text(3),opt_head(3),(opt_help(j,3),j=1,6) /
     + 'close', 'Close the plot device',
     + 'One may need to close the plot output display. It is a',
     + 'PGPLOT -device-, and one might want to swop between screen ',
     + '(xwindows) and printer, or one might want to get the ',
     + 'output from printer (this file only becomes available when',
     + 'the device is -closed-). ',
     + '[All plot options open a device, if it is closed.]'/

      data opt_text(4), opt_head(4), (opt_help(j,4),j=1,6) /
     + 'panel', 'Switch between panel and keyboard option selection',
     + 'This returns you to using the keyboard for option choice.',
     + '(In keyboard option entry mode, you can get back to -panel-',
     + '  option entry mode, by choosing the -panel- option.',
     + ' ', ' ', ' '/

      data opt_text(5), opt_head(5), (opt_help(j,5),j=1,6) /
     + 'exit', 'Exit from this program',
     + 'Exit from this program. Any windows open are closed, and any',
     + 'files open are closed.',
     + ' ', ' ', ' ', ' '/

      data opt_text(6), opt_head(6), (opt_help(j,6),j=1,6) /
     + 'line', 'Plot a line joining the points on the graph ',
     + 'A line joining the points already loaded is plotted on',
     + 'the graph. (A plot device is opened if not open.)',
     + 'The line may be displayed with an XY offset.',
     + 'You set the type of the line (solid;dashed;',
     + 'dot-dashed;fine dots;dot-dot-dot-dash) with this option',
     + 'with the LSTYLE option. '/

      data opt_text(7), opt_head(7), (opt_help(j,7),j=1,6) /
     + 'cursor', 'Use cursor to get values at point in graph ',
     + 'Place the cursor at any point in the graph and press',
     + 'any button. The X,Y value will be typed out. If a table',
     + 'has been input, the position of the nearest point is given.',
     + 'Place the cursor outside the area of the plot (but still',
     + 'in the window) and press for return. [Only give a quick',
     + 'dab on the return button, or you may upset the -panel-.]'/

      data opt_text(8), opt_head(8), (opt_help(j,8),j=1,6) /
     + 'histogram', 'Calculate and plot a histogram of the X data',
     + 'Take the X values of the loaded data points and calculate',
     + 'their histogram, and plot it out. ',
     + ' ',
     + 'You are asked for the X range in X values to use (HRANGE ',
     + 'parameter), and the number of bins (NBIN) in this range',
     + 'to put values into. Plot scaling is done automatically. '/

      data opt_text(9), opt_head(9), (opt_help(j,9),j=1,6) /
     + 'points', 'Plot the points on the graph ',
     + 'The points already loaded are plotted on the graph.',
     + '(A plot device is opened if not open.)',
     + 'The points may be displayed with an XY offset.',
     + 'You set the type of the symbol used via the LSTYLE',
     + 'parameter. This is the PGPLOT symbol code, from 0 to 31',
     + ' ' /

      data opt_text(10), opt_head(10), (opt_help(j,10),j=1,6) /
     + 'aspect', 'Set type of scaling of axes of graph',
     + 'This option allows you to choose between two styles for the',
     + 'shape of the plot. The normal style is to have the graph',
     + 'nearly fill the window and then scale the axes separately',
     + 'to fill this shape. The second is to have both axes scaled',
     + 'the same amount (say as for a map), and then the shape of the',
     + 'plot depends on the relative X and Y plotted data ranges' /

      data opt_text(11), opt_head(11), (opt_help(j,11),j=1,6) /
     + 'prange', 'Set the range of data in X and Y to be plotted',
     + ' ',
     + 'This sets the range in X and Y that data are allowed to be',
     + 'plotted in. Data points falling outside the permitted ',
     + 'XY rectangle will not be plotted.',
     + ' ',
     + 'The defaults are -1.0*10^20 to +1.0.10^20.  '/

      data opt_text(12), opt_head(12), (opt_help(j,12),j=1,6) /
     + 'psize', 'Set the range of the graph axes in X and Y',
     + ' ',
     + 'This sets the range of the scales on the X and Y axes of',
     + 'the plot. ',
     + ' ',
     + 'The defaults are from a bit below the minimum of all the',
     + 'data to a bit above the maximum. '/

      data opt_text(13), opt_head(13),(opt_help(j,13),j=1,6) /
     + 'get_table', 'Get the file(s)s with the tables(s) to plot',
     + 'The data are taken from Starman tables in files. X values',
     + 'of points are taken from one column in one table, and Y ',
     + 'values from a column in another (or the same) table. Input',
     + 'parameters are IN1 NCOL1 IN2 NCOL2. If two tables are',
     + 'used, then they must have the same number of rows. When ',
     + 'data are input again, the previous data are lost.'/

      data opt_text(14), opt_head(14),(opt_help(j,14),j=1,6) /
     + 'get_image', 'Get the file with image to display or contour',
     + 'The data are taken from a Starman image in a file.',
     + 'When a new image is input, the previous image is lost.',
     + ' ', ' ', ' ', ' ' /

      data opt_text(15), opt_head(15), (opt_help(j,15),j=1,6) /
     + 'contour', 'Plot the counter map of the image',
     + 'The image already loaded is plotted as a contour map.',
     + '(A plot device is opened if not open.)',
     + ' ',
     + 'A section of the image is chosen, and a contour map made,',
     + 'and displayed at an offset position in the graph. The default',
     + 'range is the min and max, with 10 steps between.' /

      data opt_text(16), opt_head(16), (opt_help(j,16),j=1,6) /
     + 'display', 'Display the image as a picture',
     + 'The image already loaded is displayed as a picture.',
     + '(A plot device is opened if not open.)',
     + ' ',
     + 'A section of the image is chosen, and is displayed at an',
     + 'offset position in the graph, as a grey-scale image, with',
     + 'default range +3/-2std dev of mean with black=highvalues ' /

      data opt_text(17), opt_head(17), (opt_help(j,17),j=1,6) /
     + 'text', 'Plot input text at a position on the graph',
     + 'A line of text is asked for, and the position on the graph',
     + 'asked for. The text is then plotted out.',
     + '(A plot device is opened if not open.)',
     + ' ', ' ', ' ' /

      data opt_text(18), opt_head(18), (opt_help(j,18),j=1,6) /
     + 'label', 'Plot axis labels on graph',
     + 'Text for the two axes is asked for, and plotted on the graph',
     + '(A plot device is opened if not open.)',
     + ' ', ' ', ' ', ' ' /

      data opt_text(19),opt_head(19),(opt_help(j,19),j=1,6) /
     + 'clear', 'Clear plot and leave axes',
     + 'Clear the plotted data in the graph only.',
     + 'The size and aspect of the graph are not forgtten.',
     + 'The input table(s) and/or image are not forgotten.',
     + ' ', ' ', ' '/

      character*50 title, option
      integer ncode
      data title, option, ncode / 'tbplot', 'OPTION', 1 /

      integer def_x, def_y
      parameter ( def_x=6 )
      parameter ( def_y=1 )
      character*12 def_text(def_x,def_y)
      data def_text / ' ', 'psize', 'open',
     +                 'points', 'panel', 'cursor' /

      integer sect_num
      parameter ( sect_num=8 )
      character*10 sect_head(sect_num)
      data sect_head / 'TABLES', 'IMAGES', 'OTHER', 'INPUT', 'ACTIONS',
     +                 'SETUP', 'GRAPH', 'CONTROL' /
      character*200 sect_text(sect_num)
      data sect_text(1)/ 'points:line:histogram' /
      data sect_text(2)/ 'contour:display' /
      data sect_text(3)/ 'label:text' /
      data sect_text(4)/ 'get_table:get_image' /
      data sect_text(5)/ 'cursor' /
      data sect_text(6)/ 'aspect:prange:psize' /
      data sect_text(7)/ 'all_clear:clear:close:open' /
      data sect_text(8)/ 'panel:exit' /

      integer help_num
      parameter ( help_num=4 )
      character*68 help_text(help_num)
      data (help_text(k),k=1,help_num) /
     + '             ' ,
     + 'To input cursor positions, position cursor at desired point' ,
     + 'and press any key, or maybe any cursor button. End by ',
     + 'locating cursor outside graph' /
Cbegin


      if ( ST_FAILED ) return

      call setup_option ( ktopt, set_num, koutside,
     +                    sect_num, sect_text, sect_head,
     +                    title, option, ncode,
     +                    1, opt_num, opt_text,
     +                    1, opt_head,
     +                    1, opt_help,
     +                    1, help_num, help_text,
     +                    1, def_x, def_y, def_text )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_SETUP -- Load the default parameters
C
C     a j penny                 ral               1991 May

      subroutine tbpl_setup ( )

      implicit none
      include 'tbplot.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call ds_sdef ( 2, 17 )

      GOTIM = .false.
      GOTTABLE = .false.
      GOTFILE2 = .false.
      FILE2OK = .false.
      ISHIST = .false.

      IPIM = 1
      IPTAB1 = 1
      IPTAB2 = 1
      NBIN = 40

      GOTDLIMS = .false.

      PRX(1) = -1.0e20
      PRX(2) = 1.0e20
      PRY(1) = -1.0e20
      PRY(2) = 1.0e20
      GOTPLIMS  = .true.

      PSX(1) = -1.0e20
      PSX(2) = 1.0e20
      PSY(1) = -1.0e20
      PSY(2) = 1.0e20
      GOTPSIZE = .false.

      PHX(1) = -1.0e20
      PHX(2) = 1.0e20
      GOTHLIMS  = .false.

      KASPECT = 1
      GOTASPECT = .false.
      XHEAD = ' '
      YHEAD = ' '

      DONEAXIS = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_PRANGE -- Get plot range
C
C     a j penny                 ral               1991 May

      subroutine tbpl_prange ( table1, table2, kopt )

      implicit none
      include 'tbplot.inc'
      include 'STARMAN_INC'

      real        table1(TBVX1,TBY1)	!i: Input data
      real        table2(TBVX2,TBY2)	!i: Input data
      integer     kopt			!i: Definitely get new range (1)
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTTABLE ) then
         call printo ( 'Need to have got table' )
         return
      endif

      if ( .not.GOTDLIMS ) then
         GOTDLIMS = .true.

         XMIN = table1((NCOL1+5),1)
         XMAX = table1((NCOL1+5),1)
         do k = 1, TBY1
            XMIN = min(XMIN,table1((NCOL1+5),k))
            XMAX = max(XMAX,table1((NCOL1+5),k))
         enddo

         YMIN = table2((NCOL2+5),1)
         YMAX = table2((NCOL2+5),1)
         do k = 1, TBY2
            YMIN = min(YMIN,table2((NCOL2+5),k))
            YMAX = max(YMAX,table2((NCOL2+5),k))
         enddo

         PRX(1) = XMIN
         PRX(2) = XMAX
         PRY(1) = YMIN
         PRY(2) = YMAX

      endif

      if ( kopt.eq.1 ) GOTPSIZE = .false.

      if ( .not.GOTPLIMS ) then
         PRX(1) = XMIN
         PRX(2) = XMAX
         PRY(1) = YMIN
         PRY(2) = YMAX
      endif

      call get2r ( 'XRANGE', PRX(1), PRX(2), .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return
      call cswopr ( PRX(1), PRX(2) )

      call get2r ( 'YRANGE', PRY(1), PRY(2), .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return
      call cswopr ( PRY(1), PRY(2) )

      GOTPLIMS = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_HRANGE -- Get histogram range
C
C     a j penny                 ral               1991 May

      subroutine tbpl_hrange ( table1 )

      implicit none
      include 'tbplot.inc'
      include 'STARMAN_INC'

      real        table1(TBVX1,TBY1)	!i: Input data
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTTABLE ) then
         call printo ( 'Need to have got table' )
         return
      endif

      if ( .not.GOTDLIMS ) then
         GOTDLIMS = .true.

         XMIN = table1((NCOL1+5),1)
         XMAX = table1((NCOL1+5),1)
         do k = 1, TBY1
            XMIN = min(XMIN,table1((NCOL1+5),k))
            XMAX = max(XMAX,table1((NCOL1+5),k))
         enddo

         PHX(1) = XMIN
         PHX(2) = XMAX

      endif

      if ( .not.GOTHLIMS ) then
         PHX(1) = XMIN
         PHX(2) = XMAX
      endif

      call get2r ( 'HRANGE', PHX(1), PHX(2), .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return
      call cswopr ( PHX(1), PHX(2) )

      call get1i ( 'NBIN', NBIN, NBIN, 1, 200 )
      if ( ST_FAILED ) return

      GOTHLIMS = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_PSIZE -- Get plot size
C
C     a j penny                 ral               1991 May

      subroutine tbpl_psize ( table1, table2, kopt )

      implicit none
      include 'tbplot.inc'
      include 'STARMAN_INC'

      real        table1(TBVX1,TBY1)	!i: Input data
      real        table2(TBVX2,TBY2)	!i: Input data
      integer     kopt			!i: Flag for get new size definitely (1)
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTTABLE ) then
         call printo ( 'Need to have got table' )
         return
      endif

      if ( .not.GOTDLIMS ) then
         GOTDLIMS = .true.

         XMIN = table1((NCOL1+5),1)
         XMAX = table1((NCOL1+5),1)
         do k = 1, TBY1
            XMIN = min(XMIN,table1((NCOL1+5),k))
            XMAX = max(XMAX,table1((NCOL1+5),k))
         enddo

         YMIN = table2((NCOL2+5),1)
         YMAX = table2((NCOL2+5),1)
         do k = 1, TBY2
            YMIN = min(YMIN,table2((NCOL2+5),k))
            YMAX = max(YMAX,table2((NCOL2+5),k))
         enddo

         PSX(1) = XMIN - 0.05*(XMAX-XMIN)
         PSX(2) = XMAX + 0.05*(XMAX-XMIN)
         PSY(1) = YMIN - 0.05*(YMAX-YMIN)
         PSY(2) = YMAX + 0.05*(YMAX-YMIN)

      endif

      if ( kopt.eq.1 ) GOTPSIZE = .false.

      if ( .not.GOTPSIZE ) then
         PSX(1) = XMIN - 0.05*(XMAX-XMIN)
         PSX(2) = XMAX + 0.05*(XMAX-XMIN)
         PSY(1) = YMIN - 0.05*(YMAX-YMIN)
         PSY(2) = YMAX + 0.05*(YMAX-YMIN)
      endif

      call get2r ( 'DEVLIMX', PSX(1), PSX(2), .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return

      call get2r ( 'DEVLIMY', PSY(1), PSY(2), .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return

      GOTPSIZE = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_GETTB -- Get Table
C
C     a j penny                 ral               1991 May

      subroutine tbpl_gettb ( )

      implicit none
      include 'tbplot.inc'
      include 'STARMAN_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      if ( GOTTABLE ) then
         call canpar ( 'IN1' )
         GOTTABLE = .false.
         if ( GOTFILE2 ) call canpar ( 'IN2' )
         GOTFILE2 = .false.
      endif

      call optabr ( 'IN1', IPTAB1, TBVX1, TBY1, .true., istat )
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) return

      TBX1 = TBVX1 - 5
      call get1i ( 'NCOL1', NCOL1, 1, 1, TBX1 )
      if ( ST_FAILED ) return

      GOTTABLE = .true.
      GOTDLIMS = .false.

      if ( .not.ISHIST ) then
         call printo ( 'If same table, just enter -return-' )
         call optabr ( 'IN2', IPTAB2, TBVX2, TBY2, .true., istat )
         if ( ST_FAILED ) return

         FILE2OK = .true.
         if ( istat.eq.0 ) then
            GOTFILE2 = .true.
            TBX2 = TBVX2 - 5
            call get1i ( 'NCOL2', NCOL2, 1, 1, TBX2 )
            if ( ST_FAILED ) return
            call gthead ( 'IN2', NCOL2, YHEAD, istat )
         elseif ( istat.eq.2 .or. istat.eq.3 ) then
            GOTFILE2 = .false.
            IPTAB2 = IPTAB1
            TBVX2 = TBVX1
            TBY2 = TBY1
            TBX2 = TBX1
            call get1i ( 'NCOL2', NCOL2, 1, 1, TBX2 )
            if ( ST_FAILED ) return
            call gthead ( 'IN1', NCOL2, YHEAD, istat )
         endif
         if ( ST_FAILED ) return
      endif

      call gthead ( 'IN1', NCOL1, XHEAD, istat )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_GETIM -- Get Image
C
C     a j penny                 ral               1991 May

      subroutine tbpl_getim ( )

      implicit none
      include 'tbplot.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer ierr
Cbegin


      if ( ST_FAILED ) return

      if ( GOTIM ) then
         call canpar ( 'IM' )
         GOTIM = .false.
      endif

      call opimgr ( 'IM', IPIM, NX, NY, IMTYPE, .false., ierr )
      if ( ierr.ne.0 ) return
      if ( ST_FAILED ) return

      if ( IMTYPE.ne.'REAL' .and. IMTYPE.ne.'SHORT' ) then
         call printo ( 'ERROR: Image must be REAL or INTEGER*2' )
         call pargc ( IMTYPE )
         call printo ( 'ERROR: Image type is: %c ' )
         ST_FAILED = .true.
         return
      endif

      call gtimzd ( 'IM', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE, ierr)
      if ( ST_FAILED ) return

      GOTIM = .true.
      XHEAD = 'X'
      YHEAD = 'Y'


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_CLEAR -- Clear display
C
C    a j penny                      ral                  1991 may

      subroutine tbpl_clear ( kopt )

      implicit none
      include 'tbplot.inc'
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'

      integer   kopt		!i: Option (1=just plotted data;2=all)
C--
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GD_DISPOP ) then					!Is device open?
         call printo ( 'ERROR: Device not open' )
         return
      endif

      call pgpage

      if ( kopt.eq.1 ) then
         call gd_dobox ( PSX(1), PSX(2), ' ', PSY(1), PSY(2), 		!Draw axes
     +                   ' ', ' ', KASPECT )
      else
         DONEAXIS = .false.
         GOTASPECT = .false.
         GOTPSIZE = .false.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_OPEN -- Open device
C
C   a j penny                       ral         1991 may

      subroutine tbpl_open ( )

      implicit none
      include 'tbplot.inc'
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      if ( GD_DISPOP ) then
         call printo ( 'ERROR: Device already open' )
      else
         call gd_open ( istat )
         DONEAXIS = .false.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_CLOSE -- Close device
C
C   a j penny                  ral         1991 may

      subroutine tbpl_close ( )

      implicit none
      include 'tbplot.inc'
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GD_DISPOP ) then
         call printo ( 'ERROR: Device not open' )
      else
         call gd_close
         DONEAXIS = .false.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_HIST -- Set up and start histogram
C
C   a j penny                 ral                  1991 may

      subroutine tbpl_hist ( )

      implicit none

      include 'tbplot.inc'
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'
C--
      integer istat, ipw
Cbegin


      if ( ST_FAILED ) return

      ISHIST = .true.

      if ( .not.GD_DISPOP ) call tbpl_open				!Is device open?
      if ( .not.GD_DISPOP ) return

      if ( .not.GOTTABLE ) call tbpl_gettb				!Loaded data?

      call tbpl_hrange ( %val(IPTAB1) )					!Get hist limits

      DONEAXIS = .false.

      call gtwrkr ( 'WORK', TBY1, ipw, istat )

      call copfrr ( %val(IPTAB1), TBVX1, TBY1, (NCOL1+5), 1,
     +              %val(ipw), TBY1 )

      call pghist ( TBY1, %val(ipw), PHX(1), PHX(2), NBIN, 0 )

      call wrkcan ( 'WORK' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_PLOT -- Set up and start plotting
C
C   a j penny                 ral                  1991 may

      subroutine tbpl_plot ( kopt )

      implicit none

      include 'tbplot.inc'
      include 'ST_GRAPH_INC'
      include 'STARMAN_INC'

      integer    kopt		!i: Option (1=points/2=line)
C--
      real dx, dy
      logical kas, kab
Cbegin


      if ( ST_FAILED ) return

      ISHIST = .false.

      if ( .not.GD_DISPOP ) call tbpl_open				!Is device open?
      if ( .not.GD_DISPOP ) return

      if ( .not.GOTTABLE .or. .not.FILE2OK ) call tbpl_gettb		!Loaded data?

      if ( .not.GOTTABLE ) then
         call printo ( 'Must have got data' )
         return
      endif

      if ( .not.GOTPLIMS ) call tbpl_prange ( %val(IPTAB1), 		!Get posn limits
     +                                        %val(IPTAB2), 0 )

      if ( .not.GOTPSIZE ) call tbpl_psize (%val(IPTAB1), %val(IPTAB2),	!Get plot size
     +                                      0 )

      if ( .not.GOTASPECT ) then
         kab = .true.
         if ( KASPECT.eq.0 ) kab = .false.
         call get1b ( 'ASPECT', kas, kab )
         KASPECT = 1
         if ( .not.kas ) KASPECT = 0
         GOTASPECT = .true.
      endif

      dx = 0.0
      dy = 0.0
      call get2r ( 'IMOFFSET', dx, dy, .true., -1.0e20, 1.0e20 )

      if ( .not.DONEAXIS ) call gd_dobox ( PSX(1), PSX(2), ' ',		!Draw axes
     +                          PSY(1), PSY(2), ' ', ' ', KASPECT )
      DONEAXIS = .true.

      call tbpl_doplot ( %val(IPTAB1), %val(IPTAB2), dx, dy, kopt )	!Plot the data


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_DOPLOT -- Plot points or line
C
C   a j penny               ral                     1991 may

      subroutine tbpl_doplot ( table1, table2, dx, dy, kopt )

      implicit none

      include 'tbplot.inc'
      include 'STARMAN_INC'

      real        table1(TBVX1,TBY1)	!i: Input data
      real        table2(TBVX2,TBY2)	!i: Input data
      real        dx			!i: Offset in X to plot with
      real        dy			!i: Offset in Y to plot with
      integer     kopt			!i: option (1=points/2=line)
C--
      integer j, kch
      real    x, y
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTTABLE ) then
         call printo ( 'Must have loaded tables' )
         return
      endif

      if ( kopt.eq.1 ) then
         call get1i ( 'SYMBOL', kch, 2, 0, 31 )
         if ( ST_FAILED ) return
         j = 0
         do while ( j.lt.TBY1 .and. j.lt.TBY2 )
            j = j + 1
            x = table1((NCOL1+5),j) + dx
            y = table2((NCOL2+5),j) + dy
            if ( x.ge.PRX(1) .and. x.le.PRX(2) .and. y.ge.PRY(1)
     +           .and. y.le.PRY(2) ) call pgpoint ( 1, x, y, kch )
         enddo
      else
         call get1i ( 'LSTYLE', kch, 1, 1, 5 )
         if ( ST_FAILED ) return
         call pgsls ( kch )
         x = table1((NCOL1+5),1) + dx
         y = table2((NCOL2+5),1) + dy
         call pgmove ( x, y )
         j = 1
         do while ( j.lt.TBY1 .and. j.lt.TBY2 )
            j = j + 1
            x = table1((NCOL1+5),j) + dx
            y = table2((NCOL2+5),j) + dy
            call pgdraw ( x, y )
         enddo
         call pgsls ( 1 )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_GCURSE -- Get positions with cursor
C
C     a j penny                 ral               1991 May

      subroutine tbpl_gcurse ( table1, table2 )

      implicit none
      include 'tbplot.inc'
      include 'STARMAN_INC'

      real        table1(TBVX1,TBY1)	!i: Input data
      real        table2(TBVX2,TBY2)	!i: Input data
C--
      real x, y, xa, ya, dd, ddmin
      integer j, jmin
      logical more
      character ch*1, name*20
Cbegin


      if ( ST_FAILED ) return

      if ( .not.DONEAXIS ) then
         call printo ( 'Need to have plotted area' )
         return
      endif

      more = .true.
      do while ( more )

         call pgcurse ( x, y, ch )

         if ( x.gt.max(PSX(1),PSX(2)) .or. x.lt.min(PSX(1),PSX(2)) .or.
     +      y.gt.max(PSY(1),PSY(2)) .or. y.lt.min(PSY(1),PSY(2)) ) then
            more = .false.
         else
            call pargr ( x )
            call pargr ( y )
            call printd ( 'X = %f : Y = %f' )
            if ( GOTTABLE ) then
               jmin = 1
               ddmin = 1.0e30
               j = 0
               do while ( j.lt.TBY1 .and. j.lt.TBY2 )
                  j = j + 1
                  xa = table1((NCOL1+5),j)
                  ya = table2((NCOL2+5),j)
                  dd = (x-xa)*(x-xa) + (y-ya)*(y-ya)
                  if ( dd.lt.ddmin ) then
                     ddmin = dd
                     jmin = j
                  endif
               enddo
               xa = table1((NCOL1+5),jmin)
               ya = table2((NCOL2+5),jmin)
               call namegt ( table1, TBVX1, TBY1, jmin, name )
               call pargi ( jmin )
               call pargc ( name )
               call pargr ( xa )
               call pargr ( ya )
               call printd (
     +   'Nearest point is number %d : Name %c : X = %f : Y = %f ' )
            endif
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_CONTOUR -- Put out contour plot to device
C
C  alan penny                ral                      1990-06-15

      subroutine tbpl_contour ( )

      implicit none
      include 'tbplot.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_GRAPH_INC'
C--
      integer lx, ly, ierr, ipconr, ipcons, j, ja, ngood, nbad, ncon,
     +        kl, lens, kp(4), istat, ia, ib
      real alx, aly, c(200), tr(6), alo, ahi, astep
      character*200 text
      logical kas, kab
      data tr / 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 /
      external lens
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIM ) then
         call printo ( 'ERROR: No image got yet' )
         return
      endif

      if ( .not.GD_DISPOP ) call tbpl_open				!Is device open?
      if ( .not.GD_DISPOP ) return

      ia = 1
      ib = NX
      call get2i ( 'IMXRANGE', ia, ib, .true., 1, NX )            	!Get X window
      if ( ST_FAILED ) return
      NXS = min(ia,ib)
      NXE = max(ia,ib)

      ia = 1								!Get Y window
      ib = NY
      call get2i ( 'IMYRANGE', ia, ib, .true., 1, NY )
      if ( ST_FAILED ) return
      NYS = min(ia,ib)
      NYE = max(ia,ib)

      if ( NXS.eq.NXE .or. NYS.eq.NYE ) then
         call printo ( 'ERROR: Each side must be more than 1 pixel' )
         return
      endif

      if ( .not.GOTPSIZE ) then
         PSX(1) = NXS
         PSX(2) = NXE
         PSY(1) = NYS
         PSY(2) = NYE
         call get2r ( 'DEVLIMX', PSX(1), PSX(2), .true., -1.0e20,
     +                1.0e20 )
         if ( ST_FAILED ) return
         call get2r ( 'DEVLIMY', PSY(1), PSY(2), .true., -1.0e20,
     +                1.0e20 )
         if ( ST_FAILED ) return
         GOTPSIZE = .true.
      endif

      if ( .not.GOTASPECT ) then
         kab = .true.
         if ( KASPECT.eq.0 ) kab = .false.
         call get1b ( 'ASPECT', kas, kab )
         KASPECT = 1
         if ( .not.kas ) KASPECT = 0
         GOTASPECT = .true.
      endif

      if ( .not.DONEAXIS ) then						!Draw axes
         call gd_dobox ( PSX(1), PSX(2), ' ',
     +                   PSY(1), PSY(2), ' ', ' ', KASPECT )
         DONEAXIS = .true.
      endif

      call get2r ( 'IMOFFSET', tr(1), tr(4), .true., -1.0e20, 1.0e20 )

      lx = NXE - NXS + 1						!Get area into work space
      ly = NYE - NYS + 1
      call gtwrkr ( 'CONTR', lx*ly, ipconr, ierr )
      if ( ierr.ne.0 ) return
      call gtwrkr ( 'CONTS', lx*ly, ipcons, ierr )
      if ( ierr.ne.0 ) then
         call wrkcan ( 'CONTR' )
         return
      endif
      if ( IMTYPE.eq.'SHORT' ) then
         call copss ( %val(IPIM), NX, NY, NXS, NXE, NYS, NYE,
     +                %val(ipcons), lx, ly, 1, 1 )
         call copssr ( %val(ipcons), lx, ly, BS, BZ, INVAL,
     +                 %val(ipconr) )
      else
         call coprr ( %val(IPIM), NX, NY, NXS, NXE, NYS, NYE,
     +                %val(ipcons), lx, ly, 1, 1 )
         call coprrr ( %val(ipcons), lx, ly, BS, BZ, RINVAL,
     +                 %val(ipconr) )
      endif

      call st_minmax ( %val(ipcons), lx, ly, BS, BZ, IMTYPE, INVAL, 	!Get contour values
     +             RINVAL, 1, lx, 1, ly, alo, ahi, kp, ngood, nbad )
      astep = (ahi-alo)/10.0
      if ( astep.eq.0.0 ) astep = 1.0
      call get3r ( 'CONTOUR', alo, ahi, astep, .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return
      ncon = 1 + int((ahi-alo)/astep)
      ncon = min(200,ncon)
      do j = 1, ncon
         c(j) = alo + astep*(real(j)-1.0)
      enddo

      call pgcons ( %val(ipconr), lx, ly, 1, lx, 1, ly, c, ncon, tr )

      call pgupdt

      call wrkcan ( 'CONTR' )						!Close work areas
      call wrkcan ( 'CONTS' )

      call printo ( ' ' )
      ia = tr(1)
      ib = tr(4)
      write ( text, '('' Image pixel ('',i6,'','',i6,'') is at plot'',
     +      '' position ('',i6,'','',i6,'')'' )' ) NXS, NYS, ia, ib
      j = index(text,'l (')
      call lbgone(text(j+3:))
      ja = index(text(j:),',')
      call lbgone(text(j+ja:))
      j = index(text,'n (')
      call lbgone(text(j+3:))
      ja = index(text(j:),',')
      call lbgone(text(j+ja:))
      kl = lens(text)
      call printo ( text(1:kl) )
      write ( text, '('' Contour levels start at '',f16.3,'' and '',
     +               ''step by '',f16.3)' ) alo, astep
      j = index(text,' at ')
      call lbgone(text(j+4:))
      j = index(text,' by ')
      call lbgone(text(j+4:))
      kl = lens(text)
      call printo ( text(1:kl) )
      text = ' Image title is: '//IMTITLE
      kl = lens(text)
      call printo ( text(1:kl) )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_IMAGE -- Put out image to device
C
C  alan penny                ral                      1990-06-15

      subroutine tbpl_image ( )

      implicit none
      include 'tbplot.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
      include 'ST_GRAPH_INC'
      include 'ST_DS_GEN_INC'
C--
      integer lx, ly, ierr, ipconr, ipcons, j, ja, ngood, nbad,
     +        kl, lens, kp(4), istat, ia, ib
      real alx, aly, tr(6), abot, atop
      character*200 text
      logical kas, kab
      data tr / 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 /
      external lens
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GOTIM ) then
         call printo ( 'ERROR: No image got yet' )
         return
      endif

      if ( .not.GD_DISPOP ) call tbpl_open				!Is device open?
      if ( .not.GD_DISPOP ) return

      ia = 1
      ib = NX
      call get2i ( 'IMXRANGE', ia, ib, .true., 1, NX )            	!Get X window
      if ( ST_FAILED ) return
      NXS = min(ia,ib)
      NXE = max(ia,ib)

      ia = 1								!Get Y window
      ib = NY
      call get2i ( 'IMYRANGE', ia, ib, .true., 1, NY )
      if ( ST_FAILED ) return
      NYS = min(ia,ib)
      NYE = max(ia,ib)

      if ( NXS.eq.NXE .or. NYS.eq.NYE ) then
         call printo ( 'ERROR: Each side must be more than 1 pixel' )
         return
      endif

      if ( .not.GOTPSIZE ) then
         PSX(1) = NXS
         PSX(2) = NXE
         PSY(1) = NYS
         PSY(2) = NYE
         call get2r ( 'DEVLIMX', PSX(1), PSX(2), .true., -1.0e20,
     +                1.0e20 )
         if ( ST_FAILED ) return
         call get2r ( 'DEVLIMY', PSY(1), PSY(2), .true., -1.0e20,
     +                1.0e20 )
         if ( ST_FAILED ) return
         GOTPSIZE = .true.
      endif

      if ( .not.GOTASPECT ) then
         kab = .true.
         if ( KASPECT.eq.0 ) kab = .false.
         call get1b ( 'ASPECT', kas, kab )
         KASPECT = 1
         if ( .not.kas ) KASPECT = 0
         GOTASPECT = .true.
      endif

      if ( .not.DONEAXIS ) then						!Draw axes
         call gd_dobox ( PSX(1), PSX(2), ' ',
     +                   PSY(1), PSY(2), ' ', ' ', KASPECT )
         DONEAXIS = .true.
      endif

      call get2r ( 'IMOFFSET', tr(1), tr(4), .true., -1.0e20, 1.0e20 )

      lx = NXE - NXS + 1						!Get area into work space
      ly = NYE - NYS + 1
      call gtwrkr ( 'CONTR', lx*ly, ipconr, ierr )
      if ( ierr.ne.0 ) return
      call gtwrkr ( 'CONTS', lx*ly, ipcons, ierr )
      if ( ierr.ne.0 ) then
         call wrkcan ( 'CONTR' )
         return
      endif
      if ( IMTYPE.eq.'SHORT' ) then
         call copss ( %val(IPIM), NX, NY, NXS, NXE, NYS, NYE,
     +                %val(ipcons), lx, ly, 1, 1 )
         call copssr ( %val(ipcons), lx, ly, BS, BZ, INVAL,
     +                 %val(ipconr) )
      else
         call coprr ( %val(IPIM), NX, NY, NXS, NXE, NYS, NYE,
     +                %val(ipcons), lx, ly, 1, 1 )
         call coprrr ( %val(ipcons), lx, ly, BS, BZ, RINVAL,
     +                 %val(ipconr) )
      endif

      call ds_imgscl ( %val(IPIM), NX, NY, IMTYPE, 1, NX, 1, NY )
      atop = BS*DSVMIN + BZ
      abot = BS*DSVMAX + BZ
      call get2r ( 'VPRANGE', abot, atop, .true., -1.0e10, 1.0e10 )
      if ( ST_FAILED ) return

      call pggray ( %val(ipconr), lx, ly, 1, lx, 1, ly, atop, abot, tr)

      call pgupdt

      call wrkcan ( 'CONTR' )						!Close work areas
      call wrkcan ( 'CONTS' )

      call printo ( ' ' )
      ia = tr(1)
      ib = tr(4)
      write ( text, '('' Image pixel ('',i6,'','',i6,'') is at plot'',
     +      '' position ('',i6,'','',i6,'')'' )' ) NXS, NYS, ia, ib
      j = index(text,'l (')
      call lbgone(text(j+3:))
      ja = index(text(j:),',')
      call lbgone(text(j+ja:))
      j = index(text,'n (')
      call lbgone(text(j+3:))
      ja = index(text(j:),',')
      call lbgone(text(j+ja:))
      kl = lens(text)
      call printo ( text(1:kl) )
      text = ' Image title is: '//IMTITLE
      kl = lens(text)
      call printo ( text(1:kl) )
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_TEXT -- Put out text
C
C  alan penny                ral                      1990-06-15

      subroutine tbpl_text ( )

      implicit none
      include 'tbplot.inc'
      include 'STARMAN_INC'
      include 'ST_GRAPH_INC'
C--
      integer kl, lens
      real x, y
      character*200 text
      external lens
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GD_DISPOP ) call tbpl_open				!Is device open?
      if ( .not.GD_DISPOP ) return


      call get1c ( 'TEXT', text, ' ', .true. )
      x = PSX(1)
      y = PSY(1)
      call get2r ( 'POSITION', x, y, .true., -1.0e20, 1.0e20 )

      call lbgone(text)
      kl = lens(text)
      call pgtext ( x, y, text(1:kl) )

      call pgupdt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPL_LABEL -- Put out labels
C
C  alan penny                ral                      1990-06-15

      subroutine tbpl_label ( )

      implicit none
      include 'tbplot.inc'
      include 'STARMAN_INC'
      include 'ST_GRAPH_INC'
C--
      integer klx, kly, lens
      character*200 textx, texty
      external lens
Cbegin


      if ( ST_FAILED ) return

      if ( .not.GD_DISPOP ) call tbpl_open				!Is device open?
      if ( .not.GD_DISPOP ) return

      if ( .not.DONEAXIS ) then
         call printo ( 'Need to have plotted area' )
         return
      endif

      call get1c ( 'XLABEL', textx, XHEAD, .true. )
      call get1c ( 'YLABEL', texty, YHEAD, .true. )

      call lbgone(textx)
      klx = lens(textx)
      call lbgone(texty)
      kly = lens(texty)

      call pglabel ( textx(1:klx), texty(1:kly), ' ' )

      call pgupdt


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   TBPMATCH.FOR
C
C  Contains:-
C
C T_TBPMATCH   Extract entries with matching positions in two tables
C TBPM_CHECK   Find size of output table
C TBPM_DOIT    Load the output table


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBPMATCH -- Extract entries with matching positions in two tables
C
C         A J Penny            RGO                                82-11-4

      subroutine t_tbpmatch ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      character title*50
      integer ierr, ipin1, ipin2, ipo1, ipo2, kcol, kmeth, iv, ipnwk, k
      real tolern

      character*1000 topt
      data topt / 'nearest:largest:smallest:alldup:allnodup' /
      integer nthelp
      parameter ( nthelp=11 )
      character*68 thelp(nthelp)
      data (thelp(k),k=1,10) /
     + 'If the 2nd table has more than one star near enough to the' ,
     + 'position of a star in the 1st table, then there are a number' ,
     + 'of options for dealing with this' ,
     + '   ',
     + 'Option      Function: Choose -' ,
     + '------      ------------------' ,
     + 'Alldup      All such stars - each star as often as it appears ',
     + 'Allnodup    All such stars - each star only once ',
     + 'Largest     Only the star with the largest value in NUMCOL' ,
     + 'Nearest     Only the nearest star' /
      data (thelp(k),k=11,nthelp) /
     + 'Smallest    Only the star with the smallest value in NUMCOL' /
Cbegin


      call optabr ( 'IN1', ipin1, TBVX1, TBY1, .false., ierr )		!Obtain 1st input table
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call optabr ( 'IN2', ipin2, TBVX2, TBY2, .false., ierr )		!Obtain 2nd input table
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call get1r ( 'MISMATCH', tolern, 1.0, 0.0, 1.0e10 )		!Get distance tolerance
      if ( ST_FAILED ) return

      call get_job ( 'METHOD', topt, kmeth, 1, thelp, 0  )		!Get selection method
      if ( ST_FAILED ) return
      kcol = 1
      if ( kmeth.eq.2 .or. kmeth.eq.3 ) then
         call get1i ( 'NUMCOL', kcol, 1, 1, TBVX2-5 )
         if ( ST_FAILED ) return
      endif

      call gtwrki ( 'WORK', TBY2, ipnwk, ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call tbpm_check ( %val(ipin1), %val(ipin2), %val(ipnwk), 		!Find no of entries passed
     +                 kmeth, tolern )

      if ( TBYO2.eq.0 ) then						!See if any found
         call printo ( 'No matching positions - no output tables' )
         return
      else
         if ( kmeth.eq.4 .or. kmeth.eq.5 ) then
            call pargi ( TBYO1 )
            call printd ( 'Number in first list matched = %d' )
            call pargi ( TBYO2 )
            call printd ( 'Number in second list matched = %d' )
         else
            call pargi ( TBYO1 )
            call printd (
     +              'Number of stars with matching positions = %d' )
         endif
      endif

      call optabw ( 'OUT1', ipo1, TBVX1, TBYO1, .false.,ierr )		!Open output list
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call tcopdes ( 'IN1', 'OUT1', ierr )				!Get title to output list and store it and the descriptors
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call gtdesc ( 'IN1', 'TITLE', title, 'Output from Tbpmatch', iv,
     +              ierr )
      call get1c  ( 'TITLE1', title,    title, .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUT1',   'TITLE',  title )

      call optabw ( 'OUT2', ipo2, TBVX2, TBYO2, .false., ierr )		!Open 2nd output list
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call tcopdes ( 'IN2', 'OUT2', ierr )				!Get title to output list and store it and the descriptors
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call gtdesc ( 'IN2', 'TITLE', title, 'Output from Tbpmatch', iv,
     +              ierr )
      call get1c  ( 'TITLE2',  title,    title, .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUT2',    'TITLE',  title )

      call tbpm_doit ( %val(ipin1), %val(ipin2), %val(ipo1), 		!Do the loading of the output from the input
     +                 %val(ipo2), %val(ipnwk), tolern, kmeth, kcol )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPM_CHECK -- Find size of output table
C
C    A J PENNY               RGO                      82-11-4

      subroutine tbpm_check ( tb1, tb2, ngot, kmeth, tolern )

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real	tb1(TBVX1,TBY1)		!i: 1st input table
      real	tb2(TBVX2,TBY2)		!i: @nd input table
      integer	ngot(TBY2)		!o:
      integer   kmeth			!i:
      real	tolern			!1:
C--
      integer k, j
      logical found
      real xa, ya, ttol, xb, yb, dd
Cbegin


      if ( ST_FAILED ) return

      ttol = tolern*tolern

      if ( kmeth.eq.5 ) then
         call azeroi ( ngot, TBY2 )
         TBYO1 = 0
         TBYO2 = 0
         do k = 1, TBY1
            xa = tb1(6,k)
            ya = tb1(7,k)
            found = .false.
            do j = 1, TBY2
               xb = tb2(6,j)
               yb = tb2(7,j)
               dd = (xb-xa)*(xb-xa) + (yb-ya)*(yb-ya)
               if ( dd.le.ttol ) then
                  if ( .not.found ) then
                     TBYO1 = TBYO1 + 1
                     found = .true.
                  endif
                  if ( ngot(j).eq.0 ) TBYO2 = TBYO2 + 1
                  ngot(j) = 1
               endif
            enddo
         enddo
      endif

      if ( kmeth.eq.4 ) then
         TBYO1 = 0
         TBYO2 = 0
         do k = 1, TBY1
            xa = tb1(6,k)
            ya = tb1(7,k)
            found = .false.
            do j = 1, TBY2
               xb = tb2(6,j)
               yb = tb2(7,j)
               dd = (xb-xa)*(xb-xa) + (yb-ya)*(yb-ya)
               if ( dd.le.ttol ) then
                  if ( .not.found ) then
                     TBYO1 = TBYO1 + 1
                     found = .true.
                  endif
                  TBYO2 = TBYO2 + 1
               endif
            enddo
         enddo
      endif

      if ( kmeth.eq.1 .or. kmeth.eq.2 .or. kmeth.eq.3 ) then
         do k = 1, TBY1
            xa = tb1(6,k)
            ya = tb1(7,k)
            found = .false.
            j = 0
            do while ( j.lt.TBY2 .and. .not.found )
               j = j + 1
               xb = tb2(6,j)
               yb = tb2(7,j)
               dd = (xb-xa)*(xb-xa) + (yb-ya)*(yb-ya)
               if ( dd.le.ttol ) found = .true.
            enddo
            if ( found ) then
               TBYO1 = TBYO1 + 1
               TBYO2 = TBYO2 + 1
            endif
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPM_DOIT -- Load the output table
C
C alan penny                    ral           1990-06-06

      subroutine tbpm_doit ( tb1, tb2, tbo1, tbo2, ngot, tolern,
     +                       kmeth, kcol )

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real	tb1(TBVX1,TBY1)		!i:
      real	tb2(TBVX2,TBY2)		!i:
      real	tbo1(TBVX1,TBYO1)	!o:
      real	tbo2(TBVX2,TBYO2)	!o:
      integer	ngot(TBYO2)		!i:
      real	tolern			!i:
      integer	kmeth			!i:
      integer	kcol			!i:
C--
      logical found
      real ttol, xa, ya, xb, yb, dd, cv, cmin, cmax, dmin
      integer k, j, ka, kout, kout1, kout2
Cbegin


      if ( ST_FAILED ) return

      ttol = tolern*tolern

      if ( kmeth.eq.5 ) then
         kout1 = 0
         kout2 = 0
         call amovki ( 0, ngot, TBYO2 )
         do k = 1, TBY1

            xa = tb1(6,k)
            ya = tb1(7,k)
            found = .false.
            do j = 1, TBY2
               xb = tb2(6,j)
               yb = tb2(7,j)
               dd = (xb-xa)*(xb-xa) + (yb-ya)*(yb-ya)
               if ( dd.le.ttol ) then
                  if ( .not.found ) then
                     kout1 = kout1 + 1
                     call coprr ( tb1, TBVX1, TBY1, 1, TBVX1, k, k,
     +                            tbo1, TBVX1, TBYO1, 1, kout1 )
                     found = .true.
                  endif
                  if ( ngot(j).eq.0 ) then
                     ngot(j) = 1
                     kout2 = kout2 + 1
                     call coprr ( tb2, TBVX2, TBY2, 1, TBVX2, k, k,
     +                            tbo2, TBVX2, TBYO2, 1, kout2 )
                  endif
               endif
            enddo

         enddo
      endif

      if ( kmeth.eq.4 ) then
         kout1 = 0
         kout2 = 0
         do k = 1, TBY1

            xa = tb1(6,k)
            ya = tb1(7,k)
            found = .false.
            do j = 1, TBY2
               xb = tb2(6,j)
               yb = tb2(7,j)
               dd = (xb-xa)*(xb-xa) + (yb-ya)*(yb-ya)
               if ( dd.le.ttol ) then
                  if ( .not.found ) then
                     kout1 = kout1 + 1
                     call coprr ( tb1, TBVX1, TBY1, 1, TBVX1, k, k,
     +                            tbo1, TBVX1, TBYO1, 1, kout1 )
                     found = .true.
                  endif
                  kout2 = kout2 + 1
                     call coprr ( tb2, TBVX2, TBY2, 1, TBVX2, k, k,
     +                            tbo2, TBVX2, TBYO2, 1, kout2 )
               endif
            enddo

         enddo
      endif

      if ( kmeth.eq.1 .or. kmeth.eq.2 .or. kmeth.eq.3 ) then
         kout = 0
         do k = 1, TBY1

            xa = tb1(6,k)
            ya = tb1(7,k)
            found = .false.
            do j = 1, TBY2
               xb = tb2(6,j)
               yb = tb2(7,j)
               dd = (xb-xa)*(xb-xa) + (yb-ya)*(yb-ya)
               cv = tb2(kcol+5,j)
               if ( dd.le.ttol ) then
                  if ( .not.found ) then
                     found = .true.
                     dmin = dd
                     cmin = cv
                     cmax = cv
                     ka = j
                  else
                     if ( kmeth.eq.1 .and. dd.lt.dmin ) then
                        dmin = dd
                        ka = j
                     elseif ( kmeth.eq.2 .and. cv.gt.cmax ) then
                        cmax = cv
                        ka = j
                     elseif ( kmeth.eq.3 .and. cv.lt.cmin ) then
                        cmin = cv
                        ka = j
                     endif
                  endif
               endif
            enddo

            if ( found ) then
               kout = kout + 1
               call coprr ( tb1, TBVX1, TBY1, 1, TBVX1, k, k,
     +                      tbo1, TBVX1, TBYO1, 1, kout )
               call coprr ( tb2, TBVX2, TBY2, 1, TBVX2, ka, ka,
     +                      tbo2, TBVX2, TBYO2, 1, kout )
            endif

         enddo

      endif


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   TBRENUM.FOR
C
C   Contains:-
C
C T_TBRENUM    Renumber names in a table


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBRENUM -- Renumber names in a table
C
C    alan penny                      ral              1991 May

      subroutine t_tbrenum ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      character title*50
      integer ierr, ipin, ipo, iv
Cbegin


      call optabr ( 'IN', ipin, TBVX, TBY, .false., ierr )		!Open input
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         call printo ( 'ERROR: Cant access input file' )
         ST_FAILED = .true.
         return
      endif

      call optabw ( 'OUT', ipo, TBVX, TBY, .false., ierr )		!Open output
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         call printo ( 'ERROR: Cant access output file' )
         ST_FAILED = .true.
         return
      endif

      call gtdesc ( 'IN', 'TITLE', title, ' ', iv, ierr )		!Get title
      call get1c ( 'TITLE', title, title, .true. )
      if ( ST_FAILED ) return

      call amovr ( %val(ipin), %val(ipo), TBVX*TBY )			!Copy data
      call ident ( %val(ipo), TBVX, TBY )				!Load new names
      call tcopdes ( 'IN', 'OUT', ierr )				!Copy descriptors
      call ptdesc ( 'OUT', 'TITLE', title )				!Put new title


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C This is T_TBSHEET.F
C
C It contains:-
C
C T_TBSHEET       Spread-sheet inspection and modification of a table
C TBSH_SETUP      Set starting values
C TBSH_SETRC      Set starting valid row/columns
C TBSH_FRAME      Load sheet frame
C TBSH_CWORDS     Load command words
C TBSH_SETSIZE    Set sheet format size parameters
C TBSH_DLOAD      Load sheet data
C TBSH_NLOAD      Load sheet names
C TBSH_HLOAD      Load sheet headers
C TBSH_DOIT       Do the work
C TBSH_EXIT       Exit if really wanted
C TBSH_GETOPT     Get option
C TBSH_WMESS      Put up waiting message
C TBSH_SEEPOS     Get column, row of given position
C TBSH_GETPOS     Get column, row of actual holes table
C TBSH_HELP       Output help
C TBSH_INPUT      Get new input
C TBSH_BLANK      Blank present display
C TBSH_CALC       Calculate a column
C TBSH_STORE      Store output (part 1)
C TBSH_STOREA     Store output (part 2)
C TBSH_CHANGE     Change data, name, header in table
C TBSH_FORMAT     Select and change format and colours of sheet
C TBSH_FORMLOAD   Load format of sheet
C TBSH_SETCOL     Change colours of sheet
C TBSH_WEED       Weed table on column
C TBSH_CONVERT    Convert a 'visible' row/column number to an actual holes one
C TBSH_BCONVERT   Convert an actual holes row/column number to a 'visible' one
C TBSH_WCHECK     Find how many rows will survive
C TBSH_WFLAGB     Flag 'bad' rows in table as not to be used
C TBSH_CALCGCL    Get calculator input info from the command line
C TBSH_CALCLOAD   Load present array into no 'holes' array
C TBSH_POLISH     Decode equation and sort into reverse Polish
C TBSH_DOCALC     Do the calculations
C TBSH_LOAD       Copy table into a 3-D stack
C TBSH_LOADA      Copy present work table into a 3-D stack
C TBSH_LOADB      Load column to master and display
C TBSH_SORTC      Sort identifiers to alphabetical order
C TBSH_SORT       Sort table
C TBSH_STATS      Calculate statistics of a column or two columns
C TBSH_DOSTAT1    Do the calcs for one column
C TBSH_DOSTAT2    Do the calcs for two columns
C TBSH_INSERT     Insert a set of data
C TBSH_HIST       Plot histogram of a column
C TBSH_HISTA      Set up and start histogram
C TBSH_PLOT       Plot two columns
C TBSH_PLOTA      Set up and start plotting two columns
C TBSH_COMPRESS   Remove 'holes' in array
C TBSH_LOADCOL    Load a table column into a 'no-holes' vector
C TBSH_SLOADB     Reverse order of values in a real number vector
C TBSH_TRANS      Load output with sorted input
C TBSH_RENUM      Renumber names
C TBSH_MOVE       Move table location on screen
C TBSH_DEL        Delete row(s)/col(s) from table
C TBSH_ADD        Add row(s)/col(s) to table
C TBSH_ADDR       Add row(s) to a table - part 2
C TBSH_ADDC       Add col(s) to a table - part 2
C TBSH_GETLINE    Input a line of data (with optional name at end)
C TBSH_GETVALS    Input a row of values
C TBSH_UNDDO      Restore last change (if possible)
C TBSH_VALUE      Paint value up
C TBSH_TEXT       Paint text up
C TBSH_NAMCLEAR   Clear a character array
C TBSH_LINE       Plot a line
C TBSH_BOX        Plot a box
C TBSH_CHGET      Get a 20 character string from 5 integers
C TBSH_CHPUT      Get 5 integers from a 20 character string



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBSHEET -- Spread-sheet inspection and modification of a table
C
C         A J Penny            RAL            1991 July

      subroutine t_tbsheet ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      call tbsh_input ( 1 )						!Get input and set up frame

      call tbsh_cwords							!Load control words

      call tbsh_doit 							!Do the work

      call gd_close							!Close sheet


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_SETUP -- Set starting values
C
C   alan penny             RAL               1991 July

      subroutine tbsh_setup ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
      logical mono
Cbegin

      if ( ST_FAILED ) return

      SNX = 1000.0
      SNY = 1000.0
      CHNAMES = 10
      NUMX = 6
      GOTIN = .false.

      KCOL(1) = 2							!Colours
      KCOL(2) = 1
      KCOL(3) = 3
      KCOL(4) = 4
      KCOL(5) = 5
      call get1b ( 'MONO', mono, .false. )
      if ( ST_FAILED ) return
      if ( mono ) then
         KCOL(1) = 1
         call amovki ( 2, KCOL(2), 4 )
      endif
      call tbsh_setcol

      X_OLD = 500.0
      Y_OLD = 500.0


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_SETRC -- Set starting valid row/columns
C
C   alan penny             RAL               1991 July

      subroutine tbsh_setrc ( tbi, tbvxi, tbyi, kcx, kcy, new, tbn,
     +                        tbh )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer   tbvxi			!i: Input table total X size
      integer   tbyi			!i: Input table Y size
      real      tbi(tbvxi,tbyi)		!i: Input table
      integer   kcx(TBXM)		!o: Column checks
      integer   kcy(TBYM)		!o: Row checks
      logical   new			!i: New table, not an input one?
      real      tbn(5,TBYM)		!i/o: Name table
      integer   tbh(5,TBXM)		!i/o: Header table
C--
      character text*20
      integer k, ierr
Cbegin


      if ( ST_FAILED ) return

      call azeroi ( kcx, TBXM )					!Load valid row markers
      call amovki ( 1, kcx, TBX )
      call amovi ( kcx, %val(IPXA), TBXM )

      call azeroi ( kcy, TBYM )					!Load valid column markers
      call amovki ( 1, kcy, TBY )
      call amovi ( kcy, %val(IPYA), TBYM )

      call tbsh_namclear ( tbn, TBYM )				!Clear names and headers
      call tbsh_namclear ( tbh, TBXM )
      call azeror ( %val(IPD), TBXM*TBYM )			!Clear data

      if ( new ) then
         do k = 1, TBY						!If new table make new names
            write ( text, '(i20)' ) k
            text(1:1) = '#'
            call lbgone(text(2:))
            call tbsh_chput ( text, tbn(1,k) )
         enddo
      else
         call coprr ( tbi, tbvxi, tbyi, 6, tbvxi, 1, tbyi,
     +                %val(IPD), TBXM, TBYM, 1, 1 )
         call coprr ( tbi, tbvxi, tbyi, 1, 5, 1, tbyi,
     +                tbn, 5, TBYM, 1, 1 )
         do k = 1, TBX
            call gthead ( 'IN', k, text, ierr )
            call tbsh_chput ( text, tbh(1,k) )
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_FRAME -- Load sheet frame
C
C    alan penny                ral              1991 July

      subroutine tbsh_frame ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
      real xs, xe, ys, ye, x, y
      integer k
Cbegin


      if ( ST_FAILED ) return

      call gd_bbuf

      call tbsh_wmess ( 4, 0 )						!Tell user program is working

      xs = XDOFF							!Sheet box
      xe = xs + real(NUMX)*XBOX
      ys = 10.0
      ye = 10.0 + real(NUMY)*YBOX
      call tbsh_box ( xs, ys, xe, ye, 2 )

      do k = 1, NUMX-1
        x = xs + real(k)*XBOX
        call tbsh_line ( x, ye, x, ys, 2 )
      enddo
      do k = 1, NUMY-1
        y = ys + real(k)*YBOX
        call tbsh_line ( xs, y, xe, y, 2 )
      enddo

      xs = 10.0 							!Names box
      xe = xs + (7.0+1.0+real(CHNAMES))*CHXSIZE
      ys = 10.0
      ye = 10.0 + real(NUMY)*YBOX
      call tbsh_box ( xs, ys, xe, ye, 2 )

      x = xs + 7.0*CHXSIZE
      call tbsh_line ( x, ys, x, ye, 2 )
      do k = 1, NUMY-1
         y = ys + real(k)*YBOX
         call tbsh_line ( xe, y, xs, y, 2 )
      enddo

      xs = XDOFF							!Header box
      xe = xs + real(NUMX)*XBOX
      ys = 10.0 + real(NUMY)*YBOX + 10.0
      ye = ys + 2.0*YBOX
      call tbsh_box ( xs, ys, xe, ye, 2 )

      y = ys + YBOX
      call tbsh_line ( xs, y, xe, y, 2 )
      do k = 1, NUMX-1
         x = xs + real(k)*XBOX
         call tbsh_line ( x, ys, x, ye, 2 )
      enddo

      call gd_ebuf
      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_CWORDS -- Load command words
C
C   alan penny             RAL               1991 July

      subroutine tbsh_cwords ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
      character*12 text(NCWORDS)
      data text /     'Help',       'Input',      'Store',       'Exit',
     +              'Format',     'Command',     'Insert',  'Calculate',
     +                'Weed',    'Renumber',    'Sort Up',  'Sort Down',
     +               'Stats',   'Correlate',       'Plot',  'Histogram',
     +            'Position',        'Left',      'Right',
     +                  'Up',        'Down',  'Left page', 'Right page',
     +             'Up page',   'Down page',   'Add Cols',   'Add Rows',
     +            'Insert Cols',  'Insert Rows',   'Delete Col',
     +         'Delete Cols',
     +          'Delete Row',  'Delete Rows',      'Title',    'Undo' /

      integer j, k, ka
      real x, y, xa, xs, xe, ys, ye
Cbegin


      if ( ST_FAILED ) return

      call gd_bbuf

      xs = 10.0								!Control box
      xe = xs + real(NXCW)*XBOX
      ys = 10.0 + real(NUMY)*YBOX + 10.0 + 2.0*YBOX + 10.0
      ye = ys + real(NYCW)*YBOX
      call tbsh_box ( xs, ys, xe, ye, 5 )

      y = ye
      do k = 1, NYCW-1
         y = y - YBOX
         call tbsh_line ( xs, y, xe, y, 5 )
      enddo
      do k = 1, NXCW-1
         x = 10.0 + real(k)*XBOX
         call tbsh_line ( x, ys, x, ye, 5 )
      enddo

      xa = 10.0 + 0.05*XBOX
      y = 30.0 + real(NUMY+2+NYCW-1)*YBOX + 0.5*CHYSIZE
      ka = 0
      do k = 1, NYCW
         do j = 1, NXCW
            x = xa + (j-1)*XBOX
            ka = ka + 1
            if ( ka.le.NCWORDS ) call tbsh_text (x, y, text(ka), 12, 2)
         enddo
         y = y - YBOX
      enddo

      call gd_ebuf
      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_SETSIZE -- Set sheet format size parameters
C
C    alan penny                ral              1991 July

      subroutine tbsh_setsize

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
      real rv, x1, x2, y1, y2
Cbegin


      if ( ST_FAILED ) return

      NXCW = NUMX + int((7+1+CHNAMES)/10)
      NYCW = 1 + (NCWORDS-1)/NXCW

      CHXSIZE = (SNX-30.0)/(7.0+1.0+real(CHNAMES)+(12.0+1.0)*real(NUMX))

      call pgqvp ( 2, x1, x2, y1, y2 )
      rv = 1.0
      if ( abs(y2-y1).gt.1.0e-6 ) rv = abs((x2-x1)/(y2-y1))
      CHYSIZE = (8.0/5.0)*CHXSIZE*rv

      NUMY = int((SNY-40.0)/(1.4*CHYSIZE)) - 2 - NYCW

      XDOFF = 20.0 + CHXSIZE*(7.0+1.0+real(CHNAMES))

      YBOX = 1.4*CHYSIZE
      XBOX = (12.0+1.0)*CHXSIZE

      CURX = SNX/2.0							!Cursor position
      CURY = SNY/2.0

      rv = CHYSIZE/28.0
      call pgsch ( rv )

      call gd_updt

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_DLOAD -- Load sheet data
C
C    alan penny                ral              1991 July

      subroutine tbsh_dload ( tb, kcx, kcy )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real    tb(TBXM,TBYM)	!i: Table
      integer kcx(TBXM)		!i: X selection
      integer kcy(TBYM)		!i: Y selection
C--
      integer j, k, ktx, kty, kxout, kyout
      logical xmore
      real val, x, y
      character blank*20
      data blank / '                    ' /
Cbegin


      if ( ST_FAILED ) return

      call gd_bbuf

      y = 10.0 + 0.5*CHYSIZE + real(NUMY-1)*YBOX

      kty = KYSTART
      call tbsh_convert ( kty, 2, kcx, kcy )
      kyout = 0
      do while ( kty.le.TBY .and. kyout.lt.NUMY )

         if ( kcy(kty).eq.1 ) then

            x = XDOFF + 0.05*XBOX
            ktx = KXSTART
            call tbsh_convert ( ktx, 1, kcx, kcy )
            ktx = KXSTART
            xmore = .true.
            kxout = 0
            do while ( xmore .and. ktx.le.TBX .and. kxout.lt.NUMX )
               if ( kcx(ktx).eq.1 ) then
                  val = tb(ktx,kty)
                  call tbsh_value ( x, y, val, 12, 2 )
                  x = x + XBOX
                  kxout = kxout + 1
               endif
               ktx = ktx + 1
            enddo

            y = y - YBOX
            kyout = kyout + 1

         endif
         kty = kty + 1

      enddo

      if ( kxout.ne.NUMX ) then						!Blanks in rest of X
         y = 10.0 + 0.5*CHYSIZE + real(NUMY-1)*YBOX
         do k = 1, NUMY
            x = XDOFF + 0.05*XBOX + real(kxout)*XBOX
            do j = kxout+1, NUMX
               call tbsh_text ( x, y, blank, 12, 2 )
               x = x + XBOX
            enddo
            y = y - YBOX
         enddo
      endif

      if ( kyout.ne.NUMY ) then						!Blanks in rest of Y
         y = 10.0 + 0.5*CHYSIZE + real(NUMY-1)*YBOX - real(kyout)*YBOX
         do k = kyout+1, NUMY
            x = XDOFF + 0.05*XBOX
            do j = 1, NUMX
               call tbsh_text ( x, y, blank, 12, 2 )
               x = x + XBOX
            enddo
            y = y - YBOX
         enddo
      endif

      call gd_ebuf
      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_NLOAD -- Load sheet names
C
C    alan penny                ral              1991 July

      subroutine tbsh_nload ( tbn, kcy )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real     tbn(5,TBYM)		!i: Names table
      integer  kcy(TBYM)		!i: Y selection
C--
      integer k, kty, kyout, kn
      real xa, xb, y
      character text*20, blank*20
      data blank / '                    ' /
Cbegin


      if ( ST_FAILED ) return

      call gd_bbuf

      xa = 10.0 + 0.05*XBOX
      xb = xa + 7.0*CHXSIZE
      y = 10.0 + 0.5*CHYSIZE + real(NUMY-1)*YBOX

      kty = KYSTART
      call tbsh_convert ( kty, 2, %val(IPX), kcy )
      kyout = 0
      do while ( kty.le.TBY .and. kyout.lt.NUMY )

         if ( kcy(kty).eq.1 ) then
            kn = KYSTART + kyout
            write ( text, '(i6)' ) kn
            call tbsh_text ( xa, y, text, 6, 2 )
            call tbsh_chget ( tbn(1,kty), text )
            call tbsh_text ( xb, y, text(1:CHNAMES), CHNAMES, 2 )
            y = y - YBOX
            kyout = kyout + 1
         endif
         kty = kty + 1

      enddo

      if ( kyout.ne.NUMY ) then						!Blank rest of Y
         y = 10.0 + 0.5*CHYSIZE + real(NUMY-1)*YBOX - real(kyout)*YBOX
         do k = kyout+1, NUMY
            call tbsh_text ( xa, y, blank, 6, 2 )
            call tbsh_text ( xb, y, blank, CHNAMES, 2 )
            y = y - YBOX
         enddo
      endif

      call gd_ebuf
      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_HLOAD -- Load sheet headers
C
C    alan penny                ral              1991 July

      subroutine tbsh_hload ( tbh, kcx )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer      tbh(5,TBXM)		!i: Headers table
      integer      kcx(TBXM)		!i: X selection
C--
      integer k, ktx, kxout, kn
      real x, ya, yb
      character text*20, texta*20, blank*10
      data blank / '          ' /

Cbegin


      if ( ST_FAILED ) return

      call gd_bbuf

      x = XDOFF + 0.05*XBOX
      ya = 20.0 + 0.5*CHYSIZE + real(NUMY)*YBOX
      yb = ya + YBOX

      ktx = KXSTART
      call tbsh_convert ( ktx, 1, kcx, %val(IPY) )
      kxout = 0
      do while ( ktx.le.TBX .and. kxout.lt.NUMX )

         if ( kcx(ktx).eq.1 ) then
            kn = KXSTART + kxout
            write ( text, '(i6)' ) kn
            call tbsh_text ( x, yb, text, 10, 2 )
            call tbsh_chget ( tbh(1,ktx), texta )
            call tbsh_text ( x, ya, texta, 10, 2 )
            x = x + XBOX
            kxout = kxout + 1
         endif
         ktx = ktx + 1

      enddo

      if ( kxout.ne.NUMX ) then						!Blank rest of X
         x = XDOFF + 0.05*XBOX + real(kxout)*XBOX
         do k = kxout+1, NUMX
            call tbsh_text ( x, yb, blank, 10, 2 )
            call tbsh_text ( x, ya, blank, 10, 2 )
            x = x + XBOX
         enddo
      endif

      call gd_ebuf
      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_DOIT -- Do the work
C
C    alan penny           ral                       1991 July

      subroutine tbsh_doit ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
      integer kopt, kdef
      logical loop, tlog
Cbegin


      if ( ST_FAILED ) return

      kopt = 18								!Start choice

      loop = .true.
      do while ( loop )

         kdef = kopt							!Get option
         call tbsh_getopt ( kopt, kdef )

         if ( kopt.eq.1 ) call tbsh_help				!Help option

         if ( kopt.eq.2 ) call tbsh_input ( 2 )				!New table

         if ( kopt.eq.3 ) call tbsh_store 				!Store output

         if ( kopt.eq.4 ) call tbsh_exit ( loop )			!Exit from program

         if ( kopt.eq.5 ) call tbsh_format				!Change format/colour

         if ( kopt.eq.6 ) then						!Command option
                          call printo ( ' ' )
                          call printo ( 'Type ? for help: '//
     +                     '!! for exit: <return> for return to sheet')
                          call get1b ( 'COMMAND', tlog, .false. )
                          endif

         if ( kopt.eq.7 ) call tbsh_insert ( %val(IPD) )		!Insert a set of data

         if ( kopt.eq.8 ) call tbsh_calc 				!Calculate a column

         if ( kopt.eq.9 ) call tbsh_weed				!Weed on column

         if ( kopt.eq.10 ) call tbsh_renum ( %val(IPN), %val(IPY) )	!Renumber names

         if ( kopt.eq.11 .or. kopt.eq.12 ) call tbsh_sort ( kopt, 10 )	!Sort on column

         if ( kopt.eq.13 .or. kopt.eq.14 ) call tbsh_stats ( %val(IPH),	!Calc stats of a column or two columns
     +                                                       kopt, 12 )

         if ( kopt.eq.15 ) call tbsh_plot ( %val(IPH) )			!Plot two columns

         if ( kopt.eq.16 ) call tbsh_hist ( %val(IPH) )			!Plot histogram of a column

         if ( kopt.ge.17 .and. kopt.le.25 ) call tbsh_move ( kopt, 16 )	!Change position in screen

         if ( kopt.ge.26 .and. kopt.le.29 ) call tbsh_add ( kopt, 25 )	!Add/insert row(s)/col(s)

         if ( kopt.ge.30 .and. kopt.le.33 ) call tbsh_del ( kopt, 	!Delete row(s)/col(s)
     +                                      %val(IPX), %val(IPY), 29 )

         if ( kopt.eq.34 ) call get1c ( 'TITLE', TITLE, TITLE, .true. )	!Change title

         if ( kopt.eq.35 ) call tbsh_undo				!Reverse last change

         if ( kopt.eq.(NCWORDS+1) ) call tbsh_change ( %val(IPD), 	!Change name, header, or value
     +                                         %val(IPH), %val(IPN) )

         if ( ST_FAILED ) return

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_EXIT -- Exit if really wanted
C
C    alan penny           ral                       1991 July

      subroutine tbsh_exit ( loop )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      logical loop		!i/o: Continue looping for options?
C--
      logical ok
Cbegin


      if ( ST_FAILED ) return

      if ( CHANGED ) then
         call printo ( 'WARNING: You have not'//
     +                 ' stored since the last change.' )
         call printo ( ' Is this ok?' )
         call get1b ( 'OK', ok, .false. )
         if ( ST_FAILED ) return
         if ( ok ) loop = .false.
      else
         loop = .false.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_GETOPT -- Get option
C
C    alan penny           ral                       1991 July

      subroutine tbsh_getopt ( kopt, kdef )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer    kopt		!o: Option number
      integer    kdef		!i: Default option number
C--
      real x, y, xa, ya, xb, yb, xs, xe, ys, ye
      character*1 ch
      integer ierr, j, k, ka
      logical found
      integer pgcurse
      external pgcurse
Cbegin


      if ( ST_FAILED ) return

CXA      if ( kdef.ne.29 ) then
CXA         x = 10.0 + 0.05*XBOX
CXA         y = 30.0 + real(NUMY+2+NYCW-1)*YBOX + 0.05*YBOX
CXA         k = (kdef-1)/NXCW
CXA         j = kdef - k*NXCW
CXA         x = x + real(j)*XBOX + XBOX/2.0
CXA         y = y - real(k)*YBOX + YBOX/2.0
CXA      endif

      x = X_OLD								!Put cursor at last request
      y = Y_OLD
      call tbsh_wmess ( 1, 0 )						!Put up waiting message

      found = .false.							!Find where cursor placed
      do while ( .not.found )

         ierr = pgcurse ( x, y, ch )
         X_OLD = x
         Y_OLD = y

         yb = 30.0 + real(NUMY+2+NYCW-1)*YBOX + 0.05*YBOX		!Look in commands
         ka = 0
         do k = 1, NYCW
            ya = yb - real(k-1)*YBOX
            xb = 10.0 + 0.05*XBOX
            do j = 1, NXCW
               xa = xb + real(j-1)*XBOX
               ka = ka + 1
               if ( ka.le.NCWORDS ) then
                  xs = xa
                  ys = ya
                  xe = xs + 0.9*XBOX
                  ye = ys + 0.9*YBOX
                  if ( x.ge.xs .and. x.le.xe .and.
     +                 y.ge.ys .and. y.le.ye ) then
                     found = .true.
                     kopt = ka
                  endif
               endif
            enddo
         enddo

         if ( .not.found ) then				!Look in contents
            call tbsh_seepos ( x, y, found )
            if ( found ) kopt = NCWORDS + 1
         endif

      enddo

      call tbsh_wmess ( 2, kopt )

      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_WMESS -- Put up waiting message
C
C    alan penny           ral                       1991 July

      subroutine tbsh_wmess ( kf, kopt )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer    kf		!i: Message flag (1=for choice;
				!          2=work/for loc;3=for key;4=work)
      integer    kopt		!i: Option doing
C--
      integer nc, k, ka
      real xc, yc
      integer kopta(NCWORDS+1)
      data kopta / 3, 3, 2, 3, 3,
     +             3, 2, 3, 3, 4,
     +             2, 2, 2, 2, 2,
     +             2, 2, 4, 4, 4,
     +             4, 4, 4, 4, 4,
     +             2, 2, 2, 2, 2,
     +             3, 2, 3, 3, 3,
     +             3 /
      character*18 text(4)
      data text / 'Waiting (Option)  ', 'Waiting (Position)',
     +            'Waiting (Keyboard)', 'Working           ' /
Cbegin


      if ( ST_FAILED ) return

      nc = min(18,(8+CHNAMES))
      xc = 10.0 + 0.05*XBOX
      yc= 20.0 + real(NUMY)*YBOX + 0.75*YBOX

      k = kf
      if ( kf.eq.2 ) then
         k = 2
         if ( kopt.ne.0 ) k = kopta(kopt)
      endif

      ka = 3
      if ( k.eq.4 ) ka = 4
      call tbsh_text ( xc, yc, text(k), nc, ka )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_SEEPOS -- Get column, row of given position
C
C    alan penny           ral                       1991 July

      subroutine tbsh_seepos ( x, y, found )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real      x		!i: X posn
      real      y		!i: Y posn
      logical   found		!o: In reasonable box?
C--
      real xs, xe, ys, ye, xsa, xea, ysa, yea
      integer k
Cbegin


      if ( ST_FAILED ) return

      KNCOL = -1							!Defaults
      KNROW = -1

      ys = 10.0 + 0.05*YBOX						!Outer limits
      ye = 20.0 + (real(NUMY+2)-0.05)*YBOX				!Look for column
      xs = 10.0 + 7.0*CHXSIZE + 0.05*XBOX
      xe = XDOFF + (real(NUMX)-0.05)*XBOX
      if ( x.ge.xs .and. x.le.xe .and. y.ge.ys .and. y.le.ye ) then

         do k = 1, NUMX
            xsa = XDOFF + (real(k-1)+0.05)*XBOX
            xea = xsa + 0.9*XBOX
            if ( x.ge.xsa .and. x.le.xea ) KNCOL = k
         enddo

         do k = 1, NUMY
            ysa = 10.0 + (real(NUMY-k)+0.05)*YBOX
            yea = ysa + 0.9*YBOX
            if ( y.ge.ysa .and. y.le.yea ) KNROW = k
         enddo

         xsa = 10.0 + 7.0*CHXSIZE + 0.05*XBOX
         xea = XDOFF - 10.0 - 0.05*XBOX
         if ( x.ge.xsa .and. x.le.xea ) KNCOL = 0

         ysa = 20.0 + (real(NUMY)+0.05)*YBOX
         yea = ysa + 0.9*YBOX
         if ( y.ge.ysa .and. y.le.yea ) KNROW = 0

      endif

      if ( KNCOL.eq.0 .and. KNROW.eq.0 ) then
         KNCOL = -1
         KNROW = -1
      endif

      if ( KNCOL.gt.0 .and. KNROW.gt.0 ) then
         KNCOL = KNCOL + KXSTART - 1
         KNROW = KNROW + KYSTART - 1
      elseif ( KNCOL.gt.0 ) then
         KNCOL = KNCOL + KXSTART - 1
      elseif ( KNROW.gt.0 ) then
         KNROW = KNROW + KYSTART - 1
      endif

      if ( KNCOL.gt.0 ) call tbsh_convert ( KNCOL, 1, %val(IPX),	!Convert input number to 'real'
     +                                      %val(IPY) )
      if ( KNROW.gt.0 ) call tbsh_convert ( KNROW, 2, %val(IPX),
     +                                      %val(IPY) )

      if ( KNCOL.ge.0 .or. KNROW.ge.0 ) found = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_GETPOS -- Get column, row of actual holes table
C
C    alan penny           ral                       1991 July

      subroutine tbsh_getpos ( kopt, ncol, nrow )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer    kopt		!i: Flag for choice (1=column;2=row;3=specific)
      integer    ncol		!o: Column chosen (actual holes table number)
				!         (-1=not found;0=in header)
      integer    nrow		!o: Row chosen (actual holes table number)
				!         (-1=not found;0=in name)
C--
      real x, y, xs, xe, ys, ye, xsa, xea, ysa, yea
      character*1 ch
      integer ierr, k
      logical foundr, foundc
      integer pgcurse
      external pgcurse
Cbegin


      if ( ST_FAILED ) return

      ncol = -1								!Defaults
      nrow = -1
      foundc = .false.
      foundr = .false.

      ys = 10.0 + 0.05*YBOX						!Outer limits
      if ( kopt.eq.1 ) then
         ye = 20.0 + (real(NUMY+2)-0.05)*YBOX
         xs = XDOFF + 0.05*XBOX
         xe = XDOFF + (real(NUMX)-0.05)*XBOX
      elseif ( kopt.eq.2 ) then
         ye = 10.0 + (real(NUMY)-0.05)*YBOX
         xs = 10.0 + 0.05*XBOX
         xe = XDOFF + (real(NUMX)-0.05)*XBOX
      else
         ye = 10.0 + (real(NUMY+1)-0.05)*YBOX
         xs = 10.0 + 7.0*CHXSIZE + 0.05*XBOX
         xe = XDOFF + (real(NUMX)-0.05)*XBOX
      endif

      call tbsh_wmess ( 2, 0 )
      x = X_OLD
      y = Y_OLD
      ierr = pgcurse ( x, y, ch )					!Find where cursor placed
      X_OLD = x
      Y_OLD = y
      call tbsh_wmess ( 4, 0 )

      if ( x.ge.xs .and. x.le.xe .and. y.ge.ys .and. y.le.ye ) then

         if ( kopt.eq.1 .or. kopt.eq.3 ) then
            do k = 1, NUMX
               xsa = XDOFF + (real(k-1)+0.05)*XBOX
               xea = xsa + 0.9*XBOX
               if ( x.ge.xsa .and. x.le.xea ) then
                  ncol = k
                  foundc = .true.
               endif
            enddo
         endif

         if ( kopt.eq.2 .or. kopt.eq.3 ) then
            do k = 1, NUMY
               ysa = 10.0 + (real(NUMY-k)+0.05)*YBOX
               yea = ysa + 0.9*YBOX
               if ( y.ge.ysa .and. y.le.yea ) then
                  nrow = k
                  foundr = .true.
               endif
            enddo
         endif

         if ( kopt.eq.3 ) then
            xsa = 10.0 + 7.0*CHXSIZE + 0.05*XBOX
            xea = XDOFF - 10.0 - 0.05*XBOX
            if ( x.ge.xsa .and. x.le.xea ) then
               ncol = 0
               foundc = .true.
            endif
            ysa = 20.0 + (real(NUMY)+0.05)*YBOX
            yea = ysa + 0.9*YBOX
            if ( y.ge.ysa .and. y.le.yea ) then
               nrow = 0
               foundr = .true.
            endif
         endif

      endif

      if ( ( kopt.eq.1 .and. .not.foundc ) .or.
     +     ( kopt.eq.2 .and. .not.foundr )  .or.
     +     ( kopt.eq.3 .and. (.not.foundr .or. .not.foundc)) ) then
         call printo ( 'Bad cursor position - Start again' )
         ncol = -1
         nrow = -1
      endif

      if ( ncol.gt.0 .and. nrow.gt.0 ) then
         ncol = ncol + KXSTART - 1
         nrow = nrow + KYSTART - 1
      elseif ( ncol.gt.0 ) then
         ncol = ncol + KXSTART - 1
      elseif ( nrow.gt.0 ) then
         nrow = nrow + KYSTART - 1
      endif

      if ( ncol.gt.0 ) call tbsh_convert (ncol, 1, %val(IPX),%val(IPY))	!Convert input number
      if ( nrow.gt.0 ) call tbsh_convert (nrow, 2, %val(IPX),%val(IPY))	! actual holes

      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_HELP -- Output help
C
C    alan penny           ral                       1991 July

      subroutine tbsh_help ( )

      implicit none
      include 'STARMAN_INC'
C--
      integer j, k
      logical more

      integer nt
      parameter ( nt=147 )
      character*74 text(nt)
      data (text(j),j=1,10) /
     + '           ' ,
     + '           ' ,
     + ' TBSHEET is a spread-sheet program for the tables.' ,
     + ' For a full help, go back to the OPTIONS and select' ,
     + ' the COMMAND, and in response to the COMMAND request' ,
     + ' type ?' ,
     + '           ' ,
     + ' After getting the input table, you may add up to 200' ,
     + ' columns and up to 2000 rows, as well as performing' ,
     + ' various manipulations of the table.' /
      data (text(j),j=11,20) /
     + '                    ' ,
     + '           ' ,
     + ' Numbers in the table are changed by clicking on them, ' ,
     + ' and then using the keyboard to replace the values. The ' ,
     + ' same goes for the row names and the column headers. ',
     + '           ' ,
     + ' For the other options, click on the menu panel displayed',
     + ' above the table. Often this will then involve choosing a',
     + ' row or column. This is done by then clicking on wanted ',
     + ' column or row.' /
      data (text(j),j=21,30) /
     + '   ',
     + '   ' ,
     + ' Menu of options available:-' ,
     + '       ' ,
     + ' Name          Function ',
     + ' ----          -------- ' ,
     + ' Add Cols      Add columns to the table. A column (or columns)',
     + '                is added to the data, located after the ' ,
     + '                column picked by the cursor. See below for ' ,
     + '                data entry.' /
      data (text(j),j=31,40) /
     + ' Add Rows      Add rows to the table. A row (or rows)' ,
     + '                is added to the data, located after the ' ,
     + '                row picked by the cursor. See below for ' ,
     + '                data entry.' ,
     + ' Calculate     Calculate new entries for a column.' ,
     + ' Command       Access the command line for help or exit.' ,
     + ' Correlate     Calculate the statistics of two columns',
     + '                (picked by the cursor), first of themselves',
     + '                then of the correlation between them.',
     + ' Delete Col    Delete a column, picked by the cursor' /
      data (text(j),j=41,50) /
     + ' Delete Cols   Delete a number of contiguous columns, defined',
     + '                by user input' ,
     + ' Delete Row    Delete a row, picked by the cursor' ,
     + ' Delete Rows   Delete a number of contiguous rows, defined' ,
     + '                by user input' ,
     + ' Down          Move the displayed table so that the number' ,
     + '                of the row displayed at the top increases' ,
     + '                by one.' ,
     + ' Down page     Move the displayed table so that the number' ,
     + '                of the row displayed at the top increases' /
      data (text(j),j=51,60) /
     + '                by the number of rows seen.' ,
     + ' Exit          Exit from the programme.' ,
     + ' Format        Change the format and colour of the display.' ,
     + ' Help          Type out this help information.' ,
     + ' Histogram     Histogram of a column is plotted by picking a',
     + '                column with a cursor. The sheet is then',
     + '                replaced by the histogram. When you wish',
     + '                to return to the sheet, just answer question.',
     + ' Input         Put up a new input table.' ,
     + ' Insert        Insert data into table. Select a range of rows'/
       data (text(j),j=61,70) /
     + '                of columns by the cursor (t.l.h and b.r.h',
     + '                corners). Keyboard enter data one at a time.',
     + ' Insert Cols   Add columns to the table. A column (or columns)',
     + '                is added to the data, located before the ' ,
     + '                column picked by the cursor. See below for ' ,
     + '                data entry,' ,
     + ' Insert Rows   Add rows to the table. A row (or rows)' ,
     + '                is added to the data, located before the ',
     + '                row picked by the cursor. See below for ' ,
     + '                data entry,' /
       data (text(j),j=71,80) /
     + ' Left          Move the displayed table so that the number' ,
     + '                of the column displayed at the left increases' ,
     + '                by one.' ,
     + ' Left Page     Move the displayed table so that the number' ,
     + '                of the column displayed at the left increases' ,
     + '                by the number  of columns seen.' ,
     + ' Plot          One column can be plotted against another, both',
     + '                picked by cursor. Double click gives column ',
     + '                choice by keyboard. The sheet is then',
     + '                replaced by the plot. When you wish' /
       data (text(j),j=81,90) /
     + '                to return to the sheet, just answer question.',
     + ' Position      Change the column/row shown at the top left',
     + '                of the display to that demanded.' ,
     + ' Renumber      Load the names of all the rows with numbers' ,
     + '                in the format #n, where n is the number of ' ,
     + '                the row.' ,
     + ' Right         Move the displayed table so that the number' ,
     + '                of the column displayed at the left decreases' ,
     + '                by one.',
     + ' Right page    Move the displayed table so that the number' /
       data (text(j),j=91,100) /
     + '                of the column displayed at the left decreases',
     + '                by the number  of columns seen.' ,
     + ' Sort Down     Sort the rows of the table by looking at the' ,
     + '                values of entries in a column (picked by the' ,
     + '                the cursor), and sorting so these are in' ,
     + '                descending order.' ,
     + ' Sort Up       Sort the rows of the table by looking at the' ,
     + '                values of entries in a column (picked by the' ,
     + '                the cursor), and sorting so these are in',
     + '                ascending order.' /
       data (text(j),j=101,110) /
     + ' Stats         Calculate the statistics of one or two columns',
     + '                (picked by the cursor). Double click gives' ,
     + '                column choice by keyboard' ,
     + ' Store         Store the present table in an output file' ,
     + ' Title         Change the title of the table' ,
     + ' Undo          Undo the last deletion or weed, if no sub-' ,
     + '                sequent changes have been made in the table.' ,
     + ' Up            Move the displayed table so that the number' ,
     + '                of the row displayed at the top decreases' ,
     + '                by one.' /
       data (text(j),j=111,120) /
     + ' Up page       Move the displayed table so that the number' ,
     + '                of the row displayed at the top decreases' ,
     + '                by the number of rows seen.' ,
     + ' Weed          Delete rows of the table by looking at the' ,
     + '                values of entries in a column (picked by the',
     + '                the cursor), and seeing if they lie within' ,
     + '                (or without) a chosen range.' ,
     + '                      ' ,
     + '                      ' ,
     + ' Cursor Use' /
       data (text(j),j=121,130) /
     + ' ----------' ,
     + '           ' ,
     + ' Place the cursor on the desired location and press any key' ,
     + ' on the keyboard - or maybe the cursor button.' ,
     + '                      ' ,
     + ' Row Data Entry' ,
     + ' ---------------------' ,
     + '                      ' ,
     + ' Entering data for a row is done by typing in values for' ,
     + ' the entries in the row, separated by commas. If you do not ' /
       data (text(j),j=131,140) /
     + ' enter all the values, the rest are given the value of zero.',
     + ' If you enter all the row values, you may add the name. The',
     + ' name must have no spaces (you may wish to use _ for a space)',
     + ' If you do not input a name, the row is given the name #n ' ,
     + ' where n is the number of the row. You are then asked for the',
     + ' values for the next row to be entered. A completely null ',
     + ' response ends the inputting of data.',
     + '                      ' ,
     + ' Column Data Entry' ,
     + ' ---------------------' /
       data (text(j),j=141,nt) /
     + '                      ' ,
     + ' Entering data for columns is done by typing in values for' ,
     + ' the entries in the first row of each column, separated by',
     + ' commas. The number of entries you put in will be taken as',
     + ' the number of columns to put into the table. All the rows',
     + ' in each column are then given the value of that in the their',
     + ' first row. The column header is set as a blank space.' /
Cbegin


      if ( ST_FAILED ) return

      k = 0
      more = .true.
      do while ( more .and. k.lt.nt )
         k = k + 1
         call printo ( text(k) )
         j = k - (k/20)*20
         if ( j.eq.0 ) call get1b ( 'MORE', more, .true. )
         if ( ST_FAILED ) return
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_INPUT -- Get new input
C
C    alan penny           ral                       1991 July

      subroutine tbsh_input ( kopt )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer    kopt		!i: First input (1), or not (2)?
C--
      integer ierr, tbvxi, tbyi, tbxi, ipi, iv
      logical new
Cbegin


      if ( ST_FAILED ) return

      if ( kopt.eq.2 ) then						!Forget any old table
         call tbsh_wmess ( 4, 0 )					!Tell user program is working
         call tbsh_blank						!Blank present display
      endif

      if ( kopt.eq.2 ) call tbsh_wmess ( 3, 0 )				!Tell user program is waiting
      if ( kopt.eq.2 ) call canpar ( 'IN' )
      call optabr ( 'IN', ipi, tbvxi, tbyi, .true., ierr )		!Get input table
      if ( ierr.eq.1 ) then						!Bad input
          GOTIN = .false.
          return
      elseif ( ierr.eq.2 ) then						!Null input, make new table
         tbxi = 2
         tbyi = 100
         new = .true.
         GOTIN = .false.
         ipi = 1
         call get2i ( 'SIZE', tbxi, tbyi, .true., 1, 10000 )
         if ( ST_FAILED ) return
         tbvxi = tbxi + 5
         call get1c ( 'TITLE', TITLE, 'Output from TBSHEET', .true. )
         if ( ST_FAILED ) return
      else								!Good input
         tbxi = tbvxi - 5
         new = .false.
         GOTIN = .true.
         call gtdesc ( 'IN', 'TITLE', TITLE, ' ', iv, ierr )
      endif
      if ( kopt.eq.2 ) call tbsh_wmess ( 4, 0 )				!Tell user program is working

      TBX = tbxi 							!Define work arrays
      TBY = tbyi
      TBXR = TBX
      TBYR = TBY
      TBXM = tbxi + 200
      TBYM = tbyi + 2000

      if ( kopt.eq.2) then
         call wrkcan ( 'WORKD' )
         call wrkcan ( 'WORKN' )
         call wrkcan ( 'WORKH' )
         call wrkcan ( 'WORKX' )
         call wrkcan ( 'WORKY' )
         call wrkcan ( 'WORKXA' )
         call wrkcan ( 'WORKYA' )
      endif

      									!Get work spaces
      call gtwrkr ( 'WORKD', TBXM*TBYM, IPD, ierr )			!Data
      if ( ierr.ne.0 ) ST_FAILED = .true.
      call gtwrki ( 'WORKN', 5*TBYM, IPN, ierr )			!Names
      if ( ierr.ne.0 ) ST_FAILED = .true.
      call gtwrki ( 'WORKH', 5*TBXM, IPH, ierr )			!Headers
      if ( ierr.ne.0 ) ST_FAILED = .true.
      call gtwrki ( 'WORKX', TBXM, IPX, ierr )				!X usage
      if ( ierr.ne.0 ) ST_FAILED = .true.
      call gtwrki ( 'WORKY', TBYM, IPY, ierr )				!Y usage
      if ( ierr.ne.0 ) ST_FAILED = .true.
      call gtwrki ( 'WORKXA', TBXM, IPXA, ierr )			!X usage backup
      if ( ierr.ne.0 ) ST_FAILED = .true.
      call gtwrki ( 'WORKYA', TBYM, IPYA, ierr )			!Y usage backup
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      KXSTART = 1							!Starting display position
      KYSTART = 1
      CHANGED = .false.
      RESTORE = .true.

      if ( kopt.eq.1 ) then						!Open display
         call gd_open ( ierr )
         if ( ierr.ne.0 ) then
            ST_FAILED = .true.
            return
         endif
         call pgvport ( 0.0, 1.0, 0.0, 1.0 )
         call pgwindow ( 1.0, 1000.0, 1.0, 1000.0 )
         call tbsh_setup						!Set up spread sheet
         call tbsh_setsize						!Get sheet size
         call tbsh_frame						!Write sheet frame
      endif

      call tbsh_setrc ( %val(ipi), tbvxi, tbyi, %val(IPX), %val(IPY),	!Load arrays
     +                  new, %val(IPN), %val(IPH) )

      call tbsh_dload ( %val(IPD), %val(IPX), %val(IPY) )		!Load numbers

      call tbsh_nload ( %val(IPN), %val(IPY) )				!Load names

      call tbsh_hload ( %val(IPH), %val(IPX) )				!Load headers

      call gd_updt

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_BLANK -- Blank present display
C
C    alan penny           ral                       1991 July

      subroutine tbsh_blank ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
      integer j, k
      real x, y, xa, xb, ya, yb
      character blank*20
      data blank / '                    ' /
Cbegin


      if ( ST_FAILED ) return

      call gd_bbuf

      y = 10.0 + 0.5*CHYSIZE + real(NUMY-1)*YBOX			!Blank data
      do k = 1, NUMY
         x = XDOFF + 0.05*XBOX
         do j = 1, NUMX
            call tbsh_text ( x, y, blank, 12, 2 )
            x = x + XBOX
         enddo
         y = y - YBOX
      enddo

      xa = 10.0 + 0.05*XBOX						!Blank Names
      xb = xa + 7.0*CHXSIZE
      y = 10.0 + 0.5*CHYSIZE + real(NUMY-1)*YBOX
      do k = 1, NUMY
         call tbsh_text ( xa, y, blank, 6, 2 )
         call tbsh_text ( xb, y, blank, CHNAMES, 2 )
         y = y - YBOX
      enddo

      x = XDOFF + 0.05*XBOX 						!Blank Headers
      ya = 20.0 + 0.5*CHYSIZE + real(NUMY)*YBOX
      yb = ya + YBOX
      do k = 1, NUMX
         call tbsh_text ( x, yb, blank, 10, 2 )
         call tbsh_text ( x, ya, blank, 10, 2 )
         x = x + XBOX
      enddo

      call gd_ebuf
      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_STORE -- Store output (part 1)
C
C    alan penny           ral                       1991 July

      subroutine tbsh_store ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
      integer ipo, ierr
      character*50 atitle
Cbegin


      if ( ST_FAILED ) return

      call optabw ( 'OUT', ipo, TBXR+5, TBYR, .true., ierr )
      if ( ierr.ne.0 ) return

      call tbsh_wmess ( 4, 0 )

      if ( GOTIN ) then
         call tcopdes ( 'IN', 'OUT', ierr )
         if ( ierr.ne.0 ) return
      endif
      call get1c ( 'TITLE', atitle, TITLE, .true. )
      call ptdesc ( 'OUT', 'TITLE', atitle )

      call tbsh_storea ( %val(IPD), %val(IPH), %val(IPN), %val(IPX),
     +                   %val(IPY), %val(ipo) )

      call canpar ( 'OUT' )
      CHANGED = .false.


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_STOREA -- Store output (part 2)
C
C    alan penny           ral                       1991 July

      subroutine tbsh_storea ( tb, tbh, tbn, kcx, kcy, tbo )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real      tb(TBXM,TBYM)		!i: Data table
      integer   tbh(5,TBXM)		!i: Headers table
      real      tbn(5,TBYM)		!i: Names table
      integer   kcx(TBXM)		!i: Column selection list
      integer   kcy(TBYM)		!i: Row selection list
      real      tbo(TBXR+5,TBYR)	!o: Output file
C--
      integer j, k, ka, ja, ierr
      character*20 thead
Cbegin


      if ( ST_FAILED ) return

      ka = 0
      do k = 1, TBY
         if ( kcy(k).eq.1 ) then
            ka = ka + 1
            call amovr ( tbn(1,k), tbo(1,ka), 5 )
            ja = 0
            do j = 1, TBX
               if ( kcx(j).eq.1 ) then
                  ja = ja + 1
                  tbo(ja+5,ka) = tb(j,k)
               endif
            enddo
         endif
      enddo

      ja = 0
      do j = 1, TBX
         if ( kcx(j).eq.1 ) then
            ja = ja + 1
            call tbsh_chget ( tbh(1,j), thead )
            call pthead ( 'OUT', ja, thead, ierr )
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_CHANGE -- Change data, name, header in table
C
C    alan penny           ral                       1991 July

      subroutine tbsh_change ( tb, tbh, tbn )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real      tb(TBXM,TBYM)	!i/o: Data table
      integer   tbh(5,TBXM)	!i/o: Headers table
      real      tbn(5,TBXM)	!i/o: Names table
C--
      integer ncol, nrow, kx, ky
      character*20 text, texta
      real rv, rva, x, y
Cbegin


      if ( ST_FAILED ) return

      ncol = KNCOL
      nrow = KNROW

      if ( ncol.lt.0 .or. nrow.lt.0 ) return				!Not a change

      if ( ncol.eq.0 ) then						!Name change
         call tbsh_chget ( tbn(1,nrow), text )
         call get1c ( 'NAME', texta, text, .true. )
         if ( ST_FAILED ) return
         call tbsh_chput ( texta, tbn(1,nrow) )
         x = 10.0 + 0.05*XBOX + 7.0*CHXSIZE
         ky = nrow
         call tbsh_bconvert ( ky, 2, %val(IPX), %val(IPY) )
         y = 10.0 + 0.5*CHYSIZE + real(NUMY-1-ky+KYSTART)*YBOX
         call gd_bbuf
         call tbsh_text ( x, y, texta(1:CHNAMES), CHNAMES, 2 )
         call gd_ebuf
         call gd_updt
      elseif ( nrow.eq.0 ) then						!Header change
         call tbsh_chget ( tbh(1,ncol), text )
         call get1c ( 'HEADER', texta, text, .true. )
         if ( ST_FAILED ) return
         call tbsh_chput ( texta, tbh(1,ncol) )
         x = XDOFF + 0.05*XBOX
         y = 20.0 + 0.5*CHYSIZE + real(NUMY)*YBOX
         kx = ncol
         call tbsh_bconvert ( kx, 1, %val(IPX), %val(IPY) )
         x = x + real(kx-KXSTART)*XBOX
         call gd_bbuf
         call tbsh_text ( x, y, texta(1:10), 10, 2 )
         call gd_ebuf
         call gd_updt
      else								!Value change
         rv = tb(ncol,nrow)
         call get1r ( 'VALUE', rva, rv, -1.0e20, 1.0e20 )
         if ( ST_FAILED ) return
         tb(ncol,nrow) = rva
         x = XDOFF + 0.05*XBOX
         y = 10.0 + 0.5*CHYSIZE + real(NUMY-1)*YBOX
         kx = ncol
         call tbsh_bconvert ( kx, 1, %val(IPX), %val(IPY) )
         x = x + real(kx-KXSTART)*XBOX
         ky = nrow
         call tbsh_bconvert ( ky, 2, %val(IPX), %val(IPY) )
         y = y - real(ky-KYSTART)*YBOX
         call gd_bbuf
         call tbsh_value ( x, y, rva, 10, 2 )
         call gd_ebuf
         call gd_updt
      endif

      call tbsh_wmess ( 4, 0 )

      CHANGED = .true.
      RESTORE  = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_FORMAT -- Select and change format and colours of sheet
C
C    alan penny           ral                       1991 July

      subroutine tbsh_format ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
      logical mono
      integer iva, ivb, ivc, ivd, ive
Cbegin


      if ( ST_FAILED ) return

      call get1i ( 'CHNAMES', CHNAMES, CHNAMES, 2, 20 )
      if ( ST_FAILED ) return
      call get1i ( 'COLNUM', NUMX, NUMX, 2, 100 )
      if ( ST_FAILED ) return

      call get1b ( 'MONO', mono, .false. )				!Set colour
      if ( ST_FAILED ) return
      if ( mono ) then
         KCOL(1) = 1
         call amovki ( 2, KCOL(2), 4 )
      else
         iva = KCOL(1)
         ivb = KCOL(2)
         ivc = KCOL(3)
         ivd = KCOL(4)
         ive = KCOL(5)
         call get5i ( 'COLOURS', iva, ivb, ivc, ivd, ive, .true.,1,10)
         if ( ST_FAILED ) return
         KCOL(1) = iva
         KCOL(2) = ivb
         KCOL(3) = ivc
         KCOL(4) = ivd
         KCOL(5) = ive
      endif

      call tbsh_formload


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_FORMLOAD -- Load format of sheet
C
C    alan penny           ral                       1991 July

      subroutine tbsh_formload ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
      real xa(5), ya(5)
      data xa, ya / 1.0,    1.0, 1000.0, 1000.0, 1.0,
     +              1.0, 1000.0, 1000.0,    1.0, 1.0 /
Cbegin


      if ( ST_FAILED ) return

      call gd_bbuf

      call tbsh_setsize
      call pgsci ( 0 )
      call pgpoly ( 5, xa, ya )
      call gd_updt

      call tbsh_frame							!Write sheet frame

      call tbsh_dload ( %val(IPD), %val(IPX), %val(IPY) )		!Load numbers

      call tbsh_nload ( %val(IPN), %val(IPY) )				!Load names

      call tbsh_hload ( %val(IPH), %val(IPX) )				!Load headers

      call tbsh_cwords							!Load control words

      call tbsh_setcol

      call tbsh_wmess ( 4, 0 )

      call gd_ebuf
      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_SETCOL -- Change colours of sheet
C
C    alan penny           ral                       1991 July

      subroutine tbsh_setcol ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
      integer j, k
      real r(10), g(10), b(10)
      data r / 0.0, 1.0, 1.0, 0.0, 0.0,
     +         0.0, 1.0, 1.0, 1.0, 0.5 /
      data g / 0.0, 1.0, 0.0, 1.0, 0.0,
     +         1.0, 1.0, 1.0, 0.5, 1.0 /
      data b / 0.0, 1.0, 0.0, 0.0, 1.0,
     +         1.0, 1.0, 0.0, 0.0, 0.0 /
Cbegin


      if ( ST_FAILED ) return

      do k = 1, 5
         j = k - 1
         call pgscr ( j, r(KCOL(k)), g(KCOL(k)), b(KCOL(k)) )
      enddo

      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_WEED -- Weed table on column
C
C    alan penny           ral                       1991 July

      subroutine tbsh_weed ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
      integer ncol, nrow, kacc, nout
      real bot, top
      character*70 text
Cbegin


      if ( ST_FAILED ) return

      call tbsh_getpos ( 1, ncol, nrow )					!Get column number to weed on
      if ( ncol.eq.-1 ) return

      call tbsh_wmess ( 3, 0 )
      bot = 0.0
      top = 0.0
      call get2r ( 'LIMITS', bot, top, .true., -1.0e20, 1.0e20 )	!Get the limits
      if ( ST_FAILED ) return
      call cswopr ( bot, top )

      call printo ( 'Accept or reject inside the limits ?' )		!Get wether accept or reject inside the limits
      call get_job ( 'WEEDOPT', 'accept:reject', kacc, 1, text, 0 )

      call tbsh_wmess ( 4, 0 )

      call tbsh_wcheck ( %val(IPD), %val(IPY), ncol, bot, top, kacc, 	!Find no of entries passed
     +                   nout )

      if ( nout.eq.0 ) then						!Do the weeding
         call printo ('No rows will survive this weed. Weed not done.')
      else

         call pargi ( nout )
         call printd ( ' No of rows surviving = %d' )

         call amovi ( %val(IPY), %val(IPYA), TBYM )			!Set restore

         call tbsh_wflagb (%val(IPD), %val(IPY), ncol, bot, top, kacc )	!'Weed'

         call tbsh_nload ( %val(IPN), %val(IPY) )			!Load screen
         call tbsh_dload ( %val(IPD), %val(IPX), %val(IPY) )

         CHANGED = .true.
         RESTORE = .true.
         TBYR = nout
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_CONVERT -- Convert a 'visible' row/column number to an actual holes one
C
C    a j penny               rgo                      82-11-4

      subroutine tbsh_convert ( n, kopt, kcx, kcy )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer   n		!i/o: Visible -> Real row/column number
      integer   kopt		!i: Option [1=Column(X);2=Row(Y)]
      integer   kcx(TBXM)	!i: Column selection list
      integer   kcy(TBYM)	!i: Row selection list
C--
      integer k, j, na
Cbegin


      if ( ST_FAILED ) return

      if ( kopt.eq.1 ) then
         j = 1
         do k = 1, TBX
            if ( kcx(k).eq.1 .and. j.le.n ) then
               na = k
               j = j + 1
            endif
         enddo
         n = na
      endif

      if ( kopt.eq.2 ) then
         j = 1
         do k = 1, TBY
            if ( kcy(k).eq.1 .and. j.le.n ) then
               na = k
               j = j + 1
            endif
         enddo
         n = na
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_BCONVERT -- Convert an actual holes row/column number to a 'visible' one
C
C    a j penny               rgo                      82-11-4

      subroutine tbsh_bconvert ( n, kopt, kcx, kcy )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer   n		!i/o: Real row/column number -> Visible
      integer   kopt		!i: Option [1=Column(X);2=Row(Y)]
      integer   kcx(TBXM)	!i: Column selection list
      integer   kcy(TBYM)	!i: Row selection list
C--
      integer k, j
Cbegin


      if ( ST_FAILED ) return

      j = 0
      do k = 1, n
         if ( kopt.eq.1 .and. kcx(k).eq.1 ) j = j + 1
         if ( kopt.eq.2 .and. kcy(k).eq.1 ) j = j + 1
      enddo
      n = j


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_WCHECK -- Find how many rows will survive
C
C    a j penny               rgo                      82-11-4

      subroutine tbsh_wcheck ( tb, kcy, ncol, bot, top, kacc, nout )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real	tb(TBXM,TBYM)	!i: Input table
      integer   kcy(TBYM)	!i: Row selection list
      integer	ncol		!i: Column to select on
      real	bot		!i: Bottom limit
      real	top		!i: Top limit
      integer	kacc		!i: Accept or reject flag (a/r=1/2)
      integer   nout		!o: No of surviving rows
C--
      integer k
      real d
      logical ok
Cbegin


      if ( ST_FAILED ) return

      nout = 0
      do k = 1, TBY
         if ( kcy(k).eq.1 ) then
            ok = .true.
            d = tb(ncol,k)
            if ( kacc.eq.1 ) then
               if ( d.lt.bot .or. d.gt.top ) ok = .false.
            else
               if ( d.ge.bot .and. d.lt.top ) ok = .false.
            endif
            if ( ok ) nout = nout + 1
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_WFLAGB -- Flag 'bad' rows in table as not to be used
C
C    alan penny              ral              1991 July

      subroutine tbsh_wflagb ( tb, kcy, ncol, bot, top, kacc )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real	tb(TBXM,TBYM)		!i: Input table
      integer   kcy(TBYM)		!i: Row selection list
      integer	ncol			!i: Column to select on
      real	bot			!i: Bottom limit
      real	top			!i: Top limit
      integer	kacc			!i: Accept or reject flag (a/r=1/2)
C--
      integer k
      real d
      logical ok
Cbegin


      if ( ST_FAILED ) return

      do k = 1, TBY
         if ( kcy(k).eq.1 ) then
            ok = .true.
            d = tb(ncol,k)
            if ( kacc.eq.1 ) then
               if ( d.lt.bot .or. d.gt.top ) ok = .false.
            else
               if ( d.ge.bot .and. d.lt.top ) ok = .false.
            endif
            if ( .not.ok ) kcy(k) = 0
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_CALC -- Do table calculations
C
C  alan penny                  ral              1991 Dec

      subroutine tbsh_calc ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
      character*3 temp
      integer ierr, k, ipstk, ipdd, ncol, nrow
Cbegin


      if ( ST_FAILED ) return

      USEDA = .false.							!Main array access flag
      call tbsh_getpos ( 1, ncol, nrow )				!Get column number to sort on
      if ( ncol.eq.-1 ) return

      call tbsh_wmess ( 3, 0 )
      call tbsh_calcgcl ( ierr )					!Get input info
      call tbsh_wmess ( 4, 0 )
      if ( ierr.ne.0 ) return

      if ( TBZ.eq.0 ) then						!Get work space
         call gtwrki ( 'STACKCALC', 1, ipstk, ierr )
      else
         call gtwrki ( 'STACKCALC', TBXMA*TBYA*TBZ, ipstk, ierr )
      endif
      if ( ierr.ne.0 ) return
      call gtwrkr ( 'COLCALC', TBYA, ipdd, ierr )
      if ( ierr.ne.0 ) return

      if ( TBZ.ne.0 ) then						!Load input tables into stack
         do k = 1, TBZ
            if ( TBTOT(k)(2:2).eq.'9' ) then
               call tbsh_loada (  %val(IPD), %val(IPX), %val(IPY),
     +                            k, %val(ipstk), TBXMA, TBYA )
            else
               call tbsh_load ( %val(IPCA(k)), TBVX(k), TBYA, k,
     +                          %val(ipstk), TBXMA, TBYA )
            endif
         enddo
      endif

      call tbsh_docalc ( %val(ipstk), TBXMA, TBYA, OPCODE, NOPCODE, 	!Do the calculations
     +                   IMP, IMPC, IMPV, VAR, CON, %val(ipdd),
     +                   KSEED, ncol, ierr )

      call tbsh_loadb ( %val(ipdd), ncol, %val(IPD), %val(IPX),
     +                  %val(IPY) )

      call wrkcan ( 'STACKCALC' )
      call wrkcan ( 'COLCALC' )
      if ( USEDA ) call wrkcan ( 'MAIN' )
      do k = 1, TBZ
         if ( TBTOT(k).ne.'T9' .and. TBTOT(k).ne.'TA' ) then
            temp = 'IN'//TBTOT(k)(2:2)
            call canpar ( temp )
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_CALCGCL -- Get calculator input info from the command line
C
C  alan penny                         RAL                1991 Dec

      subroutine tbsh_calcgcl ( ierro )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer   ierro		!o: Error flag (0=ok;1=bad)
C--
      integer k, kv, ierr, ny
      character texta*132, textb*70, intab*3

Cbegin


      if ( ST_FAILED ) return

      ierro = 0

      call get1c ( 'EQUATION', texta, ' ', .false. )
      if ( ST_FAILED ) return
      textb = texta(1:70)
      call uppcase ( textb, EXPRS )
      call tbsh_polish ( EXPRS, OPCODE, NOPCODE, TBID, NTB, IMP, 	!Translate it into reverse polish notation
     +                   IMPC, VARID, NVAR, IMPV, CON, NCON, ierr )
      if ( ierr.ne.0 ) then
         ierro = 1
         return
      endif

      TBZ = NTB								!Translate into TBCALC names
      TBXMA = 1								!Get tables
      TBYA = TBYR
      if ( TBZ.ne.0 ) then
         TBXMA = 0
         do k = 1, TBZ
            TBTOT(k) = TBID(k)						!Translate into TBCALC names
            if ( TBTOT(k)(1:2).eq.'T9' ) then
               TBVX(k) = TBXR + 5
               ny = TBYR
            elseif ( TBTOT(k)(2:2).eq.'A' ) then
                TBVX(k) = TBXR + 5
                ny = TBYR
                call gtwrkr ( 'MAIN', ny*TBVX(k), IPCA(k), ierr )
                USEDA = .true.
                call tbsh_calcload ( %val(IPD), %val(IPN), %val(IPX),
     +                         %val(IPY), %val(IPCA(k)), TBVX(k), ny )
            else
               intab = 'IN '
               intab(3:3) = TBTOT(k)(2:2)
               call optabr ( intab, IPCA(k), TBVX(k), ny, .false.,
     +                       ierr )
               if ( ierr.ne.0 ) then
                  if ( ierr.eq.3 ) then
                     call printo ( 'All input tables must come '//
     +                             'from different files' )
                     call printo ( 'To input a table more than '//
     +                  'once - refer to it by only one parameter')
                  endif
                    ierro = 1
                  return
               endif
            endif
            if ( (TBVX(k)-5).gt.999 ) then
               call printo ('ERROR: Cannot have more than 999 columns')
               ierro = 1
               return
            endif
            if ( ny.ne.TBYA ) then
               call printo (
     +             'ERROR: Tables must have same number of rows' )
               ierro = 1
               return
            endif
            TBXMA = max(TBXMA,(TBVX(k)-5))
         enddo
      endif

      NVARTOT = NVAR							!Translate into TBCALC names
      if ( NVARTOT.gt.0 ) then						!Get input variables
         do k = 1, NVARTOT
            VARTOT(k) = VARID(k)
            call get1r ( VARTOT(k), VAR(k), 0.0, -1.0e37, +1.0e37 )
            if ( ST_FAILED ) return
         enddo
      endif

      kv = 0								!Random generator seed
      if ( index(EXPRS,'RAN').ne.0 ) kv = 1
      if ( index(EXPRS,'GAUSS').ne.0 ) kv = 1
      KSEED = 1
      if ( kv.eq.1 ) call get1i ( 'SEED', KSEED, 1234567891,
     +                            1200000001, 1400000001 )
      if ( ST_FAILED ) return


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_CALCLOAD -- Load present array into no 'holes' array
C
C  alan penny                 ral                1991 July

      subroutine tbsh_calcload ( tb, tbn, kcx, kcy, out, nxo, nyo )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real      tb(TBXM,TBYM)		!i: Data table
      real      tbn(5,TBYM)		!i: Name table
      integer   kcx(TBXM)		!i: Column checks
      integer   kcy(TBYM)		!i: Row checks
      integer   nxo			!i: No of columns in new array
      integer   nyo			!i: No of rows (+5) in new array
      real      out(nxo,nyo)		!o: New array
C--
      integer j, k, ja, ka, jc
Cbegin


      if ( ST_FAILED ) return

      ka = 0
      do k = 1, TBY
         if ( kcy(k).eq.1 ) then
            ka = ka + 1
            ja = 0
            do j = 1, TBX
              if ( kcx(j).eq.1 ) then
                 ja = ja + 1
                 out(ja+5,ka) = tb(j,k)
              endif
            enddo
            do jc = 1, 5
               out(jc,ka) = tbn(jc,k)
            enddo
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_POLISH -- Decode equation and sort into reverse Polish
C
C  alan penny                RAL                1991 Dec

      subroutine tbsh_polish ( exprs, opcode, noper, tbid, ntb, imp,
     +                         impc, varid, nvar, impv, con, ncon,
     +                         ierr )

      implicit none
      include 'STARMAN_INC'

      character*130    exprs		!i: Equation to decode
      character*6      opcode(70)	!o: Polish stack of commands
      integer          noper		!o: No of operations
      character*2      tbid(70)		!o: Tables identifiers
      integer          ntb		!o: Number of different tables
      integer          imp(70)		!o: Pointers to tables
      integer          impc(70)		!o: Pointers to columns
      character*1      varid(26)	!o: Variables identifiers
      integer          nvar		!o: Number of variable
      integer          impv(26)		!o: Pointers to variables
      real             con(70)		!o: Constants
      integer          ncon		!o: Number of constants
      integer          ierr		!o: Error flag (0=ok;bad 2=;3=;4=)
C--
      character in*130, numchr*130, test*7, output*7, colid*3
      integer j, k, tos, stk(0:130), symb(130), ll, ncin, nnum,
     +        nsymb, iok, isymb, ndiff, kextra, istat
      real    const
      logical opnext, found, ischar, isnumb, issign, lmore, atend
      external ischar, isnumb, issign

      integer maxsym
      parameter ( maxsym=38 )

      character*7 oper(-4:maxsym), opsymb(-4:maxsym)
      integer opl(-4:maxsym), opr(-4:maxsym), l(-4:maxsym),
     +        prl(-4:maxsym), prr(-4:maxsym)

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),		!Set up table of operators,
     +        prr(j),j=-4,10 ) /					! symbols and their priorities
     +  '       ', 'LDCOL  ', 3, 0, 0, 10, 10,
     +  '       ', 'LDCON  ', 1, 0, 0, 10, 10,
     +  '       ', 'LDVAR  ', 1, 0, 0, 10, 10,
     +  '       ', 'LDTB   ', 2, 0, 0, 10, 10,
     +  '=      ', '=      ', 1, 1, 1,  0,  0,
     +  ')      ', ')      ', 1, 1, 0,  2, 10,
     +  '(      ', '(      ', 1, 0, 1, 10,  1,
     +  '-      ', '-      ', 1, 1, 1,  4,  4,
     +  '+      ', '+      ', 1, 1, 1,  4,  4,
     +  '**     ', '**     ', 2, 1, 1,  9,  6,
     +  '*      ', '*      ', 1, 1, 1,  5,  5,
     +  '/      ', '/      ', 1, 1, 1,  5,  5,
     +  ':      ', ':      ', 1, 1, 1,  2,  2,
     +  '-      ', 'NEG    ', 1, 0, 1,  8,  7,
     +  '+      ', 'POS    ', 1, 0, 1,  8,  7 /

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),
     +        prr(j),j=11,20 ) /
     +  'SQRT(  ', 'SQRT(  ', 5, 0, 1, 10,  1,
     +  'EXP(   ', 'EXP(   ', 4, 0, 1, 10,  1,
     +  'LOG10( ', 'LOG10( ', 6, 0, 1, 10,  1,
     +  'LOG(   ', 'LOG(   ', 4, 0, 1, 10,  1,
     +  'SIN(   ', 'SIN(   ', 4, 0, 1, 10,  1,
     +  'COS(   ', 'COS(   ', 4, 0, 1, 10,  1,
     +  'TAN(   ', 'TAN(   ', 4, 0, 1, 10,  1,
     +  'ASIN(  ', 'ASIN(  ', 5, 0, 1, 10,  1,
     +  'ACOS(  ', 'ACOS(  ', 5, 0, 1, 10,  1,
     +  'ATAN(  ', 'ATAN(  ', 5, 0, 1, 10,  1 /

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),
     +        prr(j),j=21,30 ) /
     +  'ATAN2( ', 'ATAN2( ', 6, 0, 1, 10,  1,
     +  'SINH(  ', 'SINH(  ', 5, 0, 1, 10,  1,
     +  'COSH(  ', 'COSH(  ', 5, 0, 1, 10,  1,
     +  'TANH(  ', 'TANH(  ', 5, 0, 1, 10,  1,
     +  'ABS(   ', 'ABS(   ', 4, 0, 1, 10,  1,
     +  'AINT(  ', 'AINT(  ', 5, 0, 1, 10,  1,
     +  'ANINT( ', 'ANINT( ', 6, 0, 1, 10,  1,
     +  'MOD(   ', 'MOD(   ', 4, 0, 1, 10,  1,
     +  'SIGN(  ', 'SIGN(  ', 5, 0, 1, 10,  1,
     +  'DIM(   ', 'DIM(   ', 4, 0, 1, 10,  1 /

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),
     +       prr(j),j=31,maxsym ) /
     +  'MIN(   ', 'MIN(   ', 4, 0, 1, 10,  1,
     +  'MAX(   ', 'MAX(   ', 4, 0, 1, 10,  1,
     +  'CLIP(  ', 'CLIP(  ', 5, 0, 1, 10,  1,
     +  'GAUSS( ', 'GAUSS( ', 6, 0, 1, 10,  1,
     +  'RAN(   ', 'RAN(   ', 4, 0, 1, 10,  1,
     +  'RR     ', 'RR     ', 2, 0, 0, 10, 10,
     +  'CC     ', 'CC     ', 2, 0, 0, 10, 10,
     +  '%      ', '/      ', 1, 1, 1,  5,  5 /
Cbegin


      if ( ST_FAILED ) return

      ierr = 0
      ncin = 0

      do k = 1, len(exprs)						!Remove embedded blanks
         if ( exprs(k:k).ne.' ' .and. ncin.lt.130  ) then		!Count no of characters
            ncin = ncin + 1
            in(ncin:ncin) = exprs(k:k)
         endif
      enddo

      exprs = in(1:ncin)						!Return the expression with blanks removed

      ncin = ncin+1							!Append an '= ' operator to terminate the expression
      in(ncin:) = '='

      ntb = 0								!Initiallise counters
      nvar = 0
      ncon = 0
      k = 1
      j = 0

      opnext = .false.							!Indicates if an operator is expected next
									! first entity must not look like an operator

      lmore = .true.							!Loop thru OPLs until '='found
      do while ( lmore )

         found = .false.						!Search through the list of symbols to
         nsymb = -1							! identify which comes next
         do while ( nsymb.lt.maxsym .and. .not.found )
            nsymb = nsymb + 1
            if ( opnext.eqv.(opl(nsymb).eq.1) ) then			!Symbol is only valid if it looks like
               test = in(k:min(k+l(nsymb)-1,130))			! an operator or operand from the left, as appropriate
               if ( test.eq.oper(nsymb) ) found = .true.
             endif
         enddo

         if ( .not.found ) then						!If symbol was not found: -
            if ( opnext ) then						!Error if an operator expected
               ierr = 3
               call printo ( 'ERROR: Cant understand Equation - ' )
               call printo ( 'ERROR: '//exprs )
               return
            else if ( in(k:k).eq.'T' .and. ischar(in(k+1:k+1)) .and.	!If an operand was expected, it may be
     +               isnumb(in(k+2:k+2)) ) then				! a table, variable or const.
               nsymb = -1						!If it is a table, add name to table stack
               ntb = ntb + 1
               tbid(ntb) = in(k:k+1)
               colid = '000'
               if ( .not.isnumb(in(k+3:k+3)) ) then
                  colid(3:3) = in(k+2:k+2)
                  kextra = 1
               elseif ( .not.isnumb(in(k+4:k+4)) ) then
                  colid(2:3) = in(k+2:k+3)
                  kextra = 2
               else
                  colid = in(k+2:k+4)
                  kextra = 3
               endif
               call chartoi ( colid, impc(ntb), istat )			!Add column to column stack
               if ( istat.ne.0 .or. impc(ntb).eq.0 ) then
                  ierr = 1
                  call printo (
     +            'ERROR: Bad column ID - '//tbid(ntb)//colid )
                  call printo ( 'ERROR: '//exprs )
                  return
               endif
            elseif ( in(k:k+2).eq.'COL'.and.isnumb(in(k+3:k+3)) ) then 	!If an operand was expected, it may be
               nsymb = -4						! a table, variable or const.
               ntb = ntb + 1						!If it is a table, add name to table stack
               tbid(ntb) = 'T9'
               colid = '000'
               if ( .not.isnumb(in(k+4:k+4)) ) then
                  colid(3:3) = in(k+3:k+3)
                  kextra = 1
               elseif ( .not.isnumb(in(k+5:k+5)) ) then
                  colid(2:3) = in(k+3:k+4)
                  kextra = 2
               else
                  colid = in(k+3:k+5)
                  kextra = 3
               endif
               call chartoi ( colid, impc(ntb), istat )			!Add column to column stack
               if ( istat.ne.0 .or. impc(ntb).eq.0 ) then
                  ierr = 1
                  if ( tbid(ntb).eq.'T9' ) then
                     call printo ( 'ERROR: Bad column ID for working'//
     +                             ' table - '//colid )
                  else
                     call printo ( 'ERROR: Bad column ID - '//
     +                             tbid(ntb)//colid )
                  endif
                  call printo ( 'ERROR: '//exprs )
                  return
               endif
            else if ( ischar(in(k:k)) ) then
               nsymb = -2						!If it is a variable, add name to variable stack
               nvar = nvar + 1
               varid(nvar) = in(k:k)
            else

               nnum = 0							!Otherwise it may be a constant...
               numchr = ' '						! extract contiguous numerical characters

               atend = .false.						!Character may be part of a numerical constant
               ll = k - 1						! if it is 0..9 or '.'
               do while ( ll.lt.ncin .and. .not.atend )			! or if it is an 'e' following one of the above
                  ll = ll + 1						! or if it is a sign following an 'e'
                  if ( isnumb(in(ll:ll)) .or. (in(ll:ll).eq.'E') .or.
     +           (issign(in(ll:ll)).and.(in(ll-1:ll-1).eq.'E')) ) then
                     nnum = nnum + 1
                     numchr(nnum:nnum) = in(ll:ll)
                  else
                     atend = .true.					!End of number as soon as one of the above tests fails
                  endif
               enddo
               call chartor ( numchr(:nnum), const, iok )		!Try to read these characters as a constant

               if ( iok.eq.0 .and. nnum.ne.0 ) then			!If successful, add constant to stack
                  ncon = ncon + 1
                  con(ncon) = const
                  nsymb = -3
                  l(nsymb) = nnum
               else
                  ierr = 2						!Otherwise there is a bad operand error
                  call printo ( 'ERROR: Cant understand Equation -' )
                  call printo ( 'ERROR: '//exprs )
                  return
               endif

            endif
         endif

         j = j + 1							!Put the identified symbol into the output
         symb(j) = nsymb						! array and move the input pointer to the
         k = k + l(nsymb)						! next symbol
         if ( nsymb.eq.-1 .or. nsymb.eq.-4 ) k = k + kextra

         opnext = opr(nsymb).ne.1					!Decide whether an operator or operand follows
         if ( opsymb(nsymb).eq.'=' ) lmore = .false.

      enddo

      call azeroi ( stk(0), j+1 )					!Zero operator stack for converting to reverse polish
      tos = 0
      isymb = 1
      noper = 0

      lmore = .true.
      do while ( lmore )
         lmore = .false.

         if ( index(oper(stk(tos)),'(').ne.0 .and.			!If the top of stack and input stream have
     +        oper(symb(isymb)).eq.')' ) then				! matching parentheses, cancel them

            if ( oper(stk(tos)).ne.'(' ) then				!If there is a function asssociated with the
 									! opening parenthesis then send it to the output stream

               output = opsymb(stk(tos))				!Remove the enclosed '(' from functions first
               output(index(output,'('):index(output,'(')) = ' '
               tos = tos - 1
               isymb = isymb + 1
             else
               tos = tos - 1
               isymb = isymb + 1
               lmore = .true.
             endif

         else if ( prr(stk(tos)).ge.prl(symb(isymb)) ) then		!If the symbol on the top of the stack has
            output = opsymb(stk(tos))					! a high enough priority, transfer it to
            tos = tos - 1						! the output stream
         else
            tos = tos + 1						!Otherwise, transfer the next symbol to the stack
            stk(tos) = symb(isymb)
            isymb = isymb + 1
            lmore = .true. 						!Return for next test
         endif

         if ( .not.lmore ) then

            if ( index(output,'(').ne.0 ) then				!If a bracket appears in the output,
               ierr = 4							! it results from unpaired parentheses
               call printo ( 'ERROR: ) missing from Equation -' )	! in the input expression...quit qith error
               call printo ( 'ERROR: '//exprs )
               return
            elseif ( index(output,')').ne.0 ) then
               ierr = 4
               call printo ( 'ERROR: (  missing from Equation -' )
               call printo ( 'ERROR: '//exprs )
               return
            endif

            if ( output.ne.'POS'.and.output.ne.':' ) then		!If there is some output, disregard it
               noper = noper + 1					! if it is unary + or a comma
               opcode(noper) = output
            endif

            if ( output.ne.'=' ) lmore = .true.				!Return for next symbol if not the end

         endif

      enddo

      if ( ntb.ge.1 ) then						!If tables or variables are referenced, sort
         call tbsh_sortc ( tbid, ntb, imp, ndiff, symb )			! their names into alphabetical order and obtain
         ntb = ndiff							! pointers to allow them to be accessed in their
      endif								! original order
      if ( nvar.ge.1 ) then
         call tbsh_sortc ( varid, nvar, impv, ndiff, symb )
         nvar = ndiff
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_DOCALC -- do the calculations
C
C  alan penny                     RAL            1991 Dec

      subroutine tbsh_docalc ( tb, tbx, tby, opcode, nopcode, imp,
     +                         impc, impv, var, con, tbcol, kseed,
     +                         kx, ierr )

      implicit none
      include 'ST_LIMITS_INC'
      include 'STARMAN_INC'

      integer      tbx			!i: Max X size of input tables
      integer      tby			!i: Max Y size of input tables
      real         tb(tbx,tby,*)	!i: Input tables
      integer      nopcode		!i: Number of operations
      character*6  opcode(nopcode)	!i: Code for operations
      integer      imp(70)		!i: Number of table in (i)th posn
      integer      impc(70)		!i: Number of column in (i)th posn
      integer      impv(70)		!i: Number variable in (i)th posn
      real         var(26)		!i: Variables
      real         con(70)		!i: Constants
      real         tbcol(tby)		!o: Output column
      integer      kseed		!i/o: Random number seed
      integer      kx			!i: Column number
      integer      ierr			!o: Error flag (0=ok;1=bad)
C--
      logical found, bad, more
      integer j, k, tos, numitab, nvar, ncon, ib, nop
      real    rv
      double precision s(0:130), a, b, c, dib, dvmin, dvmax
      integer opwhich(70)

      integer nsymb
      parameter ( nsymb=38 )

      character opsymb(nsymb)*6						!Recognised operations
      data opsymb /
     + 'LDCOL ','LDCON ','LDVAR ','LDTB  ','=     ',
     + '-     ','+     ','**    ','*     ','/     ','NEG   ','SQRT  ',
     + 'EXP   ','LOG10 ','LOG   ','SIN   ','COS   ','TAN   ','ASIN  ',
     + 'ACOS  ','ATAN  ','ATAN2 ','SINH  ','COSH  ','TANH  ','ABS   ',
     + 'AINT  ','ANINT ','MOD   ','SIGN  ','DIM   ','MIN   ','MAX   ',
     + 'CLIP  ','GAUSS ','RAN   ','RR    ','CC    ' /
Cbegin


      if ( ST_FAILED ) return

      ierr = 0								!Failure flag

      dvmax = INT_MAXRR							!Set (dble precision) max and min vals
      dvmin = INT_MINRR

      do k = 1, nopcode							!Find `opcode' for each input operator
         found = .false.
         j = 0
         do while ( j.lt.nsymb .and. .not.found )
            j = j + 1
            if ( opcode(k).eq.opsymb(j) ) then
               opwhich(k) = j
               found = .true.
            endif
         enddo
         if ( .not.found ) then
            ierr = 1
            return
         endif
      enddo

      do j = 1, tby							!Do for all rows in a column

         tos = 0							!Initialise the arith, table,
         numitab = 0							! variable, constant stack pointers
         nvar = 0
         ncon = 0

         nop = 0							!Do the operations
         more = .true.
         bad = .false.

         do while ( nop.lt.nopcode .and. more )
            nop = nop + 1

            go to (  99,101,102,103,104,105,106,107,108,109,110,111,
     +              112,113,114,115,116,117,118,119,120,121,122,123,
     +              124,125,126,127,128,129,130,131,132,133,134,135,
     +              136,137 ),
     +            opwhich(nop)

  99        continue
               tos = tos + 1						!Load column on to stack
               numitab = numitab + 1
               s(tos) = tb(impc(numitab),j,imp(numitab))
            go to 100
 101        continue
               tos = tos + 1						!Load constant on to stack
               ncon = ncon + 1
               s(tos) = con(ncon)
            go to 100
 102        continue
               tos = tos + 1						!Load variable onto stack
               nvar = nvar + 1
               s(tos) = var(impv(nvar))
            go to 100
 103        continue
               tos = tos + 1						!Load table on to stack
               numitab = numitab + 1
               s(tos) = tb(impc(numitab),j,imp(numitab))
            go to 100
 104        continue
               tbcol(j) = min(dvmax,max(dvmin,s(tos)))			!=  : end of calculation
               more = .false.
            go to 100
 105        continue
               s(tos-1) = s(tos-1) - s(tos)				!- : subtract tos
               tos = tos - 1
            go to 100
 106        continue
               s(tos-1) = s(tos-1) + s(tos)				!+ : add tos
               tos = tos - 1
            go to 100
 107        continue
               a = s(tos-1)						!** : raise to power
               b = s(tos)
               if ( a.ge.0.0 ) then
                  c = a**b
               else
                  ib = nint(b)
                  dib = ib
                  if ( abs(dib-b).le.1.0d-7 ) then
                     c = a**ib
                  else
                     bad = .true.
                  endif
               endif
               tos = tos - 1
               s(tos) = c
            go to 100
 108        continue
               s(tos-1) = s(tos-1)*s(tos)				!* : multiply tos
               tos = tos - 1
            go to 100
 109        continue
               a = s(tos)						!/ : Divide tos
               if ( a.ne.0.0d0 ) then
                  s(tos-1) = s(tos-1)/a
                  tos = tos - 1
               else
                  bad = .true.
               endif
            go to 100
 110        continue
               s(tos) = -s(tos)						!NEG : Negate tos
            go to 100
 111        continue
              a = s(tos)						!SQRT : Square root tos
               if ( a.ge.0.0d0 ) then
                  s(tos) = sqrt(a)
               else
                  bad = .true.
               endif
            go to 100
 112        continue
               a = s(tos)						!EXP : E to power tos
               if ( a.le.100.0d0 ) then
                  s(tos) = exp(a)
               else
                  bad = .true.
               endif
            go to 100
 113        continue
               a = s(tos)						!LOG10 : Log tos base 10
               if ( a.gt.0.0d0 ) then
                  s(tos) = log10(a)
               else
                  bad = .true.
               endif
            go to 100
 114        continue
               a = s(tos)						!LOG : Natural log of tos
               if ( a.gt.0.0d0 ) then
                  s(tos) = log(a)
               else
                  bad = .true.
               endif
            go to 100
 115        continue
               s(tos) = sin(s(tos))					!SIN : Sine of tos
            go to 100
 116        continue
               s(tos) = cos(s(tos))					!COS : Cosine of tos
            go to 100
 117        continue
               a = s(tos)						!TAN : Tangent of tos
               s(tos) = tan(a)
            go to 100
 118        continue
            a = s(tos)							!ASIN : Arcsine of tos
               if ( abs(a).le.1.0d0 ) then
                  s(tos) = asin(a)
               else
                  bad = .true.
               endif
            go to 100
 119        continue
               a = s(tos)						!ACOS : Arccosine of tos
               if ( abs(a).le.1.0d0 ) then
                  s(tos) = acos(a)
               else
                  bad = .true.
               endif
            go to 100
 120        continue
               s(tos) = atan(s(tos))					!ATAN : Arctangent of tos
            go to 100
 121        continue
               a = s(tos-1)						!ATAN2 : Arctangent of tos ratio
               b = s(tos)
               if ( a.ne.0.0d0 .or. b.ne.0.0d0 ) then
                  tos = tos - 1
                  s(tos) = atan2(a,b)
               else
                  bad = .true.
               endif
            go to 100
 122        continue
               a = s(tos)						!SINH : Hyperbolic sine of tos
               if ( abs(a).le.100.0d0 ) then
                  s(tos) = sinh(a)
               else
                  bad = .true.
               endif
            go to 100
 123        continue
               a = s(tos)						!COSH : Hyperbolic cosine of tos
               if ( abs(a).le.100.0d0 ) then
                  s(tos) = cosh(a)
               else
                  bad = .true.
               endif
            go to 100
 124        continue
               s(tos) = tanh(s(tos))					!TANH : Hyperbolic tangent of tos
            go to 100
 125        continue
               s(tos) = abs(s(tos)) 					!ABS : Absolute value of tos
            go to 100
 126        continue
               s(tos) = aint(s(tos))					!AINT : Truncate tos
            go to 100
 127        continue
               s(tos) = anint(s(tos))					!ANINT : Nearest integer to tos
            go to 100
 128        continue
               a = s(tos-1)						!MOD : Modulo on two tos entries
               b = s(tos)
               if ( b.ne.0.0d0 ) then
                  tos = tos - 1
                  s(tos) = mod(a,b)
               else
                  bad = .true.
               endif
            go to 100
 129        continue
               s(tos-1) = sign(s(tos-1),s(tos))				!SIGN : Transfer of sign between two tos entries
               tos = tos - 1
            go to 100
 130        continue
               s(tos-1) = dim(s(tos-1),s(tos))				!DIM : Positive difference of two tos entries
               tos = tos - 1
            go to 100
 131        continue
              s(tos-1) = min(s(tos-1),s(tos))				!MIN : Minimum of two tos entries
               tos = tos - 1
            go to 100
 132        continue
               s(tos-1) = max(s(tos-1),s(tos))				!MAX : Maximum of two tos entries
               tos = tos - 1
            go to 100
 133        continue
               a = s(tos-2)						!CLIP : Abort if tos lies outside window
               b = s(tos-1)
               c = s(tos)
               if ( a.le.b .and. b.le.c ) then
                  tos = tos - 2
                  s(tos) = b
               else
                  bad = .true.
               endif
            go to 100
 134        continue
               call gasdev ( rv, kseed )				!GAUSS : Put gaussian noise on tos
               s(tos) = s(tos)*dble(rv)
            go to 100
 135        continue
               call rano ( rv, kseed )					!RAN : Uniform random noise
               s(tos) = s(tos)*dble(rv)
            go to 100
 136        continue
               tos = tos + 1						!RR : Row number
               s(tos) = j
            go to 100
 137        continue
               tos = tos + 1						!CC : Column number
               s(tos) = kx
            go to 100
 100        continue

            if ( bad ) then						!Invalid arithmetic operation done
               tbcol(j) = 0.0
               more = .false.
            endif

         enddo

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_LOAD -- Copy table into a 3-D stack
C
C  alan penny                 ral                     1991 Dec

      subroutine tbsh_load ( tb, tbvx, tby, nin, tbs, tbxs, tbys )

      implicit none
      include 'STARMAN_INC'

      integer     tbvx			!i: X size (inc 5 for name) of input table
      integer     tby			!i: Y size of input table
      real        tb(tbvx,tby)		!i: Input table
      integer     nin			!i: Plane to put it in
      integer     tbxs			!i: X size of output table
      integer     tbys			!i: Y size of output table
      real        tbs(tbxs,tbys,*)	!i/o: Output table
C--
      integer j, k, ja, ka
Cbegin


      if ( ST_FAILED ) return

      call azeror ( tbs(1,1,nin), tbxs*tbys )

      ja = min(tbxs,(tbvx-5))
      ka = min(tbys,tby)
      do k = 1, ka
         do j = 1, ja
            tbs(j,k,nin) = tb((j+5),k)
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_LOADA -- Copy present work table into a 3-D stack
C
C  alan penny                 ral                     1991 Dec

      subroutine tbsh_loada ( tb, kcx, kcy, nin, tbs, tbxs, tbys )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real        tb(TBXM,TBYM)		!i: Table
      integer     kcx(TBXM)		!i: X selection
      integer     kcy(TBYM)		!i: Y selection
      integer     nin			!i: Plane to put it in
      integer     tbxs			!i: X size of output table
      integer     tbys			!i: Y size of output table
      real        tbs(tbxs,tbys,*)	!i/o: Output table
C--
      integer j, k, kx, ky
Cbegin


      if ( ST_FAILED ) return

      call azeror ( tbs(1,1,nin), tbxs*tbys )

      ky = 1
      do k = 1, TBY
         if ( kcy(k).eq.1 ) then
            kx = 1
            do j = 1, TBX
               if ( kcx(j).eq.1 ) then
                  tbs(kx,ky,nin) = tb(j,k)
                  kx = kx + 1
               endif
            enddo
            ky = ky + 1
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_LOADB -- Load column to master and display
C
C    alan penny                ral              1991 July

      subroutine tbsh_loadb ( tbin, ncol, tb, kcx, kcy )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real    tbin(*)		!i:   Column to load
      integer ncol		!i:   'Visible' Column number to put in
      real    tb(TBXM,TBYM)	!i/o: Table
      integer kcx(TBXM)		!i:   X selection
      integer kcy(TBYM)		!i:   Y selection
C--
      integer k, ktx, kty, kx, ky, kxout, kyout
      logical xmore
      real val, x, y
Cbegin


      if ( ST_FAILED ) return

      ky = 0								!Load data
      do k = 1, TBY
         if ( kcy(k).eq.1 ) then
            ky = ky + 1
            tb(ncol,k) = tbin(ky)
         else
            tb(ncol,k) = 0.0
         endif
      enddo

      call gd_bbuf							!Display

      kx = ncol

      y = 10.0 + 0.5*CHYSIZE + real(NUMY-1)*YBOX

      kty = KYSTART
      call tbsh_convert ( kty, 2, kcx, kcy )
      kyout = 0
      do while ( kty.le.TBY .and. kyout.lt.NUMY )

         if ( kcy(kty).eq.1 ) then

            x = XDOFF + 0.05*XBOX
            ktx = KXSTART
            call tbsh_convert ( ktx, 1, kcx, kcy )
            ktx = KXSTART
            xmore = .true.
            kxout = 0
            do while ( xmore .and. ktx.le.TBX .and. kxout.lt.NUMX )
               if ( kcx(ktx).eq.1 ) then
                  if ( ktx.eq.kx ) then
                     val = tb(ktx,kty)
                     call tbsh_value ( x, y, val, 12, 2 )
                     xmore = .false.
                  endif
                  x = x + XBOX
                  kxout = kxout + 1
               endif
               ktx = ktx + 1
            enddo

            y = y - YBOX
            kyout = kyout + 1

         endif
         kty = kty + 1

      enddo

      call gd_ebuf
      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_SORTC -- Sort identifiers to alphabetical order
C
C  alan penny            ral                     1991 Dec

      subroutine tbsh_sortc ( c, nc, imp, ndiff, iw )

      implicit none
      include 'STARMAN_INC'

      integer        nc		!i: Number of identifiers
      character*(*)  c(nc)	!i/o: Identiers
      integer        imp(nc)	!o: pointer to identifier place in stack
      integer        ndiff	!o: Number of different identifiers
      integer        iw(nc)	!o: Work space
C--
      integer k, nn, it
      character*130 text
      logical repeat
Cbegin


      if ( ST_FAILED ) return

      nn = min(130,len(c(1)))						!Length of input strings

      do k = 1, nc							!Initiallise pointers in workspace
         iw(k) = k
      enddo

      repeat = .true.							!Perform a bubble sort to put
      do while ( repeat ) 						! character strings into alphabetical order
         repeat = .false.
         do k = 1, nc-1
            if ( c(k+1).lt.c(k) ) then
               repeat = .true.
               text(:nn) = c(k+1)					!Swap pairs which are in the wrong order
               c(k+1) = c(k)
               c(k) = text(:nn)
               it = iw(k+1)						!Permute the pointers in the same way
               iw(k+1) = iw(k)
               iw(k) = it
            endif
         enddo
      enddo

      ndiff = 1								!Pointers now point to original positions. scan list to remove
      imp(iw(1)) = 1							! repeated entries and reverse the pointing direction

      do k = 2, nc
         if ( c(k).ne.c(ndiff) ) then					!If a different character string is found, count it and put it
            ndiff = ndiff + 1						! in the correct place in the list
            c(ndiff) = c(k)
         endif
         imp(iw(k)) = ndiff						!Set the appropriate output pointer to its new location
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_SORT -- Sort table
C
C   alan penny             RAL               1991 July

      subroutine tbsh_sort ( kopt, koptst )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer    kopt		!i: Sort up (offset+1) or down (offset+2)?
      integer    koptst		!i: Option offset
C--
      integer ncol, nrow, kacc, ipnum, ipval, ipda, ipna, ierr
Cbegin


      if ( ST_FAILED ) return

      call tbsh_getpos ( 1, ncol, nrow )				!Get column number to sort on
      if ( ncol.eq.-1 ) return

      call tbsh_wmess ( 3, 0 )

      kacc = kopt - koptst						!Ascending/descending sort?

      call gtwrkr ( 'NUMBER', TBY, ipnum, ierr )			!Get working space for sort
      if ( ierr.ne.0 ) return
      call gtwrkr ( 'VALUES', TBY, ipval, ierr )
      if ( ierr.ne.0 ) return

      call tbsh_wmess ( 4, 0 )

      call tbsh_compress ( %val(IPD), %val(IPN), %val(IPH), %val(IPX),	!Remove holes
     +                     %val(IPY) )

      call coprr ( %val(IPD), TBXM, TBYM, ncol, ncol, 1, TBY,		!Copy the sorting parameter into
     +             %val(ipval), 1, TBY, 1, 1 )				! the working area and perform sort
      call anumincr ( %val(ipnum), TBY )
      call sort2r ( %val(ipval), %val(ipnum), TBY )
      if ( kacc.eq.2 ) call tbsh_sloadb ( %val(ipnum), TBY )

      call gtwrkr ( 'TBDA', TBXM*TBY, ipda, ierr )			!Load the new data and names
      if ( ierr.ne.0 ) return
      call gtwrkr ( 'TBNA', 5*TBY, ipna, ierr )
      if ( ierr.ne.0 ) return
      call tbsh_trans ( %val(IPD), %val(IPN), %val(ipda), %val(ipna),
     +                  %val(ipnum) )

      call tbsh_nload ( %val(IPN), %val(IPY) )				!Load screen
      call tbsh_dload ( %val(IPD), %val(IPX), %val(IPY) )

      call wrkcan ( 'VALUES' )						!Free data area
      call wrkcan ( 'NUMBER' )
      call wrkcan ( 'TBDA' )
      call wrkcan ( 'TBNA' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_STATS -- Calculate statistics of a column
C
C   alan penny             RAL               1995 Aug

      subroutine tbsh_stats ( tbh, kopt, koptst )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer    tbh(5,TBXM)	!i: Header table
      integer    kopt		!i: One column (offset+1) or two (offset+2)?
      integer    koptst		!i: Option offset
C--
      integer ncol, nrow, ncol2, nrow2, ipa, ipb, ierr, kdo
      character*20 text, text2
Cbegin


      if ( ST_FAILED ) return

      kdo = kopt - koptst

      call tbsh_getpos ( 1, ncol, nrow )				!Get column number to work on
      if ( ncol.eq.-1 ) then
         if ( kdo.eq.1 ) then
            return
         else
            ncol = 1
            ncol2 = 2
            call get2i ( 'COLS', ncol, ncol2, .true., 1, TBX )
            call tbsh_convert ( ncol, 1, %val(IPX), %val(IPY) )
            call tbsh_convert ( ncol2, 1, %val(IPX), %val(IPY) )
         endif
      else
         if ( kdo.eq.2 ) then
            call tbsh_getpos ( 1, ncol2, nrow2 )			!Get 2nd column number to work on, if wanted
            if ( ncol2.eq.-1 ) return
         endif
      endif

      call tbsh_wmess ( 3, 0 )

      call gtwrkr ( 'A', TBY, ipa, ierr )				!Get working space for sort
      if ( ierr.ne.0 ) return

      call tbsh_wmess ( 4, 0 )

      call tbsh_loadcol ( %val(IPD), %val(IPX),	%val(IPY), ncol,	!Remove holes
     +                    %val(ipa) )
      call tbsh_chget ( tbh(1,ncol), text )

      if ( kdo.eq.2 ) then
         call gtwrkr ( 'B', TBY, ipb, ierr )
         if ( ierr.ne.0 ) then
            call wrkcan ( 'A' )
            return
         endif
         call tbsh_loadcol ( %val(IPD), %val(IPX), %val(IPY), ncol2,	!Remove holes
     +                      %val(ipb) )
         call tbsh_chget ( tbh(1,ncol2), text2 )
         call printo ( ' ' )
         call pargi ( ncol )
         call pargc ( text )
         call printd ( '1st column - number %d : Header %c' )
         call tbsh_dostat1 ( %val(ipa), TBY )
         call pargi ( ncol2 )
         call printo ( ' ' )
         call pargc ( text2 )
         call printd ( '2nd column - number %d : Header %c' )
         call tbsh_dostat1 ( %val(ipb), TBY )
         call printo ( ' ' )
         call printo ( 'Combination of both' )
         call tbsh_dostat2 ( %val(ipa), %val(ipb), TBY )
         call printo ( ' ' )
         call wrkcan ( 'A' )
         call wrkcan ( 'B' )
      else
         call printo ( ' ' )
         call pargi ( ncol )
         call pargc ( text )
         call printd ( 'Column - number %d : Header %c' )
         call tbsh_dostat1 ( %val(ipa), TBY )
         call printo ( ' ' )
         call wrkcan ( 'A' )
      endif


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_DOSTAT1 -- Do the calcs for one column
C
C    a j penny                      ral                  1991 may

      subroutine tbsh_dostat1 ( x, n )

      implicit none
      include 'STARMAN_INC'

      integer  n                !i: No of points
      real     x(n)             !i: Input data
C--
      real av, std, stdm, amin, amax
Cbegin


      if ( ST_FAILED ) return

      call alimr ( x, n, amin, amax )

      call aavgr ( x, n, av, std )
      stdm = 0.0
      if ( n.ge.3 ) stdm = std/(real(n)-2.0)

      call pargi ( n )
      call pargr ( amin )
      call pargr ( amax )
      call pargr ( av )
      call pargr ( std )
      call pargr ( stdm )
      call printd ('No of entries = %d : Minimum = %f : Maximum = %f ')
      call printd ('Mean = %f : Std dev = %f : Std dev of mean = %f ')


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_DOSTAT2 -- Do the calcs for two columns
C
C    a j penny                      ral                  1991 may

      subroutine tbsh_dostat2 ( x, y, n )

      implicit none
      include 'STARMAN_INC'

      integer  n                !i: No of points
      real     x(n)             !i: Input data
      real     y(n)             !i: Input data
C--
      double precision an, sx, sy, ssx, ssy, sxy, tx, ty,
     +                 a, b, c, sd, sda, sdb, corrl, ax, ay
      real    rv
      integer k
Cbegin


      if ( ST_FAILED ) return

      sx = 0.0d0
      sy = 0.0d0
      ssx = 0.0d0
      ssy = 0.0d0
      sxy = 0.0d0
      tx = 0.0d0
      ty = 0.0d0
      an = n
      if ( n.ge.1 ) then
         do k = 1, n
            tx = tx + x(k)
            ty = ty + y(k)
         enddo
         ax = tx/an
         ay = ty/an
          do k = 1, n
            sx = sx + (dble(x(k))-ax)
            sy = sy + (dble(y(k))-ay)
            ssx = ssx + (dble(x(k))-ax)*(dble(x(k))-ax)
            ssy = ssy + (dble(y(k))-ay)*(dble(y(k))-ay)
            sxy = sxy + (dble(x(k))-ax)*(dble(y(k))-ay)
         enddo
         b = sxy/ssx
         a = ay - b*ax
      else
         a = 0.0d0
         b = 0.0d0
      endif

      corrl = 0.0d0
      if ( n.ge.2 ) then
         if ( ssx.gt.0.0d0 .and. ssy.gt.0.0d0 ) then
            corrl = sxy/sqrt(ssx*ssy)
         endif
      endif

      sd = 0.0d0
      sda = 0.0d0
      sdb = 0.0d0
      if ( n.ge.3 ) then
         c = (ssy-((sxy*sxy)/ssx))/(an-2.0d0)
         if ( c.gt.0.0d0 ) then
            sd = sqrt(c)
         else
            sd = 0.0d0
         endif
         sdb = sd/sqrt(ssx)
         sda = sd*((1.0d0/an)+((ax*ax)/ssx))
      endif

      call printo ( 'Y = A + BX  :assuming X errors = 0' )
      rv = real(a)
      call pargr ( rv )
      rv = real(sda)
      call pargr ( rv )
      rv = real(b)
      call pargr ( rv )
      rv = real(sdb)
      call pargr ( rv )
      call printd  ( 'A = %f +/- %f : B = %f +/- %f ' )
      rv = real(sd)
      call pargr ( rv )
      call printd ( 'Std dev of a Y value from line = %f ' )
      rv = real(corrl)
      call pargr ( rv )
      call printd ( 'Correlation coefficent = %f ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_INSERT -- Insert a set of data
C
C   alan penny             RAL               1995 Aug

      subroutine tbsh_insert ( data )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real    data(TBXM,TBYM)		!i/o: Table
C--
      integer nxs, nxe, nys, nye, kx, ky, j, k, ja, ka
      real x, y, rv, rva
      logical more
Cbegin


      if ( ST_FAILED ) return

      call printo ( '  ' )
      call printo ( '  You can insert single numbers by clicking'//
     +              ' on them directly.' )
      call printo ( '  ' )
      call printo ( '  This option lets you insert a set of '//
     +               'numbers. Use cursor to')
      call printo ( '  set top left hand row and column corner of '//
     +               'the set and then ' )
      call printo ( '  again to specify the bottom right hand '//
     +               'corner. Then through the ' )
      call printo ( '  keyboard enter values, one at a time' )
      call printo ( '  ' )
      call printo ( '  The data are entered from the top down. '//
     +              'To stop, enter - !! -' )
      call printo ( '  ' )

      call tbsh_getpos ( 3, nxs, nys )					!Get 1st corner
      if ( nxs.eq.-1 ) return

      call tbsh_getpos ( 3, nxe, nye )				!Get 2nd corner
      if ( nxe.eq.-1 ) return

      call tbsh_wmess ( 3, 0 )

      call cswopi ( nxs, nxe )
      call cswopi ( nys, nye )

      call tbsh_bconvert ( nxs, 1, %val(IPX), %val(IPY) )
      call tbsh_bconvert ( nxe, 1, %val(IPX), %val(IPY) )
      call tbsh_bconvert ( nys, 2, %val(IPX), %val(IPY) )
      call tbsh_bconvert ( nye, 2, %val(IPX), %val(IPY) )

      more = .true.
      k = nys - 1
      do while ( more .and. k.lt.nye )
         k = k + 1
         ka = k
         call tbsh_convert ( ka, 2, %val(IPX), %val(IPY) )
         j = nxs - 1
         do while ( more .and. j.lt.nxe )
            j = j + 1
            call pargi ( j )
            call pargi ( k )
            call printd ( '  Column %d  : Row %d ' )

            ja = j
            call tbsh_convert ( ja, 1, %val(IPX), %val(IPY) )
            rv = data(ja,ka)
            call get1r ( 'VALUE', rva, rv, -1.0e20, 1.0e20 )
            if ( ST_FAILED )  then
               ST_FAILED = .false.
               more = .false.
               call printo ( 'Taken here as end of insertion, '//
     +                       'not exit from program' )
            endif
            data(ja,ka) = rva

            x = XDOFF + 0.05*XBOX
            y = 10.0 + 0.5*CHYSIZE + real(NUMY-1)*YBOX
            kx = ja
            call tbsh_bconvert ( kx, 1, %val(IPX), %val(IPY) )
            x = x + real(kx-KXSTART)*XBOX
            ky = ka
            call tbsh_bconvert ( ky, 2, %val(IPX), %val(IPY) )
            y = y - real(ky-KYSTART)*YBOX
            call gd_bbuf
            call tbsh_value ( x, y, rva, 10, 2 )
            call gd_ebuf
            call gd_updt

         enddo
      enddo

      call tbsh_wmess ( 4, 0 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_HIST -- Plot histogram of a column
C
C   alan penny             RAL               1995 Aug

      subroutine tbsh_hist ( tbh )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer    tbh(5,TBXM)	!i: Header table
C--
      integer ncol, nrow, ipa, ipb, ierr
      character head*20, cv*72
Cbegin


      if ( ST_FAILED ) return

      call tbsh_getpos ( 1, ncol, nrow )				!Get column number to work on
      if ( ncol.eq.-1 ) return

      call tbsh_wmess ( 3, 0 )

      call gtwrkr ( 'A', TBY, ipa, ierr )				!Get working space for sort
      if ( ierr.ne.0 ) return

      call tbsh_loadcol ( %val(IPD), %val(IPX),	%val(IPY), ncol,	!Remove holes
     +                    %val(ipa) )
      call tbsh_chget ( tbh(1,ncol), head )

      call printo  ( ' ' )
      call pargi ( ncol )
      call pargc ( head )
      call printd ( 'Column - number %d : Header %c' )
      call printo  ( ' ' )

      call tbsh_hista ( %val(ipa), TBY, head )

      call wrkcan ( 'A' )

      call tbsh_wmess ( 4, 0 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_HISTA -- Set up and start histogram
C
C   a j penny                 ral                  1995 Aug

      subroutine tbsh_hista ( data, nx, head )

      implicit none

      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer       nx		!i: No of data points
      real          data(nx)	!i: Data
      character*20  head	!i: Data header
C--
      integer k, nbin
      real ymin, ymax
      logical ok
Cbegin


      if ( ST_FAILED ) return

      ymin = data(1)
      ymax = data(1)
      do k = 1, nx
         ymin = min(ymin,data(k))
         ymax = max(ymax,data(k))
      enddo

      call get2r ( 'HRANGE', ymin, ymax, .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return
      call cswopr ( ymin, ymax, )

      nbin = 50
      call get1i ( 'NBIN', nbin, nbin, 1, 20000 )
      if ( ST_FAILED ) return

      call pgsci ( 1 )
      call pghist ( nx, data, ymin, ymax, nbin, 0 )
      call pglabel ( head, 'Number', ' ' )
      call gd_updt

      call printo ( '  ' )
      call printo ( 'If seen enough of plot, answer this question' )
      call get1b ( 'OK', ok, .true. )					!Seen the histogram?

      call pgpage

      call pgvport ( 0.0, 1.0, 0.0, 1.0 )
      call pgwindow ( 1.0, 1000.0, 1.0, 1000.0 )
      call tbsh_formload


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_PLOT -- Plot two columns
C
C   alan penny             RAL               1995 Aug

      subroutine tbsh_plot ( tbh )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer    tbh(5,TBXM)	!i: Header table
C--
      integer ncol, nrow, ncol2, nrow2, ipa, ipb, ierr
      character head*20, head2*20
Cbegin


      if ( ST_FAILED ) return

      call tbsh_getpos ( 1, ncol, nrow )				!Get 1st column number to work on
      if ( ncol.eq.-1 ) then
         ncol = 1
         ncol2 = 2
         call get2i ( 'COLS', ncol, ncol2, .true., 1, TBX )
         call tbsh_convert ( ncol, 1, %val(IPX), %val(IPY) )
         call tbsh_convert ( ncol2, 1, %val(IPX), %val(IPY) )
      else
         call tbsh_getpos ( 1, ncol2, nrow2 )				!Get 2nd column number to work on, if wanted
         if ( ncol2.eq.-1 ) return
      endif

      call tbsh_wmess ( 3, 0 )

      call gtwrkr ( 'A', TBY, ipa, ierr )				!Get working space for sort
      if ( ierr.ne.0 ) return

      call gtwrkr ( 'B', TBY, ipb, ierr )				!Get working space for sort
      if ( ierr.ne.0 ) then
         call wrkcan ( 'A' )
         return
      endif

      call tbsh_loadcol ( %val(IPD), %val(IPX),	%val(IPY), ncol,	!Remove holes
     +                    %val(ipa) )
      call tbsh_chget ( tbh(1,ncol), head )
      call tbsh_loadcol ( %val(IPD), %val(IPX),	%val(IPY), ncol2,	!Remove holes
     +                    %val(ipb) )
      call tbsh_chget ( tbh(1,ncol2), head2 )

      call printo  ( ' ' )
      call pargi ( ncol )
      call pargc ( head )
      call printd ( '1st column - number %d : Header %c' )
      call printo  ( ' ' )
      call pargi ( ncol2 )
      call pargc ( head2 )
      call printd ( '2nd column - number %d : Header %c' )
      call printo ( ' ' )

      call tbsh_plota ( %val(ipa), %val(ipb), TBY, head, head2 )

      call wrkcan ( 'A' )
      call wrkcan ( 'B' )

      call tbsh_wmess ( 4, 0 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_PLOTA -- Set up and start plotting two columns
C
C   a j penny                 ral                  1995 Aug

      subroutine tbsh_plota ( data, data2, nx, head, head2 )

      implicit none

      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer       nx		!i: No of data points
      real          data(nx)	!i: Data
      real          data2(nx)	!i: Data 2
      character*20  head	!i: Data header
      character*20  head2	!i: Data 2 header
C--
      integer j, kch, k, ka, kopt
      real xmin, xmax, ymin, ymax, x, y, ysmin, ysmax, xsmin, xsmax
      logical aspect, ok
Cbegin


      if ( ST_FAILED ) return

      xmin = data(1)
      xmax = data(1)
      do k = 1, nx
         xmin = min(xmin,data(k))
         xmax = max(xmax,data(k))
      enddo
      ymin = data2(1)
      ymax = data2(1)
      do k = 1, nx
         ymin = min(ymin,data2(k))
         ymax = max(ymax,data2(k))
      enddo

      call get2r ( 'XRANGE', xmin, xmax, .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return
      call cswopr ( xmin, xmax)

      call get2r ( 'YRANGE', ymin, ymax, .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return
      call cswopr ( ymin, ymax )

      xsmin = xmin - 0.05*(xmax-xmin)
      xsmax = xmax + 0.05*(xmax-xmin)
      ysmin = ymin - 0.05*(ymax-ymin)
      ysmax = ymax + 0.05*(ymax-ymin)

      call get2r ( 'DEVLIMX', xsmin, xsmax, .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return

      call get2r ( 'DEVLIMY', ysmin, ysmax, .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return

      call get1b ( 'ASPECT', aspect, .false. )
      ka = 0
      if ( aspect ) ka = 1

      call pgsci ( 1 )

      call gd_dobox ( xsmin, xsmax, head, ysmin, ysmax, head2, ' ', ka )

      call get_job ( 'PTYPE', 'line:points', kopt, 2, ' ', 0 )

      if ( kopt.eq.2 ) then
         call get1i ( 'SYMBOL', kch, 2, 0, 31 )
         if ( ST_FAILED ) return
         j = 0
         do while ( j.lt.nx )
            j = j + 1
            x = data(j)
            y = data2(j)
            if ( x.ge.xmin .and. x.le.xmax .and. y.ge.ymin
     +           .and. y.le.ymax ) call pgpoint ( 1, x, y, kch )
         enddo
      else
         call get1i ( 'LSTYLE', kch, 1, 1, 5 )
         if ( ST_FAILED ) return
         call pgsls ( kch )
         x = data(1)
         y = data2(1)
         call pgmove ( x, y )
         j = 1
         do while ( j.lt.TBY )
            j = j + 1
            x = data(j)
            y = data2(j)
            call pgdraw ( x, y )
         enddo
         call pgsls ( 1 )
      endif

      call gd_updt

      call printo ( '  ' )
      call printo ( 'If seen enough of plot, answer this question' )
      call get1b ( 'OK', ok, .true. )					!Seen the histogram?

      call pgpage

      call pgvport ( 0.0, 1.0, 0.0, 1.0 )
      call pgwindow ( 1.0, 1000.0, 1.0, 1000.0 )
      call tbsh_formload


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_COMPRESS -- Remove 'holes' in array
C
C  alan penny                 ral                1991 July

      subroutine tbsh_compress ( tb, tbn, tbh, kcx, kcy )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real      tb(TBXM,TBYM)		!i/o: Data table
      real      tbn(5,TBYM)		!i/o: Name table
      integer   tbh(5,TBXM)		!i/o: Header table
      integer   kcx(TBXM)		!i/o: Column checks
      integer   kcy(TBYM)		!i/o: Row checks
C--
      integer j, ja, k
Cbegin


      if ( ST_FAILED ) return

      k = TBY + 1							!Compress in Y
      do while ( k.gt.1 )
         k = k - 1
         if ( kcy(k).eq.0 ) then
            if ( k.ne.TBY ) then
               do j = k, TBY-1
                  do ja = 1, TBX
                     tb(ja,j) = tb(ja,j+1)
                  enddo
                  do ja = 1, 5
                     tbn(ja,j) = tbn(ja,j+1)
                  enddo
               enddo
            endif
            TBY = TBY - 1
            if ( k.lt.(KYSTART-1) ) KYSTART = KYSTART - 1
         endif
      enddo

      k = TBX + 1							!Compress in X
      do while ( k.gt.1 )
         k = k - 1
         if ( kcx(k).eq.0 ) then
            if ( k.ne.TBX ) then
               do j = k, TBX-1
                  do ja = 1, TBY
                     tb(j,ja) = tb(j+1,ja)
                  enddo
                  do ja = 1, 5
                     tbh(j,ja) = tbh(j+1,ja)
                  enddo
               enddo
            endif
            TBX = TBX - 1
            if ( k.lt.(KXSTART-1) ) KXSTART = KXSTART - 1
         endif
      enddo

      call azeroi ( kcx, TBXM )
      call azeroi ( kcy, TBYM )
      call amovki ( 1, kcx, TBX )
      call amovki ( 1, kcy, TBY )

      RESTORE = .false.							!Delete memory lost
      CHANGED = .true.

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_LOADCOL -- Load a table column into a 'no-holes' vector
C
C  alan penny                 ral                1991 July

      subroutine tbsh_loadcol ( tb, kcx, kcy, ncol, out )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real      tb(TBXM,TBYM)		!i: Data table
      integer   kcx(TBXM)		!i: Column checks
      integer   kcy(TBYM)		!i: Row checks
      integer   ncol			!i: Column to load
      real      out(TBY)		!o: Output vector
C--
      integer k, ka
Cbegin


      if ( ST_FAILED ) return

      ka = 0
      do k = 1, TBYM
         if ( kcy(k).eq.1 ) then
            ka = ka + 1
            out(ka) = tb(ncol,k)
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_SLOADB -- Reverse order of values in a real number vector
C
C  alan penny                 ral                1991 July

      subroutine tbsh_sloadb ( data, num )

      implicit none
      include 'STARMAN_INC'

      integer   num             !i: Number of values
      real	data(num)	!i/o: Array
C--
      integer k, ka, ks, ke
      real va
Cbegin


      if ( ST_FAILED ) return

      if ( num.le.1 ) return

      ka = num/2
      ks = 1
      ke = num
      do k = 1, ka
         va = data(ke)
         data(ke) = data(ks)
         data(ks) = va
         ke = ke - 1
         ks = ks + 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_TRANS -- Load output with sorted input
C
C  alan penny                 ral                1991 July

      subroutine tbsh_trans ( tb, tbn, tba, tbna, num )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real      tb(TBXM,TBYM)	!i: Data table
      real      tbn(5,TBYM)	!i: Name table
      real      tba(TBXM,TBY)	!i: Working Data table
      real      tbna(5,TBY)	!i: Working Name table
      real      num(TBY)	!i: Position in input table of a row in output table
C--
      integer j, k, ka
Cbegin


      if ( ST_FAILED ) return

      do k = 1, TBY
         ka = nint(num(k))
         do j = 1, TBXM
            tba(j,k) = tb(j,ka)
         enddo
         do j = 1, 5
            tbna(j,k) = tbn(j,ka)
         enddo
      enddo

      call amovr ( tba, tb, TBXM*TBY )
      call amovr ( tbna, tbn, 5*TBY )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_RENUM -- Renumber names
C
C   alan penny             RAL               1991 July

      subroutine tbsh_renum ( tbn, kcy )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real       tbn(5,TBYM)		!i/o: Name table
      integer    kcy(TBYM)		!i: Row checks
C--
      integer k, ka
      character*20 id
Cbegin


      if ( ST_FAILED ) return

      ka = 0
      do k = 1, TBY
         if ( kcy(k).eq.1 ) then
            ka = ka + 1
            write ( id, '(i20)' ) ka
            id(1:1) = '#'
            call lbgone(id(2:))
            call tbsh_chput ( id, tbn(1,k) )
         endif
      enddo

      call tbsh_nload ( tbn, kcy )

      RESTORE = .false.
      CHANGED = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_MOVE -- Move table location on screen
C
C   alan penny             RAL               1991 July

      subroutine tbsh_move ( kopt, koptst )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer   kopt		!i: Move option (values=koptst-1 to koptst+7)
      integer   koptst		!i: 1st move option
C--
      integer kxo, kyo, klim, kdo
Cbegin


      if ( ST_FAILED ) return

      kxo = KXSTART
      kyo = KYSTART
      kdo = kopt - koptst

      if ( kdo.eq.1 ) then
         klim = max(TBXR,TBYR)
         call get2i ( 'POSITION', KXSTART, KYSTART, .true., 1, klim )
         if ( ST_FAILED ) return
         call tbsh_wmess ( 4, 0 )
      elseif ( kdo.eq.2 ) then
         KXSTART = KXSTART - 1
      elseif ( kdo.eq.3 ) then
         KXSTART = KXSTART + 1
      elseif ( kdo.eq.4 ) then
         KYSTART = KYSTART - 1
      elseif ( kdo.eq.5 ) then
         KYSTART = KYSTART + 1
      elseif ( kdo.eq.6 ) then
         KXSTART = KXSTART - NUMX
      elseif ( kdo.eq.7 ) then
         KXSTART = KXSTART + NUMX
      elseif ( kdo.eq.8 ) then
         KYSTART = KYSTART - NUMY
      elseif ( kdo.eq.9 ) then
         KYSTART = KYSTART + NUMY
      endif

      KXSTART = max(1,min(KXSTART,TBXR))
      KYSTART = max(1,min(KYSTART,TBYR))

      if ( kxo.ne.KXSTART ) call tbsh_hload ( %val(IPH), %val(IPX) )
      if ( kyo.ne.KYSTART ) call tbsh_nload ( %val(IPN), %val(IPY)  )
      if ( kxo.ne.KXSTART .or. kyo.ne.KYSTART  ) call tbsh_dload (
     +                                %val(IPD), %val(IPX), %val(IPY) )

      CHANGED = .true.
      RESTORE = .false.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_DEL -- Delete row(s)/col(s) from table
C
C   alan penny             RAL               1991 July

      subroutine tbsh_del ( kopt, kcx, kcy, koptst )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer   kopt		!i: Option (23=col;24=cols;
				!   25=row;26=rows)
      integer   kcx(TBXM)	!i/o: Column use flags
      integer   kcy(TBYM)	!i/o: Row use flags
      integer   koptst		!i: Option offset
C--
      integer k, kk, ns, ne, nrow, ncol
Cbegin


      if ( ST_FAILED ) return

      kk = kopt - koptst

      if ( kk.le.2 ) then						!Do for columns

         if ( kk.eq.1 ) then
            call tbsh_getpos ( 1, ns, nrow )
            if ( ns.eq.-1 ) return
            ne = ns
         else
            ns = 1
            ne = TBXR
            call tbsh_wmess ( 3, 0 )
            call get2i ( 'NUMCOLS', ns, ne, .true., 0, TBXR )
            if ( ST_FAILED ) return
            call tbsh_wmess ( 4, 0 )
            if ( ns.eq.0 .or. ne.eq.0 ) return
            call cswopi ( ns, ne )
            call tbsh_convert ( ns, 1, kcx, kcy )
            call tbsh_convert ( ne, 1, kcx, kcy )
         endif
         call tbsh_wmess ( 4, 0 )

         call amovki ( kcx, %val(IPXA), TBX )
         do k = ns, ne
            if ( kcx(k).eq.1 ) then
               kcx(k) = 0
               TBXR = TBXR - 1
            endif
         enddo
         call tbsh_hload ( %val(IPH), kcx )				!Load screen
         call tbsh_dload ( %val(IPD), kcx, kcy )
      endif

      if ( kk.gt.2 ) then						!Do for rows

         if ( kk.eq.3 ) then
            call tbsh_getpos ( 2, ncol, ns )
            if ( ns.eq.-1 ) return
            ne = ns
         else
            ns = 1
            ne = TBYR
            call tbsh_wmess ( 3, 0 )
            call get2i ( 'NUMROWS', ns, ne, .true., 0, TBYR )
            if ( ST_FAILED ) return
            if ( ns.eq.0 .or. ne.eq.0 ) return
            call cswopi ( ns, ne )
            call tbsh_convert ( ns, 2, kcx, kcy )
            call tbsh_convert ( ne, 2, kcx, kcy )
         endif
         call tbsh_wmess ( 4, 0 )

         call amovki ( kcy, %val(IPYA), TBY )
         do k = ns, ne
            if ( kcy(k).eq.1 ) then
               kcy(k) = 0
               TBYR = TBYR - 1
            endif
         enddo
         call tbsh_nload ( %val(IPN), kcy )				!Load screen
         call tbsh_dload ( %val(IPD), kcx, kcy )
      endif

      CHANGED = .true.
      RESTORE = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_ADD -- Add row(s)/col(s) to table
C
C   alan penny             RAL               1991 July

      subroutine tbsh_add ( kopt, koptst )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer   kopt		!i: Add option
      integer   koptst		!i: Offset to these options
C--
      logical addr, addc
      integer k, nrow, ncol
Cbegin


      if ( ST_FAILED ) return

      k = kopt - koptst
      addr = .false.
      addc = .false.

      if ( k.eq.2 .or. k.eq.4 ) addr = .true.
      if ( k.eq.1 .or. k.eq.3 ) addc = .true.

      if ( addr .and. TBY.eq.TBYM ) then
         call printo ( 'Cannot add more rows, too big' )
         call tbsh_wmess ( 4, 0 )
         return
      endif

      if ( addc .and. TBX.eq.TBXM ) then
         call printo ( 'Cannot add more columns, too big' )
         call tbsh_wmess ( 4, 0 )
         return
      endif


      if ( addr ) then
         call tbsh_getpos ( 2, ncol, nrow )
         if ( nrow.eq.-1 ) return
         call tbsh_wmess ( 3, 0 )
         call tbsh_addr ( k, nrow, %val(IPD), %val(IPN),
     +                    %val(IPX), %val(IPY) )
      endif

      if ( addc ) then
         call tbsh_getpos ( 1, ncol, nrow )
         if ( ncol.eq.-1 ) return
         call tbsh_wmess ( 3, 0 )
         call tbsh_addc ( k, ncol, %val(IPD), %val(IPH),
     +                    %val(IPX), %val(IPY) )
      endif

      call TBSH_wmess ( 4, 0 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_ADDR -- Add row(s) to a table - part 2
C
C   alan penny             RAL               1991 July

      subroutine tbsh_addr ( kopt, nrow, tb, tbn, kcx, kcy )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer   kopt			!i: Add(2)/Insert(4) flag
      integer   nrow			!i: Row to place new data at
      real      tb(TBXM,TBYM)		!i/o: Data table
      real      tbn(5,TBYM)		!i/o: Name table
      integer   kcx(TBXM)		!i/o: Column checks
      integer   kcy(TBYM)		!i/o: Row checks
C--
      integer j, k, ierr, ja, jj, kk, klim
      logical more
      character name*20
      real data(20)
Cbegin


      if ( ST_FAILED ) return

      k = nrow								!Row location
      if ( kopt.eq.4 ) k = k - 1

      call printo ( ' ' )
      call printo ( '    Up to 20 numbers can be specified')
      call printo ( '    The rest will be set to 0.0' )
      call printo ( '    If values for all the columns in the table')
      call printo ( '      are entered, the name may be as well')
      call printo ( '    Just input -return- for no entry' )
      call printo ( ' ' )
      more = .true.							!Insert lines, end with blank
      do while ( more )
         k = k + 1
         klim = min(20,TBXR)
         call tbsh_getline ( name, data, klim, k, ierr )
         if ( ierr.eq.0 ) then
            TBY = TBY + 1
            TBYR = TBYR + 1
            do kk = TBY, k+1, -1
               if ( kk.ne.0 ) then
                  do jj = 1, TBX
                     tb(jj,kk) = tb(jj,kk-1)
                  enddo
                  do jj = 1, 5
                     tbn(jj,kk) = tbn(jj,kk-1)
                  enddo
                  kcy(kk) = kcy(kk-1)
               endif
            enddo
            kcy(k) = 1
            call tbsh_chput ( name, tbn(1,k) )
            ja = 0
            do j = 1, TBX
               if ( kcx(j).eq.1 ) then
                  ja = ja + 1
                  if ( ja.gt.20 ) then
                     tb(j,k) = 0.0
                  else
                     tb(j,k) = data(ja)
                  endif
               endif
            enddo
CX            KYSTART = KYSTART + 1
            call tbsh_dload ( tb, kcx, kcy )
            call tbsh_nload ( tbn, kcy )
         else
            more = .false.
         endif

         if ( TBY.eq.TBYM .and. more ) then
            call printo ( 'Cannot add more rows, too big' )
            more = .false.
         endif

      enddo

      CHANGED = .true.
      RESTORE = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_ADDC -- Add col(s) to a table - part 2
C
C   alan penny             RAL               1991 July

      subroutine tbsh_addc ( kopt, ncol, tb, tbh, kcx, kcy )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      integer   kopt			!i: Add(1)/Insert(3) flag
      integer   ncol			!i: Column to place new data at
      real      tb(TBXM,TBYM)		!i/o: Data table
      integer   tbh(5,TBXM)		!i/o: Header table
      integer   kcx(TBXM)		!i/o: Column checks
      integer   kcy(TBYM)		!i/o: Row checks
C--
      integer nncol, num, ierr, ktbxo, j, k, ka, kd, ke
      real data(20)
      character*20 text
Cbegin


      if ( ST_FAILED ) return

      if ( TBX.eq.TBXM ) then
         call printo (
     +        'ERROR: Cannot add more columns, table is too big' )
         return
      endif

      nncol = ncol							!Column location
      if ( kopt.eq.3 ) nncol = nncol - 1

      call printo ( ' ' )
      call printo ( '    Up to 20 numbers can be specified')
      call printo ( '    The number of numbers entered will be ' )
      call printo ( '       the number of columns added' )
      call printo ( ' ' )

      call tbsh_getvals ( data, 20, num, ierr )

      if ( ierr.ne.0 .or. num.eq.0 ) then
         return
      endif

      if ( (TBX+num).gt.TBXM ) then
         call pargi ( num )
         call printd ( 'Cannot add %d columns, table is too big' )
         num = TBXM - TBX
         call pargi ( num )
         call printd ( 'Will add first %d columns' )
      endif

      ktbxo = TBX
      TBX = TBX + num
      TBXR = TBXR + num

      if ( nncol.lt.ktbxo ) then						!shift columns
         kd = ktbxo - nncol
         ke = nncol + 1
         do k = ktbxo, ke, -1
            ka = k + num
            do j = 1, TBY
               tb(ka,j) = tb(k,j)
            enddo
            do j = 1, 5
               tbh(j,ka) = tbh(j,k)
            enddo
            kcx(ka) = kcx(k)
         enddo
         text = ' '
         do k = 1, num
            ka = nncol + k
            do j = 1, TBY
               tb(ka,j) = 0.0
            enddo
            kcx(ka) = 1
            call tbsh_chput ( text, tbh(1,ka) )
         enddo
         call tbsh_hload ( %val(IPH), %val(IPX) )
         call tbsh_dload ( %val(IPD), %val(IPX), %val(IPY) )
      endif

      do k = 1, num							!Load numbers
         ka = nncol + k
         do j = 1, TBY
            tb(ka,j) = data(k)
         enddo
         kcx(ka) = 1
      enddo
      call tbsh_dload ( %val(IPD), %val(IPX), %val(IPY) )

      do k = 1, num							!Load headers
         ka = nncol + k
         call pargi ( ka )
         call printd ( 'Header for column %d ' )
         call get1c ( 'HEADER', text, ' ', .true. )
         if ( ST_FAILED ) return
         call tbsh_chput ( text, tbh(1,ka) )
         call tbsh_hload ( %val(IPH), %val(IPX) )
      enddo

      CHANGED = .true.
      RESTORE = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_GETLINE -- Input a line of data (with optional name at end)
C
C  alan penny                      ral              1991 July

      subroutine tbsh_getline ( name, data, msize, ndef, ierr )

      implicit none
      include 'STARMAN_INC'

      character*20   name		!o: Name
      integer        msize		!i: Max number of entries (excl name
      real           data(msize)	!o: Data
      integer        ndef		!i: Default number in name
      integer        ierr		!o: Error flag (0=ok:1=end of data)
C--
      character*80  inbuf(21)
      character*4 text
      integer j, k, istat, numval
      logical more
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      if ( msize.gt.20 ) then
         call printo ( 'ERROR: Line too big in s/r TBSH_GETLINE' )
         call printo ( 'ERROR:   Consult program author' )
         return
      endif

      more = .true.
      k = 0
      do while ( more .and. k.lt.3 )					!Try 3 times for correct data
         k = k + 1
         more = .false.

         do j = 1, msize+1						!Get data
            inbuf(j) = ' '
         enddo
         call getnc ( 'DATA', inbuf, msize+1, numval, .true. )
         if ( ST_FAILED ) return
         do j = 1, msize+1							!Remove leading blanks
            call lbgone ( inbuf(j) )
         enddo

         istat = 0							!All blank entry?
         if ( numval.gt.0 ) then
            do j = 1, numval
               if ( inbuf(j).ne.' ' ) istat = 1
            enddo
         endif
         if ( istat.eq.0 ) then
            ierr = 1
         else
            do j = 1, min(numval,msize)					!Load numbers
               call chartor ( inbuf(j), data(j), istat )
               if ( istat.ne.0 ) then
                  write ( text, '(1x,i3)' ) j
                  call printo ( ' ERROR: Number '//
     +                          text//' is not a real number' )
                  data(j) = 0.0
                  more = .true.
               endif
            enddo
            if ( numval.lt.msize ) then
               do j = numval+1, msize
                  data(j) = 0.0
               enddo
            endif

            if ( numval.gt.msize ) then				!Load name or default
               name = inbuf(msize+1)
            else
               name(1:1) = '#'
               write ( name(2:), '(1x,i18)' ) ndef
               call lbgone ( name(2:) )
            endif

         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_GETVALS -- Input a row of values
C
C  alan penny                      ral              1991 July

      subroutine tbsh_getvals ( data, msize, num, ierr )

      implicit none
      include 'STARMAN_INC'

      integer        msize		!i: Max number of values
      real           data(msize)	!o: Data
      integer        num		!o: Number of values
      integer        ierr		!o: Error flag (0=ok:1=end of data)
C--
      character*80  inbuf(20)
      character*4 text
      integer j, k, istat, numval
      logical more
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      num = 0

      if ( msize.gt.20 ) then
         call printo ( 'ERROR: Line too big in s/r TBSH_GETVALS' )
         call printo ( 'ERROR:   Consult your system manager' )
         return
      endif

      do j = 1, msize							!Get data
         inbuf(j) = ' '
      enddo
      call getnc ( 'DATA', inbuf, msize, num, .true. )
      if ( ST_FAILED ) return
      if ( num.eq.0 ) return

      do j = 1, num							!Remove leading blanks
         call lbgone ( inbuf(j) )
      enddo

      istat = 0								!All blank entry?
      do j = 1, num
         if ( inbuf(j).ne.' ' ) istat = 1
      enddo
      if ( istat.eq.0 ) then
         ierr = 1
      else
         do j = 1, num							!Load numbers
            call chartor ( inbuf(j), data(j), istat )
            if ( istat.ne.0 ) then
               write ( text, '(1x,i3)' ) j
               call printo ( ' ERROR: Number '//
     +                          text//' is not a real number' )
               data(j) = 0.0
               more = .true.
            endif
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_UNDDO -- Restore last change (if possible)
C
C   alan penny             RAL               1991 July

      subroutine tbsh_undo ( )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( .not.RESTORE ) then						!Chack if can do
         call printo ( 'Cannot restore last change - '//
     +                  'table changed in complex way' )
         return
      endif

      call amovki ( %val(IPXA), %val(IPX), TBXM )			!Restore last setup
      call amovki ( %val(IPYA), %val(IPY),  TBYM )

      call tbsh_dload ( %val(IPD), %val(IPX), %val(IPY) )		!Load screen
      call tbsh_nload ( %val(IPN), %val(IPY) )
      call tbsh_hload ( %val(IPH), %val(IPX) )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_VALUE -- Paint value up
C
C    alan penny           ral                       1991 July

      subroutine tbsh_value ( x, y, val, len, kc )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real      x		!i: X posn
      real      y		!i: Y posn
      real      val		!i: Value to paint up
      integer   len		!i: No of characters to put up
      integer   kc		!i: Colour code
C--
      integer j, ja, k, kl, lens, jb, iv
      real xa(5), ya(5), dval, rval
      logical more
      character text*70, atc*12
      external lens
Cbegin


      if ( ST_FAILED ) return

      xa(1) = x - 0.1*CHXSIZE						!Write background
      ya(1) = y - 0.2*CHYSIZE
      xa(2) = xa(1)
      ya(2) = ya(1) + 0.9*CHYSIZE
      xa(3) = xa(1) + real(len)*CHXSIZE
      ya(3) = ya(2)
      xa(4) = xa(3)
      ya(4) = ya(1)
      xa(5) = xa(1)
      ya(5) = ya(1)
      call pgsci ( 0 )
      call pgpoly ( 5, xa, ya )

      rval = val

      if ( rval.eq.0.0 ) then
         text = '0.0'
      elseif ( abs(rval).gt.1.0e10 .or. abs(rval).lt.1.0e-3 ) then
         write ( text, '(g17.5)' ) rval
      else
         write ( text, '(f20.7)' ) rval
         j = 15 + max(0.0,alog10(1000000.0/abs(rval)))
         if ( rval.lt.10.0 ) j = j - 1
         if ( j.lt.20 ) text(j:) = ' '
      endif

      call lbgone ( text )
      kl = min(len,lens(text))
      atc = text(1:kl)

      jb = kl
      ja = index ( atc(1:kl), 'E' )
      if ( ja.ne.0 ) jb = ja - 1

      if ( atc(jb-2:jb).eq.'999' .and. ja.eq.0 ) then			!Deal with (e.g.) 5.59999
         iv = index(atc(1:jb),'.')
         if ( iv.ne.0 .and. atc(iv+1:iv+1).ne.'9' ) then
            dval = 0.5*sign(1.0,rval)*10.0**(iv-jb)
            rval = rval + dval
            write ( text, '(f20.7)' ) rval
            j = 15 + max(0.0,alog10(1000000.0/abs(rval)))
            if ( rval.lt.10.0 ) j = j - 1
            if ( j.lt.20 ) text(j:) = ' '
            call lbgone ( text )
            kl = min(len,lens(text))
            atc = text(1:kl)
            jb = kl
            ja = index ( atc(1:kl), 'E' )
            if ( ja.ne.0 ) jb = ja - 1
         endif
      endif

      j = jb								!Remove trailing '0's
      more = .true.
      do while ( more )
         if ( atc(j:j).eq.'0' .and. atc((j-1):(j-1)).ne.'.' ) then
            atc(j:j) = ' '
            j = j - 1
            if ( ja.ne.0 ) then
               do k = 1, 4
                  atc(j+k:j+k) = atc(j+k+1:j+k+1)
               enddo
               atc(j+5:j+5) = ' '
            endif
         else
            more = .false.
         endif
      enddo

      call pgsci ( kc-1 )						!Write number
      call pgtext ( x, y, atc )

      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_TEXT -- Paint text up
C
C    alan penny           ral                       1991 July

      subroutine tbsh_text ( x, y, text, len, kc )

      implicit none
      include 'tbsheet.inc'
      include 'STARMAN_INC'

      real           x          !i: X posn
      real           y          !i: Y posn
      character*(*)  text	!i: Value to paint up
      integer        len	!i: No of characters to put up
      integer        kc		!i: Colour code
C--
      real xa(5), ya(5)
Cbegin


      if ( ST_FAILED ) return

      xa(1) = x - 0.1*CHXSIZE
      ya(1) = y - 0.3*CHYSIZE
      xa(2) = xa(1)
      ya(2) = ya(1) + CHYSIZE
      xa(3) = xa(1) + real(len)*CHXSIZE
      ya(3) = ya(2)
      xa(4) = xa(3)
      ya(4) = ya(1)
      xa(5) = xa(1)
      ya(5) = ya(1)
      call pgsci ( 0 )
      call pgpoly ( 5, xa, ya )

      call pgsci ( kc-1 )
      call pgtext ( x, y, text(1:len) )

      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_NAMCLEAR -- Clear a character array
C
C a j penny             stsci                  1987-03-22

      subroutine tbsh_namclear ( tb, n )

      implicit none
      include 'STARMAN_INC'

      integer	n		!i: No of characters in array
      integer	tb(n/4)		!o: The array
C--
      integer k
      byte asc(4)
      integer kt
      equivalence ( kt, asc(1) )
Cbegin


      if ( ST_FAILED ) return

      asc(1) = ichar(' ')
      asc(2) = ichar(' ')
      asc(3) = ichar(' ')
      asc(4) = ichar(' ')
      do k = 1, n/4
         tb(k) = kt
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_LINE -- Plot a line
C
C    alan penny                ral              1991 July

      subroutine tbsh_line ( xs, ys, xe, ye, kc )

      implicit none
      include 'STARMAN_INC'

      real    xs		!i: X start
      real    ys		!i: Y start
      real    xe		!i: X end
      real    ye		!i: Y end
      integer kc		!i: Colour code
C--
      real x(2), y(2)
Cbegin


      if ( ST_FAILED ) return

      x(1) = xs
      x(2) = xe
      y(1) = ys
      y(2) = ye
      call pgsci ( kc-1 )
      call pgline ( 2, x, y )

      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_BOX -- Plot a box
C
C    alan penny                ral              1991 July

      subroutine tbsh_box ( xs, ys, xe, ye, kc )

      implicit none
      include 'STARMAN_INC'

      real    xs		!i: X start
      real    ys		!i: Y start
      real    xe		!i: X end
      real    ye		!i: Y end
      integer kc		!i: Colour code
C--
      real x(5), y(5)
Cbegin


      if ( ST_FAILED ) return

      x(1) = xs
      x(2) = xs
      x(3) = xe
      x(4) = xe
      x(5) = xs
      y(1) = ys
      y(2) = ye
      y(3) = ye
      y(4) = ys
      y(5) = ys
      call pgsci ( kc-1 )
      call pgline ( 5, x, y )

      call gd_updt


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_CHGET -- Get a 20 character string from 5 integers
C
C    alan penny           ral                       1991 July

      subroutine tbsh_chget ( kin, text )

      implicit none
      include 'STARMAN_INC'

      integer        kin(5)	!i: Input
      character*20   text	!o: Output
C--
      integer j, k, kt
      byte kb(4)
      equivalence ( kt, kb(1) )
Cbegin


      if ( ST_FAILED ) return

      do k = 1, 5
         kt = kin(k)
         j = 1 + (k-1)*4
         text(j:j) = char(kb(1))
         text(j+1:j+1) = char(kb(2))
         text(j+2:j+2) = char(kb(3))
         text(j+3:j+3) = char(kb(4))
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSH_CHPUT -- Get 5 integers from a 20 character string
C
C    alan penny           ral                       1991 July

      subroutine tbsh_chput ( text, kout )

      implicit none
      include 'STARMAN_INC'

      character*20   text	!i: Input
      integer        kout(5)	!0: Output
C--
      integer j, k, kt
      byte kb(4)
      equivalence ( kb(1), kt )
Cbegin


      if ( ST_FAILED ) return

      do k = 1, 5
         j = 1 + (k-1)*4
         kb(1) = ichar(text(j:j))
         kb(2) = ichar(text(j+1:j+1))
         kb(3) = ichar(text(j+2:j+2))
         kb(4) = ichar(text(j+3:j+3))
         kout(k) = kt
      enddo


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    TBSORT.FOR
C
C    Contains:-
C
C T_TBSORT   Sort entries in a table
C TBSO_XLOADA  Load real number increasing array
C TBSO_XLOADB  Reverse real number array
C TBSO_TRANS   Load output with sorted input


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBSORT -- Sort entries in a table
C   For a fuller description see TBSORT.HLP
C
C   alan penny                     ral           1990-06-15

      subroutine t_tbsort ( )

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer ipin, ipo, ierr, ipnum, ipval, kacc, kpar, iv, ierr1
      character title*30, text*72
Cbegin


      call optabr ( 'IN', ipin, TBVX, TBY, .false., ierr )		!Obtain input table
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return
      TBX = TBVX - 5

      call get1i ( 'NUMCOL', kpar, 1, 1, TBX )				!Get parameter number to sort on
      if ( ST_FAILED ) return
      kpar = kpar + 5

      call get_job ( 'OPTION', 'ascending:descending', kacc, 1, text,0)	!Get ascending or descending
      if ( ST_FAILED ) return

      call gtwrkr ( 'NUMBER', TBY, ipnum, ierr )			!Get working space for sort
      call gtwrkr ( 'VALUES', TBY, ipval, ierr1 )
      if ( ierr.ne.0 .or. ierr1.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call coprr ( %val(ipin), TBVX, TBY, kpar, kpar, 1, TBY,		!Copy the sorting parameter into
     +             %val(ipval), 1, TBY, 1, 1 )				! the working area and perform sort
      call tbso_xloada ( %val(ipnum) )
      call sort2r ( %val(ipval), %val(ipnum), TBY )
      if ( kacc.eq.2 ) call tbso_xloadb ( %val(ipnum) )

      call optabw ( 'OUT', ipo, TBVX, TBY, .false., ierr )		!Open output table
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call tcopdes ( 'IN', 'OUT', ierr )				!Get title to output list and
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return
      call gtdesc ( 'IN', 'TITLE', title, 'Output from Tbsort',iv,ierr)	! store it and the descriptors
      call get1c  ( 'TITLE', title, title, .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUT', 'TITLE', title )

      call tbso_trans ( %val(ipin), %val(ipnum), %val(ipo) )		!Load the Output from the Input


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSO_XLOADA -- Load real number increasing array
C
C  alan penny                 ral                1990-06-15

      subroutine tbso_xloada ( out )

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real	out(TBY)	!o: Aarray of real numbers (1.0,2.0,..)
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      do k = 1, TBY
         out(k) = k
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSO_XLOADB -- Reverse real number array
C
C  alan penny                 ral                1990-06-15

      subroutine tbso_xloadb ( data )

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real	data(TBY)	!i/o: Array
C--
      integer k, ka, ks, ke
      real va
Cbegin


      if ( ST_FAILED ) return

      if ( TBY.le.1 ) return

      ka = TBY/2
      ks = 1
      ke = TBY
      do k = 1, ka
         va = data(ke)
         data(ke) = data(ks)
         data(ks) = va
         ke = ke - 1
         ks = ks + 1
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSO_TRANS -- Load output with sorted input
C
C  alan penny                 ral                1990-06-15

      subroutine tbso_trans ( in, num, out )

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real	in(TBVX,TBY)	!i: Input table
      real	num(TBY)	!i: Position in input table of a row in output table
      real	out(TBVX,TBY)	!o: Output table
C--
      integer k, ka
Cbegin


      if ( ST_FAILED ) return

      do k = 1, TBY
         ka = nint(num(k))
         call coprr ( in, TBVX, TBY, 1, TBVX, ka, ka,
     +                out, TBVX, TBY, 1, k )
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C This is TBSTAT.F
C
C It contains:-
C
C T_TBSTAT       Get statistics for one or two columns in tables
C TBST_SETUP     Load the default parameters
C TBST_GETDATA   Get Table
C TBST_DOCALC    Set up the calcs
C TBST_DOSTAT1   Do the calcs for one column
C TBST_DOSTAT2   Do the calcs for two columns



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBSTAT -- Get statistics for one or two columns in tables
C
C         A J Penny            RAL            1991 May

      subroutine t_tbstat ( )

      implicit none
C--
Cbegin


      call tbst_setup						!Set up parameters

      call tbst_getdata						!Open a set of files

      call tbst_docalc 						!Do the calcs


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBST_SETUP -- Load the default parameters
C
C     a j penny                 ral               1991 May

      subroutine tbst_setup ( )

      implicit none
      include 'tbstat.inc'
C--
Cbegin


      IPTAB1 = 1
      IPTAB2 = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBST_GETDATA -- Get Table
C
C     a j penny                 ral               1991 May

      subroutine tbst_getdata ( )

      implicit none
      include 'tbstat.inc'
      include 'STARMAN_INC'
C--
      integer istat
Cbegin


      if ( ST_FAILED ) return

      call optabr ( 'IN1', IPTAB1, TBVX1, TBY1, .true., istat )
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return
      TBX1 = TBVX1 - 5

      call pargi ( TBX1 )
      call pargi ( TBY1 )
      call printd ( 'No of columns = %d :  No of rows = %d ' )

      call get1i ( 'NCOL1', NCOL1, 1, 1, TBX1 )
      if ( ST_FAILED ) return

      call optabr ( 'IN2', IPTAB2, TBVX2, TBY2, .true., istat )
      if ( ST_FAILED ) return
      if ( istat.eq.0 ) then
         ISTWO = .true.
         if ( TBY2.ne.TBY1 ) then
            call printo ( 'ERROR: Tables must be same length' )
            ST_FAILED = .true.
            if ( ST_FAILED ) return
         else
            TBX2 = TBVX2 - 5
            call pargi ( TBX2 )
            call pargi ( TBY2 )
            call printd ( 'No of columns = %d :  No of rows = %d ' )
            call get1i ( 'NCOL2', NCOL2, 1, 1, TBX2 )
            if ( ST_FAILED ) return
         endif
      elseif ( istat.eq.2 ) then
         ISTWO = .false.
         IPTAB2 = IPTAB1
         TBVX2 = TBVX1
         TBY2 =TBY1
         TBX2 = TBX1
         NCOL2 = NCOL1
      elseif ( istat.eq.3 ) then
         ISTWO = .true.
         IPTAB2 = IPTAB1
         TBVX2 = TBVX1
         TBY2 =TBY1
         TBX2 = TBX1
         call pargi ( TBX2 )
         call pargi ( TBY2 )
         call printd ( 'No of columns = %d :  No of rows = %d ' )
         call get1i ( 'NCOL2', NCOL2, 1, 1, TBX2 )
         if ( ST_FAILED ) return
      else
         ST_FAILED = .true.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBST_DOCALC -- Set up the calcs
C
C    a j penny                      ral                  1991 may

      subroutine tbst_docalc ( )

      implicit none
      include 'tbstat.inc'
      include 'STARMAN_INC'
C--
      integer ipwa, ipwb, istat
Cbegin


      if ( ST_FAILED ) return

      call gtwrkr ( 'A', TBY1, ipwa, istat )
      call copfrr ( %val(IPTAB1), TBVX1, TBY1, (NCOL1+5), 1,
     +              %val(ipwa), TBY1 )
      if ( ISTWO ) then
         call gtwrkr ( 'B', TBY1, ipwb, istat )
         call copfrr ( %val(IPTAB2), TBVX2, TBY1, (NCOL2+5), 1,
     +                 %val(ipwb), TBY1 )
         call printo ( ' ' )
         call printo ( '1st column' )
         call tbst_dostat1 ( %val(ipwa), TBY1 )
         call printo ( ' ' )
         call printo ( '2nd column' )
         call tbst_dostat1 ( %val(ipwb), TBY1 )
         call printo ( ' ' )
         call printo ( 'Combination of both' )
         call tbst_dostat2 ( %val(ipwa), %val(ipwb), TBY1 )
         call printo ( ' ' )
      else
         call printo ( ' ' )
         call tbst_dostat1 ( %val(ipwa), TBY1 )
         call printo ( ' ' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBST_DOSTAT1 -- Do the calcs for one column
C
C    a j penny                      ral                  1991 may

      subroutine tbst_dostat1 ( x, n )

      implicit none
      include 'STARMAN_INC'

      integer  n		!i: No of points
      real     x(n)		!i: Input data
C--
      real av, std, stdm, amin, amax
Cbegin


      if ( ST_FAILED ) return

      call alimr ( x, n, amin, amax )

      call aavgr ( x, n, av, std )
      stdm = 0.0
      if ( n.ge.3 ) stdm = std/(real(n)-2.0)

      call pargi ( n )
      call pargr ( amin )
      call pargr ( amax )
      call pargr ( av )
      call pargr ( std )
      call pargr ( stdm )
      call printd ('No of entries = %d : Minimum = %f : Maximum = %f ')
      call printd ('Mean = %f : Std dev = %f : Std dev of mean = %f ')


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBST_DOSTAT2 -- Do the calcs for two columns
C
C    a j penny                      ral                  1991 may

      subroutine tbst_dostat2 ( x, y, n )

      implicit none
      include 'STARMAN_INC'

      integer  n		!i: No of points
      real     x(n)		!i: Input data
      real     y(n)		!i: Input data
C--
      double precision an, sx, sy, ssx, ssy, sxy, tx, ty,
     +                 a, b, c, sd, sda, sdb, corrl, ax, ay
      real    rv
      integer k
Cbegin


      if ( ST_FAILED ) return

      sx = 0.0d0
      sy = 0.0d0
      ssx = 0.0d0
      ssy = 0.0d0
      sxy = 0.0d0
      tx = 0.0d0
      ty = 0.0d0
      an = n
      if ( n.ge.1 ) then
         do k = 1, n
            tx = tx + x(k)
            ty = ty + y(k)
         enddo
         ax = tx/an
         ay = ty/an
          do k = 1, n
            sx = sx + (dble(x(k))-ax)
            sy = sy + (dble(y(k))-ay)
            ssx = ssx + (dble(x(k))-ax)*(dble(x(k))-ax)
            ssy = ssy + (dble(y(k))-ay)*(dble(y(k))-ay)
            sxy = sxy + (dble(x(k))-ax)*(dble(y(k))-ay)
         enddo
         b = sxy/ssx
         a = ay - b*ax
      else
         a = 0.0d0
         b = 0.0d0
      endif

      corrl = 0.0d0
      if ( n.ge.2 ) then
         if ( ssx.gt.0.0d0 .and. ssy.gt.0.0d0 ) then
            corrl = sxy/sqrt(ssx*ssy)
         endif
      endif

      sd = 0.0d0
      sda = 0.0d0
      sdb = 0.0d0
      if ( n.ge.3 ) then
         c = (ssy-((sxy*sxy)/ssx))/(an-2.0d0)
         if ( c.gt.0.0d0 ) then
            sd = sqrt(c)
         else
            sd = 0.0d0
         endif
         sdb = sd/sqrt(ssx)
         sda = sd*((1.0d0/an)+((ax*ax)/ssx))
      endif

      call printo ( 'Y = A + BX  :assuming X errors = 0' )
      rv = real(a)
      call pargr ( rv )
      rv = real(sda)
      call pargr ( rv )
      rv = real(b)
      call pargr ( rv )
      rv = real(sdb)
      call pargr ( rv )
      call printd  ( 'A = %f +/- %f : B = %f +/- %f ' )
      rv = real(sd)
      call pargr ( rv )
      call printd ( 'Std dev of a Y value from line = %f ' )
      rv = real(corrl)
      call pargr ( rv )
      call printd ( 'Correlation coefficent = %f ' )


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBTRAN_AUTO.F -- Transform one XY table to another automatically
C
C Contains:-
C T_TBTRAN_AUTO   Transform one XY table to another automatically
C TBTAU_GCL       Get command line input
C TBTAU_DOCALCS   Do the calculations
C TBTAU_TRIS      Calculate side lengths of triangles
C TBTAU_SORT_TRI  Sort table into brightest first
C TBTAU_MATCH     Match triangles by sides ratios
C TBTAU_TCALC     Calc mean transform from matched triangles
C TBTAU_OUT       Put out transformed input table
C TBTAU_LOADXY    Load sorted positions
C TBTAU_MAG       Convert heights to mags
C
C
C        pat morris                  Leeds      1994 Jan
C        alan penny                    ral      1994 Nov


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBTRAN_AUTO -- Transform one XY table to another automatically
C
C   Find which stars in one image match those in another
C   when the images are not aligned (or even flipped or
C   stretched).
C
C   The user inputs a list of star positions from each
C   image.
C
C   The program works out the triangle between the three
C   brightest stars in each list. It then looks at the two
C   triangles, and sees if they are the same by comparing
C   the ratios in each triangle of the length of longest
C   side to the length of next longest, and to the length
C   of the shortest side. If the ratios are the same within
C   a chosen tolerance, then the triangle probably must have
C   been made with the same stars.
C
C   If the triangles do not match, then this is repeated
C   with the four brightest in each list. The program works
C   out all the possible triangles between the stars in each
C   list. It then looks at each triangle in one list, and
C   sees if there is a matching triangle in the other list.
C   If there is no match, this is then repeated with the five
C   brightest stars, and so on, until a match is made.
C
C   The matched triangles gives a list of matching stars.
C   These stars can then be used to calculate the
C   transformation.
C
C   The first ('input') list is then transformed into the
C   coordinate system of the second ('master') list.
C
C
C
C        pat morris                  Leeds      1994 Jan
C        alan penny                    ral      1994 Nov

      subroutine t_tbtran_auto

      implicit none

      include 'tbtran_auto.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer istat, ipmmag, ipimag, ipnm, ipni, ipmxy, ipixy
Cbegin


      if ( ST_FAILED ) return

      call tbtau_gcl							!Get command line inputs
      if ( ST_FAILED ) return

      call gtwrkr ( 'MAS_NUM',   TBYM,    ipnm, istat )
      call gtwrkr ( 'MAS_MAG',   TBYM,  ipmmag, istat )
      call gtwrkr ( 'MAS_POS', 2*TBYM,   ipmxy, istat )
      call gtwrkr (  'IN_NUM',    TBY,    ipni, istat )
      call gtwrkr (  'IN_MAG',    TBY,  ipimag, istat )
      call gtwrkr (  'IN_POS',  2*TBY,   ipixy, istat )
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         call printo( ' Workspace allocation in s/r t_tbtran_auto' //
     +                'failed')
         return
      endif
      if ( ST_FAILED ) return

      call anumincr ( %val(ipnm), TBYM )				!Get order of brightness
      if ( TBVXM.ge.8 .and. KW_MAS.ne.3 ) then
         call copfrr ( %val(IPMAS), TBVXM, TBYM, 8, 1, %val(ipmmag),	! in master list
     +                 TBYM )
         if ( KW_MAS.eq.2 ) call tbtau_mag ( %val(ipmmag), TBYM )
         call sort2r ( %val(ipmmag), %val(ipnm), TBYM )
      endif

      call tbtau_loadxy (%val(IPMAS),%val(ipmxy),%val(ipnm),TBVXM,TBYM)	!Load posn in order
      if ( ST_FAILED ) return

      call anumincr ( %val(ipni), TBY )					!Get order of brightness
      if ( TBVX.ge.8 .and. KW_IN.ne.3 ) then
         call copfrr ( %val(IPIN), TBVX, TBY, 8, 1, %val(ipimag), TBY )	! in input list
         if ( KW_IN.eq.2 ) call tbtau_mag ( %val(ipimag), TBY )
         call sort2r ( %val(ipimag), %val(ipni), TBY )
      endif

      call tbtau_loadxy ( %val(IPIN), %val(ipixy), %val(ipni),TBVX,TBY)	!Load posn in order
      if ( ST_FAILED ) return

      call tbtau_docalcs ( %val(ipmxy), %val(ipixy) )			!Do calcs
      if ( ST_FAILED ) return

      call wrkcan ( 'MAS_NUM' )
      call wrkcan ( 'MAS_MAG' )
      call wrkcan ( 'MAS_POS' )
      call wrkcan ( 'IN_NUM' )
      call wrkcan ( 'IN_MAG' )
      call wrkcan ( 'IN_POS' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTAU_GCL -- Get command line input
C
C        pat morris                  Leeds      1994 Jan
C        alan penny                    ral      1994 Nov

      subroutine tbtau_gcl ()
      implicit none

      include 'tbtran_auto.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer istat, k, kl
      real rv
      character*50 head, heada
Cbegin


      if ( ST_FAILED ) return


      call optabr ( 'IN', IPIN, TBVX, TBY, .true., istat )
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) then
          ST_FAILED = .true.
          return
      elseif ( TBVX.lt.7 ) then
          call printo ('ERROR: Input must have at least two columns')
          ST_FAILED = .true.
          return
      endif

      KW_IN = 3
      if ( TBVX.ge.8 ) then
         call printo ( 'Magnitude or height assumed to be in column 3')
         call gthead ( 'IN', 3, head, istat )
         if ( ST_FAILED ) return
         call lbgone ( head )
         call charln ( head, kl )
         call lowcase ( head, heada )
         if ( index(heada,'height').ne.0 .or.
     +       (kl.eq.2 .and. heada(1:kl).eq.'ht') .or.
     +       (kl.eq.3 .and. heada(1:kl).eq.'hts') ) then
            KW_IN = 2
         elseif ( index(heada,'magnitude').ne.0 .or.
     +       (kl.eq.3 .and. heada(1:kl).eq.'mag') .or.
     +       (kl.eq.4 .and. heada(1:kl).eq.'mags') .or.
     +       (kl.eq.4 .and. heada(1:kl).eq.'magn') .or.
     +       (kl.eq.5 .and. heada(1:kl).eq.'magns') ) then
            KW_IN = 1
         endif
         k = KW_IN
         call get_job ( 'IN_MAGS', 'mags:heights:none', KW_IN, k,' ',0)
         if ( ST_FAILED ) return
      endif

      call optabr ( 'MASTER', IPMAS, TBVXM, TBYM, .true., istat )
      if ( ST_FAILED ) return
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      elseif ( TBVXM.lt.7 ) then
          call printo ('ERROR: Master must have at least two columns')
          ST_FAILED = .true.
          return
      endif

      KW_MAS = 3
      if ( TBVXM.ge.8 ) then
         call printo ( 'Magnitude or height assumed to be in column 3')
         call gthead ( 'MASTER', 3, head, istat )
         if ( ST_FAILED ) return
         call lbgone ( head )
         call charln ( head, kl )
         call lowcase ( head, heada )
         if ( index(heada,'height').ne.0 .or.
     +       (kl.eq.2 .and. heada(1:kl).eq.'ht') ) then
            KW_MAS = 2
         elseif ( index(heada,'magnitude').ne.0 .or.
     +       (kl.eq.3 .and. heada(1:kl).eq.'mag') .or.
     +       (kl.eq.4 .and. heada(1:kl).eq.'magn') ) then
            KW_MAS = 1
         endif
         k = KW_MAS
         call get_job ( 'MS_MAGS', 'mags:heights:none',KW_MAS,k,' ',0)
         if ( ST_FAILED ) return
      endif

      call get_job ( 'TYPE', 'full:xyshift', KFIT, 1, ' ', 0 )
      if ( KFIT.eq.1 ) KFIT = 3
      if ( KFIT.eq.2 ) KFIT = 1
      if ( ST_FAILED ) return

      call get1r ( 'TOL', TOLER, 1.0, 0.0, 1.0e7 )
      if ( ST_FAILED ) return

      rv = 5.0
      if ( KFIT.eq.1 ) rv = 2.0
      call get1r ( 'FTOL', FTOLER, rv, 0.0, 1.0e7 )
      if ( ST_FAILED ) return

      call get1i ( 'NTRI', NTMATCH, 4, 1, 10000 )
      if ( ST_FAILED ) return

      call get1i ( 'TOTNTRI', NTOTMATCH, 8, 1, 10000 )
      if ( ST_FAILED ) return

      SIGLIM = 2.5


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTAU_DOCALCS -- Do the calculations
C
C        pat morris                  Leeds      1994 Jan
C        alan penny                    ral      1994 Nov

      subroutine tbtau_docalcs ( mdata, idata )

      implicit none
      include 'tbtran_auto.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real    mdata(2,TBYM)	!i: Master file sorted XY posns
      real    idata(2,TBY)	!i: Input file sorted XY posns
C--
      integer level, ntriangs, nstars, istat, num, ipr_m,
     +        ipn_m, ipr_i, ipn_i, ippi, ippm, ipuse, ipout, ntris
      character*50 title, cv
      logical found, ok

      integer pair(2,NTMAX), tpair(2,3,NTMAX)
Cbegin


      if ( ST_FAILED ) return

      call gtwrkr (  'PI', 2*TBYM,  ippi, istat ) 			!Open workspace for the x,y match positions
      call gtwrkr (  'PM', 2*TBYM,  ippm, istat )
      call gtwrkr ( 'USE',   TBYM, ipuse, istat )
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         call printo ( 'Workspace allocation 1 in s/r'//
     +                 ' tbtau_docalcs failed' )
         return
      endif

      found = .false.
      ok = .true.
      level = 0
      do while ( .not.found .and. ok )					!Loop until find match or user aborts

         level = level + 1
         nstars = 2 + level       					!Use nstars brightest stars
         ntriangs = (nstars*(nstars-1)*(nstars-2))/6			!Total number of triangles possible

         if ( nstars.gt.TBYM .or. nstars.gt.TBY ) then
            if ( nstars.gt.TBYM ) call printo ( 'Not enough '//
     +                 'stars in master table for this level' )
            if ( nstars.gt.TBY ) call printo ( 'Not enough '//
     +                 'stars in input table for this level')
            found = .false.
            ok = .false.
         else

            call gtwrkr ( 'TR_MR', 3*ntriangs, ipr_m, istat )		!Load master file triangles
            call gtwrki ( 'TR_MN', 3*ntriangs, ipn_m, istat )
            call gtwrkr ( 'TR_IR', 3*ntriangs, ipr_i, istat )		!Load file triangles
            call gtwrki ( 'TR_IN', 3*ntriangs, ipn_i, istat )

            if ( istat.ne.0 ) then
               ST_FAILED = .true.
               call printo ( 'Workspace allocation 2 in s/r'//
     +                       ' tbtau_docalcs failed' )
               return
            endif

            call tbtau_tris ( mdata, TBYM, nstars, ntriangs,		!Calc trangle lengths - master list
     +                        %val(ipr_m), %val(ipn_m) )

            call tbtau_tris ( idata,  TBY, nstars, ntriangs,		!Calc trangle lengths - input list
     +                        %val(ipr_i), %val(ipn_i) )

            call tbtau_match ( %val(ipr_m), %val(ipn_m), %val(ipr_i),	!Look for matches
     +                         %val(ipn_i), ntriangs, pair, num, tpair,
     +                         ntris )

            call tbtau_tcalc ( mdata, idata, pair, num, tpair, 		 !Calc transform from matches
     +                         ntriangs, ntris, %val(ippm), %val(ippi),
     +                         %val(ipuse), level, nstars, found )

            call wrkcan ( 'TR_MR' )
            call wrkcan ( 'TR_MN' )
            call wrkcan ( 'TR_IR' )
            call wrkcan ( 'TR_IN' )

         endif

         if ( ST_FAILED ) return
      enddo								! Next level

      if ( ok ) then
         call optabw ( 'OUT', ipout, TBVX, TBY, .true., istat )
         if ( ST_FAILED ) return
         if ( istat.eq.0 ) then
            call tbtau_out ( %val(IPIN), %val(ipout) )
            call tcopdes ( 'IN', 'OUT', istat )
            if ( ST_FAILED ) return
            call gtdesc ( 'IN', 'TITLE', cv,
     +                    'Output from TBTRAN_AUTO', num, istat )
            call get1c ( 'TITLE', title, cv, .true. )
            call ptdesc ( 'OUT', 'TITLE', title )
           endif
      endif

      call wrkcan ( 'PI' )
      call wrkcan ( 'PM' )
      call wrkcan ( 'USE' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTAU_TRIS -- Calculate side lengths of triangles
C
C        pat morris                  Leeds      1994 Jan
C        alan penny                    ral      1994 Nov

      subroutine tbtau_tris ( data, ny, nstars, ntriangs, rtri,
     +                        ntri )

      implicit none
      include 'STARMAN_INC'

      integer  ny			!i: Size of XY posn array
      real     data(2,ny)		!i: XY posns
      integer  nstars			!i: No of stars to do
      integer  ntriangs			!i: Number of triangles
      real     rtri(3,ntriangs)		!o: Triangle side lengths
      integer  ntri(3,ntriangs)		!o: Star numbers in triangles
C--
      integer i, j, k, m, n
      real dx, dy
      real tarr(3,2)
Cbegin


      if ( ST_FAILED ) return

      if ( nstars.eq.-1 ) return

      n = 0
      do i = 1, nstars-2
       do j = i+1, nstars-1
        do k = j+1, nstars

           dx = data(1,i) - data(1,j)					!i -> j opp vertex is k
           dy = data(2,i) - data(2,j)
           tarr(1,1) = max(sqrt(dx*dx+dy*dy),1.0e-7)
           tarr(1,2) = k

           dx = data(1,i) - data(1,k)					!i -> k opp vertex is j
           dy = data(2,i) - data(2,k)
           tarr(2,1) = max(sqrt(dx*dx+dy*dy),1.0e-7)
           tarr(2,2) = j

           dx = data(1,j) - data(1,k)					!j -> k opp vertex is i
           dy = data(2,j) - data(2,k)
           tarr(3,1) = max(sqrt(dx*dx+dy*dy),1.0e-7)
           tarr(3,2) = i

           call tbtau_sort_tri ( tarr )					!Sort into correct order - longest side first

           n = n + 1							!Load ouput
           do m = 1, 3
              rtri(m,n) = tarr(m,1)
              ntri(m,n) = nint(tarr(m,2))
           enddo

        enddo
       enddo
      enddo

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTAU_SORT_TRI -- Sort table into brightest first
C
C        pat morris                  Leeds      1994 Jan
C        alan penny                    ral      1994 Nov

      subroutine tbtau_sort_tri ( tarr )

      implicit none
      include 'tbtran_auto.inc'
      include 'STARMAN_INC'

      real tarr(3,2)		!i/o:
C--
      integer i
      logical loop
      real temp
Cbegin


      if ( ST_FAILED ) return

      loop = .true.
      do while ( loop )						!Loop while positions changed last time
         loop = .false.
         do i = 1, 2
            if ( tarr(i,1).lt.tarr(i+1,1) ) then
               loop = .true.
               temp = tarr(i,1)
               tarr(i,1) = tarr(i+1,1)
               tarr(i+1,1) = temp
               temp = tarr(i,2)
               tarr(i,2) = tarr(i+1,2)
               tarr(i+1,2) = temp
            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTAU_MATCH -- Match triangles by sides ratios
C
C        pat morris                  Leeds      1994 Jan
C        alan penny                    ral      1994 Nov

      subroutine tbtau_match ( rtri_mas, ntri_mas, rtri_in, ntri_in,
     +                         ntriangs, pair, num, tpair, ntris )

      implicit none
      include 'tbtran_auto.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      integer  ntriangs			!i: Number of possible triangles
      real     rtri_mas(3,ntriangs)	!i: Lengths of master triangles
      integer  ntri_mas(3,ntriangs)	!i: Star numbers in master triangles
      real     rtri_in(3,ntriangs)	!i: Lengths of input triangles
      integer  ntri_in(3,ntriangs)	!i: Star numbers in input triangles
      integer  pair(2,NTMAX)		!o: Star numbers in each list for
					!    different stars matched
      integer  num			!o: Number of different stars matched
      integer  ntris			!o: Number of triangles matched
      integer  tpair(2,3,NTMAX)		!o: Star numbers for all matched triangles
C--
      integer km, ki, k, m, m1, m2
      real rv, dimin1, dimax1, dmmin1, dmmax1, dimin2, dimax2,
     +     dmmin2, dmmax2
      logical got, ok1, ok2
Cbegin


      if ( ST_FAILED ) return

      num = 0
      ntris = 0

      do km = 1, ntriangs						!For each master
         do ki = km, ntriangs

            ok1 = .false.						!Difference of ratios of first two sides
            dmmin1 = (rtri_mas(2,km)-TOLER)/(rtri_mas(1,km)+TOLER)
            rv = max(1.0e-7,(rtri_mas(1,km)-TOLER))
            dmmax1 = (rtri_mas(2,km)+TOLER)/rv
            dimin1 = (rtri_in(2,ki)-TOLER)/(rtri_in(1,ki)+TOLER)
            rv = max(1.0e-7,(rtri_in(1,ki)-TOLER))
            dimax1 = (rtri_in(2,ki)+TOLER)/rv
            if ( dimin1.ge.dmmin1 .and. dimin1.le.dmmax1 ) ok1 = .true.
            if ( dimax1.ge.dmmin1 .and. dimax1.le.dmmax1 ) ok1 = .true.
            if ( dmmin1.ge.dimin1 .and. dmmin1.le.dimax1 ) ok1 = .true.
            if ( dmmax1.ge.dimin1 .and. dmmin1.le.dimax1 ) ok1 = .true.

            ok2 = .false.						!Difference of ratios of
            dmmin2 = (rtri_mas(3,km)-TOLER)/(rtri_mas(1,km)+TOLER)	! first and third sides
            rv = max(1.0e-7,(rtri_mas(1,km)-TOLER))
            dmmax2 = (rtri_mas(3,km)+TOLER)/rv
            dimin2 = (rtri_in(3,ki)-TOLER)/(rtri_in(1,ki)+TOLER)
            rv = max(1.0e-7,(rtri_in(1,ki)-TOLER))
            dimax2 = (rtri_in(3,ki)+TOLER)/rv
            if ( dimin2.ge.dmmin2 .and. dimin2.le.dmmax2 ) ok2 = .true.
            if ( dimax2.ge.dmmin2 .and. dimax2.le.dmmax2 ) ok2 = .true.
            if ( dmmin2.ge.dimin2 .and. dmmin2.le.dimax2 ) ok2 = .true.
            if ( dmmax2.ge.dimin2 .and. dmmin2.le.dimax2 ) ok2 = .true.

CX            if ( ok1 .and. ok2 ) write
CX     +        ( 6,'(1x,2i5,f10.1,8f6.3)')km,ki,rtri_mas(1,km),
CX     +        dmmin1,dmmax1,dimin1,dimax1,dmmin2,dmmax2,dimin2,dimax2

            if ( ok1 .and. ok2  ) then					!Is it within the tolerance limits ?

               if ( ntris.lt.NTMAX ) then

                  ntris = ntris + 1

                  do m = 1, 3						!Load x,y position pairs
                     m1 = ntri_mas(m,km)
                     m2 = ntri_in(m,ki)

                     tpair(1,m,ntris) = m1
                     tpair(2,m,ntris) = m2

                     got = .false.

                     if ( num.ge.1 ) then				!Check we dont have match for either star already
                        do k = 1, num
                           if ( pair(1,k).eq.m1 .or.
     +                          pair(2,k).eq.m2 ) got = .true.
                        enddo
                     endif

                     if ( .not.got ) then
                        num = num + 1
                        pair(1,num) = m1
                        pair(2,num) = m2
                     endif
                  enddo

               endif
            endif

         enddo								! Next j

      enddo								! Next i


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTAU_TCALC -- Calc mean transform from matched triangles
C
C        pat morris                  Leeds      1994 Jan
C        alan penny                    ral      1994 Nov

      subroutine tbtau_tcalc ( mdata, idata, pair,  num, tpair,
     +                         ntriangs, ntris, pm, pi, use, level,
     +                         nstars, found )

      implicit none
      include 'tbtran_auto.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real     mdata(2,TBYM)		!i: Master XY posns
      real     idata(2,TBY)		!i: Input XY posns
      integer  pair(2,NTMAX)		!i: List numbers in each list of matched stars
      integer  num                      !i: Number of stars matched
      integer  ntriangs			!i: Number of possible triangles
      integer  tpair(2,3,NTMAX)		!i: List numbers in each list of triamgle stars
      integer  ntris                    !i: Number of triangles matched
      real     pm(TBYM,2)		!o: master positions (matches)
      real     pi(TBYM,2)		!o: input positions (matches)
      logical  use(TBYM)		!o: workspace for treansform fit
      integer  level                    !i: Level working at
      integer  nstars                   !i: Stars searched
      logical  found			!o: Flag for found a match
C--
      integer j, k, ngood, kk, nok, kkgot, nmatch, nv, nch
      real trc(6), sd, xmax, xmin, ymax, ymin, dx, dy, dds, rvc, rv,
     +     otol, avc(6)
      logical ok
      character*80 text
      real pos(2,4,NTMAX), atrc(6,NTMAX), rva(NTMAX)
      integer npos(NTMAX), nstar(NTMAX)
      real trunc
      external trunc
Cbegin

      if ( ST_FAILED ) return

      found = .false.
      if ( ntris.eq.0 ) return						! got some matches

      call printo ( '   ---------------------------------- ' )          !Print out status
      call pargi ( level )
      call pargi ( nstars )
      call pargi ( ntris )
      call printd (
     +   'Level %d : Stars searched - %d : Triangles searched - %d' )

      if ( ntris.eq.1 .and. (TBYM.eq.3 .or. TBY.eq.3) ) then

         found = .true.
         num = 0
         do j = 1, 3
            num = num + 1
            pm(num,1) = mdata(1,tpair(1,j,1))
            pm(num,2) = mdata(2,tpair(1,j,1))
            pi(num,1) = idata(1,tpair(2,j,1))
            pi(num,2) = idata(2,tpair(2,j,1))
         enddo

         call tran_doit ( pi, pm, TBYM, num, KFIT, SIGLIM, use,trc, 	!Do the transform
     +                    ngood, sd )
         if ( ST_FAILED ) return
         do j = 1, 6                                               	!Check values O$
            if ( abs(trc(j)).lt.1.0e-13 ) then
               if ( trc(j).lt.0.0 ) trc(j) = -1.0e-13
               if ( trc(j).gt.0.0 ) trc(j) =  1.0e-13
            elseif ( abs(trc(j)).gt.1.0e13 ) then
               if ( trc(j).lt.0.0 ) trc(j) = -1.0e13
               if ( trc(j).gt.0.0 ) trc(j) =  1.0e13
            endif
         enddo

         call pargi ( ngood )
         call pargr ( sd )
         call printd('   Stars used in fit - %d : Std dev of fit - %f')
         call pargr ( trc(1) )
         call pargr ( trc(2) )
         call pargr ( trc(3) )
         call printd ( '   X = %f + %f*Xin + %f *Yin ')
         call pargr ( trc(4) )
         call pargr ( trc(5) )
         call pargr ( trc(6) )
         call printd ( '   Y = %f + %f*Xin + %f *Yin ')
         call printo ( ' ' )

         call amovr ( trc(1), XEQN, 3 )
         call amovr ( trc(4), YEQN, 3 )
         call put3r ( 'XCOEFF', trc(1), trc(2), trc(3) )		!Put in output parameters
         call put3r ( 'YCOEFF', trc(4), trc(5), trc(6) )

         return

      endif

      if ( ntris.ge.2 ) then

         xmax = idata(1,tpair(1,1,1))
         xmin = xmax
         ymax = idata(2,tpair(1,1,1))
         ymin = ymax
         do k = 1, ntris
            do j = 1, 3
               rv = idata(1,tpair(1,j,k))
               ymax = max(ymax,rv)
               ymin = min(ymin,rv)
               rv = idata(2,tpair(1,j,k))
               xmax = max(xmax,rv)
               xmin = min(xmin,rv)
            enddo
         enddo
         xmax = xmax + 0.5*(xmax-xmin)
         xmin = xmin - 0.5*(xmax-xmin)
         ymax = ymax + 0.5*(ymax-ymin)
         ymin = ymin - 0.5*(ymax-ymin)

         kkgot = 0
         do k = 1, ntris
            do j = 1, 3
               pm(j,1) = mdata(1,tpair(1,j,k))
               pm(j,2) = mdata(2,tpair(1,j,k))
               pi(j,1) = idata(1,tpair(2,j,k))
               pi(j,2) = idata(2,tpair(2,j,k))
            enddo
            call tran_doit ( pi, pm, TBYM, 3, KFIT, SIGLIM, use,
     +                       trc, ngood, sd )
            do j = 1, 6                                               	!Check values O$
               if ( abs(trc(j)).lt.1.0e-13 ) then
                  if ( trc(j).lt.0.0 ) trc(j) = -1.0e-13
                  if ( trc(j).gt.0.0 ) trc(j) =  1.0e-13
               elseif ( abs(trc(j)).gt.1.0e13 ) then
                  if ( trc(j).lt.0.0 ) trc(j) = -1.0e13
                  if ( trc(j).gt.0.0 ) trc(j) =  1.0e13
               endif
            enddo
            call amovr ( trc, atrc(1,k), 6 )
            pos(1,1,k) = trc(1) + xmin*trc(2) + ymin*trc(3)
            pos(2,1,k) = trc(4) + xmin*trc(5) + ymin*trc(6)
            pos(1,2,k) = trc(1) + xmin*trc(2) + ymax*trc(3)
            pos(2,2,k) = trc(4) + xmin*trc(5) + ymax*trc(6)
            pos(1,3,k) = trc(1) + xmax*trc(2) + ymin*trc(3)
            pos(2,3,k) = trc(4) + xmax*trc(5) + ymin*trc(6)
            pos(1,4,k) = trc(1) + xmax*trc(2) + ymax*trc(3)
            pos(2,4,k) = trc(4) + xmax*trc(5) + ymax*trc(6)
         enddo

         nmatch = 0
         do k = 1, ntris

            npos(k) = 0
            nok = 0
            dds = atrc(2,k)*atrc(2,k) + atrc(5,k)*atrc(5,k)
            otol = FTOLER*sqrt(dds)

            j = 0
            do while ( npos(k).eq.0 .and. j.lt.ntris )
               j = j + 1
               if ( j.ne.k ) then
                  ok = .true.
                  do kk = 1, 4
                     dx = pos(1,kk,k) - pos(1,kk,j)
                     dy = pos(2,kk,k) - pos(2,kk,j)
                     if ( abs(dx).gt.otol ) ok = .false.
                     if ( abs(dy).gt.otol ) ok = .false.
                  enddo
                  if ( abs(atrc(1,k)-atrc(1,j)).gt.otol ) ok = .false.
                  if ( abs(atrc(4,k)-atrc(4,j)).gt.otol ) ok = .false.
                  if ( ok ) nok = nok + 1
               endif
            enddo

            nv = max(1,min(NTMATCH,(ntris-2)))
            if ( npos(k).eq.0 .and. nok.ge.nv ) then
               npos(k) = 1
               nmatch = nmatch + 1
            endif

         enddo

         nch = 0
         do k = 1, ntris
            if ( npos(k).eq.1 ) then
               if ( nch.eq.0 ) then
                  call printo ( ' Triangles matched:- ' )
                  call printo (
     +            '   Number            X fit coeffs     '//
     +            '             Y fit coeffs' )
               endif
               nch = nch + 1
               do j = 1, 6
                  avc(j) = trunc ( atrc(j,k), 5 )
               enddo
               write ( text,'(4x,i5,5x,
     +                2(f7.1,2f9.3,5x))') k, (avc(j),j=1,6)
               call printo ( text )
            endif
         enddo
         if ( nch.ne.0 ) call printo ( ' ' )

         nv = max(1,min(NTOTMATCH,(ntris-2)))
         if ( nmatch.gt.nv ) then

            found = .true.
            do j = 1, 6
               kk = 0
               do k = 1, ntris
                  if ( npos(k).eq.1 ) then
                     kk = kk + 1
                     rva(kk) = atrc(j,k)
                  endif
               enddo
               call amedianr ( rva, nmatch, trc(j) )
            enddo

            num = 0
            do k = 1, ntris						!Load up (x,y) positions for matches
               if ( npos(k).eq.1 ) then
                  ok = .true.

                  if ( abs(atrc(1,k)-trc(1)).gt.otol ) ok = .false.
                  if ( abs(atrc(4,k)-trc(4)).gt.otol ) ok = .false.

                  rvc = max(abs(trc(2)),abs(trc(3)))
                  rv = abs((atrc(2,k)-trc(2))/rvc)
                  if ( rv.gt.0.05 ) ok = .false.
                  rv = abs((atrc(3,k)-trc(3))/rvc)
                  if ( rv.gt.0.05 ) ok = .false.

                  rvc = max(abs(trc(5)),abs(trc(6)))
                  rv = abs((atrc(5,k)-trc(5))/rvc)
                  if ( rv.gt.0.05 ) ok = .false.
                  rv = abs((atrc(6,k)-trc(6))/rvc)
                  if ( rv.gt.0.05 ) ok = .false.

                  if ( ok ) then
                     do j = 1, 3
                        if ( num.lt.NTMAX ) then
                           num = num + 1
                           nok = .true.
                           if ( num.ne.1 ) then
                              do kk = 1, num-1
                                 if ( nstar(kk).eq.tpair(1,j,k) )
     +                                nok = .false.
                              enddo
                           endif
                           if ( .not.nok ) then
                              num = num - 1
                           else
                              nstar(num ) = tpair(1,j,k)
                              pm(num,1) = mdata(1,tpair(1,j,k))
                              pm(num,2) = mdata(2,tpair(1,j,k))
                              pi(num,1) = idata(1,tpair(2,j,k))
                              pi(num,2) = idata(2,tpair(2,j,k))
                           endif
                        endif
                     enddo
                  endif
               endif
            enddo

            call tran_doit ( pi, pm, TBYM, num, KFIT, SIGLIM, use,trc, 	!Do the transform
     +                       ngood, sd )
            if ( ST_FAILED ) return
            do j = 1, 6                                               	!Check values O$
               if ( abs(trc(j)).lt.1.0e-13 ) then
                  if ( trc(j).lt.0.0 ) trc(j) = -1.0e-13
                  if ( trc(j).gt.0.0 ) trc(j) =  1.0e-13
               elseif ( abs(trc(j)).gt.1.0e13 ) then
                  if ( trc(j).lt.0.0 ) trc(j) = -1.0e13
                  if ( trc(j).gt.0.0 ) trc(j) =  1.0e13
               endif
            enddo

            call pargi ( ngood )
            call pargr ( sd )
            call printd(
     +              '   Stars used in fit - %d : Std dev of fit - %f')
            call printo ( '   These final coefficents should'//
     +                    ' be a good sample of the fits above' )
            call pargr ( trc(1) )
            call pargr ( trc(2) )
            call pargr ( trc(3) )
            call printd ( '   X = %f + %f*Xin + %f *Yin ')
            call pargr ( trc(4) )
            call pargr ( trc(5) )
            call pargr ( trc(6) )
            call printd ( '   Y = %f + %f*Xin + %f *Yin ')
            call printo ( ' ' )

            call amovr ( trc(1), XEQN, 3 )
            call amovr ( trc(4), YEQN, 3 )
            call put3r ( 'XCOEFF', trc(1), trc(2), trc(3) )		!Put in output parameters
            call put3r ( 'YCOEFF', trc(4), trc(5), trc(6) )

         endif

      endif


      end


CXCXCXCXCXCXCXCXCXCXCXCXCXCXCXCXCXCXCXCXCXCXCCXCXCXCXCXCXCXCXCXCXCXCXCXCXC
CX        real  adx(4), ady(4), rdd(4), fxdd(4), fydd(4), ddx(4), ddy(4)

CX                     adx(kk) = dx
CX                     ady(kk) = dy
CX                if(ok)write(6,'(1x,''F '',2i5,2x,4(2f6.1,2x),f6.1,3x,
CX     +          2f6.1)')k,j,((adx(kk),ady(kk)),kk=1,4),otol,atrc(1,k),
CX     +          atrc(4,k)

CX         if ( abs(trc(1)+35).lt.50 .and. abs(trc(2)-1.0).lt.0.2 .and.
CX     +        abs(trc(3)+0.2).lt.0.2 .and. abs(trc(4)-35).lt.50 .and.
CX     +        abs(trc(5)-0.2).lt.0.2 .and. abs(trc(6)-1.0).lt.0.2 )
CX     +   then
CX            if ( kkgot.eq.0 ) then
CX               kkgot = 1
CX               do kk = 1, 4
CX                  fxdd(kk) = pos(1,kk,k)
CX                  fydd(kk) = pos(2,kk,k)
CX               enddo
CX               write (6,'(1x, ''B '',i4, 1x, 2(f7.1,2f8.2,2x))' )
CX     +           k, trc(1), trc(2), trc(3), trc(4), trc(5), trc(6)
CX            else
CX               do kk = 1, 4
CX                  ddx(kk) = pos(1,kk,k) - fxdd(kk)
CX                  ddy(kk) = pos(2,kk,k) - fydd(kk)
CX               enddo
CX           dds = atrc(2,k)*atrc(2,k) + atrc(5,k)*atrc(5,k)
CX           otol = FTOLER*sqrt(dds)
CX            write(6,'(1x,''B '',i4,1x,2(f7.1,2f8.2,2x),f4.1,8f4.0)')
CX     +           k, trc(1), trc(2), trc(3), trc(4), trc(5), trc(6),
CX     +                otol,(ddx(kk),ddy(kk),kk=1,4)
CX            endif
CX         endif
CX
CX      print*,'  '
CX      write(6,'(1x,''A3   '',2(2f7.1,3x))')xmin,ymin,xmax,ymax
CX      print*,'  '
CX
CX      do k = 1, num							!Load up (x,y) positions for matches
CX         kxa = pi(k,1) - pm(k,1)
CX         kya = pi(k,2) - pm(k,2)
CX         write ( 6,'(''e5a '',i4,2i5,2f9.2,2i7)')k,pair(1,k),
CX     +             pair(k,2),pi(k,1),pi(k,2),kxa,kya
CX         enddo

CX                            print*,'A1 ', num, pi(num,1),pi(num,2)


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTAU_OUT -- Put out transformed input table
C
C        pat morris                  Leeds      1994 Jan
C        alan penny                    ral      1994 Nov

      subroutine tbtau_out ( in, out )

      implicit none
      include 'tbtran_auto.inc'
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real  in(TBVX,TBY)	!i: Input table to be transformed
      real  out(TBVX,TBY)	!o: Output table
C--
      integer i, n
      real nx, ny
Cbegin


      if ( ST_FAILED ) return

      do n = 1, TBY

         call amovz ( in(1,n), out(1,n), 20 )
         do i = 6, TBVX
            out(i,n) = in(i,n)
         enddo

         nx = XEQN(1) + in(5+1,n)*XEQN(2) + in(5+2,n)*XEQN(3)
         ny = YEQN(1) + in(5+1,n)*YEQN(2) + in(5+2,n)*YEQN(3)
         out(5+1,n) = nx
         out(5+2,n) = ny

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTAU_LOADXY -- Load sorted positions
C
C        pat morris                  Leeds      1994 Jan
C        alan penny                    ral      1994 Nov

      subroutine tbtau_loadxy ( in, out, order, nx, ny )

      implicit none
      include 'STARMAN_INC'

      integer nx		!i:
      integer ny		!i:
      real    in(nx,ny)		!i:
      real    out(2,ny)		!o:
      real    order(ny)		!i:
C--
      integer k, kl
Cbegin


      if ( ST_FAILED ) return

      do k = 1, ny
         kl = nint(order(k))
         out(1,k) = in(6,kl)
         out(2,k) = in(7,kl)
      enddo


      end





CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTAU_MAG -- Convert heights to mags
C
C        pat morris                  Leeds      1994 Jan
C        alan penny                    ral      1994 Nov

      subroutine tbtau_mag ( a, n )

      implicit none
      include 'STARMAN_INC'

      integer n			!i:
      real    a(n)		!i/o:
C--
      integer k
      real rv
Cbegin


      if ( ST_FAILED ) return

      do k = 1, n
         rv = a(k)
         if ( rv.gt.1.0e-7 ) then
            a(k) = 30.0 - 2.5*alog10(rv)
         else
            a(k) = 1000.0
         endif
      enddo


      end















CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    TBTRAN_DO.FOR
C
C    Contains:-
C
C T_TBTRAN_DO      Apply transformation to positions
C TBTRD_TRANS      Load output with transformed input


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBTRAN_DO -- Apply transformation
C   For a fuller description see TBTRAN_DO.HLP
C
C   alan penny                     ral           1990-06-15

      subroutine t_tbtran_do ()

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'
C--
      real trc(6)
      integer ipin, ipo, ierr, iv
      character title*50
Cbegin


      call optabr ( 'IN', ipin, TBVX, TBY, .false., ierr )		!Obtain input table
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call azeror ( trc, 6 )						!Get transfomations
      trc(2) = 1.0
      trc(6) = 1.0
      call get3r ( 'XCOEFF', trc(1), trc(2), trc(3), .true., -1.0e20,
     +             1.0e20 )
      if ( ST_FAILED ) return
      call get3r ( 'YCOEFF', trc(4), trc(5), trc(6), .true., -1.0e20,
     +             1.0e20 )
      if ( ST_FAILED ) return

      call optabw ( 'OUT', ipo, TBVX, TBY, .false., ierr )		!Open output table
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call tcopdes ( 'IN', 'OUT', ierr )				!Get title to output list and
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call gtdesc ( 'IN', 'TITLE', title, 'Output from Tbtran_do',	! store it and the descriptors
     +              iv, ierr )
      call get1c  ( 'TITLE', title, title, .true. )
      if ( ST_FAILED ) return
      call ptdesc ( 'OUT', 'TITLE', title )

      call tbtrd_trans ( %val(ipin), trc, %val(ipo) )			!Load the Output from the Input


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTRD_TRANS -- Load output with transformed input
C
C  alan penny                 ral                1990-06-15

      subroutine tbtrd_trans ( in, trc, out )

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real	in(TBVX,TBY)	!i: Input table
      real	trc(6)		!i: Transformation table
      real	out(TBVX,TBY)	!o: Output table
C--
      integer k
Cbegin


      if ( ST_FAILED ) return

      call amovr ( in, out, TBVX*TBY )					!Transfer whole table

      do k = 1, TBY							!Make new positions
         out(6,k) = trc(1) + trc(2)*in(6,k) + trc(3)*in(7,k)
         out(7,k) = trc(4) + trc(5)*in(6,k) + trc(6)*in(7,k)
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   T_TBTRAN_LOAD.F
C
C    Contains:-
C
C T_TBTRAN_LOAD Load transformation coefficents from simple parameters


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBTRAN_LOAD -- Load transformation coefficents from simple parametersE
C
C         A J Penny            RAL			1991 May

      subroutine t_tbtran_load ()

      implicit none
      include 'STARMAN_INC'
C--
      real    cs, sn, cen(2), mag, rot, shift(2), trc(6)
Cbegin


      cen(1) = 0.0							!Get centre of rotn and magn
      cen(2) = 0.0
      call get2r ( 'CENTRE', cen(1), cen(2), .true., -1.0e20, 1.0e20 )
      if ( ST_FAILED ) return

      call get1r ( 'MAGNIFY', mag, 1.0, 0.0, 1.0e20 )			!Get magnification
      if ( ST_FAILED ) return

      call get1r ( 'ROTATE', rot, 0.0, -1.0e20, 1.0e20 )		!Get rotation
      if ( ST_FAILED ) return
      rot = rot*1.745329e-2

      shift(1) = 0.0							!Get shift
      shift(2) = 0.0
      call get2r ( 'SHIFT', shift(1), shift(2), .true.,-1.0e20,1.0e20)
      if ( ST_FAILED ) return

      cs = cos(rot)							!Calc transformation
      sn = sin(rot)
      trc(1) = shift(1) - cen(1)*mag*cs + cen(2)*mag*sn
      trc(2) = mag*cs
      trc(3) = -1.0*mag*sn
      trc(4) = shift(2) - cen(1)*mag*sn - cen(2)*mag*cs
      trc(5) = mag*sn
      trc(6) = mag*cs

      call pargr ( trc(1) )						!Output results
      call pargr ( trc(2) )
      call pargr ( trc(3) )
      call printd ( 'X2 = %f + %f*X1 + %f*Y1')
      call pargr ( trc(4) )
      call pargr ( trc(5) )
      call pargr ( trc(6) )
      call printd ( 'Y2 = %f + %f*X1 + %f*Y1')

      call put3r ( 'XCOEFF', trc(1), trc(2), trc(3) )			!Put in program parameters
      call put3r ( 'YCOEFF', trc(4), trc(5), trc(6) )


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    TBTRAN_MAKE.FOR
C
C    Contains:-
C
C T_TBTRAN_MAKE  Calc transformation coefficents between posns in two tables
C TBTRM_LOAD       Load positions into work arrays
C TBTRM_DOIT       Orgainise for transformation calculations
C TBTRM_LINTRAN    Calculate transformation


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBTRAN_MAKE -- Calc transformation coefficents between posns in two tables
C
C         A J Penny            RAL			1991 May

      subroutine t_tbtran_make ()

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'
C--
      integer k, ierr, ip1, ip2, ktype, ipwk1, ipwk2, ipwk3, ngood
      real    siglim, trc(6), sd
      logical donames

      integer nthelp
      parameter ( nthelp=6 )
      character*68 thelp(nthelp)
      data (thelp(k),k=1,6) /
     + 'Option        Function: Choose -' ,
     + '------        ------------------' ,
     + 'Full          Full 6 parameter fit',
     + 'Magnrotshift  Magnify, Rotate, XY shift fit',
     + 'Rotshift      Rotate, XY shift fit',
     + 'Shift         XY shift fit' /
CbegCbegin


      call optabr ( 'IN1', ip1, TBVX1, TBY1, .false., ierr )		!Input 1st table
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call optabr ( 'IN2', ip2, TBVX2, TBY2, .false., ierr )		!Input 2nd table
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) then
         if ( ierr.eq.3 ) then
            call printo ( 'ERROR: Cannot have just one file' )
         endif
         ST_FAILED = .true.
         return
      endif

      call get_job ( 'TRTYPE', 'shift:rotshift:magnrotshift:full', 	!Get type of transformation
     +                         ktype, 4, ' ', 0 )
      if ( ST_FAILED ) return

      call get1r ( 'SIGLIM', siglim, 2.5, 0.0, 1.0e20 )			!Get limit to mismatch for a star
      if ( ST_FAILED ) return

      call get1b ( 'NAMES', donames, .false. )				!Get if to do names or position in table
      if ( ST_FAILED ) return

      if ( .not.donames .and. (TBY1.ne.TBY2) ) then
         call printo ( 'ERROR: Tables must be of same length' )
         ST_FAILED = .true.
         return
      endif

      call gtwrkr ( 'WORK1', TBY1*2, ipwk1, ierr )
      if ( ierr.ne.0 ) ST_FAILED = .true.
      call gtwrkr ( 'WORK2', TBY1*2, ipwk2, ierr )
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call tbtrm_load ( %val(ip1), %val(ip2), %val(ipwk1),
     +                  %val(ipwk2), donames )
      if ( ST_FAILED ) return
      if ( TBYO.eq.0 ) then
         ST_FAILED = .true.
         return
      endif

      call gtwrkr ( 'WORK3', TBY1, ipwk3, ierr )
      if ( ierr.ne.0 ) ST_FAILED = .true.

      call tran_doit ( %val(ipwk1), %val(ipwk2), TBY1, TBYO, ktype,	!Do the loading of the Output from the Input
     +                siglim, %val(ipwk3), trc, ngood, sd )
      if ( ST_FAILED ) return

      call pargi ( ngood )						!Output results
      call printd ( 'Number of points used = %d ' )
      call pargr ( sd )
      call printd ( 'Std Dev of points from fit = %f ' )
      call pargr ( trc(1) )
      call pargr ( trc(2) )
      call pargr ( trc(3) )
      call printd ( 'X2 = %f + %f *X1 + %f *Y1 ')
      call pargr ( trc(4) )
      call pargr ( trc(5) )
      call pargr ( trc(6) )
      call printd ( 'Y2 = %f + %f *X1 + %f *Y1 ')

      call put3r ( 'XCOEFF', trc(1), trc(2), trc(3) )			!Put in program parameters
      call put3r ( 'YCOEFF', trc(4), trc(5), trc(6) )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTRM_LOAD -- Load positions into work arrays
C
C    alan penny           RAL                  1991 May

      subroutine tbtrm_load ( tb1, tb2, wk1, wk2, donames )

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real	tb1(TBVX1,TBY1)		!i: Input table 1
      real	tb2(TBVX2,TBY2)		!i: Input table 2
      real	wk1(TBY1,2)		!i: Work space 1
      real	wk2(TBY1,2)		!i: Work space 2
      logical   donames			!i: Use names or raw positions?
C--
      integer j, k, ka, kd
      real temp(5)
Cbegin


      if ( ST_FAILED ) return

      if ( .not.donames ) then
         TBYO = TBY1
         do k = 1, TBYO
            wk1(k,1) = tb1(6,k)
            wk1(k,2) = tb1(7,k)
            wk2(k,1) = tb2(6,k)
            wk2(k,2) = tb2(7,k)
         enddo
      else
         ka = 0
         do k = 1, TBY1
            call amovr ( tb1(1,k), temp, 5 )
            kd = 1
            j = 0
            do while ( j.lt.TBY2 .and. kd.eq.1 )
               j = j + 1
               call namechr ( tb2(1,j), temp, kd )
            enddo
            if ( kd.eq.0 ) then
               ka = ka + 1
               wk1(ka,1) = tb1(6,k)
               wk1(ka,2) = tb1(7,k)
               wk2(ka,1) = tb2(6,j)
               wk2(ka,2) = tb2(7,j)
            endif
         enddo
         TBYO = ka
         if ( TBYO.eq.0 ) call printo ( 'ERROR: No matching names' )
      endif


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    TBVALUE.FOR
C
C    Contains:-
C
C T_TBVALUE     Type out a tableelement


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBVALUE -- Type out a tableelement
C   For a fuller description see TBVALUE.HLP
C
C   alan penny                     ral           1991 Dec

      subroutine t_tbvalue ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      integer ip, ierr, iv
      character text*80
Cbegin


      call optabr ( 'IN', ip, TBVX, TBY, .false., ierr )		!Obtain input table
      if ( ST_FAILED ) return
      if ( ierr.ne.0 ) return
      TBX = TBVX - 5

      call gtdesc ( 'IN', 'TITLE', text, ' ', iv, ierr ) 		!Type summary
      call pargc ( text )
      call printd ( 'Title is:  %c' )
      call pargi ( TBX )
      call pargi ( TBY )
      call printd ( 'No of Columns - %d  ; No of Rows - %d' )
      call printo ( ' ' )

      call tbval_doit ( %val(ip) )					!Do the work


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBV_DOIT -- Do the work
C
C  alan penny                 ral                1990-06-15

      subroutine tbval_doit ( tb )

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'

      real         tb(TBVX,TBY)		!i: Input table
C--
      logical dohead, doname, found
      character*20 head, inhead, name, inname
      integer k, ncol, nrow, ierr
      real    val
Cbegin


      if ( ST_FAILED ) return

      call get1b ( 'USEHEAD', dohead, .false. )				!Get column
      if ( ST_FAILED ) return
      if ( dohead ) then
         call get1c ( 'HEADER', head, ' ', .false. )
      else
         call get1i ( 'COLUMN', ncol, 1, 1, TBX )
      endif
      if ( ST_FAILED ) return

      call get1b ( 'USENAME',   doname, .false. )			!Get row
      if ( ST_FAILED ) return
      if ( doname ) then
         call get1c ( 'NAME', name, ' ', .false. )
      else
         call get1i ( 'ROW', nrow, 1, 1, TBY )
      endif
      if ( ST_FAILED ) return

      if ( dohead ) then						!Get column from header
         found = .false.
         k = 0
         do while ( .not.found .and. k.lt.TBX )
            k = k + 1
            call gthead ( 'IN', k, inhead, ierr )
            if ( ierr.ne.0 ) then
               ST_FAILED = .true.
               return
            endif
            if ( inhead.eq.head ) then
               found = .true.
               ncol = k
            endif
         enddo
         if ( .not.found ) then
            call printo ( 'ERROR: Header not found' )
            ST_FAILED = .true.
            return
         endif
      endif

      if ( doname ) then						!Get column from header
         found = .false.
         k = 0
         do while ( .not.found .and. k.lt.TBY )
            k = k + 1
            call namegt ( tb, TBVX, TBY, k, inname )
            if ( inname.eq.name ) then
               found = .true.
               nrow = k
            endif
         enddo
         if ( .not.found ) then
            call printo ( 'ERROR: Name not found' )
            ST_FAILED = .true.
            return
         endif
      endif

      val = tb(5+ncol,nrow)						!Get value

      call pargbeg							!Type out results
      call pargr ( val )
      call printd ( 'Value is: %f' )
      if ( .not.dohead ) call gthead ( 'IN', ncol, head, ierr )
      if ( .not.doname ) call namegt ( tb, TBVX, TBY, nrow, name )
      call pargi ( ncol )
      call pargc ( head )
      call printd ( 'Column is  - Number : %d : Header : %c ' )
      call pargi ( nrow )
      call pargc ( name )
      call printd ( 'Row is     - Number : %d : Name : %c ' )

      call put1r ( 'VALUE', val )					!Put value out


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  TBWEED.FOR
C
C    Contains:-
C
C T_TBWEED   Make a table with only those entries with parameters in ranges
C TBWE_GCL    Get weeding parameters
C TBWE_CHECK  Find how many rows to output
C TBWE_DOIT   Move 'good' rows from input table to output table


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_TBWEED -- Make a table with only those entries with parameters in ranges
C This makes a new table which is a copy of the old table, but only those
C rows whose defined parameters fall in (or outside )the assigned ranges are
C copied over.
C
C         A J Penny            RAL                           1991 May

      subroutine t_tbweed ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_TABLE_INC'
C--
      character title*30
      integer ierr, kacc, ip, ipo, iv, ncol(10), ntot
      real toplim(10), botlim(10)
Cbegin


      call optabr ( 'IN', ip, TBVX, TBY, .false., ierr )		!Obtain input table
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call gtdesc ( 'IN', 'TITLE', title, 'Output from Tbweed', iv, 	!Extract Title
     +              ierr )
      TBX = TBVX - 5

      call pargc ( title )						!Type out info about table
      call printd ( ' Title = %c' )
      call pargi ( TBX )
      call pargi ( TBY )
      call printd ( ' No of columns = %d  No of rows = %d' )

      call tbwe_gcl ( ncol, botlim, toplim, ntot, kacc )			!Get weeding factors

      call tbwe_check ( %val(ip), ntot, ncol, botlim, toplim, kacc )	!Find no of entries passed

      if ( ST_FAILED ) return

      if ( TBYO.ne.0 ) then

         call optabw ( 'OUT', ipo, TBVX, TBYO, .false., ierr )		!Open output list
         if ( ierr.ne.0 ) ST_FAILED = .true.
         if ( ST_FAILED ) return

         call tcopdes ( 'IN', 'OUT', ierr )				!Get title to output list and store it and the descriptors
         if ( ierr.ne.0 ) ST_FAILED = .true.
         call get1c  ( 'TITLE', title, title, .true. )
         if ( ST_FAILED ) return
         call ptdesc ( 'OUT', 'TITLE', title )

         call tbwe_doit ( %val(ip), %val(ipo), ntot, ncol, botlim,	!Do the loading of the Output from the Input
     +                   toplim, kacc )

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBWE_GCL -- Get weeding parameters
C
C    a j penny               rgo                      82-11-4

      subroutine tbwe_gcl ( ncol, botlim, toplim, ntot, kacc )

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      integer	ncol(10)	!o: Columns to select on
      real	botlim(10)	!o: Bottom limits
      real	toplim(10)	!o: Top limits
      integer   ntot            !o: No of parameters to check
      integer	kacc		!o: Accept or reject flag (a/r=1/2)
C--
      integer k, kk, ncolin
      real    botlimin, toplimin
      logical more
      character text*72, namea*5, nameb*6
Cbegin


      if ( ST_FAILED ) return

      call printo ( 'Accept or reject inside the limits ?' )		!Get wether accept or reject inside the limits
      call get_job ( 'OPTION', 'accept:reject', kacc, 1, text, 0 )
      if ( ST_FAILED ) return

      more = .true.
      ntot = 0
      k = 0
      do while ( more .and. k.lt.10 )

         k = k + 1							!Get position of test parameter
         kk = min(k,TBX)
         if ( k.lt.10 ) then
            write ( namea, '(''NCOL'',i1)' ) k
            call get1i ( namea, ncolin, kk, 0, TBX )
         else
            write ( nameb, '(''NCOL'',i2)' ) k
            call get1i ( nameb, ncolin, kk, 0, TBX )
         endif
         if ( ST_FAILED ) return
         if ( ncolin.eq.0 ) then
            more = .false.
         else
            botlimin = 0.0						!Get the limits
            toplimin = 0.0
            if ( k.lt.10 ) then
               write ( namea, '(''LIMS'',i1)' ) k
               call get2r ( namea, botlimin, toplimin, .true.,
     +                      -1.0e20, 1.0e20 )
            else
               write ( nameb, '(''LIMS'',i2)' ) k
               call get2r ( nameb, botlimin, toplimin, .true.,
     +                      -1.0e20, 1.0e20 )
            endif
            if ( ST_FAILED ) return
            ntot = ntot + 1
            ncol(ntot) = ncolin
            botlim(ntot) = botlimin
            toplim(ntot) = toplimin
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBWE_CHECK -- Find how many rows to output
C
C    a j penny               rgo                      82-11-4

      subroutine tbwe_check ( tb, ntot, ncol, botlim, toplim, kacc )

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real	tb(TBVX,TBY)	!i: Input table
      integer   ntot            !i: No of parameters to check
      integer	ncol(10)	!i: Columns to select on
      real	botlim(10)	!i: Bottom limits
      real	toplim(10)	!i: Top limits
      integer	kacc		!i: Accept or reject flag (a/r=1/2)
C--
      integer j, k, ka
      real d
      logical ok
Cbegin


      if ( ST_FAILED ) return

      TBYO = 0
      do k = 1, TBY
         ok = .true.
         do j = 1, ntot
            ka = 5 + ncol(j)
            d = tb(ka,k)
            if ( kacc.eq.1 ) then
               if ( d.lt.botlim(j) .or. d.gt.toplim(j) ) ok = .false.
            else
               if ( d.ge.botlim(j) .and. d.lt.toplim(j) ) ok = .false.
            endif
         enddo
         if ( ok ) TBYO = TBYO + 1
      enddo

      if ( TBYO.eq.0 ) then
         call printo ('No entries in range(s) - no output table')
         return
      else
         call pargi ( TBYO )
         call printd ( 'Number of rows in output table = %d' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBWE_DOIT -- Move 'good' rows from input table to output table
C
C    alan penny              ral              1990-06-12

      subroutine tbwe_doit ( tb, tbo, ntot, ncol, botlim, toplim, kacc)

      implicit none
      include 'ST_TABLE_INC'
      include 'STARMAN_INC'

      real	tb(TBVX,TBY)		!i: Input table
      real	tbo(TBVX,TBYO)		!o: Output table
      integer   ntot            	!i: No of parameters to check
      integer	ncol(10)		!i: Columns to select on
      real	botlim(10)		!i: Bottom limits
      real	toplim(10)		!i: Top limits
      integer	kacc			!i: Accept or reject flag (a/r=1/2)
C--
      integer n, j, k, ka
      real d
      logical ok
Cbegin


      if ( ST_FAILED ) return

      n = 0
      do k = 1, TBY
         ok = .true.
         do j = 1, ntot
            ka = 5 + ncol(j)
            d = tb(ka,k)
            if ( kacc.eq.1 ) then
               if ( d.lt.botlim(j) .or. d.gt.toplim(j) ) ok = .false.
            else
               if ( d.ge.botlim(j) .and. d.lt.toplim(j) ) ok = .false.
            endif
         enddo
         if ( ok ) then
            n = n + 1
            call coprr ( tb, TBVX, TBY, 1, TBVX, k, k,
     +                   tbo, TBVX, TBYO, 1, n )
         endif
      enddo


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCALC -- Applies equations to columns in a table
C
C   For a description of this program see TBCALC.HLP
C
C
C           A J Penny          RAL           1991 Dec

      subroutine tbcalc ( ierradam )

      implicit none

      integer       ierradam            !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbcalc

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCHART -- (Program) Plot out a realistic star map
C   See TBCHART.HLP for details
C
C         A.J.Penny                RAL                  1991 May

      subroutine tbchart ( ierradam )

      implicit none

      integer      ierradam           !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbchart

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCOMPS -- (Program) Find stars within/not within a given annulus of each other
C   See TBCOMPS.HLP for details
C
C         A.J.Penny                RAL		1991 May

      subroutine tbcomps ( ierradam )

      implicit none

      integer      ierradam              !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbcomps

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCUT -- (Program) Cut sections out of a table
C   See TBCUT.HLP for details
C
C         A.J.Penny                RGO                             83-8-14

      subroutine tbcut ( ierradam )

      implicit none

      integer      ierradam              !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbcut

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBDES -- (Program) List, Write, delete descriptors of a table
C
C         A J Penny                RAL             1991 June

      subroutine tbdes ( ierradam )

      implicit none

      integer     ierradam        !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbdes

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  TBJOIN -- (Program) Join together two tables
C   See TBJOIN.HLP for details
C
C         A.J.Penny                RGO                             83-8-14

      subroutine tbjoin ( ierradam )

      implicit none

      integer     ierradam        !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbjoin

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBKEY -- Input numbers from keyboard into a table
C   See TBKEY.HLP for details
C
C         A.J.Penny                RAL               1991 May

      subroutine tbkey ( ierradam )

      implicit none

      integer        ierradam        !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbkey

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLIST -- Type and/or put in file the contents of a table
C   See TBLIST.HLP for details
C
C         A.J.Penny                RAL               1991 May

      subroutine tblist ( ierradam )

      implicit none

      integer      ierradam       !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tblist

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBLOAD -- Input an ASCII text file into a table
C   See TBLOAD.HLP for details
C
C         A.J.Penny                RAL               1991 May

      subroutine tbload ( ierradam )

      implicit none

      integer      ierradam          !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbload

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBMATCH -- (Program) Extract entries with matching names in two tables
C   See TBMATCH.HLP for details
C
C         A.J.Penny                RGO                             83-8-14

      subroutine tbmatch ( ierradam )

      implicit none

      integer     ierradam         !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbmatch

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBNATIVE -- (Program) Perform TBNATIVE program
C   See TBNATIVE.HLP for details
C
C         A.J.Penny                RAL                      1994 May

      subroutine tbnative ( ierradam )

      implicit none

      integer     ierradam         !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbnative

      call starman_end ( ierradam )

      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBNMATCH -- (Program) Extract entries with matching names in a number of tables
C   See TBNMATCH.HLP for details
C
C         A.J.Penny                RAL                      1994 May

      subroutine tbnmatch ( ierradam )

      implicit none

      integer     ierradam         !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbnmatch

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPLOT -- (Program) Plot out graph/histogram
C   See TBPLOT.HLP for details
C
C         A.J.Penny                RAL                  1991 May

      subroutine tbplot ( ierradam )

      implicit none

      integer    ierradam                 !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbplot

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBPMATCH -- (Program) Extract entries with matching positions in two tables
C   See TBPMATCH.HLP for details
C
C         A.J.Penny                RAL                      1991 May

      subroutine tbpmatch ( ierradam )

      implicit none

      integer      ierradam            !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbpmatch

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBRENUM -- (Program) Renumber names in a table
C   See TBRENUM.HLP for details
C
C         A.J.Penny                RGO                             83-8-14

      subroutine tbrenum ( ierradam )

      implicit none

      integer      ierradam              !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbrenum

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSHEET -- Spread sheet operation on a table
C
C         A J Penny                RAL             1991 June

      subroutine tbsheet ( ierradam )

      implicit none

      integer     ierradam             !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbsheet

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSORT -- (Program) Sort entries in a table
C   See TBSORT.HLP for details
C
C         A.J.Penny                RGO                             83-8-14

      subroutine tbsort ( ierradam )

      implicit none

      integer      ierradam            !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbsort

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  TBSTAT -- (Program) Calculate statistics on one or two columns
C   See TBSTAT.HLP for details
C
C      alan penny               RAL               1991 May

      subroutine tbstat  ( ierradam )

      implicit none

      integer     ierradam            !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbstat

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTRAN_AUTO -- (Program) Transform one XY table to another automatically
C
C    Pat Morris             leeds             1994 Jan
C    Alan Penny             ral               1994 Nov

      subroutine tbtran_auto ( ierradam )

      implicit none

      integer    ierradam            !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbtran_auto

      call starman_end ( ierradam )

      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTRAN_DO -- (Program) Apply transformation to positions in a table
C   See TBTRAN_DO.HLP for details
C
C         A.J.Penny                RGO                             83-8-14

      subroutine tbtran_do ( ierradam )

      implicit none

      integer     ierradam            !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbtran_do

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTRAN_LOAD -- (Program) Load transformation between positions
C   See TBTRAN_LOAD.HLP for details
C
C         A.J.Penny             RAL                     1991 May

      subroutine tbtran_load ( ierradam )

      implicit none

      integer      ierradam                !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbtran_load

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBTRAN_MAKE -- (Program) Calc transformation between positions in two tables
C   See TBTRAN_MAKE.HLP for details
C
C         A.J.Penny                RGO                             83-8-14

      subroutine tbtran_make ( ierradam )

      implicit none

      integer      ierradam           !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbtran_make

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBVALUE -- (Program) Types out table element and puts out as parameter
C   See TBVALUEM.HLP for details
C
C         A.J.Penny                ral               1991 Dec

      subroutine tbvalue ( ierradam )

      implicit none

      integer      ierradam               !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbvalue

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBWEED -- (Program) Weed from a table rows with values in forbidden ranges
C   See TBWEED.HLP for details
C
C         A.J.Penny                RAL                      1991 May

      subroutine tbweed ( ierradam )

      implicit none

      integer      ierradam           !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_tbweed

      call starman_end ( ierradam )

      end

