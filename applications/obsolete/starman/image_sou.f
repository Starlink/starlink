CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCALC -- (Program) Create an image by calculations on other images
C
C  alan penny                       ral                       1992 Dec

      subroutine imcalc ( ierradam )

      implicit none

      integer     ierradam           !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_imcalc

      call starman_end ( ierradam )


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCUBE -- (Program) Cuts a 2-D plane out of a 3- or 4-D cube
C
C     a j penny               ral                      1991 April

      subroutine imcube ( ierradam )

      implicit none

      integer     ierradam        !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_imcube

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCUT -- Cut, bin, and/or invert an area from an image to another
C
C         A J Penny                RAL             1991 June

      subroutine imcut ( ierradam )

      implicit none

      integer     ierradam        !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_imcut

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMDES -- (Program) List, write, delete descriptors of image
C
C         A J Penny                RAL             1991 June

      subroutine imdes ( ierradam )

      implicit none

      integer     ierradam        !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_imdes

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFITS_DR -- (Program) Convert a simple FITS image to a Starman NDF
C
C         A.J.Penny                RAL                  1991 May

      subroutine imfits_dr ( ierradam )

      implicit none

      integer      ierradam           !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_imfits_dr

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFLASH -- (Program) Flash up an image on the display
C
C    alan penny             ral               1990 Jan

      subroutine imflash ( ierradam )

      implicit none

      integer     ierradam           !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_imflash

      call starman_end ( ierradam )

      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJOIN -- (Program) Load an image into another
C
C  alan penny                       ral                       1991 march

      subroutine imjoin ( ierradam )

      implicit none

      integer     ierradam           !o: ADAM error flag

C--
Cbegin

      call starman_start

      call t_imjoin

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMKEY -- (Program) Create/Amend an image by keyboard entry
C
C  alan penny                       ral                       1992 Dec

      subroutine imkey ( ierradam )

      implicit none

      integer     ierradam           !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_imkey

      call starman_end ( ierradam )


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMPORT -- (Program) Port Extensions to/from Starman, FITS, Figaro others
C
C         A J Penny                RAL             1991 June

      subroutine import ( ierradam )

      implicit none

      integer     ierradam        !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_import

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMROTATE -- Rotate an image by 90/180/270 degrees
C
C         A J Penny                RAL             1991 June

      subroutine imrotate ( ierradam )

      implicit none

      integer     ierradam            !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_imrotate

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMSMOOTH -- (Program)
C
C         A.J.Penny                RAL                  1991 May

      subroutine imsmooth ( ierradam )

      implicit none

      integer      ierradam           !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_imsmooth

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMSTAT -- Calc statistics of an area of an image
C
C         A J Penny                RAL             1991 June

      subroutine imstat ( ierradam )

      implicit none

      integer      ierradam            !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_imstat

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMTYPE -- Convert type of image
C
C         A J Penny                RAL             1991 June

      subroutine imtype ( ierradam )

      implicit none

      integer     ierradam        !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_imtype

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMWEED -- Set a defined range of pixel values to INVALID
C
C         A J Penny                RAL             1995 Jan

      subroutine imweed ( ierradam )

      implicit none

      integer     ierradam            !o: ADAM error flag
C--
Cbegin

      call starman_start

      call t_imweed

      call starman_end ( ierradam )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMCALC.F
C
C  Contains:-
C
C T_IMCALC      Do image maths
C IMCA_SDEF     Set up defaults
C IMCA_GCL      Get input and output info from the command line
C IMCA_OUTTS    Get output image type and scale
C IMCA_SPACE    Get work space for line stack and line
C IMCA_POLISH   Decode equation and sort into reverse Polish
C IMCA_EQNERR   Put out equation error
C IMCA_DOIT     Combine the lines from the stack of images into one line
C IMCA_IMDIFF   Get number of diff input images and point to stack
C IMCA_VARDIFF  Get number of diff input variables and point to stack
C IMCA_LOAD(RS) Copy image into a 3-D stack
C IMCA_OUTL(RS) Load a line to the output image
C IMCA_SORT     Sort identifiers to alphabetical order



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMCALC -- Do image calculations
C
C  alan penny                  ral              1991 Dec

      subroutine t_imcalc ()

      implicit none
      include 'imcalc.inc'
      include 'STARMAN_INC'
C--
      integer k, kk, ierr
Cbegin

      call imca_sdef							!Set up defaults

      call imca_gcl							!Get input/output info
      if ( ST_FAILED ) return

      call imca_space							!Get work space
      if ( ST_FAILED ) return

      do k = 1, NYO

         if ( ANYIN ) then						!Load input images into stack
            do kk = 1, NZ
               if ( IMTY(kk).eq.'SHORT' ) then
                  call imca_loads ( %val(IP(kk)), NX(kk), NY(kk),
     +                              BS(kk), BZ(kk), INVALI(kk), k, kk,
     +                              %val(IPSTK), NXO, NZ )
               else
                  call imca_loadr ( %val(IP(kk)), NX(kk), NY(kk),
     +                              BS(kk), BZ(kk), RINVALI(kk), k, kk,
     +                              %val(IPSTK), NXO, NZ )
               endif
            enddo
         endif

         call imca_doit ( %val(IPSTK), NXO, NZ, K, OPCODE, NOPCODE, 	!Do the calculations
     +                    IMP, IMPV, VAR, CON, %val(ipl), KSEED, ierr )
         if ( ierr.ne.0 ) then
            ST_FAILED = .true.
            return
         endif

         if ( OUTTYPE.eq.'SHORT' ) then
            call imca_outls ( %val(ipl), %val(IPO), NXO, NYO, BSO,
     +                        BZO, INVALO, k )
         else
            call imca_outlr ( %val(ipl), %val(IPO), NXO, NYO, BSO,
     +                        BZO, RINVALO, k )
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_SDEF -- Set up defaults
C
C  alan penny                         RAL                1991 Dec

      subroutine imca_sdef ( )

      implicit none
      include 'imcalc.inc'
      include 'ST_LIMITS_INC'
C--
Cbegin


      IPL = 1
      IPSTK = 1

      NXO = 1
      NYO = 1

      ANYIN = .false.
      NZ = 1
      BSO = 1.0
      BZO = 0.0
      INVALO = INT_INVALSI
      RINVALO = INT_INVALR

      call azeroi ( IMP, 70 )						!Zero locators for images and varaibles
      call azeroi ( IMPV, 70 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_GCL -- Get input and output info from the command line
C
C  alan penny                         RAL                1991 Dec

      subroutine imca_gcl ( )

      implicit none
      include 'imcalc.inc'
      include 'STARMAN_INC'
C--
      integer k, ierr
      character title*50, texta*132, inim*3
Cbegin


      if ( ST_FAILED ) return

      call get1c ( 'EQN', texta, ' ', .false. )
      call lbgone ( texta )
      call charln ( texta, k )
      if ( k.eq.0 ) then
         call printo ( 'ERROR: Must have an equation' )
         ST_FAILED = .true.
         return
      endif
      EXPRS = texta

      call imca_polish ( EXPRS, OPCODE, NOPCODE, IMID, NIM,		!Translate it into reverse polish notation
     +                   IMP, VARID, NVAR, IMPV, CON, NCON, ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call imca_imdiff							!How many diff images?

      if ( .not.ANYIN ) then						!Get images
         NXO = 1
         NYO = 1
         call get2i ( 'NXY', NXO, NYO, .true., 1, 100000 )
      else
         do k = 1, NZ
            inim = 'IN'//IMTOT(k)(2:2)
            call opimzr ( inim, IP(k), NX(k), NY(k), IMTY(k), .false.,
     +                    ierr )
            if ( ierr.ne.0 ) then
               ST_FAILED = .true.
               return
            endif
            call gtimzd ( inim, IMTY(k), BS(k), BZ(k), INVALI(k),
     +                    RINVALI(k), texta, ierr)
            if ( ierr.ne.0 ) then
               ST_FAILED = .true.
               return
            endif
            if ( k.eq.1 ) then
               NXO = NX(k)
               NYO = NY(k)
               title = texta(1:50)
            else
               NXO = min(NXO,NX(k))
               NYO = min(NYO,NY(k))
            endif
         enddo
      endif

      call imca_vardiff							!How many diff variables?

      if ( NVARTOT.gt.0 ) then						!Get input variables
         do k = 1, NVARTOT
            call get1r ( VARTOT(k), VAR(k), 0.0, -1.0e37, +1.0e37 )
         enddo
      endif

      KSEED = 1								!Random generator seed
      call uppcase ( EXPRS, texta )
      if ( index(texta,'RAN').ne.0 .or. index(texta,'GAUSS').ne.0 )
     +   then
         call get1i ( 'SEED', KSEED, 1234567891, 1200000001,
     +                        1400000001 )
         call ajseed ( KSEED )
      endif

      call imca_outts							!Output image type and scale

      call opimzw ( 'OUT', OUTTYPE, IPO, NXO, NYO, .true., ierr )	!Get output image
      if ( ierr.eq.1 ) then
         ST_FAILED = .true.
         return
      endif

      inim = 'IN'//IMTOT(1)(2:2)
      if ( ANYIN ) then
         call icopdes ( inim, 'OUT', ierr )
         if ( ierr.eq.1 ) then
            ST_FAILED = .true.
            return
         endif
      endif

      if ( .not.ANYIN ) title = 'Output from IMCALC' 			!Output image title
      call get1c  ( 'TITLE', title, title, .true. )
      call ptdesc ( 'OUT', 'TITLE', title )

      call ptdesr ( 'OUT', 'BSCALE', BSO )
      call ptdesr ( 'OUT', 'BZERO', BZO )
      if ( OUTTYPE.eq.'REAL' ) then
         call ptdesr ( 'OUT', 'INVAL', RINVALO )
      else
         call ptdesi ( 'OUT', 'INVAL', INVALO )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_OUTTS -- Get output image type and scale
C
C  alan penny                RAL                1991 Dec

      subroutine imca_outts ( )

      implicit none
      include 'imcalc.inc'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
C--
      integer k
      logical bv

      integer kopt, kinr, kins
      integer nth
      parameter ( nth=5 )
      character*68 th(nth)
      data th /
     + 'Are the output data ? :- ',
     + 'Option   Choice',
     + '------   --------',
     + 'Real     Real 32-bit numbers',
     + 'Short    Signed 16-bit integer '/
Cbegin


      if ( ST_FAILED ) return

      if ( ANYIN ) then
         kins = 0
         kinr = 0
         do k = 1, NZ
            if ( IMTY(k).eq.'REAL' .and. kinr.eq.0 ) kinr = k
            if ( IMTY(k).eq.'SHORT'.and. kins.eq.0 ) kins = k
         enddo
      endif

      call printo ( ' ' )
      call printo ( 'Default output image type is REAL' )
      if ( ANYIN ) then
         if ( kins.ne.0 .and. kinr.eq.0 ) then
            call printo ( 'All input images have SHORT type -' )
            call printo ( 'Perhaps you want to keep the SHORT type' )
         elseif ( kins.ne.0 .and. kinr.ne.0 ) then
            call printo ( 'Input images are a mixture of REAL and'//
     +                    ' SHORT type -' )
            call printo ( 'Perhaps you want to have SHORT type' )
         else
            call printo ( 'All input images have REAL type -' )
            call printo ( 'Perhaps you want to change to SHORT type' )
         endif
      else
         call printo ( 'Perhaps you want to have SHORT type' )
      endif
      OUTTYPE = 'REAL'
      call get_job ( 'OUTTYPE', 'real:short', kopt, 1, th, nth )
      if ( kopt.eq.2 ) OUTTYPE = 'SHORT'
      if ( ST_FAILED ) return

      if ( ANYIN ) then

         if ( OUTTYPE.eq.'SHORT' ) then
            call printo ( ' ' )
            call printo ( 'The pixel values are stored as 16-bit'//
     +                     ' integers, as ((value-BZERO)/BSCALE)' )
            call printo ( 'As these are integers in the range '//
     +                    ' -32765 to +32767,' )
            call printo ( 'you may wish to change the -scale and '//
     +                    'zero-,' )
            call printo ( 'either to prevent integer steps being'//
     +                    ' important, or to store big numbers' )
            call get1r ( 'BSCALE', BSO, BS(1), -1.0e10, 1.0e10 )
            call get1r ( 'BZERO', BZO, BZ(1), -1.0e10, 1.0e10 )
            if ( kins.ne.0 ) then
               INVALO = INVALI(kins)
            else
               INVALO = INT_INVALSI
            endif
         else
            if ( BS(1).ne.1.0 .or. BZ(1).ne.0.0 ) then
               call printo ( ' ' )
               call pargr ( BS(1) )
               call pargr ( BZ(1) )
               call printd ( 'Output BSCALE and BZERO are %f %f ' )
               call printo ( 'For REAL images like this 1.0 and 0.0'//
     +                       ' are often more sensible.' )
               call get1b ( 'BCHOICE', bv, .true. )
               if ( bv ) then
                  BSO = 1.0
                  BZO = 0.0
               else
                  BSO = BS(1)
                  BZO = BZ(1)
               endif
            endif
            if ( kinr.ne.0 ) then
               RINVALO = RINVALI(kinr)
            else
               RINVALO = INT_INVALR
            endif
         endif

      else

         if ( OUTTYPE.eq.'SHORT' ) then
            call printo ( ' ' )
            call printo ( 'The pixel values are stored as 16-bit'//
     +                     ' integers, as ((value-BZERO)/BSCALE)' )
            call printo ( 'As these are integers in the range '//
     +                    ' -32765 to +32767,' )
            call printo ( 'you may wish to change the -scale and '//
     +                    'zero-,' )
            call printo ( 'either to prevent integer steps being'//
     +                    ' important, or to store big numbers' )
            call get1r ( 'BSCALE', BSO, 1.0, -1.0e10, 1.0e10 )
            call get1r ( 'BZERO', BZO, 0.0, -1.0e10, 1.0e10 )
            INVALO = INT_INVALSI
         else
            BSO = 1.0
            BZO = 0.0
            RINVALO = INT_INVALR
         endif

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_SPACE -- Get work space for line stack and line
C
C  alan penny                RAL                1991 Dec

      subroutine imca_space ( )

      implicit none
      include 'imcalc.inc'
      include 'STARMAN_INC'
C--
      integer ierr
Cbegin


      if ( .not.ANYIN ) then							!Work space for stack
         call gtwrkr ( 'STACK', 1, IPSTK, ierr )
         call azeror ( %val(IPSTK), 1 )
      else
         call gtwrkr ( 'STACK', NXO*NZ, IPSTK, ierr )
         call azeror ( %val(IPSTK), NXO*NZ )
      endif
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call gtwrkr ( 'LINE', NXO, IPL, ierr )				!Work space for stack
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_POLISH -- Decode equation and sort into reverse Polish
C
C  alan penny                RAL                1991 Dec

      subroutine imca_polish ( exprs, opcode, noper, imid,
     +                         nim, imp, varid, nvar, impv, con,
     +                         ncon, ierr)

      implicit none

      character*132    exprs		!i: Equation to decode
      character*6      opcode(70)	!o: Polish stack of commands
      integer          noper		!o: No of operations
      character*2      imid(26)		!o: images identifiers
      integer          nim		!o: Number of different images
      integer          imp(70)		!o: Pointers to images
      character*1      varid(70)	!o: Variables identifiers
      integer          nvar		!o: Number of variable
      integer          impv(70)		!o: Pointers to variables
      real             con(70)		!o: Constants
      integer          ncon		!o: Number of constants
      integer          ierr		!o: Error flag (0=ok;bad 2=;3=;4=)
C--

      character in*133, numchr*132, test*7, testa*7,
     +          output*7, fin*2, fina*3
      integer j, k, tos, stk(0:132), symb(132), ll, ncin, nnum,
     +        nsymb, iok, isymb, ndiff, istat, jj
      real    const
      logical opnext, found, ischar, isnumb, issign, lmore, atend,
     +        thisok, isfile
      external ischar, isnumb, issign

      integer maxsym
      parameter ( maxsym=38 )

      integer numendoper
      parameter ( numendoper=7 )
      character*1 endoper(numendoper)

      character*7 oper(-3:maxsym), opsymb(-3:maxsym)
      integer opl(-3:maxsym), opr(-3:maxsym), l(-3:maxsym),
     +        prl(-3:maxsym), prr(-3:maxsym)

      data ( oper(j), opsymb(j), l(j), opl(j), opr(j), prl(j),		!Set up image of operators,
     +        prr(j),j=-3,10 ) /					! symbols and their priorities
     +  '       ', 'LDCON  ', 1, 0, 0, 10, 10,
     +  '       ', 'LDVAR  ', 1, 0, 0, 10, 10,
     +  '       ', 'LDIM   ', 2, 0, 0, 10, 10,
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
     +  'XX     ', 'XX     ', 2, 0, 0, 10, 10,
     +  'YY     ', 'YY     ', 2, 0, 0, 10, 10,
     +  'CLIP(  ', 'CLIP(  ', 5, 0, 1, 10,  1,
     +  'GAUSS( ', 'GAUSS( ', 6, 0, 1, 10,  1,
     +  'RAN(   ', 'RAN(   ', 4, 0, 1, 10,  1,
     +  '%      ', '/      ', 1, 1, 1,  5,  5 /

      data endoper / '+', '-', '/', '*', ')', '=', ' ' /
Cbegin


      ierr = 0
      ncin = 0

      do k = 1, len(exprs)						!Remove embedded blanks
         if ( exprs(k:k).ne.' ' .and. ncin.lt.132  ) then		!Count no of characters
            ncin = ncin + 1
            in(ncin:ncin) = exprs(k:k)
         endif
      enddo

      exprs = in(1:ncin)						!Return the expression with blanks removed

      ncin = ncin + 1							!Append an '= ' operator to terminate the expression
      in(ncin:ncin) = '='

      nim = 0								!Initiallise counters
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
               call uppcase ( test, testa )
               if ( testa.eq.oper(nsymb) ) found = .true.
            endif
         enddo

         isfile = .false.
         if ( .not.found ) then						!If symbol was not found: -

            if ( opnext ) then						!Error if an operator expected
               ierr = 3
               call printo ( ' ' )
               call printo ( 'ERROR: Cant understand Equation - ' )
               call pargi ( k )
               call printd (
     +              'ERROR:   An Operator missing, at character: %d' )
               call printo (
     +              'ERROR:   - that is a plus, or a minus, etc')
               call imca_eqnerr ( exprs, k )
               return
            elseif ( in(k:k).eq.'!' ) then				!Is it marked as filename?

               nnum = 0
               numchr = ' '						!extract contiguous characters

               ll = index(in(k+1:),'!')					!File name would end with an !
               if ( ll.eq.0 ) then
                  ierr = 3
                  call printo ( ' ' )
                  call printo ( 'ERROR: Cant understand Equation - ' )
                  call pargi ( k )
                  call printd (
     +              'ERROR:   An ! at character: %d marks start of' )
                  call printo (
     +              'ERROR:   filename - but there is no end !')
                  call imca_eqnerr ( exprs, k )
                  return
               elseif ( ll.eq.1 ) then
                  ierr = 3
                  call printo ( ' ' )
                  call printo ( 'ERROR: Cant understand Equation - ' )
                  call pargi ( k )
                  call printd (
     +              'ERROR:   An ! at character: %d marks start of' )
                  call printo (
     +              'ERROR:   filename - but only another ! follows')
                  call imca_eqnerr ( exprs, k )
                  return
               endif
               nnum = ll + 1
               numchr = in(k+1:k+ll-1)

               call file_is ( numchr(:nnum-2), isfile )			!Try to read this as a file
               if ( isfile ) then
                  thisok = .true.
                  nsymb = -1
                  nim = nim + 1						!If it is a image, add name to image stack
                  fin(1:1) = 'I'
                  fin(2:2) = char(64+nim)
                  imid(nim) = fin
                  fina(1:2) = 'IN'
                  fina(3:3) = char(64+nim)
                  call file_par ( numchr(:nnum-2), fina, istat )
               endif

            else if ( (in(k:k).eq.'I' .or. in(k:k).eq.'i')		!If an operand was expected, it may be
     +                 .and. ischar(in(k+1:k+1)) ) then
               nsymb = -1						! a image, variable or const.
               nim = nim + 1						!If it is a image, add name to image stack
               imid(nim) = in(k:k+1)
            else if ( ischar(in(k:k)) ) then
               nsymb = -2						!If it is a variable, add name to variable stack
               nvar = nvar + 1
               varid(nvar) = in(k:k)
            else

               thisok = .false.

               nnum = 0							!Otherwise it may be a constant...
               numchr = ' '						! extract contiguous numerical characters

               atend = .false.						!Character may be part of a numerical constant
               ll = k - 1						! if it is 0..9 or '.'
               do while ( ll.lt.ncin .and. .not.atend )			! or if it is an 'e' following one of the above
                  ll = ll + 1						! or if it is a sign following an 'e'
                  if ( isnumb(in(ll:ll)) .or. (in(ll:ll).eq.'E') .or.
     +                 (in(ll:ll).eq.'E') .or.
     +                 (issign(in(ll:ll)).and.
     +               ((in(ll-1:ll-1).eq.'E').or.(in(ll-1:ll-1).eq.'E'))
     +               ) ) then
                     nnum = nnum + 1
                     numchr(nnum:nnum) = in(ll:ll)
                  else
                     atend = .true.					!End of number as soon as one of the above tests fails
                  endif
               enddo
               call chartor ( numchr(:nnum), const, iok )

               if ( iok.eq.0 .and. nnum.ne.0 ) then			!If successful, add constant to stack
                  thisok = .true.
                  ncon = ncon + 1
                  con(ncon) = const
                  nsymb = -3
                  l(nsymb) = nnum
               endif

               if ( .not.thisok ) then					!May be filename

                  nnum = 0						!Otherwise it may be a filename..
                  numchr = ' '						! extract contiguous characters

                  atend = .false.					!File name would end with an operator
                  ll = k - 1
                  do while ( ll.lt.ncin .and. .not.atend )
                     ll = ll + 1
                     do jj = 1, numendoper
                        if ( in(ll:ll).eq.endoper(jj) ) atend = .true.
                     enddo
                     if ( .not.atend ) then
                        nnum = nnum + 1
                        numchr(nnum:nnum) = in(ll:ll)
                     endif
                  enddo

                  call file_is ( numchr(:nnum), isfile )			!Try to read this as a file
                  if ( isfile .and. nnum.ne.0 ) then
                     thisok = .true.
                     nsymb = -1						!
                     nim = nim + 1						!If it is a image, add name to image stack
                     fin(1:1) = 'I'
                     fin(2:2) = char(64+nim)
                     imid(nim) = fin
                     fina(1:2) = 'IN'
                     fina(3:3) = char(64+nim)
                     call file_par ( numchr(:nnum), fina, istat )
                  endif

               endif

               if ( .not.thisok ) then
                  ierr = 2						!Otherwise there is a bad operand error
                  call printo ( ' ' )
                  call printo ( 'ERROR: Cant understand Equation -' )
                  call pargi ( k )
                  call printd (
     +            'ERROR:   Operand missing or bad, at character: %d' )
                  call printo ( 'ERROR:   - that is a file, '//
     +                      'a parameter, a constant, or a variable' )
                  call printo (
     +            'ERROR:   - If file name, the file may not exist' )
                  call imca_eqnerr ( exprs, k )
                  return
               endif

            endif
         endif

         j = j + 1							!Put the identified symbol into the output
         symb(j) = nsymb						! array and move the input pointer to the
         if ( isfile ) then
            k = k + nnum
         else
            k = k + l(nsymb)						! next symbol
         endif

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
               call printo ( ' ' )
               call printo ( 'ERROR: ) missing from Equation -' )	! in the input expression...quit qith error
               call printo ( 'ERROR: '//exprs(1:70) )
               return
            elseif ( index(output,')').ne.0 ) then
               ierr = 4
               call printo ( 'ERROR: (  missing from Equation -' )
               call printo ( 'ERROR: '//exprs(1:70) )
               call printo ( ' ' )
               return
            endif

            if ( output.ne.'POS'.and.output.ne.':' ) then		!If there is some output, disregard it
               noper = noper + 1					! if it is unary + or a comma
               opcode(noper) = output
            endif

            if ( output.ne.'=' ) lmore = .true.				!Return for next symbol if not the end

         endif

      enddo

      if ( nim.ge.1 ) then						!If images or variables are referenced, sort
         call imca_sort ( imid, nim, imp, ndiff, symb )			! their names into alphabetical order and obtain
         nim = ndiff							! pointers to allow them to be accessed in their
      endif								! original order
      if ( nvar.ge.1 ) then
         call imca_sort ( varid, nvar, impv, ndiff, symb )
         nvar = ndiff
      endif


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_EQNERR -- Put out equation error
C
C  alan penny                     RAL            1994 Jul

      subroutine imca_eqnerr ( exprs, k )

      implicit none

      character*(*)     exprs	!i: Equation
      integer           k	!i: Character where error is
C--
      character texto*90
Cbegin


      call printo ( 'ERROR:   Eqn: '//exprs(1:70) )
      texto(1:90) = 'ERROR:   Err: '
      if ( k.le.14 ) then
         texto(14+k:14+k+14) = '^ Here is error'
         call printo ( texto )
      elseif ( k.le.70 ) then
         texto(14+k-14:14+k) = 'Error is here ^'
         call printo ( texto )
      endif
      call printo ( ' ' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_DOIT -- Combine the lines from the stack of images into one line
C
C  alan penny                     RAL            1991 Dec

      subroutine imca_doit ( riml, nx, nz, ky, opcode, nopcode, imp,
     +                       impv, var, con, rimo, kseed, ierr )

      implicit none
      include 'ST_LIMITS_INC'

      integer      nx			!i: X size of input lines
      integer      nz			!i: Z size of input lines
      real         riml(nx,nz)		!i: Input lines
      integer      ky			!i: Y line in output image being done
      integer      nopcode		!i: Number of operations
      character*6  opcode(70)		!i: Code for operations
      integer      imp(70)		!i: Number of image in (i)th posn
      integer      impv(70)		!i: Number variable in (i)th posn
      real         var(26)		!i: Variables
      real         con(70)		!i: Constants
      real         rimo(nx)		!o: Output line
      integer      kseed		!i/o: Random number seed
      integer      ierr			!o: Error flag (0=ok;1=bad)
C--
      logical found, bad, more
      integer j, k, tos, numiim, nvar, ncon, ib, nop
      real    rv
      double precision s(0:132), a, b, c, dib, dvmin, dvmax, dv
      integer opwhich(70)

      integer nsymb
      parameter ( nsymb=37 )

      character opsymb(nsymb)*6						!Recognised operations
      data opsymb /
     + 'LDCON ', 'LDVAR ', 'LDIM  ', '=     ',
     + '-     ', '+     ', '**    ', '*     ', '/     ', 'NEG   ',
     + 'SQRT  ', 'EXP   ', 'LOG10 ', 'LOG   ', 'SIN   ', 'COS   ',
     + 'TAN   ', 'ASIN  ', 'ACOS  ', 'ATAN  ', 'ATAN2 ', 'SINH  ',
     + 'COSH  ', 'TANH  ', 'ABS   ', 'AINT  ', 'ANINT ', 'MOD   ',
     + 'SIGN  ', 'DIM   ', 'MIN   ', 'MAX   ', 'XX    ', 'YY    ',
     + 'CLIP  ', 'GAUSS ', 'RAN   ' /
Cbegin


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

      do j = 1, nx							!Do for all pixels in line

         tos = 0							!Initialise the arith, image,
         numiim = 0							! variable, constant stack pointers
         nvar = 0
         ncon = 0

         nop = 0							!Do the operations
         more = .true.
         bad = .false.

         do while ( nop.lt.nopcode .and. more )
            nop = nop + 1

            go to ( 101,102,103,104,105,106,107,108,109,110,111,112,
     +              113,114,115,116,117,118,119,120,121,122,123,124,
     +              125,126,127,128,129,130,131,132,133,134,135,136,
     +              137 ),
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
               tos = tos + 1						!Load image pixel on to stack
               numiim = numiim + 1
               rv = riml(j,imp(numiim))
               if ( rv.ne.INT_INVALR ) then
                  s(tos) = rv
               else
                  bad = .true.
               endif
            go to 100
 104        continue
               dv = s(tos)						!=  : end of calculation
               if ( dv.ge.dvmax .or. dv.le.dvmin ) then
                  rimo(j) = INT_INVALR
               else
                  rimo(j) = dv
               endif
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
               tos = tos + 1						!XX : X position in output image
               s(tos) = j
            go to 100
 134        continue
               tos = tos + 1						!YY : Y position in output image
               s(tos) = ky
            go to 100
 135        continue
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
 136        continue
               call gasdev ( rv, kseed )				!GAUSS : Put gaussian noise on tos
               s(tos) = s(tos)*dble(rv)
            go to 100
 137        continue
               call rano ( rv, kseed )					!RAN : Uniform random noise
               s(tos) = s(tos)*dble(rv)
            go to 100
 100        continue

            if ( bad ) then						!Invalid arithmetic operation done
               rimo(j) = INT_INVALR
               more = .false.
            endif

         enddo

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_IMDIFF -- Get number of diff input images and point to stack
C
C  alan penny                         RAL                1991 Dec

      subroutine imca_imdiff ( )

      implicit none
      include 'imcalc.inc'
C--
      integer j, kk
      character ch*2
      logical found
Cbegin


      NZ = 0

      if ( NIM.ne.0 ) then
         do j = 1, NIM
            if ( NZ.eq.0 ) then
               NZ = 1
               IMTOT(NZ) = IMID(j)(1:2)
            else
               found = .false.
               ch = IMID(j)(1:2)
               do kk = 1, NZ
                  if ( ch.eq.IMTOT(kk) ) found = .true.
               enddo
               if ( .not.found ) then
                  NZ = NZ + 1
                  IMTOT(NZ) = IMID(j)(1:2)
               endif
            endif
         enddo
      endif

      if ( NZ.ne.0 ) then
         ANYIN = .true.
      else
         ANYIN = .false.
         NZ = 1
      endif

      do j = 1, 70
         if ( IMP(j).ne.0 ) then
            ch = IMID(IMP(j))(1:2)
            do kk = 1, NZ
               if ( ch.eq.IMTOT(kk) ) IMP(j) = kk
            enddo
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_VARDIFF -- Get number of diff input variables and point to stack
C
C  alan penny                         RAL                1991 Dec

      subroutine imca_vardiff ( )

      implicit none
      include 'imcalc.inc'
C--
      integer j, kk
      character ch*1
      logical found
Cbegin


      NVARTOT = 0

      if ( NVAR.ne.0 ) then
         do j = 1, NVAR
            if ( NVARTOT.eq.0 ) then
               NVARTOT = 1
               VARTOT(NVARTOT) = VARID(j)(1:1)
            else
               found = .false.
               ch = VARID(j)(1:1)
               do kk = 1, NVARTOT
                  if ( ch.eq.VARTOT(kk) ) found = .true.
               enddo
               if ( .not.found ) then
                  NVARTOT = NVARTOT + 1
                  VARTOT(NVARTOT) = VARID(j)(1:1)
               endif
            endif
         enddo
      endif

      do j = 1, 70
         if ( IMPV(j).ne.0 ) then
            ch = VARID(IMPV(j))(1:1)
            do kk = 1, NVARTOT
               if ( ch.eq.VARTOT(kk) ) IMPV(j) = kk
            enddo
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_LOADR -- Copy real image into a 3-D stack
C
C  alan penny                 ral                     1991 Dec

      subroutine imca_loadr ( im, nx, ny, bs, bz, rinvali, jn, jzn,
     +                        rim, nxo, nz )

      implicit none
      include 'ST_LIMITS_INC'

      integer     nx			!i: X size of input image
      integer     ny			!i: Y size of input image
      real        im(nx,ny)		!i: Input image
      real        bs			!i: Scale of input image
      real        bz			!i: Zero of input image
      real        rinvali		!i: Invalid flag of input image
      integer     jn			!i: Y line of input image to take
      integer     jzn			!i: Z line to put it in
      integer     nxo			!i: X size of stack line
      integer     nz			!i: Z size of stack line
      real        rim(nxo,nz)		!i/o: stack image of a line
C--
      integer k
      real    rv
Cbegin


      do k = 1, nxo
         rv = im(k,jn)
         if ( rv.eq.rinvali ) then
            rim(k,jzn) = INT_INVALR
         else
            rim(k,jzn) = bs*rv + bz
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_LOADS -- Copy int*2 image into a 3-D stack
C
C  alan penny                 ral                     1991 Dec

      subroutine imca_loads ( im, nx, ny, bs, bz, invali, jn, jzn,
     +                        rim, nxo, nz )

      implicit none
      include 'ST_LIMITS_INC'

      integer     nx			!i: X size of input image
      integer     ny			!i: Y size of input image
      integer*2   im(nx,ny)		!i: Input image
      real        bs			!i: Scale of input image
      real        bz			!i: Zero of input image
      integer     invali		!i: Invalid flag of input image
      integer     jn			!i: Y line of input image to take
      integer     jzn			!i: Z line to put it in
      integer     nxo			!i: X size of stack line
      integer     nz			!i: Z size of stack line
      real        rim(nxo,nz)		!i/o: stack image of a line
C--
      integer k, kv
Cbegin


      do k = 1, nxo
         kv = im(k,jn)
         if ( kv.eq.invali ) then
            rim(k,jzn) = INT_INVALR
         else
            rim(k,jzn) = bs*real(kv) + bz
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_OUTLR -- Load a line to the real output image
C
C  alan penny            ral                     1991 Dec

      subroutine imca_outlr  ( rimi, rimo, nx, ny, bs, bz, rinval, kl )

      implicit none
      include 'ST_LIMITS_INC'

      integer    nx		!i: length of line (= X size of output)
      integer    ny		!i: Y Size of output
      real       rimi(nx)	!i: Line
      real       rimo(nx,ny)	!o: Output image
      real       bs             !i: Image scale
      real       bz             !i: Image zero
      real       rinval         !i: Image invalid flag
      integer    kl             !i: Y Line to load
C--
      integer j
      real rv
Cbegin


      do j = 1, nx
         rv = rimi(j)
         if ( rv.eq.INT_INVALR ) then
            rimo(j,kl) = rinval
         else
            rv = (rv-bz)/bs
            if ( rv.lt.INT_MINRR .or. rv.gt.INT_MAXRR ) then
               rimo(j,kl) = rinval
            else
               rimo(j,kl) = rv
            endif
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_OUTLS -- Load a line to the int*2 output image
C
C  alan penny            ral                     1991 Dec

      subroutine imca_outls  ( rim, imo, nx, ny, bs, bz, inval, kl )

      implicit none
      include 'ST_LIMITS_INC'

      integer    nx		!i: length of line (= X size of output)
      integer    ny		!i: Y Size of output
      real       rim(nx)	!i: Line
      integer*2  imo(nx,ny)	!o: Output image
      real       bs             !i: Image scale
      real       bz             !i: Image zero
      integer    inval          !i: Image invalid flag
      integer    kl             !i: Y Line to load
C--
      integer j
      real rv
Cbegin


      do j = 1, nx
         rv = rim(j)
         if ( rv.eq.INT_INVALR ) then
            imo(j,kl) = inval
         else
            rv = (rv-bz)/bs
            if ( rv.lt.INT_MINSR .or. rv.gt.INT_MAXSR ) then
               imo(j,kl) = inval
            else
               imo(j,kl) = rv
            endif
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCA_SORT -- Sort identifiers to alphabetical order
C
C  alan penny            ral                     1991 Dec

      subroutine imca_sort ( c, nc, imp, ndiff, iw )

      implicit none

      integer        nc		!i: Number of identifiers
      character*(*)  c(nc)	!i/o: Identiers
      integer        imp(nc)	!o: pointer to identifier place in stack
      integer        ndiff	!o: Number of different identifiers
      integer        iw(nc)	!o: Work space
C--
      integer k, nn, it
      character*132 text
      logical repeat
Cbegin


      ndiff = 1								!Only 1?
      iw(1) = 1
      imp(1) = 1
      if ( nc.le.1 ) return

      nn = min(132,len(c(1)))						!Length of input strings

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


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  T_IMCUBE.F
C
C   Contains:-
C
C T_IMCUBE     Cut a plane out of a 3- or 4-D cube
C IMCB_START   Get parameters and Open input and output image
C IMCB_DOIT    Copy data from input to output - base
C IMCB_DOITR   Copy data from input to output - real
C IMCB_DOITI   Copy data from input to output - integer
C IMCB_DOITS   Copy data from input to output - short


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMCUBE -- Cut a plane out of a 3- or 4-D cube
C
C   alan penny               ral                 1991 April

      subroutine t_imcube ()

      implicit none
C--
Cbegin


      call imcb_start			!Get parameters and open images

      call imcb_doit			!Do the conversion


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCB_START -- Get parameters and Open input and output image
C
C alan penny          ral                   1991 April

      subroutine imcb_start ()

      implicit none
      include 'imcube.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer i, j, nd(4), istat
      character*80 cv, cva

      character*6 tplane1(3), tplane2(6), tplane3(6)
      data tplane1, tplane2, tplane3 / 'ZPLANE', 'YPLANE', 'XPLANE',
     +   'ZPLANE', 'YPLANE', 'YPLANE', 'XPLANE', 'XPLANE', 'XPLANE',
     +   'TPLANE', 'TPLANE', 'ZPLANE', 'TPLANE', 'ZPLANE', 'YPLANE' /
      integer nd1(3), nd2(6), nd3(6)
      data nd1, nd2, nd3 / 3, 2, 1, 3, 2, 2, 1, 1, 1,
     +                              4, 4, 3, 4, 3, 2 /

      integer nth2
      parameter ( nth2=6 )
      character*68 th2(nth2)
      data th2 /
     + 'Cut which plane out of 3-D (XYZ) cube? :- ',
     + 'Option   Choice',
     + '------   --------',
     + 'XY       XY plane',
     + 'XZ       XZ plane',
     + 'YZ       YZ plane'/

      integer nth3
      parameter ( nth3=9 )
      character*68 th3(nth3)
      data th3 /
     + 'Cut which plane out of 4-D (XYZT) cube? :- ',
     + 'Option   Choice',
     + '------   --------',
     + 'XY       XY plane',
     + 'XZ       XZ plane',
     + 'XT       XT plane',
     + 'YZ       YZ plane',
     + 'YT       YT plane',
     + 'TZ       TZ plane'/
Cbegin


      call opim4zr ( 'IN', IPIM, NX, NY, NZ, NT, NDIM, IMTYPE,		!Get input image
     +               .false., i )
      if ( i.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      if ( NDIM.lt.4 ) NT = 1
      if ( NDIM.lt.3 ) NZ = 1

      call pargi ( NX )							!Type out size of input image
      call pargi ( NY )
      if ( NDIM.eq.2 ) then
         call printd ( ' Input image is 2-D (XY) : %d x %d' )
      elseif ( NDIM.eq.3 ) then
         call pargi ( NZ )
         call printd ( ' Input image is 3-D (XYZ) : %d x %d x %d' )
      else
         call pargi ( NZ )
         call pargi ( NT )
         call printd(' Input image is 4-D (XYZT) : %d x %d x %d x %d')
      endif
      call pargc ( IMTYPE )
      call printd ( ' Image type is: %c ' )
      call printo ( ' ' )
      NPLANE = 1							!Get plane to cut
      if ( NDIM.le.2 ) then
         call printo ( 'As input image is 2D, 2D output image is same')
      elseif ( NDIM.eq.3 ) then
         call printo ( 'Planes to choose from are: xy:xz:yz' )
         call get_job ( 'PLANE3', 'xy:xz:yz', NPLANE, 1, th2, nth2 )
      elseif ( NDIM.eq.4 ) then
         call printo ( 'Planes to choose from are: xy:xz:xt:yz:yt:zt')
         call get_job ( 'PLANE4', 'xy:xz:xt:yz:yt:zt', NPLANE, 1, th3,
     +                  nth3 )
      endif

      nd(1) = NX							!Get which plane(s) to cut from
      nd(2) = NY
      nd(3) = NZ
      nd(4) = NT
      NP(1) = 1
      NP(2) = NZ
      NPP(1) = 1
      NPP(2) = NT
      if ( NDIM.eq.3 ) then
         call get2i ( tplane1(NPLANE), NP(1), NP(2),
     +                .true., 1, nd(nd1(NPLANE)) )
      elseif ( NDIM.eq.4 ) then
         call get2i ( tplane2(NPLANE), NP(1), NP(2),
     +                .true., 1, nd(nd2(NPLANE)) )
         call get2i ( tplane3(NPLANE), NPP(1), NPP(2),
     +                .true., 1, nd(nd3(NPLANE)) )
      endif

      NXO = NX
      if ( NPLANE.eq.3 .and. NDIM.eq.3 ) NXO = NY
      if ( NPLANE.eq.4 ) NXO = NY
      if ( NPLANE.eq.5 ) NXO = NY
      if ( NPLANE.eq.6 ) NXO = NZ
      NYO = NY
      if ( NPLANE.eq.2 ) NYO = NZ
      if ( NPLANE.eq.3 .and. NDIM.eq.3 ) NYO = NZ
      if ( NPLANE.eq.3 .and. NDIM.eq.4 ) NYO = NT
      if ( NPLANE.eq.4 ) NYO = NZ
      if ( NPLANE.eq.5 ) NYO = NT
      if ( NPLANE.eq.6 ) NYO = NT

      if ( IMTYPE.eq.'REAL' )  call opimrw ( 'OUT', IPO, NXO, NYO, 	!Get output image
     +                                       .false.,i)
      if ( IMTYPE.eq.'INT' )   call opimiw ( 'OUT', IPO, NXO, NYO,
     +                                       .false.,i)
      if ( IMTYPE.eq.'SHORT' ) call opimsw ( 'OUT', IPO, NXO, NYO,
     +                                       .false.,i)
      if ( i.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call icopdes (  'IN', 'OUT', i )					!Copy descriptors
      call ptdesi ( 'OUT', 'NAXIS', 2 )
      if ( NDIM.ge.3 ) call dldes ( 'OUT', 'NAXIS3', istat )
      if ( NDIM.ge.4 ) call dldes ( 'OUT', 'NAXIS4', istat )

      call gtdesc ( 'IN', 'TITLE', cv, ' ', j, i )			!Get output image title
      call get1c ( 'TITLE', cva, cv, .true. )
      call ptdesc ( 'OUT', 'TITLE', cva )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCB_DOIT -- Copy data from input to output - base
C
C alan penny          ral                   1991 April

      subroutine imcb_doit ( )

      implicit none
      include 'imcube.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
Cbegin


      if ( ST_FAILED ) return

      if ( IMTYPE.eq.'REAL'  ) call imcb_doitr ( %val(IPIM), %val(IPIM),
     +                                           %val(IPIM), %val(IPO) )
      if ( IMTYPE.eq.'INT'   ) call imcb_doiti ( %val(IPIM), %val(IPIM),
     +                                           %val(IPIM), %val(IPO) )
      if ( IMTYPE.eq.'SHORT' ) call imcb_doits ( %val(IPIM), %val(IPIM),
     +                                           %val(IPIM), %val(IPO) )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCB_DOITR -- Copy data from input to output - real
C
C alan penny          ral                   1991 April

      subroutine imcb_doitr ( in2, in3, in4, out )

      implicit none
      include 'imcube.inc'
      include 'ST_IMAGE_INC'

      real       in2(NX,NY)		!i: Input 2-D data
      real       in3(NX,NY,NZ)		!i: Input 3-D data
      real       in4(NX,NY,NZ,NT)	!i: Input 4-D data
      real       out(NXO,NYO)		!o: Output integer image
C--
      integer j, k, kk, kka, kkb, kf
Cbegin


      if ( NDIM.eq.2 ) then

         do k = 1, NYO
            do j = 1, NXO
               out(j,k) = in2(j,k)
            enddo
         enddo

      elseif ( NDIM.eq.3 ) then

         if ( NPLANE.eq.1 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kk = NP(1), NP(2)
                     if ( in3(j,k,kk).eq.RINVAL ) then
                        kf = 1
                     else
                        out(j,k) = out(j,k) + in3(j,k,kk)
                     endif
                  enddo
                  if ( kf.eq.1 ) out(j,k) = RINVAL
               enddo
            enddo
         elseif ( NPLANE.eq.2 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kk = NP(1), NP(2)
                     if ( in3(j,kk,k).eq.RINVAL ) then
                        kf = 1
                     else
                        out(j,k) = out(j,k) + in3(j,kk,k)
                     endif
                  enddo
                  if ( kf.eq.1 ) out(j,k) = RINVAL
               enddo
            enddo
         else
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kk = NP(1), NP(2)
                     if ( in3(kk,j,k).eq.RINVAL ) then
                        kf = 1
                     else
                        out(j,k) = out(j,k) + in3(kk,j,k)
                     endif
                  enddo
                  if ( kf.eq.1 ) out(j,k) = RINVAL
               enddo
            enddo
         endif

      elseif ( NDIM.eq.4 ) then

        if ( NPLANE.eq.1 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(j,k,kka,kkb).eq.RINVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(j,k,kka,kkb)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = RINVAL
               enddo
            enddo
        elseif ( NPLANE.eq.2 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(j,kka,k,kkb).eq.RINVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(j,kka,k,kkb)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = RINVAL
               enddo
            enddo
        elseif ( NPLANE.eq.3 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(j,kka,kkb,k).eq.RINVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(j,kka,kkb,k)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = RINVAL
               enddo
            enddo
        elseif ( NPLANE.eq.4 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(kka,j,k,kkb).eq.RINVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(kka,j,k,kkb)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = RINVAL
               enddo
            enddo
        elseif ( NPLANE.eq.5 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(kka,j,kkb,k).eq.RINVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(kka,j,kkb,k)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = RINVAL
               enddo
            enddo
        else
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(kka,kkb,j,k).eq.RINVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(kka,kkb,j,k)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = RINVAL
               enddo
            enddo
        endif

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCB_DOITI -- Copy data from input to output - integer
C
C alan penny          ral                   1991 April

      subroutine imcb_doiti ( in2, in3, in4, out )

      implicit none
      include 'imcube.inc'
      include 'ST_IMAGE_INC'

      integer    in2(NX,NY)		!i: Input 2-D data
      integer    in3(NX,NY,NZ)		!i: Input 3-D data
      integer    in4(NX,NY,NZ,NT)	!i: Input 4-D data
      integer    out(NXO,NYO)		!o: Output integer image
C--
      integer j, k, kk, kka, kkb, kf
Cbegin


      if ( NDIM.eq.2 ) then

         do k = 1, NYO
            do j = 1, NXO
               out(j,k) = in2(j,k)
            enddo
         enddo

      elseif ( NDIM.eq.3 ) then

         if ( NPLANE.eq.1 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kk = NP(1), NP(2)
                     if ( in3(j,k,kk).eq.INVAL ) then
                        kf = 1
                     else
                        out(j,k) = out(j,k) + in3(j,k,kk)
                     endif
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
         elseif ( NPLANE.eq.2 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kk = NP(1), NP(2)
                     if ( in3(j,kk,k).eq.INVAL ) then
                        kf = 1
                     else
                        out(j,k) = out(j,k) + in3(j,kk,k)
                     endif
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
         else
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kk = NP(1), NP(2)
                     if ( in3(kk,j,k).eq.INVAL ) then
                        kf = 1
                     else
                        out(j,k) = out(j,k) + in3(kk,j,k)
                     endif
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
         endif

      elseif ( NDIM.eq.4 ) then

        if ( NPLANE.eq.1 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(j,k,kka,kkb).eq.INVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(j,k,kka,kkb)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
        elseif ( NPLANE.eq.2 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(j,kka,k,kkb).eq.INVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(j,kka,k,kkb)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
        elseif ( NPLANE.eq.3 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(j,kka,kkb,k).eq.INVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(j,kka,kkb,k)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
        elseif ( NPLANE.eq.4 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(kka,j,k,kkb).eq.INVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(kka,j,k,kkb)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
        elseif ( NPLANE.eq.5 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(kka,j,kkb,k).eq.INVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(kka,j,kkb,k)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
        else
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(kka,kkb,j,k).eq.INVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(kka,kkb,j,k)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
        endif

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCB_DOITS -- Copy data from input to output - short
C
C alan penny          ral                   1991 April

      subroutine imcb_doits ( in2, in3, in4, out )

      implicit none
      include 'imcube.inc'
      include 'ST_IMAGE_INC'

      integer*2  in2(NX,NY)		!i: Input 2-D data
      integer*2  in3(NX,NY,NZ)		!i: Input 3-D data
      integer*2  in4(NX,NY,NZ,NT)	!i: Input 4-D data
      integer*2  out(NXO,NYO)		!o: Output integer image
C--
      integer j, k, kk, kka, kkb, kf
Cbegin


      if ( NDIM.eq.2 ) then

         do k = 1, NYO
            do j = 1, NXO
               out(j,k) = in2(j,k)
            enddo
         enddo

      elseif ( NDIM.eq.3 ) then

         if ( NPLANE.eq.1 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kk = NP(1), NP(2)
                     if ( in3(j,k,kk).eq.INVAL ) then
                        kf = 1
                     else
                        out(j,k) = out(j,k) + in3(j,k,kk)
                     endif
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
         elseif ( NPLANE.eq.2 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kk = NP(1), NP(2)
                     if ( in3(j,kk,k).eq.INVAL ) then
                        kf = 1
                     else
                        out(j,k) = out(j,k) + in3(j,kk,k)
                     endif
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
         else
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kk = NP(1), NP(2)
                     if ( in3(kk,j,k).eq.INVAL ) then
                        kf = 1
                     else
                        out(j,k) = out(j,k) + in3(kk,j,k)
                     endif
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
         endif

      elseif ( NDIM.eq.4 ) then

        if ( NPLANE.eq.1 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(j,k,kka,kkb).eq.INVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(j,k,kka,kkb)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
        elseif ( NPLANE.eq.2 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(j,kka,k,kkb).eq.INVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(j,kka,k,kkb)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
        elseif ( NPLANE.eq.3 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(j,kka,kkb,k).eq.INVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(j,kka,kkb,k)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
        elseif ( NPLANE.eq.4 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(kka,j,k,kkb).eq.INVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(kka,j,k,kkb)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
        elseif ( NPLANE.eq.5 ) then
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(kka,j,kkb,k).eq.INVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(kka,j,kkb,k)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
        else
            do k = 1, NYO
               do j = 1, NXO
                  kf = 0
                  out(j,k) = 0.0
                  do kkb = NPP(1), NPP(2)
                     do kka = NP(1), NP(2)
                        if ( in4(kka,kkb,j,k).eq.INVAL ) then
                           kf = 1
                        else
                           out(j,k) = out(j,k) + in4(kka,kkb,j,k)
                        endif
                     enddo
                  enddo
                  if ( kf.eq.1 ) out(j,k) = INVAL
               enddo
            enddo
        endif

      endif


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  IMCUT.FOR
C
C    Contains:-
C
C T_IMCUT       Cut, bin, and/or invert an area from an image to another
C IMC_DOIT(RS)  Load the output image from the input image


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMCUT -- Cut, bin, and/or invert an area from an image to another
C   For a fuller description see IMCUT.HLP
C
C   alan penny                     ral           1991 May

      subroutine t_imcut ( )

      implicit none
      include 'imcut.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
C--
      integer ierr, num, ipo, nxo, nyo, lim, k
      character*50 title, cv
      logical bv

      integer nth
      parameter ( nth=6 )
      character*68 th(nth)
      data th /
     + 'Method of extracting output value from binning box',
     + 'Option   Choice',
     + '------   --------',
     + 'Average  Take average of all pixels',
     + 'Highest  Take highest pixel',
     + 'Sample   Take value of bottom left hand pixel in box'/

Cbegin


      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., ierr )
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return
      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE, ierr )
      if ( ST_FAILED ) return

      NXS = 1
      NXE = NX
      call get2i ( 'XRANGE', NXS, NXE, .true., 1, NX )
      if ( ST_FAILED ) return
      XREV = .false.
      if ( NXS.gt.NXE ) XREV = .true.
      call cswopi ( NXS, NXE )

      NYS = 1
      NYE = NY
      call get2i ( 'YRANGE', NYS, NYE, .true., 1, NY )
      if ( ST_FAILED ) return
      YREV = .false.
      if ( NYS.gt.NYE ) YREV = .true.
      call cswopi ( NYS, NYE )

      NXST = 1
      NYST = 1
      lim = max(NX,NY)
      call get2i ( 'BIN', NXST, NYST, .true., 1, lim )
      if ( ST_FAILED ) return
      NXST = min(NXST,NX)
      NYST = min(NYST,NY)

      nxo = NXE - NXS + 1
      nyo = NYE - NYS + 1
      if ( NXST.ne.1 .or. NYST.ne.1 ) then
         nxo = nxo/NXST
         nyo = nyo/NYST
         if ( nxo.lt.1 .or. nyo.lt.1 ) then
            call printo ( 'ERROR: Bin size must less than image size' )
            return
         endif
      endif

      SAMPLE = 'sample '
      if ( NXST.ne.1 .or. NYST.ne.1 ) then
         call get_job ( 'SAMPLE', 'sample:highest:average', k, 2, th,
     +                  nth )
         if ( ST_FAILED ) return
         SAMPLE = 'sample '
         if ( k.eq.2 ) SAMPLE = 'highest'
         if ( k.eq.3 ) SAMPLE = 'average'
         if ( SAMPLE.eq.'average' ) then
            call get1b ( 'CHECK', bv, .false. )
            if ( ST_FAILED ) return
            IGNORE = .not.bv
         endif
      endif

      call opimzw ( 'OUT', IMTYPE, ipo, nxo, nyo, .false., ierr )
      if ( ierr.ne.0 ) ST_FAILED = .true.

      call gtdesc ( 'IN', 'TITLE', cv, 'Output from Imcut', num, ierr )

      call get1c ( 'TITLE', title, cv, .true. )

      if ( ST_FAILED ) return
      if ( IMTYPE.eq.'SHORT' ) then
         call imc_doits ( %val(IPIM), %val(ipo), nxo, nyo )
      else
         call imc_doitr ( %val(IPIM), %val(ipo), nxo, nyo )
      endif

      call icopdes ( 'IN', 'OUT', ierr )
      call ptdesc ( 'OUT', 'TITLE', title )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMC_DOITR -- Load the output image from the real input image
C
C   alan penny                     ral           1991 May

      subroutine imc_doitr ( in, out, nxo, nyo )

      implicit none
      include 'ST_IMAGE_INC'
      include 'imcut.inc'
C--
      real        in(NX,NY)		!i: Input image
      integer     nxo			!i: Output image X size
      integer     nyo			!i: Output image Y size
      real        out(nxo,nyo)		!o: Output image
C--
      integer j, jj, ja, jb, k, kk, ka, kb, jja, kka
      real    rv, rvs, high
      logical set
Cbegin


      if ( NXST.eq.1 .and. NYST.eq.1 .and. .not.XREV .and.
     +     .not.YREV ) then

         if ( NXS.eq.1 .and. NXE.eq.NX .and.
     +        NYS.eq.1 .and. NYE.eq.NY ) then
            call amovr ( in, out, NX*NY )
         else
            call coprr ( in, NX, NY, NXS, NXE, NYS, NYE,
     +                   out, nxo, nyo, 1, 1 )
         endif

      else

         if ( XREV ) then
            k = NXS
            NXS = NXE
            NXE = k
            NXST = -1*NXST
         endif

         if ( YREV ) then
            k = NYS
            NYS = NYE
            NYE = k
            NYST = -1*NYST
         endif

         if ( SAMPLE.eq.'sample ' ) then

            ka = 0
            do kka = 1, nyo
               k = NYS + (kka-1)*NYST
               ka = ka + 1
               ja = 0
               do jja = 1, nxo
                  j = NXS + (jja-1)*NXST
                  ja = ja + 1
                  out(ja,ka) = in(j,k)
               enddo
            enddo

         elseif ( SAMPLE.eq.'highest' ) then

            ka = 0
            do kka = 1, nyo
               k = NYS + (kka-1)*NYST
               ka = ka + 1
               ja = 0
               do jja = 1, nxo
                  j = NXS + (jja-1)*NXST
                  ja = ja + 1

                  out(ja,ka) = RINVAL
                  set = .false.
                  do kk = 1, iabs(NYST)
                     kb = k + (kk-1)*NYST/iabs(NYST)
                     do jj = 1, iabs(NXST)
                        jb = j + (jj-1)*NXST/iabs(NXST)
                        if ( in(jb,kb).ne.RINVAL ) then
                           if ( .not.set ) then
                              high = in(jb,kb)
                              set = .true.
                           else
                              if(in(jb,kb).gt.high)high=in(jb,kb)
                           endif
                        endif
                     enddo
                  enddo
                  if ( set ) out(ja,ka) = high

               enddo
            enddo

         elseif ( SAMPLE.eq.'average' ) then

            ka = 0
            do kka = 1, nyo
               k = NYS + (kka-1)*NYST
               ka = ka + 1
               ja = 0
              do jja = 1, nxo
                 j = NXS + (jja-1)*NXST
                 ja = ja + 1

                  rv = 0.0
                  rvs = 0.0
                  set = .false.
                  do kk = 1, iabs(NYST)
                     kb = k + (kk-1)*NYST/iabs(NYST)
                     do jj = 1, iabs(NXST)
                        jb = j + (jj-1)*NXST/iabs(NXST)
                        if ( in(jb,kb).ne.RINVAL ) then
                           rv = rv + in(jb,kb)
                           rvs = rvs + 1.0
                        else
                           if ( .not.IGNORE ) set = .true.
                        endif
                     enddo
                  enddo

                  if ( rvs.lt.0.5 .or.set ) then
                     out(ja,ka) = RINVAL
                  else
                     out(ja,ka) = rv/rvs
                  endif
               enddo
            enddo

         endif

      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMC_DOITS -- Load the output image from the int*2 input image
C
C   alan penny                     ral           1991 May

      subroutine imc_doits ( in, out, nxo, nyo )

      implicit none
      include 'ST_IMAGE_INC'
      include 'imcut.inc'
C--
      integer*2   in(NX,NY)		!i: Input image
      integer     nxo			!i: Output image X size
      integer     nyo			!i: Output image Y size
      integer*2   out(nxo,nyo)		!o: Output image
C--
      integer j, jj, ja, jb, k, kk, ka, kb, jja, kka
      integer*2 high
      real    rv, rvs
      logical set
Cbegin


      if ( NXST.eq.1 .and. NYST.eq.1 .and. .not.XREV .and.
     +     .not.YREV ) then

         if ( NXS.eq.1 .and. NXE.eq.NX .and.
     +        NYS.eq.1 .and. NYE.eq.NY ) then
            call amovs ( in, out, NX*NY )
         else
            call copss ( in, NX, NY, NXS, NXE, NYS, NYE,
     +                   out, nxo, nyo, 1, 1 )
         endif

      else

         if ( XREV ) then
            k = NXS
            NXS = NXE
            NXE = k
            NXST = -1*NXST
         endif

         if ( YREV ) then
            k = NYS
            NYS = NYE
            NYE = k
            NYST = -1*NYST
         endif

         if ( SAMPLE.eq.'sample ' ) then

            ka = 0
            do kka = 1, nyo
               k = NYS + (kka-1)*NYST
               ka = ka + 1
               ja = 0
               do jja = 1, nxo
                  j = NXS + (jja-1)*NXST
                  ja = ja + 1
                  out(ja,ka) = in(j,k)
               enddo
            enddo


         elseif ( SAMPLE.eq.'highest' ) then

            ka = 0
            do kka = 1, nyo
               k = NYS + (kka-1)*NYST
               ka = ka + 1
               ja = 0
               do jja = 1, nxo
                  j = NXS + (jja-1)*NXST
                  ja = ja + 1

                  out(ja,ka) = INVAL
                  set = .false.
                  do kk = 1, iabs(NYST)
                     kb = k + (kk-1)*NYST/iabs(NYST)
                     do jj = 1, iabs(NXST)
                        jb = j + (jj-1)*NXST/iabs(NXST)
                        if ( in(jb,kb).ne.INVAL ) then
                           if ( .not.set ) then
                              high = in(jb,kb)
                              set = .true.
                           else
                              if(in(jb,kb).gt.high)high=in(jb,kb)
                           endif
                        endif
                     enddo
                  enddo
                  if ( set ) out(ja,ka) = high

               enddo
            enddo

         elseif ( SAMPLE.eq.'average' ) then

            ka = 0
            do kka = 1, nyo
               k = NYS + (kka-1)*NYST
               ka = ka + 1
               ja = 0
              do jja = 1, nxo
                 j = NXS + (jja-1)*NXST
                 ja = ja + 1

                  rv = 0.0
                  rvs = 0.0
                  set = .false.
                  do kk = 1, iabs(NYST)
                     kb = k + (kk-1)*NYST/iabs(NYST)
                     do jj = 1, iabs(NXST)
                        jb = j + (jj-1)*NXST/iabs(NXST)
                        if ( in(jb,kb).ne.INVAL ) then
                           rv = rv + in(jb,kb)
                           rvs = rvs + 1.0
                        else
                           if ( .not.IGNORE ) set = .true.
                        endif
                     enddo
                  enddo

                  if ( rvs.lt.0.5 .or.set ) then
                     out(ja,ka) = INVAL
                  else
                     out(ja,ka) = rv/rvs
                  endif
               enddo
            enddo

         endif

      endif


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  T_IMDES.FOR
C
C    Contains:-
C
C T_IMDES      List, write, delete descriptors of an image
C IMD_LIST     List descriptors
C IMD_SINGLE   Put out wanted descriptors
C IMD_ALL      Put out all descriptors
C IMD_WRITE    Write descriptors to image
C IMD_DELETE   Delete descriptors from an image
C IMD_COPY     Copy descriptors from one image to another
C IMD_GETEM    Get existing descriptors in output image


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMDES -- List, copy, write, delete descriptors of an image
C   For a fuller description see IMDES.HLP
C
C   alan penny                     ral           1991 May

      subroutine t_imdes ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer kout, ierr, ka
      character*1000 topt
      data topt / 'list:write:delete:copy' /
      integer nth
      parameter ( nth=6)
      character*68 th(nth)
      data th /
     + 'Option    Function',
     + '------    --------',
     + 'Copy      Copy descriptors from on image to another',
     + 'Delete    Delete descriptors from image',
     + 'List      List descriptors of image',
     + 'Write     Write descriptors to image' /
Cbegin


      call get_job ( 'OPTION', topt, kout, 1, th, nth )
      if ( ST_FAILED ) return

      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., ierr )		!Obtain input image
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE,ierr)	!Get scale,zero,title


      if ( kout.eq.1 ) then
                       call get_job ('ALL_LIST','all:some',ka,2,' ',0)
                       if ( ST_FAILED ) return
                       if ( ka.eq.1 ) then
                          call imd_alist
                       else
                          call imd_slist
                       endif
                       endif

      if ( kout.eq.2 ) call imd_write

      if ( kout.eq.3 ) call imd_delete

      if ( kout.eq.4 ) call imd_copy


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMD_SLIST -- Put out wanted descriptors
C
C  alan penny                 ral                1990-06-15

      subroutine imd_slist ( )

      implicit none
      include 'imdes.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      logical more, amore, paging, onlyone
      integer k, nval, ierr, kout
      character textb*80, name*20, blank*8
      data blank / '        ' /
Cbegin


      if ( ST_FAILED ) return

      call get1b ( 'ONLYONE', onlyone, .true. )
      if ( ST_FAILED ) return

      call get1b ( 'PAGING', paging, .false. )
      if ( ST_FAILED ) return

      call pargc ( IMTYPE )						!Output main details
      call printd ( ' Image type is: %c ' )
      call pargi ( NX )
      call pargi ( NY )
      call printd ( ' Size:   %d x %d')
      call pargc ( IMTITLE )
      call printd ( ' Title:  %c' )
      call pargr ( BS )
      call pargr ( BZ )
      if ( IMTYPE.eq.'SHORT' ) then
         call pargi ( INVAL )
         call printd ( ' Scale: %f   Zero: %f   Invalid flag:  %d' )
      else
         call pargr ( RINVAL )
         call printd ( ' Scale: %f   Zero: %f   Invalid flag:  %f' )
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
               call printo ( 'ERROR: No such descriptor' )
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
                      if ( paging ) call get1b ( 'MORE', more, .true. )
                      if ( ST_FAILED ) return
                  endif
               enddo
            endif
         endif
         if ( onlyone ) amore = .false.
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMD_ALIST -- Put out all descriptors
C
C  alan penny                 ral                1990-06-15

      subroutine imd_alist ()

      implicit none
      include 'imdes.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      logical more, paging, some
      integer k, ncount, nval, ierr, kout
      character descr*80, text*80
Cbegin


      if ( ST_FAILED ) return

      call get1b ( 'PAGING', paging, .false. )
      if ( ST_FAILED ) return

      call pargc ( IMTYPE )						!Output main details
      call printd ( ' Image type is: %c ' )
      call pargi ( NX )
      call pargi ( NY )
      call printd ( ' Size:   %d x %d')
      call pargc ( IMTITLE )
      call printd ( ' Title:  %c' )
      call pargr ( BS )
      call pargr ( BZ )
      if ( IMTYPE.eq.'SHORT' ) then
         call pargi ( INVAL )
         call printd ( ' Scale: %f   Zero: %f   Invalid flag:  %d' )
      else
         call pargr ( RINVAL )
         call printd ( ' Scale: %f   Zero: %f   Invalid flag:  %f' )
      endif
      call printo ( ' ' )

      some = .false.
      more = .true.							!Put out descriptors
      kout = 0
      ncount = 0
      nval = 1
      do while ( more )
         ncount = ncount + nval
         call gtdesn ( 'IN', ncount, descr, ierr )
         if ( ierr.ne.0 ) then
            more = .false.
         else
            if ( (descr(1:5).ne.'NAXIS') .and.
     +           (descr(1:6).ne.'BSCALE') .and.
     +           (descr(1:5).ne.'BZERO') .and.
     +           (descr(1:5).ne.'INVAL') .and.
     +           (descr(1:5).ne.'TITLE') ) then
               some = .true.
               call gtdesc ( 'IN', descr, TEXTA, ' ', nval, ierr )
               if ( TEXTA(1).ne.' ' .and. ierr.eq.0 ) then
                  k = 0
                  more = .true.
                  do while ( k.lt.nval .and. more )
                     k = k + 1
                     text = descr(1:8)//'  '//TEXTA(k)(1:68)
                     call printo ( text )
                     kout = kout + 1
                     if ( kout.eq.21 ) then
                        kout = 0
                        if ( paging ) call get1b ( 'MORE', more, .true.)
                        if ( ST_FAILED ) return
                      endif
                  enddo
               endif
            endif
         endif
      enddo

      if ( .not.some ) call printo ( 'No descriptors' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMD_WRITE -- Write descriptors to image
C
C  p morris      leeds                   Jun 1992

      subroutine imd_write ()

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer ierr, iv, ka
      real rv
      character desnam*20, val*72
      logical loop, first
Cbegin


      if ( ST_FAILED ) return

      call get1b ( 'LOOP', loop, .true. )				!Loop needed?
      if ( ST_FAILED ) return

      first = .true.
      do while ( loop .or. first )					!Get and write descriptor(s)
         call get1c ( 'NAME', desnam, ' ', .true. )
         if ( ST_FAILED ) return
         if ( desnam.eq.' ' ) then
            loop = .false.
         else
            call get_job ( 'FORMAT', 'char:int:real', ka,1,' ',0)
            if ( ST_FAILED ) return
            call get1c ( 'VALUE', val, val, .true. )
            if ( ST_FAILED ) return
            if ( ka.eq.1 ) then
               call ptdesc ( 'IN', desnam, val )
            elseif ( ka.eq.3 ) then
               call chartor ( val, rv, ierr )
               if ( ierr.eq.0 ) then
                  call ptdesr ( 'IN', desnam, rv )
               else
                  call printo ( 'ERROR: Not a real value' )
               endif
            elseif ( ka.eq.2 ) then
               call chartoi ( val, iv, ierr )
               if ( ierr.eq.0 ) then
                  call ptdesi ( 'IN', desnam, iv )
               else
                  call printo ( 'ERROR: Not an integer value' )
               endif
            else
               call printo ( 'Invalid type' )
               ierr = 1
            endif
            if ( ierr.ne.0 ) then
               ST_FAILED = .true.
               return
            endif
         endif
         first = .false.
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMD_DELETE -- Delete descriptors from an image/table
C
C  p morris             leeds       jun 1992

      subroutine imd_delete ()

      implicit none
      include 'imdes.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer ierr, ncount, nval, ka
      character descr*20
      logical loop, first, more
Cbegin


      if ( ST_FAILED ) return

      call get_job ( 'ALL_DEL', 'all:some', ka, 1, ' ', 0 )
      if ( ST_FAILED ) return

      if ( ka.eq.1 ) then

         more = .true.
         ncount = 0
         nval = 1
         do while ( more )

            ncount = ncount + nval
            call gtdesn ( 'IN', ncount, descr, ierr )
            if ( descr.eq.' ' .or. ierr.ne.0 ) then
               more = .false.
            else
               if ( descr(1:5).ne.'NAXIS' ) then
                  call gtdesc ( 'IN', descr, TEXTA, ' ', nval, ierr )
                  if ( TEXTA(1).ne.' ' .and. ierr.eq.0 ) then
                     call dldes ( 'IN', descr, ierr )
                  else
                     ST_FAILED = .true.
                     more = .false.
                  endif
               endif
            endif

         enddo

      else

         call get1b ( 'LOOP', loop, .true. )				!Loop needed?
         if ( ST_FAILED ) return

         first = .true.
         do while ( loop .or. first )					!Get and write descriptor(s)
            call get1c ( 'NAME', descr, ' ', .true. )
            if ( ST_FAILED ) return
            if ( descr.eq.' ' ) then
               loop = .false.
            else
               call dldes ( 'IN', descr, ierr )
            endif
            if ( ierr.ne.0 ) then
               ST_FAILED = .true.
               loop = .false.
            endif
            first = .false.
         enddo

      endif


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMD_COPY -- Copy descriptors from one image to another
C  Optionally you can choose to overwrite or not descriptors that
C  already occur in the target file.
C
C   alan penny                ral      1990 jan

      subroutine imd_copy ()

      implicit none
      include 'imdes.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      character*60 atext(10000)
      character*8  adescr(10000)
      integer numdes(10000), ierr, j, k, ip2, nx2, ny2, nval,
     +        ntot, ncount
      character*6 imt2
      logical over, dele
Cbegin


      if ( ST_FAILED ) return

      call opimgr ( 'OUT', ip2, nx2, ny2, imt2, .false., ierr )		!Open output
      if ( ierr.ne.0 ) ST_FAILED = .true.

      call get1b ( 'OVER', over, .true. )				!Get wether to overwrite

      if ( over ) call get1b ('DELETE', dele, .true. )			!Get wether to delete before overwriting
      if ( ST_FAILED ) return

      if ( over ) then
         if ( dele ) then
            call icopdes ( 'IN', 'OUT', ierr )
            if ( ierr.ne.0 ) then
               ST_FAILED = .true.
               return
            endif
         else
            call imd_getem ( adescr, numdes, atext, ncount )
            call icopdes ( 'IN', 'OUT', ierr )
            if ( ierr.ne.0 ) then
               ST_FAILED = .true.
               return
            endif
            ntot = 0
            do k = 1, ncount
               call gtdesc ( 'OUT', adescr(k), TEXTA, ' ', nval,
     +                              ierr )
               if ( ierr.ne.0 ) then
                  do j = 1, numdes(k)
                     TEXTA(j) = atext(ntot+j)
                  enddo
                  call ptdescn ( 'OUT', adescr(k), TEXTA, numdes(k) )
               endif
               ntot = ntot + numdes(k)
            enddo
         endif
      else
         call imd_getem ( adescr, numdes, atext, ncount )
         call icopdes ( 'IN', 'OUT' ,ierr )
         if ( ierr.ne.0 ) then
            ST_FAILED = .true.
            return
         endif
         ntot = 0
         do k = 1, ncount
            do j = 1, numdes(k)
               TEXTA(j) = atext(ntot+j)
            enddo
            ntot = ntot + numdes(k)
            call ptdescn ( 'OUT', adescr(k), TEXTA, numdes(k) )
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMD_GETEM -- Get existing descriptors in output image
C
C   alan penny                ral         1990 jan

      subroutine imd_getem ( adescr, numdes, atext, ncount )

      implicit none
      include 'imdes.inc'
      include 'STARMAN_INC'

      character*8  adescr(10000)	!o: Names of descriptors
      integer      numdes(10000)	!o: Number of values in descriptors
      character*60 atext(10000)		!o: Contents of descriptors
      integer      ncount		!o: Number of descriptors
C--
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
            call gtdesc ( 'OUT', descr, TEXTA, ' ', nval, ierr )
            numtot = numtot + nval
            if ( numtot.gt.10000 ) then
               more = .false.
              call printo('WARNING: Too much - only 10000 lines copied')
            else
               adescr(ncount) = descr(1:8)
               numdes(ncount) = nval
               do k = 1, nval
                 btext = TEXTA(k)
                 atext(ncount+k-1) = btext(1:60)
               enddo
            endif
         endif
      enddo
      ncount = ncount - 1


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMFITS_DR.F   Convert a simple FITS disk image into an NDF image
C
C Contains:-
C
C T_IMFITS_DR           Convert Fits disk file into an NDF File
C IMFI_GETFITS          Read a disk FITS file header
C IMFI_FITSHEAD         Read information from FITS header.
C IMFI_PUTH             Store header in output array
C IMFI_PUT(IRS)(RS)     Move image data values from input file to output
C IMFI_NUMGET(IRS)(RS)  Gets data from FITS block into array
C IMFI_BSWOP            Swop bytes in character array
C IMFI_CHECK            Check FITS Header start is OK
C
C Alan Penny                       RAL               1994 Feb

      subroutine t_imfits_dr ( )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      character*57600 header			!Fits header
      integer nbl				!Number of Fits blocks in header
      character*6     intype			!Input type ('REAL','INT', 'SHORT',)
      character*6     outtype			!Output type ('REAL','INT', 'SHORT',)
      integer ierr, ipout
Cbegin


      if ( ST_FAILED ) return

      call imfi_gcl ( header, nbl, intype, outtype, ipout )		!Get command line input

      call imfi_puth ( header, intype, outtype )			!Get INVAL and put out headers

      if     ( intype.eq.'SHORT' .and. outtype.eq.'REAL'  ) then	!Read in image
         call imfi_putsr ( %val(ipout),  nbl, ierr )
      elseif ( intype.eq.'INT'   .and. outtype.eq.'REAL'  ) then
         call imfi_putir ( %val(ipout), nbl, ierr )
      elseif ( intype.eq.'REAL'  .and. outtype.eq.'REAL'  ) then
         call imfi_putrr ( %val(ipout), nbl, ierr )
      elseif ( intype.eq.'SHORT' .and. outtype.eq.'SHORT' ) then
         call imfi_putss ( %val(ipout), nbl, ierr )
      elseif ( intype.eq.'INT'   .and. outtype.eq.'SHORT' ) then
         call imfi_putis ( %val(ipout), nbl, ierr )
      else
         call imfi_putrs ( %val(ipout), nbl, ierr )
      endif
      if ( ierr.ne.0 ) ST_FAILED = .true.

      close ( 1 )
      call canpar ( 'OUT' )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_GCL -- Get command line information
C
C   alan penny                    ral            1994 Nov

      subroutine imfi_gcl ( header, nbl, intype, outtype, ipout )

      implicit none

      include 'imfits_dr.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      character*57600 header	!o: Fits header
      integer         nbl	!o: Number of Fits blocks in header
      character*6     intype	!o: Input type ('REAL','INT', 'SHORT',)
      character*6     outtype	!o: Output type ('REAL','INT', 'SHORT',)
      integer         ipout
C--
      character infile*30, htext*8, line*240
      integer ierr, ka, kopt

      integer nth
      parameter ( nth=5 )
      character*68 th(nth)
      data th /
     + 'Are the output data ? :- ',
     + 'Option   Choice',
     + '------   --------',
     + 'Real     Real 32-bit numbers',
     + 'Short    Signed 16-bit integer (0 to 65636)'/
Cbegin


      if ( ST_FAILED ) return

      call get1c ( 'IN', infile, ' ', .true. ) 				!Get file
      if ( ST_FAILED ) return
      if ( infile(1:1).eq.' ' ) return

      call get1c ( 'OBJECT', htext, M_N_OBJECT, .true. )
      call uppcase ( htext, N_OBJECT )
      call get1b ( 'DO_INFO', DO_INFO, .false. )
      if ( DO_INFO ) then
         call get1c ( 'FILTER', htext, M_N_FILTER, .true. )
         call uppcase ( htext, N_FILTER )
         call get1c ( 'RA', htext, M_N_RA, .true. )
         call uppcase ( htext, N_RA )
         call get1c ( 'DEC', htext, M_N_DEC, .true. )
         call uppcase ( htext, N_DEC )
         call get1c ( 'EXPOSURE', htext, M_N_EXP, .true. )
         call uppcase ( htext, N_EXP )
         call get1c ( 'TIME', htext, M_N_TIME, .true. )
         call uppcase ( htext, N_TIME )
         call get1c ( 'DATE', htext, M_N_DATE, .true. )
         call uppcase ( htext, N_DATE )
      else
         N_FILTER = M_N_FILTER
         N_RA     = M_N_RA
         N_DEC    = M_N_DEC
         N_EXP    = M_N_EXP
         N_TIME   = M_N_TIME
         N_DATE   = M_N_DATE
      endif

      call imfi_getfits ( infile, header, nbl, intype, line, ierr )	!Read in header
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( NX.lt.1 .or. NY.lt.1 ) then
         call printo ( 'ERROR: Both dimensions must be at least 1' )
         ST_FAILED = .true.
      endif
      if ( ST_FAILED ) return

      ka = 2
      if ( intype.eq.'INT' .or. intype.eq.'REAL' ) ka = 1
      call get_job ('OUTTYPE', 'real:short', kopt, ka, th, nth )	!Get type of output $
      outtype = 'REAL'
      if ( kopt.eq.1 ) outtype = 'REAL'
      if ( kopt.eq.2 ) outtype = 'SHORT'

      call opimgw ( 'OUT', outtype, ipout, NX, NY, .false., ierr )
      if ( ierr.ne.0 ) ST_FAILED = .true.

      call printo ( ' ' )
      if ( DO_INFO ) then
         call printo ( line(1:80) )
         call printo ( line(81:160) )
      endif
      call printo ( line(161:240) )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_GETFITS -- Reads a disk FITS file and returns header information.
C
C   alan penny                    ral            1994 Nov

      subroutine imfi_getfits ( infile, header, nbl, intype, line,
     +                          ierr )

      implicit none

      include 'imfits_dr.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      character*(*) infile	!i: File name
      character*(*) header	!i: Header contents
      integer       nbl		!o: No of blocks in header
      character*6   intype	!o: 32-bit real/32-bit int/16-bit int/?
				!    ('REAL', 'INT', 'SHORT',)
      character*240 line	!o: Information lines
      integer       ierr	!o: Error flag (0=ok; 1=bad)
C--
      character block*57600, tblock*2880, aline*80
      integer k, kl, ja, nc, irec, ios, idot
      logical endfound
Cbegin


      if ( ST_FAILED ) return

      irec = 2880/4
      open ( unit=1, file=infile, access='direct', status='old',
     +       recl=irec, form='unformatted', iostat=ios)

      ierr = 0
      if ( ios.ne.0 ) then
         ierr = 1
         call printo ( 'ERROR: Error in opening file' )
         return
      endif

      nbl = 0								!Read header block(s)
      endfound = .false.
      do while ( .not.endfound .and. nbl.lt.20 )

         nbl = nbl + 1
         read ( 1, rec=nbl, err=998 ) tblock(1:2880)

         goto 995
  998       call printo ( ' ' )
            call pargi ( nbl )
            call printd (
     +  ' WARNING: Error encountered in header block number: %d' )
            call printo (
     +      '          Setting contents of that block to null' )
            call printo ( ' ' )
            tblock = ' '
  995    continue

         ja = 1 + (nbl-1)*2880
         block(ja:ja+2880-1) = tblock(1:2880)
         do k = 1, 36
            ja = 1 + (k-1)*80
            if ( tblock(ja:ja+2).eq.'END' ) endfound = .true.
         enddo

      enddo

      if ( .not.endfound .and. nbl.eq.20 ) then
         ierr = 1
         call printo ( 'ERROR: Can only deal with 20 header blocks' )
         close ( 1 )
         return
      elseif ( .not.endfound ) then
         ierr = 1
         call printo ( 'ERROR: END line not found in header blocks' )
         close ( 1 )
         return
      endif

      call imfi_fitshead ( block, nbl, aline, intype, ierr )		!Decode header
      if ( ierr.eq.1 ) then
         close ( 1 )
         return
      endif

      kl = 10
      idot = index(infile(1:kl),'.')
      if ( idot.ne.0 ) kl = idot - 1

      if ( DO_INFO ) then
         line(1:80) = ' File     Object      Filt     RA      '//
     +       ' Dec       Date      Time    Exposure'
         line(81:160) = aline(1:80)
         line(81:80+kl) = infile(1:kl)
      endif

      write ( line(161:240),
     +   '('' Dimension: '',i6,'' x'',i6,''  : Number of blocks'',
     +    '' in header '',i2)' ) NX, NY, nbl

      nc = nbl*2880
      header(1:nc) = block(1:nc)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_PUTH -- Store header in NDF
C
C         A J Penny                RAL                     1994 Feb

      subroutine imfi_puth ( header, intype, outtype )

      implicit none

      include 'imfits_dr.inc'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      include 'ST_IMAGE_INC'

      character*57600  header		!i: Contents of headers (20 x 28880 byte blocks)
      character*6      intype		!i: 'real' 'int' or 'short' for input image type
      character*6      outtype		!i: 'real' or 'short' for output image type
C--
      integer   ka, kb, ja, jb, kk, kf, kl, k, ierr, i_inval
      character ahead*80, bhead*80, name*8
      real      rv
      logical   more, g_bscale, g_bzero, g_inval, g_title, ison, found
Cbegin


      if ( ST_FAILED ) return

      g_title = .false.
      more = .true.
      k = 5
      do while ( more )
         k = k + 1

         ka = 1 + (k-1)*80
         kb = ka + 79
         ahead = header(ka:kb)
         name = ahead(1:8)

         ison = .true.
         found = .false.
         kf = 80
         do kk = 10, 80
            if ( ahead(kk:kk).eq.char(39) ) ison = .not.ison
            if ( .not.found .and. ison ) then
               if ( ahead(kk:kk).eq.'/' ) then
                  found = .true.
                  kf = kk
               endif
            endif
         enddo
         bhead = ahead(10:kf-1)

         call endstrip ( bhead, ja, jb )

         if ( name.eq.'END') more = .false.
         if ( name.eq.'TITLE' ) g_title = .true.

      enddo

      g_bscale = .false.
      g_bzero  = .false.
      g_inval  = .false.

      more = .true.
      k = 5
      do while ( more )
         k = k + 1

         ka = 1 + (k-1)*80
         kb = ka + 79
         ahead = header(ka:kb)
         name = ahead(1:8)

         ison = .true.
         found = .false.
         kf = 80
         do kk = 10, 80
            if ( ahead(kk:kk).eq.char(39) ) ison = .not.ison
            if ( .not.found .and. ison ) then
               if ( ahead(kk:kk).eq.'/' ) then
                  found = .true.
                  kf = kk
               endif
            endif
         enddo
         bhead = ahead (10:kf-1)
         call endstrip ( bhead, ja, jb )

         if ( name.ne.'END') call ptdesc ( 'OUT', name, bhead(ja:jb) )

         if ( name.eq.N_OBJECT ) then
            if ( .not.g_title ) then
               do kk = ja, jb
                  if ( bhead(kk:kk).eq.char(39) ) bhead(kk:kk) = ' '
               enddo
               call lbgone ( bhead )
               call charln ( bhead, kl )
               call ptdesc ( 'OUT', 'TITLE', bhead(1:kl) )
            endif
         elseif ( name.eq.'BSCALE' ) then
            g_bscale = .true.
            call chartor ( bhead(ja:jb), BS, ierr )
            if ( ierr.ne.0 ) call printo (
     +         '  WARNING: BSCALE descriptor not a proper number' )
         elseif ( name.eq.'BZERO' )  then
            g_bzero  = .true.
            call chartor ( bhead(ja:jb), BZ, ierr )
            if ( ierr.ne.0 ) call printo (
     +         '  WARNING: BZERO descriptor not a proper number' )
         elseif ( name.eq.'INVAL' )  then
            g_inval  = .true.
            call chartor ( bhead(ja:jb), rv, ierr )
            if ( ierr.ne.0 ) call printo (
     +         '  WARNING: INVAL descriptor not a proper number' )
            if ( outtype.eq.'REAL' ) then
               RINVAL = rv
            else
               i_inval = nint(rv)
               INVAL = max(-32768,min(32767,nint(rv)))
            endif
         endif

         if ( name.eq.'END') more = .false.

      enddo

      if ( .not.g_inval ) then
         if ( outtype.eq.'REAL' ) then
            RINVAL = INT_INVALR
         else
            INVAL = INT_INVALSI
         endif
      endif
      if ( .not.g_bzero ) BZ = 0.0
      if ( .not.g_bscale ) BS = 1.0

      call pargc ( intype )
      call pargr ( BS )
      call pargr ( BZ )
      if ( g_inval ) then
         if ( intype.eq.'REAL' ) then
            call pargr ( RINVAL )
            call printd (
     +' Type %c : Bscale %f : Bzero %f : Inval flag %f' )
         else
            call pargi ( i_inval )
            call printd (
     +'   Type  %c  :   Bscale  %f  :  Bzero  %f  :  Inval flag  %d' )
         endif
      else
         call printd ( ' Type %c : Bscale %f : Bzero %f ' )
      endif

      call ptdesr ( 'OUT', 'BZERO',  BZ )
      call ptdesr ( 'OUT', 'BSCALE', BS )
      if ( outtype.eq.'REAL' ) then
         call ptdesr ( 'OUT', 'INVAL', RINVAL )
      else
         call ptdesi ( 'OUT', 'INVAL', INVAL )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_PUTSS -- Move image data values from input file to output
C
C   alan penny                    ral            1994 Nov


      subroutine imfi_putss ( im, nbl, ierr )

      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      include 'ST_IMAGE_INC'

      integer*2    im(NX,NY)	!o: Output image
      integer      nbl		!i: No of FITS blocks in header
      integer      ierr		!o: Error flag (0=ok;1=bad)
C--
      character block*2880
      integer kx, ky, kok, jx, jy, mb, mx, nb, np
      logical bswop
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      call isbswop ( bswop )

      kx = 1
      ky = 1
      mx = 2*NX*NY
      nb = nbl

      do while ( mx.gt.0 )

         nb = nb + 1
         mb = min(2880,mx)
         read ( 1, rec=nb, err=998 ) block(1:mb)

         kok = 0
         goto 995
  998       call printo ( ' ' )
            call pargi ( nb )
            call printo ( '  WARNING: Error encountered in image body')
            call printd ( '           block (2880 bytes) number = %d' )
            call pargi ( kx )
            call pargi ( ky )
            call printd ( '           Data start at X = %d ; Y = %d ' )
            kok = 1
            block = ' '
  995    continue

         mx = mx - 2880

         if ( bswop ) call imfi_bswop ( block, mb, 2 )
         np = mb/2
         call imfi_numgetss ( block, np, im, kx, ky, kok )

         if ( kok.eq.1 ) then
            jx = kx - 1
            jy = ky
            if ( jx.eq.0 ) then
               jx = NX
               jy = jy - 1
            endif
            call pargi ( jx )
            call pargi ( jy )
            call printd ( '           Data   end at X = %d ; Y = %d ' )
            call printo (
     +    '           Setting disk contents of that block to INVALID' )
            call printo ( ' ' )
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_PUTSR -- Move image data values from input file to output
C
C   alan penny                    ral            1994 Nov


      subroutine imfi_putsr ( rim, nbl, ierr )

      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      include 'ST_IMAGE_INC'

      real         rim(NX,NY)	!o: Output image
      integer      nbl		!i: No of FITS blocks in header
      integer      ierr		!o: Error flag (0=ok;1=bad)
C--
      character block*2880
      integer kx, ky, kok, jx, jy, mb, mx, nb, np
      logical bswop
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      call isbswop ( bswop )

      kx = 1
      ky = 1
      mx = 2*NX*NY
      nb = nbl

      do while ( mx.gt.0 )

         nb = nb + 1
         mb = min(2880,mx)
         read ( 1, rec=nb, err=998 ) block(1:mb)
         mx = mx - 2880

         kok = 0
         goto 995
  998       call printo ( ' ' )
            call pargi ( nb )
            call printo ( 'ERROR: Error encountered in image body' )
            call printd ( '       block (2880 bytes) number = %d' )
            call pargi ( kx )
            call pargi ( ky )
            call printd ( '       Data start at X = %d ; Y = %d ' )
            call printo (
     +     '       Setting disk contents of that block to INVALID' )
            call printo ( ' ' )
            kok = 1
            block = ' '
  995    continue

         if ( bswop ) call imfi_bswop ( block, mb, 2 )
         np = mb/2
         call imfi_numgetsr ( block, np, rim, kx, ky, kok )

         if ( kok.eq.1 ) then
            jx = kx - 1
            jy = ky
            if ( jx.eq.0 ) then
               jx = NX
               jy = jy - 1
            endif
            call pargi ( jx )
            call pargi ( jy )
            call printd ( '       Data   end at X = %d ; Y = %d ' )
            call printo (
     +      '       Setting disk contents of that block to INVALID' )
            call printo ( ' ' )
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_PUTIR -- Move image data values from input file to output
C
C   alan penny                    ral            1994 Nov


      subroutine imfi_putir ( rim, nbl, ierr )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real         rim(NX,NY)	!o: Output image
      integer      nbl		!i: No of FITS blocks in header
      integer      ierr		!o: Error flag (0=ok;1=bad)
C--
      character block*2880
      integer kx, ky, kok, jx, jy, mb, mx, nb, np
      logical bswop
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      call isbswop ( bswop )

      kx = 1
      ky = 1
      mb = 2880
      mx = 4*NX*NY
      nb = nbl

      do while ( mx.gt.0 )

         nb = nb + 1
         mb = min(2880,mx)
         read ( 1, rec=nb, err=998 ) block(1:mb)
         mx = mx - 2880

         kok = 0
         goto 995
  998       call printo ( ' ' )
            call pargi ( nb )
            call printo ( 'ERROR: Error encountered in image body' )
            call printd ( '       block (2880 bytes) number = %d' )
            call pargi ( kx )
            call pargi ( ky )
            call printd ( '       Data start at X = %d ; Y = %d ' )
            call printo (
     +     '       Setting disk contents of that block to INVALID' )
            call printo ( ' ' )
            kok = 1
            block = ' '
  995    continue

         if ( bswop ) call imfi_bswop ( block, mb, 4 )
         np = mb/4
         call imfi_numgetir ( block, np, rim, kx, ky, kok )

         if ( kok.eq.1 ) then
            jx = kx - 1
            jy = ky
            if ( jx.eq.0 ) then
               jx = NX
               jy = jy - 1
            endif
            call pargi ( jx )
            call pargi ( jy )
            call printd ( '       Data   end at X = %d ; Y = %d ' )
            call printo (
     +      '       Setting disk contents of that block to INVALID' )
            call printo ( ' ' )
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_PUTIS -- Move image data values from input file to output
C
C   alan penny                    ral            1994 Nov


      subroutine imfi_putis ( im, nbl, ierr )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      integer*2    im(NX,NY)	!o: Output image
      integer      nbl		!i: No of FITS blocks in header
      integer      ierr		!o: Error flag (0=ok;1=bad)
C--
      character block*2880
      integer kx, ky, kok, jx, jy, mb, mx, nb, np
      logical bswop
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      call isbswop ( bswop )

      kx = 1
      ky = 1
      mb = 2880
      mx = 4*NX*NY
      nb = nbl

      do while ( mx.gt.0 )

         nb = nb + 1
         mb = min(2880,mx)
         read ( 1, rec=nb, err=998 ) block(1:mb)
         mx = mx - 2880

         kok = 0
         goto 995
  998       call printo ( ' ' )
            call pargi ( nb )
            call printo ( 'ERROR: Error encountered in image body' )
            call printd ( '       block (2880 bytes) number = %d' )
            call pargi ( kx )
            call pargi ( ky )
            call printd ( '       Data start at X = %d ; Y = %d ' )
            call printo (
     +     '       Setting disk contents of that block to INVALID' )
            call printo ( ' ' )
            kok = 1
            block = ' '
  995    continue

         if ( bswop ) call imfi_bswop ( block, mb, 4 )
         np = mb/4
         call imfi_numgetis ( block, np, im, kx, ky, kok )

         if ( kok.eq.1 ) then
            jx = kx - 1
            jy = ky
            if ( jx.eq.0 ) then
               jx = NX
               jy = jy - 1
            endif
            call pargi ( jx )
            call pargi ( jy )
            call printd ( '       Data   end at X = %d ; Y = %d ' )
            call printo (
     +      '       Setting disk contents of that block to INVALID' )
            call printo ( ' ' )
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_PUTRR -- Move image data values from input file to output
C
C   alan penny                    ral            1994 Nov


      subroutine imfi_putrr ( rim, nbl, ierr )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real         rim(NX,NY)	!o: Output image
      integer      nbl		!i: No of FITS blocks in header
      integer      ierr		!o: Error flag (0=ok;1=bad)
C--
      character block*2880
      integer kx, ky, kok, jx, jy, mb, mx, nb, np
      logical bswop
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      call isbswop ( bswop )

      kx = 1
      ky = 1
      mb = 2880
      mx = 4*NX*NY
      nb = nbl

      do while ( mx.gt.0 )

         nb = nb + 1
         mb = min(2880,mx)
         read ( 1, rec=nb, err=998 ) block(1:mb)
         mx = mx - 2880

         kok = 0
         goto 995
  998       call printo ( ' ' )
            call pargi ( nb )
            call printo ( 'ERROR: Error encountered in image body' )
            call printd ( '       block (2880 bytes) number = %d' )
            call pargi ( kx )
            call pargi ( ky )
            call printd ( '       Data start at X = %d ; Y = %d ' )
            call printo (
     +     '       Setting disk contents of that block to INVALID' )
            call printo ( ' ' )
            kok = 1
            block = ' '
  995    continue

         if ( bswop ) call imfi_bswop ( block, mb, 4 )
         np = mb/4
         call imfi_numgetrr ( block, np, rim, kx, ky, kok )

         if ( kok.eq.1 ) then
            jx = kx - 1
            jy = ky
            if ( jx.eq.0 ) then
               jx = NX
               jy = jy - 1
            endif
            call pargi ( jx )
            call pargi ( jy )
            call printd ( '       Data   end at X = %d ; Y = %d ' )
            call printo (
     +      '       Setting disk contents of that block to INVALID' )
            call printo ( ' ' )
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_PUTRS -- Move image data values from input file to output
C
C   alan penny                    ral            1994 Nov


      subroutine imfi_putrs ( im, nbl, ierr )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      integer*2    im(NX,NY)	!o: Output image
      integer      nbl		!i: No of FITS blocks in header
      integer      ierr		!o: Error flag (0=ok;1=bad)
C--
      character block*2880
      integer kx, ky, kok, jx, jy, mb, mx, nb, np
      logical bswop
Cbegin


      if ( ST_FAILED ) return

      ierr = 0

      call isbswop ( bswop )

      kx = 1
      ky = 1
      mb = 2880
      mx = 4*NX*NY
      nb = nbl

      do while ( mx.gt.0 )

         nb = nb + 1
         mb = min(2880,mx)
         read ( 1, rec=nb, err=998 ) block(1:mb)
         mx = mx - 2880

         kok = 0
         goto 995
  998       call printo ( ' ' )
            call pargi ( nb )
            call printo ( 'ERROR: Error encountered in image body' )
            call printd ( '       block (2880 bytes) number = %d' )
            call pargi ( kx )
            call pargi ( ky )
            call printd ( '       Data start at X = %d ; Y = %d ' )
            call printo (
     +      '       Setting disk contents of that block to INVALID' )
            call printo ( ' ' )
            kok = 1
            block = ' '
  995    continue

         if ( bswop ) call imfi_bswop ( block, mb, 4 )
         np = mb/4
         call imfi_numgetrs ( block, np, im, kx, ky, kok )

         if ( kok.eq.1 ) then
            jx = kx - 1
            jy = ky
            if ( jx.eq.0 ) then
               jx = NX
               jy = jy - 1
            endif
            call pargi ( jx )
            call pargi ( jy )
            call printd ( '       Data   end at X = %d ; Y = %d ' )
            call printo (
     +      '       Setting disk contents of that block to INVALID' )
            call printo ( ' ' )
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_FITSHEAD -- Reads information from FITS header.
C
C   alan penny                    ral            1994 Nov

      subroutine imfi_fitshead ( block, nbl, aline, intype, ierr )

      implicit none

      include 'imfits_dr.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      character*57600  block	!i: Header
      integer          nbl	!i: No of blocks in header
      character*80     aline	!o: Summry of headers
      character*6      intype	!o: 32-bit real/32-bit int/16-bit int/?
				!    ('REAL', 'INT', 'SHORT',)
      integer          ierr	!o: Error flag (0=ok; 1=bad)
C--
      character qot*1, head*80, name*8
      logical more, endfound, ison, found
      integer i, ka, kb, kl, kk, kf
Cbegin


      if ( ST_FAILED ) return

      NX = 1
      NY = 1
      intype = 'REAL'
      ierr = 0

      qot = char(39)							!The ' character

      aline = ' '

      endfound = .false.
      more = .true.
      kb = 0
      do while ( kb.lt.nbl*36 .and. more )
         kb = kb + 1
         i = (kb-1)*80

         name = block(i+1:i+8)

         head = ' '

         ison = .true.
         found = .false.
         kf = 80
         do kk = 10, 80
            if ( block(i+kk:i+kk).eq.char(39) ) ison = .not.ison
            if ( .not.found .and. ison ) then
               if ( block(i+kk:i+kk).eq.'/' ) then
                  found = .true.
                  kf = kk
               endif
            endif
         enddo
         head = block(i+10:i+kf-1)

         call lbgone ( head )
         if ( head(1:1).eq.qot ) head(1:1) = ' '
         call lbgone ( head )
         ka = index(head,qot)
         if ( ka.ne.0 ) head(ka:ka) = ' '
         call lbgone ( head )
         call charln ( head, kl )

         call imfi_check ( kb, name, head, ierr )
         if ( ierr.ne.0 ) then
            ST_FAILED = .true.
            return
         endif

         if ( name.eq.'BITPIX' ) then
            read ( head(1:kl),'(i)' ) ka
            if ( ka.eq.32 ) then
               intype = 'INT'
            elseif ( ka.eq.16 ) then
               intype = 'SHORT'
            elseif ( ka.eq.-32 ) then
               intype = 'REAL'
            endif
         elseif ( name.eq.'NAXIS1' ) then
            read ( head(1:kl),'(i)' ) NX
	 elseif ( name.eq.'NAXIS2' ) then
            read ( head(1:kl),'(i)' ) NY
 	 elseif ( name.eq.N_OBJECT ) then
            ka = index(head(1:15),char(47))
            if ( ka.ge.1 ) then
               ka = ka - 1
               if ( ka.gt.10 ) ka = 10
            else
               ka = 10
            endif
            aline(11:22) = head(1:ka)
	 elseif ( name.eq.N_FILTER ) then				!Remove the / character
            aline(23:26) = head(1:4)
         elseif ( name.eq.N_RA )   then
            aline(28:36) = head(1:8)
	 elseif ( name.eq.N_DEC )  then
            aline(38:47) = head(1:9)
	 elseif ( name.eq.N_DATE ) then
            aline(49:57) = head(1:8)
	 elseif ( name.eq.N_TIME ) then
            aline(59:67) = head(1:8)
	 elseif ( name.eq.N_EXP ) then
            aline(69:80) = head(1:11)
         endif

	 if ( name.eq.'END' ) then
            endfound = .true.
            more = .false.
         endif

      enddo

      do i = 1, 80							!Remove ' characters
          if ( aline(i:i).eq.qot ) aline(i:i) = ' '
      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_CHECK -- Check FITS Header start is OK
C
C   alan penny                    ral            1994 Nov

      subroutine imfi_check ( kb, name, head, ierr )

      implicit none
      include 'STARMAN_INC'

      integer     kb		!i: File line number
      character   name*8	!i: Line name
      character   head*80	!i: Line value
      integer     ierr		!i/o: Error flag (1=bad)
C--
      integer ka, kl, nka
Cbegin


      if ( ST_FAILED ) return

      call charln ( head, kl )

      if ( kb.eq.1 ) then
         if ( name.ne.'SIMPLE' ) then
            call printo ( 'ERROR: First header must be SIMPLE' )
            ierr = 1
         else
            call charln ( head, kl )
            if ( kl.ne.1 .or. head(1:1).ne.'T' ) then
               call printo ( 'ERROR: Image not FITS SIMPLE' )
               ierr = 1
            endif
         endif
      endif

      if ( kb.eq.2 ) then
         if ( name.ne.'BITPIX' ) then
            call printo ( 'ERROR: Second header must be BITPIX')
            ierr = 1
         else
            call charln ( head, kl )
            read ( head(1:kl),'(i)' ) ka
            if ( ka.ne.32 .and. ka.ne.16 .and. ka.ne.-32 ) then
               call printo ( 'ERROR: Image must be 16 or 32 bit '//
     +                        'integer or 32 bit real' )
               ierr = 1
            endif
         endif
      endif

      if ( kb.eq.3 ) then
         if ( name.ne.'NAXIS' ) then
            call printo ( 'ERROR: Third header must be NAXIS')
            ierr = 1
         else
            nka = 0
            call charln ( head, kl )
            read ( head(1:kl),'(i)' ) ka
            if ( ka.eq.1 ) then
               call printo ('WARNING: Image is 1-D: treated as 2-D')
               call printo ( '         2nd axis = 1' )
               nka = 1
            elseif ( ka.gt.2 ) then
               call pargi ( ka )
             call printd ('WARNING: Image is %d -D; treated as 2-D')
               call printo ( '         Only first plane read' )
            endif
         endif
      endif

      if ( kb.eq.4 ) then
         if ( name.ne.'NAXIS1' ) then
            call printo ( 'ERROR: Fourth header must be NAXIS1')
            ierr = 1
         endif
      endif

      if ( kb.eq.5 .and. nka.ne.1 ) then
         if ( name.ne.'NAXIS2' ) then
            call printo ( 'ERROR: Fifth header must be NAXIS2')
            ierr = 1
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_NUMGETIR -- Gets real data from 32-bit integer FITS block
C

      subroutine imfi_numgetir ( block, np, rim, kx, ky, kok )

      implicit none
      include 'ST_IMAGE_INC'

      character*(*) block	!i: Character FITS block array
      integer       np		!i: No of pixels to read
      real          rim(NX,NY)	!i/o: Data array
      integer       kx		!i/o: Present X position
      integer       ky		!i/o: Present Y position
      integer       kok         !i: Block ok flag (0=ok;1=bad)
C--
      integer k, i, n1, n2, n3, n4, num
Cbegin


      do k = 1, np

         if ( kok.eq.1 ) then
            rim(kx,ky) = RINVAL
         else
            i = 1 + (k-1)*4
            n4 = ichar(block(i:i))
            n3 = ichar(block(i+1:i+1))
            n2 = ichar(block(i+2:i+2))
            n1 = ichar(block(i+3:i+3))
            if ( n1.gt.127 ) then
               n1 = 255 - n1
               n2 = 255 - n2
               n3 = 255 - n3
               n4 = 255 - n4
               num = 256*256*256*n1 +256*256*n2 + 256*n3 + n4 + 1
               num = -num
            else
               num = 256*256*256*n1 +256*256*n2 + 256*n3 + n4
            endif
            rim(kx,ky) = num
         endif

         kx = kx + 1
         if ( kx.gt.NX ) then
            kx = 1
            ky = ky + 1
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_NUMGETIS -- Gets int*2 data from 32-bit integer FITS block
C

      subroutine imfi_numgetis ( block, np, im, kx, ky, kok )

      implicit none
      include 'ST_IMAGE_INC'

      character*(*) block	!i: Character FITS block array
      integer       np		!i: No of pixels to read
      integer*2     im(NX,NY)	!i/o: Data array
      integer       kx		!i/o: Present X position
      integer       ky		!i/o: Present Y position
      integer       kok         !i: Block ok flag (0=ok;1=bad)
C--
      integer k, i, n1, n2, n3, n4, num, kv
Cbegin


      do k = 1, np

         if ( kok.eq.1 ) then
            im(kx,ky) = INVAL
         else
            i = 1 + (k-1)*4
            n4 = ichar(block(i:i))
            n3 = ichar(block(i+1:i+1))
            n2 = ichar(block(i+2:i+2))
            n1 = ichar(block(i+3:i+3))
            if ( n1.gt.127 ) then
               n1 = 255 - n1
               n2 = 255 - n2
               n3 = 255 - n3
               n4 = 255 - n4
               num = 256*256*256*n1 +256*256*n2 + 256*n3 + n4 + 1
               num = -num
            else
               num = 256*256*256*n1 +256*256*n2 + 256*n3 + n4
            endif
            kv = num
            im(kx,ky) = min(32767,max(kv,-32768))
         endif

         kx = kx + 1
         if ( kx.gt.NX ) then
            kx = 1
            ky = ky + 1
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_NUMGETRR -- Get real data from 32-bit real FITS block
C

      subroutine imfi_numgetrr ( block, np, rim, kx, ky, kok )

      implicit none
      include 'ST_IMAGE_INC'

      real      block(720)	!i: Character FITS block array (read as real*4)
      integer   np		!i: No of pixels to read
      real      rim(NX,NY)	!i/o: Data array
      integer   kx		!i/o: Present X position
      integer   ky		!i/o: Present Y position
      integer       kok         !i: Block ok flag (0=ok;1=bad)
C--
      integer k
Cbegin


      do k = 1, np

         if ( kok.eq.1 ) then
            rim(kx,ky) = RINVAL
         else
            rim(kx,ky) = block(k)
         endif

         kx = kx + 1
         if ( kx.gt.NX ) then
            kx = 1
            ky = ky + 1
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_NUMGETRS -- Get int*2 data from 32-bit real FITS block
C

      subroutine imfi_numgetrs ( block, np, im, kx, ky, kok )

      implicit none
      include 'ST_IMAGE_INC'

      real       block(720)	!i: Character FITS block array (read as real*4)
      integer    np		!i: No of pixels to read
      integer*2  im(NX,NY)	!i/o: Data array
      integer    kx		!i/o: Present X position
      integer    ky		!i/o: Present Y position
      integer    kok            !i: Block ok flag (0=ok;1=bad)
C--
      integer k
      real rv
Cbegin


      do k = 1, np

         if ( kok.eq.1 ) then
            im(kx,ky) = INVAL
         else
            rv = block(k)
            im(kx,ky) = min(32767.0,max(-32768.0,rv))
         endif

         kx = kx + 1
         if ( kx.gt.NX ) then
            kx = 1
            ky = ky + 1
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_NUMGETSS -- Gets short data from 16-bit integer FITS block
C

      subroutine imfi_numgetss ( block, np, im, kx, ky, kok )

      implicit none
      include 'ST_IMAGE_INC'

      character*(*) block	!i: Character FITS block array
      integer       np		!i: No of pixels to read
      integer*2     im(NX,NY)	!i/o: Data array
      integer       kx		!i/o: Present X position
      integer       ky		!i/o: Present Y position
      integer       kok         !i: Block ok flag (0=ok;1=bad)
C--
      integer k, i, n1, n2, num
Cbegin


      do k = 1, np

         if ( kok.eq.1 ) then
            im(kx,ky) = INVAL
         else
            i = 1 + (k-1)*2
            n2 = ichar(block(i:i))
            n1 = ichar(block(i+1:i+1))
            if ( n1.gt.127 ) then
               n1 = 255 - n1
               n2 = 255 - n2
               num = 256*n1 + n2 + 1
               num = -num
            else
               num = 256*n1 + n2
            endif
            im(kx,ky) = num
         endif

         kx = kx + 1
         if ( kx.gt.NX ) then
            kx = 1
            ky = ky + 1
         endif

      enddo


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_NUMGETSR -- Gets real data from 16-bit integer FITS block
C

      subroutine imfi_numgetsr ( block, np, rim, kx, ky, kok )

      implicit none
      include 'ST_IMAGE_INC'

      character*(*) block	!i: Character FITS block array
      integer       np		!i: No of pixels to read
      real          rim(NX,NY)	!i/o: Data array
      integer       kx		!i/o: Present X position
      integer       ky		!i/o: Present Y position
      integer       kok         !i: Block ok flag (0=ok;1=bad)
C--
      integer k, i, n1, n2, num
Cbegin


      do k = 1, np

         if ( kok.eq.1 ) then
            rim(kx,ky) = RINVAL
         else
            i = 1 + (k-1)*2
            n2 = ichar(block(i:i))
            n1 = ichar(block(i+1:i+1))
            if ( n1.gt.127 ) then
               n1 = 255 - n1
               n2 = 255 - n2
               num = 256*n1 + n2+ 1
               num = -num
            else
               num = 256*n1 + n2
            endif
            rim(kx,ky) = num
         endif

         kx = kx + 1
         if ( kx.gt.NX ) then
            kx = 1
            ky = ky + 1
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMFI_BSWOP -- Swop bytes in character array
C
C  alan penny                     ral               1994 Dec

      subroutine imfi_bswop ( block, mb, ns )

      implicit none
      include 'STARMAN_INC'

      character*(*) block	!i/o: Character block array
      integer       mb		!i: No of characters to cover
      integer       ns		!i: No of bytes to swop
C--
      integer k
      character*1  cv1, cv2, cv3, cv4
Cbegin


      if ( ST_FAILED ) return

      if ( ns.ne.2 .and. ns.ne.4 ) then
         call printo ( ' ERROR: Programmer error in s/r imfi_bswop' )
         call printo ( '        Code needs rewriting: Contact '//
     +                 ' person who wrote the program' )
         return
      endif

      if ( ns.eq.2 ) then
         do k = 1, mb-ns+1, ns
            cv1 = block(k:k)
            cv2 = block(k+1:k+1)
            block(k:k)     = cv2
            block(k+1:k+1) = cv1
         enddo
      elseif ( ns.eq.4 ) then
         do k = 1, mb-ns+1, ns
            cv1 = block(k:k)
            cv2 = block(k+1:k+1)
            cv3 = block(k+2:k+2)
            cv4 = block(k+3:k+3)
            block(k:k)     = cv4
            block(k+1:k+1) = cv3
            block(k+2:k+2) = cv2
            block(k+3:k+3) = cv1
         enddo
      endif


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   IMFLASH.FOR
C
C   Contains:-
C
C T_IMFLASH   Flash up an image on the display

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMFLASH -- Flash up an image on the display
C
C   alan penny                ral          1990 Jan

      subroutine t_imflash ()

      implicit none

      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
      include 'ST_DS_GEN_INC'
C--
      real amin, amax
      integer ierr
      logical enough
Cbegin


      call ds_sdef ( 1, 1 )						!Set Display defaults

      BS     = 1.0							!Image value scales
      BZ     = 0.0
      INVAL  = INT_INVALSI
      RINVAL  = INT_INVALR

      call ds_gtype ( ierr )						!Get type of display
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         call printo ( 'ERROR: Forbidden answer' )
         return
      endif

      call opimgr ( 'IN', IPIM, NX, NY, IMTYPE, .false., ierr )		!Get image
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      if ( IMTYPE.ne.'REAL' .and. IMTYPE.ne.'SHORT' ) then
         call printo ( 'ERROR: Image must be REAL or INTEGER*2' )
         call pargc ( IMTYPE )
         call printo ( 'ERROR: Image type is: %c ' )
         ST_FAILED = .true.
         return
      endif

      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE, ierr)

      DSNXS = 1								!Image size
      DSNXE = NX
      DSNYS = 1
      DSNYE = NY
      call ds_init ( IMTITLE, 1, ierr )					!Open display
      if ( ierr.ne.0 ) then
         call printo ( 'ERROR: Can not open that display device' )
         ST_FAILED = .true.
         return
      endif

      call ds_gtcomf ( 0 )						!Get image display size

      call ds_imgscl ( %val(IPIM), NX, NY, IMTYPE, 1, NX, 1, NY )	!Get display scale

      call printo ( ' Title is: '//IMTITLE )				!Write title

      amin = BS*DSVMIN + BZ						!Write display range
      amax = BS*DSVMAX + BZ
      call pargr ( amin )
      call pargr ( amax )
      call printd ( ' Display values: Min = %f  Max = %f' )
      call pargi ( NX )							!Write size
      call pargi ( NY )
      call pargc ( IMTYPE )
      call printd ( ' Size = %dx%d  :  Type = %c' )

      call ds_disp ( %val(IPIM), NX, NY, IMTYPE, 0 )			!Display

      enough = .false.
      do while ( .not.enough )
         call get1b ( 'ENOUGH', enough, .true. )
      enddo

      call ds_close ( ierr )

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMJOIN.F
C
C   Contains:-
C
C T_IMJOIN      Place images into an new or existing image
C IMJ_TYPE1     Place new images into (maybe) existing image
C IMJ_SETUP     Set up program
C IMJ_GCLA      Get user info - (A)
C IMJ_GCLB      Get user info and input images - (B)
C IMJ_OPINOUT   Open input/output
C IMJ_GETSIZE   Get suggested size of new output image
C IMJ_DOIT      Add images in
C IMJ_OUT       Output images
C IMJ_LOAD(RS)O Output output image
C IMJ_WORK1(RS) Add image in, in nearest mode
C IMJ_WORK2(RS) Add image in, in linear mode
C IMJ_WORK3(RS) Add image in, in uniform mode
C IMJ_TYPE2     Add two reference images
C IMJ_XXDOIT    Do work of adding two reference images



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMJOIN -- Place images into an new or existing image
C
C alan penny                    ral            1991 March

      subroutine t_imjoin ( )

      implicit none
      include 'imjoin.inc'
C---
      integer nthelp, k
      parameter ( nthelp=4 )
      character*68 thelp(nthelp)
      data (thelp(k),k=1,nthelp) /
     + 'Option     Action' ,
     + '------     ------' ,
     + 'New        Load new images - maybe with input reference image',
     + 'Reference  Add two reference images together' /
Cbegin


      call get_job ( 'OUTMODE', 'new:reference', k, 1, thelp, nthelp )	!Get type of loading

      if ( k.eq.1 ) call imj_type1					!Load input images

      if ( k.eq.2 ) call imj_type2					!Load 2 reference images


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_TYPE1 -- Place new images into (maybe) existing image
C
C alan penny                    ral            1991 March

      subroutine imj_type1 ( )

      implicit none
C---
Cbegin


      call imj_setup							!Set up defaults

      call imj_gcla							!Get user info

      call imj_gclb							!Get user info

      call imj_opinout							!Open input, output images

      call imj_doit							!Add input images in

      call imj_out							!Put out images


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_SETUP -- Set up program
C
C alan penny                    ral            1991 March

      subroutine imj_setup ( )

      implicit none
      include 'imjoin.inc'
C---
Cbegin


      IPWR = 1
      IPWL = 1


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_GCLA -- Get user info - (A)
C
C alan penny                    ral            1991 March

      subroutine imj_gcla ( )

      implicit none
      include 'imjoin.inc'
      include 'STARMAN_INC'
C---
      integer k
      integer nthelp
      parameter ( nthelp=13 )
      character*68 thelp(nthelp)
      data (thelp(k),k=1,10) /
     + 'Option    Action' ,
     + '------    ------' ,
     + 'Nearest   Take the nearest input image pixel' ,
     + 'Linear    Make a linear combination of the four input image' ,
     + '           pixels around the output image pixel' ,
     + 'Uniform   Make a weighted mean of the nine adjacent input' ,
     + '           image pixels (linearly combined), so that the ouput',
     + '           image pixels have a constant noise value. (i.e., in',
     + '           (Linear), an output pixel in the centre of four ' ,
     + '           input pixels will be the mean of those, and thus ' /
      data (thelp(k),k=11,nthelp) /
     + '           have a lower noise than an output pixel on an input',
     + '           pixel, in which case it would only have the mean ' ,
     + '           of that single pixel.' /
      integer nthelp1
      parameter ( nthelp1=4 )
      character*68 thelp1(nthelp1)
      data (thelp1(k),k=1,nthelp1) /
     + 'Option    Action' ,
     + '------    ------' ,
     + 'Full      Do a full shift with shift, rotation, stretch',
     + 'Shift     Do a shift with just an XY shift'/
Cbegin


      if ( ST_FAILED ) return

      call get1b ( 'ZERO', DOZERO, .true. ) 				!Get if subtract median

      call get_job ( 'METHOD', 'nearest:linear:uniform', KMETH, 2,	!Get transform coeffs
     +                         thelp, nthelp )

      call get_job ( 'SHIFT', 'full:xyshift', KSHIFT, 2, thelp1, 	!Get full transform or just XY shift
     +               nthelp1 )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_GCLB -- Get user info and input images - (B)
C
C alan penny                    ral            1991 March

      subroutine imj_gclb ( )

      implicit none
      include 'imjoin.inc'
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C---
      integer istat, k, kk, px(4), py(4), pmx, pmy
      real rv
      logical more
      character texta*3, textb*4, textc*7, textd*8, texte*6, textf*7
      real atrc(6), btrc(6)
      data atrc / 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 /
Cbegin


      if ( ST_FAILED ) return

      k = 0
      more = .true.
      do while ( more )

         k = k + 1

         if ( k.lt.10 ) then						!Input image
            write ( texta, '(''IN'',i1)' ) k
            call opimzr ( texta, IPI(k), NXI(k), NYI(k), IMTY(k),
     +                    .true., istat )
         else
            write ( textb, '(''IN'',i2)' ) k
            call opimzr ( textb, IPI(k), NXI(k), NYI(k), IMTY(k),
     +                    .true., istat )
         endif

         if ( istat.eq.1 .or. istat.eq.3 ) then				!Check input image
            ST_FAILED = .true.
            call pargi ( k )
            call printd ( 'ERROR: Error is in input image %d' )
            return
         elseif ( k.eq.1 .and. istat.eq.2 ) then
            ST_FAILED = .true.
            call printo ( 'ERROR: Must have at least one input image')
            return
         elseif ( istat.eq.2 ) then
            NTOT = k - 1
            return
         endif

         call pargi ( k )						!Give size
         call pargi ( NXI(k) )
         call pargi ( NYI(k) )
         call printd ( '  Input image %d is size: %d x %d ' )

         call amovr ( atrc, btrc, 6 )					!Get transform coords
         if ( KSHIFT.eq.1 ) then
            if ( k.lt.10 ) then
               write ( textc, '(''XCOEFF'',i1)' ) k
               call get3r ( textc, btrc(1), btrc(2), btrc(3), .true.,
     +                      -1.0e8, 1.0e8 )
               write ( textc, '(''YCOEFF'',i1)' ) k
               call get3r ( textc, btrc(4), btrc(5), btrc(6), .true.,
     +                      -1.0e8, 1.0e8 )
            else
               write ( textd, '(''XCOEFF'',i2)' ) k
               call get3r ( textd, btrc(1), btrc(2), btrc(3), .true.,
     +                      -1.0e8, 1.0e8 )
               write ( textd, '(''YCOEFF'',i2)' ) k
               call get3r ( textd, btrc(4), btrc(5), btrc(6), .true.,
     +                      -1.0e8, 1.0e8 )
            endif
         else
            if ( k.lt.10 ) then
               write ( textc, '(''XYSHFT'',i1)' ) k
               call get2r ( textc, btrc(1), btrc(4), .true., -1.0e8,
     +                      1.0e8 )
            else
               write ( textd, '(''XYSHFT'',i2)' ) k
               call get2r ( textd, btrc(1), btrc(4), .true., -1.0e8,
     +                      1.0e8 )
            endif
         endif

         rv = btrc(5)*btrc(3) - btrc(2)*btrc(6) 			!Check them
         if ( abs(rv).lt.1.0e-20 ) then
            call pargi ( k )
            call printd ( 'ERROR: Transform Eqn for image %d is bad' )
            ST_FAILED = .true.
            return
         endif

         px(1) = btrc(1) + 1.0*btrc(2) + 1.0*btrc(3)			!Check if image goes to -ve posns
         px(2) = btrc(1) + NXI(k)*btrc(2) + 1.0*btrc(3)
         px(3) = btrc(1) + 1.0*btrc(2) + NYI(k)*btrc(3)
         px(4) = btrc(1) + NYI(k)*btrc(2) + NYI(k)*btrc(3)
         py(1) = btrc(4) + 1.0*btrc(5) + 1.0*btrc(6)
         py(2) = btrc(4) + NXI(k)*btrc(5) + 1.0*btrc(6)
         py(3) = btrc(4) + 1.0*btrc(5) + NYI(k)*btrc(6)
         py(4) = btrc(4) + NXI(k)*btrc(5) + NYI(k)*btrc(6)
         call pargi ( k )
         do kk = 1, 4
            call pargi ( px(kk) )
            call pargi ( py(kk) )
         enddo
         call printd ( '  Image %d put into: %d,%d ; %d,%d ; '//
     +                  '%d,%d ; %d,%d ' )
         pmx = min(px(1),px(2),px(3),px(4))
         pmy = min(py(1),py(2),py(3),py(4))
         if ( pmx.lt.1 .or. pmy.lt.1 ) call printo ( ' ' )
         if ( pmx.lt.1 ) then
            call pargi ( k )
            call pargi ( pmx )
            call printd ( 'WARNING: Image %d has an X corner '//
     +                     'at %d - Output image starts at 1,1' )
         endif
         if ( pmy.lt.1 ) then
            call pargi ( k )
            call pargi ( pmy )
            call printd ( 'WARNING: Image %d has a Y corner '//
     +                     'at %d - Output image starts at 1,1' )
         endif
         if ( pmx.lt.1 .or. pmy.lt.1 ) call printo ( ' ' )

         call amovr ( btrc(1), ARC(1,k), 6 )				!Store then

         TRC(1,k) = (btrc(1)*btrc(6)-btrc(3)*btrc(4))/rv		!Invert them
         TRC(2,k) = -1.0*btrc(6)/rv
         TRC(3,k) = btrc(3)/rv
         TRC(4,k) = (btrc(2)*btrc(4)-btrc(5)*btrc(1))/rv
         TRC(5,k) = btrc(5)/rv
         TRC(6,k) = -1.0*btrc(2)/rv

         if ( k.lt.10 ) then						!Get 'scale'
            write ( texte, '(''SCALE'',i1)' ) k
            call get1r ( texte, SCALE(k), 1.0, 0.0, 1.0e8 )
         else
            write ( textf, '(''SCALE'',i2)' ) k
            call get1r ( textf, SCALE(k), 1.0, 0.0, 1.0e8 )
         endif

         if ( k.eq.MAXN ) then
            more = .false.
            call pargi ( MAXN )
            call printd ( 'Maximum number of images - %d - reached' )
         endif

      enddo
      NTOT = k


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_OPINOUT -- Open input/output
C
C alan penny                    ral            1991 March

      subroutine imj_opinout ( )

      implicit none
      include 'imjoin.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
C---
      real rva, rvb, rvc, bsa, bza, rv
      integer k, ka, istat, ivb, iv
      logical first
      character cv*1, texta*3, textb*4
      character*70 titler, titlel, otitler, otitlel, title
Cbegin


      if ( ST_FAILED ) return

      call opimrr ( 'INREF', IPR, NXR, NYR, .true., istat )		!Reference image
      if ( istat.eq.1 ) then
         ST_FAILED = .true.
         call printo ( 'ERROR: Error is in input Reference image' )
         return
      elseif ( istat.eq.2 ) then
         call imj_getsize
         call get2i ( 'SIZE', NXR, NYR, .true., 1, 100000 )
         NOREF = .true.
      else
         NOREF = .false.
         call gtimrd ( 'INREF', rva, rvb, INVALREF, titler, istat )
      endif

      if ( .not.NOREF ) then						!Reference load image
         call opimrr ( 'INLOAD', IPL, iv, ivb, .false., istat )
         if ( istat.ne.0 ) then
            ST_FAILED = .true.
            call printo ( 'ERROR: Error is in input Load image' )
            return
         endif
         if ( iv.ne.NXR .or. ivb.ne.NYR ) then
            call printo (
     +         'ERROR: Load image different size to reference image' )
            ST_FAILED = .true.
            return
         endif
         call gtimrd ( 'INLOAD', rva, rvb, rvc, titlel, istat )
      endif

      call opimrw ( 'OUTREF', IPOR, NXR, NYR, .true., istat )		!Open output reference image
      DOUTREF = .true.
      if ( istat.ne.0 ) DOUTREF = .false.

      if ( .not.DOUTREF ) IPOR = 1
      IPOL = 1
      if ( DOUTREF ) then
         if ( NOREF ) titler = ' ' 					!Add title
         call get1c (  'TITLER', otitler, titler, .true. )
         call opimrw ( 'OUTLOAD', IPOL, NXR, NYR, .true., istat )	!Open output load image
         if ( NOREF ) titlel = ' '					!Add title
         call get1c (  'TITLEL', otitlel, titlel, .true. )
         if ( NOREF ) then						!Load descriptors
            call ptdesr ( 'OUTREF',   'INVAL',   INT_INVALR )
            call ptdesr ( 'OUTLOAD',  'INVAL',   INT_INVALR )
         else
            call icopdes ( 'INREF',  'OUTREF', istat )
            call icopdes ( 'INLOAD', 'OUTLOAD', istat )
         endif
         call ptdesc ( 'OUTREF',  'TITLE', otitler )
         call ptdesc ( 'OUTLOAD', 'TITLE', otitlel )
      endif

      OUTTYPE = 'SHORT'							!Output image tye
      first = .true.							! and scale,zero
      do k = 1, NTOT
         if ( k.lt.10 ) then
            write ( texta, '(''IN'',i1)' ) k
            call gtimzd ( texta, IMTY(k), bsa, bza, iv, rv, cv, istat)
         else
            write ( textb, '(''IN'',i2)' ) k
            call gtimzd ( textb, IMTY(k), bsa, bza, iv, rv, cv, istat)
         endif
         if ( k.eq.1 ) IMTITLE = cv
         if ( k.eq.1 .and. IMTY(k).eq.'SHORT' ) then
            BSO = bsa
            BZO = bza
            INVALO = iv
         endif
         if ( IMTY(k).eq.'REAL' ) then
            OUTTYPE = 'REAL'
            if ( first ) then
               first = .false.
               RINVALO = rv
            endif
         endif
      enddo

      if ( OUTTYPE.eq.'REAL' ) then
         BSO = 1.0
         BZO = 0.0
      endif

      if ( OUTTYPE.eq.'SHORT' ) then
         call printo ( 'Input images were SHORT' )
         call get_job ( 'OUTTYPE', 'real:short', ka, 1, ' ', 0 )
         if ( ka.eq.1 ) then
            OUTTYPE = 'REAL'
            BSO = 1.0
            BZO = 0.0
            RINVALO = INT_INVALR
         endif
      endif

      if ( DOUTIM .and. OUTTYPE.eq.'SHORT' ) then
         call get2r ( 'OUTSCALE', BSO, BZO, .true., -1.0e20, 1.0e20 )
      endif

      call opimzw ( 'OUT', OUTTYPE, IPO, NXR, NYR, .true., istat )	!Open output image
      DOUTIM = .true.
      if ( istat.ne.0 ) DOUTIM = .false.

      if ( .not.DOUTIM ) IPO = 1
      if ( DOUTIM ) then
         call get1c (  'TITLE', title, IMTITLE, .true. )
         call icopdes ( 'IN1',  'OUT', istat )
         call ptdesc ( 'OUT',  'TITLE', title )
         call ptdesr ( 'OUT', 'BZERO', BZO )
         call ptdesr ( 'OUT', 'BSCALE', BSO )
         if ( OUTTYPE.eq.'SHORT' ) then
            call ptdesi ( 'OUT', 'INVAL', INVALO )
         else
            call ptdesr ( 'OUT', 'INVAL', RINVALO )
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_GETSIZE -- Get suggested size of new output image
C
C alan penny                    ral            1991 March

      subroutine imj_getsize ( )

      implicit none
      include 'imjoin.inc'
C--
      integer k, kx, ky
      real xa, xb, xc, xd, x, ya, yb, yc, yd, y
Cbegin


      NXR = 100
      NYR = 100
      if ( NTOT.ne.0 ) then
         NXR = 1
         NYR = 1
         do k = 1, NTOT

            xa = 1.0*ARC(2,k) + 1.0*ARC(3,k)
            xb = 1.0*ARC(2,k) + real(NYI(k))*ARC(3,k)
            xc = real(NXI(k))*ARC(2,k) + 1.0*ARC(3,k)
            xd = real(NXI(k))*ARC(2,k) + real(NYI(k))*ARC(3,k)
            x = max(xa,xb,xc,xd)
            kx = x + ARC(1,k)
            NXR = max(NXR,kx)

            ya = 1.0*ARC(5,k) + 1.0*ARC(6,k)
            yb = 1.0*ARC(5,k) + real(NYI(k))*ARC(6,k)
            yc = real(NXI(k))*ARC(5,k) + 1.0*ARC(6,k)
            yd = real(NXI(k))*ARC(5,k) + real(NYI(k))*ARC(6,k)
            y = max(ya,yb,yc,yd)
            ky = y + ARC(4,k)
            NYR = max(NYR,ky)

         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_DOIT -- Add images in
C  1) resample the input images over the entire area covered by the
C     output image
C  2) Scan the selected area of the output image (location x,y) and
C     calculate the transformed position (xdash,ydash) in the input
C     images
C
C alan penny                    ral            1991 March

      subroutine imj_doit ( )

      implicit none
      include 'imjoin.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
C--
      integer istat, k
      real std, v1, v2, v3, v4
      character texta*3, textb*4, cv*1
Cbegin


      if ( ST_FAILED ) return						!Failed or no more images

      call gtwrkr ( 'REF', NXR*NYR, IPWR, istat )			!Start reference/load arrays
      if ( istat.ne.0 ) then						!Open work space
         call printo ( 'ERROR: Cant open work space REF' )
         ST_FAILED = .true.
         return
      endif
      call gtwrkr ( 'LOAD', NXR*NYR, IPWL, istat )
      if ( istat.ne.0 ) then
         call printo ( 'ERROR: Cant open work space LOAD' )
         ST_FAILED = .true.
         return
      endif

      if ( NOREF ) then
         call amovkr ( INT_INVALR, %val(IPWR), NXR*NYR )		!Zero them
         call azeror ( %val(IPWL), NXR*NYR )
      else
         call amovr ( %val(IPR), %val(IPWR), NXR*NYR )			!Load input reference
         call achvalr ( %val(IPWR), INVALREF, INT_INVALR, NXR*NYR )
         call amovr ( %val(IPL), %val(IPWL), NXR*NYR )
         if ( .not.NOREF ) call canpar  ( 'INREF' )
         call canpar  ( 'INLOAD' )
      endif

      do k = 1, NTOT
         if ( k.lt.10 ) then
            write ( texta, '(''IN'',i1)' ) k
            call gtimzd ( texta, IMTY(k), BS, BZ, INVALIS, INVALIR,
     +                    cv, istat )
         else
            write ( textb, '(''IN'',i2)' ) k
            call gtimzd ( textb, IMTY(k), BS, BZ, INVALIS, INVALIR,
     +                    cv, istat )
         endif

         call printo ( ' ' )						!Tell user doing it
         call pargi ( k )
         call printd ( ' Doing image %d' )

         ZERO = 0.0							!Get image zero
         if ( DOZERO ) then
            call sky_0 ( %val(IPI(k)), IMTY(k), NXI(k), NYI(k),
     +                   INVALIS, INVALIR, ZERO, std )
            ZERO = BS*ZERO + BZ
            call pargr ( ZERO )
            call printd ( ' Sky level subtracted = %f' )
         else
            call printo ( ' No sky subtraction' )
         endif

         v1 = ARC(1,k) + ARC(2,k)*1.0          + ARC(3,k)*1.0		!Image limits
         v2 = ARC(1,k) + ARC(2,k)*1.0          + ARC(3,k)*real(NYI(k))
         v3 = ARC(1,k) + ARC(2,k)*real(NXI(k)) + ARC(3,k)*1.0
         v4 = ARC(1,k) + ARC(2,k)*real(NXI(k)) + ARC(3,k)*real(NYI(k))
         KELIM(1,1,k) = min(NXR,max(1,int(min(v1,v2,v3,v4)-3.0)))
         KELIM(2,1,k) = max(1,min(NXR,int(max(v1,v2,v3,v4)+3.0)))
         v1 = ARC(4,k) + ARC(5,k)*1.0          + ARC(6,k)*1.0
         v2 = ARC(4,k) + ARC(5,k)*1.0          + ARC(6,k)*real(NYI(k))
         v3 = ARC(4,k) + ARC(5,k)*real(NXI(k)) + ARC(6,k)*1.0
         v4 = ARC(4,k) + ARC(5,k)*real(NXI(k)) + ARC(6,k)*real(NYI(k))
         KELIM(1,2,k) = min(NYR,max(1,int(min(v1,v2,v3,v4)-3.0)))
         KELIM(2,2,k) = max(1,min(NYR,int(max(v1,v2,v3,v4)+3.0)))

         if ( KMETH.eq.1 ) then						!Add image in
            if ( IMTY(k).eq.'SHORT' ) then
               call imj_work1s ( %val(IPI(k)), NXI(k), NYI(k),
     +                           %val(IPWR), %val(IPWL), k )
            else
               call imj_work1r ( %val(IPI(k)), NXI(k), NYI(k),
     +                           %val(IPWR), %val(IPWL), k )
            endif
         elseif ( KMETH.eq.2 ) then
            if ( IMTY(k).eq.'SHORT' ) then
               call imj_work2s ( %val(IPI(k)), NXI(k), NYI(k),
     +                           %val(IPWR), %val(IPWL), k )
            else
               call imj_work2r ( %val(IPI(k)), NXI(k), NYI(k),
     +                           %val(IPWR), %val(IPWL), k )
            endif
         elseif ( KMETH.eq.3 ) then
            if ( IMTY(k).eq.'SHORT' ) then
               call imj_work3s ( %val(IPI(k)), NXI(k), NYI(k),
     +                           %val(IPWR), %val(IPWL), k )
            else
               call imj_work3r ( %val(IPI(k)), NXI(k), NYI(k),
     +                           %val(IPWR), %val(IPWL), k )
            endif
         endif

         if ( k.lt.10 ) then						!Free data access to image
            call canpar ( texta )
         else
            call canpar ( textb )
         endif

         call pargi ( k )
         call printd ( ' Done image %d' )

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_OUT -- Output images
C
C alan penny                    ral            1991 March

      subroutine imj_out ( )

      implicit none
      include 'imjoin.inc'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
C--
Cbegin


      if ( ST_FAILED ) then
         call printo ( 'WARNING: Output images may be corrupted' )
         return
      endif

      if ( DOUTREF ) then
         call printo ( ' ' )
         call printd ( ' Loading output reference and load images' )
         call amovr ( %val(IPWR), %val(IPOR), NXR*NYR )
         call amovr ( %val(IPWL), %val(IPOL), NXR*NYR )
         call printd ( ' Loaded output reference and load images' )
      endif

      if ( DOUTIM ) then
         call printo ( ' ' )
         call printd ( ' Loading output image' )
         if ( OUTTYPE.eq.'SHORT' ) then
            call imj_loadso ( %val(IPWR), %val(IPWL), %val(IPO),
     +                       NXR*NYR, INT_INVALR, BSO, BZO, INVALO )
         else
            call imj_loadro ( %val(IPWR), %val(IPWL), %val(IPO),
     +                  NXR*NYR, INT_INVALR, BSO, BZO, INT_INVALR )
         endif
         call printd ( ' Loaded output image' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_LOADRO -- Output output real image
C
C alan penny                    ral            1991 March

      subroutine imj_loadro ( ref, rlo, out, n, invali, bs, bz, rinvalo)

      implicit none

      integer    n		!i: Size of image
      real       ref(n)		!i: Work image
      real       rlo(n)		!i: Load image
      real       out(n)		!o: Output image
      integer    invali         !i: Flag value of invalid pixel
      real       bs		!i: Scale of output
      real       bz		!i: Zero of output
      real       rinvalo	!i: Magic level of output
C--
      integer k
Cbegin


      do k = 1, n
         if ( rlo(k).eq.0.0 .or. ref(k).eq.invali ) then
            out(k) = rinvalo
         else
            out(k) = (ref(k)-bz)/bs
         endif
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_LOADSO -- Output output int*2 image
C
C alan penny                    ral            1991 March

      subroutine imj_loadso ( ref, rlo, out, n, invali, bs, bz, invalo )

      implicit none

      integer    n		!i: Size of image
      real       ref(n)		!i: Work image
      real       rlo(n)		!i: Load image
      integer*2  out(n)		!o: Output image
      integer    invali         !i: Flag value of invalid pixel
      real       bs		!i: Scale of output
      real       bz		!i: Zero of output
      integer    invalo         !i: Magic level of output
C--
      integer k
      real    rv
Cbegin


      do k = 1, n
         if ( rlo(k).eq.0.0 .or. ref(k).eq.invali ) then
            out(k) = invalo
         else
            rv = (ref(k)-bz)/bs
            if ( rv.gt.32767.0 .or. rv.lt.-32768.0 ) then
               out(k) = invalo
            else
               out(k) = rv
            endif
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_WORK1R -- Add image in, in nearest mode
C
C alan penny                    ral            1991 March

      subroutine imj_work1r ( im, nxc, nyc, wref, wload, kk )

      implicit none
      include 'imjoin.inc'
      include 'ST_LIMITS_INC'
      include 'ST_IMAGE_INC'

      integer      nxc			!i: Input image X size
      integer      nyc			!i: Input image Y size
      real         im(nxc,nyc)		!i: Input image
      real         wref(NXR,NYR)	!i/o: Working refernce image
      real         wload(NXR,NYR)	!i/o: Working load image
      integer      kk			!i: Input image number
C--
      integer ix, iy, jx, jy
      real valr, ajx, ajy
      real anumh, anuml
      parameter ( anumh=32767.0*32767.0 )
      parameter ( anuml=-1.0*32767.0*32767.0 )
      real refi, rloadi, refim, rloadim, ovalr, ovall
Cbegin


      do iy = KELIM(1,2,kk), KELIM(2,2,kk)
         do ix = KELIM(1,1,kk), KELIM(2,1,kk)

            ajx = TRC(1,kk) + TRC(2,kk)*ix + TRC(3,kk)*iy		!Find nearest pixel location
            ajy = TRC(4,kk) + TRC(5,kk)*ix + TRC(6,kk)*iy
            jx = nint(min(anumh,max(anuml,ajx)))
            jy = nint(min(anumh,max(anuml,ajy)))

            if( jx.ge.1.and.jx.le.nxc.and.jy.ge.1.and.jy.le.nyc ) then	!Inside image?
               valr = INT_INVALR					!Input invalid?
               if ( im(jx,jy).ne.INVALIR ) valr = BS*im(jx,jy) +BZ-ZERO

               if ( valr.eq.INT_INVALR .and.				!Check for both in, out bad
     +              ( wref(ix,iy).eq.INT_INVALR .or.
     +                wload(ix,iy).eq.0.0 ) ) then

                  wref(ix,iy) = INT_INVALR				!If so load bad
                  wload(ix,iy) = wload(ix,iy) + SCALE(kk)

               else							!Good value

                  refi = 0.0						!New pixel
                  rloadi = 0.0
                  if ( valr.ne.INT_INVALR ) then
                     refi = valr
                     rloadi = SCALE(kk)
                  endif

                  refim = 0.0						!Old pixel
                  rloadim = 0.0
                  if ( wref(ix,iy).ne.INT_INVALR ) then
                     refim = wref(ix,iy)
                     rloadim = wload(ix,iy)
                  endif

                  ovalr = refi*rloadi + refim*rloadim			!Add
                  ovall = rloadi + rloadim
                  if ( ovall.ne.0.0 ) then
                     wref(ix,iy) = ovalr/ovall
                  else
                     wref(ix,iy) = 0.0
                  endif
                  wload(ix,iy) = ovall

               endif

            endif

         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_WORK1S -- Add int*2 image in, in nearest mode
C
C alan penny                    ral            1991 March

      subroutine imj_work1s ( im, nxc, nyc, wref, wload, kk )

      implicit none
      include 'imjoin.inc'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'

      integer      nxc			!i: Input image X size
      integer      nyc			!i: Input image Y size
      integer*2    im(nxc,nyc)		!i: Input image
      real         wref(NXR,NYR)	!i/o: Working refernce image
      real         wload(NXR,NYR)	!i/o: Working load image
      integer      kk			!i: Input image number
C--
      integer ix, iy, jx, jy
      real valr, ajx, ajy
      real anumh, anuml
      parameter ( anumh=32767.0*32767.0 )
      parameter ( anuml=-1.0*32767.0*32767.0 )
      real refi, rloadi, refim, rloadim, ovalr, ovall
Cbegin


      do iy = KELIM(1,2,kk), KELIM(2,2,kk)
         do ix = KELIM(1,1,kk), KELIM(2,1,kk)

            ajx = TRC(1,kk) + TRC(2,kk)*ix + TRC(3,kk)*iy		!Find nearest pixel location
            ajy = TRC(4,kk) + TRC(5,kk)*ix + TRC(6,kk)*iy
            jx = nint(min(anumh,max(anuml,ajx)))
            jy = nint(min(anumh,max(anuml,ajy)))

            if( jx.ge.1.and.jx.le.nxc.and.jy.ge.1.and.jy.le.nyc ) then	!Inside image?
               valr = INT_INVALR					!Input invalid?
               if ( im(jx,jy).ne.INVALIS ) valr = BS*im(jx,jy) +BZ-ZERO

               if ( valr.eq.INT_INVALR .and.				!Check for both in, out bad
     +              ( wref(ix,iy).eq.INT_INVALR .or.
     +                wload(ix,iy).eq.0.0 ) ) then

                  wref(ix,iy) = INT_INVALR				!If so load bad
                  wload(ix,iy) = wload(ix,iy) + SCALE(kk)

               else							!Good value

                  refi = 0.0						!New pixel
                  rloadi = 0.0
                  if ( valr.ne.INT_INVALR ) then
                     refi = valr
                     rloadi = SCALE(kk)
                  endif

                  refim = 0.0						!Old pixel
                  rloadim = 0.0
                  if ( wref(ix,iy).ne.INT_INVALR ) then
                     refim = wref(ix,iy)
                     rloadim = wload(ix,iy)
                  endif

                  ovalr = refi*rloadi + refim*rloadim			!Add
                  ovall = rloadi + rloadim
                  if ( ovall.ne.0.0 ) then
                     wref(ix,iy) = ovalr/ovall
                  else
                     wref(ix,iy) = 0.0
                  endif
                  wload(ix,iy) = ovall

               endif

            endif

         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_WORK2R -- Add real image in, in linear mode
C
C alan penny                    ral            1991 March

      subroutine imj_work2r ( im, nxc, nyc, wref, wload, kk )

      implicit none
      include 'imjoin.inc'
      include 'ST_LIMITS_INC'
      include 'ST_IMAGE_INC'
C--
      integer      nxc			!i: Input image X size
      integer      nyc			!i: Input image Y size
      real         im(nxc,nyc)		!i: Input image
      real         wref(NXR,NYR)	!i/o: Working refernce image
      real         wload(NXR,NYR)	!i/o: Working load image
      integer      kk			!i: Input image number
Cbegin
      integer ix, iy, jx, jy, i, j
      real xdash, ydash, xref, yref, sum, wt, wtsum, dx, dy, valr
      real anumh, anuml
      parameter ( anumh=32767.0*32767.0 )
      parameter ( anuml=-1.0*32767.0*32767.0 )
      real refi, rloadi, refim, rloadim, ovalr, ovall
Cbegin


      do iy = KELIM(1,2,kk), KELIM(2,2,kk)
         do ix = KELIM(1,1,kk), KELIM(2,1,kk)

            xref = TRC(1,kk) + TRC(3,kk)*iy				!Set up transform
            yref = TRC(4,kk) + TRC(6,kk)*iy
            xdash = xref + TRC(2,kk)*ix
            ydash = yref + TRC(5,kk)*ix

            jx = nint(min(anumh,max(anuml,xdash)))			!Find nearest pixel location
            jy = nint(min(anumh,max(anuml,ydash)))

            if( jx.ge.1.and.jx.le.nxc.and.jy.ge.1.and.jy.le.nyc ) then	!Inside image?

               i = xdash						!Find shift from next lowest pixel,line location
               j = ydash
               dx = xdash - i
               dy = ydash - j

               sum = 0.0						!Initiallise sums for forming weighted mean
               wtsum = 0.0

               if ( j.ge.1 .and. j.le.nyc ) then			!Form weighted mean of adjacent 4 pixels,

                  if ( i.ge.1 .and. i.le.nxc ) then			! checking that each lies
                    if ( im(i,j).ne.INVALIR ) then			! within the input image and is not invalid
                        wt = (1.0-dx)*(1.0-dy)				!Weight is calculated from the x,y
                        sum = sum + im(i,j)*wt				!shift from integer pixel locations
                        wtsum = wtsum + wt
                     endif
                  endif

                  if ( i+1.ge.1 .and. i+1.le.nxc ) then
                     if ( im(i+1,j).ne.INVALIR ) then
                        wt = dx*(1.0-dy)
                        sum = sum + im(i+1,j)*wt
                        wtsum = wtsum + wt
                     endif
                  endif

               endif

               if ( j+1.ge.1 .and. j+1.le.nyc ) then

                  if ( i.ge.1 .and. i.le.nxc ) then
                     if (im(i,j+1).ne.INVALIR ) then
                        wt = (1.0-dx)*dy
                        sum = sum + im(i,j+1)*wt
                        wtsum = wtsum + wt
                     endif
                  endif

                  if ( i+1.ge.1 .and. i+1.le.nxc ) then
                     if ( im(i+1,j+1).ne.INVALIR ) then
                        wt = dx*dy
                        sum = sum + im(i+1,j+1)*wt
                        wtsum = wtsum + wt
                     endif
                  endif

               endif

               valr = INT_INVALR					!Load output
               if ( wtsum.gt.0.001 ) then
                  valr = sum/wtsum
                  valr = BS*valr + BZ - ZERO
               endif

               if ( valr.eq.INT_INVALR .and.				!Check for both in, out bad
     +              ( wref(ix,iy).eq.INT_INVALR .or.
     +                wload(ix,iy).eq.0.0 ) ) then

                  wref(ix,iy) = INT_INVALR				!If so load bad
                  wload(ix,iy) = wload(ix,iy) + SCALE(kk)

               else							!Good value

                  refi = 0.0						!New pixel
                  rloadi = 0.0
                  if ( valr.ne.INT_INVALR ) then
                     refi = valr
                     rloadi = SCALE(kk)
                  endif

                  refim = 0.0						!Old pixel
                  rloadim = 0.0
                  if ( wref(ix,iy).ne.INT_INVALR ) then
                     refim = wref(ix,iy)
                     rloadim = wload(ix,iy)
                  endif

                  ovalr = refi*rloadi + refim*rloadim			!Add
                  ovall = rloadi + rloadim
                  if ( ovall.ne.0.0 ) then
                     wref(ix,iy) = ovalr/ovall
                  else
                     wref(ix,iy) = 0.0
                  endif
                  wload(ix,iy) = ovall

               endif

            endif

         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_WORK2S -- Add int*2 image in, in linear mode
C
C alan penny                    ral            1991 March

      subroutine imj_work2s ( im, nxc, nyc, wref, wload, kk )

      implicit none
      include 'imjoin.inc'
      include 'ST_LIMITS_INC'
      include 'ST_IMAGE_INC'
C--
      integer      nxc			!i: Input image X size
      integer      nyc			!i: Input image Y size
      integer*2    im(nxc,nyc)		!i: Input image
      real         wref(NXR,NYR)	!i/o: Working refernce image
      real         wload(NXR,NYR)	!i/o: Working load image
      integer      kk			!i: Input image number
Cbegin
      integer ix, iy, jx, jy, i, j
      real xdash, ydash, xref, yref, sum, wt, wtsum, dx, dy, valr
      real anumh, anuml
      parameter ( anumh=32767.0*32767.0 )
      parameter ( anuml=-1.0*32767.0*32767.0 )
      real refi, rloadi, refim, rloadim, ovalr, ovall
Cbegin


      do iy = KELIM(1,2,kk), KELIM(2,2,kk)
         do ix = KELIM(1,1,kk), KELIM(2,1,kk)

            xref = TRC(1,kk) + TRC(3,kk)*iy				!Set up transform
            yref = TRC(4,kk) + TRC(6,kk)*iy
            xdash = xref + TRC(2,kk)*ix
            ydash = yref + TRC(5,kk)*ix

            jx = nint(min(anumh,max(anuml,xdash)))			!Find nearest pixel location
            jy = nint(min(anumh,max(anuml,ydash)))

            if( jx.ge.1.and.jx.le.nxc.and.jy.ge.1.and.jy.le.nyc ) then	!Inside image?

               i = xdash						!Find shift from next lowest pixel,line location
               j = ydash
               dx = xdash - i
               dy = ydash - j

               sum = 0.0						!Initiallise sums for forming weighted mean
               wtsum = 0.0

               if ( j.ge.1 .and. j.le.nyc ) then			!Form weighted mean of adjacent 4 pixels,

                  if ( i.ge.1 .and. i.le.nxc ) then			! checking that each lies
                    if ( im(i,j).ne.INVALIS ) then			! within the input image and is not invalid
                        wt = (1.0-dx)*(1.0-dy)				!Weight is calculated from the x,y
                        sum = sum + im(i,j)*wt				!shift from integer pixel locations
                        wtsum = wtsum + wt
                     endif
                  endif

                  if ( i+1.ge.1 .and. i+1.le.nxc ) then
                     if ( im(i+1,j).ne.INVALIS ) then
                        wt = dx*(1.0-dy)
                        sum = sum + im(i+1,j)*wt
                        wtsum = wtsum + wt
                     endif
                  endif

               endif

               if ( j+1.ge.1 .and. j+1.le.nyc ) then

                  if ( i.ge.1 .and. i.le.nxc ) then
                     if (im(i,j+1).ne.INVALIS ) then
                        wt = (1.0-dx)*dy
                        sum = sum + im(i,j+1)*wt
                        wtsum = wtsum + wt
                     endif
                  endif

                  if ( i+1.ge.1 .and. i+1.le.nxc ) then
                     if ( im(i+1,j+1).ne.INVALIS ) then
                        wt = dx*dy
                        sum = sum + im(i+1,j+1)*wt
                        wtsum = wtsum + wt
                     endif
                  endif

               endif

               valr = INT_INVALR					!Load output
               if ( wtsum.gt.0.001 ) then
                  valr = sum/wtsum
                  valr = BS*valr + BZ - ZERO
               endif

               if ( valr.eq.INT_INVALR .and.				!Check for both in, out bad
     +              ( wref(ix,iy).eq.INT_INVALR .or.
     +                wload(ix,iy).eq.0.0 ) ) then

                  wref(ix,iy) = INT_INVALR				!If so load bad
                  wload(ix,iy) = wload(ix,iy) + SCALE(kk)

               else							!Good value

                  refi = 0.0						!New pixel
                  rloadi = 0.0
                  if ( valr.ne.INT_INVALR ) then
                     refi = valr
                     rloadi = SCALE(kk)
                  endif

                  refim = 0.0						!Old pixel
                  rloadim = 0.0
                  if ( wref(ix,iy).ne.INT_INVALR ) then
                     refim = wref(ix,iy)
                     rloadim = wload(ix,iy)
                  endif

                  ovalr = refi*rloadi + refim*rloadim			!Add
                  ovall = rloadi + rloadim
                  if ( ovall.ne.0.0 ) then
                     wref(ix,iy) = ovalr/ovall
                  else
                     wref(ix,iy) = 0.0
                  endif
                  wload(ix,iy) = ovall

               endif

            endif

         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_WORK3R -- Add real image in, in uniform mode
C
C alan penny                    ral            1991 March

      subroutine imj_work3r ( im, nxc, nyc, wref, wload, kk )

      implicit none
      include 'imjoin.inc'
      include 'ST_LIMITS_INC'
      include 'ST_IMAGE_INC'
C--
      integer      nxc			!i: Input image X size
      integer      nyc			!i: Input image Y size
      real         im(nxc,nyc)		!i: Input image
      real         wref(NXR,NYR)	!i/o: Working refernce image
      real         wload(NXR,NYR)	!i/o: Working load image
      integer      kk			!i: Input image number
Cbegin
      integer ix, iy, jx, jy, jj, jshift, ii, ishift
      real xdash, ydash, xref, yref, valr, dx, dy, r1, r2, wt, wtsum,
     +     sum
      real wx(-1:+1),wy(-1:+1)						!Weight arrays for constant noise interpolation
      data wx(0)/1.0/,wy(0)/1.0/
      real anumh, anuml
      parameter ( anumh=32767.0*32767.0 )
      parameter ( anuml=-1.0*32767.0*32767.0 )
      real refi, rloadi, refim, rloadim, ovalr, ovall
Cbegin


      do iy = KELIM(1,2,kk), KELIM(2,2,kk)
         do ix = KELIM(1,1,kk), KELIM(2,1,kk)

            xref = TRC(1,kk) + TRC(3,kk)*iy				!Set up transform
            yref = TRC(4,kk) + TRC(6,kk)*iy
            xdash = xref + TRC(2,kk)*ix
            ydash = yref + TRC(5,kk)*ix

            jx = nint(min(anumh,max(anuml,xdash)))			!Find nearest pixel location
            jy = nint(min(anumh,max(anuml,ydash)))

            if( jx.ge.1.and.jx.le.nxc.and.jy.ge.1.and.jy.le.nyc ) then	!Inside image?

               dx = xdash - jx						!Shift from the nearest pixel
               dy = ydash - jy

               r1 = dx*dx + 0.25					!X and Y weight arrays (dependent on the phase dx,dy)
               r2 = dy*dy + 0.25
               wx(-1) = r1 - dx
               wx(1)  = r1 + dx
               wy(-1) = r2 - dy
               wy(1)  = r2 + dy

               sum = 0.0						!scan the 9 nearest pixels, forming
               wtsum = 0.0						! a weighted sum of all the valid ones

               do jshift = -1, 1
                  jj = jy + jshift
                  if ( jj.ge.1 .and. jj.le.nyc ) then			!Still in the image?
                      do ishift = -1, 1
                         ii = jx + ishift
                         if ( ii.ge.1 .and. ii.le.nxc ) then
                           if ( im(ii,jj).ne.INVALIR ) then		!Pixel Valid?
                              wt = wx(ishift)*wy(jshift)
                              sum = sum + im(ii,jj)*wt
                              wtsum = wtsum + wt
                           endif
                        endif
                     enddo
                  endif
               enddo

               valr = INT_INVALR					!Load to output
               if ( wtsum.gt.0.001 ) then
                  valr = sum/wtsum
                  valr = BS*valr + BZ - ZERO
               endif

               if ( valr.eq.INT_INVALR .and.				!Check for both in, out bad
     +              ( wref(ix,iy).eq.INT_INVALR .or.
     +                wload(ix,iy).eq.0.0 ) ) then

                  wref(ix,iy) = INT_INVALR				!If so load bad
                  wload(ix,iy) = wload(ix,iy) + SCALE(kk)

               else							!Good value

                  refi = 0.0						!New pixel
                  rloadi = 0.0
                  if ( valr.ne.INT_INVALR ) then
                     refi = valr
                     rloadi = SCALE(kk)
                  endif

                  refim = 0.0						!Old pixel
                  rloadim = 0.0
                  if ( wref(ix,iy).ne.INT_INVALR ) then
                     refim = wref(ix,iy)
                     rloadim = wload(ix,iy)
                  endif

                  ovalr = refi*rloadi + refim*rloadim			!Add
                  ovall = rloadi + rloadim
                  if ( ovall.ne.0.0 ) then
                     wref(ix,iy) = ovalr/ovall
                  else
                     wref(ix,iy) = 0.0
                  endif
                  wload(ix,iy) = ovall

               endif

            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_WORK3S -- Add int*2 image in, in uniform mode
C
C alan penny                    ral            1991 March

      subroutine imj_work3s ( im, nxc, nyc, wref, wload, kk )

      implicit none
      include 'imjoin.inc'
      include 'ST_LIMITS_INC'
      include 'ST_IMAGE_INC'
C--
      integer      nxc			!i: Input image X size
      integer      nyc			!i: Input image Y size
      integer*2    im(nxc,nyc)		!i: Input image
      real         wref(NXR,NYR)	!i/o: Working refernce image
      real         wload(NXR,NYR)	!i/o: Working load image
      integer      kk			!i: Input image number
Cbegin
      integer ix, iy, jx, jy, jj, jshift, ii, ishift
      real xdash, ydash, xref, yref, valr, dx, dy, r1, r2, wt, wtsum,
     +     sum
      real wx(-1:+1),wy(-1:+1)						!Weight arrays for constant noise interpolation
      data wx(0)/1.0/,wy(0)/1.0/
      real anumh, anuml
      parameter ( anumh=32767.0*32767.0 )
      parameter ( anuml=-1.0*32767.0*32767.0 )
      real refi, rloadi, refim, rloadim, ovalr, ovall
Cbegin


      do iy = KELIM(1,2,kk), KELIM(2,2,kk)
         do ix = KELIM(1,1,kk), KELIM(2,1,kk)

            xref = TRC(1,kk) + TRC(3,kk)*iy				!Set up transform
            yref = TRC(4,kk) + TRC(6,kk)*iy
            xdash = xref + TRC(2,kk)*ix
            ydash = yref + TRC(5,kk)*ix

            jx = nint(min(anumh,max(anuml,xdash)))			!Find nearest pixel location
            jy = nint(min(anumh,max(anuml,ydash)))

            if( jx.ge.1.and.jx.le.nxc.and.jy.ge.1.and.jy.le.nyc ) then	!Inside image?

               dx = xdash - jx						!Shift from the nearest pixel
               dy = ydash - jy

               r1 = dx*dx + 0.25					!X and Y weight arrays (dependent on the phase dx,dy)
               r2 = dy*dy + 0.25
               wx(-1) = r1 - dx
               wx(1)  = r1 + dx
               wy(-1) = r2 - dy
               wy(1)  = r2 + dy

               sum = 0.0						!scan the 9 nearest pixels, forming
               wtsum = 0.0						! a weighted sum of all the valid ones

               do jshift = -1, 1
                  jj = jy + jshift
                  if ( jj.ge.1 .and. jj.le.nyc ) then			!Still in the image?
                      do ishift = -1, 1
                         ii = jx + ishift
                         if ( ii.ge.1 .and. ii.le.nxc ) then
                           if ( im(ii,jj).ne.INVALIS ) then		!Pixel Valid?
                              wt = wx(ishift)*wy(jshift)
                              sum = sum + im(ii,jj)*wt
                              wtsum = wtsum + wt
                           endif
                        endif
                     enddo
                  endif
               enddo

               valr = INT_INVALR					!Load to output
               if ( wtsum.gt.0.001 ) then
                  valr = sum/wtsum
                  valr = BS*valr + BZ - ZERO
               endif

               if ( valr.eq.INT_INVALR .and.				!Check for both in, out bad
     +              ( wref(ix,iy).eq.INT_INVALR .or.
     +                wload(ix,iy).eq.0.0 ) ) then

                  wref(ix,iy) = INT_INVALR				!If so load bad
                  wload(ix,iy) = wload(ix,iy) + SCALE(kk)

               else							!Good value

                  refi = 0.0						!New pixel
                  rloadi = 0.0
                  if ( valr.ne.INT_INVALR ) then
                     refi = valr
                     rloadi = SCALE(kk)
                  endif

                  refim = 0.0						!Old pixel
                  rloadim = 0.0
                  if ( wref(ix,iy).ne.INT_INVALR ) then
                     refim = wref(ix,iy)
                     rloadim = wload(ix,iy)
                  endif

                  ovalr = refi*rloadi + refim*rloadim			!Add
                  ovall = rloadi + rloadim
                  if ( ovall.ne.0.0 ) then
                     wref(ix,iy) = ovalr/ovall
                  else
                     wref(ix,iy) = 0.0
                  endif
                  wload(ix,iy) = ovall

               endif

            endif
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_TYPE2 -- Add two reference images
C
C alan penny                    ral            1991 March

      subroutine imj_type2 ( )

      implicit none
      include 'imjoin.inc'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
C---
      real    rva, rvb, rvc, bs1, bz1, rinval1, bs2, bz2, rinval2
      integer istat, iv, ivb, ipr1, ipr2, ipl1, ipl2, ipoa
      character*70 titler, titlel, otitler, otitlel, otitle
      character*68 texth
Cbegin


      call opimrr ( 'INREF1', ipr1, NXR, NYR, .true., istat )		!Reference image
      if ( istat.ne.0 ) then
         call printo ( 'ERROR: Error is in first input Reference image')
         ST_FAILED = .true.
         return
      endif
      call gtimrd ( 'INREF1', bs1, bz1, rinval1, titler, istat )

      call opimrr ( 'INLOAD1', ipl1, iv, ivb, .false., istat )
      if ( istat.ne.0 ) then
         call printo ( 'ERROR: Error is in first input Load image' )
         ST_FAILED = .true.
         return
      endif
      if ( iv.ne.NXR .or. ivb.ne.NYR ) then
         call printo (
     +    'ERROR: First Load image different size to reference image' )
         ST_FAILED = .true.
         return
      endif
      call gtimrd ( 'INLOAD1', rva, rvb, rvc, titlel, istat )

      call opimrr ( 'INREF2', ipr2, iv, ivb, .true., istat )			!Reference image
      if ( istat.ne.0 ) then
         call printo ('ERROR: Error is in second input Reference image')
         ST_FAILED = .true.
         return
      endif
      if ( iv.ne.NXR .or. ivb.ne.NYR ) then
         call printo ( 'ERROR: Second Reference image different '//
     +                         'size to reference image' )
         ST_FAILED = .true.
         return
      endif
      call gtimrd ( 'INREF2', bs2, bz2, rinval2, titler, istat )

      call opimrr ( 'INLOAD2', ipl2, iv, ivb, .false., istat )
      if ( istat.ne.0 ) then
         call printo ( 'ERROR: Error is in second input Load image' )
         ST_FAILED = .true.
         return
      endif
      if ( iv.ne.NXR .or. ivb.ne.NYR ) then
         call printo (
     +   'ERROR: Second Load image different size to reference image' )
         ST_FAILED = .true.
         return
      endif

      call get1b ( 'DOOUT', DOUTIM, .true. )				!Open output image
      IPO = 1
      ipoa = 1
      if ( DOUTIM ) then
         call get_job ( 'OUTTYPE', 'real:short', iv, 2, texth, 0 )
         OUTTYPE = 'REAL'
         if ( iv.eq.2 ) OUTTYPE = 'SHORT'
         call opimzw ( 'OUT', OUTTYPE, IPO, NXR, NYR, .true., istat )
         if ( istat.ne.0 ) DOUTIM = .false.
      endif

      if ( DOUTIM ) then
         if ( OUTTYPE.eq.'SHORT' ) call gtwrkr ( 'WORKO', NXR*NYR,
     +                                            ipoa, istat )
         BSO = bs1
         BZO = bz1
         call get2r ( 'OUTSCALE', BSO, BZO, .true., -1.0e20, 1.0e20 )
         call get1c (  'TITLE', otitle, titler, .true. )
         call icopdes ( 'INREF1',  'OUT', istat )
         call ptdesc ( 'OUT',  'TITLE',  otitle )
         call ptdesr ( 'OUT',  'BZERO',  BZO    )
         call ptdesr ( 'OUT',  'BSCALE', BSO    )
         if ( OUTTYPE.eq.'SHORT' ) then
            call ptdesi ( 'OUT',  'INVAL',  INVALO )
         else
            call ptdesr ( 'OUT',  'INVAL',  INT_INVALR )
         endif
      endif

      call opimrw ( 'OUTREF', IPOR, NXR, NYR, .true., istat )		!Open output reference image
      DOUTREF = .true.
      if ( istat.ne.0 ) DOUTREF = .false.
      if ( .not.DOUTREF ) IPOR = 1
      IPOL = 1
      if ( DOUTREF ) then
         call get1c (  'TITLER', otitler, titler, .true. )
         call opimrw ( 'OUTLOAD', IPOL, NXR, NYR, .true., istat )	!Open output load image
         call get1c (  'TITLEL', otitlel, titlel, .true. )
         call icopdes ( 'INREF1',  'OUTREF', istat )
         call icopdes ( 'INLOAD1', 'OUTLOAD', istat )
         call ptdesc ( 'OUTREF',  'TITLE', otitler )
         call ptdesc ( 'OUTLOAD', 'TITLE', otitlel )
      endif

      if ( OUTTYPE.eq.'REAL' ) then
         ipoa = IPO
      else
         RINVALO = INT_INVALR
      endif
      call imj_xxdoit ( %val(ipr1), %val(ipl1), %val(ipr2),	 	!Load ouput images
     +                  %val(ipl2), %val(IPOR), %val(IPOL), %val(ipoa),
     +                  NXR, NYR, bs1, bz1, rinval1, bs2, bz2,
     +                  rinval2, DOUTREF, DOUTIM, BSO, BZO, RINVALO )
      if ( DOUTIM .and. OUTTYPE.eq.'SHORT' ) call azchtrs ( %val(ipoa),
     +   INT_INVALR, INT_MINSR, INT_MAXSR, %val(IPO), INVALO, NXR*NYR )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMJ_XXDOIT -- Do work of adding two reference images
C
C alan penny                    ral            1991 March

      subroutine imj_xxdoit ( rim1, lim1, rim2, lim2, orim, olim, out,
     +                        nx, ny, bs1, bz1, rinval1, bs2, bz2,
     +                        rinval2, doutref, doutim, bso, bzo,
     +                        rinvalo )

      implicit none

      integer   nx		!i: X size of images
      integer   ny		!i: Y size of images
      real      rim1(nx,ny)	!i: 1st input reference image
      real      lim1(nx,ny)	!i: 1st input load image
      real      rim2(nx,ny)	!i: 2nd input reference image
      real      lim2(nx,ny)	!i: 2nd input load image
      real      orim(nx,ny)	!o: Output reference image
      real      olim(nx,ny)	!o: Output load image
      real      out(nx,ny)	!o: Output image
      real      bs1		!i: Scale of 1st reference image
      real      bz1		!i: Zero of 1st reference image
      real      rinval1		!i: 'INVAL' value of 1st reference image
      real      bs2		!i: Scale of 1st reference image
      real      bz2		!i: Zero of 1st reference image
      real      rinval2		!i: 'INVAL' value of 1st reference image
      logical   doutref		!i: Output reference image?
      logical   doutim		!i: Output image?
      real      bso		!i: Scale of output image
      real      bzo		!i: Zero of output image
      real      rinvalo		!i: Magic value of output image
C--
      integer j, k
      logical fl1, fl2
      real vr1, vl1, vr2, vl2, rro, rlo
Cbegin


      do k = 1 , ny
         do j = 1, nx

            vr1 = rim1(j,k) 						!Input values
            vl1 = lim1(j,k)
            vr2 = rim2(j,k)
            vl2 = lim2(j,k)

            if ( vr1.eq.rinval1 ) then					!Calc output
               vr1 = 0.0
               vl1 = 0.0
               fl1 = .true.
            else
               fl1 = .false.
               vr1 = vr1*bs1 + bz1
            endif

            if ( vr2.eq.rinval2 ) then
               vr2 = 0.0
               vl2 = 0.0
               fl2 = .true.
            else
               fl2 = .false.
               vr2 = vr2*bs2 + bz2
            endif

            rlo = vl1 + vl2
            if ( fl1 .and. fl2 ) then
               rro = rinval1
            else
               rro = 0.0
               if ( rlo.ne.0.0 ) then
                  rro = (vr1*vl1+vr2*vl2)/rlo
                  rro = (rro-bz1)/bs1
               endif
            endif

            if ( DOUTREF ) then
               orim(j,k) = rro						!Load output
               olim(j,k) = rlo
            endif

            if ( DOUTIM ) then
               if ( rlo.eq.0.0 ) then
                  out(j,k) = rinvalo
               else
                  out(j,k) = (rro-bzo)/bso
               endif
            endif

         enddo
      enddo


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMKEY.F
C
C  Contains:-
C
C T_IMKEY       Do image keyboard
C IMKE_GCL      Get input and output info from the command line
C IMKE_OUTT     Get output image type
C IMKE_SPACE    Get work space for line stack and line
C IMKE_DOIT(RS) Combine the lines from the stack of images into one line



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMKEY -- Do image keyboard creation/amending
C
C  alan penny                  ral              1993 Dec

      subroutine t_imkey ()

      implicit none
      include 'imkey.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer istat
Cbegin


      call imke_gcl							!Get input/output info
      if ( ST_FAILED ) return

      if ( IMTYPE.eq.'REAL' ) then
         call gtwrkr ( 'WORKA', NX*NY, IPIMA, istat )
      else
         call gtwrks ( 'WORKA', NX*NY, IPIMA, istat )
      endif
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call imke_load							!Load base image
      if ( ST_FAILED ) return

      if ( IMTYPE.eq.'REAL' ) then					!Keyboard input of data
         call imke_doitr ( %val(IPIMA), NX, NY )
         call amovr ( %val(IPIMA), %val(IPO), NX*NY )
      else
         call imke_doits ( %val(IPIMA), NX, NY )
         call amovs ( %val(IPIMA), %val(IPO), NX*NY )
      endif


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMKE_GCL -- Get input and output info from the command line
C
C  alan penny                         RAL                1991 Dec

      subroutine imke_gcl ( )

      implicit none
      include 'imkey.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
C--
      integer ierr
      character title*50

      integer kopt
      integer nth
      parameter ( nth=5 )
      character*68 th(nth)
      data th /
     + 'Is the output image ? :- ',
     + 'Option   Choice',
     + '------   --------',
     + 'Real     Real 32-bit ',
     + 'Short    Signed 16-bit integer '/

Cbegin


      if ( ST_FAILED ) return

      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .true., ierr )
      if ( ST_FAILED ) return

      INIM = .true.
      if ( ierr.eq.2 ) then
         INIM = .false.
         IPIM = 1
         NX = 1
         NY = 1
         call get2i ( 'SIZE', NX, NY, .true., 1, 100000 )
         if ( ST_FAILED ) return
         IMTYPE = 'REAL'
         call get_job ( 'OUTTYPE', 'real:short', kopt, 1, th, nth )
         if ( ST_FAILED ) return
         if ( kopt.eq.2 ) IMTYPE = 'SHORT'
         if ( IMTYPE.eq.'SHORT' ) then
            call printo ( ' ' )
            call printo ( 'The pixel values are stored as 16-bit'//
     +                     ' integers, as ((value-BZERO)/BSCALE)' )
            call printo ( 'As these are integers in the range '//
     +                    ' -32765 to +32767,' )
            call printo ( 'you may wish to change the -scale and '//
     +                    'zero-,' )
            call printo ( 'either to prevent integer steps being'//
     +                    ' important, or to store big numbers' )
            BS = 1.0
            BZ = 0.0
            call get2r ( 'OUTSCALE', BS, BZ, .true., 1.0,
     +                   -1.0e20, 1.0e20 )
            if ( ST_FAILED ) return
         else
            BS = 1.0
            BZ = 0.0
         endif
         call get1r ( 'LEVEL', RLEV, 0.0, -1.0e10, 1.0e10 )
         if ( ST_FAILED ) return
         IMTITLE = 'Output from IMKEY'
         INVAL = INT_INVALSI
         RINVAL = INT_INVALR
      elseif ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      else
         call pargi ( NX )
         call pargi ( NY )
         call printd ( 'Image size is: %d x %d' )
         call pargc ( IMTYPE )
         call printd ( 'Image type is: %c')
         call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE,
     +                 ierr )
         if ( ierr.ne.0 ) then
            ST_FAILED = .true.
            return
         endif
      endif

      call opimzw ( 'OUT', IMTYPE, IPO, NX, NY, .true., ierr )       !Get output image
      if ( ierr.eq.1 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call get1c  ( 'TITLE', title, IMTITLE, .true. )
      if ( ST_FAILED ) return

      if ( INIM ) then
         call icopdes ( 'IN', 'OUT', ierr )
         if ( ierr.eq.1 ) ST_FAILED = .true.
      else
         call ptdesr ( 'OUT', 'BSCALE', BS )
         call ptdesr ( 'OUT', 'BZERO', BZ )
         if ( IMTYPE.eq.'REAL' ) then
            call ptdesr ( 'OUT', 'INVAL', RINVAL )
         else
            call ptdesi ( 'OUT', 'INVAL', INVAL )
         endif
      endif
      if ( ST_FAILED ) return
      call ptdesc ( 'OUT', 'TITLE', title )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMKE_LOAD -- Load work image with starter image
C
C  alan penny                RAL                1991 Dec

      subroutine imke_load ( )

      implicit none
      include 'imkey.inc'
      include 'ST_IMAGE_INC'
C--
      real rv
      integer*2 isv
Cbegin


      if ( INIM ) then
         if ( IMTYPE.eq.'REAL' ) then
            call amovr ( %val(IPIM), %val(IPIMA), NX*NY )
         else
            call amovs ( %val(IPIM), %val(IPIMA), NX*NY )
         endif
      else
         rv = (RLEV-BZ)/BS
         if ( IMTYPE.eq.'REAL' ) then
            call amovkr ( rv, %val(IPIMA), NX*NY )
         else
            isv = min(32767.0,max(-32768.0,rv))
            call amovks ( isv, %val(IPIMA), NX*NY )
         endif
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMKE_DOITS -- Put keyboard entry in
C
C  alan penny                     RAL            1991 Dec

      subroutine imke_doits ( in, nxa, nya )

      implicit none
      include 'imkey.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer    nxa		!i:   X size of image
      integer    nya		!i:   Y size of image
      integer*2  in(nxa,nya)	!i/o: Work image
C--
      integer kx, ky, iv, nmax
      real rv, rva
      logical loop
Cbegin


      nmax = max(nxa,nya)
      kx = 1
      ky = 1

      call printo ( ' ' )
      call printo ( 'Respond 1,0 to input INVALID pixel, 0,1 to end ' )
      call printo ( ' ' )

      loop = .true.
      do while ( loop )

         call get2i ( 'XY', kx, ky, .true., 0, nmax )
         if ( ST_FAILED ) return
         if ( kx.lt.1 ) then
            loop = .false.
            call printo ( 'Input finished' )
         elseif ( kx.gt.nxa .or. ky.gt.nya ) then
            call printo ( 'ERROR: out of image' )
         elseif ( ky.lt.1 ) then
            call printo ( 'Next pixel to be marked as INVALID ' )
            call get2i ( 'INVALXY', kx, ky, .true., 1, nmax )
            if ( ST_FAILED ) return
            in(kx,ky) = INVAL
         else
            if ( in(kx,ky).eq.INVAL ) then
               call printo ( 'Pixel INVALID in input image' )
               rv = 0.0
            else
               rv = in(kx,ky)
               rv = rv*BS + BZ
            endif
            call get1r ( 'VALUE', rva, rv, -1.0e10, 1.0e10 )
            if ( ST_FAILED ) return
            rva = (rva-BZ)/BS
            iv = min(32767.0,max(-32768.0,rva))
            in(kx,ky) = iv
         endif

       enddo


       end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMKE_DOITR -- Put keyboard entry in
C
C  alan penny                     RAL            1991 Dec

      subroutine imke_doitr ( in, nxa, nya )

      implicit none
      include 'imkey.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer    nxa		!i:   X size of image
      integer    nya		!i:   Y size of image
      real       in(nxa,nya)	!i/o: Work image
C--
      integer kx, ky, nmax
      real rv, rva
      logical loop
Cbegin


      nmax = max(nxa,nya)
      kx = 1
      ky = 1

      call printo ( ' ' )
      call printo ( 'Respond 1,0 to input INVALID pixel, 0,1 to end ' )
      call printo ( ' ' )

      loop = .true.
      do while ( loop )

         call get2i ( 'XY', kx, ky, .true., 0, nmax )
         if ( ST_FAILED ) return
         if ( kx.lt.1 ) then
            loop = .false.
            call printo ( 'Input finished' )
         elseif ( kx.gt.nxa .or. ky.gt.nya ) then
            call printo ( 'ERROR: out of image' )
         elseif ( ky.lt.1 ) then
            call printo ( 'Next pixel to be marked as INVALID ' )
            call get2i ( 'INVALXY', kx, ky, .true., 1, nmax )
            if ( ST_FAILED ) return
            in(kx,ky) = RINVAL
         else
            if ( in(kx,ky).eq.RINVAL ) then
               call printo ( 'Pixel INVALID in input image' )
               rv = 0.0
            else
               rv = in(kx,ky)
               rv = rv*BS + BZ
            endif
            call get1r ( 'VALUE', rva, rv, -1.0e10, 1.0e10 )
            if ( ST_FAILED ) return
            in(kx,ky) = (rva-BZ)/BS
         endif

       enddo


       end












CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  T_IMPORT.FOR
C
C    Contains:-
C
C T_IMPORT      Port Extensions to/from Starman, FITS, Figaro others
C IMPO_INPUT    Input image
C IMPO_DCOP     Port Extensions
C IMPO_DOIFITS  Deal with input FITS extensions
C IMPO_DOOFITS  Deal with output FITS extensions


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMPORT -- Port Extensions to/from Starman, FITS, Figaro others
C   For a fuller description see IMPORT.HLP
C
C   pat morris    leeds     April 1994

      subroutine t_import ()

      implicit none
      include 'STARMAN_INC'
C--
      integer kin, kout, ndf
      logical dodel
      character*40 istruct, ostruct

      character*1000 topt1
      data topt1 / 'figaro:fits:starman:other' /
      integer nth1
      parameter ( nth1=6)
      character*68 th1(nth1)
      data th1 /
     + 'Option    Function',
     + '------    --------',
     + 'Figaro    From a Figaro NDF image',
     + 'Fits      From an NDF image with a Fits Extension ',
     + 'Other     From an NDF image with a user defined Extension',
     + 'Starman   From an NDF image with a Starman Extension ' /

      character*1000 topt2
      data topt2 / 'figaro:fits:starman:other' /
      integer nth2
      parameter ( nth2=6)
      character*68 th2(nth2)
      data th2 /
     + 'Option    Function',
     + '------    --------',
     + 'Figaro    From a Figaro NDF image',
     + 'Fits      From an NDF image with a Fits Extension ',
     + 'Other     From an NDF image with a user defined Extension',
     + 'Starman   From an NDF image with a Starman Extension '/
Cbegin


      call impo_input ( ndf )
      if ( ST_FAILED ) return

      call get_job ( 'FROM', topt1, kin, 2, th1, nth1 )
      if ( ST_FAILED ) return
      if ( kin.eq.1 ) then
         istruct = 'FIGARO'
      elseif ( kin.eq.2 ) then
         istruct = 'FITS'
      elseif ( kin.eq.3 ) then
         istruct = 'STARMAN'
      else
         call get1c ( 'IN_EXTEN', istruct, ' ', .true.)
         if ( ST_FAILED ) return
      endif

      call get_job ( 'TO', topt2, kout, 3, th2, nth2 )
      if ( ST_FAILED ) return

      if ( kout.eq.kin .and. kout.ne.4 ) then
         call printo ( 'No porting needed' )
         return
      endif

      if ( kout.eq.1 ) then
         ostruct = 'FIGARO'
      elseif ( kout.eq.2 ) then
         ostruct = 'FITS'
      elseif ( kout.eq.3 ) then
         ostruct = 'STARMAN'
      else
         call get1c ( 'OUT_EXTEN', ostruct, ' ', .true.)
         if ( ST_FAILED ) return
      endif

      call get1b ( 'DELETE', dodel, .false. )				!Delete input extension?
      if ( ST_FAILED ) return

      call impo_dcop ( ndf, istruct, ostruct, dodel )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMPO_INPUT -- Input NDF
C
C   pat morris    leeds     April 1994

      subroutine impo_input ( ndf )

      implicit none
      include 'STARMAN_INC'
      include 'SAE_PAR'

      integer          ndf		!o: Pointer to image
C--
      integer  status
Cbegin


      if ( ST_FAILED ) return

      status = SAI__OK
      call ndf_assoc ( 'IN', 'UPDATE', ndf, status )        		!Input NDF
      if ( status.ne.SAI__OK ) ST_FAILED = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMPO_DCOP -- Port extensions
C
C   pat morris    leeds     April 1994

      subroutine impo_dcop ( ndf, istruct, ostruct, dodel )

      implicit none
      include 'STARMAN_INC'
      include 'DAT_PAR'
      include 'SAE_PAR'

      integer          ndf		!i: Pointer to image
      character*(*)    istruct		!i: Structure to copy from
      character*(*)    ostruct		!i: Structure to copy to
      logical          dodel		!i: Delete old extension?
C--
      integer status, ncomp, k, num, ipi
      logical there
      character*(DAT__SZNAM) name
      character*(DAT__SZLOC) ieloc, oeloc, loc
Cbegin


      if ( ST_FAILED ) return

      status = SAI__OK

      if ( ostruct.ne.' ' .and. ostruct.ne.'FITS' ) then		!Get locator to ouput extension - if it is not FITS

         call ndf_xstat ( ndf, ostruct, there, status )			!Is the output structure there ?
         if ( .not.there ) then 					! Create it
            call ndf_xnew ( ndf, ostruct, 'EXT', 0, 0, oeloc, status)
         else
            call ndf_xloc ( ndf, ostruct, 'WRITE', oeloc, status )
         endif

      endif

      if ( istruct.eq.'FITS' ) then

         call ndf_xloc ( ndf, 'FITS', 'READ', ieloc, status )		!Obtain access to the input extension
         call dat_mapv ( ieloc, '_CHAR*80', 'READ', ipi, num, status )
         call impo_doifits ( %val(ipi), num, ndf, ostruct, status )
         call dat_annul ( ieloc, status )
         if ( dodel ) call ndf_xdel  ( ndf, istruct, status )		!Delete the old structure/extension

      elseif ( ostruct.eq.'FITS' ) then

         call ndf_xstat ( ndf, istruct, there, status )
         if ( .not.there ) then
            call pargc ( istruct )
            call printd ( 'ERROR: Input image has no %c extension' )
            return
         endif
         call ndf_xloc ( ndf, istruct, 'READ', ieloc, status )		!Obtain access to the input structure
         call dat_ncomp ( ieloc, ncomp, status )			!Copy extension

         call ndf_xstat ( ndf, 'FITS', there, status )
         if (.not.there ) then
            call ndf_xnew (ndf,'FITS','_CHAR*80',1,ncomp,oeloc,status)
         else
            call ndf_xloc ( ndf, 'FITS', 'WRITE', oeloc, status )
         endif

         call dat_mapv (oeloc,'_CHAR*80','WRITE',ipi,num,status )
         call impo_doofits ( %val(ipi), num, ndf, ieloc, status )

         call dat_annul ( oeloc, status )
         if ( dodel ) call ndf_xdel  ( ndf, istruct, status )   	!Delete the old structure/extension

      else

         call ndf_xstat ( ndf, istruct, there, status )			!Check Input extensions is there
         if ( .not.there ) then
            call pargc ( istruct )
            call printd ( 'ERROR: Input image has no %c extension' )
            return
         endif

         call ndf_xloc ( ndf, istruct, 'READ', ieloc, status )		!Obtain access to the input structure
         call dat_ncomp ( ieloc, ncomp, status )			!Copy extension
         if (ncomp.ge.2) then
          do k = 1, ncomp
             call dat_index ( ieloc, k, loc, status )
             call dat_name ( loc, name, status )
             call dat_copy ( loc, oeloc, name, status )
             call dat_annul ( loc, status )
          enddo
         endif

         call dat_annul ( ieloc, status )
         call dat_annul ( oeloc, status )

         if ( dodel ) call ndf_xdel  ( ndf, istruct, status )   	!Delete the old structure/extension

      endif

      if ( status.ne.SAI__OK ) ST_FAILED = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMPO_DOIFITS -- Deal with input FITS extensions
C
C   pat morris    leeds     April 1994

      subroutine impo_doifits ( ctext, num, ndf, ostruct, status )

      implicit none
      include 'STARMAN_INC'
      include 'SAE_PAR'

      integer          num              !i: Number of entries in array
      character*80     ctext(num)	!i: Character array
      integer          ndf		!i: Pointer to image
      character*(*)    ostruct		!i: Structure to copy to
      integer          status           !i: Status flag
C--
      integer j, k, iv, istat
      real rv
      character text*80, dtext*80, descr*8
Cbegin


      if ( status.ne.SAI__OK ) return
      if ( ST_FAILED ) return

      do k = 1, num

         text = ctext(k)

         descr = text(1:8)

         j = index(text,'/')
         if ( j.eq.0 ) then
            dtext = text(10:)
         else
            dtext = text(10:j-1)
         endif

         istat = SAI__OK
         call lbgone ( dtext )
         if ( descr.ne.' ' ) then
            if ( dtext.eq.' ' ) then
               call ndf_xpt0c ( ' ', ndf, ostruct, descr, istat )
            else
               call chr_ctoi ( dtext, iv, istat )
               if ( istat.eq.SAI__OK ) then
                  call ndf_xpt0i ( iv, ndf, ostruct, descr, istat )
               else
                  istat = SAI__OK
                  call chr_ctor ( dtext, rv, istat )
                  if ( istat.eq.SAI__OK ) then
                     call ndf_xpt0r ( rv, ndf, ostruct, descr, istat )
                  else
                     istat = SAI__OK
                     call ndf_xpt0c ( dtext, ndf, ostruct,descr,istat)
                  endif
               endif
            endif
         endif

      enddo

      if ( status.ne.SAI__OK ) ST_FAILED = .true.


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMPO_DOOFITS -- Deal with output FITS extensions
C
C   pat morris    leeds     April 1994

      subroutine impo_doofits ( ctext, num, ndf, ieloc, status )

      implicit none
      include 'STARMAN_INC'
      include 'SAE_PAR'
      include 'DAT_PAR'

      integer          num              !i: Number of entries in array
      character*80     ctext(num)	!i: Character array
      integer          ndf		!i: Pointer to image
      character*(DAT__SZLOC) ieloc	!i: Locator to input structure
      integer          status           !i: Status flag
C--
      integer j, k, l,chr_len
      character text*80, descr*8, dtext*70
      character*(DAT__SZLOC) loc
      character*(DAT__SZTYP) type
      external chr_len
Cbegin


      if ( ST_FAILED ) return

      call dat_ncomp ( ieloc, j, status )				!Copy extension

      if ( j.ge.2 ) then
        do k = 1, j
         call dat_index ( ieloc, k, loc, status )
         call dat_name ( loc, descr, status )
         text = descr
         text(9:9) = '='
         call chr_term ( 70, text(10:) )
         call dat_get0c ( loc, dtext, status )
         call dat_type ( loc, type,  status )
         if ( type(1:5).eq.'_CHAR' ) then
            call chr_putc ( dtext, text, 12 )
         else
            l = chr_len (dtext)
            l = 30 - l
            call chr_putc ( dtext, text, l )
         endif
         ctext(k) = text
         call dat_annul ( loc, status )
        enddo
      endif

      call dat_annul ( ieloc, status )

      if ( status.ne.SAI__OK ) ST_FAILED = .true.


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  T_IMROTATE.F
C
C    Contains:-
C
C T_IMROTATE   Rotate an image by 90/180/270 degrees
C IMR_DOIT   Load the output image from the input image


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMROTATE -- Rotate an image by 90/180/270 degrees
C
C   alan penny                     ral           1991 May

      subroutine t_imrotate ( )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer ierr, ipo, nxo, nyo, k
      character title*50, thelp*68
Cbegin


      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE, ierr )

      call get_job ( 'DEGREES', '90:180:270', k, 1, thelp, 0 )

      if ( k.eq.1 .or. k .eq.3 ) then
         nxo = NY
         nyo = NX
      else
         nxo = NX
         nyo = NY
      endif

      call opimzw ( 'OUT', IMTYPE, ipo, nxo, nyo, .false., ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      if ( IMTITLE.eq.' ' ) IMTITLE = 'Output from IMROTATE'
      call get1c ( 'TITLE', title, IMTITLE, .true. )

      if ( IMTYPE.eq.'SHORT' ) then
         call imr_doits ( %val(IPIM), %val(ipo), nxo, nyo, k )
      else
         call imr_doitr ( %val(IPIM), %val(ipo), nxo, nyo, k )
      endif

      call icopdes ( 'IN', 'OUT', ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call ptdesc ( 'OUT', 'TITLE', title )

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMR_DOITR -- Rotate real image
C
C   alan penny                     ral           1991 May

      subroutine imr_doitr ( in, out, nxo, nyo, kopt )

      implicit none
      include 'ST_IMAGE_INC'
C--
      real        in(NX,NY)		!i: Input image
      integer     nxo			!i: Output image X size
      integer     nyo			!i: Output image Y size
      real        out(nxo,nyo)		!o: Output image
      integer     kopt                  !i: Rotation code (1,2,3=90,180,270)
C--
      integer j, ja, k, ka
Cbegin


      if ( kopt.eq.1 ) then
         do k = 1, nyo
            do j = 1, nxo
               ja = nxo - j + 1
               if ( k.lt.NX .and. ja.lt.NY ) then
                  out(j,k) = in(k,ja)
               else
                  out(j,k) = RINVAL
               endif
            enddo
         enddo
      elseif ( kopt.eq.2 ) then
         do k = 1, nyo
            ka = nyo - k + 1
            do j = 1, nxo
               ja = nxo - j + 1
               if ( ja.lt.NX .and. ka.lt.NY ) then
                  out(j,k) = in(ja,ka)
               else
                  out(j,k) = RINVAL
               endif
            enddo
         enddo
      elseif ( kopt.eq.3 ) then
         do k = 1, nyo
            ka = nyo - k + 1
            do j = 1, nxo
               if ( ka.lt.NX .and. j.lt.NY ) then
                  out(j,k) = in(ka,j)
               else
                  out(j,k) = RINVAL
               endif
            enddo
         enddo
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMR_DOITS -- Rotate int*2 image
C
C   alan penny                     ral           1991 May

      subroutine imr_doits ( in, out, nxo, nyo, kopt )

      implicit none
      include 'ST_IMAGE_INC'
C--
      integer*2   in(NX,NY)		!i: Input image
      integer     nxo			!i: Output image X size
      integer     nyo			!i: Output image Y size
      integer*2   out(nxo,nyo)		!o: Output image
      integer     kopt                  !i: Rotation code (1,2,3=90,180,270)
C--
      integer j, ja, k, ka
Cbegin


      if ( kopt.eq.1 ) then
         do k = 1, nyo
            do j = 1, nxo
               ja = nxo - j + 1
               if ( k.lt.NX .and. ja.lt.NY ) then
                  out(j,k) = in(k,ja)
               else
                  out(j,k) = INVAL
               endif
            enddo
         enddo
      elseif ( kopt.eq.2 ) then
         do k = 1, nyo
            ka = nyo - k + 1
            do j = 1, nxo
               ja = nxo - j + 1
               if ( ja.lt.NX .and. ka.lt.NY ) then
                  out(j,k) = in(ja,ka)
               else
                  out(j,k) = INVAL
               endif
            enddo
         enddo
      elseif ( kopt.eq.3 ) then
         do k = 1, nyo
            ka = nyo - k + 1
            do j = 1, nxo
               if ( ka.le.NX .and. j.le.NY ) then
                  out(j,k) = in(ka,j)
               else
                  out(j,k) = INVAL
               endif
            enddo
         enddo
      endif


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMSMOOTH.F  Smooth int*2/real image with gauss or rectangle mean/median box
C
C Contains:-
C
C T_IMSMOOTH      Smooth int*2/real image with gauss or rectangle mean/median box
C IMSM_GAUSS(SR)  Smooth a int*2/real flagged array with gaussian
C IMSM_SETV(SR)   Get a replacement int*2/real value for an INVALID pixel


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMSMOOTH -- Smooth int*2/real image with gauss or rectangle mean/median box
C  Take account of invalid pixels.
C
C Alan Penny                       RAL               1994 Feb

      subroutine t_imsmooth ( )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer ierr, ipo, iv, nxb, nyb, ipwa, ipwb, ipwc, ipwd,
     +        ipwe, kmeth, nxa, nya
      real    rad
      character*50 title
Cbegin


      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., ierr )
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE, ierr )

      call get_job ( 'METHOD', 'gauss:box:median', kmeth, 2, ' ', 0 )

      if ( kmeth.eq.1 ) then
         call get1r ( 'GRADIUS', rad, 2.0, 1.0e-5, 1.0e10 )
         nxb = rad*10.0
         nyb = rad*10.0
         iv = max(NX,NY)
         nxb = min(nxb,(NX-1))
         nyb = min(nyb,(NY-1))
         if ( nxb.eq.(2*(nxb/2)) ) nxb = nxb + 1
         if ( nyb.eq.(2*(nyb/2)) ) nyb = nyb + 1
         call get2i ( 'GBOX', nxb, nyb, .true., 1, iv )
         if ( ST_FAILED ) return
         if ( nxb.eq.(2*(nxb/2)) ) then
            call printo ( 'ERROR: X Size of box must be odd' )
            ST_FAILED = .true.
         endif
         if ( nyb.eq.(2*(nyb/2)) ) then
            call printo ( 'ERROR: Y Size of box must be odd' )
            ST_FAILED = .true.
         endif
         if ( ST_FAILED ) return
      else
         nxb = 3
         nyb = 3
         iv = max(NX,NY)
         call get2i ( 'BOX', nxb, nyb, .true., 1, iv )
         if ( ST_FAILED ) return
      endif

      call opimzw ( 'OUT', IMTYPE, ipo, NX, NY, .false., ierr )
      if ( ierr.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call get1c ( 'TITLE', title, IMTITLE, .true. )
      if ( ST_FAILED ) return

      if ( kmeth.eq.1 ) then

         call gtwrkr ( 'WB', nxb*nyb, ipwb, ierr )

         nxa = NX + 2*(nxb/2)
         nya = NY + 2*(nyb/2)
         if ( IMTYPE.eq.'SHORT' ) then
            call gtwrks ( 'WA', nxa*nya, ipwa, ierr )
            call gtwrks ( 'WC', nx*ny, ipwc, ierr )
            call imsm_gausss ( %val(IPIM), NX, NY, INVAL, rad,
     +                         %val(ipwa), nxa, nya,
     +                         %val(ipwc), %val(ipwb), nxb, nyb )
            call amovs ( %val(ipwc), %val(ipo), NX*NY )
         else
            call gtwrkr ( 'WA', nxa*nya, ipwa, ierr )
            call gtwrkr ( 'WC', nx*ny, ipwc, ierr )
            call imsm_gaussr ( %val(IPIM), NX, NY, RINVAL, rad,
     +                         %val(ipwa), nxa, nya,
     +                         %val(ipwc), %val(ipwb), nxb, nyb )
            call amovr ( %val(ipwc), %val(ipo), NX*NY )
         endif

      elseif ( kmeth.eq.2 ) then

         call gtwrkr ( 'WB', NX*NY, ipwb, ierr )
         call gtwrkr ( 'WC', NX*NY, ipwc, ierr )
         call gtwrkr ( 'WD', NX,    ipwd, ierr )
         call gtwrkr ( 'WE', NX,    ipwe, ierr )

         if ( IMTYPE.eq.'SHORT' ) then
            call gtwrks ( 'WA', NX*NY, ipwa, ierr )
            call amovs ( %val(IPIM), %val(ipwa), NX*NY )
            call smoothbs ( %val(ipwa), NX, NY, INVAL, nxb, nyb,
     +                      1, %val(ipwb), %val(ipwc), %val(ipwd),
     +                      %val(ipwe) )
            call amovs ( %val(ipwa), %val(ipo), NX*NY )
         else
            call gtwrkr ( 'WA', NX*NY, ipwa, ierr )
            call amovr ( %val(IPIM), %val(ipwa), NX*NY )
            call smoothbr ( %val(ipwa), NX, NY, RINVAL, nxb, nyb,
     +                     1, %val(ipwb), %val(ipwc), %val(ipwd),
     +                     %val(ipwe) )
            call amovr ( %val(ipwa), %val(ipo), NX*NY )
         endif

      else

         if ( IMTYPE.eq.'SHORT' ) then
            call gtwrks ( 'WB', NX*NY, ipwb, ierr )
            call gtwrki ( 'WC', nxb*nyb, ipwc, ierr )
            call imsm_medians ( %val(IPIM), %val(ipwb), %val(ipwc),
     +                          nxb, nyb )
            call amovs ( %val(ipwb), %val(ipo), NX*NY )
         else
            call gtwrkr ( 'WB', NX*NY, ipwb, ierr )
            call gtwrkr ( 'WC', nxb*nyb, ipwc, ierr )
            call imsm_medianr ( %val(IPIM), %val(ipwb), %val(ipwc),
     +                          nxb, nyb )
            call amovr ( %val(ipwb), %val(ipo), NX*NY )
         endif

      endif

      call icopdes ( 'IN', 'OUT', ierr )
      call ptdesc ( 'OUT', 'TITLE', title )


      end




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMSM_GAUSSS -- Smooth a int*2 flagged array with gaussian
C  Take account of invalid pixels.
C
C  alan penny                  ral                  1994-05-26

      subroutine imsm_gausss ( in, nx, ny, inval, rad,
     +                         ia, nxa, nya, io, ee, nxb, nyb )

      implicit none
      include 'STARMAN_INC'

      integer   nx                      !i: X size of input array
      integer   ny                      !i: Y size of input array
      integer*2 in(nx,ny)              !i: Input array
      integer   inval                   !i: Invalid pixel magic value flag
      real      rad			!i: Gaussian radius
      integer   nxa                     !i: X size of work array
      integer   nya                     !i: Y size of work array
      integer*2 ia(nxa,nya)             !o: Work array
      integer*2 io(nx,ny)               !o: Output array
      integer   nxb                     !i: X size of area to smooth
      integer   nyb                     !i: Y size of area to smooth
      real      ee(nxb,nyb)             !o: Smmoth work array
C--
      integer j, ja, jb, jx, jy, jdx, k, ka, kb, kdy, iv
      real ex, ey, esum, rv
Cbegin


      if ( ST_FAILED ) return

      call azeros ( ia, nxa*nya )
      call azeros ( io, nx*ny )

      jb = nxb/2
      kb = nyb/2

      do k = 1, ny
         do j = 1, nx
            ja = j + jb
            ka = k + kb
            ia(ja,ka) = in(j,k)
         enddo
      enddo

      do k = 1, nya
         do j = 1, nxa
            if ( j.le.jb .or. j.gt.(nxa-jb) .or.
     +           k.le.kb .or. k.gt.(nya-kb) ) then
               ja = j - jb
               if ( j.le.jb ) then
                  ja = 1
               elseif ( j.gt.(nxa-jb) ) then
                  ja = nx
               endif
               ka = k - kb
               if ( k.le.kb ) then
                  ka = 1
               elseif ( k.gt.(nya-kb) ) then
                  ka = ny
               endif
               ia(j,k) = in(ja,ka)
            endif
         enddo
      enddo


      esum = 0.0
      do k = 1, nyb
         kdy = k - kb
         ey = 1.0
         if ( kdy.ne.0 ) ey = exp(-1.0*((real(kdy)/rad)**2.0))
         do j = 1, nxb
            jdx = j - jb
            ex = 1.0
            if ( jdx.ne.0 ) ex = exp(-1.0*((real(jdx)/rad)**2.0))
            ee(j,k) = ex*ey
            esum = esum + ee(j,k)
         enddo
      enddo
      rv = 1.0/esum
      do k = 1, nyb
         do j = 1, nxb
            ee(j,k) = rv*ee(j,k)
         enddo
      enddo

      do k = 1, nya
         do j = 1, nxa

            iv = ia(j,k)
            if (iv.eq.inval) call imsm_setvs(ia,nxa,nya,inval,j,k,iv)
            do ka = 1, nyb
               jy = k - kb + ka - kb
               if ( jy.ge.1 .and. jy.le.ny ) then
                  kdy = ka - kb
                  do ja = 1, nxb
                     jx = j - jb + ja - jb
                     if ( jx.ge.1 .and. jx.le.nx ) io(jx,jy) =
     +                                    io(jx,jy) + iv*ee(ja,ka)
                  enddo
               endif
            enddo

         enddo
      enddo

      do k = 1, ny
         do j = 1, nx
            if ( ia(j,k).eq.inval ) io(j,k) = inval
         enddo
      enddo


      end




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMSM_GAUSSR -- Smooth a real flagged array with gaussian
C  Take account of invalid pixels.
C
C  alan penny                  ral                  1994-05-26

      subroutine imsm_gaussr ( rin, nx, ny, rinval, rad,
     +                         ria, nxa, nya, rio, ee, nxb, nyb )

      implicit none
      include 'STARMAN_INC'

      integer   nx                      !i: X size of input array
      integer   ny                      !i: Y size of input array
      real      rin(nx,ny)              !i: Input array
      real      rinval                  !i: Invalid pixel magic value flag
      real      rad			!i: Gaussian radius
      integer   nxa                     !i: X size of work array
      integer   nya                     !i: Y size of work array
      real      ria(nxa,nya)            !o: Work array
      real      rio(nx,ny)              !o: Output array
      integer   nxb                     !i: X size of area to smooth
      integer   nyb                     !i: Y size of area to smooth
      real      ee(nxb,nyb)             !o: Smmoth work array
C--
      integer j, ja, jb, jx, jy, jdx, k, ka, kb, kdy
      real ex, ey, esum, rv
Cbegin


      if ( ST_FAILED ) return

      call azeror ( ria, nxa*nya )
      call azeror ( rio, nx*ny )

      jb = nxb/2
      kb = nyb/2

      do k = 1, ny
         do j = 1, nx
            ja = j + jb
            ka = k + kb
            ria(ja,ka) = rin(j,k)
         enddo
      enddo

      do k = 1, nya
         do j = 1, nxa
            if ( j.le.jb .or. j.gt.(nxa-jb) .or.
     +           k.le.kb .or. k.gt.(nya-kb) ) then
               ja = j - jb
               if ( j.le.jb ) then
                  ja = 1
               elseif ( j.gt.(nxa-jb) ) then
                  ja = nx
               endif
               ka = k - kb
               if ( k.le.kb ) then
                  ka = 1
               elseif ( k.gt.(nya-kb) ) then
                  ka = ny
               endif
               ria(j,k) = rin(ja,ka)
            endif
         enddo
      enddo


      esum = 0.0
      do k = 1, nyb
         kdy = k - kb
         ey = 1.0
         if ( kdy.ne.0 ) ey = exp(-1.0*((real(kdy)/rad)**2.0))
         do j = 1, nxb
            jdx = j - jb
            ex = 1.0
            if ( jdx.ne.0 ) ex = exp(-1.0*((real(jdx)/rad)**2.0))
            ee(j,k) = ex*ey
            esum = esum + ee(j,k)
         enddo
      enddo
      rv = 1.0/esum
      do k = 1, nyb
         do j = 1, nxb
            ee(j,k) = rv*ee(j,k)
         enddo
      enddo

      do k = 1, nya
         do j = 1, nxa

            rv = ria(j,k)
            if(rv.eq.rinval)call imsm_setvr(ria,nxa,nya,rinval,j,k,rv)
            do ka = 1, nyb
               jy = k - kb + ka - kb
               if ( jy.ge.1 .and. jy.le.ny ) then
                  kdy = ka - kb
                  do ja = 1, nxb
                     jx = j - jb + ja - jb
                     if ( jx.ge.1 .and. jx.le.nx ) rio(jx,jy) =
     +                                    rio(jx,jy) + rv*ee(ja,ka)
                  enddo
               endif
            enddo

         enddo
      enddo

      do k = 1, ny
         do j = 1, nx
            if ( ria(j,k).eq.rinval ) rio(j,k) = rinval
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMSM_SETVS -- Get a replacement int*2 value for an INVALID pixel
C  Take account of invalid pixels.
C
C  alan penny                  ral                  1994-05-26

      subroutine imsm_setvs ( ia, nx, ny, inval, ja, ka, iv )

      implicit none

      integer   nx                      !i: X size of array
      integer   ny                      !i: Y size of array
      integer*2 ia(nx,ny)               !i: Input array
      integer   inval                   !i: Invalid pixel magic value flag
      integer   ja			!i: X Position
      integer   ka			!i: Y Position
      integer   iv			!o: replacement value
C--
      integer j, k, kn, kna, nsum
      real sum
      logical found, some
Cbegin


      found = .false.
      some = .true.

      kn = 1
      do while ( .not.found .and. some )
         kn = kn + 2
         kna = kn/2
         some = .false.
         sum = 0.0
         nsum = 0
         do k = ka-kna, ka+kna
            if ( k.ge.1 .and. k.le.ny ) then
               do j = ja-kna, ja+kna
                  if ( j.ge.1 .and. j.le.nx ) then
                     some = .true.
                     if ( ia(j,k).ne.inval ) then
                        found = .true.
                        sum = sum + ia(j,k)
                        nsum = nsum + 1
                     endif
                  endif
               enddo
            endif
         enddo

         if ( .not.some ) then
            iv = 0
         elseif ( found ) then
            iv = sum/real(nsum)
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMSM_SETVR -- Get a replacement real value for an INVALID pixel
C  Take account of invalid pixels.
C
C  alan penny                  ral                  1994-05-26

      subroutine imsm_setvr ( ra, nx, ny, rinval, ja, ka, rv )

      implicit none

      integer   nx                      !i: X size of array
      integer   ny                      !i: Y size of array
      real      ra(nx,ny)               !i: Input array
      real      rinval                  !i: Invalid pixel magic value flag
      integer   ja			!i: X Position
      integer   ka			!i: Y Position
      real      rv			!o: replacement value
C--
      integer j, k, kn, kna, nsum
      real sum
      logical found, some
Cbegin


      found = .false.
      some = .true.

      kn = 1
      do while ( .not.found .and. some )
         kn = kn + 2
         kna = kn/2
         some = .false.
         sum = 0.0
         nsum = 0
         do k = ka-kna, ka+kna
            if ( k.ge.1 .and. k.le.ny ) then
               do j = ja-kna, ja+kna
                  if ( j.ge.1 .and. j.le.nx ) then
                     some = .true.
                     if ( ra(j,k).ne.rinval ) then
                        found = .true.
                        sum = sum + ra(j,k)
                        nsum = nsum + 1
                     endif
                  endif
               enddo
            endif
         enddo

         if ( .not.some ) then
            rv = 0.0
         elseif ( found ) then
            rv = sum/real(nsum)
         endif

      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMSM_MEDIANR -- Smooth a real flagged array with boxed median
C  Take account of invalid pixels.
C
C  alan penny                  ral                  1994-05-26

      subroutine imsm_medianr ( in, out, box, nxb, nyb )
      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      real      in(NX,NY)              !i: Input array
      real      out(NX,NY)             !o: Output array
      integer   nxb                    !i: X size of box
      integer   nyb                    !i: Y size of box
      real      box(nxb,nyb)           !o: work array
C--
      integer kx(2), ky(2), ierr, kxl, kyl, j, k
      real rm
Cbegin



      if ( ST_FAILED ) return

      kxl = (nxb-1)/2
      kyl = (nyb-1)/2
      do k = 1, NY
         ky(1) = k - kyl
         ky(2) = k + kyl
         ky(1) = min(NY,max(1,ky(1)))
         ky(2) = min(NY,max(1,ky(2)))
         do j = 1, NX
            kx(1) = j - kxl
            kx(2) = j + kxl
            kx(1) = min(NX,max(1,kx(1)))
            kx(2) = min(NX,max(1,kx(2)))
            call medianr ( in, NX, NY, box, kx, ky, RINVAL,
     +                     rm, ierr )
            out(j,k) = rm
         enddo
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMSM_MEDIANS -- Smooth an int*2 flagged array with boxed median
C  Take account of invalid pixels.
C
C  alan penny                  ral                  1994-05-26

      subroutine imsm_medians ( in, out, box, nxb, nyb )
      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      integer*2 in(NX,NY)              !i: Input array
      integer*2 out(NX,NY)             !o: Output array
      integer   nxb                    !i: X size of box
      integer   nyb                    !i: Y size of box
      integer   box(nxb,nyb)           !o: work array
C--
      integer kx(2), ky(2), ierr, kxl, kyl, j, k
      real rm
Cbegin


      if ( ST_FAILED ) return

      kxl = (nxb-1)/2
      kyl = (nyb-1)/2
      do k = 1, NY
         ky(1) = k - kyl
         ky(2) = k + kyl
         ky(1) = min(NY,max(1,ky(1)))
         ky(2) = min(NY,max(1,ky(2)))
         do j = 1, NX
            kx(1) = j - kxl
            kx(2) = j + kxl
            kx(1) = min(NX,max(1,kx(1)))
            kx(2) = min(NX,max(1,kx(2)))
            call medians ( in, NX, NY, box, kx, ky, INVAL,
     +                     rm, ierr )
            out(j,k) = min(32767.0,max(-32768.0,rm))
         enddo
      enddo


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  T_IMSTAT.F
C
C    Contains:-
C
C T_IMSTAT       Calc statistics of an area of an image
C IMS_ASTAT      Area statistics
C IMS_SUM(RS)    Get sum of flagged area of array
C IMS_MINMAX(RS) Get max and min of flagged area of array


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMSTAT   Calc statistics of an area of an image
C   For a fuller description see IMSTAT.HLP
C
C   alan penny                     ral           1991 May

      subroutine t_imstat ( )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer nxs, nxe, nys, nye, ierr
Cbegin


      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      if ( ST_FAILED ) return

      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE, ierr )

      nxs = 1
      nxe = NX
      call get2i ( 'XRANGE', nxs, nxe, .true., 1, NX )
      if ( ST_FAILED ) return
      call cswopi ( nxs, nxe )

      nys = 1
      nye = NY
      call get2i ( 'YRANGE', nys, nye, .true., 1, NY )
      if ( ST_FAILED ) return
      call cswopi ( nys, nye )

      call ims_astat ( nxs, nxe, nys, nye )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMS_ASTAT -- Area statistics
C
C  alan penny                ral                      1990-06-15

      subroutine ims_astat ( nxs, nxe, nys, nye )

      implicit none
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'

      integer   nxs		!i: X start of area
      integer   nxe		!i: X   end of area
      integer   nys		!i: Y start of area
      integer   nye		!i: Y   end of area
C--
      integer kx(2), ky(2), ngood, nbad, ierr, kp(4), iv
      real am, std, amax, amin, sum, rv
Cbegin


      if ( ST_FAILED ) return

      kx(1) = nxs
      kx(2) = nxe
      ky(1) = nys
      ky(2) = nye
      if ( IMTYPE.eq.'SHORT' ) then
         call ranges ( %val(IPIM), NX, NY, kx, ky, INVAL, am, std, ierr)
      else
         call ranger ( %val(IPIM), NX, NY, kx, ky, RINVAL, am, std,ierr)
      endif
      am = BS*am + BZ
      std = BS*std

      if ( IMTYPE.eq.'SHORT' ) then
         call ims_sums ( %val(IPIM), NX, NY, INVAL, kx, ky, sum )
      else
         call ims_sumr ( %val(IPIM), NX, NY, RINVAL, kx, ky, sum )
      endif

      if ( IMTYPE.eq.'SHORT' ) then
         call ims_minmaxs ( %val(IPIM), NX, NY, INVAL, nxs, nxe,
     +                      nys, nye, amin, amax, kp, ngood, nbad )
      else
         call ims_minmaxr ( %val(IPIM), NX, NY, RINVAL, nxs, nxe,
     +                      nys, nye, amin, amax, kp, ngood, nbad )
      endif

      sum = BS*sum + real(ngood)*BZ

      amin = BS*amin + BZ
      amax = BS*amax + BZ
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

      call pargc ( IMTYPE )
      call printd ( ' Image type is: %c' )
      call pargi ( nxs )
      call pargi ( nxe )
      call pargi ( nys )
      call pargi ( nye )
      call printd ( '       Area is: X = %d - %d ; Y = %d - %d ' )
      call pargr ( am )
      call pargr ( std )
      call printd ( ' Mean = %f ; Std Dev = %f  ' )
      call pargr ( amin )
      call pargi ( kp(1) )
      call pargi ( kp(2) )
      call pargr ( amax )
      call pargi ( kp(3) )
      call pargi ( kp(4) )
      call printd ( ' Min = %f at ( %d ,%d ) ; Max = %f at ( %d ,%d )' )

      call pargi ( ngood )
      call pargi ( nbad )
      call printd ( ' Number of good pixels = %d ; '
     +              //'of invalid pixels = %d' )

      call pargr ( sum )
      call printd ( ' Sum of good pixels = %f')

      call put1r ( 'MEAN', am )
      call put1r ( 'MAX', amax )
      call put1i ( 'XMAXLOC', kp(1) )
      call put1i ( 'YMAXLOC', kp(2) )
      call put1r ( 'MIN', amin )
      call put1i ( 'XMINLOC', kp(3) )
      call put1i ( 'YMINLOC', kp(4) )
      call put1r ( 'STDDEV', std )
      call put1i ( 'NGOOD', ngood )
      call put1i ( 'NBAD', nbad )
      call put1r ( 'SUM', sum )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMS_SUMR -- Get sum of flagged area of real array
C
C  alan penny                ral                      1990-06-15

      subroutine ims_sumr ( im, nx, ny, rinval, kx, ky, sum )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: Image X size
      integer	ny		!i: Image Y size
      real      im(nx,ny)	!i: Image
      real      rinval		!i: Image pixel value invalid flag
      integer	kx(2)		!i: Area X start/end
      integer	ky(2)		!i: Area Y start/end
      real      sum		!o: Sum
C--
      integer j, k
      double precision dv
Cbegin


      if ( ST_FAILED ) return

      dv = 0.0d0
      do k = ky(1), ky(2)
         do j = kx(1), kx(2)
            if ( im(j,k).ne.rinval ) dv = dv + dble(im(j,k))
         enddo
      enddo
      sum = real(dv)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMS_SUMS -- Get sum of flagged area of short array
C
C  alan penny                ral                      1990-06-15

      subroutine ims_sums ( im, nx, ny, inval, kx, ky, sum )

      implicit none
      include 'STARMAN_INC'

      integer	nx		!i: Image X size
      integer	ny		!i: Image Y size
      integer*2 im(nx,ny)	!i: Image
      integer	inval		!i: Image pixel value invalid flag
      integer	kx(2)		!i: Area X start/end
      integer	ky(2)		!i: Area Y start/end
      real      sum		!o: Sum
C--
      integer j, k
      double precision dv
Cbegin


      if ( ST_FAILED ) return

      dv = 0.0d0
      do k = ky(1), ky(2)
         do j = kx(1), kx(2)
            if ( im(j,k).ne.inval ) dv = dv + dble(im(j,k))
         enddo
      enddo
      sum = real(dv)


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMS_MINMAXS -- Get max and min of flagged area of short array
C
C  alan penny                ral                      1990-06-15

      subroutine ims_minmaxs ( im, nx, ny, inval, nxs, nxe,
     +                         nys, nye, amin, amax, kp, ngood, nbad )

      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'

      integer	nx		!i: Image X size
      integer	ny		!i: Image Y size
      integer*2 im(nx,ny)	!i: Image
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
      integer j, k, iv
      real    rv
      double precision dgood, dbad
Cbegin


      if ( ST_FAILED ) return

      amax = -1.0e20
      amin = 1.0e20
      call azeroi ( kp, 4 )
      dgood = 0.0d0
      dbad = 0.0d0

      do k = nys, nye
         do j = nxs, nxe
            iv = im(j,k)
            if ( iv.ne.inval ) then
               dgood = dgood + 1.0d0
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
               dbad = dbad + 1.0d0
            endif
         enddo
      enddo

      ngood = min(real(dgood),int_maxir)
      nbad  = min(real(dbad),int_maxir)

      amax = amax
      amin = amin
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
C IMS_MINMAXR -- Get max and min of flagged area of real array
C
C  alan penny                ral                      1990-06-15

      subroutine ims_minmaxr ( im, nx, ny, rinval, nxs, nxe,
     +                         nys, nye, amin, amax, kp, ngood, nbad )

      implicit none
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'

      integer	nx		!i: Image X size
      integer	ny		!i: Image Y size
      real      im(nx,ny)	!i: Image
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
      integer j, k, iv
      real    rv
      double precision dbad, dgood
Cbegin


      if ( ST_FAILED ) return

      amax = -1.0e20
      amin = 1.0e20
      call azeroi ( kp, 4 )
      dgood = 0.0d0
      dbad = 0.0d0

      do k = nys, nye
         do j = nxs, nxe
            rv = im(j,k)
            if ( rv.ne.rinval ) then
               dgood = dgood + 1.0d0
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
               dbad = dbad + 1.0d0
            endif
         enddo
      enddo

      ngood = min(real(dgood),int_maxir)
      nbad  = min(real(dbad),int_maxir)

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

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  T_IMTYPE.F
C
C   Contains:-
C
C T_IMTYPE       Do the IMTYPE program function
C IMT_OPIMS      Open input and output image
C IMT_GCL        Get options
C IMT_DOIT       Do the conversion
C IMT_COPIS      Copy data from integer input to short output
C IMT_COPRI      Copy data from real input to integer output
C IMT_COPRS      Copy data from real input to short output


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMTYPE -- Do the 'IMTYPE' program function
C
C   alan penny               ral                 1991 April

      subroutine t_imtype ()

      implicit none
C--
Cbegin


      call imt_opims			!Open input and output images

      call imt_doit			!Do the conversion


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMT_OPIMS -- Open input and output image
C
C alan penny          ral                   1991 April

      subroutine imt_opims ()

      implicit none
      include 'imtype.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
C--
      integer istat, iv
      character cv*80
Cbegin


      if ( ST_FAILED ) return

      call opimgr ( 'IN', IPIM, NX, NY, IMTYPE, .false., istat )	!Get input image
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      if ( IMTYPE.eq.'REAL' ) then
         call printo ( 'Image Type is: REAL' )
         call gtimrd ( 'IN', BS, BZ, RINVAL, IMTITLE, istat )
      elseif ( IMTYPE.eq.'INT' ) then
         call printo ( 'Image Type is: INTEGER' )
         call gtimid ( 'IN', BS, BZ, INVAL, IMTITLE, istat )
      elseif ( IMTYPE.eq.'SHORT' ) then
         call printo ( 'Image Type is: INTEGER*2' )
         call gtimsd ( 'IN', BS, BZ, INVAL, IMTITLE, istat )
      else
         call printo (
     +   'ERROR: Can only deal with: REAL;INTEGER;SHORT type images' )
         ST_FAILED = .true.
         return
      endif
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call imt_gcl

      if ( RANGE ) call imt_range

      BSO = BS
      BZO = BZ
      if ( AUTO ) call imt_auto

      if ( (IMTYPE.eq.'REAL' .and. OUTTYPE.ne.'REAL') .or.
     +     (IMTYPE.eq.'INT' .and. OUTTYPE.eq.'SHORT') ) then
         call printo ( '  ' )
         call printo ( '  The Output image may need to have a '//
     +                 'different scale -' )
         call printo ( '  as it has a smaller dynamic range.' )
         call printo ('  Data are stored as ((value-BZERO)/BSCALE)')
         call pargr ( BS )
         call pargr ( BZ )
         call printd ( '  The input image BSCALE and BZERO are:- %f '
     +                 //'and %f' )
         call pargr ( BSO )
         call pargr ( BZO )
         call printd ( '  The suggested output image BSCALE and'//
     +                 ' BZERO are:- %f and %f' )
         call get2r ( 'OUTSCALE', BSO, BZO, .true., -1.0e20, 1.0e20 )
         if ( ST_FAILED ) return
      endif

      call get1c ( 'TITLE', cv, IMTITLE, .true. )			!Get output image title
      if ( ST_FAILED ) return

      call opimgw ( 'OUT', OUTTYPE, IPO, NX, NY, .false., istat ) 	!Get output image
      if ( istat.ne.0 ) ST_FAILED = .true.
      if ( ST_FAILED ) return

      call icopdes (  'IN', 'OUT', istat )				!Copy descriptors

      call ptdesr ( 'OUT', 'BSCALE', BSO )
      call ptdesr ( 'OUT', 'BZERO', BZO )

      if ( OUTTYPE.eq.'REAL' ) then
         if ( IMTYPE.eq.'REAL' ) then
            INVALOR = RINVAL
         else
            INVALOR = INT_INVALR
         endif
         call ptdesr ( 'OUT', 'INVAL', INVALOR )
      elseif ( OUTTYPE.eq.'INT' ) then
         if ( IMTYPE.eq.'INT' ) then
            INVALOI = INVAL
         else
            INVALOI = INT_INVALI
         endif
         call ptdesi ( 'OUT', 'INVAL', INVALOI )
       else
         if ( IMTYPE.eq.'SHORT' ) then
            INVALOS = INVAL
            iv = INVAL
         else
            INVALOS = INT_INVALSI
            iv = INT_INVALSI
         endif
         call ptdesi ( 'OUT', 'INVAL', iv )
      endif

      call ptdesc ( 'OUT', 'TITLE', cv )


      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMT_GCL -- Get control parameters
C
C alan penny          ral                   1991 April

      subroutine imt_gcl ()

      implicit none
      include 'imtype.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
C--
      integer kopt, kin
      integer nth
      parameter ( nth=6 )
      character*68 th(nth)
      data th /
     + 'Are the output data ? :- ',
     + 'Option   Choice',
     + '------   --------',
     + 'Integer  Signed 32-bit integer (-2**16 to +2**16 - 1)',
     + 'Real     Real 32-bit numbers',
     + 'Short    Signed 16-bit integer (-32768 to 32767)'/
Cbegin


      if ( ST_FAILED ) return

      kin = 3
      if ( IMTYPE.eq.'REAL' ) kin = 3
      if ( IMTYPE.eq.'INT' ) kin = 2
      if ( IMTYPE.eq.'SHORT') kin = 1

      call get_job ('OUTTYPE', 'real:integer:short', kopt,kin,th,nth) 	!Get type of output image
      if ( ST_FAILED ) return

      OUTTYPE = 'REAL'
      if ( kopt.eq.1 ) OUTTYPE = 'REAL'
      if ( kopt.eq.2 ) OUTTYPE = 'INT'
      if ( kopt.eq.3 ) OUTTYPE = 'SHORT'

      call get1b ( 'RANGE', RANGE, .false. )				!Calc range of input data?
      if ( ST_FAILED ) return

      AUTO = .false.							!Use auto scaling?
      if ( (IMTYPE.eq.'REAL' .and. (OUTTYPE.eq.'INT'.or.
     +                              OUTTYPE.eq.'SHORT')) .or.
     +     (IMTYPE.eq.'INT' .and. OUTTYPE.eq.'SHORT') ) then
         call get1b ( 'AUTO', AUTO, .false. )
         if ( ST_FAILED ) return
         if ( AUTO ) RANGE = .true.
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMT_DOIT -- Copy data from input to output - base
C
C alan penny          ral                   1991 April

      subroutine imt_doit ( )

      implicit none
      include 'imtype.inc'
      include 'ST_IMAGE_INC'
      include 'STARMAN_INC'
      include 'ST_LIMITS_INC'
C--
      integer istat, iptmp
      integer*2 ivs
Cbegin


      if ( ST_FAILED ) return

      if ( ALLBAD ) then
         if ( OUTTYPE.eq.'REAL' )  call amovkr ( INVALOR, %val(IPIM),
     +                                           NX*NY )
         if ( OUTTYPE.eq.'INT' )   call amovki ( INVALOI, %val(IPIM),
     +                                           NX*NY )
         if ( OUTTYPE.eq.'SHORT' ) call amovks ( INVALOS, %val(IPIM),
     +                                           NX*NY )
         return
      endif

      if ( IMTYPE.eq.'REAL' .and. OUTTYPE.eq.'INT' ) then
         call gtwrkr ( 'TEMP', NX, iptmp, istat )
         call imt_copri ( %val(IPIM), %val(IPO), %val(iptmp))
         call wrkcan ( 'TEMP' )
      elseif ( IMTYPE.eq.'REAL' .and. OUTTYPE.eq.'SHORT' ) then
         call gtwrkr ( 'TEMP', NX, iptmp, istat )
         call imt_coprs ( %val(IPIM), %val(IPO), %val(iptmp))
         call wrkcan ( 'TEMP' )
      elseif ( IMTYPE.eq.'INT' .and. OUTTYPE.eq.'SHORT' ) then
         call gtwrkr ( 'TEMP', NX, iptmp, istat )
         call imt_copis ( %val(IPIM), %val(IPO), %val(iptmp) )
         call wrkcan ( 'TEMP' )
      elseif ( IMTYPE.eq.'REAL' .and. OUTTYPE.eq.'REAL' ) then
         call amovr ( %val(IPIM), %val(IPO), NX*NY )
      elseif ( IMTYPE.eq.'INT' .and. OUTTYPE.eq.'REAL' ) then
         call azchtir ( %val(IPIM), INVAL, %val(IPO), INVALOR, NX*NY )
      elseif ( IMTYPE.eq.'INT' .and. OUTTYPE.eq.'INT' ) then
         call amovi ( %val(IPIM), %val(IPO), NX*NY )
      elseif ( IMTYPE.eq.'SHORT' .and. OUTTYPE.eq.'REAL' ) then
         ivs = INVAL
         call azchtsr ( %val(IPIM), ivs, %val(IPO), INVALOR, NX*NY )
      elseif ( IMTYPE.eq.'SHORT' .and. OUTTYPE.eq.'INT' ) then
         ivs = INVAL
         call azchtsi ( %val(IPIM), ivs, %val(IPO), INVALOI, NX*NY )
      elseif ( IMTYPE.eq.'SHORT' .and. OUTTYPE.eq.'SHORT' ) then
         call amovs ( %val(IPIM), %val(IPO), NX*NY )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMT_COPRS -- Copy data from real input to short output
C
C alan penny          ral                   1991 April

      subroutine imt_coprs ( in, out, rv )

      implicit none
      include 'imtype.inc'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'

      real       in(NX,NY)	!i: Input data
      integer*2  out(NX,NY)	!o: Output short integer image
      real       rv(NX)		!o: Work space
C--
      integer j, k
      real rva
Cbegin


      do k = 1, NY
         do j = 1, NX
            rva = in(j,k)
            if ( rva.ne.RINVAL ) then
               rva = rva*BS + BZ
               rv(j) = (rva-BZO)/BSO
            else
               rv(j) = rva
            endif
         enddo
         call azchtrs ( rv, RINVAL, INT_MINSR, INT_MAXSR,
     +                  out(1,k), INVALOS, NX )
      enddo


      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMT_COPRI -- Copy data from real input to integer output
C
C alan penny          ral                   1991 April

      subroutine imt_copri ( in, out, rv )

      implicit none
      include 'imtype.inc'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'

      real       in(NX,NY)	!i: Input data
      integer    out(NX,NY)	!o: Output integer image
      real       rv(NX)		!o: Work space
C--
      integer j, k
      real rva
Cbegin


      do k = 1, NY
         do j = 1, NX
            rva = in(j,k)
            if ( rva.ne.RINVAL ) then
               rva = rva*BS + BZ
               rv(j) = (rva-BZO)/BSO
            else
               rv(j) = rva
            endif
         enddo
         call azchtri ( rv, RINVAL, INT_MINIR, INT_MAXIR,
     +                  out(1,k), INVALOI, NX )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMT_COPIS -- Copy data from integer input to short output
C
C alan penny          ral                   1991 April

      subroutine imt_copis ( in, out, rv )

      implicit none
      include 'imtype.inc'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'

      integer    in(NX,NY)	!i: Input data
      integer*2  out(NX,NY)	!o: Output short integer image
      real       rv(NX)		!o: Work space
C--
      integer j, k
      real rva
Cbegin


      do k = 1, NY
         do j = 1, NX
            if ( in(j,k).eq.INVAL ) then
               rv(j) = INT_INVALR
            else
               rva = real(in(j,k))*BS + BZ
               rv(j) = (rva-BZO)/BSO
            endif
         enddo
         call azchtrs ( rv, INT_INVALR, INT_MINSR, INT_MAXSR,
     +                  out(1,k), INVALOS, NX )
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMT_RANGE -- Get range of data
C
C alan penny          ral                   1991 April

      subroutine imt_range ( )

      implicit none
      include 'imtype.inc'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'

C--
      real rmin, rmax
      integer imin, imax, ierr
      integer*2 ismin, ismax, ivs
Cbegin


      rmin = 0.0
      rmax = 0.0
      imin = 0
      imax = 0
      ismin = 0
      ismax = 0

      if ( IMTYPE.eq.'REAL' ) then
         call azlimr ( %val(IPIM), NX*NY, RINVAL, rmin, rmax, ierr )
         rmin = BS*rmin + BZ
         rmax = BS*rmax + BZ
      elseif ( IMTYPE.eq.'INT' ) then
         call azlimi ( %val(IPIM), NX*NY, INVAL, imin, imax, ierr )
         rmin = BS*real(imin) + BZ
         rmax = BS*real(imax) + BZ
      elseif ( IMTYPE.eq.'SHORT' ) then
         ivs = INVAL
         call azlims ( %val(IPIM), NX*NY, ivs, ismin, ismax,ierr)
         rmin = BS*real(ismin) + BZ
         rmax = BS*real(ismax) + BZ
      endif

      if ( ierr.eq.0 ) then
         ALLBAD = .false.
         RMINI = rmin
         RMAXI = rmax
         call pargr ( rmin )
         call pargr ( rmax )
         call printd ( 'Input image - Minimum = :  %f  Maximum =  %f ' )
      else
         ALLBAD = .true.
         call printo ( 'WARNING: All pixels in image are INVALID' )
      endif


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMT_AUTO -- Suggest output scaling
C
C alan penny          ral                   1991 April

      subroutine imt_auto ( )

      implicit none
      include 'imtype.inc'
      include 'ST_IMAGE_INC'
      include 'ST_LIMITS_INC'

C--
      double precision dva, dvb, dvc, dvd
      real  outmin, outmax
Cbegin


      if ( ALLBAD ) return

      if ( OUTTYPE.eq.'REAL' ) then
         if ( RMAXI.ge.INT_MAXRR .or. RMINI.le.INT_MINRR ) then
            dva = RMAXI
            dvb = RMINI
            dvc = INT_MAXRR
            dvd = INT_MINRR
            if ( (dva-dvb).ge.0.96d0*(dvc-dvd) ) then
               outmin = dvd + 0.01d0*(dvc-dvd)
               outmax = outmin + real(0.96d0*(dvc-dvd))
               BSO = (outmax-outmin)/(RMAXI-RMINI)
               BZO = outmax - BSO*RMAXI
            else
               BSO = 1.0
               BZO = INT_MINRR + 0.01*(RMAXI-RMINI)
            endif
         endif
      elseif( OUTTYPE.eq.'INT' ) then
         if ( RMAXI.ge.INT_MAXIR .or. RMINI.le.INT_MINIR ) then
            dva = RMAXI
            dvb = RMINI
            dvc = INT_MAXII
            dvd = INT_MINII
            if ( (dva-dvb).ge.0.96d0*(dvc-dvd) ) then
               outmin = dvd + 0.01d0*(dvc-dvd)
               outmax = outmin + real(0.96d0*(dvc-dvd))
               BSO = (outmax-outmin)/(RMAXI-RMINI)
               BZO = outmax - BSO*RMAXI
            else
               BSO = 1.0
               BZO = INT_MINIR + 0.01*(RMAXI-RMINI)
            endif
         endif
      elseif( OUTTYPE.eq.'SHORT' ) then
         if ( RMAXI.ge.INT_MAXSR .or. RMINI.le.INT_MINSR ) then
            dva = RMAXI
            dvb = RMINI
            dvc = INT_MAXSS
            dvd = INT_MINSS
            if ( (dva-dvb).ge.0.96d0*(dvc-dvd) ) then
               outmin = dvd + 0.01d0*(dvc-dvd)
               outmax = outmin + real(0.96d0*(dvc-dvd))
               BSO = (outmax-outmin)/(RMAXI-RMINI)
               BZO = outmax - BSO*RMAXI
            else
               BSO = 1.0
               BZO = INT_MINSR + 0.01*(RMAXI-RMINI)
            endif
         endif
      endif


      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  T_IMWEED.F
C
C    Contains:-
C
C T_IMWEED       Set a defined range of pixel value to INVALID
C IMWE_GCL       Get command line
C IMWE_DOIT(RS)  Do the weeding


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C T_IMWEED -- Set a defined range of pixel valueto INVALID
C
C   alan penny                     ral           1991 May

      subroutine t_imweed ( )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'
C--
      integer   ierr, ipo
      real      bot, top
      character title*50
Cbegin


      call imwe_gcl ( ipo, bot, top, title, ierr )
      if ( ST_FAILED .or. ierr.ne.0 ) return

      if ( IMTYPE.eq.'SHORT' ) then
         call imwe_doits ( %val(IPIM), %val(ipo), bot, top )
      else
         call imwe_doitr ( %val(IPIM), %val(ipo), bot, top )
      endif

      call icopdes ( 'IN', 'OUT', ierr )
      if ( ierr.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      call ptdesc ( 'OUT', 'TITLE', title )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMWE_DOITR -- Weed real image
C
C   alan penny                     ral           1995 Jan

      subroutine imwe_gcl ( ipo, bot, top, title, ierr )

      implicit none
      include 'STARMAN_INC'
      include 'ST_IMAGE_INC'

      integer       ipo		!o: Pointer to output array
      real          bot		!o: Lower limit to INVALID range
      real          top		!o: Lower limit to INVALID range
      character*50  title	!o: Title for output image
      integer       ierr	!o: Error flag (0=ok;1=bad)

C--
      integer istat
      real    rv
Cbegin


      ierr = 0

      call opimzr ( 'IN', IPIM, NX, NY, IMTYPE, .false., istat )
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif
      call gtimzd ( 'IN', IMTYPE, BS, BZ, INVAL, RINVAL, IMTITLE, istat)

      bot = 0.0
      top = 0.0
      call get2r ( 'RANGE', bot, top, .true., -1.0e20, 1.0e20 )
      if ( bot.gt.top ) then
         rv = top
         top = bot
         bot = rv
      endif

      if ( IMTYPE.eq.'SHORT' ) then
         rv = -32768.0*BS + BZ
         if ( top.lt.rv ) then
            call pargr ( rv )
            call printo (
     +      'WARNING: Upper limit below minimum possible value for' )
            call printd ('         this 16-bit integer image of - %f')
            call printo ( '         No pixels will need weeding' )
         endif
         rv = 32767.0*BS + BZ
         if ( bot.gt.rv ) then
            call printo (
     +      'WARNING: Lower limit above maximum possible value for' )
            call printd ('         this 16-bit integer image of - %f')
            call printo ('         No pixels will need weeding' )
         endif
      endif
      if ( ierr.ne.0 ) return

      call opimzw ( 'OUT', IMTYPE, ipo, NX, NY, .false., istat )
      if ( istat.ne.0 ) then
         ST_FAILED = .true.
         return
      endif

      if ( IMTITLE.eq.' ' ) IMTITLE = 'Output from IMWEED'
      call get1c ( 'TITLE', title, IMTITLE, .true. )


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMWE_DOITR -- Weed real image
C
C   alan penny                     ral           1995 Jan

      subroutine imwe_doitr ( in, out, bot, top )

      implicit none
      include 'ST_IMAGE_INC'

      real        in(NX*NY)	!i: Input image
      real        out(NX*NY)	!o: Output image
      real        bot		!i: Lower limit of invalid range
      real        top		!i: Upper limit of invalid range
C--
      integer j
      real rv
Cbegin


      bot = (bot-BZ)/BS
      top = (top-BZ)/BS

      do j = 1, NX*NY
         rv = in(j)
         if ( rv.eq.RINVAL ) then
            out(j) = RINVAL
         elseif ( rv.ge.bot .and. rv.le.top ) then
            out(j) = RINVAL
         else
            out(j) = rv
         endif
      enddo


      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMWE_DOITS -- Weed short image
C
C   alan penny                     ral           1995 Jan

      subroutine imwe_doits ( in, out, bot, top )

      implicit none
      include 'ST_IMAGE_INC'

      integer*2   in(NX*NY)	!i: Input image
      integer*2   out(NX*NY)	!o: Output image
      real        bot		!i: Lower limit of invalid range
      real        top		!i: Upper limit of invalid range
C--
      integer j, iv, kbot, ktop
      real    rv
Cbegin


      rv = (bot-BZ)/BS
      kbot = min(max(rv,-32768.0),32767.0)
      rv = (top-BZ)/BS
      ktop = min(max(rv,-32768.0),32767.0)

      do j = 1, NX*NY
         iv = in(j)
         if ( iv.eq.INVAL ) then
            out(j) = INVAL
         elseif ( iv.ge.kbot .and. iv.le.ktop ) then
            out(j) = INVAL
         else
            out(j) = iv
         endif
      enddo


      end
