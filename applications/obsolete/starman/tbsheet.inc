CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBSHEET.INC for TBSHEET.F
C

      integer    ncwords	!Number of command words
      parameter ( ncwords=35 )

      integer    tbxm		!Max X size of work table
      integer    tbym		!Max Y size of work table
      integer    tbx		!X size of current section of work table (inc holes)
      integer    tby		!Y size of current section of work table (inc holes)
      integer    tbxr		!X size of current section of work table (no holes)
      integer    tbyr		!Y size of current section of work table (no holes)

      integer    ipd		!Pointer to data    array
      integer    iph		!Pointer to headers array
      integer    ipn		!Pointer to names   array
      integer    ipx		!Pointer to X use   array (0=no;1=yes)
      integer    ipy		!Pointer to Y use   array (0=no;1=yes)
      integer    ipxa		!Pointer to X use back array
      integer    ipya		!Pointer to Y use back array

      real       snx		!X screen size
      real       sny		!Y screen size
      integer    numx		!Number of columns
      integer    numy		!Number of rows
      integer    nxcw		!X size of command word array
      integer    nycw		!Y size of command word array
      real       xbox		!Width of columns
      real       ybox		!Height of rows
      real       curx		!X position of cursor
      real       cury		!Y position of cursor
      real       x_old		!Last X position of cursor
      real       y_old		!Last Y position of cursor
      integer    kxstart	!No ('real') of column at left of screen
      integer    kystart	!No ('real') of row at top of screen
      real       chysize	!Character height size
      real       chxsize	!Character width size
      integer    chnames	!No of characters in names
      real       xdoff		!Screen X start of sheet data
      integer    kncol		!No of column found for 'change'
      integer    knrow		!No of row found for 'change'

      integer    kcol(10)	!Writing colours

      character*50 title	!Table title

      logical    changed        !Has the sheet been changed since last
				! store?
      logical    restore	!Can the last delete be restored?
      logical    gotin          !Opened an input file?
      logical    useda          !made temporay version of present table

CX      logical    tbs_dum1	!4*n logicals for common


      common / sheeta /   tbx,      tby,   tbxm,     tbym,     nycw,
     +                    snx,      sny,   numx,     numy,     xbox, 
     +                   ybox,     curx,   cury,  chxsize,  kxstart, 
     +                kystart,  chnames,  xdoff,  chysize,     nxcw,
     +                    ipd,      iph,    ipn,      ipx,      ipy,
     +                   ipxa,     ipya,   tbxr,     tbyr,     kcol,
     +                  kncol,    knrow,  x_old,    y_old


      common / sheetb / changed,  restore, gotin, useda

      common / sheetc / title


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C (those bits for the calculator)


      character*130 exprs		!Input Equations
      character*6   opcode(70)		!(i)th operation in stack of equation
      character*2   tbid(70)		!Identifiers of input tables
      character*1   varid(70)		!Identifiers of input variables
      character*2   tbtot(26)		!List of all unique table names
      character*1   vartot(26)		!List of all unique variable names

      integer       ipca(26)		!Pointer to input tables
      integer       tbvx(26)		!X size (inc 5 for names) of input tables
      integer       tbya		!Y size of input tbales
      integer       tbxma		!Max X size of input tables
      integer       tbz			!Number of different input tables
      integer       imp(70)		!Number of Table to use for (i)th op in stack of equation
      integer       impc(70)		!Number of Column to use for (i)th op in stack of equation
      integer       impv(70)		!Number of Variable to use for (i)th op in stack of equation
      integer       ntb			!Number of input tables mentioned in eqn
      integer       nvar		!Number of input variables mentioned in eqn
      integer       ncon		!Number of constants mentioned in eqn
      integer       nopcode		!Number of operations in stack
      integer       nvartot		!Total number of input variables
      integer       kseed		!Random number generator seed

      real          var(26)		!Input variables
      real          con(70)		!Constant to use for (i)th op in stack of equation



      common / tbscalca /  nvartot,   tbvx,   tbya,     tbz,   imp,
     +                     nopcode,    con,   tbxma,   ncon,  impv,
     +                         ntb,   nvar,   impc,     var,  ipca,  
     +                       kseed

      common / tbscalcc / exprs,  opcode,  tbid,  varid,  tbtot,
     +                    vartot

