CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TBCALC.INC     for TBCALC.F
C

      character*70  exprs(20)		!Input Equations
      character*6   opcode(70,20)	!(i)th operation in stack of (j)th equation
      character*2   tbid(70,20)		!Identifiers of input tables
      character*1   varid(70,20)	!Identifiers of input variables
      character*2   tbtot(26)		!List of all unique table names
      character*1   vartot(26)		!List of all unique variable names

      integer       ip(26)		!Pointer to input tables
      integer       ipo			!Pointer to output table
      integer       tbvx(26)		!X size (inc 5 for names) of input tables
      integer       tby			!Y size of input tbales
      integer       tbxm		!Max X size of input tables
      integer       tbz			!Number of different input tables
      integer       tbvxo		!X size (inc 5 for names) of output table
      integer       tbxo		!X size of output table
      integer       numeqn		!Number of equations to process
      integer       imp(70,20)		!Number of Table to use for (i)th op in stack of (j) equation
      integer       impc(70,20)		!Number of Column to use for (i)th op in stack of (j) equation
      integer       impv(70,20)		!Number of Variable to use for (i)th op in stack of (j) equation
      integer       ntb(20)		!Number of input tables mentioned in (j)th eqn
      integer       nvar(20)		!Number of input variables mentioned in (j)th eqn
      integer       ncon(20)		!Number of constants mentioned in (j)th eqn
      integer       nopcode(20)		!Number of operations in (j)th stack
      integer       ncol(20)		!Column in output table to put results of (j)th eqn
      integer       template		!No of table to use as template (0=dont use one)
      integer       nvartot		!Total number of input variables
      integer       kseed		!Random number generator seed

      real          var(26)		!Input variables
      real          con(70,20)		!Constant to use for (i)th op in stack of (j) equation

      logical       clear		!Clear output table before load columns

      logical    tbca_dum1, tbca_dum2, tbca_dum3	!$*n logicals


      common / tbcalca /  nvartot,   tbvx,   tby,    tbz,   imp,     
     +                    nopcode,    con,  tbxm,  tbvxo,  ncon,
     +                   template,    ipo,  tbxo,    ntb,  nvar,
     +                     numeqn,   impc,   var,     ip,  ncol,
     +                      kseed,   impv

      common /tbcalcb / clear,  tbca_dum1,  tbca_dum2, tbca_dum3

      common / tbcalcc / exprs,  opcode,  tbid,  varid,  tbtot,
     +                   vartot

