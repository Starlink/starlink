CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ALTER_CALC.INC     for ALTER_CALC.F
C

      character*132 exprs		!Input Equation
      character*6   opcode(70)		!(i)th operation in stack 
      character*1   varid(70)		!Identifiers of input variables
      character*2   imid(26)		!Identifiers of input images
      character*6   imty(26)		!Types (SHORT,REAL) of input images
      character*2   imtot(26)		!List of all unique image names
      character*1   vartot(26)		!List of all unique variable names
      character*6   outtype		!Output image type (REAL,SHORT)

      integer       ipc(26)		!Pointer to input images
      integer       ipco		!Pointer to output image
      integer       nxc(26)		!X size of input images
      integer       nyc(26)		!Y size of input images
      integer       nxim		!X size of input work image
      integer       nyim		!Y size of input work image
      integer       nz			!Number of different input images
      integer       nxo			!X size of output image
      integer       nyo			!Y size of output image
      integer       imp(70)		!Number of image to use for (i)th op in stack 
      integer       impv(70)		!Number of Variable to use for (i)th op in stack 
      integer       nim			!Number of input images mentioned 
      integer       nvar		!Number of input variables mentioned 
      integer       ncon		!Number of constants mentioned 
      integer       nopcode		!Number of operations in stack
      integer       nvartot		!Total number of input variables
      integer       kseed		!Random number generator seed
      integer       invali(26)		!Invalid flags for int*2 input images
      real          rinvali(26)		!Invalid flags for real input images
      integer       invalo		!Invalid flag for int*2 output image
      real          rinvalo		!Invalid flag for real output image
      real          rinvalim		!Invalid flag for real input work image
      integer       ipstk		!Pointer to working stack
      integer       ipl			!Pointer to lines stack

      real          var(26)		!Input variables
      real          con(70)		!Constant to use for (i)th op in stack 
      real          bsc(26)		!Scales for input images
      real          bzc(26)		!Zeroes for input images
      real          bso			!Scale for output image
      real          bzo			!Zeroe for output image

      logical       anyin		!Any input images?

      logical       work_calc		!Opened workspace for calculated image?
      logical       work_stack		!Opened workspace for stack of images?

      logical    altc_dum1		!4*n logicals
CX      logical    altc_dum2


      common / imcalca /  nvartot,   nxc,   nyc,     nz,   imp,     
     +                    nopcode,   con,  ncon,    nyo,   nxo,
     +                      kseed,  ipco,   ipc,    nim,  nvar,
     +                       impv,   var,   bsc,    bzc,   bso,
     +                     invali, invalo,  bzo,  ipstk,   ipl,
     +                    rinvali, rinvalo, rinvalim, nxim, nyim

      common /imcalcb / anyin,   work_calc, work_stack, altc_dum1

      common / imcalcc / exprs,  opcode,  imid,  varid,  imtot,
     +                   vartot, imty,  outtype

