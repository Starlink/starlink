CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C IMCALC.INC     for IMCALC.FOR
C

      character*132 exprs		!Input Equation
      character*6   opcode(70)		!(i)th operation in stack 
      character*1   varid(70)		!Identifiers of input variables
      character*2   imid(26)		!Identifiers of input images
      character*6   imty(26)		!Types (SHORT,REAL) of input images
      character*2   imtot(26)		!List of all unique image names
      character*1   vartot(26)		!List of all unique variable names
      character*6   outtype		!Output image type (REAL,SHORT)

      integer       ip(26)		!Pointer to input images
      integer       ipo			!Pointer to output image
      integer       nx(26)		!X size of input images
      integer       ny(26)		!Y size of input images
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
      integer       ipstk		!Pointer to working stack
      integer       ipl			!Pointer to lines stack

      real          var(26)		!Input variables
      real          con(70)		!Constant to use for (i)th op in stack 
      real          bs(26)		!Scales for input images
      real          bz(26)		!Zeroes for input images
      real          bso			!Scale for output image
      real          bzo			!Zeroe for output image

      logical       anyin		!Any input images?

      logical    imca_dum1		!4*n logicals
      logical    imca_dum2
      logical    imca_dum3


      common / imcalca /  nvartot,    nx,    ny,     nz,   imp,     
     +                    nopcode,   con,  ncon,    nyo,   nxo,
     +                      kseed,   ipo,    ip,    nim,  nvar,
     +                       impv,   var,    bs,     bz,   bso,
     +                     invali, invalo,  bzo,  ipstk,   ipl,
     +                    rinvali, rinvalo

      common /imcalcb / anyin,   imca_dum1,  imca_dum2, imca_dum3

      common / imcalcc / exprs,  opcode,  imid,  varid,  imtot,
     +                   vartot, imty,  outtype

