CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C TABLE.INC   General include file for table programs
C

      integer   tby		!Input table no of rows
      integer   tbvx		!Input table no of columns (+5 for name)
      integer   tbx		!Input table no of columns 

      integer   tby1		!Input table 1 - no of rows
      integer   tbvx1		!Input table 1 - no of columns (+5 for name)
      integer   tbx1		!Input table 1 - no of columns 

      integer   tby2		!Input table 2 - no of rows
      integer   tbvx2		!Input table 2 - no of columns (+5 for name)
      integer   tbx2		!Input table 2 - no of columns 

      integer   tbyo		!Output table no of rows
      integer   tbvxo		!Output table no of columns (+5 for name)
      integer   tbxo		!Output table no of columns 

      integer   tbyo1		!Output table 1 - no of rows
      integer   tbvxo1		!Output table 1 - no of columns (+5 for name)
      integer   tbxo1		!Output table 1 - no of columns 

      integer   tbyo2		!Output table 2 - no of rows
      integer   tbvxo2		!Output table 2 - no of columns (+5 for name)
      integer   tbxo2		!Output table 2 - no of columns 


      common / tbcoma /   tby,    tbvx,    tbx,   tby1,   tbvx1,  tbx1, 
     +                   tby2,   tbvx2,   tbx2,   tbyo,   tbvxo,  tbxo,
     +                  tbyo1,  tbvxo1,  tbxo1,  tbyo2,  tbvxo2,  
     +                  tbxo2  


