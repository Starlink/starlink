!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! LUT.INC
!
!  Color tables copyright Smithsonian Astrophysical Observatory 1989

      integer   numdcol			!No of colours in LUT
      parameter (numdcol=150)
      integer   nlent			!'Length' of a LUT storage table
      parameter (nlent=25)

      integer   numddcol		!No of colours in display for LUT colours 

      integer   depth			! No of memory planes

      integer	lut_stored 		! Number of stored LUTs
      integer	lut_ends		! LUT end types (1=black/white;2=wh/bl;3=col/col;4=wrap
      real	lut_val(NUMDCOL,3)	! RGB arrays
      integer   lut_num			! Present LUT number
      real	lut_sc			! LUT scale
      real	lut_ze			! LUT zero

      logical   lut_flipped		! Is LUT flipped?

      logical   lut_dum1, lut_dum2, lut_dum3	!Make up to 4*n logicals




      common /lutbcom/ lut_flipped, lut_dum1, lut_dum2, lut_dum3

      common /lutacom/ lut_ends, lut_stored,  lut_val,  lut_num,   
     +                 lut_sc,   lut_ze,   numddcol,    depth

