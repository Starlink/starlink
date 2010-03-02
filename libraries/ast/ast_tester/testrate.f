      program testrate
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'PRM_PAR'

      integer status, m, outp(4), inp(4), c1, c2, c3, c4
      double precision at(4), r, mat(4), b1(2), b2(2), a1(2), 
     :                 a2(4)

      status = sai__ok

      at(1) = 10.0D0
      at(2) = 1.2D6

*  UnitMap 
      m = ast_unitmap( 2, ' ', status )

      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 1.0D0 ) call stopit( 1, r, status )
      r = ast_rate( m, at, 2, 1, status )
      if( r .ne. 0.0D0 ) call stopit( 2, r, status )

      call ast_invert( m, status )
      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 1.0D0 ) call stopit( 202, r, status )

*  ZoomMap 
      m = ast_zoommap( 2, 2.0D0, ' ', status )

      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 2.0D0 ) call stopit( 3, r, status )
      r = ast_rate( m, at, 2, 1, status )
      if( r .ne. 0.0D0 ) call stopit( 4, r, status )

      call ast_invert( m, status )
      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 0.5D0 ) call stopit( 402, r, status )

*  MatrixMap 
      m = ast_matrixmap( 2, 2, 2, mat, ' ', status )

      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 1.0D0 ) call stopit( 5, r, status )
      r = ast_rate( m, at, 2, 1, status )
      if( r .ne. 0.0D0 ) call stopit( 6, r, status )

      call ast_invert( m, status )
      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 1.0D0 ) call stopit( 602, r, status )
      r = ast_rate( m, at, 1, 2, status )
      if( r .ne. 0.0D0 ) call stopit( 603, r, status )

      mat(1)= -2.0D0
      mat(2)= 1.5D0
      m = ast_matrixmap( 2, 2, 1, mat, ' ', status )

      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. -2.0D0 ) call stopit( 7, r, status )
      r = ast_rate( m, at, 2, 2, status )
      if( r .ne. 1.5D0 ) call stopit( 8, r, status )
      r = ast_rate( m, at, 1, 2, status )
      if( r .ne. 0.0D0 ) call stopit( 9, r, status )
      r = ast_rate( m, at, 2, 1, status )
      if( r .ne. 0.0D0 ) call stopit( 10, r, status )

      call ast_invert( m, status )
      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. -0.5D0 ) call stopit( 1002, r, status )

      mat(1)= 1.2D0
      mat(2)= 1.6D0
      mat(3)= -1.6D0
      mat(4)= 2.2D0
      m = ast_matrixmap( 2, 2, 0, mat, ' ', status )

      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 1.2D0 ) call stopit( 11, r, status )
      r = ast_rate( m, at, 2, 2, status )
      if( r .ne. 2.2D0 ) call stopit( 12, r, status )
      r = ast_rate( m, at, 1, 2, status )
      if( r .ne. 1.6D0 ) call stopit( 13, r, status )
      r = ast_rate( m, at, 2, 1, status )
      if( r .ne. -1.6D0 ) call stopit( 14, r, status )

      call ast_invert( m, status )

      r = ast_rate( m, at, 1, 1, status )
      if( abs( r - 0.423076923 ) .gt. 1.0E-6 ) call stopit( 15, r, 
     :                                                      status )

      r = ast_rate( m, at, 2, 2, status )
      if( abs( r - 0.230769231 ) .gt. 1.0E-6 ) call stopit( 16, r, 
     :                                                      status )

      r = ast_rate( m, at, 1, 2, status )
      if( abs( r + 0.307692308 ) .gt. 1.0E-6 ) call stopit( 17, r, 
     :                                                   status )

      r = ast_rate( m, at, 2, 1, status )
      if( abs( r - 0.307692308 ) .gt. 1.0E-6 ) call stopit( 18, r, 
     :                                                   status )


*  ShiftMap 
      mat(1) = -1.2D0
      mat(2) = 1.2D0
      m = ast_shiftmap( 2, mat, ' ', status )

      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 1.0D0 ) call stopit( 20, r, status )
      r = ast_rate( m, at, 2, 1, status )
      if( r .ne. 0.0D0 ) call stopit( 21, r, status )

      call ast_invert( m, status )
      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 1.0D0 ) call stopit( 23, r, status )


*  WinMap 
      a1( 1 ) = 0.0D0
      a1( 2 ) = 0.0D0
      a2( 1 ) = 1.0D0
      a2( 2 ) = 1.0D0
      b1( 1 ) = 0.5D0
      b1( 2 ) = 0.5D0
      b2( 1 ) = 2.5D0
      b2( 2 ) = 2.5D0
      m = ast_winmap( 2, a1, a2, b1, b2, ' ', status )

      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 2.0D0 ) call stopit( 24, r, status )
      r = ast_rate( m, at, 2, 1, status )
      if( r .ne. 0.0D0 ) call stopit( 25, r, status )

      r = ast_rate( m, at, 2, 2, status )
      if( r .ne. 2.0D0 ) call stopit( 26, r, status )
      r = ast_rate( m, at, 1, 2, status )
      if( r .ne. 0.0D0 ) call stopit( 27, r, status )


      call ast_invert( m, status )

      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 0.5D0 ) call stopit( 29, r, status )
      r = ast_rate( m, at, 2, 1, status )
      if( r .ne. 0.0D0 ) call stopit( 30, r, status )

      r = ast_rate( m, at, 2, 2, status )
      if( r .ne. 0.5D0 ) call stopit( 31, r, status )
      r = ast_rate( m, at, 1, 2, status )
      if( r .ne. 0.0D0 ) call stopit( 32, r, status )



*  PermMap 
      outp(1)=2
      outp(2)=1
      inp(1)=1
      inp(2)=2
      m = ast_permmap( 2, inp, 2, outp, 0.0D0, ' ', status )

      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 0.0D0 ) call stopit( 34, r, status )

      r = ast_rate( m, at, 2, 1, status )
      if( r .ne. 1.0D0 ) call stopit( 35, r, status )

      r = ast_rate( m, at, 2, 2, status )
      if( r .ne. 0.0D0 ) call stopit( 37, r, status )

      r = ast_rate( m, at, 1, 2, status )
      if( r .ne. 1.0D0 ) call stopit( 38, r, status )

      call ast_invert( m, status )

      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 1.0D0 ) call stopit( 40, r, status )

      r = ast_rate( m, at, 2, 1, status )
      if( r .ne. 0.0D0 ) call stopit( 41, r, status )

      r = ast_rate( m, at, 2, 2, status )
      if( r .ne. 1.0D0 ) call stopit( 43, r, status )

      r = ast_rate( m, at, 1, 2, status )
      if( r .ne. 0.0D0 ) call stopit( 44, r, status )

*  TranMap 
      a1( 1 ) = 0.0D0
      a1( 2 ) = 0.0D0
      a2( 1 ) = 1.0D0
      a2( 2 ) = 1.0D0
      b1( 1 ) = 0.5D0
      b1( 2 ) = 0.5D0
      b2( 1 ) = 2.5D0
      b2( 2 ) = 2.5D0
      c1 = ast_winmap( 2, a1, a2, b1, b2, ' ', status )

      mat(1)= 1.2D0
      mat(2)= 1.6D0
      mat(3)= -1.6D0
      mat(4)= 2.2D0
      c2 = ast_matrixmap( 2, 2, 0, mat, ' ', status )

      m = ast_tranmap( c1, c2, ' ', status )

      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 2.0D0 ) call stopit( 46, r, status )
      r = ast_rate( m, at, 2, 1, status )
      if( r .ne. 0.0D0 ) call stopit( 47, r, status )

      r = ast_rate( m, at, 2, 2, status )
      if( r .ne. 2.0D0 ) call stopit( 48, r, status )
      r = ast_rate( m, at, 1, 2, status )
      if( r .ne. 0.0D0 ) call stopit( 49, r, status )

      call ast_invert( m, status )

      r = ast_rate( m, at, 1, 1, status )
      if( abs( r - 0.423076923 ) .gt. 1.0E-6 ) call stopit( 51, r, 
     :                                                      status )
      r = ast_rate( m, at, 2, 2, status )
      if( abs( r - 0.230769231 ) .gt. 1.0E-6 ) call stopit( 52, r, 
     :                                                      status )
      r = ast_rate( m, at, 1, 2, status )
      if( abs( r + 0.307692308 ) .gt. 1.0E-6 ) call stopit( 53, r, 
     :                                                   status )
      r = ast_rate( m, at, 2, 1, status )
      if( abs( r - 0.307692308 ) .gt. 1.0E-6 ) call stopit( 54, r, 
     :                                                   status )


*  CmpMap 
      mat(1) = -1.0D0
      mat(2) = 1.0D0
      c1 = ast_shiftmap( 2, mat, ' ', status )
      mat(1)= 1.0D0
      mat(2)= 2.0D0
      mat(3)= -2.0D0
      mat(4)= 3.0D0
      c2 = ast_matrixmap( 2, 2, 0, mat, ' ', status )
      c3 = ast_cmpmap( c1, c2, 0, ' ', status )

      outp(1) = 3
      outp(2) = 4
      outp(3) = 1
      outp(4) = 2
      inp(1) = 3
      inp(2) = 4
      inp(3) = 1
      inp(4) = 2
      c1 = ast_permmap( 4, inp, 4, outp, 0.0D0, ' ', status )
      c2 = ast_ZoomMap( 4, 0.25D0, ' ', status )
      call ast_invert( c2, status )

      c4 = ast_cmpmap( c1, c2, 1, ' ', status )
      call ast_invert( c4, status )

      m = ast_cmpmap( c3, c4, 1, ' ', status )

      call ast_invert( c2, status )
      call ast_invert( c3, status )
      call ast_invert( c4, status )

      at(1) = 1.0D0
      at(2) = 2.0D0
      at(3) = 3.0D0
      at(4) = 4.0D0
      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 0.0D0 ) call stopit( 55, r, status )

      r = ast_rate( m, at, 1, 2, status )
      if( r .ne. 0.0D0 ) call stopit( 56, r, status )

      r = ast_rate( m, at, 1, 3, status )
      if( abs( r - 0.25D0 ) .gt. 1.0D-6 ) call stopit( 57, r, status )

      r = ast_rate( m, at, 1, 4, status )
      if( r .ne. 0.5D0 ) call stopit( 58, r, status )

      r = ast_rate( m, at, 3, 1, status )
      if( r .ne. 0.25D0 ) call stopit( 59, r, status )

      r = ast_rate( m, at, 3, 2, status )
      if( r .ne. 0.0D0 ) call stopit( 60, r, status )

      r = ast_rate( m, at, 3, 3, status )
      if( r .ne. 0.0D0 ) call stopit( 61, r, status )

      r = ast_rate( m, at, 3, 4, status )
      if( r .ne. 0.0D0 ) call stopit( 62, r, status )


      call ast_invert( m, status )

      r = ast_rate( m, at, 1, 1, status )
      if( r .ne. 0.0D0 ) call stopit( 63, r, status )

      r = ast_rate( m, at, 1, 2, status )
      if( r .ne. 0.0D0 ) call stopit( 64, r, status )

      r = ast_rate( m, at, 1, 3, status )
      if( r .ne. 4.0D0 ) call stopit( 65, r, status )

      r = ast_rate( m, at, 1, 4, status )
      if( r .ne. 0.0D0 ) call stopit( 66, r, status )

      r = ast_rate( m, at, 3, 1, status )
      if( abs( r - 12.0D0/7.0D0 ) .gt. 1.0D-6 ) 
     :                            call stopit( 67, r, status )

      r = ast_rate( m, at, 3, 2, status )
      if( abs( r - (-8.0D0/7.0D0) ) .gt. 1.0D-6 ) 
     :                            call stopit( 68, r, status )

      r = ast_rate( m, at, 3, 3, status )
      if( r .ne. 0.0D0 ) call stopit( 69, r, status )

      r = ast_rate( m, at, 3, 4, status )
      if( r .ne. 0.0D0 ) call stopit( 70, r, status )




      if( status .eq. sai__ok ) then
         write(*,*) 'All AST_RATE tests passed'
      else
         write(*,*) 'AST_RATE tests failed'
      end if

      end


      subroutine stopit( i, r, status )
      implicit none
      include 'SAE_PAR'
      integer i, status
      double precision r
      if( status .eq. sai__ok ) then
         write( *,* ) 'Error ',i,': ',r
         status = sai__error
      end if
      end
