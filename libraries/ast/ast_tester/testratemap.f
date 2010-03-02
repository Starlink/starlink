      program testratemap
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'PRM_PAR'

      integer status, m, outp(4), inp(4), c1, c2, c3, c4, rm
      double precision at(4), r, mat(4), b1(2), b2(2), a1(2), 
     :                 a2(4)

      status = sai__ok

      at(1) = 10.0D0
      at(2) = 1.2D6

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

      rm = ast_ratemap( m, 1, 1, ' ', status )
      at(1) = 1.0D0
      at(2) = 2.0D0
      at(3) = 3.0D0
      at(4) = 4.0D0
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 0.0D0 ) call stopit( 1, r, status )

      if( .not. ast_getl( rm, 'TranForward', status ) ) call stopit( 2,
     :                                                   r, status )

      if( ast_getl( rm, 'TranInverse', status ) ) call stopit( 3, r, 
     :                                                     status )

      rm = ast_ratemap( m, 1, 2, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 0.0D0 ) call stopit( 4, r, status )

      rm = ast_ratemap( m, 1, 3, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( abs( r - 0.25D0 ) .gt. 1.0D-6 ) call stopit( 5, r, status )

      rm = ast_ratemap( m, 1, 4, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 0.5D0 ) call stopit( 6, r, status )

      rm = ast_ratemap( m, 3, 1, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 0.25D0 ) call stopit( 7, r, status )

      rm = ast_ratemap( m, 3, 2, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 0.0D0 ) call stopit( 8, r, status )

      rm = ast_ratemap( m, 3, 3, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 0.0D0 ) call stopit( 9, r, status )

      rm = ast_ratemap( m, 3, 4, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 0.0D0 ) call stopit( 10, r, status )

      call ast_invert( m, status )

      rm = ast_ratemap( m, 1, 1, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 0.0D0 ) call stopit( 11, r, status )

      rm = ast_ratemap( m, 1, 2, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 0.0D0 ) call stopit( 12, r, status )

      rm = ast_ratemap( m, 1, 3, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 4.0D0 ) call stopit( 13, r, status )

      rm = ast_ratemap( m, 1, 4, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 0.0D0 ) call stopit( 14, r, status )

      rm = ast_ratemap( m, 3, 1, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( abs( r - 12.0D0/7.0D0 ) .gt. 1.0E-6 ) 
     :                          call stopit( 15, r, status )

      rm = ast_ratemap( m, 3, 2, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( abs( r - (-8.0D0/7.0D0) ) .gt. 1.0E-6 ) 
     :                          call stopit( 16, r, status )

      rm = ast_ratemap( m, 3, 3, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 0.0D0 ) call stopit( 17, r, status )

      rm = ast_ratemap( m, 3, 4, ' ', status )
      call ast_trann( rm, 1, 4, 1, at, 1, 1, 1, r, status )
      if( r .ne. 0.0D0 ) call stopit( 18, r, status )






      if( status .eq. sai__ok ) then
         write(*,*) 'All RateMap tests passed'
      else
         write(*,*) 'RateMap tests failed'
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
