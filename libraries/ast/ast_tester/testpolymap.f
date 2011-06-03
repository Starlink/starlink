      program testpolymap
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'PRM_PAR'

      integer status, pm, pm2, i
      double precision coeff( 16 ), lbnd( 2 ), ubnd( 2 ),
     :                 xin(3), yin(3), xout(3), yout(3),
     :                 xin2(3), yin2(3), coeff_1d(6),
     :                 coeff2( 24 )

      data coeff / 1.0, 1.0, 0.0, 0.0,
     :             2.0, 1.0, 1.0, 0.0,
     :             1.0, 2.0, 0.0, 0.0,
     :             3.0, 2.0, 0.0, 1.0 /

      data coeff2 / 1.0, 1.0, 0.0, 0.0,
     :              2.0, 1.0, 1.0, 0.0,
     :              1.0, 1.0, 0.0, 1.0,
     :              1.0, 2.0, 0.0, 0.0,
     :              1.0, 2.0, 1.0, 0.0,
     :              2.0, 2.0, 0.0, 1.0 /

      data coeff_1d / 1.0, 1.0, 0.0,
     :                2.0, 1.0, 1.0 /

      data lbnd / -10.0D0, -10.0D0 /
      data ubnd / 10.0D0, 10.0D0 /



      status = sai__ok
      call ast_begin( status )

      pm = ast_polymap( 2, 2, 4, coeff, 0, coeff, ' ', status )

      pm2 = ast_polytran( pm, .FALSE., 1.0D-7, lbnd, ubnd, status )

      xin( 1 ) = 1.0d0
      xin( 2 ) = 100.0d0
      xin( 3 ) = -50.0d0
      yin( 1 ) = 1.0d0
      yin( 2 ) = 100.0d0
      yin( 3 ) = -50.0d0

      call ast_tran2( pm2, 3, xin, yin, .true., xout, yout,
     :                status )
      call ast_tran2( pm2, 3, xout, yout, .false., xin2, yin2,
     :                status )

      do i = 1, 3
         if( 2*abs( xin( i ) - xin2( i ) )/( xin( i ) + xin2( i ) ) .gt.
     :       1.0E-6 ) call stopit( 1, status )
         if( 2*abs( yin( i ) - yin2( i ) )/( yin( i ) + yin2( i ) ) .gt.
     :       1.0E-6 ) call stopit( 2, status )
      end do


      pm = ast_polymap( 1, 1, 2, coeff_1d, 0, coeff_1d, ' ', status )
      pm2 = ast_polytran( pm, .FALSE., 1.0D-7, lbnd, ubnd, status )

      xin( 1 ) = 1.0d0
      xin( 2 ) = 100.0d0
      xin( 3 ) = -50.0d0

      call ast_tran1( pm2, 3, xin, .true., xout, status )
      call ast_tran1( pm2, 3, xout, .false., xin2, status )

      do i = 1, 3
         if( 2*abs( xin( i ) - xin2( i ) )/( xin( i ) + xin2( i ) ) .gt.
     :       1.0E-6 ) call stopit( 3, status )
      end do



      pm = ast_polymap( 2, 2, 6, coeff2, 0, coeff2, ' ', status )
      pm2 = ast_polytran( pm, .FALSE., 1.0D-7, lbnd, ubnd, status )

      xin( 1 ) = 1.0d0
      xin( 2 ) = 100.0d0
      xin( 3 ) = -50.0d0
      yin( 1 ) = 1.0d0
      yin( 2 ) = 100.0d0
      yin( 3 ) = -50.0d0

      call ast_tran2( pm2, 3, xin, yin, .true., xout, yout,
     :                status )
      call ast_tran2( pm2, 3, xout, yout, .false., xin2, yin2,
     :                status )

      do i = 1, 3
         if( 2*abs( xin( i ) - xin2( i ) )/( xin( i ) + xin2( i ) ) .gt.
     :       1.0E-6 ) call stopit( 4, status )
         if( 2*abs( yin( i ) - yin2( i ) )/( yin( i ) + yin2( i ) ) .gt.
     :       1.0E-6 ) call stopit( 5, status )
      end do


      call ast_end( status )
      call ast_flushmemory( 1 )

      if( status .eq. sai__ok ) then
         write(*,*) 'All PolyMap tests passed'
      else
         write(*,*) 'PolyMap tests failed'
      end if

      end


      subroutine stopit( i, status )
      implicit none
      include 'SAE_PAR'
      integer i, status
      if( status .eq. sai__ok ) then
         write( *,* ) 'Error ',i
         status = sai__error
      end if
      end
