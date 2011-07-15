      program testpolymap
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'PRM_PAR'

      integer status, pm, pm2, i, maxord
      double precision coeff( 16 ), lbnd( 2 ), ubnd( 2 ),
     :                 xin(3), yin(3), xout(3), yout(3), errlim,
     :                 xin2(3), yin2(3), coeff_1d(6), acc,
     :                 coeff2( 24 ), coeff3( 6*4 ), err, maxacc

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

c      data coeff3 / -0.1,     1.0, 0.0, 0.0,
c     :              0.99,     1.0, 1.0, 0.0,
c     :              1.0E-4,   1.0, 1.0, 1.0,
c     :              -1.0E-9,  1.0, 2.0, 1.0,
c     :              -0.1,     2.0, 0.0, 0.0,
c     :              0.99,     2.0, 0.0, 1.0,
c     :              1.0E-4,   2.0, 1.0, 1.0,
c     :              -1.0E-9,  2.0, 1.0, 2.0 /

      data coeff3 / -0.1,     1.0, 0.0, 0.0,
     :              0.99,     1.0, 1.0, 0.0,
     :              1.0E-4,   1.0, 1.0, 1.0,
     :              -0.1,     2.0, 0.0, 0.0,
     :              0.99,     2.0, 0.0, 1.0,
     :              1.0E-4,   2.0, 1.0, 1.0 /


      data coeff_1d / 1.0, 1.0, 0.0,
     :                2.0, 1.0, 1.0 /

      data lbnd / -10.0D2, -10.0D2 /
      data ubnd / 10.0D2, 10.0D2 /



      status = sai__ok
      call ast_begin( status )

      acc = 1.0D-7
      errlim = 1000*acc
      maxacc = 1.0D-3
      maxord = 10

      pm = ast_polymap( 2, 2, 4, coeff, 0, coeff, ' ', status )
      pm2 = ast_polytran( pm, .FALSE., acc, maxacc, maxord, lbnd,
     :                    ubnd, status )

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
         if( abs( xin( i ) - xin2( i ) ) .gt. errlim ) then
            call stopit( 1, status )
         endif
         if( abs( yin( i ) - yin2( i ) ) .gt. errlim ) then
            call stopit( 2, status )
         endif
      end do







      pm = ast_polymap( 1, 1, 2, coeff_1d, 0, coeff_1d, ' ', status )
      pm2 = ast_polytran( pm, .FALSE., acc, maxacc, maxord, lbnd,
     :                    ubnd, status )

      xin( 1 ) = 1.0d0
      xin( 2 ) = 100.0d0
      xin( 3 ) = -50.0d0

      call ast_tran1( pm2, 3, xin, .true., xout, status )
      call ast_tran1( pm2, 3, xout, .false., xin2, status )

      do i = 1, 3
         if( abs( xin( i ) - xin2( i ) ) .gt. errlim ) then
            call stopit( 3, status )
         endif
      end do






      pm = ast_polymap( 2, 2, 6, coeff2, 0, coeff2, ' ', status )
      pm2 = ast_polytran( pm, .FALSE., acc, maxacc, maxord, lbnd,
     :                    ubnd, status )

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
         if( abs( xin( i ) - xin2( i ) ) .gt. errlim ) then
            call stopit( 4, status )
         endif
         if( abs( yin( i ) - yin2( i ) ) .gt. errlim ) then
            call stopit( 5, status )
         endif
      end do








      pm = ast_polymap( 2, 2, 6, coeff3, 0, coeff3, ' ', status )
      pm2 = ast_polytran( pm, .FALSE., acc, maxacc, maxord, lbnd,
     :                    ubnd, status )

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
         if( abs( xin( i ) - xin2( i ) ) .gt. errlim ) then
             write(*,*) i, xin( i ), xin2( i ), errlim
             call stopit( 6, status )
         endif
         if( abs( yin( i ) - yin2( i ) ) .gt. errlim ) then
            call stopit( 7, status )
         endif
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
