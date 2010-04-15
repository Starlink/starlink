      program testtrangrid
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'PRM_PAR'

      integer status, i, m, outp(4), inp(4), c1, c2, c3, c4, fc, fs
      double precision at(4), r, mat(4), b1(2), b2(2), a1(2),
     :                 a2(4)
      character cards(9)*80

      status = sai__ok

      at(1) = 10.0D0
      at(2) = 1.2D6

*  UnitMap
      m = ast_unitmap( 2, ' ', status )
      call testmap( m, 1, 0.5D0, status )

*  ZoomMap
      m = ast_zoommap( 2, 2.0D0, ' ', status )
      call testmap( m, 2, 0.5D0, status )

*  MatrixMap
      m = ast_matrixmap( 2, 2, 2, mat, ' ', status )
      call testmap( m, 3, 0.5D0, status )

*  PermMap
      outp(1)=2
      outp(2)=1
      inp(1)=2
      inp(2)=1
      m = ast_permmap( 2, inp, 2, outp, 0.0D0, ' ', status )
      call testmap( m, 4, 0.5D0, status )

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
      c2 = ast_copy( c1, status )

      m = ast_tranmap( c1, c2, ' ', status )
      call testmap( m, 5, 0.5D0, status )

*  3D CmpMap
      mat(1) = -1.0D0
      c1 = ast_shiftmap( 1, mat, ' ', status )
      mat(1)= 1.0D0
      mat(2)= 2.0D0
      mat(3)= -2.0D0
      mat(4)= 3.0D0
      c2 = ast_matrixmap( 2, 2, 0, mat, ' ', status )
      c3 = ast_cmpmap( c1, c2, 0, ' ', status )

      outp(1) = 3
      outp(2) = 2
      outp(3) = 1
      inp(1) = 3
      inp(2) = 2
      inp(3) = 1
      c1 = ast_permmap( 3, inp, 3, outp, 0.0D0, ' ', status )
      c2 = ast_ZoomMap( 3, 0.25D0, ' ', status )
      call ast_invert( c2, status )

      c4 = ast_cmpmap( c1, c2, 1, ' ', status )
      call ast_invert( c4, status )

      m = ast_cmpmap( c3, c4, 1, ' ', status )
      call testmap( m, 6, 0.5D0, status )



* 1D non-linear Mapping
      m = ast_mathmap( 1, 1, 1, 'y=x**3', 1,
     :                 'x=sign((abs(y)**(1/3)),y)', ' ', status )
      call testmap( m, 7, 0.0001D0, status )



* A FITS-WCS pixel->sky mapping
      cards(1) = 'CTYPE1  = ''RA---TAN'''
      cards(2) = 'CTYPE2  = ''DEC--TAN'''
      cards(3) = 'CRPIX1  = 20'
      cards(4) = 'CRPIX2  = 20'
      cards(5) = 'CRVAL1  = 0.0'
      cards(6) = 'CRVAL2  = 0.0'
      cards(7) = 'CROTA1  = 30.0'
      cards(8) = 'CDELT1  = -0.001'
      cards(9) = 'CDELT2  = 0.001'

      fc = ast_fitschan( ast_null, ast_null, ' ', status )
      do i = 1, 9
         call ast_putfits( fc, cards(i), .false., status )
      end do
      call ast_clear( fc, 'card', status )
      fs = ast_read( fc, status )

      call testmap( fs, 8, 0.0001D0, status )









      if( status .eq. sai__ok ) then
         write(*,*) 'All AST_TRANGRID tests passed'
      else
         write(*,*) 'AST_TRANGRID tests failed'
      end if

      end





      subroutine testmap( map, itest, tol, status )
      implicit none
      include 'SAE_PAR'

      integer status, maxpix, itest, map
      double precision tol

      if( status .ne. sai__ok ) return

      maxpix = 100
      call testgridpair( map, tol, maxpix, itest*10, status )
      call testgridpair( map, 0.0D0, maxpix, itest*10+2, status )

      maxpix = 4
      call testgridpair( map, tol, maxpix, itest*10+4, status )
      call testgridpair( map, 0.0D0, maxpix, itest*10+6, status )

      end




      subroutine testgridpair( map, tol, maxpix, itest, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      integer status, lbnd( 3 ), ubnd( 3 ), nin, nout, i, outdim,
     :         maxpix, itest, size, pos(3), j, map, lbndi(3), ubndi(3)
      logical fwd
      double precision tol, xl(3), xu(3), dlbndi(3), dubndi(3),
     :                 dlbnd(3), dubnd(3)

      data lbnd / -6, 0, 10 /
      data ubnd / 15, 30, 40/

      if( status .ne. sai__ok ) return


      nin = ast_geti( map, 'nin', status )
      nout = ast_geti( map, 'nout', status )

      outdim = 1
      do i = 1, nin
         outdim = outdim*( ubnd( i ) - lbnd( i ) + 1 )
         dlbnd( i ) = lbnd( i )
         dubnd( i ) = ubnd( i )
      end do

      call testgrid( map, nin, lbnd, ubnd, tol, maxpix, .true., nout,
     :               outdim, itest, status )

      outdim = 1
      do i = 1, nout
         call ast_mapbox( map, dlbnd, dubnd, .true., i, dlbndi(i),
     :                    dubndi(i), xl, xu, status )
         lbndi( i ) = dlbndi( i )
         ubndi( i ) = dubndi( i )
         outdim = outdim*( ubndi( i ) - lbndi( i ) + 1 )
      end do

      call testgrid( map, nout, lbndi, ubndi, tol, maxpix, .false., nin,
     :               outdim, itest + 1, status )


      end





      subroutine testgrid( map, nin, lbnd, ubnd, tol, maxpix, fwd, nout,
     :                     outdim, itest, status )
      implicit none
      include 'SAE_PAR'

      integer MAXPNT
      parameter( MAXPNT = 50000 )

      integer status, lbnd( 3 ), ubnd( 3 ), nin, nout, i, outdim,
     :         maxpix, itest, pos(3), j, map
      logical fwd
      double precision tol, out( MAXPNT, 3 ), in( MAXPNT, 3 )

      if( status .ne. sai__ok ) return

* Check arrays are not over full
      if( outdim .gt. MAXPNT ) then
         status = sai__error
         write(*,*) 'Array length exceeded in TESTTRANGRID:TESTGRID'
         return
      end if

*  Create a regular grid of positions within the input space of the
*  Mapping, and transform it to the output space of the Mapping.
      call ast_trangrid( map, nin, lbnd, ubnd, tol, maxpix, fwd,
     :                   nout, MAXPNT, out, status )

*  Convert the transformed output positions back into the input space.
      call ast_trann( map, outdim, nout, MAXPNT, out, .not. fwd, nin,
     :                MAXPNT, in, status )

*  Check the input space positions are close to a regular grid.
      if( status .eq. sai__ok ) then

         do i = 1, nin
            pos( i ) = lbnd( i )
         end do

         do j = 1, outdim

            do i = 1, nin

               if( pos( i ) .ne. 0 ) then
                  if( abs( in( j, i ) - pos( i ) ) .gt.
     :                1.0E-6*abs( 0.5*( in( j, i ) + pos( i ) ) ) ) then
                     status = sai__error
                     write(*,*) 'Test ',itest,' failed at point ',j,
     :                          ' axis ',i,': ',in( j, i ),
     :                          ' should be ',pos(1)
                     return
                  end if
               else
                  if( abs( in( j, i ) ) .gt. 1.0E-5 ) then
                     status = sai__error
                     write(*,*) 'Test ',itest,' failed at point ',j,
     :                          ' axis ',i,': ',in( j, i ),
     :                          ' should be ', pos(i)
                     return
                  end if
               end if
            end do

            pos( 1 ) = pos( 1 ) + 1
            if( pos( 1 ) .gt. ubnd( 1 ) ) then
               pos( 1 ) = lbnd( 1 )
               if( nin .gt. 1 ) then
                  pos( 2 ) = pos( 2 ) + 1
                  if( pos( 2 ) .gt. ubnd( 2 ) ) then
                     pos( 2 ) = lbnd( 2 )
                     if( nin .gt. 2 ) then
                        pos( 3 ) = pos( 3 ) + 1
                     end if
                  end if
               end if
            end if
         end do
      end if

      end
