      program testlutmap
      implicit none

      include 'AST_PAR'
      include 'SAE_PAR'

      integer lm, status, i
      double precision lut1( 10 ), x( 7 ), y(7)

      status = sai__ok
      call err_mark( status )
      call ast_begin( status )


      data lut1/ -1D0, 0D0, 1D0, 2D0, 3D0, 4D0, 5D0, 6D0, 7D0, 8D0 /




      lm = ast_lutmap( 10, lut1, -1.0D0, 1.0D0, ' ', status )
      x( 1 ) = -2.0D0
      x( 2 ) = -1.0D0
      x( 3 ) = -0.5D0
      x( 4 ) = 3.0D0
      x( 5 ) = 7.5D0
      x( 6 ) = 8.0D0
      x( 7 ) = 8.5D0

      call ast_tran1( lm, 7, x, .TRUE., y, status )

      do i = 1, 7
         if( x( i ) .ne. y( i ) ) then
            call stopit( status, "Error 1" );
         end if
      end do

      call ast_tran1( lm, 7, y, .FALSE., x, status )

      do i = 1, 7
         if( x( i ) .ne. y( i ) ) then
            call stopit( status, "Error 2" );
         end if
      end do




      lut1( 1 ) = lut1( 2 )
      lm = ast_lutmap( 10, lut1, -1.0D0, 1.0D0, ' ', status )
      x( 1 ) = -2.0D0
      x( 2 ) = -1.0D0
      x( 3 ) = -0.5D0
      x( 4 ) = 0.5D0
      x( 5 ) = 3.0D0
      x( 6 ) = 8.0D0
      x( 7 ) = 8.5D0

      call ast_tran1( lm, 7, x, .TRUE., y, status )

      do i = 1, 3
         if( y( i ) .ne. 0.0 ) then
            call stopit( status, "Error 3" );
         end if
      end do

      do i = 4, 7
         if( x( i ) .ne. y( i ) ) then
            call stopit( status, "Error 4" );
         end if
      end do

      call ast_tran1( lm, 7, y, .FALSE., x, status )

      do i = 1, 3
         if( x( i ) .ne. AST__BAD ) then
            call stopit( status, "Error 5" );
         end if
      end do

      do i = 4, 7
         if( x( i ) .ne. y( i ) ) then
            call stopit( status, "Error 6" );
         end if
      end do


      lut1( 5 ) = AST__BAD
      lm = ast_lutmap( 10, lut1, -1.0D0, 1.0D0, ' ', status )
      x( 1 ) = -2.0D0
      x( 2 ) = -1.0D0
      x( 3 ) = -0.5D0
      x( 4 ) = 0.5D0
      x( 5 ) = 3.0D0
      x( 6 ) = 8.0D0
      x( 7 ) = 8.5D0

      call ast_tran1( lm, 7, x, .TRUE., y, status )

      do i = 1, 3
         if( y( i ) .ne. 0.0 ) then
            call stopit( status, "Error 7" );
         end if
      end do

      if( y( 5 ) .ne. AST__BAD ) then
         call stopit( status, "Error 8" );
      end if
      y(5) = x( 5 )

      do i = 4, 7
         if( x( i ) .ne. y( i ) ) then
            call stopit( status, "Error 9" );
         end if
      end do

      call ast_tran1( lm, 7, y, .FALSE., x, status )

      do i = 1, 3
         if( x( i ) .ne. AST__BAD ) then
            call stopit( status, "Error 10" );
         end if
      end do

      if( x( 5 ) .ne. AST__BAD ) then
         call stopit( status, "Error 11" );
      end if
      x(5) = y( 5 )

      do i = 4, 7
         if( x( i ) .ne. y( i ) ) then
            call stopit( status, "Error 12" );
         end if
      end do
















      call ast_end( status )
      call err_rlse( status )

c      call ast_activememory( 'testlutmap' )
      call ast_flushmemory( 1 )

      if( status .eq. sai__ok ) then
         write(*,*) 'All LutMap tests passed'
      else
         write(*,*) 'LutMap tests failed'
      end if

      end



      subroutine stopit( status, text )
      implicit none
      include 'SAE_PAR'
      integer status
      character text*(*)
      if( status .ne. sai__ok ) return
      status = sai__error
      write(*,*) text
      end



