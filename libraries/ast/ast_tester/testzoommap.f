      program testzoommap
      implicit none

      include 'AST_PAR'
      include 'AST_ERR'
      include 'SAE_PAR'

      integer zm, cm, status

      status = sai__ok
      call ast_begin( status )

      zm = ast_zoommap( 1, -1.0D0, ' ', status )
      if( .not. ast_test( zm, 'Zoom', status ) ) then
         call stopit( status, "Error 1" );
      else if( ast_getd( zm, 'Zoom', status ) .ne. -1.0D0 ) then
         call stopit( status, "Error 2" );
      end if

      call ast_clear( zm, 'Zoom', status )
      if( ast_test( zm, 'Zoom', status ) ) then
         call stopit( status, "Error 3" );
      else if( ast_getd( zm, 'Zoom', status ) .ne. 1.0D0 ) then
         call stopit( status, "Error 4" );
      end if

      call ast_setd( zm, 'Zoom', 2.5D0, status )
      if( .not. ast_test( zm, 'Zoom', status ) ) then
         call stopit( status, "Error 5" );
      else if( ast_getd( zm, 'Zoom', status ) .ne. 2.5D0 ) then
         call stopit( status, "Error 6" );
      end if

      cm = ast_cmpmap( zm, ast_unitmap( 1, ' ', status ), .TRUE.,
     :                 ' ', status )

      if( .not. ast_test( zm, 'Zoom', status ) ) then
         call stopit( status, "Error 7" );
      else if( ast_getd( zm, 'Zoom', status ) .ne. 2.5D0 ) then
         call stopit( status, "Error 8" );
      end if

      call err_mark

      call ast_setd( zm, 'Zoom', 1.5D0, status )
      if( status .eq. ast__immut ) then
         call err_annul( status )
      else if( status .eq. sai__ok ) then
         call stopit( status, "Error 9" );
      else
         call stopit( status, "Error 10" );
      end if

      call ast_clear( zm, 'Zoom', status )
      if( status .eq. ast__immut ) then
         call err_annul( status )
      else if( status .eq. sai__ok ) then
         call stopit( status, "Error 11" );
      else
         call stopit( status, "Error 12" );
      end if

      call err_rlse

      call ast_end( status )

c      call ast_activememory( 'testzoommap' )
      call ast_flushmemory( 1 )

      if( status .eq. sai__ok ) then
         write(*,*) 'All ZoomMap tests passed'
      else
         write(*,*) 'ZoomMap tests failed'
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



