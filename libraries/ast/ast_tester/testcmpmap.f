      program testcmpmap
      implicit none

      include 'AST_PAR'
      include 'SAE_PAR'

      integer m1, m2, m3, m4, m5, status, i, in(7), out(7)
      double precision x( 7 ), y(7), y2(7), matrix( 3 )

      data matrix /-1.0D0, 1.0D0, 2.0D0 /

      status = sai__ok
      call err_mark( status )
      call ast_begin( status )


      m1 = ast_UnitMap( 1, ' ', status )
      m2 = ast_ZoomMap( 2, 2.0D0, ' ', status )
      m3 = ast_MatrixMap( 3, 3, 1, matrix, ' ', status )
      m4 = ast_CmpMap( ast_CmpMap( m1, m2, .false., ' ', status ), m3,
     :                 .false., ' ', status )


      in( 1 ) = 3
      in( 2 ) = 6
      in( 3 ) = 4
      call ast_mapsplit( m4, 3, in, out, m5, status )
      if( m5 .eq. AST__NULL ) then
         call stopit( status, 'Error 1' )
      else if( ast_geti( m5, 'Nin', status ) .ne. 3 ) then
         call stopit( status, 'Error 2' )
      else if( ast_geti( m5, 'Nout', status ) .ne. 3 ) then
         call stopit( status, 'Error 3' )
      end if

      if( out( 1 ) .ne. 3 ) call stopit( status, 'Error 4' )
      if( out( 2 ) .ne. 4 ) call stopit( status, 'Error 5' )
      if( out( 3 ) .ne. 6 ) call stopit( status, 'Error 6' )


      call readobj( 'splittest1.ast', m1, status )
      in(1)= 1
      call ast_mapsplit( m1, 1, in, out, m2, status )
      if( m2 .ne. AST__NULL )  call stopit( status, 'Error 7' )

      in(2)= 4
      in(3)= 2
      call ast_mapsplit( m1, 3, in, out, m2, status )
      if( m2 .eq. AST__NULL )  then
         call stopit( status, 'Error 8' )
      else if( ast_geti( m2, 'Nin', status ) .ne. 3 ) then
         call stopit( status, 'Error 9' )
      else if( ast_geti( m2, 'Nout', status ) .ne. 3 ) then
         call stopit( status, 'Error 10' )
      end if



      x(1) = 1.0D0
      x(2) = 2.0D0
      x(3) = 4.0D0
      x(4) = 8.0D0
      call ast_trann( m1, 1,4, 1, x, .true., 4, 1, y, status )

      x(1) = 1.0D0
      x(2) = 8.0D0
      x(3) = 2.0D0
      call ast_trann( m2, 1, 3, 1, x, .true., 3, 1, y2, status )

      if( y2( 1 ) .ne. y( 1 ) ) then
         call stopit( status, 'Error 11' )
      else if( y2( 2 ) .ne. y( 2 ) ) then
         call stopit( status, 'Error 12' )
      else if( y2( 3 ) .ne. y( 4 ) ) then
         call stopit( status, 'Error 13' )
      end if







      call ast_end( status )
      call err_rlse( status )

c      call ast_activememory( 'testcmpmap' )
      call ast_flushmemory( 1 )

      if( status .eq. sai__ok ) then
         write(*,*) 'All cmpmap tests passed'
      else
         write(*,*) 'cmpmap tests failed'
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



      subroutine readobj( file, iobj, status )
      implicit none

      include 'AST_PAR'
      include 'SAE_PAR'

      external chsource
      integer iobj, status, ch
      character file*(*)

      open( 10, status='old', file=file )

      ch = ast_channel( chsource, AST_NULL, ' ', status )
      iobj = ast_read( ch, status )
      call ast_annul( ch, status )

      close( 10 )

      end

      subroutine chsource( status )
      implicit none

      include 'AST_PAR'
      include 'SAE_PAR'

      integer status
      character line*200

      read( 10, '(A)', end=99 ) line

      call ast_putline( line, len( line ), status )
      return

 99   call ast_putline( line, -1, status )

      end

