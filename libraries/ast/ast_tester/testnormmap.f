      program testnormmap
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'PRM_PAR'

      integer status, m, m2, m3, f, perm(3)
      double precision at(3), bt(3)

      call ast_begin( status )


      status = sai__ok

      f = ast_cmpframe( ast_specframe( ' ', status ),
     :                  ast_skyframe( ' ', status ), ' ', status )

      perm( 1 )  = 3
      perm( 2 )  = 1
      perm( 3 )  = 2
      call ast_permaxes( f, perm, status )
      m = ast_normmap( f, ' ', status )

      if( ast_geti( m, 'nin', status ) .ne. 3 ) call stopit( 1, status )
      if( ast_geti( m, 'nout', status ) .ne. 3 ) call stopit( 2, status)

      if( .not. ast_getl( m, 'TranForward', status ) ) call stopit( 3,
     :                                                        status )
      if( .not. ast_getl( m, 'TranInverse', status ) ) call stopit( 4,
     :                                                        status )


      m2 = ast_copy( m, status )
      call ast_invert( m2, status )
      m3 = ast_simplify( ast_cmpmap( m, m2, .true., ' ', status ),
     :                   status )
      if( .not. ast_isaunitmap( m3, status ) ) call stopit( 5, status )



      at( 1 ) = 2.0D0
      at( 2 ) = 3.0D4
      at( 3 ) = 1.0D0

      call ast_trann( m, 1, 3, 1, at, 1, 3, 1, bt, status )

      if( abs( bt(1)-1.14159265D0) .gt. 1.0D-6 ) then
         write(*,*) bt(1)-1.14159265D0
         call stopit(6,status)
      end if
      if( bt(2) .ne. 3.0D4 ) call stopit(7,status)
      if( abs( bt(3)-4.14159265D0) .gt. 1.0D-6 ) call stopit(8,status)


      call ast_end( status )

      call ast_flushmemory( 1 )

      if( status .eq. sai__ok ) then
         write(*,*) 'All NormMap tests passed'
      else
         write(*,*) 'NormMap tests failed'
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
