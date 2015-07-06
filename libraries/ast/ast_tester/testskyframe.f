      program testskyframe
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      integer status, sf1, sf2, fs
      status = sai__ok

      sf1 = ast_skyframe( 'system=fk5,epoch=2015.0', status )
      sf2 = ast_skyframe( 'system=fk5,epoch=2015.1', status )
      fs = ast_convert( sf1, sf2, 'SKY', status )
      if( fs .eq. AST__NULL ) then
         call stopit( status, 'Error 0' )
      end if

      if( .not. ast_isaunitmap( ast_getmapping( fs, ast__base,
     :                                         ast__current, status ),
     :                         status ) ) then
         call stopit( status, 'Error 1' )
      end if

      if( ast_getd( sf1, 'SkyTol', status ) .ne. 0.001D0 ) then
         call stopit( status, 'Error 2' )
      end if

      call ast_setd( sf2, 'SkyTol', 1.0D-6, status )
      fs = ast_convert( sf1, sf2, 'SKY', status )

      if( ast_isaunitmap( ast_getmapping( fs, ast__base,
     :                                    ast__current, status ),
     :                    status ) ) then
         call stopit( status, 'Error 3' )
      end if

      sf2 = ast_skyframe( 'system=fk5,epoch=2016.6', status )
      fs = ast_convert( sf1, sf2, 'SKY', status )
      if( ast_isaunitmap( ast_getmapping( fs, ast__base,
     :                                    ast__current, status ),
     :                    status ) ) then
         call stopit( status, 'Error 4' )
      end if








      if( status .eq. sai__ok ) then
         write(*,*) 'All SkyFrame tests passed'
      else
         write(*,*) 'SkyFrame tests failed'
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


