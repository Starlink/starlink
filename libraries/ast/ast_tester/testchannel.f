      program testrate
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'AST_ERR'

      integer status, ch, sf, sf2
      character buff*50
      status = sai__ok

      call err_begin( status )
      call ast_begin( status )


      call ast_tunec( "hrdel", AST__TUNULLC, buff, status )
      if( buff .ne. '%-%^50+%s70+h%+' .and. status .eq. sai__ok ) then
         call stopit( 0, status )
      endif
      call ast_tunec( "hrdel", "junk", buff, status )
      call ast_tunec( "hrdel", AST__TUNULLC, buff, status )
      if( buff .ne. 'junk' .and. status .eq. sai__ok ) then
         call stopit( -1, status )
      endif

      sf = ast_skyframe( ' ', status )
      ch = ast_channel( AST_NULL, AST_NULL, 'SinkFile=./fred.txt',
     :                  status )
      if( ast_write( ch, sf, status ) .ne. 1 ) then
         call stopit( 1, status )
      end if

      call ast_set( ch, 'SourceFile=./fred.txt', status )
      if( status .eq. SAI__OK ) then
         sf2 = ast_read( ch, status )
         if( status .eq. AST__RDERR ) then
            call err_annul( status )
         else
            call stopit( 7, status )
         end if
      end if

      call ast_clear( ch, 'SinkFile', status )

      call ast_set( ch, 'SourceFile=./fred.txt', status )
      sf2 = ast_read( ch, status )
      if( sf2 .eq. AST__NULL ) call stopit( 2, status )
      if( .not. ast_equal( sf, sf2, status ) ) then
         call stopit( 3, status )
      end if


      call ast_set( ch, 'SinkFile=./fred2.txt', status )
      if( ast_write( ch, sf, status ) .ne. 1 ) then
         call stopit( 4, status )
      end if
      call ast_clear( ch, 'SinkFile', status )

      call ast_set( ch, 'SourceFile=./fred2.txt', status )
      sf2 = ast_read( ch, status )
      if( sf2 .eq. AST__NULL ) call stopit( 5, status )
      if( .not. ast_equal( sf, sf2, status ) ) then
         call stopit( 6, status )
      end if


      call ast_end( status )
      call err_end( status )



      if( status .eq. sai__ok ) then
         write(*,*) 'All Channel tests passed'
      else
         write(*,*) 'Channel tests failed'
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
