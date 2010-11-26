      program testtable
      implicit none

      include 'AST_PAR'
      include 'AST_ERR'
      include 'SAE_PAR'

      integer status, table, table2, dims( 7 ), ival, l, nval
      byte bytes(1,2),bval
      real rval
      character cval*30, text(2,2)*10


c      call ast_watchmemory(483)


      status = sai__ok
      call err_mark( status )
      call ast_begin( status )

      table = ast_table( ' ', status )

      call ast_mapput0i( table, 'Fred', 123, 'com 1', status )
      if( status .eq. AST__BADKEY ) then
         call err_annul( status )
      else
         if( status .ne. sai__ok ) call err_annul( status )
         call stopit( status, 'Table error 1' )
      endif

      call ast_mapput0i( table, 'Fred(2)', 123, 'com 1', status )
      if( status .eq. AST__BADKEY ) then
         call err_annul( status )
      else
         if( status .ne. sai__ok ) call err_annul( status )
         call stopit( status, 'Table error 2' )
      endif

      dims( 1 ) = 5
      dims( 2 ) = 2
      call ast_addcolumn( table, 'Fred', AST__FLOATTYPE, 2, dims,
     :                    status )

      if( ast_geti( table, 'NColumn', status ) .ne. 1 ) then
         call stopit( status, 'Table error 2b' )
      endif

      if( ast_columnname( table, 1, status ) .ne. 'Fred' ) then
         call stopit( status, 'Table error 2c' )
      endif

      if( ast_columntype( table, 'Fred', status ) .ne.
     :                    AST__FLOATTYPE ) then
         call stopit( status, 'Table error 2d' )
      endif

      call ast_columnshape( table, 'Fred', 2, nval, dims, status )
      if( nval .ne. 2 ) then
         call stopit( status, 'Table error 2e' )
      else if( dims( 1 ) .ne. 5 ) then
         call stopit( status, 'Table error 2f' )
      else if( dims( 2 ) .ne. 2 ) then
         call stopit( status, 'Table error 2g' )
      endif

      call ast_mapput0i( table, 'Fred(2)', 123, 'com 1', status )
      if( status .eq. AST__BADTYP ) then
         call err_annul( status )
      else if( status .eq. sai__ok ) then
         call stopit( status, 'Table error 3' )
      endif

      call ast_mapput0r( table, 'Fred(2)', 123, 'com 1', status )
      if( status .eq. AST__BADTYP ) then
         call err_annul( status )
      else if( status .eq. sai__ok ) then
         call stopit( status, 'Table error 4' )
      endif

      call ast_addcolumn( table, 'Fred', AST__FLOATTYPE, 0, 0,
     :                    status )
      if( status .eq. AST__OLDCOL ) then
         call err_annul( status )
      else if( status .eq. sai__ok ) then
         call stopit( status, 'Table error 5' )
      endif

      call ast_removecolumn( table, 'Fred', status )

      if( ast_geti( table, 'NColumn', status ) .ne. 0 ) then
         call stopit( status, 'Table error 5b' )
      endif

      call ast_addcolumn( table, 'Fred', AST__FLOATTYPE, 0, 0,
     :                    status )

      call ast_mapput0r( table, 'Fred(1)', -123.0, 'com 1', status )
      call ast_mapput0r( table, 'Fred(2)', 123.0, 'com 2', status )

      if( ast_mapget0r( table, 'Fred(2)', rval, status ) ) then
         if( rval .ne. 123.0 ) call stopit( status, 'Table error 6' )
      else
         call stopit( status, 'Table error 7' )
      endif

      call ast_addcolumn( table, 'Dick', AST__OBJECTTYPE, 0, 0,
     :                    status )
      call ast_mapput0a( table, 'Dick(1)', table, 'com 1', status )
      if( status .eq. AST__KYCIR ) then
         call err_annul( status )
      else if( status .eq. sai__ok ) then
         call stopit( status, 'Table error 8' )
      endif

      if( ast_geti( table, 'NColumn', status ) .ne. 2 ) then
         call stopit( status, 'Table error 8b' )
      endif

      if( ast_columnname( table, 1, status ) .ne. 'Dick' ) then
         call stopit( status, 'Table error 8c' )
      endif

      if( ast_columnname( table, 2, status ) .ne. 'Fred' ) then
         call stopit( status, 'Table error 8d' )
      endif

      call ast_removecolumn( table, 'Dick', status )

      if( ast_geti( table, 'NRow', status ) .ne. 2 ) then
         call stopit( status, 'Table error 9' )
      endif

      if( ast_mapget0r( table, 'Fred(3)', rval, status ) ) then
         call stopit( status, 'Table error 10' )
      endif

      if( ast_mapget0i( table, 'Fred(2)', ival, status ) ) then
         if( ival .ne. 123 ) call stopit( status, 'Table error 11' )
      else
         call stopit( status, 'Table error 12' )
      endif

      if( ast_mapget0c( table, 'Fred(2)', cval, l, status ) ) then
         if( cval .ne. '123' ) call stopit( status, 'Table error 13' )
         if( l .ne. 3 ) call stopit( status, 'Table error 14' )
      else
         call stopit( status, 'Table error 15' )
      endif

      call ast_removerow( table, 3, status )
      if( ast_geti( table, 'NRow', status ) .ne. 2 ) then
         call stopit( status, 'Table error 16' )
      endif

      call ast_removerow( table, 2, status )
      if( ast_geti( table, 'NRow', status ) .ne. 1 ) then
         call stopit( status, 'Table error 17' )
      endif

      if( ast_mapget0r( table, 'Fred(2)', rval, status ) ) then
         call stopit( status, 'Table error 18' )
      endif






      call checkDump( table, 'checkDump 1 ', status )

      table2 = ast_copy( table, status )

      if( ast_mapget0c( table2, 'Fred(1)', cval, l, status ) ) then
         if( cval .ne. '-123' ) call stopit( status, 'Table error 19' )
         if( l .ne. 4 ) call stopit( status, 'Table error 20' )
      else
         call stopit( status, 'Table error 21' )
      endif

      dims( 1 ) = 2
      dims( 2 ) = 2
      call ast_addcolumn( table, 'Dick', AST__STRINGTYPE, 2, dims,
     :                    status )

      text(1,1) = 'One'
      text(2,1) = 'two'
      text(1,2) = 'three'
      text(2,2) = 'FouR'

      call ast_mapput1c( table, 'Dick(4)', 4, text, 'jjjj', status )
      if( ast_mapget1c( table, 'Dick(4)', 4, nval, text, status ) ) then

         if( text(1,1) .ne. 'One' )
     :      call stopit( status, 'Table error 22' )
         if( text(2,1) .ne. 'two' )
     :      call stopit( status, 'Table error 23' )
         if( text(1,2) .ne. 'three' )
     :      call stopit( status, 'Table error 24' )
         if( text(2,2) .ne. 'FouR' )
     :      call stopit( status, 'Table error 25' )

         if( nval .ne. 4 ) call stopit( status, 'Table error 26' )
      else
         call stopit( status, 'Table error 27' )
      endif

      call ast_mapputelemc( table, 'Dick(4)', 3, 'OHOHOHOH', status )
      if( ast_mapgetelemc( table, 'Dick(4)', 3, cval, status ) ) then
         if( cval .ne. 'OHOHOHOH' )
     :      call stopit( status, 'Table error 28' )
      else
         call stopit( status, 'Table error 29' )
      endif



      dims( 1 ) = 1
      dims( 2 ) = 2
      call ast_addcolumn( table, 'HeHe', AST__BYTETYPE, 2, dims,
     :                    status )
      bytes(1,1) = 127
      bytes(1,2) = 255
      call ast_mapput1b( table, 'HeHe(2)', 2, bytes, 'jjjj', status )
      if( ast_mapget1b( table, 'HeHe(2)', 2, nval, bytes,
     :                  status ) ) then
         if( nval .ne. 2 ) call stopit( status, 'Table error 30' )
         if( bytes(1,1) .ne. 127 ) call stopit( status,
     :                                          'Table error 31' )
         if( bytes(1,2) .ne. -1 ) call stopit( status,
     :                                         'Table error 32' )
      else
         call stopit( status, 'Table error 33' )
      endif

      call ast_addcolumn( table, 'GoGo', AST__BYTETYPE, 0, 0,
     :                    status )
      call ast_mapput0b( table, 'GoGo(2)', -10, ' ', status )
      if( ast_mapget0b( table, 'GoGo(2)', bval, status ) ) then
         if( bval .ne. -10 ) call stopit( status, 'Table error 33' )
      else
         call stopit( status, 'Table error 34' )
      endif









      call ast_end( status )
      call err_rlse( status )

      call ast_activememory( 'testtable' )
      call ast_flushmemory( 1 );

      if( status .eq. sai__ok ) then
         write(*,*) 'All Table tests passed'
      else
         write(*,*) 'Table tests failed'
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

      subroutine checkdump( obj, text, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      character text*(*),key*30,txt1*50,txt2*50
      integer obj, status, next, end, ch, result, ll, overlap, size,
     :        i, type,obj1,obj2,l1,l2,nl,nrow,nrowold
      external mysource, mysink
      character buf*400000

      common /ss1/ buf
      common /ss2/ next, end, ll, nl

      if( status .ne. sai__ok ) return

      ch = ast_channel( mysource, mysink, ' ', status )


      nl = 0
      ll = 110
      next = 1
      if( ast_write( ch, obj, status ) .ne.1 ) then
         write(*,*) text
         call stopit( status, 'Cannot write supplied object to '//
     :                'channel' )
      end if

      next = 1
      nl = 0
      result = ast_read( ch, status )

      if( result .eq. ast__null ) then
         write(*,*) text
         call stopit( status, 'Cannot read object from channel' )
      end if

      if( .not. ast_isatable( result ) ) then
         call stopit( status, 'Object read from channel is not a table')
      end if

      nrowold = ast_geti( obj, 'NRow', status )
      nrow = ast_geti( result, 'NRow', status )
      if(  nrow .ne. nrowold ) then
         write(*,*) nrow, nrowold
         call stopit( status, 'checkDump 0' )
      endif

      size = ast_mapsize( result, status )
      if(  ast_mapsize( obj, status ) .ne. size ) then
         write(*,*) size,  ast_mapsize( obj, status )
         call stopit( status, 'checkDump 1' )
      else
         do i = 1, size
            key = ast_mapkey( result, i, status )
            type = ast_maptype( result, key, status )
            if( ast_maptype( obj, key, status ) .ne. type ) then
               write(*,*) type,  ast_maptype( obj, key, status )
               call stopit( status, 'checkDump 4' )
            else

               if( type .eq. AST__OBJECTTYPE ) then

                  if( .not. ast_mapGet0A( result, key, obj1,
     :               status ) ) call stopit( status, 'checkDump 5' )
                  if( .not. ast_mapGet0A( obj, key, obj2,
     :               status ) ) call stopit( status, 'checkDump 6' )
                  if( ast_GetC( obj1, 'class', status ) .ne.
     :                ast_GetC( obj2, 'class', status ) ) then
                     call stopit( status, 'checkDump 7' )
                  end if

               else

                  if( .not. ast_mapGet0C( result, key, txt1, l1,
     :               status ) ) call stopit( status, 'checkDump 8' )
                  if( .not. ast_mapGet0C( obj, key, txt2, l2,
     :               status ) ) call stopit( status, 'checkDump 9' )
                  if( txt1( : l1 ) .ne. txt2( : l2 ) .or.
     :                l1 .ne. l2 ) then
                     call stopit( status, 'checkDump 10' )
                  end if

               end if
            end if
         end do
      end if

      end

      subroutine mysource( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, ll, nl
      character buf*400000

      common /ss1/ buf
      common /ss2/ next, end, ll,nl

      if( status .ne. sai__ok ) return

      if( next .ge. end ) then
         call ast_putline( buf, -1, status )
      else

c         write(*,*) buf( next : next + ll - 1 )
         call ast_putline( buf( next : ), ll, status )
         nl = nl + 1
      endif

      next = next + ll

      end

      subroutine mysink( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, f, l, ll, nl
      character buf*400000
      character line*1000

      common /ss1/ buf
      common /ss2/ next, end, ll, nl

      if( status .ne. sai__ok ) return

      line = ' '
      call ast_getline( line, l, status )
      call chr_fandl( line( : l ), f, l )
      buf( next : ) = line( f : l )
      l = l - f + 1

      if( next + ll - 1 .ge. 400000 ) then
         write(*,*)
         call stopit( status, 'Buffer overflow in mysink!!' )
      else if( l .gt. ll ) then
         write(*,*)
         write(*,*) buf( next : next + l)
         write(*,*) 'Line length ',l
         call stopit( status, 'Line overflow in mysink!!' )
      else
         end = next + l
         buf( end : next + ll - 1 ) = ' '
         nl = nl + 1
      endif

      next = next + ll

      end

