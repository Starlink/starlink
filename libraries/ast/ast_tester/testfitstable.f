      program testfitstable
      implicit none

      include 'AST_PAR'
      include 'AST_ERR'
      include 'SAE_PAR'
      include 'CNF_PAR'

      integer status, table, table2, dims( 7 ), header, ival, l, nval,
     :        icard, colsize, pntr
      byte bytes(2,3),bval
      logical wasset, hasnull
      real rval
      character cval*30, text(3)*10, card*70
      character header1(18)*30
      character header2(20)*30

      data header1 / 'XTENSION= ''BINTABLE''',
     :               'BITPIX  =                    8',
     :               'NAXIS   =                    2',
     :               'NAXIS1  =                   10',
     :               'NAXIS2  =                    0',
     :               'PCOUNT  =                    0',
     :               'GCOUNT  =                    1',
     :               'TFIELDS =                    3',
     :               'TFORM1  = ''6B      ''',
     :               'TTYPE1  = ''BYTECOL ''',
     :               'TUNIT1  = ''ADU     ''',
     :               'TDIM1   = ''(2,3)   ''',
     :               'TFORM2  = ''1J      ''',
     :               'TTYPE2  = ''INTCOL  ''',
     :               'TUNIT2  = ''m       ''',
     :               'TFORM3  = ''0A      ''',
     :               'TTYPE3  = ''STRINGCOL''',
     :               'TDIM3   = ''(0,3)   ''' /


      data header2 / 'XTENSION= ''BINTABLE''',
     :               'BITPIX  =                    8',
     :               'NAXIS   =                    2',
     :               'NAXIS1  =                   40',
     :               'NAXIS2  =                    3',
     :               'PCOUNT  =                    0',
     :               'GCOUNT  =                    1',
     :               'TFIELDS =                    3',
     :               'TFORM1  = ''6B      ''',
     :               'TTYPE1  = ''BYTECOL ''',
     :               'TUNIT1  = ''ADU     ''',
     :               'TNULL1  =                  254',
     :               'TDIM1   = ''(2,3)   ''',
     :               'TFORM2  = ''1J      ''',
     :               'TTYPE2  = ''INTCOL  ''',
     :               'TUNIT2  = ''m       ''',
     :               'TNULL2  =           2147483647',
     :               'TFORM3  = ''30A     ''',
     :               'TTYPE3  = ''STRINGCOL''',
     :               'TDIM3   = ''(10,3)  ''' /

c      call ast_watchmemory(483)

      status = sai__ok
      call err_mark( status )
      call ast_begin( status )



      table = ast_fitstable( ' ', status )


      header = ast_gettableheader( table, status )
      if( ast_geti( header, 'NCard', status ) .ne. 8 ) then
         call stopit( status, 'FitsTable error 1' )
      else if( .not. ast_getfitsi( header, 'NAXIS', ival, status )) then
         call stopit( status, 'FitsTable error 2' )
      else if( ival .ne. 2 ) then
         call stopit( status, 'FitsTable error 3' )
      else if( .not. ast_getfitsi( header, 'NAXIS1', ival, status)) then
         call stopit( status, 'FitsTable error 4' )
      else if( ival .ne. 0 ) then
         call stopit( status, 'FitsTable error 5' )
      else if( .not. ast_getfitsi( header, 'NAXIS2', ival, status)) then
         call stopit( status, 'FitsTable error 6' )
      else if( ival .ne. 0 ) then
         call stopit( status, 'FitsTable error 7' )
      endif
      call ast_annul( header, status )


      call ast_addcolumn( table, 'JUNK', AST__OBJECTTYPE, 0, 0, 'm',
     :                    status )
      if( status .eq. AST__NAXIN ) then
         call err_annul( status )
      else if( status .eq. sai__ok ) then
         call stopit( status, 'FitsTable error 8' )
      endif

      dims( 1 ) = 2
      dims( 2 ) = 3
      call ast_addcolumn( table, 'BYTECOL', AST__BYTETYPE, 2, dims,
     :                    'ADU', status )

      call ast_addcolumn( table, 'INTCOL', AST__INTTYPE, 0, 0, 'm',
     :                    status )

      dims( 1 ) = 3
      call ast_addcolumn( table, 'STRINGCOL', AST__STRINGTYPE, 1, dims,
     :                    ' ', status )


      header = ast_gettableheader( table, status )
      icard = 0
      do while( ast_findfits( header, '%f', card, .true., status ) )
         icard = icard + 1
         if( icard .gt. 18 ) then
            call stopit( status, 'FitsTable error 9' )
         else if( card .ne. header1( icard ) ) then
            call stopit( status, 'FitsTable error 10' )
         end if
      end do
      call ast_annul( header, status )
      if( icard .ne. 18 ) call stopit( status, 'FitsTable error 11' )


      bytes(1,1) = 0
      bytes(1,2) = 128
      bytes(1,3) = -127
      bytes(2,1) = 1
      bytes(2,2) = 127
      bytes(2,3) = -1
      call ast_mapput1b( table, 'BYTECOL(1)', 6, bytes, ' ', status )

      bytes(1,1) = 0
      bytes(1,2) = 0
      bytes(1,3) = 0
      bytes(2,1) = 1
      bytes(2,2) = 1
      bytes(2,3) = 1
      call ast_mapput1b( table, 'BYTECOL(2)', 6, bytes, ' ', status )

      call ast_mapput0i( table, 'INTCOL(2)', 10, ' ', status )

      call ast_mapput0i( table, 'INTCOL(3)', -10, ' ', status )

      text( 1 ) = 'hello'
      text( 2 ) = ' '
      text( 3 ) = 'goodbye'
      call ast_mapput1c( table, 'STRINGCOL(1)', 3, text, ' ', status )

      text( 1 ) = ' '
      text( 2 ) = ' '
      text( 3 ) = ' '
      call ast_mapput1c( table, 'STRINGCOL(3)', 3, text, ' ', status )

      if( ast_geti( table, 'Nrow', status ) .ne. 3 ) then
         call stopit( status, 'FitsTable error 12' )
      endif

      if( ast_geti( table, 'Ncolumn', status ) .ne. 3 ) then
         call stopit( status, 'FitsTable error 13' )
      endif

      colsize = ast_columnsize( table, 'stringcol', status )
      if( colsize .ne. 99 ) then
         call stopit( status, 'FitsTable error 13b' )
      else
         call psx_malloc( colsize, pntr, status )
         call ast_getcolumndata( table, 'StringCol', 0.0, 0.0D0,
     :                           colsize, %val( cnf_pval(pntr)),
     :                           colsize, status )
         if( colsize .ne. 9 ) call stopit( status,
     :                                     'FitsTable error 13c' )
         call checkstrings( table, %val( CNF_PVAL( pntr ) ), status )
         call psx_free( pntr, status )
      end if

      colsize = ast_columnsize( table, 'bytecol', status )
      if( colsize .ne. 18 ) then
         call stopit( status, 'FitsTable error 13d' )
      else
         call psx_malloc( colsize, pntr, status )
         call ast_getcolumndata( table, 'BYTECOL', 0.0, 0.0D0, colsize,
     :                           %val( cnf_pval( pntr ) ), colsize,
     :                           status )
         if( colsize .ne. 18 ) call stopit( status,
     :                                      'FitsTable error 13e' )
         call checkbytes( table, %val( CNF_PVAL( pntr ) ),
     :                    ast_columnnull( table, 'BYTECOL', .FALSE., 0,
     :                                   wasset, hasnull, status ),
     :                    status )
         call psx_free( pntr, status )
      end if

      colsize = ast_columnsize( table, 'intcol', status )
      if( colsize .ne. 12 ) then
         call stopit( status, 'FitsTable error 13f' )
      else
         call psx_malloc( colsize, pntr, status )
         call ast_getcolumndata( table, 'INTCOL', 0.0, 0.0D0, colsize,
     :                           %val( cnf_pval( pntr ) ), colsize,
     :                           status )
         if( colsize .ne. 3 ) call stopit( status,
     :                                    'FitsTable error 13g' )
         call checkints( table, %val( CNF_PVAL( pntr ) ),
     :                    ast_columnnull( table, 'INTCOL', .FALSE., 0,
     :                                   wasset, hasnull, status ),
     :                    status )
         call psx_free( pntr, status )
      end if


      call ast_addcolumn( table, 'REALCOL', AST__FLOATTYPE, 0, 0, ' ',
     :                    status )
      call ast_mapput0r( table, 'REALCOL(1)', -10.0, ' ', status )
      call ast_mapput0r( table, 'REALCOL(3)', 10.0, ' ', status )

      colsize = ast_columnsize( table, 'realcol', status )
      if( colsize .ne. 12 ) then
         call stopit( status, 'FitsTable error 13h' )
      else
         call psx_malloc( colsize, pntr, status )
         call ast_getcolumndata( table, 'REALCOL', -1.0, 0.0D0, colsize,
     :                           %val( cnf_pval( pntr ) ), colsize,
     :                           status )
         if( colsize .ne. 3 ) call stopit( status,
     :                                    'FitsTable error 13i' )
         call checkreals( table, %val( CNF_PVAL( pntr ) ), -1.0,
     :                    status )
         call psx_free( pntr, status )
      end if

      call ast_removecolumn( table, 'REALCOL', status )




      call ast_mapremove( table, 'BYTECOL(3)', status )
      call ast_mapremove( table, 'INTCOL(3)', status )
      call ast_mapremove( table, 'STRINGCOL(3)', status )

      if( ast_geti( table, 'Nrow', status ) .ne. 3 ) then
         call stopit( status, 'FitsTable error 14' )
      endif

      if( ast_geti( table, 'Ncolumn', status ) .ne. 3 ) then
         call stopit( status, 'FitsTable error 15' )
      endif




      header = ast_gettableheader( table, status )
      icard = 0
      do while( ast_findfits( header, '%f', card, .true., status ) )
         icard = icard + 1
         if( icard .gt. 20 ) then
            call stopit( status, 'FitsTable error 16' )
         else if( card .ne. header2( icard ) ) then
            call stopit( status, 'FitsTable error 17' )
         end if
      end do
      call ast_annul( header, status )
      if( icard .ne. 20 ) call stopit( status, 'FitsTable error 18' )



      if( ast_columnnull( table, 'BYTECOL', .FALSE., 0, wasset,
     :                    hasnull, status ) .ne. 254 ) then
         call stopit( status, 'FitsTable error 19' )
      else if( wasset ) then
         call stopit( status, 'FitsTable error 20' )
      else if( .not. hasnull ) then
         call stopit( status, 'FitsTable error 21' )
      end if



      call ast_purgerows( table, status )
      if( ast_geti( table, 'Nrow', status ) .ne. 2 ) then
         call stopit( status, 'FitsTable error 22' )
      endif

      if( ast_geti( table, 'Ncolumn', status ) .ne. 3 ) then
         call stopit( status, 'FitsTable error 23' )
      endif

      header = ast_gettableheader( table, status )
      if( ast_getfitsi( header, 'TNULL1', ival, status ) ) then
         call stopit( status, 'FitsTable error 24' )
      endif
      call ast_annul( header, status )

      if( ast_columnnull( table, 'BYTECOL', .TRUE., 11, wasset,
     :                    hasnull, status ) .ne. 11 ) then
         call stopit( status, 'FitsTable error 25' )
      else if( wasset ) then
         call stopit( status, 'FitsTable error 26' )
      else if( hasnull ) then
         call stopit( status, 'FitsTable error 27' )
      end if

      if( ast_columnnull( table, 'BYTECOL', .FALSE., 0, wasset,
     :                    hasnull, status ) .ne. 11 ) then
         call stopit( status, 'FitsTable error 28' )
      else if( .not. wasset ) then
         call stopit( status, 'FitsTable error 29' )
      else if( hasnull ) then
         call stopit( status, 'FitsTable error 30' )
      end if


      table2 = ast_copy( table, status )

      call ast_end( status )
      call err_rlse( status )

c      call ast_activememory( 'testfitstable' )
      call ast_flushmemory( 1 )

      if( status .eq. sai__ok ) then
         write(*,*) 'All FitsTable tests passed'
      else
         write(*,*) 'FitsTable tests failed'
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



      subroutine checkbytes( table, vals, null, status )
      implicit none
      include 'SAE_PAR'
      integer status, table, null, i
      byte vals( * ), ans( 12 ), bnull

      data ans / 0, 1, 128, 127, -127, -1, 0, 1, 0, 1, 0, 1 /

      if( status .ne. sai__ok ) return

      do i = 1, 12
         if( vals( i ) .ne. ans( i ) ) then
            call stopit( status, 'FitsTable error checkbytes 1' )
         end if
      end do

      bnull = null
      do i = 13, 18
         if( vals( i ) .ne. bnull ) then
            call stopit( status, 'FitsTable error checkbytes 2' )
         end if
      end do

      end

      subroutine checkints( table, vals, null, status )
      implicit none
      include 'SAE_PAR'
      integer status, table, null
      integer vals( * )

      if( status .ne. sai__ok ) return

      if( vals( 1 ) .ne. null ) then
         call stopit( status, 'FitsTable error checkints 1' )
      end if

      if( vals( 2 ) .ne. 10 ) then
         call stopit( status, 'FitsTable error checkints 2' )
      end if

      if( vals( 3 ) .ne. -10 ) then
         call stopit( status, 'FitsTable error checkints 3' )
      end if

      end

      subroutine checkreals( table, vals, null, status )
      implicit none
      include 'SAE_PAR'
      integer status, table
      real vals( * ), null

      if( status .ne. sai__ok ) return

      if( vals( 1 ) .ne. -10.0 ) then
         call stopit( status, 'FitsTable error checkreals 1' )
      end if

      if( vals( 2 ) .ne. null ) then
         call stopit( status, 'FitsTable error checkreals 2' )
      end if

      if( vals( 3 ) .ne. 10.0 ) then
         call stopit( status, 'FitsTable error checkreals 3' )
      end if

      end

      subroutine checkstrings( table, vals, status )
      implicit none
      include 'SAE_PAR'
      integer status, table, i, start, end, j
      character ans( 9 )*10
      character vals*( * ), test*11

      data ans / 'hello', ' ', 'goodbye', '', '', '', ' ', ' ', ' ' /

      if( status .ne. sai__ok ) return

      start = 1
      end = 11

      do i = 1, 9

         do j = 1, 11
            if( vals( start + j - 1 : start + j - 1 ) .lt. ' ' ) then
               vals( start + j - 1 : start + j - 1 ) = ' '
            endif
         end do

         if( vals( start : end ) .ne. ans( i ) ) then
            call stopit( status, 'FitsTable error checkstrings 1' )
         end if

         start = start + 11
         end = end + 11

      end do

      end




