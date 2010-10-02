      program testkeymap
      implicit none
      include 'AST_PAR'
      include 'AST_ERR'
      include 'SAE_PAR'
      integer status,map,map2,ival,aval,l,ivec(2),avec(4),nval,i,iat,
     :        map1, map3, km2
      character cval*20,cvec(3)*10,key*20,cval0*40
      double precision dval, dvec(2)
      logical gota, gotc, gotd, goti, gotr, gotw, lval
      real rval
      integer*2 wval,wvec(2)

      status = sai__ok
      call err_mark( status )
      call ast_begin( status )

c      call ast_watchmemory( 29286 )

      call testsorting( status )

      map = ast_keymap( ' ', status )

      call ast_MapPut0w( map, 'Fredw', 1999, 'com 1', status )
      call ast_MapPut0i( map, 'Fredi', 1999, 'com 1', status )
      call ast_MapPut0d( map, 'Fredd', 1999.9D0, 'com2 ', status )
      call ast_MapPut0r( map, 'Fredr', 1999.9, 'com2 ', status )
      call ast_MapPut0c( map, 'Fredc', 'Hello', ' ', status )
      call ast_MapPut0A( map, 'Freda', ast_skyframe( ' ', status ),
     :                   ' ', status )

      if( ast_maplenc( map, 'Fredi', status ) .ne. 4 ) then
         write(*,*) ast_maplenc( map, 'Fredi', status )
         call stopit( status, 'Error -11' )
      end if

      if( ast_maplenc( map, 'Freda', status ) .ne. 0 ) then
         write(*,*) ast_maplenc( map, 'Freda', status )
         call stopit( status, 'Error -10' )
      end if

      if( ast_maplenc( map, 'Fredc', status ) .ne. 5 ) then
         write(*,*) ast_maplenc( map, 'Fredc', status )
         call stopit( status, 'Error -9' )
      end if

      if( ast_maptype( map, 'freda', status ) .ne. AST__BADTYPE) then
         call stopit( status, 'Error -8' )
      end if

      if( ast_maptype( map, 'Freda', status ) .ne. AST__OBJECTTYPE) then
         call stopit( status, 'Error -7' )
      end if

      if( ast_maptype( map, 'Fredc', status ) .ne. AST__STRINGTYPE) then
         call stopit( status, 'Error -6' )
      end if

      if( ast_maptype( map, 'Fredd', status ) .ne. AST__DOUBLETYPE) then
         call stopit( status, 'Error -5' )
      end if

      if( ast_maptype( map, 'Fredr', status ) .ne. AST__FLOATTYPE) then
         call stopit( status, 'Error -5b' )
      end if

      if( ast_maptype( map, 'Fredi', status ) .ne. AST__INTTYPE ) then
         call stopit( status, 'Error -4' )
      end if

      if( ast_maphaskey( map, 'fredi', status ) ) then
         call stopit( status, 'Error -3' )
      end if

      if( .not. ast_maphaskey( map, 'Fredi', status ) ) then
         call stopit( status, 'Error -2' )
      end if

      map2 = ast_copy( map, status )


      if( ast_mapsize( map2, status ) .ne. 6 ) then
         write(*,*) ast_mapsize( map2, status )
         call stopit( status, 'Error 0' )
      end if

      goti = .false.
      gotd = .false.
      gotr = .false.
      gotc = .false.
      gota = .false.
      gotw = .false.

      do i = 1, ast_mapsize( map2, status )
         key = ast_mapkey( map2, i, status )
         if( .not. goti .and. key .eq. 'Fredi' ) then
            goti = .true.
         else if( .not. gotd .and. key .eq. 'Fredd' ) then
            gotd = .true.
         else if( .not. gotw .and. key .eq. 'Fredw' ) then
            gotw = .true.
         else if( .not. gotr .and. key .eq. 'Fredr' ) then
            gotr = .true.
         else if( .not. gotc .and. key .eq. 'Fredc' ) then
            gotc = .true.
         else if( .not. gota .and. key .eq. 'Freda' ) then
            gota = .true.
         else
            call stopit( status, 'Error badkey' )
         endif
      end do

      if( .not. ( goti .AND. gotd .AND. gotc
     :            .AND. gota .and. gotr .and. gotw) ) then
         call stopit( status, 'Error nokey' )
      endif

      if( ast_maplength( map2, 'Fredi', status ) .ne. 1 ) then
         write(*,*) ast_maplength( map2, 'Fredi', status )
         call stopit( status, 'Error -1' )
      end if

      if( .not. ast_mapget0i( map2, 'Fredi', ival, status ) ) then
         call stopit( status, 'Error 1' )
      else if( ival .ne. 1999 ) then
         write(*,*) ival
         call stopit( status, 'Error 2' )
      end if

      if( .not. ast_mapget0w( map2, 'Fredw', wval, status ) ) then
         call stopit( status, 'Error 1' )
      else if( wval .ne. 1999 ) then
         write(*,*) ival
         call stopit( status, 'Error 2B' )
      end if

      if( .not. ast_mapget0d( map2, 'Fredd', dval, status ) ) then
         call stopit( status, 'Error 3' )
      else if( dval .ne. 1999.9D0 ) then
         write(*,*) dval - 1999.9D0
         call stopit( status, 'Error 4' )
      end if

      if( .not. ast_mapget0r( map2, 'Fredr', rval, status ) ) then
         call stopit( status, 'Error 3b' )
      else if( rval .ne. 1999.9 ) then
         write(*,*) rval - 1999.9
         call stopit( status, 'Error 4b' )
      end if

      if( .not. ast_mapget0c( map2, 'Fredc', cval, l, status ) ) then
         call stopit( status, 'Error 5' )
      else if( l .ne. 5  ) then
         write(*,*) l
         call stopit( status, 'Error 6' )
      else if( cval( :l ) .ne. 'Hello'  ) then
         write(*,*) cval( :l )
         call stopit( status, 'Error 7' )
      end if

      if( .not. ast_mapget0a( map2, 'Freda', aval, status ) ) then
         call stopit( status, 'Error 8' )
      else if( .not. ast_IsASkyFrame( aval, STATUS ) ) then
         call stopit( status, 'Error 9' )
      end if

      if( .not. ast_mapget0d( map2, 'Fredi', dval, status ) ) then
         call stopit( status, 'Error 10' )
      else if( dval .ne. 1999 ) then
         write(*,*) dval
         call stopit( status, 'Error 11' )
      end if

      if( .not. ast_mapget0r( map2, 'Fredi', rval, status ) ) then
         call stopit( status, 'Error 10b' )
      else if( rval .ne. 1999 ) then
         call stopit( status, 'Error 11b' )
      end if

      if( .not. ast_mapget0c( map2, 'Fredi', cval, l, status ) ) then
         call stopit( status, 'Error 12' )
      else if( l .ne. 4 ) then
         write(*,*) l
         call stopit( status, 'Error 13a' )
      else if( cval( :l ) .ne. '1999' ) then
         write(*,*) cval
         call stopit( status, 'Error 13' )
      end if

      if( .not. ast_mapget0i( map2, 'Fredd', ival, status ) ) then
         call stopit( status, 'Error 14' )
      else if( ival .ne. 2000.0 ) then
         write(*,*) ival
         call stopit( status, 'Error 15' )
      end if

      if( .not. ast_mapget0w( map2, 'Fredd', wval, status ) ) then
         call stopit( status, 'Error 14b' )
      else if( wval .ne. 2000.0 ) then
         write(*,*) wval
         call stopit( status, 'Error 15b' )
      end if

      if( .not. ast_mapget0c( map2, 'Fredd', cval, l, status ) ) then
         call stopit( status, 'Error 16' )
      else if( l .ne. 6 ) then
         write(*,*) l
         call stopit( status, 'Error 17a' )
      else if( cval( :l ) .ne. '1999.9' ) then
         write(*,*) cval
         call stopit( status, 'Error 17' )
      end if


      ivec(1) = -10
      ivec(2) = -10
      if( .not. ast_mapget1i( map2, 'Fredi', 2, nval, ivec,
     :                        status ) ) then
         call stopit( status, 'Error 18' )
      else if( nval .ne. 1 ) then
         write(*,*) nval
         call stopit( status, 'Error 19' )
      else if( ivec( 1 ) .ne. 1999 ) then
         write(*,*) ivec( 1 )
         call stopit( status, 'Error 20' )
      else if( ivec( 2 ) .ne. -10 ) then
         write(*,*) ivec( 2 )
         call stopit( status, 'Error 21' )
      end if


      dvec(1) = -10.0D0
      dvec(2) = -10.0D0
      if( .not. ast_mapget1d( map2, 'Fredd', 2, nval, dvec,
     :                        status ) ) then
         call stopit( status, 'Error 22' )
      else if( nval .ne. 1 ) then
         write(*,*) nval
         call stopit( status, 'Error 23' )
      else if( dvec( 1 ) .ne. 1999.9D0 ) then
         write(*,*) dvec( 1 )
         call stopit( status, 'Error 24' )
      else if( dvec( 2 ) .ne. -10.0D0 ) then
         write(*,*) dvec( 2 )
         call stopit( status, 'Error 25' )
      end if

      avec(1) = AST__NULL
      avec(2) = AST__NULL
      if( .not. ast_mapget1a( map2, 'Freda', 2, nval, avec,
     :                        status ) ) then
         call stopit( status, 'Error 26' )
      else if( nval .ne. 1 ) then
         write(*,*) nval
         call stopit( status, 'Error 27' )
      else if( .not. ast_IsASkyFrame( avec( 1 ), STATUS ) ) then
         write(*,*) ast_getc( avec( 1 ), 'class', status )
         call stopit( status, 'Error 28' )
      else if( avec( 2 ) .ne. AST__NULL ) then
         write(*,*) ast_getc( avec( 2 ), 'class', status )
         call stopit( status, 'Error 29' )
      end if


      ivec(1)=1999
      ivec(2)=0
      call ast_mapput1i( map, 'Fredi', 2, ivec, 'com 1', STATUS )

      if( ast_maplength( map, 'Fredi', status ) .ne. 2 ) then
         write(*,*) ast_maplength( map, 'Fredi', status )
         call stopit( status, 'Error 29b' )

      end if

      wvec(1)=1999
      wvec(2)=0
      call ast_mapput1w( map, 'Fredw', 2, wvec, 'com 1', STATUS )

      if( ast_maplength( map, 'Fredw', status ) .ne. 2 ) then
         write(*,*) ast_maplength( map, 'Fredw', status )
         call stopit( status, 'Error 29c' )

      end if

      dvec(1)=1999.9D0
      dvec(2)=-0.01D0
      call ast_mapput1d( map, 'Fredd', 2, dvec, 'com2', STATUS )

      cvec(1)='Hello'
      cvec(2)=' '
      cvec(3)='  Hello'
      call ast_mapput1c( map, 'Fredc', 3, cvec, ' ', STATUS )

      if( ast_maplenc( map, 'Fredc', status ) .ne. len(cvec(3)) ) then
         write(*,*) ast_maplenc( map, 'Fredc', status )
         call stopit( status, 'Error 29c' )
      end if

      avec(1) = ast_skyframe( ' ', status )
      avec(2) = AST__NULL
      avec(3) = ast_specframe( ' ', status )
      avec(4) = AST__NULL
      call ast_mapput1a( map, 'Freda', 4, avec, ' ', STATUS )

      map2 = ast_copy( map, status )

      if( .not. ast_mapget0i( map2, 'Fredi', ival, status ) ) then
         call stopit( status, 'Error A1' )
      else if( ival .ne. 1999 ) then
         write(*,*) ival
         call stopit( status, 'Error A2' )
      end if

      if( .not. ast_mapget0d( map2, 'Fredd', dval, status ) ) then
         call stopit( status, 'Error A3' )
      else if( dval .ne. 1999.9D0 ) then
         write(*,*) dval - 1999.9D0
         call stopit( status, 'Error A4' )
      end if

      if( .not. ast_mapget0c( map2, 'Fredc', cval, l, status ) ) then
         call stopit( status, 'Error A5' )
      else if( l .ne. 10  ) then
         write(*,*) l
         call stopit( status, 'Error A6' )
      else if( cval( :l ) .ne. 'Hello               '  ) then
         write(*,*) cval( :l )
         call stopit( status, 'Error A7' )
      end if

      if( .not. ast_mapget0a( map2, 'Freda', aval, status ) ) then
         call stopit( status, 'Error A8' )
      else if( .not. ast_IsASkyFrame( aval, STATUS ) ) then
         call stopit( status, 'Error A9' )
      end if

      if( .not. ast_mapget0d( map2, 'Fredi', dval, status ) ) then
         call stopit( status, 'Error A10' )
      else if( dval .ne. 1999 ) then
         write(*,*) dval
         call stopit( status, 'Error A11' )
      end if

      if( .not. ast_mapget0c( map2, 'Fredi', cval, l, status ) ) then
         call stopit( status, 'Error A12' )
      else if( l .ne. 4 ) then
         write(*,*) l
         call stopit( status, 'Error A13a' )
      else if( cval( :l ) .ne. '1999' ) then
         write(*,*) cval
         call stopit( status, 'Error A13' )
      end if

      if( .not. ast_mapget0i( map2, 'Fredd', ival, status ) ) then
         call stopit( status, 'Error A14' )
      else if( ival .ne. 2000.0 ) then
         write(*,*) ival
         call stopit( status, 'Error A15' )
      end if

      if( .not. ast_mapget0c( map2, 'Fredd', cval, l, status ) ) then
         call stopit( status, 'Error A16' )
      else if( l .ne. 6 ) then
         write(*,*) l
         call stopit( status, 'Error A17a' )
      else if( cval( :l ) .ne. '1999.9' ) then
         write(*,*) cval
         call stopit( status, 'Error A17' )
      end if


c  Read vector entries as vectors.
      if( .not. ast_mapget1i( map2, 'Fredi', 2, nval, ivec,
     :                        status ) ) then
         call stopit( status, 'Error B1' )
      else if( nval .ne. 2 ) then
         write(*,*) nval
         call stopit( status, 'Error B2a' )
      else if( ivec( 1 ) .ne. 1999 ) then
         write(*,*) ivec( 1 )
         call stopit( status, 'Error B2b' )
      else if( ivec( 2 ) .ne. 0 ) then
         write(*,*) ivec( 2 )
         call stopit( status, 'Error B2c' )
      end if

      if( .not. ast_mapget1d( map2, 'Fredd', 2, nval, dvec,
     :                        status ) ) then
         call stopit( status, 'Error B3' )
      else if( nval .ne. 2 ) then
         write(*,*) nval
         call stopit( status, 'Error B4a' )
      else if( dvec( 1 ) .ne. 1999.9D0 ) then
         write(*,*) dvec( 1 )
         call stopit( status, 'Error B4b' )
      else if( dvec( 2 ) .ne. -0.01D0 ) then
         write(*,*) dvec( 2 )
         call stopit( status, 'Error B4c' )
      end if

      if( .not. ast_mapget1a( map2, 'Freda', 4, nval, avec,
     :                        status ) ) then
         call stopit( status, 'Error B5' )
      else if( nval .ne. 4 ) then
         write(*,*) nval
         call stopit( status, 'Error B6a' )
      else if( .not. ast_isaskyframe( avec( 1 ), status ) ) then
         write(*,*) ast_getc( avec( 1 ), 'class', status )
         call stopit( status, 'Error B6b' )
      else if( avec( 2 ) .NE. AST__NULL ) then
         write(*,*) ast_getc( avec( 2 ), 'class', status )
         call stopit( status, 'Error B6c' )
      else if( .not. ast_isaspecframe( avec( 3 ), status ) ) then
         write(*,*) ast_getc( avec( 3 ), 'class', status )
         call stopit( status, 'Error B6d' )
      else if( avec( 4 ) .ne. AST__NULL ) then
         write(*,*) ast_getc( avec( 4 ), 'class', status )
         call stopit( status, 'Error B6e' )
      end if


      if( .not. ast_mapget1c( map2, 'Fredc', 3, nval, cvec,
     :                        status ) ) then
         call stopit( status, 'Error B7' )
      else if( nval .ne. 3 ) then
         write(*,*) nval
         call stopit( status, 'Error B8a' )
      else if( cvec( 1 ) .ne. 'Hello     ' ) then
         write(*,*) cvec( 1 )
         call stopit( status, 'Error B8b' )
      else if( cvec( 2 ) .ne. '          ' ) then
         write(*,*) cvec( 2 )
         call stopit( status, 'Error B8c' )
      else if( cvec( 3 ) .ne. '  Hello   ' ) then
         write(*,*) cvec( 2 )
         call stopit( status, 'Error B8d' )
      end if

c  Read single elements of vector entries as scalars.
      if( .not. ast_mapgetelemi( map2, 'Fredi', 1, ivec,
     :                           status ) ) then
         call stopit( status, 'Error B1z' )
      else if( ivec( 1 ) .ne. 1999 ) then
         write(*,*) ivec( 1 )
         call stopit( status, 'Error B2bz' )
      end if

      if( .not. ast_mapgetelemd( map2, 'Fredd', 2, dvec,
     :                           status ) ) then
         call stopit( status, 'Error B3z' )
      else if( dvec( 1 ) .ne. -0.01D0 ) then
         write(*,*) dvec( 1 )
         call stopit( status, 'Error B4cz' )
      end if

      if( .not. ast_mapgetelema( map2, 'Freda', 3, avec,
     :                           status ) ) then
         call stopit( status, 'Error B5z' )
      else if( .not. ast_isaspecframe( avec( 1 ), status ) ) then
         write(*,*) ast_getc( avec( 1 ), 'class', status )
         call stopit( status, 'Error B6dz' )
      end if


      if( .not. ast_mapgetelemc( map2, 'Fredc', 3, cval0,
     :                           status ) ) then
         call stopit( status, 'Error B7z' )
      else if( cval0 .ne. '  Hello   ' ) then
         write(*,*) cval0
         call stopit( status, 'Error B8dz' )
      end if


      call ast_mapremove( map2, 'Bert', status )
      call ast_mapremove( map2, 'Fredc', status )
      if( ast_mapget1c( map2, 'Fredc', 3, nval, cvec, status ) ) then
         call stopit( status, 'Error C1' )
      endif


      call checkDump( map2, 'checkDump 1 ', status )

      call ast_Annul( map, status  )
      call ast_Annul( map2, status  )


      map = ast_keymap( ' ', status )

      do i = 1, 500
         key = 'Fred'
         iat = 4
         call chr_puti( i, key, iat )
         call ast_MapPut0i( map, key, i, ' ', status )
      end do

      if( ast_mapsize( map, status ) .ne. 500 ) then
         call stopit( status, 'Error d1 ' )
      end if

      if( ast_maptype( map, 'Fred123', status ) .ne. AST__INTTYPE ) then
         call stopit( status, 'Error d2 ' )
      end if

      if( .not. ast_mapget0c( map, 'Fred489', cval, l, status ) ) then
         call stopit( status, 'Error d2 ' )
      else if( cval( : l ) .ne. '489' ) then
         call stopit( status, 'Error d3 ' )
      end if

      call checkDump( map, 'checkDump 2 ', status )



c  Test putting single elements into vector entries.
      map = ast_keymap( ' ', status )

      ivec(1) = 1
      ivec(2) = 2
      call ast_mapput1i( map, 'Fredi', 2, ivec, 'com 1', STATUS )

      call ast_mapputelemi( map, 'Fredi', 1, -1, STATUS )
      if( .not. ast_mapgetelemi( map, 'Fredi', 1, ival,
     :                           status ) ) then
         call stopit( status, 'Error GETELEM_1' )
      else if( ival .ne. -1 ) then
         write(*,*) ival
         call stopit( status, 'Error GETELEM_2' )
      end if

      call ast_mapputelemi( map, 'Fredi', 10, -2, STATUS )
      if( .not. ast_mapgetelemi( map, 'Fredi', 3, ival,
     :                           status ) ) then
         call stopit( status, 'Error GETELEM_3' )
      else if( ival .ne. -2 ) then
         write(*,*) ival
         call stopit( status, 'Error GETELEM_4' )
      end if

      call ast_mapputelemi( map, 'Fredi', 0, -3, STATUS )
      if( .not. ast_mapgetelemi( map, 'Fredi', 4, ival,
     :                           status ) ) then
         call stopit( status, 'Error GETELEM_5' )
      else if( ival .ne. -3 ) then
         write(*,*) ival
         call stopit( status, 'Error GETELEM_6' )
      end if

      if( ast_maplength( map, 'Fredi', status ) .ne. 4 ) then
         write(*,*) ast_maplength( map, 'Fredi', status )
         call stopit( status, 'Error GETELEM_7' )
      end if

      map2 = ast_keymap( ' ', status )
      call ast_mapputelema( map2, 'A A', 1, map, STATUS )
      if( ast_maplength( map2, 'A A', status ) .ne. 1 ) then
         write(*,*) ast_maplength( map, 'Fredi', status )
         call stopit( status, 'Error GETELEM_8' )
      end if

      if( .not. ast_mapgetelema( map2, 'A A', 1, map3,
     :                           status ) ) then
         call stopit( status, 'Error GETELEM_9' )
      else if( .not. ast_mapgetelemi( map3, 'Fredi', 4, ival,
     :                           status ) ) then
         call stopit( status, 'Error GETELEM_10' )
      else if( ival .ne. -3 ) then
         write(*,*) ival
         call stopit( status, 'Error GETELEM_11' )
      end if

      if( status .eq. sai__ok ) then
         call ast_mapputelema( map2, 'A A', 1, map2, STATUS )
         if( status .eq. ast__kycir ) then
            call err_annul( status )
         else
            call stopit( status, 'Error GETELEM_12' )
         end if
      end if

      call ast_mapput0c( map, ' B', 'Hello', ' ', status )


      call ast_setl( map, 'MapLocked', .TRUE., status )
      if( status .eq. sai__ok ) then
         call ast_mapput0c( map, ' BZZ', 'Bye Bye', ' ', STATUS )
         if( status .eq. AST__BADKEY ) then
            call err_annul( status )
            call ast_clear( map, 'maplocked', status )
         else
            call stopit( status, 'Error GETELEM_12B' )
         end if
      end if

      call ast_mapput0c( map, ' BZZ', 'Bye Bye', ' ', STATUS )
      km2 = ast_keymap( ' ', status )
      call ast_mapput0a( map, ' BZY', km2, ' ', STATUS )
      call ast_mapput0c( km2, ' BZZ', 'Bye Bye', ' ', STATUS )

      call ast_setl( map, 'MapLocked', .TRUE., status )
      call ast_mapput0c( map, ' BZZ', 'You Bye', ' ', STATUS )
      call ast_mapput0c( km2, ' BZZ', 'You Bye', ' ', STATUS )
      if( status .eq. sai__ok ) then
         call ast_mapput0c( km2, ' BZA', 'No Bye', ' ', STATUS )
         if( status .eq. AST__BADKEY ) then
            call err_annul( status )
            call ast_clear( map, 'maplocked', status )
            call ast_mapput0c( km2, ' BZA', 'No Bye', ' ', STATUS )
         else
            call stopit( status, 'Error GETELEM_12C' )
         end if
      end if

      if( ast_getl( km2, 'KeyError', status ) ) then
         call stopit( status, 'Error GETELEM_12D' )
      end if

      call ast_setl( map, 'KeyError', .TRUE., status )

      if( .not. ast_getl( km2, 'KeyError', status ) ) then
         call stopit( status, 'Error GETELEM_12E' )
      end if

      if( status .eq. sai__ok ) then
         lval = ast_mapget0c( km2, 'FRED', cval, l, status )
         if( status .eq. AST__MPKER ) then
            call err_annul( status )
            call ast_clear( map, 'keyerror', status )
            lval = ast_mapget0c( km2, 'FRED', cval, l, status )
         else
            call stopit( status, 'Error GETELEM_12F' )
         end if
      endif








      call ast_mapputelemc( map, ' B ', 3, 'YES YES', STATUS )

      if( ast_maplength( map, ' B', status ) .ne. 2 ) then
         write(*,*) ast_maplength( map, ' B', status )
         call stopit( status, 'Error GETELEM_13' )

      else if( .not. ast_mapgetelemc( map, ' B ', 2, cval0,
     :                                status ) ) then
         call stopit( status, 'Error GETELEM_14' )

      else if( cval0 .ne. 'YES YES' ) then
         write(*,*) cval0
         call stopit( status, 'Error GETELEM_15' )
      end if

      call ast_annul( map, status )


C  Test ast_mapcopy
      map = ast_keymap( ' ', status )
      map1 = ast_keymap( ' ', status )
      map2 = ast_keymap( ' ', status )
      map3 = ast_keymap( ' ', status )

      call ast_mapput0i( map1, 'a1', 1, ' ', status )
      call ast_mapput0i( map1, 'a2', 2, ' ', status )
      call ast_mapput0i( map1, 'a3', 3, ' ', status )

      call ast_mapput0c( map, 'aa1', 'Yes', ' ', status )
      call ast_mapput0i( map, 'aa2', 2, ' ', status )
      call ast_mapput0a( map, 'aa3', map1, ' ', status )

      call ast_mapput0i( map2, 'b1', 10, ' ', status )
      call ast_mapput0i( map2, 'b2', 20, ' ', status )
      call ast_mapput0i( map2, 'b3', 30, ' ', status )

      call ast_mapput0c( map3, 'bb1', 'No', ' ', status )
      call ast_mapput0i( map3, 'aa2', 20, ' ', status )
      call ast_mapput0a( map3, 'bb3', map2, ' ', status )

      call ast_mapcopy( map, map3, status )

      if( ast_mapsize( map, status ) .ne. 5 ) then
         write(*,*) ast_mapsize( map, status )
         call stopit( status, 'Error MAPCOPY_0' )
      end if

      if( .not. ast_mapget0c( map, 'aa1', cval, l, status ) ) then
         call stopit( status, 'Error MAPCOPY_1' )
      else if( cval .ne. 'Yes' ) then
         write(*,*) cval
         call stopit( status, 'Error MAPCOPY_2' )
      end if

      if( .not. ast_mapget0i( map, 'aa2', ival, status ) ) then
         call stopit( status, 'Error MAPCOPY_3' )
      else if( ival .ne. 20 ) then
         write(*,*) ival
         call stopit( status, 'Error MAPCOPY_4' )
      end if

      if( .not. ast_mapget0a( map, 'aa3', aval, status ) ) then
         call stopit( status, 'Error MAPCOPY_5' )
      else if( .not. ast_isakeymap( aval, status ) ) then
         write(*,*) ast_getc( aval, 'Class' )
         call stopit( status, 'Error MAPCOPY_6' )

         if( .not. ast_mapget0i( aval, 'a1', ival, status ) ) then
            call stopit( status, 'Error MAPCOPY_7' )
         else if( ival .ne. 1 ) then
            write(*,*) ival
            call stopit( status, 'Error MAPCOPY_8' )
         end if

         if( .not. ast_mapget0i( aval, 'a2', ival, status ) ) then
            call stopit( status, 'Error MAPCOPY_9' )
         else if( ival .ne. 20 ) then
            write(*,*) ival
            call stopit( status, 'Error MAPCOPY_10' )
         end if

         if( .not. ast_mapget0i( aval, 'a3', ival, status ) ) then
            call stopit( status, 'Error MAPCOPY_11' )
         else if( ival .ne. 3 ) then
            write(*,*) ival
            call stopit( status, 'Error MAPCOPY_12' )
         end if
      end if

      if( .not. ast_mapget0c( map, 'bb1', cval, l, status ) ) then
         call stopit( status, 'Error MAPCOPY_13' )
      else if( cval .ne. 'No' ) then
         write(*,*) cval
         call stopit( status, 'Error MAPCOPY_14' )
      end if

      if( .not. ast_mapget0a( map, 'bb3', aval, status ) ) then
         call stopit( status, 'Error MAPCOPY_15' )
      else if( .not. ast_isakeymap( aval, status ) ) then
         write(*,*) ast_getc( aval, 'Class' )
         call stopit( status, 'Error MAPCOPY_16' )

         if( .not. ast_mapget0i( aval, 'b1', ival, status ) ) then
            call stopit( status, 'Error MAPCOPY_17' )
         else if( ival .ne. 10 ) then
            write(*,*) ival
            call stopit( status, 'Error MAPCOPY_18' )
         end if

         if( .not. ast_mapget0i( aval, 'b2', ival, status ) ) then
            call stopit( status, 'Error MAPCOPY_19' )
         else if( ival .ne. 20 ) then
            write(*,*) ival
            call stopit( status, 'Error MAPCOPY_20' )
         end if

         if( .not. ast_mapget0i( aval, 'b3', ival, status ) ) then
            call stopit( status, 'Error MAPCOPY_21' )
         else if( ival .ne. 30 ) then
            write(*,*) ival
            call stopit( status, 'Error MAPCOPY_22' )
         end if
      end if


      map = ast_keymap( ' ', status )
      map1 = ast_keymap( ' ', status )
      map2 = ast_keymap( ' ', status )
      map3 = ast_keymap( ' ', status )

      call ast_mapput0i( map1, 'a1', 1, ' ', status )
      call ast_mapput0i( map1, 'a2', 2, ' ', status )
      call ast_mapput0i( map1, 'a3', 3, ' ', status )

      call ast_mapput0c( map, 'aa1', 'Yes', ' ', status )
      call ast_mapput0i( map, 'aa2', 2, ' ', status )
      call ast_mapput0a( map, 'aa3', map1, ' ', status )

      call ast_mapput0i( map2, 'b1', 10, ' ', status )
      call ast_mapput0i( map2, 'b2', 20, ' ', status )
      call ast_mapput0i( map2, 'b3', 30, ' ', status )

      call ast_mapput0i( map3, 'aa1', 0, ' ', status )
      call ast_mapput0i( map3, 'aa2', 20, ' ', status )
      call ast_mapput0a( map3, 'aa3', map2, ' ', status )

      call ast_mapcopy( map, map3, status )

      if( ast_mapsize( map, status ) .ne. 3 ) then
         write(*,*) ast_mapsize( map, status )
         call stopit( status, 'Error MAPCOPY_23' )
      end if

      if( .not. ast_mapget0i( map, 'aa1', ival, status ) ) then
         call stopit( status, 'Error MAPCOPY_24' )
      else if( ival .ne. 0 ) then
         write(*,*) ival
         call stopit( status, 'Error MAPCOPY_25' )
      end if

      if( .not. ast_mapget0i( map, 'aa2', ival, status ) ) then
         call stopit( status, 'Error MAPCOPY_26' )
      else if( ival .ne. 20 ) then
         write(*,*) ival
         call stopit( status, 'Error MAPCOPY_27' )
      end if

      if( .not. ast_mapget0a( map, 'aa3', aval, status ) ) then
         call stopit( status, 'Error MAPCOPY_28' )
      else if( .not. ast_isakeymap( aval, status ) ) then
         write(*,*) ast_getc( aval, 'Class' )
         call stopit( status, 'Error MAPCOPY_29' )

         if( .not. ast_mapget0i( aval, 'a1', ival, status ) ) then
            call stopit( status, 'Error MAPCOPY_30' )
         else if( ival .ne. 1 ) then
            write(*,*) ival
            call stopit( status, 'Error MAPCOPY_31' )
         end if

         if( .not. ast_mapget0i( aval, 'a2', ival, status ) ) then
            call stopit( status, 'Error MAPCOPY_32' )
         else if( ival .ne. 20 ) then
            write(*,*) ival
            call stopit( status, 'Error MAPCOPY_33' )
         end if

         if( .not. ast_mapget0i( aval, 'a3', ival, status ) ) then
            call stopit( status, 'Error MAPCOPY_34' )
         else if( ival .ne. 3 ) then
            write(*,*) ival
            call stopit( status, 'Error MAPCOPY_35' )
         end if

         if( .not. ast_mapget0i( aval, 'b1', ival, status ) ) then
            call stopit( status, 'Error MAPCOPY_36' )
         else if( ival .ne. 10 ) then
            write(*,*) ival
            call stopit( status, 'Error MAPCOPY_37' )
         end if

         if( .not. ast_mapget0i( aval, 'b2', ival, status ) ) then
            call stopit( status, 'Error MAPCOPY_38' )
         else if( ival .ne. 20 ) then
            write(*,*) ival
            call stopit( status, 'Error MAPCOPY_39' )
         end if

         if( .not. ast_mapget0i( aval, 'b3', ival, status ) ) then
            call stopit( status, 'Error MAPCOPY_40' )
         else if( ival .ne. 30 ) then
            write(*,*) ival
            call stopit( status, 'Error MAPCOPY_41' )
         end if

      end if


      map = ast_keymap( ' ', status )
      map1 = ast_keymap( ' ', status )
      map2 = ast_keymap( ' ', status )
      map3 = ast_keymap( ' ', status )

      call ast_mapput0i( map1, 'a1', 1, ' ', status )
      call ast_mapput0i( map1, 'a2', 2, ' ', status )
      call ast_mapput0i( map1, 'a3', 3, ' ', status )

      call ast_mapput0c( map, 'aa1', 'Yes', ' ', status )
      call ast_mapput0i( map, 'aa2', 2, ' ', status )
      call ast_mapput0a( map, 'aa3', map1, ' ', status )

      call ast_mapput0i( map2, 'b1', 10, ' ', status )
      call ast_mapput0i( map2, 'b2', 20, ' ', status )
      call ast_mapput0i( map2, 'b3', 30, ' ', status )

      call ast_mapput0i( map3, 'aa1', 0, ' ', status )
      call ast_mapput0i( map3, 'aa2', 20, ' ', status )
      call ast_mapput0a( map3, 'aa3', map2, ' ', status )

      call ast_setl( map, 'MapLocked', .TRUE., status )
      if( status .eq. SAI__OK ) then
         call ast_mapcopy( map, map3, status )
         if( status .eq. AST__BADKEY ) then
            call err_annul( status )
         else
            call stopit( status, 'Error MAPCOPY_42' )
         end if
      end if


C  Test AST_MAPPUTU and undefined values
      map = ast_keymap( ' ', status )
      call ast_mapputu( map, 'GG', 'A comment', status )

      if( ast_mapget0i( map, 'GG', ival, status ) ) then
         call stopit( status, 'Error UNDEF_1' )
      else if( ast_mapget0w( map, 'GG', wval, status ) ) then
         call stopit( status, 'Error UNDEF_1B' )
      else if( ast_mapget0c( map, 'GG', cval, l, status ) ) then
         call stopit( status, 'Error UNDEF_2' )
      else if( ast_mapget0a( map, 'GG', aval, status ) ) then
         call stopit( status, 'Error UNDEF_3' )
      else if( ast_mapget1i( map, 'GG', 2, nval, ivec,
     :                      status ) ) then
         call stopit( status, 'Error UNDEF_4' )
      else if( ast_mapgetelemc( map, 'gg', 1,  cval, status ) ) then
         call stopit( status, 'Error UNDEF_5' )
      else if( .not. ast_maphaskey( map, 'GG', status ) ) then
         call stopit( status, 'Error UNDEF_6' )
      end if

      if( ast_maptype( map, 'GG', status ) .ne. AST__UNDEFTYPE ) then
         write(*,*) ast_maptype( map, 'GG', status )
         call stopit( status, 'Error UNDEF_7' )
      else if( ast_mapsize( map, status ) .ne. 1 ) then
         call stopit( status, 'Error UNDEF_8' )
      end if

      call ast_mapput0i( map, 'GG', 0, ' ', status )
      if( .not. ast_mapget0i( map, 'GG', ival, status ) ) then
         call stopit( status, 'Error UNDEF_9' )
      else if( ival .ne. 0 ) then
         call stopit( status, 'Error UNDEF_10' )
      endif

      call ast_end( status )

      call ast_activememory( ' ' )
      call ast_flushmemory( 1 );

      call err_rlse( status )

      if( status .eq. sai__ok ) then
         write(*,*) 'All KeyMap tests passed'
      else
         write(*,*) 'KeyMap tests failed'
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
     :        i, type,obj1,obj2,l1,l2,nl
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



      subroutine testsorting( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      integer status, km, i
      character keys(5)*15
      character skeys(5)*15
      character key*( AST__SZCHR )


      data keys / 'ABC', 'zzzzzzzzzzz', 'this_is_a_key', 'HE-HE', 'A' /
      data skeys / 'A', 'ABC', 'HE-HE', 'this_is_a_key', 'zzzzzzzzzzz' /


      if( status .ne. sai__ok ) return

C  Age sorting...

C  First test adding entries into an already sorted KeyMap

      km = ast_keymap( 'Sortby=AgeDown', status )

      do i = 1, 5
         call ast_mapput0i( km, keys(i), i, ' ', status )
      end do

      do i = 1, 5
         key = ast_mapkey( km, i, status )
         if( key .ne. keys(i) .and. status .eq. SAI__OK ) then
            write(*,*) 'Key ',i,' is ',key,' (should be ',keys(i),')'
            call stopit( status, 'Error Sort 1' )
            return
         end if
      end do

C  Now test sorting existing entries in a KeyMap.
      call ast_set( km, 'Sortby=AgeUp', status )

      do i = 1, 5
         key = ast_mapkey( km, i, status )
         if( key .ne. keys(6-i) .and. status .eq. SAI__OK ) then
            write(*,*) 'Key ',i,' is ',key,' (should be ',keys(6-i),')'
            call stopit( status, 'Error Sort 2' )
            return
         end if
      end do

      call ast_annul( km, status )

C  Key sorting...

C  First test adding entries into an already sorted KeyMap

      km = ast_keymap( 'Sortby=KeyUp', status )

      do i = 1, 5
         call ast_mapput0i( km, keys(i), i, ' ', status )
      end do

      do i = 1, 5
         key = ast_mapkey( km, i, status )
         if( key .ne. skeys(i) .and. status .eq. SAI__OK ) then
            write(*,*) 'Key ',i,' is ',key,' (should be ',skeys(i),')'
            call stopit( status, 'Error Sort 3' )
            return
         end if
      end do

C  Now test sorting existing entries in a KeyMap.
      call ast_set( km, 'Sortby=KeyDown', status )

      do i = 1, 5
         key = ast_mapkey( km, i, status )
         if( key .ne. skeys(6-i) .and. status .eq. SAI__OK ) then
            write(*,*) 'Key ',i,' is ',key,' (should be ',skeys(6-i),')'
            call stopit( status, 'Error Sort 4' )
            return
         end if
      end do

      call ast_annul( km, status )




      end




