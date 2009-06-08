      program teststc
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'PRM_PAR'



      integer status

      status = sai__ok


c      call ast_SetWatchId( 565300 )

      call ast_begin( status )
      call Example5( status )
      call Example1( status )
      call Example1b( status )
      call Example4( status )
      call misc( status )
      call Example3( status )
      call Example2( status )
      call ast_end( status )

c      call ast_listissued( 'teststc' )


      if( status .eq. sai__ok ) then
         write(*,*) 'All Stc tests passed'
      else
         write(*,*) 'Stc tests failed'
      end if

      end


      subroutine misc( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'AST_ERR'


      integer status, obj1, obj2, overlap
      double precision x, y, xo, yo

      if( status .ne. sai__ok ) return

      call ast_begin( status )

      call puteg( 'teststc_eg6', 1, status )
      call xmlread( 1, obj1, ' ', status )
      obj1 = ast_simplify( obj1, status )
      call checkdump( obj1, 'checkdump 1', status ) 

      call puteg( 'teststc_eg7', 1, status )
      call xmlread( 1, obj2, ' ', status )
      obj2 = ast_simplify( obj2, status )
      call checkdump( obj2, 'checkdump 2', status ) 

      overlap = ast_overlap( obj1, obj2, status )
      if( overlap .ne. 4 .and.status .eq. sai__ok ) then
         write(*,*) 'Overlap is ',overlap,' (should be 4)'
         call stopit( status, 'Error 1' )
      endif

      call puteg( 'teststc_eg8', 1, status )
      call xmlread( 1, obj2, ' ', status )
      obj2 = ast_simplify( obj2, status )
      call checkdump( obj2, 'checkdump 3', status ) 

      overlap = ast_overlap( obj1, obj2, status )
      if( overlap .ne. 3 .and.status .eq. sai__ok ) then
         write(*,*) 'Overlap is ',overlap,' (should be 3)'
         call stopit( status, 'Error 2' )
      endif

      overlap = ast_overlap( obj2, obj1, status )
      if( overlap .ne. 2 .and.status .eq. sai__ok ) then
         write(*,*) 'Overlap is ',overlap,' (should be 2)'
         call stopit( status, 'Error 3' )
      endif

      call puteg( 'teststc_eg9', 1, status )
      call xmlread( 1, obj2, ' ', status )

      overlap = ast_overlap( obj1, obj2, status )
      if( overlap .ne. 1 .and.status .eq. sai__ok ) then
         write(*,*) 'Overlap is ',overlap,' (should be 1)'
         call stopit( status, 'Error 4' )
      endif


      call puteg( 'teststc_eg10', 1, status )
      call xmlread( 1, obj2, ' ', status )

      x = 2.4958208
      y = 0.73303829
      call ast_tran2( obj2, 1, x, y, .true., xo, yo, status )
      if( xo .ne. 2.4958208 .or. yo .ne. 0.73303829 ) then
         call stopit( status, 'Error 5' )
      end if

      x = 2.4958208
      y = -0.73303829
      call ast_tran2( obj2, 1, x, y, .true., xo, yo, status )
      if( xo .ne. AST__BAD .or. yo .ne. AST__BAD ) then
         call stopit( status, 'Error 6' )
      end if




      call ast_end( status )

      if( status .ne. sai__ok ) write(*,*) 'teststc: miscellaneous '//
     :                                     'tests failed'

      end

      subroutine Example1( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'AST_ERR'

      integer status, obj2, obj, i, j, unc, km, nval, fs, m, r, f,
     :        axes(2), map
      double precision in(8,4), out(8,4), lbnd(5), ubnd(5)
      character cvals(10)*30

      if( status .ne. sai__ok ) return

      call ast_begin( status )

*  Put an example of an STCResourceProfile into file 1.
      call puteg( 'teststc_eg1', 1, status )

*  Use a new XmlChan to read an object from file 1,and simplify it.
      call xmlread( 1, obj, ' ', status )
      obj = ast_simplify( obj, status )

*  Write out the object through a Channel and read it back.
      call checkdump( obj, 'checkdump 1', status ) 

*  Test simplify by negating and simplifying twice.
      call ast_negate( obj, status )
      obj = ast_simplify( obj, status )
      call ast_negate( obj, status )
      obj = ast_simplify( obj, status )

*  Check it is a STCResourceProfile
      if( .not. ast_isastcresourceprofile( obj, status ) ) 
     :                      call stopit( status, 'Error 1' )

*  Check it contains an Interval.
      if( .not. ast_isainterval( ast_getstcregion( obj, status ),
     :                           status  ) )
     :                      call stopit( status, 'Error 1a' )

*  Timescale should be tt. Try changing it to TAI.
      if( ast_getc( obj, 'timescale', status ) .ne. 'TT' ) 
     :    call stopit( status, 'Error 0a' )

      if( abs( ast_getd(obj,'TimeOrigin',status)-51382.6666666D0 ) 
     :   .gt. 1.0D-7 ) call stopit( status, 'Error 0b' )

      call ast_getregionbounds( obj, lbnd, ubnd, status )
      if( lbnd(3) .ne. 0.0 ) call stopit( status, 'Error 0c' )
      
      call ast_set( obj, 'timescale=tai', status )
      if( ast_getc( obj, 'timescale', status ) .ne. 'TAI' ) 
     :    call stopit( status, 'Error 0d' )
      
      if( abs( ast_getd(obj,'TimeOrigin',status)-51382.6662941667D0 ) 
     :   .gt. 1.0D-7 ) call stopit( status, 'Error 0e' )

      call ast_getregionbounds( obj, lbnd, ubnd, status )
      if( abs( lbnd(3) ) .gt. 1.0D-6 ) THEN
         write(*,*) lbnd(3)
         call stopit( status, 'Error 0f' )
      END IF


      call ast_set( obj, 'timescale=tt', status )
      obj = ast_Simplify( obj, status )
      if( ast_getc( obj, 'timescale', status ) .ne. 'TT' ) 
     :    call stopit( status, 'Error 0g' )

      if( abs( ast_getd(obj,'TimeOrigin',status)-51382.6666666D0 ) 
     :   .gt. 1.0D-7 ) call stopit( status, 'Error 0h' )

      call ast_getregionbounds( obj, lbnd, ubnd, status )
      if( abs( lbnd(3) ) .gt. 1.0D-6 ) 
     :       call stopit( status, 'Error 0i' )

          

* Other tests
      if( ast_getd( obj, 'fillfactor', status ) .ne. 0.02D0 ) 
     :    call stopit( status, 'Error 1b' )

      if( ast_getc( obj, 'ident', status ) .ne. 'AllSky-CXO' ) 
     :    call stopit( status, 'Error 1c' )

      if( ast_getc( obj, 'domain(3)', status ) .ne. 'TIME' ) 
     :    call stopit( status, 'Error 2' )

      if( ast_getc( obj, 'title(3)', status ) .ne. 'Time' )
     :    call stopit( status, 'Error 2a' )

      if( ast_getc( obj, 'label(3)', status ) .ne. 
     :    'Modified Julian Date offset from 1999-07-23 16:00:00' ) THEN
         call stopit( status, 'Error 2b' )
      end if

      if( ast_getc( obj, 'domain(1)', status ) .ne. 'SKY' )
     :    call stopit( status, 'Error 3' )

      if( ast_getc( obj, 'system(1)', status ) .ne. 'ICRS' )
     :    call stopit( status, 'Error 3a' )

      if( ast_getc( obj, 'label(1)', status ) .ne. 'Right ascension' )
     :    call stopit( status, 'Error 3b' )

      if( ast_getc( obj, 'label(2)', status ) .ne. 'Declination' )
     :    call stopit( status, 'Error 3c' )

      if( ast_getc( obj, 'title(2)', status ) .ne. 'Space' ) 
     :    call stopit( status, 'Error 3d' )

      if( ast_getc( obj, 'domain(2)', status ) .ne. 'SKY' )
     :    call stopit( status, 'Error 4' )

      if( ast_getc( obj, 'domain(4)', status ) .ne. 'SPECTRUM' ) 
     :    call stopit( status, 'Error 5' )

      if( ast_getc( obj, 'system(4)', status ) .ne. 'ENER' )
     :    call stopit( status, 'Error 5a' )

      if( ast_getc( obj, 'stdofrest', status ) .ne. 'Topocentric' )
     :    call stopit( status, 'Error 5b' )

      if( ast_getc( obj, 'title(4)', status ) .ne. 
     :    'Energy (Topocentric)' ) call stopit( status, 'Error 5c' )

      if( ast_getc( obj, 'unit(4)', status ) .ne. 'keV' )
     :    call stopit( status, 'Error 5d' )

      if( ast_geti( obj, 'naxes', status ) .ne. 4 ) 
     :    call stopit( status, 'Error 6' )

      in(1,1) = 10.0
      in(1,2) = 10.0
      in(1,3) = -0.1
      in(1,4) = 0.11

      in(2,1) = -10.0
      in(2,2) = 10.0
      in(2,3) = 0.1
      in(2,4) = 0.11

      in(3,1) = 0.0                 ! inside
      in(3,2) = 0.0
      in(3,3) = 100.0
      in(3,4) = 0.13

      in(4,1) = -1.0
      in(4,2) = 1.0
      in(4,3) = -100.0
      in(4,4) = 0.13

      in(5,1) = 10.0
      in(5,2) = 10.0
      in(5,3) = -1000.0
      in(5,4) = 9.9

      in(6,1) = -10.0               ! inside
      in(6,2) = 10.0
      in(6,3) = 1000.0
      in(6,4) = 9.9

      in(7,1) = 0.0
      in(7,2) = 0.0
      in(7,3) = 10.0
      in(7,4) = 10.1

      in(8,1) = -1.0
      in(8,2) = 1.0
      in(8,3) = -10.0
      in(8,4) = 10.1

      call ast_trann( obj, 8, 4, 8, in, .true., 4, 8, out, status )

      do i = 1, 8
         if( i .eq. 3 .or. i .eq. 6 ) then
            do j = 1, 4
               if( out(i,j) .ne. in(i,j) ) then
                  if( status .eq. sai__ok ) then 
                     write(*,*) i,j,out(i,j),in(i,j)
                     call stopit( status, 'Error 7' )
                  end if
               end if
            end do           
         else
            do j = 1, 4
               if( out(i,j) .ne. AST__BAD ) then
                  if( status .eq. sai__ok ) then 
                     write(*,*) i,j,out(i,j),in(i,j)
                     call stopit( status, 'Error 8' )
                  end if
               end if
            end do           
         end if
      end do

* AstroCoords
      if( ast_getstcncoord( obj, status ) .ne. 1 ) then
         call stopit( status, 'Error 25' )
      end if
      km = ast_getstccoord( obj, 1, status )

      if( ast_mapsize( km, status ) .ne. 4 ) then
         call stopit( status, 'Error 25b' )
      endif

      if( .not. ast_mapget0A( km, AST__STCERROR, r, status ) ) then
         call stopit( status, 'Error 26' )
      else if( .not.ast_isabox( r, status ) ) then
         call stopit( status, 'Error 27' )
      else if( ast_geti( r, 'naxes', status ) .ne. 4 ) then
         call stopit( status, 'Error 28' )
      else 
         fs = ast_convert( obj, r, ' ', status )              
         if( fs .eq. AST__NULL ) then
            call stopit( status, 'Error 29' )
         else 
            m = ast_getMapping( fs, AST__BASE, AST__CURRENT, status )
            m = ast_simplify( m, status )
            if( .not. ast_isaunitmap( m, status ) ) then
               call stopit( status, 'Error 30' )
            endif
         end if


         call ast_getregionbounds( r, lbnd, ubnd, status )

         if( abs( lbnd(1)+2.42406841E-06 ) .gt. 0.0001E-6 ) 
     :       call stopit( status, 'Error 31a' )
         if( abs( ubnd(1)-2.42406841E-06 ) .gt. 0.0001E-6 ) 
     :       call stopit( status, 'Error 31b' )
         if( abs( lbnd(2)+2.42406841E-06 ) .gt. 0.0001E-6 ) 
     :       call stopit( status, 'Error 31c' )
         if( abs( ubnd(2)-2.42406841E-06 ) .gt. 0.0001E-6 ) 
     :       call stopit( status, 'Error 31d' )
         if( abs( 0.5*(ubnd(3)+lbnd(3)) ) .gt. 1.0E-10) 
     :       call stopit( status, 'Error 31e' )
         if( abs( 0.5*(ubnd(3)-lbnd(3))-0.578703703718D-09 ) .gt. 
     :       1.0E-15 ) call stopit( status, 'Error 31e2' )
         if( abs( lbnd(4)-5.01 ) .gt. 0.00001 ) 
     :       call stopit( status, 'Error 31g' )
         if( abs( ubnd(4)-5.11 ) .gt. 0.00001 ) 
     :       call stopit( status, 'Error 31h' )

      end if


      if( .not. ast_mapget1C( km, AST__STCNAME, 6, nval, cvals, 
     :                        status ) ) then
         call stopit( status, 'Error 32' )

      else if( nval .ne. 4 ) then
         call stopit( status, 'Error 33' )
      else
         if( cvals(1) .ne. 'Position' ) 
     :        call stopit( status, 'Error 34a' )
         if( cvals(2) .ne. 'Position' ) 
     :        call stopit( status, 'Error 34b' )
         if( cvals(3) .ne. 'Time' ) 
     :        call stopit( status, 'Error 34c' )
         if( cvals(4) .ne. 'Energy' ) 
     :        call stopit( status, 'Error 34d' )
      end if

      if( .not. ast_mapget0A( km, AST__STCRES, r, status ) ) then
         call stopit( status, 'Error 35' )
      else if( .not.ast_isabox( r, status ) ) then
         call stopit( status, 'Error 36' )
      else if( ast_geti( r, 'naxes', status ) .ne. 4 ) then
         call stopit( status, 'Error 37' )
      else 
         fs = ast_convert( obj, r, ' ', status )              
         if( fs .eq. AST__NULL ) then
            call stopit( status, 'Error 38' )
         else 
            m = ast_getMapping( fs, AST__BASE, AST__CURRENT, status )
            m = ast_simplify( m, status )
            if( .not. ast_isaunitmap( m, status ) ) then
               call stopit( status, 'Error 39' )
            endif
         end if

         call ast_getregionbounds( r, lbnd, ubnd, status )

         if( abs( lbnd(1)+1.2120342E-06 ) .gt. 0.0001E-6 ) 
     :       call stopit( status, 'Error 40a' )
         if( abs( ubnd(1)-1.2120342E-06 ) .gt. 0.0001E-6 ) 
     :       call stopit( status, 'Error 40b' )
         if( abs( lbnd(2)+1.2120342E-06 ) .gt. 0.0001E-6 ) 
     :       call stopit( status, 'Error 40c' )
         if( abs( ubnd(2)-1.2120342E-06 ) .gt. 0.0001E-6 ) 
     :       call stopit( status, 'Error 40d' )

         if( abs( 86400.0D0*(ubnd(3)-lbnd(3))-1.6D-5 ) .gt. 1.0E-10 ) 
     :       call stopit( status, 'Error 40e' )
         if( abs( 0.5*(ubnd(3)+lbnd(3)) ) .gt. 1.0E-10 ) 
     :       call stopit( status, 'Error 40f' )
         if( abs( lbnd(4)-5.05 ) .gt. 0.00001 ) 
     :       call stopit( status, 'Error 40g' )
         if( abs( ubnd(4)-5.07 ) .gt. 0.00001 ) 
     :       call stopit( status, 'Error 40h' )

      end if

      if( .not. ast_mapget0A( km, AST__STCSIZE, r, status ) ) then
         call stopit( status, 'Error 41' )
      else if( .not.ast_isabox( r, status ) ) then
         call stopit( status, 'Error 42' )
      else if( ast_geti( r, 'naxes', status ) .ne. 4 ) then
         call stopit( status, 'Error 43' )
      else 
         fs = ast_convert( obj, r, ' ', status )              
         if( fs .eq. AST__NULL ) then
            call stopit( status, 'Error 44' )
         else 
            m = ast_getMapping( fs, AST__BASE, AST__CURRENT, status )
            m = ast_simplify( m, status )
            if( .not. ast_isaunitmap( m, status ) ) then
               call stopit( status, 'Error 45' )
            endif
         end if

         call ast_getregionbounds( r, lbnd, ubnd, status )

         if( abs( lbnd(1)+0.00242406841 ) .gt. 0.01E-6 ) 
     :       call stopit( status, 'Error 46a' )
         if( abs( ubnd(1)-0.00242406841 ) .gt. 0.01E-6 ) 
     :       call stopit( status, 'Error 46b' )
         if( abs( lbnd(2)+0.00242406841 ) .gt. 0.01E-6 ) 
     :       call stopit( status, 'Error 46c' )
         if( abs( ubnd(2)-0.00242406841 ) .gt. 0.01E-6 ) 
     :       call stopit( status, 'Error 46d' )
         if( abs( 86400.0D0*(ubnd(3)-lbnd(3))- 1000.0 ) .gt. 1.0E-10 ) 
     :       call stopit( status, 'Error 46e' )
         if( abs( 0.5*(ubnd(3)+lbnd(3)) ) .gt. 1.0E-10 ) 
     :       call stopit( status, 'Error 46f' )
         if( abs( lbnd(4)-4.06 ) .gt. 0.001 ) 
     :       call stopit( status, 'Error 46g' )
         if( abs( ubnd(4)-6.06 ) .gt. 0.001 ) 
     :       call stopit( status, 'Error 46h' )

      end if






      obj2 = ast_Copy( obj, status )

      call ast_setl( obj2, 'Adaptive', .false., status )
      call ast_setc( obj2, 'epoch', '2005', status )
      call ast_clear( obj2, 'Adaptive', status )

      call ast_setc( obj2, 'system(1)', 'galactic', status )

      if( ast_getstcncoord( obj2, status ) .ne. 1 ) then
         call stopit( status, 'Error 25b' )
      end if
      km = ast_getstccoord( obj2, 1, status )

      if( ast_mapsize( km, status ) .ne. 3 ) then
          call stopit( status, 'Error 25bb' )
      end if

      if( .not. ast_mapget0A( km, AST__STCERROR, r, status ) ) then
         call stopit( status, 'Error 26b' )
      else if( .not.ast_isabox( r, status ) ) then
         call stopit( status, 'Error 27b' )
      else if( ast_geti( r, 'naxes', status ) .ne. 4 ) then
         call stopit( status, 'Error 28b' )
      else 
         fs = ast_convert( obj2, r, ' ', status )              
         if( fs .eq. AST__NULL ) then
            call stopit( status, 'Error 29b' )
         else 
            m = ast_getMapping( fs, AST__BASE, AST__CURRENT, status )
            m = ast_simplify( m, status )
            if( .not. ast_isaunitmap( m, status ) ) then
               call stopit( status, 'Error 30b' )
            endif
         end if

         call ast_getregionbounds( r, lbnd, ubnd, status )

         if( abs( lbnd(1)-1.68139639 ) .gt. 1.0E-7 ) 
     :       call stopit( status, 'Error 31ab' )
         if( abs( ubnd(1)-1.68140922 ) .gt. 1.0E-7 ) 
     :       call stopit( status, 'Error 31bb' )
         if( abs( lbnd(2)+1.05049161 ) .gt. 1.0E-7 ) 
     :       call stopit( status, 'Error 31cb' )
         if( abs( ubnd(2)+1.05048523 ) .gt. 1.0E-7 ) 
     :       call stopit( status, 'Error 31db' )
         if( abs( 0.5*86400.0D0*(ubnd(3)-lbnd(3))- 5.0D-5) .gt. 1.0E-10) 
     :       call stopit( status, 'Error 31eb' )
         if( abs( 0.5*86400.0D0*(ubnd(3)+lbnd(3))) .gt. 1.0E-10 ) 
     :       call stopit( status, 'Error 31fb' )
         if( abs( lbnd(4)-5.01 ) .gt. 0.000001 ) 
     :       call stopit( status, 'Error 31gb' )
         if( abs( ubnd(4)-5.11 ) .gt. 0.000001 ) 
     :       call stopit( status, 'Error 31hb' )

      end if


      if( ast_mapget1C( km, AST__STCNAME, 6, nval, cvals, status )) 
     :    call stopit( status, 'Error 32b' )


*  Uncertainty tests

      unc = ast_getunc( obj, .true., status )

      if( unc .eq. AST__NULL ) call stopit( status, 'Error 9' )
      if( ast_getunc( unc, .false., status ) .ne. AST__NULL ) 
     :               call stopit( status, 'Error 9a' )

      call ast_getregionbounds( unc, lbnd, ubnd, status )


      if( abs( lbnd(1) + 2.42406841E-06 ) .gt. 0.0000001E-06 ) 
     :      call stopit( status, 'Error 10' )

      if( abs( lbnd(2) + 2.42406841E-06 ) .gt. 0.0000001E-06 ) 
     :      call stopit( status, 'Error 11' )
      if( abs( 86400.0D0*lbnd(3) + 5.0D-5 ) .gt. 0.1E-10 ) 
     :      call stopit( status, 'Error 12' )
      if( abs( lbnd(4) - 0.07 ) .gt. 0.0001 ) 
     :      call stopit( status, 'Error 13' )
      if( abs( ubnd(1) - 2.42406841E-06 ) .gt. 0.0000001E-06 ) 
     :      call stopit( status, 'Error 14' )
      if( abs( ubnd(2) - 2.42406841E-06 ) .gt. 0.0000001E-06 ) 
     :      call stopit( status, 'Error 15' )
      if( abs( 86400.0D0*ubnd(3) - 5.0D-5 ) .gt. 0.1E-10 ) 
     :      call stopit( status, 'Error 16' )
      if( abs( ubnd(4) - 0.17 ) .gt. 0.0001 ) 
     :      call stopit( status, 'Error 17' )


* UseDefs tests.
c      if( status .eq. SAI__OK ) then
c         obj2 = ast_copy( obj, status )
c         call err_begin( status )
c         call ast_set( obj2, 'System=FK4', status )
c         if( status .ne. AST__NOVAL ) then
c            write(*,*) 'status is ',status,': should be ',AST__NOVAL
c            if( status .ne. sai__ok ) call err_annul( status )
c            call stopit( status, 'Error 18' )
c         else 
c            call err_annul( status )
c         end if
c         call err_end( status )
c         call ast_annul( obj2, status )
c      end if
c
c      if( status .eq. SAI__OK ) then
c         obj2 = ast_copy( obj, status )
c         call err_begin( status )
c         call ast_set( obj2, 'System=velo', status )
c         if( status .ne. AST__NOVAL ) then
c            write(*,*) 'status is ',status,': should be ',AST__NOVAL
c            if( status .ne. sai__ok ) call err_annul( status )
c            call stopit( status, 'Error 19' )
c         else 
c            call err_annul( status )
c         end if
c         call err_end( status )
c         call ast_annul( obj2, status )
c      end if

      call ast_set( obj, 'Unit(4)=J', status )

      if( status .ne. SAI__OK ) call stopit( status, 'Error 20' )

*  Tests on reference values
      if( ast_test( obj, 'RefRA(4)', status ) ) then
         call stopit( status, 'Error 21' )
      end if

      if( ast_test( obj, 'RefDec(4)', status ) ) then
         call stopit( status, 'Error 22' )
      end if

c      if( ast_test( obj, 'Epoch(4)', status ) ) then
c         call stopit( status, 'Error 22' )
c      end if

c      if( ast_test( obj, 'Epoch(1)', status ) ) then
c         call stopit( status, 'Error 23' )
c      end if

c      if( ast_test( obj, 'Epoch(2)', status ) ) then
c         call stopit( status, 'Error 24' )
c      end if




      call ast_end( status )

      if( status .ne. sai__ok ) write(*,*) 'teststc: example 1 '//
     :                                     'tests failed'

      end


      subroutine Example1b( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'AST_ERR'

      include 'teststc_com'

      integer status, obj, i, l
      character value*200

      if( status .ne. sai__ok ) return

      call ast_begin( status )

*  Test the Strict attribute.
      call puteg( 'teststc_eg1', 1, status )

      call err_mark
      call xmlread( 1, obj, 'Strict=1', status )

      if( status .ne. ast__badin ) then
         if( status .ne. sai__ok ) call err_flush( status )
         call stopit( status, 'Error 1' )
      else
         call err_annul( status )
      end if
      call err_rlse

*  Test the ast_warnings function.
      call xmlread( 1, obj, 'Strict=0', status )
      if( warns .EQ. AST__NULL ) then
         call stopit( status, 'Error 2' )

      else if( ast_mapsize( warns, status ) .ne. 5 ) then
         call stopit( status, 'Error 3' )

      else if( .not. ast_mapget0c( warns, 'Warning_1', value, l, 
     :                             status ) ) then
         call stopit( status, 'Error 4' )

      else if( value(:l) .ne. 'astRead(XmlChan): Warning whilst '//
     :         'reading a Position2D element: contains more than '//
     :         'one <Size> element. AST can only use the first' ) then
         call stopit( status, 'Error 5' )
      end if 


      call ast_end( status )

      if( status .ne. sai__ok ) write(*,*) 'teststc: example 1b '//
     :                                     'tests failed'

      end


      subroutine Example2( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      integer status, obj, i, j
      double precision in(12,5), out(12,5)

      if( status .ne. sai__ok ) return

      call ast_begin( status )

*  Put an example of a CatalogEntryLocation into file 1.
      call puteg( 'teststc_eg2', 1, status )

*  Use a new XmlChan to read an object from file 1,and simplify it.
      call xmlread( 1, obj, ' ', status )
      obj = ast_simplify( obj, status )

*  Test simplify by negating and simplifying twice.
      call ast_negate( obj, status )
      obj = ast_simplify( obj, status )
      call ast_negate( obj, status )
      obj = ast_simplify( obj, status )
      call checkdump( obj, 'checkdump 1', status ) 

*  Check it is a StcCatalogEntryLocation
      if( .not. ast_isastcCatalogEntryLocation( obj, status ) ) 
     :                      call stopit( status, 'Error 1' )

*  Check it is an Interval.
      if( .not. ast_isainterval( ast_getstcregion( obj, status ),
     :                           status  ) )
     :                      call stopit( status, 'Error 1a' )

*  Check it has no uncertainty
      if( ast_getunc( obj, .false., status ) .NE. AST__NULL )
     :           call stopit( status, 'Error 1b' )

* Other tests
      if( ast_geti( obj, 'naxes', status ) .ne. 5 ) 
     :    call stopit( status, 'Error 1ab' )

      if( ast_getd( obj, 'fillfactor', status ) .ne. 1.0D0 ) 
     :    call stopit( status, 'Error 1b' )

      if( ast_getc( obj, 'ident', status ) .ne. 'RA6-18hDec20-70deg' ) 
     :    call stopit( status, 'Error 1c' )

      if( ast_getc( obj, 'domain(3)', status ) .ne. 'TIME' ) 
     :    call stopit( status, 'Error 2' )

      if( ast_getc( obj, 'title(3)', status ) .ne. 
     :              'Julian Date [TT] offset from 1968-05-23 12:00:00' ) 
     :    call stopit( status, 'Error 2a' )

      if( ast_getc( obj, 'label(3)', status ) .ne. 
     :    'Julian Date offset from 1968-05-23 12:00:00' ) THEN
         call stopit( status, 'Error 2b' )
      endif

      if( ast_getc( obj, 'domain(1)', status ) .ne. 'SKY' )
     :    call stopit( status, 'Error 3' )

      if( ast_getc( obj, 'system(1)', status ) .ne. 'FK4' )
     :    call stopit( status, 'Error 3a' )

      if( ast_getc( obj, 'label(1)', status ) .ne. 'Right ascension' )
     :    call stopit( status, 'Error 3b' )

      if( ast_getc( obj, 'label(2)', status ) .ne. 'Declination' )
     :    call stopit( status, 'Error 3c' )

      if( ast_getc( obj, 'title(2)', status ) .ne. 'PosEq' )
     :    call stopit( status, 'Error 3d' )

      if( ast_getd( obj, 'Equinox', status ) .ne. 1950D0 ) 
     :    call stopit( status, 'Error 3d' )

      if( ast_getc( obj, 'domain(2)', status ) .ne. 'SKY' )
     :    call stopit( status, 'Error 4' )

      if( ast_getc( obj, 'domain(4)', status ) .ne. 'SPECTRUM' ) 
     :    call stopit( status, 'Error 5' )

      if( ast_getc( obj, 'system(4)', status ) .ne. 'WAVE' )
     :    call stopit( status, 'Error 5a' )

      if( ast_getc( obj, 'stdofrest', status ) .ne. 'Topocentric' )
     :    call stopit( status, 'Error 5b' )

      if( ast_test( obj, 'title(4)', status ) ) 
     :    call stopit( status, 'Error 5c' )

      if( ast_geti( obj, 'naxes', status ) .ne. 5 ) 
     :    call stopit( status, 'Error 6' )

      if( ast_getc( obj, 'domain(5)', status ) .ne. 'REDSHIFT' ) 
     :    call stopit( status, 'Error 6a' )

      if( ast_getc( obj, 'system(5)', status ) .ne. 'VOPT' )
     :    call stopit( status, 'Error 6b' )

      if( ast_getc( obj, 'label(5)', status ) .ne. 'Optical velocity' )
     :    call stopit( status, 'Error 6c' )

      if( ast_getc( obj, 'unit(5)', status ) .ne. 'km/s' )
     :    call stopit( status, 'Error 6d' )

      if( ast_getc( obj, 'unit(4)', status ) .ne. 'Angstrom' )
     :    call stopit( status, 'Error 6e' )


      in(1,1) = 4.71238   ! inside
      in(1,2) = 1.2216
      in(1,3) = 1
      in(1,4) = 6499.9
      in(1,5) = 9999.9

      in(2,1) = 4.71240   ! outside
      in(2,2) = 1.2216
      in(2,3) = 1
      in(2,4) = 6499.9
      in(2,5) = 9999.9

      in(3,1) = 4.71238   ! outside
      in(3,2) = 1.2218
      in(3,3) = 1
      in(3,4) = 6499.9
      in(3,5) = 9999.9

      in(4,1) = 4.71238   ! outside
      in(4,2) = 1.2216
      in(4,3) = -0.6
      in(4,4) = 6499.9
      in(4,5) = 9999.9

      in(5,1) = 4.71238   ! outside
      in(5,2) = 1.2216
      in(5,3) = 1
      in(5,4) = 6500.1
      in(5,5) = 9999.9

      in(6,1) = 4.71238   ! outside
      in(6,2) = 1.2216
      in(6,3) = 1
      in(6,4) = 6499.9
      in(6,5) = 10000.1

      in(7,1) = 1.5709    ! inside
      in(7,2) = 0.3492
      in(7,3) = 999.6
      in(7,4) = 5000.1
      in(7,5) = 5000

      in(8,1) = 1.5707    ! outside
      in(8,2) = 0.3492
      in(8,3) = 999.6
      in(8,4) = 5000.1
      in(8,5) = 5000

      in(9,1) = 1.5709    ! outside
      in(9,2) = 0.3490
      in(9,3) = 999.6
      in(9,4) = 5000.1
      in(9,5) = 5000

      in(10,1) = 1.5709    ! outside
      in(10,2) = 0.3492
      in(10,3) = 1000.4
      in(10,4) = 5000.1
      in(10,5) = 5000

      in(11,1) = 1.5709    ! outside
      in(11,2) = 0.3492
      in(11,3) = 999.6
      in(11,4) = 4999.9
      in(11,5) = 5000

      in(12,1) = 1.5709    ! inside
      in(12,2) = 0.3492
      in(12,3) = 999.6
      in(12,4) = 5000.1
      in(12,5) = 1000

      call ast_trann( obj, 12, 5, 12, in, .true., 5, 12, out, status )

      do i = 1, 12
         if( i .eq. 1 .or. i .eq. 7 .or. i .eq. 12 ) then     ! inside points
            do j = 1, 5
               if( out(i,j) .ne. in(i,j) ) then
                  if( status .eq. sai__ok ) then 
                     write(*,*) i,j,out(i,j),in(i,j)
                     call stopit( status, 'Error 7' )
                  end if
               end if
            end do           
         else                                  ! outside points
            do j = 1, 5
               if( out(i,j) .ne. AST__BAD ) then
                  if( status .eq. sai__ok ) then 
                     write(*,*) i,j,out(i,j),in(i,j)
                     call stopit( status, 'Error 8' )
                  end if
               end if
            end do           
         end if
      end do

*  Tests on reference values
      if( ast_test( obj, 'RefRA(4)', status ) ) then
         call stopit( status, 'Error 9' )
      end if

      if( ast_test( obj, 'RefDec(4)', status ) ) then
         call stopit( status, 'Error 10' )
      end if

c      if( ast_test( obj, 'Epoch(4)', status ) ) then
c         call stopit( status, 'Error 11' )
c      end if

c      if( ast_test( obj, 'Epoch(1)', status ) ) then
c         call stopit( status, 'Error 12' )
c      end if

c      if( ast_test( obj, 'Epoch(2)', status ) ) then
c         call stopit( status, 'Error 13' )
c      end if

      if( ast_test( obj, 'RestFreq(5)', status ) ) then
         call stopit( status, 'Error 14' )
      end if






      call ast_end( status )

      if( status .ne. sai__ok ) write(*,*) 'teststc: example 2 '//
     :                                     'tests failed'

      end







      subroutine Example3( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      integer status, obj, i, j
      double precision in(12,5), out(12,5)

      if( status .ne. sai__ok ) return

      call ast_begin( status )

*  Put an example of a CatalogEntryLocation into file 1.
      call puteg( 'teststc_eg3', 1, status )

*  Use a new XmlChan to read an object from file 1,and simplify it.
      call xmlread( 1, obj, ' ', status )
      obj = ast_simplify( obj, status )

*  Test simplify by negating and simplifying twice.
      call ast_negate( obj, status )
      obj = ast_simplify( obj, status )
      call ast_negate( obj, status )
      obj = ast_simplify( obj, status )
      call checkdump( obj, 'checkdump 1', status ) 

*  Check it is a StcCatalogEntryLocation
      if( .not. ast_isastcCatalogEntryLocation( obj, status ) ) 
     :                      call stopit( status, 'Error 1' )

*  Check it is an Interval.
      if( .not. ast_isainterval( ast_getstcregion( obj, status ),
     :                           status  ) ) then
         write(*,*) ast_GetC( ast_getstcregion( obj, status ), 'Class',
     :                       status )
         call stopit( status, 'Error 1a' )
      end if

*  Check it has no uncertainty
      if( ast_getunc( obj, .false., status ) .NE. AST__NULL )
     :           call stopit( status, 'Error 1b' )

*  Check it has 5 axes.
      if( ast_geti( obj, 'naxes', status ) .ne. 5 ) 
     :    call stopit( status, 'Error 1ab' )

*  Check the rest frequency for axis 5 (redshift) is 5000 Angstrom
      if( abs( ast_getd( obj, 'restfreq(5)', status ) - 599584.916D0 )
     :    .gt. 0.001D0 ) call stopit( status, 'Error A1' )

*  Check the epoch for allaxes is JD 2440000
      if( abs( ast_getd( obj, 'epoch(1)', status ) - 1968.39212D0 ) .gt.
     :    0.00001D0 ) call stopit( status, 'Error B1' )
      if( abs( ast_getd( obj, 'epoch(2)', status ) - 1968.39212D0 ) .gt.
     :    0.00001D0 ) call stopit( status, 'Error B2' )
      if( abs( ast_getd( obj, 'epoch(3)', status ) - 1968.39212D0 ) .gt.
     :    0.00001D0 ) call stopit( status, 'Error B3' )
      if( abs( ast_getd( obj, 'epoch(4)', status ) - 1968.39212D0 ) .gt.
     :    0.00001D0 ) call stopit( status, 'Error B4' )
      if( abs( ast_getd( obj, 'epoch(5)', status ) - 1968.39212D0 ) .gt.
     :    0.00001D0 ) call stopit( status, 'Error B5' )

*  Other tests
      if( ast_getd( obj, 'fillfactor', status ) .ne. 1.0D0 ) 
     :    call stopit( status, 'Error 1b' )

      if( ast_getc( obj, 'ident', status ) .ne. 'RA6-18hDec20-70deg' ) 
     :    call stopit( status, 'Error 1c' )

      if( ast_getc( obj, 'domain(3)', status ) .ne. 'TIME' ) 
     :    call stopit( status, 'Error 2' )

      if( ast_getc( obj, 'label(3)', status ) .ne.
     :    'Julian Date offset from 1968-05-23 12:00:00' ) THEN
         call stopit( status, 'Error 2b' )
      end if

      if( ast_getc( obj, 'domain(1)', status ) .ne. 'SKY' )
     :    call stopit( status, 'Error 3' )

      if( ast_getc( obj, 'system(1)', status ) .ne. 'FK4' )
     :    call stopit( status, 'Error 3a' )

      if( ast_getc( obj, 'label(1)', status ) .ne. 'Right ascension' )
     :    call stopit( status, 'Error 3b' )

      if( ast_getc( obj, 'label(2)', status ) .ne. 'Declination' )
     :    call stopit( status, 'Error 3c' )

      if( ast_getc( obj, 'title(2)', status ) .ne. 'PosEq' )
     :    call stopit( status, 'Error 3d' )

      if( ast_getd( obj, 'Equinox', status ) .ne. 1950D0 ) 
     :    call stopit( status, 'Error 3d' )

      if( ast_getc( obj, 'domain(2)', status ) .ne. 'SKY' )
     :    call stopit( status, 'Error 4' )

      if( ast_getc( obj, 'domain(4)', status ) .ne. 'SPECTRUM' ) 
     :    call stopit( status, 'Error 5' )

      if( ast_getc( obj, 'system(4)', status ) .ne. 'WAVE' )
     :    call stopit( status, 'Error 5a' )

      if( ast_getc( obj, 'stdofrest', status ) .ne. 'Topocentric' )
     :    call stopit( status, 'Error 5b' )

      if( ast_test( obj, 'title(4)', status ) ) 
     :    call stopit( status, 'Error 5c' )

      if( ast_geti( obj, 'naxes', status ) .ne. 5 ) 
     :    call stopit( status, 'Error 6' )

      if( ast_getc( obj, 'domain(5)', status ) .ne. 'REDSHIFT' ) 
     :    call stopit( status, 'Error 6a' )

      if( ast_getc( obj, 'system(5)', status ) .ne. 'VOPT' )
     :    call stopit( status, 'Error 6b' )

      if( ast_getc( obj, 'label(5)', status ) .ne. 'Optical velocity' )
     :    call stopit( status, 'Error 6c' )

      if( ast_getc( obj, 'unit(5)', status ) .ne. 'km/s' )
     :    call stopit( status, 'Error 6d' )

      if( ast_getc( obj, 'unit(4)', status ) .ne. 'Angstrom' )
     :    call stopit( status, 'Error 6e' )


      in(1,1) = 4.71238   ! inside
      in(1,2) = 1.2216
      in(1,3) = 0.0D0
      in(1,4) = 5000
      in(1,5) = 9999.9

      in(2,1) = 4.71240   ! outside
      in(2,2) = 1.2216
      in(2,3) = 0.0D0
      in(2,4) = 5000
      in(2,5) = 9999.9

      in(3,1) = 4.71238   ! outside
      in(3,2) = 1.2218
      in(3,3) = 0.0D0
      in(3,4) = 5000
      in(3,5) = 9999.9

      in(4,1) = 4.71238   ! outside
      in(4,2) = 1.2216
      in(4,3) = 0.5D0
      in(4,4) = 5000
      in(4,5) = 9999.9

      in(5,1) = 4.71238   ! outside
      in(5,2) = 1.2216
      in(5,3) = 0.0D0
      in(5,4) = 6500.1
      in(5,5) = 9999.9

      in(6,1) = 4.71238   ! outside
      in(6,2) = 1.2216
      in(6,3) = 0.0D0
      in(6,4) = 5000
      in(6,5) = 10000.1

      in(7,1) = 1.5709    ! inside
      in(7,2) = 0.3492
      in(7,3) = 0.0D0
      in(7,4) = 5000
      in(7,5) = 5000

      in(8,1) = 1.5707    ! outside
      in(8,2) = 0.3492
      in(8,3) = 0.0D0
      in(8,4) = 5000
      in(8,5) = 5000

      in(9,1) = 1.5709    ! outside
      in(9,2) = 0.3490
      in(9,3) = 0.0D0
      in(9,4) = 5000
      in(9,5) = 5000

      in(10,1) = 1.5709    ! outside
      in(10,2) = 0.3492
      in(10,3) = 39999.4D0
      in(10,4) = 5000
      in(10,5) = 5000

      in(11,1) = 1.5709    ! outside
      in(11,2) = 0.3492
      in(11,3) = 0.0D0
      in(11,4) = 4999.9
      in(11,5) = 5000

      in(12,1) = 1.5709    ! inside
      in(12,2) = 0.3492
      in(12,3) = 0.0D0
      in(12,4) = 5000
      in(12,5) = 1000

      call ast_trann( obj, 12, 5, 12, in, .true., 5, 12, out, status )

      do i = 1, 12
         if( i .eq. 1 .or. i .eq. 7 .or. i .eq. 12 ) then     ! inside points
            do j = 1, 5
               if( out(i,j) .ne. in(i,j) ) then
                  if( status .eq. sai__ok ) then 
                     write(*,*) i,j,out(i,j),in(i,j)
                     call stopit( status, 'Error 7' )
                  end if
               end if
            end do           
         else                                  ! outside points
            do j = 1, 5
               if( out(i,j) .ne. AST__BAD ) then
                  if( status .eq. sai__ok ) then 
                     write(*,*) i,j,out(i,j),in(i,j)
                     call stopit( status, 'Error 8' )
                  end if
               end if
            end do           
         end if
      end do

*  Tests on reference values
      if( ast_test( obj, 'RefRA(4)', status ) ) then
         call stopit( status, 'Error 9' )
      end if

      if( ast_test( obj, 'RefDec(4)', status ) ) then
         call stopit( status, 'Error 10' )
      end if

      if( .not. ast_test( obj, 'Epoch(4)', status ) ) then
         call stopit( status, 'Error 11' )
      end if

      if( .not. ast_test( obj, 'Epoch(1)', status ) ) then
         call stopit( status, 'Error 12' )
      end if

      if( .not. ast_test( obj, 'Epoch(2)', status ) ) then
         call stopit( status, 'Error 13' )
      end if

      if( .not. ast_test( obj, 'RestFreq(5)', status ) ) then
         call stopit( status, 'Error 14' )
      end if






      call ast_end( status )

      if( status .ne. sai__ok ) write(*,*) 'teststc: example 3 '//
     :                                     'tests failed'

      end






      subroutine Example4( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'AST_ERR'

      integer status, obj2, obj, i, j, unc, frm
      double precision in(12,4), out(12,4), lbnd(4), ubnd(4)

      if( status .ne. sai__ok ) return

      call ast_begin( status )

*  Put an example of an STCSearchLocation into file 1.
      call puteg( 'teststc_eg4', 1, status )

*  Use a new XmlChan to read an object from file 1,and simplify it.
      call xmlread( 1, obj, ' ', status )
      obj = ast_simplify( obj, status )

*  Test simplify by negating and simplifying twice.
      call ast_negate( obj, status )
      obj = ast_simplify( obj, status )
      call ast_negate( obj, status )
      obj = ast_simplify( obj, status )
      call checkdump( obj, 'checkdump 1', status ) 


*  Check it is a STCSearchLocation
      if( .not. ast_isastcsearchlocation( obj, status ) ) 
     :                      call stopit( status, 'Error 1' )

*  Check it is a Prism.
      if( .not. ast_isaprism( ast_getstcregion( obj, status ),
     :                           status  ) )
     :                      call stopit( status, 'Error 1a' )

*  Check it has no uncertainty
      if( ast_getunc( obj, .false., status ) .NE. AST__NULL )
     :           call stopit( status, 'Error 1b' )

* Other tests
      if( ast_geti( obj, 'naxes', status ) .ne. 4 ) 
     :    call stopit( status, 'Error 1ab' )

      if( ast_getd( obj, 'fillfactor', status ) .ne. 1.0D0 ) 
     :    call stopit( status, 'Error 1b' )

      if( ast_getc( obj, 'ident', status ) .ne. 'M81' ) 
     :    call stopit( status, 'Error 1c' )

      if( ast_getc( obj, 'domain(3)', status ) .ne. 'TIME' ) 
     :    call stopit( status, 'Error 2' )

      if( ast_getc( obj, 'label(3)', status ) .ne. 
     :    'Modified Julian Date offset from 1900-01-01' ) THEN
         call stopit( status, 'Error 2b' )
      end if

      if( ast_getc( obj, 'domain(1)', status ) .ne. 'SKY' )
     :    call stopit( status, 'Error 3' )

      if( ast_getc( obj, 'system(1)', status ) .ne. 'ICRS' )
     :    call stopit( status, 'Error 3a' )

      if( ast_getc( obj, 'label(1)', status ) .ne. 'Right ascension' )
     :    call stopit( status, 'Error 3b' )

      if( ast_getc( obj, 'label(2)', status ) .ne. 'Declination' )
     :    call stopit( status, 'Error 3c' )

      if( ast_getc( obj, 'title(2)', status ) .ne. 'Equatorial' )
     :    call stopit( status, 'Error 3d' )

      if( ast_test( obj, 'Equinox', status ) ) 
     :    call stopit( status, 'Error 3d2' )

      if( ast_getc( obj, 'domain(2)', status ) .ne. 'SKY' )
     :    call stopit( status, 'Error 4' )

      if( ast_getc( obj, 'domain(4)', status ) .ne. 'SPECTRUM' ) 
     :    call stopit( status, 'Error 5' )

      if( ast_getc( obj, 'system(4)', status ) .ne. 'WAVE' )
     :    call stopit( status, 'Error 5a' )

      if( ast_getc( obj, 'stdofrest', status ) .ne. 'Barycentric' )
     :    call stopit( status, 'Error 5b' )

      if( ast_getc( obj, 'title(4)', status ) .ne. 'Wavelength' ) 
     :    call stopit( status, 'Error 5c' )

      if( ast_geti( obj, 'naxes', status ) .ne. 4 ) 
     :    call stopit( status, 'Error 6' )

      if( ast_getc( obj, 'unit(4)', status ) .ne. 'Angstrom' )
     :    call stopit( status, 'Error 6e' )

      frm = ast_getregionframe( obj, status )
      if( ast_getc( frm, 'Ident', status ) .ne. 'ICRS-TT-BARY' )
     :    call stopit( status, 'Error 7' )
            


*  Tests on reference values
      if( ast_test( obj, 'RefRA(4)', status ) ) then
         call stopit( status, 'Error 9' )
      end if

      if( ast_test( obj, 'RefDec(4)', status ) ) then
         call stopit( status, 'Error 10' )
      end if

c      if( ast_test( obj, 'Epoch(4)', status ) ) then
c         call stopit( status, 'Error 11' )
c      end if

c      if( ast_test( obj, 'Epoch(1)', status ) ) then
c         call stopit( status, 'Error 12' )
c      end if

c      if( ast_test( obj, 'Epoch(2)', status ) ) then
c         call stopit( status, 'Error 13' )
c      end if

      if( ast_test( obj, 'RestFreq(4)', status ) ) then
         call stopit( status, 'Error 14' )
      end if

      if( abs( ast_getd( obj, 'Epoch(3)', status ) - 1900.00051056532 )
     :    .gt. 0.0001  ) then
         call stopit( status, 'Error 12b' )
      end if

      if( abs( ast_getd( obj, 'TimeOrigin', status ) - 15020.0D0 )
     :    .gt. 0.0001  ) then
         call stopit( status, 'Error 12c' )
      end if




      in(1,1) = 2.51126532207628 ! inside
      in(1,2) = 1.22218015796595
      in(1,3) = 0.01
      in(1,4) = 4001

      in(2,1) = 2.5094191311777 ! outside
      in(2,2) = 1.22248014367694
      in(2,3) = 0.01
      in(2,4) = 4001

      in(3,1) = 2.51126532207628 ! outside
      in(3,2) = 1.22218015796595
      in(3,3) = 0.01
      in(3,4) = 3999

      in(4,1) = 2.51126532207628 ! outside
      in(4,2) = 1.22218015796595
      in(4,3) = -0.2
      in(4,4) = 4001

      in(5,1) = 2.5094191311777 ! outside
      in(5,2) = 1.22248014367694
      in(5,3) = -0.2
      in(5,4) = 4001

      in(6,1) = 2.51126532207628 ! outside
      in(6,2) = 1.22218015796595
      in(6,3) = -0.2
      in(6,4) = 3999

      in(7,1) = 2.51682141503858 ! inside
      in(7,2) = 1.18868060989363
      in(7,3) = 0.01
      in(7,4) = 6999

      in(8,1) = 2.51524001365674 ! outside
      in(8,2) = 1.18830732379242
      in(8,3) = 0.01
      in(8,4) = 6999

      in(9,1) = 2.51682141503858 ! outside
      in(9,2) = 1.18868060989363
      in(9,3) = 0.01
      in(9,4) = 7001

      in(10,1) = 2.51682141503858 ! outside
      in(10,2) = 1.18868060989363
      in(10,3) = -0.2
      in(10,4) = 6999

      in(11,1) = 2.51524001365674 ! outside
      in(11,2) = 1.18830732379242
      in(11,3) = -0.2
      in(11,4) = 6999

      in(12,1) = 2.51682141503858 ! outside
      in(12,2) = 1.18868060989363
      in(12,3) = -0.2
      in(12,4) = 7001

      call ast_trann( obj, 12, 4, 12, in, .true., 4, 12, out, status )

      do i = 1, 12
         if( i .eq. 1 .or. i .eq. 7 ) then     ! inside points
            do j = 1, 4
               if( out(i,j) .ne. in(i,j) ) then
                  if( status .eq. sai__ok ) then 
                     write(*,*) i,j,out(i,j),in(i,j)
                     call stopit( status, 'Error 13c' )
                  end if
               end if
            end do
         else                                  ! outside points
            do j = 1, 4
               if( out(i,j) .ne. AST__BAD ) then
                  if( status .eq. sai__ok ) then 
                     write(*,*) i,j,out(i,j),in(i,j)
                     call stopit( status, 'Error 14c' )
                  end if
               end if
            end do
         end if
      end do


      call ast_end( status )

      if( status .ne. sai__ok ) write(*,*) 'teststc: example 4 '//
     :                                     'tests failed'

      end




      subroutine Example5( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'AST_ERR'

      integer status, obj2, obj, i, j, unc, frm
      double precision in(12,4), out(12,4), lbnd(4), ubnd(4)

      if( status .ne. sai__ok ) return

      call ast_begin( status )

*  Put an example of an STCSearchLocation into file 1.
      call puteg( 'teststc_eg5', 1, status )

*  Use a new XmlChan to read an object from file 1,and simplify it.
      call xmlread( 1, obj, ' ', status )
      call checkdump( obj, 'checkdump 2', status ) 
      obj = ast_simplify( obj, status )

*  Test simplify by negating and simplifying twice.
      call ast_negate( obj, status )
      obj = ast_simplify( obj, status )
      call ast_negate( obj, status )
      obj = ast_simplify( obj, status )
      call checkdump( obj, 'checkdump 1', status ) 

*  Check it is a STCObsDataLocation
      if( .not. ast_isastcobsdatalocation( obj, status ) ) 
     :                      call stopit( status, 'Error 1' )

*  Check it contains a Prism.
      if( .not. ast_isaprism( ast_getstcregion( obj, status ),
     :                           status  ) )
     :                      call stopit( status, 'Error 1a' )

* Other tests
      if( ast_getd( obj, 'fillfactor', status ) .ne. 1.0D0 ) 
     :    call stopit( status, 'Error 1b' )

      if( ast_getc( obj, 'ident', status ) .ne. 'M81' ) 
     :    call stopit( status, 'Error 1c' )

      if( ast_getc( obj, 'domain(3)', status ) .ne. 'TIME' ) 
     :    call stopit( status, 'Error 2' )

      if( ast_getc( obj, 'label(3)', status ) .ne. 
     :    'Modified Julian Date offset from 2004-07-15 08:23:56' ) then
         call stopit( status, 'Error 2b' )
      end if

      if( ast_getc( obj, 'domain(1)', status ) .ne. 'SKY' )
     :    call stopit( status, 'Error 3' )

      if( ast_getc( obj, 'system(1)', status ) .ne. 'ICRS' )
     :    call stopit( status, 'Error 3a' )

      if( ast_getc( obj, 'label(1)', status ) .ne. 'Right ascension' )
     :    call stopit( status, 'Error 3b' )

      if( ast_getc( obj, 'label(2)', status ) .ne. 'Declination' )
     :    call stopit( status, 'Error 3c' )

      if( ast_getc( obj, 'title(2)', status ) .ne. 'Equatorial' ) 
     :    call stopit( status, 'Error 3d' )

      if( ast_getc( obj, 'domain(2)', status ) .ne. 'SKY' )
     :    call stopit( status, 'Error 4' )

      if( ast_getc( obj, 'domain(4)', status ) .ne. 'SPECTRUM' ) 
     :    call stopit( status, 'Error 5' )

      if( ast_getc( obj, 'system(4)', status ) .ne. 'WAVE' )
     :    call stopit( status, 'Error 5a' )

      if( ast_getc( obj, 'stdofrest', status ) .ne. 'Topocentric' )
     :    call stopit( status, 'Error 5b' )

      if( ast_getc( obj, 'title(4)', status ) .ne. 
     :    'Wavelength' ) call stopit( status, 'Error 5c' )

      if( ast_getc( obj, 'unit(4)', status ) .ne. 'Angstrom' )
     :    call stopit( status, 'Error 5d' )

      if( ast_geti( obj, 'naxes', status ) .ne. 4 ) 
     :    call stopit( status, 'Error 6' )

      call ast_getregionbounds( obj, lbnd, ubnd, status )



      lbnd(1) = 0.5*(lbnd(1) + ubnd(1))
      lbnd(2) = 0.5*(lbnd(2) + ubnd(2))
      lbnd(3) = 0.5*(lbnd(3) + ubnd(3))
      lbnd(4) = 0.5*(lbnd(4) + ubnd(4))

      if( abs( lbnd(1) - 2.59858948190075 ) .gt. 1E-06 ) 
     :      call stopit( status, 'Error 10' )
      if( abs( lbnd(2) - 1.20541670934471 ) .gt. 1E-06 ) 
     :      call stopit( status, 'Error 11' )
      if( abs( lbnd(3) ) .gt. 1E-5 ) 
     :      call stopit( status, 'Error 12' )
      if( abs( lbnd(4) - 4600 ) .gt. 0.0001 ) 
     :      call stopit( status, 'Error 13' )
      if( abs( ubnd(1) - 2.61080678666471 ) .gt. 1E-06 ) 
     :      call stopit( status, 'Error 14' )
      if( abs( ubnd(2) - 1.2097800324747 ) .gt. 1E-06 ) 
     :      call stopit( status, 'Error 15' )
      if( abs( ubnd(3) - 380.0D0 ) .gt. 1E-5 ) 
     :      call stopit( status, 'Error 16' )
      if( abs( ubnd(4) - 4800 ) .gt. 0.0001 ) 
     :      call stopit( status, 'Error 17' )
      if( ast_getc( obj, 'ObsLon', status ) .ne. 'W111:35:39.84' ) 
     :      call stopit( status, 'Error 18' )
      if( ast_getc( obj, 'ObsLat', status ) .ne. 'N31:57:30.96' ) 
     :      call stopit( status, 'Error 19' )

      unc = ast_getunc( obj, .true., status )
      if( unc .eq. AST__NULL ) call stopit( status, 'Error 20' )

      call ast_getregionbounds( unc, lbnd, ubnd, status )

      lbnd(1) = 0.5*(lbnd(1) + ubnd(1))
      lbnd(2) = 0.5*(lbnd(2) + ubnd(2))
      lbnd(3) = 0.5*(lbnd(3) + ubnd(3))
      lbnd(4) = 0.5*(lbnd(4) + ubnd(4))

      if( abs( lbnd(1) - 2.59858948190075D0) .gt. 1E-05 ) 
     :      call stopit( status, 'Error 21' )
      if( abs( lbnd(2) - 1.20541670934471D0) .gt. 1E-05 ) 
     :      call stopit( status, 'Error 22' )
      if( abs( lbnd(3) ) .gt. 1.0D-05 ) 
     :      call stopit( status, 'Error 23' )
      if( abs( lbnd(4) - 4600.0D0) .gt. 0.0001 ) 
     :      call stopit( status, 'Error 24' )
      if( abs( ubnd(1) - 2.59859209989462D0) .gt. 1E-05 ) 
     :      call stopit( status, 'Error 25' )

      if( abs( ubnd(2) - 1.20541932733859D0) .gt. 1E-05 ) 
     :      call stopit( status, 'Error 26' )
      if( abs( ubnd(3) - 0.3803143212621760D-03 ) .gt. 1E-05 ) 
     :      call stopit( status, 'Error 27' )
      if( abs( ubnd(4) - 4600.0002D0) .gt. 0.000001 ) 
     :      call stopit( status, 'Error 28' )

      if( status .ne. sai__ok ) write(*,*) 'teststc: example 5 '//
     :                                     'tests failed'

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





*
*  Read an object out of the specified internal file using an XmlChan.
*
      subroutine xmlread( ifil, obj, opts, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      include 'teststc_com'

      external xmlSource
      integer obj, ifil, status, ch
      character opts*(*)

      if( status .ne. sai__ok ) return 

      ifile = ifil
      iline = 1

      ch = ast_xmlchan( xmlSource, ast_null, opts, status )
      obj = ast_read( ch, status )
      if( obj .eq. ast__null ) then
         call stopit( status, 'checkXmlChan: Failed to read STC '//
     :                'object from XmlChan.' )
      end if

      warns = ast_warnings( ch, status )
      call ast_annul( ch, status )

      end




*
*  Reads line "iline" from internal file "ifile" and returns it to AST using 
*  the AST_PUTLINE routine. Then increments "iline" ready for next time.
*
      subroutine xmlSource( status )
      implicit none

      include 'teststc_com'

      integer status, l, chr_len

      if( iline .le. filelen( ifile ) ) then
         l = chr_len( files(ifile,iline) )
         call ast_putline( files(ifile,iline), l, status )
         iline = iline + 1
      else
         call ast_putline( ' ', -1, status )
      end if

      end        

*
*  Append a line obtained using ast_getline function to the end of the
*  internal file indicated by "ifile", and increment the file length.
*
      subroutine xmlSink( status )
      implicit none

      include 'teststc_com'

      integer status, l
      character line*(linelen)

      call ast_getline( line, l, status ) 
      if( l .gt. 0 ) then

         if( filelen( ifile ) .ge. mxline ) then
            call stopit( status, 'checkXmlChan: Too many lines sent '//
     :                   'to sink function' )

         else if( l .gt. linelen ) then
            call stopit( status, 'checkXmlChan: Text truncated in '//
     :                   'sink function' )

         else
            filelen( ifile ) = filelen( ifile ) + 1
            files( ifile, filelen( ifile ) ) = line(:l)
         end if

      end if

      end      


      subroutine puteg( flnam, ifl, status )
      implicit none

      include 'SAE_PAR'
      include 'AST_PAR'
      include 'teststc_com'

      integer status, ifl
      character flnam*(*)      

      if( status .ne. sai__ok ) return 

      open( file=flnam, status='old', unit=10 )

      iline = 1
 10   continue
      if( iline .gt. mxline ) call stopit( status, 
     :                                     'mxline exceeded in puteg' )
      read( 10, '(A)', end=20 ) files( ifl, iline )

      if(  files( ifl, iline )( linelen : linelen ) .ne. ' ' ) then
         call stopit( status, 'linelen exceeded in puteg' )
      end if

      iline = iline + 1
      go to 10  

 20   continue
      close( 10 )
      filelen( ifl ) = iline - 1

      end


*
*  Tests the dump function, the loader, and the astOverlap method.
*
      subroutine checkdump( obj, text, status )

      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      character text*(*)
      integer obj, status, next, end, ch, result, ll, overlap
      external mysource, mysink
      character buf*190000

      common /ss1/ buf 
      common /ss2/ next, end, ll

      if( status .ne. sai__ok ) return

*  Create a Channel which reads and writes to an internal string buffer.
      ch = ast_channel( mysource, mysink, ' ', status )

*  Write the supplied Region out to this Channel.
      ll = 160
      next = 1
      if( ast_write( ch, obj, status ) .ne.1 ) then
         write(*,*) text
         call stopit( status, 'Cannot write supplied object to '//
     :                'channel' )
      end if

*  Read an Object back from this Channel.
      next = 1
      result = ast_read( ch, status )
      if( result .eq. ast__null ) then
         write(*,*) text
         call stopit( status, 'Cannot read object from channel' )
      end if

*  Check that it is a Region and its boundary is identical to the supplied 
*  Region.
      overlap = ast_overlap( obj, result, status )
      if( overlap .ne. 5 ) then
         write(*,*) 'obj result Overlap: ', overlap
         write(*,*) 'obj self-Overlap: ', ast_overlap( obj, obj,
     :                                                 status )
         write(*,*) 'result self-Overlap: ', ast_overlap( result, 
     :                                                 result, status )
         call ast_Show( obj, status )
         call ast_Show( result, status )
         write(*,*) text
         call stopit( status, 'Object has changed' )
      end if

*  Return the new Region pointer in place of the old. 
      obj = result      

      end  

      subroutine mysource( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, ll
      character buf*190000

      common /ss1/ buf 
      common /ss2/ next, end, ll

      if( status .ne. sai__ok ) return

      if( next .ge. end ) then
         call ast_putline( buf, -1, status )
      else
         call ast_putline( buf( next : ), ll, status )
      endif

      next = next + ll

      end

      subroutine mysink( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, f, l, ll
      character buf*190000
      character line*1000

      common /ss1/ buf 
      common /ss2/ next, end, ll

      if( status .ne. sai__ok ) return

      line = ' '
      call ast_getline( line, l, status )
      call chr_fandl( line( : l ), f, l )
      buf( next : ) = line( f : l )
      l = l - f + 1

      if( next + ll - 1 .ge. 190000 ) then
         write(*,*) buf
         call stopit( status, 'Buffer overflow in mysink!!' )
      else if( l .gt. ll ) then
         write(*,*)
         write(*,*) buf( next : next + l)
         write(*,*) 'Line length ',l,' greater than ',ll
         call stopit( status, 'Line overflow in mysink!!' )
      else 
         end = next + l
         buf( end : next + ll - 1 ) = ' '
      endif

      next = next + ll

      end


