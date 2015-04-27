      program testregions
      implicit none
      include 'SAE_PAR'
      integer status

      status = sai__ok

c      call ast_watchmemory( 282905 )

      call ast_begin( status )
      call checkConvex( status )
      call checkRemoveRegions( status )
      call checkInterval( status )
      call checkEllipse( status )
      call checkPrism( status )
      call checkPolygon( status )
      call checkCircle( status )
      call checkBox( status )
      call checkNullRegion( status )
      call generalChecks( status )
      call checkCmpRegion( status )
      call checkPointList( status )

      call ast_end( status )

c      call ast_activememory( 'testregions' )

      if( status .eq. sai__ok ) then
         write(*,*) 'All Region tests passed'
      else
         write(*,*) 'Region tests failed'
      end if

      end


      subroutine generalChecks( status )
      implicit none
      include 'AST_PAR'
      include 'PRM_PAR'
      include 'SAE_PAR'

      integer status, frm1, frm2, frm3, reg1, reg2, reg3, reg4, reg5
      double precision lbnd(3), ubnd(3), p1(2), p2(2)

      if( status .ne.sai__ok ) return

      call ast_begin( status )



      lbnd(1) = 0.0D0
      lbnd(2) = AST__BAD
      ubnd(1) = AST__BAD
      ubnd(2) = 0.0D0
      frm1 = ast_frame( 2, ' ', status )
      reg1 = ast_interval( frm1, lbnd, ubnd, AST__NULL, ' ', status )

      call ast_getregionbounds( reg1, lbnd, ubnd, status )
      if( lbnd(1) .ne. 0.0D0 ) call stopit( status, 'General 1' )
      if( lbnd(2) .gt. 0.99*val__mind ) call stopit( status,
     :                                               'General 2' )
      if( ubnd(1) .lt. 0.99*val__maxd ) call stopit( status,
     :                                               'General 3' )
      if( ubnd(2) .ne. 0.0D0 ) call stopit( status, 'General 4' )



      p1(1) = 0.0D0
      p1(2) = 0.0D0
      p2(1) = 1.0D0
      reg2 = ast_circle( frm1, 1, p1, p2, AST__NULL, ' ', status )

      call ast_getregionbounds( reg2, lbnd, ubnd, status )
      if( lbnd(1) .ne. -1.0D0 ) call stopit( status, 'General 5' )
      if( lbnd(2) .ne. -1.0D0 ) call stopit( status, 'General 6' )
      if( ubnd(1) .ne. 1.0D0 ) call stopit( status, 'General 7' )
      if( ubnd(2) .ne. 1.0D0 ) call stopit( status, 'General 8' )



      reg3 = ast_cmpregion( reg1, reg2, AST__OR, ' ', status )

      call ast_getregionbounds( reg3, lbnd, ubnd, status )
      if( lbnd(1) .ne. -1.0D0 ) call stopit( status, 'General 9' )
      if( lbnd(2) .gt. 0.99*val__mind ) call stopit( status,
     :                                               'General 10' )
      if( ubnd(1) .lt. 0.99*val__maxd ) call stopit( status,
     :                                               'General 11' )
      if( ubnd(2) .ne. 1.0D0 ) call stopit( status, 'General 12' )



      lbnd(1) = -1.0D0
      ubnd(1) = 1.0D0
      frm2 = ast_frame( 1, ' ', status )
      reg4 = ast_interval( frm2, lbnd, ubnd, AST__NULL, ' ', status )

      call ast_getregionbounds( reg4, lbnd, ubnd, status )
      if( lbnd(1) .ne. -1.0D0 ) call stopit( status, 'General 13' )
      if( ubnd(1) .ne. 1.0D0 ) call stopit( status,  'General 14' )



      reg5 = ast_prism( reg3, reg4, ' ', status )

      call ast_getregionbounds( reg5, lbnd, ubnd, status )
      if( lbnd(1) .ne. -1.0D0 ) call stopit( status, 'General 15' )
      if( lbnd(2) .gt. 0.99*val__mind ) call stopit( status,
     :                                               'General 16' )
      if( ubnd(1) .lt. 0.99*val__maxd ) call stopit( status,
     :                                               'General 17' )
      if( ubnd(2) .ne. 1.0D0 ) call stopit( status, 'General 18' )
      if( lbnd(3) .ne. -1.0D0 ) call stopit( status, 'General 19' )
      if( ubnd(3) .ne. 1.0D0 ) call stopit( status,  'General 20' )



      call ast_negate( reg2, status )
      reg3 = ast_cmpregion( reg1, reg2, AST__OR, ' ', status )

      call ast_getregionbounds( reg3, lbnd, ubnd, status )
      if( lbnd(1) .gt. 0.99*val__mind ) call stopit( status,
     :                                               'General 21' )
      if( lbnd(2) .gt. 0.99*val__mind ) call stopit( status,
     :                                               'General 22' )
      if( ubnd(1) .lt. 0.99*val__maxd ) call stopit( status,
     :                                               'General 23' )
      if( ubnd(2) .lt. 0.99*val__maxd ) call stopit( status,
     :                                               'General 24' )


      reg5 = ast_prism( reg3, reg4, ' ', status )

      call ast_getregionbounds( reg5, lbnd, ubnd, status )
      if( lbnd(1) .gt. 0.99*val__mind ) call stopit( status,
     :                                               'General 25' )
      if( lbnd(2) .gt. 0.99*val__mind ) call stopit( status,
     :                                               'General 26' )
      if( ubnd(1) .lt. 0.99*val__maxd ) call stopit( status,
     :                                               'General 27' )
      if( ubnd(2) .lt. 0.99*val__maxd ) call stopit( status,
     :                                               'General 28' )
      if( lbnd(3) .ne. -1.0D0 ) call stopit( status, 'General 29' )
      if( ubnd(3) .ne. 1.0D0 ) call stopit( status,  'General 30' )


      reg3 = ast_cmpregion( reg1, reg2, AST__AND, ' ', status )

      call ast_getregionbounds( reg3, lbnd, ubnd, status )
      if( lbnd(1) .ne. 0.0D0 ) call stopit( status, 'General 31' )
      if( lbnd(2) .gt. 0.99*val__mind ) call stopit( status,
     :                                               'General 32' )
      if( ubnd(1) .lt. 0.99*val__maxd ) call stopit( status,
     :                                               'General 33' )
      if( ubnd(2) .ne. 0.0D0 ) call stopit( status, 'General 34' )



      reg5 = ast_prism( reg3, reg4, ' ', status )

      call ast_getregionbounds( reg5, lbnd, ubnd, status )
      if( lbnd(1) .ne. 0.0D0 ) call stopit( status, 'General 35' )
      if( lbnd(2) .gt. 0.99*val__mind ) call stopit( status,
     :                                               'General 36' )
      if( ubnd(1) .lt. 0.99*val__maxd ) call stopit( status,
     :                                               'General 37' )
      if( ubnd(2) .ne. 0.0D0 ) call stopit( status, 'General 38' )
      if( lbnd(3) .ne. -1.0D0 ) call stopit( status, 'General 39' )
      if( ubnd(3) .ne. 1.0D0 ) call stopit( status,  'General 40' )


      call ast_end( status )
      if( status .ne. sai__ok ) write(*,*) 'General tests failed'

      end




      subroutine checkInterval( status )
      implicit none
      include 'AST_PAR'
      include 'SAE_PAR'

      integer status, frm1, frm2, frm3, unc, int1, int2, int3, int4,
     :        int5, frm4, map, outperm(6), inperm(6), pm, reg
      double precision lbnd(3), ubnd(3), p(5,3), q(5,3),in(4,3),out(4,3)
      double precision xin(9), yin(9), xout(9), yout(9)

      logical hasframeset

      if( status .ne.sai__ok ) return

      call ast_begin( status )

      frm1 = ast_skyframe( ' ', status )
      frm2 = ast_specframe( 'Unit=Angstrom', status )
      frm3 = ast_cmpframe( frm1, frm2, ' ', status )

      if( ast_getc( frm1, 'InternalUnit(1)', status ) .ne. 'rad' )
     :    call stopit( status, 'InternalUnit 1' )
      if( ast_getc( frm1, 'InternalUnit(2)', status ) .ne. 'rad' )
     :    call stopit( status, 'InternalUnit 2' )
      if( ast_getc( frm2, 'InternalUnit(1)', status ) .ne. 'Angstrom' )
     :    call stopit( status, 'InternalUnit 3' )
      if( ast_getc( frm3, 'InternalUnit(1)', status ) .ne. 'rad' )
     :    call stopit( status, 'InternalUnit 4' )
      if( ast_getc( frm3, 'InternalUnit(2)', status ) .ne. 'rad' )
     :    call stopit( status, 'InternalUnit 5' )
      if( ast_getc( frm3, 'InternalUnit(3)', status ) .ne. 'Angstrom' )
     :    call stopit( status, 'InternalUnit 6' )

      lbnd( 1 ) = AST__BAD
      lbnd( 2 ) = AST__BAD
      lbnd( 3 ) = 5000.0
      ubnd( 1 ) = AST__BAD
      ubnd( 2 ) = AST__BAD
      ubnd( 3 ) = 6000.0

      int1 = ast_interval( frm3, lbnd, ubnd, AST__NULL, ' ', status )
      call checkdump( int1, 'checkdump int1', status )

      p(1,1) = 0.0            ! On boundary
      p(1,2) = 0.0
      p(1,3) = 5000.0
      p(2,1) = 2.0            ! On boundary
      p(2,2) = -1.0
      p(2,3) = 6000.0
      p(3,1) = -2.0           ! Inside
      p(3,2) = 1.0
      p(3,3) = 5999.0
      p(4,1) = 2.0            ! Outside
      p(4,2) = -2.0
      p(4,3) = 6010.0
      p(5,1) = 1.0            ! Outside
      p(5,2) = -1.0
      p(5,3) = 4910.0

      call ast_trann( int1, 5, 3, 5, p, .true., 3, 5, q, status )
      if( q(1,1) .ne. p(1,1)) call stopit( status, 'Interval 1' )
      if( q(1,2) .ne. p(1,2)) call stopit( status, 'Interval 1b' )
      if( q(1,3) .ne. p(1,3)) call stopit( status, 'Interval 1c' )
      if( q(2,1) .ne. p(2,1)) call stopit( status, 'Interval 2' )
      if( q(2,2) .ne. p(2,2)) call stopit( status, 'Interval 2b' )
      if( q(2,3) .ne. p(2,3)) call stopit( status, 'Interval 2c' )
      if( q(3,1) .ne. p(3,1)) call stopit( status, 'Interval 3' )
      if( q(3,2) .ne. p(3,2)) call stopit( status, 'Interval 3b' )
      if( q(3,3) .ne. p(3,3)) call stopit( status, 'Interval 3c' )
      if( q(4,1) .ne. AST__BAD ) call stopit( status, 'Interval 4' )
      if( q(4,2) .ne. AST__BAD ) call stopit( status, 'Interval 4b' )
      if( q(4,3) .ne. AST__BAD ) call stopit( status, 'Interval 4c' )
      if( q(5,1) .ne. AST__BAD ) call stopit( status, 'Interval 5' )
      if( q(5,2) .ne. AST__BAD ) call stopit( status, 'Interval 5b' )
      if( q(5,3) .ne. AST__BAD ) call stopit( status, 'Interval 5c' )

      call ast_negate( int1, status )
      call ast_trann( int1, 5, 3, 5, p, .true., 3, 5, q, status )
      if( q(1,1) .ne. p(1,1)) call stopit( status, 'Interval 6' )
      if( q(1,2) .ne. p(1,2)) call stopit( status, 'Interval 6b' )
      if( q(1,3) .ne. p(1,3)) call stopit( status, 'Interval 6c' )
      if( q(2,1) .ne. p(2,1)) call stopit( status, 'Interval 7' )
      if( q(2,2) .ne. p(2,2)) call stopit( status, 'Interval 7b' )
      if( q(2,3) .ne. p(2,3)) call stopit( status, 'Interval 7c' )
      if( q(3,1) .ne. AST__BAD) call stopit( status, 'Interval 8' )
      if( q(3,2) .ne. AST__BAD) call stopit( status, 'Interval 8b' )
      if( q(3,3) .ne. AST__BAD) call stopit( status, 'Interval 8c' )
      if( q(4,1) .ne. p(4,1) ) call stopit( status, 'Interval 9' )
      if( q(4,2) .ne. p(4,2) ) call stopit( status, 'Interval 9b' )
      if( q(4,3) .ne. p(4,3) ) call stopit( status, 'Interval 9c' )
      if( q(5,1) .ne. p(5,1) ) call stopit( status, 'Interval 10' )
      if( q(5,2) .ne. p(5,2) ) call stopit( status, 'Interval 10b' )
      if( q(5,3) .ne. p(5,3) ) call stopit( status, 'Interval 10c' )

      call ast_set( int1, 'closed=0,negated=0', status )
      call ast_trann( int1, 5, 3, 5, p, .true., 3, 5, q, status )
      if( q(1,1) .ne. AST__BAD ) call stopit( status, 'Interval 11' )
      if( q(1,2) .ne. AST__BAD ) call stopit( status, 'Interval 11b' )
      if( q(1,3) .ne. AST__BAD ) call stopit( status, 'Interval 11c' )
      if( q(2,1) .ne. AST__BAD ) call stopit( status, 'Interval 12' )
      if( q(2,2) .ne. AST__BAD ) call stopit( status, 'Interval 12b' )
      if( q(2,3) .ne. AST__BAD ) call stopit( status, 'Interval 12c' )
      if( q(3,1) .ne. p(3,1)) call stopit( status, 'Interval 13' )
      if( q(3,2) .ne. p(3,2)) call stopit( status, 'Interval 13b' )
      if( q(3,3) .ne. p(3,3)) call stopit( status, 'Interval 13c' )
      if( q(4,1) .ne. AST__BAD ) call stopit( status, 'Interval 14' )
      if( q(4,2) .ne. AST__BAD ) call stopit( status, 'Interval 14b' )
      if( q(4,3) .ne. AST__BAD ) call stopit( status, 'Interval 14c' )
      if( q(5,1) .ne. AST__BAD ) call stopit( status, 'Interval 15' )
      if( q(5,2) .ne. AST__BAD ) call stopit( status, 'Interval 15b' )
      if( q(5,3) .ne. AST__BAD ) call stopit( status, 'Interval 15c' )

      call ast_negate( int1, status )
      call ast_trann( int1, 5, 3, 5, p, .true., 3, 5, q, status )
      if( q(1,1) .ne. AST__BAD) call stopit( status, 'Interval 16' )
      if( q(1,2) .ne. AST__BAD) call stopit( status, 'Interval 16b' )
      if( q(1,3) .ne. AST__BAD) call stopit( status, 'Interval 16c' )
      if( q(2,1) .ne. AST__BAD) call stopit( status, 'Interval 17' )
      if( q(2,2) .ne. AST__BAD) call stopit( status, 'Interval 17b' )
      if( q(2,3) .ne. AST__BAD) call stopit( status, 'Interval 17c' )
      if( q(3,1) .ne. AST__BAD) call stopit( status, 'Interval 18' )
      if( q(3,2) .ne. AST__BAD) call stopit( status, 'Interval 18b' )
      if( q(3,3) .ne. AST__BAD) call stopit( status, 'Interval 18c' )
      if( q(4,1) .ne. p(4,1) ) call stopit( status, 'Interval 19' )
      if( q(4,2) .ne. p(4,2) ) call stopit( status, 'Interval 19b' )
      if( q(4,3) .ne. p(4,3) ) call stopit( status, 'Interval 19c' )
      if( q(5,1) .ne. p(5,1) ) call stopit( status, 'Interval 11' )
      if( q(5,2) .ne. p(5,2) ) call stopit( status, 'Interval 11b' )
      if( q(5,3) .ne. p(5,3) ) call stopit( status, 'Interval 11c' )


      lbnd( 1 ) = AST__BAD
      lbnd( 2 ) = AST__BAD
      lbnd( 3 ) = 6000.0
      ubnd( 1 ) = AST__BAD
      ubnd( 2 ) = AST__BAD
      ubnd( 3 ) = 5000.0

      int2 = ast_interval( frm3, lbnd, ubnd, AST__NULL, ' ', status )
      call checkdump( int2, 'checkdump int2', status )

      p(1,1) = 0.0            ! On boundary
      p(1,2) = 0.0
      p(1,3) = 5000.0
      p(2,1) = 2.0            ! On boundary
      p(2,2) = -1.0
      p(2,3) = 6000.0
      p(3,1) = -2.0           ! Outside
      p(3,2) = 1.0
      p(3,3) = 5999.0
      p(4,1) = 2.0            ! Inside
      p(4,2) = -2.0
      p(4,3) = 6010.0
      p(5,1) = 1.0            ! Inside
      p(5,2) = -1.0
      p(5,3) = 4910.0

      call ast_negate( int2, status )
      call ast_trann( int2, 5, 3, 5, p, .true., 3, 5, q, status )
      if( q(1,1) .ne. p(1,1)) call stopit( status, 'Interval B 1' )
      if( q(1,2) .ne. p(1,2)) call stopit( status, 'Interval B 1b' )
      if( q(1,3) .ne. p(1,3)) call stopit( status, 'Interval B 1c' )
      if( q(2,1) .ne. p(2,1)) call stopit( status, 'Interval B 2' )
      if( q(2,2) .ne. p(2,2)) call stopit( status, 'Interval B 2b' )
      if( q(2,3) .ne. p(2,3)) call stopit( status, 'Interval B 2c' )
      if( q(3,1) .ne. p(3,1)) call stopit( status, 'Interval B 3' )
      if( q(3,2) .ne. p(3,2)) call stopit( status, 'Interval B 3b' )
      if( q(3,3) .ne. p(3,3)) call stopit( status, 'Interval B 3c' )
      if( q(4,1) .ne. AST__BAD ) call stopit( status, 'Interval B 4' )
      if( q(4,2) .ne. AST__BAD ) call stopit( status, 'Interval B 4b' )
      if( q(4,3) .ne. AST__BAD ) call stopit( status, 'Interval B 4c' )
      if( q(5,1) .ne. AST__BAD ) call stopit( status, 'Interval B 5' )
      if( q(5,2) .ne. AST__BAD ) call stopit( status, 'Interval B 5b' )
      if( q(5,3) .ne. AST__BAD ) call stopit( status, 'Interval B 5c' )

      call ast_negate( int2, status )
      call ast_trann( int2, 5, 3, 5, p, .true., 3, 5, q, status )
      if( q(1,1) .ne. p(1,1)) call stopit( status, 'Interval B 6' )
      if( q(1,2) .ne. p(1,2)) call stopit( status, 'Interval B 6b' )
      if( q(1,3) .ne. p(1,3)) call stopit( status, 'Interval B 6c' )
      if( q(2,1) .ne. p(2,1)) call stopit( status, 'Interval B 7' )
      if( q(2,2) .ne. p(2,2)) call stopit( status, 'Interval B 7b' )
      if( q(2,3) .ne. p(2,3)) call stopit( status, 'Interval B 7c' )
      if( q(3,1) .ne. AST__BAD) call stopit( status, 'Interval B 8' )
      if( q(3,2) .ne. AST__BAD) call stopit( status, 'Interval B 8b' )
      if( q(3,3) .ne. AST__BAD) call stopit( status, 'Interval B 8c' )
      if( q(4,1) .ne. p(4,1) ) call stopit( status, 'Interval B 9' )
      if( q(4,2) .ne. p(4,2) ) call stopit( status, 'Interval B 9b' )
      if( q(4,3) .ne. p(4,3) ) call stopit( status, 'Interval B 9c' )
      if( q(5,1) .ne. p(5,1) ) call stopit( status, 'Interval B 10' )
      if( q(5,2) .ne. p(5,2) ) call stopit( status, 'Interval B 10b' )
      if( q(5,3) .ne. p(5,3) ) call stopit( status, 'Interval B 10c' )

      call ast_set( int2, 'closed=0,negated=1', status )
      call ast_trann( int2, 5, 3, 5, p, .true., 3, 5, q, status )
      if( q(1,1) .ne. AST__BAD ) call stopit( status, 'Interval B 11' )
      if( q(1,2) .ne. AST__BAD ) call stopit( status, 'Interval B 11b' )
      if( q(1,3) .ne. AST__BAD ) call stopit( status, 'Interval B 11c' )
      if( q(2,1) .ne. AST__BAD ) call stopit( status, 'Interval B 12' )
      if( q(2,2) .ne. AST__BAD ) call stopit( status, 'Interval B 12b' )
      if( q(2,3) .ne. AST__BAD ) call stopit( status, 'Interval B 12c' )
      if( q(3,1) .ne. p(3,1)) call stopit( status, 'Interval B 13' )
      if( q(3,2) .ne. p(3,2)) call stopit( status, 'Interval B 13b' )
      if( q(3,3) .ne. p(3,3)) call stopit( status, 'Interval B 13c' )
      if( q(4,1) .ne. AST__BAD ) call stopit( status, 'Interval B 14' )
      if( q(4,2) .ne. AST__BAD ) call stopit( status, 'Interval B 14b' )
      if( q(4,3) .ne. AST__BAD ) call stopit( status, 'Interval B 14c' )
      if( q(5,1) .ne. AST__BAD ) call stopit( status, 'Interval B 15' )
      if( q(5,2) .ne. AST__BAD ) call stopit( status, 'Interval B 15b' )
      if( q(5,3) .ne. AST__BAD ) call stopit( status, 'Interval B 15c' )

      call ast_negate( int2, status )
      call ast_trann( int2, 5, 3, 5, p, .true., 3, 5, q, status )
      if( q(1,1) .ne. AST__BAD) call stopit( status, 'Interval B 16' )
      if( q(1,2) .ne. AST__BAD) call stopit( status, 'Interval B 16b' )
      if( q(1,3) .ne. AST__BAD) call stopit( status, 'Interval B 16c' )
      if( q(2,1) .ne. AST__BAD) call stopit( status, 'Interval B 17' )
      if( q(2,2) .ne. AST__BAD) call stopit( status, 'Interval B 17b' )
      if( q(2,3) .ne. AST__BAD) call stopit( status, 'Interval B 17c' )
      if( q(3,1) .ne. AST__BAD) call stopit( status, 'Interval B 18' )
      if( q(3,2) .ne. AST__BAD) call stopit( status, 'Interval B 18b' )
      if( q(3,3) .ne. AST__BAD) call stopit( status, 'Interval B 18c' )
      if( q(4,1) .ne. p(4,1) ) call stopit( status, 'Interval B 19' )
      if( q(4,2) .ne. p(4,2) ) call stopit( status, 'Interval B 19b' )
      if( q(4,3) .ne. p(4,3) ) call stopit( status, 'Interval B 19c' )
      if( q(5,1) .ne. p(5,1) ) call stopit( status, 'Interval B 11' )
      if( q(5,2) .ne. p(5,2) ) call stopit( status, 'Interval B 11b' )
      if( q(5,3) .ne. p(5,3) ) call stopit( status, 'Interval B 11c' )




      lbnd( 1 ) = AST__BAD
      lbnd( 2 ) = AST__BAD
      lbnd( 3 ) = 5000.0
      ubnd( 1 ) = 0.5
      ubnd( 2 ) = AST__BAD
      ubnd( 3 ) = AST__BAD

      int3 = ast_interval( frm3, lbnd, ubnd, AST__NULL, ' ', status )

      call checkdump( int3, 'checkdump int3', status )

      p(1,1) = 0.0            ! On boundary
      p(1,2) = 0.0
      p(1,3) = 5000.0
      p(2,1) = 0.5            ! On boundary
      p(2,2) = -1.0
      p(2,3) = 6000.0
      p(3,1) = -2.0           ! Inside
      p(3,2) = 0.4
      p(3,3) = 5999.0
      p(4,1) = 2.0            ! Outside
      p(4,2) = -2.0
      p(4,3) = 6010.0
      p(5,1) = 0.0            ! Outside
      p(5,2) = -3.0
      p(5,3) = 4910.0

      call ast_trann( int3, 5, 3, 5, p, .true., 3, 5, q, status )
      if( q(1,1) .ne. p(1,1)) call stopit( status, 'Interval C 1' )
      if( q(1,2) .ne. p(1,2)) call stopit( status, 'Interval C 1b' )
      if( q(1,3) .ne. p(1,3)) call stopit( status, 'Interval C 1c' )
      if( q(2,1) .ne. p(2,1)) call stopit( status, 'Interval C 2' )
      if( q(2,2) .ne. p(2,2)) call stopit( status, 'Interval C 2b' )
      if( q(2,3) .ne. p(2,3)) call stopit( status, 'Interval C 2c' )
      if( q(3,1) .ne. p(3,1)) call stopit( status, 'Interval C 3' )
      if( q(3,2) .ne. p(3,2)) call stopit( status, 'Interval C 3b' )
      if( q(3,3) .ne. p(3,3)) call stopit( status, 'Interval C 3c' )
      if( q(4,1) .ne. AST__BAD ) call stopit( status, 'Interval C 4' )
      if( q(4,2) .ne. AST__BAD ) call stopit( status, 'Interval C 4b' )
      if( q(4,3) .ne. AST__BAD ) call stopit( status, 'Interval C 4c' )
      if( q(5,1) .ne. AST__BAD ) call stopit( status, 'Interval C 5' )
      if( q(5,2) .ne. AST__BAD ) call stopit( status, 'Interval C 5b' )
      if( q(5,3) .ne. AST__BAD ) call stopit( status, 'Interval C 5c' )

      call ast_negate( int3, status )
      call ast_trann( int3, 5, 3, 5, p, .true., 3, 5, q, status )
      if( q(1,1) .ne. p(1,1)) call stopit( status, 'Interval C 6' )
      if( q(1,2) .ne. p(1,2)) call stopit( status, 'Interval C 6b' )
      if( q(1,3) .ne. p(1,3)) call stopit( status, 'Interval C 6c' )
      if( q(2,1) .ne. p(2,1)) call stopit( status, 'Interval C 7' )
      if( q(2,2) .ne. p(2,2)) call stopit( status, 'Interval C 7b' )
      if( q(2,3) .ne. p(2,3)) call stopit( status, 'Interval C 7c' )
      if( q(3,1) .ne. AST__BAD) call stopit( status, 'Interval C 8' )
      if( q(3,2) .ne. AST__BAD) call stopit( status, 'Interval C 8b' )
      if( q(3,3) .ne. AST__BAD) call stopit( status, 'Interval C 8c' )
      if( q(4,1) .ne. p(4,1) ) call stopit( status, 'Interval C 9' )
      if( q(4,2) .ne. p(4,2) ) call stopit( status, 'Interval C 9b' )
      if( q(4,3) .ne. p(4,3) ) call stopit( status, 'Interval C 9c' )
      if( q(5,1) .ne. p(5,1) ) call stopit( status, 'Interval C 10' )
      if( q(5,2) .ne. p(5,2) ) call stopit( status, 'Interval C 10b' )
      if( q(5,3) .ne. p(5,3) ) call stopit( status, 'Interval C 10c' )

      call ast_set( int3, 'closed=0,negated=0', status )
      call ast_trann( int3, 5, 3, 5, p, .true., 3, 5, q, status )
      if( q(1,1) .ne. AST__BAD ) call stopit( status, 'Interval C 11' )
      if( q(1,2) .ne. AST__BAD ) call stopit( status, 'Interval C 11b' )
      if( q(1,3) .ne. AST__BAD ) call stopit( status, 'Interval C 11c' )
      if( q(2,1) .ne. AST__BAD ) call stopit( status, 'Interval C 12' )
      if( q(2,2) .ne. AST__BAD ) call stopit( status, 'Interval C 12b' )
      if( q(2,3) .ne. AST__BAD ) call stopit( status, 'Interval C 12c' )
      if( q(3,1) .ne. p(3,1)) call stopit( status, 'Interval C 13' )
      if( q(3,2) .ne. p(3,2)) call stopit( status, 'Interval C 13b' )
      if( q(3,3) .ne. p(3,3)) call stopit( status, 'Interval C 13c' )
      if( q(4,1) .ne. AST__BAD ) call stopit( status, 'Interval C 14' )
      if( q(4,2) .ne. AST__BAD ) call stopit( status, 'Interval C 14b' )
      if( q(4,3) .ne. AST__BAD ) call stopit( status, 'Interval C 14c' )
      if( q(5,1) .ne. AST__BAD ) call stopit( status, 'Interval C 15' )
      if( q(5,2) .ne. AST__BAD ) call stopit( status, 'Interval C 15b' )
      if( q(5,3) .ne. AST__BAD ) call stopit( status, 'Interval C 15c' )

      call ast_negate( int3, status )
      call ast_trann( int3, 5, 3, 5, p, .true., 3, 5, q, status )
      if( q(1,1) .ne. AST__BAD) call stopit( status, 'Interval C 16' )
      if( q(1,2) .ne. AST__BAD) call stopit( status, 'Interval C 16b' )
      if( q(1,3) .ne. AST__BAD) call stopit( status, 'Interval C 16c' )
      if( q(2,1) .ne. AST__BAD) call stopit( status, 'Interval C 17' )
      if( q(2,2) .ne. AST__BAD) call stopit( status, 'Interval C 17b' )
      if( q(2,3) .ne. AST__BAD) call stopit( status, 'Interval C 17c' )
      if( q(3,1) .ne. AST__BAD) call stopit( status, 'Interval C 18' )
      if( q(3,2) .ne. AST__BAD) call stopit( status, 'Interval C 18b' )
      if( q(3,3) .ne. AST__BAD) call stopit( status, 'Interval C 18c' )
      if( q(4,1) .ne. p(4,1) ) call stopit( status, 'Interval C 19' )
      if( q(4,2) .ne. p(4,2) ) call stopit( status, 'Interval C 19b' )
      if( q(4,3) .ne. p(4,3) ) call stopit( status, 'Interval C 19c' )
      if( q(5,1) .ne. p(5,1) ) call stopit( status, 'Interval C 11' )
      if( q(5,2) .ne. p(5,2) ) call stopit( status, 'Interval C 11b' )
      if( q(5,3) .ne. p(5,3) ) call stopit( status, 'Interval C 11c' )



      lbnd( 1 ) = AST__BAD
      lbnd( 2 ) = 0.0
      lbnd( 3 ) = AST__BAD
      ubnd( 1 ) = AST__BAD
      ubnd( 2 ) = -1.0
      ubnd( 3 ) = 6000.0

      call ast_setl( int3, 'Negated', .false., status )
      int4 = ast_interval( frm3, lbnd, ubnd, AST__NULL, ' ', status )
      if( ast_overlap( int3, int4, status ) .ne. 4 )
     :    call stopit( status, 'Interval overlap 1' )

      call ast_negate( int3, status )
      if( ast_overlap( int3, int4, status ) .ne. 4 )
     :    call stopit( status, 'Interval overlap 2' )

      call ast_negate( int4, status )
      if( ast_overlap( int3, int4, status ) .ne. 4 )
     :    call stopit( status, 'Interval overlap 3' )

      call ast_negate( int3, status )
      if( ast_overlap( int3, int4, status ) .ne. 4 )
     :    call stopit( status, 'Interval overlap 4' )


      lbnd( 1 ) = 0.6
      lbnd( 2 ) = 0.0
      lbnd( 3 ) = AST__BAD
      ubnd( 1 ) = AST__BAD
      ubnd( 2 ) = -1.0
      ubnd( 3 ) = 6000.0

      int4 = ast_interval( frm3, lbnd, ubnd, AST__NULL, ' ', status )
      if( ast_overlap( int3, int4, status ) .ne. 1 )
     :    call stopit( status, 'Interval overlap 5' )

      call ast_negate( int3, status )
      if( ast_overlap( int3, int4, status ) .ne. 3 )
     :    call stopit( status, 'Interval overlap 6' )

      call ast_negate( int4, status )
      if( ast_overlap( int3, int4, status ) .ne. 4 )
     :    call stopit( status, 'Interval overlap 7' )

      call ast_negate( int3, status )
      if( ast_overlap( int3, int4, status ) .ne. 2 )
     :    call stopit( status, 'Interval overlap 8' )


      int4 = ast_copy( int3, status )
      if( ast_overlap( int3, int4, status ) .ne. 5 )
     :    call stopit( status, 'Interval overlap 9' )

      call ast_negate( int4, status )
      if( ast_overlap( int3, int4, status ) .ne. 6 )
     :    call stopit( status, 'Interval overlap 10' )



*  Changing the number of axes in the Interval.

      frm1 = ast_frame( 2, 'Domain=A', status )

      lbnd(1) = 0.0
      lbnd(2) = 0.0
      ubnd(1) = 0.01
      ubnd(2) = 0.01
      unc = ast_box( frm1, 0, lbnd, ubnd, AST__NULL, ' ', status )

      lbnd(1) = -2.0
      lbnd(2) = 0.5
      ubnd(1) = 0.0
      ubnd(2) = AST__BAD
      int1 = ast_interval( frm1, lbnd, ubnd, unc, ' ', status )

      outperm(1) = 2
      outperm(2) = -1
      outperm(3) = 1

      inperm(1) = 3
      inperm(2) = 1

      pm = ast_permmap( 2, inperm, 3, outperm, 0.0D0, ' ', status )

      frm2 = ast_frame( 3, 'Domain=B', status )
      reg = ast_mapregion( int1, pm, frm2, status )

      if( .not. ast_isainterval( reg, status ) ) call stopit( status,
     :                                            'Int: perm check 1' )
      if( hasFrameSet( reg, status ) ) call stopit( status,
     :                                            'Int: perm check 2' )
      if( ast_geti( reg, 'naxes', status ) .ne. 3 ) call stopit( status,
     :                                            'Int: perm check 3' )

      in( 1, 1 ) = 0.0    ! Outside
      in( 1, 2 ) = 0.0
      in( 1, 3 ) = -0.5
      in( 2, 1 ) = 20.0   ! Inside
      in( 2, 2 ) = 0.0
      in( 2, 3 ) = -0.5
      in( 3, 1 ) = 20.0   ! Outside
      in( 3, 2 ) = -10.0
      in( 3, 3 ) = 0.5
      in( 4, 1 ) = 20.0   ! Boundary
      in( 4, 2 ) = 0.0
      in( 4, 3 ) = -2.0

      call ast_trann( reg, 4, 3, 4, in, .true., 3, 4, out, status )

      if( out( 1, 1 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 1' )
      if( out( 1, 2 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 2' )
      if( out( 1, 3 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 3' )

      if( out( 2, 1 ) .ne. in( 2,1 )) call stopit( status, 'Int: pc 4' )
      if( out( 2, 2 ) .ne. in( 2,2 )) call stopit( status, 'Int: pc 5' )
      if( out( 2, 3 ) .ne. in( 2,3 )) call stopit( status, 'Int: pc 6' )

      if( out( 3, 1 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 7' )
      if( out( 3, 2 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 8' )
      if( out( 3, 3 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 9' )

      if( out( 4, 1 ) .ne. in( 4,1 )) call stopit( status, 'Int: pc 10')
      if( out( 4, 2 ) .ne. in( 4,2 )) call stopit( status, 'Int: pc 11')
      if( out( 4, 3 ) .ne. in( 4,3 )) call stopit( status, 'Int: pc 12')



      outperm(1) = 2
      outperm(2) = -1
      outperm(3) = 1

      inperm(1) = 3
      inperm(2) = 1

      pm = ast_permmap( 2, inperm, 3, outperm, 1.5D0, ' ', status )

      frm2 = ast_frame( 3, 'Domain=B', status )
      reg = ast_mapregion( int1, pm, frm2, status )

      if( .not. ast_isainterval( reg, status ) ) call stopit( status,
     :                                            'Int: perm check 4' )
      if( hasFrameSet( reg, status ) ) call stopit( status,
     :                                            'Int: perm check 5' )
      if( ast_geti( reg, 'naxes', status ) .ne. 3 ) call stopit( status,
     :                                            'Int: perm check 6' )

      in( 1, 1 ) = 20.0   ! Outside
      in( 1, 2 ) =  0.0
      in( 1, 3 ) = -0.5
      in( 2, 1 ) = 20.0   ! Inside
      in( 2, 2 ) = 1.5
      in( 2, 3 ) = -0.5
      in( 3, 1 ) = 20.0   ! Outside
      in( 3, 2 ) = 1.6
      in( 3, 3 ) = -0.5
      in( 4, 1 ) = 0.5   ! Boundary
      in( 4, 2 ) = 1.5
      in( 4, 3 ) = 0.0

      call ast_trann( reg, 4, 3, 4, in, .true., 3, 4, out, status )

      if( out( 1, 1 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 13')
      if( out( 1, 2 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 14')
      if( out( 1, 3 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 15')

      if( out( 2, 1 ) .ne. in( 2,1 )) call stopit( status, 'Int: pc 16')
      if( out( 2, 2 ) .ne. in( 2,2 )) call stopit( status, 'Int: pc 17')
      if( out( 2, 3 ) .ne. in( 2,3 )) call stopit( status, 'Int: pc 18')

      if( out( 3, 1 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 19')
      if( out( 3, 2 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 20')
      if( out( 3, 3 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 21')

      if( out( 4, 1 ) .ne. in( 4,1 )) call stopit( status, 'Int: pc 22')
      if( out( 4, 2 ) .ne. in( 4,2 )) call stopit( status, 'Int: pc 23')
      if( out( 4, 3 ) .ne. in( 4,3 )) call stopit( status, 'Int: pc 24')



      call ast_negate( int1, status )
      call ast_set( int1, 'closed=0', status )
      reg = ast_mapregion( int1, pm, frm2, status )
      call ast_negate( int1, status )
      call ast_set( int1, 'closed=1', status )

      if( .not. ast_isainterval( reg, status ) ) call stopit( status,
     :                                            'Int: perm check 7' )
      if( hasFrameSet( reg, status ) ) call stopit( status,
     :                                            'Int: perm check 8' )
      if( ast_geti( reg, 'naxes', status ) .ne. 3 ) call stopit( status,
     :                                            'Int: perm check 9' )

      in( 1, 1 ) = 20.0   ! Inside
      in( 1, 2 ) =  0.0
      in( 1, 3 ) = -0.5
      in( 2, 1 ) = 20.0   ! Outside
      in( 2, 2 ) = 1.5
      in( 2, 3 ) = -0.5
      in( 3, 1 ) = 20.0   ! Inside
      in( 3, 2 ) = 1.6
      in( 3, 3 ) = -0.5
      in( 4, 1 ) = 0.5   ! Outside
      in( 4, 2 ) = 1.5
      in( 4, 3 ) = 0.0

      call ast_trann( reg, 4, 3, 4, in, .true., 3, 4, out, status )

      if( out( 1, 1 ) .ne. in( 1,1 )) call stopit( status, 'Int: pc 25')
      if( out( 1, 2 ) .ne. in( 1,2 )) call stopit( status, 'Int: pc 26')
      if( out( 1, 3 ) .ne. in( 1,3 )) call stopit( status, 'Int: pc 27')

      if( out( 2, 1 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 28')
      if( out( 2, 2 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 29')
      if( out( 2, 3 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 30')

      if( out( 3, 1 ) .ne. in( 3,1 )) call stopit( status, 'Int: pc 31')
      if( out( 3, 2 ) .ne. in( 3,2 )) call stopit( status, 'Int: pc 32')
      if( out( 3, 3 ) .ne. in( 3,3 )) call stopit( status, 'Int: pc 33')

      if( out( 4, 1 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 34')
      if( out( 4, 2 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 35')
      if( out( 4, 3 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 36')




      frm1 = ast_frame( 3, 'Domain=A', status )


      lbnd(1) = 0.0
      lbnd(2) = 0.0
      lbnd(3) = 0.0
      ubnd(1) = 0.01
      ubnd(2) = 0.01
      ubnd(3) = 0.01
      unc = ast_box( frm1, 0, lbnd, ubnd, AST__NULL, ' ', status )

      lbnd(1) = 0.5
      lbnd(2) = -1.0
      lbnd(3) = -2.0
      ubnd(1) = AST__BAD
      ubnd(2) = AST__BAD
      ubnd(3) = 0.0

      int1 = ast_interval( frm1, lbnd, ubnd, unc, ' ', status )

      outperm(1) = 1
      outperm(2) = 3

      inperm(1) = 1
      inperm(2) = -1
      inperm(3) = 2

      pm = ast_permmap( 3, inperm, 2, outperm, 1.0D0, ' ', status )

      frm2 = ast_frame( 2, 'Domain=B', status )
      reg = ast_mapregion( int1, pm, frm2, status )

      if( .not. ast_isainterval( reg, status ) ) call stopit( status,
     :                                            'Int: perm check 10' )
      if( hasFrameSet( reg, status ) ) call stopit( status,
     :                                            'Int: perm check 11' )
      if( ast_geti( reg, 'naxes', status ) .ne. 2 ) call stopit( status,
     :                                            'Int: perm check 12' )

      xin( 1 ) = 0.4    ! Out
      yin( 1 ) = -1.0
      xin( 2 ) = 1.0    ! Out
      yin( 2 ) = 0.1
      xin( 3 ) = 1.0    ! Out
      yin( 3 ) = -2.1
      xin( 4 ) = 0.5    ! Boundary
      yin( 4 ) = -1.0
      xin( 5 ) = 10.0   ! In
      yin( 5 ) = -0.1
      xin( 6 ) =  0.55  ! Boundary
      yin( 6 ) = -2.0


      call ast_tran2( reg, 6, xin, yin, .true., xout, yout, status )

      if( xout( 1 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 37')
      if( yout( 1 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 38')
      if( xout( 2 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 39')
      if( yout( 2 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 40')
      if( xout( 3 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 41')
      if( yout( 3 ) .ne. AST__BAD ) call stopit( status, 'Int: pc 42')
      if( xout( 4 ) .ne. xin( 4 ) ) call stopit( status, 'Int: pc 43')
      if( yout( 4 ) .ne. yin( 4 ) ) call stopit( status, 'Int: pc 44')
      if( xout( 5 ) .ne. xin( 5 ) ) call stopit( status, 'Int: pc 45')
      if( yout( 5 ) .ne. yin( 5 ) ) call stopit( status, 'Int: pc 46')
      if( xout( 6 ) .ne. xin( 6 ) ) call stopit( status, 'Int: pc 47')
      if( yout( 6 ) .ne. yin( 6 ) ) call stopit( status, 'Int: pc 48')


      pm = ast_permmap( 3, inperm, 2, outperm, -2.0D0, ' ', status )
      reg = ast_mapregion( int1, pm, frm2, status )

      if( .not. ast_isanullregion( reg, status ) ) call stopit( status,
     :                                            'Int: perm check 13' )
      if( hasFrameSet( reg, status ) ) call stopit( status,
     :                                            'Int: perm check 14' )
      if( ast_geti( reg, 'naxes', status ) .ne. 2 ) call stopit( status,
     :                                            'Int: perm check 15' )
      if( ast_getl( reg, 'negated', status ) ) call stopit( status,
     :                                            'Int: perm check 16' )


      call ast_negate( int1, status )
      reg = ast_mapregion( int1, pm, frm2, status )

      if( .not. ast_isanullregion( reg, status ) ) call stopit( status,
     :                                            'Int: perm check 17' )
      if( hasFrameSet( reg, status ) ) call stopit( status,
     :                                            'Int: perm check 18' )
      if( ast_geti( reg, 'naxes', status ) .ne. 2 ) call stopit( status,
     :                                            'Int: perm check 19' )
      if( .NOT.ast_getl( reg, 'negated', status ) ) call stopit( status,
     :                                            'Int: perm check 20' )




      call ast_end( status )
      if( status .ne. sai__ok ) write(*,*) 'Interval tests failed'

      end


      subroutine checkPolygon( status )
      implicit none
      include 'AST_PAR'
      include 'SAE_PAR'

      integer status, frm, unc, pol1, pol2, f2, r2, r3, r4
      double precision pi, p(5,2), q(5,2), p1(2), p2(2)
      double precision xin(2), yin(2), xout(2), yout(2), lbnd(5),
     :                 ubnd(5)
      logical hasframeset


      if( status .ne.sai__ok ) return

      call ast_begin( status )

      pi = acos( -1.0d0 )

      frm = ast_SkyFrame( ' ', status )

      p1(1) = 0.0
      p1(2) = 0.5*pi
      p2(1) = 0.01
      unc = ast_circle( frm, 1, p1, p2, AST__NULL, ' ', status )

      p(1,1) = 0.0
      p(1,2) = 0.0
      p(2,1) = 1.0
      p(2,2) = 0.5*pi
      p(3,1) = 0.5*pi
      p(3,2) = 0.25*pi
      p(4,1) = 0.25*pi
      p(4,2) = 0.0
      p(5,1) = 0.25*pi
      p(5,2) = 0.25*pi

      pol1 = ast_polygon( frm, 5, 5, p, unc, 'closed=0', status )

      call checkdump( pol1, 'checkdump pol1', status )


      p(1,1) = 0.0            ! On boundary
      p(1,2) = 0.0
      p(2,1) = 1.0            ! Outside
      p(2,2) = 0.5*pi + 0.1
      p(3,1) = 0.5*pi - 0.1   ! Inside
      p(3,2) = 0.25*pi
      p(4,1) = 0.0            ! On boundary
      p(4,2) = 0.1
      p(5,1) = 0.25*pi        ! Inside
      p(5,2) = 0.25*pi + 0.1

      call ast_trann( pol1, 5, 2, 5, p, .true., 2, 5, q, status )

      if( q(1,1) .ne. AST__BAD ) call stopit( status, 'Poly 1' )
      if( q(1,2) .ne. AST__BAD ) call stopit( status, 'Poly 1b' )
      if( q(2,1) .ne. AST__BAD ) call stopit( status, 'Poly 2' )
      if( q(2,2) .ne. AST__BAD ) call stopit( status, 'Poly 2b' )
      if( q(3,1) .ne. p(3,1) ) call stopit( status, 'Poly 3' )
      if( q(3,2) .ne. p(3,2) ) call stopit( status, 'Poly 3b' )
      if( q(4,1) .ne. AST__BAD ) call stopit( status, 'Poly 4' )
      if( q(4,2) .ne. AST__BAD ) call stopit( status, 'Poly 4b' )
      if( q(5,1) .ne. p(5,1) ) call stopit( status, 'Poly 5' )
      if( q(5,2) .ne. p(5,2) ) call stopit( status, 'Poly 5b' )


      call ast_setl( pol1, 'closed', .true., status )
      call ast_trann( pol1, 5, 2, 5, p, .true., 2, 5, q, status )
      if( q(1,1) .ne. p(1,1) ) call stopit( status, 'Poly 6' )
      if( q(1,2) .ne. p(1,2) ) call stopit( status, 'Poly 6b' )
      if( q(2,1) .ne. AST__BAD ) call stopit( status, 'Poly 7' )
      if( q(2,2) .ne. AST__BAD ) call stopit( status, 'Poly 7b' )
      if( q(3,1) .ne. p(3,1) ) call stopit( status, 'Poly 8' )
      if( q(3,2) .ne. p(3,2) ) call stopit( status, 'Poly 8b' )
      if( q(4,1) .ne. p(4,1) ) call stopit( status, 'Poly 9' )
      if( q(4,2) .ne. p(4,2) ) call stopit( status, 'Poly 9b' )
      if( q(5,1) .ne. p(5,1) ) call stopit( status, 'Poly 10' )
      if( q(5,2) .ne. p(5,2) ) call stopit( status, 'Poly 10b' )

      call ast_setl( pol1, 'negated', .true., status )
      call ast_trann( pol1, 5, 2, 5, p, .true., 2, 5, q, status )
      if( q(1,1) .ne. p(1,1) ) call stopit( status, 'Poly 11' )
      if( q(1,2) .ne. p(1,2) ) call stopit( status, 'Poly 11b' )
      if( q(2,1) .ne. p(2,1) ) call stopit( status, 'Poly 12' )
      if( q(2,2) .ne. p(2,2) ) call stopit( status, 'Poly 12b' )
      if( q(3,1) .ne. AST__BAD ) call stopit( status, 'Poly 13' )
      if( q(3,2) .ne. AST__BAD ) call stopit( status, 'Poly 13b' )
      if( q(4,1) .ne. p(4,1) ) call stopit( status, 'Poly 14' )
      if( q(4,2) .ne. p(4,2) ) call stopit( status, 'Poly 14b' )
      if( q(5,1) .ne. AST__BAD ) call stopit( status, 'Poly 15' )
      if( q(5,2) .ne. AST__BAD ) call stopit( status, 'Poly 15b' )

      call ast_setl( pol1, 'closed', .false., status )
      call ast_trann( pol1, 5, 2, 5, p, .true., 2, 5, q, status )
      if( q(1,1) .ne. AST__BAD ) call stopit( status, 'Poly 16' )
      if( q(1,2) .ne. AST__BAD ) call stopit( status, 'Poly 16b' )
      if( q(2,1) .ne. p(2,1) ) call stopit( status, 'Poly 17' )
      if( q(2,2) .ne. p(2,2) ) call stopit( status, 'Poly 17b' )
      if( q(3,1) .ne. AST__BAD ) call stopit( status, 'Poly 18' )
      if( q(3,2) .ne. AST__BAD ) call stopit( status, 'Poly 18b' )
      if( q(4,1) .ne. AST__BAD ) call stopit( status, 'Poly 19' )
      if( q(4,2) .ne. AST__BAD ) call stopit( status, 'Poly 19b' )
      if( q(5,1) .ne. AST__BAD ) call stopit( status, 'Poly 20' )
      if( q(5,2) .ne. AST__BAD ) call stopit( status, 'Poly 20b' )


      if( hasframeset( pol1, status ) ) then
         call stopit( status, 'pol1 has FrameSet' )
      end if

      call ast_setc( pol1, 'system', 'fk5', status )
      call checkdump( pol1, 'checkdump pol2', status )

      if( .not. hasframeset( pol1, status ) ) then
         call stopit( status, 'pol1 does not have FrameSet' )
      end if

      call ast_seti( pol1, 'meshsize', 30, status )

      pol2 = ast_simplify( pol1, status )

      if( hasframeset( pol2, status ) ) then
         call stopit( status, 'pol2 has FrameSet' )
      end if



      frm = ast_SkyFrame( ' ', status )

      p1(1) = 0.0
      p1(2) = 0.5*pi
      p2(1) = 0.01
      unc = ast_circle( frm, 1, p1, p2, AST__NULL, ' ', status )

      p(1,1) = 1.5*pi
      p(1,2) = 0.4*pi
      p(2,1) = pi
      p(2,2) = 0.4*pi
      p(3,1) = 0.5*pi
      p(3,2) = 0.4*pi
      p(4,1) = 0.0
      p(4,2) = 0.4*pi

      pol1 = ast_polygon( frm, 4, 5, p, unc, ' ', status )

      xin(1) = 0.0
      yin(1) = 0.5*pi
      call ast_tran2( pol1, 1, xin, yin, .true., xout, yout, status )

      if( xout(1) .ne. xin(1) ) call stopit( status, 'Poly 21' )
      if( yout(1) .ne. yin(1) ) call stopit( status, 'Poly 22' )

      call ast_getregionbounds( pol1, lbnd, ubnd, status )
      if( abs( lbnd(1) ) .gt. 1.0E-10 ) call stopit( status, 'Poly 23' )
      if( abs( lbnd(2) - 1.25663708 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Poly 24' )
      if( abs( ubnd(1) - 6.28318531 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Poly 25' )
      if( abs( ubnd(2) - 1.57079633 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Poly 26' )


      f2 = ast_specframe( 'Unit=Angstrom', status )
      lbnd( 1 ) = 5000.0
      ubnd( 1 ) = 6000.0
      r2 = ast_interval( f2, lbnd, ubnd, AST__NULL, ' ', status )
      r3 = ast_prism( pol1, r2, ' ', status )
      r4 = ast_Simplify( r3, status )

      call ast_getregionbounds( r4, lbnd, ubnd, status )
      if( abs( lbnd(1) ) .gt. 1.0E-10 ) call stopit( status, 'Poly 27' )
      if( abs( lbnd(2) - 1.25663708 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Poly 28' )
      if( abs( ubnd(1) - 6.28318531 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Poly 29' )
      if( abs( ubnd(2) - 1.57079633 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Poly 30' )
      if( abs( lbnd(3) - 5000.0 ) .gt. 1.0E-10 )
     :           call stopit( status, 'Poly 31' )
      if( abs( ubnd(3) - 6000.0 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Poly 32' )







      call ast_end( status )
      if( status .ne. sai__ok ) write(*,*) 'Polygon tests failed'

      end





      subroutine checkBox( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'PRM_PAR'

      integer status, box1, frm1, i, fc, fs, map1, perm(3), frm2, box3,
     :                box2, frm3, map2, res, j, bfrm, cfrm, reg1, map,
     :                npoint
      double precision p1(3), p2(3), v2, xin(9), yin(9), xout(9),
     :                 yout(9),in(4,3),out(4,3),matrix(9),grid(250,2)
      character*(AST__SZCHR) t1, t2, cards(9)*80
      logical hasframeset

      integer lbnd_in(2), ubnd_in(2)
      real rin(5,5),image(50,50)
      integer outperm(3), inperm(3),pm, reg, unc


      data cards /'CTYPE1  = ''RA---TAN''',
     :            'CTYPE2  = ''DEC--TAN''',
     :            'CRPIX1  = 100',
     :            'CRPIX2  = 100',
     :            'CRVAL1  = 71.619724',
     :            'CRVAL2  = 42.971835',
     :            '  ',
     :            'CDELT1  = 0.6',
     :            'CDELT2  = 0.6' /

      if( status .ne. sai__ok ) return

      call ast_begin( status )

      fc = ast_fitschan( ast_null, ast_null, ' ', status )
      do i = 1, 9
         call ast_putfits( fc, cards(i), .false., status )
      end do
      call ast_clear( fc, 'card', status )
      fs = ast_read( fc, status )

      frm1 = ast_getframe( fs, ast__current, status )
      call ast_seti( frm1, 'digits(1)', 12, status )

      p1( 1 ) = 1.25
      p1( 2 ) = 0.75
      p2( 1 ) = 1.5
      p2( 2 ) = 0.5

      box1 = ast_box( frm1, 0, p1, p2, AST__NULL, ' ', status )
      call checkdump( box1, 'checkdump box1', status )

      if( ast_getc( box1, 'system', status ) .ne. 'ICRS' )
     :        call stopit( status, 'box1 system is not ICRS' )

      call ast_setc( box1, 'system', 'galactic', status )

      perm(1)=2
      perm(2)=1
      call ast_permaxes( box1, perm, status )

      box3 = ast_copy( box1, status )

      yin(1) = 2.82175432250852
      xin(1) = -0.0269096590283195
      yin(2) = 2.70798275154741
      xin(2) = 0.2467384819891

      call ast_tran2( box1, 2, xin, yin, .true., xout, yout, status )

      if( abs( yout(1)-2.82175422 ) .gt. 1.0E-6 )
     :       call stopit( status, 'error 1' )
      if( abs( xout(1)+0.0269096587 ) .gt. 1.0E-7 )
     :       call stopit( status, 'error 2' )
      if( yout(2) .ne. AST__BAD ) call stopit( status, 'error 3' )
      if( xout(2) .ne. AST__BAD ) call stopit( status, 'error 4' )


      if( .not. ast_getl( box3, 'Adaptive', status ) )
     :      call stopit( status, 'error 4a' )

      call ast_setl( box3, 'Adaptive', .false., status )
      call ast_setc( box3, 'system', 'icrs', status )

      yin(1) = 2.82175432250852
      xin(1) = -0.0269096590283195
      yin(2) = 2.70798275154741
      xin(2) = 0.2467384819891

      call ast_tran2( box3, 2, xin, yin, .true., xout, yout, status )

      if( abs( yout(1)-2.82175422 ) .gt. 1.0E-8 )
     :       call stopit( status, 'error 1' )
      if( abs( xout(1)+0.0269096587 ) .gt. 1.0E-8 )
     :       call stopit( status, 'error 2' )
      if( yout(2) .ne. AST__BAD ) call stopit( status, 'error 4b' )
      if( xout(2) .ne. AST__BAD ) call stopit( status, 'error 4c' )

      call ast_clear( box3, 'system', status )

      yin(1) = 2.82175432250852
      xin(1) = -0.0269096590283195
      yin(2) = 2.70798275154741
      xin(2) = 0.2467384819891

      call ast_tran2( box3, 2, xin, yin, .true., xout, yout, status )

      if( abs( yout(1)-2.82175422 ) .gt. 1.0E-8 )
     :       call stopit( status, 'error 1' )
      if( abs( xout(1)+0.0269096587 ) .gt. 1.0E-8 )
     :       call stopit( status, 'error 2' )
      if( yout(2) .ne. AST__BAD ) call stopit( status, 'error 4d' )
      if( xout(2) .ne. AST__BAD ) call stopit( status, 'error 4e' )

      box2 = ast_simplify( box1, status )

      call ast_setc( box1, 'system', 'icrs', status )
      call ast_permaxes( box1, perm, status )

      t1 = ast_format( frm1, 1, 0.25D0, status )
      call ast_annul( frm1, status )
      t2 = ast_format( box1, 1, 0.25D0, status )
      if( t1 .ne. t2 ) call stopit( status,
     :       'ast_format is different for frm1 and box1' )

      i = ast_unformat( box1, 1, t2, v2, status )
      if( abs( v2 - 0.25 ) .GT. 1.0E-10 ) then
         call stopit( status, 'ast_unformat failed for box1' )
      end if

      if( ast_getc( box1, 'System', status ) .ne. 'ICRS' ) then
         call stopit( status, 'Box1(b) system is not ICRS' )
      end if

      if( ast_getc( box1, 'Equinox', status ) .ne. '2000.0' ) then
         call stopit( status, 'Box1 equinox is not 2000.0' )
      end if

      if( .not. ast_getl( box1, 'Closed', status ) ) then
         call stopit( status, 'Box1 closed is not .true.' )
      end if

      xin( 1 ) = 1.25
      yin( 1 ) = 0.75
      xin( 2 ) = 1.0
      yin( 2 ) = 1.0
      xin( 3 ) = 1.0
      yin( 3 ) = 0.5
      xin( 4 ) = 1.5
      yin( 4 ) = 0.5
      xin( 5 ) = 1.5
      yin( 5 ) = 1.0
      xin( 6 ) = 1.0
      yin( 6 ) = 1.2
      xin( 7 ) = 0.8
      yin( 7 ) = 0.5
      xin( 8 ) = 1.5
      yin( 8 ) = 0.45
      xin( 9 ) = 1.501
      yin( 9 ) = 1.0

      call ast_tran2( BOX1, 9, xin, yin, .true., xout, yout, status )

      if( xout( 1 ) .ne. 1.25 ) call stopit( status, 'error A1' )
      if( yout( 1 ) .ne. 0.75 ) call stopit( status, 'error A2' )
      if( xout( 2 ) .ne. 1.0 ) call stopit( status, 'error A3' )
      if( yout( 2 ) .ne. 1.0 ) call stopit( status, 'error A4' )
      if( xout( 3 ) .ne. 1.0 ) call stopit( status, 'error A5' )
      if( yout( 3 ) .ne. 0.5 ) call stopit( status, 'error A6' )
      if( xout( 4 ) .ne. 1.5 ) call stopit( status, 'error A7' )
      if( yout( 4 ) .ne. 0.5 ) call stopit( status, 'error A8' )
      if( xout( 5 ) .ne. 1.5 ) call stopit( status, 'error A9' )
      if( yout( 5 ) .ne. 1.0 ) call stopit( status, 'error A10' )
      if( xout( 6 ) .ne. AST__BAD ) call stopit( status, 'error A11' )
      if( yout( 6 ) .ne. AST__BAD ) call stopit( status, 'error A12' )
      if( xout( 7 ) .ne. AST__BAD ) call stopit( status, 'error A13' )
      if( yout( 7 ) .ne. AST__BAD ) call stopit( status, 'error A14' )
      if( xout( 8 ) .ne. AST__BAD ) call stopit( status, 'error A15' )
      if( yout( 8 ) .ne. AST__BAD ) call stopit( status, 'error A16' )
      if( xout( 9 ) .ne. AST__BAD ) call stopit( status, 'error A17' )
      if( yout( 9 ) .ne. AST__BAD ) call stopit( status, 'error A18' )

      call ast_tran2( box1, 9, xin, yin, .false., xout, yout, status )

      if( xout( 1 ) .ne. 1.25 ) call stopit( status, 'error B1' )
      if( yout( 1 ) .ne. 0.75 ) call stopit( status, 'error B2' )
      if( xout( 2 ) .ne. 1.0 ) call stopit( status, 'error B3' )
      if( yout( 2 ) .ne. 1.0 ) call stopit( status, 'error B4' )
      if( xout( 3 ) .ne. 1.0 ) call stopit( status, 'error B5' )
      if( yout( 3 ) .ne. 0.5 ) call stopit( status, 'error B6' )
      if( xout( 4 ) .ne. 1.5 ) call stopit( status, 'error B7' )
      if( yout( 4 ) .ne. 0.5 ) call stopit( status, 'error B8' )
      if( xout( 5 ) .ne. 1.5 ) call stopit( status, 'error B9' )
      if( yout( 5 ) .ne. 1.0 ) call stopit( status, 'error B10' )
      if( xout( 6 ) .ne. AST__BAD ) call stopit( status, 'error B11' )
      if( yout( 6 ) .ne. AST__BAD ) call stopit( status, 'error B12' )
      if( xout( 7 ) .ne. AST__BAD ) call stopit( status, 'error B13' )
      if( yout( 7 ) .ne. AST__BAD ) call stopit( status, 'error B14' )
      if( xout( 8 ) .ne. AST__BAD ) call stopit( status, 'error B15' )
      if( yout( 8 ) .ne. AST__BAD ) call stopit( status, 'error B16' )
      if( xout( 9 ) .ne. AST__BAD ) call stopit( status, 'error B17' )
      if( yout( 9 ) .ne. AST__BAD ) call stopit( status, 'error B18' )

      call ast_negate( box1, status )
      call ast_tran2( box1, 9, xin, yin, .true., xout, yout, status )

      if( xout( 1 ) .ne. AST__BAD ) call stopit( status, 'error C1' )
      if( yout( 1 ) .ne. AST__BAD ) call stopit( status, 'error C2' )
      if( xout( 2 ) .ne. 1.0 ) call stopit( status, 'error C3' )
      if( yout( 2 ) .ne. 1.0 ) call stopit( status, 'error C4' )
      if( xout( 3 ) .ne. 1.0 ) call stopit( status, 'error C5' )
      if( yout( 3 ) .ne. 0.5 ) call stopit( status, 'error C6' )
      if( xout( 4 ) .ne. 1.5 ) call stopit( status, 'error C7' )
      if( yout( 4 ) .ne. 0.5 ) call stopit( status, 'error C8' )
      if( xout( 5 ) .ne. 1.5 ) call stopit( status, 'error C9' )
      if( yout( 5 ) .ne. 1.0 ) call stopit( status, 'error C10' )
      if( xout( 6 ) .ne. 1.0 ) call stopit( status, 'error C11' )
      if( yout( 6 ) .ne. 1.2 ) call stopit( status, 'error C12' )
      if( xout( 7 ) .ne. 0.8 ) call stopit( status, 'error C13' )
      if( yout( 7 ) .ne. 0.5 ) call stopit( status, 'error C14' )
      if( xout( 8 ) .ne. 1.5 ) call stopit( status, 'error C15' )
      if( yout( 8 ) .ne. 0.45 ) call stopit( status, 'error C16' )
      if( xout( 9 ) .ne. 1.501 ) call stopit( status, 'error C17' )
      if( yout( 9 ) .ne. 1.0 ) call stopit( status, 'error C18' )

      call ast_setl( box1, 'closed', .false., status )
      call ast_negate( box1, status )
      call ast_tran2( box1, 9, xin, yin, .true., xout, yout, status )

      if( xout( 1 ) .ne. 1.25 ) call stopit( status, 'error D1' )
      if( yout( 1 ) .ne. 0.75 ) call stopit( status, 'error D2' )
      if( xout( 2 ) .ne. AST__BAD ) call stopit( status, 'error D3' )
      if( yout( 2 ) .ne. AST__BAD ) call stopit( status, 'error D4' )
      if( xout( 3 ) .ne. AST__BAD ) call stopit( status, 'error D5' )
      if( yout( 3 ) .ne. AST__BAD ) call stopit( status, 'error D6' )
      if( xout( 4 ) .ne. AST__BAD ) call stopit( status, 'error D7' )
      if( yout( 4 ) .ne. AST__BAD ) call stopit( status, 'error D8' )
      if( xout( 5 ) .ne. AST__BAD ) call stopit( status, 'error D9' )
      if( yout( 5 ) .ne. AST__BAD ) call stopit( status, 'error D10' )
      if( xout( 6 ) .ne. AST__BAD ) call stopit( status, 'error D11' )
      if( yout( 6 ) .ne. AST__BAD ) call stopit( status, 'error D12' )
      if( xout( 7 ) .ne. AST__BAD ) call stopit( status, 'error D13' )
      if( yout( 7 ) .ne. AST__BAD ) call stopit( status, 'error D14' )
      if( xout( 8 ) .ne. AST__BAD ) call stopit( status, 'error D15' )
      if( yout( 8 ) .ne. AST__BAD ) call stopit( status, 'error D16' )
      if( xout( 9 ) .ne. AST__BAD ) call stopit( status, 'error D17' )
      if( yout( 9 ) .ne. AST__BAD ) call stopit( status, 'error D18' )

      call ast_setl( box1, 'Negated', .true., status )
      call ast_tran2( box1, 9, xin, yin, .true., xout, yout, status )

      if( xout( 1 ) .ne. AST__BAD ) call stopit( status, 'error E1' )
      if( yout( 1 ) .ne. AST__BAD ) call stopit( status, 'error E2' )
      if( xout( 2 ) .ne. AST__BAD ) call stopit( status, 'error E3' )
      if( yout( 2 ) .ne. AST__BAD ) call stopit( status, 'error E4' )
      if( xout( 3 ) .ne. AST__BAD ) call stopit( status, 'error E5' )
      if( yout( 3 ) .ne. AST__BAD ) call stopit( status, 'error E6' )
      if( xout( 4 ) .ne. AST__BAD ) call stopit( status, 'error E7' )
      if( yout( 4 ) .ne. AST__BAD ) call stopit( status, 'error E8' )
      if( xout( 5 ) .ne. AST__BAD ) call stopit( status, 'error E9' )
      if( yout( 5 ) .ne. AST__BAD ) call stopit( status, 'error E10' )
      if( xout( 6 ) .ne. 1.0 ) call stopit( status, 'error E11' )
      if( yout( 6 ) .ne. 1.2 ) call stopit( status, 'error E12' )
      if( xout( 7 ) .ne. 0.8 ) call stopit( status, 'error E13' )
      if( yout( 7 ) .ne. 0.5 ) call stopit( status, 'error E14' )
      if( xout( 8 ) .ne. 1.5 ) call stopit( status, 'error E15' )
      if( yout( 8 ) .ne. 0.45 ) call stopit( status, 'error E16' )
      if( xout( 9 ) .ne. 1.501 ) call stopit( status, 'error E17' )
      if( yout( 9 ) .ne. 1.0 ) call stopit( status, 'error E18' )

      call ast_clear( box1, 'Negated', status )
      call ast_clear( box1, 'Closed', status )

      call ast_addframe( fs, ast__current, ast_unitmap(2,' ',status),
     :                   box1, status )



      map1 = ast_getmapping( fs, ast__current, ast__current, status )

      if( .not.ast_isaregion( map1, status ) )
     :         call stopit( status, 'map1 is not a Region' )

      call ast_setl( fs, 'Negated', .true., status )
      if( .not. ast_getl( box1, 'Negated', status ) )
     :           call stopit( status,
     :           'FrameSet Negated attribute not reflected in box1' )
      call ast_clear( fs, 'Negated', status )

      map1 = ast_getmapping( fs, ast__base, ast__current, status )

      call ast_tran2( map1, 9, xin, yin, .false., xout, yout, status )

      if( xout( 1 ) .eq. AST__BAD ) call stopit( status, 'error F1' )
      if( yout( 1 ) .eq. AST__BAD ) call stopit( status, 'error F2' )
      if( xout( 2 ) .eq. AST__BAD ) call stopit( status, 'error F3' )
      if( yout( 2 ) .eq. AST__BAD ) call stopit( status, 'error F4' )
      if( xout( 3 ) .eq. AST__BAD ) call stopit( status, 'error F5' )
      if( yout( 3 ) .eq. AST__BAD ) call stopit( status, 'error F6' )
      if( xout( 4 ) .eq. AST__BAD ) call stopit( status, 'error F7' )
      if( yout( 4 ) .eq. AST__BAD ) call stopit( status, 'error F8' )
      if( xout( 5 ) .eq. AST__BAD ) call stopit( status, 'error F9' )
      if( yout( 5 ) .eq. AST__BAD ) call stopit( status, 'error F10' )
      if( xout( 6 ) .ne. AST__BAD ) call stopit( status, 'error F11' )
      if( yout( 6 ) .ne. AST__BAD ) call stopit( status, 'error F12' )
      if( xout( 7 ) .ne. AST__BAD ) call stopit( status, 'error F13' )
      if( yout( 7 ) .ne. AST__BAD ) call stopit( status, 'error F14' )
      if( xout( 8 ) .ne. AST__BAD ) call stopit( status, 'error F15' )
      if( yout( 8 ) .ne. AST__BAD ) call stopit( status, 'error F16' )
      if( xout( 9 ) .ne. AST__BAD ) call stopit( status, 'error F17' )
      if( yout( 9 ) .ne. AST__BAD ) call stopit( status, 'error F18' )

      call ast_tran2( map1, 9, xout, yout, .true., xout, yout, status )

      if( abs( xout( 1 ) - 1.25 ) .gt. 1.0D-7 ) call stopit( status,
     :                                                  'error G1' )
      if( abs( yout( 1 ) - 0.75 ) .gt. 1.0D-7 ) call stopit( status,
     :                                                  'error G2' )
      if( xout( 6 ) .ne. AST__BAD ) call stopit( status, 'error G11' )
      if( yout( 6 ) .ne. AST__BAD ) call stopit( status, 'error G12' )
      if( xout( 7 ) .ne. AST__BAD ) call stopit( status, 'error G13' )
      if( yout( 7 ) .ne. AST__BAD ) call stopit( status, 'error G14' )
      if( xout( 8 ) .ne. AST__BAD ) call stopit( status, 'error G15' )
      if( yout( 8 ) .ne. AST__BAD ) call stopit( status, 'error G16' )
      if( xout( 9 ) .ne. AST__BAD ) call stopit( status, 'error G17' )
      if( yout( 9 ) .ne. AST__BAD ) call stopit( status, 'error G18' )




      frm2 = ast_specframe( 'Unit=Angstrom', status )
      p1( 1 ) = 1000.0
      p2( 1 ) = 1100.0
      box2 = ast_box( frm2, 0, p1, p2, AST__NULL, ' ', status )
      frm3 = ast_cmpframe( box1, box2, ' ', status )

      perm(1)=2
      perm(2)=3
      perm(3)=1
      call ast_permaxes( frm3, perm, status )
      call ast_setc( frm3, 'system(1)', 'galactic', status )
      call ast_setc( frm3, 'system(2)', 'Freq', status )

      in( 1, 1 ) = -0.0269096590283195  ! In both boxes
      in( 1, 2 ) = 2997924.58
      in( 1, 3 ) = 2.82175432250852
      in( 2, 1 ) = 0.2467384819891      ! In spec box, out sky box
      in( 2, 2 ) = 2997924.58
      in( 2, 3 ) = 2.70798275154741
      in( 3, 1 ) = -0.0269096590283195  ! Out spec box in sky box
      in( 3, 2 ) = 4000000.0
      in( 3, 3 ) = 2.82175432250852
      in( 4, 1 ) = 0.2467384819891      ! Out spec box, out sky box
      in( 4, 2 ) = 4000000.0
      in( 4, 3 ) = 2.70798275154741
      call ast_trann( frm3, 4, 3, 4, in, .true., 3, 4, out, status )

      if( abs( out(1,1)+0.0269096587 ) .gt. 1.0E-8 )
     :       call stopit( status, 'error H1' )
      if( abs( out(1,2)-2997924.5 ) .gt. 1.0E-1 )
     :       call stopit( status, 'error H2' )
      if( abs( out(1,3)-2.82175422 ) .gt. 1.0E-6 )
     :       call stopit( status, 'error H3' )

      if( out(2,1) .ne. ast__bad ) call stopit( status, 'error H4' )
      if( abs( out(2,2)-2997924.5 ) .gt. 1.0E-1 )
     :       call stopit( status, 'error H5' )
      if( out(2,3) .ne. ast__bad ) call stopit( status, 'error H6' )

      if( abs( out(3,1)+0.0269096587 ) .gt. 1.0E-8 )
     :       call stopit( status, 'error H7' )
      if( out(3,2) .ne. ast__bad ) call stopit( status, 'error H8' )
      if( abs( out(3,3)-2.82175422 ) .gt. 1.0E-6 )
     :       call stopit( status, 'error H9' )

      if( out(4,1) .ne. ast__bad ) call stopit( status, 'error H10' )
      if( out(4,2) .ne. ast__bad ) call stopit( status, 'error H11' )
      if( out(4,3) .ne. ast__bad ) call stopit( status, 'error H12' )

      if( .not. ast_getl( frm3, 'closed(1)', status ) )
     :    call stopit( status, 'compound frame region is not closed' )




C
C Testing astMapRegion
C

      frm1 = ast_frame( 3, 'Domain=A', status )
      p1(1) = 100
      p1(2) = 200
      p1(3) = 300
      p2(1) = 0
      p2(2) = 400
      p2(3) = 250
      box1 = ast_box( frm1, 0, p1, p2, AST__NULL, ' ', status )

      frm2 = ast_frame( 3, 'Domain=B', status )

      matrix(1) = 2.0
      matrix(2) = 0.0
      matrix(3) = 0.0
      matrix(4) = 0.0
      matrix(5) = 4.0
      matrix(6) = 0.0
      matrix(7) = 0.0
      matrix(8) = 0.0
      matrix(9) = 6.0

      map2 = ast_matrixmap( 3, 3, 0, matrix, ' ', status )
      box2 = ast_mapregion( box1, map2, frm2, status )

      if( ast_getc( box2, 'Domain', status ) .ne. 'B' ) then
         call stopit( status, 'ast_mapregion1: Box2 domain is not B' )
      end if

      if( hasframeset( box2, status ) ) then
         call stopit( status, 'ast_mapregion2: Box2 has FrameSet' )
      end if

      matrix(1) = 2.0
      matrix(2) = .1
      matrix(3) = 0.0
      matrix(4) = 0.0
      matrix(5) = 4.0
      matrix(6) = 0.0
      matrix(7) = 0.0
      matrix(8) = 0.0
      matrix(9) = 6.0

      map2 = ast_matrixmap( 3, 3, 0, matrix, ' ', status )

      box2 = ast_mapregion( box1, map2, frm2, status )

      if( ast_getc( box2, 'Domain', status ) .ne. 'B' ) then
         call stopit( status, 'ast_mapregion3: Box2 domain is not B' )
      end if

      if( hasframeset( box2, status ) ) then
         call stopit( status, 'ast_mapregion4: Box2 has FrameSet' )
      end if

      call checkdump( box2, 'checkdump box2', status )

      frm1 = ast_frame( 1, 'Domain=A', status )
      p1(1) = 100
      p2(1) = 0
      box1 = ast_box( frm1, 0, p1, p2, AST__NULL, ' ', status )

      frm2 = ast_frame( 1, 'Domain=B', status )

      map2 = ast_zoommap( 1, 2.0D0, ' ', status )
      box2 = ast_mapregion( box1, map2, frm2, status )

      if( ast_getc( box2, 'Domain', status ) .ne. 'B' ) then
         call stopit( status, 'ast_mapregion5: Box2 domain is not B' )
      end if

      if( hasframeset( box2, status ) ) then
         call stopit( status, 'ast_mapregion6: Box2 has FrameSet (B)' )
      end if

      frm1 = ast_skyframe( ' ', status )
      p1(1) = 0
      p1(2) = 0
      p2(1) = 0.001
      p2(2) = 0.001
      box1 = ast_box( frm1, 0, p1, p2, AST__NULL, ' ', status )

      frm2 = ast_copy( frm1, status )
      call ast_setd( frm2, 'skyref(1)', 0.0005D0, status )
      call ast_setc( frm2, 'skyrefis', 'origin', status )

      fs = ast_convert( frm1, frm2, ' ', status )

      box2 = ast_mapregion( box1, fs, frm2, status )

      if( hasframeset( box2, status ) ) then
         call stopit( status, 'ast_mapregion7: Box2 has FrameSet (C)' )
      end if

      xin( 1 ) = 0.00049
      yin( 1 ) = 0.0009
      xin( 2 ) = 0.00051
      yin( 2 ) = 0.0009
      xin( 3 ) = -0.0016
      yin( 3 ) = 0.0
      xin( 4 ) = -0.0014
      yin( 4 ) = 0.0
      xin( 5 ) = 6.2815853
      yin( 5 ) = 0.0
      xin( 6 ) = 6.2817853
      yin( 6 ) = 0.0

      call ast_tran2( box2, 6, xin, yin, .true., xout, yout, status )

      if( abs( xout( 1 ) - xin( 1 ) ) .gt. 1D-10 ) call stopit( status,
     :                                                  'error I1' )
      if( abs( yout( 1 ) - yin( 1 ) ) .gt. 1D-10 ) call stopit( status,
     :                                                  'error I2' )

      if( xout(2) .ne. AST__BAD ) call stopit( status, 'error I3' )
      if( yout(2) .ne. AST__BAD ) call stopit( status, 'error I4' )
      if( xout(5) .ne. AST__BAD ) call stopit( status, 'error I5' )
      if( yout(5) .ne. AST__BAD ) call stopit( status, 'error I6' )
      if( xout(3) .ne. AST__BAD ) call stopit( status, 'error I7' )
      if( yout(3) .ne. AST__BAD ) call stopit( status, 'error I8' )

      if( abs( xout( 4 ) - xin( 4 ) ) .gt. 1D-10 ) call stopit( status,
     :                                                  'error I9' )
      if( abs( yout( 4 ) - yin( 4 ) ) .gt. 1D-10 ) call stopit( status,
     :                                                  'error I10' )
      if( abs( xout( 6 ) - xin( 6 ) ) .gt. 1D-10 ) call stopit( status,
     :                                                  'error I11' )
      if( abs( yout( 6 ) - yin( 6 ) ) .gt. 1D-10 ) call stopit( status,
     :                                                  'error I12' )


      call ast_setc( box2, 'skyrefis', 'pole', status )
      box2 = ast_Simplify( box2, status )

      if( hasframeset( box2, status ) ) then
         call stopit( status, 'ast_mapregion8: Box2 has '//
     :               'FrameSet (B)' )
      end if

C
C Testing astOverlap
C

      frm1 = ast_frame( 3, 'Domain=A', status )
      p1(1) = 100
      p1(2) = 200
      p1(3) = 300
      p2(1) = 0
      p2(2) = 400
      p2(3) = 250
      box1 = ast_box( frm1, 0, p1, p2, AST__NULL, 'closed=1', status )

      frm2 = ast_frame( 3, 'Domain=B', status )
      box2 = ast_box( frm2, 0, p1, p2, AST__NULL, 'closed=0', status )

      if( ast_overlap( box1, box2, status ) .ne. 0 ) then
         call stopit( status, 'ast_overlap A: result should be zero' )
      end if

      if( ast_overlap( box1, box1, status ) .ne. 5 ) then
         call stopit( status, 'ast_overlap B: result should be 5' )
      end if

      if( ast_overlap( box2, box2, status ) .ne. 5 ) then
         call stopit( status, 'ast_overlap C: result should be 5' )
      end if

      call ast_setc( frm2, 'Domain', 'A', status )
      p1(1) = 100
      p1(2) = 200
      p1(3) = 300
      p2(1) = -100
      p2(2) = 600
      p2(3) = 400
      box2 = ast_box( frm2, 0, p1, p2, AST__NULL, ' ', status )

      if( ast_overlap( box1, box2, status ) .ne. 2 ) then
         write(*,*) 'Result is ',ast_overlap( box1, box2, status )
         call stopit( status, 'ast_overlap D: result should be 2' )
      end if

      if( ast_overlap( box2, box1, status ) .ne. 3 ) then
         write(*,*) 'Result is ',ast_overlap( box2, box1, status )
         call stopit( status, 'ast_overlap E: result should be 3' )
      end if

      p1(1) = 300
      p1(2) = 200
      p1(3) = 300
      p2(1) = 201
      p2(2) = 400
      p2(3) = 250
      box2 = ast_box( frm2, 0, p1, p2, AST__NULL, ' ', status )

      if( ast_overlap( box1, box2, status ) .ne. 1 ) then
         call stopit( status, 'ast_overlap F: result should be 1' )
      end if

      if( ast_overlap( box2, box1, status ) .ne. 1 ) then
         call stopit( status, 'ast_overlap G: result should be 1' )
      end if

      p1(1) = 150
      p1(2) = 200
      p1(3) = 300
      p2(1) = 50
      p2(2) = 400
      p2(3) = 250
      box2 = ast_box( frm2, 0, p1, p2, AST__NULL, ' ', status )

      if( ast_overlap( box1, box2, status ) .ne. 4 ) then
         call stopit( status, 'ast_overlap H: result should be 4' )
      end if

      if( ast_overlap( box2, box1, status ) .ne. 4 ) then
         call stopit( status, 'ast_overlap I: result should be 4' )
      end if


*  Pixel masks
      frm1 = ast_frame( 2, 'Domain=A', status )
      p1(1) = 1.0
      p1(2) = 1.0
      p2(1) = 3.1
      p2(2) = 4.1
      box1 = ast_box( frm1, 0, p1, p2, AST__NULL, ' ', status )

      lbnd_in(1) = 1
      lbnd_in(2) = 1
      ubnd_in(1) = 5
      ubnd_in(2) = 5

      do i =1, 5
         do j = 1, 5
            rin( j,i)=1.0
         end do
      end do

      res = ast_maskr( box1, AST__NULL, .false., 2, lbnd_in, ubnd_in,
     :                 rin, VAL__BADR, status )

      if( res .ne. 13 ) then
         write(*,*) 'Res is ',res
         call stopit( status, 'res should be 13' )
      end if

      do i =1, 5
         do j = 1, 5
            if( j .le. 3 .and. i .le. 4 ) then
               if( rin(j,i) .NE. 1.0 ) then
                  write(*,*) 'rin(',j,',',i,') = ',rin(j,i)
                  call stopit( status, 'Above value should be 1.0' )
               end if
            else
               if( rin(j,i) .NE. VAL__BADR ) then
                  write(*,*) 'rin(',j,',',i,') = ',rin(j,i)
                  call stopit( status, 'Above value should be '//
     :                         'VAL__BADR' )
               end if
            endif
         end do
      end do

      cards(3) = 'CRPIX1  = 20'
      cards(4) = 'CRPIX2  = 20'
      cards(5) = 'CRVAL1  = 0.0'
      cards(6) = 'CRVAL2  = 0.0'
      cards(7) = '   '
      cards(8) = 'CDELT1  = 1.6'
      cards(9) = 'CDELT2  = 1.6'

      fc = ast_fitschan( ast_null, ast_null, ' ', status )
      do i = 1, 9
         call ast_putfits( fc, cards(i), .false., status )
      end do
      call ast_clear( fc, 'card', status )
      fs = ast_read( fc, status )

      p1( 1 ) = 0.13089969  ! RA at centre = 0h30m
      p1( 2 ) = 0.17453293  ! Dec at centre = 10d
      p2( 1 ) = -0.13089971 ! RA at corner = 23h30m
      p2( 2 ) = -0.17453293 ! Dec at corner = -10d

      box1 = ast_box( fs, 0, p1, p2, AST__NULL, ' ', status )

      do i =1, 50
         do j = 1, 50
            image( j,i)=1.0
         end do
      end do

      lbnd_in(1) = 1
      lbnd_in(2) = 1
      ubnd_in(1) = 50
      ubnd_in(2) = 50

      call ast_negate( box1, status )
      call ast_invert( fs, status )
      res = ast_maskr( box1, fs, .false., 2, lbnd_in, ubnd_in,
     :                 image, VAL__BADR, status )

      if( res .ne. 522 ) then
         write(*,*) 'Res is ',res
         call stopit( status, 'res should be 522' )
      end if

      if( image(34,42) .ne. VAL__BADR ) then
         write(*,*) 'image(34,42) = ',image(34,42)
         call stopit( status, 'Above value should be VAL__BADR' )
      end if

      if( image(33,42) .ne. 1.0 ) then
         write(*,*) 'image(33,42) = ',image(33,42)
         call stopit( status, 'Above value should be 1.0' )
      end if

      if( image(16,14) .ne. VAL__BADR ) then
         write(*,*) 'image(16,14) = ',image(16,14)
         call stopit( status, 'Above value should be VAL__BADR' )
      end if

      if( image(15,13) .ne. 1.0 ) then
         write(*,*) 'image(15,13) = ',image(15,13)
         call stopit( status, 'Above value should be 1.0' )
      end if


*  Changing the number of axes in the Region

      frm1 = ast_frame( 2, 'Domain=A', status )

      p1(1) = 0.0
      p1(2) = 0.0
      p2(1) = 0.01
      unc = ast_circle( frm1, 1, p1, p2, AST__NULL, ' ', status )

      p1(1) = -1.0
      p1(2) = 1.0
      p2(1) = -2.0
      p2(2) = 1.5
      box1 = ast_box( frm1, 0, p1, p2, unc, ' ', status )


      outperm(1) = 2
      outperm(2) = -1
      outperm(3) = 1

      inperm(1) = 3
      inperm(2) = 1

      pm = ast_permmap( 2, inperm, 3, outperm, 0.0D0, ' ', status )

      frm2 = ast_frame( 3, 'Domain=B', status )
      reg = ast_mapregion( box1, pm, frm2, status )

      if( .not. ast_isabox( reg, status ) ) call stopit( status,
     :                                            'Box: perm check 1' )
      if( hasFrameSet( reg, status ) ) call stopit( status,
     :                                            'Box: perm check 2' )
      if( ast_geti( reg, 'naxes', status ) .ne. 3 ) call stopit( status,
     :                                            'Box: perm check 3' )

      in( 1, 1 ) = 0.0   ! Outside
      in( 1, 2 ) = 0.0
      in( 1, 3 ) = 0.0
      in( 2, 1 ) = 0.7   ! Inside
      in( 2, 2 ) = 0.0
      in( 2, 3 ) = -0.5
      in( 3, 1 ) = 2.0   ! Outside
      in( 3, 2 ) = 0.0
      in( 3, 3 ) = -1.0
      in( 4, 1 ) = 1.5   ! Boundary
      in( 4, 2 ) = 0.0
      in( 4, 3 ) = 0.0

      call ast_trann( reg, 4, 3, 4, in, .true., 3, 4, out, status )

      if( out( 1, 1 ) .ne. AST__BAD ) call stopit( status, 'box: pc 1' )
      if( out( 1, 2 ) .ne. AST__BAD ) call stopit( status, 'box: pc 2' )
      if( out( 1, 3 ) .ne. AST__BAD ) call stopit( status, 'box: pc 3' )

      if( out( 2, 1 ) .ne. in( 2,1 )) call stopit( status, 'box: pc 4' )
      if( out( 2, 2 ) .ne. in( 2,2 )) call stopit( status, 'box: pc 5' )
      if( out( 2, 3 ) .ne. in( 2,3 )) call stopit( status, 'box: pc 6' )

      if( out( 3, 1 ) .ne. AST__BAD ) call stopit( status, 'box: pc 7' )
      if( out( 3, 2 ) .ne. AST__BAD ) call stopit( status, 'box: pc 8' )
      if( out( 3, 3 ) .ne. AST__BAD ) call stopit( status, 'box: pc 9' )

      if( out( 4, 1 ) .ne. in( 4,1 )) call stopit( status, 'box: pc 10')
      if( out( 4, 2 ) .ne. in( 4,2 )) call stopit( status, 'box: pc 11')
      if( out( 4, 3 ) .ne. in( 4,3 )) call stopit( status, 'box: pc 12')


      outperm(1) = 2
      outperm(2) = -1
      outperm(3) = 1

      inperm(1) = 3
      inperm(2) = 1

      pm = ast_permmap( 2, inperm, 3, outperm, 1.0D0, ' ', status )

      frm2 = ast_frame( 3, 'Domain=B', status )
      reg = ast_mapregion( box1, pm, frm2, status )

      if( .not. ast_isabox( reg, status ) ) call stopit( status,
     :                                            'Box: perm check 4' )
      if( hasFrameSet( reg, status ) ) call stopit( status,
     :                                            'Box: perm check 5' )
      if( ast_geti( reg, 'naxes', status ) .ne. 3 ) call stopit( status,
     :                                            'Box: perm check 6' )

      in( 1, 1 ) = 0.0   ! Outside
      in( 1, 2 ) = 0.0
      in( 1, 3 ) = 0.0
      in( 2, 1 ) = 0.7   ! boundary
      in( 2, 2 ) = 1.0
      in( 2, 3 ) = -0.5
      in( 3, 1 ) = 0.7   ! outside
      in( 3, 2 ) = 1.1
      in( 3, 3 ) = -0.5
      in( 4, 1 ) = 0.7   ! outside
      in( 4, 2 ) = 0.9
      in( 4, 3 ) = -0.5

      call ast_trann( reg, 4, 3, 4, in, .true., 3, 4, out, status )

      if( out( 1, 1 ) .ne. AST__BAD ) call stopit( status, 'box: pc 11')
      if( out( 1, 2 ) .ne. AST__BAD ) call stopit( status, 'box: pc 12')
      if( out( 1, 3 ) .ne. AST__BAD ) call stopit( status, 'box: pc 13')

      if( out( 2, 1 ) .ne. in( 2,1 )) call stopit( status, 'box: pc 14')
      if( out( 2, 2 ) .ne. in( 2,2 )) call stopit( status, 'box: pc 15')
      if( out( 2, 3 ) .ne. in( 2,3 )) call stopit( status, 'box: pc 16')

      if( out( 3, 1 ) .ne. AST__BAD ) call stopit( status, 'box: pc 17')
      if( out( 3, 2 ) .ne. AST__BAD ) call stopit( status, 'box: pc 18')
      if( out( 3, 3 ) .ne. AST__BAD ) call stopit( status, 'box: pc 19')

      if( out( 4, 1 ) .ne. AST__BAD ) call stopit( status, 'box: pc 17')
      if( out( 4, 2 ) .ne. AST__BAD ) call stopit( status, 'box: pc 18')
      if( out( 4, 3 ) .ne. AST__BAD ) call stopit( status, 'box: pc 19')



      outperm(1) = 1

      inperm(1) = 1
      inperm(2) = -1

      pm = ast_permmap( 2, inperm, 1, outperm, 1.4D0, ' ', status )

      frm2 = ast_frame( 1, 'Domain=B', status )
      reg = ast_mapregion( box1, pm, frm2, status )

      if( .not. ast_isabox( reg, status ) ) call stopit( status,
     :                                            'Box: perm check 7' )
      if( hasFrameSet( reg, status ) ) call stopit( status,
     :                                            'Box: perm check 8' )
      if( ast_geti( reg, 'naxes', status ) .ne. 1 ) call stopit( status,
     :                                            'Box: perm check 9' )

      xin( 1 ) = -2.5   ! Outside
      xin( 2 ) = -1.9   ! Inside
      xin( 3 ) = 0.0    ! boundary
      xin( 4 ) = 0.5    ! outside

      call ast_tran1( reg, 4, xin, .true., xout, status )

      if( xout( 1 ) .ne. AST__BAD ) call stopit( status, 'box: pc 20')
      if( xout( 2 ) .ne. xin(2) ) call stopit( status, 'box: pc 21')
      if( xout( 3 ) .ne. xin(3) ) call stopit( status, 'box: pc 22')
      if( xout( 4 ) .ne. AST__BAD ) call stopit( status, 'box: pc 23')



      outperm(1) = 1

      inperm(1) = 1
      inperm(2) = -1

      pm = ast_permmap( 2, inperm, 1, outperm, 1.6D0, ' ', status )
      frm2 = ast_frame( 1, 'Domain=B', status )
      reg = ast_mapregion( box1, pm, frm2, status )
      if( .not. ast_isanullregion( reg, status ) ) call stopit( status,
     :                                            'Box: perm check 10' )


      if( hasFrameSet( reg, status ) ) call stopit( status,
     :                                            'Box: perm check 11' )
      if( ast_geti( reg, 'naxes', status ) .ne. 1 ) call stopit( status,
     :                                            'Box: perm check 12' )

      xin( 1 ) = -2.5
      xin( 2 ) = -1.9
      xin( 3 ) = 0.0
      xin( 4 ) = 0.5

      call ast_tran1( reg, 4, xin, .true., xout, status )

      if( xout( 1 ) .ne. AST__BAD ) call stopit( status, 'box: pc 24')
      if( xout( 2 ) .ne. AST__BAD ) call stopit( status, 'box: pc 25')
      if( xout( 3 ) .ne. AST__BAD ) call stopit( status, 'box: pc 26')
      if( xout( 4 ) .ne. AST__BAD ) call stopit( status, 'box: pc 27')


      frm1 = ast_frame( 3, 'Domain=A', status )

      p1(1) = 0.5
      p1(2) = -1.0
      p1(3) = -2.0
      p2(1) = 30.0
      p2(2) = 5.0
      p2(3) = 0.0

      box1 = ast_box( frm1, 1, p1, p2, AST__NULL, ' ', status )

      outperm(1) = 1
      outperm(2) = 3

      inperm(1) = 1
      inperm(2) = -1
      inperm(3) = 2

      pm = ast_permmap( 3, inperm, 2, outperm, 1.0D0, ' ', status )

      frm2 = ast_frame( 2, 'Domain=B', status )
      reg = ast_mapregion( box1, pm, frm2, status )

      if( .not. ast_isabox( reg, status ) ) call stopit( status,
     :                                            'Box: perm check 13' )

      if( hasFrameSet( reg, status ) ) call stopit( status,
     :                                            'Box: perm check 14' )
      if( ast_geti( reg, 'naxes', status ) .ne. 2 ) call stopit( status,
     :                                            'Box: perm check 15' )

      xin( 1 ) = 0.4    ! Out
      yin( 1 ) = -1.0
      xin( 2 ) = 1.0    ! Out
      yin( 2 ) = 0.1
      xin( 3 ) = 1.0    ! Out
      yin( 3 ) = -2.1
      xin( 4 ) = 0.5    ! Boundary
      yin( 4 ) = -1.0
      xin( 5 ) = 10.0   ! In
      yin( 5 ) = -0.1
      xin( 6 ) =  0.55  ! Boundary
      yin( 6 ) = -2.0


      call ast_tran2( reg, 6, xin, yin, .true., xout, yout, status )

      if( xout( 1 ) .ne. AST__BAD ) call stopit( status, 'Box: pc 37')
      if( yout( 1 ) .ne. AST__BAD ) call stopit( status, 'Box: pc 38')
      if( xout( 2 ) .ne. AST__BAD ) call stopit( status, 'Box: pc 39')
      if( yout( 2 ) .ne. AST__BAD ) call stopit( status, 'Box: pc 40')
      if( xout( 3 ) .ne. AST__BAD ) call stopit( status, 'Box: pc 41')
      if( yout( 3 ) .ne. AST__BAD ) call stopit( status, 'Box: pc 42')
      if( xout( 4 ) .ne. xin( 4 ) ) call stopit( status, 'Box: pc 43')
      if( yout( 4 ) .ne. yin( 4 ) ) call stopit( status, 'Box: pc 44')
      if( xout( 5 ) .ne. xin( 5 ) ) call stopit( status, 'Box: pc 45')
      if( yout( 5 ) .ne. yin( 5 ) ) call stopit( status, 'Box: pc 46')
      if( xout( 6 ) .ne. xin( 6 ) ) call stopit( status, 'Box: pc 47')
      if( yout( 6 ) .ne. yin( 6 ) ) call stopit( status, 'Box: pc 48')

      cards(1) = 'CTYPE1  = ''RA---TAN'''
      cards(2) = 'CTYPE2  = ''DEC--TAN'''
      cards(3) = 'CRPIX1  = 20'
      cards(4) = 'CRPIX2  = 20'
      cards(5) = 'CRVAL1  = 0.0'
      cards(6) = 'CRVAL2  = 0.0'
      cards(7) = 'CROTA1  = 30.0'
      cards(8) = 'CDELT1  = -0.00001'
      cards(9) = 'CDELT2  = 0.00001'

      fc = ast_fitschan( ast_null, ast_null, ' ', status )
      do i = 1, 9
         call ast_putfits( fc, cards(i), .false., status )
      end do
      call ast_clear( fc, 'card', status )
      fs = ast_read( fc, status )

      bfrm = ast_getFrame( fs, AST__BASE, status )

      p1(1) = 0.0
      p1(2) = 0.0
      p2(1) = 0.1
      unc = ast_circle( bfrm, 1, p1, p2, AST__NULL, ' ', status )

      p1( 1 ) = 100.0 ! Pix_X at centre
      p1( 2 ) = 150.0 ! Pix_Y at centre
      p2( 1 ) = 150.0 ! Pix_X at corner
      p2( 2 ) = 170.0 ! Pix_Y at corner

      box1 = ast_box( bfrm, 0, p1, p2, AST__NULL, ' ', status )


      call ast_getregionmesh( box1, .false., 250, 2, npoint, grid,
     :                        status )

      if( npoint .ne. 176 ) then
         write(*,*) npoint
         call stopit( status, 'Box: Error mesh 3' )
      endif

      do i = 1, npoint
         if( abs( grid(i,1) - 100 ) .gt. 50.0D0 ) then
             call stopit( status, 'Box: Error mesh 1' )
         else if( abs( grid(i,2) - 150 ) .gt. 20.0D0 ) then
             call stopit( status, 'Box: Error mesh 2' )
         endif
      enddo

      call ast_getregionmesh( box1, .true., 250, 2, npoint, grid,
     :                        status )
      if( npoint .ne. 198 )
     :    call stopit( status, 'Box: Error mesh 4' )

      do i = 1, npoint
         if( grid(i,1) .ne. 50.0D0 .and. grid(i,1) .ne. 150.0D0 .and.
     :       grid(i,2) .ne. 130.0D0 .and. grid(i,2) .ne. 170.0D0 ) then
             call stopit( status, 'Box: Error mesh 5' )
         endif
      enddo

      cfrm = ast_getFrame( fs, AST__CURRENT, status )
      map = ast_getmapping( fs, AST__BASE, AST__CURRENT, status )
      reg1 = ast_mapregion( box1, map, cfrm, status )

      if( hasFrameSet( reg1, status ) ) call stopit( status,
     :                                            'Box: poly simp 1' )
      if( .not. ast_isapolygon( reg1, status) ) call stopit( status,
     :                                            'Box: poly simp 2' )

      call ast_end( status )
      if( status .ne. sai__ok ) write(*,*) 'Box tests failed'

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



      logical function hasframeset( reg, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer reg, status, ch,nw

      logical fsfound, done
      common /sink1com/ fsfound, done
      external sink1

      hasframeset = .false.
      if( status .ne. sai__ok ) return


      fsfound = .false.
      done = .false.
      ch = ast_channel( AST_NULL, sink1, ' ', STATUS )
      nw = ast_write( ch, reg, status )
      call ast_annul( ch, status )

      hasframeset = fsfound

      end

      subroutine sink1( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      logical fsfound, done
      common /sink1com/ fsfound, done

      integer status, l
      character line*200

      if( status .ne. sai__ok ) return
      call ast_getline( line, l, status )

      if( index( line( : l ),'Unc =' ) .GT. 0 ) then
         done = .true.

      else if( .not. done .and.
     :         index( line( : l ),'FrameSet' ) .GT. 0 ) then
         fsfound= .true.
      end if

      end



      subroutine checkPointList( status )
      implicit none
      include 'AST_PAR'
      include 'SAE_PAR'

      character fwd(1)*30,inv(1)*30
      integer status, frm, reg, reg2, reg3, reg4, mm, map
      integer mdata(-1:15),lbnd,ubnd,nbad,unc
      double precision pnts( 3 ), xin(3),xout(3), acc, ina, inb, outa,
     :                 outb
      data mdata /17*0/

      if( status .ne.sai__ok ) return

      call ast_begin( status )
      frm = ast_specframe( ' ', status )

      pnts(1)=0.0
      pnts(2)=1.0E-5
      unc = ast_box( frm, 0, pnts(1), pnts(2), AST__NULL, ' ', status )

      pnts(1)=1.0
      pnts(2)=1.1
      reg = ast_pointlist( frm, 2, 1, 3, pnts, unc, ' ', status )
      call checkdump( reg, 'checkdump reg', status )

      if( ast_overlap( reg, reg, status ) .ne. 5 ) then
         call stopit( status,
     :                'PointList: self is not identical with self' )
      end if

      reg2 = ast_copy( reg, status )
      call ast_negate( reg2, status )
      call checkdump( reg2, 'checkdump reg2', status )

      if( ast_overlap( reg, reg2, status ) .ne. 6 ) then
         call stopit( status,
     :                'PointList: overlap with self-exclusion' )
      end if


      xin( 1 ) = 1.0
      xin( 2 ) = 1.05
      xin( 3 ) = 1.1
      call ast_tran1( reg, 3, xin, .true., xout, status )

      if( xout( 1 ) .ne. 1.0 ) then
         call stopit( status, 'PointList: Error 1' )
      else if( xout( 2 ) .ne. AST__BAD ) then
         call stopit( status, 'PointList: Error 2' )
      else if( xout( 3 ) .ne. 1.1 ) then
         call stopit( status, 'PointList: Error 3' )
      end if


      call ast_tran1( reg2, 3, xin, .true., xout, status )
      if( xout( 1 ) .ne. AST__BAD ) then
         call stopit( status, 'PointList: Error 4' )
      else if( xout( 2 ) .ne. 1.05 ) then
         call stopit( status, 'PointList: Error 5' )
      else if( xout( 3 ) .ne. AST__BAD ) then
         call stopit( status, 'PointList: Error 6' )
      end if

      fwd(1) = 'y=x**2'
      inv(1) = 'x=y**0.5'
      mm = ast_mathmap( 1, 1, 1, fwd, 1, inv, ' ', status )
      reg3 = ast_mapregion( reg, mm, ast_frame(1,' ', status ), status )
      reg4 = ast_simplify( reg3, status )
      call checkdump( reg4, 'checkdump reg4', status )

      xin( 1 ) = 1.21
      xin( 2 ) = 1.5
      call ast_tran1( reg4, 2, xin, .true., xout, status )
      if( xout( 1 ) .ne. 1.21 ) then
         write(*,*) xout(1), ' (should be 1.21)'
         call stopit( status, 'PointList: Error 7' )
      else if( xout( 2 ) .ne. AST__BAD ) then
         write(*,*) xout(2), ' (should be bad)'
         call stopit( status, 'PointList: Error 8' )
      end if


      lbnd = -1
      ubnd = 15

      ina = 1.01
      inb = 1.11
      outa = 2.0
      outb = 7.0
      map = ast_winmap( 1, ina, inb, outa, outb, ' ', status )

      nbad = ast_maski( reg, map, .true., 1, lbnd, ubnd, mdata, 2,
     :                  status )

      if( nbad .ne. 2 ) then
         write(*,*) 'nbad = ',nbad
         call stopit( status, 'Above value should be 2' )
      end if

      if( mdata(1) .ne. 0 ) then
         write(*,*) 'mdata(1) = ',mdata(1)
         call stopit( status, 'Above value should be 0' )
      end if

      if( mdata(2) .ne. 2 ) then
         write(*,*) 'mdata(2) = ',mdata(2)
         call stopit( status, 'Above value should be 2' )
      end if

      if( mdata(3) .ne. 0 ) then
         write(*,*) 'mdata(3) = ',mdata(3)
         call stopit( status, 'Above value should be 0' )
      end if

      if( mdata(6) .ne. 0 ) then
         write(*,*) 'mdata(6) = ',mdata(6)
         call stopit( status, 'Above value should be 0' )
      end if

      if( mdata(7) .ne. 2 ) then
         write(*,*) 'mdata(7) = ',mdata(7)
         call stopit( status, 'Above value should be 2' )
      end if

      if( mdata(8) .ne. 0 ) then
         write(*,*) 'mdata(8) = ',mdata(8)
         call stopit( status, 'Above value should be 0' )
      end if




      call ast_end( status )
      if( status .ne. sai__ok ) write(*,*) 'PointList tests failed'

      end








      subroutine checkCircle( status )
      implicit none
      include 'AST_PAR'
      include 'SAE_PAR'

      integer status, cir1, cir2, fc, i, fs, frm1,unc,f1,f2,f3,
     :        npoint, j
      double precision p1(4),p2(4),xin(2),yin(2),xout(2),yout(2),
     :                 p3(3),rad,zin(2),zout(2),pp1(3),pp2(3),
     :                 lbnd(2),ubnd(2), mesh(250,3)
      character cards(8)*80, sys*40
      logical hasframeset

      double precision in( 2, 3 ), out( 2, 3 )


      data cards /'CTYPE1  = ''RA---TAN''',
     :            'CTYPE2  = ''DEC--TAN''',
     :            'CRPIX1  = 100',
     :            'CRPIX2  = 100',
     :            'CRVAL1  = 70.0',
     :            'CRVAL2  = 80.0',
     :            'CDELT1  = 0.6',
     :            'CDELT2  = 0.6' /


      if( status .ne.sai__ok ) return
      call ast_begin( status )

* Test 2D circles.

      fc = ast_fitschan( ast_null, ast_null, ' ', status )
      do i = 1, 8
         call ast_putfits( fc, cards(i), .false., status )
      end do
      call ast_clear( fc, 'card', status )
      fs = ast_read( fc, status )

      frm1 = ast_getframe( fs, ast__current, status )

      p1( 1 ) = 0.0
      p1( 2 ) = 1.0
      p2( 1 ) = 0.01

      cir1 = ast_circle( frm1, 1, p1, p2, AST__NULL, ' ', status )
      call ast_getregionbounds( cir1, lbnd, ubnd, status )

      if( abs(lbnd(1)-(-0.01850666061475259)) .gt. 1.0E-6 )
     :    call stopit( status, 'Circle: Error AA1' )
      if( abs(lbnd(2)-(0.9900000002235173)) .gt. 1.0E-6 )
     :    call stopit( status, 'Circle: Error AA2' )
      if( abs(ubnd(1)-(0.01850666061475276)) .gt. 1.0E-6 )
     :    call stopit( status, 'Circle: Error AA3' )
      if( abs(ubnd(2)-(1.009994987166073)) .gt. 1.0E-6 )
     :    call stopit( status, 'Circle: Error AA4' )

      p1( 1 ) = 0.0
      p1( 2 ) = 1.57
      p2( 1 ) = 0.01

      cir1 = ast_circle( frm1, 1, p1, p2, AST__NULL, ' ', status )
      call ast_getregionbounds( cir1, lbnd, ubnd, status )

      if( abs(lbnd(1)-(0.0)) .gt. 1.0E-6 )
     :    call stopit( status, 'Circle: Error AA5' )
      if( abs(lbnd(2)-(1.560000052675599)) .gt. 1.0E-6 )
     :    call stopit( status, 'Circle: Error AA6' )
      if( abs(ubnd(1)-(6.283185307179586)) .gt. 1.0E-6 )
     :    call stopit( status, 'Circle: Error AA7' )
      if( abs(ubnd(2)-(1.5707963267948966)) .gt. 1.0E-6 )
     :    call stopit( status, 'Circle: Error AA8' )

      call ast_getregionmesh( cir1, .true., 0, 0, npoint, 0, status )
      if( npoint .ne. 200 )
     :    call stopit( status, 'Circle: Error mesh 1' )

      call ast_getregionmesh( cir1, .true., 250, 3, npoint, mesh,
     :                        status )

      do i = 1, npoint
         p2(1) = mesh(i,1)
         p2(2) = mesh(i,2)
         if( abs( ast_distance( frm1, p1, p2, status ) - 0.01 ) .gt.
     :       1.0E-6 ) call stopit( status, 'Circle: Error mesh 2' )
      enddo

      call ast_getregionmesh( cir1, .false., 250, 3, npoint, mesh,
     :                        status )

      if( npoint .ne. 201 ) then
         write(*,*) npoint
         call stopit( status, 'Circle: Error mesh 3' )
      endif

      do i = 1, npoint
         p2(1) = mesh(i,1)
         p2(2) = mesh(i,2)
         if( ast_distance( frm1, p1, p2, status ) .gt. 0.01 )
     :       call stopit( status, 'Circle: Error mesh 4' )
      enddo

      p1( 1 ) = 1.2217305
      p1( 2 ) = 1.3962634
      p2( 1 ) = 0.8
      p2( 2 ) = 0.8

      cir1 = ast_circle( frm1, 0, p1, p2, AST__NULL, ' ', status )
      call checkdump( cir1, 'checkdump cir1', status )

      rad = ast_distance( cir1, p1, p2, status )

      call ast_offset( frm1, p1, p2, rad*0.999, p3, status )
      xin(1) = p3(1)
      yin(1) = p3(2)
      call ast_offset( frm1, p1, p2, rad*1.001, p3, status )
      xin(2) = p3(1)
      yin(2) = p3(2)

      call ast_tran2( cir1, 2, xin, yin, .true., xout, yout, status )
      if( xout(1) .ne. xin(1) ) call stopit( status, 'Circle: Error 1' )
      if( yout(1) .ne. yin(1) ) call stopit( status, 'Circle: Error 2' )
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Circle: Error 3' )
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Circle: Error 4' )


      xin(1) = 0.0
      yin(1) = 1.5707963
      call ast_tran2( cir1, 1, xin, yin, .true., xout, yout, status )
      if( xout(1) .ne. xin(1) ) call stopit( status, 'Circle: Error 1b')
      if( yout(1) .ne. yin(1) ) call stopit( status, 'Circle: Error 2b')

      p2(1)=0.0
      p2(2)=0.0
      call ast_offset( frm1, p1, p2, rad*0.999, p3, status )
      xin(1) = p3(1)
      yin(1) = p3(2)
      call ast_offset( frm1, p1, p2, rad*1.001, p3, status )
      xin(2) = p3(1)
      yin(2) = p3(2)

      call ast_tran2( cir1, 2, xin, yin, .true., xout, yout, status )
      if( xout(1) .ne. xin(1) ) call stopit( status, 'Circle: Error 5' )
      if( yout(1) .ne. yin(1) ) call stopit( status, 'Circle: Error 6' )
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                          'Circle: Error 7' )
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                          'Circle: Error 8' )


      call ast_setc( cir1, 'system', 'galactic', status )
      cir1 = ast_simplify( cir1, status )
      if( .not. hasframeset( cir1,status ) ) call stopit( status,
     :                  'Circle: error 9' )


      pp1( 1 ) = 0.0
      pp1( 2 ) = 0.0
      pp2( 1 ) = 1.0D-6
      pp2( 2 ) = 1.0D-6
      unc = ast_box( frm1, 0, pp1, pp2, AST__NULL, ' ', status )

      p1( 1 ) = 1.2217305
      p1( 2 ) = 1.3962634
      p2( 1 ) = 1.2218
      p2( 2 ) = 1.3963
      cir1 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )

      rad = ast_distance( cir1, p1, p2, status )

      call ast_offset( frm1, p1, p2, rad*0.999, p3, status )
      xin(1) = p3(1)
      yin(1) = p3(2)
      call ast_offset( frm1, p1, p2, rad*1.001, p3, status )
      xin(2) = p3(1)
      yin(2) = p3(2)

      call ast_tran2( cir1, 2, xin, yin, .true., xout, yout, status )
      if( xout(1) .ne. xin(1) ) call stopit( status, 'Circle: Error 1b')
      if( yout(1) .ne. yin(1) ) call stopit( status, 'Circle: Error 2b')
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Circle: Error 3b' )
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Circle: Error 4b' )

      p2(1)=0.0
      p2(2)=0.0
      call ast_offset( frm1, p1, p2, rad*0.999, p3, status )
      xin(1) = p3(1)
      yin(1) = p3(2)
      call ast_offset( frm1, p1, p2, rad*1.001, p3, status )
      xin(2) = p3(1)
      yin(2) = p3(2)

      call ast_tran2( cir1, 2, xin, yin, .true., xout, yout, status )
      if( xout(1) .ne. xin(1) ) call stopit( status, 'Circle: Error 5b')
      if( yout(1) .ne. yin(1) ) call stopit( status, 'Circle: Error 6b')
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                          'Circle: Error 7b' )
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                          'Circle: Error 8b' )

      cir2 = ast_copy( cir1, status )
      call ast_setc( cir2, 'system', 'galactic', status )
      call checkdump( cir2, 'checkdump cir2', status )

      cir2 = ast_simplify( cir2, status )

      if( hasframeset( cir2,status ) ) call stopit( status,
     :                  'Circle: error 9b' )

      if( ast_overlap( cir1, cir2, status ) .ne. 5 ) call stopit(status,
     :                                          'Circle: Error 10' )
      if( ast_overlap( cir2, cir1, status ) .ne. 5 ) call stopit(status,
     :                                          'Circle: Error 11' )

      p1( 1 ) = 1.2217305
      p1( 2 ) = 1.3964
      p2( 1 ) = 1.2218
      p2( 2 ) = 1.3963
      cir2 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )
      if( ast_overlap( cir1, cir2, status ) .ne. 4 ) call stopit(status,
     :                                          'Circle: Error 12' )
      if( ast_overlap( cir2, cir1, status ) .ne. 4 ) call stopit(status,
     :                                          'Circle: Error 13' )

      p1( 1 ) = 1.2217305
      p1( 2 ) = 1.3962634
      p2( 1 ) = 1.221731
      p2( 2 ) = 1.396268
      cir2 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )
      if( ast_overlap( cir1, cir2, status ) .ne. 3 ) call stopit(status,
     :                                          'Circle: Error 14' )
      if( ast_overlap( cir2, cir1, status ) .ne. 2 ) call stopit(status,
     :                                          'Circle: Error 15' )

      p1( 1 ) = 0.8
      p1( 2 ) = 1.0
      p2( 1 ) = 0.88
      p2( 2 ) = 1.05
      cir2 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )
      if( ast_overlap( cir1, cir2, status ) .ne. 1 ) call stopit(status,
     :                                          'Circle: Error 16' )


      p1( 1 ) = 0.8
      p1( 2 ) = 1.5707963
      p2( 1 ) = 0.1
      cir2 = ast_circle( frm1, 1, p1, p2, unc, ' ', status )
      call ast_getregionbounds( cir2, lbnd, ubnd, status )
      if( lbnd(1) .ne. 0.0D0 ) call stopit( status,
     :                                      'Circle: Error 16a'  )
      if( abs( lbnd(2) - 1.47079625 ) .gt. 1.0E-6 ) call stopit( status,
     :                                      'Circle: Error 16b'  )
      if( abs( ubnd(1) - 6.28318531 ) .gt. 1.0E-6 ) call stopit( status,
     :                                      'Circle: Error 16c'  )
      if( abs( ubnd(2) - 1.57079633 ) .gt. 1.0E-6 ) call stopit( status,
     :                                      'Circle: Error 16d'  )


      frm1 = ast_frame(2,"domain=aa",status)

      pp1( 1 ) = 0.0
      pp1( 2 ) = 0.0
      pp2( 1 ) = 1.0D-6
      pp2( 2 ) = 1.0D-6
      unc = ast_box( frm1, 0, pp1, pp2, AST__NULL, ' ', status )

      p1( 1 ) = 1.2217305
      p1( 2 ) = 1.3962634
      p2( 1 ) = 1.2218
      p2( 2 ) = 1.3963
      cir2 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )

      if( ast_overlap( cir1, cir2, status ) .ne. 0 ) call stopit(status,
     :                                          'Circle: Error 17' )
      if( ast_overlap( cir2, cir1, status ) .ne. 0 ) call stopit(status,
     :                                          'Circle: Error 18' )


      f1 = ast_skyframe( ' ', status )
      f2 = ast_frame( 2, ' ', status )
      f3 = ast_cmpframe( f1, f2, ' ', status )

      p1( 1 ) = 1.0
      p1( 2 ) = 1.0
      p1( 3 ) = 3.0
      p1( 4 ) = 3.0
      p2( 1 ) = 1.01
      p2( 2 ) = 1.02
      p2( 3 ) = 3.01
      p2( 4 ) = 3.01
      cir2 = ast_circle( f3, 0, p1, p2, AST__NULL, ' ', status )
      if( ast_overlap( cir2, cir2, status ) .ne. 5 ) call stopit(status,
     :                                          'Circle: Error 18b' )

* Test 3D spheres

      frm1 = ast_frame( 3, ' ', status )

      pp1( 1 ) = 0.0
      pp1( 2 ) = 0.0
      pp1( 3 ) = 0.0
      pp2( 1 ) = 1.0E-6
      pp2( 2 ) = 2.0E-6
      pp2( 3 ) = 2.0E-6
      unc = ast_box( frm1, 0, pp1, pp2, AST__NULL, ' ', status )

      p1( 1 ) = 1.0
      p1( 2 ) = 2.0
      p1( 3 ) = 3.0
      p2( 1 ) = 0.0
      p2( 2 ) = -1.0
      p2( 3 ) = -2.0
      cir1 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )
      call checkdump( cir1, 'checkdump sph1', status )

      rad = ast_distance( cir1, p1, p2, status )

      call ast_offset( frm1, p1, p2, rad*0.999, p3, status )
      in(1,1) = p3(1)
      in(1,2) = p3(2)
      in(1,3) = p3(3)
      call ast_offset( frm1, p1, p2, rad*1.001, p3, status )
      in(2,1) = p3(1)
      in(2,2) = p3(2)
      in(2,3) = p3(3)


      call ast_trann( cir1, 2, 3, 2, in, .true., 3, 2, out, status )

      if( out(1,1) .ne. in(1,1) ) call stopit( status,
     :                                         'Sphere: Error 1' )
      if( out(1,2) .ne. in(1,2) ) call stopit( status,
     :                                         'Sphere: Error 2' )
      if( out(1,3) .ne. in(1,3) ) call stopit( status,
     :                                         'Sphere: Error 2z')
      if( out(2,1) .ne. AST__BAD ) call stopit( status,
     :                                         'Sphere: Error 3' )
      if( out(2,2) .ne. AST__BAD ) call stopit( status,
     :                                         'Sphere: Error 4' )
      if( out(2,3) .ne. AST__BAD ) call stopit( status,
     :                                         'Sphere: Error 4z' )

      p2(1)=0.0
      p2(2)=0.0
      p2(3)=0.0
      call ast_offset( frm1, p1, p2, rad*0.999, p3, status )
      in(1,1) = p3(1)
      in(1,2) = p3(2)
      in(1,3) = p3(3)
      call ast_offset( frm1, p1, p2, rad*1.001, p3, status )
      in(2,1) = p3(1)
      in(2,2) = p3(2)
      in(2,3) = p3(3)

      call ast_trann( cir1, 2, 3, 2, in, .true., 3, 2, out, status )
      if( out(1,1) .ne. in(1,1) ) call stopit( status,
     :                                          'Sphere: Error 5' )
      if( out(1,2) .ne. in(1,2) ) call stopit( status,
     :                                          'Sphere: Error 6' )
      if( out(1,3) .ne. in(1,3) ) call stopit( status,
     :                                          'Sphere: Error 6z')
      if( out(2,1) .ne. AST__BAD ) call stopit( status,
     :                                          'Sphere: Error 7' )
      if( out(2,2) .ne. AST__BAD ) call stopit( status,
     :                                          'Sphere: Error 8' )
      if( out(2,3) .ne. AST__BAD ) call stopit( status,
     :                                          'Sphere: Error 8z' )


      if( ast_overlap( cir1, cir1, status ) .ne. 5 ) call stopit(status,
     :                                          'Sphere: Error 10' )




      p1( 1 ) = 1.0
      p1( 2 ) = 2.0
      p1( 3 ) = 3.0
      p2( 1 ) = 0.5
      p2( 2 ) = 0.0
      p2( 3 ) = -1.0
      cir2 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )
      call checkdump( cir2, 'checkdump sph2', status )

      if( ast_overlap( cir2, cir1, status ) .ne. 2 ) call stopit(status,
     :                                          'Sphere: Error 11' )

      if( ast_overlap( cir1, cir2, status ) .ne. 3 ) call stopit(status,
     :                                          'Sphere: Error 12' )



      p1( 1 ) = 1.0
      p1( 2 ) = 0.0
      p1( 3 ) = 3.0
      p2( 1 ) = 0.0
      p2( 2 ) = -1.0
      p2( 3 ) = -2.0
      cir2 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )
      if( ast_overlap( cir1, cir2, status ) .ne. 4 ) call stopit(status,
     :                                          'Sphere: Error 13' )

      p1( 1 ) = 1.0
      p1( 2 ) = 102.0
      p1( 3 ) = 3.0
      p2( 1 ) = 0.0
      p2( 2 ) = 99.0
      p2( 3 ) = -2.0
      cir2 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )
      if( ast_overlap( cir1, cir2, status ) .ne. 1 ) call stopit(status,
     :                                          'Sphere: Error 14' )


      p1( 1 ) = 0.0
      p1( 2 ) = 0.0
      p1( 3 ) = 0.0
      p2( 1 ) = 0.0
      p2( 2 ) = 0.0
      p2( 3 ) = 1.0
      cir1 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )

      p1( 1 ) = 2.0000001
      p1( 2 ) = 0.0
      p1( 3 ) = 0.0
      p2( 1 ) = 2.000001
      p2( 2 ) = 1.0
      p2( 3 ) = 0.0
      cir2 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )

      if( ast_overlap( cir1, cir2, status ) .ne. 4 ) then
         write(*,*) ast_overlap( cir1, cir2, status ),' should be 4 '
         call stopit(status, 'Sphere: Error 15' )
      end if

      p1( 1 ) = 2.000001
      p1( 2 ) = 0.0
      p1( 3 ) = 0.0
      p2( 1 ) = 2.000001
      p2( 2 ) = 1.0
      p2( 3 ) = 0.0
      cir2 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )

      if( ast_overlap( cir1, cir2, status ) .ne. 4 ) call stopit(status,
     :                                          'Sphere: Error 16' )

      cir2 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )

      call ast_setl( cir1, 'Closed', .false., status )
      call ast_setl( cir2, 'Closed', .false., status )
      if( ast_overlap( cir1, cir2, status ) .ne. 1 ) call stopit(status,
     :                                          'Sphere: Error 17' )
      call ast_clear( cir1, 'Closed', status )
      call ast_clear( cir2, 'Closed', status )

      p1( 1 ) = 2.000004
      p1( 2 ) = 0.0
      p1( 3 ) = 0.0
      p2( 1 ) = 2.000004
      p2( 2 ) = 1.0
      p2( 3 ) = 0.0
      cir2 = ast_circle( frm1, 0, p1, p2, unc, ' ', status )

      if( ast_overlap( cir1, cir2, status ) .ne. 1 ) call stopit(status,
     :                                          'Sphere: Error 18' )

      call ast_end( status )
      if( status .ne. sai__ok ) write(*,*) 'Circle tests failed'

      end







      subroutine checkEllipse( status )
      implicit none
      include 'AST_PAR'
      include 'SAE_PAR'

      integer status, ell1, ell2, fc, i, fs, frm1, fs2,mm,ell3,ell4,
     :       reg, unc, f1, f2, f3, f4, f5, map, perm(2)
      double precision p1(2),p2(2),p3(2),p4(2),pp1(2),pp2(2)
      double precision q1(2),q2(2),q3(2),q4(2),lbnd(2),ubnd(2)
      double precision q1b(2),q2b(2),q3b(2),q4b(2)
      double precision p1b(2),p2b(2),p3b(2),p4b(2),matrix(4)
      character cards(10)*80
      double precision xin(4),yin(4),xout(4),yout(4),rad
      logical hasframeset

      data cards /'NAXIS1  = 300',
     :            'NAXIS2  = 300',
     :            'CTYPE1  = ''RA---TAN''',
     :            'CTYPE2  = ''DEC--TAN''',
     :            'CRPIX1  = 100',
     :            'CRPIX2  = 100',
     :            'CRVAL1  = 0.0',
     :            'CRVAL2  = 90.0',
     :            'CDELT1  = 0.6',
     :            'CDELT2  = 0.6' /


      if( status .ne.sai__ok ) return
      call ast_begin( status )

      f1 = ast_SkyFrame( 'system=fk4', status )
      f3 = ast_cmpframe( ast_pickaxes( f1, 1, 1, map, status ),
     :                   ast_specframe( 'system=wave,unit=um', status ),
     :                   ' ', status )
      perm(1)=2
      perm(2)=1
      call ast_permaxes( f3, perm, status )
      p1(1) = 0.0
      p1(2) = 0.0
      p2(1) = 0.001
      p2(2) = 0.001
      p3(1) = -0.001
      p3(2) = 0.001
      ell1 = ast_ellipse( f3, 0, p1, p2, p3, AST__NULL, ' ', status )

      xin(1) = 0.0
      yin(1) = 0.00141421
      xin(2) = 0.0
      yin(2) = 0.00141422
      xin(3) = -0.000999
      yin(3) = 0.0009999
      xin(4) = -0.001001
      yin(4) = 0.001001
      call ast_tran2( ell1, 4, xin, yin, .true., xout, yout, status )

      if( xout(1) .ne. xin(1) ) call stopit( status, 'Ellipse: Cmp 1')
      if( yout(1) .ne. yin(1) ) call stopit( status, 'Ellipse: Cmp 2')
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Ellipse: Cmp 3' )
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Ellipse: Cmp 4' )
      if( xout(3) .ne. xin(3) ) call stopit( status, 'Ellipse: Cmp 5')
      if( yout(3) .ne. yin(3) ) call stopit( status, 'Ellipse: Cmp 6')
      if( xout(4) .ne. AST__BAD ) call stopit( status,
     :                                         'Ellipse: Cmp 7' )
      if( yout(4) .ne. AST__BAD ) call stopit( status,
     :                                         'Ellipse: Cmp 8' )

      call checkdump( ell1, 'checkdump ell1 cmp', status )
      ell2 = ast_simplify( ell1, status )
      call checkdump( ell2, 'checkdump ell2 cmp', status )
      if( ast_overlap( ell1, ell2, status ) .ne. 5 ) call stopit(status,
     :                                          'ellipse: Error 5 cmp' )

      fc = ast_fitschan( ast_null, ast_null, ' ', status )
      do i = 1, 10
         call ast_putfits( fc, cards(i), .false., status )
      end do
      call ast_clear( fc, 'card', status )
      fs = ast_read( fc, status )

      frm1 = ast_getframe( fs, ast__current, status )

      p1( 1 ) = 1.2217305
      p1( 2 ) = 1.570796
      p2( 1 ) = 0.9
      p2( 2 ) = 1.470796
      p3( 1 ) = 2.9217305
      p3( 2 ) = 1.370796

      ell1 = ast_ellipse( frm1, 0, p1, p2, p3, AST__NULL, ' ', status )
      call checkdump( ell1, 'checkdump ell1', status )


      call ast_getregionbounds( ell1, lbnd, ubnd, status )
      if( abs( lbnd(1) ) .gt. 1.0E-10 ) call stopit( status,
     :                                               'Error b1' )
      if( abs( lbnd(2) - 1.19059777 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Error b2' )
      if( abs( ubnd(1) - 6.28318531 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Error b3' )
      if( abs( ubnd(2) - 1.57079633 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Error b4' )

      rad = ast_distance( ell1, p1, p2, status )

      call ast_offset( frm1, p1, p2, rad*0.999, p4, status )
      xin(1) = p4(1)
      yin(1) = p4(2)
      call ast_offset( frm1, p1, p2, rad*1.001, p4, status )
      xin(2) = p4(1)
      yin(2) = p4(2)

      call ast_tran2( ell1, 2, xin, yin, .true., xout, yout, status )
      if( xout(1) .ne. xin(1) ) call stopit( status, 'Ellipse: Error 1')
      if( yout(1) .ne. yin(1) ) call stopit( status, 'Ellipse: Error 2')
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Ellipse: Error 3' )
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Ellipse: Error 4' )


      call ast_offset( frm1, p1, p2, -rad*0.999, p4, status )
      xin(1) = p4(1)
      yin(1) = p4(2)
      call ast_offset( frm1, p1, p2, -rad*1.001, p4, status )
      xin(2) = p4(1)
      yin(2) = p4(2)

      call ast_tran2( ell1, 2, xin, yin, .true., xout, yout, status )
      if( xout(1) .ne. xin(1) ) call stopit( status,
     :                                         'Ellipse: Error 1b')
      if( yout(1) .ne. yin(1) ) call stopit( status,
     :                                         'Ellipse: Error 2b')
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Ellipse: Error 3b' )
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Ellipse: Error 4b' )


      rad = ast_distance( ell1, p1, p3, status )

      call ast_offset( frm1, p1, p3, rad*0.999, p4, status )
      xin(1) = p4(1)
      yin(1) = p4(2)
      call ast_offset( frm1, p1, p3, rad*1.001, p4, status )
      xin(2) = p4(1)
      yin(2) = p4(2)

      call ast_tran2( ell1, 2, xin, yin, .true., xout, yout, status )
      if( xout(1) .ne. xin(1) ) call stopit( status,
     :                                         'Ellipse: Error 1c')
      if( yout(1) .ne. yin(1) ) call stopit( status,
     :                                         'Ellipse: Error 2c')
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Ellipse: Error 3c' )
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Ellipse: Error 4c' )

      call ast_offset( frm1, p1, p3, -rad*0.999, p4, status )
      xin(1) = p4(1)
      yin(1) = p4(2)
      call ast_offset( frm1, p1, p3, -rad*1.001, p4, status )
      xin(2) = p4(1)
      yin(2) = p4(2)

      call ast_tran2( ell1, 2, xin, yin, .true., xout, yout, status )
      if( xout(1) .ne. xin(1) ) call stopit( status,
     :                                         'Ellipse: Error 1d')
      if( yout(1) .ne. yin(1) ) call stopit( status,
     :                                         'Ellipse: Error 2d')
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Ellipse: Error 3d' )
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                         'Ellipse: Error 4d' )


      ell2 = ast_copy( ell1, status )

      if( ast_overlap( ell1, ell2, status ) .ne. 5 ) call stopit(status,
     :                                          'ellipse: Error 5' )
      if( ast_overlap( ell2, ell1, status ) .ne. 5 ) call stopit(status,
     :                                          'ellipse: Error 6' )

      call ast_set( ell2, 'system=galactic', status )
      if( ast_overlap( ell1, ell2, status ) .ne. 5 ) call stopit(status,
     :                                          'ellipse: Error 7' )
      if( ast_overlap( ell2, ell1, status ) .ne. 5 ) call stopit(status,
     :                                          'ellipse: Error 8' )




      xin(1) = p1( 1 )
      yin(1) = p1( 2 )
      xin(2) = p2( 1 )
      yin(2) = p2( 2 )
      xin(3) = p3( 1 )
      yin(3) = p3( 2 )
      call ast_tran2( fs, 3, xin, yin, .false., xout, yout, status )
      q1(1) = xout(1)
      q1(2) = yout(1)
      q2(1) = xout(2)
      q2(2) = yout(2)
      q3(1) = xout(3)
      q3(2) = yout(3)

      frm1 = ast_GetFrame( fs, AST__BASE, status )

      rad = ast_distance( frm1, q1, q2, status )

      call ast_offset( frm1, q1, q2, rad*1.95, q1b, status )

      q2b( 1 ) = q2( 1 )  + ( q1b( 1 ) - q1( 1 ) )
      q2b( 2 ) = q2( 2 )  + ( q1b( 2 ) - q1( 2 ) )

      q3b( 1 ) = q3( 1 )  + ( q1b( 1 ) - q1( 1 ) )
      q3b( 2 ) = q3( 2 )  + ( q1b( 2 ) - q1( 2 ) )

      xout(1) = q1b(1)
      yout(1) = q1b(2)
      xout(2) = q2b(1)
      yout(2) = q2b(2)
      xout(3) = q3b(1)
      yout(3) = q3b(2)
      call ast_tran2( fs, 3, xout, yout, .true., xin, yin, status )
      p1b( 1 ) = xin(1)
      p1b( 2 ) = yin(1)
      p2b( 1 ) = xin(2)
      p2b( 2 ) = yin(2)
      p3b( 1 ) = xin(3)
      p3b( 2 ) = yin(3)

      frm1 = ast_GetFrame( fs, AST__CURRENT, status )

      pp1( 1 ) = 0.0
      pp1( 2 ) = 0.0
      pp2( 1 ) = 1.0D-7
      pp2( 2 ) = 1.0D-7
      unc = ast_box( frm1, 0, pp1, pp2, AST__NULL, ' ', status )
      ell2 = ast_ellipse( frm1, 0, p1b, p2b, p3b, unc, ' ', status )
      if( ast_overlap( ell2, ell1, status ) .ne. 4 ) call stopit(status,
     :                                          'ellipse: Error 9' )
      if( ast_overlap( ell1, ell2, status ) .ne. 4 ) call stopit(status,
     :                                          'ellipse: Error 10' )


      call ast_offset( frm1, q1, q2, rad*2.05, q1b, status )

      q2b( 1 ) = q2( 1 )  + ( q1b( 1 ) - q1( 1 ) )
      q2b( 2 ) = q2( 2 )  + ( q1b( 2 ) - q1( 2 ) )

      q3b( 1 ) = q3( 1 )  + ( q1b( 1 ) - q1( 1 ) )
      q3b( 2 ) = q3( 2 )  + ( q1b( 2 ) - q1( 2 ) )

      xout(1) = q1b(1)
      yout(1) = q1b(2)
      xout(2) = q2b(1)
      yout(2) = q2b(2)
      xout(3) = q3b(1)
      yout(3) = q3b(2)
      call ast_tran2( fs, 3, xout, yout, .true., xin, yin, status )
      p1b( 1 ) = xin(1)
      p1b( 2 ) = yin(1)
      p2b( 1 ) = xin(2)
      p2b( 2 ) = yin(2)
      p3b( 1 ) = xin(3)
      p3b( 2 ) = yin(3)

      frm1 = ast_GetFrame( fs, AST__CURRENT, status )
      pp1( 1 ) = 0.0
      pp1( 2 ) = 0.0
      pp2( 1 ) = 1.0D-7
      pp2( 2 ) = 1.0D-7
      unc = ast_box( frm1, 0, pp1, pp2, AST__NULL, ' ', status )
      ell2 = ast_ellipse( frm1, 0, p1b, p2b, p3b, unc, ' ', status )
      if( ast_overlap( ell2, ell1, status ) .ne. 1 ) call stopit(status,
     :                                          'ellipse: Error 11' )
      if( ast_overlap( ell1, ell2, status ) .ne. 1 ) call stopit(status,
     :                                          'ellipse: Error 12' )

      p1b( 1 ) = p1( 1 )
      p1b( 2 ) = p1( 2 )
      p2b( 1 ) = p2( 1 )
      p2b( 2 ) = 0.9*p2( 2 ) + 0.1*p1( 2 )
      p3b( 1 ) = p3( 1 )
      p3b( 2 ) = 0.9*p3( 2 ) + 0.1*p1( 2 )

      ell2 = ast_ellipse( frm1, 0, p1b, p2b, p3b, unc, ' ', status )
      if( ast_overlap( ell2, ell1, status ) .ne. 2 ) call stopit(status,
     :                                          'ellipse: Error 13' )
      if( ast_overlap( ell1, ell2, status ) .ne. 3 ) call stopit(status,
     :                                          'ellipse: Error 14' )






      frm1 = ast_frame( 2, ' ', status )

      pp1( 1 ) = 0.0
      pp1( 2 ) = 0.0
      pp2( 1 ) = 1.0D-7
      pp2( 2 ) = 1.0D-7
      unc = ast_box( frm1, 0, pp1, pp2, AST__NULL, ' ', status )

      p1(1)=0.0
      p1(2)=0.0
      p2(1)=1.0
      p2(2)=0.0
      p3(1)=0.0
      p3(2)=0.5
      ell1 = ast_ellipse( frm1, 0, p1, p2, p3, unc, ' ', status )


      matrix(1) = 1.73
      matrix(2) = 0.5003
      matrix(3) = -1.0006
      matrix(4) = 0.866
      mm = ast_matrixmap( 2, 2, 0, matrix, ' ', status )

      ell2 = ast_mapregion( ell1, mm, frm1, status )
      call checkdump( ell2, 'checkdump ell2', status )
      if( hasframeset( ell2, status ) ) call stopit( status,
     :                  'Ellipse: error 15' )
      call ast_invert( mm, status )
      ell3 = ast_mapregion( ell2, mm, frm1, status )
      if( hasframeset( ell3,status ) ) call stopit( status,
     :                  'Ellipse: error 16' )
      if( ast_overlap( ell1, ell3, status ) .ne. 5 ) call stopit(status,
     :                                          'ellipse: Error 17' )


      frm1 = ast_frame( 2, ' ', status )
      pp1( 1 ) = 0.0
      pp1( 2 ) = 0.0
      pp2( 1 ) = 1.0D-7
      pp2( 2 ) = 1.0D-7
      unc = ast_box( frm1, 0, pp1, pp2, AST__NULL, ' ', status )

      p1(1)=0.0
      p1(2)=0.0
      p2(1)=1.0
      p2(2)=0.0
      p3(1)=0.0
      p3(2)=1.0
      ell1 = ast_ellipse( frm1, 0, p1, p2, p3, unc, ' ', status )
      reg = ast_simplify( ell1, status )
      if( .not. ast_IsACircle( reg, status ) ) call stopit(status,
     :                                          'ellipse: Error 18' )

      ell1 = ast_circle( frm1, 0, p1, p2, AST__NULL, ' ', status )
      if( ast_overlap( reg, ell1, status ) .ne. 5 ) call stopit(status,
     :                                          'Ellipse: Error 19' )



      frm1 = ast_skyframe( ' ', status )
      p1(1)=0.0D0
      p1(2)=0.0D0
      p2(1)=0.01D0
      p2(2)=0.01D0
      p3(1)=0.0D0
      p3(2)=0.0D0
      ell1 = ast_ellipse( frm1, 1, p1, p2, p3, AST__NULL, ' ', status )

      p1(1)=-0.015D0
      p1(2)=0.0D0
      p2(1)=0.01D0
      p2(2)=0.01D0
      p3(1)=0.0D0
      p3(2)=0.0D0
      ell2 = ast_ellipse( frm1, 1, p1, p2, p3, AST__NULL, ' ', status )

      if( ast_overlap( ell1, ell2, status ) .ne. 4 ) call stopit(status,
     :                                          'Ellipse: Error 20' )

      p1(1)=6.2681853D0
      p1(2)=0.0D0
      p2(1)=0.01D0
      p2(2)=0.01D0
      p3(1)=0.0D0
      p3(2)=0.0D0
      ell2 = ast_ellipse( frm1, 1, p1, p2, p3, AST__NULL, ' ', status )

      if( ast_overlap( ell1, ell2, status ) .ne. 4 ) call stopit(status,
     :                                          'Ellipse: Error 21' )

      p1(1)=-0.015D0
      p1(2)=0.0D0
      p2(1)=0.01D0
      p2(2)=0.01D0
      p3(1)=0.0D0
      p3(2)=0.0D0
      ell1 = ast_ellipse( frm1, 1, p1, p2, p3, AST__NULL, ' ', status )

      if( ast_overlap( ell1, ell2, status ) .ne. 5 ) call stopit(status,
     :                                          'Ellipse: Error 22' )




      call ast_end( status )
      if( status .ne. sai__ok ) write(*,*) 'Ellipse tests failed'

      end


      subroutine checkNullRegion( status )
      implicit none
      include 'AST_PAR'
      include 'SAE_PAR'
      include 'PRM_PAR'

      integer status, f1, f2, f3, nr, cir, i, j, lbnd_in(2), ubnd_in(2),
     :        nr2, res
      double precision p1(4),p2(4),rin(5,5)
      logical hasframeset

      if( status .ne.sai__ok ) return

      call ast_begin( status )

      f1 = ast_skyframe( ' ', status )
      f2 = ast_frame( 2, ' ', status )
      f3 = ast_cmpframe( f1, f2, ' ', status )
      nr = ast_NullRegion( f3, AST__NULL, ' ', status )

      call checkdump( nr, 'checkdump NullRegion:nr', status )

      p1( 1 ) = 1.0
      p1( 2 ) = 1.0
      p1( 3 ) = 3.0
      p1( 4 ) = 3.0
      p2( 1 ) = 1.01
      p2( 2 ) = 1.02
      p2( 3 ) = 3.01
      p2( 4 ) = 3.01
      cir = ast_circle( nr, 0, p1, p2, AST__NULL, ' ', status )
      call checkdump( cir, 'checkdump NullRegion:cir', status )

      if( ast_overlap( cir, nr, status ) .ne. 1 ) call stopit(status,
     :                                          'NullRegion: Error 1' )

      if( ast_overlap( nr, cir, status ) .ne. 1 ) call stopit(status,
     :                                          'NullRegion: Error 2' )

      if( ast_overlap( nr, nr, status ) .ne. 5 ) call stopit(status,
     :                                          'NullRegion: Error 3' )

      call ast_negate( nr, status )

      if( ast_overlap( cir, nr, status ) .ne. 2 ) call stopit(status,
     :                                          'NullRegion: Error 4' )

      if( ast_overlap( nr, cir, status ) .ne. 3 ) call stopit(status,
     :                                          'NullRegion: Error 5' )

      if( ast_overlap( nr, nr, status ) .ne. 5 ) call stopit(status,
     :                                          'NullRegion: Error 6' )

      call ast_set( nr, 'system(1)=FK4', status )
      nr2 = ast_simplify( nr, status )
      call ast_set( nr2, 'system(1)=ICRS', status )
      nr = ast_simplify( nr2, status )
      if( hasframeset( nr, status ) ) call stopit( status,
     :                                          'NullRegion: error 7' )

      lbnd_in(1) = 1
      lbnd_in(2) = 1
      ubnd_in(1) = 5
      ubnd_in(2) = 5

      do i =1, 5
         do j = 1, 5
            rin( j,i)=1.0
         end do
      end do

      nr = ast_NullRegion( f2, AST__NULL, 'negated=1', status )
      res = ast_maskd( nr, AST__NULL, .false., 2, lbnd_in, ubnd_in,
     :                 rin, VAL__BADD, status )

      if( res .ne. 0 ) then
         write(*,*) 'NullRegion:Res is ',res
         call stopit( status, 'res should be 0' )
      end if

      do i =1, 5
         do j = 1, 5
            if( rin(j,i) .NE. 1.0 ) then
               write(*,*) 'rin(',j,',',i,') = ',rin(j,i)
               call stopit( status, 'Above value should be 1.0' )
            end if
         end do
      end do

      call ast_negate( nr, status )
      res = ast_maskd( nr, AST__NULL, .false., 2, lbnd_in, ubnd_in,
     :                 rin, VAL__BADD, status )

      if( res .ne. 25 ) then
         write(*,*) 'NullRegion:Res is ',res
         call stopit( status, 'res should be 25' )
      end if

      do i =1, 5
         do j = 1, 5
            if( rin(j,i) .NE. VAL__BADD ) then
               write(*,*) 'rin(',j,',',i,') = ',rin(j,i)
               call stopit( status, 'Above value should be BAD' )
            end if
         end do
      end do





      call ast_end( status )
      if( status .ne. sai__ok ) write(*,*) 'NullRegion tests failed'

      end




      subroutine checkCmpRegion( status )
      implicit none
      include 'AST_PAR'
      include 'SAE_PAR'
      include 'PRM_PAR'

      integer status, r1, r2, r3, cr, f1, f2, cr2, cr3, frm, map, fs
      double precision p1(2),p2(2),xout(4),yout(4),xin(4),yin(4)
      logical hasframeset

      if( status .ne.sai__ok ) return

      call ast_begin( status )


      f1 = ast_skyframe( 'system=fk5', status )
      p1(1) = 0.0
      p1(2) = 0.0
      p2(1) = 1.0E-4
      p2(2) = 1.0E-4
      r1 = ast_box( f1, 0, p1, p2, AST__NULL, ' ', status )

      f2 = ast_skyframe( 'system=galactic', status )

      p1(1) = 1.68166715892457
      p1(2) = -1.050436507472
      p2(1) = 1.68140254777194
      p2(2) = -1.05048840003467
      r2 = ast_circle( f2, 0, p1, p2, AST__NULL, ' ', status )

      if( ast_overlap( r1, r2, status ) .ne. 4 ) call stopit(status,
     :                                        'CmpRegion: Error 0' )

      cr = ast_cmpregion( r1, r2, AST__AND, ' ', status )
      cr = ast_Copy( cr, status )
      if( ast_overlap( cr, cr, status ) .ne. 5 ) call stopit(status,
     :                                        'CmpRegion: Error 1' )

      xin( 1 ) = 0.5E-4! In both r1 and r2
      xin( 2 ) = 1.5E-4! In r2 but not r1
      xin( 3 ) = -0.5E-4! In r1 but not r2
      xin( 4 ) = 1.1E-4! In neither

      yin( 1 ) = 0.5E-4
      yin( 2 ) = 1.5E-4
      yin( 3 ) = -0.5E-4
      yin( 4 ) = -1.1E-4

      call ast_tran2( cr, 4, xin, yin, .true., xout, yout, status )

      if( xout(1) .ne. xin(1) ) call stopit( status,
     :                                    'CmpRegion: AND Error 1x')
      if( yout(1) .ne. yin(1) ) call stopit( status,
     :                                    'CmpRegion: AND Error 1y')
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion: AND Error 2x')
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion: AND Error 2y')
      if( xout(3) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion: AND Error 3x')
      if( yout(3) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion: AND Error 3y')
      if( xout(4) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion: AND Error 4x')
      if( yout(4) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion: AND Error 4y')

      cr = ast_cmpregion( r1, r2, AST__OR, ' ', status )
      call ast_tran2( cr, 4, xin, yin, .true., xout, yout, status )

      if( xout(1) .ne. xin(1) ) call stopit( status,
     :                                     'CmpRegion: OR Error 1x')
      if( yout(1) .ne. yin(1) ) call stopit( status,
     :                                     'CmpRegion: OR Error 1y')
      if( xout(2) .ne. xin(2) ) call stopit( status,
     :                                     'CmpRegion: OR Error 2x')
      if( yout(2) .ne. yin(2) ) call stopit( status,
     :                                     'CmpRegion: OR Error 2y')
      if( xout(3) .ne. xin(3) ) call stopit( status,
     :                                     'CmpRegion: OR Error 3x')
      if( yout(3) .ne. yin(3) ) call stopit( status,
     :                                     'CmpRegion: OR Error 3y')
      if( xout(4) .ne. AST__BAD ) call stopit( status,
     :                                     'CmpRegion: OR Error 4x')
      if( yout(4) .ne. AST__BAD ) call stopit( status,
     :                                     'CmpRegion: OR Error 4y')


      call ast_negate( r2, status )
      cr = ast_cmpregion( r1, r2, AST__AND, ' ', status )
      call ast_tran2( cr, 4, xin, yin, .true., xout, yout, status )

      if( xout(1) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDb Error 1x')
      if( yout(1) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDb Error 1y')
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDb Error 2x')
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDb Error 2y')
      if( xout(3) .ne. xin(3) ) call stopit( status,
     :                                   'CmpRegion: ANDb Error 3x')
      if( yout(3) .ne. yin(3) ) call stopit( status,
     :                                   'CmpRegion: ANDb Error 3y')
      if( xout(4) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDb Error 4x')
      if( yout(4) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDb Error 4y')

      call ast_negate( r1, status )
      cr = ast_cmpregion( r1, r2, AST__AND, ' ', status )
      call ast_tran2( cr, 4, xin, yin, .true., xout, yout, status )

      if( xout(1) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDc Error 1x')
      if( yout(1) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDc Error 1y')
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDc Error 2x')
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDc Error 2y')
      if( xout(3) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDc Error 3x')
      if( yout(3) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDc Error 3y')
      if( xout(4) .ne. xin(4) ) call stopit( status,
     :                                   'CmpRegion: ANDc Error 4x')
      if( yout(4) .ne. yin(4) ) call stopit( status,
     :                                   'CmpRegion: ANDc Error 4y')


      cr = ast_cmpregion( r1, r2, AST__AND, ' ', status )
      call ast_negate( cr, status )
      call ast_tran2( cr, 4, xin, yin, .true., xout, yout, status )

      if( xout(1) .ne. xin(1) ) call stopit( status,
     :                                   'CmpRegion: ANDd Error 1x')
      if( yout(1) .ne. yin(1) ) call stopit( status,
     :                                   'CmpRegion: ANDd Error 1y')
      if( xout(2) .ne. xin(2) ) call stopit( status,
     :                                   'CmpRegion: ANDd Error 2x')
      if( yout(2) .ne. yin(2) ) call stopit( status,
     :                                   'CmpRegion: ANDd Error 2y')
      if( xout(3) .ne. xin(3) ) call stopit( status,
     :                                   'CmpRegion: ANDd Error 3x')
      if( yout(3) .ne. yin(3) ) call stopit( status,
     :                                   'CmpRegion: ANDd Error 3y')
      if( xout(4) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDd Error 4x')
      if( yout(4) .ne. AST__BAD ) call stopit( status,
     :                                   'CmpRegion: ANDd Error 4y')


      cr2 = ast_cmpregion( r2, r1, AST__AND, ' ', status )

      fs = ast_convert( cr, cr2, ' ', status )
      if( fs .eq. AST__NULL ) call stopit( status,
     :                                    'CmpRegion: Error 5')
      map = ast_getmapping( fs, AST__BASE, AST__CURRENT, status )
      frm = ast_getframe( fs, AST__CURRENT, status )
      cr3 = ast_mapRegion( cr, map, frm, status )
      if( ast_overlap( cr3, cr2, status ) .ne. 6 ) call stopit(status,
     :                                        'CmpRegion: Error 6' )

      cr = ast_Copy( cr, status )
      call checkdump( cr, 'checkdump CmpRegion: cr', status )


      cr2 = ast_Copy( cr, status )
      call ast_negate( cr2, status )

      cr3 = ast_cmpregion( cr2, cr, AST__OR, ' ', status )
      cr3 = ast_Simplify( cr3, status )
      if( .not. ast_isanullregion( cr3, status ) ) then
         call stopit(status, 'CmpRegion: Error 7' )
      else if( .not. ast_getl( cr3, 'negated', status ) ) then
         call stopit(status, 'CmpRegion: Error 8' )
      end if

      cr3 = ast_cmpregion( cr2, cr, AST__AND, ' ', status )
      cr3 = ast_Simplify( cr3, status )
      if( .not. ast_isanullregion( cr3, status ) ) then
         call stopit(status, 'CmpRegion: Error 9' )
      else if( ast_getl( cr3, 'negated', status ) ) then
         call stopit(status, 'CmpRegion: Error 10' )
      end if



      f1 = ast_frame( 2, ' ', status )
      p1(1) = 0.0
      p1(2) = 0.0
      p2(1) = 1.0
      p2(2) = 1.0
      r1 = ast_box( f1, 0, p1, p2, AST__NULL, ' ', status )

      p1(1) = -1.0
      p1(2) = 0.0
      p2(1) = 0.0
      p2(2) = 0.0
      r2 = ast_circle( f1, 0, p1, p2, AST__NULL, ' ', status )

      p1(1) = 1.0
      p1(2) = 0.0
      p2(1) = 0.0
      p2(2) = 0.0
      r3 = ast_circle( f1, 0, p1, p2, AST__NULL, ' ', status )

      cr = ast_cmpregion( r2, r3, AST__OR, ' ', status )
      call checkdump( cr, 'checkdump CmpRegion: cr', status )

      call ast_negate( cr, status )
      cr2 = ast_cmpregion( cr, r1, AST__AND, ' ', status )
      call checkdump( cr2, 'checkdump CmpRegion: cr2', status )

      cr2 = ast_simplify( cr2, status )

      xin( 1 ) = 0.0
      xin( 2 ) = 0.2
      xin( 3 ) = 0.5
      xin( 4 ) = -0.5

      yin( 1 ) = 0.5
      yin( 2 ) = 1.5
      yin( 3 ) = 0.5
      yin( 4 ) = 0.5

      call ast_tran2( cr2, 4, xin, yin, .true., xout, yout, status )

      if( xout(1) .ne. xin(1) ) call stopit( status,
     :                                    'CmpRegion:Error 11')
      if( yout(1) .ne. yin(1) ) call stopit( status,
     :                                    'CmpRegion:Error 12')
      if( xout(2) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion:Error 13')
      if( yout(2) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion:Error 14')
      if( xout(3) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion:Error 15')
      if( yout(3) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion:Error 16')
      if( xout(4) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion:Error 17')
      if( yout(4) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion:Error 18')



      call ast_negate( cr2, status )
      cr2 = ast_simplify( cr2, status )
      call ast_tran2( cr2, 4, xin, yin, .true., xout, yout, status )

      if( xout(1) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion:Error 19')
      if( yout(1) .ne. AST__BAD ) call stopit( status,
     :                                    'CmpRegion:Error 20')
      if( xout(2) .ne. xin(2) ) call stopit( status,
     :                                    'CmpRegion:Error 21')
      if( yout(2) .ne. yin(2) ) call stopit( status,
     :                                    'CmpRegion:Error 22')
      if( xout(3) .ne. xin(3) ) call stopit( status,
     :                                    'CmpRegion:Error 23')
      if( yout(3) .ne. yin(3) ) call stopit( status,
     :                                    'CmpRegion:Error 24')
      if( xout(4) .ne. xin(4) ) call stopit( status,
     :                                    'CmpRegion:Error 25')
      if( yout(4) .ne. yin(4) ) call stopit( status,
     :                                    'CmpRegion:Error 26')





      call ast_end( status )
      if( status .ne. sai__ok ) write(*,*) 'CmpRegion tests failed'

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
      character buf*45000

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

      end

      subroutine mysource( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, ll
      character buf*45000

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
      character buf*45000
      character line*1000

      common /ss1/ buf
      common /ss2/ next, end, ll

      if( status .ne. sai__ok ) return

      line = ' '
      call ast_getline( line, l, status )
      call chr_fandl( line( : l ), f, l )
      buf( next : ) = line( f : l )
      l = l - f + 1

      if( next + ll - 1 .ge. 45000 ) then
         write(*,*)
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




      subroutine checkPrism( status )
      implicit none
      include 'AST_PAR'
      include 'SAE_PAR'

      integer f1, f2, r1, r2, r3, r4, status
      double precision lbnd(5),ubnd(5),p1(5),p2(5)
      logical hasframeset

      if( status .ne.sai__ok ) return

      call ast_begin( status )

      f1 = ast_skyframe( 'system=fk5', status )
      p1(1) = 0.0
      p1(2) = 0.0
      p2(1) = 1.0E-4
      p2(2) = 1.0E-4
      r1 = ast_box( f1, 0, p1, p2, AST__NULL, ' ', status )

      f2 = ast_specframe( 'Unit=Angstrom', status )
      lbnd( 1 ) = 5000.0
      ubnd( 1 ) = 6000.0
      r2 = ast_interval( f2, lbnd, ubnd, AST__NULL, ' ', status )
      r3 = ast_prism( r1, r2, ' ', status )

      call checkdump( r3, 'checkdump Prism 1', status )

      if( ast_overlap( r3, r3, status ) .ne. 5 ) call stopit( status,
     :                                                    'Prism 1' )

      r4 = ast_Simplify( r3, status )
      if( .not. ast_isabox( r4, status ) ) call stopit( status,
     :                                                    'Prism 1b' )
      if( hasframeset( r4, status ) ) call stopit( status, 'Prism 1c' )
      if( ast_overlap( r3, r4, status ) .ne. 5 ) call stopit( status,
     :                                                    'Prism 1d' )


      lbnd( 1 ) = 5500.0
      ubnd( 1 ) = 5800.0
      r2 = ast_interval( f2, lbnd, ubnd, AST__NULL, ' ', status )
      r4 = ast_prism( r1, r2, ' ', status )

      if( ast_overlap( r3, r4, status ) .ne. 3 ) call stopit( status,
     :                                                    'Prism 2' )
      if( ast_overlap( r4, r3, status ) .ne. 2 ) then
         write(*,*) ast_overlap( r4, r3, status ),' should be 2'
         call stopit( status, 'Prism 3' )
      end if

      lbnd( 1 ) = 5500.0
      ubnd( 1 ) = 6500.0
      r2 = ast_interval( f2, lbnd, ubnd, AST__NULL, ' ', status )
      r4 = ast_prism( r1, r2, ' ', status )
      if( ast_overlap( r3, r4, status ) .ne. 4 ) call stopit( status,
     :                                                    'Prism 4' )
      if( ast_overlap( r4, r3, status ) .ne. 4 ) call stopit( status,
     :                                                    'Prism 5' )

      lbnd( 1 ) = 6500.0
      ubnd( 1 ) = 7500.0
      r2 = ast_interval( f2, lbnd, ubnd, AST__NULL, ' ', status )
      r4 = ast_prism( r1, r2, ' ', status )
      if( ast_overlap( r3, r4, status ) .ne. 1 ) call stopit( status,
     :                                                    'Prism 6' )
      if( ast_overlap( r4, r3, status ) .ne. 1 ) call stopit( status,
     :                                                    'Prism 7' )

      r4 = ast_copy( r3, status )
      call ast_Negate( r4, status )
      if( ast_overlap( r4, r3, status ) .ne. 6 ) call stopit( status,
     :                                                    'Prism 8' )


      p1(1) = 2.0E-4
      p1(2) = 2.0E-4
      p2(1) = 1.1E-4
      p2(2) = 1.0E-4
      r1 = ast_box( f1, 0, p1, p2, AST__NULL, ' ', status )
      lbnd( 1 ) = 5000.0
      ubnd( 1 ) = 6000.0
      r2 = ast_interval( f2, lbnd, ubnd, AST__NULL, ' ', status )
      r4 = ast_prism( r1, r2, ' ', status )
      if( ast_overlap( r3, r4, status ) .ne. 1 ) call stopit( status,
     :                                                    'Prism 9' )

      p1(1) = 2.0E-4
      p1(2) = 2.0E-4
      p2(1) = 1.0E-4
      p2(2) = 1.0E-4
      r1 = ast_box( f1, 0, p1, p2, AST__NULL, ' ', status )
      r4 = ast_prism( r1, r2, ' ', status )
      if( ast_overlap( r3, r4, status ) .ne. 4 ) call stopit( status,
     :                                                    'Prism 10' )

      call ast_setl( r3, 'Closed', .false., status )
      call ast_setl( r4, 'Closed', .false., status )
      if( ast_overlap( r3, r4, status ) .ne. 1 ) call stopit( status,
     :                                                    'Prism 11' )



      f1 = ast_skyframe( 'system=fk5', status )
      p1(1) = 0.0
      p1(2) = 0.0
      p2(1) = 1.0E-4
      p2(2) = 1.0E-4
      r1 = ast_box( f1, 0, p1, p2, AST__NULL, ' ', status )

      f2 = ast_specframe( 'System=Wavelen,Unit=Angstrom', status )
      lbnd( 1 ) = 5000.0
      ubnd( 1 ) = AST__BAD
      r2 = ast_interval( f2, lbnd, ubnd, AST__NULL, ' ', status )
      r3 = ast_prism( r1, r2, ' ', status )

      lbnd( 1 ) = 6000.0
      ubnd( 1 ) = AST__BAD
      r2 = ast_interval( f2, lbnd, ubnd, AST__NULL, ' ', status )
      r4 = ast_prism( r1, r2, ' ', status )

      call ast_setc( r3, 'system(1)', 'galactic', status )

      if( ast_overlap( r3, r4, status ) .ne. 3 ) call stopit( status,
     :                                                    'Prism 12' )

      ubnd( 1 ) = 6000.0
      lbnd( 1 ) = AST__BAD
      r2 = ast_interval( f2, lbnd, ubnd, AST__NULL, ' ', status )
      r4 = ast_prism( r1, r2, ' ', status )

      if( ast_overlap( r3, r4, status ) .ne. 4 ) call stopit( status,
     :                                                    'Prism 13' )

      ubnd( 1 ) = 5000.0
      lbnd( 1 ) = AST__BAD
      r2 = ast_interval( f2, lbnd, ubnd, AST__NULL, ' ', status )
      r4 = ast_prism( r1, r2, ' ', status )
      call ast_setc( r4, 'system(3)', 'freq', status )
      if( ast_overlap( r3, r4, status ) .ne. 4 ) call stopit( status,
     :                                                    'Prism 14' )

      call ast_setl( r4, 'closed', .false., status )
      if( ast_overlap( r3, r4, status ) .ne. 1 ) call stopit( status,
     :                                                    'Prism 15' )


      f1 = ast_skyframe( 'system=fk5', status )
      p1(1) = 0.0
      p1(2) = -1.57
      p2(1) = 0.8
      p2(2) = -1.5
      r1 = ast_box( f1, 0, p1, p2, AST__NULL, ' ', status )

      f2 = ast_specframe( 'Unit=Angstrom', status )
      lbnd( 1 ) = 5000.0
      ubnd( 1 ) = 6000.0
      r2 = ast_interval( f2, lbnd, ubnd, AST__NULL, ' ', status )
      r3 = ast_prism( r1, r2, ' ', status )
      r4 = ast_Simplify( r3, status )

      call ast_getregionbounds( r4, lbnd, ubnd, status )
      if( abs( lbnd(1) + 0.8D0 ) .gt. 1.0E-6 ) call stopit( status,
     :                                               'Prism 16' )
      if( abs( lbnd(2) + 1.64D0 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Prism 17' )
      if( abs( ubnd(1) - 0.8D0 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Prism 18' )
      if( abs( ubnd(2) + 1.5 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Prism 19' )
      if( abs( lbnd(3) - 5000.0 ) .gt. 1.0E-10 )
     :           call stopit( status, 'Prism 20' )
      if( abs( ubnd(3) - 6000.0 ) .gt. 1.0E-6 )
     :           call stopit( status, 'Prism 21' )

      call ast_end( status )
      if( status .ne. sai__ok ) write(*,*) 'Prism tests failed'

      end



      subroutine checkRemoveRegions( status )
      implicit none

      include 'SAE_PAR'
      include 'AST_PAR'

      integer status, sf1, sf2, reg, fs, map, fs2
      double precision cen(2), ixin(2), iyin(2), gxin(2), gyin(2),
     :                 xout(2), yout(2)

      if( status .ne. sai__ok ) return

      call ast_begin( status )

      sf1 = ast_skyframe( 'System=ICRS', status )
      cen(1) = 0.0
      cen(2) = 0.0
      reg = ast_circle( sf1, 1, cen, 0.001D0, AST__NULL, ' ', status )

      ixin(1) = 0.0
      iyin(1) = 0.0
      ixin(2) = 0.01
      iyin(2) = 0.01

      call ast_tran2( reg, 2, ixin, iyin, .true., xout, yout, status )

      if( xout(1) .eq. AST__BAD .or. yout(1) .eq. AST__BAD ) then
         call stopit( status, 'RemoveRegions test 1 failed' )

      else if( abs( xout(1) - ixin(1) ) .gt. 1.0E-10 .or.
     :         abs( yout(1) - iyin(1) ) .gt. 1.0E-10 ) then
         call stopit( status, 'RemoveRegions test 2 failed' )

      else if( xout(2) .ne. AST__BAD .or. yout(2) .ne. AST__BAD ) then
         write(*,*) xout(2), ixin(2)
         write(*,*) yout(2), iyin(2)
         call stopit( status, 'RemoveRegions test 3 failed' )
      end if




      sf2 = ast_skyframe( 'System=Galactic', status )
      fs = ast_convert( sf1, sf2, ' ', status )
      call ast_tran2( fs, 2, ixin, iyin, .true., gxin, gyin, status )

      fs2 = ast_frameset( sf2, ' ', status )
      call ast_addframe( fs2, AST__BASE, ast_unitmap( 2, ' ', status ),
     :                   sf2, status )


      fs = ast_convert( fs2, reg, ' ', status )


      map = ast_getmapping( fs, AST__BASE, AST__CURRENT, status )
      call ast_tran2( map, 2, gxin, gyin, .true., xout, yout, status )

      if( xout(1) .eq. AST__BAD .or. yout(1) .eq. AST__BAD ) then
         call stopit( status, 'RemoveRegions test 4 failed' )

      else if( abs( xout(1) - ixin(1) ) .gt. 1.0E-10 .or.
     :           abs( yout(1) - iyin(1) ) .gt. 1.0E-10 ) then
         call stopit( status, 'RemoveRegions test 5 failed' )

      else if( xout(2) .ne. AST__BAD .or. yout(2) .ne. AST__BAD ) then
         write(*,*) xout(2), ixin(2)
         write(*,*) yout(2), iyin(2)
         call stopit( status, 'RemoveRegions test 6 failed' )
      end if



      fs2 = ast_removeregions( fs, status )

      map = ast_getmapping( fs2, AST__BASE, AST__CURRENT, status )
      call ast_tran2( map, 2, gxin, gyin, .true., xout, yout, status )

      if( xout(1) .eq. AST__BAD .or. yout(1) .eq. AST__BAD ) then
         call stopit( status, 'RemoveRegions test 7 failed' )

      else if( abs( xout(1) - ixin(1) ) .gt. 1.0E-10 .or.
     :           abs( yout(1) - iyin(1) ) .gt. 1.0E-10 ) then
         call stopit( status, 'RemoveRegions test 8 failed' )

      else if( abs( xout(2) - ixin(2) ) .gt. 1.0E-10 .or.
     :           abs( yout(2) - iyin(2) ) .gt. 1.0E-10 ) then
         call stopit( status, 'RemoveRegions test 9 failed' )
      end if



      call ast_end( status )

      end





      subroutine checkConvex( status )
      implicit none

      include 'SAE_PAR'
      include 'AST_PAR'

      integer nx, ny, nel
      parameter( nx  = 8 )
      parameter( ny  = 7 )
      parameter( nel  = nx*ny )

      integer status, poly, lbnd(2), ubnd(2), npoint
      real array( nx, ny )
      double precision points( 10, 2 )

      data array / nel*0.0 /
      data lbnd / -10, 3 /

      if( status .ne. sai__ok ) return

      call ast_begin( status )

      ubnd( 1 ) = lbnd( 1 ) + nx- 1
      ubnd( 2 ) = lbnd( 2 ) + ny- 1

      array( 6, 1 ) = 1.0
      array( 7, 1 ) = 1.0
      array( 8, 1 ) = 1.0
      array( 7, 2 ) = 1.0
      array( 8, 2 ) = 1.0
      array( 2, 3 ) = 1.0
      array( 8, 3 ) = 1.0
      array( 1, 4 ) = 1.0
      array( 1, 6 ) = 1.0
      array( 2, 6 ) = 1.0
      array( 6, 6 ) = 1.0

      poly = ast_convexr( 1.0, AST__EQ, array, lbnd, ubnd, .FALSE.,
     :                    status )

      call ast_getregionpoints( poly, 10, 2, npoint, points, status )

      if( npoint .ne. 7 ) call stopit( status, 'Convex 1' )
      if( points( 1, 1 ) .ne. -3) call stopit( status, 'Convex 2' )
      if( points( 1, 2 ) .ne. 3) call stopit( status, 'Convex 3' )
      if( points( 2, 1 ) .ne. -3) call stopit( status, 'Convex 4' )
      if( points( 2, 2 ) .ne. 5) call stopit( status, 'Convex 5' )
      if( points( 3, 1 ) .ne. -5) call stopit( status, 'Convex 6' )
      if( points( 3, 2 ) .ne. 8) call stopit( status, 'Convex 7' )
      if( points( 4, 1 ) .ne. -10) call stopit( status, 'Convex 8' )
      if( points( 4, 2 ) .ne. 8) call stopit( status, 'Convex 9' )
      if( points( 5, 1 ) .ne. -10) call stopit( status, 'Convex 10' )
      if( points( 5, 2 ) .ne.  6) call stopit( status, 'Convex 11' )
      if( points( 6, 1 ) .ne.  -9) call stopit( status, 'Convex 12' )
      if( points( 6, 2 ) .ne.  5) call stopit( status, 'Convex 13' )
      if( points( 7, 1 ) .ne.  -5) call stopit( status, 'Convex 14' )
      if( points( 7, 2 ) .ne.  3) call stopit( status, 'Convex 15' )

      call ast_end( status )

      end

