      program fcarytest
      implicit none
      include 'SAE_PAR'
      include 'ARY_PAR'
      include 'ARY_ERR'
      include 'DAT_PAR'
      include 'CNF_PAR'

      character form*(ARY__SZFRM)
      character ftype*(ARY__SZFTP)
      character loc*(dat__szloc)
      character loc2*(dat__szloc)
      character loc3*(dat__szloc)
      character name2*(dat__sznam)
      character name3*(dat__sznam)
      character sctype*(ARY__SZTYP)
      character text*200
      character type*(DAT__SZTYP)
      double precision scale
      double precision dsum
      double precision zero
      integer axis
      integer iary
      integer iary2
      integer iary3
      integer iary4
      integer iipntr
      integer ipntr
      integer irpntr
      integer ival
      integer lstat
      integer ndim
      integer oplen
      integer place
      integer status
      integer*8 dims( ARY__MXDIM )
      integer*8 el
      integer*8 el2
      integer*8 i
      integer*8 lbnd( ARY__MXDIM )
      integer*8 ngood
      integer*8 offs( ARY__MXDIM )
      integer*8 shift(2)
      integer*8 ubnd( ARY__MXDIM )
      logical bad
      logical base
      logical defined
      logical isect
      logical lval
      logical ok
      logical same
      logical temp
      logical there
      logical mapped
      real zratio


      status = sai__ok

      call err_mark

c Test accessing an existing array.
c ================================

      call hds_open( './test_array', 'Read', loc, status )
      call ary_find( loc, 'data_array', iary, status )
      call ary_msg( 'A', iary )

      call msg_load( ' ', '^A', text, oplen, status )
      if( status .eq. SAI__OK .and. text .ne. '/stardev/git/starlink/'//
     :         'libraries/ary/./test_array.DATA_ARRAY' ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 1a', status )
      end if

      call dat_find( loc, 'data_array', loc2, status )
      call ary_imprt( loc2, iary2, status )
      call ary_same( iary, iary2, same, isect, status )
      if( .not. same .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 1b', status )
      end if

      call ary_loc( iary, loc3, status )
      call dat_name( loc2, name2, status )
      call dat_name( loc3, name3, status )
      call dat_annul( loc3, status )
      if( status .eq. SAI__OK .and. name2 .ne. name3 ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 1b1', status )
      end if

      call ary_annul( iary2, status )
      call dat_annul( loc2, status )




      call ary_ftype( iary, ftype, status )
      if( ftype .ne. '_REAL' .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 1c', status )
      end if

      call ary_isacc( iary, 'WRITE', ok, status )
      if( ok .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 1c1', status )
      end if

      if( status .eq. SAI__OK ) then
         call ary_ptszd( iary, 2.0, -1.0, status )
         if( status .ne. ARY__ACDEN ) then
            if( status .eq. SAI__OK ) status = SAI__ERROR
            call err_rep( ' ', 'Error 1c1a', status )
         else
            call err_annul( status )
         end if
      end if

      call ary_gtszd( iary, scale, zero, status )
      if( ( scale .ne. 1.0 .or. zero .ne. 0.0 ) .and.
     :      status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 1c2', status )
      end if

      call ary_ismap( iary, mapped, status )
      if( mapped .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 1c3', status )
      end if

      lbnd( 1 ) = 1000
      lbnd( 2 ) = 1
      lbnd( 3 ) = 1950
      ubnd( 1 ) = 1020
      ubnd( 2 ) = 16
      ubnd( 3 ) = 2040
      call ary_sect( iary, 3, lbnd, ubnd, iary2, status )

      call ary_offs( iary, iary2, 6, offs, status )
      do i = 1,6
         if( offs( i ) .ne. 0 .and. status .eq. SAI__OK ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 1c4', status )
         end if
      end do

      call ary_map( iary2, '_DOUBLE', 'Read', ipntr, el, status )
      if( el .ne. 30576 .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 2', status )
      else if( status .eq. SAI__OK ) then
         call statsD( el, %val( cnf_pval( ipntr ) ), dsum, ngood,
     :                status )
         if( ngood .ne. 13650 ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 3', status )
         else if( dsum .ne. 20666.916872823029D0 ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 4', status )
         end if
      end if

      if( status .eq. SAI__OK ) then
         call ary_mapz( iary2, '_DOUBLE', 'Read', irpntr, iipntr,
     :                  el, status )
         if( status .ne. ARY__ISMAP ) then
            if( status .eq. SAI__OK ) status = SAI__ERROR
            call err_rep( ' ', 'Error 4a0', status )
         else
            call err_annul( status )
         end if
      end if

      call ary_llone( iary2, iary3, status )
      call ary_mapz( iary3, '_DOUBLE', 'Read', irpntr, iipntr,
     :               el, status )
      if( el .ne. 30576 .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4a1', status )
      else if( status .eq. SAI__OK ) then
         call statsD( el, %val( cnf_pval( irpntr ) ), dsum, ngood,
     :                status )
         if( ngood .ne. 13650 ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 4a2', status )
         else if( dsum .ne. 20666.916872823029D0 ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 4a3', status )
         end if

         call statsD( el, %val( cnf_pval( iipntr ) ), dsum, ngood,
     :                status )
         if( ngood .ne. 30576 ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 4a4', status )
         else if( dsum .ne. 0.0D0 ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 4a5', status )
         end if
      end if
      call ary_annul( iary3, status )

      call ary_bad( iary2, .true., bad, status )
      if( .not. bad .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4c', status )
      end if

      call ary_bad( iary2, .false., bad, status )
      if( .not. bad .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4b', status )
      end if

      call ary_same( iary, iary2, same, isect, status )
      if( .not. same .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4b1', status )
      end if
      if( .not. isect .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4b2', status )
      end if

      call ary_base( iary2, iary3, status )
      call ary_same( iary, iary3, same, isect, status )
      if( .not. same .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4b3', status )
      end if

      call ary_isbas( iary, base, status )
      if( .not. base .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4b4', status )
      end if

      call ary_isbas( iary2, base, status )
      if( base .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4b5', status )
      end if

      call ary_ismap( iary2, mapped, status )
      if( .not. mapped .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4b6', status )
      end if

      call ary_istmp( iary2, temp, status )
      if( temp .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4b7', status )
      end if

      call ary_annul( iary3, status )
      call ary_annul( iary2, status )

      lbnd( 1 ) = 1023
      lbnd( 2 ) = 7
      lbnd( 3 ) = 2008
      ubnd( 1 ) = 1023
      ubnd( 2 ) = 7
      ubnd( 3 ) = 2008
      call ary_sect( iary, 3, lbnd, ubnd, iary2, status )
      call ary_map( iary2, '_DOUBLE', 'Read', ipntr, el, status )

      call ary_bad( iary2, .true., bad, status )
      if( bad .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4d', status )
      end if

      call ary_bad( iary2, .false., bad, status )
      if( .not. bad .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4e', status )
      end if

      call hds_new( 'cary_test2', 'TEST', 'TEST', 0, 0, loc2, status )
      call ary_place( loc2, 'DATA_ARRAY', place, status )
      call ary_copy( iary2, place, iary3, status )
      call ary_bound( iary3, 3, lbnd, ubnd, ndim, status )
      if( lbnd( 1 ) .ne. 1023 .or.
     :    lbnd( 2 ) .ne. 7 .or.
     :    lbnd( 3 ) .ne. 2008 .or.
     :    ubnd( 1 ) .ne. 1023 .or.
     :    ubnd( 2 ) .ne. 7 .or.
     :    ubnd( 3 ) .ne. 2008 .or.
     :    ndim .ne. 3 ) then
         if( status .eq. SAI__OK ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 4f', status )
         end if
      end if

      call ary_clone( iary3, iary4, status )
      if( .not. ary_valid( iary4, status ) .and.
     :    status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4f1', status )
      end if
      call dat_there( loc2, 'DATA_ARRAY', there, status )
      if( .not. there .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4g', status )
      end if
      call ary_delet( iary3, status )
      call dat_there( loc2, 'DATA_ARRAY', there, status )
      if( there .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4h', status )
      end if
      if( ary_valid( iary4, status ) .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4h1', status )
      end if

      call dat_annul( loc2, status )

      call ary_temp( place, status )
      call ary_dupe( iary2, place, iary3, status )
      call ary_bound( iary3, 3, lbnd, ubnd, ndim, status )
      if( lbnd( 1 ) .ne. 1023 .or.
     :    lbnd( 2 ) .ne. 7 .or.
     :    lbnd( 3 ) .ne. 2008 .or.
     :    ubnd( 1 ) .ne. 1023 .or.
     :    ubnd( 2 ) .ne. 7 .or.
     :    ubnd( 3 ) .ne. 2008 .or.
     :    ndim .ne. 3 ) then
         if( status .eq. SAI__OK ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 4h2', status )
         end if
      end if

      call ary_state( iary2, defined, status )
      if( .not. defined .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4h3', status )
      end if

      call ary_state( iary3, defined, status )
      if( defined .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4h4', status )
      end if

      call ary_istmp( iary3, temp, status )
      if( .not. temp .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4h5', status )
      end if

      call ary_annul( iary3, status )

      call ary_annul( iary2, status )
      iary3 = iary





c  Test creating a new array.

      call hds_new( 'cary_test', 'TEST', 'TEST', 0, 0, loc2, status )
      call ary_place( loc2, 'data_array', place, status )
      lbnd( 1 ) = -10
      lbnd( 2 ) = -30
      lbnd( 3 ) = -20
      lbnd( 4 ) = -50
      ubnd( 1 ) = 0
      ubnd( 2 ) = 10
      ubnd( 3 ) = 20
      ubnd( 4 ) = 30

      call ary_new( '_UWORD', 4, lbnd, ubnd, place, iary, status )
      call ary_map( iary, '_INTEGER', 'Write/ZERO', ipntr, el, status )
      call ary_unmap( iary, status )

      call ary_same( iary, iary3, same, isect, status )
      if( same .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4e1', status )
      end if

      call ary_form( iary, form, status )
      if( form .ne. 'SIMPLE' .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4e2', status )
      end if

      call ary_clone( iary, iary2, status )
      call ary_isacc( iary2, 'WRITE', ok, status )
      if( .not. ok .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4e3', status )
      end if

      call ary_noacc( 'Write', iary2, status )
      call ary_isacc( iary2, 'WRITE', ok, status )
      if( ok .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4e4', status )
      end if
      call ary_annul( iary2, status )

c THESE TWO CALLS FAIL IF THEY ARE SWAPPED .not. !! But the same
c happens with the F77 version of iary, so presumably it's correct
c behaviour.
      call ary_annul( iary3, status )
      call dat_annul( loc, status )


      lbnd( 1 ) = -15
      lbnd( 2 ) = -20
      lbnd( 3 ) = -20
      lbnd( 4 ) = -10
      ubnd( 1 ) = 10
      ubnd( 2 ) = 0
      ubnd( 3 ) = 20
      ubnd( 4 ) = 40
      call ary_sect( iary, 4, lbnd, ubnd, iary2, status )
      call ary_map( iary2, '_DOUBLE', 'Update', ipntr, el, status )

      if( el .ne. 1141686 .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 5', status )
      else if( status .eq. SAI__OK ) then
         call fillD( el, %val( cnf_pval( ipntr ) ), 1.0D0, status )
      end if

      call ary_ndim( iary2, ndim, status )
      if( ndim .ne. 4 .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 5a', status )
      end if

      call ary_annul( iary2, status )
      call ary_annul( iary, status )



      call ary_place( loc2, 'image', place, status )
      ubnd( 1 ) = 2
      ubnd( 2 ) = 10
      call ary_newp( '_BYTE', 2, ubnd, place, iary, status )
      call ary_bound( iary, ARY__MXDIM, lbnd, ubnd, ndim, status )
      if( ( lbnd( 1 ) .ne. 1 ) .or.
     :    ( lbnd( 2 ) .ne. 1 ) .or.
     :    ( ubnd( 1 ) .ne. 2 ) .or.
     :    ( ubnd( 2 ) .ne. 10 ) ) then
         if( status .eq. SAI__OK ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 5b', status )
         end if
      end if
      call ary_form( iary, form, status )
      if( form .ne. 'PRIMITIVE' .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 5c', status )
      end if
      call ary_map( iary, '_DOUBLE', 'Write', ipntr, el, status )
      if( el .ne. 20 .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 5d', status )
      else if( status .eq. SAI__OK ) then
         call fillD( el, %val( cnf_pval( ipntr ) ), 1.0D0, status )
      end if

      call ary_annul( iary, status )
      call dat_annul( loc2, status )





      call hd_open( 'cary_test', 'Read', loc, status )
      call ary_find( loc, 'data_array', iary, status )
      call ary_map( iary, '_DOUBLE', 'Read', ipntr, el, status )
      if( el .ne. 1497771 .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 6', status )
      else if( status .eq. SAI__OK ) then
         call statsD( el, %val( cnf_pval( ipntr ) ), dsum, ngood,
     :                status )
         if( ngood .ne. 1497771 ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 7', status )
         else if( dsum .ne. 388311.0D0 ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 8', status )
         end if
      end if
      call ary_annul( iary, status )
      call dat_annul( loc, status )


c Test creating a temporary array.

      call ary_temp( place, status )
      lbnd( 1 ) = -10
      lbnd( 2 ) = -30
      lbnd( 3 ) = -20
      lbnd( 4 ) = -50
      ubnd( 1 ) = 0
      ubnd( 2 ) = 10
      ubnd( 3 ) = 20
      ubnd( 4 ) = 30

      call ary_new( '_UWORD', 4, lbnd, ubnd, place, iary, status )
      call ary_map( iary, '_INTEGER', 'Write/ZERO', ipntr, el, status )
      call ary_unmap( iary, status )

      lbnd( 1 ) = -15
      lbnd( 2 ) = -20
      lbnd( 3 ) = -20
      lbnd( 4 ) = -10
      ubnd( 1 ) = 10
      ubnd( 2 ) = 0
      ubnd( 3 ) = 20
      ubnd( 4 ) = 40
      call ary_sect( iary, 4, lbnd, ubnd, iary2, status )
      call ary_ssect( iary, iary2, iary3, status )
      call ary_bound( iary3, ARY__MXDIM, lbnd, ubnd, ndim, status )
      if( ( lbnd( 1 ) .ne. -15 ) .or.
     :    ( lbnd( 2 ) .ne. -20 ) .or.
     :    ( lbnd( 3 ) .ne. -20 ) .or.
     :    ( lbnd( 4 ) .ne. -10 ) .or.
     :    ( ubnd( 1 ) .ne. 10 ) .or.
     :    ( ubnd( 2 ) .ne. 0 ) .or.
     :    ( ubnd( 3 ) .ne. 20 ) .or.
     :    ( ubnd( 4 ) .ne. 40 ) ) then
         if( status .eq. SAI__OK ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 11', status )
         end if
      end if
      call ary_annul( iary3, status )

      call ary_map( iary2, '_DOUBLE', 'Update', ipntr, el, status )

      if( el .ne. 1141686 .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 9', status )
      else if( status .eq. SAI__OK ) then
         call fillD( el, %val( cnf_pval( ipntr ) ), 1.0D0, status )
      end if

      call ary_annul( iary2, status )

      if( status .eq. SAI__OK ) then
         call ary_bound( iary, 2, lbnd, ubnd, ndim, status )
         if( status .ne. ARY__XSDIM ) then
            lstat = status
            if( status .ne. SAI__OK ) call err_annul( status )
            status = SAI__ERROR
            call err_rep( ' ', 'Error 10', status )
         else
            call err_annul( status )
         end if
      end if

      call ary_bound( iary, ARY__MXDIM, lbnd, ubnd, ndim, status )
      if( ( lbnd( 1 ) .ne. -10 ) .or.
     :    ( lbnd( 2 ) .ne. -30 ) .or.
     :    ( lbnd( 3 ) .ne. -20 ) .or.
     :    ( lbnd( 4 ) .ne. -50 ) .or.
     :    ( ubnd( 1 ) .ne. 0 ) .or.
     :    ( ubnd( 2 ) .ne. 10 ) .or.
     :    ( ubnd( 3 ) .ne. 20 ) .or.
     :    ( ubnd( 4 ) .ne. 30 ) ) then
         if( status .eq. SAI__OK ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 11', status )
         end if
      else if( status .eq. SAI__OK ) then
         do i = 5, ARY__MXDIM
            if( lbnd(i) .ne. 1 .or. ubnd(i) .ne. 1 ) then
               status = SAI__ERROR
               call err_rep( ' ', 'Error 12', status )
            end if
         end do
      end if

      call ary_clone( iary, iary2, status )
      call ary_same( iary, iary2, same, isect, status )
      if( .not. same .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 13', status )
      end if

      call ary_cmplx( iary2, lval, status )
      if( lval .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 14', status )
      end if

      call ary_annul( iary2, status )

      call ary_ptszd( iary, 2.0, -1.0, status )
      call ary_gtszd( iary, scale, zero, status )
      if( ( scale .ne. 2.0 .or. zero .ne. -1.0 ) .and.
     :      status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4h6', status )
      end if
      call ary_form( iary, form, status )
      if( form .ne. 'SCALED'  .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4h7', status )
      end if
      call ary_sctyp( iary, sctype, status )
      if( sctype .ne. '_UWORD' .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 4h8', status )
      end if
      if( status .eq. SAI__OK ) then
         call ary_verfy( iary, status )
         if( status .ne. SAI__OK ) then
            call err_rep( ' ', 'Error 4h9', status )
         end if
      end if
      call ary_annul( iary, status )




c  Test delta compression

      call ary_temp( place, status )
      lbnd( 1 ) = -10
      lbnd( 2 ) = -20
      lbnd( 3 ) = 0
      ubnd( 1 ) = 0
      ubnd( 2 ) = 10
      ubnd( 3 ) = 20

      call ary_new( '_INTEGER', 3, lbnd, ubnd, place, iary, status )
      call ary_map( iary, '_INTEGER', 'Write', ipntr, el, status )
      if( status .eq. SAI__OK ) then
         call filli( el, %val( cnf_pval( ipntr ) ), status )
      end if
      call ary_unmap( iary, status )

      call ary_dim( iary, ARY__MXDIM, dims, ndim, status )
      if( ndim .ne. 3 .or. dims(1) .ne. 11 .or. dims(2) .ne. 31 .or.
     :    dims(3) .ne. 21 ) then
         if( status .eq. SAI__OK ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 14b', status )
         end if
      end if

      call ary_temp( place, status )
      call ary_delta( iary, 0, ' ', 0.8, place, zratio, iary2, status )

      call ary_form( iary2, form, status )
      if( form .ne. 'DELTA' .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 14c', status )
      end if

      if( ( zratio .lt. 3.09731 .or. zratio > 3.09733 ) .and.
     :    status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 15', status )
      end if

      call ary_gtdlt( iary2, axis, type, zratio, status )
      if( ( zratio .lt. 3.09731 .or. zratio .gt. 3.09733 ) .and.
     :    status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 16', status )
      end if
      if( type .ne. '_BYTE' .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 17 (%s)', status, type )
      end if
      if( axis .ne. 2 .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 18', status )
      end if

      if( status .eq. SAI__OK ) then
         call ary_verfy( iary2, status )
         if( status .ne. SAI__OK ) then
            call err_rep( ' ', 'Error 18b', status )
         end if
      end if

      call ary_map( iary2, '_INTEGER', 'Read', ipntr, el2, status )

      if( status .eq. SAI__OK ) then
         if( el .ne. el2 ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 19', status )
         end if
         call checki( el, %val( cnf_pval( ipntr ) ), status )
      end if
      call ary_unmap( iary2, status )

      if( status .eq. SAI__OK ) then
         call ary_reset( iary2, status )
         if( status .ne. ARY__CMPAC ) then
            if( status .eq. SAI__OK ) status = SAI__ERROR
            call err_rep( ' ', 'Error 21', status )
         else
            call err_annul( status )
         end if
      end if

      call ary_bad( iary, .true., bad, status )
      if( bad .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 22', status )
      end if

      call ary_sbad( 1, iary, status )
      call ary_bad( iary, .false., bad, status )
      if( .not. bad .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 23', status )
      end if

      lbnd( 1 ) = -10
      lbnd( 2 ) = -10
      lbnd( 3 ) = 0
      ubnd( 1 ) = 10
      ubnd( 2 ) = 10
      ubnd( 3 ) = 20
      call ary_sbnd( 3, lbnd, ubnd, iary, status )
      call ary_bound( iary, 3, lbnd, ubnd, ndim, status )
      if( ( lbnd( 1 ) .ne. -10 ) .or.
     :    ( lbnd( 2 ) .ne. -10 ) .or.
     :    ( lbnd( 3 ) .ne. 0 ) .or.
     :    ( ubnd( 1 ) .ne. 10 ) .or.
     :    ( ubnd( 2 ) .ne. 10 ) .or.
     :    ( ubnd( 3 ) .ne. 20 ) ) then
         if( status .eq. SAI__OK ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 23a', status )
         end if
      end if

      call ary_state( iary, defined, status )
      if( .not. defined .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 24', status )
      end if
      call ary_reset( iary, status )
      call ary_state( iary, defined, status )
      if( defined .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 25', status )
      end if

      shift( 1 ) = 1
      shift( 2 ) = -2
      call ary_shift( 2, shift, iary, status )
      call ary_bound( iary, 3, lbnd, ubnd, ndim, status )
      if( ( lbnd( 1 ) .ne. -9 ) .or.
     :    ( lbnd( 2 ) .ne. -12 ) .or.
     :    ( lbnd( 3 ) .ne. 0 ) .or.
     :    ( ubnd( 1 ) .ne. 11 ) .or.
     :    ( ubnd( 2 ) .ne. 8 ) .or.
     :    ( ubnd( 3 ) .ne. 20 ) ) then
         if( status .eq. SAI__OK ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 26', status )
         end if
      end if

      call ary_size( iary, el, status )
      if( el .ne.
     :    ( ubnd( 1 ) - lbnd( 1 ) + 1 )*
     :    ( ubnd( 2 ) - lbnd( 2 ) + 1 )*
     :    ( ubnd( 3 ) - lbnd( 3 ) + 1 ) .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 27', status )
      end if

      call ary_stype( 'COMPLEX_REAL', iary, status )
      call ary_ftype( iary, ftype, status )
      if( ftype .ne. 'COMPLEX_REAL' .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 28', status )
      end if

      call ary_type( iary, type, status )
      if( type .ne. '_REAL' .and. status .eq. SAI__OK ) then
         status = SAI__ERROR
         call err_rep( ' ', 'Error 29', status )
      end if

      call ary_annul( iary2, status )
      call ary_annul( iary, status )





      call err_rlse

      end





      subroutine statsD( el, data, dsum, ngood, status )
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'

      integer status
      integer*8 el, i, ngood
      double precision data( el ), dsum

      if( status .ne. sai__ok ) return

      dsum = 0.0D0
      ngood = 0
      do i = 1, el
         if( data( i ) .ne. VAL__BADD ) then
            dsum = dsum + data( i )
            ngood = ngood + 1
         end if
      end do

      end


      subroutine fillD( el, data, ival, status )
      implicit none
      include 'SAE_PAR'

      integer*8 el, i
      integer status
      integer ival
      double precision data( el )

      if( status .ne. sai__ok ) return

      do i = 1, el
         data( i ) = ival
      end do

      end

      subroutine filli( el, data, status )
      implicit none
      include 'SAE_PAR'

      integer status
      integer*8 el, i
      double precision data( el )

      if( status .ne. sai__ok ) return

      do i = 1, el
         data( i ) = i
      end do

      end


      subroutine checki( el, data, status )
      implicit none
      include 'SAE_PAR'

      integer status
      integer*8 el, i
      double precision data( el )

      if( status .ne. sai__ok ) return

      do i = 1, el
         if( data( i ) .ne. i .and. status .eq. sai__ok ) then
            status = SAI__ERROR
            call err_rep( ' ', 'Error 20', status )
         end if
      end do

      end
