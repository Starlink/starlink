      program fcarytest
      implicit none
      include 'SAE_PAR'
      include 'ARY_PAR'
      include 'DAT_PAR'

      character loc*(dat__szloc)
      character loc2*(dat__szloc)
      character loc3*(dat__szloc)
      character name2*(dat__sznam)
      character name3*(dat__sznam)
      character text*200
      integer status, iary, oplen, iary2
      logical same, isect

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










      call ary_annul( iary, status )
      call dat_annul( loc, status )











      call err_rlse

      end


