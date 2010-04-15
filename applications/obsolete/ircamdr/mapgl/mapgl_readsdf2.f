	subroutine mapgl_readsdf2( sdf_filename, nx, ny, arr, xdim, ydim)

	integer
     :	  status,
     :	  nx,
     :	  ny,
     :	  xdim,
     :	  ydim,
     :	  nptr,
     :	  dims( 2),
     :	  actdim,
     :	  j,
     :	  k,
     :	  ndim,
     :	  sptr

	real
     :	  arr( nx, ny)

	character*15
     :	  locd,
     :	  locda2,
     :	  locda

	character*(*)
     :	  sdf_filename

	call hds_start( status)

        call hds_open( sdf_filename, 'read', locd, status)

        call dat_find( locd, 'data_array', locda, status)
        call dat_find( locda, 'data', locda2, status)

        call dat_shape( locda2, 2, dims, actdim, status)
!	type *, 'Image is ', dims( 1), dims( 2)

	ndim = actdim
	xdim = dims( 1)
	ydim = dims( 2)

!	type *, 'before dat_mapr'
	call dat_mapr( locda2, 'read', ndim, dims, sptr, status)

	call mapgl_fillsdf2( xdim, ydim, %val( sptr), nx, ny, arr, status)

	call dat_annul( locda2, status)
	call dat_annul( locda, status)
	call dat_annul( locd, status)

	call hds_stop( status)

	end
