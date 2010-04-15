	program fprun

	integer fps, fpe, fpi, l1, nc, chr_len

	character*50 fnam, fnam1, fpstring, fpj

	type *, 'FP Start, End, Increment : '
	read( 5, *) fps, fpe, fpi

	type *, 'FP scan filename : '
	read( 5, '(a)') fnam1
	l1 = chr_len( fnam1)

	fnam = fnam1( 1:l1)//'.exec'
	open( unit=142, file=fnam, status='new')

	do jj = fps, fpe, fpi
	  call chr_itoc( jj, fpj, nc)
	  l1 = chr_len( fpj)
	  fpstring = 'fpz '//fpj( 1:l1)
	  write( 142, '(a)') fpstring
	  fpstring = 'object'
	  write( 142, '(a)') fpstring
	end do

	close( 142)

	end
