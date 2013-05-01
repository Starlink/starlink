      program testswitchmap
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      integer status, outperm(2), inperm(2), wm1, wm2, pm1, pm2, rm(2),
     :               fs, is, swm, i, swm2, cm, cm2, sm, oc, mc, fc,
     :               box(2), gridframe
      double precision x1, x2, l1, l2, in(4,2), out(4,2), at(2), r,
     :                 p1(2),p2(2), rmout(4,2), r1, r2
      character text*10, fwd(1)*40, inv(2)*40, card(10)*80

      status = sai__ok

      call ast_begin( status )

c      call ast_watchmemory(22617)

      oc = ast_tune( 'ObjectCaching', 1, status )
      mc = ast_tune( 'MemoryCaching', 1, status )



*  A 2D input grid has 2 rows and 101 columns. Each row contains a spectrum.
*  The two spectra cover overlaping regions of wavelength. The spectrum in
*  row 1 has "wavelength = (gridx - 1)*10+1000", whilst the spectrum in
*  row 2 has "wavelength = (gridx - 1)*11+1600". We use a (Nin=2,Nout=1)
*  SwitchMap to describe the Mapping from grid (x,y) to wavelength. This
*  SwitchMap contains 2 route Mappings, one for each row of the grid.
*  --------------------------------------------------------------------

*  Produce a 1D Mapping from gridx to wavelength for row 1.
      x1 = 1.0
      x2 = 101.0
      l1 = 1000.0
      l2 = 2000.0
      wm1 = ast_winmap( 1, x1, x2, l1, l2, ' ', status )

*  Since the SwicthMap has 2 inputs, each of the route Mappings must also
*  have 2 inputs. Produce a PermMap which passes on its 1st input to its
*  (one and only) output. The inverse transformation supplied a value of
*  1.0 for the missing 2nd input (1.0 is the grid Y value for the first
*  row).
      outperm( 1 ) = 1
      inperm( 1 ) = 1
      inperm( 2 ) = -1
      pm1 = ast_permmap( 2, inperm, 1, outperm, 1.0D0, ' ', status )

*  Combine the PermMap and WinMap in series to get the total route
*  Mapping for the first row.
      rm(1) = ast_cmpmap( pm1, wm1, .true., ' ', status )

*  Likewise, produce the route Mapping for the second row. The grid y
*  value for the second row is 2.0, so use this as the constant in the
*  PermMap (i.e. the value which the inverse transformation supplies for
*  the missing second input).
      l1 = 1600.0
      l2 = 2700.0
      wm2 = ast_winmap( 1, x1, x2, l1, l2, ' ', status )
      pm2 = ast_permmap( 2, inperm, 1, outperm, 2.0D0, ' ', status )
      rm(2) = ast_cmpmap( pm2, wm2, .true., ' ', status )

*  The forward selector Mapping just uses the second input (the grid Y
*  value) as the selector value (i.e. gridy=1 selects the first route
*  Mapping and gridy=2 selects the second route Mapping). The inverse
*  transformation of this Mapping is never used and so does not matter.
      outperm( 1 ) = 2
      inperm( 2 ) = 1
      inperm( 1 ) = 0
      fs = ast_permmap( 2, inperm, 1, outperm, 0.0D0, ' ', status )

*  The inverse selector function needs to decide which route Mapping to
*  use for any supplied ("output") wavelength value. We arbitrarily
*  decide to to use the first row for wavelengths less than or equal to
*  1800, and the second row for wavelengths larger than 1800 (1800 is the
*  mid-point of the overlap between the two spectra). We use a MathMap to
*  implement this transformation, which must be the *inverse*
*  transformation of the MathMap. The forward transformation of the
*  inverse slector Mapping is never used and so is left unspecified in
*  the MathMap constructor.
      is = ast_mathmap( 1, 1, 1, 'y', 1, 'x=qif(y>1800,2,1)', ' ',
     :                  status )

*  Now create the SwitchMap.
      swm = ast_switchmap( fs, is, 2, rm, ' ', status )

*  Test the forward transformation of the SwitchMap. To add complication,
*  we first invert the SwitchMap and then use ast_trann in the inverse
*  direction (the two inversions cancel resulting in the forward
*  transformation being used).
      call ast_invert( swm, status )

      in(1,1) = 1.0
      in(1,2) = 1.0
      in(2,1) = 101.0
      in(2,2) = 2.0
      in(3,1) = 1.0
      in(3,2) = 2.0
      in(4,1) = 101.0
      in(4,2) = 1.0
      call ast_trann( swm, 4, 2, 4, in, .false., 1, 4, out, status )

      do i = 1, 4
         if( out(i,1) .eq. ast__bad ) then
            call stopit( i, out(i,1), status )
         end if
      end do

      if( abs( out(1,1) - 1000.0 ) .gt. 1.0E-5 ) then
         call stopit( 5, out(1,1), status )
      else if( abs( out(2,1) - 2700.0 ) .gt. 1.0E-5 ) then
         call stopit( 6, out(2,1), status )
      else if( abs( out(3,1) - 1600.0 ) .gt. 1.0E-5 ) then
         call stopit( 7, out(3,1), status )
      else if( abs( out(4,1) - 2000.0 ) .gt. 1.0E-5 ) then
         call stopit( 8, out(4,1), status )
      end if

*  Test the inverse transformation of the SwitchMap.
      call ast_trann( swm, 4, 1, 4, out, .true., 2, 4, in, status )

      do i = 1, 4
         if( in(i,1) .eq. ast__bad ) then
            call stopit( 7 + 2*i, in(i,1), status )
         else if( in(i,2) .eq. ast__bad ) then
            call stopit( 8 + 2*i, in(i,2), status )
         end if
      end do

      if( abs( in(1,1) - 1.0 ) .gt. 1.0E-5 ) then
         call stopit( 17, in(1,1), status )
      else if( abs( in(1,2) - 1.0 ) .gt. 1.0E-5 ) then
         call stopit( 18, in(1,2), status )
      else if( abs( in(2,1) - 101.0 ) .gt. 1.0E-5 ) then
         call stopit( 19, in(2,1), status )
      else if( abs( in(2,2) - 2.0 ) .gt. 1.0E-5 ) then
         call stopit( 20, in(2,2), status )
      else if( abs( in(3,1) - 61.0 ) .gt. 1.0E-5 ) then
         call stopit( 21, in(3,1), status )
      else if( abs( in(3,2) - 1.0 ) .gt. 1.0E-5 ) then
         call stopit( 22, in(3,2), status )
      else if( abs( in(4,1) - 37.3636364 ) .gt. 1.0E-5 ) then
         call stopit( 23, in(4,1), status )
      else if( abs( in(4,2) - 2.0 ) .gt. 1.0E-5 ) then
         call stopit( 24, in(4,2), status )
      end if

*  Check no simplification is done on a single non-inverted SwicthMap.
      call ast_setl( swm, 'Invert', .false., status )
      swm2 = ast_simplify( swm, status )
      call compare( swm, swm2, 'ast_equal 1', status )

*  Check an inverted SwitchMap simplies to an non-inverted switchmap.
      call ast_setl( swm, 'Invert', .true., status )
      swm2 = ast_simplify( swm, status )
      if( ast_getl( swm2, 'Invert', status ) ) then
         call stopit( 25, 1.0D0, status )
      end if

*  Check two adjacent opposite SwitchMaps cancel.
      swm2 = ast_copy( swm, status )
      call ast_invert( swm2, status )
      cm = ast_cmpmap( swm, swm2, .true., ' ', status )
      cm2 = ast_simplify( cm, status )
      if( .not. ast_isaunitmap( cm2, status ) ) then
         call stopit( 26, 1.0D0, status )
      end if

      cm = ast_cmpmap( swm2, swm, .true., ' ', status )
      cm2 = ast_simplify( cm, status )
      if( .not. ast_isaunitmap( cm2, status ) ) then
         call stopit( 27, 1.0D0, status )
      end if

*  Check that the SwitchMap can be written out to a AstChannel and read
*  back again succesfully.
      call checkdump( swm, 'Channel test 1', status )

*  Check the ast_rate function works OK.
      call ast_setl( swm, 'Invert', .false., status )

      at(1) = 20.0
      at(2) = 1.0
      r = ast_rate( swm, at, 1, 1, status )
      if( abs( r - 10.0 ) .gt. 1.0E-6 ) call stopit( 28, r, status )

      at(1) = 20.0
      at(2) = 2.0
      r = ast_rate( swm, at, 1, 1, status )
      if( abs( r - 11.0 ) .gt. 1.0E-6 ) call stopit( 29, r, status )

      call ast_setl( swm, 'Invert', .true., status )

      at(1) = 1700.0
      r = ast_rate( swm, at, 1, 1, status )
      if( abs( r - 1.0/10.0 ) .gt. 1.0E-6 ) call stopit( 30, r, status )

      at(1) = 1900.0
      r = ast_rate( swm, at, 1, 1, status )
      if( abs( r - 1.0/11.0 ) .gt. 1.0E-6 ) call stopit( 31, r, status )

      call ast_setl( swm, 'Invert', .false., status )


*  A 1D input grid has 1 rows and 202 columns. Each half of the row
*  (1:101 and 102:202) contains a spectrum. The two spectra cover
*  overlaping regions of wavelength. The spectrum in thw lower half has
*  "wavelength = (gridx - 1)*10+1000", whilst the spectrum in the upper
*  half has "wavelength = (gridx - 1)*11+1600". We use a (Nin=1,Nout=1)
*  SwitchMap to describe the Mapping from grid (x) to wavelength. This
*  SwitchMap contains 2 route Mappings, one for each half of the row.
*  --------------------------------------------------------------------

*  Produce a 1D Mapping from gridx to wavelength for the lower half.
      x1 = 1.0
      x2 = 101.0
      l1 = 1000.0
      l2 = 2000.0
      rm(1) = ast_winmap( 1, x1, x2, l1, l2, ' ', status )

*  Likewise, produce a 1D Mapping from gridx to wavelength for the upper half.
      x1 = 102.0
      x2 = 202.0
      l1 = 1600.0
      l2 = 2700.0
      rm(2) = ast_winmap( 1, x1, x2, l1, l2, ' ', status )

*  We can use a single MathMap for both selector Mappings - the forward
*  transformation (used as the forward selector) gives 1 for all gridx less
*  than 101.5 and 2 for all gridx greater than 101.5. The inverse
*  transformation (used as the inverse selector) gives 1 for all
*  wavelengths les than 1800 and 2 for all wavelength greater than 1800
*  (1800 is the mid point of the overlap region).
      sm = ast_mathmap( 1, 1, 1, 'y=qif(x>101.5,2,1)',
     :                        1, 'x=qif(y>1800,2,1)',
     :                  ' ', status )

*  Now create the SwitchMap.
      swm = ast_switchmap( sm, sm, 2, rm, ' ', status )

*  Test the forward transformation of the SwitchMap. To add complication,
*  we first invert the SwitchMap and then use ast_trann in the inverse
*  direction (the two inversions cancel resulting in the forward
*  transformation being used). We alo invert the selector mapping (this
*  should have no effect on the SwitchMap).
      call ast_invert( swm, status )
      call ast_invert( sm, status )

      in(1,1) = 1.0
      in(2,1) = 202.0
      in(3,1) = 102.0
      in(4,1) = 101.0
      call ast_trann( swm, 4, 1, 4, in, .false., 1, 4, out, status )

      do i = 1, 4
         if( out(i,1) .eq. ast__bad ) then
            call stopit( 100+i, out(i,1), status )
         end if
      end do

      if( abs( out(1,1) - 1000.0 ) .gt. 1.0E-5 ) then
         call stopit( 105, out(1,1), status )
      else if( abs( out(2,1) - 2700.0 ) .gt. 1.0E-5 ) then
         call stopit( 106, out(2,1), status )
      else if( abs( out(3,1) - 1600.0 ) .gt. 1.0E-5 ) then
         call stopit( 107, out(3,1), status )
      else if( abs( out(4,1) - 2000.0 ) .gt. 1.0E-5 ) then
         call stopit( 108, out(4,1), status )
      end if

*  Test the inverse transformation of the SwitchMap.
      call ast_trann( swm, 4, 1, 4, out, .true., 1, 4, in, status )

      do i = 1, 4
         if( in(i,1) .eq. ast__bad ) then
            call stopit( 107 + 2*i, in(i,1), status )
         end if
      end do

      if( abs( in(1,1) - 1.0 ) .gt. 1.0E-5 ) then
         call stopit( 117, in(1,1), status )
      else if( abs( in(2,1) - 202.0 ) .gt. 1.0E-5 ) then
         call stopit( 119, in(2,1), status )
      else if( abs( in(3,1) - 61.0 ) .gt. 1.0E-5 ) then
         call stopit( 121, in(3,1), status )
      else if( abs( in(4,1) - 138.3636364 ) .gt. 1.0E-5 ) then
         call stopit( 123, in(4,1), status )
      end if

*  Check no simplification is done on a single non-inverted SwicthMap.
      call ast_setl( swm, 'Invert', .false., status )
      swm2 = ast_simplify( swm, status )
      call compare( swm, swm2, 'ast_equal 2', status )

*  Check an inverted SwitchMap simplies to an non-inverted switchmap.
      call ast_setl( swm, 'Invert', .true., status )
      swm2 = ast_simplify( swm, status )
      if( ast_getl( swm2, 'Invert', status ) ) then
         call stopit( 125, 1.0D0, status )
      end if

*  Check two adjacent opposite SwitchMaps cancel.
      swm2 = ast_copy( swm, status )
      call ast_invert( swm2, status )
      cm = ast_cmpmap( swm, swm2, .true., ' ', status )
      cm2 = ast_simplify( cm, status )
      if( .not. ast_isaunitmap( cm2, status ) ) then
         call stopit( 126, 1.0D0, status )
      end if

      cm = ast_cmpmap( swm2, swm, .true., ' ', status )
      cm2 = ast_simplify( cm, status )
      if( .not. ast_isaunitmap( cm2, status ) ) then
         call stopit( 127, 1.0D0, status )
      end if

*  Check that the SwitchMap can be written out to a AstChannel and read
*  back again succesfully.
      call checkdump( swm, 'Channel test 2', status )

*  Check the ast_rate function works OK.
      call ast_setl( swm, 'Invert', .false., status )

      at(1) = 20.0
      r = ast_rate( swm, at, 1, 1, status )
      if( abs( r - 10.0 ) .gt. 1.0E-6 ) call stopit( 128, r, status )

      at(1) = 120.0
      r = ast_rate( swm, at, 1, 1, status )
      if( abs( r - 11.0 ) .gt. 1.0E-6 ) call stopit( 129, r, status )

      call ast_setl( swm, 'Invert', .true., status )

      at(1) = 1700.0
      r = ast_rate( swm, at, 1, 1, status )
      if( abs( r - 1.0/10.0 ) .gt. 1.0E-6 ) call stopit( 130, r,
     :                                                   status )

      at(1) = 1900.0
      r = ast_rate( swm, at, 1, 1, status )
      if( abs( r - 1.0/11.0 ) .gt. 1.0E-6 ) call stopit( 131, r,
     :                                                   status )

      call ast_setl( swm, 'Invert', .false., status )


*  A 2D input grid has bounds (1:180,1:100). The area (10:80,10:80)
*  and the area (100:170,10:80) both contain images of a fixed part of
*  the sky (e.g. in different polarisations). The WCS for each sub-region
*  is derived from a single FITS-WCS header,with suitably shifted origin.
*  --------------------------------------------------------------------
      card(1) = 'CRPIX1  = 45'
      card(2) = 'CRPIX2  = 45'
      card(3) = 'CRVAL1  = 45'
      card(4) = 'CRVAL2  = 89.9'
      card(5) = 'CDELT1  = -0.01'
      card(6) = 'CDELT2  = 0.01'
      card(7) = 'CTYPE1  = ''RA---TAN'''
      card(8) = 'CTYPE2  = ''DEC--TAN'''

      fc = ast_fitschan( AST_NULL, AST_NULL, ' ', status )
      do i = 1, 8
         call ast_putfits( fc, card(i), .false., status )
      end do
      call ast_clear( fc, 'Card', status )
      fs = ast_read( fc, status )
      rm(1) = ast_getMapping( fs, ast__base, ast__current, status )

      card(1) = 'CRPIX1  = 135'
      call ast_clear( fc, 'Card', status )
      do i = 1, 8
         call ast_putfits( fc, card(i), .true., status )
      end do
      call ast_clear( fc, 'Card', status )
      fs = ast_read( fc, status )
      rm(2) = ast_getMapping( fs, ast__base, ast__current, status )

*  Forward Selector Mapping: A SelectorMap which encapsulates the two Box
*  Regions in GRID coords.
      gridframe = ast_getframe( fs, AST__BASE, status )

      p1(1) = 10
      p1(2) = 10
      p2(1) = 80
      p2(2) = 80
      box(1) = ast_box( gridframe, 1, p1, p2, AST__NULL, ' ', status )

      p1(1) = 100
      p1(2) = 10
      p2(1) = 170
      p2(2) = 80
      box(2) = ast_box( gridframe, 1, p1, p2, AST__NULL, ' ', status )

      fs = ast_selectormap( 2, box, AST__BAD, ' ', status )

*  Inverse Selector Mapping: A PermMap which has an inverse transformation
*  which gives an input value of 1 for all output values. This means that the
*  inverse transformation of the SwitchMap always returns a GRID position in
*  the lower (left-hand) of the two images.
      inperm(1) = -1
      outperm(1) = 0
      outperm(2) = 0
      is = ast_permmap( 1, inperm, 2, outperm, 1.0D0, ' ', status )

*  Now create the SwitchMap.
      swm = ast_switchmap( fs, is, 2, rm, ' ', status )

*  Test the forward transformation of the SwitchMap. To add complication,
*  we first invert the SwitchMap and then use ast_trann in the inverse
*  direction (the two inversions cancel resulting in the forward
*  transformation being used). We alo invert the selector mapping (this
*  should have no effect on the SwitchMap).
      call ast_invert( swm, status )
      call ast_invert( fs, status )

      in(1,1) = 5.0
      in(1,2) = 5.0

      in(2,1) = 50.0
      in(2,2) = 50.0

      in(3,1) = 90.0
      in(3,2) = 50.0

      in(4,1) = 140.0
      in(4,2) = 50.0

      call ast_trann( swm, 4, 2, 4, in, .false., 2, 4, out, status )

*  Transform these same positions using the Mapping for the left hand
*  image obtained from the FITS header
      call ast_trann( rm(1), 4, 2, 4, in, .true., 2, 4, rmout, status )

*  Check the SwitchMap results. Points 1 and 3 should be bad because they
*  fall outside either image. points 2 and 4 should both be equal to the
*  result of transforming point 2 using the FITS-WCS mapping.

      if( out(1,1) .ne. AST__BAD ) then
         call stopit( 132, out(1,1), status )

      else if( out(1,2) .ne. AST__BAD ) then
         call stopit( 133, out(1,1), status )

      else if( out(2,1) .ne. rmout(2,1) ) then
         call stopit( 134, out(2,1), status )

      else if( out(2,2) .ne. rmout(2,2) ) then
         call stopit( 135, out(2,2), status )

      else if( out(3,1) .ne. AST__BAD ) then
         call stopit( 136, out(1,1), status )

      else if( out(3,2) .ne. AST__BAD ) then
         call stopit( 137, out(1,1), status )

      else if( out(4,1) .ne. rmout(2,1) ) then
         call stopit( 138, out(2,1), status )

      else if( out(4,2) .ne. rmout(2,2) ) then
         call stopit( 139, out(2,2), status )

      end if


*  Test the inverse transformation of the SwitchMap.
      call ast_trann( swm, 4, 2, 4, out, .true., 2, 4, in, status )

      if( in(1,1) .ne. AST__BAD ) then
         call stopit( 140, in(1,1), status )

      else if( in(1,2) .ne. AST__BAD ) then
         call stopit( 141, in(1,1), status )

      else if( abs( in(2,1) - 50.0 ) .gt. 1.0E-6 ) then
         call stopit( 142, in(2,1), status )

      else if( abs( in(2,2) - 50.0 ) .gt. 1.0E-6 ) then
         call stopit( 143, in(2,2), status )

      else if( in(3,1) .ne. AST__BAD ) then
         call stopit( 144, in(1,1), status )

      else if( in(3,2) .ne. AST__BAD ) then
         call stopit( 145, in(1,1), status )

      else if( abs( in(4,1) - 50.0 ) .gt. 1.0E-6 ) then
         call stopit( 146, in(2,1), status )

      else if( abs( in(4,2) - 50.0 ) .gt. 1.0E-6 ) then
         call stopit( 147, in(2,2), status )

      end if

*  Check no simplification is done on a single non-inverted SwicthMap.
      call ast_setl( swm, 'Invert', .false., status )
      swm2 = ast_simplify( swm, status )
      call compare( swm, swm2, 'ast_equal 3', status )

*  Check an inverted SwitchMap simplies to an non-inverted switchmap.
      call ast_setl( swm, 'Invert', .true., status )
      swm2 = ast_simplify( swm, status )
      if( ast_getl( swm2, 'Invert', status ) ) then
         call stopit( 148, 1.0D0, status )
      end if

*  Check two adjacent opposite SwitchMaps cancel.
      swm2 = ast_copy( swm, status )
      call ast_invert( swm2, status )
      cm = ast_cmpmap( swm, swm2, .true., ' ', status )
      cm2 = ast_simplify( cm, status )
      if( .not. ast_isaunitmap( cm2, status ) ) then
         call stopit( 149, 1.0D0, status )
      end if

      cm = ast_cmpmap( swm2, swm, .true., ' ', status )
      cm2 = ast_simplify( cm, status )
      if( .not. ast_isaunitmap( cm2, status ) ) then
         call stopit( 150, 1.0D0, status )
      end if

*  Check that the SwitchMap can be written out to a AstChannel and read
*  back again succesfully.
      call checkdump( swm, 'Channel test 3', status )

*  Check the ast_rate function works OK.
      call ast_setl( swm, 'Invert', .false., status )

      at(1) = 140.0
      at(2) = 50.0
      r1 = ast_rate( swm, at, 1, 1, status )

      at(1) = 50.0
      r2 = ast_rate( rm(1), at, 1, 1, status )

      if( abs( r1 - r2 ) .gt. abs( 1.0E-6*r2 )  ) call stopit( 151, r1,
     :                                                  status )

      at(1) = 140.0
      r1 = ast_rate( swm, at, 2, 2, status )

      at(1) = 50.0
      r2 = ast_rate( rm(1), at, 2, 2, status )

      if( abs( r1 - r2 ) .gt. abs( 1.0E-6*r2 )  ) call stopit( 152, r1,
     :                                                  status )

      at(1) = 140.0
      r1 = ast_rate( swm, at, 1, 2, status )

      at(1) = 50.0
      r2 = ast_rate( rm(1), at, 1, 2, status )

      if( abs( r1 - r2 ) .gt. abs( 1.0E-6*r2 )  ) call stopit( 153, r1,
     :                                                  status )

      at(1) = 140.0
      r1 = ast_rate( swm, at, 2, 1, status )

      at(1) = 50.0
      r2 = ast_rate( rm(1), at, 2, 1, status )

      if( abs( r1 - r2 ) .gt. abs( 1.0E-6*r2 )  ) call stopit( 154, r1,
     :                                                  status )




      mc = ast_tune( 'MemoryCaching', mc, status )
      oc = ast_tune( 'ObjectCaching', oc, status )
      call ast_end( status )

      call ast_activememory( 'testswitchmap' )
      call ast_flushmemory( 1 )

      if( status .eq. sai__ok ) then
         write(*,*) 'All SwitchMap tests passed'
      else
         write(*,*) 'SwitchMap tests failed'
      end if

      end


      subroutine stopit( i, r, status )
      implicit none
      include 'SAE_PAR'
      integer i, status
      double precision r
      if( status .eq. sai__ok ) then
         write( *,* ) 'Error ',i,': ',r
         status = sai__error
      end if
      end



      subroutine checkdump( obj, text, status )

      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      character text*(*)
      integer obj, status, next, end, ch, result, ll, overlap, ibuf,
     :        len1, len2
      external mysource, mysink
      character buf1*45000
      character buf2*45000

      common /ss1/ buf1
      common /ss3/ buf2
      common /ss2/ next, end, ll, ibuf

      if( status .ne. sai__ok ) return

*  Create a Channel which reads and writes to an internal string buffer.
      ch = ast_channel( mysource, mysink, ' ', status )

*  Write the supplied Object out to this Channel.
      ll = 160
      next = 1
      ibuf = 1
      if( ast_write( ch, obj, status ) .ne.1 ) then
         write(*,*) text
         call stopit( 1000, 1.0D0, status )
      end if
      len1 = next

*  Read an Object back from this Channel.
      next = 1
      result = ast_read( ch, status )
      if( result .eq. ast__null ) then
         write(*,*) text
         call stopit( 1001, 1.0D0, status )
      end if

*  Check that it is of the same class as the supplied object.
      if( ast_getc( result, 'Class', status ) .ne.
     :    ast_getc( obj, 'Class', status ) ) then
         write(*,*) text
         call stopit( 1002, 1.0D0, status )
      end if

*  Compare it to the suuplied object.
      call compare( obj, result, text, status )

      end




      subroutine compare( obj1, obj2, text, status )

      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      character text*(*)
      integer obj1, status, next, end, ch, result, ll, overlap, ibuf,
     :        len1, len2, obj2
      external mysource, mysink
      character buf1*45000
      character buf2*45000

      common /ss1/ buf1
      common /ss3/ buf2
      common /ss2/ next, end, ll, ibuf

      if( status .ne. sai__ok ) return

*  Create a Channel which reads and writes to an internal string buffer.
      ch = ast_channel( mysource, mysink, ' ', status )

*  Write the first supplied Object out to this Channel, using buf1
      ll = 160
      next = 1
      ibuf = 1
      if( ast_write( ch, obj1, status ) .ne.1 ) then
         write(*,*) text
         call stopit( 2000, 1.0D0, status )
      end if
      len1 = next

*  Write the second object out to the second buffer.
      ll = 160
      next = 1
      ibuf = 2
      if( ast_write( ch, obj2, status ) .ne.1 ) then
         write(*,*) text
         call stopit( 2001, 1.0D0, status )
      end if
      len2 = next

*  Compare the contents of the two buffers.
      if( buf1( : len1 ) .ne. buf2( : len2 ) ) then
         write(*,*) text
         write(*,*) len1, len2
         call ast_show( obj1, status )
         call ast_show( obj2, status )
         call stopit( 2002, 1.0D0, status )

      end if

      end

      subroutine mysource( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, ll, ibuf
      character buf1*45000
      character buf2*45000

      common /ss1/ buf1
      common /ss3/ buf2
      common /ss2/ next, end, ll, ibuf

      if( status .ne. sai__ok ) return

      if( next .ge. end ) then
         if( ibuf .eq. 1 ) then
            call ast_putline( buf1, -1, status )
         else
            call ast_putline( buf2, -1, status )
         endif
      else
         if( ibuf .eq. 1 ) then
            call ast_putline( buf1( next : ), ll, status )
         else
            call ast_putline( buf2( next : ), ll, status )
         endif
      endif

      next = next + ll

      end

      subroutine mysink( status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      integer status, next, end, f, l, ll, ibuf
      character line*1000
      character buf1*45000
      character buf2*45000

      common /ss1/ buf1
      common /ss3/ buf2
      common /ss2/ next, end, ll, ibuf

      if( status .ne. sai__ok ) return

      line = ' '
      call ast_getline( line, l, status )
      call chr_fandl( line( : l ), f, l )

      if( ibuf .eq. 1 ) then
         buf1( next : ) = line( f : l )
      else
         buf2( next : ) = line( f : l )
      end if

      l = l - f + 1

      if( next + ll - 1 .ge. 45000 ) then
         write(*,*) 'Buffer overflow in mysink!!'
         status = SAI__ERROR

      else if( l .gt. ll ) then
         write(*,*)
         if( ibuf .eq. 1 ) then
            write(*,*) buf1( next : next + l)
         else
            write(*,*) buf2( next : next + l)
         end if
         write(*,*) 'Line length ',l,' greater than ',ll
         write(*,*) 'Line overflow in mysink!!'
         status = SAI__ERROR
      else
         end = next + l
         if( ibuf .eq. 1 ) then
            buf1( end : next + ll - 1 ) = ' '
         else
            buf2( end : next + ll - 1 ) = ' '
         end if
      endif

      next = next + ll

      end




