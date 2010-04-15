      program testswitchmap
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'

      integer status, fs, fc, i
      character cards(10)*80, card*80

      status = sai__ok

*  Put a simple FITS-WCS header into a FitsChan.
      cards(1) = 'CRPIX1  = 45'
      cards(2) = 'CRPIX2  = 45'
      cards(3) = 'CRVAL1  = 45'
      cards(4) = 'CRVAL2  = 89.9'
      cards(5) = 'CDELT1  = -0.01'
      cards(6) = 'CDELT2  = 0.01'
      cards(7) = 'CTYPE1  = ''RA---TAN'''
      cards(8) = 'CTYPE2  = ''DEC--TAN'''

      fc = ast_fitschan( AST_NULL, AST_NULL, ' ', status )
      do i = 1, 8
         call ast_putfits( fc, cards(i), .false., status )
      end do

*  Indicate that the CTYPE1 card should be retained by ast_read.
      call ast_clear( fc, 'Card', status )
      if( ast_findfits( fc, 'CTYPE1', card, .FALSE., status ) ) then
         call ast_retainfits( fc, status )
      endif

*  Read a FrameSet from the FitsChan.
      call ast_clear( fc, 'Card', status )
      fs = ast_read( fc, status )
      if( fs .eq. AST__NULL ) then
         call stopit( 1, 'No FrameSet read from FitsChan', status )
      end if

*  Check the CTYPE1 card is still present in the FitsChan.
      call ast_clear( fc, 'Card', status )
      if( .not. ast_findfits( fc, 'CTYPE1', card, .FALSE.,
     :                        status ) ) then
         call stopit( 2, 'CTYPE1 has not been retained', status )
      end if

*  Check the CTYPE2 card is not present in the FitsChan.
      call ast_clear( fc, 'Card', status )
      if( ast_findfits( fc, 'CTYPE2', card, .FALSE., status ) ) then
         call stopit( 3, 'CTYPE2 has been retained', status )
      end if





*  Do it again, this time with an illegal value for CRPIX2. This will
*  cause ast_read to report an error.
      cards(1) = 'CRPIX1  = 45'
      cards(2) = 'CRPIX2  = ''fred'''
      cards(3) = 'CRVAL1  = 45'
      cards(4) = 'CRVAL2  = 89.9'
      cards(5) = 'CDELT1  = -0.01'
      cards(6) = 'CDELT2  = 0.01'
      cards(7) = 'CTYPE1  = ''RA---TAN'''
      cards(8) = 'CTYPE2  = ''DEC--TAN'''

      fc = ast_fitschan( AST_NULL, AST_NULL, ' ', status )
      do i = 1, 8
         call ast_putfits( fc, cards(i), .false., status )
      end do

*  Set the Clean attribute to true so that used cards are removed from the
*  FitsChan even if an error occurrs in astRead.
c      call ast_setl( fc, 'Clean', .true., status )

*  Indicate that the CTYPE1 card should be retained by ast_read.
      call ast_clear( fc, 'Card', status )
      if( ast_findfits( fc, 'CTYPE1', card, .FALSE., status ) ) then
         call ast_retainfits( fc, status )
      endif

*  Abort if an error has occurred.
      if( status .ne. sai__ok ) go to 999

*  Read a FrameSet from the FitsChan, deferring error reporting. Check an
*  error is reported by ast_read.
      call ast_clear( fc, 'Card', status )

      call err_begin( status )
      fs = ast_read( fc, status )

      if( fs .ne. AST__NULL ) then
         call stopit( 4, 'A FrameSet has been read from the FitsChan',
     :                status )

      else if( status .eq. sai__ok ) then
         call stopit( 5, 'No error has been reported by ast_read',
     :                status )

      else
         call err_annul( status )
      end if

      call err_end( status )

*  Check the CTYPE1 card is still present in the FitsChan.
      call ast_clear( fc, 'Card', status )
      if( .not. ast_findfits( fc, 'CTYPE1', card, .FALSE.,
     :                        status ) ) then
         call stopit( 6, 'CTYPE1 has not been retained', status )
      end if

*  Check the CTYPE2 card is also still present in the FitsChan (because
*  cards are not removed if an error is reported in ast_read unless the
*  Clean attribute is set true).
      call ast_clear( fc, 'Card', status )
      if( .not. ast_findfits( fc, 'CTYPE2', card, .FALSE.,
     :                        status ) ) then
         call stopit( 7, 'CTYPE2 has not been retained', status )
      end if





*  Do it again, again with an illegal value for CRPIX2, but this time
*  setting the Clean attribute true.
      cards(1) = 'CRPIX1  = 45'
      cards(2) = 'CRPIX2  = ''fred'''
      cards(3) = 'CRVAL1  = 45'
      cards(4) = 'CRVAL2  = 89.9'
      cards(5) = 'CDELT1  = -0.01'
      cards(6) = 'CDELT2  = 0.01'
      cards(7) = 'CTYPE1  = ''RA---TAN'''
      cards(8) = 'CTYPE2  = ''DEC--TAN'''

      fc = ast_fitschan( AST_NULL, AST_NULL, 'Clean=1', status )
      do i = 1, 8
         call ast_putfits( fc, cards(i), .false., status )
      end do

*  Indicate that the CTYPE1 card should be retained by ast_read.
      call ast_clear( fc, 'Card', status )
      if( ast_findfits( fc, 'CTYPE1', card, .FALSE., status ) ) then
         call ast_retainfits( fc, status )
      endif

*  Abort if an error has occurred.
      if( status .ne. sai__ok ) go to 999

*  Read a FrameSet from the FitsChan, deferring error reporting. Check an
*  error is reported by ast_read.
      call ast_clear( fc, 'Card', status )

      call err_begin( status )
      fs = ast_read( fc, status )

      if( fs .ne. AST__NULL ) then
         call stopit( 8, 'A FrameSet has been read from the FitsChan',
     :                status )

      else if( status .eq. sai__ok ) then
         call stopit( 9, 'No error has been reported by ast_read',
     :                status )

      else
         call err_annul( status )
      end if

      call err_end( status )

*  Check the CTYPE1 card is still present in the FitsChan (because of the
*  call to ast_retainfits).
      call ast_clear( fc, 'Card', status )
      if( .not. ast_findfits( fc, 'CTYPE1', card, .FALSE.,
     :                        status ) ) then
         call stopit( 10, 'CTYPE1 has not been retained', status )
      end if

*  Check the CTYPE2 card is no longer present in the FitsChan (because
*  the Clean attribute is set true).
      call ast_clear( fc, 'Card', status )
      if( ast_findfits( fc, 'CTYPE2', card, .FALSE., status ) ) then
         call stopit( 11, 'CTYPE2 has been retained', status )
      end if




 999  continue

      if( status .eq. sai__ok ) then
         write(*,*) 'All FitsChan tests passed'
      else
         write(*,*) 'FitsChan tests failed'
      end if

      end


      subroutine stopit( errnum, text, status )
      implicit none
      include 'SAE_PAR'
      character text*(*)
      integer errnum, status

      if( status .eq. sai__ok ) then
         status = sai__error
         call msg_seti( 'N', errnum )
         call msg_setc( 'T', text )
         call err_rep( ' ', 'Error ^N: ^T', status )
      end if

      end


