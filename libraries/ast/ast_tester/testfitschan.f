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
         call stopit( 'No FrameSet read from FitsChan', status )
      end if

*  Check the CTYPE1 card is still present in the FitsChan.
      call ast_clear( fc, 'Card', status )
      if( .not. ast_findfits( fc, 'CTYPE1', card, .FALSE., 
     :                        status ) ) then
         call stopit( 'CTYPE1 has not been retained', status )
      end if

*  Check the CTYPE2 card is not present in the FitsChan.
      call ast_clear( fc, 'Card', status )
      if( ast_findfits( fc, 'CTYPE2', card, .FALSE., status ) ) then
         call stopit( 'CTYPE2 has been retained', status )
      end if

      if( status .eq. sai__ok ) then
         write(*,*) 'All FitsChan tests passed'
      else
         write(*,*) 'FitsChan tests failed'
      end if

      end


      subroutine stopit( text, status )
      implicit none
      include 'SAE_PAR'
      character text*(*)
      integer status

      if( status .eq. sai__ok ) then
         status = sai__error
         call err_rep( ' ', text, status )
      end if

      end


