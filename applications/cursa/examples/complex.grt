!+
! More complicated graphics translation file.
!
! This file is suitable for use with target lists extracted from
! the version of the Bonner Durchmusterung available on-line at the
! Department of Physics and Astronomy, University of Leicester.
!
! Stars brighter than magnitude 7.5 are plotted as blue open stars.
! Fainter stars are plotted as red circles.  If the star is between
! magnitudes 7.5 and 9.0 the circle is solid, otherwise it is open.
! In all cases the size is scaled according to magnitude between the
! fixed range 7.5 - 10.0.
!
! A.C. Davenhall (Edinburgh) 10/6/97.
!-

IF VMAG < 7.5
  SYMBOL = openstar       ! Open star,
  COLOUR = blue           ! coloured blue.

ELSE IF VMAG >= 7.5  AND  VMAG < 9.0
  SYMBOL = filledcircle   ! filled circle,
  COLOUR = red            ! coloured red.

ELSE 
  SYMBOL = opencircle     ! open circle,
  COLOUR = red            ! coloured red.

END IF

UNITS  = fraction         ! Symbol size expressed as fraction of X range.

!
! Determine the symbol size by scaling the magnitudes between the
! fixed range 7.5 - 10.0  VMAG is the magnitude column in the Bonner
! Durchmusterung.  Note how the minimum and maximum symbol sizes are
! flipped to accommodate magnitudes increasing 'the wrong way round'.

SIZE1  = scale(VMAG, 7.5D0, 1.0D1, 5.0D-2, 1.0D-2)
