!+
! Simple graphics translation file.
!
! This file is suitable for use with target lists extracted from
! the version of the Bonner Durchmusterung available on-line at the
! Department of Physics and Astronomy, University of Leicester.
!
! All the stars are plotted as red filled circles scaled according
! to magnitude.
!
! A.C. Davenhall (Edinburgh) 10/6/97.
!-

SYMBOL = filledcircle   ! Plot the stars as filled circles,
COLOUR = red            ! coloured red.

UNITS  = fraction       ! Symbol size expressed as fraction of X range.

!
! Determine the symbol size by scaling the magnitudes between brightest
! and faintest stars in the target list.  VMAG is the magnitude column
! in the Bonner Durchmusterung.  Note how the minimum and maximum
! symbol sizes are flipped to accommodate magnitudes increasing 'the wrong
! way round'.

SIZE1  =  ascale(VMAG, 5.0D-2, 1.0D-2)
