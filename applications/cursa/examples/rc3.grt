!+
! Graphics translation file for the CADC version of the Third Reference
! Catalogue of Bright Galaxies (RC3).
!
! Author:
!   ACD: A.C. Davenhall (Edinburgh)
!
! History:
!   8/5/01 (ACD): Original version.
!-

! The galaxies are plotted as circles whose radius corresponds to the
! major axis of the 25.0 B magnitude/square arcsec isophote.
!
! Note that the columns returned do not contain enough information to
! plot the galaxies as ellipses.

SYMBOL = opencircle   ! Plot galaxies sources as open squares.
COLOUR = red
UNITS  = arcmin       ! Symbol size expressed in minutes of arc.
LABEL  = ID

!
! The circle radius is computed in minutes of arc.  It is calculated from
! column LOG_D25.  The paper version of the RC3 gives the following
! description of this column: 'mean decimal logarithm of the apparent
! isophotal diameter measured at a reduced surface brightness of level
! 25.0 B magnitudes/square arcsec.  The unit of the diameter is 0.1
! arcmin to avoid negative entries'.  The values tabulated in the CADC
! version of RC3 have been scaled by a factor or 100.
!
! Thus to compute the radius: the tabulated value is de-scaled, the
! anti-logarithm taken and the value divided by 20 (which last combines
! converting to arcmin and converting the diameter into the radius).
!
! If a LOG_D25 is not available for the galaxy (which in the CADC version
! is indicated by a negative value) then a radius of 0.5 minutes of arc
! is used.

IF LOG_D25 > 0.0 
  SIZE1 = (10.0**(LOG_D25/100.0)) / 20.0
ELSE
  SIZE1 = 0.5
END IF
