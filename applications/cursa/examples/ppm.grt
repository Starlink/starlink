!+
! Graphics translation file for the LEDAS version of the PPM.
!
! Author:
!   ACD: A.C. Davenhall (Edinburgh)
!
! History:
!   7/5/01 (ACD): Original version.
!-

SYMBOL = opencircle     ! Plot stars  as open circles.
COLOUR = default
UNITS  = fraction       ! Symbol size expressed as fraction of X range.

!
! Determine the symbol size by scaling the magnitudes between brightest
! and faintest stars in the target list.

SIZE1  =  ascale(VMAG, 5.0D-2, 1.0D-2)
