!+
! Graphics translation file for catalogue extracted from the SuperCOSMOS
! surveys.
!
! All the objects are plotted as ellipses drawn in the default colour.
!
! Author:
!   ACD: A.C. Davenhall (Edinburgh)
!
! History:
!   24/5/01 (ACD): Original version.
!-

SYMBOL = openellipse  ! Plot objects as open ellipses.
COLOUR = default      ! Use the default colour.
UNITS  = arcsec       ! Symbol size expressed in seconds of arc.

!
! Calculate the semi-major and semi-minor axes in seconds of arc from
! the tabulated values.  The position angle is used unaltered.

SIZE1  =  A_I * 67.1 / 1.0E5
SIZE2  =  B_I * 67.1 / 1.0E5
SIZE3  =  PA
