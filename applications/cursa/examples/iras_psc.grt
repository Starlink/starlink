!+
! Graphics translation file for the ESO version of the IRAS PSC.
!
! Author:
!   ACD: A.C. Davenhall (Edinburgh)
!
! History:
!   7/5/01 (ACD): Original version.
!-

SYMBOL = opentriangle   ! Plot IRAS sources as open triangles.
COLOUR = blue
UNITS  = fraction       ! Symbol size expressed as fraction of X range.
LABEL  = Id

!
! Determine the symbol size by scaling the flux between brightest
! and faintest sources in the target list.

SIZE1  =  ascale(F25, 1.0D-2, 5.0D-2)
