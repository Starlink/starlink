!+
! Graphics translation file for catalogue extracted from the SuperCOSMOS
! surveys.
!
! All the objects are plotted as ellipses.  However the colour used for
! each object depends on its classification.
!
! Author:
!   ACD: A.C. Davenhall (Edinburgh)
!
! History:
!   24/5/01 (ACD): Original version.
!-

SYMBOL = openellipse  ! Plot objects as open ellipses.

!
! Set the colour depending on the object classification.

IF CLASS = 1          ! galaxies,
   COLOUR = red
ELSE IF CLASS = 2     ! stars,
   COLOUR = blue
ELSE IF CLASS = 3     ! unclassifiable objects,
   COLOUR = green
ELSE IF CLASS = 4     ! noise.
   COLOUR = yellow
END IF

UNITS  = arcsec       ! Symbol size expressed in seconds of arc.

!
! Calculate the semi-major and semi-minor axes in seconds of arc from
! the tabulated values.  The position angle is used unaltered.

SIZE1  =  A_I * 67.1 / 1.0E5
SIZE2  =  B_I * 67.1 / 1.0E5
SIZE3  =  PA
