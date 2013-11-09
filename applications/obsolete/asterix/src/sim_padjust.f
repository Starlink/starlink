*+  SIM_PADJUST - Copy a section of psf array
      SUBROUTINE SIM_PADJUST( W, IN )
*
*    Description :
*
*     Adds values to edge of psf to array to normalise to unity
*
*    History :
*
*     12 Mar 92 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      INTEGER    W                            ! Array full width
*
*    Import / Export :
*
      REAL       IN(-W/2:W/2,-W/2:W/2)        ! Psf array
*
*    Local variables :
*
      REAL       DEF                          ! Psf deficit per pixel
      REAL       PSUM                         ! Psf sum over input region

      INTEGER    I,J                          ! Loop over psf data
*-

*    Find sum of psf values
      PSUM = 0.0
      DO J = -W/2, W/2
        DO I = -W/2, W/2
          PSUM = PSUM + IN(I,J)
        END DO
      END DO

*    Find deficit averaged over all border pixels
      DEF = (1.0-PSUM) / REAL(4*(W-1))

*    Add a small amount to each border pixel to bring sum up to unity
      DO I = -W/2+1, W/2
        IN(I,-W/2) = IN(I,-W/2) + DEF		! Along bottom, L to R
        IN(+W/2,I) = IN(+W/2,I) + DEF		! Up right
        IN(-I,+W/2) = IN(-I,+W/2) + DEF		! Along top, R to L
        IN(-W/2,I) = IN(-W/2,I) + DEF		! Down left
      END DO

      END
