*+  PSS_PPIX - Return psf radius in pixels
      INTEGER FUNCTION PSS_PPIX( R )
*
*    Description :
*
*     Returns psf box half-width as a function of image position.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      1 Apr 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      REAL                     R                    ! Radius in radians
*
*    Local variables :
*
      REAL                     AR                   ! Local copy
      REAL                     PPIX                 ! result
*-

      IF ( PSF_CONSTANT ) THEN
        PPIX = PSF_PPS(1)
      ELSE
        AR = ABS(R)
        IF ( AR .LT. PSF_PPR(1) ) THEN
          PPIX = PSF_PPS(1)
        ELSE IF ( AR .GT. PSF_PPR(3) ) THEN
          PPIX = PSF_PPS(3)
        ELSE IF ( AR .GT. PSF_PPR(2) ) THEN
          PPIX = PSF_PPS(2) + NINT((AR-PSF_PPR(2))*
     :            (PSF_PPS(3)-PSF_PPS(2))/(PSF_PPR(3)-PSF_PPR(2)))
        ELSE
          PPIX = PSF_PPS(1) + NINT((AR-PSF_PPR(1))*
     :            (PSF_PPS(2)-PSF_PPS(1))/(PSF_PPR(2)-PSF_PPR(1)))
        END IF
      END IF

*    User psf area
      PSF_UDIMS(1) = PPIX*2 + PSF_BORDER*2 + 1
      PSF_UDIMS(2) = PSF_UDIMS(1)

*    Return value
      PSS_PPIX = PPIX

      END
