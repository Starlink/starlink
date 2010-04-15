

      SUBROUTINE GRA1_CONV(RADISP,WHATD,SIGMA,ZEROP,FLAG,RADIUS,
     :                     BRIGHT,STATUS)
*+
*  Name:
*     GRA1_CONV

*  Purpose:
*     Transforms the radius and pixel count value into the currently
*     required format. Returns an error flag if one of the values is
*     unusable. Is employed when displaying data points or data
*     fits.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRA1_CONV(RADISP,WHATD,SIGMA,ZEROP,FLAG,RADIUS,BRIGHT,STATUS)

*  Description:
*     Depending on the value of RADISP the subroutine converts the values
*     for radius and pixel count into the appropriate form. These may be
*     R, R**2, R**.5 or Log10(R) (in the case of R) and Log10(I)
*     I/SIGMA in the case of pixel count value.

*  Arguments:
*     RADISP = CHAR (Given)
*        Character variable denoting the format to be used for the radius
*        value. R=Linear Q=Quarter power, L=Logarithmic and S=Squared.
*     WHATD *(256) = CHARACTER  (Given)
*        Describes what parameter is to be displayed against
*        radius on the graphs.
*     BACK = REAL (Given)
*        The background count value for the image. Units counts.
*     SIGMA = REAL (Given)
*        The standard deviation the image background count. Units counts.
*     ZEROP = REAL (Given)
*        Zero point of the brightness scale. Units magnitudes.
*     FLAG = INTEGER (Returned)
*        Set to non-zero if the radius or brightness value could not
*        be transformed. eg log of a negative number.
*     RADIUS = REAL (Given and Returned)
*        The radius value to be transformed. Units arc seconds.
*     BRIGHT = REAL (Given and Returned)
*        The pixel count value to be transformed. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     30-Nov-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      CHARACTER RADISP *(256)         ! Defines the type of transformation
                                      ! to be applied to the radius value
      CHARACTER *(256) WHATD          ! What is to be displayed against
                                      ! the radius
      REAL SIGMA                      ! Standard deviation of the background count value
      REAL ZEROP                      ! The zero point of the magnitude scale

*  Arguments Returned:
      INTEGER FLAG                    ! Indicates whether or not the
                                      ! transformations were successful
*  Arguments Given and Returned:
      REAL BRIGHT                     ! The pixel count value to be
                                      ! transformed
      REAL RADIUS                     ! The radius value to be transformed

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the transformation successful? flag.
      FLAG=0

*   Perform pixel count transformations.

*   Modify the incoming Y axis values if required.
*   Not required when WHATD = E, P, C, X, Y or the Fourier descriptors..

*   In terms of sigma compared to sky.
      IF (WHATD.EQ.'B') BRIGHT=BRIGHT/SIGMA

*   In terms of surface brightness.
      IF (WHATD.EQ.'S') THEN
         IF (BRIGHT.GT.0.0) THEN
            BRIGHT=ZEROP-2.5*LOG10(BRIGHT)
         ELSE
*         Avoid calculating Log of zero or a negative number.
            FLAG=1
         END IF
      END IF

*   Perform radius transformations.

*   Linear radius transformation.
      IF (RADISP.EQ.'R') RADIUS=RADIUS

*   Quarter power radius transformation.
      IF (RADISP.EQ.'Q') RADIUS=RADIUS**(0.25)

*   Logarithmic radius transform.
      IF (RADISP.EQ.'L') THEN
         IF (RADIUS.GT.0.0) THEN
            RADIUS=LOG10(RADIUS)
         ELSE
*         Avoid calculation Log of zero or a negative number.
            FLAG=-1
         END IF
      END IF

*   Squared radius transformation.
      IF (RADISP.EQ.'S') RADIUS=RADIUS*RADIUS

 9999 CONTINUE

      END


      SUBROUTINE SEC1_CONV(RADISP,SURF,BACK,SIGMA,ZEROP,FLAG,RADIUS,
     :                     BRIGHT,STATUS)
*+
*  Name:
*     SEC1_CONV

*  Purpose:
*     Transforms the radius and pixel count value into the currently
*     required format. Returns an error flag if one of the values is
*     unusable. Is employed when displaying data points or data
*     fits.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SEC1_CONV(RADISP,SURF,BACK,SIGMA,ZEROP,FLAG,RADIUS,BRIGHT,STATUS)

*  Description:
*     Depending on the value of RADISP the subroutine converts the values
*     for radius and pixel count into the appropriate form. These may be
*     R, R**2, R**.5 or Log10(R) (in the case of R) and Log10(I-BACK)
*     (I-BACK)/SIGMA in the case of pixel count value.

*  Arguments:
*     RADISP = CHAR (Given)
*        Character variable denoting the format to be used for the radius
*        value. R=Linear Q=Quarter power, L=Logarithmic and S=Squared.
*     SURF = LOGICAL (Given)
*        Logical variable denoting the format to be used for the brightness.
*        FALSE=Sigma and TRUE=Suyrface brightness.
*     BACK = REAL (Given)
*        The background count value for the image. Units counts.
*     SIGMA = REAL (Given)
*        The standard deviation of the image background. Units counts.
*     ZEROP = REAL (Given)
*        Zero point of the brightness scale. Units magnitudes.
*     FLAG = INTEGER (Returned)
*        Set to non-zero if the radius or brightness value could not
*        be transformed. eg log of a negative number.
*     RADIUS = REAL (Given and Returned)
*        The radius value to be transformed. Units arc seconds.
*     BRIGHT = REAL (Given and Returned)
*        The pixel count value to be transformed. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     30-Nov-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      CHARACTER RADISP *(256)         ! Defines the type of transformation
                                      ! to be applied to the radius value
      LOGICAL SURF                    ! Denotes how the pixel count is
                                      ! to be transformed i.e. surface brightness
                                      ! or in terms of sky
      REAL BACK                       ! Pixel count value for the background
      REAL SIGMA                       ! Standard deviation of the background
                                      ! count value
      REAL ZEROP                      ! The zero point of the magnitude scale

*  Arguments Returned:
      INTEGER FLAG                    ! Indicates whether or not the
                                      ! transformations were successful
*  Arguments Given and Returned:
      REAL BRIGHT                     ! The pixel count value to be
                                      ! transformed
      REAL RADIUS                     ! The radius value to be transformed

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the transformation successful? flag.
      FLAG=0

*   Perform pixel count transformations.

*   Convert the pixel values to sigma or surface brightness.
      IF (.NOT.SURF) THEN
         BRIGHT=(BRIGHT-BACK)/SIGMA
      ELSE
         BRIGHT=(BRIGHT-BACK)
         IF (BRIGHT.GT.0.0) THEN
            BRIGHT=ZEROP-2.5*LOG10(BRIGHT)
         ELSE
*         Avoid calculating Log of zero or a negative number.
            FLAG=1
         END IF
      END IF

*   Perform radius transformations.

*   Linear radius transformation.
      IF (RADISP.EQ.'R') RADIUS=RADIUS

*   Quarter power radius transformation.
      IF (RADISP.EQ.'Q') RADIUS=RADIUS**(0.25)

*   Logarithmic radius transform.
      IF (RADISP.EQ.'L') THEN
         IF (RADIUS.GT.0.0) THEN
            RADIUS=LOG10(RADIUS)
         ELSE
*         Avoid calculation Log of zero or a negative number.
            FLAG=-1
         END IF
      END IF

*   Squared radius transformation.
      IF (RADISP.EQ.'S') RADIUS=RADIUS*RADIUS

 9999 CONTINUE

      END
