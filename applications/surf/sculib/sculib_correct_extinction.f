*+  SCULIB_CORRECT_EXTINCTION - correct bolometers for sky opacity
      SUBROUTINE SCULIB_CORRECT_EXTINCTION (SIZE_BOL, N_BOL, BOL_DATA,
     :  BOL_VARIANCE, BOL_QUALITY, BOL_RA, BOL_DEC, LST, LAT_OBS,
     :  TAUZ, STATUS)
*    Description :
*     This routine corrects bolometer data for the effect of sky opacity.
*     It does this by calculating the airmass of the point that each
*     bolometer was looking at, then multiplying the data by 
*     exp (airmass * TAUZ). Bolometers with bad data quality will be
*     ignored.
*    Invocation :
*     CALL SCULIB_CORRECT_EXTINCTION (SIZE_BOL, N_BOL, BOL_DATA,
*    :  BOL_VARIANCE, BOL_QUALITY, BOL_RA, BOL_DEC, LST, LAT_OBS,
*    :  TAUZ, STATUS)
*    Parameters :
*     SIZE_BOL                       = INTEGER (Given)
*           dimension of arrays
*     N_BOL                          = INTEGER (Given)
*           used size of arrays
*     BOL_DATA (SIZE_BOL)            = REAL (Given and returned)
*           bolometer data
*     BOL_VARIANCE (SIZE_BOL)        = REAL (Given and returned)
*           variance on BOL_DATA
*     BOL_QUALITY (SIZE_BOL)         = INTEGER (Given)
*           quality on BOL_DATA
*     BOL_RA (SIZE_BOL)              = DOUBLE PRECISION (Given)
*           apparent RA of bolometer (radians)
*     BOL_DEC (SIZE_BOL)             = DOUBLE PRECISION (Given)
*           apparent dec of bolometer (radians)
*     LST                            = DOUBLE PRECISION (Given)
*           sidereal time (radians)
*     LAT_OBS                        = DOUBLE PRECISION (Given)
*           latitude of observatory (radians)
*     TAUZ                           = REAL (Given)
*           the zenith sky opacity
*     STATUS                         = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (jfl@roe.ac.uk)
*    History :
*     $Id$
*     2-AUG-1995: original version.
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER          SIZE_BOL
      INTEGER          N_BOL
      INTEGER          BOL_QUALITY (SIZE_BOL)
      DOUBLE PRECISION BOL_RA (SIZE_BOL)
      DOUBLE PRECISION BOL_DEC (SIZE_BOL)
      DOUBLE PRECISION LST
      DOUBLE PRECISION LAT_OBS
      REAL             TAUZ
*    Import-Export :
      REAL             BOL_DATA (SIZE_BOL)
      REAL             BOL_VARIANCE (SIZE_BOL)
*    Export :
*    Status :
      INTEGER          STATUS
*    External references :
*    Global variables :
*    Local Constants :
      DOUBLE PRECISION PI                    !
      PARAMETER (PI = 3.14159265359D0)
*    Local variables :
      REAL             AIRMASS               !
      INTEGER          BOL                   ! bolometer index in DO loop
      REAL             CORRECTION            ! correction for sky opacity
      DOUBLE PRECISION HOUR_ANGLE            ! hour angle (radians)
      DOUBLE PRECISION SIN_E                 ! sin(elevation)
      DOUBLE PRECISION Z                     ! zenith disatance (radians)
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

      IF (N_BOL .GT. 0) THEN

         DO BOL = 1, N_BOL
            IF (BOL_QUALITY(BOL) .EQ. 0) THEN

*  calculate the zenith distance and airmass of the bolometer

               HOUR_ANGLE = LST - BOL_RA (BOL)
               SIN_E = SIN (LAT_OBS) * SIN (BOL_DEC(BOL)) +
     :           COS(LAT_OBS) * COS(BOL_DEC(BOL)) * COS(HOUR_ANGLE)
               Z = PI/2.0D0 - ASIN(SIN_E)

               CALL SCULIB_AIRMASS (REAL(Z), AIRMASS, STATUS)

*  and the correction for the extinction

               CORRECTION = EXP (AIRMASS*TAUZ)

*  correct the data

               BOL_DATA(BOL) = BOL_DATA(BOL) * CORRECTION
               BOL_VARIANCE(BOL) = BOL_VARIANCE(BOL) * CORRECTION**2
            END IF
         END DO

      END IF

      END
