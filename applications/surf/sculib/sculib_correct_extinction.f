      SUBROUTINE SCULIB_CORRECT_EXTINCTION (SIZE_BOL, N_BOL, BOL_DATA,
     :  BOL_VARIANCE, BOL_RA, BOL_DEC, LST, LAT_OBS,
     :  TAUZ, STATUS)
*+
*  Name:
*     SCULIB_CORRECT_EXTINCTION

*  Purpose:
*     correct bolometers for sky opacity

*  Description:
*     This routine corrects bolometer data for the effect of sky opacity.
*     It does this by calculating the airmass of the point that each
*     bolometer was looking at, then multiplying the data by
*     exp (airmass * TAUZ). Bolometers with bad data quality will be
*     ignored.

*  Invocation:
*     CALL SCULIB_CORRECT_EXTINCTION (SIZE_BOL, N_BOL, BOL_DATA,
*    :  BOL_VARIANCE, BOL_RA, BOL_DEC, LST, LAT_OBS,
*    :  TAUZ, STATUS)

*  Arguments:
*     SIZE_BOL                       = INTEGER (Given)
*           dimension of arrays
*     N_BOL                          = INTEGER (Given)
*           used size of arrays
*     BOL_DATA (SIZE_BOL)            = REAL (Given and returned)
*           bolometer data
*     BOL_VARIANCE (SIZE_BOL)        = REAL (Given and returned)
*           variance on BOL_DATA
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


*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)
*     Tim Jenness (JAC, Hawaii)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999,2005 Particle Physics
*     and Astronomy Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     2-AUG-1995: original version.
*     $Log$
*     Revision 1.7  2005/09/22 01:54:43  timj
*     pull constants out of loops and make sure that we minimize type conversion by making SCULIB_AIRMASS use double precision
*
*     Revision 1.6  1999/08/19 03:37:06  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.5  1999/08/03 19:34:52  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.4  1998/07/23 23:10:41  timj
*     Check for bad pixels
*
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER          SIZE_BOL
      INTEGER          N_BOL
      DOUBLE PRECISION BOL_RA (SIZE_BOL)
      DOUBLE PRECISION BOL_DEC (SIZE_BOL)
      DOUBLE PRECISION LST
      DOUBLE PRECISION LAT_OBS
      REAL             TAUZ

*  Arguments Given & Returned:
      REAL             BOL_DATA (SIZE_BOL)
      REAL             BOL_VARIANCE (SIZE_BOL)

*  Arguments Returned:

*  Status:
      INTEGER          STATUS

*  External references:

*  Global variables:

*  Local Constants:
      DOUBLE PRECISION PI                    !
      PARAMETER (PI = 3.14159265359D0)
      DOUBLE PRECISION PIBY2
      PARAMETER ( PIBY2 = PI / 2.0D0 )

*  Local variables:
      DOUBLE PRECISION AIRMASS               !
      INTEGER          BOL                   ! bolometer index in DO loop
      REAL             CORRECTION            ! correction for sky opacity
      DOUBLE PRECISION COS_LAT_OBS           ! cos(lat_obs)
      DOUBLE PRECISION HOUR_ANGLE            ! hour angle (radians)
      DOUBLE PRECISION SIN_E                 ! sin(elevation)
      DOUBLE PRECISION SIN_LAT_OBS           ! sin(lat_obs)
      DOUBLE PRECISION Z                     ! zenith disatance (radians)

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      IF (N_BOL .GT. 0) THEN

         COS_LAT_OBS = COS( LAT_OBS )
         SIN_LAT_OBS = SIN( LAT_OBS )

         DO BOL = 1, N_BOL

*  calculate the zenith distance and airmass of the bolometer

            HOUR_ANGLE = LST - BOL_RA (BOL)
            SIN_E = SIN_LAT_OBS * SIN (BOL_DEC(BOL)) +
     :           COS_LAT_OBS * COS(BOL_DEC(BOL)) * COS(HOUR_ANGLE)
            Z = PIBY2 - ASIN(SIN_E)

            CALL SCULIB_AIRMASS (Z, AIRMASS, STATUS)

*  and the correction for the extinction

            CORRECTION = EXP (REAL(AIRMASS)*TAUZ)

*  correct the data

            IF (BOL_DATA(BOL) .NE. VAL__BADR)
     :           BOL_DATA(BOL) = BOL_DATA(BOL) * CORRECTION
            IF (BOL_VARIANCE(BOL) .NE. VAL__BADR)
     :           BOL_VARIANCE(BOL) = BOL_VARIANCE(BOL) * CORRECTION**2
         END DO

      END IF

      END
