      SUBROUTINE SCULIB_APPARENT_2_MP(RA_APP, DEC_APP, OUT_COORDS,
     :     LST, MJD, LAT_OBS, LONG, LAT,
     :     STATUS )
*+
*  Name:
*     SCULIB_APPARENT_2_MP

*  Purpose:
*     Calculate mean place from a given apparent RA,Dec

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_APPARENT_2_MP(RA_APP, DEC_APP, OUT_COORDS,
*    :     LST, MJD, LAT_OBS, LONG, LAT,
*    :     STATUS )

*  Description:
*     This routine takes the apparent RA,Dec as input and converts
*     them to the position in a specified output coordinate system.
*     Allowed OUTPUT_COORDS are AZ, RJ, RB, GA, EQ.
*     This is the reverse of SCULIB_CALC_APPARENT
*

*  Arguments:
*     RA_APP                 = DOUBLE PRECISION (Given)
*           Apparent RA of point at date (radians)
*     DEC_APP                = DOUBLE PRECISION (Given)
*           Apparent Dec
*     OUT_COORDS             = CHARACTER * (*) (Given)
*     LST                    = DOUBLE PRECISION (Given)
*           LST for requested coordinates (for AZ and HA)
*     MJD                    = DOUBLE PRECISION (Given)
*           Modified Julian date of observation
*     LAT_OBS                = DOUBLE PRECISION (Given)
*           Latitude of observatory in radians.
*           For JCMT this value is 3.46026051751D-1
*     LONG                   = DOUBLE PRECISION (Returned)
*           longitude of centre in input coord system (radians)
*     LAT                    = DOUBLE PRECISION (Returned)
*           latitude of centre in input coord system (radians)
*     STATUS                 = INTEGER (Given and returned)
*           Global status

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)
*     T.Jenness (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.6  1999/08/19 03:36:59  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.5  1999/08/03 19:34:43  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.4  1999/07/14 20:13:26  timj
*     Pass LAT_OBS into SCULIB_CALC_APPARENT rather than having it as
*     a parameter.
*
*     Revision 1.3  1999/07/13 06:27:03  timj
*     Pass LAT_OBS in as a argument
*
*     Revision 1.2  1997/11/19 02:24:36  timj
*     Fix calculation of Azimuth.
*
*     Revision 1.1  1997/11/04 23:20:48  timj
*     Initial revision
*
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      DOUBLE PRECISION RA_APP
      DOUBLE PRECISION DEC_APP
      CHARACTER*(*)    OUT_COORDS
      DOUBLE PRECISION LAT_OBS
      DOUBLE PRECISION LST
      DOUBLE PRECISION MJD

*  Arguments Returned:
      DOUBLE PRECISION LAT
      DOUBLE PRECISION LONG

*  Status:
      INTEGER          STATUS

*  Local Constants:

*  Local Variables:
      DOUBLE PRECISION COS_LONG             ! cos(az)
      DOUBLE PRECISION DEC_2000             ! Declination (J2000)
      DOUBLE PRECISION HA                   ! Hour angle
      DOUBLE PRECISION PM1                  ! Proper motion
      DOUBLE PRECISION PM2                  ! Proper motion
      DOUBLE PRECISION RA_2000              ! RA (J2000)
      DOUBLE PRECISION SIN_LAT              ! sin(elevation)
      DOUBLE PRECISION SIN_LONG             ! sin(az)

*.

      IF (STATUS .NE. SAI__OK) RETURN

      CALL CHR_UCASE (OUT_COORDS)

*     Handle each coord type in turn

      IF ((OUT_COORDS .EQ. 'RB') .OR.
     :     (OUT_COORDS .EQ. 'RJ') .OR.
     :     (OUT_COORDS .EQ. 'GA') .OR.
     :     (OUT_COORDS .EQ. 'EQ')) THEN

*     Convert to RJ as a first step
         CALL SLA_AMP(RA_APP, DEC_APP, MJD, 2000.0D0, RA_2000, DEC_2000)

*     RJ -> RJ
         IF (OUT_COORDS .EQ. 'RJ') THEN

            LONG = RA_2000
            LAT  = DEC_2000

*     RJ->RB
         ELSE IF (OUT_COORDS .EQ. 'RB') THEN

            CALL SLA_FK54Z(RA_2000, DEC_2000, 1950.0D0, LONG, LAT,
     :           PM1, PM2)

*     RJ->GA
         ELSE IF (OUT_COORDS .EQ. 'GA') THEN

            CALL SLA_EQGAL(RA_2000, DEC_2000, LONG, LAT)

*     RJ->EQ
         ELSE IF (OUT_COORDS .EQ. 'EQ') THEN

            CALL SLA_EQECL(RA_2000, DEC_2000, MJD, LONG, LAT)

         END IF

      ELSE IF (OUT_COORDS .EQ. 'AZ') THEN

         HA = LST - RA_APP
         SIN_LAT = SIN(LAT_OBS) * SIN(DEC_APP) +
     :        COS(LAT_OBS) * COS(DEC_APP) * COS(HA)

         LAT = ASIN(SIN_LAT)

*     Calculate the Az from COS and SIN
*     Ignore the cosE term since it is on the denominator of
*     both expressions

         SIN_LONG = -COS(DEC_APP) * SIN(HA)
         COS_LONG = (SIN(DEC_APP) - SIN(LAT_OBS) * SIN_LAT)
     :        / COS(LAT_OBS)

         LONG = ATAN2(SIN_LONG, COS_LONG)

      ELSE IF (OUT_COORDS .EQ. 'HA') THEN

         LONG = LST - RA_APP
         LAT  = DEC_APP

      ELSE IF (OUT_COORDS .EQ. 'RD') THEN

         LONG = RA_APP
         LAT  = DEC_APP

      ELSE

         STATUS = SAI__ERROR
         CALL MSG_SETC('IN', OUT_COORDS)
         CALL ERR_REP(' ','SCULIB_APPARENT_2_MP: Output coordinate '//
     :        '^IN not supported (only RB,RJ,GA,EQ,AZ,RD,HA)', STATUS)

      END IF


      END
