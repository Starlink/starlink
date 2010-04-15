      SUBROUTINE SCULIB_CALC_OUTPUT_COORDS (RA_APP, DEC_APP, MJD,
     :  OUTPUT_COORDS, LONG, LAT, STATUS)
*+
*  Name:
*     SCULIB_CALC_OUTPUT_COORDS

*  Purpose:
*     calculate output coords of map centre and angle
*     of output coord system N relative to apparent N

*  Description:
*     This routine takes the apparent centre coords at the time of the
*     observation and converts them to the output coordinate system. In
*     addition, the angle between the north direction in the output
*     coordinate frame and that in the apparent frame is calculated
*     (measured anticlockwise from output north, in radians).


*  Invocation:
*     CALL SCULIB_CALC_OUTPUT_COORDS (RA_APP, DEC_APP, MJD,
*    :  OUTPUT_COORDS, LONG, LAT, STATUS)

*  Arguments:
*     RA_APP                 = DOUBLE PRECISION (Given)
*           apparent RA of map centre on MJD (radians)
*     DEC_APP                = DOUBLE PRECISION (Given)
*           apparent declination of map centre on MJD (radians)
*     MJD                    = DOUBLE PRECISION (Given)
*           U1 of observation expressed as modified Julian day
*     OUTPUT_COORDS          = CHARACTER*(*) (Given)
*           output coordinated system; RB, RJ, RD, GA or EQ
*     LONG                   = DOUBLE PRECISION (Returned)
*           longitude of map centre in output coord system (radians)
*     LAT                    = DOUBLE PRECISION (Returned)
*           latitude of map centre in output coord system (radians)
*     STATUS                 = INTEGER (Given and returned)
*           global status

*  Notes:
*     SLA routines are used to perform the coordinate conversions.


*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:

*  History:
*     $Id$
*     14-AUG-1995: original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      DOUBLE PRECISION RA_APP
      DOUBLE PRECISION DEC_APP
      DOUBLE PRECISION MJD
      CHARACTER*(*)    OUTPUT_COORDS

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION LONG
      DOUBLE PRECISION LAT

*  Status:
      INTEGER          STATUS

*  External references:

*  Global variables:

*  Local Constants:
      DOUBLE PRECISION DPI
      PARAMETER (DPI = 3.14159265359D0)
      DOUBLE PRECISION DPI2
      PARAMETER (DPI2 = DPI / 2.0D0)

*  Local variables:
      CHARACTER*10     COORD_TYPE             ! upper case version of
                                              ! OUTPUT_COORDS
      DOUBLE PRECISION DTEMP                  ! scratch double
      DOUBLE PRECISION RA_2000, DEC_2000      ! RA,Dec J2000 FK5 coords of point
      DOUBLE PRECISION RA_N_2000, DEC_N_2000  ! RA,Dec J2000 FK5 coords of N
                                              ! pole of output system
      DOUBLE PRECISION RA_N_APP, DEC_N_APP    ! apparent RA,Dec of N pole of
                                              ! output system

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      COORD_TYPE = OUTPUT_COORDS
      CALL CHR_UCASE (COORD_TYPE)

      IF (COORD_TYPE .EQ. 'RB') THEN

*  convert map centre

         CALL SLA_AMP (RA_APP, DEC_APP, MJD, 2000.0D0, RA_2000,
     :     DEC_2000)
         CALL SLA_FK54Z (RA_2000, DEC_2000, 1950.0D0, LONG, LAT,
     :     DTEMP, DTEMP)

*  calculate apparent RA,Dec of N pole

         CALL SLA_FK45Z (0.0, DPI2, 1950.0D0, RA_N_2000,
     :     DEC_N_2000)
         CALL SLA_MAP (RA_N_2000, DEC_N_2000, 0.0D0, 0.0D0, 0.0D0,
     :     0.0D0, 2000.0D0, MJD, RA_N_APP, DEC_N_APP)

      ELSE IF (COORD_TYPE .EQ. 'RJ') THEN
         CALL SLA_AMP (RA_APP, DEC_APP, MJD, 2000.0D0, LONG, LAT)

         CALL SLA_MAP (0.0D0, DPI2, 0.0D0, 0.0D0, 0.0D0, 0.0D0,
     :     2000.0D0, MJD, RA_N_APP, DEC_N_APP)

      ELSE IF (COORD_TYPE .EQ. 'RD') THEN
         LONG = RA_APP
         LAT = DEC_APP

      ELSE IF (COORD_TYPE .EQ. 'GA') THEN
         CALL SLA_AMP (RA_APP, DEC_APP, MJD, 2000.0D0, RA_2000,
     :     DEC_2000)
         CALL SLA_EQGAL (RA_2000, DEC_2000, LONG, LAT)

         CALL SLA_GALEQ (0.0D0, DPI2, RA_N_2000, DEC_N_2000)
         CALL SLA_MAP (RA_N_2000, DEC_N_2000, 0.0D0, 0.0D0, 0.0D0,
     :     0.0D0, 2000.0D0, MJD, RA_N_APP, DEC_N_APP)

      ELSE IF (COORD_TYPE .EQ. 'EQ') THEN
         CALL SLA_AMP (RA_APP, DEC_APP, MJD, 2000.0D0, RA_2000,
     :     DEC_2000)
         CALL SLA_EQECL (RA_2000, DEC_2000, MJD, LONG, LAT)

         CALL SLA_ECLEQ (0.0D0, DPI2, MJD, RA_N_2000, DEC_N_2000)
         CALL SLA_MAP (RA_N_2000, DEC_N_2000, 0.0D0, 0.0D0, 0.0D0,
     :     0.0D0, 2000.0D0, MJD, RA_N_APP, DEC_N_APP)

      ELSE
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_CALC_OUTPUT_COORDS: can '//
     :        'only handle RB, RJ, RD, GA and EQ coordinates',
     :        STATUS)
         END IF
      END IF

      END
