      SUBROUTINE SURF_REQUEST_OUTPUT_COORDS( TASK, PARLONG, PARLAT,
     :     OUT_COORDS, LAT_OBS, DEF_RA_CEN, DEF_DEC_CEN, MJD, HOURS,
     :     OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION, OUT_LONG, OUT_LAT,
     :     STATUS )
*+
*  Name:
*     SURF_REQUEST_OUTPUT_COORDS

*  Purpose:
*     Request longitude and latitude of output map from user


*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURF_REQUEST_OUTPUT_COORDS( TASK, PARLONG, PARLAT,
*    :     OUT_COORDS, LAT_OBS, DEF_RA_CEN, DEF_DEC_CEN, MJD, HOURS,
*    :     OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION, OUT_LONG, OUT_LAT,
*    :     STATUS)


*  Description:
*     Prompts user for output coordinates of map in the specified
*     coordinate frame. The default values are derived from the
*     supplied apparent RA/Dec centre. The selected centre
*     is returned (in apparent ra/dec).

*  Arguments:
*     TASK = CHARACTER (Given)
*        Description of task to be used in error messages
*     PARLONG = CHARACTER (Given)
*        Name of the parameter used to request the LONGITUDE
*     PARLAT = CHARACTER (Given)
*        Name of the parameter used to request the LATITUDE
*     OUT_COORDS = CHARACTER (Given)
*        Coordinate frame of output coordinates.
*     LAT_OBS = DOUBLE (Given)
*        Latitude of observatory (Radians)
*     DEF_RA_CEN = DOUBLE (Given)
*        Apparent RA of the default map centre (radians)
*     DEF_DEC_CEN = DOUBLE (Given)
*        Apparent Dec of the default map centre (radians)
*     MJD = DOUBLE (Given)
*        Modified Julian date to be used as reference for apparent
*        RA/Dec coordinates
*     HOURS = LOGICAL (Given)
*        Flag to decide whether longitude is expressed as hours or degrees
*     OUT_RA_CEN = DOUBLE (Returned)
*        Apparent RA of output map centre (radians)
*     OUT_DEC_CEN = DOUBLE (Returned)
*        Apparent Dec of output map centre (radians)
*     OUT_ROTATION = DOUBLE (Returned)
*        angle between apparent N and N of output coord system (radians)
*     OUT_LONG = DOUBLE (Returned)
*        longitude of output map centre in the selected coordinate frame (rad)
*     OUT_LAT = DOUBLE (Returned)
*        latitude of output map centre in the selected coordinate frame (rad)
*     STATUS = INTEGER (Given & Returned)
*        Global status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)

*  Notes:
*     - The output coordinates are always slightly different from the
*       when defaults are accepted since there is a loss of precision
*       converting the ra/dec to and from a string form.


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.4  1999/08/19 03:37:44  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.3  1999/08/03 20:01:39  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.2  1999/07/14 20:13:30  timj
*     Pass LAT_OBS into SCULIB_CALC_APPARENT rather than having it as
*     a parameter.
*
*     Revision 1.1  1999/07/14 04:50:32  timj
*     New
*

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      DOUBLE PRECISION DEF_DEC_CEN
      DOUBLE PRECISION DEF_RA_CEN
      LOGICAL          HOURS
      DOUBLE PRECISION LAT_OBS
      DOUBLE PRECISION MJD
      CHARACTER *(*)   OUT_COORDS
      CHARACTER *(*)   PARLAT
      CHARACTER *(*)   PARLONG
      CHARACTER *(*)   TASK

*  Arguments Returned:
      DOUBLE PRECISION OUT_DEC_CEN
      DOUBLE PRECISION OUT_LAT
      DOUBLE PRECISION OUT_LONG
      DOUBLE PRECISION OUT_RA_CEN
      DOUBLE PRECISION OUT_ROTATION

*     Status
      INTEGER STATUS

*  Local Variables:
      INTEGER          HMSF (4) ! holds converted angle information from
                                ! SLA routine
      INTEGER          ITEMP    ! Scratch integer
      CHARACTER*1      SIGN     ! + or -
      CHARACTER*80     STEMP    ! scratch string
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Decide whether we have a sky frame

      IF ((OUT_COORDS.NE.'NA'.AND.OUT_COORDS.NE.'AZ'
     :     .AND. OUT_COORDS.NE.'PL')) THEN

*     Convert the input coordinates to the output coordinates
*     and use them as the default.

         CALL SCULIB_CALC_OUTPUT_COORDS (DEF_RA_CEN, DEF_DEC_CEN,
     :        MJD, OUT_COORDS, OUT_LONG, OUT_LAT, STATUS)

*     Construct a string containing the suggested output centre
*     (ie the input centre)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (HOURS) then
               CALL SLA_DR2TF (2, OUT_LONG, SIGN, HMSF)

               STEMP = SIGN
               WRITE (STEMP(2:3),'(I2.2)') HMSF(1)
               STEMP (4:4) = ' '
               WRITE (STEMP(5:6),'(I2.2)') HMSF(2)
               STEMP (7:7) = ' '
               WRITE (STEMP(8:9),'(I2.2)') HMSF(3)
               STEMP (10:10) = '.'
               WRITE (STEMP(11:12),'(I2.2)') HMSF(4)
            ELSE
               CALL SLA_DR2AF (1, OUT_LONG, SIGN, HMSF)

               STEMP = SIGN
               WRITE (STEMP(2:4), '(I3.3)') HMSF(1)
               STEMP (5:5) = ' '
               WRITE (STEMP(6:7), '(I2.2)') HMSF(2)
               STEMP (8:8) = ' '
               WRITE (STEMP(9:10), '(I2.2)') HMSF(3)
               STEMP (11:11) = '.'
               WRITE (STEMP(12:12), '(I1.1)') HMSF(4)
            END IF
         END IF


*     Ask for long of output image

         CALL PAR_DEF0C (PARLONG, STEMP, STATUS)
         CALL PAR_GET0C (PARLONG, STEMP, STATUS)

*     Decode longitude string

         IF (STATUS .EQ. SAI__OK) THEN
            ITEMP = 1
            CALL SLA_DAFIN (STEMP, ITEMP, OUT_LONG, STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TASK)
               CALL ERR_REP (' ', '^TASK: error reading '//
     :              'output centre longitude - it must be in '//
     :              '5 10 34.6 format', STATUS)
            ELSE
               IF (HOURS) THEN
                  OUT_LONG = OUT_LONG * 15.0D0
               END IF
            END IF
         END IF

*     Construct the latitude string

         IF (STATUS .EQ. SAI__OK) THEN
            CALL SLA_DR2AF (1, OUT_LAT, SIGN, HMSF)

            STEMP = SIGN
            WRITE (STEMP(3:4),'(I2.2)') HMSF(1)
            STEMP (5:5) = ' '
            WRITE (STEMP(6:7),'(I2.2)') HMSF(2)
            STEMP (8:8) = ' '
            WRITE (STEMP(9:10),'(I2.2)') HMSF(3)
            STEMP (11:11) = '.'
            WRITE (STEMP(12:12), '(I1.1)') HMSF(4)
         END IF

*     Ask for the lat

         CALL PAR_DEF0C (PARLAT, STEMP, STATUS)
         CALL PAR_GET0C (PARLAT, STEMP, STATUS)

*     Decode the string

         IF (STATUS .EQ. SAI__OK) THEN
            ITEMP = 1
            CALL SLA_DAFIN (STEMP, ITEMP, OUT_LAT, STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TASK)
               CALL ERR_REP (' ', '^TASK: error reading '//
     :              'output centre latitude -  it must be in '//
     :              '-30 13 56.4 format', STATUS)
            END IF
         END IF

*     Convert this centre to apparent ra/dec

         CALL SCULIB_CALC_APPARENT (LAT_OBS, OUT_LONG, OUT_LAT, 0.0D0,
     :        0.0D0, 0.0D0, 0.0D0, OUT_COORDS, 0.0, MJD, 0.0D0, 0.0D0,
     :        OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION, STATUS)

      ELSE

*     This is not a sky frame so set the centre to 0.0
         OUT_RA_CEN = 0.0D0
         OUT_DEC_CEN = 0.0D0
         OUT_ROTATION = 0.0D0

      END IF

      END
