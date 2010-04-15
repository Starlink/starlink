      SUBROUTINE SCULIB_SCAN_2_RD(VERSION, CENTRE_COORDS, RA_CEN,
     :     DEC_CEN, LONG, LAT, LST, MJD, LAT_OBS, RA_APP, DEC_APP,
     :     STATUS )
*+
*  Name:
*     SCULIB_SCAN_2_RD

*  Purpose:
*     Calculate the apparent RA/Dec of a scan

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_SCAN_2_RD(VERSION, CENTRE_COORDS, RA_CEN, DEC_CEN,
*    :     LONG, LAT, LST, MJD, LAT_OBS, RA_APP, DEC_APP,
*    :     STATUS )

*  Description:
*     The start and end of each scan is calculated in different
*     coordinate systems dependent on the coordinate system of the
*     tracking centre.
*     This routine calculates the apparent RA/DEC centre for
*     the supplied long and Lat using knowledge of the transputer system.

*  Arguments:
*     VERSION = REAL (Given)
*       Version number of the file. This governs whether we need to
*       even run this subroutine.
*     CENTRE_COORDS = CHAR (Given)
*       Centre coordinates of tracking centre
*     RA_CEN = DOUBLE (Given)
*       Apparent RA of map centre
*     DEC_CEN = DOUBLE (Given)
*       Apparent dec of map centre
*     LONG = DOUBLE (Given)
*       Longitude of array at LST
*     LAT = DOUBLE (Given)
*       Latitude of array at LST
*     LST = DOUBLE (Given)
*       Local sidereal time (radians)
*     MJD = DOUBLE (Given)
*       Modified Julian data of observation (should be the MJD of the
*       time for which lst = LST).
*     LAT_OBS                = DOUBLE PRECISION (Given)
*           Latitude of observatory in radians.
*     RA_APP                 = DOUBLE PRECISION (Returned)
*           Apparent RA of point at date (radians)
*     DEC_APP                = DOUBLE PRECISION (Returned)
*           Apparent Dec
*     STATUS                 = INTEGER (Given and returned)
*           Global status

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)
*     T.Jenness (timj@jach.hawaii.edu)

*  Notes:
*     Before November 1997 the situation is a bit tricky:
*       -[1] Scan ends were assumed to be RD by the transputers
*       -[2] The telescope assumed RJ offsets

*     Therefore I need to do the following to recreate what actually
*     happened:
*       -[1] Calculate tangent plane offsets from the RD centre
*       -[2] Add these offsets onto the actual RJ tracking centre
*       -[3] Convert back into RD
*
*     Post November 1997 the offsets really are RD even when the telescope
*     goes to RJ so this routine should not be called. Version 1.0 data
*     will not be modified.



*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.6  1999/08/19 03:37:23  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.5  1999/08/03 19:35:27  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.4  1999/07/14 20:13:27  timj
*     Pass LAT_OBS into SCULIB_CALC_APPARENT rather than having it as
*     a parameter.
*
*     Revision 1.3  1999/07/13 06:27:15  timj
*     Pass LAT_OBS in as a argument
*
*     Revision 1.2  1998/02/05 19:41:36  timj
*     Change VERSION to REAL
*
*     Revision 1.1  1997/11/24 23:39:36  timj
*     Initial revision
*
*
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      DOUBLE PRECISION DEC_CEN
      DOUBLE PRECISION RA_CEN
      DOUBLE PRECISION LAT
      DOUBLE PRECISION LAT_OBS
      DOUBLE PRECISION LONG
      CHARACTER*(*)    CENTRE_COORDS
      DOUBLE PRECISION LST
      DOUBLE PRECISION MJD
      REAL             VERSION

*  Arguments Returned:
      DOUBLE PRECISION RA_APP
      DOUBLE PRECISION DEC_APP

*  Status:
      INTEGER          STATUS

*  Local Constants:

*  Local Variables:
      DOUBLE PRECISION DTEMP                ! Scratch double
      DOUBLE PRECISION ETA                  ! Tangent plane Y offset
      DOUBLE PRECISION MYLONG               ! Intermediate longitude
      DOUBLE PRECISION MYLAT                ! Intermediate latitude
      INTEGER          SLA_STATUS           ! Status from SLA call
      CHARACTER *(3)   STEMP                ! Scratch string
      DOUBLE PRECISION XI                   ! Tangent plane x offset

*.

      IF (STATUS .NE. SAI__OK) RETURN

      CALL CHR_UCASE (CENTRE_COORDS)

*     If we are in an RD centre then everything is already fine
*     We also do nothing if this is version 1.0 data or newer.

      IF (CENTRE_COORDS .EQ. 'RD' .OR.
     :     CENTRE_COORDS .EQ. 'PLANET' .OR.
     :     VERSION .GE. 1.0) THEN

         RA_APP  = LONG
         DEC_APP = LAT

      ELSE

*     First I need to calculate the tangent plane offsets to the map centre

         CALL SLA_DS2TP(LONG, LAT, RA_CEN, DEC_CEN, XI, ETA, SLA_STATUS)

*     Check return status
         IF (SLA_STATUS .NE. 0) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETI('ERR', SLA_STATUS)
            CALL ERR_REP(' ','SCULIB_SCAN_2_RD: Error converting '//
     :           'to tangent plane offsets (Err code = ^ERR)',
     :           STATUS)

         ELSE

*     If we have an RB,RJ,GA centre then these offsets are in fact
*     RJ offsets. If we have an AZ centre then they are AZ offsets

            IF (CENTRE_COORDS .EQ. 'RJ' .OR.
     :           CENTRE_COORDS .EQ. 'RB' .OR.
     :           CENTRE_COORDS .EQ. 'GA') THEN

               STEMP = 'RJ'

            ELSE IF (CENTRE_COORDS .EQ. 'AZ') THEN

               STEMP = 'AZ'

            ELSE
               STEMP = 'NO'

               IF (STATUS .EQ. SAI__OK) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC('CD', CENTRE_COORDS)
                  CALL ERR_REP(' ','SCULIB_SCAN_2_RD: ^CD centre'//
     :                 ' coords is not supported', STATUS)

               END IF


            END IF

*     Now add on the offset

*     Convert map centre from apparent to our new 'local' coords

            CALL SCULIB_APPARENT_2_MP(RA_CEN, DEC_CEN, STEMP,
     :           LST, MJD, LAT_OBS, MYLONG, MYLAT, STATUS)

*     Add on our offsets
            CALL SCULIB_CALC_APPARENT(LAT_OBS, MYLONG, MYLAT, 0.0D0,
     :           0.0D0, XI, ETA, STEMP, LST, MJD, 0.0D0,
     :           0.0D0, RA_APP, DEC_APP, DTEMP, STATUS)


         END IF


      END IF


      END
