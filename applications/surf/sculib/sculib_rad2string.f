      SUBROUTINE SCULIB_RAD2STRING ( ANGLE, NDP, ISTIME, RESLT, STATUS )
*+
*  Name:
*     SCULIB_RAD2STRING

*  Purpose:
*     Translate an angle or time in radians to a nicely formatted string

*  Invocation:
*     CALL SCULIB_RAD2STRING( ANGLE, NDP, ISTIME, RESLT, STATUS )

*  Description:
*     Converts an angle or time in radians to a string that can be
*     used for display. The sexagesimal format is colon separated.
*     For time output, the output is format SHH:MM:SS.NDP.
*     For angle output, the format used is SDD:MM:SS.NDP. Note
*     that the sign is used for the first character but this can
*     be either blank or a '-'.

*  Arguments:
*     ANGLE = DOUBLE PRECISION (Given)
*        Angle to be translated (radians)
*     NDP = INTEGER (Given)
*        Number of decimal places to use for the seconds
*        Can not be greater than 6.
*     ISTIME = LOGICAL (Given)
*        If true the angle will be assumed to be a time and will
*        be converted to HH:MM:SS.NDP format. If false it will be
*        assumed to be an angle and converted to SDDD:MM:SS.NDP.
*     RESLT = CHARACTER (Returned)
*        String containing the translated angle. Colon separated.
*        Should be at least 11+NDP characters long.
*     STATUS = INTEGER (Given & Returned)
*        Global Status

*  Notes:
*     Essentially a wrapper around SLA_DR2AF and SLA_DR2TF

*  Authors:
*     Tim Jenness (TIMJ)
*        Joint Astronomy Centre, Hilo, HI

*  Copyright:
*     Copyright (C) 2000 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.3  2000/07/11 02:38:00  timj
*     Remove unused variables
*
*     Revision 1.2  2000/07/10 21:09:31  timj
*     Documentation tweaks for V1.6
*
*     Revision 1.1  2000/06/17 00:54:39  timj
*     First version
*

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      DOUBLE PRECISION ANGLE
      LOGICAL          ISTIME
      INTEGER          NDP

*  Arguments Returned:
      CHARACTER *(*)   RESLT

*  Status:
      INTEGER          STATUS

*  Local Constants:

*  Local Variables:
      INTEGER          I        ! loop counter
      INTEGER          IHMSF( 4 ) ! Hours, minutes, seconds and fraction
      CHARACTER * 1    SIGN     ! Sign of angle

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Check NDP
      IF (NDP .GT. 6 .OR. NDP .LT. 0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('NDP', NDP)
         CALL ERR_REP( ' ', 'SCULIB_RAD2STRING: NDP must be not be'//
     :        ' greater than 6 or less than 0 (was ^NDP)', STATUS)
         RETURN
      END IF

*     Translate the radians to sexagesimal
*     and decide on the format statement to use
      IF (ISTIME) THEN
         CALL SLA_DR2TF( NDP, ANGLE, SIGN, IHMSF )
      ELSE
         CALL SLA_DR2AF( NDP, ANGLE, SIGN, IHMSF )
      END IF

*     Only put a SIGN if it is negative
      IF (SIGN .EQ. '+') SIGN = ' '

*     Create the formatted string. For some reason the format number
*     can not be a variable so we need to select the format explicitly.
*     This is a pain. All because I wanted NDP to be an argument....
*     Should be using perl

      IF (ISTIME) THEN
         IF (NDP .EQ. 0) THEN
            WRITE( RESLT, 20 ) SIGN, (IHMSF(I),I=1,3)
         ELSE IF (NDP .EQ. 1) THEN
            WRITE( RESLT, 21 ) SIGN, (IHMSF(I),I=1,4)
         ELSE IF (NDP .EQ. 1) THEN
            WRITE( RESLT, 22 ) SIGN, (IHMSF(I),I=1,4)
         ELSE IF (NDP .EQ. 1) THEN
            WRITE( RESLT, 23 ) SIGN, (IHMSF(I),I=1,4)
         ELSE IF (NDP .EQ. 1) THEN
            WRITE( RESLT, 24 ) SIGN, (IHMSF(I),I=1,4)
         ELSE IF (NDP .EQ. 1) THEN
            WRITE( RESLT, 25 ) SIGN, (IHMSF(I),I=1,4)
         ELSE IF (NDP .EQ. 1) THEN
            WRITE( RESLT, 26 ) SIGN, (IHMSF(I),I=1,4)
         END IF
      ELSE
         IF (NDP .EQ. 0) THEN
            WRITE( RESLT, 10 ) SIGN, (IHMSF(I),I=1,3)
         ELSE IF (NDP .EQ. 1) THEN
            WRITE( RESLT, 11 ) SIGN, (IHMSF(I),I=1,4)
         ELSE IF (NDP .EQ. 1) THEN
            WRITE( RESLT, 12 ) SIGN, (IHMSF(I),I=1,4)
         ELSE IF (NDP .EQ. 1) THEN
            WRITE( RESLT, 13 ) SIGN, (IHMSF(I),I=1,4)
         ELSE IF (NDP .EQ. 1) THEN
            WRITE( RESLT, 14 ) SIGN, (IHMSF(I),I=1,4)
         ELSE IF (NDP .EQ. 1) THEN
            WRITE( RESLT, 15 ) SIGN, (IHMSF(I),I=1,4)
         ELSE IF (NDP .EQ. 1) THEN
            WRITE( RESLT, 16 ) SIGN, (IHMSF(I),I=1,4)
         END IF
      END IF

*     All the format statements since I cant seem to use
*     a variable NDP to specify the width of the last field
*     Since I want to make sure I get an 'INDP.NDP' format
*     so that the last entry is zero-padded
 10   FORMAT ( A1, I3.3, ':', I2.2, ':', I2.2)
 11   FORMAT ( A1, I3.3, ':', I2.2, ':', I2.2, '.', I1)
 12   FORMAT ( A1, I3.3, ':', I2.2, ':', I2.2, '.', I2.2)
 13   FORMAT ( A1, I3.3, ':', I2.2, ':', I2.2, '.', I3.3)
 14   FORMAT ( A1, I3.3, ':', I2.2, ':', I2.2, '.', I4.4)
 15   FORMAT ( A1, I3.3, ':', I2.2, ':', I2.2, '.', I5.5)
 16   FORMAT ( A1, I3.3, ':', I2.2, ':', I2.2, '.', I6.6)

 20   FORMAT ( A1, I2.2, ':', I2.2, ':', I2.2)
 21   FORMAT ( A1, I2.2, ':', I2.2, ':', I2.2, '.', I1)
 22   FORMAT ( A1, I2.2, ':', I2.2, ':', I2.2, '.', I2.2)
 23   FORMAT ( A1, I2.2, ':', I2.2, ':', I2.2, '.', I3.3)
 24   FORMAT ( A1, I2.2, ':', I2.2, ':', I2.2, '.', I4.4)
 25   FORMAT ( A1, I2.2, ':', I2.2, ':', I2.2, '.', I5.5)
 26   FORMAT ( A1, I2.2, ':', I2.2, ':', I2.2, '.', I6.6)

      END

