      SUBROUTINE SCULIB_SCAN_APPARENT_TP_2_AZNA(OUT_COORDS, N_POS,
     :     N_BOL, ELEVATION, PAR_ANGLE,
     :     BOL_DEC, BOL_RA, STATUS)
*+
*  Name:
*     SCULIB_SCAN_APPARENT_TP_2_AZNA

*  Purpose:
*     Calculate NAsmyth and AZel coordinates for SCAN/MAP

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_SCAN_APPARENT_TP_2_AZNA(OUT_COORDS, N_POS,
*    :     N_BOL, ELEVATION, PAR_ANGLE,
*    :     BOL_DEC, BOL_RA, STATUS)

*  Description:
*     This routine calculates the NA/AZ offsets from tangent plane
*     offsets for SCAN map data.

*  Arguments:
*     OUT_COORDS = _CHAR (Given)
*        Output coordinate system
*     N_POS = _INTEGER (Given)
*        Number of `samples' taken
*     N_BOL = _INTEGER (Given)
*        Number of bolometers in the input data
*     ELEVATION ( N_POS ) = _DOUBLE (Given)
*        Elevation of each exposure
*     PAR_ANGLE ( N_POS ) = _DOUBLE (Given)
*        Parallactic angle of each exposure
*     RA_CEN = _DOUBLE (Given)
*        apparent RA of output map centre (radians) Only used for JIGGLE data.
*     DEC_CEN = _DOUBLE (Given)
*        apparent Dec of output map centre (radians) Only used for JIGGLE data.
*     BOL_DEC(N_BOL, N_POS) = _DOUBLE (Returned)
*         Apparent DEC of bolometers for each measurement for MJD_STANDARD
*     BOL_RA(N_BOL, N_POS) = _DOUBLE (Returned)
*         Apparent RA of bolometers for each measurement for MJD_STANDARD
*     STATUS = _INTEGER (Given & Returned)
*        Global status

*  Prior Requirements:
*     The locator to the structure must already be available.

*  Notes:
*     This routine does not annul the locator.

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL:  John Lightfoot (RoE)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1997 March 20 (TIMJ)
*        Extract from main tasks

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER          N_POS
      DOUBLE PRECISION ELEVATION (N_POS)
      INTEGER          N_BOL
      CHARACTER *(*)   OUT_COORDS
      DOUBLE PRECISION PAR_ANGLE (N_POS)

*     Arguments Given & Returned:
      DOUBLE PRECISION BOL_DEC(N_BOL, N_POS)
      DOUBLE PRECISION BOL_RA(N_BOL, N_POS)

*  Status:
      INTEGER STATUS                      ! Global status

*  Local constants:
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.14159265359D0)

*  Local Variables:
      INTEGER          BOL                ! Counter
      DOUBLE PRECISION COS_E              ! cos (E)
      DOUBLE PRECISION COS_Q              ! cos (Q)
      DOUBLE PRECISION DAZ                ! azimuth of point (radians)
      DOUBLE PRECISION DEL                ! elevation of point (radians)
      DOUBLE PRECISION DX                 ! X offset of point (radians)
      DOUBLE PRECISION DY                 ! Y offset of point (radians)
      DOUBLE PRECISION E                  ! Elevation (radians)
      INTEGER I                           ! Loop counter
      DOUBLE PRECISION SIN_E              ! sin (E)
      DOUBLE PRECISION SIN_Q              ! sin (Q)
      DOUBLE PRECISION Q                  ! parallactic angle (radians)

*.

      IF (STATUS .NE. SAI__OK) RETURN

* Check the OUT_COORDS

      IF (OUT_COORDS.NE.'NA'.AND.OUT_COORDS.NE.'AZ') THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC ('COORDS', OUT_COORDS)
         CALL ERR_REP (' ', 'SCAN_APPARENT_TP_2_AZNA: bad value for '//
     :     'OUT_COORDS - ^COORDS', STATUS)
      END IF


*     Loop over all data points (for all bolometers at a time)
      DO I = 1, N_POS

         E = ELEVATION(I)
         Q = PAR_ANGLE(I)

*       Check that the elevation is valid
         IF ( E .NE. VAL__BADD .AND. Q .NE. VAL__BADD
     :        .AND. STATUS .EQ. SAI__OK) THEN

            SIN_Q = SIN ( Q )
            COS_Q = COS ( Q )
            SIN_E = SIN ( E )
            COS_E = SIN ( E )

            IF (N_BOL .GT. 0) THEN

               DO BOL = 1, N_BOL

*     Calculate the Az and El offsets

                  DAZ = - BOL_RA(BOL, I) * COS_Q
     :                 + BOL_DEC(BOL, I) * SIN_Q

                  DEL =   BOL_RA(BOL, I) * SIN_Q
     :                 + BOL_DEC(BOL, I) * COS_Q

*       Calculate the NA offsets

                  IF (OUT_COORDS .EQ. 'NA') THEN

                     DX = DAZ * COS_E - DEL * SIN_E
                     DY = DAZ * SIN_E + DEL * COS_E

                  ELSE
                     DX = DAZ
                     DY = DEL
                  END IF

                  BOL_RA(BOL, I) = DX
                  BOL_DEC(BOL, I) = DY

               END DO

            END IF
         END IF


      END DO


      END
