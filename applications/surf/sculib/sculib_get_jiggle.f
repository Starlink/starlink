      SUBROUTINE SCULIB_GET_JIGGLE(LOC, MAX_JIGGLE, N_FITS,
     :     FITS, JIGGLE_COUNT, JIGGLE_REPEAT, JIGGLE_P_SWITCH,
     :     SAMPLE_PA, SAMPLE_COORDS, JIGGLE_X, JIGGLE_Y,
     :     STATUS)
*+
*  Name:
*     SCULIB_GET_JIGGLE

*  Purpose:
*     Get the jiggle parameters

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_GET_JIGGLE(LOC, MAX_JIGGLE, N_FITS,
*    :     FITS, JIGGLE_COUNT, JIGGLE_REPEAT, JIGGLE_P_SWITCH,
*    :     SAMPLE_PA, SAMPLE_COORDS, JIGGLE_X, JIGGLE_Y,
*    :     STATUS)

*  Description:
*     This routine obtains the parameters of the JIGGLE pattern:
*     JIGL_X, JIGL_Y and some FITS parameters.
*     The dimensions of these arrays are checked and an error returned
*     if necessary.

*  Arguments:
*     LOC = _CHARACTER (Given)
*        Locator to the SCUBA extension
*     MAX_JIGGLE = _INTEGER (Given)
*        Maximum number of jiggle positions
*     N_FITS = _INTEGER (Given)
*        Size of FITS array
*     FITS  = _CHARACTER (Given)
*        FITS values
*     JIGGLE_COUNT = _INTEGER (Returned)
*        Actual size of jiggle pattern
*     JIGGLE_REPEAT = _INTEGER (Returned)
*        Number of times jiggle pattern is repeated in a switch
*     JIGGLE_P_SWITCH = _INTEGER (Returned)
*        Number of jiggles per switch
*     SAMPLE_PA = _REAL
*        position angle of sample x axis relative to x axis of SAMPLE_COORDS
*     SAMPLE_COORDS = _CHARACTER
*        coordinate system of sample offsets
*     JIGGLE_X = _REAL (Returned)
*        X jiggle positions
*     JIGGLE_Y = _REAL (Returned)
*        Y jiggle positions
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
*     1997 March 21 (TIMJ)
*        Extract from main tasks

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*) LOC
      INTEGER       MAX_JIGGLE
      INTEGER       N_FITS
      CHARACTER*(*) FITS(N_FITS)

*  Arguments Returned:
      INTEGER       JIGGLE_COUNT
      INTEGER       JIGGLE_P_SWITCH
      INTEGER       JIGGLE_REPEAT
      REAL          JIGGLE_X (MAX_JIGGLE)
      REAL          JIGGLE_Y (MAX_JIGGLE)
      CHARACTER*(*) SAMPLE_COORDS
      REAL          SAMPLE_PA

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:

*  Local Variables:
      INTEGER ITEMP              ! Dummy variable
*.
      IF (STATUS .NE. SAI__OK) RETURN


*     basic jiggle parameters
      CALL SCULIB_GET_FITS_I (N_FITS, N_FITS, FITS,
     :     'JIGL_CNT', JIGGLE_COUNT, STATUS)
      CALL SCULIB_GET_FITS_I (N_FITS, N_FITS, FITS,
     :     'J_REPEAT', JIGGLE_REPEAT, STATUS)
      CALL SCULIB_GET_FITS_I (N_FITS, N_FITS, FITS,
     :     'J_PER_S', JIGGLE_P_SWITCH, STATUS)

*     the jiggle pattern itself

      CALL CMP_GET1R (LOC, 'JIGL_X',
     :     MAX_JIGGLE, JIGGLE_X, ITEMP, STATUS)

      IF (ITEMP .NE. JIGGLE_COUNT) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_GET_JIGGLE: '//
     :           'mismatch between JIGGLE_COUNT and number '//
     :           'of X jiggle offsets read', STATUS)
         END IF
      END IF

      CALL CMP_GET1R (LOC, 'JIGL_Y',
     :     MAX_JIGGLE, JIGGLE_Y, ITEMP, STATUS)

      IF (ITEMP .NE. JIGGLE_COUNT) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_GET_JIGGLE: '//
     :           'mismatch between JIGGLE_COUNT and number '//
     :           'of Y jiggle offsets read', STATUS)
         END IF
      END IF

*     the name and rotation of the jiggle coordinate system

      CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS,
     :     'SAM_PA', SAMPLE_PA, STATUS)
      CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS,
     :     'SAM_CRDS', SAMPLE_COORDS, STATUS)
      CALL CHR_UCASE (SAMPLE_COORDS)

      END
