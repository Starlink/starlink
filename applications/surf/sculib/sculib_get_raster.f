      SUBROUTINE SCULIB_GET_RASTER(LOC, N_SWITCHES, N_EXPOSURES,
     :     N_INTEGRATIONS, N_MEASUREMENTS, RA1_PTR, RA2_PTR,
     :     DEC1_PTR, DEC2_PTR, STATUS)
*+
*  Name:
*     SCULIB_GET_RASTER

*  Purpose:
*     Get the raster parameters

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_GET_RASTER(LOC, N_SWITCHES, N_EXPOSURES,
*    :     N_INTEGRATIONS, N_MEASUREMENTS, RA1_PTR, RA2_PTR,
*    :     DEC1_PTR, DEC2_PTR, STATUS)

*  Description:
*     This routine obtains the parameters of the SCAN/MAP raster:
*     RA1, RA2, DEC1 and DEC2.
*     The dimensions of these arrays are checked and an error returned
*     if necessary.

*  Arguments:
*     LOC = _CHARACTER (Given)
*        Locator to the SCUBA extension
*     N_SWITCHES = _INTEGER (Given)
*        Actual number of switches
*     N_EXPOSURES = _INTEGER (Given)
*        Actual number of exposures
*     N_INTEGRATIONS = _INTEGER (Given)
*        Actual number of integrations
*     N_MEASUREMENTS = _INTEGER (Given)
*        Actual number of measurements
*     RA1_PTR = _INTEGER (Given)
*        Pointer to mapped RA1 array
*     RA2_PTR = _INTEGER (Given)
*        Pointer to mapped RA2 array
*     DEC1_PTR = _INTEGER (Given)
*        Pointer to mapped DEC1 array
*     DEC2_PTR = _INTEGER (Given)
*        Pointer to mapped DEC2 array
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
      INTEGER       N_SWITCHES
      INTEGER       N_EXPOSURES
      INTEGER       N_INTEGRATIONS
      INTEGER       N_MEASUREMENTS

*  Arguments Returned:
      INTEGER       RA1_PTR
      INTEGER       RA2_PTR
      INTEGER       DEC1_PTR
      INTEGER       DEC2_PTR

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER   MAX_DIM          ! max number of dims in array
      PARAMETER (MAX_DIM = 4)

*  Local Variables:
      CHARACTER*4 CMP(4)         ! Component Name
      INTEGER DIM(MAX_DIM)       ! Actual dimensions of mapped array
      INTEGER I                  ! Loop counter
      INTEGER ITEMP              ! Dummy variable
      INTEGER NDIM               ! Number of dimensions

*  Local Data:
      DATA CMP / 'RA1', 'RA2', 'DEC1', 'DEC2'/

*.
      IF (STATUS .NE. SAI__OK) RETURN


* Loop through all components

      DO I = 1, 4

         CALL CMP_SHAPE(LOC, CMP(I), MAX_DIM, DIM, NDIM, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (NDIM .NE. 4) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETC ('CMP', CMP(I))
               CALL ERR_REP (' ', 'SCULIB_GET_RASTER: '//
     :           '.SCUCD.^CMP array has bad number of '//
     :           'dimensions - ^NDIM', STATUS)
            ELSE
               IF (DIM(1) .NE. N_SWITCHES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_SWITCHES)
                  CALL MSG_SETI ('D', DIM(1))
                  CALL MSG_SETC ('CMP', CMP(I))
                  CALL ERR_REP (' ', 'SCULIB_GET_RASTER: '//
     :                 'there is a mismatch between the number '//
     :                 'of switches in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.^CMP (^D)', STATUS)
               END IF
               IF (DIM(2) .NE. N_EXPOSURES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_EXPOSURES)
                  CALL MSG_SETI ('D', DIM(2))
                  CALL MSG_SETC ('CMP', CMP(I))
                  CALL ERR_REP (' ', 'SCULIB_GET_RASTER: '//
     :                 'there is a mismatch between the number '//
     :                 'of exposures in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.^CMP (^D)', STATUS)
               END IF
               IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_INTEGRATIONS)
                  CALL MSG_SETI ('D', DIM(3))
                  CALL MSG_SETC ('CMP', CMP(I))
                  CALL ERR_REP (' ', 'SCULIB_GET_RASTER: '//
     :                 'there is a mismatch between the number '//
     :                 'of integrations in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.^CMP (^D)', STATUS)
               END IF
               IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_MEASUREMENTS)
                  CALL MSG_SETI ('D', DIM(4))
                  CALL MSG_SETC ('CMP', CMP(I))
                  CALL ERR_REP (' ', 'SCULIB_GET_RASTER: '//
     :                 'there is a mismatch between the number '//
     :                 'of measurements in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.^CMP (^D)', STATUS)
               END IF
            END IF
         END IF
      END DO

*     Map all the arrays.
*     Leave until the end since there is no point mapping them if STATUS
*     is bad for one and it is easier to leave them out of the loop.

      CALL CMP_MAPV(LOC, 'RA1', '_REAL', 'READ', RA1_PTR,
     :     ITEMP, STATUS)
      CALL CMP_MAPV(LOC, 'RA2', '_REAL', 'READ', RA2_PTR,
     :     ITEMP, STATUS)
      CALL CMP_MAPV(LOC, 'DEC1', '_REAL', 'READ', DEC1_PTR,
     :     ITEMP, STATUS)
      CALL CMP_MAPV(LOC, 'DEC2', '_REAL', 'READ', DEC2_PTR,
     :     ITEMP, STATUS)

      END
