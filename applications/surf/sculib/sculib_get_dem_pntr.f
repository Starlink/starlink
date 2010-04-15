      SUBROUTINE SCULIB_GET_DEM_PNTR(ACTDIM, LOC,
     :     DEM_PNTR_PTR, N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)
*+
*  Name:
*     SCULIB_GET_DEM_PNTR

*  Purpose:
*     Get the DEM_PNTR array and its dimensions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_GET_DEM_PNTR(ACTDIM, LOC, DEM_PNTR_PTR,
*    :     N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
*    :     STATUS)

*  Description:
*     This routine obtains the dem_pntr array and returns the
*     dimensions in terms of switches, exposures, inegrations and
*     measurements.


*  Arguments:
*     ACTDIM = _INTEGER (Given)
*        Number of expected dimensions
*     LOC = _CHARACTER (Given)
*        Locator to the SCUBA extension
*     DEM_PNTR_PTR = _INTEGER (Returned)
*        Pointer to location of DEM_PTR array in memory
*     N_SWITCHES = _INTEGER (Returned)
*        Number of switches indicated by DEM_PNTR
*     N_EXPOSURES = _INTEGER (Returned)
*        Number of exposures indicated by DEM_PNTR
*     N_INTEGRATIONS = _INTEGER (Returned)
*        Number of integrations indicated by DEM_PNTR
*     N_MEASUREMENTS = _INTEGER (Returned)
*        Number of measurements indicated by DEM_PNTR
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
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER ACTDIM
      CHARACTER*(*) LOC

*  Arguments Returned:
      INTEGER DEM_PNTR_PTR
      INTEGER N_EXPOSURES
      INTEGER N_INTEGRATIONS
      INTEGER N_MEASUREMENTS
      INTEGER N_SWITCHES

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER   MAX_DIM          ! max number of dims in array
      PARAMETER (MAX_DIM = 4)

*  Local Variables:
      INTEGER DIM (0:MAX_DIM)    ! array dimensions
      INTEGER ITEMP              ! Dummy variable
      INTEGER NDIM               ! number of dimensions
      INTEGER OFFSET             ! Array offset for Switches/Integrations
*.
      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise
      N_INTEGRATIONS = 0
      N_MEASUREMENTS = 0
      N_EXPOSURES    = 0
      N_SWITCHES     = 0

*     Use an array offset in order to deal with Switches
      IF (ACTDIM .EQ. 4) THEN
         OFFSET = 0
      ELSE
         OFFSET = 1
      END IF

*     Get shape of DEM_PNTR
      CALL CMP_SHAPE(LOC, 'DEM_PNTR', MAX_DIM, DIM(OFFSET), NDIM,STATUS)

*     Check dimensions
      IF (STATUS .EQ. SAI__OK) THEN
         IF (NDIM .NE. ACTDIM) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL ERR_REP (' ', 'SCULIB_GET_DEM_PNTR: '//
     :           '.SCUBA.DEM_PNTR array has bad number of '//
     :           'dimensions', STATUS)
         ELSE

*     Switches
            IF (ACTDIM .EQ. 4) THEN
               IF (DIM(0) .LE. 0) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('DIM0',DIM(0))
                  CALL ERR_REP (' ', 'SCULIB_GET_DEM_PNTR: '//
     :                 '.SCUBA.DEM_PNTR array contains bad number '//
     :                 'of switches - ^DIM0', STATUS)
               END IF
            END IF

*     Exposures
            IF (DIM(1) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM1',DIM(1))
               CALL ERR_REP (' ', 'SCULIB_GET_DEM_PNTR: '//
     :              '.SCUBA.DEM_PNTR array contains bad number '//
     :              'of exposures - ^DIM1', STATUS)
            END IF

*     Integrations
            IF (DIM(2) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM2',DIM(2))
               CALL ERR_REP (' ', 'SCULIB_GET_DEM_PNTR: '//
     :              '.SCUBA.DEM_PNTR array contains bad number '//
     :              'of integrations - ^DIM2', STATUS)
            END IF

*     Measurements
            IF (DIM(3) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM3',DIM(3))
               CALL ERR_REP (' ', 'SCULIB_GET_DEM_PNTR: '//
     :              '.SCUBA.DEM_PNTR array contains bad number '//
     :              'of measurements - ^DIM3', STATUS)
            END IF

         END IF
      END IF

*     Setup values for return
      IF (STATUS .EQ. SAI__OK) THEN
         IF (ACTDIM .EQ. 4) N_SWITCHES  = DIM (0)
         N_EXPOSURES = DIM (1)
         N_INTEGRATIONS = DIM (2)
         N_MEASUREMENTS = DIM (3)
      END IF

*     Only bother to map if STATUS okay after shape check
      CALL CMP_MAPV(LOC, 'DEM_PNTR', '_INTEGER', 'READ', DEM_PNTR_PTR,
     :     ITEMP, STATUS)


      END
