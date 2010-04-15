      SUBROUTINE SCULIB_GET_LST_STRT(LOC, LST_STRT_PTR,
     :     N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)
*+
*  Name:
*     SCULIB_GET_LST_STRT

*  Purpose:
*     Get the LST_STRT array and check its dimensions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_GET_LST_STRT(LOC, LST_STRT_PTR,
*    :     N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
*    :     STATUS)

*  Description:
*     This routine obtains the lst_strt array and compares the
*     dimensions with the number of exposures, inegrations and
*     measurements. The number of switches is returned.


*  Arguments:
*     LOC = _CHARACTER (Given)
*        Locator to the SCUBA extension
*     LST_STRT_PTR = _INTEGER (Returned)
*        Pointer to location of DEM_PTR array in memory
*     N_SWITCHES = _INTEGER (Returned)
*        Number of switches indicated by LST_STRT
*     N_EXPOSURES = _INTEGER (Given)
*        Actual number of exposures
*     N_INTEGRATIONS = _INTEGER (Given)
*        Actual number of integrations
*     N_MEASUREMENTS = _INTEGER (Given)
*        Actual number of measurements
*     STATUS = _INTEGER (Given & Returned)
*        Global status

*  Prior Requirements:
*     The locator to the structure must already be available.

*  Notes:
*     This routine does not annul the locator.
*     The array must be unmapped before finishing the program.

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
      CHARACTER*(*) LOC
      INTEGER N_EXPOSURES
      INTEGER N_INTEGRATIONS
      INTEGER N_MEASUREMENTS

*  Arguments Returned:
      INTEGER LST_STRT_PTR
      INTEGER N_SWITCHES

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER   MAX_DIM          ! max number of dims in array
      PARAMETER (MAX_DIM = 4)

*  Local Variables:
      INTEGER DIM (MAX_DIM)    ! array dimensions
      INTEGER ITEMP              ! Dummy variable
      INTEGER NDIM               ! number of dimensions
*.
      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise
      N_SWITCHES = 0

*     Get shape of LST_STRT
      CALL CMP_SHAPE(LOC, 'LST_STRT', MAX_DIM, DIM, NDIM, STATUS)

*     Check dimensions

      IF (STATUS .EQ. SAI__OK) THEN
         IF (NDIM .NE. 4) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL ERR_REP (' ', 'SCULIB_GET_LST_STRT: '//
     :           '.SCUCD.LST_STRT array has bad number of '//
     :           'dimensions - ^NDIM', STATUS)
         ELSE
            IF (DIM(1) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL ERR_REP (' ', 'SCULIB_GET_LST_STRT: '//
     :              '.SCUCD.LST_STRT array contains bad '//
     :              'number of switch(es) - ^DIM1', STATUS)
            END IF
            IF (DIM(2) .NE. N_EXPOSURES) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NEXP', N_EXPOSURES)
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL ERR_REP (' ', 'SCULIB_GET_LST_STRT: '//
     :              'there is a mismatch between the number of '//
     :              'exposures in .SCUBA.DEM_PNTR (^NEXP) and '//
     :              'in .SCUCD.LST_STRT (^DIM2)', STATUS)
            END IF
            IF (DIM(3) .NE. N_INTEGRATIONS) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NINT', N_INTEGRATIONS)
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL ERR_REP (' ', 'SCULIB_GET_LST_STRT: '//
     :              'there is a mismatch between the number of '//
     :              'integrations in .SCUBA.DEM_PNTR (^NINT) '//
     :              'and in .SCUCD.LST_STRT (^DIM3)', STATUS)
            END IF
            IF (DIM(4) .NE. N_MEASUREMENTS) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NMEAS', N_MEASUREMENTS)
               CALL MSG_SETI ('DIM4', DIM(4))
               CALL ERR_REP (' ', 'SCULIB_GET_LST_STRT: '//
     :              'there is a mismatch between the number of '//
     :              'measurements in .SCUBA.DEM_PNTR (^NMEAS) '//
     :              'and in .SCUCD.LST_STRT (^DIM4)', STATUS)
            END IF
         END IF
      END IF


*     Setup return value
      IF (STATUS .EQ. SAI__OK) N_SWITCHES = DIM(1)

*     Only bother to map if STATUS okay after shape check
      CALL CMP_MAPV(LOC, 'LST_STRT', '_DOUBLE', 'READ', LST_STRT_PTR,
     :     ITEMP, STATUS)


      END
