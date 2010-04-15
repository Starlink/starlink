      SUBROUTINE SCULIB_GET_BOL_DESC(LOC, NUM_CHAN, NUM_ADC, N_BOL,
     :     BOL_TYPE, BOL_DU3, BOL_DU4, BOL_ADC, BOL_CHAN,
     :     STATUS)
*+
*  Name:
*     SCULIB_GET_BOL_DESC

*  Purpose:
*     Get the bolometer description arrays and check their dimensions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_GET_BOL_DESC(LOC, NUM_CHAN, NUM_ADC, BOL_TYPE,
*    :     BOL_DU3, BOL_DU4, BOL_ADC, BOL_CHAN, STATUS)

*  Description:
*     This routine obtains the bolometer description arrays. ie:
*     BOL_TYPE, BOL_ADC, BOL_CHAN, BOL_DU3 and BOL_DU4 arrays.
*     The dimensions of these arrays are checked and an error returned
*     if necessary.

*  Arguments:
*     LOC = _CHARACTER (Given)
*        Locator to the SCUBA extension
*     NUM_CHAN = _INTEGER (Given)
*        Number of SCUBA channels per ADC
*     NUM_ADC = _INTEGER (Given)
*        Number of ADC cards
*     N_BOL = _INTEGER (Given)
*        Number of bolometers
*     BOL_TYPE = _CHARACTER*20() (Returned)
*        Bolometer types
*     BOL_DU3  = _REAL() (Returned)
*        du3 Nasmyth coordinated of bolometers
*     BOL_DU4  = _REAL() (Returned)
*        du4 Nasmyth coordinated of bolometers
*     BOL_ADC  = _INTEGER() (Returned)
*        A/D numbers of bolometers measured
*     BOL_CHAN  = _INTEGER() (Returned)
*        Channel numbers of bolometers measured
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
      INTEGER       NUM_ADC
      INTEGER       NUM_CHAN
      INTEGER       N_BOL

*  Arguments Returned:
      INTEGER       BOL_ADC (NUM_CHAN * NUM_ADC)
      INTEGER       BOL_CHAN (NUM_CHAN * NUM_ADC)
      REAL          BOL_DU3 (NUM_CHAN, NUM_ADC)
      REAL          BOL_DU4 (NUM_CHAN, NUM_ADC)
      CHARACTER*(*) BOL_TYPE (NUM_CHAN, NUM_ADC)

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER   MAX_DIM          ! max number of dims in array
      PARAMETER (MAX_DIM = 2)

*  Local Variables:
      INTEGER DIM (MAX_DIM)      ! actual array dimensions
      INTEGER DIMX (MAX_DIM)     ! expected array dimensions
      INTEGER ITEMP              ! Dummy variable
      INTEGER NDIM               ! number of dimensions
*.
      IF (STATUS .NE. SAI__OK) RETURN


*     Set dimensionality
      NDIM = 2
      DIMX (1) = NUM_CHAN
      DIMX (2) = NUM_ADC

*     BOL_TYPE

      CALL CMP_GETNC (LOC, 'BOL_TYPE', NDIM, DIMX, BOL_TYPE,
     :     DIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2)                 .OR.
     :        (DIM(1) .NE. NUM_CHAN) .OR.
     :        (DIM(2) .NE. NUM_ADC)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL ERR_REP (' ', 'SCULIB_GET_BOL_DESC: .SCUBA.BOL_TYPE '//
     :           'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2',
     :           STATUS)
         END IF
      END IF

*     BOL_DU3

      CALL CMP_GETNR (LOC, 'BOL_DU3', NDIM, DIMX,
     :     BOL_DU3, DIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2)                 .OR.
     :        (DIM(1) .NE. NUM_CHAN) .OR.
     :        (DIM(2) .NE. NUM_ADC)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL ERR_REP (' ', 'SCULIB_GET_BOL_DESC: '//
     :           '.SCUBA.BOL_DU3 array has bad dimensions - '//
     :           '(^NDIM) ^DIM1 ^DIM2', STATUS)
         END IF
      END IF

*     BOL_DU4

      CALL CMP_GETNR (LOC, 'BOL_DU4', NDIM, DIMX,
     :     BOL_DU4, DIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2)                 .OR.
     :        (DIM(1) .NE. NUM_CHAN) .OR.
     :        (DIM(2) .NE. NUM_ADC)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL ERR_REP (' ', 'SCULIB_GET_BOL_DESC: '//
     :           '.SCUBA.BOL_DU4 array has bad dimensions - '//
     :           '(^NDIM) ^DIM1 ^DIM2', STATUS)
         END IF
      END IF

*     BOL_CHAN

      CALL CMP_GET1I (LOC, 'BOL_CHAN', NUM_CHAN * NUM_ADC,
     :     BOL_CHAN, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_BOL) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_GET_BOL_DESC: dimension '//
     :           'of .SCUBA.BOL_CHAN does not match number of '//
     :           'expected bolometers.', STATUS)
         END IF
      END IF

*     BOL_ADC

      CALL CMP_GET1I(LOC, 'BOL_ADC', NUM_CHAN * NUM_ADC,
     :     BOL_ADC, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_BOL) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_GET_BOL_DESC: dimension '//
     :           'of .SCUBA.BOL_ADC does not match main data '//
     :           'array', STATUS)
         END IF
      END IF


      END
