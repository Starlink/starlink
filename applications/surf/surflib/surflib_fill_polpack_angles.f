      SUBROUTINE SURFLIB_FILL_POLPACK_ANGLES( MAX_FILE, MAX_INT,
     :     MAX_MEAS, N_FILE, N_INT, N_MEAS,
     :     WPLATE, ANGROT, FAST_AXIS, ANG_INT, ANG_MEAS, STATUS)
*+
*  Name:
*     SURFLIB_FILL_POLPACK_ANGLES

*  Purpose:
*     Average

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_FILL_POLPACK_ANGLES( MAX_FILE, MAX_INT,
*    :     MAX_MEAS, N_FILE, N_INT, N_MEAS,
*    :     WPLATE, ANGROT, FAST_AXIS, ANG_INT, ANG_MEAS, STATUS)

*  Description:
*     Copy the waveplate and rotation angles from WPLATE and ANGROT
*     (Which have dimensions (N_INT,N_MEAS) to a 2-dim array containing
*     the angles (waveplate and rotation) for each integration and
*     (in a separate array) the angles for each measurement.
*     Note that since the rotation angles are stored in the file
*     as one per integration, they need to be averaged over the
*     integration in order to calculate the correct value for the
*     measurement. ANG_MEAS and ANG_INT should be set to VAL__BADR
*     before entry to this routine.

*  Arguments:
*     MAX_FILE = INTEGER (Given)
*       First Dimension of ANG_INT,ANG_MEAS
*     MAX_INT = INTEGER (given)
*       Second dimension of ANG_INT
*     MAX_MEAS = INTEGER (given)
*       Second dimension of ANG_MEAS
*     N_FILE = INTEGER (Given)
*       Current slice in ANG_INT, ANG_MEAS
*     N_INT = INTEGER (Given)
*       Number of integrations per measurement
*     N_MEAS = INTEGER (Given)
*       Number of measurements
*     WPLATE(N_INT, N_MEAS) = REAL (Given)
*       Waveplate angles for each integration
*     ANGROT(N_INT, N_MEAS) = REAL (Given)
*       Waveplate angles for each integration
*     FAST_AXIS = REAL (Given)
*       Angle of the fast axis of the waveplate and the zero position
*       of the waveplate. Added to each waveplate angle to calculate
*       the true angle of the waveplate.
*     ANG_INT(MAX_FILE, MAX_INT, 2) = REAL (Returned)
*       Waveplate and rotation angles for each integration
*       1=Waveplate, 2=Rotation. The waveplate angle is corrected for
*       fast axis.
*     ANG_MEAS(MAX_FILE, MAX_MEAS, 2) = REAL (Returned)
*       Waveplate and rotation angles for each measurement.
*       1=Waveplate, 2=Rotation. The waveplate angle is corrected for
*       fast axis. The rotation angle is average over each integration
*       during the measurement. The waveplate angle is set to bad
*       if it is not constant for the given measurement.
*     STATUS = INTEGER (Given and Returned)
*       Global status

*  Authors:
*     Tim Jenness (JACH)


*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.3  1999/08/03 19:32:50  timj
*     Add copyright message to header.
*
*     Revision 1.2  1999/05/12 22:46:10  timj
*     Move array initialisation code out of this routine to the calling routine
*
*     Revision 1.1  1999/02/27 04:33:14  timj
*     First version
*

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'         ! Status
      INCLUDE 'PRM_PAR'         ! For VAL__BAD and VAL__NB

*  Arguments Given:
      INTEGER MAX_FILE
      INTEGER MAX_INT
      INTEGER MAX_MEAS
      INTEGER N_FILE
      INTEGER N_INT
      INTEGER N_MEAS
      REAL    WPLATE(N_INT, N_MEAS)
      REAL    ANGROT(N_INT, N_MEAS)
      REAL    FAST_AXIS

*  Arguments Returned:
      REAL    ANG_INT(MAX_FILE, MAX_INT, 2)
      REAL    ANG_MEAS(MAX_FILE, MAX_MEAS, 2)

*  Global Status:
      INTEGER STATUS

*  Local Variables:
      REAL    ANGROT_MEAN       ! Mean rotation angle over the measurement
      INTEGER INTEGRATION       ! Integration loop counter
      INTEGER ITEMP             ! Scratch integer
      INTEGER MEASUREMENT       ! Measurement loop counter
      INTEGER NUM_INTS          ! Total number of integrations so far in loop
      REAL    WPLATE_REF        ! Reference waveplate angle for measurement

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Check that N_INT and N_MEAS are in bounds

*     First check the number of integrations
      IF (N_INT .GT. MAX_INT) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('I',N_INT)
         CALL MSG_SETI('M',MAX_INT)
         CALL ERR_REP(' ','SURFLIB_FILL_POLPACK_ANGLES: Number of '//
     :        'integrations (^I) exceeds maximum allowed (^M)',
     :        STATUS)
      END IF

*     Check measurements
      IF (N_MEAS .GT. MAX_MEAS) THEN
         IF (STATUS .EQ. SAI__OK) STATUS = SAI__ERROR
         CALL MSG_SETI('M',N_MEAS)
         CALL MSG_SETI('MX',MAX_MEAS)
         CALL ERR_REP(' ','SURFLIB_FILL_POLPACK_ANGLES: Number of '//
     :        'measurements (^M) exceeds maximum allowed (^MX)',
     :        STATUS)
      END IF

*     Now check the TOTAL number of integrations
      ITEMP = N_INT * N_MEAS
      IF (ITEMP .GT. MAX_INT * MAX_MEAS) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('I',N_INT * N_MEAS)
         CALL MSG_SETI('M',MAX_INT * MAX_MEAS)
         CALL ERR_REP(' ','SURFLIB_FILL_POLPACK_ANGLES: Total Number '//
     :        'of integrations (^I) exceeds maximum allowed (^M)',
     :        STATUS)
      END IF


*     Return if needed
      IF (STATUS .NE. SAI__OK) RETURN

*     Start routine proper

*     Set the integration counter to zero
      NUM_INTS = 0

*     Now we can start copying the input data to the output

      DO MEASUREMENT = 1, N_MEAS

*     Reset the mean rotation angle per measurement
         ANGROT_MEAN = 0.0

*     Store reference waveplate position
         WPLATE_REF = WPLATE(1,MEASUREMENT) + FAST_AXIS

*     Loop over integrations
         DO INTEGRATION = 1, N_INT

*     Increment the integration counter
            NUM_INTS = NUM_INTS + 1

*     Read the waveplate position and store it
            ANG_INT(N_FILE, NUM_INTS, 1) =
     :           WPLATE(INTEGRATION, MEASUREMENT)
     :           + FAST_AXIS

*     Read the rotation angle and store it
            ANG_INT(N_FILE,NUM_INTS,2) =
     :           ANGROT(INTEGRATION, MEASUREMENT)

*     Check to see whether WPLATE is the same as the reference
*     position (if it isnt we set reference to bad so that it
*     wont be stored in ANG_MEAS - a non-constant waveplate
*     position is meaningless)
            IF (ANG_INT(N_FILE,NUM_INTS,1) .NE. WPLATE_REF) THEN
               WPLATE_REF = VAL__BADR
            END IF

*     Increment ANGROT_MEAN
            ANGROT_MEAN = ANGROT_MEAN + ANGROT(INTEGRATION,MEASUREMENT)

         END DO

*     Store Measurements waveplate angle (the reference angle
         ANG_MEAS(N_FILE,MEASUREMENT,1) = WPLATE_REF

*     Store the average rotation angle
         ANG_MEAS(N_FILE,MEASUREMENT,2) = ANGROT_MEAN / N_INT

      END DO

      END
