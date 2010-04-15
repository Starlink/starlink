      SUBROUTINE SURFLIB_FILL_WPLATE(N_WPLATE, SCUCD_WPLATE,
     :     N_POS, N_EXP, N_INT, N_MEAS, DEM_PNTR, WPLATE_OUT,
     :     WPLATE_ANG, STATUS)
*+
*  Name:
*     SURFLIB_FILL_WPLATE

*  Purpose:
*     Populate the WavePlate array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_FILL_WPLATE(USE_WP, N_WPLATE, SCUCD_WPLATE, N_POS, N_EXP,
*    :     N_INT, N_MEAS, DEM_PNTR, WPLATE_OUT, WPLATE_ANG, STATUS)

*  Description:
*     Populate an array containing the waveplate angle for each
*     sample. This can be calculated in two ways. If the SCUCD_WPLATE
*     array is present (N_WPLATE>0), then this array contains the waveplate
*     position for each measurement. If this array is not present
*     the Waveplate angle is assumed to start at 0 degrees and increment
*     by 22.5 degrees for each integration. If the number of
*     measurements exceeds the number of positions in the SCUCD_WPLATE
*     array the sequence wraps round to the first value.
*     The final output array will contain a single number (waveplate
*     position) per sample (up to N_POS). The output value will be
*     in degrees.
*
*     Since integrations and measurements are stored sequentially
*     in a data file (in DEM_PNTR) there is no need to use
*     SCULIB_FIND_SWITCH to determine the start and end points
*     in the data array - use SCULIB_FIND_INT.
*
*     The waveplate angles are also stored in a small
*     array containing a single wave plate angle for each integration
*     and each measurement. This is created to make it easier to
*     extract the individual waveplate positions later on when
*     storing them in output file

*  Arguments:
*     N_WPLATE = INTEGER (Given)
*        Size of SCUCD_WPLATE array. If the value is zero then
*        SCUCD_WPLATE will not be used.
*     SCUCD_WPLATE(N_WPLATE) = REAL (Given)
*        Waveplate positions for each measurement (degrees)
*     N_POS = INTEGER (Given)
*        Number of samples in WPLATE_OUT
*     N_EXP = INTEGER (Given)
*        Number of exposures in DEM_PNTR array
*     N_INT = INTEGER (Given)
*        Number of integrations in DEM_PNTR array
*     N_MEAS= INTEGER (Given)
*        Number of measurements in DEM_PNTR array
*     DEM_PNTR ( 1, N_EXP, N_INT, N_MEAS ) = INTEGER (Given)
*        Start position for each EXP, INT, MEAS
*     WPLATE_OUT ( N_POS ) = REAL (Returned)
*        Waveplate position for each sample (degrees)
*     WPLATE_ANG( N_INT, N_MEAS ) = REAL (Given)
*        Waveplate position for each integration/measurement
*     STATUS = INTEGER (Given & Returned)
*        Global status

*  Authors:
*     Tim Jenness (JACH)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.6  2004/09/01 01:06:58  timj
*     fix uninitialised warnings
*
*     Revision 1.5  2002/10/07 03:15:39  timj
*     Should now correctly calculate waveplate angles even if the observation
*     was aborted.
*
*     Revision 1.4  2000/07/06 00:00:31  timj
*     Check that the return from SCULIB_FIND_INT is valid.
*
*     Revision 1.3  1999/08/03 19:32:50  timj
*     Add copyright message to header.
*
*     Revision 1.2  1999/06/16 21:09:51  timj
*     use SCULIB_FIND_INT
*
*     Revision 1.1  1999/02/27 04:33:18  timj
*     First version
*

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'         ! Status
      INCLUDE 'PRM_PAR'         ! Bad values
      INCLUDE 'MSG_PAR'         ! MSG__

*  Arguments Given:
      INTEGER N_EXP
      INTEGER N_INT
      INTEGER N_MEAS
      INTEGER N_POS
      INTEGER N_WPLATE
      REAL    SCUCD_WPLATE( N_WPLATE )
      INTEGER DEM_PNTR( 1, N_EXP, N_INT, N_MEAS )

*  Arguments Returned:
      REAL    WPLATE_OUT( N_POS )
      REAL    WPLATE_ANG( N_INT, N_MEAS )

*  Status:
      INTEGER STATUS

*  Local Constants:

*  Local variables:
      INTEGER I                 ! Loop counter
      INTEGER INTEGRATION       ! Current integration
      INTEGER LAST_END          ! Last good measurement in loop
      INTEGER MEASUREMENT       ! Current measurement
      INTEGER MEND              ! End position in WPLATE_OUT
      INTEGER MSTART            ! Start position in WPLATE_OUT
      INTEGER NEXT_MEAS         ! Next measurement
      INTEGER WP_INDEX          ! Position in SCUCD_WPLATE array
      REAL    WPLATE_POS        ! Actual position of wave plate

*.

      IF (STATUS .NE. SAI__OK) RETURN

      LAST_END = 0

*     First task is to decide whether we are using SCUCD_WPLATE
*     or calculating the positions internally

      IF (N_WPLATE .GT. 0) THEN
*     Use N_WPLATE

*     Loop over measurements
         WP_INDEX = 1

*     Keep track of last end
         LAST_END = -1

         DO MEASUREMENT = 1, N_MEAS

*     Find where the measurement starts
*     This should all be done in SCULIB_FIND_MEAS (if it existed)
*     [This should really just start at 1 and from then on be
*     1+MEND]
            MSTART = DEM_PNTR(1,1,1, MEASUREMENT)

*     If we have an aborted observation this will return 0
*     and we have to assume MEND is the end of the array (since
*     the system is not designed to restart at a later measurement)
            IF (MSTART .LE. 0) THEN

               CALL MSG_SETI ('M', MEASUREMENT)
               CALL MSG_OUTIF (MSG__NORM, ' ',
     :              'SURFLIB_FILL_WPLATE: no data '//
     :              'for meas ^M', STATUS)


               MSTART = LAST_END + 1
               MEND = N_POS
            ELSE

*     Find where the measurement ends
               NEXT_MEAS = MEASUREMENT + 1
               IF (NEXT_MEAS .GT. N_MEAS) THEN
                  MEND = N_POS
               ELSE
*     Note how the measurement ends one before the start of the
*     next measurement
                  MEND = DEM_PNTR(1,1,1,NEXT_MEAS) - 1

                  IF (MEND .EQ. -1) THEN
                     MEND = N_POS
                  END IF

               END IF

               LAST_END = MEND

            END IF

*     Store the waveplate position
            IF (MSTART .LE. N_POS) THEN
               DO I = MSTART, MEND
                  WPLATE_OUT(I) = SCUCD_WPLATE(WP_INDEX)
               END DO
            END IF

*     Increment the wplate index
            WP_INDEX = WP_INDEX + 1

*     Wrap if the number of waveplates is smaller than the number
*     of measurements
            IF (WP_INDEX .GT. N_WPLATE) WP_INDEX = 1

         END DO

      ELSE
*     Calculate from N_INTEGRATIONS. In this case we simply add 22.5
*     degrees to the waveplate position each time we increment
*     by an integration

*     Start position
         WPLATE_POS = 0.0

         DO MEASUREMENT = 1, N_MEAS
            DO INTEGRATION = 1, N_INT

*     Find integration start and end
               CALL SCULIB_FIND_INT(DEM_PNTR, 1, N_EXP, N_INT,
     :              N_MEAS, N_POS, INTEGRATION, MEASUREMENT,
     :              MSTART, MEND, STATUS)

               IF (MSTART .LE. 0) THEN
                  CALL MSG_SETI ('I', INTEGRATION)
                  CALL MSG_SETI ('M', MEASUREMENT)
                  CALL MSG_OUTIF (MSG__NORM, ' ',
     :                 'SURFLIB_FILL_WPLATE: no data '//
     :                 'for int ^I, meas ^M', STATUS)

*     Fill with bad values starting with the last good position
*     and filling the array. In general this should not happen because
*     on the previous loop we should have assumed we were filling to
*     the length of the array (since MEND would be -1)
*     We would like to LAST the loop at this point....
                  IF (LAST_END .LT. N_POS) THEN
                     DO I = LAST_END+1, N_POS
                        WPLATE_OUT(I) = VAL__BADR
                     END DO
                  END IF

               ELSE


                  IF (MEND .LT. 0) THEN
*     This indicates that the integration was started okay
*     but we aborted during it and so do not know explicitly
*     where it finished but must assume the end of the observation

                     MEND = N_POS
                  END IF

*     Store the last good position [not really useful if
*     we trigger MEND=-1 to indicate N_POS
                  LAST_END = MEND

*     Now fill the waveplate array (angle in degrees)
                  DO I = MSTART, MEND
                     WPLATE_OUT(I) = WPLATE_POS
                  END DO

*     Skip to the next waveplate positions
                  WPLATE_POS = WPLATE_POS + 22.5

*     Stop the waveplate angle going over 360 degrees
                  IF (WPLATE_POS .GE. 360.0) THEN
                     WPLATE_POS = WPLATE_POS - 360.0
                  END IF

               END IF

            END DO

         END DO

      END IF

*     Now fill WPLATE_ANG

      DO MEASUREMENT = 1, N_MEAS
         DO INTEGRATION = 1, N_INT

            MSTART = DEM_PNTR(1,1,INTEGRATION, MEASUREMENT)
            IF (MSTART .GT. 0) THEN
               WPLATE_ANG(INTEGRATION,MEASUREMENT) = WPLATE_OUT(MSTART)
            ELSE
               WPLATE_ANG(INTEGRATION,MEASUREMENT) = VAL__BADR
            END IF
         END DO
      END DO

      END
