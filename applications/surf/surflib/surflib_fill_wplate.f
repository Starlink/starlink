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
      INTEGER MEASUREMENT       ! Current measurement
      INTEGER MEND              ! End position in WPLATE_OUT
      INTEGER MSTART            ! Start position in WPLATE_OUT
      INTEGER NEXT_MEAS         ! Next measurement
      INTEGER WP_INDEX          ! Position in SCUCD_WPLATE array
      REAL    WPLATE_POS        ! Actual position of wave plate

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     First task is to decide whether we are using SCUCD_WPLATE
*     or calculating the positions internally

      IF (N_WPLATE .GT. 0) THEN
*     Use N_WPLATE

*     Loop over measurements
         WP_INDEX = 1

         DO MEASUREMENT = 1, N_MEAS
            
*     Find where the measurement starts
*     [This should really just start at 1 and from then on be
*     1+MEND]
            MSTART = DEM_PNTR(1,1,1, MEASUREMENT)

*     Find where the measurement ends
            NEXT_MEAS = MEASUREMENT + 1
            IF (NEXT_MEAS .GT. N_MEAS) THEN
               MEND = N_POS
            ELSE
*     Note how the measurement ends one before the start of the
*     next measurement
               MEND = DEM_PNTR(1,1,1,NEXT_MEAS) - 1
            END IF

*     Store the waveplate position
            DO I = MSTART, MEND
               WPLATE_OUT(I) = SCUCD_WPLATE(WP_INDEX)
            END DO

*     Increment the wplate index
            WP_INDEX = WP_INDEX + 1
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

            END DO

         END DO

      END IF

*     Now fill WPLATE_ANG

      DO MEASUREMENT = 1, N_MEAS
         DO INTEGRATION = 1, N_INT

            MSTART = DEM_PNTR(1,1,INTEGRATION, MEASUREMENT)
            WPLATE_ANG(INTEGRATION,MEASUREMENT) = WPLATE_OUT(MSTART)

         END DO
      END DO

      END
