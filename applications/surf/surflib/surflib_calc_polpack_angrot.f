      SUBROUTINE SURFLIB_CALC_POLPACK_ANGROT(N_POS,
     :     N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :     DEM_PNTR, ANGROT_IN, ANGROT_OUT, ANGROT_VAR, STATUS)
*+
*  Name:
*     SURFLIB_CALC_POLPACK_ANGROT

*  Purpose:
*     Average rotations angles over integration

*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL SURFLIB_CALC_POLPACK_ANGROT(N_POS, N_EXPOSURES,
*     :    N_INTEGRATIONS, N_MEASUREMENTS, DEM_PNTR, ANGROT_IN,
*     :    ANGROT_OUT, STATUS)

*  Description:
*     This routine calculates the mean rotation angle for each
*     integration. The input data contains one angle per sample
*     and this is averaged to calculate the rotation angle per
*     integration.
*     The input angle is the rotation angle (anti-clockwise)
*     between the RA frame and the nasmyth (waveplate) frame.
*     The output angle is the angle between the waveplate 0 and
*     the X PIXEL axis (not the RA axis)
*     Since the supplied angle is the angle from the Y-axis
*     (ie all angles are from Nasmyth Y axis) we need to add
*     90 degrees

*  Arguments:
*     N_POS = INTEGER (Given)
*        Size of ANGROT_IN array
*     N_EXPOSURES = INTEGER (Given)
*        Number of exposures in DEM_PNTR
*     N_INTEGRATIONS = INTEGER (Given)
*        Number of integrations in DEM_PNTR
*     N_MEASUREMENTS = INTEGER (Given)
*        Number of measurements in DEM_PNTR
*     DEM_PNTR(1,N_EXPOSURES,N_INTEGRATIONS,N_MEASUREMENTS) = INTEGER (Given)
*        Start position for each EXP, INT, MEAS
*     ANGROT_IN (N_POS) = REAL (Given)
*        Rotation for each sample (N_POS)
*     ANGROT_OUT (N_INTEGRATIONS, N_MEASUREMENTS) = REAL (Returned)
*        Rotation angle averaged over each integration
*     ANGROT_VAR (N_INTEGRATIONS, N_MEASUREMENTS) = REAL (Returned)
*        Variance on averaged angle
*     STATUS = INTEGER (Given & Returned)
*        Global status

*  Authors:
*     Tim Jenness (JACH)

*  History:
*     $Log$
*     Revision 1.1  1999/02/27 04:33:10  timj
*     First version
*

*-

*  Type Definitions:
      IMPLICIT NONE
 
*  Global constants:
      INCLUDE 'SAE_PAR'         ! Status
      INCLUDE 'PRM_PAR'         ! For VAL__BAD and VAL__NB

*  Arguments Given:
      INTEGER N_POS
      INTEGER N_EXPOSURES
      INTEGER N_INTEGRATIONS
      INTEGER N_MEASUREMENTS
      INTEGER DEM_PNTR(1,N_EXPOSURES,N_INTEGRATIONS,N_MEASUREMENTS)
      REAL    ANGROT_IN(N_POS)

*  Arguments Returned:
      REAL    ANGROT_OUT(N_INTEGRATIONS, N_MEASUREMENTS)
      REAL    ANGROT_VAR(N_INTEGRATIONS, N_MEASUREMENTS)

*  Global status:
      INTEGER STATUS

*  Local Constants:
      BYTE BADBIT               ! Dummy bad bits mask
      PARAMETER (BADBIT = 0)
      REAL POLPACK_ANG          ! Angle required to convert to POLPACK
      PARAMETER (POLPACK_ANG = 90.0) ! ANGROT definition

*  Local variables:
      INTEGER IEND              ! End index of integration
      INTEGER INTEGRATION       ! Current integration
      INTEGER ISTART            ! Start index of integration
      INTEGER ITEMP             ! Scratch int
      DOUBLE PRECISION MEAN     ! Mean of angle
      INTEGER MEASUREMENT       ! Current measurement
      DOUBLE PRECISION MEDIAN   ! Median of angle
      INTEGER N_PTS             ! Number of points in average
      INTEGER NEXT_INT          ! Next integration number
      INTEGER NEXT_MEAS         ! Next measurement
      INTEGER QSORT_END         ! End of sorted data
      INTEGER QSORT_PTR         ! Start of sorted data
      INTEGER SCRATCHQ_END      ! End of scratch quality array
      INTEGER SCRATCHQ_PTR      ! Start of scratch quality array
      DOUBLE PRECISION STDEV    ! Standard deviation of angle
      DOUBLE PRECISION SUM      ! Sum of angle data
      DOUBLE PRECISION SUMSQ    ! sum of squares of angle data      

*.
      
      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise pointers
      SCRATCHQ_PTR = 0
      SCRATCHQ_END = 0
      QSORT_PTR    = 0
      QSORT_END    = 0
      
*     Loop over integrations

      DO MEASUREMENT = 1, N_MEASUREMENTS
         DO INTEGRATION = 1, N_INTEGRATIONS

*     Find the start position in ANGROT_IN
            ISTART = DEM_PNTR(1,1,INTEGRATION, MEASUREMENT)

*     Find the end position of the integration
*     Have to find the next integration
            NEXT_INT = INTEGRATION + 1
            NEXT_MEAS = MEASUREMENT
            IF (NEXT_INT .GT. N_INTEGRATIONS) THEN
               NEXT_INT = 1
               NEXT_MEAS = NEXT_MEAS + 1
            END IF

            IF (NEXT_MEAS .GT. N_MEASUREMENTS) THEN
               IEND = N_POS
            ELSE
               IEND = DEM_PNTR(1,1,NEXT_INT, NEXT_MEAS) - 1
            END IF

*     Allocate some dummy memory for the quality array [I really
*     need a flag in STATR to not use QUALITY] and for the sorted
*     data. Note that I cant allocate this outside of the loop
*     since for scan maps the number of points in an integration
*     changes depending on the length of the scan

            N_PTS = IEND - ISTART + 1

            CALL SCULIB_MALLOC(N_PTS * VAL__NBUB, SCRATCHQ_PTR, 
     :           SCRATCHQ_END, STATUS)
            CALL SCULIB_MALLOC(N_PTS * VAL__NBR, QSORT_PTR, 
     :           QSORT_END, STATUS)

*     Fill the byte array with 0
            IF (STATUS .EQ. SAI__OK) THEN
               CALL SCULIB_CFILLB(N_PTS, 0, %VAL(SCRATCHQ_PTR))
            END IF

*     Find the mean of the input angles
            MEAN = VAL__BADD
            STDEV = VAL__BADD

            CALL SCULIB_STATR(N_PTS, -1.0, ANGROT_IN(ISTART), 
     :           %VAL(SCRATCHQ_PTR), BADBIT, ITEMP, MEAN, MEDIAN, 
     :           SUM, SUMSQ, STDEV, %VAL(QSORT_PTR) ,STATUS)


*     Fill the output array
            IF (MEAN .EQ. VAL__BADD) THEN
               ANGROT_OUT(INTEGRATION,MEASUREMENT) = VAL__BADR
            ELSE
               ANGROT_OUT(INTEGRATION,MEASUREMENT) = SNGL(MEAN)
     :              + POLPACK_ANG
            END IF

            IF (STDEV .EQ. VAL__BADD) THEN
               ANGROT_VAR(INTEGRATION,MEASUREMENT) = VAL__BADR
            ELSE
               ANGROT_VAR(INTEGRATION,MEASUREMENT) = SNGL(STDEV ** 2)
            END IF

*     Free scratch memory
            CALL SCULIB_FREE('SCRATCHQ', SCRATCHQ_PTR, SCRATCHQ_END,
     :           STATUS)
            CALL SCULIB_FREE('QSORT',QSORT_PTR, QSORT_END, STATUS)


         END DO
      END DO

      END
