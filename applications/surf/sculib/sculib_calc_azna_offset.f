      SUBROUTINE SCULIB_CALC_AZNA_OFFSET(OUT_COORDS,
     :     RA_CENTRE, DEC_CENTRE, LST, LAT_OBS,
     :     OFFSET_X, OFFSET_Y,
     :     N_POINT, MAX_POINT, POINT_LST, POINT_DAZ, POINT_DEL,
     :     NUM_CHAN, NUM_ADC, N_BOL, BOL_CHAN, BOL_ADC, U3, U4,
     :     U3_CENTRE, U4_CENTRE, X_BOL, Y_BOL, STATUS)
*+
*  Name:
*     SCULIB_CALC_NA_OFFSET

*  Purpose:
*     Calculate the bolometer offsets in nasmyth coordinates
*     or AzEl offsets.

*  Invocation:
*      SUBROUTINE SCULIB_CALC_AZNA_OFFSET(OUT_COORDS,
*    :     RA_CENTRE, DEC_CENTRE, LST, LAT_OBS,
*    :     OFFSET_X, OFFSET_Y,
*    :     N_POINT, MAX_POINT, POINT_LST, POINT_DAZ, POINT_DEL,
*    :     NUM_CHAN, NUM_ADC, N_BOL, BOL_CHAN, BOL_ADC, U3, U4,
*    :     U3_CENTRE, U4_CENTRE, X_BOL, Y_BOL, STATUS)

*  Description:
*     This routine calculates the nasmyth offset of a specified set
*     of bolometers. No astronomical coordinate transformations are
*     necessary.
*           BOL_X(I) = U3(I) - OFFSET_X - U3_CENTRE
*     and similarly for Y.
*     Azimuth-Elevation offsets are then calculated by using the
*     RA,Dec,LST to calculate the actual elevation of the telescope
*     and then rotating U3 and U4 to dAz dEl.


*  Arguments:
*     OUT_COORDS = CHARACTER * (*) (Given)
*          Output coordinate system (NA, AZ)
*     RA_CENTRE              = DOUBLE PRECISION (Given)
*           the apparent RA of the `centre' (radians)
*     DEC_CENTRE             = DOUBLE PRECISION (Given)
*           the apparent dec of the `centre' (radians)
*     LST                    = DOUBLE PRECISION (Given)
*           the local sidereal time (radians)
*     LAT_OBS                = DOUBLE PRECISION (Given)
*           the latitude of the observatory (radians)
*     OFFSET_X = REAL (Given)
*          the x offset of the array origin from the 'centre'
*     OFFSET_Y = REAL (Given)
*          the y offset of the array origin from the 'centre'
*     N_POINT                = INTEGER (Given)
*           number of elements used in pointing correction arrays
*     MAX_POINT              = INTEGER (Given)
*           dimension of pointing correction arrays
*     POINT_LST (MAX_POINT)  = DOUBLE PRECISION (Given)
*           LST of measured corrections (radians)
*     POINT_DAZ (MAX_POINT)  = REAL (Given)
*           correction to be added in azimuth (arcsec)
*     POINT_DEL (MAX_POINT)  = REAL (Given)
*           correction to be added in elevation (arcsec)
*     NUM_CHAN               = INTEGER (Given)
*           the number of channels per A/D card
*     NUM_ADC                = INTEGER (Given)
*           the number of A/D cards
*     N_BOL                  = INTEGER (Given)
*           the actual number of bolometers
*     BOL_CHAN (N_BOL)       = INTEGER (Given)
*           channel numbers of bolometers
*     BOL_ADC (N_BOL)        = INTEGER (Given)
*           ADC numbers of bolometers
*     U3 (NUM_ADC,NUM_CHAN)  = REAL (Given)
*           the U3 offsets of the bolometers (arcsec)
*     U4 (NUM_ADC,NUM_CHAN)  = REAL (Given)
*           the U4 offsets of the bolometers (arcsec)
*     U3_CENTRE              = REAL (Given)
*           the U3 offset of the tracking `centre' on the array (arcsec)
*     U4_CENTRE              = REAL (Given)
*           the U4 offset of the tracking `centre' on the array (arcsec)
*     X_BOL (N_BOL)         = DOUBLE PRECISION (Returned)
*           the X coordinate of the bolometer (radians)
*     Y_BOL (N_BOL)        = DOUBLE PRECISION (Returned)
*           the Y coordinate of the bolometer (radians)

*  Notes:
*     - Only NASMYTH supported

*  Implementation Status:
*     - Pointing offsets are ignored

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}
 
*  History:
*     1996 October 13(TIMJ):
*       Original version
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions
 
*  Arguments Given:
      INTEGER NUM_CHAN
      INTEGER NUM_ADC
      INTEGER N_BOL

      INTEGER BOL_ADC(N_BOL)
      INTEGER BOL_CHAN(N_BOL)
      DOUBLE PRECISION DEC_CENTRE
      DOUBLE PRECISION LAT_OBS
      DOUBLE PRECISION LST
      INTEGER          MAX_POINT
      INTEGER          N_POINT
      REAL    OFFSET_X
      REAL    OFFSET_Y
      CHARACTER * (*) OUT_COORDS
      DOUBLE PRECISION POINT_LST (MAX_POINT)
      REAL             POINT_DAZ (MAX_POINT)
      REAL             POINT_DEL (MAX_POINT)
      DOUBLE PRECISION RA_CENTRE
      REAL    U3(NUM_CHAN, NUM_ADC)
      REAL    U3_CENTRE
      REAL    U4(NUM_CHAN, NUM_ADC)
      REAL    U4_CENTRE

*  Arguments Returned:
      DOUBLE PRECISION X_BOL(N_BOL)
      DOUBLE PRECISION Y_BOL(N_BOL)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants :
      DOUBLE PRECISION ARCSEC2RAD         ! arcsec 2 radians conversion
      PARAMETER (ARCSEC2RAD = 4.84813681110D-6)

*  Local Variables:
      INTEGER          ADC                ! ADC number in loop
      INTEGER          BOL                ! Loop counter
      DOUBLE PRECISION COS_E              ! sin (E)
      INTEGER          CHAN               ! Channel number in loop
      LOGICAL          DONE               ! .TRUE. if pointing corrections
                                          ! straddling LST of measured point
                                          ! have been found
      DOUBLE PRECISION E                  ! elevation of `centre' (radians)
      DOUBLE PRECISION HOUR_ANGLE         ! hour angle (radians)
      INTEGER I
      DOUBLE PRECISION P_DAZ              ! applied az pointing correction
                                          ! (arcsec)
      DOUBLE PRECISION P_DEL              ! applied el pointing correction
                                          ! (arcsec)
      DOUBLE PRECISION SIN_E              ! sin (E)
      DOUBLE PRECISION U3_OFF             ! bolometer offset from `centre'
                                          ! (radians)
      DOUBLE PRECISION U4_OFF             ! bolometer offset from `centre'
                                          ! (radians)

*.

      IF (STATUS .NE. SAI__OK) RETURN

* Check the OUT_COORDS

      IF (OUT_COORDS.NE.'NA'.AND.OUT_COORDS.NE.'AZ') THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC ('COORDS', OUT_COORDS)
         CALL ERR_REP (' ', 'SCULIB_CALC_AZNA_COORDS: bad value for '//
     :     'OUT_COORDS - ^COORDS', STATUS)
      END IF


*  calculate the pointing offset for the time of the measurement
 
      IF (STATUS .EQ. SAI__OK) THEN
         IF (N_POINT .GT. 0) THEN
            IF (LST .LE. POINT_LST(1)) THEN
               P_DAZ = DBLE (POINT_DAZ (1))
               P_DEL = DBLE (POINT_DEL (1)) 
            ELSE IF (LST .GE. POINT_LST(N_POINT)) THEN
               P_DAZ = DBLE (POINT_DAZ (N_POINT))
               P_DEL = DBLE (POINT_DEL (N_POINT))
            ELSE
 
*  look for correction points that straddle the LST of the measurement, then
*  linear interpolate
               DONE = .FALSE.
               I = 1
 
               DO WHILE (.NOT. DONE)
                  IF ((LST .GT. POINT_LST(I))   .AND.
     :                (LST .LE. POINT_LST(I+1))) THEN
                     P_DAZ = DBLE (POINT_DAZ(I)) + 
     :                 (LST - POINT_LST(I)) *
     :                 DBLE (POINT_DAZ(I+1) - POINT_DAZ(I)) /
     :                 (POINT_LST(I+1) - POINT_LST(I))
                     P_DEL = DBLE (POINT_DEL(I)) + 
     :                 (LST - POINT_LST(I)) *
     :                 DBLE (POINT_DEL(I+1) - POINT_DEL(I)) /
     :                 (POINT_LST(I+1) - POINT_LST(I))
                  ELSE
                     I = I + 1
                  END IF
               END DO
 
            END IF
         END IF
      END IF

*  calculate the current elevation of the telescope
 
      IF (STATUS .EQ. SAI__OK) THEN
         HOUR_ANGLE = LST - RA_CENTRE
         SIN_E = SIN (LAT_OBS) * SIN (DEC_CENTRE) + 
     :        COS (LAT_OBS) * COS (DEC_CENTRE) * COS (HOUR_ANGLE)
         E = ASIN (SIN_E)
         
         COS_E = COS (E)


         
         DO BOL = 1, N_BOL
            
*     calculate the Nasmyth offset
            
            CHAN = BOL_CHAN (BOL)
            ADC = BOL_ADC (BOL)

            U3_OFF = (DBLE(U3(CHAN,ADC)) - DBLE(U3_CENTRE) -
     :           DBLE(OFFSET_X)) * ARCSEC2RAD
            U4_OFF = (DBLE(U4(CHAN,ADC)) - DBLE(U4_CENTRE) -
     :           DBLE(OFFSET_Y)) * ARCSEC2RAD

* Transfer to AZEL if necessary
            IF (OUT_COORDS.EQ.'NA') THEN
               X_BOL(BOL) = U3_OFF
               Y_BOL(BOL) = U4_OFF

            ELSE IF (OUT_COORDS.EQ.'AZ') THEN
               Y_BOL(BOL) = -(U3_OFF * SIN_E) + (U4_OFF * COS_E)
               X_BOL(BOL) =  (U3_OFF * COS_E) + (U4_OFF * SIN_E)

*     Add AZ pointing offsets
               IF (N_POINT .GT. 0 ) THEN
                  X_BOL(BOL) = X_BOL(BOL) +
     :                 (P_DAZ * COS(Y_BOL(BOL)) * ARCSEC2RAD)
                  Y_BOL(BOL) = Y_BOL(BOL) + (P_DEL * ARCSEC2RAD)
               END IF

            END IF


         END DO

      END IF

      END
