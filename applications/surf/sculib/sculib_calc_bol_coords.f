*+  SCULIB_CALC_BOL_COORDS - calculate apparent RA, Dec of bolometers
      SUBROUTINE SCULIB_CALC_BOL_COORDS (RA_CENTRE, DEC_CENTRE, LST,
     :  LAT_OBS, OFFSET_COORDS, OFFSET_X, OFFSET_Y, ROTATION, 
     :  N_POINT, MAX_POINT, POINT_LST, POINT_DAZ, POINT_DEL,
     :  NUM_CHAN, NUM_ADC, N_BOL, BOL_CHAN, BOL_ADC, U3, U4, U3_CENTRE,
     :  U4_CENTRE, RA_BOL, DEC_BOL, STATUS)
*    Description :
*     This routine calculates the apparent RA and dec of a specified set
*     of bolometers. It does this by:-
*
*          calculating the offsets that must be added to the bolometer
*          positions to cater for the fact that the origin of the bolometer
*          coordinate system may be offset from the `centre' specified by
*          RA_CENTRE , DEC_CENTRE. 
*
*          Three types of offsets may be added; Nasmyth offsets which are 
*          simply subtracted from the bolometer coordinates, azimuth offsets
*          which are subtracted from the bolometer positions in az and el, 
*          or offsets in a coordinate system fixed relative to the sky, 
*          rotated relative to apparent RA,Dec by the angle ROTATION. The
*          latter are added to the bolometer coordinates when they are in
*          the form of tangent plane coords in apparent RA,Dec.
*
*          working out the elevation and parallactic angle for the sidereal
*          time and apparent RA, dec of the `centre'.
*
*          calculating the Nasmyth offset of each bolometer, then rotating
*          them into tangent plane offsets in azimuth and elevation.
*
*          adding pointing corrections in azimuth and elevation, then
*          rotating the coords into the apparent RA,Dec tangent plane.
*
*          calling SLA_DTP2S to work out the apparent RA, dec of the offset
*          positions.
*
*    Invocation :
*     CALL SCULIB_CALC_BOL_COORDS (RA_CENTRE, DEC_CENTRE, LST,
*    :  LAT_OBS, OFFSET_COORDS, OFFSET_X, OFFSET_Y, ROTATION, 
*    :  N_POINT, MAX_POINT, POINT_LST, POINT_DAZ, POINT_DEL,
*    :  NUM_CHAN, NUM_ADC, N_BOL, BOL_CHAN, BOL_ADC, U3, U4, U3_CENTRE,
*    :  U4_CENTRE, RA_BOL, DEC_BOL, STATUS)
*    Parameters :
*     RA_CENTRE              = DOUBLE PRECISION (Given)
*           the apparent RA of the `centre' (radians)
*     DEC_CENTRE             = DOUBLE PRECISION (Given)
*           the apparent dec of the `centre' (radians)
*     LST                    = DOUBLE PRECISION (Given)
*           the local sidereal time (radians)
*     LAT_OBS                = DOUBLE PRECISION (Given)
*           the latitude of the observatory (radians)
*     OFFSET_COORDS          = CHARACTER*(*) (Given)
*           the coordinate system of the offset of the array origin from
*           the `centre'; NA or RD 
*     OFFSET_X               = REAL (Given)
*           the x offset of the array origin from the `centre'
*           (arcseconds)
*     OFFSET_Y               = REAL (Given)
*           the y offset of the array origin from the `centre'
*           (arcseconds)
*     ROTATION               = DOUBLE PRECISION (Given)
*           for OFFSET_COORDS other than NA, this gives the angle
*           from N in the offset coordinate system to N in apparent
*           RA,Dec (radians, increasing clockwise)
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
*     RA_BOL (N_BOL)         = DOUBLE PRECISION (Returned)
*           the apparent RA of the bolometer (radians)
*     DEC_BOL (N_BOL)        = DOUBLE PRECISION (Returned)
*           the apparent dec of the bolometer (radians)
*     STATUS                 = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (jfl@roe.ac.uk)
*    History :
*     $Id$
*     1-AUG-1995: Original version.
*    15-JUL-1996: Jiggle offsets subtracted rather than added (JFL).
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      DOUBLE PRECISION RA_CENTRE
      DOUBLE PRECISION DEC_CENTRE
      DOUBLE PRECISION LST
      DOUBLE PRECISION LAT_OBS
      CHARACTER*(*)    OFFSET_COORDS
      REAL             OFFSET_X
      REAL             OFFSET_Y
      DOUBLE PRECISION ROTATION
      INTEGER          N_POINT
      INTEGER          MAX_POINT
      DOUBLE PRECISION POINT_LST (MAX_POINT)
      REAL             POINT_DAZ (MAX_POINT)
      REAL             POINT_DEL (MAX_POINT)
      INTEGER          NUM_CHAN
      INTEGER          NUM_ADC
      INTEGER          N_BOL
      INTEGER          BOL_CHAN (N_BOL)
      INTEGER          BOL_ADC (N_BOL)
      REAL             U3 (NUM_CHAN,NUM_ADC)
      REAL             U4 (NUM_CHAN,NUM_ADC)
      REAL             U3_CENTRE
      REAL             U4_CENTRE
*    Import-Export :
*    Export :
      DOUBLE PRECISION RA_BOL (N_BOL)
      DOUBLE PRECISION DEC_BOL (N_BOL)
*    Status :
      INTEGER          STATUS
*    External references :
*    Global variables :
*    Local Constants :
      DOUBLE PRECISION ARCSEC2RAD         ! arcsec 2 radians conversion
      PARAMETER (ARCSEC2RAD = 4.84813681110D-6)
      DOUBLE PRECISION PI                 !
      PARAMETER (PI = 3.14159265359)
*    Local variables :
      INTEGER          ADC                ! ADC number of bolometer
      DOUBLE PRECISION AZ_OFFSET          ! offset in az (arcsec)
      DOUBLE PRECISION DAZ                ! azimuth of point (radians)
      INTEGER          BOL                ! bolometer index in DO loop
      DOUBLE PRECISION BOL_XOFF           ! tangent plane offset in apparent
                                          ! RA, Dec system (radians)
      DOUBLE PRECISION BOL_YOFF           ! tangent plane offset in apparent
                                          ! RA, Dec system (radians)
      INTEGER          CHAN               ! channel number of bolometer
      DOUBLE PRECISION COS_E              ! sin (E)
      DOUBLE PRECISION COS_Q              ! cos (Q)
      LOGICAL          DONE               ! .TRUE. if pointing corrections
                                          ! straddling LST of measured point
                                          ! have been found
      DOUBLE PRECISION E                  ! elevation of `centre' (radians)
      DOUBLE PRECISION EL_OFFSET          ! offset in el (arcsec)
      DOUBLE PRECISION DEL                ! elevation of point (radians)
      DOUBLE PRECISION HOUR_ANGLE         ! hour angle (radians)
      INTEGER          I                  ! array index
      DOUBLE PRECISION P_DAZ              ! applied az pointing correction
                                          ! (arcsec)
      DOUBLE PRECISION P_DEL              ! applied el pointing correction
                                          ! (arcsec)
      DOUBLE PRECISION Q                  ! parallactic angle (radians)
      DOUBLE PRECISION RD_X_OFFSET        ! offset of Nasmyth origin from 
                                          ! `centre' in RD tangent plane
                                          ! (radians)
      DOUBLE PRECISION RD_Y_OFFSET        ! offset of Nasmyth origin from
                                          ! `centre' in RD tangent plane
                                          ! (radians)
      DOUBLE PRECISION SIN_E              ! sin (E)
      DOUBLE PRECISION SIN_Q              ! sin (Q)
      DOUBLE PRECISION U3_OFFSET          ! offset of Nasmyth origin from
                                          ! `centre' (arcsec)
      DOUBLE PRECISION U4_OFFSET          ! offset of Nasmyth origin from
                                          ! `centre' (arcsec)
      DOUBLE PRECISION U3_OFF             ! bolometer offset from `centre'
                                          ! (radians)
      DOUBLE PRECISION U4_OFF             ! bolometer offset from `centre'
                                          ! (radians)
*    Internal References :
*    Local data :
*-
      IF (STATUS .NE. SAI__OK) RETURN

*  calculate the extra offsets to be added due to jiggling or scanning

      IF (OFFSET_COORDS .EQ. 'AZ') THEN
         AZ_OFFSET = DBLE (OFFSET_X)
         EL_OFFSET = DBLE (OFFSET_Y)
         U3_OFFSET = 0.0D0
         U4_OFFSET = 0.0D0
         RD_X_OFFSET = 0.0D0
         RD_Y_OFFSET = 0.0D0
      ELSE IF (OFFSET_COORDS .EQ. 'NA') THEN
         AZ_OFFSET = 0.0D0
         EL_OFFSET = 0.0D0
         U3_OFFSET = DBLE (OFFSET_X)
         U4_OFFSET = DBLE (OFFSET_Y)
         RD_X_OFFSET = 0.0D0
         RD_Y_OFFSET = 0.0D0
      ELSE IF (OFFSET_COORDS .EQ. 'RD') THEN
         AZ_OFFSET = 0.0D0
         EL_OFFSET = 0.0D0
         U3_OFFSET = 0.0D0
         U4_OFFSET = 0.0D0
         RD_X_OFFSET = DBLE(OFFSET_X) * COS (ROTATION) -
     :     DBLE(OFFSET_Y) * SIN (ROTATION)
         RD_Y_OFFSET = DBLE(OFFSET_X) * SIN (ROTATION) +
     :     DBLE(OFFSET_Y) * COS (ROTATION)
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC ('COORDS', OFFSET_COORDS)
         CALL ERR_REP (' ', 'SCULIB_CALC_BOL_COORDS: bad value for '//
     :     'OFFSET_COORDS - ^COORDS', STATUS)
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
                     DONE = .TRUE.
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
     :     COS (LAT_OBS) * COS (DEC_CENTRE) * COS (HOUR_ANGLE)
         E = ASIN (SIN_E)

*  and the parallactic angle

         IF (COS(DEC_CENTRE) .EQ. 0.0D0) THEN
            Q = PI - HOUR_ANGLE
            SIN_Q = SIN (Q)
            COS_Q = COS (Q)
         ELSE
            SIN_Q = SIN (HOUR_ANGLE) * COS (LAT_OBS) / COS (E)
            COS_Q = (SIN (LAT_OBS) - SIN (DEC_CENTRE) * SIN_E) /
     :        (COS (DEC_CENTRE) * COS(E))
         END IF
      END IF

*  now go through the bolometers calculating their positions

      IF (STATUS .EQ. SAI__OK) THEN
         IF (N_BOL .GT. 0) THEN
            COS_E = COS (E)

            DO BOL = 1, N_BOL

*  calculate the Nasmyth offset

               CHAN = BOL_CHAN (BOL)
               ADC = BOL_ADC (BOL)

               U3_OFF = (DBLE(U3(CHAN,ADC)) - DBLE(U3_CENTRE) - 
     :           U3_OFFSET) * ARCSEC2RAD
               U4_OFF = (DBLE(U4(CHAN,ADC)) - DBLE(U4_CENTRE) -
     :           U4_OFFSET) * ARCSEC2RAD

*  rotate the offset to az-el

               DAZ = U3_OFF * COS_E + U4_OFF * SIN_E
               DEL = -U3_OFF * SIN_E + U4_OFF * COS_E

*  add any AZ offset and the pointing offset (assuming pointing corrections 
*  are tangent plane alt-az offsets with the azimuth value refering to the 
*  error at zero elevation)

               IF (N_POINT .GT. 0) THEN
                  DAZ = DAZ + (P_DAZ * COS (DEL) - AZ_OFFSET) * 
     :              ARCSEC2RAD
                  DEL = DEL + (P_DEL - EL_OFFSET) * ARCSEC2RAD
               END IF

*  now rotate the offset to apparent RA,dec

               BOL_XOFF = -DAZ * COS_Q + DEL * SIN_Q
               BOL_YOFF = DAZ * SIN_Q + DEL * COS_Q

*  add in any extra offset in RD tangent plane coords

               BOL_XOFF = BOL_XOFF + RD_X_OFFSET * ARCSEC2RAD
               BOL_YOFF = BOL_YOFF + RD_Y_OFFSET * ARCSEC2RAD

*  and calculate the apparent RA,Dec of the offset position

               CALL SLA_DTP2S (BOL_XOFF, BOL_YOFF, RA_CENTRE,
     :           DEC_CENTRE, RA_BOL(BOL), DEC_BOL(BOL))
            END DO

         END IF
      END IF

      END
