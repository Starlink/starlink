      SUBROUTINE SCULIB_PHOTOM_BOLSELECT (BOLOMETERS, BOL_TYPE,
     :   BOL_CALIB, BOL_DU3, BOL_DU4, BOL_QUAL, BOL_ENABLED, NUM_CHAN,
     :   NUM_ADC, BOL_SELECT_CHAN, BOL_SELECT_ADC, N_BOLS, MAX_SUB,
     :   SUB_INSTRMNT, N_SUB, CENTRE_DU3, CENTRE_DU4, CHOP_COORDS,
     :   BOL_SPACING, CHOP_PA, N_BOL_SUB, BOLS_MEASURED,
     :   PHOT_BEAM_CHAN, PHOT_BEAM_ADC, PHOT_BEAM_BOL, STATUS)
*+
*  Name:
*     SCULIB_PHOTOM_BOLSELECT

*  Purpose:
*     select photometers for a PHOTOM observation

*  Description:
*     This routine selects the bolometers to be used in a PHOTOM observation.
*     If status is good on entry the routine will start by setting to zero
*     the arrays describing which bolometers have been selected. Then it will
*     call SCULIB_BOLSELECT to get the chan/ADC numbers of the bolometers
*     directly specified in BOLOMETERS.
*
*       The observer can specify between 1 and 3 bolometers by name. Otherwise
*     an error will be reported and the routine will return with bad status.
*     Likewise, if any of these directly selected bolometers have bad quality
*     or have identical Nasmyth offsets.
*
*     Now the routine branches on the number of directly selected bolometers:-
*
*      - 3 bolometers specified. In this case the observer must want to chop
*      between 3 bolometers on an array.
*        All 3 bolometers should belong to a single array, if not the routine
*      will error and return with bad status. The routine will arbitrarily
*      assign the 3 bolometers to the `left', 'middle' and `right' projected
*      positions in the order they were selected. The routine now checks that
*      the 3 bolometers do lie in roughly a straight line on the sky (allowing
*      for distortion) and that the left and right bolometers lie roughly the
*      same distance on either side of the middle. Errors will occur and the
*      routine return with bad status if these conditions are not met.
*        If all is OK the routine will now store the channel/ADC numbers of
*      the directly selected bolometers, set CHOP_COORDS to Nasmyth, the
*      bolometer spacing to half the distance between the left and right,
*      the chopper position angle, and the instrument `centre' to the coords of
*      the middle bolometer.
*        Next, the routine will search the bolometers belonging to the other
*      array. If 3 can be found that match the positions of the 3 direct
*      bolometers and have good quality then the routine will store the
*      channel/ADC numbers of those bolometers as well.
*        Lastly, the routine will set the bolometers to actually be measured
*      to all those belonging to the array(s) containing the bolometers
*      already selected.
*      - 2 bolometers specified. In this case the observer must want to observe
*      a source by chopping between the 2 named bolometers.
*        The routine will arbitrarily call the first of the 2 bolometers
*      the `middle' projected bolometer, the second `right'. Then it will
*      store the channel/ADC numbers of these directly selected bolometers and
*      set the types of sub-instruments that they imply. If any of the
*      sub-instruments are arrays the routine will search for bolometer(s)
*      at the same Nasmyth offset in the other array and select them too.
*        Lastly, the routine will set the bolometers to actually be measured
*      to all those in the selected sub-instruments. CHOP_COORDS is set to
*      Nasmyth, the bolometer spacing to the distance between the
*      directly selected bolometers, the chopper position angle is set, and
*      the instrument `centre' to the Nasmyth coords of the middle bolometer.
*      - 1 bolometer specified. In this case the observer must want to observe
*      a source without chopping between different bolometers.
*        The routine will arbitrarily call the primary selected bolometer
*      the `middle' projected bolometer. `Left' and `right' bolometers will
*      not be assigned.
*        The routine will now store the channel/ADC numbers of the directly
*      selected bolometer and set the type of sub-instrument that is implied.
*      If the sub-instrument is one of the arrays the routine will search
*      for a bolometer at the same Nasmyth offset in the other array and
*      select it too.
*        Lastly, the routine will set the bolometers to actually be measured
*      to all those in the selected sub-instruments. The instrument `centre'
*      is set to the Nasmyth offset of the primary selected bolometer.
*      CHOP_COORDS, the bolometer spacing and chopper position angle are
*      all set to bad values so that the calling routine knows these have to
*      be read explicitly from the observation definition file.
*
*     Finally, SCULIB_BOLSELECT is called to select for measurement all the
*     bolometers belonging to the sub-instruments involved in this PHOTOM
*     observation. An array is then calculated holding the index in the array
*     of ALL the photometers to be measured of each projected bolometer
*     in each sub-instrument that was directly selected.

*  Invocation:
*     CALL  SCULIB_PHOTOM_BOLSELECT (BOLOMETERS, BOL_TYPE,
*    :   BOL_CALIB, BOL_DU3, BOL_DU4, BOL_QUAL, BOL_ENABLED, NUM_CHAN,
*    :   NUM_ADC, BOL_SELECT_CHAN, BOL_SELECT_ADC, N_BOLS, MAX_SUB,
*    :   SUB_INSTRMNT, N_SUB, CENTRE_DU3, CENTRE_DU4, CHOP_COORDS,
*    :   BOL_SPACING, CHOP_PA, N_BOL_SUB, BOLS_MEASURED,
*    :   PHOT_BEAM_CHAN, PHOT_BEAM_ADC, PHOT_BEAM_BOL, STATUS)

*  Arguments:
*     BOLOMETERS                  = CHARACTER*(*) (Given)
*           list of bolometer selections
*     BOL_TYPE (NUM_CHAN, NUM_ADC)
*                                 = CHARACTER*(*) (Given)
*           type of bolometer
*     BOL_CALIB (NUM_CHAN, NUM_ADC)
*                                 = REAL (Given)
*           target calibrator values for bolometers
*     BOL_DU3 (NUM_CHAN, NUM_ADC) = REAL (Given)
*           Nasmyth DU3 offset of bolometer from field centre
*     BOL_DU4 (NUM_CHAN, NUM_ADC) = REAL (Given)
*           Nasmyth DU4 offset of bolometer from field centre
*     BOL_QUAL (NUM_CHAN, NUM_ADC)
*                                 = INTEGER (Given)
*           quality of bolometers
*     BOL_ENABLED (NUM_CHAN, NUM_ADC)
*                                 = LOGICAL (Returned)
*           .TRUE. if bolometer was selected
*     NUM_CHAN                    = INTEGER (Given)
*           number of channels per A/D
*     NUM_ADC                     = INTEGER (Given)
*           number of A/D cards
*     BOL_SELECT_CHAN (NUM_CHAN * NUM_ADC)
*                                 = INTEGER (Returned)
*           channel numbers of selected bolometers
*     BOL_SELECT_ADC (NUM_CHAN * NUM_ADC)
*                                 = INTEGER (Returned)
*           A/D card numbers of selected bolometers
*     N_BOLS                      = INTEGER (Returned)
*           total number of bolometers selected
*     MAX_SUB                     = INTEGER (Given)
*           maximum number of sub instruments
*     SUB_INSTRMNT (MAX_SUB)      = CHARACTER*(*) (Returned)
*           names of sub instrument sections to be used
*     N_SUB                       = INTEGER (Returned)
*           the number of sub instruments to be used
*     CENTRE_DU3                  = REAL (Returned)
*           Nasmyth DU3 offset from instrument centre to which telescope
*           is to pointed
*     CENTRE_DU4                  = REAL (Returned)
*           Nasmyth DU4 offset from instrument centre to which telescope
*           is to pointed
*     CHOP_COORDS                 = CHARACTER*(*) (Returned)
*           Chopper coordinate system required
*     BOL_SPACING                 = REAL (Returned)
*           Spacing between bolometers (arcsec)
*     CHOP_PA                     = REAL (Returned)
*           Chop position angle required (degrees)
*     N_BOL_SUB (MAX_SUB)         = INTEGER (Returned)
*           Number of bolometers selected in each sub instrument
*     BOLS_MEASURED               = CHARACTER*(*) (Returned)
*           Bolometers to be measured by transputer system
*     PHOT_BEAM_CHAN (3, MAX_SUB) = INTEGER (Returned)
*           Channel numbers of selected bolometers projected left, middle,
*           right on sky for each sub-instrument (I index = 1 for left, 2
*           for middle and 3 for right)
*     PHOT_BEAM_ADC (3, MAX_SUB)  = INTEGER (Returned)
*           ADC numbers of selected bolometers projected left, middle,
*           right on sky for each sub-instrument
*     PHOT_BEAM_BOL (3, MAX_SUB)  = INTEGER (Returned)
*           The index in the array of selected bolometers of those projected
*           left, middle, right on sky for each sub-instrument.
*     STATUS                      = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     20-JUL-1993: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'            ! for VAL__BADR

*  Arguments Given:
      CHARACTER*(*) BOLOMETERS
      INTEGER NUM_CHAN, NUM_ADC
      CHARACTER*(*) BOL_TYPE (NUM_CHAN, NUM_ADC)
      REAL BOL_CALIB (NUM_CHAN, NUM_ADC)
      REAL BOL_DU3 (NUM_CHAN, NUM_ADC)
      REAL BOL_DU4 (NUM_CHAN, NUM_ADC)
      INTEGER BOL_QUAL (NUM_CHAN, NUM_ADC)
      INTEGER MAX_SUB

*  Arguments Given & Returned:

*  Arguments Returned:
      LOGICAL BOL_ENABLED (NUM_CHAN, NUM_ADC)
      INTEGER BOL_SELECT_CHAN (NUM_CHAN * NUM_ADC)
      INTEGER BOL_SELECT_ADC (NUM_CHAN * NUM_ADC)
      INTEGER N_BOLS
      CHARACTER*(*) SUB_INSTRMNT (MAX_SUB)
      INTEGER N_SUB
      REAL CENTRE_DU3, CENTRE_DU4
      CHARACTER*(*) CHOP_COORDS
      REAL BOL_SPACING
      REAL CHOP_PA
      INTEGER N_BOL_SUB (MAX_SUB)
      CHARACTER*(*) BOLS_MEASURED
      INTEGER PHOT_BEAM_CHAN (3, MAX_SUB)
      INTEGER PHOT_BEAM_ADC (3, MAX_SUB)
      INTEGER PHOT_BEAM_BOL (3, MAX_SUB)

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN                 ! CHR string-length function

*  Global variables:

*  Local Constants:
      REAL R2D                        ! radians to degrees
      PARAMETER (R2D = 57.29578)

*  Local variables:
      LOGICAL ERROR                   ! T if any of selected bolometers have
                                      ! bad quality
      LOGICAL BOTH_ARRAYS             ! T if 3 beams on selected array have
                                      ! matching bolometers on other array
      LOGICAL DONE                    !
      INTEGER I, J, K                 ! DO loop
      INTEGER LEFT, MIDDLE, RIGHT     ! indices of bolometers projected to
                                      ! left, middle and right
      INTEGER CHAN, ADC               ! channel and ADC of bolometer
      INTEGER OTHER_CHAN, OTHER_ADC   ! channel and ADC of bolometer at same
                                      ! position on other array as that
                                      ! selected directly
      INTEGER N_SUB_AFTER             ! number of sub instruments after check
                                      ! has been made for bolometers on the
                                      ! `other' array
      INTEGER SLASH                   ! position of / in string
      REAL DU3, DU4                   ! Nasmyth coords of a bolometer
      REAL DELTA_DU3                  ! used in calculation of SLOPE
      REAL SLOPE, CONSTANT            ! constants of equation of line linking
                                      ! left and right bolometers
      REAL SLOPE_P, CONSTANT_P        ! constants of equation of line
                                      ! perpendicular to the above running
                                      ! through the middle bolometer
      REAL DU3_I, DU4_I               ! coords of intersection between the 2
                                      ! lines
      REAL M_L, M_M, M_R              ! distances from middle bolometer to
                                      ! left bolometer, intersection of lines
                                      ! and right bolometer
      REAL L_R                        ! distance from left to right bolometer
      CHARACTER*15 TYPE               ! type of bolometer
      CHARACTER*15 PRIME_TYPE         ! prime type of bolometer; SHORT, LONG,
                                      ! P1100, P1300, P2000
      CHARACTER*15 SEC_TYPE           ! secondary type, e.g. CENTRE, DC
      CHARACTER*15 OTHER_TYPE         ! type of array other than directly
                                      ! selected
      CHARACTER*1 CTEMP (10)          ! scratch character array

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  initialise returned variables

      N_SUB = 0
      N_BOLS = 0

      DO J = 1, MAX_SUB
         N_BOL_SUB (I) = 0
         DO I = 1, 3
            PHOT_BEAM_CHAN (I,J) = 0
            PHOT_BEAM_ADC (I,J) = 0
            PHOT_BEAM_BOL (I,J) = 0
         END DO
      END DO

*  decode bolometer string in normal way

      CALL SCULIB_BOLSELECT (BOLOMETERS, BOL_TYPE, BOL_CALIB,
     :   BOL_DU3, BOL_DU4, BOL_QUAL, BOL_ENABLED, NUM_CHAN, NUM_ADC,
     :   BOL_SELECT_CHAN, BOL_SELECT_ADC, N_BOLS, MAX_SUB,
     :   SUB_INSTRMNT, N_SUB, STATUS)

*  there should have been 1-3 bolometers

      IF ((N_BOLS .GT. 3) .OR. (N_BOLS .LT. 1)) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI ('N_BOLS', N_BOLS)
         CALL ERR_REP (' ', 'SCULIB_PHOTOM_BOLSELECT: error '//
     :     'selecting bolometers for photometry; 1-3 bolometers '//
     :     'should be selected, not ^N_BOLS', STATUS)
      END IF

*  check that bolometers all have good quality

      IF (STATUS .EQ. SAI__OK) THEN
         ERROR = .FALSE.
         DO I = 1, N_BOLS
            IF (BOL_QUAL(BOL_SELECT_CHAN(I),BOL_SELECT_ADC(I)) .NE. 0)
     :        THEN
               ERROR = .TRUE.
            END IF
         END DO
         IF (ERROR) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_PHOTOM_BOLSELECT: error '//
     :        'selecting bolometers for photometry; one or '//
     :        'more of the bolometers selected has bad quality', STATUS)
         END IF
      END IF

*  and that none of them are coincident

      IF (STATUS .EQ. SAI__OK) THEN
         DO J =1, N_BOLS
            DO I = 1, N_BOLS
               IF (I .NE. J) THEN
                  IF ((BOL_DU3(BOL_SELECT_CHAN(I),BOL_SELECT_ADC(I))
     :              .EQ.
     :              BOL_DU3(BOL_SELECT_CHAN(J),BOL_SELECT_ADC(J)))
     :              .AND.
     :              (BOL_DU4(BOL_SELECT_CHAN(I),BOL_SELECT_ADC(I))
     :              .EQ.
     :              BOL_DU4(BOL_SELECT_CHAN(J),BOL_SELECT_ADC(J))))
     :              THEN
                     CALL MSG_SETI ('I', I)
                     CALL MSG_SETI ('J', J)
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ', 'SCULIB_PHOTOM_BOLSELECT: '//
     :                 'error in bolometer selection; '//
     :                 'bolometers ^I and ^J are at the same '//
     :                 'position', STATUS)
                  END IF
               END IF
            END DO
         END DO
      END IF


      IF (STATUS .EQ. SAI__OK) THEN

*  now go through various possible combinations

         IF (N_BOLS .EQ. 3) THEN

*  chopping between 3 bolometers

            IF (N_SUB .NE. 1) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_PHOTOM_BOLSELECT: error '//
     :           'in 3 bolometer selection; not all bolometers '//
     :           'belong to the same array', STATUS)
            END IF


            IF (STATUS .EQ. SAI__OK) THEN

*  assign the first selected bolometer as the left projected beam, the second
*  middle and the last right.

               LEFT = 1
               MIDDLE = 2
               RIGHT = 3

*  now check that the 3 bolometers fall roughly on a straight line (allowing
*  5 arcsec for distortion) and that the L and R bolometers are equidistant
*  from the M bolometer, i.e. verifying that M really is in the middle
*  (the method used is a bit overcomplex)

*  first get equation of line between L and R bolometers

               DELTA_DU3 =
     :           BOL_DU3(BOL_SELECT_CHAN(RIGHT),BOL_SELECT_ADC(RIGHT)) -
     :           BOL_DU3(BOL_SELECT_CHAN(LEFT),BOL_SELECT_ADC(LEFT))
               DELTA_DU3 = MAX (DELTA_DU3, 1.0E-10)

               SLOPE =
     :           (BOL_DU4(BOL_SELECT_CHAN(RIGHT),BOL_SELECT_ADC(RIGHT))-
     :           BOL_DU4(BOL_SELECT_CHAN(LEFT),BOL_SELECT_ADC(LEFT))) /
     :           DELTA_DU3

               CONSTANT =
     :           BOL_DU4(BOL_SELECT_CHAN(RIGHT),BOL_SELECT_ADC(RIGHT)) -
     :           SLOPE *
     :           BOL_DU3(BOL_SELECT_CHAN(RIGHT),BOL_SELECT_ADC(RIGHT))

*  now of line perpendicular to the first passing through the M bolometer

               SLOPE_P = -1.0 / SLOPE
               CONSTANT_P =
     :           BOL_DU4(BOL_SELECT_CHAN(MIDDLE),BOL_SELECT_ADC(MIDDLE))
     :           - SLOPE_P *
     :           BOL_DU3(BOL_SELECT_CHAN(MIDDLE),BOL_SELECT_ADC(MIDDLE))

*  now coord where 2 lines intersect

               DU3_I = (CONSTANT - CONSTANT_P) / (SLOPE_P - SLOPE)
               DU4_I = SLOPE * DU3_I + CONSTANT

*  distance from the middle beam to the 3 points on the line between L and R

               M_L =
     :           (BOL_DU3(BOL_SELECT_CHAN(MIDDLE),
     :           BOL_SELECT_ADC(MIDDLE)) -
     :           BOL_DU3(BOL_SELECT_CHAN(LEFT),
     :           BOL_SELECT_ADC(LEFT))) **2 +
     :           (BOL_DU4(BOL_SELECT_CHAN(MIDDLE),
     :           BOL_SELECT_ADC(MIDDLE)) -
     :           BOL_DU4(BOL_SELECT_CHAN(LEFT),
     :           BOL_SELECT_ADC(LEFT))) **2
               IF (M_L .GT. 0.0) THEN
                  M_L = SQRT (M_L)
               END IF

               M_R =
     :           (BOL_DU3(BOL_SELECT_CHAN(MIDDLE),
     :           BOL_SELECT_ADC(MIDDLE)) -
     :           BOL_DU3(BOL_SELECT_CHAN(RIGHT),
     :           BOL_SELECT_ADC(RIGHT)))**2 +
     :           (BOL_DU4(BOL_SELECT_CHAN(MIDDLE),
     :           BOL_SELECT_ADC(MIDDLE)) -
     :           BOL_DU4(BOL_SELECT_CHAN(RIGHT),
     :           BOL_SELECT_ADC(RIGHT))) **2
               IF (M_R .GT. 0.0) THEN
                  M_R = SQRT (M_R)
               END IF

               M_M =
     :           (BOL_DU3(BOL_SELECT_CHAN(MIDDLE),
     :           BOL_SELECT_ADC(MIDDLE)) - DU3_I) **2 +
     :           (BOL_DU4(BOL_SELECT_CHAN(MIDDLE),
     :           BOL_SELECT_ADC(MIDDLE)) - DU4_I) **2
               IF (M_M .GT. 0.0) THEN
                  M_M = SQRT (M_M)
               END IF

*  and from L to R

               L_R =
     :           (BOL_DU3(BOL_SELECT_CHAN(LEFT),
     :           BOL_SELECT_ADC(LEFT)) -
     :           BOL_DU3(BOL_SELECT_CHAN(RIGHT),
     :           BOL_SELECT_ADC(RIGHT))) **2 +
     :           (BOL_DU4(BOL_SELECT_CHAN(LEFT),
     :           BOL_SELECT_ADC(LEFT)) -
     :           BOL_DU4(BOL_SELECT_CHAN(RIGHT),
     :           BOL_SELECT_ADC(RIGHT))) **2
               IF (L_R .GT. 0.0) THEN
                  L_R = SQRT (L_R)
               END IF


*  check that M_L and M_R are roughly equal

               IF (ABS(M_L - M_R) .GT. 5.0) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP (' ', 'SCULIB_PHOTOM_BOLSELECT: error '//
     :              'in 3 bolometer selection; middle bolometer is '//
     :              'not equidistant from left and right', STATUS)
               END IF

*  and that L_R is roughly twice M_L

               IF (ABS(2.0*M_L - L_R) .GT. 5.0) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP (' ', 'SCULIB_PHOTOM_BOLSELECT: error '//
     :              'in 3 bolometer selection; middle bolometer does '//
     :              'not lie between left and right', STATUS)
               END IF

*  and that M is close to the line joing L and R

               IF (M_M .GT. 5.0) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP (' ', 'SCULIB_PHOTOM_BOLSELECT: error '//
     :              'in 3 bolometer selection; middle bolometer does '//
     :              'not lie close to line joining left and right',
     :              STATUS)
               END IF

            END IF


            IF (STATUS .EQ. SAI__OK) THEN

*  set up bolometer arrays for the sub instrument selected directly

               N_BOL_SUB (1) = 3

               PHOT_BEAM_CHAN (1,1) = BOL_SELECT_CHAN (LEFT)
               PHOT_BEAM_ADC (1,1) = BOL_SELECT_ADC (LEFT)
               PHOT_BEAM_CHAN (2,1) = BOL_SELECT_CHAN (MIDDLE)
               PHOT_BEAM_ADC (2,1) = BOL_SELECT_ADC (MIDDLE)
               PHOT_BEAM_CHAN (3,1) = BOL_SELECT_CHAN (RIGHT)
               PHOT_BEAM_ADC (3,1) = BOL_SELECT_ADC (RIGHT)

*  set the chop coords, calculate the spacing between bolometers and position
*  angle

               CHOP_COORDS = 'NA'
               BOL_SPACING = L_R / 2.0
               CHOP_PA = ATAN2 (
     :           (BOL_DU4(BOL_SELECT_CHAN(RIGHT),BOL_SELECT_ADC(RIGHT))-
     :           BOL_DU4(BOL_SELECT_CHAN(LEFT),BOL_SELECT_ADC(LEFT))),
     :           (BOL_DU3(BOL_SELECT_CHAN(RIGHT),BOL_SELECT_ADC(RIGHT))-
     :           BOL_DU3(BOL_SELECT_CHAN(LEFT),BOL_SELECT_ADC(LEFT))))
               CHOP_PA = CHOP_PA * R2D

*  set the instrument `centre' to be the offset of the middle bolometer

               CENTRE_DU3 =
     :           BOL_DU3(BOL_SELECT_CHAN(MIDDLE),BOL_SELECT_ADC(MIDDLE))
               CENTRE_DU4 =
     :           BOL_DU4(BOL_SELECT_CHAN(MIDDLE),BOL_SELECT_ADC(MIDDLE))

*  now see if the other array has bolometers at the same positions as those
*  already selected

               IF (SUB_INSTRMNT(1) .EQ. 'LONG') THEN
                  OTHER_TYPE = 'SHORT'
               ELSE
                  OTHER_TYPE = 'LONG'
               END IF

               BOTH_ARRAYS = .TRUE.

               DO I = 1, 3

                  DU3 = BOL_DU3(PHOT_BEAM_CHAN(I,1),PHOT_BEAM_ADC(I,1))
                  DU4 = BOL_DU4(PHOT_BEAM_CHAN(I,1),PHOT_BEAM_ADC(I,1))

                  OTHER_CHAN = VAL__BADI

                  DO ADC = 1, NUM_ADC
                     DO CHAN = 1, NUM_CHAN

                        IF (BOL_QUAL(CHAN,ADC ) .EQ. 0) THEN

                           SLASH = INDEX (BOL_TYPE(CHAN,ADC),'/')
                           IF (SLASH .EQ. 0) THEN
                              PRIME_TYPE = BOL_TYPE(CHAN,ADC)
                              SEC_TYPE = ' '
                           ELSE
                              PRIME_TYPE = BOL_TYPE(CHAN,ADC)(:SLASH-1)
                              SEC_TYPE = BOL_TYPE(CHAN,ADC)(SLASH+1:)
                           END IF

                           IF ((PRIME_TYPE .EQ. OTHER_TYPE) .AND.
     :                       ((SEC_TYPE .EQ. ' ') .OR.
     :                       (SEC_TYPE .EQ. 'CENTRE'))) THEN
                              IF ((ABS(DU3-BOL_DU3(CHAN,ADC)).LT.0.5)
     :                          .AND.
     :                          (ABS(DU4-BOL_DU4(CHAN,ADC)).LT.0.5))
     :                          THEN
                                 OTHER_CHAN = CHAN
                                 OTHER_ADC = ADC
                              END IF
                           END IF

                        END IF

                     END DO
                  END DO

                  IF (OTHER_CHAN .NE. VAL__BADI) THEN
                     PHOT_BEAM_CHAN (I, 2) = OTHER_CHAN
                     PHOT_BEAM_ADC (I, 2) = OTHER_ADC
                  ELSE
                     BOTH_ARRAYS = .FALSE.
                  END IF

               END DO

*  set N_SUB to 2 and set the second sub instrument if both arrays are to be
*  used

               IF (BOTH_ARRAYS) THEN
                  N_SUB = 2
                  SUB_INSTRMNT (2) = OTHER_TYPE
               END IF

*  set types of bolometers to be measured by the transputer system

               BOLS_MEASURED = SUB_INSTRMNT (1)
               IF (N_SUB .GT. 1) THEN
                  DO I = 2, N_SUB
                     BOLS_MEASURED =
     :                 BOLS_MEASURED (:CHR_LEN(BOLS_MEASURED))//
     :                 ','//SUB_INSTRMNT (I)
                  END DO
               END IF

            END IF


         ELSE IF (N_BOLS .EQ. 2) THEN

*  chopping between 2 bolometers

*  assign `middle' bolometer as first input, `right' as second, `left' not
*  used
            MIDDLE = 1
            RIGHT = 2

*  set up bolometer arrays for the sub instrument selected directly

            IF (N_SUB .EQ. 1) THEN

*  both bolometers are on one of the arrays

               N_BOL_SUB (1) = 2

               PHOT_BEAM_CHAN (2,1) = BOL_SELECT_CHAN (MIDDLE)
               PHOT_BEAM_ADC (2,1) = BOL_SELECT_ADC (MIDDLE)
               PHOT_BEAM_CHAN (3,1) = BOL_SELECT_CHAN (RIGHT)
               PHOT_BEAM_ADC (3,1) = BOL_SELECT_ADC (RIGHT)

            ELSE IF (N_SUB .EQ. 2) THEN

*  bolometers are not on the same array

               N_BOL_SUB (1) = 1
               N_BOL_SUB (2) = 1

               TYPE = SUB_INSTRMNT (1)

               IF (INDEX(BOL_TYPE(BOL_SELECT_ADC(MIDDLE),
     :           BOL_SELECT_CHAN(MIDDLE)), TYPE(:CHR_LEN(TYPE))).NE.0)
     :           THEN
                  PHOT_BEAM_CHAN (2,1) = BOL_SELECT_CHAN (MIDDLE)
                  PHOT_BEAM_ADC (2,1) = BOL_SELECT_ADC (MIDDLE)
                  PHOT_BEAM_CHAN (3,2) = BOL_SELECT_CHAN (RIGHT)
                  PHOT_BEAM_ADC (3,2) = BOL_SELECT_ADC (RIGHT)
               ELSE
                  PHOT_BEAM_CHAN (3,1) = BOL_SELECT_CHAN (RIGHT)
                  PHOT_BEAM_ADC (3,1) = BOL_SELECT_ADC (RIGHT)
                  PHOT_BEAM_CHAN (2,2) = BOL_SELECT_CHAN (MIDDLE)
                  PHOT_BEAM_ADC (2,2) = BOL_SELECT_ADC (MIDDLE)
               END IF

            END IF

*  now, if any of the selected bolometers belong to one of the arrays
*  select the one at the same position on the other array if such exists.

            N_SUB_AFTER = N_SUB

            DO J = 1, N_SUB

               IF ((SUB_INSTRMNT(J) .EQ. 'LONG') .OR.
     :           (SUB_INSTRMNT(J) .EQ. 'SHORT')) THEN

                  IF (SUB_INSTRMNT(J) .EQ. 'LONG') THEN
                     OTHER_TYPE = 'SHORT'
                  ELSE
                     OTHER_TYPE = 'LONG'
                  END IF

                  DO I = 1, 3

                     IF (PHOT_BEAM_CHAN(I,J) .NE. 0) THEN

                        CHAN = PHOT_BEAM_CHAN (I,J)
                        ADC = PHOT_BEAM_ADC (I,J)

                        DU3 = BOL_DU3 (CHAN, ADC)
                        DU4 = BOL_DU4 (CHAN, ADC)

                        OTHER_CHAN = VAL__BADI

                        DO ADC = 1, NUM_ADC
                           DO CHAN = 1, NUM_CHAN

                              IF (BOL_QUAL(CHAN,ADC ) .EQ. 0) THEN

                                 SLASH = INDEX (BOL_TYPE(CHAN,ADC),'/')
                                 IF (SLASH .EQ. 0) THEN
                                    PRIME_TYPE = BOL_TYPE(CHAN,ADC)
                                    SEC_TYPE = ' '
                                 ELSE
                                    PRIME_TYPE =
     :                                BOL_TYPE(CHAN,ADC)(:SLASH-1)
                                    SEC_TYPE =
     :                                BOL_TYPE(CHAN,ADC)(SLASH+1:)
                                 END IF

                                 IF ((PRIME_TYPE .EQ. OTHER_TYPE) .AND.
     :                             ((SEC_TYPE .EQ. ' ') .OR.
     :                             (SEC_TYPE .EQ. 'CENTRE'))) THEN

                                    IF ((ABS(DU3-BOL_DU3(CHAN,ADC))
     :                                .LT. 0.5) .AND.
     :                                (ABS(DU4-BOL_DU4(CHAN,ADC))
     :                                .LT. 0.5)) THEN
                                       OTHER_CHAN = CHAN
                                       OTHER_ADC = ADC
                                    END IF

                                 END IF
                              END IF

                           END DO
                        END DO

                        IF (OTHER_CHAN .NE. VAL__BADI) THEN

                           DONE = .FALSE.

                           DO K = 1, N_SUB_AFTER
                              IF (SUB_INSTRMNT(K) .EQ. OTHER_TYPE)
     :                          THEN
                                 DONE = .TRUE.
                                 PHOT_BEAM_CHAN (I,K) = OTHER_CHAN
                                 PHOT_BEAM_ADC (I,K) = OTHER_ADC
                              END IF
                           END DO

                           IF (.NOT. DONE) THEN
                              N_SUB_AFTER = N_SUB_AFTER + 1
                              SUB_INSTRMNT (N_SUB_AFTER) = OTHER_TYPE
                              PHOT_BEAM_CHAN (I,N_SUB_AFTER) =
     :                          OTHER_CHAN
                              PHOT_BEAM_ADC (I,N_SUB_AFTER) = OTHER_ADC
                           END IF

                        END IF
                     END IF

                  END DO

               END IF

            END DO

            N_SUB = N_SUB_AFTER


*  set types of bolometers to be measured by the transputer system

            BOLS_MEASURED = SUB_INSTRMNT (1)
            IF (N_SUB .GT. 1) THEN
               DO I = 2, N_SUB
                  BOLS_MEASURED =
     :              BOLS_MEASURED (:CHR_LEN(BOLS_MEASURED))//
     :              ','//SUB_INSTRMNT (I)
               END DO
            END IF


*  set the chop coords, calculate the bolometer spacing and position
*  angle

            CHOP_COORDS = 'NA'
            BOL_SPACING = (BOL_DU3(BOL_SELECT_CHAN(RIGHT),
     :        BOL_SELECT_ADC(RIGHT)) -
     :        BOL_DU3(BOL_SELECT_CHAN(MIDDLE),
     :        BOL_SELECT_ADC(MIDDLE))) **2 +
     :        (BOL_DU4(BOL_SELECT_CHAN(RIGHT),
     :        BOL_SELECT_ADC(RIGHT)) -
     :        BOL_DU4(BOL_SELECT_CHAN(MIDDLE),
     :        BOL_SELECT_ADC(MIDDLE))) **2
            IF (BOL_SPACING .GT. 0.0) THEN
               BOL_SPACING = SQRT (BOL_SPACING)
            END IF

            CHOP_PA = ATAN2 (
     :        (BOL_DU4(BOL_SELECT_CHAN(RIGHT),
     :        BOL_SELECT_ADC(RIGHT)) -
     :        BOL_DU4(BOL_SELECT_CHAN(MIDDLE),
     :        BOL_SELECT_ADC(MIDDLE))),
     :        (BOL_DU3(BOL_SELECT_CHAN(RIGHT),
     :        BOL_SELECT_ADC(RIGHT)) -
     :        BOL_DU3(BOL_SELECT_CHAN(MIDDLE),
     :        BOL_SELECT_ADC(MIDDLE))))
            CHOP_PA = CHOP_PA * R2D

*  set the instrument `centre' to be the offset of the middle bolometer

            CENTRE_DU3 =
     :        BOL_DU3(BOL_SELECT_CHAN(MIDDLE),BOL_SELECT_ADC(MIDDLE))
            CENTRE_DU4 =
     :        BOL_DU4(BOL_SELECT_CHAN(MIDDLE),BOL_SELECT_ADC(MIDDLE))


         ELSE IF (N_BOLS .EQ. 1) THEN

*  not chopping on array, assign `middle' bolometer to be one selected

            MIDDLE = 1

*  set up bolometer arrays for the sub instrument selected directly

            N_BOL_SUB (1) = 1

            PHOT_BEAM_CHAN (2,1) = BOL_SELECT_CHAN (MIDDLE)
            PHOT_BEAM_ADC (2,1) = BOL_SELECT_ADC (MIDDLE)

*  now, if the selected bolometer belongs to one of the arrays select the
*  one at the same position on the other array if such exists.

            IF ((SUB_INSTRMNT(MIDDLE) .EQ. 'LONG') .OR.
     :        (SUB_INSTRMNT(MIDDLE) .EQ. 'SHORT')) THEN

               IF (SUB_INSTRMNT(MIDDLE) .EQ. 'LONG') THEN
                  OTHER_TYPE = 'SHORT'
               ELSE
                  OTHER_TYPE = 'LONG'
               END IF

               DU3 = BOL_DU3 (BOL_SELECT_CHAN(MIDDLE),
     :           BOL_SELECT_ADC(MIDDLE))
               DU4 = BOL_DU4 (BOL_SELECT_CHAN(MIDDLE),
     :           BOL_SELECT_ADC(MIDDLE))

               OTHER_CHAN = VAL__BADI

               DO ADC = 1, NUM_ADC
                  DO CHAN = 1, NUM_CHAN

                     SLASH = INDEX (BOL_TYPE(CHAN,ADC),'/')
                     IF (SLASH .EQ. 0) THEN
                        PRIME_TYPE = BOL_TYPE(CHAN,ADC)
                        SEC_TYPE = ' '
                     ELSE
                        PRIME_TYPE = BOL_TYPE(CHAN,ADC)(:SLASH-1)
                        SEC_TYPE = BOL_TYPE(CHAN,ADC)(SLASH+1:)
                     END IF

                     IF ((PRIME_TYPE .EQ. OTHER_TYPE) .AND.
     :                 ((SEC_TYPE .EQ. ' ') .OR.
     :                 (SEC_TYPE .EQ. 'CENTRE'))) THEN

                        IF ((ABS(DU3-BOL_DU3(CHAN,ADC)) .LT. 0.5) .AND.
     :                    (ABS(DU4-BOL_DU4(CHAN,ADC)) .LT. 0.5))  THEN

                           OTHER_CHAN = CHAN
                           OTHER_ADC = ADC

                        END IF
                     END IF

                  END DO
               END DO

               IF (OTHER_CHAN .NE. VAL__BADI) THEN
                  N_SUB = 2
                  SUB_INSTRMNT (N_SUB) = OTHER_TYPE
                  PHOT_BEAM_CHAN (2,N_SUB) = OTHER_CHAN
                  PHOT_BEAM_ADC (2,N_SUB) = OTHER_ADC
               END IF

            END IF

*  set types of bolometers to be measured by the transputer system

            BOLS_MEASURED = SUB_INSTRMNT (1)
            IF (N_SUB .GT. 1) THEN
               DO I = 2, N_SUB
                  BOLS_MEASURED =
     :              BOLS_MEASURED (:CHR_LEN(BOLS_MEASURED))//
     :              ','//SUB_INSTRMNT (I)
               END DO
            END IF

*  set the instrument centre to be the offset of the selected bolometer

            CENTRE_DU3 =
     :        BOL_DU3(BOL_SELECT_CHAN(MIDDLE), BOL_SELECT_ADC(MIDDLE))
            CENTRE_DU4 =
     :        BOL_DU4(BOL_SELECT_CHAN(MIDDLE), BOL_SELECT_ADC(MIDDLE))

*  set the other parameters to `UNKNOWN' or bad values

            CHOP_COORDS = 'UNKNOWN'
            BOL_SPACING = VAL__BADR
            CHOP_PA = VAL__BADR

         END IF

      END IF



      IF (STATUS .EQ. SAI__OK) THEN

*  finally, select all the bolometers belonging to the sub-instruments to
*  be used (being careful not to overwrite the SUB_INSTRMNT
*  array; its order is important for a PHOTOM observation whereas
*  SCULIB_BOLSELECT will just output the sub-instruments in order of
*  increasing wavelength)


         CALL SCULIB_BOLSELECT (BOLS_MEASURED, BOL_TYPE, BOL_CALIB,
     :      BOL_DU3, BOL_DU4, BOL_QUAL, BOL_ENABLED, NUM_CHAN, NUM_ADC,
     :      BOL_SELECT_CHAN, BOL_SELECT_ADC, N_BOLS, MAX_SUB,
     :      CTEMP, N_SUB, STATUS)

*  and set the PHOT_BEAM_BOL array to point to the positions in the demodulated
*  data array where the bolometers to be used directly for the PHOTOM
*  observation can be found

         DO J = 1, N_SUB
            DO I = 1, 3
               PHOT_BEAM_BOL (I,J) = 0
               DO K = 1, N_BOLS
                  IF ((BOL_SELECT_CHAN(K).EQ.PHOT_BEAM_CHAN(I,J)) .AND.
     :              (BOL_SELECT_ADC(K) .EQ. PHOT_BEAM_ADC(I,J))) THEN
                     PHOT_BEAM_BOL (I,J) = K
                  END IF
               END DO
            END DO
         END DO

      END IF

      END
