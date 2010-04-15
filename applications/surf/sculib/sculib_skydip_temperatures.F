      SUBROUTINE SCULIB_SKYDIP_TEMPERATURES (T_COLD, T_HOT, N_SUB,
     :  SUB_INSTRUMENT, WAVELENGTH, NUM_CHAN, NUM_ADC, BOL_TYPE,
     :  N_BOLS, BOL_CHAN, BOL_ADC, N_SAMPLES_IN, RAW_DATA,
     :  N_SAMPLES_OUT, J_SKY, J_SKY_VARIANCE, J_SKY_QUALITY, J_SKY_AV,
     :  J_SKY_AV_VARIANCE, J_SKY_AV_QUALITY, STATUS)
*+
*  Name:
*     SCULIB_SKYDIP_TEMPERATURES

*  Purpose:
*     derive sky temperatures from chopper-wheel data

*  Language:
*     Starlink Fortran 77

*  Description:
*     This routine derives sky brightness temperatures from data containing
*     the measured signals from cold and ambient loads and sky. The sky
*     temperature is calculated for each sample by linear interpolation between
*     the signals from the cold and ambient loads.
*
*        In addition, mean values of the sky temperature for each bolometer
*     are derived from the samples taken. If more than one sample contributes
*     to the mean then the variance is calculated from the spread of samples
*     about the mean.

*  Invocation:
*     CALL SCULIB_SKYDIP_TEMPERATURES (T_COLD, T_HOT, N_SUB,
*    :  SUB_INSTRUMENT, WAVELENGTH, NUM_CHAN, NUM_ADC, BOL_TYPE,
*    :  N_BOLS, BOL_CHAN, BOL_ADC, N_SAMPLES_IN, RAW_DATA,
*    :  N_SAMPLES_OUT, J_SKY, J_SKY_VARIANCE, J_SKY_QUALITY, J_SKY_AV,
*    :  J_SKY_AV_VARIANCE, J_SKY_AV_QUALITY, STATUS)

*  Arguments:
*     T_COLD (N_SUB)             = REAL (Given)
*           the temperature of the cold load for each sub-instrument
*     T_HOT (N_SUB)              = REAL (Given)
*           the temperature of the hot load for each sub-instrument
*     N_SUB                      = INTEGER (Given)
*           the number of sub-instruments being used
*     SUB_INSTRUMENT (N_SUB)     = CHARACTER*(*) (Given)
*           the names of the sub-instruments being used
*     WAVELENGTH (N_SUB)         = REAL (Given)
*           the wavelengths of the filters in front of the sub-instruments
*           (microns)
*     NUM_CHAN                   = INTEGER (Given)
*           number of A/D channels per A/D card
*     NUM_ADC                    = INTEGER (Given)
*           number of A/D cards
*     BOL_TYPE (NUM_CHAN, NUM_ADC) = CHARACTER*(*) (Given)
*           the types of the bolometers
*     N_BOLS                     = INTEGER (Given)
*           the number of bolometers making measurements
*     BOL_CHAN (N_BOLS)          = INTEGER (Given)
*           channel numbers of selected bolometers
*     BOL_ADC (N_BOLS)           = INTEGER (Given)
*           ADC numbers of selected bolometers
*     N_SAMPLES_IN               = INTEGER (Given)
*           the number of samples taken by each bolometer
*     RAW_DATA (3, N_BOLS, N_SAMPLES_IN) = REAL (Given)
*           the measured signals from the cold and ambient loads and sky
*             -  RAW_DATA (1,i,j) = ambient load signal
*             -  RAW_DATA (2,i,j) = sky signal
*             -  RAW_DATA (3,i,j) = cold load signal
*     N_SAMPLES_OUT              = INTEGER (Given)
*           the number of samples requested for each bolometer
*     J_SKY (N_BOLS, N_SAMPLES_OUT) = REAL (Returned)
*           the brightness temperature of the sky
*     J_SKY_VARIANCE (N_BOLS, N_SAMPLES_OUT) = REAL (Returned)
*           the variance on J_SKY
*     J_SKY_QUALITY (N_BOLS, N_SAMPLES_OUT) = BYTE (Returned)
*           the quality on J_SKY
*     J_SKY_AV (N_BOLS)          = REAL (Returned)
*           the average of J_SKY for each bolometer
*     J_SKY_AV_VARIANCE (N_BOLS) = REAL (Returned)
*           the variance on the average of J_SKY
*     J_SKY_AV_QUALITY (N_BOLS)  = BYTE (Returned)
*           the quality of the average of J_SKY
*     STATUS                     = INTEGER (Given and returned)
*           Global status

*  Authors:
*     J.Lightfoot (REVAD::JFL)
*     Tim Jenness (JAC)

*  Implementation Status:
*     - Should be converted to use double precision internally since sometimes
*       can return a negative temperature.

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Bugs:

*  History:
*     $Id$
*     $Log$
*     Revision 1.1  2004/07/14 02:04:36  timj
*     autoconfed
*
*     Revision 1.11  2001/03/02 01:12:51  timj
*     Remove special case for a hot load of 0.0 volts. Fixes problem with
*     dropped skydip points.
*
*     Revision 1.10  2000/10/28 03:06:27  timj
*     Catch division by zero error in SCULIB_JNU
*
*     Revision 1.9  2000/05/11 20:00:51  timj
*     T_HOT is now an array
*
*     Revision 1.8  1999/08/19 03:37:27  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.7  1999/08/06 02:24:49  timj
*     Tweak headers for use with PROLAT.
*
*     9-SEP-1993: Original version.
*     12-DEC-1994: Checked.
*     16-JUL-1996: Fixed bug where J_COLD was not being added to
*                  interpolated result (JFL).
*     18-JUL-1996: Made T_COLD an array (JFL).

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER N_SUB
      REAL T_COLD (N_SUB)
      REAL T_HOT (N_SUB)
      CHARACTER*(*) SUB_INSTRUMENT (N_SUB)
      REAL WAVELENGTH (N_SUB)
      INTEGER NUM_CHAN, NUM_ADC
      CHARACTER*(*) BOL_TYPE (NUM_CHAN, NUM_ADC)
      INTEGER N_BOLS
      INTEGER BOL_CHAN (N_BOLS)
      INTEGER BOL_ADC (N_BOLS)
      INTEGER N_SAMPLES_IN
      REAL RAW_DATA (3, N_BOLS, N_SAMPLES_IN)
      INTEGER N_SAMPLES_OUT

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL J_SKY (N_BOLS, N_SAMPLES_OUT)
      REAL J_SKY_VARIANCE (N_BOLS, N_SAMPLES_OUT)
      BYTE J_SKY_QUALITY (N_BOLS, N_SAMPLES_OUT)
      REAL J_SKY_AV (N_BOLS)
      REAL J_SKY_AV_VARIANCE (N_BOLS)
      BYTE J_SKY_AV_QUALITY (N_BOLS)

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN                     ! CHR string-length function
      REAL SCULIB_JNU                     ! Rayleigh_Jeans corrected brightness
                                          ! temperature
*  Global variables:

*  Local Constants:
      REAL C                               ! speed of light
      PARAMETER (C = 2.997929E8)

*  Local variables:
      INTEGER      BOL                     ! bolometer number in loop
      INTEGER      BOL_SUB                 ! sub-instrument index for bolometer
      CHARACTER*15 BOLNAME                 ! bolometer name
      INTEGER      FOUND                   ! number of matches
      INTEGER      I                       ! DO loop index
      REAL         J_COLD                  ! brightness temperature of T_COLD
      REAL         J_HOT                   ! brightness temperature of T_HOT
      LOGICAL      MATCH                   ! did we match sub instruments
      REAL         NU                      ! frequency corresponding to WAVE
      INTEGER      SUB                     ! sub-instrument number in DO loop
      REAL         WAVE                    ! wavelength in microns

*    Internal References:

*    Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      FOUND = 0
      BOL = 0

      DO WHILE ((STATUS .EQ. SAI__OK) .AND. (BOL .LT. N_BOLS))
         BOL = BOL + 1

*  get Jhot and Jcold appropriate for the wavelength observed by this bolometer

         WAVE = 0.0
         MATCH = .FALSE.
         DO SUB = 1, N_SUB
            IF (INDEX(BOL_TYPE(BOL_CHAN(BOL),BOL_ADC(BOL)),
     :        SUB_INSTRUMENT(SUB)(:CHR_LEN(SUB_INSTRUMENT(SUB))))
     :        .NE. 0) THEN
               WAVE = WAVELENGTH (SUB)
               BOL_SUB = SUB
               MATCH = .TRUE.
               FOUND = FOUND + 1
            END IF
         END DO

         IF (MATCH) THEN

            IF (WAVE .GT. 0.0) THEN
               NU = C / (WAVE * 1.0E-6)
            ELSE
               CALL SCULIB_BOLNAME (BOL_ADC(BOL), BOL_CHAN(BOL),BOLNAME,
     :              STATUS)
               CALL MSG_SETC ('BOLNAME', BOLNAME)
               STATUS = SAI__ERROR
               CALL ERR_REP (' ','SCULIB_SKYDIP_TEMPERATURES: bad '//
     :              'wavelength for bolometer ^BOLNAME', STATUS)
            END IF

            J_HOT = SCULIB_JNU (NU, T_HOT(BOL_SUB), STATUS)
            J_COLD = SCULIB_JNU (NU, T_COLD(BOL_SUB), STATUS)

            IF (STATUS .EQ. SAI__OK) THEN

*     use J_SKY_AV quality to count number of samples coadded into average

               J_SKY_AV_QUALITY (BOL) = 0
               J_SKY_AV (BOL) = 0.0
               J_SKY_AV_VARIANCE (BOL) = 0.0

               DO I = 1, N_SAMPLES_OUT
                  J_SKY (BOL,I) = 0.0
                  J_SKY_VARIANCE (BOL,I) = 0.0
                  J_SKY_QUALITY (BOL,I) = 1
               END DO

*     If J_HOT or J_COLD are bad dont do the following
               IF (J_HOT .NE. VAL__BADR
     :              .AND. J_COLD .NE. VAL__BADR) THEN


                  DO I = 1, MIN (N_SAMPLES_IN,N_SAMPLES_OUT)

*     calculate sky temperature for sample
*     The voltages are arbritrary so there is nothing
*     special about the number 0.0. Just try to catch
*     the division by zero

                     IF ( ABS(RAW_DATA(1,BOL,I)-RAW_DATA(3,BOL,I))
     :                    .GT.0.00001) THEN

                        J_SKY (BOL,I) = J_COLD +
     :                       (J_HOT - J_COLD) *
     :                       (RAW_DATA(2,BOL,I) - RAW_DATA(3,BOL,I)) /
     :                       (RAW_DATA(1,BOL,I) - RAW_DATA(3,BOL,I))
                        J_SKY_VARIANCE (BOL,I) = 0.0
                        J_SKY_QUALITY (BOL,I) = 0

*     include this sample in the mean

                        J_SKY_AV_QUALITY (BOL) = J_SKY_AV_QUALITY (BOL)
     :                       + 1
                        J_SKY_AV_VARIANCE (BOL) = J_SKY_AV_VARIANCE(BOL)
     :                       + J_SKY (BOL,I) **2
                        J_SKY_AV (BOL) = J_SKY_AV (BOL) + J_SKY (BOL,I)

                     ELSE
                        J_SKY (BOL,I) = 0.0
                        J_SKY_VARIANCE (BOL,I) = 0.0
                        J_SKY_QUALITY (BOL,I) = 1
                     END IF

                  END DO

*     now calculate averages etc.

                  IF (J_SKY_AV_QUALITY (BOL) .EQ. 0) THEN
                     J_SKY_AV_QUALITY (BOL) = 1
                  ELSE IF (J_SKY_AV_QUALITY (BOL) .EQ. 1) THEN
                     J_SKY_AV_VARIANCE (BOL) = 0.0
                     J_SKY_AV_QUALITY (BOL) = 0
                  ELSE
                     J_SKY_AV (BOL) = J_SKY_AV (BOL) /
     :                    REAL (J_SKY_AV_QUALITY(BOL))
                     J_SKY_AV_VARIANCE (BOL) = (J_SKY_AV_VARIANCE(BOL) -
     :                    REAL (J_SKY_AV_QUALITY(BOL))
     :                          * J_SKY_AV(BOL) **2)
     :                    / REAL (J_SKY_AV_QUALITY(BOL) *
     :                    (J_SKY_AV_QUALITY(BOL) - 1))
                     J_SKY_AV_QUALITY (BOL) = 0
                  END IF
               END IF
            END IF
#ifdef DEBUG
         call msg_setr ('t_cold', t_cold(BOL_SUB))
         call msg_setr ('t_hot', t_hot(bol_sub))
         call msg_out (' ', 'sculib_skydip_temperatures: t_cold='//
     :     '^t_cold t_hot=^t_hot', status)

         call sculib_bolname (bol_adc(bol),bol_chan(bol),bolname,
     :     status)
         call msg_setc ('bol',bolname)
         call msg_setr ('wave', wave)
         call msg_setr ('j_cold', j_cold)
         call msg_setr ('j_hot', j_hot)
         call msg_setr ('j_sky', j_sky_av(bol))
         call msg_out (' ', 'bol=^bol wave=^wave j_cold=^j_cold '//
     :     'j_hot=^j_hot j_sky=^j_sky', status)
#endif
         END IF

      END DO

      IF (FOUND .EQ. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','SCULIB_SKYDIP_TEMPERATURES: No bolometers'//
     :        ' matched', STATUS)
      END IF

      END
