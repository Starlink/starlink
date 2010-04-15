      SUBROUTINE SCULIB_CROSSTALK (SWITCH_PER_EXP, EXP_PER_INT,
     :  N_INTEGRATIONS, N_MEASUREMENTS, DEMOD_POINTER, N_BOLS,
     :  NUMPOS, DATA, BOL_CHAN, BOL_ADC, STATUS)
*+
*  Name:
*     SCULIB_CROSSTALK

*  Purpose:
*     crosstalk measurements

*  Description:
*     Experimental routine for calculating crosstalk between
*     A-to-D cards. Averages signal for each bolometer and writes
*     to a file. The file name comes from the FILE ADAM parameter.

*  Invocation:
*     CALL SCULIB_CROSSTALK (SWITCH_PER_EXP, EXP_PER_INT,
*    :  N_INTEGRATIONS, N_MEASUREMENTS, DEMOD_POINTER, N_BOLS,
*    :  NUMPOS, DATA, BOL_CHAN, BOL_ADC, STATUS)

*  Arguments:
*     SWITCH_PER_EXP         = INTEGER (Given)
*           number of switches per exposure
*     EXP_PER_INT            = INTEGER (Given)
*           number of exposures per integration
*     N_INTEGRATIONS         = INTEGER (Given)
*           number of integrations
*     N_MEASUREMENTS         = INTEGER (Given)
*           number of measurments
*     DEMOD_POINTER (SWITCH_PER_EXP, EXP_PER_INT, N_INTEGRATIONS,
*       N_MEASUREMENTS)      = INTEGER (Given)
*           pointer to start of demodulated data for each switch in
*           DATA array
*     N_BOLS                 = INTEGER (Given)
*           number of bolometers being measured
*     NUMPOS                 = INTEGER (Given)
*           total number of positions measured
*     DATA (4, N_BOLS, NUMPOS)
*                            = REAL (Given)
*           the demodulated data
*     BOL_CHAN (N_BOLS)      = INTEGER (Given)
*           the channel number of the measured bolometers
*     BOL_ADC (N_BOLS)       = INTEGER (Given)
*           the ADC numbers of the measured bolometers
*     STATUS                 = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (JFL/ROE)

*  Copyright:
*     Copyright (C) 1995,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     13-JAN-1995: Orginal version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER SWITCH_PER_EXP
      INTEGER EXP_PER_INT
      INTEGER N_INTEGRATIONS
      INTEGER N_MEASUREMENTS
      INTEGER DEMOD_POINTER (SWITCH_PER_EXP, EXP_PER_INT,
     :  N_INTEGRATIONS, N_MEASUREMENTS)
      INTEGER N_BOLS
      INTEGER NUMPOS
      REAL DATA (4, N_BOLS, NUMPOS)
      INTEGER BOL_CHAN (N_BOLS)
      INTEGER BOL_ADC (N_BOLS)

*  Arguments Given & Returned:

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER BOL                        ! index of bolometer data in array
      INTEGER BOL_NUMBER                 ! number of numbers in average
      REAL    BOL_SUM                    ! sum and average
      CHARACTER*80 BUFFER                ! FIO buffer
      INTEGER EXPOSURE                   ! exposure number
      INTEGER FD                         ! FIO file identifier
      INTEGER INTEGRATION                ! integration number
      INTEGER MEAS                       ! measurement number
      INTEGER OFFSET                     ! offset in data array
      INTEGER SWITCH                     ! switch number

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*  open the file

      CALL FIO_ASSOC ('FILE', 'WRITE', 'FORTRAN', 0, FD, STATUS)

*  work through the bolometers

      IF (STATUS .EQ. SAI__OK) THEN
         DO BOL = 1, N_BOLS

            BOL_SUM = 0.0
            BOL_NUMBER = 0

*  now work through switches, exposures, integrations and measurements
*  coadding the results for each bolometer

            DO MEAS = 1, N_MEASUREMENTS
               DO INTEGRATION = 1, N_INTEGRATIONS
                  DO EXPOSURE = 1, EXP_PER_INT
                     DO SWITCH = 1, SWITCH_PER_EXP

      if (bol .eq. 1) then
         print *, bol, meas, integration, exposure,switch,
     :     data(1,bol,demod_pointer(switch,exposure,integration,
     :     meas))
      end if
                        OFFSET = DEMOD_POINTER (SWITCH, EXPOSURE,
     :                    INTEGRATION, MEAS)
                        IF (NINT(DATA(4,BOL,OFFSET)) .EQ. 0) THEN
                           BOL_SUM = BOL_SUM + DATA (1, BOL, OFFSET)
                           BOL_NUMBER = BOL_NUMBER + 1
                        END IF

                     END DO
                  END DO
               END DO
            END DO

*  calculate average and write it out

            IF (BOL_NUMBER .GT. 0) THEN
               BOL_SUM = BOL_SUM / REAL (BOL_NUMBER)
            END IF
            WRITE (BUFFER, 10) BOL_CHAN(BOL), BOL_ADC(BOL), BOL_SUM
            CALL FIO_WRITE (FD, BUFFER, STATUS)

 10         FORMAT (2I5, E12.3)

         END DO

*  close the file

         CALL FIO_CANCL ('FILE', STATUS)

      END IF

      END
