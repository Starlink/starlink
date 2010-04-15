      SUBROUTINE SCULIB_DECODE_FILTER (FILTER, N_SUB, SUB_INSTRUMENT,
     :  SUB_FILTER, WAVELENGTH, STATUS)
*+
*  Name:
*     SCULIB_DECODE_FILTER

*  Purpose:
*     decode filter name into names and wavelengths of
*     filters in front of each instrument section in use

*  Description:
*     This routine decodes the filter name to give the name and central
*     wavelength of the filter in front of each sub-instrument being used.
*
*        The filter name should be in format <short>:<long> where these
*     are the names of the filters in front of the short and long-wave
*     focal planes.
*
*        <short > can be:-  '350'     short wavelength = 350.0  name = '350'
*                           '450'                        450.0         '450'
*                           '600'                        600.0         '600'
*                           '750'                        750.0         '750'
*                           '850'                        850.0         '850'
*                           'PHOT'                        -1.0         'BLANK'
*        <long>             '350'           long array = 350.0  name = '350'
*                                  P1100, P1300, P2000   350.0         '350'
*                           '450'           long array = 450.0         '450'
*                                  P1100, P1300, P2000   450.0         '450'
*                           '600'           long array = 600.0         '600'
*                                  P1100, P1300, P2000   600.0         '600'
*                           '750'           long array = 750.0         '750'
*                                  P1100, P1300, P2000   750.0         '750'
*                           '850'           long array = 850.0         '850'
*                                  P1100, P1300, P2000   850.0         '850'
*                           'PHOT'          long array = -1.0          'BLANK'
*                                                P1100 = 1100.0        '1100'
*                                                P1300 = 1300.0        '1300'
*                                                P2000 = 2000.0        '2000'
*
*     If the filter name does not fit the `pattern' described an error message
*     will be output and the routine return with bad status. The routine is
*     insensitive to the case of `PHOT'.
*

*  Invocation:
*     CALL SCULIB_DECODE_FILTER (FILTER, N_SUB, SUB_INSTRUMENT,
*    :  SUB_FILTER, WAVELENGTH, STATUS)

*  Arguments:
*     FILTER                   = CHARACTER*(*) (Given)
*           name of filter
*     N_SUB                    = INTEGER (Given)
*           number of sub instruments being used
*     SUB_INSTRUMENT (N_SUB)   = CHARACTER*(*) (Given)
*           the names of the sub instruments being used
*     SUB_FILTER (N_SUB)       = CHARACTER*(*) (Returned)
*           the name of the filter in front of each sub-instrument
*     WAVELENGTH (N_SUB)       = REAL (Returned)
*           the wavelength in microns of the filters in front of the sub
*           instruments being used. An inappropriate filter will have
*           wavelength set to VAL__BADR
*     STATUS                   = INTEGER (Given and returned)
*           global status

*  Notes:
*     The wavelength values are probably out of step with those currently
*     in use by the real-time system.


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
*     24-MAR-1993: Original version.
*     30-APR-1993: Modified to give wavelengths as real numbers rather than
*                  strings
*      4-OCT-1994: Modified to return filter names as well as wavelengths
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) FILTER
      INTEGER N_SUB
      CHARACTER*(*) SUB_INSTRUMENT (N_SUB)

*  Arguments Given & Returned:

*  Arguments Returned:
      CHARACTER*(*) SUB_FILTER (N_SUB)
      REAL WAVELENGTH (N_SUB)

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER      DELIM
      CHARACTER*20 LONG
      CHARACTER*20 LONG_NAME
      REAL         LONG_WAVE
      CHARACTER*20 P1100_NAME
      REAL         P1100_WAVE
      CHARACTER*20 P1300_NAME
      REAL         P1300_WAVE
      CHARACTER*20 P2000_NAME
      REAL         P2000_WAVE
      CHARACTER*20 SHORT
      CHARACTER*20 SHORT_NAME
      REAL         SHORT_WAVE
      INTEGER      SUB

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      DELIM = INDEX (FILTER, ':')
      IF (DELIM .LE. 1) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC ('FILTER', FILTER)
         CALL ERR_REP (' ', 'SCULIB_DECODE_FILTER: bad filter name '//
     :     '^FILTER', STATUS)
      ELSE

         SHORT = FILTER (1:DELIM-1)
         LONG = FILTER (DELIM+1:)
         CALL CHR_UCASE (SHORT)
         CALL CHR_UCASE (LONG)

         SHORT_NAME = SHORT

         IF (SHORT .EQ. '350') THEN
            SHORT_WAVE = 350.0
         ELSE IF (SHORT .EQ. '450') THEN
            SHORT_WAVE = 450.0
         ELSE IF (SHORT .EQ. '600') THEN
            SHORT_WAVE = 600.0
         ELSE IF (SHORT .EQ. '750') THEN
            SHORT_WAVE = 750.0
         ELSE IF (SHORT .EQ. '850') THEN
            SHORT_WAVE = 850.0
         ELSE IF (SHORT .EQ. 'PHOT') THEN
            SHORT_NAME = 'BLANK'
            SHORT_WAVE = -1.0
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC ('SHORT', SHORT)
            CALL ERR_REP (' ','SCULIB_DECODE_FILTER: bad filter '//
     :        '^SHORT in front of short-wave array', STATUS)
         END IF


         LONG_NAME = LONG
         P1100_NAME = LONG
         P1300_NAME = LONG
         P2000_NAME = LONG

         IF (LONG .EQ. '350') THEN
            LONG_WAVE = 350.0
            P1100_WAVE = 350.0
            P1300_WAVE = 350.0
            P2000_WAVE = 350.0
         ELSE IF (LONG .EQ. '450') THEN
            LONG_WAVE = 450.0
            P1100_WAVE = 450.0
            P1300_WAVE = 450.0
            P2000_WAVE = 450.0
         ELSE IF (LONG .EQ. '600') THEN
            LONG_WAVE = 600.0
            P1100_WAVE = 600.0
            P1300_WAVE = 600.0
            P2000_WAVE = 600.0
         ELSE IF (LONG .EQ. '750') THEN
            LONG_WAVE = 750.0
            P1100_WAVE = 750.0
            P1300_WAVE = 750.0
            P2000_WAVE = 750.0
         ELSE IF (LONG .EQ. '850') THEN
            LONG_WAVE = 850.0
            P1100_WAVE = 850.0
            P1300_WAVE = 850.0
            P2000_WAVE = 850.0
         ELSE IF (LONG .EQ. 'PHOT') THEN
            LONG_NAME = 'BLANK'
            P1100_NAME = '1100'
            P1300_NAME = '1300'
            P2000_NAME = '2000'
            LONG_WAVE = -1.0
            P1100_WAVE = 1100.0
            P1300_WAVE = 1300.0
            P2000_WAVE = 2000.0
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC ('LONG', LONG)
            CALL ERR_REP (' ','SCULIB_DECODE_FILTER: bad filter '//
     :           '^LONG in front of long-wave array', STATUS)
         END IF

      END IF

      IF ((STATUS .EQ. SAI__OK) .AND. (N_SUB .GE. 1)) THEN
         DO SUB = 1, N_SUB
            IF (INDEX(SUB_INSTRUMENT(SUB),'SHORT').NE.0) THEN
               SUB_FILTER (SUB) = SHORT_NAME
               WAVELENGTH (SUB) = SHORT_WAVE
            ELSE IF (INDEX(SUB_INSTRUMENT(SUB),'LONG').NE.0) THEN
               SUB_FILTER (SUB) = LONG_NAME
               WAVELENGTH (SUB) = LONG_WAVE
            ELSE IF (INDEX(SUB_INSTRUMENT(SUB),'P1100').NE.0) THEN
               SUB_FILTER (SUB) = P1100_NAME
               WAVELENGTH (SUB) = P1100_WAVE
            ELSE IF (INDEX(SUB_INSTRUMENT(SUB),'P1300').NE.0) THEN
               SUB_FILTER (SUB) = P1300_NAME
               WAVELENGTH (SUB) = P1300_WAVE
            ELSE IF (INDEX(SUB_INSTRUMENT(SUB),'P2000').NE.0) THEN
               SUB_FILTER (SUB) = P2000_NAME
               WAVELENGTH (SUB) = P2000_WAVE
            END IF
         END DO
      END IF

      END
