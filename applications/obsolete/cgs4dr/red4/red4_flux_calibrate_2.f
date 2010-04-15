*+  RED4_FLUX_CALIBRATE_2, general flux manipulation procedure
      SUBROUTINE RED4_FLUX_CALIBRATE_2( FVALUE, BAND, INPUT, OUTPUT,
     :  RFLAMBDA, FLUX, STATUS )
*    Description :
*     This routine accepts magnitudes and fluxes and outputs a flux
*     value in selected units.
*      CALL SUBROUTINE RED4_FLUX_CALIBRATE_2( FVALUE, BAND, INPUT, OUTPUT,
*     :  RFLAMBDA, FLUX, STATUS )
*    Parameters :
*     FVALUE    = REAL( READ )
*           The input flux or magnitude (makes no check to see if sensible)
*     BAND      = CHAR( READ )
*           The input band for the flux or magnitude
*           Expects one of V, J, H, K, L, L', M, N, Q or 'Flux'
*     INPUT     = CHAR( READ )
*           The input units of FVALUE
*           Expects one of MAG, W/M2/UM, W/M2/HZ, ERGS/S/CM2/UM or MJY
*     OUTPUT    = CHAR( READ )
*           The output units of FVALUE
*           Expects one of MAG, W/M2/UM, W/M2/HZ, ERGS/S/CM2/UM or MJY
*           and encodes it for PGPLOT PGTEXT output.
*     RFLAMBDA  = REAL( READ )
*           The standard or BB reference wavelength (in microns)
*     FLUX      = REAL( UPDATE )
*           The output flux in the given units based upon values
*           for Vega as given in the IRTF Photometry Handbook (Feb 1986)
*    STATUS     = INTEGER( UPDATE )
*           The global ADAM status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     P N Daly (JACH::PND)
*    History :
*      2-Sep-1992: Original version.                                 (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'             ! Contains SAI__ERROR
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
      INTEGER NINFO                 ! Number of information items
      PARAMETER ( NINFO = 2 )
      REAL SPEED_OF_LIGHT           ! Speed of light in microns /second!
      PARAMETER ( SPEED_OF_LIGHT = 3.0E+14 )
*    Local variables :
      CHARACTER*(*)
     :  BAND,                       ! Flux band
     :  INPUT,                      ! Input units
     :  OUTPUT                      ! Output units
      REAL
     :  FVALUE,                     ! Flux or magnitude value
     :  RFLAMBDA,                   ! Reference wavelength of standard
     :  FLUX,                       ! Output flux in appropriate units
     :  DELTA_LAMBDA,               ! Difference in ref lambda and known
     :  KNOWN_LAMBDA(0:15),         ! Wavelength of band (microns)
     :  KNOWN_FLUX1(0:15),          ! Vega flux in W/m2/um
     :  KNOWN_FLUX2(0:15),          ! Vega flux in W/m2/Hz
     :  KNOWN_FLUX3(0:15),          ! Vega flux in ergs/s/cm2/um
     :  KNOWN_FLUX4(0:15)           ! Vega flux in mJy
      DOUBLE PRECISION
     :  FLUX1,                      ! Output flux in W/m2/um
     :  FLUX2,                      ! Output flux in W/m2/Hz
     :  FLUX3,                      ! Output flux in ergs/s/cm2/um
     :  FLUX4                       ! Output flux in mJy
      INTEGER
     :  ARRAY_POS,                  ! Array posiiton
     :  OUTPUT_POS,                 ! Output position
     :  INPUT_POS                   ! Input position
      CHARACTER*3
     :  KNOWN_BANDS(0:15)           ! Known flux bands
*    Internal References :
*    Local data : from IRTF Photometry Handbook, February 1986
      DATA KNOWN_BANDS / ' ',
     :   'V', 'J', 'H', 'K', 'L', 'L''', 'M', ' ', ' ', ' ',
     :   'N', ' ', ' ', ' ', 'Q'/
      DATA KNOWN_LAMBDA /0.0E00,
     :   0.5556, 1.25, 1.65, 2.20, 3.45, 3.80, 4.80, 7.8, 8.7,
     :   9.8, 10.1, 10.3, 11.6, 12.5, 20.0 /
      DATA KNOWN_FLUX1 /0.0E00,
     :   3.44E-08, 3.07E-09, 1.12E-09, 4.07E-10, 7.30E-11,
     :   5.24E-11, 2.12E-11, 3.22E-12, 2.10E-12, 1.32E-12,
     :   1.17E-12, 1.09E-12, 6.81E-13, 5.07E-13, 7.80E-14/
      DATA KNOWN_FLUX2 /0.0E00,
     :   3.54E-23, 1.60E-23, 1.02E-23, 6.57E-24, 2.90E-24,
     :   2.52E-24, 1.63E-24, 6.53E-25, 5.30E-25, 4.23E-25,
     :   3.98E-25, 3.85E-25, 3.05E-25, 2.64E-25, 1.04E-25/
      DATA KNOWN_FLUX3 /0.0E00,
     :   3.44E-05, 3.07E-06, 1.12E-06, 4.07E-07, 7.30E-08,
     :   5.24E-08, 2.12E-08, 3.22E-09, 2.10E-09, 1.32E-09,
     :   1.17E-09, 1.09E-09, 6.81E-10, 5.07E-10, 7.80E-11/
      DATA KNOWN_FLUX4 /0.0E00,
     :   3.54E+06, 1.60E+06, 1.02E+06, 6.57E+05, 2.90E+05,
     :   2.52E+05, 1.63E+05, 6.53E+04, 5.30E+04, 4.23E+04,
     :   3.98E+04, 3.85E+04, 3.05E+04, 2.64E+04, 1.04E+04/
*-

*    Exit is status on entry is bad
      IF ( STATUS .NE. ADAM__OK ) RETURN

*    Start by upper casing all input character strings
      CALL CHR_UCASE( BAND )
      CALL CHR_UCASE( INPUT )
      CALL CHR_UCASE( OUTPUT )

*    Set the output label encoded for PGTEXT
      IF ( OUTPUT .EQ. 'W/M2/UM' ) THEN

         OUTPUT = 'W/m.sq./micron'
         OUTPUT_POS = 1
      ELSE IF ( OUTPUT .EQ. 'W/M2/HZ' ) THEN

         OUTPUT = 'W/m.sq./Hz'
         OUTPUT_POS = 2
      ELSE IF ( OUTPUT .EQ. 'ERGS/S/CM2/UM' ) THEN

         OUTPUT = 'ergs/sec/cm.sq./micron'
         OUTPUT_POS = 3
      ELSE IF ( OUTPUT .EQ. 'MJY' ) THEN

         OUTPUT = 'mJy'
         OUTPUT_POS = 4
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_FLUX_CALIBRATE_2: '/
     :     /'Unknown output units!', STATUS)
      END IF

*    Set the input position
      IF ( INPUT .EQ. 'MAG' ) THEN

         INPUT_POS = 0
      ELSE IF ( INPUT .EQ. 'W/M2/UM' ) THEN

         INPUT_POS = 1
      ELSE IF ( INPUT .EQ. 'W/M2/HZ' ) THEN

         INPUT_POS = 2
      ELSE IF ( INPUT .EQ. 'ERGS/S/CM2/UM' ) THEN

         INPUT_POS = 3
      ELSE IF ( INPUT .EQ. 'MJY' ) THEN

         INPUT_POS = 4
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_FLUX_CALIBRATE_2: '/
     :     /'Unknown input units!', STATUS)
      END IF

*     Do we have a known magnitude band?
      IF ( BAND .EQ. 'V' ) THEN                    ! V band

         ARRAY_POS = 1
      ELSE IF ( BAND .EQ. 'J' ) THEN               ! J band

         ARRAY_POS = 2
      ELSE IF ( BAND .EQ. 'H' ) THEN               ! H band

         ARRAY_POS = 3
      ELSE IF ( BAND .EQ. 'K' ) THEN               ! K band

         ARRAY_POS = 4
      ELSE IF ( BAND .EQ. 'L' ) THEN               ! L band

         ARRAY_POS = 5
      ELSE IF ( BAND .EQ. 'L''' ) THEN             ! L' band

         ARRAY_POS = 6
      ELSE IF ( BAND .EQ. 'M' ) THEN               ! M band

         ARRAY_POS = 7
      ELSE IF ( BAND .EQ. 'N' ) THEN               ! N band

         ARRAY_POS = 11
      ELSE IF ( BAND .EQ. 'Q' ) THEN               ! Q band

         ARRAY_POS = 15
      ELSE IF ( BAND .EQ. 'FLUX' ) THEN            ! Flux

         ARRAY_POS = 0
         KNOWN_LAMBDA( ARRAY_POS ) = RFLAMBDA
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_FLUX_CALIBRATE_2: '/
     :     /'Unknown magnitude band or flux option', STATUS)
      ENDIF

*    Report the difference between the reference lambda and the band
      DELTA_LAMBDA = ABS( RFLAMBDA - KNOWN_LAMBDA( ARRAY_POS ) )
      CALL MSG_SETR( 'DELTA_LAMBDA', DELTA_LAMBDA )
      CALL MSG_OUT( ' ',
     :  'Reference and band wavelength differ by '/
     :  /'^DELTA_LAMBDA microns', STATUS )

*    First case:  known magnitude band in magnitude units
      IF ( ( BAND .NE. 'FLUX' ) .AND. ( INPUT_POS .EQ. 0 ) ) THEN

         IF ( (FVALUE.LT.-50.0) .OR. (FVALUE.GT.50.0) ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_FLUX_CALIBRATE_2: '/
     :        /'Magnitude out of range (-50 < Mag < 50)', STATUS )
         END IF

         FLUX1 = KNOWN_FLUX1(ARRAY_POS)*(10**(-0.4*FVALUE))
         FLUX2 = KNOWN_FLUX2(ARRAY_POS)*(10**(-0.4*FVALUE))
         FLUX3 = KNOWN_FLUX3(ARRAY_POS)*(10**(-0.4*FVALUE))
         FLUX4 = KNOWN_FLUX4(ARRAY_POS)*(10**(-0.4*FVALUE))

         IF ( OUTPUT_POS .EQ. 1 ) FLUX = REAL(FLUX1)
         IF ( OUTPUT_POS .EQ. 2 ) FLUX = REAL(FLUX2)
         IF ( OUTPUT_POS .EQ. 3 ) FLUX = REAL(FLUX3)
         IF ( OUTPUT_POS .EQ. 4 ) FLUX = REAL(FLUX4)

         CALL MSG_SETR( 'FLUX', FLUX )
         CALL MSG_OUT( ' ',
     :      'The flux at the band wavelength is ^FLUX', STATUS )

         GOTO 500
      END IF

* Second case:  Flux is given and we wish to convert units (probably)
      IF ( ( BAND .EQ. 'FLUX' ) .AND. ( INPUT_POS .GT. 0 ) ) THEN

         IF ( INPUT_POS .EQ. 1 ) THEN

            FLUX1 = FVALUE
            FLUX2 = FVALUE * (RFLAMBDA**2) / SPEED_OF_LIGHT
            FLUX3 = FVALUE * 1000
            FLUX4 = FVALUE * (RFLAMBDA**2) * (1.0E29) / SPEED_OF_LIGHT
         ELSE IF ( INPUT_POS .EQ. 2 ) THEN

            FLUX1 = FVALUE * SPEED_OF_LIGHT / (RFLAMBDA**2)
            FLUX2 = FVALUE
            FLUX3 = FVALUE * SPEED_OF_LIGHT * 1000 / (RFLAMBDA**2)
            FLUX4 = FVALUE * (1.0E29)
         ELSE IF ( INPUT_POS .EQ. 3 ) THEN

            FLUX1 = FVALUE * 0.001
            FLUX2 = FVALUE * (RFLAMBDA**2) * 0.001 / SPEED_OF_LIGHT
            FLUX3 = FVALUE
            FLUX4 = FVALUE * (RFLAMBDA**2) * (1.0E26) / SPEED_OF_LIGHT
         ELSE IF ( INPUT_POS .EQ. 4 ) THEN

            FLUX1 = FVALUE * SPEED_OF_LIGHT * (1.0E-29) / (RFLAMBDA**2)
            FLUX2 = FVALUE * (1.0E-29)
            FLUX3 = FVALUE * SPEED_OF_LIGHT * (1.0E-26) / (RFLAMBDA**2)
            FLUX4 = FVALUE
         END IF

         IF ( OUTPUT_POS .EQ. 1 ) FLUX = REAL(FLUX1)
         IF ( OUTPUT_POS .EQ. 2 ) FLUX = REAL(FLUX2)
         IF ( OUTPUT_POS .EQ. 3 ) FLUX = REAL(FLUX3)
         IF ( OUTPUT_POS .EQ. 4 ) FLUX = REAL(FLUX4)

         CALL MSG_SETR( 'FLUX', FLUX )
         CALL MSG_OUT( ' ',
     :      'The flux at the reference wavelength is ^FLUX', STATUS )

         GOTO 500
      END IF

*    Exit from this subroutine
 500  CONTINUE
      END
