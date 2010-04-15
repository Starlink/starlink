      SUBROUTINE SPD_UAAP( LBCODE, UNITS, LGCODE, UNFACT, STATUS )
*+
*  Name:
*     SPD_UAAP

*  Purpose:
*     Evaluate unit string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAAP( LBCODE, UNITS, LGCODE, UNFACT, STATUS )

*  Description:
*     This routine compares a given unit string with a set of
*     recognised strings and their permitted variations. It returns a
*     code telling whether the data are logarithms and with what base.
*     It also returns a conversion factor. For non-logarithmic data
*     multiplication with the conversion factor converts the data to
*     standard units.

*  Arguments:
*     LBCODE = INTEGER (Given)
*        The label identification code. Codes between 101 and 199
*        identify data labels. Codes 201 to 299 identify labels for the
*        spectroscopic axis. This code is used to decide which unit
*        strings are valid.
*     UNITS = CHARACTER * ( * ) (Given)
*        The unit string to identify.
*     LGCODE = INTEGER (Returned)
*        This code is 0 for non-logarithmic, 1 for natural logarithms
*        or 2 for decadic logarithms. This depends on whether UNITS
*        starts with "ln", "log10", or not.
*     UNFACT = REAL (Returned)
*        The unit conversion factor, or rather its logarithm. For some
*        data labels several units are valid. If e.g. UNITS =
*        'W/(m**2*micron)', then UNFACT = +6, because the standard unit
*        is "W/m**3". UNFACT will also take account of leading powers of
*        ten. If e.g. UNITS = '10**15*Hz', then UNFACT = +15. The value
*        of UNFACT is unaffected by the value of the logarithm code.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     Lang, K.R., 1980, Astrophysical Formulae, Springer, Heidelberg,
*     Berlin, New York, p. XI, 21

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     17 Jun 1992 (hme):
*        Original version.
*     25 Nov 1994 (hme):
*        Renamed from SPACD.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER LBCODE
      CHARACTER * ( * ) UNITS

*  Arguments Returned:
      INTEGER LGCODE
      REAL UNFACT

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER LOCLEN             ! Declared length of local string
      PARAMETER ( LOCLEN = 64 )
      REAL LGHOVE                ! lg[10^26 Hz (h/e)]
      PARAMETER ( LGHOVE = 11.616645 )

*  Local Variables:
      LOGICAL FAILED             ! True as long as no identification
      INTEGER INDEX1, INDEX2     ! String indices
      INTEGER SIGLEN             ! Significant length of UNITS
      INTEGER POWER              ! Leading power of ten
      CHARACTER * ( LOCLEN ) LOCUNT ! Local copy of unit

*  Internal References:
      INTEGER CHR_LEN            ! Significant length of a string
      LOGICAL CHR_SIMLR          ! True if strings are similar

*.

*  Check inherited global status and that label was identified.
      IF ( STATUS .NE. SAI__OK .OR. LBCODE .EQ. 0 ) RETURN

*  Default values.
      LGCODE = 0
      UNFACT = 0.

*  Take a copy of the string to identify.
      SIGLEN = CHR_LEN( UNITS )
      IF ( SIGLEN .GT. LOCLEN ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_UAAP_SIGLEN', 'SPD_UAAP: Error: Given ' //
     :      'unit string contains too many significant characters.',
     :      STATUS )
         GO TO 500
      END IF
      LOCUNT = UNITS

*  Derive the logarithm code and strip off the ln(...) or log10(...).
      IF ( LOCUNT(:3) .EQ. 'ln(' ) THEN
         LGCODE = 1
         SIGLEN = SIGLEN - 1
         LOCUNT = UNITS(4:SIGLEN)
      ELSE IF ( LOCUNT(:6) .EQ. 'log10(' ) THEN
         LGCODE = 2
         SIGLEN = SIGLEN - 1
         LOCUNT = UNITS(7:SIGLEN)
      ELSE
         LGCODE = 0
      END IF

*  Extract any power of ten and strip off the 10**n*.
*  It is not actually stripped off, but INDEX1 will point behind it.
      IF ( LOCUNT(:4) .EQ. '10**' ) THEN

*     Find the '*' behind the power. It is at INDEX1
         CALL CHR_DELIM( LOCUNT(5:), '*', INDEX1, INDEX2 )
         INDEX1 = INDEX1 + 4

*     Try to read the power, trapping any errors.
         READ ( LOCUNT(5:INDEX1-1), *, ERR = 400 ) POWER
         UNFACT = UNFACT + POWER

*     Let INDEX1 point to the rest, i.e. behind "10**n*".
         INDEX1 = INDEX1 + 1
      ELSE
         INDEX1 = 1
      END IF

*  Compare the rest with the valid unit strings.
      FAILED = .TRUE.

*  If intensity.
      IF      ( LBCODE .EQ. 101 ) THEN
         IF      ( CHR_SIMLR(LOCUNT(INDEX1:),'Jy/sr') ) THEN
            FAILED = .FALSE.
         ELSE IF ( CHR_SIMLR(LOCUNT(INDEX1:),'W/(m**2*eV*sr)') ) THEN
            UNFACT = UNFACT + LGHOVE
            FAILED = .FALSE.
         END IF

*  If lambda intensity.
      ELSE IF ( LBCODE .EQ. 102 ) THEN
         IF      ( CHR_SIMLR(LOCUNT(INDEX1:),'W/(m**3*sr)') ) THEN
            FAILED = .FALSE.
         ELSE IF ( CHR_SIMLR(LOCUNT(INDEX1:),'W/(m**2*micron*sr)')) THEN
            UNFACT = UNFACT + 6.
            FAILED = .FALSE.
         ELSE IF ( CHR_SIMLR(LOCUNT(INDEX1:),'W/(m**2*Ang*sr)') ) THEN
            UNFACT = UNFACT + 10.
            FAILED = .FALSE.
         END IF

*  If flux density.
      ELSE IF ( LBCODE .EQ. 103 ) THEN
         IF      ( CHR_SIMLR(LOCUNT(INDEX1:),'Jy') ) THEN
            FAILED = .FALSE.
         ELSE IF ( CHR_SIMLR(LOCUNT(INDEX1:),'W/(m**2*eV)') ) THEN
            UNFACT = UNFACT + LGHOVE
            FAILED = .FALSE.
         END IF

*  If lambda flux density.
      ELSE IF ( LBCODE .EQ. 104 ) THEN
         IF      ( CHR_SIMLR(LOCUNT(INDEX1:),'W/m**3') ) THEN
            FAILED = .FALSE.
         ELSE IF ( CHR_SIMLR(LOCUNT(INDEX1:),'W/(m**2*micron)') ) THEN
            UNFACT = UNFACT + 6.
            FAILED = .FALSE.
         ELSE IF ( CHR_SIMLR(LOCUNT(INDEX1:),'W/(m**2*Ang)') ) THEN
            UNFACT = UNFACT + 10.
            FAILED = .FALSE.
         END IF

*  If AB magnitude, unit should be blank, is ignored here.
      ELSE IF ( LBCODE .EQ. 105 ) THEN
         FAILED = .FALSE.

*  If brightness temperature.
      ELSE IF ( LBCODE .EQ. 106 ) THEN
         IF      ( CHR_SIMLR(LOCUNT(INDEX1:),'K') ) THEN
            FAILED = .FALSE.
         END IF

*  If counts, unit should be blank, is ignored here.
      ELSE IF ( LBCODE .EQ. 107 ) THEN
         FAILED = .FALSE.

*  If count rate.
      ELSE IF ( LBCODE .EQ. 108 ) THEN
         IF      ( CHR_SIMLR(LOCUNT(INDEX1:),'1/s') ) THEN
            FAILED = .FALSE.
         END IF

*  If frequency.
      ELSE IF ( LBCODE .EQ. 201 ) THEN
         IF      ( CHR_SIMLR(LOCUNT(INDEX1:),'Hz') ) THEN
            FAILED = .FALSE.
         END IF

*  If particle energy.
      ELSE IF ( LBCODE .EQ. 202 ) THEN
         IF      ( CHR_SIMLR(LOCUNT(INDEX1:),'eV') ) THEN
            FAILED = .FALSE.
         END IF

*  If wavelength.
      ELSE IF ( LBCODE .EQ. 203 ) THEN
         IF      ( CHR_SIMLR(LOCUNT(INDEX1:),'m') ) THEN
            FAILED = .FALSE.
         ELSE IF ( CHR_SIMLR(LOCUNT(INDEX1:),'micron') ) THEN
            UNFACT = UNFACT - 6.
            FAILED = .FALSE.
         ELSE IF ( CHR_SIMLR(LOCUNT(INDEX1:),'um') ) THEN
            UNFACT = UNFACT - 6.
            FAILED = .FALSE.
         ELSE IF ( CHR_SIMLR(LOCUNT(INDEX1:),'Angstrom') ) THEN
            UNFACT = UNFACT - 10.
            FAILED = .FALSE.
         END IF

*  If radial velocity.
      ELSE IF ( LBCODE .EQ. 204 ) THEN
         IF      ( CHR_SIMLR(LOCUNT(INDEX1:),'m/s') ) THEN
            FAILED = .FALSE.
         ELSE IF ( CHR_SIMLR(LOCUNT(INDEX1:),'km/s') ) THEN
            UNFACT = UNFACT + 3.
            FAILED = .FALSE.
         END IF

*  If redshift, unit should be blank, is ignored here.
      ELSE IF ( LBCODE .EQ. 205 ) THEN
         FAILED = .FALSE.
      END IF

*  Did the identification succeed?
      IF ( FAILED ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_UAAP_INVUNT',
     :      'SPD_UAAP: Error identifying unit. Unit string is ' //
     :      'invalid or incompatible with label code.', STATUS )
         GO TO 500
      END IF

*  Goto 500 anyway, since now comes the READ error trap.
      GO TO 500

*  READ error trap.
 400  CONTINUE
      STATUS = SAI__ERROR
      CALL ERR_REP( 'SPD_UAAP_INVPOW', 'SPD_UAAP: Error reading ' //
     :   'power of ten. Unit string is invalid.', STATUS )

*  Return.
 500  CONTINUE
      END
