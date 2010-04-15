      SUBROUTINE SPD_WZNA( NELM, TEMP, LBCODE, UNFACT, LGCOD1, LGCOD2,
     :   XVAL, DATA, STATUS )
*+
*  Name:
*     SPD_WZNA

*  Purpose:
*     Black body intensities from frequencies or wavelengths.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZNA( NELM, TEMP, LBCODE, UNFACT, LGCOD1, LGCOD2,
*        XVAL, DATA, STATUS )

*  Description:
*     For a given vector of x values, this routine calculates the Planck
*     function of intensity versus wavelength or frequency. This routine
*     is ignorant of the refractive index, it assumes wavelengths to be
*     vacuum wavelengths.
*     Non-logarithmic intensities below 10^-35 Jy/sr are set to 0,
*     Non-logarithmic intensities above 10^+35 Jy/sr are set to bad,

*  Arguments:
*     NELM = INTEGER (Given)
*        The length of arrays XVAL and DATA.
*     TEMP = REAL (Given)
*        The black body temperature in Kelvin.
*     LBCODE = INTEGER (Given)
*        The code number that identifies the label belonging to the
*        spectroscopic values.
*        This must be 201 for frequency or 203 for wavelenth.
*     UNFACT = REAL (Given)
*        The unit conversion factor, or rather its logarithm. For some
*        data labels several units are valid. If e.g. UNITS =
*        'W/(m**2*micron)', then UNFACT = +6, because the standard unit
*        is "W/m**3". UNFACT will also take account of leading powers of
*        ten. If e.g. UNITS = '10**15*Hz', then UNFACT = +15. The value
*        of UNFACT is unaffected by the value of the logarithm code.
*        In the context of this routine, UNFACT must convert to Hz for
*        frequency or to m (metre) for wavelength.
*     LGCOD1 = INTEGER (Given)
*        This code number is 0 or 2 when the spectroscopic values are
*        no logarithms or decadic logarithms respectively.
*     LGCOD2 = INTEGER (Given)
*        This code number is 0 or 2 when the intensity values are to
*        be no logarithms or decadic logarithms respectively.
*     XVAL( NELM ) = REAL (Given)
*        The wavelength or frequency values.
*     DATA( NELM ) = REAL (Returned)
*        The intensity (per frequency interval) values. These will be in
*        Jy/sr or log10(Jy/sr), depending on LGCOD2.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     Lang, K.R., 1980, Astrophysical Formulae, Springer, Heidelberg,
*     Berlin, New York, p. XI, 21

*  Authors:
*     hme: Horst Meyerdierks (UoE)
*     {enter_new_authors_here}

*  History:
*     28 Mar 1991 (hme):
*        Original version (BBODDO).
*     17 Jun 1992 (hme):
*        Handle units and logarithm-codes differently.
*     25 Nov 1994 (hme):
*        Renamed from SPACB.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      INTEGER NELM
      REAL TEMP
      INTEGER LBCODE
      REAL UNFACT
      INTEGER LGCOD1
      INTEGER LGCOD2
      REAL XVAL( NELM )

*  Arguments Returned:
      REAL DATA( NELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION LGC       ! log10(c/(m/s))
      PARAMETER ( LGC = 8.4768207003204D0 )

*  Local Variables:
      INTEGER I                  ! Counter
      DOUBLE PRECISION LGT       ! Temperature
      DOUBLE PRECISION LGNU      ! log10(nu/Hz)

*  Internal References:
      DOUBLE PRECISION SPD_UAAQD ! log10(B_nu/(Jy/sr))

*.

*  Check status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check codes.
      IF ( ( LGCOD1 .NE. 0 .AND. LGCOD1 .NE. 2 ) .OR.
     :     ( LGCOD2 .NE. 0 .AND. LGCOD2 .NE. 2 ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WZND_INVLOG', 'SPD_WZND: Error: ' //
     :      'Cannot handle requested kind of logarithm.', STATUS )
         GO TO 500
      END IF

*  log10(double precision temperature).
      LGT    = LOG10(DBLE(TEMP))

*  If frequency, take logarithm, convert to Hz, get Planck function.
      IF ( LBCODE .EQ. 201 ) THEN

*     If log10(nu).
         IF ( LGCOD1 .EQ. 2 ) THEN
            DO 1 I = 1, NELM
               LGNU = XVAL(I) + UNFACT
               DATA(I) = SPD_UAAQD( LGT, LGNU )
 1          CONTINUE

*     Else (nu).
         ELSE
            DO 2 I = 1, NELM
               LGNU = LOG10(XVAL(I)) + UNFACT
               DATA(I) = SPD_UAAQD( LGT, LGNU )
 2          CONTINUE
         END IF


*  Else if wavelength, take logarithm, convert to frequency in Hz, get
*  Planck function. This is ignorant of refractive index.
      ELSE IF ( LBCODE .EQ. 203 ) THEN

*     If log10(lambda).
         IF ( LGCOD1 .EQ. 2 ) THEN
            DO 3 I = 1, NELM
               LGNU = LGC - XVAL(I) - UNFACT
               DATA(I) = SPD_UAAQD( LGT, LGNU )
 3          CONTINUE

*     Else (nu).
         ELSE
            DO 4 I = 1, NELM
               LGNU = LGC - LOG10(XVAL(I)) - UNFACT
               DATA(I) = SPD_UAAQD( LGT, LGNU )
 4          CONTINUE
         END IF

*  Else, error.
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WZND_INVLAB', 'SPD_WZND: Error: ' //
     :      'Can handle only frequency or wavelength.', STATUS )
         GO TO 500
      END IF

*  So far we have stored log10(B_nu/(Jy/sr)) in DATA. We might want
*  B_nu/(Jy/sr).
      IF ( LGCOD2 .EQ. 0 ) THEN
         DO 5 I = 1, NELM
            IF ( DATA(I) .LT. -35. ) THEN
               DATA(I) = 0.
            ELSE IF ( DATA(I) .GT. +35. ) THEN
               DATA(I) = VAL__BADR
            ELSE
               DATA(I) = 10. ** DATA(I)
            END IF
 5       CONTINUE
      END IF

*  Return.
 500  CONTINUE
      END
