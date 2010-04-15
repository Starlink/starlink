      SUBROUTINE PREPB0( BAND, PFACT, INDF1, NCARD, FITS, TYPE, UNITS,
     :                   PIXSIZ, SCALE, ZERO, U, FACTOR, STATUS )
*+
*  Name:
*     PREPB0

*  Purpose:
*     Get the transformation for converting input data to output data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPB0( BAND, PFACT, INDF1, NCARD, FITS, TYPE, UNITS,
*                  PIXSIZ, SCALE, ZERO, U, FACTOR, STATUS )

*  Description:
*     This routine returns two values SCALE and ZERO such that the
*     values to be stored in the output data array are equal to
*     SCALE*IN+ZERO where IN is a data value from the input NDF. These
*     values take account of the following scalings:
*
*     1) If the input is not in floating point format (either _REAL or
*     _DOUBLE) then it is assumed that the scaling defined by the FITS
*     keywords BSCALE and BZERO has not been applied to the input data.
*     In this case, these scalings are included in the SCALE and ZERO
*     values returned by this routine.
*
*     2) If the output data is required to be in different units to the
*     input data, then the conversion from the input system to the
*     output system is included. If either the input or output units
*     are unknown, then the scaling factor for this conversion is
*     obtained from the environment using the parameter specified by
*     PFACT.
*
*     3) If the input is a PO image, the FITS keyword BIAS will hold a
*     further base level offset to be added on to the output data
*     values.

*  Arguments:
*     BAND = INTEGER (Given)
*        The survey or CPC waveband index.
*     PFACT = CHARACTER * ( * ) (Given)
*        The parameter to use when getting the conversion factor between
*        unknown units.
*     INDF1 = INTEGER (Given)
*        The NDF identifier for the output NDF.
*     NCARD = INTEGER (Given)
*        The number of FITS header cards.
*     FITS( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS header cards.
*     TYPE = CHARACTER * ( * ) (Given)
*        The image type. This should be equal to one of the symbolic
*        constants defined within the IRI subsystem.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units required for the output NDF. If this is supplied
*        blank, then the units are left in the same system as the
*        input.
*     PIXSIZ = DOUBLE PRECISION (Given)
*        The nominal solid angle of an output pixel, in steradians.
*     SCALE = REAL (Returned)
*        The scale factor for converting input data to output data.
*     ZERO = REAL (Returned)
*        The zero offset for converting input data to output data.
*     U = CHARACTER * ( * )  (Returned)
*        The units actually used for the output. If UNITS is supplied
*        with a blank value, then this will be derived from the units of
*        the input NDF. Otherwise, it will equal UNITS.
*     FACTOR = REAL (Returned)
*        The scale factor for converting between the input and output
*        units. This excludes the effects of FITS keywords BSCALE, BZERO
*        and BIAS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'IRI_PAR'          ! IRI_ constants

*  Arguments Given:
      INTEGER BAND
      CHARACTER PFACT*(*)
      INTEGER INDF1
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)
      CHARACTER TYPE*(*)
      CHARACTER UNITS*(*)
      DOUBLE PRECISION PIXSIZ

*  Arguments Returned:
      REAL SCALE
      REAL ZERO
      CHARACTER U*(*)
      REAL FACTOR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! True if 2 strings are equal apart
                                 ! from case.

*  Local Variables:
      CHARACTER DTYPE*(NDF__SZTYP) ! Data type of input NDF.


      INTEGER CARD               ! FITS header card in which the keyword
                                 ! was found.


      LOGICAL FLUX               ! True if image is a PO FLUX grid.
      LOGICAL NOISE              ! True if image is a PO noise map.
      LOGICAL OK                 ! True if input units are standard.
      LOGICAL THERE              ! True if the keyword was found.


      REAL BIAS                  ! Value of BIAS keyword.
      REAL BSCALE                ! Value of BSCALE keyword.
      REAL BZERO                 ! Value of BZERO keyword.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the data type of the input NDF.
      CALL NDF_TYPE( INDF1, 'DATA', DTYPE, STATUS )

*  If the NDF has a data type other than _REAL or _DOUBLE, the scaling
*  described by FITS keywords BSCALE and BZERO must be applied to the
*  output data values.  type conversion is necessary.
      IF ( DTYPE .NE. '_REAL' .AND. DTYPE .NE. '_DOUBLE' ) THEN

*  Get the FITS keyword BSCALE.
         CALL IRM_GKEYR( NCARD, FITS, 1, 'BSCALE', THERE, BSCALE, CARD,
     :                   STATUS )

*  If no such keyword exists in the FITS header, use a value of 1.0
         IF( .NOT. THERE ) BSCALE = 1.0

*  Get the FITS keyword BZERO.
         CALL IRM_GKEYR( NCARD, FITS, 1, 'BZERO', THERE, BZERO, CARD,
     :                   STATUS )

*  If no such keyword exists in the FITS header, use a value of 0.0
         IF( .NOT. THERE ) BZERO = 0.0

*  If the data type is _REAl or _DOUBLE use default BSCALE and BZERO
*  values.
      ELSE
         BSCALE = 1.0
         BZERO = 0.0
      END IF

*  Get the 'BUNIT' keyword and remove blanks.
      CALL IRM_GKEYC( NCARD, FITS, 1, 'BUNIT', THERE, U, CARD, STATUS )
      CALL CHR_RMBLK( U )

*  If the value of keyword BUNIT corresponds to one of the standard IRI
*  system of units except for differences of case, replace U with the
*  corresponding standard system. This involves correcting the case of
*  each character so that it conforms with one of the standards.
      CALL IRI_CHECK( U, OK, STATUS )

*  If the input NDF is a PO image, see what type it is.
      IF( TYPE .EQ. IRI__DSCO ) THEN
         CALL PREPC7( NCARD, FITS, FLUX, NOISE, STATUS )

*  If the input NDF is a PO intensity data map, get the value of the
*  BIAS keyword.
         IF( .NOT. (FLUX .OR. NOISE ) ) THEN
            CALL IRM_GKEYR( NCARD, FITS, 1, 'BIAS', THERE, BIAS, CARD,
     :                      STATUS )

*  If the keyword was not found, use zero.
            IF( .NOT. THERE ) BIAS = 0.0

*  If the input is of any other type, use a bias of zero.
         ELSE
            BIAS = 0.0
         END IF

      ELSE
         BIAS = 0.0
      END IF

*  If the input has units of JY or PW/M**2, change them to JY/PIXEL and
*  (PW/M**2)/PIXEL, respectively.
      IF( CHR_SIMLR( U, 'JY' ) ) THEN
         U = IRI__JPP

      ELSE IF( CHR_SIMLR( U, 'PW/M**2') .OR.
     :         CHR_SIMLR( U, 'PW/(M**2)' ) ) THEN
         U = IRI__FPP

      END IF

*  Get the scaling factor between the input and output units.
      CALL PREPC0( PFACT, TYPE, BAND, PIXSIZ, UNITS, U, FACTOR, STATUS )

*  Find the total scale and total zero to be applied to the input data
*  to get the true data value.
      SCALE = FACTOR*BSCALE
      ZERO = FACTOR*( BZERO + BIAS )

      END
