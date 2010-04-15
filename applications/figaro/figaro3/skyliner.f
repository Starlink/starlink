      SUBROUTINE SKYLINER( STATUS )
*+
*  Name:
*     SKYLINER

*  Purpose:
*     Removes a sky spectrum normalised by the height of the 5577 [OI]
*     emission line.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SKYLINER( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Removes a sky spectrum normalised by the height of the 5577 [OI]
*     emission line.

*  Usage:
*     SKYLINER IN SKY OUT

*  ADAM Parameters:
*     IN = NDF (Read)
*        The one- or two-dimensional spectrum to be sky-subtracted.
*     SKY = NDF (Read)
*        The one-dimensional sky spectrum.  It must not contain bad
*        pixels as they could affect the estimation of the [OI] line
*        strength.
*     OUT = NDF (Write)
*        The sky-subtracted one- or two-dimensional spectrum.
*        [parameter_default]
*     [parameter_spec]...

*  [examples]
*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     ACD: A C Davenhall (Edinburgh)
*     {enter_new_authors_here}

*  History:
*     1994 October 13 (MJC):
*        Original version.
*     1998 October 27 (ACD)
*        Added a minimal entry for the 'Description' prologue section.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Dimensionality of object spectra
      PARAMETER ( NDIM = 2 )
      INTEGER SDIM               ! Dimensionality of sky spectrum
      PARAMETER ( SDIM = 1 )
      INTEGER NLINES             ! Number of emission lines to
                                 ! interpolate across
      PARAMETER ( NLINES = 1 )
      INTEGER NWAVE              ! Number of wavelengths
      PARAMETER ( NWAVE = 3 )

*  Local Variables:
      LOGICAL BAD                ! True if data array contains bad
                                 ! values
      REAL CALLAM( NWAVE )       ! Wavelengths of the start, centre and
                                 ! end of the OI emission line
      INTEGER DIM( NDIM )        ! Dimensions of object NDFs
      INTEGER EL                 ! Number of elements in object NDFs
      INTEGER FPIND( 2, NLINES ) ! Indices of pixels that lie just above
                                 ! and below the emission lines
      INTEGER I                  ! Loop counter
      INTEGER IAEL               ! Number of elements along wavelength
                                 ! axis of object spectrum
      INTEGER IALCO              ! Lower-bound of axis centres of object
                                 ! spectra
      INTEGER IAPNTR( 1 )        ! Pointer to mapped object axis
      INTEGER IAUCO              ! Upper-bound of axis centres of object
                                 ! spectra
      INTEGER IPNTR( 1 )         ! Pointer to mapped input
                                 ! object-spectrum data array
      REAL LINLAM( 2, NLINES )   ! Wavelengths of the start and
                                 ! end of the emission lines to
                                 ! interpolate across
      INTEGER LBND( NDIM )       ! Lower bounds of object spectra
      INTEGER LBNDS( SDIM )      ! Lower bounds of sky spectrum
      INTEGER NDIMI              ! Number of dimensions in object
                                 ! spectrum
      INTEGER NDIMS              ! Number of dimensions in sky spectrum
      INTEGER NDFI               ! Identifier for input object spectrum
      INTEGER NDFO               ! Identifier for output object spectrum
      INTEGER NDFS               ! Identifier for sky spectrum
      INTEGER OPNTR( 1 )         ! Pointer to mapped output
                                 ! object-spectrum data array
      INTEGER PIND( NWAVE )      ! Indices of pixels that lie just below
                                 ! the OI bounds and centre in spectrum
      REAL RPIND                 ! Pixel index of emission line's
                                 ! nominal central wavelength
      INTEGER SAEL               ! Number of elements in sky spectrum
      INTEGER SALCO              ! Lower-bound of axis centres of sky
                                 ! spectra
      INTEGER SAPNTR( 1 )        ! Pointer to mapped sky-spectrum axis
      INTEGER SAUCO              ! Upper-bound of axis centres of sky
                                 ! spectra
      CHARACTER * ( 16 ) SLABEL  ! Sky-spectrum axis label
      REAL SLAM                  ! Wavelength of pixel centres (not
                                 ! used)
      INTEGER SPNTR( 1 )         ! Pointer to mapped sky spectrum data
      REAL SSCALE                ! Scale factor for sky-axis units to A
      CHARACTER * ( 16 ) SUNITS  ! Sky-spectrum axis units
      LOGICAL THERE              ! True if a NDF component is defined
      INTEGER UBND( NDIM )       ! Upper bounds of object spectra
      INTEGER UBNDS( SDIM )      ! Upper bounds of sky spectrum
      CHARACTER * ( 16 ) WLABEL  ! Object-spectrum axis label
      REAL WSCALE                ! Scale factor for object-axis units
                                 ! to Angstroms
      CHARACTER * ( 16 ) WUNITS  ! Object-spectrum axis units
      LOGICAL VALID              ! True if axis is valid

*  Local Data:
      DATA CALLAM / 5566.0, 5577.4, 5592.0 /
      DATA LINLAM / 5566.0, 5592.0 / ! OI
*     :              5879.0, 5916.0,
*     :              6284.0, 6322.0,
*     :              6352.0, 6388.0 /

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Obtain the sky spectrum.
      CALL NDF_ASSOC( 'SKY', 'READ', NDFS, STATUS )

*  Check that this is one-dimensional.
      CALL NDF_BOUND( NDFS, SDIM, LBNDS, UBNDS, NDIMS, STATUS )
      IF ( STATUS .EQ. NDF__XSDIM ) THEN
         CALL MSG_SETI( 'NDIM', NDIMS )
         CALL ERR_REP( 'SKYLINER_ERR1',
     :     'The sky spectrum has ^NDIM significant dimensions.  It '/
     :     /'should be one-dimensional.', STATUS )
         GOTO 999
      END IF

*  Check that the array does not have bad pixels.
      BAD = .FALSE.
      CALL NDF_BAD( NDFS, 'Data', .TRUE., BAD, STATUS )

*  Report the error.
      IF ( BAD ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SKYLINER_ERR5',
     :     'The object spectrum has bad pixels.  These must be '/
     :     /'interpolated across before using SKYLINER.', STATUS )
         GOTO 999
      END IF

*  There must be a wavelength calibration.  Look for an axis component.
      VALID = .FALSE.
      CALL NDF_STATE( NDFS, 'Axis', THERE, STATUS )
      IF ( THERE ) THEN

*  Check that the label is wavelength.
         SLABEL = ' '
         CALL NDF_ACGET( NDFS, 'Label', 1, SLABEL, STATUS )

*  Convert to uppercase and remove leading blanks for a comparison.
         CALL CHR_UCASE( SLABEL )
         CALL CHR_LDBLK( SLABEL )
         IF ( SLABEL( 1:10 ) .EQ. 'WAVELENGTH' ) THEN

*  Check that the units along first axis are Angstroms or nanometres.
            SUNITS = ' '
            CALL NDF_ACGET( NDFS, 'Units', 1, SUNITS, STATUS )

*  Convert to uppercase and remove leading blanks for a comparison.
            CALL CHR_UCASE( SUNITS )
            CALL CHR_LDBLK( SUNITS )

*  Look for certain accepted values.  In time some proper units system
*  should be implemented.
            IF ( SUNITS .EQ. 'A' .OR.
     :           SUNITS( 1:8 ) .EQ. 'ANGSTROM' ) THEN
               SSCALE = 0.1
               VALID = .TRUE.

            ELSE IF ( SUNITS .EQ. 'NM' .OR.
     :                SUNITS( 1:7 ) .EQ. 'NANOMET' ) THEN
               SSCALE = 1.0
               VALID = .TRUE.

            END IF
         END IF
      END IF

*  The axis is not a recognised wavelength, so make an error report.
      IF ( .NOT. VALID ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SKYLINER_ERR2',
     :     'The sky spectrum does not have a recognised '/
     :     /'wavelength axis.', STATUS )
         GOTO 999
      END IF

*  Obtain the spectrum to be sky-subtracted.
      CALL NDF_ASSOC( 'IN', 'READ', NDFI, STATUS )

*  Check that this is two-dimensional.  The application works on
*  two-dimensional spectra, but a single spectrum is processed as if
*  its second dimension was one.
*  is one.
      CALL NDF_BOUND( NDFI, NDIM, LBND, UBND, NDIMI, STATUS )
      IF ( STATUS .EQ. NDF__XSDIM ) THEN
         CALL MSG_SETI( 'NDIM', NDIMI )
         CALL ERR_REP( 'SKYLINER_ERR3',
     :     'The spectrum to be sky-subtracted has ^NDIM significant '/
     :     /'dimensions.  It should be one- or two-dimensional.',
     :     STATUS )
         GOTO 999
      END IF

*  Check that the array does not have bad pixels.
      BAD = .FALSE.
      CALL NDF_BAD( NDFI, 'Data', .TRUE., BAD, STATUS )

*  Report the error.
      IF ( BAD ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SKYLINER_ERR6',
     :     'The sky spectrum has bad pixels.  These must be '/
     :     /'interpolated across before using SKYLINER.', STATUS )
         GOTO 999
      END IF

*  There must be a wavelength calibration in the input NDF.  Look for
*  an axis component.
      VALID = .FALSE.
      CALL NDF_STATE( NDFI, 'Axis', THERE, STATUS )
      IF ( THERE ) THEN

*  Check that the label is wavelength.
         WLABEL = ' '
         CALL NDF_ACGET( NDFI, 'Label', 1, WLABEL, STATUS )

*  Convert to uppercase and remove leading blanks for a comparison.
         CALL CHR_UCASE( WLABEL )
         CALL CHR_LDBLK( WLABEL )
         IF ( WLABEL( 1:10 ) .EQ. 'WAVELENGTH' ) THEN

*  Check that the units along first axis are Angstroms or nanometres.
            WUNITS = ' '
            CALL NDF_ACGET( NDFI, 'Units', 1, WUNITS, STATUS )

*  Convert to uppercase and remove leading blanks for a comparison.
            CALL CHR_UCASE( WUNITS )
            CALL CHR_LDBLK( WUNITS )

*  Look for certain accepted values.  In time some proper units system
*  should be implemented.
            IF ( WUNITS .EQ. 'A' .OR.
     :           WUNITS( 1:8 ) .EQ. 'ANGSTROM' ) THEN
               WSCALE = 0.1
               VALID = .TRUE.

            ELSE IF ( WUNITS .EQ. 'NM' .OR.
     :                WUNITS( 1:7 ) .EQ. 'NANOMET' ) THEN
               WSCALE = 1.0
               VALID = .TRUE.

            END IF
         END IF
      END IF

*  The axis is not a recognised wavelength, so make an error report.
      IF ( .NOT. VALID ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SKYLINER_ERR4',
     :     'The imput spectrum does not have a recognised '/
     :     /'wavelength axis.', STATUS )
         GOTO 999
      END IF

*  Derive the dimensions of the input NDF.
      DIM( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      DIM( 2 ) = UBND( 2 ) - LBND( 2 ) + 1

*  Map the axis array of the sky and the input spectrum.
      CALL NDF_AMAP( NDFS, 'Centre', 1, '_REAL', 'READ', SAPNTR, SAEL,
     :               STATUS )
      CALL NDF_AMAP( NDFI, 'Centre', 1, '_REAL', 'READ', IAPNTR, IAEL,
     :               STATUS )

*  Check that the bounds match.  Report an error when this is not true.
*  Use the dimension of the object spectrum for both of these variables
*  henceforth.
      IF ( SAEL .NE. IAEL ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SKYFINDER_ERR7',
     :     'The sky spectrum and object spectra have different '/
     :     /'numbers of pixels.', STATUS )
         GOTO 999
      END IF

*  Get the bounds of the sky and object spectra.
      CALL KPG1_AXBNR( IAEL, %VAL( CNF_PVAL( IAPNTR( 1 ) ) ), IALCO,
     :                 IAUCO, STATUS )
      CALL KPG1_AXBNR( IAEL, %VAL( CNF_PVAL( SAPNTR( 1 ) ) ), SALCO,
     :                 SAUCO, STATUS )

*  Check that the bounds are the same.  Report an error if they are not.
      IF ( ABS ( IALCO - SALCO ) .GT. VAL__EPSR .OR.
     :     ABS ( IAUCO - SAUCO ) .GT. VAL__EPSR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SKYFINDER_ERR8',
     :     'The sky spectrum and object spectra have different '/
     :     /'Wavelength scales.', STATUS )
         GOTO 999
      END IF

*  Determine pixel limits of the [OI] line.  The three values are the
*  lower limit, centre and upper limit of the line.  So find the pixel
*  above the lower limit.
      CALL KPG1_AXGVR( IAEL, %VAL( CNF_PVAL( IAPNTR( 1 ) ) ),
     :                 CALLAM( 1 ), PIND( 1 ), SLAM, STATUS )

*  Now get the pixel index just below the upper value.
      CALL KPG1_AXLVR( IAEL, %VAL( CNF_PVAL( IAPNTR( 1 ) ) ),
     :                 CALLAM( 3 ), PIND( 3 ), SLAM, STATUS )

*  Circumvent the bug in the above routine.
      PIND( 3 ) = ABS( PIND( 3 ) )

*  Finally get the centre value.  This is a floating-point value.
*  Convert it to integer.
      CALL KPG1_AINDR( LBND, UBND, %VAL( CNF_PVAL( IAPNTR( 1 ) ) ), 1,
     :                 CALLAM( 2 ), RPIND, STATUS )
      PIND( 2 ) = NINT( RPIND )

*  Create the two-dimensional output spectrum.
      CALL NDF_PROP( NDFI, 'Axis,Variance,Quality,Units', 'OUT', NDFO,
     :               STATUS )

*  Map the input and output data arrays, and the sky spectrum.
      CALL NDF_MAP( NDFI, 'Data', '_REAL', 'Read', IPNTR, EL, STATUS )
      CALL NDF_MAP( NDFO, 'Data', '_REAL', 'Write', OPNTR, EL, STATUS )
      CALL NDF_MAP( NDFS, 'Data', '_REAL', 'Read', SPNTR, SAEL, STATUS )

*  Normalise the sky spectrum to each object spectrum in the
*  2-dimensional spectrum, and then sky-subtract to form the output
*  2-dimensional spectrum.
      CALL FLA_OISS( LBND( 1 ), UBND( 1 ), DIM( 2 ),
     :               %VAL( CNF_PVAL( IPNTR( 1 ) ) ), PIND,
     :               %VAL( CNF_PVAL( SPNTR( 1 ) ) ),
     :               %VAL( CNF_PVAL( OPNTR( 1 ) ) ), STATUS )

*  Find the pixel indices of the lines.
      DO I = 1, NLINES

*  Determine pixel limits of the line.  First find the pixel
*  above the lower limit.
         CALL KPG1_AXGVR( IAEL, %VAL( CNF_PVAL( IAPNTR( 1 ) ) ),
     :                    LINLAM( 1, I ), FPIND( 1, I ), SLAM, STATUS )

*  Now get the pixel index just below the upper value.
         CALL KPG1_AXLVR( IAEL, %VAL( CNF_PVAL( IAPNTR( 1 ) ) ),
     :                    LINLAM( 2, I ), FPIND( 2, I ), SLAM, STATUS )

*  Circumvent the bug in the above routine.
         FPIND( 2, I ) = ABS( FPIND( 2, I ) )

      END DO

*  Interpolate across the strongest lines.
      CALL FLA_SLINT( LBND( 1 ), UBND( 1 ), DIM( 2 ), NLINES, FPIND,
     :                %VAL( CNF_PVAL( OPNTR( 1 ) ) ), STATUS )

*  Come here if there has been an error.
  999 CONTINUE

*  Release mapped arrays and tidy NDFs.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SKYLINER_ERR',
     :     'SKYLINER: Unable to subtract a normalised sky spectrum.',
     :     STATUS )
      END IF

      END
