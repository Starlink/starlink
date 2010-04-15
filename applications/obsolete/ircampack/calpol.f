      SUBROUTINE CALPOL( STATUS )
*+
*  Name:
*     CALPOL

*  Purpose:
*     Calculate polarisation parameters

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CALPOL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine calculates various parameters describing the
*     polarisation described by four intensity arrays analysed at 0, 45
*     90, and 135 degrees to a reference direction. Variance values are
*     stored in the output NDFs if all the input NDFs have variances and
*     if the user gives a true value for parameter VAR.
*
*     By default, three output NDFs are created holding percentage
*     polarisation, polarisation angle and total intensity. However,
*     NDFs holding other quantities can also be produced by over-riding
*     the default null values associated with the corresponding
*     parameters. The creation of any output NDF can be supressed by
*     supplying a null value for the corresponding parameter.

*     There is an option to correct the calculated values of percentage
*     polarisation and polarised intensity to take account of the
*     statistical bias introduced by the asymetric distribution of
*     percentage polarisation (see parameter DEBIAS). This correction
*     subtracts the variance of the percentage polarisation from the
*     squared percentage polarisation, and uses the square root of this
*     as the corrected percentage polarisation. The corresponding
*     polarised intensity is then found by multiply the corrected
*     percentage polarisation by the total intensity. Returned variance
*     values take no account of this correction.

*  Usage:
*     CALPOL IN1 IN2 IN3 IN4 P T I

*  ADAM Parameters:
*     DEBIAS = LOGICAL (Read)
*        True if a correction for statistical bias is to be made to
*        percentage polarisation and polarised intensity. This
*        correction cannot be used if any of the input NDFs do not
*        contain variance values, or if the user supplies a false value
*        for parameter VAR. [NO]
*     I = NDF (Write)
*        An output NDF holding the total intensity derived from all four
*        input NDFs.
*     I1 = NDF (Read)
*        An NDF holding the measured intensity analysed at an angle of 0
*        degrees to the reference direction.
*     I2 = NDF (Read)
*        An NDF holding the measured intensity analysed at an angle of
*        45 degrees to the reference direction.
*     I3 = NDF (Read)
*        An NDF holding the measured intensity analysed at an angle of
*        90 degrees to the reference direction.
*     I4 = NDF (Read)
*        An NDF holding the measured intensity analysed at an angle of
*        135 degrees to the reference direction.
*     IA = NDF (Write)
*        An output NDF holding the total intensity derived from input
*        NDFs I1 and I3. [!]
*     IB = NDF (Write)
*        An output NDF holding the total intensity derived from input
*        NDFs I2 and I4. [!]
*     IP = NDF (Write)
*        An output NDF holding the polarised intensity. [!]
*     P = NDF (Write)
*        An output NDF holding percentage polarisation.
*     Q = NDF (Write)
*        An output NDF holding the normalised Stokes parameter, Q. [!]
*     U = NDF (Write)
*        An output NDF holding the normalised Stokes parameter, U. [!]
*     THETA = NDF (Write)
*        An output NDF holding the polarisation angle in degrees.
*     VARIANCE = LOGICAL (Read)
*        True if output variances are to be calculated. This parameter
*        is only accessed if all input NDFs contain variances, otherwise
*        no variances are generated.  [YES]

*  Examples:
*     CALPOL M51_0 M51_45 M51_90 M51_135 M51_P M51_T M51_I IP=M51_IP
*        This example produces NDFs holding percentage polarisation,
*        polarisation angle, total intensity and polarised intensity,
*        based on the four NDFs M51_0, M51_45, M51_90 and M51_135.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-SEP-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        EL,                ! No. of elements in mapped arrays
     :        I,                 ! Loop count
     :        IPDIN( 4 ),        ! Pointers to input DATA arrays
     :        IPI,               ! Pointer to total intensity output
     :        IPIA,              ! Pointer to tot. int. (A) output
     :        IPIAV,             ! Pointer to tot. int. (A) variance
     :        IPIB,              ! Pointer to tot. int. (B) output
     :        IPIBV,              ! Pointer to tot. int. (B) variance
     :        IPIP,              ! Pointer to polarised int. output
     :        IPIPV,             ! Pointer to polarised int. variance
     :        IPIV,              ! Pointer to total intensity variance
     :        IPP,               ! Pointer to % polarisation output
     :        IPPV               ! Pointer to % polarisation variance

      INTEGER
     :        IPQ,               ! Pointer to Q output
     :        IPQV,              ! Pointer to Q variance
     :        IPT,               ! Pointer to angle output
     :        IPTV,              ! Pointer to angle variance
     :        IPU,               ! Pointer to U output
     :        IPUV,              ! Pointer to U variance
     :        IPVIN( 4 ),        ! Pointers to input VARIANCE arrays
     :        NDFI,              ! Identifier for total intensity output
     :        NDFIA,             ! Identifier for tot. int. (A) output
     :        NDFIB,             ! Identifier for tot. int. (B) output
     :        NDFIN( 4 ),        ! Input NDF identifiers
     :        NDFIP,             ! Identifier for polarised int. output
     :        NDFP               ! Identifier for % polarisation output

      INTEGER
     :        NDFQ,              ! Identifier for Q output
     :        NDFT,              ! Identifier for angle output
     :        NDFU               ! Identifier for U output

      LOGICAL
     :        DEBIAS,            ! Statistical de-biassing required?
     :        MAKEI,             ! Total intensity output required?
     :        MAKEIA,            ! Tot. int. (A) output required?
     :        MAKEIB,            ! Tot. int. (B) output required?
     :        MAKEIP,            ! Polarised int. output required?
     :        MAKEP,             ! % polarisation output required?
     :        MAKEQ,             ! Q output required?
     :        MAKET,             ! Angle output required?
     :        MAKEU,             ! U output required?
     :        VAR                ! Output variances required?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Defer error reports.
      CALL ERR_MARK

*  Start an NDF context.
      CALL NDF_BEGIN

*  Attempt to get the first input NDF. This conatins the intensity
*  which passes through the polariser when oriented at 0 degrees to the
*  reference direction.
      CALL NDF_ASSOC( 'IN1', 'READ', NDFIN( 1 ), STATUS )

*  See if the VARIANCE component is defined.
      CALL NDF_STATE( NDFIN( 1 ), 'VARIANCE', VAR, STATUS )

*  Attempt to get the second input NDF. This conatins the intensity
*  which passes through the polariser when oriented at 45 degrees to the
*  reference direction.
      CALL NDF_ASSOC( 'IN2', 'READ', NDFIN( 2 ), STATUS )

*  If necessary, see if the VARIANCE component is defined.
      IF( VAR ) CALL NDF_STATE( NDFIN( 2 ), 'VARIANCE', VAR, STATUS )

*  Attempt to get the third input NDF. This conatins the intensity
*  which passes through the polariser when oriented at 90 degrees to the
*  reference direction.
      CALL NDF_ASSOC( 'IN3', 'READ', NDFIN( 3 ), STATUS )

*  If necessary, see if the VARIANCE component is defined.
      IF( VAR ) CALL NDF_STATE( NDFIN( 3 ), 'VARIANCE', VAR, STATUS )

*  Attempt to get the fourth input NDF. This conatins the intensity
*  which passes through the polariser when oriented at 135 degrees to
*  the reference direction.
      CALL NDF_ASSOC( 'IN4', 'READ', NDFIN( 4 ), STATUS )

*  If necessary, see if the VARIANCE component is defined.
      IF( VAR ) CALL NDF_STATE( NDFIN( 4 ), 'VARIANCE', VAR, STATUS )

*  See if output NDFs are to have VARIANCE components.
      IF( VAR ) CALL PAR_GET0L( 'VARIANCE', VAR, STATUS )

*  Replace the identifiers to the supplied NDFs with identifiers for
*  sections of the supplied NDFs which have equal bounds.
      CALL NDF_MBNDN( 'TRIM', 4, NDFIN, STATUS )

*  Map the data arrays of each NDF section, and if all NDFs have
*  VARIANCE components, also map the VARIANCE components.
      DO I = 1, 4
         CALL NDF_MAP( NDFIN( I ), 'DATA', '_REAL', 'READ', IPDIN( I ),
     :                 EL, STATUS )
         IF( VAR ) CALL NDF_MAP( NDFIN( I ), 'VARIANCE', '_REAL',
     :                           'READ', IPVIN( I ), EL, STATUS )
      END DO

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an output NDF to hold total intensity.
      CALL NDF_PROP( NDFIN( 1 ), 'AXIS,QUALITY,UNITS', 'I', NDFI,
     :               STATUS )

*  If succesful, set a flag indicating that a total intensity NDF is to
*  be produced.
      IF( STATUS .EQ. SAI__OK ) THEN
         MAKEI = .TRUE.

*  Set the LABEL in the output NDF to 'Total Intensity'.
         CALL NDF_CPUT( 'Total Intensity', NDFI, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( NDFI, 'DATA', '_REAL', 'WRITE', IPI, EL, STATUS )
         IF( VAR ) CALL NDF_MAP( NDFI, 'VARIANCE', '_REAL', 'WRITE',
     :                           IPIV, EL, STATUS )

*  If no total intensity NDF was obtained, annul the error and set a
*  flag to indicate that no total intensity NDF need be produced.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEI = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an output NDF to hold the normalised Q Stokes
*  parameter.
      CALL NDF_PROP( NDFIN( 1 ), 'AXIS,QUALITY', 'Q', NDFQ, STATUS )

*  If succesful, set a flag indicating that a normalised Q Stokes
*  parameter NDF is to be produced.
      IF( STATUS .EQ. SAI__OK ) THEN
         MAKEQ = .TRUE.

*  Set the LABEL in the output NDF to 'Q'.
         CALL NDF_CPUT( 'Q', NDFQ, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( NDFQ, 'DATA', '_REAL', 'WRITE', IPQ, EL, STATUS )
         IF( VAR ) CALL NDF_MAP( NDFQ, 'VARIANCE', '_REAL', 'WRITE',
     :                           IPQV, EL, STATUS )

*  If no Q NDF was obtained, annul the error and set a flag to
*  indicate that no Q NDF need be produced.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEQ = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an output NDF to hold the normalised U Stokes
*  parameter.
      CALL NDF_PROP( NDFIN( 1 ), 'AXIS,QUALITY', 'U', NDFU, STATUS )

*  If succesful, set a flag indicating that a U NDF is to be produced.
      IF( STATUS .EQ. SAI__OK ) THEN
         MAKEU = .TRUE.

*  Set the LABEL in the output NDF to 'U'.
         CALL NDF_CPUT( 'U', NDFU, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( NDFU, 'DATA', '_REAL', 'WRITE', IPU, EL, STATUS )
         IF( VAR ) CALL NDF_MAP( NDFU, 'VARIANCE', '_REAL', 'WRITE',
     :                           IPUV, EL, STATUS )

*  If no U NDF was obtained, annul the error and set a flag to indicate
*  that no U NDF need be produced.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEU = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an output NDF to hold percentage polarisation.
      CALL NDF_PROP( NDFIN( 1 ), 'AXIS,QUALITY', 'P', NDFP, STATUS )

*  If succesful, set a flag indicating that a percent polarisation NDF
*  is to be produced.
      IF( STATUS .EQ. SAI__OK ) THEN
         MAKEP = .TRUE.

*  Set the LABEL in the output NDF to 'Percentage polarisation'.
         CALL NDF_CPUT( 'Percentage Polarisation', NDFP, 'LABEL',
     :                  STATUS )

*  Store "%" in the output NDF UNITS component.
         CALL NDF_CPUT( '%', NDFP, 'UNITS', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( NDFP, 'DATA', '_REAL', 'WRITE', IPP, EL, STATUS )
         IF( VAR ) CALL NDF_MAP( NDFP, 'VARIANCE', '_REAL', 'WRITE',
     :                           IPPV, EL, STATUS )

*  If no percent polarisation NDF was obtained, annul the error and set
*  a flag to indicate that no percent polarisation NDF need be
*  produced.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEP = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an output NDF to hold polarisation angle.
      CALL NDF_PROP( NDFIN( 1 ), 'AXIS,QUALITY', 'THETA', NDFT, STATUS )

*  If succesful, set a flag indicating that an angle NDF is to
*  be produced.
      IF( STATUS .EQ. SAI__OK ) THEN
         MAKET = .TRUE.

*  Set the LABEL in the output NDF to 'Polarisation Angle'.
         CALL NDF_CPUT( 'Polarisation Angle', NDFT, 'LABEL', STATUS )

*  Store "Degrees" in the output NDFs UNITS component.
         CALL NDF_CPUT( 'Degrees', NDFT, 'UNITS', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( NDFT, 'DATA', '_REAL', 'WRITE', IPT, EL, STATUS )
         IF( VAR ) CALL NDF_MAP( NDFT, 'VARIANCE', '_REAL', 'WRITE',
     :                           IPTV, EL, STATUS )

*  If no angle NDF was obtained, annul the error and set a flag to
*  indicate that no angle NDF need be produced.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKET = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an output NDF to hold polarised intensity.
      CALL NDF_PROP( NDFIN( 1 ), 'AXIS,QUALITY,UNITS', 'IP', NDFIP,
     :               STATUS )

*  If succesful, set a flag indicating that a polarised intensity NDF
*  is to be produced.
      IF( STATUS .EQ. SAI__OK ) THEN
         MAKEIP = .TRUE.

*  Set the LABEL in the output NDF to 'Polarised Intensity'.
         CALL NDF_CPUT( 'Polarised Intensity', NDFIP, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( NDFIP, 'DATA', '_REAL', 'WRITE', IPIP, EL,
     :                 STATUS )
         IF( VAR ) CALL NDF_MAP( NDFIP, 'VARIANCE', '_REAL', 'WRITE',
     :                           IPIPV, EL, STATUS )

*  If no polarised intensity NDF was obtained, annul the error and set a
*  flag to indicate that no polarised intensity NDF need be produced.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEIP = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an output NDF to hold total intensity estimate based
*  on the first and third input NDFs.
      CALL NDF_PROP( NDFIN( 1 ), 'AXIS,QUALITY,UNITS', 'IA', NDFIA,
     :               STATUS )

*  If succesful, set a flag indicating that this total intensity NDF is
*  to be produced.
      IF( STATUS .EQ. SAI__OK ) THEN
         MAKEIA = .TRUE.

*  Set the LABEL in the output NDF to 'Total Intensity'.
         CALL NDF_CPUT( 'Total Intensity', NDFIA, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( NDFIA, 'DATA', '_REAL', 'WRITE', IPIA, EL,
     :                 STATUS )
         IF( VAR ) CALL NDF_MAP( NDFIA, 'VARIANCE', '_REAL', 'WRITE',
     :                           IPIAV, EL, STATUS )

*  If no total intensity NDF was obtained, annul the error and set a
*  flag to indicate that this total intensity NDF need be produced.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEIA = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to get an output NDF to hold total intensity estimate based
*  on the second and fourth input NDFs.
      CALL NDF_PROP( NDFIN( 1 ), 'AXIS,QUALITY,UNITS', 'IB', NDFIB,
     :               STATUS )

*  If succesful, set a flag indicating that this total intensity NDF is
*  to be produced.
      IF( STATUS .EQ. SAI__OK ) THEN
         MAKEIB = .TRUE.

*  Set the LABEL in the output NDF to 'Total Intensity'.
         CALL NDF_CPUT( 'Total Intensity', NDFIB, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( NDFIB, 'DATA', '_REAL', 'WRITE', IPIB, EL,
     :                 STATUS )
         IF( VAR ) CALL NDF_MAP( NDFIB, 'VARIANCE', '_REAL', 'WRITE',
     :                           IPIBV, EL, STATUS )

*  If no total intensity NDF was obtained, annul the error and set a
*  flag to indicate that this total intensity NDF need be produced.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEIB = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Abort if no output images are required.
      IF( .NOT. ( MAKEI .OR. MAKEIA .OR. MAKEIB .OR. MAKEIP .OR. MAKEP
     :   .OR. MAKEQ .OR. MAKET .OR. MAKEU ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CALPOL_ERR1', 'No output NDFs requested',
     :                 STATUS )
         GO TO 999
      END IF

*  See if a correction is to be made to the percentage polarisation to
*  correct for bias introduced as a result of the noise distribution not
*  being symetric.
      CALL PAR_GET0L( 'DEBIAS', DEBIAS, STATUS )

*  Issue a warning if the user wants to debias the results and there are
*  no variances available.
      IF( DEBIAS .AND. (.NOT. VAR) ) THEN
         CALL MSG_OUT( 'CALPOL', 'WARNING: Results cannot be '//
     :                 'corrected for statistical bias because '//
     :                 'no variance values are available.', STATUS )
         DEBIAS = .FALSE.
      END IF

*  Call the routine to do the work.
      CALL POLCLC( EL, %VAL( IPDIN( 1 ) ), %VAL( IPDIN( 2 ) ),
     :             %VAL( IPDIN( 3 ) ), %VAL( IPDIN( 4 ) ),
     :             %VAL( IPVIN( 1 ) ), %VAL( IPVIN( 2 ) ),
     :             %VAL( IPVIN( 3 ) ), %VAL( IPVIN( 4 ) ), DEBIAS, VAR,
     :             MAKEI, MAKEQ, MAKEU, MAKEP, MAKET, MAKEIP, MAKEIA,
     :             MAKEIB, %VAL( IPI ), %VAL( IPQ ), %VAL( IPU ),
     :             %VAL( IPP ),%VAL( IPT ),%VAL( IPIP ),%VAL( IPIA ),
     :             %VAL( IPIB ), %VAL( IPIV ), %VAL( IPQV ),
     :             %VAL( IPUV ), %VAL( IPPV ), %VAL( IPTV ),
     :             %VAL( IPIPV ), %VAL( IPIAV ), %VAL( IPIBV ), STATUS )

*  Arrive here if an error occurs.
 999  CONTINUE

*  If an error has occurred, delete the output NDFs.
      IF( STATUS .NE. SAI__OK ) THEN
         IF( MAKEI ) CALL NDF_DELET( NDFI, STATUS )
         IF( MAKEQ ) CALL NDF_DELET( NDFQ, STATUS )
         IF( MAKEU ) CALL NDF_DELET( NDFU, STATUS )
         IF( MAKEP ) CALL NDF_DELET( NDFP, STATUS )
         IF( MAKET ) CALL NDF_DELET( NDFT, STATUS )
         IF( MAKEIP ) CALL NDF_DELET( NDFIP, STATUS )
         IF( MAKEIA ) CALL NDF_DELET( NDFIA, STATUS )
         IF( MAKEIB ) CALL NDF_DELET( NDFIB, STATUS )
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CALPOL_ERR2',
     :   'CALPOL: Error producing maps of polarisation parameters.',
     :   STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
