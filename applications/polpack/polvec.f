      SUBROUTINE POLVEC( STATUS )
*+
*  Name:
*     POLVEC

*  Purpose:
*     Calculates polarisation vectors from supplied Stokes parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLVEC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine creates four output NDFs holding percentage polarisation, 
*     polarisation angle, total intensity and polarised intensity at each
*     pixel in the input cube, which should contain Stokes parameters and 
*     will normally have been created by POLCAL. 

*  Usage:
*     polvec in p theta i ip

*  ADAM Parameters:
*     DEBIAS = _LOGICAL (Read)
*        TRUE if a correction for statistical bias is to be made to
*        percentage polarisation and polarised intensity. This correction
*        subtracts the variance of the percentage polarisation from the
*        squared percentage polarisation, and uses the square root of this
*        as the corrected percentage polarisation.  The corresponding
*        polarised intensity is then found by multiplying the corrected
*        percentage polarisation by the total intensity.  The returned 
*        variance values are unchanged. This correction only applies to
*        calculations of plane polarisation, and cannot be used if the 
*        input NDF does not contain variance values, or if you supply a 
*        FALSE value for parameter VARIANCE. If a null value (!) is
*        supplied, then the correction is applied if output variances
*        are being created, and not otherwise.           [!]
*     I = NDF (Write)
*        An output NDF holding the total intensity. A null value can be
*        supplied if this output image is not required.
*     IN = NDF (Read)
*        The 3-d NDF holding the Stokes parameters. This should have been
*        created by POLCAL.
*     IP = NDF (Write)
*        An output NDF holding the polarised intensity. A null value can be
*        supplied if this output image is not required.
*     P = NDF (Write)
*        An output NDF holding percentage polarisation. A null value can be
*        supplied if this output image is not required.
*     THETA = NDF (Write)
*        An output NDF holding the polarisation angle in degrees. In the
*        the case of circular polarisation, a value of zero is stored 
*        if the normalised Stokes parameter V is positive, and a value
*        of 90 is stored otherwise. A null value can be supplied if this 
*        output image is not required.
*     VARIANCE = _LOGICAL (Read)
*        TRUE if output variances are to be calculated.  This parameter
*        is only accessed if all input NDFs contain variances, otherwise
*        no variances are generated.  [TRUE]

*  Notes:
*     -  The output NDFs are deleted if there is an error during the
*     formation of the polarisation parameters.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL DEBIAS             ! Statistical de-biassing required?
      INTEGER EL                 ! No. of elements in mapped arrays
      INTEGER I                  ! Loop count
      INTEGER IPDIN              ! Pointers to input DATA arrays
      INTEGER IPI                ! Pointer to total intensity output
      INTEGER IPIA               ! Pointer to total int. (A) output
      INTEGER IPIAV              ! Pointer to total int. (A) variance
      INTEGER IPIB               ! Pointer to total int. (B) output
      INTEGER IPIBV              ! Pointer to total int. (B) variance
      INTEGER IPIP               ! Pointer to polarised int. output
      INTEGER IPIPV              ! Pointer to polarised int. variance
      INTEGER IPIV               ! Pointer to total intensity variance
      INTEGER IPP                ! Pointer to % polarisation output
      INTEGER IPPV               ! Pointer to % polarisation variance
      INTEGER IPQ                ! Pointer to Q output
      INTEGER IPQV               ! Pointer to Q variance
      INTEGER IPT                ! Pointer to angle output
      INTEGER IPTV               ! Pointer to angle variance
      INTEGER IPU                ! Pointer to U output
      INTEGER IPUV               ! Pointer to U variance
      INTEGER IPVIN              ! Pointers to input VARIANCE arrays
      LOGICAL MAKEI              ! Total intensity output required?
      LOGICAL MAKEIP             ! Polarised int. output required?
      LOGICAL MAKEP              ! % polarisation output required?
      LOGICAL MAKET              ! Angle output required?
      INTEGER INDF1              ! Identifier for input Stokes cube
      INTEGER INDFI              ! Identifier for total intensity output
      INTEGER INDFIP             ! Identifier for polarised int. output
      INTEGER INDFP              ! Identifier for % polarisation output
      INTEGER NDFQ               ! Identifier for Q output
      INTEGER INDFT              ! Identifier for angle output
      INTEGER NDFU               ! Identifier for U output
      CHARACTER STOKES*(NDF__MXDIM) ! Identifiers for each plane of input
      LOGICAL VAR                ! Output variances required?
      INTEGER DIM( 3 )           ! Dimensions of input Stokes cube
      INTEGER NDIM               ! No. of dimensions in input Stokes cube
      INTEGER LBND( 3 )          ! Lower bounds of input NDF
      INTEGER UBND( 3 )          ! Upper bounds of input NDF
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
*  =====================

*  Get the input NDF holding the Stokes parameters.
      CALL NDF_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Get its bounds and dimensions.
      CALL NDF_BOUND( INDF1, 3, LBND, UBND, NDIM, STATUS ) 
      DIM( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      DIM( 2 ) = UBND( 2 ) - LBND( 2 ) + 1
      DIM( 3 ) = UBND( 3 ) - LBND( 3 ) + 1

*  Get the value of the STOKES component in the POLPACK extension. 
*  This is a string in which each character identifies the corresponding
*  plane in the DATA array.
      STOKES = ' '
      CALL NDF_XGT0C( INDF1, 'POLPACK', 'STOKES', STOKES, STATUS ) 
      IF( ( NDIM .NE. 3 .OR. STOKES .EQ. ' ' ) .AND. 
     :     STATUS .EQ. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF1 )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLVEC_ERR1', '''^NDF'' does not contain '//
     :                 'Stokes parameter values.', STATUS )
         GO TO 999
      END IF

*  See if the VARIANCE component is defined.
      CALL NDF_STATE( INDF1, 'VARIANCE', VAR, STATUS )

*  See if output NDFs are to have VARIANCE components, provided the
*  input NDF has variance arrays.
      IF ( VAR ) CALL PAR_GET0L( 'VARIANCE', VAR, STATUS )

*  Map the data array of the input NDF, and also the VARIANCE component
*  if required.
      CALL NDF_MAP( INDF1, 'DATA', '_REAL', 'READ', IPDIN, EL, STATUS )
      IF ( VAR ) CALL NDF_MAP( INDF1, 'VARIANCE', '_REAL', 'READ', 
     :                         IPVIN, EL, STATUS )

*  Obtain the total-intensity NDF.
*  ===============================

*  Attempt to get an output NDF to hold total intensity.
      CALL NDF_CREAT( 'I', '_REAL', 2, LBND, UBND, INDFI, STATUS ) 

*  If successful, set a flag indicating that a total-intensity NDF is to
*  be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEI = .TRUE.

*  Propagate WCS information from the input.
         CALL POL1_PRWCS( INDF1, INDFI, STATUS )

*  Set the LABEL and TITLE in the output NDF to 'Total Intensity'.
         CALL NDF_CPUT( 'Total Intensity', INDFI, 'TITLE', STATUS )
         CALL NDF_CPUT( 'Total Intensity', INDFI, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( INDFI, 'DATA', '_REAL', 'WRITE', IPI, EL, 
     :                 STATUS )
         IF ( VAR ) CALL NDF_MAP( INDFI, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPIV, EL, STATUS )

*  If no total-intensity NDF was obtained, annul the error and set a
*  flag to indicate that no total intensity NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEI = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for percentage polarisation.
*  ===========================================

*  Attempt to get an output NDF to hold percentage polarisation.
      CALL NDF_CREAT( 'P', '_REAL', 2, LBND, UBND, INDFP, STATUS ) 

*  If successful, set a flag indicating that a percent-polarisation NDF
*  is to be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEP = .TRUE.

*  Propagate WCS information from the input.
         CALL POL1_PRWCS( INDF1, INDFP, STATUS )

*  Set the LABEL and TITLE in the output NDF to 'Percentage polarisation'.
         CALL NDF_CPUT( 'Percentage Polarisation', INDFP, 'LABEL',
     :                  STATUS )
         CALL NDF_CPUT( 'Percentage Polarisation', INDFP, 'TITLE',
     :                  STATUS )

*  Store "%" in the output NDF UNITS component.
         CALL NDF_CPUT( '%', INDFP, 'UNITS', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( INDFP, 'DATA', '_REAL', 'WRITE', IPP, EL, 
     :                 STATUS )
         IF ( VAR ) CALL NDF_MAP( INDFP, 'VARIANCE', '_REAL', 'WRITE',
     :                           IPPV, EL, STATUS )

*  If no percent-polarisation NDF was obtained, annul the error and set
*  a flag to indicate that no percent polarisation NDF need be
*  produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEP = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for polarisation angle.
*  ======================================

*  Attempt to get an output NDF to hold polarisation angle.
      CALL NDF_CREAT( 'THETA', '_REAL', 2, LBND, UBND, INDFT, STATUS ) 

*  If successful, set a flag indicating that an angle NDF is to
*  be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKET = .TRUE.

*  Propagate WCS information from the input.
         CALL POL1_PRWCS( INDF1, INDFT, STATUS )

*  Set the LABEL and TITLE in the output NDF to 'Polarisation Angle'.
         CALL NDF_CPUT( 'Polarisation Angle', INDFT, 'LABEL', STATUS )
         CALL NDF_CPUT( 'Polarisation Angle', INDFT, 'TITLE', STATUS )

*  Store "Degrees" in the output NDFs UNITS component.
         CALL NDF_CPUT( 'Degrees', INDFT, 'UNITS', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( INDFT, 'DATA', '_REAL', 'WRITE', IPT, EL, 
     :                 STATUS )
         IF ( VAR ) CALL NDF_MAP( INDFT, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPTV, EL, STATUS )

*  If no angle NDF was obtained, annul the error and set a flag to
*  indicate that no angle NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKET = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for polarised intensity.
*  =======================================

*  Attempt to get an output NDF to hold polarised intensity.
      CALL NDF_CREAT( 'IP', '_REAL', 2, LBND, UBND, INDFIP, STATUS ) 

*  If successful, set a flag indicating that a polarised-intensity NDF
*  is to be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEIP = .TRUE.

*  Propagate WCS information from the input.
         CALL POL1_PRWCS( INDF1, INDFIP, STATUS )

*  Set the LABEL and TITLE in the output NDF to 'Polarised Intensity'.
         CALL NDF_CPUT( 'Polarised Intensity', INDFIP, 'LABEL', STATUS )
         CALL NDF_CPUT( 'Polarised Intensity', INDFIP, 'TITLE', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL NDF_MAP( INDFIP, 'DATA', '_REAL', 'WRITE', IPIP, EL,
     :                 STATUS )
         IF ( VAR ) CALL NDF_MAP( INDFIP, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPIPV, EL, STATUS )

*  If no polarised-intensity NDF was obtained, annul the error and set a
*  flag to indicate that no polarised intensity NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEIP = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Check that there is an output NDF.
*  ==================================

*  Abort if no output images are required.
      IF ( .NOT. ( MAKEI .OR. MAKEIP .OR. MAKEP .OR. MAKET ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLVEC_ERR2', 'No output NDFs requested',
     :                 STATUS )
         GO TO 999
      END IF         

*  Decide whether or not a bias correction is needed and possible.
*  ===============================================================

*  See if a correction is to be made to the percentage polarisation to
*  correct for bias introduced as a result of the noise distribution not
*  being symmetric.
      CALL PAR_GET0L( 'DEBIAS', DEBIAS, STATUS )

*  If a null value is supplied, annull the error, and debias if the
*  varianes required are available.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         DEBIAS = VAR

*  Otherwise issue a warning if the user wants to debias the results and there 
*  are no variances available.
      ELSE IF ( DEBIAS .AND. ( .NOT. VAR ) .AND. 
     :          STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_OUT( 'POLVEC_ERR3', 'Vectors will not be '/
     :                 /'corrected for statistical bias because no '/
     :                 /'variance values are available.', STATUS )
         CALL ERR_FLUSH( STATUS )
         DEBIAS = .FALSE.
      END IF

*  Calculate the output arrays.
*  ============================

*  Call the routine to do the work.
      CALL POL1_PLVEC( DIM( 1 ), DIM( 2 ), DIM( 3 ), %VAL( IPDIN ), 
     :                 %VAL( IPVIN ), STOKES, DEBIAS, VAR, MAKEI, MAKEP,
     :                 MAKET, MAKEIP, %VAL( IPI ), 
     :                 %VAL( IPP ), %VAL( IPT ), %VAL( IPIP ), 
     :                 %VAL( IPIV ), %VAL( IPPV ), %VAL( IPTV ), 
     :                 %VAL( IPIPV ), STATUS )

*  Closedown.
*  ==========

*  Arrive here if an error occurs.
 999  CONTINUE

*  If an error has occurred, delete the output NDFs.
      IF ( STATUS .NE. SAI__OK ) THEN 
         IF ( MAKEI ) CALL NDF_DELET( INDFI, STATUS )
         IF ( MAKEP ) CALL NDF_DELET( INDFP, STATUS )
         IF ( MAKET ) CALL NDF_DELET( INDFT, STATUS )
         IF ( MAKEIP ) CALL NDF_DELET( INDFIP, STATUS )
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLVEC_ERR4', 'POLVEC: Error producing '//
     :                 'polarisation vectors.', STATUS )
      END IF

      END
