      SUBROUTINE IRG1_NCUT( INDF1, STR, INDF2, STATUS )
*+
*  Name:
*     IRG1_NCUT

*  Purpose:
*     Cut a section specified by a character string from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG1_NCUT( INDF1, STR, INDF2, STATUS )

*  Description:
*     The routine creates a section from an NDF, generating a new NDF
*     identifier for the new NDF.  The dimension bounds defining the
*     section are supplied as a parenthesised character string via the
*     STR argument (e.g.  '(256,256)', '(,,~3)', '(3.5:5,8:)' or
*     '(,7.0~100,,:6)', etc.  If this string is blank, then the routine
*     returns an NDF identifier for the original NDF by cloning the
*     given NDF identifier.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the input (base) NDF.
*     STR = CHARACTER * ( * ) (Given)
*        Section bounds expression.
*     INDF2 = INTEGER (Returned)
*        Identifier for the output (section) NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The syntax of the STR string will be fully validated by this
*     routine. It must contain enclosing parentheses unless it is
*     completely blank.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for INDF2. The same value will also be
*     returned if the routine should fail for any reason.


*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-JAN-1992 (DSB):
*        Original version (based on NDF_$NCUT by R.F. Warren-Smith).
*     16-MAR-1992 (PDRAPER):
*        Changed NDF internals to NDF1 calls, port to Unix.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      INTEGER INDF1
      CHARACTER STR*(*)

*  Arguments Returned:
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION VALUE1( NDF__MXDIM ) ! First bound specifier
      DOUBLE PRECISION VALUE2( NDF__MXDIM ) ! Second bound specifier
      INTEGER F                 ! First non-blank character position
      INTEGER I                 ! Loop counter for dimensions
      INTEGER L                 ! Last non-blank character position
      INTEGER LBND( DAT__MXDIM ) ! Lower dimension bounds
      INTEGER LBNDD( NDF__MXDIM ) ! Default lower bounds
      INTEGER NDIM              ! Number of section dimensions
      INTEGER NDIMD             ! Input NDF number of dimensions
      INTEGER UBND( DAT__MXDIM ) ! Upper dimension bounds
      INTEGER UBNDD( NDF__MXDIM ) ! Default upper bounds
      LOGICAL ISBND( NDF__MXDIM ) ! Are VALUEs explicit bounds?
      LOGICAL ISPIX1( NDF__MXDIM ) ! Is VALUE1 a pixel index?
      LOGICAL ISPIX2( NDF__MXDIM ) ! Is VALUE2 a pixel index?

*.

*  Initialise the returned NDF identifier.
      INDF2 = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the dimension bounds
*  string.
      CALL CHR_FANDL( STR, F, L )

*  If the string is blank, then simply clone the NDF identifier.
      IF ( F .GT. L ) THEN
         CALL NDF_CLONE( INDF1, INDF2, STATUS )

*  Otherwise, check that the string has enclosing parentheses and report
*  an error if it does not.
      ELSE IF ( ( STR( F : F ) .NE. '(' ) .OR.
     :          ( STR( L : L ) .NE. ')' ) ) THEN
         STATUS = NDF__BNDIN
         CALL MSG_SETC( 'SECTION', STR( F : L ) )
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL ERR_REP( 'IRG1_NCUT_ERR1',
     :                 'Invalid section ''^SECTION'' specified for ' //
     :                 'the file ^NDF -- enclosing parenthesis ' //
     :                 'missing.', STATUS )

*  Otherwise, obtain the NDF's bounds and number of dimensions.
      ELSE
         CALL NDF_BOUND( INDF1, NDF__MXDIM, LBNDD, UBNDD, NDIMD,
     :                   STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Remove the enclosing parentheses (supply a blank bounds expression
*  if '()' was specified) and parse the dimension bounds expression.
            IF ( STR( F : L ) .EQ. '()' ) THEN
               CALL NDF1_PSNDE( ' ', NDIMD, LBNDD, UBNDD,
     :                          VALUE1, VALUE2, NDIM,
     :                          ISPIX1, ISPIX2, ISBND, STATUS )
            ELSE
               CALL NDF1_PSNDE( STR( F + 1 : L - 1 ),
     :                          NDIMD, LBNDD, UBNDD,
     :                          VALUE1, VALUE2, NDIM,
     :                          ISPIX1, ISPIX2, ISBND, STATUS )
            END IF

*  If an error occurs, then report contextual information.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL NDF_MSG( 'NDF', INDF1 )
               CALL ERR_REP( 'IRG1_NCUT_ERR2',
     :                       'Unable to select the specified ' //
     :                       'section of the file ^NDF', STATUS )

*  Otherwise, calculate the actual lower and upper bounds of each
*  dimension in pixels.
            ELSE
               DO 1 I = 1, NDIM
                  CALL IRG1_AXLIM( I, INDF1, VALUE1( I ), VALUE2( I ),
     :                             ISPIX1( I ), ISPIX2( I ), ISBND( I ),
     :                             LBND( I ), UBND( I ), STATUS )

*  If an error occurs, then report context information and quit.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETI( 'DIM', I )
                     CALL MSG_SETC( 'SECTION', STR( F : L ) )
                     CALL ERR_REP( 'IRG1_NCUT_ERR3',
     :                             'Error in dimension ^DIM of the ' //
     :                             'section specification ' //
     :                             '''^SECTION''.',
     :                             STATUS )
                     GO TO 2
                  END IF
 1             CONTINUE
 2             CONTINUE

*  Select the section from the NDF.
               CALL NDF_SECT( INDF1, NDIM, LBND, UBND, INDF2, STATUS )

*  Report further context information if needed.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL NDF_MSG( 'NDF', INDF1 )
                  CALL ERR_REP( 'IRG1_NCUT_ERR4',
     :                          'Unable to select the specified ' //
     :                          'section of the file ^NDF', STATUS )
               END IF
            END IF
         END IF
      END IF

      END
* $Id$
