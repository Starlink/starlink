      SUBROUTINE NDF1_NCUT( IACB1, STR, IACB2, STATUS )
*+
*  Name:
*     NDF1_NCUT

*  Purpose:
*     Cut a section specified by a character string from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_NCUT( IACB1, STR, IACB2, STATUS )

*  Description:
*     The routine creates a section from an NDF, generating a new ACB
*     entry describing the new NDF.  The dimension bounds defining the
*     section are supplied as a parenthesised character string via the
*     STR argument (e.g.  '(256,256)', '(,,~3)', '(3.5:5,8:)' or
*     '(,7.0~100,,:6)', etc.  If this string is blank, then the routine
*     returns an ACB entry describing the original NDF by cloning the
*     ACB entry given.

*  Arguments:
*     IACB1 = INTEGER (Given)
*        Index to the input ACB entry.
*     STR = CHARACTER * ( * ) (Given)
*        Section bounds expression.
*     IACB2 = INTEGER (Returned)
*        Index to the output ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The syntax of the STR string will be fully validated by this
*     routine. It must contain enclosing parentheses unless it is
*     completely blank.
*     -  If this routine is called with STATUS set, then a value of
*     zero will be returned for IACB2. The same value will also be
*     returned if the routine should fail for any reason.

*  Algorithm:
*     -  Initialise the returned ACB index before checking the
*     inherited global status.
*     -  Find the first and last non-blank characters in the dimension
*     bounds string.
*     -  If the string is blank, then simply clone the ACB entry.
*     -  Otherwise, check that the string has enclosing parentheses and
*     report an error if it does not.
*     -  Otherwise, obtain the NDF's bounds and number of dimensions.
*     -  Remove the enclosing parentheses (supply a blank bounds
*     expression if '()' was specified) and parse the dimension bounds
*     expression.
*     -  If an error occurs, then report contextual information.
*     -  Otherwise, calculate the actual lower and upper bounds of each
*     dimension in pixels.
*     -  If an error occurs, then report context information and quit.
*     -  Select the section from the NDF.
*     -  Report further context information if needed.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1991 (RFWS):
*        Original version.
*     12-MAR-1991 (RFWS):
*        Installed call to NDF1_AXLIM to enable axis coordinate values
*        to appear in section specifications.
*     13-MAR-1991 (RFWS):
*        Added contextual error reports.
*     22-OCT-1991 (RFWS):
*        Removed unused variable.
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

*  Arguments Given:
      INTEGER IACB1
      CHARACTER * ( * ) STR

*  Arguments Returned:
      INTEGER IACB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION VALUE1( NDF__MXDIM ) ! First bound specifier
      DOUBLE PRECISION VALUE2( NDF__MXDIM ) ! Second bound specifier
      INTEGER F                  ! First non-blank character position
      INTEGER I                  ! Loop counter for dimensions
      INTEGER L                  ! Last non-blank character position
      INTEGER LBND( DAT__MXDIM ) ! Lower dimension bounds
      INTEGER LBNDD( NDF__MXDIM ) ! Default lower bounds
      INTEGER NDIM               ! Number of section dimensions
      INTEGER NDIMD              ! Input NDF number of dimensions
      INTEGER UBND( DAT__MXDIM ) ! Upper dimension bounds
      INTEGER UBNDD( NDF__MXDIM ) ! Default upper bounds
      LOGICAL ISBND( NDF__MXDIM ) ! Are VALUEs explicit bounds?
      LOGICAL ISPIX1( NDF__MXDIM ) ! Is VALUE1 a pixel index?
      LOGICAL ISPIX2( NDF__MXDIM ) ! Is VALUE2 a pixel index?

*.

*  Initialise the returned ACB index.
      IACB2 = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the dimension bounds
*  string.
      CALL CHR_FANDL( STR, F, L )

*  If the string is blank, then simply clone the ACB entry.
      IF ( F .GT. L ) THEN
         CALL NDF1_CLN( IACB1, IACB2, STATUS )

*  Otherwise, check that the string has enclosing parentheses and report
*  an error if it does not.
      ELSE IF ( ( STR( F : F ) .NE. '(' ) .OR.
     :          ( STR( L : L ) .NE. ')' ) ) THEN
         STATUS = NDF__BNDIN
         CALL MSG_SETC( 'SECTION', STR( F : L ) )
         CALL NDF1_AMSG( 'NDF', IACB1 )
         CALL ERR_REP( 'NDF1_NCUT_BND1',
     :                 'Invalid section ''^SECTION'' specified for ' //
     :                 'the NDF ^NDF -- enclosing parenthesis ' //
     :                 'missing.', STATUS )

*  Otherwise, obtain the NDF's bounds and number of dimensions.
      ELSE
         CALL ARY_BOUND( ACB_DID( IACB1 ), NDF__MXDIM, LBNDD, UBNDD,
     :                   NDIMD, STATUS )
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
               CALL NDF1_AMSG( 'NDF', IACB1 )
               CALL ERR_REP( 'NDF1_NCUT_BND2',
     :                       'Unable to select the specified ' //
     :                       'section of the NDF ^NDF', STATUS )

*  Otherwise, calculate the actual lower and upper bounds of each
*  dimension in pixels.
            ELSE
               DO 1 I = 1, NDIM
                  CALL NDF1_AXLIM( I, IACB1, VALUE1( I ), VALUE2( I ),
     :                             ISPIX1( I ), ISPIX2( I ), ISBND( I ),
     :                             LBND( I ), UBND( I ), STATUS )

*  If an error occurs, then report context information and quit.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETI( 'DIM', I )
                     CALL MSG_SETC( 'SECTION', STR( F : L ) )
                     CALL ERR_REP( 'NDF1_NCUT_DIM',
     :                             'Error in dimension ^DIM of the ' //
     :                             'NDF section specification ' //
     :                             '''^SECTION''.',
     :                             STATUS )
                     GO TO 2
                  END IF
 1             CONTINUE              
 2             CONTINUE              

*  Select the section from the NDF.
               CALL NDF1_CUT( IACB1, NDIM, LBND, UBND, IACB2, STATUS )

*  Report further context information if needed.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL NDF1_AMSG( 'NDF', IACB1 )
                  CALL ERR_REP( 'NDF1_NCUT_FAIL',
     :                          'Unable to select the specified ' //
     :                          'section of the NDF ^NDF', STATUS )
               END IF
            END IF
         END IF
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_NCUT', STATUS )

      END
