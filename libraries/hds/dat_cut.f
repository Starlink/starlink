      SUBROUTINE DAT_CUT( LOC1, STR, LOC2, STATUS )
*+
*  Name:
*     DAT_CUT

*  Purpose:
*     Cut a cell or slice from an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DAT_CUT( LOC1, STR, LOC2, STATUS )

*  Description:
*     The routine selects a sub-section of an HDS object, generating a
*     new locator to a cell or a slice of the object, as appropriate.
*     The dimension bounds defining the subsection are supplied as a
*     parenthesised character string via the STR argument (e.g.
*     '(256,256)', '(,,3)', '(3:5,8:)' or '(,7,,:6)', etc. The number
*     of dimensions implied by this string must match the number of HDS
*     object dimensions and the dimension bounds must lie within the
*     object's bounds. If this string is blank, then the routine
*     returns a locator to the whole object by cloning the locator
*     given.

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*        Locator to HDS object.
*     STR = CHARACTER * ( * ) (Given)
*        Dimension bounds expression.
*     LOC2 = CHARACTER * ( * ) (Returned)
*        Locator to the specified sub-section of the HDS object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The syntax of the STR string will be fully validated by this
*     routine. It must contain enclosing parentheses unless it is
*     completely blank.
*     -  If this routine is called with STATUS set, then an invalid
*     locator will be returned via the LOC2 argument. The same value
*     will also be returned if the routine should fail for any reason.

*  Algorithm:
*     -  Initialise the returned locator before checking the inherited
*     global status.
*     -  Find the first and last non-blank characters in the dimension
*     bounds string.
*     -  If the string is blank, then simply clone the object's
*     locator.
*     -  Otherwise, check that the string has enclosing parentheses and
*     report an error if it does not.
*     -  Otherwise, obtain the object's shape.
*     -  If the number of dimensions is zero, then report an error,
*     since no dimension bounds can be applied (a blank dimension bound
*     string has already been checked for above).
*     -  Remove the enclosing parentheses (supply a blank bounds
*     expression if '()' was specified) and parse the dimension bounds
*     expression.
*     -  If an error occurs, then report contextual information.
*     -  Otherwise, loop to test if the lower and upper bounds of each
*     dimension are equal. If so, then an HDS cell must be selected.
*     -  Select a cell or a slice from the object, as appropriate.
*     -  If an error occurred, then return a null locator.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David Berry (UCLan, Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-DEC-1990 (RFWS):
*        Original version.
*     11-DEC-1990 (RFWS):
*        Improved error reporting.
*     15-FEB-1998 (DSB):
*        Brought into NDG from NDF.
*     23-DEC-2005 (TIMJ):
*        Brought into HDS
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'DAT_ERR'          ! DAT_ error codes

*  Arguments Given:
      CHARACTER * ( * ) LOC1
      CHARACTER * ( * ) STR

*  Arguments Returned:
      CHARACTER * ( * ) LOC2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM( DAT__MXDIM )  ! Default upper dimension bounds
      INTEGER F                  ! First non-blank character position
      INTEGER I                  ! Loop counter for dimensions
      INTEGER L                  ! Last non-blank character position
      INTEGER LBND( DAT__MXDIM ) ! Lower dimension bounds
      INTEGER NDIM               ! Number of object dimensions
      INTEGER UBND( DAT__MXDIM ) ! Upper dimension bounds
      LOGICAL CELL               ! Is an HDS cell required?

*.

*  Initialise the returned locator.
      LOC2 = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the dimension bounds
*  string.
      CALL CHR_FANDL( STR, F, L )

*  If the string is blank, then simply clone the object's locator.
      IF ( F .GT. L ) THEN
         CALL DAT_CLONE( LOC1, LOC2, STATUS )

*  Otherwise, check that the string has enclosing parentheses and report
*  an error if it does not.
      ELSE IF ( ( STR( F : F ) .NE. '(' ) .OR.
     :          ( STR( L : L ) .NE. ')' ) ) THEN
         STATUS = DAT__SUBIN
         CALL EMS_SETC( 'SUBSET', STR( F : L ) )
         CALL DAT_MSG( 'OBJECT', LOC1 )
         CALL EMS_REP( 'DAT_CUT_BND1',
     :                 'Invalid subset ''^SUBSET'' specified for ' //
     :                 'the HDS object ^OBJECT -- enclosing ' //
     :                 'parenthesis missing.', STATUS )

*  Otherwise, obtain the object's shape.
      ELSE
         CALL DAT_SHAPE( LOC1, DAT__MXDIM, DIM, NDIM, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the number of dimensions is zero, then report an error, since no
*  dimension bounds can be applied (a blank dimension bound string has
*  already been checked for above).
            IF ( NDIM .EQ. 0 ) THEN
               STATUS = DAT__SUBIN
               CALL EMS_SETC( 'SUBSET', STR( F : L ) )
               CALL DAT_MSG( 'OBJECT', LOC1 )
               CALL EMS_REP( 'DAT_CUT_BND2',
     :                       'Invalid subset ''^SUBSET'' specified ' //
     :                       'for the HDS object ^OBJECT -- this ' //
     :                       'object is scalar.', STATUS )

*  Remove the enclosing parentheses (supply a blank bounds expression
*  if '()' was specified) and parse the dimension bounds expression.
            ELSE
               IF ( STR( F : L ) .EQ. '()' ) THEN
                  CALL DAT1_PSHDE( ' ', NDIM, DIM, LBND, UBND, STATUS )
               ELSE
                  CALL DAT1_PSHDE( STR( F + 1 : L - 1 ),
     :                             NDIM, DIM, LBND, UBND, STATUS )
               END IF

*  If an error occurs, then report contextual information.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL DAT_MSG( 'OBJECT', LOC1 )
                  CALL EMS_REP( 'DAT_CUT_BND3',
     :                          'Unable to select the specified ' //
     :                          'subset of the HDS object ^OBJECT',
     :                          STATUS )

*  Otherwise, loop to test if the lower and upper bounds of each
*  dimension are equal. If so, then an HDS cell must be selected.
               ELSE
                  CELL = .TRUE.
                  DO 1 I = 1, NDIM
                     IF ( LBND( I ) .NE. UBND( I ) ) THEN
                        CELL = .FALSE.
                        GO TO 2
                     END IF
 1                CONTINUE
 2                CONTINUE

*  Select a cell or a slice from the object, as appropriate.
                  IF ( CELL ) THEN
                     CALL DAT_CELL( LOC1, NDIM, LBND, LOC2, STATUS )
                  ELSE
                     CALL DAT_SLICE( LOC1, NDIM, LBND, UBND, LOC2,
     :                               STATUS )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then return a null locator.
      IF ( STATUS .NE. SAI__OK ) THEN
         LOC2 = DAT__NOLOC
      END IF

      END
