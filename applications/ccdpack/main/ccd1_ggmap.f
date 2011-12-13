      SUBROUTINE CCD1_GGMAP( FORMAP, INVMAP, UNIPAR, NUMUNI, FORVAL,
     :                       INVVAL, GOTINV, SIMPFI, SIMPIF, MAP,
     :                       STATUS )
*+
*  Name:
*     CCD1_GGMAP

*  Purpose:
*     Get an AST Mapping for a general transform.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_GGMAP( FORMAP, INVMAP, UNIPAR, NUMUNI, FORVAL, INVVAL,
*                      GOTINV, SIMPFI, SIMPIF, MAP, STATUS )

*  Description:
*     This routine substitutes the parameter values into the forward and
*     inverse transformations given in FORMAP and INVMAP and generates
*     an AST MathMap from the resulting expression.  The process relies
*     on the fact that the format for AST MathMap specifications is so
*     similar to (is a superset of) that used for TRANSFORM structures.
*     If no inverse transformation is supplied then a dummy one is
*     supplied to the mapping.

*  Arguments:
*     FORMAP( 2 ) = CHARACTER * ( * ) (Given and Returned)
*        The forward mapping transformations expressions. These strings
*        contain the tokens in UNIPAR for which the values in FORVAL
*        are to be subsituted. On exit this string contains the token
*        substituted form of the expression
*     INVMAP( 2 ) = CHARACTER * ( * ) (Given and Returned)
*        The (optional) inverse mapping transformations expressions.
*        These strings contain the tokens in UNIPAR for which the
*        values in INVVAL are to be subsituted.
*     UNIPAR( NUMUNI ) = CHARACTER * ( * ) (Given)
*        The token strings which are to be replaced with values in the
*        forward and inverse mappings.
*     NUMUNI = INTEGER (Given)
*        The number of parameter tokens given in UNIPAR.
*     FORVAL( NUMUNI ) = DOUBLE PRECISION (Given)
*        The values which are associated with the tokens in UNIPAR and
*        which will be substituted into the forward expression.
*     INVVAL( NUMUNI ) = DOUBLE PRECISION (Given)
*        The values which are associated with the tokens in UNIPAR and
*        which will be substituted in the inverse expression.
*     GOTINV = LOGICAL (Given)
*        If true then an inverse expression has been given and is to be
*        included as part of the transform.
*     SIMPFI = LOGICAL (Given)
*        The value of the resulting mapping's 'SimpFI' attribute.  This
*        decides whether the mapping followed by its inverse can be
*        legitimately simplified to the unit mapping.
*     SIMPIF = LOGICAL (Given)
*        The value of the resulting mapping's 'SimpIF' attribute.  This
*        decides whether the mapping preceded by its inverse can be
*        legitimately simplified to the unit mapping.
*     MAP = INTEGER (Returned)
*        An AST pointer to the mapping represented by the tranformations
*        given.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     12-NOV-1999 (MBT):
*        Original version (adapted from CCD1_WGTRN).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
      INCLUDE 'PRM_PAR'          ! PRIMDAT standard constants

*  Arguments Given:
      INTEGER NUMUNI
      CHARACTER * ( * ) UNIPAR( NUMUNI )
      DOUBLE PRECISION FORVAL( NUMUNI )
      DOUBLE PRECISION INVVAL( NUMUNI )
      LOGICAL GOTINV
      LOGICAL SIMPFI
      LOGICAL SIMPIF

*  Arguments Given and Returned:
      CHARACTER * ( * ) FORMAP( 2 )
      CHARACTER * ( * ) INVMAP( 2 )

*  Arguments Returned:
      INTEGER MAP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) LINE ! Line used to write output information
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER NSUBS              ! Number of substituted tokens
      INTEGER SPACE              ! Space used to write out value
      INTEGER IAT                ! Position in string
      INTEGER NCHAR              ! Number of characters written into string
      INTEGER NPER               ! Number of values written per line
      INTEGER NLINES             ! Number of lines used to report parameters

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the transformations to the log and add any classification
*  information.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '     General transformation:', STATUS )
      CALL CCD1_MSG( ' ', '     -----------------------', STATUS )
      CALL MSG_SETC( 'FOR1', FORMAP( 1 ) )
      CALL CCD1_MSG( ' ', '  ^FOR1', STATUS )
      CALL MSG_SETC( 'FOR2', FORMAP( 2 ) )
      CALL CCD1_MSG( ' ', '  ^FOR2', STATUS )
      IF ( GOTINV ) THEN
         CALL MSG_SETC( 'INV1', INVMAP( 1 ) )
         CALL CCD1_MSG( ' ', '  ^INV1', STATUS )
         CALL MSG_SETC( 'INV2', INVMAP( 2 ) )
         CALL CCD1_MSG( ' ', '  ^INV2', STATUS )
      END IF

*  Substitute the forward transformation tokens.
      DO 1 I = 1, NUMUNI
         CALL TRN_STOKD( UNIPAR( I ), FORVAL( I ), FORMAP( 1 ), NSUBS,
     :                   STATUS )
         CALL TRN_STOKD( UNIPAR( I ), FORVAL( I ), FORMAP( 2 ), NSUBS,
     :                   STATUS )
 1    CONTINUE

*  Substitute the inverse transformation tokens if they have been
*  provided. Otherwise create a dummy inverse transformation
      IF ( GOTINV ) THEN
         DO 2 I = 1, NUMUNI
            CALL TRN_STOKD( UNIPAR( I ), INVVAL( I ), INVMAP( 1 ),
     :                      NSUBS, STATUS )
            CALL TRN_STOKD( UNIPAR( I ), INVVAL( I ), INVMAP( 2 ),
     :                      NSUBS, STATUS )
 2       CONTINUE
      ELSE
         INVMAP( 1 ) = 'X'
         INVMAP( 2 ) = 'Y'
      END IF

*  Generate the AST mapping.
      MAP = AST_MATHMAP( 2, 2, 2, FORMAP, 2, INVMAP, ' ', STATUS )

*  Set options to apply to the mapping (both default to zero).
      IF ( SIMPFI ) CALL AST_SETI( MAP, 'SimpFI', 1, STATUS )
      IF ( SIMPIF ) CALL AST_SETI( MAP, 'SimpIF', 1, STATUS )

*  Inform the user about the final fits.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    General transformation coefficiencts:',
     :               STATUS )
      CALL CCD1_MSG( ' ', '    -------------------------------------',
     :               STATUS )
      CALL CCD1_MSG( ' ', '  Forward:', STATUS )

*  Work out how many parameters we can fit on a 72 character line.
*  VAL__SZD = maximum number of characters needed to represent a double
*  precision value CCD1__STRNP = size of transform parameter name + 8
*  for comfort.
      NSUBS = 0
      SPACE = VAL__SZD + CCD1__STRNP + 8
      NPER = 72 / SPACE
      NLINES = NUMUNI / NPER + 1
      DO 4 I = 1, NLINES
         LINE = ' '
         IAT = 4
         DO 5 J = 1, NPER
            NSUBS = NSUBS + 1
            IF ( NSUBS .LE. NUMUNI ) THEN
               CALL CHR_APPND( UNIPAR( NSUBS ), LINE, IAT )
               IAT = IAT + 2
               LINE( IAT: IAT) = '='
               IAT = IAT + 2
               CALL CHR_DTOC( FORVAL( NSUBS ), LINE( IAT : ), NCHAR )
               IAT = IAT + NCHAR + 1
            END IF

*  Increment position within string.
            IAT = J * SPACE + 3
 5       CONTINUE

*  Write out this line.
         CALL CCD1_MSG( ' ', LINE, STATUS )
 4    CONTINUE

*  Repeat for the inverse transformation.
      IF ( GOTINV ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '  Inverse:', STATUS )
         NSUBS = 0
         SPACE = VAL__SZD + CCD1__STRNP + 8
         NPER = 72 / SPACE
         NLINES = NUMUNI / NPER + 1
         DO 6 I = 1, NLINES
            LINE = ' '
            IAT = 4
            DO 7 J = 1, NPER
               NSUBS = NSUBS + 1
               IF ( NSUBS .LE. NUMUNI ) THEN
                  CALL CHR_APPND( UNIPAR( NSUBS ), LINE, IAT )
                  IAT = IAT + 2
                  LINE( IAT: IAT) = '='
                  IAT = IAT + 2
                  CALL CHR_DTOC( INVVAL( NSUBS ), LINE( IAT : ), NCHAR )
                  IAT = IAT + NCHAR + 1
               END IF

*  Increment position within string.
               IAT = J * SPACE + 3
 7          CONTINUE

*  Write out this line.
            CALL CCD1_MSG( ' ', LINE, STATUS )
 6       CONTINUE
      END IF

      END
* $Id$
