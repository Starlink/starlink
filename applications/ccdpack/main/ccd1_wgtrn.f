      SUBROUTINE CCD1_WGTRN( ELOC, FORMAP, INVMAP, UNIPAR, NUMUNI,
     :                       FORVAL, INVVAL, GOTINV, ADDCLS, CLASS,
     :                       COMM, STATUS )
*+
*  Name:
*     CCD1_WGTRN

*  Purpose:
*     Writes out a general transformation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_WGTRN( ELOC, FORMAP, INVMAP, UNIPAR, NUMUNI,
*                      FORVAL, INVVAL, GOTINV, ADDCLS, CLASS, COMM,
*                      STATUS )

*  Description:
*     This routine substitutes the parameter values into the forward
*     and inverse transformations given in FORMAP and INVMAP and creates
*     a TRN_TRANSFORM structure to which it writes the transformation.
*     If no inverse transformation is specified then a dummy inverse
*     using 'X=' and 'Y=' is used instead. If supplied a classification
*     structure is added.

*  Arguments:
*     ELOC = CHARACTER * ( * ) (Given)
*        HDS locator to the place at which the TRN_TRANSFORM structure
*        containing the transformation is to created.
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
*     ADDCLS = LOGICAL (Given)
*        If true then the transformation is to have a classification
*        structure added.
*     CLASS( TRN__MXCLS ) = LOGICAL (Given)
*        The classification flags. These are in the order within the
*        array:
*             1 = LINEAR
*             2 = INDEPENDENT
*             3 = DIAGONAL
*             4 = ISOTROPIC
*             5 = POSITIVE-DET
*             6 = NEGATIVE_DET
*             7 = CONSTANT_DET
*             8 = UNIT_DET
*     COMM = CHARACTER * ( * ) (Given)
*        Comment to be added to transform structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1992 (PDRAPER):
*        Original version.
*     8-OCT-1992 (PDRAPER):
*        Added possible classification section.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameterisations
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
      INCLUDE 'TRN_PAR'          ! Transform parameterisations

*  Arguments Given:
      CHARACTER * ( * ) ELOC
      INTEGER NUMUNI
      CHARACTER * ( * ) UNIPAR( NUMUNI )
      DOUBLE PRECISION FORVAL( NUMUNI )
      DOUBLE PRECISION INVVAL( NUMUNI )
      LOGICAL GOTINV
      LOGICAL ADDCLS
      LOGICAL CLASS( TRN__MXCLS )
      CHARACTER * ( * ) COMM

*  Arguments Given and Returned:
      CHARACTER * ( * ) FORMAP( 2 )
      CHARACTER * ( * ) INVMAP( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) LINE ! Line used to write output
                                      ! information
      CHARACTER * ( DAT__SZLOC ) LOCTR ! HDS locator to structure
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER NSUBS              ! Number of substituted tokens
      INTEGER SPACE              ! Space used to write out value
      INTEGER IAT                ! Position in string
      INTEGER NCHAR              ! Number of characters written into
                                 ! string
      INTEGER NPER               ! Number of values written per line
      INTEGER NLINES             ! Number of lines used to report
                                 ! parameters
      LOGICAL RCLASS( TRN__MXCLS ) ! Classification flags sorted
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

*  Report the classification (if one has been provided).
      IF ( ADDCLS ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '  Transformations are:', STATUS )
         IF ( CLASS( TRN__LIN ) ) THEN
            CALL CCD1_MSG( ' ', '    Linear', STATUS )
         END IF
         IF ( CLASS( TRN__INDEP) ) THEN
            CALL CCD1_MSG( ' ', '    Independent', STATUS )
         END IF
         IF ( CLASS( TRN__DIAG ) ) THEN
            CALL CCD1_MSG( ' ', '    Diagonal', STATUS )
         END IF
         IF ( CLASS( TRN__ISOT ) ) THEN
            CALL CCD1_MSG( ' ', '    Isotropic', STATUS )
         END IF
         IF ( CLASS( TRN__POSDT ) ) THEN
            CALL CCD1_MSG( ' ', '    Positive determinant', STATUS )
         END IF
         IF ( CLASS( TRN__NEGDT ) ) THEN
            CALL CCD1_MSG( ' ', '    Negative determinant', STATUS )
         END IF
         IF ( CLASS( TRN__CONDT ) ) THEN
            CALL CCD1_MSG( ' ', '    Constant determinant', STATUS )
         END IF
         IF ( CLASS( TRN__UNIDT ) ) THEN
            CALL CCD1_MSG( ' ', '    Unit determinant', STATUS )
         END IF
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

*  And create the fitted transformation structure, storing it
*  in the given object.
      CALL TRN_NEW( 2, 2, FORMAP, INVMAP, '_DOUBLE', COMM, ELOC,
     :              'TRANSFORM', LOCTR, STATUS )

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

*  If given add the classifications.
      IF ( ADDCLS ) THEN
        RCLASS( TRN__LIN )   = CLASS( 1 )
        RCLASS( TRN__INDEP ) = CLASS( 2 )
        RCLASS( TRN__DIAG )  = CLASS( 3 )
        RCLASS( TRN__ISOT )  = CLASS( 4 )
        RCLASS( TRN__POSDT ) = CLASS( 5 )
        RCLASS( TRN__NEGDT ) = CLASS( 6 )
        RCLASS( TRN__CONDT ) = CLASS( 7 )
        RCLASS( TRN__UNIDT ) = CLASS( 8 )
        CALL TRN_PTCL( RCLASS, LOCTR, STATUS )
      END IF

      END
* $Id$
