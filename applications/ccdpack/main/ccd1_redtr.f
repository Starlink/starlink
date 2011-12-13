      SUBROUTINE CCD1_REDTR( TRTYPE, TR, IFIT, FOR, INV, HAVCLS, CLASS,
     :                       NCLASS, LOCTR, STATUS )
*+
*  Name:
*     CCD1_REDTR

*  Purpose:
*     Reports the various parameters as used by CCDEDIT mode=transform.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_REDTR( TRTYPE, TR, IFIT, FOR, INV, HAVCLS, CLASS,
*                      NCLASS, LOCTR, STATUS )

*  Description:
*     This routine writes a report about the transformation options
*     used by CCDEDIT mode=transform.

*  Arguments:
*     TRTYPE = CHARACTER * ( * ) (Given)
*        The type of transformation information supplied by the user.
*        One of COEFF, EXPRES or STRUCT.
*     TR( 6 ) = DOUBLE PRECISION (Given)
*        If TRTYPE = 'COEFF' then these values are the six linear
*        transformation coefficients.
*     IFIT = INTEGER (Given)
*        If TRTYPE=COEFF then this specifies the REGISTER fittype that
*        the transform is classified as. Should be in the range 1 to 5.
*     FOR( 2 ) = CHARACTER * ( * ) (Given)
*        The forward transformations expression, used if
*        TRTYPE = 'EXPRES'.
*     INV( 2 ) = CHARACTER * ( * ) (Given)
*        The inverse transformation expression, used if
*        TRTYPE = 'EXPRES'.
*     HAVCLS = LOGICAL (Given)
*        Used if TRTYPE=EXPRES. This defines if a classification of
*        the transformation has been supplied.
*     CLASS( NCLASS ) = CHARACTER * ( * ) (Given)
*        Used if TRTYPE=EXPRES. This defines the classification, only
*        used if HAVCLS is true.
*     NCLASS = INTEGER (Given)
*        The number of classifications in CLASS.
*     LOCTR = CHARACTER * ( * ) (Given)
*        Locator to the transform structure used if TRTYPE = 'STRUCT'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
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
*     20-JUL-1993 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message system parameters
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      CHARACTER * ( * ) TRTYPE
      DOUBLE PRECISION TR( 6 )
      INTEGER IFIT
      CHARACTER * ( * ) FOR( 2 )
      CHARACTER * ( * ) INV( 2 )
      LOGICAL HAVCLS
      INTEGER NCLASS
      CHARACTER * ( * ) CLASS( NCLASS )
      CHARACTER * ( * ) LOCTR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) BUFFER ! Buffer to hold output
                                               ! strings
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Position in string
      INTEGER NCHAR              ! Number of characters returned

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start with a blank.
      CALL CCD1_MSG( ' ', ' ', STATUS )

      IF ( TRTYPE .EQ. 'COEFF' ) THEN

*  Transformation given as a series of linear coefficients.
         CALL CCD1_MSG( ' ',
     : '  Transformation defined by a series of linear coefficients',
     :   STATUS )

*  Construct an output string with a tabular look.
         IAT = 5
         BUFFER( IAT: ) = 'C1 = '
         IAT = 11
         CALL CHR_DTOC( TR( 1 ), BUFFER( IAT : ), NCHAR )
         IAT = IAT + VAL__SZD
         BUFFER( IAT: ) = '    C2 = '
         IAT = IAT + 12
         CALL CHR_DTOC( TR( 2 ), BUFFER( IAT : ), NCHAR )
         CALL CCD1_MSG( ' ', BUFFER, STATUS )

         IAT = 5
         BUFFER( IAT: ) = 'C3 = '
         IAT = 11
         CALL CHR_DTOC( TR( 3 ), BUFFER( IAT : ), NCHAR )
         IAT = IAT + VAL__SZD
         BUFFER( IAT: ) = '    C4 = '
         IAT = IAT + 12
         CALL CHR_DTOC( TR( 4 ), BUFFER( IAT : ), NCHAR )
         CALL CCD1_MSG( ' ', BUFFER, STATUS )

         IAT = 5
         BUFFER( IAT: ) = 'C5 = '
         IAT = 11
         CALL CHR_DTOC( TR( 5 ), BUFFER( IAT : ), NCHAR )
         IAT = IAT + VAL__SZD
         BUFFER( IAT: ) = '    C6 = '
         IAT = IAT + 12
         CALL CHR_DTOC( TR( 6 ), BUFFER( IAT : ), NCHAR )
         CALL CCD1_MSG( ' ', BUFFER, STATUS )

*  Comment on transformation classification.
         IF ( IFIT .EQ. 1 ) THEN
            CALL CCD1_MSG( ' ',
     :'  Classified as a shift of origin only', STATUS )
         ELSE IF ( IFIT .EQ. 2 ) THEN
            CALL CCD1_MSG( ' ',
     :'  Classified as a shift of origin and rotation', STATUS )
         ELSE IF ( IFIT .EQ. 3 ) THEN
            CALL CCD1_MSG( ' ',
     :'  Classified as a shift of origin and single magnification',
     :      STATUS )
         ELSE IF ( IFIT .EQ. 4 ) THEN
            CALL CCD1_MSG( ' ',
     :'  Classified as a shift of origin, rotation and magnification'//
     :' (solid body)', STATUS )
         ELSE IF ( IFIT .EQ. 5 ) THEN
            CALL CCD1_MSG( ' ',
     :'  Classified as an unconstrained full 6 parameter fit', STATUS )
         END IF

*  Transformation given as a expression.
      ELSE IF ( TRTYPE .EQ. 'EXPRES' ) THEN

*  Inform user of the mappings they have selected.
         CALL CCD1_MSG( ' ',
     : '  Transformation defined by the expressions:', STATUS )
         CALL MSG_SETC( 'FOR1', FOR( 1 ) )
         CALL CCD1_MSG( ' ', '  ^FOR1', STATUS )
         CALL MSG_SETC( 'FOR2', FOR( 2 ) )
         CALL CCD1_MSG( ' ', '  ^FOR2', STATUS )
         CALL MSG_SETC( 'INV1', INV( 1 ) )
         CALL CCD1_MSG( ' ', '  ^INV1', STATUS )
         CALL MSG_SETC( 'INV2', INV( 2 ) )
         CALL CCD1_MSG( ' ', '  ^INV2', STATUS )

*  Inform user of classification.
         IF ( HAVCLS ) THEN
            CALL CCD1_MSG( ' ', ' ', STATUS )
            CALL CCD1_MSG( ' ', '    Transformation classified as:',
     :                     STATUS )
            DO 9 I = 1, NCLASS
               CALL MSG_SETC( 'CLASS', CLASS( I ) )
               CALL CCD1_MSG( ' ', '  ^CLASS', STATUS )
 9          CONTINUE
         END IF
      ELSE

*  Transformation given as a TRN_TRANSFORM structure.
         CALL CCD1_MSG( ' ',
     : '  Transformation to be copied from:', STATUS )

*  Get the name of the object which has the TRN_TRANSFORM structure.
         CALL DAT_MSG( 'OBJ', LOCTR )
         CALL CCD1_MSG( ' ', '    ^OBJ', STATUS )
      END IF
      END
* $Id$
