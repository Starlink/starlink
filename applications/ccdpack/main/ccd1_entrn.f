      SUBROUTINE CCD1_ENTRN( LOC, STORE, OBJECT, VALUE, TRNI, TRNC,
     :                       TRNL, STATUS )
*+
*  Name:
*     CCD1_ENTRN

*  Purpose:
*     Stores and ultimately encodes TRANSFORM information restored
*     in FITS.

*  Language:
*     Starlink Fortran-77

*  Invocation:
*     CALL CCD1_ENTRN( LOC, STORE, OBJECT, VALUE, TRNI, TRNC, TRNL, STATUS )

*  Description:
*     This routine encapsulates the methods necessary to restore a
*     TRANSFORM structure that has been stored away as a sequence of
*     strings (this is expected to happen when using foreign data types
*     and the CCDPACK extension has to be recoded using FITS headers).
*     It uses supplied workspace to gradually regather all the
*     information and then uses all this (and TRANSFORM) to generate a
*     facsimile of the original TRANSFORM structure.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Locator for the CCDPACK extension where the TRANSFORM structure
*        is to be created.
*     STORE = LOGICAL (Given)
*        Whether the given information is for storage. This should
*        always be TRUE until all the information about the TRANSFORM
*        structure has been gathered, when it should be set FALSE and
*        the TRANSFORM structure will be created (if possible).
*     OBJECT = CHARACTER * ( * ) (Given)
*        The name of the HDS object, within the TRANSFORM structure,
*        that the incoming data corresponds to. The recognised names are:
*           TRANSFORM.TRN_VERSION
*           TRANSFORM.FORWARD
*           TRANSFORM.INVERSE
*           TRANSFORM.MODULE_ARRAY(1).NVAR_IN
*           TRANSFORM.MODULE_ARRAY(1).NVAR_OUT
*           TRANSFORM.MODULE_ARRAY(1).COMMENT
*           TRANSFORM.MODULE_ARRAY(1).PRECISION
*           TRANSFORM.MODULE_ARRAY(1).FORWARD_FUNC(1)
*           TRANSFORM.MODULE_ARRAY(1).FORWARD_FUNC(2)
*           TRANSFORM.MODULE_ARRAY(1).INVERSE_FUNC(1)
*           TRANSFORM.MODULE_ARRAY(1).INVERSE_FUNC(2)
*           TRANSFORM.CLASSIFICATION.LINEAR
*           TRANSFORM.CLASSIFICATION.INDEPENDENT
*           TRANSFORM.CLASSIFICATION.DIAGONAL
*           TRANSFORM.CLASSIFICATION.ISOTROPIC
*           TRANSFORM.CLASSIFICATION.POSITIVE_DET
*           TRANSFORM.CLASSIFICATION.NEGATIVE_DET
*           TRANSFORM.CLASSIFICATION.CONSTANT_DET
*           TRANSFORM.CLASSIFICATION.UNIT_DET
*        Which is the same as a HDSTRACE of the TRANSFORM
*        structure. Note that no joined transformations can be stored.
*        Note also that a TRANSFORM.TRN_VERSION object will be ignored.
*     VALUE = CHARACTER * ( * ) (Given)
*        The value of the object to stored.
*     TRNI( 2 ) = INTEGER (Given and Returned)
*        Workspace for storing the incremental state. This should not
*        be modified between calls to this routine. The elements of this
*        array should be initialised to 0 before the first call.
*     TRNC( 6 ) = CHARACTER * ( * ) (Given and Returned)
*        Workspace for storing the incremental state. This should not
*        be modified between calls to this routine. The individual
*        strings in this case should be long enough to hold the
*        functions and comments (CCD1__SZTRN is usual). The elements of
*        this array should be set to ' ' before the first call of this
*        routine.
*     TRNL( TRN__MXCLS + 2 ) = LOGICAL (Given and Returned)
*        Workspace for storing the incremental state. This should not
*        be modified between calls to this routine. The elements
*        of this array should be set to .FALSE. before the first call
*        to this routine.
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*  Notes:
*      It is important that all character storage buffers are cleared
*      before the first call to this routine as later calls for the same
*      object will have their contents appended to the existing
*      contents. This allows multiple lines to be reconstructed, but
*      note that trailing blanks within the strings are ignored.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     5-MAY-1997 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'TRN_PAR'         ! TRANSFORM parameters
      INCLUDE 'DAT_PAR'         ! HDS constants

*  Arguments Given:
      CHARACTER * ( * ) LOC
      LOGICAL STORE
      CHARACTER * ( * ) OBJECT
      CHARACTER * ( * ) VALUE

*  Arguments Given and Returned:
      INTEGER TRNI( 2 )
      CHARACTER * ( * ) TRNC( 6 )
      LOGICAL TRNL( * )

*  Status:
      INTEGER STATUS            ! Global status

*  External references:
      INTEGER CHR_LEN           ! Used length of string
      EXTERNAL CHR_LEN

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCTR ! Locator to structure
      LOGICAL PROCED            ! Can proceed to create transform struct
      LOGICAL THERE             ! Object exists
      INTEGER NC                ! Number of characters in string
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Either store the given value or try to create the transform
*  structure.
      IF ( STORE ) THEN

*  Check against all known names, convert the value if necessary and
*  store it.
         IF ( OBJECT .EQ. 'TRANSFORM.TRN_VERSION' ) THEN

*  Do nothing.
         ELSE IF ( OBJECT .EQ. 'TRANSFORM.FORWARD' ) THEN
            IF ( VALUE .EQ. 'DEFINED' ) THEN
               TRNL( TRN__MXCLS + 1 ) = .TRUE.
            END IF
         ELSE IF ( OBJECT .EQ. 'TRANSFORM.INVERSE' ) THEN
            IF ( VALUE .EQ. 'DEFINED' ) THEN
               TRNL( TRN__MXCLS + 2 ) = .TRUE.
            END IF
         ELSE IF ( OBJECT .EQ. 'TRANSFORM.MODULE_ARRAY(1).NVAR_IN' )
     :           THEN
            CALL CHR_CTOI( VALUE, TRNI( 1 ), STATUS )
         ELSE IF ( OBJECT .EQ. 'TRANSFORM.MODULE_ARRAY(1).NVAR_OUT' )
     :           THEN
            CALL CHR_CTOI( VALUE, TRNI( 2 ), STATUS )
         ELSE IF ( OBJECT .EQ. 'TRANSFORM.MODULE_ARRAY(1).COMMENT' )
     :           THEN
            TRNC( 1 ) = VALUE
         ELSE IF ( OBJECT .EQ. 'TRANSFORM.MODULE_ARRAY(1).PRECISION' )
     :           THEN
            TRNC( 2 ) = VALUE
         ELSE IF ( OBJECT .EQ.
     :           'TRANSFORM.MODULE_ARRAY(1).FORWARD_FUNC(1)' ) THEN

*  Append information to any existing content.
            NC = CHR_LEN( TRNC( 3 ) )
            CALL CHR_APPND( VALUE, TRNC( 3 ), NC )

         ELSE IF ( OBJECT .EQ.
     :           'TRANSFORM.MODULE_ARRAY(1).FORWARD_FUNC(2)' ) THEN

*  Append information to any existing content.
            NC = CHR_LEN( TRNC( 4 ) )
            CALL CHR_APPND( VALUE, TRNC( 4 ), NC )

         ELSE IF ( OBJECT .EQ.
     :           'TRANSFORM.MODULE_ARRAY(1).INVERSE_FUNC(1)' ) THEN

*  Append information to any existing content.
            NC = CHR_LEN( TRNC( 5 ) )
            CALL CHR_APPND( VALUE, TRNC( 5 ), NC )

         ELSE IF ( OBJECT .EQ.
     :           'TRANSFORM.MODULE_ARRAY(1).INVERSE_FUNC(2)' ) THEN

*  Append information to any existing content.
            NC = CHR_LEN( TRNC( 6 ) )
            CALL CHR_APPND( VALUE, TRNC( 6 ), NC )

         ELSE IF ( OBJECT .EQ.
     :           'TRANSFORM.CLASSIFICATION.LINEAR' ) THEN
            TRNL( TRN__LIN ) = .TRUE.
         ELSE IF ( OBJECT .EQ.
     :           'TRANSFORM.CLASSIFICATION.INDEPENDENT') THEN
            TRNL( TRN__INDEP ) = .TRUE.
         ELSE IF ( OBJECT .EQ.
     :           'TRANSFORM.CLASSIFICATION.DIAGONAL' ) THEN
            TRNL( TRN__DIAG ) = .TRUE.
         ELSE IF ( OBJECT .EQ.
     :           'TRANSFORM.CLASSIFICATION.ISOTROPIC' ) THEN
            TRNL( TRN__ISOT ) = .TRUE.
         ELSE IF ( OBJECT .EQ.
     :           'TRANSFORM.CLASSIFICATION.POSITIVE_DET' ) THEN
            TRNL( TRN__POSDT ) = .TRUE.
         ELSE IF ( OBJECT .EQ.
     :           'TRANSFORM.CLASSIFICATION.NEGATIVE_DET' ) THEN
            TRNL( TRN__NEGDT ) = .TRUE.
         ELSE IF ( OBJECT .EQ.
     :           'TRANSFORM.CLASSIFICATION.CONSTANT_DET' ) THEN
            TRNL( TRN__CONDT ) = .TRUE.
         ELSE IF ( OBJECT .EQ.
     :           'TRANSFORM.CLASSIFICATION.UNIT_DET' ) THEN
            TRNL( TRN__UNIDT ) = .TRUE.
         ELSE

*  Unknown object name.
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OBJ', OBJECT )
            CALL ERR_REP( 'CCD1_ENTRN_1',
     :           '  CCD1_ENTRN: Unrecognised TRANSFORM object ^OBJ',
     :           STATUS )
         END IF
      ELSE

*  Try to create the TRANSFORM structure. First do some rudimentary
*  error checking.
         PROCED = .TRUE.
         IF ( .NOT. TRNL( TRN__MXCLS + 1 ) .AND.
     :        .NOT. TRNL( TRN__MXCLS + 2 ) ) THEN

*  No transforms have been defined.
            PROCED = .FALSE.
         END IF
         IF ( TRNL( TRN__MXCLS + 1 ) ) THEN
            IF ( TRNC( 3 ) .EQ. ' ' .OR. TRNC( 4 ) .EQ. ' ' ) THEN

*  Lack forward transforms.
               PROCED = .FALSE.
            END IF
         END IF
         IF ( TRNL( TRN__MXCLS + 2 ) ) THEN
            IF ( TRNC( 5 ) .EQ. ' ' .OR. TRNC( 6 ) .EQ. ' ' ) THEN

*  Lack inverse transforms.
               PROCED = .FALSE.
            END IF
         END IF

*  Now we proceed.
         IF ( PROCED ) THEN

*  Remove any existing TRANSFORM structure.
            CALL DAT_THERE( LOC, 'TRANSFORM', THERE, STATUS )
            IF ( THERE ) THEN
               CALL DAT_ERASE( LOC, 'TRANSFORM', STATUS )
            END IF

*  Now create the structure.
            CALL TRN_NEW( TRNI( 1 ), TRNI( 2 ), TRNC( 3 ), TRNC( 5 ),
     :           TRNC( 2 ), TRNC( 1 ), LOC, 'TRANSFORM', LOCTR,
     :           STATUS )
            CALL TRN_PTCL( TRNL, LOCTR, STATUS )
            CALL DAT_ANNUL( LOCTR, STATUS )
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_ENTRN_2',
     :'  CCD1_ENTRN: Information on TRANSFORM structure is incomplete',
     :           STATUS )
         END IF
      END IF
      END
* $Id$
