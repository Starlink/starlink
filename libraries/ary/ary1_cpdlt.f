      SUBROUTINE ARY1_CPDLT( IDCB1, IDCB2, STATUS )
*+
*  Name:
*     ARY1_CPDLT

*  Purpose:
*     Copy delta compression information from one DCB entry to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_CPDLT( IDCB1, IDCB2, STATUS )

*  Description:
*     The routine copies the supplemental information describing the
*     compression of a supplied delta array (identified by its DCB entry)
*     to another existing DCB entry. Note, it only copies components of
*     the DELTA array structure that are not also conmponents of a SIMPLE
*     array (e.g. DATA, ORIGIN, etc are not copied).

*  Arguments:
*     IDCB1 = INTEGER (Given)
*        Index to the DCB entry of the array to be copied.
*     IDCB2 = INTEGER (Given)
*        Index to the DCB entry to recieve the copy.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-NOV-2010 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNV_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_CPX( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the data object is complex.
*        DCB_FRM( ARY__MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Data object storage form.

*  Arguments Given:
      INTEGER IDCB1
      INTEGER IDCB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER LOC2*(DAT__SZLOC)! Component locator
      LOGICAL THERE              ! Does the component exist?
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Ensure that form information is available for the input DCB entry.
      CALL ARY1_DFRM( IDCB1, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  If the first DCB entry is a delta array then we copy the compression
*  information.
         IF( DCB_FRM( IDCB1 ) .EQ. 'DELTA' ) THEN

*  First do mandatory components. Report an error if any of these does
*  not exist in the input. Otherwise, copy them to the output.

*  ZAXIS
*  -----
            CALL DAT_THERE( DCB_LOC( IDCB1 ), 'ZAXIS', THERE,
     :                      STATUS )
            IF( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB1 ), 'ZAXIS', LOC2,
     :                        STATUS )
               CALL DAT_COPY( LOC2,  DCB_LOC( IDCB2 ), 'ZAXIS',
     :                        STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )

            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__DLTIN
               CALL DAT_MSG( 'A', DCB_LOC( IDCB1 )  )
               CALL ERR_REP( ' ', 'The DELTA compressed array '//
     :                       '''^A'' is invalid - the ZAXIS '//
     :                       'component is missing.', STATUS )
            END IF

*  ZDIM
*  ----
            CALL DAT_THERE( DCB_LOC( IDCB1 ), 'ZDIM', THERE,
     :                      STATUS )
            IF( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB1 ), 'ZDIM', LOC2,
     :                        STATUS )
               CALL DAT_COPY( LOC2,  DCB_LOC( IDCB2 ), 'ZDIM',
     :                        STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )

            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__DLTIN
               CALL DAT_MSG( 'A', DCB_LOC( IDCB1 ) )
               CALL ERR_REP( ' ', 'The DELTA compressed array '//
     :                       '''^A'' is invalid - the ZDIM '//
     :                       'component is missing.', STATUS )
            END IF

*  VALUE
*  -----
            CALL DAT_THERE( DCB_LOC( IDCB1 ), 'VALUE', THERE,
     :                      STATUS )
            IF( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB1 ), 'VALUE', LOC2,
     :                        STATUS )
               CALL DAT_COPY( LOC2,  DCB_LOC( IDCB2 ), 'VALUE',
     :                        STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )

            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__DLTIN
               CALL DAT_MSG( 'A', DCB_LOC( IDCB1 ) )
               CALL ERR_REP( ' ', 'The DELTA compressed array '//
     :                       '''^A'' is invalid - the VALUE '//
     :                       'component is missing.', STATUS )
            END IF

*  FIRST_DATA
*  ----------
            CALL DAT_THERE( DCB_LOC( IDCB1 ), 'FIRST_DATA', THERE,
     :                      STATUS )
            IF( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB1 ), 'FIRST_DATA', LOC2,
     :                        STATUS )
               CALL DAT_COPY( LOC2,  DCB_LOC( IDCB2 ), 'FIRST_DATA',
     :                        STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )

            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__DLTIN
               CALL DAT_MSG( 'A', DCB_LOC( IDCB1 ) )
               CALL ERR_REP( ' ', 'The DELTA compressed array '//
     :                       '''^A'' is invalid - the FIRST_DATA '//
     :                       'component is missing.', STATUS )
            END IF


*  FIRST_VALUE
*  ----------
            CALL DAT_THERE( DCB_LOC( IDCB1 ), 'FIRST_VALUE', THERE,
     :                      STATUS )
            IF( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB1 ), 'FIRST_VALUE', LOC2,
     :                        STATUS )
               CALL DAT_COPY( LOC2,  DCB_LOC( IDCB2 ), 'FIRST_VALUE',
     :                        STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )

            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__DLTIN
               CALL DAT_MSG( 'A', DCB_LOC( IDCB1 ) )
               CALL ERR_REP( ' ', 'The DELTA compressed array '//
     :                       '''^A'' is invalid - the FIRST_VALUE '//
     :                       'component is missing.', STATUS )
            END IF

*  ZRATIO
*  ------
            CALL DAT_THERE( DCB_LOC( IDCB1 ), 'ZRATIO', THERE,
     :                      STATUS )
            IF( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB1 ), 'ZRATIO', LOC2,
     :                        STATUS )
               CALL DAT_COPY( LOC2,  DCB_LOC( IDCB2 ), 'ZRATIO',
     :                        STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )

            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = ARY__DLTIN
               CALL DAT_MSG( 'A', DCB_LOC( IDCB1 ) )
               CALL ERR_REP( ' ', 'The DELTA compressed array '//
     :                       '''^A'' is invalid - the ZRATIO '//
     :                       'component is missing.', STATUS )
            END IF

*  Now do optional components. Copy these if they exist in the input, but
*  do not report an error if they do not exist.

*  SCALE
*  -----
            CALL DAT_THERE( DCB_LOC( IDCB1 ), 'SCALE', THERE,
     :                      STATUS )
            IF( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB1 ), 'SCALE', LOC2,
     :                        STATUS )
               CALL DAT_COPY( LOC2,  DCB_LOC( IDCB2 ), 'SCALE',
     :                        STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )
            END IF


*  ZERO
*  ----
            CALL DAT_THERE( DCB_LOC( IDCB1 ), 'ZERO', THERE,
     :                      STATUS )
            IF( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB1 ), 'ZERO', LOC2,
     :                        STATUS )
               CALL DAT_COPY( LOC2,  DCB_LOC( IDCB2 ), 'ZERO',
     :                        STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )
            END IF

*  REPEAT
*  ------
            CALL DAT_THERE( DCB_LOC( IDCB1 ), 'REPEAT', THERE,
     :                      STATUS )
            IF( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB1 ), 'REPEAT', LOC2,
     :                        STATUS )
               CALL DAT_COPY( LOC2,  DCB_LOC( IDCB2 ), 'REPEAT',
     :                        STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )
            END IF

*  FIRST_REPEAT
*  ------------
            CALL DAT_THERE( DCB_LOC( IDCB1 ), 'FIRST_REPEAT', THERE,
     :                      STATUS )
            IF( THERE ) THEN
               CALL DAT_FIND( DCB_LOC( IDCB1 ), 'FIRST_REPEAT', LOC2,
     :                        STATUS )
               CALL DAT_COPY( LOC2,  DCB_LOC( IDCB2 ),
     :                        'FIRST_REPEAT', STATUS )
               CALL DAT_ANNUL( LOC2, STATUS )
            END IF

*  Ensure the storage form is now delta in both the data object and the
*  DCB.
            CALL CMP_MODC( DCB_LOC( IDCB2 ), 'VARIANT', 6, 0, 0,
     :                     STATUS )
            CALL CMP_PUT0C( DCB_LOC( IDCB2 ), 'VARIANT', 'DELTA',
     :                      STATUS )
            DCB_FRM( IDCB2 ) = 'DELTA'

*  Report an error if the input array is not a DELTA array.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = ARY__FATIN
            CALL ERR_REP( 'ARY1_CPDLT_FORM', 'ARY1_CPDLT: Input '//
     :                    'array is not a DELTA array (internal '//
     :                    'programming error).', STATUS )
         END IF

      END IF

*  Call error tracing routine and exit.
      IF( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_CPDLT', STATUS )

      END
