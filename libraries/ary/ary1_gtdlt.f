      SUBROUTINE ARY1_GTDLT( IDCB, ZAXIS, ZTYPE, ZRATIO, STATUS )
*+
*  Name:
*     ARY1_GTDLT

*  Purpose:
*     Get the compressed axis and data type for a DELTA array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_GTDLT( IDCB, ZAXIS, ZTYPE, ZRATIO, STATUS )

*  Description:
*     The routine returns the details of the compression used to produce
*     an array stored in DELTA form. If the array is not stored in
*     DELTA form, then null values are returned as listed below, but no
*     error is reported.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index of the data object entry in the DCB.
*     ZAXIS = INTEGER (Returned)
*        The index of the pixel axis along which compression occurred.
*        The first axis has index 1. Zero is returned if the array is not
*        stored in DELTA form.
*     ZTYPE = CHARACTER * ( DAT__SZTYP ) (Returned)
*        The data type in which the differences between adjacent array
*        values are stored. This will be one of '_BYTE', '_WORD' or
*        '_INTEGER'. The data type of the array itself is returned if the
*        supplid array is not stored in DELTA form.
*     ZRATIO = REAL (Returned)
*        The compression factor - the ratio of the uncompressed array size
*        to the compressed array size. This is approximate as it does not
*        include the effects of the metadata needed to describe the extra
*        components of a DELTA array (i.e. the space needed to hold the
*        component names, types, dimensions, etc). A value of 1.0 is
*        returned if the supplid array is not stored in DELTA form.
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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     2-NOV-2010 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block

*  Arguments Given:
      INTEGER IDCB

*  Arguments Returned:
      INTEGER ZAXIS
      CHARACTER ZTYPE * ( DAT__SZTYP )
      REAL ZRATIO

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER LOCC*(DAT__SZLOC)! Locator for component
      LOGICAL THERE              ! Does the component exist?
*.

*  Initialise returned values
      ZAXIS = 0
      ZTYPE = ' '
      ZRATIO = 1.0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that form information is available for the array.
      CALL ARY1_DFRM( IDCB, STATUS )

*  If the array is not stored in delta form, return the data type of the
*  array itself.
      IF ( DCB_FRM( IDCB ) .NE. 'DELTA' ) THEN

         IF( DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN
            CALL CMP_TYPE( DCB_SCLOC( IDCB ), 'SCALE', ZTYPE, STATUS )
         ELSE
            ZTYPE = DCB_TYP( IDCB )
         END IF

*  For delta arrays, get the required components from the data object.
      ELSE

         CALL DAT_THERE( DCB_LOC( IDCB ), 'ZAXIS', THERE, STATUS )
         IF( THERE ) THEN
            CALL DAT_FIND( DCB_LOC( IDCB ), 'ZAXIS', LOCC, STATUS )
            CALL DAT_GET0I( LOCC, ZAXIS, STATUS )
            CALL DAT_ANNUL( LOCC, STATUS )
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = ARY__DLTIN
            CALL DAT_MSG( 'A', DCB_LOC( IDCB ) )
            call ERR_REP( ' ', 'The DELTA compressed array ''^A'' is '//
     :                    'invalid - the ZAXIS component is missing.',
     :                    STATUS )
         END IF

         CALL DAT_THERE( DCB_LOC( IDCB ), 'ZRATIO', THERE, STATUS )
         IF( THERE ) THEN
            CALL DAT_FIND( DCB_LOC( IDCB ), 'ZRATIO', LOCC, STATUS )
            CALL DAT_GET0R( LOCC, ZRATIO, STATUS )
            CALL DAT_ANNUL( LOCC, STATUS )
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = ARY__DLTIN
            CALL DAT_MSG( 'A', DCB_LOC( IDCB ) )
            call ERR_REP( ' ', 'The DELTA compressed array ''^A'' is '//
     :                    'invalid - the ZRATIO component is missing.',
     :                    STATUS )
         END IF

         CALL DAT_THERE( DCB_LOC( IDCB ), 'DATA', THERE, STATUS )
         IF( THERE ) THEN
            CALL DAT_FIND( DCB_LOC( IDCB ), 'DATA', LOCC, STATUS )
            CALL DAT_TYPE( LOCC, ZTYPE, STATUS )
            CALL DAT_ANNUL( LOCC, STATUS )
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = ARY__DLTIN
            CALL DAT_MSG( 'A', DCB_LOC( IDCB ) )
            call ERR_REP( ' ', 'The DELTA compressed array ''^A'' is '//
     :                    'invalid - the DATA component is missing.',
     :                    STATUS )
         END IF

      END IF

      END
