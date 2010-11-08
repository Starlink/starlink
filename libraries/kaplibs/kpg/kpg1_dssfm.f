      SUBROUTINE KPG1_DSSFM( INDF, COMP, REPORT, FORM, SCALE, ZERO,
     :                       SCTYP, STATUS )
*+
*  Name:
*     KPG1_DSSFM

*  Purpose:
*     Displays information about the storage form of an NDF array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_DSSFM( INDF, COMP, REPORT, FORM, SCALE, ZERO, SCTYP,
*                      STATUS )

*  Description:
*     This routine displays a textual description of the storage form of
*     the specified NDF array.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the ndf.
*     COMP = CHARACTER * ( * ) (Given)
*        The name of the NDF component (DATA, QUALITY or VARIANCE) to be
*        displayed.
*     REPORT = LOGICAL (Given)
*        If .TRUE. the storage form information is displayed on standard
*        output.
*     FORM = CHARACTER * ( * ) (Returned)
*        The storage form.
*     SCALE = DOUBLE PRECISION (Returned)
*        The scale factor. This will be set to 1.0 if the storage form
*        does not support scaled storage.
*     ZERO = DOUBLE PRECISION (Returned)
*        The zero offset. This will be set to 0.0 if the storage form
*        does not support scaled storage.
*     SCTYP = CHARACTER * ( * ) (Returned)
*        The scaled data type. This will be set to ' ' if the storage form
*        does not support scaled storage.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*      8-NOV-2010 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER COMP*(*)
      LOGICAL REPORT

*  Arguments Returned:
      CHARACTER FORM*(*)
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION ZERO
      CHARACTER SCTYP*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables :
      INTEGER ZAXIS              ! Index of compression axis
      REAL ZRATIO                ! Compression ratio
      CHARACTER FTYPE*( DAT__SZTYP )! Array data type  (unscaled)
      CHARACTER ZTYPE*( DAT__SZTYP )! Data type for delta compressed values

*.

*  Initialise
      FORM = ' '
      SCTYP = ' '
      SCALE = 1.0
      ZERO = 0.0

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the array storage form
      CALL NDF_FORM( INDF, COMP, FORM, STATUS )

*  Get the unscaled array data type.
      CALL NDF_TYPE( INDF, COMP, FTYPE, STATUS )

*  If required, display the form.
      IF( REPORT ) THEN
         CALL MSG_SETC( 'FORM', FORM )
         CALL MSG_OUT( 'DATA_FORM', '      Storage form:  ^FORM',
     :                 STATUS )
      END IF

*  Both SCALED and DELTA forms can contain scaling information. Note,
*  scaling information is not available for quality arrays.
      IF( COMP( 1 : 1 )  .NE. 'Q' .AND.  (
     :    FORM .EQ. 'SCALED' .OR. FORM .EQ. 'DELTA' ) ) THEN

*  Get the scale, zero and data type.
         CALL NDF_SCTYP( INDF, COMP, SCTYP, STATUS )
         CALL NDF_GTSZD( INDF, COMP, SCALE, ZERO, STATUS )

*  If required, report them. If the form is delta, only display them if
*  they are not the default values.
         IF ( REPORT .AND. ( FORM .EQ. 'SCALED' .OR.
     :                       SCALE .NE. 1.0 .OR. ZERO .NE. 0.0 ) ) THEN
            CALL MSG_SETC( 'SCTY', SCTYP )
            CALL MSG_OUT( 'DATA_SCTYP', '         Scaled type :  ^SCTY',
     :                    STATUS )

            IF( FTYPE .EQ. '_DOUBLE' ) THEN
               CALL MSG_SETD( 'SCAL', SCALE )
               CALL MSG_OUT( 'DATA_SCALE', '         Scale factor:  '//
     :                       '^SCAL', STATUS )
               CALL MSG_SETD( 'ZERO', ZERO )
               CALL MSG_OUT( 'DATA_ZERO',  '         Zero offset :  '//
     :                       '^ZERO', STATUS )

            ELSE IF( FTYPE .EQ. '_REAL' ) THEN
               CALL MSG_SETR( 'SCAL', REAL( SCALE ) )
               CALL MSG_OUT( 'DATA_SCALE', '         Scale factor:  '//
     :                       '^SCAL', STATUS )
               CALL MSG_SETR( 'ZERO', REAL( ZERO ) )
               CALL MSG_OUT( 'DATA_ZERO',  '         Zero offset :  '//
     :                       '^ZERO', STATUS )

            ELSE
               CALL MSG_SETI( 'SCAL', NINT( SCALE ) )
               CALL MSG_OUT( 'DATA_SCALE', '         Scale factor:  '//
     :                       '^SCAL', STATUS )
               CALL MSG_SETI( 'ZERO', NINT( ZERO ) )
               CALL MSG_OUT( 'DATA_ZERO',  '         Zero offset :  '//
     :                       '^ZERO', STATUS )
            END IF
         END IF
      END IF

*  Display extra information for delta compressed arrays.
      IF( FORM .EQ. 'DELTA' ) THEN
         CALL NDF_GTDLT( INDF, COMP, ZAXIS, ZTYPE, ZRATIO, STATUS )
         IF ( REPORT ) THEN
            CALL MSG_SETC( 'ZTY', ZTYPE )
            CALL MSG_OUT( 'DATA_ZTYP',
     :                    '         Compressed type   :  ^ZTY', STATUS )
            CALL MSG_SETI( 'ZAX', ZAXIS )
            CALL MSG_OUT( 'DATA_ZAX',
     :                    '         Compression axis  :  ^ZAX', STATUS )
            CALL MSG_SETR( 'ZRA', ZRATIO )
            CALL MSG_OUT( 'DATA_ZRA',
     :                    '         Compression ratio :  ^ZRA', STATUS )
         END IF
      END IF

      END
