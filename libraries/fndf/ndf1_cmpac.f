      SUBROUTINE NDF1_CMPAC( IDCB, COMP, STATUS )
*+
*  Name:
*     NDF1_CMPAC

*  Purpose:
*     Check for an error accessing a compressed array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CMPAC( IDCB, COMP, STATUS )

*  Description:
*     The routine check to see if an error has occurred whilst accessing
*     a compressed (and therefore read-only) array component of an NDF.
*     If such an error has been reported by the ARY system, then the
*     error is annulled and re-reported using more NDF-centric message.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the NDF entry in the DCB.
*     COMP = CHARACTER * ( * ) (Given)
*        The array component name; 'DATA', 'QUALITY' or 'VARIANCE' (or
*        'ERROR') (case insensitive). The name should not include any
*        white space.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-NOV-2010 (DSB):
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
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block

*  Arguments Given:
      INTEGER IDCB
      CHARACTER * ( * ) COMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      CHARACTER FORM*( NDF__SZFRM ) ! Array storage form
      CHARACTER CNAME*8          ! Array name

*.

*  Return if the error status does not indicate that an error occurred
*  accessing a compressed array.
      IF ( STATUS .NE. ARY__CMPAC ) RETURN

*  Get the name of the array to include in the error emssage. Also get he
*  storage form.
      IF( NDF1_SIMLR( COMP, 'DATA', NDF__MINAB ) ) THEN
         CNAME = 'DATA'
         FORM = DCB_DFRM( IDCB )

      ELSE IF( NDF1_SIMLR( COMP, 'ERRORS', NDF__MINAB ) ) THEN
         CNAME = 'ERROR'
         FORM = DCB_VFRM( IDCB )

      ELSE IF( NDF1_SIMLR( COMP, 'QUALITY', NDF__MINAB ) ) THEN
         CNAME = 'QUALITY'
         FORM = DCB_QFRM( IDCB )

      ELSE IF( NDF1_SIMLR( COMP, 'VARIANCE', NDF__MINAB ) ) THEN
         CNAME = 'VARIANCE'
         FORM = DCB_VFRM( IDCB )

      ELSE
         CNAME = ' '

      END IF

*  Do nothing if the NDF component is not an array component (something
*  has gone wrong!).
      IF( CNAME .NE. ' ' ) THEN

*  Annull the original error message.
         CALL ERR_ANNUL( STATUS )

*  Re-report the error.
         STATUS = NDF__CMPAC
         CALL DAT_MSG( 'N', DCB_LOC( IDCB ) )
         CALL MSG_SETC( 'A',  CNAME )
         CALL MSG_SETC( 'F',  FORM )
         CALL ERR_REP( ' ', 'The ^A array of the NDF ''^N'' is '//
     :                 'stored using ^F compression and therefore '//
     :                 'cannot be changed (^F compressed arrays '//
     :                 'are read-only).', STATUS )

      END IF

      END
