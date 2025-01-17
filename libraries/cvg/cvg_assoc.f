      SUBROUTINE CVG_ASSOC( PARAM, MODE, FUNIT, BLOCKF, STATUS )
*+
*  Name:
*     CVG_ASSOC

*  Purpose:
*     Opens an existing FITS file specified by an environment parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CVG_ASSOC( PARAM, MODE, FUNIT, BLOCKF, STATUS )

*  Description:
*     This function opens an existing FITS file with a path obtained from
*     the environment, and returns a logical unit number that can be used
*     to access it using CVG and FITSIO functions.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the environemt parameter to use.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode: 'READ' or 'UPDATE. Case insensitive.
*        Abbreviations can be used.
*     FUNIT = INTEGER (Returned)
*        The logical unit number of the FITS file. Returned equal to
*        CVG_NOLUN if an error occurs.
*     BLOCKF = INTEGER (Returned)
*        The logical record blocking factor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council. All
*     Rights Reserved.

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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-NOV-2013 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CVG_PAR'          ! CVG constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER MODE*(*)

*  Arguments Returned:
      INTEGER FUNIT
      INTEGER BLOCKF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER PATH*(CVG__MXPTH)
*.

*  Initialise the returned logical unit number.
      FUNIT = CVG__NOLUN

*  Check inherited status
      IF( STATUS .NE. SAI__OK ) RETURN

*  Loop until a FITS file has been opened successfully or a parameter
*  null or abort error occurs.
      DO WHILE( FUNIT .EQ. CVG__NOLUN .AND. STATUS .NE. PAR__NULL
     :          .AND. STATUS .NE. PAR__ABORT )

*  Flush any error from the previous loop and cancel the parameter
*  so that we can try again.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL PAR_CANCL( PARAM, STATUS )
         END IF

*  Get a string from the user.
         CALL PAR_GET0C( PARAM, PATH, STATUS )

*  Attempt to open it as an existing FITS file.
         CALL CVG_OPEN( PATH, MODE, FUNIT, BLOCKF, STATUS )
      END DO

*  If an error occurred, then classify it...

*  If an "abort" was requested, then annul any error messages and issue
*  an appropriate new one.
      IF ( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )

         STATUS = PAR__ABORT
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( ' ', 'Aborted attempt to associate an '//
     :                 'existing FITS file with the ''%^PARAM'' '//
     :                 'parameter.', STATUS )

*  If an "null" NDF was specified, then annul any error messages and
*  issue an appropriate new one.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

         STATUS = PAR__NULL
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( ' ', 'Null FITS file specified for the ' //
     :                 '''%^PARAM'' parameter.', STATUS )

*  For other errors, add context information.
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( ' ', 'CVG_ASSOC: Error associating an existing'//
     :                 ' FITS file with the ''%^PARAM'' parameter.',
     :                 STATUS )
      END IF

*  Attempt to close the file if an error occurred.
      IF( STATUS .NE. SAI__OK ) CALL CVG_CLOSE( FUNIT, STATUS )

      END
