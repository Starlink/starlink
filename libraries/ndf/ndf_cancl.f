      SUBROUTINE NDF_CANCL( PARAM, STATUS )
*+
*  Name:
*     NDF_CANCL

*  Purpose:
*     Cancel the association of an NDF with an ADAM parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_CANCL( PARAM, STATUS )

*  Description:
*     This routine cancels the association of an NDF with an ADAM
*     parameter. A subsequent attempt to get a value for the parameter
*     will result in a new value being obtained by the underlying
*     parameter system.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter. If a blank string is supplied, all
*        NDF parameters that have been accessed using NDF_ASSOC or
*        NDF_EXIST are cancelled.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The behaviour of this routine is identical to PAR_CANCL, except
*     that it allows all NDF parameters to be cancelled by supplying a
*     blank parameter name.
*     -  Any remaining NDF identifiers for the associated NDF are
*     unaffected by this routine. It's only affect is to cause NDF_ASSOC
*     or NDF_EXIST to prompt for a new NDF when called subsequently.
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     19-OCT-2012 (DSB):
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
      INCLUDE 'AST_PAR'          ! AST_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_APB'          ! NDF_ ADAM Parameter Block
*        APB_PARS = INTEGER (Read and Write)
*           An AST KeyMap holding a list of parameters with associated
*           NDFs.


*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) KEY ! Current parameter name
      LOGICAL THERE              ! Was the parameter found in the APB?
*.

*  Begin a new error reporting context.
      CALL ERR_BEGIN( STATUS )

*  If a parameter name was supplied...
      IF( PARAM .NE. ' ' ) THEN

*  Attempt to remove the parameter from the ADAM Parameter Block. This block
*  contains the names and SUBPAR codes for all parameters that have been
*  associated with NDFs by the NDF library (e.g. by NDF_ASSOC or NDF_EXIST).
         IF( APB_PARS .NE. 0 ) THEN
            THERE = AST_MAPHASKEY( APB_PARS, PARAM, STATUS )
            IF( THERE ) CALL AST_MAPREMOVE( APB_PARS, PARAM, STATUS )
         END IF

*  If the parameter was found, cancel it. Otherwise report an error.
         IF( THERE ) THEN
            CALL PAR_CANCL( PARAM, STATUS )
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = NDF__NOPAR
            CALL MSG_SETC( 'P', PARAM )
            CALL ERR_REP( 'NDF_CANCL_NOP', 'The parameter ''^P'' '//
     :                    'is not associated with an NDF (possible '//
     :                    'programming error).', STATUS )
         END IF

*  If no parameter name was supplied, and a list of parameters is
*  available...
      ELSE IF( APB_PARS .NE. 0 ) THEN

*  Loop round all parameters in the APB, cancelling and removing each one.
         DO WHILE( AST_MAPSIZE( APB_PARS, STATUS ) .GT. 0 .AND.
     :             STATUS .EQ. SAI__OK )
            KEY = AST_MAPKEY( APB_PARS, 1, STATUS )
            CALL PAR_CANCL( KEY, STATUS )
            CALL AST_MAPREMOVE( APB_PARS, KEY, STATUS )
         END DO

      END IF

* If an error occurred, report context information and call the error
* tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_CANCL_ERR',
     :                 'NDF_CANCL: Error cancelling an NDF parameter.',
     :                 STATUS )
         CALL NDF1_TRACE( 'NDF_CANCL', STATUS )
      END IF

*  End the error reporting context.
      CALL ERR_END( STATUS )

      END
