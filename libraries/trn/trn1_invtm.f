      SUBROUTINE TRN1_INVTM( LOCTM, STATUS )
*+
*  Name:
*     TRN1_INVTM

*  Purpose:
*     invert a transformation module.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_INVTM( LOCTM, STATUS )

*  Description:
*     The routine inverts a standard transformation module passed by
*     HDS locator.  The locator is validated to ensure that it is
*     associated with a structure of type TRN_MODULE before use.

*  Copyright:
*     Copyright (C) 1988, 1992 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     25-APR-1988 (RFWS):
*        Original version.
*     13-FEB-1992 (RFWS):
*        Added handling of character string lengths when passing mapped
*        values (for Unix compatibility).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) LOCTM   ! Locator to the transformation module


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOCC ! Locator to COMMENT component
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS type string
      INTEGER IPC                ! Pointer to mapped comment string
      INTEGER LENC               ! Length of mapped character value
      LOGICAL THERE              ! Whether there is a COMMENT component


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Obtain the structure type.  If there is no error, check it is a
*   valid transformation module and report an error if it is not.
      CALL DAT_TYPE( LOCTM, TYPE, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN
        IF( TYPE .NE. 'TRN_MODULE' ) THEN
          STATUS = TRN__TYPIN   ! type invalid
          CALL TRN1_ERRL( 'TRN1_INVTM', LOCTM, STATUS )
        ENDIF
      ENDIF


*   See if there is a COMMENT component in the transformation module.
      THERE = .FALSE.
      CALL DAT_THERE( LOCTM, 'COMMENT', THERE, STATUS )


*   If there is one, map it as a character string and "invert" it, then
*   unmap it.
      IF( THERE ) THEN
        CALL DAT_FIND( LOCTM, 'COMMENT', LOCC, STATUS )
        CALL DAT_MAP( LOCC, '_CHAR', 'UPDATE', 0, 0, IPC, STATUS )
        CALL DAT_CLEN( LOCC, LENC, STATUS )
        CALL TRN1_INVCM( %VAL( CNF_PVAL( IPC ) ), STATUS,
     :                   %VAL( CNF_CVAL( LENC ) ) )
        CALL DAT_UNMAP( LOCC, STATUS )
        CALL DAT_ANNUL( LOCC, STATUS )
      ENDIF

*   Interchange the components specifying the numbers of input and
*   output variables.
      CALL TRN1_CSWAP( LOCTM, 'NVAR_IN', 'NVAR_OUT', STATUS )


*   Interchange the components specifying the forward and inverse
*   transformation functions.
      CALL TRN1_CSWAP( LOCTM, 'FORWARD_FUNC', 'INVERSE_FUNC', STATUS )


*   Exit routine.
      END
