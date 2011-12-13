      SUBROUTINE TRN1_WRDST( LOCTR, DFOR, DINV, STATUS )








*+
*  Name:
*     TRN1_WRDST

*  Purpose:
*     write definition status information.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_WRDST( LOCTR, DFOR, DINV, STATUS )

*  Description:
*     The routine writes information describing the definition status
*     into the FORWARD and INVERSE components of a transformation
*     structure passed by HDS locator.  Symbolic constants are used to
*     specify the definition status values - these are defined in the
*     include file TRN_CONST. The values supplied are not validated
*     before use.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     29-MAR-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_CONST'        ! TRN_ private constants


*  Arguments Given:
      CHARACTER * ( * ) LOCTR   ! Locator to transformation structure
      INTEGER DFOR              ! Forward definition status
      INTEGER DINV              ! Inverse definition status


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      INTEGER IDIR              ! Direction counter: 1=forward 2=inverse
      INTEGER DVAL              ! Definition status
      CHARACTER * ( DAT__SZNAM ) DNAME
                                ! Direction name: 'FORWARD' or 'INVERSE'
      CHARACTER * 9 VAL         ! Character value: 'DEFINED' or
                                ! 'UNDEFINED'


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Loop to write to each object (FORWARD & INVERSE) in turn.
      DO IDIR = 1, 2


*   Set the direction name and definition status.
        IF( IDIR .EQ. 1 ) THEN
          DNAME = 'FORWARD'
          DVAL = DFOR
        ELSE
          DNAME = 'INVERSE'
          DVAL = DINV
        ENDIF


*   Set the character representation of the definition status.
        IF( DVAL .EQ. TRN_DS_DEF ) THEN
          VAL = 'DEFINED'
        ELSE IF( DVAL .EQ. TRN_DS_UDEF ) THEN
          VAL = 'UNDEFINED'
        ELSE
          VAL = '?'
        ENDIF


*   Ensure a component of the required type and precision exists.
        CALL CMP_MODC( LOCTR, DNAME, LEN( VAL ), 0, 0, STATUS )


*   Set the value.
        CALL CMP_PUT0C( LOCTR, DNAME, VAL, STATUS )


*   End of "loop to write to each object in turn" loop.
      ENDDO


*   Exit routine.
      END
