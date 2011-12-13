      SUBROUTINE TRN1_VPREC( PREC, VPREC, IPRC, STATUS )








*+
*  Name:
*     TRN1_VPREC

*  Purpose:
*     validate a precision string.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_VPREC( PREC, VPREC, IPRC, STATUS )

*  Description:
*     The routine validates a precision string.  If the string is valid,
*     a validated version (converted to upper case) is returned along
*     with a matching integer precision code.  The precision code values
*     are defined in the include file TRN_CONST.  If the string is not
*     valid, then STATUS is set and an error is reported.

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
*     9-MAY-1988:  Original version (DUVAD::RFWS)
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
      INCLUDE 'TRN_PAR'          ! TRN_ public constants
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      CHARACTER * ( * ) PREC    ! The precision string to be validated


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      CHARACTER * ( * ) VPREC   ! The validated precision string,
                                ! converted to upper case
      INTEGER IPRC              ! Precision code


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      CHARACTER * ( TRN__SZPRC ) PTEMP
                                ! Temporary precision string


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Make a temporary copy of the precision string.  Check that the input
*   string is not too long and report an error if it is.
      PTEMP = PREC
      IF( PTEMP .NE. PREC ) THEN
        STATUS = TRN__PRCIN     ! precision invalid
        CALL TRN1_ERROR( 'TRN1_VPREC', ' ', STATUS )


*   If there is no error, then convert the string to upper case and
*   compare it with each of the valid values it may take, setting the
*   precision code appropriately.
      ELSE
        CALL CHR_UCASE( PTEMP )

*   ...integer types:
        IF( PTEMP .EQ. '_INTEGER:' ) THEN
          IPRC = TRN_PR_EI
        ELSE IF ( PTEMP .EQ. '_INTEGER' ) THEN
          IPRC = TRN_PR_I

*   ...real types:
        ELSE IF( PTEMP .EQ. '_REAL:' ) THEN
          IPRC = TRN_PR_ER
        ELSE IF( PTEMP .EQ. '_REAL' ) THEN
          IPRC = TRN_PR_R

*   ...double precision types:
        ELSE IF( PTEMP .EQ. '_DOUBLE:' ) THEN
          IPRC = TRN_PR_ED
        ELSE IF( PTEMP .EQ. '_DOUBLE' ) THEN
          IPRC = TRN_PR_D


*   If the string was not recognised, report an error.
        ELSE
          STATUS = TRN__PRCIN   ! precision invalid
          CALL TRN1_ERROR( 'TRN1_VPREC', ' ', STATUS )
        ENDIF


*   End of "the string was not too long" condition.
      ENDIF


*   If there is no error, copy the validated string to the output.
      IF( STATUS .EQ. SAI__OK ) VPREC = PTEMP


*   Exit routine.
      END
