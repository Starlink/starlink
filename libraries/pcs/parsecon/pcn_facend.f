      SUBROUTINE PARSECON_FACEND ( STATUS )
*+
*  Name:
*     PARSECON_FACEND

*  Purpose:
*     ENDINTERFACE action.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_FACEND ( STATUS )

*  Description:
*     Check that defined 'positions' are correct and clear
*     the action name from the error report common block.

*  Arguments:
*     STATUS=INTEGER

*  Algorithm:
*     Compare the highest position specified (saved in COMMON) with
*     the number of parameters - report if it is greater.
*     Also check that the positions are a continuous set starting at 1.
*     Set ACNAME to blank.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     Copyright (C) 1192, 1990, 1991, 1993 Science & Engineering Research Council.
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
*     A.J.Chipperfield
*     A J Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16.08.1990: Original (RLVAD::AJC)
*     20.11.1991: Add check on contiguous position nos. (RLVAD::AJC)
*        6.03.1192: Correct above checking (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     2009-JUL-31 (TIMJ):
*        Force every interface entry to define MSG_FILTER and QUIET
*        parameters. This allows every application to have a standardised
*        parameter definition without forcing every IFL file to include it.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON3_CMN'
      INCLUDE 'PARSECON4_CMN'


*  Local Variables:
      INTEGER I                         ! Loop counter
      INTEGER J                         ! Loop counter

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*     Add additional default parameter definitions that are to be supported
*     by all interface entries.

*     Global message filtering in MERS
      CALL PARSECON_NEWPAR( 'MSG_FILTER', STATUS )
      IF (STATUS .EQ. PARSE__OLDPAR ) THEN
*     If the parameter is already defined we do nothing
         CALL EMS_ANNUL ( STATUS )
      ELSE
         CALL PARSECON_SETTYP( '_CHAR', STATUS )
         CALL PARSECON_SETACC( 'READ', STATUS )
         CALL PARSECON_SETPROM( 'Message level', STATUS )
         CALL PARSECON_SETVP( 'DEFAULT', STATUS )
         CALL PARSECON_SETHEL(
     :        'Selects the message filtering level. NONE, QUIET, '/
     :        /'NORMAL, VERBOSE, DEBUG or ALL. Or numeric equivalent.',
     :        STATUS )
         CALL PARSECON_SETDEF( '!', STATUS )
         CALL PARSECON_PAREND( STATUS )
      END IF

*     Global control of message output using a logical. QUIET=TRUE
*     will cause message filtering level to be "QUIET".
      CALL PARSECON_NEWPAR( 'QUIET', STATUS )
      IF (STATUS .EQ. PARSE__OLDPAR ) THEN
*     If the parameter is already defined we do nothing
         CALL EMS_ANNUL ( STATUS )
      ELSE
         CALL PARSECON_SETTYP( '_LOGICAL', STATUS )
         CALL PARSECON_SETACC( 'READ', STATUS )
         CALL PARSECON_SETPROM( 'Force message filter level to QUIET?',
     :        STATUS )
         CALL PARSECON_SETVP( 'DEFAULT', STATUS )
         CALL PARSECON_SETHEL( 'A true value suppresses output by'/
     :        /' using the QUIET message filtering level', STATUS )
         CALL PARSECON_SETDEF( '!', STATUS )
         CALL PARSECON_PAREND( STATUS )
      END IF

      IF ( HIPOS .GT. PARPTR ) THEN
*     Position storage for task overflowed
         STATUS = PARSE__NCPOS
         CALL EMS_SETI( 'POS', HIPOS )
         CALL EMS_REP( 'PCN_FACEND1',
     :   'PARSECON: Parameter "position" specified (^POS) '//
     :   'exceeds the number of parameters',
     :    STATUS )
*     Clear erroneous positions - ie those from the end of this
*     task to the start+HIPOS
         DO J = PROGADD(1,ACTPTR)+PARPTR, PROGADD(1,ACTPTR)+HIPOS
            PARPOS(J) = 0
         ENDDO

      ELSEIF ( HIPOS .NE. 0 ) THEN
*     Check for contiguous set of positions - starting at 1
*     For each element of PARPOS from PROGADD(1,ACTPTR) to HIPOS
*     check that it has been used.
         DO 10, I = PROGADD(1,ACTPTR), HIPOS

            IF ( PARPOS(I) .EQ. 0 ) THEN
*           The element wasn't used
*           i.e. the set isn't contiguous
               STATUS = PARSE__NCPOS
               CALL EMS_SETI( 'POS', I-PROGADD(1,ACTPTR)+1 )
               CALL EMS_REP( 'PCN_FACEND2',
     :         'PARSECON: Parameter "position" ^POS not allocated',
     :          STATUS )
               CALL EMS_REP( 'PCN_FACEND3',
     :         'Non-contiguous set of positions', STATUS )
            ENDIF

10       ENDDO
      ENDIF

*  Reset the name if status is OK - otherwise rely on a following
*  INTERFACE to reset it so that any error report refers to this
*  action.
      IF ( STATUS .EQ. SAI__OK ) ACNAME = ' '

      END
