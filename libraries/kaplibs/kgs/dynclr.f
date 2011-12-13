      SUBROUTINE DYNCLR( WKID, DYNAMC, STATUS )
*+
*  Name:
*     DYNCLR

*  Purpose:
*     Has the specified graphics workstation a dynamic colour.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DYNCLR( WKID, DYNAMC, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     For the current GKS workstation determine whether or not the
*     colour representation is dynamic.  GKS must be in state WSOP or
*     WSAC.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Inquire the workstation type
*     Inquire whether GKS has reported an error
*     If so report context and exit
*     Inquire workstation dynamic attributes
*     Inquire whether GKS has reported an error
*     If no error occurred then
*        Set the dynamic colour table flag if appropriate
*     Else
*        Report context and exit
*     Endif
*     End

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     Malcolm J. Currie  Starlink (RAL::CUR)
*     {enter_new_authors_here}

*  History:
*     1989 Apr 13: Original (RAL::CUR).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! no implicit typing allowed


*  Global Constants:
      INCLUDE  'SAE_PAR'       ! global SSE definitions


*  Arguments Given:
      INTEGER
     :    WKID


*  Arguments Returned:
      LOGICAL
     :    DYNAMC


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER
     :    COLREP,              ! colour representation changeable?
     :    CONID,               ! connection identifier
     :    GSTAT,               ! graphics status
     :    FABUN,               ! dummy for GQDWKA
     :    PAREP,               ! dummy for GQDWKA
     :    PLBUN,               ! dummy for GQDWKA
     :    PMBUN,               ! dummy for GQDWKA
     :    TXBUN,               ! dummy for GQDWKA
     :    WKTR,                ! dummy for GQDWKA
     :    WTYPE                ! workstation type


*.

*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

      DYNAMC = .FALSE.

*    Inquire the workstation type

      CALL GQWKC( WKID, GSTAT, CONID, WTYPE )

*    Inquire whether GKS has reported an error

      CALL GKS_GSTAT( STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_DYNCLR_WTYPE',
     :     'DYNCLR: Error while obtaining graphics workstation type',
     :     STATUS )

         GOTO 999
      END IF

*    Inquire workstation dynamic attributes

      CALL GQDWKA( WTYPE, GSTAT, PLBUN, PMBUN, TXBUN, FABUN, PAREP,
     :             COLREP, WKTR )

*    Inquire whether GKS has reported an error

      CALL GKS_GSTAT( STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( COLREP .EQ. 1 ) DYNAMC = .TRUE.
      ELSE
         CALL ERR_REP( 'ERR_DYNCLR_QDWKA',
     :     'DYNCLR: Error while inquiring workstation dynamic '/
     :     /'attributes', STATUS )

         GOTO 999
      END IF

 999  CONTINUE
      END
