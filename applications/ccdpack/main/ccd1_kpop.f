      SUBROUTINE CCD1_KPOP( LABEL, GOTU, USTR, NK, KEYS, KSTRS, STATUS )
*+
*  Name:
*     CCD1_KPOP

*  Purpose:
*     Output keyed global parameter values.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_KPOP( LABEL, GOTU, USTR, NK, KEYS, KSTRS, STATUS )

*  Description:
*     Outputs the values of global variables via the CCDPACK logging
*     system.  Both the unkeyed value and any values keyed by Set Index
*     may be supplied.  In the general case a line will be output
*     with the unkeyed value and an extra line for each of the keyed
*     values labelled by Set Index.  However, in the event that all
*     the keyed values are the same as the unkeyed one, only the
*     unkeyed line will be output.
*
*     All variable values should be supplied as strings to this routine -
*     no interpretation of the values is done apart from assessing
*     equality between them.

*  Arguments:
*     LABEL = CHARACTER * ( * ) (Given)
*        Heading label for these values.
*     GOTU = LOGICAL (Given)
*        Is there an unkeyed value?
*     USTR = CHARACTER * ( * ) (Given)
*        The unkeyed value of the parameter (only used if GOTU is true).
*     NK = INTEGER (Given)
*        The number of keyed values given.
*     KEYS( NK ) = INTEGER (Given)
*        Set Index values (keys) for the keyed values.
*     KSTRS( NK ) = CHARACTER * ( * ) (Given)
*        The keyed values of the parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-MAY-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) LABEL
      LOGICAL GOTU
      CHARACTER * ( * ) USTR
      INTEGER NK
      INTEGER KEYS( NK )
      CHARACTER * ( * ) KSTRS( NK )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Used length of string

*  Local Variables:
      CHARACTER * ( 80 ) BUFFER  ! Line buffer
      INTEGER I                  ! Loop variable
      LOGICAL KSAME              ! Are all the keyed values identical?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if all the keyed values are identical.
      IF ( NK .GT. 1 ) THEN
         KSAME = .FALSE.
         DO I = 2, NK
            IF ( KSTRS( I ) .NE. KSTRS( 1 ) ) GO TO 1
         END DO
         KSAME = .TRUE.
 1       CONTINUE
      ELSE
         KSAME = .TRUE.
      END IF

*  Set up a message token controlling the unkeyed variable.
      IF ( GOTU ) THEN
         CALL MSG_SETC( 'UVAL', USTR )
      ELSE
         CALL MSG_SETC( 'UVAL', ' ' )
      END IF

*  Output the unkeyed line.
      BUFFER = ' '
      BUFFER( 3: ) = LABEL
      BUFFER( 49: ) = ': '
      CALL MSG_SETC( 'BUFFER', BUFFER )
      CALL CCD1_MSG( ' ', '^BUFFER ^UVAL', STATUS )

*  If there are keyed values and they are not all the same as the
*  unkeyed one, then write one line for each.
      IF ( NK .GT. 0 .AND.
     :     .NOT. ( GOTU .AND. KSAME .AND. USTR .EQ. KSTRS( 1 ) ) ) THEN
         DO I = 1, NK
            CALL MSG_SETI( 'SINDEX', KEYS( I ) )
            CALL MSG_SETC( 'KVAL', KSTRS( I ) )
            CALL CCD1_MSG( ' ',
     :'                                       Set Index ^SINDEX : '//
     :                     '^KVAL', STATUS )
         END DO
      END IF

      END
* $Id$
