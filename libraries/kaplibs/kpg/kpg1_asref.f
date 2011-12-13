      SUBROUTINE KPG1_ASREF( PNNDF, MODE, GOTNAM, NAME, NDF, STATUS )
*+
*  Name:
*     KPG1_ASREF

*  Purpose:
*     Associates an NDF optionally from a reference name or locator.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASREF( PNNDF, MODE, GOTNAM, NAME, NDF, STATUS )

*  Description:
*     This routine obtains an NDF.  There is a search path of sources
*     for the NDF checked in the following order: a) command line, b) a
*     reference locator or name, c) elsewhere in the parameter system
*     (usually by prompting.)  For a) and c) the NDF is obtained by
*     association.  For b) a locator or name is imported into the NDF_
*     system; an error will result if the locator does not point to a
*     valid NDF.  The role of this routine is to permit automatic
*     processing of NDFs associated with database pictures.  The
*     command-line access allows the NDF reference stored with the last
*     DATA picture to be overridden.

*  Arguments:
*     PNNDF = CHARACTER * ( * ) (Given)
*        The name of the Starlink parameter to be associated with the
*        input NDF. May be blank, in which case no attempt will be made
*        to obtain the NDF form the environment.
*     MODE = CHARACTER * ( * ) (Given)
*        Access mode to the NDF required: 'READ' or 'UPDATE'.
*     GOTNAM = LOGICAL (Given)
*        If .TRUE., a name or locator to a potential NDF has already
*        been obtained and is supplied to this routine through the NAME
*        argument.  The name or locator may come from a reference in
*        the graphics database.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the potential NDF, or a locator to it.
*     NDF = INTEGER (Returned)
*        The identifier to the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  The parameter should not have been associated before calling
*     this routine.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1996, 1999 Central Laboratory of the Research
*                   Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research
*                   Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 March 24 (MJC):
*        Original version.
*     1996 March 19 (MJC):
*        The reference string can also be an NDF specification as well
*        as a locator.  The GOTLOC and LOC arguments are renamed to
*        GOTNAM and NAME to reflect this.  Used the modern-style of
*        commenting.
*     2-JUL-1999 (DSB):
*        Modified so that an NDF is obtained by association if the
*        displayed NDF cannot be accessed.
*     13-JAN-2006 (DSB):
*        Modified so that the parameter name is ignored if blank.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PAR_PAR'          ! PAR constants

*  Arguments Given:
      CHARACTER * ( * ) PNNDF
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) NAME

      LOGICAL GOTNAM

*  Arguments Returned:
      INTEGER NDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 6 ) CMODE    ! Uppercase copy of the access mode
      INTEGER STATE              ! The state of the parameter
      LOGICAL VALID              ! Supplied reference is a locator?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check input mode is valid by converting to uppercase and removing
*  leading blanks.
      CMODE = MODE
      CALL CHR_UCASE( CMODE )
      CALL CHR_LDBLK( CMODE )
      IF ( CMODE( 1:1 ) .NE. 'R' .AND. CMODE( 1:1 ) .NE. 'U' ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'MODE', MODE )
         CALL ERR_REP( 'KPG1_ASREF_INVMODE',
     :     'Access mode must be READ or UPDATE to access by reference.'/
     :     /'Input mode is ^MODE. (Programmming error).', STATUS )
         GOTO 999
      END IF

*  Obtain the NDF.
*  ===============
*
*  There is a choice from where to obtain the NDF.  In order try the
*  a) command line, b) graphics database, c) prompting.

*  If a parameter was supplied, see if the NDF is pre-supplied on the
*  command line.
      IF( PNNDF .NE. ' ' ) THEN
         CALL LPG_STATE( PNNDF, STATE, STATUS )
      END IF

*  Get an identifier to the input NDF if it has not already been
*  obtained on the command line, using a supplied reference.  See if the
*  reference is by locator or name.
      IF ( (PNNDF .EQ. ' ' .OR. STATE .NE. PAR__ACTIVE ) .AND.
     :     GOTNAM ) THEN

         CALL DAT_VALID( NAME( :DAT__SZLOC ), VALID, STATUS )
         IF ( VALID ) THEN
            CALL NDF_FIND( NAME( :DAT__SZLOC ), ' ', NDF, STATUS )
         ELSE
            CALL NDF_FIND( DAT__ROOT, NAME, NDF, STATUS )
         END IF

*  Tell the user which file is being accessed.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL NDF_MSG( 'NDF', NDF )
            CALL MSG_OUT( 'REFNDF',
     :        'Using ^NDF as the input NDF.', STATUS )

*  Flush the error and then tell the user that the displayed NDF cannot
*  be used. Then obtain an alternative NDF by association.
         ELSE
            CALL ERR_FLUSH( STATUS )
            CALL MSG_BLANK( STATUS )
            IF( PNNDF .NE. ' ' ) THEN
               CALL MSG_OUT( 'KPG1_ASREF_REFOBJ', 'The displayed NDF '//
     :                       'cannot be accessed. Please supply an '//
     :                       'alternative NDF.', STATUS )
               CALL LPG_ASSOC( PNNDF, MODE, NDF, STATUS )
            ELSE
               CALL MSG_OUT( 'KPG1_ASREF_REFOBJ', 'The displayed NDF '//
     :                       'cannot be accessed.', STATUS )
            END IF
         END IF

*  Obtain the identifier of the NDF by association.
      ELSE IF( PNNDF .NE. ' ' ) THEN
         CALL LPG_ASSOC( PNNDF, MODE, NDF, STATUS )
      END IF

  999 CONTINUE

      END
