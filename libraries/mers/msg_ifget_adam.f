      SUBROUTINE MSG_IFGET( PNAME, STATUS )
*+
*  Name:
*     MSG_IFGET

*  Purpose:
*     Get the filter level for conditional message output from the ADAM
*     parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_IFGET( PNAME, STATUS )

*  Description:
*     Translate the given parameter name into a value for the filter
*     level for conditional message output. The translation accepts
*     abbreviations. This value is then used to set the informational
*     filtering level. It is recommended that one parameter name is
*     used universally for this purpose, namely MSG_FILTER, in order to
*     clarify the interface file entries.  The three acceptable strings
*     for MSG_FILTER are
*
*        -  QUIET -- representing MSG__QUIET;
*        -  NORMAL -- representing MSG__NORM;
*        -  VERBOSE -- representing MSG__VERB.
*        -  DEBUG -- representing MSG__DEBUG
*
*     MSG_IFGET accepts abbreviations of these strings; any other value
*     will result in an error report and the status value being
*     returned set to MSG__INVIF. If an error occurs getting the
*     parameter value, the status value is returned and an additional
*     error report is made.

*  Arguments:
*     PNAME = CHARACTER * ( * ) (Given)
*        The filtering level parameter name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 1996, 1999, 2004 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A. J. Chipperfield (STARLINK)
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (PCTR):
*        Original version.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being annulled
*        on error.
*     27-AUG-1992 (PCTR):
*        Changed PAR call to a SUBPAR call and enabled abbreviations in
*        the accepted parameter values.
*     25-JAN-1996 (AJC):
*        re-format CHARACTER declarations
*     17-SEP-1999 (AJC):
*        Avoid calling MSG_IFSET - linking problem
*     1-JUL-2004 (DSB):
*        Use MSG1_GT... functions to get the values from the MSG_CMN 
*        common blocks rather than directly accessing the common blocks
*        (which are initialised in a different shared library).
*     02-MAY-2008 (TIMJ):
*        Add MSG__DEBUG
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ public constants
      INCLUDE 'MSG_ERR'          ! MSG_ error codes

*  Arguments Given:
      CHARACTER * ( * ) PNAME

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL CHR_SIMLR          ! Caseless string equality
      INTEGER CHR_LEN            ! String length

*  Local Constants:
      CHARACTER * ( 7 ) QUIET    ! QUIET string
      PARAMETER ( QUIET = 'QUIET' )

      CHARACTER * ( 7 ) NORMAL   ! NORMAL string
      PARAMETER ( NORMAL = 'NORMAL' )

      CHARACTER * ( 7 ) VERBOS   ! VERBOSE string
      PARAMETER ( VERBOS = 'VERBOSE' )

      CHARACTER * ( 7 ) DEBUG    ! DEBUG string
      PARAMETER ( DEBUG = 'DEBUG' )

*  Local Variables:
      LOGICAL MATCH              ! Whether a match has been found

      INTEGER FILTER             ! Message filtering level
      INTEGER FLEN               ! Length of filter name
      INTEGER NAMCOD             ! SUBPAR pointer to parameter
      INTEGER VLEN               ! Filtering level name length

      CHARACTER FNAME * ( 8 )    ! Name of message filtering level
      CHARACTER VNAME * ( 7 )    ! Name of message filetering value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark a new error reporting context.
      CALL EMS_MARK

*  Get the message filtering level from the parameter system.
      CALL SUBPAR_FINDPAR( PNAME, NAMCOD, STATUS )
      CALL SUBPAR_GET0C( NAMCOD, FNAME, STATUS )

*  Check the returned status.
      IF ( STATUS .NE. SAI__OK ) THEN

*     A parameter system error has occured: set the returned status,
*     report the error and abort.
         CALL EMS_REP( 'MSG_GETIF_NOPAR',
     :   'MSG_IFGET: Unable to get the informational filtering ' //
     :   'level from the parameter system.', STATUS )
      ELSE

*     Initialise the filtering level match flag.
         MATCH = .FALSE.

*     Get the length of the given string.
         FLEN = CHR_LEN( FNAME )

*     Compare the given string with the filtering level strings. If the
*     length of the given string is greater than the filtering level
*     value, no match is possible.
         VNAME = NORMAL
         VLEN = CHR_LEN( VNAME )

         IF ( FLEN .LE. VLEN ) THEN
            MATCH = CHR_SIMLR( FNAME( : FLEN ), VNAME( : FLEN ) )

*        Check whether a match has been found: if so, set NORMAL mode.
            IF ( MATCH ) FILTER = MSG__NORM
         END IF

*     If there has been no match, try QUIET mode.
         IF ( .NOT. MATCH ) THEN
            VNAME = QUIET
            VLEN = CHR_LEN( VNAME )

            IF ( VLEN .GE. FLEN ) THEN
               MATCH = CHR_SIMLR( FNAME( : FLEN ), VNAME( : FLEN ) )

*           Check whether a match has been found: if so, set QUIET mode.
               IF ( MATCH ) FILTER = MSG__QUIET
            END IF
         END IF

*     If there has been no match, try VERBOSE mode.
         IF ( .NOT. MATCH ) THEN
            VNAME = VERBOS
            VLEN = CHR_LEN( VNAME )

            IF ( VLEN .GE. FLEN ) THEN
               MATCH = CHR_SIMLR( FNAME( : FLEN ), VNAME( : FLEN ) )

*           Check whether a match has been found: if so, set VERBOSE mode.
               IF ( MATCH ) FILTER = MSG__VERB
            END IF
         END IF

*     If there has been no match, try DEBUG mode.
         IF ( .NOT. MATCH ) THEN
            VNAME = DEBUG
            VLEN = CHR_LEN( VNAME )

            IF ( VLEN .GE. FLEN ) THEN
               MATCH = CHR_SIMLR( FNAME( : FLEN ), VNAME( : FLEN ) )

*           Check whether a match has been found: if so, set DEBUG mode.
               IF ( MATCH ) FILTER = MSG__DEBUG
            END IF
         END IF

*     Set the message filtering level.
         IF ( MATCH ) THEN
            CALL MSG1_PTINF( FILTER )
         ELSE

*        An invalid filter name has been used, so report an error.
            STATUS = MSG__INVIF
            CALL EMS_SETC( 'FILTER', FNAME( 1 : FLEN ) )
            CALL EMS_REP( 'MSG_IFGET_INVIF', 
     :      'MSG_IFGET: Invalid message filtering level: ^FILTER',
     :      STATUS )
         END IF
      END IF

*  Release the current error reporting context.
      CALL EMS_RLSE

      END
