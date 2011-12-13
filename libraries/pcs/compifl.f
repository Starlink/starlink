      PROGRAM COMPIFL
*+
*  Name:
*     COMPIFL

*  Purpose:
*     To produce the Unix compiled form (.IFC) of an interface module.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Program

*  Invocation:
*     $ COMPIFL interface_module_name

*  Description:
*     The program obtains the name of the interface module via the
*     system and then uses the PARSECON facility routines to open
*     the file containing the text (.IFL) form, to read and parse it
*     creating the SUBPAR COMMON blocks and then to create and write
*     the compiled (.IFC) form.

*  Deficiencies:
*     The prompting for task name is non-standard.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
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
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-OCT-1991 (BDK):
*        Original version.
*     18-JUN-1991 (AJC):
*        No need for LIB$FREELUN with portable PARSECON_OPENIFL
*        Completely re-arrange error reporting (with EMS)
*        Correct extra argument in calling DUMPIFC
*     27-JUN-1992 (AJC):
*        Remove final status message now all PARSECON uses EMS
*     05-MAR-1992 (AJC):
*        Use SUBPAR_GTCMD not LIB$GET_FOREIGN
*     10-MAR-1992 (AJC):
*        Separate VMS and Unix versions _ this is Unix
*        Specify extensions in lower case.
*        Revised calling sequence for _OPENIFC use 'UNKNOWN'
*        Remove 1X, from FORMAT statement
*     31-MAR-1992 (AJC):
*        Rely on EMS_RLSE for output of final message - EMS changed
*      5-JUN-1992 (AJC):
*        Allow task name to have .ifl on it
*        Remove <> from format
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of string
      EXTERNAL CHR_LEN

*  Local Variables:
      CHARACTER*80 TASKNAME      ! Name of task
      CHARACTER*4 EXT            ! File extension
      INTEGER LENGTH             ! Length of TASKNAME
      INTEGER LU                 ! Fortran unit number for reading
                                 ! .IFL then writing .IFC
      INTEGER NUMERR             ! number of parsing errors
      INTEGER ISTAT              ! Local status

*.

      STATUS = SAI__OK

*  Start error message deferral
      CALL EMS_MARK

*  Get the name of the interface module
      CALL SUBPAR_GTCMD ( TASKNAME, STATUS )
*  If the command line is blank, prompt for the application name
      DOWHILE ( TASKNAME .EQ. ' ' )

         WRITE( *, 10 ) 'Task name? > '
10       FORMAT ( A, $ )
         READ ( *, 20 ) TASKNAME
20       FORMAT ( A )

      ENDDO

*  Get used length of the name
      LENGTH = CHR_LEN ( TASKNAME )

*  Open and parse the IFL text file
*  Remove extension if present
      EXT = TASKNAME(LENGTH-3:LENGTH)
      CALL CHR_UCASE( EXT )
      IF ( EXT .EQ. '.IFL' ) LENGTH = LENGTH-4
      CALL PARSECON_OPENIFL ( TASKNAME(1:LENGTH)//'.ifl', LU, STATUS )
      CALL PARSECON_READIFL ( LU, NUMERR, STATUS )
      CLOSE ( LU, IOSTAT = ISTAT )

*  Open and write the IFC compiled form of interface module
      CALL PARSECON_CREATIFC
     :( TASKNAME(1:LENGTH)//'.ifc', LENGTH + 4, 'UNKNOWN', LU, STATUS )
      CALL PARSECON_DUMPIFC ( LU, STATUS )
      CLOSE ( LU, IOSTAT = ISTAT )

*  Report conclusion

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_SETI( 'NUMERR', NUMERR )
         CALL EMS_REP( 'COMPIFL1',
     :    'COMPIFL: Failed with ^NUMERR parsing errors',
     :    STATUS )

*  If status OK but error reported
      ELSE IF ( NUMERR .GT. 0 ) THEN
         STATUS = SAI__ERROR
         CALL EMS_SETI( 'NUMERR', NUMERR )
         CALL EMS_REP( 'COMPIFL3',
     :    'COMPIFL: Completed with ^NUMERR parsing errors',
     :    STATUS )

*  Else all OK.
      ELSE
         STATUS = SAI__ERROR
         CALL EMS_REP( 'COMPIFL4',
     :    'COMPIFL: Successful completion', STATUS )

      ENDIF

*  Release the error context - flushes the messages
      CALL EMS_RLSE

      END

