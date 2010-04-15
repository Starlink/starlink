      SUBROUTINE GLOBALS( STATUS )
*+
*  Name:
*     GLOBALS

*  Purpose:
*     Displays the values of the CATPAC global parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GLOBALS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This procedure lists the meanings and values of the CATPAC global
*     parameters.  If a global parameter does not have a value, the
*     string "<undefined>" is substituted where the value would have been
*     written.

*  Usage:
*     GLOBALS

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)

*  History:
*     1991 July 18 (MJC):
*        Original version.
*     17-OCT-1991 (ARW):
*        For CATPAC

*  Bugs:
*     None known.

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_ERR'          ! HDS error definitions
      INCLUDE 'DAT_PAR'          ! HDS error definitions

*  Status:
      INTEGER STATUS             ! Global status

*  External Variables:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER * ( 32 ) FILENAME ! Full filename
      CHARACTER * ( 28 ) FILENAME1 ! Full filename
      INTEGER LENGTH
      CHARACTER*(9) DIRNAME
      PARAMETER ( DIRNAME = 'ADAM_USER')
      CHARACTER*(132) PATH
      CHARACTER*(132) LIB
      INTEGER NC

      CHARACTER * ( DAT__SZLOC )
     :  LOC,                     ! Locator to the global file.
     :  PLOC                     ! Locator to the parameter-system
                                 ! structure containing the file and
                                 ! device names

      CHARACTER
     :  GLOVAL * ( 132 )         ! Global variable.

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the global file.
*  =====================
      filename = 'GLOBAL'
      call psx_getenv( dirname, path, status)
      nc = chr_len( path )
      lib = path( :nc)//'/'
      path = lib
      nc = nc + 1
      lib = path( :nc)//filename
      LOC = ' '
      CALL HDS_OPEN( lib, 'READ', LOC, STATUS )

*  Inquire the various global parameters.
*  ======================================

*  Bracket each with an error context, since we want to annul the
*  error if the object is not found (DAT__OBJNF) and substitute another
*  report.

*  The data array.  This is a file name so it is inside a structure.
      CALL ERR_MARK
      PLOC = ' '

*  Find and obtain the value.
*      CALL DAT_FIND( LOC, 'CATPAC_INPUT', PLOC, STATUS )
*      CALL CMP_GET0C( PLOC, 'NAMEPTR', GLOVAL, STATUS )
      CALL CMP_GET0C( LOC, 'CATPAC_INPUT', GLOVAL, STATUS )

*  Create the appropriate token when there is no object.
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF

*    Report the current global value.
      CALL MSG_OUT( 'GLOBAL1',
     :  'The current INPUT catalogue is     : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*  Close the global file.
*  =====================
      CALL HDS_CLOSE( LOC, STATUS )

*  Closedown.
*  ==========

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GLOBALS_ERR',
     :     'GLOBALS: Error listing the CATPAC global parameters.',
     :     STATUS )
      END IF

      END
