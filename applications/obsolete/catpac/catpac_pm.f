      SUBROUTINE CATPAC_PM( STATUS )
*+
*  Name:
*     CATPAC_PM

*  Purpose:
*     Top-level CATPAC subroutine for A-task pseudo-monolith on Unix.

*  Language:
*     UNIX Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CATPAC_PM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This is the top-level pseudo-monolith subroutine for the CATPAC
*     suite of A-tasks.  It is a pseudo-monolith as the command does
*     not come from the ADAM interface, but is taken from the UNIX
*     command line.  Each CATPAC command is an alias to a softlink that
*     points to this monolith.  The chosen commands is obtained from
*     the UNIX Fortran run-time library routine GETARG.  Given the
*     command,the requested A-task is called after a successful matching
*     of the input string with a valid task name.
*
*     There are different versions for each platform, because certain
*     libraries are not available on all, and so some applications are
*     not implemented on some platforms.

*  Notes:
*     This is the DECstation version.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     The input string NAME is tested against all the valid A-task
*     names after having been forced to upper-case. If a valid test
*     is made, the relevant A-task is called. If not, an error message
*     is output to the environment.

*  Implementation Deficiencies:
*     The input string has to be forced to upper-case.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     ARW: Alan R. Wood (RAL)
*     {enter_new_authors_here}

*  History:
*     1993 Nov 19 (ARW):
*        Original version.
*        Based upon the KAPPA version by MJC
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE              ! no implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'

*  Status:
      INTEGER  STATUS

*  External References:
      INTEGER CHR_LEN             ! Length of a character string less
                                  ! any trailing blanks

*  Local Variables:
      CHARACTER
     :  COMAND * ( 132 ),         ! Command
     :  NAME * ( 15 )             ! Task name from the command

      INTEGER
     :  NC,                       ! Number of characters in command
     :  NCOFF,                    ! Number of characters offset to
                                  ! search for the path sladh in the
                                  ! command
     :  NCS                       ! Position of a slash within a part of
                                  ! the command

      LOGICAL                     ! True if:
     :  PATH                      ! There is a slash in the part of the
                                  ! command being examined

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the command from a Fortran RTL routine.  Zero is the command.
*      CALL GETARG( 0, COMAND )

       CALL TASK_GET_NAME(COMAND, STATUS)

*  Remove any path present in the command to derive the command name by
*  looking for the last slash.
      PATH = .TRUE.
      NCS = 0
      NCOFF = 0
      DO WHILE ( PATH )
         NCS = INDEX( COMAND( NCOFF+1: ), '/' )
         PATH = NCS .NE. 0
         NCOFF = NCOFF + NCS
      END DO

*  Extract the name less trailing blanks.
      NC = CHR_LEN( COMAND )
      NAME = COMAND( NCOFF + 1:NC )

*  Force the input string to upper-case before testing
      CALL CHR_UCASE( NAME )

*  Identify and execute the task.
*  ==============================
*
*  Check the string against valid A-task names---if matched then call
*  the relevant A-task

      IF ( NAME .EQ. 'ADDPARAM' ) THEN
         CALL ADDPARAM( STATUS )

      ELSE IF ( NAME .EQ. 'ASCIICAT' ) THEN
         CALL ASCIICAT( STATUS )

      ELSE IF ( NAME .EQ. 'ASCIITO' ) THEN
         CALL ASCIITO( STATUS )

      ELSE IF ( NAME .EQ. 'CATHELP' ) THEN
         CALL CATHELP( STATUS )

      ELSE IF ( NAME .EQ. 'CATJOIN' ) THEN
         CALL CATJOIN( STATUS )

      ELSE IF ( NAME .EQ. 'CATRENAME' ) THEN
         CALL CATRENAME( STATUS )

      ELSE IF ( NAME .EQ. 'CATREPORT' ) THEN
         CALL CATREPORT( STATUS )

      ELSE IF ( NAME .EQ. 'CATSEARCH' ) THEN
         CALL CATSEARCH( STATUS )

      ELSE IF ( NAME .EQ. 'CATSORT' ) THEN
         CALL CATSORT( STATUS )

      ELSE IF ( NAME .EQ. 'COPYCAT' ) THEN
         CALL COPYCAT( STATUS )

      ELSE IF ( NAME .EQ. 'DELCAT' ) THEN
         CALL DELCAT( STATUS )


      ELSE IF ( NAME .EQ. 'DELFIELD' ) THEN
         CALL DELFIELD( STATUS )

      ELSE IF ( NAME .EQ. 'DELPARAM' ) THEN
         CALL DELPARAM( STATUS )

      ELSE IF ( NAME .EQ. 'ENTRIES' ) THEN
         CALL ENTRIES( STATUS )

      ELSE IF ( NAME .EQ. 'FIELDINFO' ) THEN
         CALL FIELDINFO( STATUS )

      ELSE IF ( NAME .EQ. 'FIELDS' ) THEN
         CALL FIELDS( STATUS )

      ELSE IF ( NAME .EQ. 'FK425' ) THEN
         CALL FK425( STATUS )

      ELSE IF ( NAME .EQ. 'FK45Z' ) THEN
         CALL FK45Z( STATUS )

      ELSE IF ( NAME .EQ. 'FK524' ) THEN
         CALL FK524( STATUS )

      ELSE IF ( NAME .EQ. 'FK54Z' ) THEN
         CALL FK54Z( STATUS )

      ELSE IF ( NAME .EQ. 'GLOBALS' ) THEN
         CALL GLOBALS( STATUS )

      ELSE IF ( NAME .EQ. 'PARAMINFO' ) THEN
         CALL PARAMINFO( STATUS )

      ELSE IF ( NAME .EQ. 'PARAMS' ) THEN
         CALL PARAMS( STATUS )

      ELSE IF ( NAME .EQ. 'PROPERM' ) THEN
         CALL PROPERM( STATUS )

      ELSE IF ( NAME .EQ. 'SAMPLE' ) THEN
         CALL SAMPLE( STATUS )

      ELSE IF ( NAME .EQ. 'UPFIELD' ) THEN
         CALL UPFIELD( STATUS )

      ELSE IF ( NAME .EQ. 'UPPARAM' ) THEN
         CALL UPPARAM( STATUS )

      ELSE

*  No such option exists.

         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'CATPAC_ERR',
     :                 'CATPAC: The NAME name ''^ACTION'' is ' //
     :                 'not recognised by the CATPAC monolith.',
     :                 STATUS )
      END IF

      END
