      PROGRAM MAINTASK
*+
*  Name:
*     MAINTASK

*  Purpose:
*     This is the main routine for a UNIX ADAM task for running from ICL

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     The task is invoked in the normal way for running tasks from ICL

*  Description:
*     Start the ERR system (switches on message deferral).
*     Tune the MAXWPL parameter of HDS
*     Tune the SHELL parameter of HDS - Allow environment variable to 
*     override the ADAM default of C-shell (Note the HDS default is Bourne
*     shell which will not translate ~)
*     Determine if the task has been started from ICL (ie if the environment
*     variable ICL_TASK_NAME has been defined) and call DTASK_DTASK
*     or DTASK_DCLTASK as appropriate. ICL_TASK_NAME should be set by
*     ICL to the name by which it wants the task to register with the
*     message system.
*     Close down the ERR system (It is expected that all messages have
*     already been output.
*     Then, if STATUS and environment variable ADAM_EXIT are set, exit
*     with system status set to 1; otherwise exit normally.

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUL-1991 (AJC):
*        Original version.
*    17-MAY-1993 (AJC):
*        Modify for ICL or shell running
*    23-AUG-1993 (AJC):
*        Change name of environment variable to ICL_TASK_NAME
*    16-FEB-1994 (AJC):
*        Tune HDS_SHELL
*    02-DEC-1994 (BKM):
*       Add DTS_SETSIG
*    24-SEP-1996 (AJC):
*       Change HDS shell value from 2 (tcsh) to 1 (csh) as HDS doesn't
*       fall through to lower numbers if shell unavailable
*    14-SEP-1998 (AJC):
*       Indicate succes or failure on exit (subject to ADAM_EXIT set).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSX_ERR'

*  External References:
      EXTERNAL DEVINIT           ! Dummy task initialisation
      EXTERNAL DTASK_APPLIC      ! User code calling routine

*  Local Constants:
      INTEGER PAGE_NUM           ! HDS working page limit
      PARAMETER ( PAGE_NUM = 100 )
      
*  Local Variables:
      INTEGER STATUS             ! Task status
      CHARACTER*80 ICLID         ! If we were started by ICL this local 
                                 ! environment variable will be inherited
*.

*  Initialise Memory Routines
      CALL STARMEM_INIT()

*  Initialise error system
      CALL ERR_START

*  Initialise task status
      STATUS = SAI__OK

*  Increase HDS's maximum working page list size (MAXWPL) from the default
      CALL HDS_TUNE ( 'MAXWPL', PAGE_NUM, STATUS )

*  If environment variable HDS_SHELL is not set, Select C-shell option for
*  HDS file name translation
      CALL EMS_MARK
      CALL PSX_GETENV( 'HDS_SHELL', ICLID, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_ANNUL( STATUS )
         CALL HDS_TUNE( 'SHELL', 1, STATUS )
      ENDIF
      CALL EMS_RLSE
      
*  Attempt to get the ICL environment variable
*  Be prepared for 'normal' situation of failure
      CALL ERR_MARK
      CALL PSX_GETENV( 'ICL_TASK_NAME', ICLID, STATUS )

*  If it's set we are running from ICL
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_RLSE
*  Setup Unix signal handling
         CALL DTASK_SETSIG( .TRUE. )
         CALL DTASK_DTASK ( DEVINIT, DTASK_APPLIC, STATUS )

*  Otherwise we are running from the shell
      ELSE IF ( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE
*  Setup Unix signal handling
         CALL DTASK_SETSIG( .FALSE. )
         CALL DTASK_DCLTASK ( DEVINIT, DTASK_APPLIC, STATUS )

      ENDIF

*   Close error reporting
      CALL ERR_STOP ( STATUS )

*   Exit 
*   If STATUS and environment variable ADAM_EXIT are set,
*   make an error exit. We have no further use for STATUS or error
*   reporting.
      IF ( STATUS .NE. SAI__OK ) THEN
*    We have a bad status
         STATUS = SAI__OK
         CALL ERR_MARK
         CALL PSX_GETENV( 'ADAM_EXIT', ICLID, STATUS )
*      If ADAM_EXIT is set, exit with status 1
         IF ( STATUS .EQ. SAI__OK ) CALL EXIT( 1 )
      ENDIF
          
*   Otherwise exit normally

      END

*+  DEVINIT - Dummy initialisation routine for ADAM tasks.
      SUBROUTINE DEVINIT ( STATUS )
*    Description :
*     This is a dummy initialisation routine for ADAM tasks. It will be
*     used if the programmer does not supply one.
*    Parameters :
*     STATUS=INTEGER
*    Method :
*     Return.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*    History :
*     30.04.1989:  Original (AAOEPP::WFL)
*     09.04.1991:  call SUBPAR_SETCHECK (REVAD::BDK)
*     25.04.1991:  check status on entry (REVAD::BDK)
*     14.05.1991:  Remove NEEDS list checking control (ROE::BMC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*-
      IF ( STATUS .NE. SAI__OK ) RETURN

      END

