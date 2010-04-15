       SUBROUTINE FIND26( DISFIL, ILEVEL, PAUNAM, PBANDS,
     : PCROSS, PDISFI, PINSCA,
     : PMAXLN, PMENU2, PNEXTP,
     : POUTF2, PRETSW, PSLFIL,
     : MAXLEN, MENU, STATUS )
*+
*  Name:
*     FIND26

*  Purpose:
*  Allows the user to input or modify the size of the region, and
*  wavebands, he requires for each source.


*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL  FIND26( DISFIL, ILEVEL, PAUNAM, PBANDS,
*     : PCROSS, PDISFI, PINSCA,
*     : PMAXLN, PMENU2, PNEXTP,
*     : POUTF2, PRETSW, PSLFIL,
*     : MAXLEN, MENU, STATUS )

*  Description:
*
*  Allows the user to input or modify the size of the region, and
*  wavebands, he requires for each source.
*
*  The program allows the user the option to:-
*     Enter a region size for all sources,
*     Enter a set of wavebands for all sources,
*     Enter region size and/or waveband details for sources
*     individually,
*     Save file and continue processing.
*
*  After all editing is done the program checks that a region size and
*  waveband requirements have been entered for all sources.
*
*  Arguments:
*     DISFIL = CHARACTER * ( * ) (Given)
*        Value of the DISPLAYORFILE parameter
*     ILEVEL = INTEGER (Given)
*        Program interaction level
*     PAUNAM = CHARACTER * ( * ) (Given)
*        Parameter AUGSOURCENAME for source name to be augmented with
*        region size and wavebands.
*     PBANDS = CHARACTER * ( * ) (Given)
*        Parameter BANDSREQ for bands required entered as 12,25,60,100
*     PCROSS = CHARACTER * ( * ) (Given)
*        Parameter CROSSCAN size of req. region in cross scan direction
*        in arc minutes
*     PDISFI = CHARACTER * ( * ) (Given)
*        Parameter DISPLAYORFILE
*     PINSCA = CHARACTER * ( * ) (Given)
*        Parameter INSCAN size of req. region in in scan direction
*        in arc minutes
*     PMAXLN = CHARACTER * ( * ) (Given)
*        Parameter PAGELENGTH for the number of lines per page on
*        display
*     PMENU2 = CHARACTER * ( * ) (Given)
*        Parameter MENU82CHOICE for choice from add size and wavebands
*        menu
*     PNEXTP = CHARACTER * ( * ) (Given)
*        Parameter NEXTPAGE to trigger next page of display
*     POUTF2 = CHARACTER * ( * ) (Given)
*        Parameter OUTSOURCEFILE8 for name of file to contain source
*        positions
*     PRETSW = CHARACTER * ( * ) (Given)
*        Parameter RETURNSWMENU for user reqires program to return to
*        size and wavebands menu if source details are missing.
*     PSLFIL = CHARACTER * ( * ) (Given)
*        Parameter SOURCELISTFILE file name to which to file a source
*        list.
*     MENU   = CHARACTER * ( 1 ) (Given and Returned)
*        Choice from FINDCRDD main menu
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm:
*
*  *********************************************************************
*  Add source region size and wavebands to all sources
*  *********************************************************************
*
*
*     -  Set MENU2 to menu 'M'.
*
*  Repeat until MENU2 is accept source list with size/wavebands 'Y'
*            or MENU2 is return to main menu 'R':-
*
*     -  If MENU2 is menu 'M':-
*           Ask do you want to:-
*              Enter a region size for all sources 'Z',
*              Enter a set of wavebands for all sources 'W',
*              Enter region size and/or waveband details
*                for sources individually 'I',
*              Display current list of sources 'L',
*              Change number of lines displayed 'P',
*              Return to main menu 'R',
*              Accept sources and continue processing 'Y',
*
*     (If you alter the menu options you should also alter the valid
*      MENU2CHOICE options in the .IFL file)
*
*     -  If MENU2 is region size for all sources 'Z'
*           CALL FIND04 to
*           Ask for INSCAN and CROSSCAN sizes.
*           Enter these values in all sources.
*           Set MENU2 to menu 'M'.
*
*     -  If MENU2 is set of wavebands for all sources 'W'
*           CALL FIND05 to
*           Ask for bands required.
*           Enter these values in all sources.
*           Set MENU2 to menu 'M'.
*
*     -  If MENU2 is Enter region size and/or waveband details
*        for sources individually 'I',
*           CALL FIND44 to
*           Repeatedly ask the user for source names to be individually
*           modified until he responds with a !
*           Search for all source details with matching source names
*           For each match found display the source details and ask the
*           user for cross scan and in scan size and wavebands.
*           Inside FIND44, MENU2 is set to menu 'M'.
*
*     -  If MENU2 is display list of sources 'L',
*           CALL FIND28 to display a paged list of sources.
*           Inside FIND28, MENU2 is set to menu 'M'.
*
*     -  If MENU2 is change page length 'P',
*           CALL PAR_GET0I to get new pagelength
*           MENU2 is set to 'M'
*
*  Continue the repeat until
*
*     -  If MENU2 is accept source list and save file 'Y',
*           CALL FIND07 to
*           Check that all sources have a non zero in scan size,
*           Check that all sources have at least one required waveband
*           If there are any sources for which this is not the case:-
*              They are displayed
*              The user has the option of continuing or returning to
*              the set size and wavebands menu. (If he continues these
*              sources will be ommitted from subsequent processing)
*           If the user is not returning to the size and wavebands menu
*           CALL FIND40 to save the source details including size and
*           wavebands to a file.
*
*  External Routines Used:
*     FINDCRDD:
*        FIND07, FIND04, FIND05, FIND28, FIND38, FIND40, FIND44
*     ERR:
*        ERR_ANNUL
*     MSG:
*        MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_CHOIC, PAR_GET0I

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     17-SEP-1991 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      CHARACTER * ( * )  DISFIL
      INTEGER ILEVEL
      CHARACTER * ( * )  PAUNAM
      CHARACTER * ( * )  PBANDS
      CHARACTER * ( * )  PCROSS
      CHARACTER * ( * )  PDISFI
      CHARACTER * ( * )  PINSCA
      CHARACTER * ( * )  PMAXLN
      CHARACTER * ( * )  PMENU2
      CHARACTER * ( * )  PNEXTP
      CHARACTER * ( * )  POUTF2
      CHARACTER * ( * )  PRETSW
      CHARACTER * ( * )  PSLFIL

*  Arguments Given and Returned:
      INTEGER MAXLEN
      CHARACTER * ( 1 )  MENU

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 1 ) MENU2    ! Choice from add size and w'band menu
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  *********************************************************************
*  *********************************************************************
*  Add region size and wavebands to data
*  *********************************************************************
*  *********************************************************************
*  Set up size and wavebands menu variable to 'M' in order to obtain
*  menu as first action
      MENU2 = 'M'

*  The program will allow the user to add region size and wavebands for
*  all the sources in the source list or to input them for sources
*  individulaly. The accepted source list is saved to a file.
*  The selection is controlled by the variable MENU2.


 100       CONTINUE            ! Start of 'DO WHILE' loop

*  This is a block of IF statements that carries out the various
*  options from the add region size and wavebands menu.

*  *********************************************************************
*  If the option is select_from_menu, M.
*  *********************************************************************
      IF ( (MENU2 .EQ. 'M') .AND. (STATUS .EQ. SAI__OK) ) THEN

*  If ILEVEL is greater than 2 display the select source list menu
         IF ( ILEVEL .GT. 2 ) THEN
            CALL MSG_OUT( ' ', ' ', STATUS )
            CALL MSG_OUT( ' ', ' ', STATUS )
            CALL MSG_OUT( ' ', 'ADD REGION SIZE AND WAVEBANDS MENU',
     :      STATUS )
            CALL MSG_OUT( ' ',
     :      '  Z = Enter region size for all sources', STATUS )
            CALL MSG_OUT( ' ',
     :      '  W = Enter wavebands required for all sources', STATUS )
            CALL MSG_OUT( ' ',
     :      '  I = Enter region size and wavebands for individual'
     :      //' sources', STATUS )
            CALL MSG_OUT( ' ',
     :      '  L = Display current list of sources ', STATUS )
            CALL MSG_OUT( ' ', '  P = Change number of lines displayed'
     :      //' on a page',STATUS )
            CALL MSG_OUT( ' ', '  R = Return to main menu', STATUS )
            CALL MSG_OUT( ' ', '  Y = Accept source list', STATUS )
         END IF

*  Ask user for choice from main menu
         CALL PAR_CHOIC( PMENU2, ' ', 'Z,W,I,L,P,R,Y',.FALSE., MENU2,
     :   STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
         CALL PAR_CANCL( PMENU2, STATUS )

      END IF

*  *********************************************************************
*  If the user chooses Z = Enter region size for all sources
*  *********************************************************************

      IF ( (MENU2 .EQ. 'Z') .AND. (STATUS .EQ. SAI__OK)  ) THEN

         CALL FIND04( PCROSS, PINSCA, MENU2, STATUS )

      END IF

*  *********************************************************************
*  If the user chooses W = Enter wavebands required for all sources
*  *********************************************************************

      IF ( (MENU2 .EQ. 'W') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND05( PBANDS, MENU2, STATUS )

      END IF

*  *********************************************************************
*  If the user chooses I = Enter region size and wavebands for
*  individual sources
*  *********************************************************************

      IF ( (MENU2 .EQ. 'I') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND44( MAXLEN, PAUNAM, PBANDS, PCROSS, PINSCA, MENU2,
     :   STATUS )

      END IF

*  *********************************************************************
*  If the user chooses to list sources 'L'
*  *********************************************************************
      IF ( (MENU2 .EQ. 'L') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND28( DISFIL, MAXLEN, PDISFI, PNEXTP, PSLFIL,
     :   .TRUE., MENU2, STATUS )
      END IF

*  *********************************************************************
*  If the option is change page length.
*  *********************************************************************
      IF ( (MENU2 .EQ. 'P') .AND. (STATUS .EQ. SAI__OK) ) THEN

*  Obtain the page length for displayed pages
         CALL PAR_GET0I( PMAXLN, MAXLEN, STATUS )

*  Cancel the parameter for next time through
         CALL PAR_CANCL( PMAXLN, STATUS )

*  Set the parameter back to obtain select data to use menu
         MENU2 = 'M'

      END IF

*  *********************************************************************
*  If the option is return to main menu
*  *********************************************************************
      IF ( (MENU2 .EQ. 'R') .AND. (STATUS .EQ. SAI__OK)  ) THEN

*  The program sets the FINDCRDD main menu choice to 'M'
         MENU = 'M'

      END IF



*  *********************************************************************
*  If the option is accept source list
*  *********************************************************************
      IF ( (MENU2 .EQ. 'Y') .AND. (STATUS .EQ. SAI__OK)  ) THEN

*  If the number of sources is zero
         IF ( NOFSO .EQ. 0 ) THEN
            CALL MSG_OUT( ' ',
     :      'WARNING - Current list of sources is empty', STATUS )

*  The choice is changed to M so that the user may select from the menu.
            MENU2 = 'M'

*  If there is one or more sources
         ELSE

*  Call routine to check that there are region size values and at least
*  one waveband required for each source
            CALL FIND07( MAXLEN, PRETSW, MENU2, STATUS )

*  If MENU2 is set to 'M' then the data is incomplete and the routine
*  should not store the source data
            IF ( MENU2 .NE. 'M' ) THEN

*  Call routine to store source data
               CALL FIND40( POUTF2, STATUS )

            END IF

         END IF

      END IF

*  *********************************************************************
*  Return to the edit sources menu if required, or the user enters a !
*  *********************************************************************

      IF ( ( (MENU2 .EQ. 'M') .AND. (STATUS .EQ. SAI__OK) ) .OR.
     :     ( STATUS .EQ. PAR__NULL ) ) THEN

*  If the status is PAR__NULL reset it to SAI__OK and set the menu
*  choice to the implied value of 'M'
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MENU2 = 'M'
         END IF

         GO TO 100
      END IF

*  *********************************************************************
*  End of the set of IF's which add size and wavebands to sources
*  *********************************************************************

      END
