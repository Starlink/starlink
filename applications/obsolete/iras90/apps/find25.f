      SUBROUTINE FIND25( CONREQ, DISFIL, ILEVEL,
     : PCONAD, PCONDE, PDENAM,
     : PDISFI, PEDNAM, PINFIL,
     : PMAXLN, PMEN11, PMEN12, PNEXTP, POUTSF,
     : PSCOR1, PSCOR2, PSCOSY,
     : PSLFIL, PSNAME, PSTITL,
     : MAXLEN, MENU, STATUS )
*+
*  Name:
*     FIND25

*  Purpose:
*     Allows the user to input or edit source positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND25(CONREQ, DISFIL, ILEVEL,
*     : PCONAD, PCONDE, PDENAM,
*     : PDISFI, PEDNAM, PINFIL,
*     : PMAXLN, PMEN11, PMEN12, PNEXTP, POUTSF,
*     : PSCOR1, PSCOR2, PSCOSY,
*     : PSLFIL, PSNAME, PSTITL,
*     : MAXLEN, MENU, STATUS )

*  Description:
*
*  Allows the user to input or edit source positions.
*
*  The user is first asked to selet the data which he wishes to use.
*  The select menu offers him the choice of:-
*     Enter a new list of sources, therefore an empty source list is
*     created,
*     Use the current list of sources,
*     Use a list of sources saved to a file.
*     List his sources to the screen display or to a file for printing
*     Change the number of lines displayed on a page
*     Return to the IRASFIND main menu
*     Accept the source list
*
*  When the user has accepted the selected source list the program
*  then moves on to the editing menu which allows thw user
*  to:-
*     Add new sources to the current list of sources, or to a "new"
*     source list.
*     Edit data in current list of sources,
*     Delete from current list of sources,
*     Change the coordinate system in which sources are to be entered.
*     List his sources to the screen display or to a file for printing
*     Change the number of lines displayed on a page
*     Return to the IRASFIND main menu
*     Accept the current list with, or without saving a file containing
*     the source list.
*
*  Please note that entering ! to a parameter request will normally
*  cause the program to redisplay the menu or menu prompt.
*
*  The setting of ILEVEL will have the following actions
*     ILEVEL = 1 - Neither menu details or continuously updated lists of
*                  sources will be displayed. A list of sources can be
*                  obtained by entering L on either menu prompt.
*                  Only the menu prompt will be displayed.
*     ILEVEL = 2 - Continuously updated lists of sources will be
*                  displayed. A list of sources can also be
*                  obtained by entering L on either menu prompt.
*                  Only the menu prompt will be displayed.
*     ILEVEL = 3 - Complete menu details will be displayed, but
*                  continuously updated lists of sources will not be
*                  displayed. A list of sources can be obtained by
*                  entering L on either menu prompt.
*     ILEVEL = 4 - Complete menu details, and continuously updated
*                  lists of sources will be displayed. A list of
*                  sources can also be obtained by entering L on
*                  either menu prompt.

*  Arguments:
*     CONREQ = LOGICAL (Given)
*        Set .TRUE. if added, and edited, sources are to be confirmed
*     DISFIL = CHARACTER * ( * ) (Given)
*        Value of the DISPLAYORFILE parameter
*     ILEVEL = INTEGER (Given)
*        Program interaction level
*     PCONAD = CHARACTER * ( * ) (Given)
*        Parameter CONFIRMADDEDIT for confirmation that added or edited
*        source has correct details
*     PCONDE = CHARACTER * ( * ) (Given)
*        Parameter CONFIRMDELETE for confirmation source displayed is
*        to be deleted
*     PDENAM = CHARACTER * ( * ) (Given)
*        Parameter DELETESOURCENAME for source name to be deleted
*     PDISFI = CHARACTER * ( * ) (Given)
*        Parameter DISPLAYORFILE
*     PEDNAM = CHARACTER * ( * ) (Given)
*        Parameter EDITSOURCENAME for source name to be edited
*     PINFIL = CHARACTER * ( * ) (Given)
*        Parameter INSOURCEFILE1 for name of file containing previously
*        entered source positions
*     PMAXLN = CHARACTER * ( * ) (Given)
*        Parameter PAGELENGTH for the number of lines per page on
*        display
*     PMEN11 = CHARACTER * ( * ) (Given)
*        Parameter MENU11CHOICE for choice from type_of_data_to_edit
*        menu
*     PMEN12 = CHARACTER * ( * ) (Given)
*        Parameter MENU12CHOICE for choice from editing menu
*     PNEXTP = CHARACTER * ( * ) (Given)
*        Parameter NEXTPAGE to trigger next page of display
*     POUTSF = CHARACTER * ( * ) (Given)
*        Parameter OUTSOURCEFILE1 for name of file to contain source
*        positions
*     PSCOR1 = CHARACTER * ( * ) (Given)
*        Parameter SOURCECOORD1 for first coordinate of source position
*     PSCOR2 = CHARACTER * ( * ) (Given)
*        Parameter SOURCECOORD2 for second coordinate of source position
*     PSCOSY = CHARACTER * ( * ) (Given)
*        Parameter SOURCECOORDSYS for coordinate system in which source
*        positions are given
*     PSLFIL = CHARACTER * ( * ) (Given)
*        Parameter SOURCELISTFILE file name to which to file a source
*        list.
*     PSNAME = CHARACTER * ( * ) (Given)
*        Parameter SOURCENAME for source name to be used in EXCRDD file
*        names
*     PSTITL = CHARACTER * ( * ) (Given)
*        Parameter SOURCETITLE for title of source for headings
*     MAXLEN = INTEGER
*        Number of lines per page on display
*     MENU   = CHARACTER * ( 1 ) (Given and Returned)
*        Choice from FINDCRDD main menu

*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm:
*
*  *********************************************************************
*  Select data to use
*  *********************************************************************
*
*     -  Set MENU11 to menu 'M'.
*
*  "Repeat until"
*        MENU11 is Yes 'Y', or MENU11 is new 'N', or
*        MENU11 is return to main menu 'R'
*
*     -  If MENU11 is menu 'M':-
*           Ask do you want to:-
*              Enter new list of sources 'N'
*              Use list of sources currently being used 'C',
*              Use list of sources stored in a file 'F',
*              Display list of sources 'L',
*              Change number of lines per page 'P',
*              Source list is O.K. - Yes 'Y',
*              Return to main menu  'R',
*           Print message that
*              'N','Y',and 'R' will take you out of this menu,
*              'C','F','L', and 'P' will return to menu.
*
*     (If you alter the menu options you should also alter the valid
*      MENU11CHOICE options in the .IFL file)
*
*     -  If MENU11 is enter new list of sources 'N',
*           Clear source data area.
*           Set number of sources to zero.
*           MENU11 is still 'N'.
*
*     -  If MENU11 is use list of sources currently being used 'C',
*           Check number of sources is >0 if not print error message.
*           Set MENU11 to menu 'M'.
*
*     -  If MENU11 is use list of sources stored in a file 'F',
*           CALL FIND38 to read a source data file.
*           Inside FIND38, MENU11 is set to menu 'M'.
*
*     -  If MENU11 is display list of sources 'L',
*           CALL FIND28 to display a paged list of sources.
*           Inside FIND28, MENU11 is set to menu 'M'.
*
*     -  If MENU11 is change number of lines per page 'P',
*           Obtain new value for number of lines per page
*           MENU11 is set to menu 'M'.
*
*     -  If MENU11 is 'Y' accept list of sources,
*           If the source list is not empty the program will continue
*           into the edit section. If the source list is empty the
*           program will display a message and return to the menu.

*
*  Continue the "repeat until"
*
*     -  If the final MENU11 is to return to main menu 'R',
*           Go to the end of the subroutine.
*
*     -  If the user responded ! to the request for MENU11
*           Redisplay the menu or menu prompt.
*
*  *********************************************************************
*  Set up current coordinate system
*  *********************************************************************
*
*     -  If no data has been entered from file (and therefore the
*     number of sources is 0 ) set the coordinate system to
*     Equatorial(1950), otherwise set the cooordinate system to the one
*     used for the last source.
*
*     -  If MENU12 is .NOT. enter new sources 'N'
*           Set MENU12 to menu 'M'.
*
*  *********************************************************************
*  Edit Data
*  *********************************************************************
*
*  "Repeat until"  MENU12 is save file 'Y'
*
*     -  If MENU12 is menu 'M':-
*           Ask do you want to:-
*              Add new sources to the current list of sources 'A',
*              Edit data in current list of sources 'E',
*              Delete from current list of sources 'D',
*              Display current list of sources 'L',
*              Change number of lines per page 'P',
*              Change current coordinate system 'C',
*              Accept source list 'Y',
*              ( The enter new sources MENU12 'N' is carried forward
*              from the previous menu )
*
*     (If you alter the menu options you should also alter the valid
*      MENU12CHOICE options in the .IFL file)
*
*     -  If MENU12 is 'C' change coordinate system, or 'N' enter new
*     sources :-
*           Ask user new coordinate system, using old as default.
*           If this section was entered because change coordinate system
*           was chosen 'C' reset MENU12 to 'M' to go back to the menu.
*
*     -  If MENU12 is 'N' enter new sources, or 'A' add new sources to
*     existing list :-
*           CALL FIND03 to add new sources to the end of the existing
*           list. This routine adds new sources until the user responds
*           with ! to the source name. It uses FIND02 to obtain details
*           of each individual source. At the end of FIND03 the MENU is
*           set back to 'M' so that the user returns to the editing
*           menu.
*
*     -  If MENU12 is to edit file 'E':-
*           CALL FIND13 to edit sources within the existing list. This
*           routine asks the user to give the name of the source to be
*           edited, and will continue to do so until the user responds
*           with a ! to the edit source name. For each name the user
*           enters it will search through the whole of the existing
*           list offering the user each source whose name matches that
*           entered. The user may reject the source under consideration
*           at any time by entering ! to any parameter. It uses FIND02
*           to obtain details of each individual source. At the end of
*           FIND13 the MENU is set back to 'M' so that the user returns
*           to the editing menu.
*
*     -  If MENU12 is to delete sources 'D':-
*           CALL FIND11 to delete sources within the existing list. This
*           routine asks the user to give the name of the source to be
*           deleted, and will continue to do so until the user responds
*           with a ! to the delete source name. For each name the user
*           enters it will search through the whole of the existing
*           list offering the user each source whose name matches that
*           entered. For each match found the details will be printed
*           and the user will be asked to confirm the deletion. Until
*           the user enters ! to the delete source name the deletions
*           will not be consolidated so the user may recover any by
*           rentering the source name. At the end of FIND11 the MENU
*           is set back to 'M' so that the user returns to the editing
*           menu.
*
*     -  If MENU12 is display list of sources 'L',
*           CALL FIND28 to display a paged list of sources.
*           Inside FIND28, MENU11 is set to menu 'M'.
*
*     -  If MENU12 is change number of lines per page 'P',
*           Obtain new value for number of lines per page
*           MENU12 is set to menu 'M'.
*
*  Continue the "repeat until".
*
*    -   If MENU12 is 'Y' accept source list:-
*           CAL FIND40 to prepare an output source list file
*
*    -   The program then returns to the main FINDCRDD menu
*
*  External Routines Used:
*     FINDCRDD:
*        FIND03, FIND11, FIND13, FIND28, FIND38, FIND40
*     ERR:
*        ERR_ANNUL
*     IRA:
*        IRA_GTSCS
*     MSG:
*        MSG_FMTC, MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_CHOICE, PAR_GET0I
*
*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     {original_version_entry}

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
      LOGICAL CONREQ
      CHARACTER * ( * )  DISFIL
      INTEGER ILEVEL
      CHARACTER * ( * )  PCONAD
      CHARACTER * ( * )  PCONDE
      CHARACTER * ( * )  PDENAM
      CHARACTER * ( * )  PDISFI
      CHARACTER * ( * )  PEDNAM
      CHARACTER * ( * )  PINFIL
      CHARACTER * ( * )  PMAXLN
      CHARACTER * ( * )  PMEN11
      CHARACTER * ( * )  PMEN12
      CHARACTER * ( * )  PNEXTP
      CHARACTER * ( * )  POUTSF
      CHARACTER * ( * )  PSCOR1
      CHARACTER * ( * )  PSCOR2
      CHARACTER * ( * )  PSCOSY
      CHARACTER * ( * )  PSLFIL
      CHARACTER * ( * )  PSNAME
      CHARACTER * ( * )  PSTITL

*  Arguments Given and Returned:
      INTEGER MAXLEN
      CHARACTER * ( 1 )  MENU

*  Status:
      INTEGER STATUS             ! Global status

*  External References:

*  Local Variables:
      CHARACTER * ( 3 ) FMAT     ! Format used in change coord sys
      INTEGER I                  ! DO loop control variable
      INTEGER II                 ! DO loop control variable
      CHARACTER * ( 1 ) MENU11   ! Choice from data_to_use menu
      CHARACTER * ( 1 ) MENU12   ! Choice from edit_data menu
      CHARACTER * ( IRA__SZSCS ) SCS ! Value of Coordinate system
      INTEGER SOPOS              ! Pointer to position in source common
                                 ! in which source is to be entered

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  *********************************************************************
*  *********************************************************************
*  Select data to use
*  *********************************************************************
*  *********************************************************************

*  The program will allow the user to input a new list of sources, or
*  modify the current list, or to select a list of sources that he
*  entered previously and stored in a file. It also allows the user to
*  list the sources he has selected, before he decides he has got the
*  right ones and accepts the source list. The selection is controlled
*  by the variable MENU11.

*  Set MENU11 to select_from_menu; M.
      MENU11 = 'M'

 100       CONTINUE            ! Start of 'DO WHILE' loop

*  This is a block of IF statements that carries out the various
*  options from the select_data_to_use menu.

*  *********************************************************************
*  If the option is select_from_menu, M.
*  *********************************************************************
      IF ( (MENU11 .EQ. 'M') .AND. (STATUS .EQ. SAI__OK) ) THEN
*  If ILEVEL is greater than 1 display the select source list menu
         IF ( ILEVEL .GT. 2 ) THEN
            CALL MSG_OUT( ' ', ' ', STATUS )
            CALL MSG_OUT( ' ', ' ', STATUS )
            CALL MSG_OUT( ' ', 'SELECT DATA TO USE MENU', STATUS )
            CALL MSG_OUT( ' ', '  N = Input new list of sources',
     :      STATUS )
            CALL MSG_OUT( ' ', '  C = Modify current list', STATUS )
            CALL MSG_OUT( ' ', '  F = Modify list from file', STATUS )
            CALL MSG_OUT( ' ', '  L = Display sources selected',
     :      STATUS )
            CALL MSG_OUT( ' ', '  P = Change number of lines displayed'
     :      //' on a page',STATUS )
            CALL MSG_OUT( ' ', '  R = Return to main menu', STATUS )
            CALL MSG_OUT( ' ', '  Y = Accept selected source list',
     :      STATUS )
            CALL MSG_OUT( ' ', ' ', STATUS )
            CALL MSG_OUT( ' ', 'N, Y, and R will take you out of this
     : menu', STATUS )
            CALL MSG_OUT( ' ', 'C, F, L, and P will return to this '//
     :      'menu',STATUS )
         END IF

*  Ask user for choice from main menu
         CALL PAR_CHOIC( PMEN11, 'N', 'N,C,F,L,P,R,Y',
     :   .FALSE., MENU11, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
         CALL PAR_CANCL( PMEN11, STATUS )

      END IF

*  *********************************************************************
*  If the option is use_new_list_of_sources.
*  *********************************************************************
      IF ( (MENU11 .EQ. 'N') .AND. (STATUS .EQ. SAI__OK) ) THEN

*  Clear source common - see FICOMN.FOR for a list of variables.
*  - MAXSO is the maximum number of sources and is a parameter in
*  FICOMN.FOR
         DO 300 I = 1, MAXSO
            SONAME( I ) = ' '
            SOTITL( I ) = ' '
            SOCO1( I )  = ' '
            SOCO2( I )  = ' '
            SOCOSY( I ) = ' '
            SORA( I )   = 0.0
            SODEC( I )  = 0.0
            SOINSZ( I ) = 0.0
            SOCRSZ( I ) = 0.0
            SOWAB1( I ) = .FALSE.
            SOWAB2( I ) = .FALSE.
            SOWAB3( I ) = .FALSE.
            SOWAB4( I ) = .FALSE.
            SOBPLI( I ) = 0
            SONOSC( I ) = 0
*         Note  - MAXSS is the maximum number of scans for a source and
*         is a parameter in FICOMN.FOR
            DO 200 II = 1, MAXSS
               SOSCI( I,II) = 0
 200        CONTINUE
            SOMADE(I)= .FALSE.
 300     CONTINUE

*  Set the number of sources in use to zero
         NOFSO = 0

*  The choice MENU11 should remain as N so that the program comes out
*  of the select data to use section.

      END IF

*  *********************************************************************
*  If the option is use_current_list_of_sources.
*  *********************************************************************
      IF ( (MENU11 .EQ. 'C') .AND. (STATUS .EQ. SAI__OK) ) THEN

*  The program checks whether there are any sources currently in source
*  common, by checking number of sources.
         IF ( NOFSO .EQ. 0 ) THEN
            CALL MSG_OUT( ' ',
     :      'WARNING - Current list of sources is empty', STATUS )
         END IF

*  The choice is changed to M so that the user may select from the menu.
         MENU11 = 'M'
      END IF

*  *********************************************************************
*  If the option is use_previously_entered_list_of_sources.
*  *********************************************************************
      IF ( (MENU11 .EQ. 'F') .AND. (STATUS .EQ. SAI__OK) ) THEN
*     -  If OPTION is use list of sources stored in a file (F),
*           Ask user for preexisting file of sources, and wait until a
*           valid source file or ! (PAR__NULL) for menu is given.
*           If a valid source file is given it is read in.
*           Set OPTION to menu(M).

         CALL FIND38( PINFIL, MENU11, STATUS )

      END IF

*  *********************************************************************
*  If the option is list_sources_selected.
*  *********************************************************************
      IF ( (MENU11 .EQ. 'L') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND28( DISFIL, MAXLEN, PDISFI, PNEXTP, PSLFIL,
     :   .FALSE., MENU11, STATUS )

      END IF

*  *********************************************************************
*  If the option is change page length.
*  *********************************************************************
      IF ( (MENU11 .EQ. 'P') .AND. (STATUS .EQ. SAI__OK) ) THEN

*  Obtain the page length for displayed pages
         CALL PAR_GET0I( PMAXLN, MAXLEN, STATUS )

*  Cancel the parameter for next time through
         CALL PAR_CANCL( PMAXLN, STATUS )

*  Set the parameter back to obtain select data to use menu
         MENU11 = 'M'

      END IF

*  *********************************************************************
*  If the option is accept_list_of_sources, or the user enteres a !
*  *********************************************************************
      IF ( (MENU11 .EQ. 'Y') .AND. (STATUS .EQ. SAI__OK) ) THEN

*  If the number of sources is zero
         IF ( NOFSO .EQ. 0 ) THEN
            CALL MSG_OUT( ' ',
     :      'WARNING - Current list of sources is empty', STATUS )
            CALL MSG_OUT( ' ',
     :      'Choose either new (N), or file (F), from the menu',
     :      STATUS )

*  The choice is changed to M so that the user may select from the menu.
             MENU11 = 'M'

         END IF

      END IF

*  *********************************************************************
*  If the option is return_to_main_menu.
*  *********************************************************************
      IF ( (MENU11 .EQ. 'R') .AND. (STATUS .EQ. SAI__OK) ) THEN

*  The program will set the FINDCRDD main menu choice to 'M'
         MENU = 'M'

      END IF

*  If the option chosen by the user was C (), F (), or L (), he may wish
*  to examine the data further before confirming that he has the data
*  he intended to work on. Therfore in the processing for each of these
*  options (above) the program finally sets the choice variable to M so
*  that this IF statement can ensure that the user returns to the
*  select data menu.
      IF ( ( (MENU11 .EQ. 'M') .AND. (STATUS .EQ. SAI__OK) ) .OR.
     :     ( STATUS .EQ. PAR__NULL ) ) THEN

*  If the status is PAR__NULL reset it to SAI__OK and set the menu
*  choice to the implied value of 'M'
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MENU11 = 'M'
         END IF
         GO TO 100
      END IF



*  The program will exit from this subroutine if the option chosen is
*  return_to_main_menu.
      IF ( (MENU11 .EQ. 'R') .OR. (STATUS .NE. SAI__OK) ) RETURN

*  *********************************************************************
*  *********************************************************************
*  Edit data
*  *********************************************************************
*  *********************************************************************

*  The program will allow the user to input a new list of sources,
*  to add, edit, or delete sources from the current list of sources.
*  It also allows the user to list the sources he has selected, before
*  he decides he has got the right ones and accepts the source list.
*  The accepted source list is saved to a file.
*  The selection is controlled by the variable MENU12.

*  If the user wants to input a new list of sources (MENU11='N'), the
*  program sets the edit choice (MENU12) to new 'N', or else the
*  program sets the edit choice (MENU12) to select_from_menu 'M'.
      IF ( MENU11 .EQ. 'N' ) THEN
         MENU12 = 'N'
      ELSE
         MENU12 = 'M'
      END IF

*  *********************************************************************
*  Set current coordinate system
*  *********************************************************************

*  Set coordinate system to the last used outside the main edit loop
      IF ( STATUS .EQ. SAI__OK ) THEN

*  This will be Equatorial (B1950) if there is no current list of
*  sources, or the coordinate system in which the last source was
*  entered if there is a source list.
         IF ( NOFSO .EQ. 0 ) THEN
            SCS = 'EQUATORIAL(B1950.0)'
         ELSE
            SCS = SOCOSY( NOFSO )
         END IF

      END IF

 400       CONTINUE            ! Start of 'DO WHILE' loop

*  This is a block of IF statements that carries out the various
*  options from the edit_data menu.

*  *********************************************************************
*  If the option is select_from_menu, M.
*  *********************************************************************
      IF ( (MENU12 .EQ. 'M') .AND. (STATUS .EQ. SAI__OK) ) THEN

*  If ILEVEL is greater than 1 display the select source list menu
         IF ( ILEVEL .GT. 2 ) THEN
            CALL MSG_OUT( ' ', ' ', STATUS )
            CALL MSG_OUT( ' ', ' ', STATUS )
            CALL MSG_OUT( ' ', 'EDIT SOURCE LIST MENU', STATUS )
            CALL MSG_OUT( ' ', '  A = Add new sources to current list ',
     :      STATUS )
            CALL MSG_OUT( ' ',
     :      '  E = Edit data in current list of sources', STATUS )
            CALL MSG_OUT( ' ',
     :      '  D = Delete sources from current list ', STATUS )
            CALL MSG_OUT( ' ',
     :      '  L = Display current list of sources ', STATUS )
            CALL MSG_OUT( ' ', '  P = Change number of lines displayed'
     :      //' on a page',STATUS )
            CALL MSG_OUT( ' ',
     :      '  C = Change coordinate system in which position is'
     :      //' entered', STATUS )
            CALL MSG_OUT( ' ', '  R = Return to main menu', STATUS )
            CALL MSG_OUT( ' ', '  Y = Accept source list', STATUS )
         END IF

*  Ask user for choice from main menu
         CALL PAR_CHOIC( PMEN12, ' ', 'A,E,D,L,P,C,R,Y',.FALSE., MENU12,
     :   STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
         CALL PAR_CANCL( PMEN12, STATUS )

      END IF

*  *********************************************************************
*  The user is asked which coordinate system he wishes to use if either
*  the option is use_new_list_of_sources, or change_coordinate system
*  *********************************************************************

      IF ( ((MENU12 .EQ. 'N') .OR. (MENU12 .EQ. 'C'))
     : .AND. (STATUS .EQ. SAI__OK)  ) THEN

*  Use IRA_GTSCS to get the users choice of coordinate system. The coord
*  system is passed back in the variable SCS.
         CALL IRA_GTSCS( PSCOSY, .TRUE., SCS, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
         CALL PAR_CANCL( PSCOSY, STATUS )

*  If the status is null ! then reset it to SAI__OK and set MENU12 to
*  'M' to take the program back to the edit menu
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MENU12 = 'M'
         END IF

*  If the status is abort !!
         IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  Display message to confirm chosen coordinate system. Since we can't
*  use the size of the SCS character string directly (IRA__SZSCS), we
*  have to use an internal write to set it up in a format variable
         WRITE ( FMAT,9999 ) IRA__SZSCS
 9999    FORMAT ( 'A', I2 )
         CALL MSG_FMTC( 'C1', FMAT, SCS )
         CALL MSG_OUT( ' ', 'Current coordinate system is ^C1', STATUS )

*  If the the option is change_coordinate_system then this is all the
*  action required so the choice is set to select_from_data_edit_menu.
         IF ( (MENU12 .EQ. 'C') .AND. (STATUS .EQ. SAI__OK) ) THEN
            MENU12 = 'M'
         END IF

      END IF

*  *********************************************************************
*  If the option is use_new_list_of_sources,  or add new sources
*  the user is requested for details and they are added to the end of
*  the source list, which may be empty in the case of a new list.
*  *********************************************************************
      IF ( ((MENU12 .EQ. 'N') .OR. (MENU12 .EQ. 'A'))
     : .AND. (STATUS .EQ. SAI__OK)  ) THEN

         CALL FIND03( CONREQ, ILEVEL, MAXLEN, PCONAD, PSCOR1, PSCOR2,
     :     PSCOSY, PSNAME, PSTITL, SCS, MENU12, SOPOS, STATUS )

      END IF

*  *********************************************************************
*  If the option is edit existing list of sources
*  *********************************************************************
      IF ( (MENU12 .EQ. 'E') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND13( CONREQ, ILEVEL, MAXLEN, PCONAD, PEDNAM, PSCOR1,
     :              PSCOR2, PSCOSY, PSNAME, PSTITL, SCS, MENU12, SOPOS,
     :              STATUS )

      END IF

*  *********************************************************************
*  If the option is delete sources from current list
*  *********************************************************************
      IF ( (MENU12 .EQ. 'D') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND11( ILEVEL, MAXLEN, PCONDE, PDENAM, MENU12, SOPOS,
     :   STATUS )

      END IF

*  *********************************************************************
*  If the option is list sources
*  *********************************************************************
      IF ( (MENU12 .EQ. 'L') .AND. (STATUS .EQ. SAI__OK) ) THEN

         CALL FIND28( DISFIL, MAXLEN, PDISFI, PNEXTP, PSLFIL,
     :   .FALSE., MENU12, STATUS )

      END IF

*  *********************************************************************
*  If the option is change page length.
*  *********************************************************************
      IF ( (MENU12 .EQ. 'P') .AND. (STATUS .EQ. SAI__OK) ) THEN

*  Obtain the page length for displayed pages
         CALL PAR_GET0I( PMAXLN, MAXLEN, STATUS )

*  Cancel the parameter for next time through
         CALL PAR_CANCL( PMAXLN, STATUS )

*  Set the parameter back to obtain edit source list menu
         MENU12 = 'M'

      END IF

*  *********************************************************************
*  If the option is accept source list, or the user enters a !
*  *********************************************************************
      IF ( (MENU12 .EQ. 'Y') .AND. (STATUS .EQ. SAI__OK) ) THEN

*  If the number of sources is zero
         IF ( NOFSO .EQ. 0 ) THEN
            CALL MSG_OUT( ' ',
     :      'WARNING - Current list of sources is empty', STATUS )

*  The choice is changed to M so that the user may select from the menu.
             MENU12 = 'M'

*  If there is one or more sources
        ELSE

*  Call routine to store source data
            CALL FIND40( POUTSF, STATUS )

*  Set the FINDCRDD main menu choice to 'M'
            MENU = 'M'
         END IF

      END IF

*  *********************************************************************
*  If the option is return_to_main_menu.
*  *********************************************************************
      IF ( (MENU12 .EQ. 'R') .AND. (STATUS .EQ. SAI__OK) ) THEN

*  The program will set the FINDCRDD main menu choice to 'M'
         MENU = 'M'

      END IF

*  *********************************************************************
*  Return to the edit sources menu if required
*  *********************************************************************

      IF ( ( (MENU12 .EQ. 'M') .AND. (STATUS .EQ. SAI__OK) ) .OR.
     :     ( STATUS .EQ. PAR__NULL ) ) THEN

*  If the status is PAR__NULL reset it to SAI__OK and set the menu
*  choice to the implied value of 'M'
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MENU12 = 'M'
         END IF

         GO TO 400
      END IF
*  *********************************************************************
*  End of the set of IF's which do the edit sources
*  *********************************************************************
      END
