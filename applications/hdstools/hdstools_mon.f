      SUBROUTINE HDSTOOLS_MON( STATUS )
*+
* Name:
*    HDSTOOLS_MON

*  Purpose:
*     Top-level ADAM monolith routine for the HDSTOOLS package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDSTOOLS_MON( STATUS )

*  Description:
*     This routine obtains the name of the current action and calls the
*     appropriate routine to perform the specified operation. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) {year} Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     BC:  Brad Cavanagh (Joint Astronomy Centre, Hawaii)
*     {enter_new_authors_here}

*  History:
*     18-SEP-2001 (AJC):
*        Original version.
*     02-FEB-2007 (BC):
*        Add GRP and HDS locator tracking code.
*     04-FEB-2007 (TIMJ):
*        GRP not used by HDSTOOLS so don't check it.
*     {date} ({author_identifier}):
*        {changes}
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'GRP_PAR'         ! GRP definitions
      INCLUDE 'DAT_PAR'         ! DAT definitions
      INCLUDE 'PAR_PAR'         ! PAR_ public constants

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN             ! Length of string

*  Local Variables:
      CHARACTER * ( PAR__SZNAM ) NAME ! Action name
      CHARACTER FILTER*( 60 )     ! HDS_INFOI filter string
      INTEGER IPOSN               ! Position in string
      INTEGER LSTAT               ! Local status
      INTEGER NLOC0               ! Number of active locators on entry
      INTEGER NLOC1               ! Number of active locators on exit

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Need to construct a string to filter out parameter locators
*  from the list of active locators. Need monolith and taskname
*  excluded. Also seem to need HDS_SCRATCH excluded.
      FILTER = '!HDSTOOLS_MON,!HDS_SCRATCH,!GLOBAL,!'
      IPOSN = CHR_LEN( FILTER )
      CALL CHR_APPND(NAME, FILTER, IPOSN)

*  Note the current number of active locators (excluding parameter
*  system
      CALL HDS_INFOI( DAT__NOLOC, 'LOCATORS', FILTER,
     :     NLOC0, STATUS )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...


      IF ( NAME .EQ. 'HCOPY' ) THEN
         CALL HCOPY( STATUS )

      ELSE IF ( NAME .EQ. 'HCREATE' ) THEN
         CALL HCREATE( STATUS )

      ELSE IF ( NAME .EQ. 'HDELETE' ) THEN
         CALL HDELETE( STATUS )

      ELSE IF ( NAME .EQ. 'HDIR' ) THEN
         CALL HDIR( STATUS )

      ELSE IF ( NAME .EQ. 'HDISPLAY' ) THEN
         CALL HDISPLAY( STATUS )

      ELSE IF ( NAME .EQ. 'HFILL' ) THEN
         CALL HFILL( STATUS )

      ELSE IF ( NAME .EQ. 'HGET' ) THEN
         CALL HGET( STATUS )

      ELSE IF ( NAME .EQ. 'HHELP' ) THEN
         CALL HHELP( STATUS )

      ELSE IF ( NAME .EQ. 'HMODIFY' ) THEN
         CALL HMODIFY( STATUS )

      ELSE IF ( NAME .EQ. 'HREAD' ) THEN
         CALL HREAD( STATUS )

      ELSE IF ( NAME .EQ. 'HRENAME' ) THEN
         CALL HRENAME( STATUS )

      ELSE IF ( NAME .EQ. 'HRESET' ) THEN
         CALL HRESET( STATUS )

      ELSE IF ( NAME .EQ. 'HRESHAPE' ) THEN
         CALL HRESHAPE( STATUS )

      ELSE IF ( NAME .EQ. 'HRETYPE' ) THEN
         CALL HRETYPE( STATUS )

      ELSE IF ( NAME .EQ. 'HTAB' ) THEN
         CALL HTAB( STATUS )

      ELSE IF ( NAME .EQ. 'HWRITE' ) THEN
         CALL HWRITE( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'HDT_MON_ERR',
     :        'HDT_MON: The action name ''^NAME'' is ' //
     :        'not recognised by the {routine_name} monolith.',
     :        STATUS )
      END IF

*  Note the current number of active locators. Do this in a new
*  error reporting context so that we get the correct value even if an
*  error has occurred.
      CALL ERR_MARK
      LSTAT = SAI__OK
      CALL HDS_INFOI( DAT__NOLOC, 'LOCATORS', FILTER,
     :     NLOC1, LSTAT )

*  If there are more active locators now than there were on entry, there
*  must be a problem (HDS locators are not being freed somewhere). So
*  report it.
      IF( LSTAT .EQ. SAI__OK .AND. NLOC1 .GT. NLOC0 ) THEN
         CALL MSG_BLANK( LSTAT )
         CALL MSG_SETC( 'NAME', NAME )
         CALL MSG_SETI( 'NLOC0', NLOC0 )
         CALL MSG_SETI( 'NLOC1', NLOC1 )
         CALL MSG_OUT( 'KAPPA_NLOC', 'WARNING: The number of active '//
     :             'HDS locators increased from ^NLOC0 to ^NLOC1 '//
     :             'during execution of ^NAME (HDSTOOLS programming '//
     :             'error).', LSTAT )
         CALL MSG_BLANK( LSTAT )
      END IF
      IF (LSTAT .NE. SAI__OK) CALL ERR_ANNUL( LSTAT )

      CALL ERR_RLSE

      END
