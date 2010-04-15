      SUBROUTINE CHT_SSET( STATUS )
*+
*  Name:
*     CHT_SSET

*  Purpose:
*     Set CHART parameters used for searching catalogues

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_SSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Prompt the user for all of the CHART parameters that are used when
*     searching a catalogue.

*  Usage:
*     SSET SAREA ...

*  [ADAM_parameters]
*  [examples]

*  Notes:
*     This routine is a concatenation of the original SSET, CSET and
*     FSET.

*  Algorithm:
*     For each CHART parameter, get the value and store it.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     UNKNOWN: Someone (Somewhere)
*     KFH: Ken Hartley (RGO)
*     MJV: Don't know (RGO)
*     JVC: J. Carey (RGO)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     Don't know when (UNKNOWN):
*        Original version.
*     10-JAN-1983 (KFH):
*        Modified by KFH.
*     30-JUN-1983 (MJV):
*        Add code to skip parameters that are unavailable in
*        Non-Stellar Object Mode.
*     10-MAY-1984 (JVC):
*        Add code to check for provate catalogues.
*     9-DEC-1991: PMA
*       Changed calls to CTOI to CHR_CTOI
*       Changed calls to CTOR to CHR_CTOR
*     10-DEC-1991: (PMA)
*       Changed from main program to subroutine to be an ADAM task
*     13-SEP-1992 (PMA):
*        Tidy up prologue.
*        Convert to use proper ADAM routines.
*     26-FEB-1993 (PMA):
*        Change the name of the routine to CHR_SSET.
*     3-MAR-1993 (AJJB):
*        STATUS argument added to GETDEFLT call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     22-MAR-1993 (AJJB):
*        Commented out declaration of unused local variable, to keep
*        Sun compiler quiet.
*     23-APR-1993 (AJJB):
*        Removed conversions of FIELDS and INPUT parameters to
*        uppercase when they get read in, as they are names of files,
*        and since Unix is case sensitive we don't really want the case
*        changing !
*     7-MAY-1993 (AJJB):
*        Removed bit which strips file type extension off value for
*        FIELDS parameter when it's read in.
*     13-MAY-1993 (AJJB):
*        Added an IF which detects if FIELDS is being set to 'terminal',
*        which is a standard value and must be uppercase, and converts
*        it to
*        uppercase, in order not to confuse other programs which use it.
*     3-JUN-1993 (AJJB):
*        Converts the value of the CATALOGUES parameter to uppercase if
*        necessary, after it's read in, for cht_search to be able to
*        make sense of it.
*     7-JUN-1993 (AJJB):
*        Commented out unused local variable "L".
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

*  Local Variables:
      CHARACTER * ( 70 ) PARAMS( 25 ) ! The CHART parameters
      CHARACTER * ( 50 ) VALUE   ! [local_variable_description]
      CHARACTER * ( 50 ) FIELD   ! [local_variable_description]
*     CHARACTER * ( 50 ) TEMP    ! [local_variable_description]
      CHARACTER * ( 1 ) FIRSTCH  ! [local_variable_description]
      CHARACTER * ( 1 ) MODE     ! [local_variable_description]
      INTEGER NVAL               ! Number of values
      INTEGER NPOS               ! [local_variable_description]
      REAL DATA                  ! [local_variable_description]
      REAL DATE                  ! [local_variable_description]
      INTEGER NUM                ! [local_variable_description]
      REAL BRT                   ! [local_variable_description]
*     INTEGER L                  ! Length of filename
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Send a message to the user.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', '             Define new SEARCH parameters',
     :   STATUS )
      CALL MSG_BLANK( STATUS )

*  Read the full set of parameters and get the default for the first
*  one.
      CALL GETPARAMS( PARAMS, NVAL, STATUS )
      CALL GETDEFLT( PARAMS, NVAL, 'LABEL', VALUE, NPOS, STATUS )

*  If this parameter is found then go ahead and ask for a new value.
*  (This basic process is repeated ad nauseam.)
      IF ( NPOS .GT. 0 ) THEN

*  Ask for a new value.
         CALL PAR_DEF0C( 'LABEL', VALUE, STATUS )
         CALL PAR_GET0C( 'LABEL', VALUE, STATUS )

*  If a valid response was found then store it.
         IF ( STATUS .EQ. SAI__OK ) THEN
             PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  Repeat the same process for the size of the search area.
      CALL GETDEFLT( PARAMS, NVAL, 'SAREA', VALUE, NPOS, STATUS )
      IF ( NPOS .GT. 0 ) THEN
         CALL PAR_DEF0C( 'SAREA', VALUE, STATUS )
  110    CONTINUE
         CALL PAR_GET0C( 'SAREA', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Convert the characters into a real number and test it against the
*  valid range. (In this case it should be positive.)
            CALL CHR_CTOR( VALUE, DATA, STATUS )
            IF ( STATUS .NE. SAI__OK .OR. DATA .LE. 0.0 ) THEN
               CALL ERR_REP('SEARCH_NEG',
     :            'Must be a positive real number', STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( 'SAREA', STATUS )
               GO TO 110
            END IF
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  Repeat the process for the equinox.
      CALL GETDEFLT( PARAMS, NVAL, 'EQUINOX', VALUE, NPOS, STATUS )
      IF ( NPOS .GT. 0 ) THEN
         CALL PAR_DEF0C( 'EQUINOX', VALUE, STATUS )
  120    CONTINUE
         CALL PAR_GET0C( 'EQUINOX', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )

*  Check for an entry of TODAY
         IF ( VALUE( 1:3 ) .EQ. 'TOD' ) THEN
            CALL TODAY( DATE , STATUS )
            WRITE ( VALUE, '( F10.5 )' ) DATE
         END IF
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_CTOR( VALUE, DATA, STATUS )
            IF ( STATUS .NE. SAI__OK .OR. DATA .LE. 0.0 ) THEN
               CALL ERR_REP( 'SEARCH_BC', 'Must be A.D.!', STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( 'EQUINOX', STATUS )
               GO TO 120
            END IF
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  Repeat the process for the EPOCH, though strictly speaking this is
*  only relevant if the ASTROM mode was selected.
      CALL GETDEFLT( PARAMS, NVAL, 'EPOCH', VALUE, NPOS, STATUS )
      IF ( NPOS .GT. 0 ) THEN
         CALL PAR_DEF0C( 'EPOCH', VALUE, STATUS )
  130    CONTINUE
         CALL PAR_GET0C( 'EPOCH', VALUE, STATUS )
         CALL CHR_UCASE( VALUE )
         IF ( VALUE( 1:3 ) .EQ. 'TOD' ) THEN
            CALL TODAY( DATE , STATUS )
            WRITE ( VALUE, '( F10.5 )' ) DATE
         END IF
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_CTOR( VALUE, DATA, STATUS )
            IF ( STATUS .NE. SAI__OK .OR. DATA .LE. 0.0 ) THEN
               CALL ERR_REP( 'SEARCH_BC', 'Must be A.D.!', STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( 'EPOCH', STATUS )
               GO TO 130
            END IF
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  Allows the user to define the catalogues to be searched.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( 'SEARCH_CATS',
     :   '             Define the catalogue parameters', STATUS )
      CALL MSG_BLANK( STATUS )

*  First get the current default for the mode.
      CALL GETDEFLT( PARAMS, NVAL, 'MODE', VALUE, NPOS, STATUS )

*  Only procede if an entry occurs. (This means that this parameter
*  may be discarded without changing the program, and avoids some
*  possible errors.)
      IF ( NPOS .GT. 0 ) THEN

*  Ask for a new value.
         CALL PAR_DEF0C( 'MODE', VALUE, STATUS )
         CALL PAR_GET0C( 'MODE', VALUE, STATUS )

*  Nothing else is done if a null response is given, or if an error
*  occurs.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Convert to upper case to make testing easier.
            CALL CHR_UCASE( VALUE )

*  Store standard responses whatever is entered.
            IF ( INDEX( VALUE, 'CSI' ) .GT. 0 ) VALUE = 'CSI'
            IF ( INDEX( VALUE, 'AST' ) .GT. 0 ) VALUE = 'AST'
            IF ( INDEX( VALUE, 'NSO' ) .GT. 0 ) VALUE = 'NSO'

*  If nothing has been recognized assume that CSI was intended.
            IF ( VALUE( 4:4 ) .NE. ' ' ) VALUE = 'CSI'

*  Store the new value in the parameter array.
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  For later use retain the first (significant) character.
      MODE = VALUE( 1:1 )
      IF ( MODE .NE. 'P' ) GO TO 208

*  Code inserted to set up the catalogue name to be used if a private
*  astrometric catalogue is being used.
*
*  First get the current default for the mode.
      CALL GETDEFLT( PARAMS, NVAL, 'INPUT', VALUE, NPOS, STATUS )

*  Only procede if an entry occurs. (This means that this parameter
*  may be discarded without changing the program, and avoids some
*  possible errors.)
      IF ( NPOS .GT. 0 ) THEN

*  Ask for a new value.
         CALL PAR_DEF0C( 'INPUT', VALUE, STATUS )
         CALL PAR_GET0C( 'INPUT', VALUE, STATUS )

*  Nothing else is done if a null response is given, or if an error
*  occurs.
         IF ( STATUS .EQ. SAI__OK ) THEN
*
* Next line commented out as now Chart is running on Unix, which is case
* sensitive, we don't want filenames converting to uppercase ! Don't
* know why it was there in the first place, but thought I'd better just
* comment it out.
*
**  Convert to upper case to make testing easier.
*           CALL CHR_UCASE( VALUE )

*  Store the new value in the parameter array.
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  Skip parameters that are unavailable in Non-Stellar Object Mode.

  208    IF ( MODE .EQ. 'N' ) GO TO 250

*  Repeat the process for SELECTION.
      CALL GETDEFLT( PARAMS, NVAL, 'SELECTION', VALUE, NPOS, STATUS )
      IF ( NPOS .GT. 0 ) THEN
         CALL PAR_DEF0C( 'SELECT', VALUE, STATUS )
         CALL PAR_GET0C( 'SELECT', VALUE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_UCASE( VALUE )

*  In this case the valid responses are ALL, N, M and C. ALL is assumed
*  if nothing else is detected.
            IF ( INDEX( VALUE, 'ALL' ) .GT. 0 ) VALUE = 'ALL'
            IF ( INDEX( VALUE, 'N' ) .GT. 0 ) VALUE = 'N'
            IF ( INDEX( VALUE, 'M' ) .GT. 0 ) VALUE = 'M'
            IF ( INDEX( VALUE, 'C' ) .GT. 0 ) VALUE = 'C'
            IF ( VALUE( 2:3 ) .NE. '  ') VALUE = 'ALL'
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  Again store the first character for later use.
      FIRSTCH = VALUE( 1:1 )

*  Ask for the appropriate parameters, depending on the type of
*  selection chosen.
      IF ( FIRSTCH .EQ. 'N' ) THEN

*  Obtain the maximum number of stars to be retained.
         CALL GETDEFLT( PARAMS, NVAL, 'NUMBER', VALUE, NPOS, STATUS )
         CALL PAR_DEF0C( 'NUMBER', VALUE, STATUS )
  220    CONTINUE
         CALL PAR_GET0C( 'NUMBER', VALUE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_UCASE( VALUE )

*  The input characters are converted into an integer to validate the
*  input.
            CALL CHR_CTOI( VALUE, NUM, STATUS )
            IF ( STATUS .NE. SAI__OK .OR. NUM. LE. 0 ) THEN
               CALL ERR_REP( 'SEARCH_NEG',
     :            'Value must be a positive integer', STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( 'NUMBER', STATUS )
               GO TO 220
            END IF
            PARAMS(NPOS)(21:70)=VALUE
         END IF
      END IF

*  Do the same kind of thing for the magnitude limit.
      IF ( FIRSTCH .EQ. 'M' ) THEN
         CALL GETDEFLT( PARAMS, NVAL, 'MAGNITUDE', VALUE, NPOS, STATUS )
         CALL PAR_DEF0C( 'MAGNITUD', VALUE, STATUS )
  230    CONTINUE
         CALL PAR_GET0C( 'MAGNITUD', VALUE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_UCASE( VALUE )

*  In this case the input may be a real number.
            CALL CHR_CTOR( VALUE, BRT, STATUS )
            IF ( STATUS .NE. SAI__OK .OR. BRT .LE. 0.0 .OR.
     :         BRT .GT. 13.0 ) THEN
               CALL ERR_REP( 'SEARCH_MAGRANGE',
     :            'Value must be between 0.0 and 13.0', STATUS )
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( 'MAGNITUD', STATUS )
               GO TO 230
            END IF
            PARAMS( NPOS )( 21:70 ) = VALUE
         END IF
      END IF

*  If the type selection is by catalogue ("C"), and the MODE is CSI or
*  ASTROM then ask for further specification of the catalogues chosen.
      IF ( ( MODE .EQ. 'C' .AND. FIRSTCH .EQ. 'C' ) .OR.
     :   ( MODE .EQ. 'A' ) ) THEN
         CALL GETDEFLT( PARAMS, NVAL, 'CATALOGUES', VALUE, NPOS,
     :      STATUS )
         CALL PAR_DEF0C( 'CATALOGU', VALUE, STATUS )
         CALL PAR_GET0C( 'CATALOGU', VALUE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Remove any commas from VALUE. This is only done because the INTERIM
*  version of CHART would never have commas in a list of catalogues, so
*  it is safest to remove any that exist in the ADAM version.
            DO I = 1, LEN( VALUE )
               IF ( VALUE( I:I ) .EQ. ',' ) THEN
                  VALUE( I:I ) = ' '
               END IF
            END DO
            CALL CHR_UCASE( VALUE )
            PARAMS(NPOS)(21:70)=VALUE
         END IF
      END IF
 250  CONTINUE

*  Allows the user to specify the source of field centres.  The options
*  are from a terminal (standard) or from a file (equivalent to the
*  BATCH command in Chart).

      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', '       Define source of field centres',
     :   STATUS )
      CALL MSG_BLANK( STATUS )

*  Read the parameter file and extract the current value.
      CALL GETDEFLT( PARAMS, NVAL, 'FIELDS', FIELD, NPOS, STATUS )

*  Read a new value.
      CALL PAR_DEF0C( 'FIELDS', FIELD, STATUS )
      CALL PAR_GET0C( 'FIELDS', FIELD, STATUS )

*  If a valid response was given then store it, after checking whether
*  it's 'terminal' and hence needs converting to uppercase
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( FIELD(:8) .EQ. 'terminal') CALL CHR_UCASE( FIELD )
         PARAMS( NPOS )( 21:70 ) = FIELD
      END IF

*  Write the new parameters to disk.
      CALL PUTPARAMS( PARAMS, NVAL, STATUS )
      CALL PAR_CANCL( 'FIELDS', STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SSET_ERR',
     :   'SEARCH: Failed to set the CHART parameters',
     :   STATUS )
      END IF

      END
