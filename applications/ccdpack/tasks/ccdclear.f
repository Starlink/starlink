      SUBROUTINE CCDCLEAR( STATUS )
*+
*  Name:
*     CCDCLEAR

*  Purpose:
*     Clears CCDPACK global parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CCDCLEAR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     CCDCLEAR removes CCDPACK specific parameters from the globals
*     file. It has the capability of removing all the CCDPACK global
*     parameters or just a named subset.

*  Usage:
*     ccdclear byname

*  ADAM Parameters:
*     BYNAME = _LOGICAL (Read)
*        This parameter controls how the parameters are cleared.
*        If FALSE then all CCDPACK global parameters will be cleared.
*        If TRUE then a list of the names of the global parameters to
*        clear is requested (see parameter NAMES).
*        [FALSE]
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     NAMES = LITERAL (Read)
*        Only used when BYNAME is TRUE. The response to this parameter
*        should be a comma separated list of the names of the CCDPACK
*        parameters which are to be cleared. Valid names are:
*
*           ADC, BIAS, BOUNDS, CAL, DEFERRED, DIRECTION, EXTENT, FLAT,
*           GENVAR, MASK, NDFNAMES, PRESERVE, RNOISE, SATURATE,
*           SATURATION, SETSAT
*
*        These correspond to the parameter names used in CCDSETUP (and
*        in the other applications which access these parameters).
*
*        The names may be abbreviated to unique values.

*  Examples:
*     ccdclear
*        Invoking CCDCLEAR without any arguments will clear all the
*        CCDPACK globals, unless the BYNAME=TRUE option has been used in
*        a previous invocation.
*
*     ccdclear false
*        Using this invocation will definitely clear all the CCDPACK
*        global parameters.
*
*     ccdclear byname names='"adc,rnoise,direc"'
*        This example shows how to clear specific CCDPACK global
*        parameters. The NAMES need only be unique amongst the
*        possibilities so could have been abbreviated to "a,r,di".

*  Behaviour of parameters:
*     All parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application. The intrinsic
*     default behaviour of the application may be restored by using the
*     RESET keyword on the command line.
*
*     Certain parameters (LOGTO and LOGFILE ) have global values. These
*     global values will always take precedence, except when an
*     assignment is made on the command line.  Global values may be set
*     using the CCDSETUP command.

*  Deficiencies:
*     - Relies on subpar routine call to get global file. Uses direct
*       HDS calls to erase components.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JUL-1991 (PDRAPER):
*        Original version.
*     20-JUL-1993 (PDRAPER):
*        Added capability to clear specific names. Removed necessity
*        for specifying the global file name.
*     28-JAN-1994 (PDRAPER):
*        Added saturation parameters.
*     17-AUG-1995 (PDRAPER):
*        Increased size of NAME buffer to DAT__SZNAM. This removes the
*        possibility of comparing non-existent elements.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'DAT_ERR'          ! HDS and DAT error codes
      INCLUDE 'IRH_PAR'          ! IRH parameters

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string

*  Local Constants:
      INTEGER MAXNAM             ! The number of parameter names
      PARAMETER ( MAXNAM = 18 )

*  Local Variables:
      CHARACTER * ( 80 ) FILE    ! Name of global file
      CHARACTER * ( DAT__SZNAM ) ANAMES( MAXNAM ) ! Actual names of parameters
      CHARACTER * ( DAT__SZNAM ) ONAMES( MAXNAM ) ! Official names of parameters
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to global file
      CHARACTER * ( DAT__SZLOC ) NEWLOC ! Locator to object
      CHARACTER * ( DAT__SZNAM ) NAME ! Name of object
      INTEGER INDEX              ! Index of current object
      INTEGER NAMGRP             ! Group of parameter NAMES
      INTEGER I                  ! Loop variable
      INTEGER NMATCH             ! Number of matched characters
      INTEGER BEST               ! Position of best match
      INTEGER HAVE               ! Number of given names matched
      INTEGER J                  ! Loop variable
      INTEGER K                  ! Loop variable
      INTEGER NCLEAR             ! Number of cleared parameters
      INTEGER NNAMES             ! Number of names to clear
      INTEGER LENFIL             ! Length of ADAM_USER directory
      INTEGER THSMAT             ! Number of matched character this time
      INTEGER NAMLEN             ! Length of name string
      LOGICAL BYNAME             ! If true only named parameters are cleared
      LOGICAL DELET( MAXNAM )    ! Delete parameter ?
      LOGICAL OK                 ! Flag to control locating loop
      LOGICAL OPEN               ! Globals file is open

*  Local Data:
      DATA ONAMES / 'ADC', 'BOUNDS', 'RNOISE', 'MASK', 'DIRECTION',
     :              'DEFERRED', 'EXTENT', 'PRESERVE', 'GENVAR',
     :              'NDFNAMES', 'LOGTO', 'LOGFILE', 'FLAT', 'BIAS',
     :              'CAL', 'SATURATE', 'SATURATION', 'SETSAT'  /
      DATA ANAMES / 'CCDPACK_ADC', 'CCDPACK_BOUNDS', 'CCDPACK_RNOISE',
     :              'CCDPACK_MASK', 'CCDPACK_DIRECT', 'CCDPACK_DEFER',
     :              'CCDPACK_EXTENT', 'CCDPACK_PRESER',
     :              'CCDPACK_GENVAR', 'CCDPACK_NDFNAM',
     :              'CCDPACK_LOGTO', 'CCDPACK_LOGFILE', 'CCDPACK_FLAT',
     :              'CCDPACK_BIAS', 'CCDPACK_CAL', 'CCDPACK_SATUR',
     :              'CCDPACK_SATVAL', 'CCDPACK_SETSAT' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup CCDPACK logging system.
      CALL CCD1_START( 'CCDCLEAR', STATUS )

*  Get the mode which we're going to work in. Either we will clear
*  all global parameters or a specific list.
      CALL PAR_GET0L( 'BYNAME', BYNAME, STATUS )
      IF ( BYNAME ) THEN

*  Need a list of names.
         CALL CCD1_STRGR( 'NAMES', IRH__NOID, 1, MAXNAM, NAMGRP,
     :                    NNAMES, STATUS )

*  Clear the deletion flags.
         DO 6 I = 1, MAXNAM
            DELET( I ) = .FALSE.
 6       CONTINUE

*  Count number of names actually matched (require at least one).
         HAVE = 0

*  Check these out against the "official" list. Resolve ambiguities.
         DO 3 I = 1, NNAMES

*  Get the name from the list.
            CALL IRH_GET( NAMGRP, I, 1, NAME, STATUS )
            CALL CHR_UCASE( NAME )

*  Set the maximum number of characters matched and the match this
*  correspond too (BEST). Blanks in names do not count.
            NMATCH = 0
            NAMLEN = CHR_LEN( NAME )
            BEST = 0
            DO 4 J = 1, MAXNAM
               THSMAT = 0
               DO 5 K = 1, NAMLEN
                  IF ( NAME( K:K ) .EQ. ONAMES( J )( K:K ) .AND.
     :                 NAME( K:K ) .NE. ' ' ) THEN
                     THSMAT = THSMAT + 1
                  END IF
 5             CONTINUE
               IF ( THSMAT .GT. NMATCH ) THEN
                  NMATCH = THSMAT
                  BEST = J
               END IF

*  Stop if match exact.
               IF ( NMATCH .EQ. NAMLEN ) THEN
                  DELET( BEST ) = .TRUE.
                  HAVE = HAVE + 1
                  GO TO 3
               END IF
 4          CONTINUE

*  Arrive here if match is not exact. Has any match been made?
            IF ( BEST .NE. 0 ) THEN

*  Yes go for this.
               DELET( BEST ) = .TRUE.
               HAVE = HAVE + 1
            ELSE

*  No match for this string.
               CALL MSG_SETC( 'NAME', NAME )
               CALL CCD1_MSG( ' ', '  ^NAME does not match any known'//
     :' CCDPACK global parameter -- ignored', STATUS )
            END IF
 3       CONTINUE

*  Check that at least some names have been matched.
         IF ( HAVE .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCDCLEAR_NONE',
     :           '  Failed to match any of the given names '//
     :           'with any known global variables. ', STATUS )
            GO TO 99
         END IF
      ELSE

*  Set the deletion flags for all names.
         DO 9 I = 1, MAXNAM
            DELET( I ) = .TRUE.
 9       CONTINUE
      END IF

*  Access the name of the global file. Use a SUBPAR call to get the name
*  of the directory which is defined as ADAM_USER, then append the
*  GLOBAL name to.
      OPEN = .FALSE.
      FILE = ' '
      CALL SUBPAR_ADMUS( FILE, LENFIL, STATUS )
      CALL CHR_APPND( 'GLOBAL', FILE, LENFIL )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Initialisation clear of the locators.
      LOC = ' '
      NEWLOC = ' '

*  Check that it exists.
      CALL ERR_MARK
      CALL HDS_OPEN( FILE, 'UPDATE', LOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'FILE', FILE )
         CALL CCD1_MSG( ' ',
     :'  Sorry unable to locate a global parameters file (^FILE)', 
     :               STATUS )
         CALL CCD1_MSG( ' ',
     :'  No parameters cleared', STATUS )
         CALL ERR_RLSE
         GO TO 99
      END IF
      CALL ERR_RLSE
      OPEN = .TRUE.

*  Set number of parameters cleared.
      NCLEAR = 0

*  Loop Locating all objects with name CCDPACK_ something in the
*  globals file.
      OK = .TRUE.
      INDEX = 1
 2    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( OK ) THEN

*  Find out if an object with this index exists. If so get its name.
         CALL DAT_INDEX( LOC, INDEX, NEWLOC, STATUS )
         IF ( STATUS .EQ. DAT__OBJNF ) THEN
            CALL ERR_ANNUL( STATUS )
            OK = .FALSE.
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*  Things are still ok get the object name.
            NAME = ' '
            CALL DAT_NAME( NEWLOC, NAME, STATUS )
            NAMLEN = CHR_LEN( NAME )

*  Can release the locator in preparation for deletion now.
            CALL DAT_ANNUL( NEWLOC, STATUS )
            NEWLOC = ' '

*  Check this name against the official list.
            THSMAT = 0
            DO 7 I = 1, MAXNAM
               IF ( NAME( :NAMLEN ) .EQ. ANAMES( I ) ) THEN

*  Ok this is a name we may erase. Are we allowed?
                  IF ( DELET( I ) ) THEN
                     THSMAT = I
                     GO TO 8
                  END IF
               END IF
 7          CONTINUE
 8          CONTINUE

*  Only continue is this is a genuine name which we may delete.
            IF ( THSMAT .NE. 0 ) THEN

*  Erase it. Note that it is erased below the parent object, which is
*  the file locator in this case.
               CALL DAT_ERASE( LOC, NAME( :NAMLEN ), STATUS )

*  Issue message showing that this object has been erased.
               CALL MSG_SETC( 'NAME', ONAMES( THSMAT ) )
               CALL CCD1_MSG( ' ', '  Parameter ^NAME has been cleared',
     :                        STATUS )

*  Increment count of object cleared.
               IF ( STATUS .EQ. SAI__OK ) NCLEAR = NCLEAR + 1
            ELSE

*  Increment for next object. Note that increment only occurs if the
*  last object has not been deleted. The objects move up the list
*  if previous ones have been deleted.
               INDEX = INDEX + 1
            END IF
         ELSE

*  Unidentified error - get out.
            GO TO 99
         END IF
         GO TO 2
      END IF

*  If failed to clear any parameters comment on this.
      IF ( NCLEAR .EQ. 0 ) THEN
         CALL CCD1_MSG( ' ', '  No CCDPACK global parameters '//
     :                       'cleared - use CCDSETUP to set and '//
     :                       'CCDSHOW to examine their values', STATUS )
      END IF

*  Close down section
99    CONTINUE

*  Release the IRH group of names.
      IF ( BYNAME ) CALL IRH_ANNUL( NAMGRP, STATUS )

*  Close the globals file.
      IF ( OPEN ) CALL HDS_CLOSE( LOC, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'CCDCLEAR_ERR',
     :   'CCDCLEAR: Error clearing global parameters...',
     :   STATUS )
      END IF

*  Terminate CCDPACK logging
      CALL CCD1_END( STATUS )

      END
* $Id$
