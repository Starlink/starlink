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
*           SATURATION, SETSAT, USESET
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

*  Behaviour of Parameters:
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
*     - Uses direct HDS calls to erase GLOBAL parameter file components.

*  Copyright:
*     Copyright (C) 1991, 1993-1994 Science & Engineering Research
*     Council. Copyright (C) 1995, 2000-2001 Central Laboratory of the
*     Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
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
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     26-MAR-2001 (MBT):
*        Added USESET parameter.
*     10-MAY-2001 (MBT):
*        Modified to access parameter values keyed by Set Index values.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string

*  Local Constants:
      INTEGER MAXNAM             ! The number of parameter names
      PARAMETER ( MAXNAM = 19 )

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) GNAME ! Name of global parameter
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to global file
      CHARACTER * ( DAT__SZNAM ) NAME ! Name of object
      CHARACTER * ( DAT__SZNAM ) PNAMES( MAXNAM ) ! Names of parameters
      CHARACTER * ( DAT__SZLOC ) SLOC ! Locator to keyed parameters structure
      INTEGER NAMGRP             ! Group of parameter NAMES
      INTEGER I                  ! Loop variable
      INTEGER NMATCH             ! Number of matched characters
      INTEGER BEST               ! Position of best match
      INTEGER HAVE               ! Number of given names matched
      INTEGER J                  ! Loop variable
      INTEGER K                  ! Loop variable
      INTEGER NCLEAR             ! Number of cleared parameters
      INTEGER NCOMP              ! Number of components in structure
      INTEGER NNAMES             ! Number of names to clear
      INTEGER THSMAT             ! Number of matched character this time
      INTEGER NAMLEN             ! Length of name string
      LOGICAL BYNAME             ! If true only named parameters are cleared
      LOGICAL DELET( MAXNAM )    ! Delete parameter ?
      LOGICAL ERASED             ! Successfully erased this parameter?
      LOGICAL THERE              ! Does HDS component exist?

*  Local Data:
      DATA PNAMES / 'ADC', 'BOUNDS', 'RNOISE', 'MASK', 'DIRECTION',
     :              'DEFERRED', 'EXTENT', 'PRESERVE', 'GENVAR',
     :              'NDFNAMES', 'USESET', 'LOGTO', 'LOGFILE', 'FLAT',
     :              'BIAS', 'CAL', 'SATURATE', 'SATURATION', 'SETSAT' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup CCDPACK logging system.
      CALL CCD1_START( 'CCDCLEAR', STATUS )

*  Get the mode which we're going to work in. Either we will clear
*  all global parameters or a specific list.
      CALL PAR_GET0L( 'BYNAME', BYNAME, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      IF ( BYNAME ) THEN

*  Need a list of names.
         CALL CCD1_STRGR( 'NAMES', GRP__NOID, 1, MAXNAM, NAMGRP,
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
            CALL GRP_GET( NAMGRP, I, 1, NAME, STATUS )
            CALL CHR_UCASE( NAME )

*  Set the maximum number of characters matched and the match this
*  correspond too (BEST). Blanks in names do not count.
            NMATCH = 0
            NAMLEN = CHR_LEN( NAME )
            BEST = 0
            DO 4 J = 1, MAXNAM
               THSMAT = 0
               DO 5 K = 1, NAMLEN
                  IF ( NAME( K:K ) .EQ. PNAMES( J )( K:K ) .AND.
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

*  Get locators for the global parameter file and the keyed parameter
*  structure.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL CCD1_GPARF( 'UPDATE', .FALSE., LOC, SLOC, STATUS )

*  Check that it exists.
      IF ( LOC .EQ. DAT__NOLOC ) THEN
         CALL CCD1_MSG( ' ',
     :'  Sorry unable to locate the GLOBAL parameters file', STATUS )
         CALL CCD1_MSG( ' ', '  No parameters cleared', STATUS )
         GO TO 99
      END IF

*  Set number of parameters cleared.
      NCLEAR = 0

*  Loop over parameters which we are to delete.
      DO I = 1, MAXNAM
         ERASED = .FALSE.
         IF ( DELET( I ) ) THEN

*  Get the name of the corresponding global parameter.
            CALL CCD1_GPNAM( PNAMES( I ), GNAME, STATUS )

*  Erase the global variable if it exists.
            CALL DAT_THERE( LOC, GNAME, THERE, STATUS )
            IF ( THERE ) THEN
               CALL DAT_ERASE( LOC, GNAME, STATUS )
               ERASED = .TRUE.
            END IF

*  Erase the corresponding variable in the keyed parameters structure if
*  it exists.
            IF ( SLOC .NE. DAT__NOLOC ) THEN
               CALL DAT_THERE( SLOC, PNAMES( I ), THERE, STATUS )
               IF ( THERE ) THEN
                  CALL DAT_ERASE( SLOC, PNAMES( I ), STATUS )
                  ERASED = .TRUE.
               END IF
            END IF

*  Record that this object has been erased.
            IF ( ERASED .AND. STATUS .EQ. SAI__OK ) THEN
               CALL MSG_SETC( 'NAME', PNAMES( I ) )
               CALL CCD1_MSG( ' ', '  Parameter ^NAME has been cleared',
     :                        STATUS )
               NCLEAR = NCLEAR + 1
            END IF
         END IF
      END DO

*  If failed to clear any parameters comment on this.
      IF ( NCLEAR .EQ. 0 ) THEN
         CALL CCD1_MSG( ' ', '  No CCDPACK global parameters '//
     :                       'cleared - use CCDSETUP to set and '//
     :                       'CCDSHOW to examine their values', STATUS )
      END IF

*  Finally for the sake of tidiness, if the keyed parameters structure
*  now contains nothing, try to erase it.
      IF ( SLOC .NE. DAT__NOLOC .AND. STATUS .EQ. SAI__OK ) THEN
         CALL DAT_NCOMP( SLOC, NCOMP, STATUS )
         IF ( NCOMP .EQ. 0 ) THEN
            CALL DAT_ANNUL( SLOC, STATUS )
            CALL DAT_ERASE( LOC, 'CCDPACK_KEYPARS', STATUS )
         END IF

*  Any error here is not very important and can be ignored.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF
      END IF

*  Close down section
99    CONTINUE

*  Release the GRP group of names.
      CALL CCD1_GRDEL( NAMGRP, STATUS )

*  Release the locator for the keyed parameters structure.
      IF ( SLOC .NE. DAT__NOLOC ) CALL DAT_ANNUL( SLOC, STATUS )

*  Close the globals file.
      IF ( LOC .NE. DAT__NOLOC ) CALL HDS_CLOSE( LOC, STATUS )

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
