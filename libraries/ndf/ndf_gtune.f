      SUBROUTINE NDF_GTUNE( TPAR, VALUE, STATUS )
*+
*  Name:
*     NDF_GTUNE

*  Purpose:
*     Obtain the value of an NDF_ system tuning parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_GTUNE( TPAR, VALUE, STATUS )

*  Description:
*     The routine returns the current value of an NDF_ system internal
*     tuning parameter.

*  Arguments:
*     TPAR = CHARACTER * ( * ) (Given)
*        Name of the tuning parameter whose value is required (case
*        insensitive). This name may be abbreviated, to no less than 3
*        characters.
*     VALUE = INTEGER (Returned)
*        Value of the parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     See the NDF_TUNE routine for a list of the tuning parameters
*     currently available.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2007-2009 Science & Technology Facilities Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S. Berry (JACH, UClan)
*     {enter_new_authors_here}

*  History:
*     4-OCT-1991 (RFWS):
*        Original version.
*     14-OCT-1991 (RFWS):
*        Report contextual error information.
*     17-OCT-1991 (RFWS):
*        Fixed bug: missing argument to ERR_REP.
*     5-NOV-1993 (RFWS):
*        Added new tuning parameters to control foreign format
*        conversion.
*     11-MAR-1997 (RFWS):
*        Add the DOCVT tuning parameter.
*     1-NOV-2007 (DSB):
*        Add the PXT... tuning parameters.
*     18-SEP-2009 (DSB):
*        Add the AUTO_HISTORY tuning parameter.
*     16-JUL-2012 (DSB):
*        Add the SECMAX tuning parameter.
*     10-AUG-2018 (DSB):
*        Add the FIXDT tuning parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'AST_PAR'          ! AST_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_AUTOHISTORY = LOGICAL (Read)
*           Automatic History creation flag.
*        TCB_DOCVT = LOGICAL (Read)
*           Do format conversions flag.
*        TCB_ETFLG = LOGICAL (Read)
*           Error tracing flag.
*        TCB_KEEP = LOGICAL (Read)
*           Keep NDF data objects flag.
*        TCB_SECMAX = INTEGER (Read)
*           Maximum no. of mega-pixels in a section.
*        TCB_SHCVT = LOGICAL (Read)
*           Show format conversions flag.
*        TCB_WARN = LOGICAL (Read)
*           Warning message flag.
*        TCB_PXT = INTEGER (Read)
*           An AST pointer to a KeyMap holding the names of NDF
*           extensions and their associated default propagation flags.
*        TCB_FIXDT = LOGICAL (Read)
*           Use a fixed date and time in history records.

*  Arguments Given:
      CHARACTER * ( * ) TPAR

*  Arguments Returned:
      INTEGER VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      CHARACTER XNAME*(DAT__SZNAM) ! An NDF extension name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the TCB is initialised.
      CALL NDF1_INTCB( STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Test the tuning parameter name supplied against each permitted value
*  in turn, allowing abbreviation...

*  Error tracing flag.
*  ==================
*  If TRACE was specified, then return the error tracing flag value.
         IF ( NDF1_SIMLR( TPAR, 'TRACE', NDF__MINAB ) ) THEN
            IF ( TCB_ETFLG ) THEN
               VALUE = 1
            ELSE
               VALUE = 0
            END IF

*  Do format conversion flag.
*  =========================
*  If DOCVT was specified, then return the do format conversion flag
*  value.
         ELSE IF ( NDF1_SIMLR( TPAR, 'DOCVT', NDF__MINAB ) ) THEN
            IF ( TCB_DOCVT ) THEN
               VALUE = 1
            ELSE
               VALUE = 0
            END IF

*  Keep NDF objects flag.
*  =====================
*  If KEEP was specified, then return the keep NDF objects flag value.
         ELSE IF ( NDF1_SIMLR( TPAR, 'KEEP', NDF__MINAB ) ) THEN
            IF ( TCB_KEEP ) THEN
               VALUE = 1
            ELSE
               VALUE = 0
            END IF

*  Create History component automatically flag.
*  ============================================
         ELSE IF ( NDF1_SIMLR( TPAR, 'AUTO_HISTORY', NDF__MINAB ) ) THEN
            IF ( TCB_AUTOHISTORY ) THEN
               VALUE = 1
            ELSE
               VALUE = 0
            END IF

*  Show data conversion flag.
*  =========================
*  If SHCVT was specified, then return the show data conversion flag
*  value.
         ELSE IF ( NDF1_SIMLR( TPAR, 'SHCVT', NDF__MINAB ) ) THEN
            IF ( TCB_SHCVT ) THEN
               VALUE = 1
            ELSE
               VALUE = 0
            END IF

*  Warning message flag.
*  ====================
*  If WARN was specified, then return the warning message flag value.
         ELSE IF ( NDF1_SIMLR( TPAR, 'WARN', NDF__MINAB ) ) THEN
            IF ( TCB_WARN ) THEN
               VALUE = 1
            ELSE
               VALUE = 0
            END IF

*  Fix History date/time flag.
*  ===========================
*  If FIXDT was specified, then return the fixed history date/time flag value.
         ELSE IF ( NDF1_SIMLR( TPAR, 'FIXDT', NDF__MINAB ) ) THEN
            IF ( TCB_FIXDT ) THEN
               VALUE = 1
            ELSE
               VALUE = 0
            END IF

*  Extension propagation.
*  ======================
*  Any tuning flag that begins with "PXT" is assumed to be terminated
*  with the name of an extension. The tuning flag value is non-zero if
*  the extension should be propagated by default by NDF_PROP, etc, and
*  is zero if the extension should not be propagated by default.
         ELSE IF ( NDF1_SIMLR( TPAR(1:3), 'PXT', NDF__MINAB ) ) THEN

* Get the extension name and check it is not blank.
            XNAME = TPAR( 4 : )
            IF( XNAME .NE. ' ' ) THEN

*  If there is a KeyMap, and it contains a value for the named extension,
*  use the stored value. Otherwise use a defualt value of 1 (all
*  extensions are propagated by default).
               IF( TCB_PXT .NE. AST__NULL ) THEN
                  IF( .NOT. AST_MAPGET0I( TCB_PXT, XNAME, VALUE,
     :                                    STATUS ) ) VALUE = 1
               ELSE
                  VALUE = 1
               END IF

*  If the extension name was blank, then report an error.
            ELSE
               STATUS = NDF__TPNIN
               CALL ERR_REP( 'NDF_GTUNE_WARN',
     : '''PXT'' is not a valid tuning parameter name - it should '//
     : 'be followed by an NDF extension name (possible programming '//
     : 'error).', STATUS )
            END IF

*  Maximum size of a section.
*  ==========================
         ELSE IF ( NDF1_SIMLR( TPAR, 'SECMAX', NDF__MINAB ) ) THEN
            VALUE = TCB_SECMAX

*  Unknown tuning parameter.
*  ========================
*  Report an error if the tuning parameter name is not recognised.
         ELSE
            STATUS = NDF__TPNIN
            CALL MSG_SETC( 'TPAR', TPAR )
            CALL ERR_REP( 'NDF_GTUNE_TPAR',
     : '''^TPAR'' is not a valid tuning parameter name (possible ' //
     : 'programming error).',
     :                    STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_GTUNE_ERR',
     : 'NDF_GTUNE: Error obtaining the value of an NDF_ system ' //
     : 'tuning parameter.',
     :                 STATUS )
         CALL NDF1_TRACE( 'NDF_GTUNE', STATUS )
      END IF

      END
