      SUBROUTINE NDF_TUNE( VALUE, TPAR, STATUS )
*+
*  Name:
*     NDF_TUNE

*  Purpose:
*     Set an NDF_ system tuning parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_TUNE( VALUE, TPAR, STATUS )

*  Description:
*     The routine sets a new value for an NDF_ system internal tuning
*     parameter.

*  Arguments:
*     VALUE = INTEGER (Given)
*        New value for the tuning parameter.
*     TPAR = CHARACTER * ( * ) (Given)
*        Name of the parameter to be set (case insensitive).  This name
*        may be abbreviated, to no less than 3 characters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The following tuning parameters are currently available:
*     - 'DOCVT': Controls whether to convert foreign format data files
*     to and from native NDF format for access (using the facilities
*     described in SSN/20). If DOCVT is set to 1 (the default), and the
*     other necessary steps described in SSN/20 have been taken, then
*     such conversions will be performed whenever they are necessary to
*     gain access to data stored in a foreign format. If DOCVT is set
*     to 0, no such conversions will be attempted and all data will be
*     accessed in native NDF format only. The value of DOCVT may be
*     changed at any time. It is the value current when a dataset is
*     first accessed by the NDF_ library which is significant.
*     -  'KEEP': Controls whether to retain a native format NDF copy of
*     any foreign format data files which are accessed by the NDF_
*     library (and automatically converted using the facilities
*     described in SSN/20). If KEEP is set to 0 (the default), then
*     the results of converting foreign format data files will be stored
*     in scratch filespace and deleted when no longer required. If KEEP
*     is set to 1, the results of the conversion will instead be stored
*     in permanent NDF data files in the default directory (such files
*     will have the same name as the foreign file from which they are
*     derived and a file type of '.sdf'). Setting KEEP to 1 may be
*     useful if the same datasets are to be re-used, as it avoids having
*     to convert them on each occasion. The value of KEEP may be changed
*     at any time. It is the value current when a foreign format file is
*     first accessed by the NDF_ library which is significant.
*     -  'SHCVT': Controls whether diagnostic information is displayed
*     to show the actions being taken to convert to and from foreign
*     data formats (using the facilities described in SSN/20). If SHCVT
*     is set to 1, then this information is displayed to assist in
*     debugging external format conversion software whenever a foreign
*     format file is accessed. If SHCVT is set to 0 (the default), this
*     information does not appear and format conversion proceeds
*     silently unless an error occurs.
*     -  'TRACE': Controls the reporting of additional error messages
*     which may occasionally be useful for diagnosing internal problems
*     within the NDF_ library. If TRACE is set to 1, then any error
*     occurring within the NDF_ system will be accompanied by error
*     messages indicating which internal routines have exited
*     prematurely as a result. If TRACE is set to 0 (the default),
*     this internal diagnostic information will not appear and only
*     standard error messages will be produced.
*     -  'WARN': Controls the issuing of warning messages when certain
*     non-fatal errors in the structure of NDF data objects are
*     detected. If WARN is set to 1 (the default), then a warning
*     message is issued. If WARN is set to 0, then no message is
*     issued.  In both cases normal execution continues and no STATUS
*     value is set.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     4-OCT-1991 (RFWS):
*        Original version.
*     14-OCT-1991 (RFWS):
*        Change argument order and report contextual error information.
*     17-OCT-1991 (RFWS):
*        Fixed bug: missing argument to ERR_REP.
*     5-NOV-1993 (RFWS):
*        Added new tuning parameters to control foreign format
*        conversion.
*     11-MAR-1997 (RFWS):
*        Add the DOCVT tuning parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_DOCVT = LOGICAL (Write)
*           Do format conversions flag.
*        TCB_ETFLG = LOGICAL (Write)
*           Error tracing flag.
*        TCB_KEEP = LOGICAL (Write)
*           Keep NDF data objects flag.
*        TCB_SHCVT = LOGICAL (Write)
*           Show format conversions flag.
*        TCB_WARN = LOGICAL (Write)
*           Warning message flag.

*  Arguments Given:
      INTEGER VALUE
      CHARACTER * ( * ) TPAR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

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
*  If TRACE was specified, the set the error tracing flag
*  appropriately.
         IF ( NDF1_SIMLR( TPAR, 'TRACE', NDF__MINAB ) ) THEN
            IF ( VALUE .EQ. 0 ) THEN
               TCB_ETFLG = .FALSE.
            ELSE IF ( VALUE .EQ. 1 ) THEN
               TCB_ETFLG = .TRUE.

*  If the value supplied is not valid, then report an error.
            ELSE
               STATUS = NDF__TPVIN
               CALL MSG_SETI( 'VALUE', VALUE )
               CALL ERR_REP( 'NDF_TUNE_TRAC',
     : 'The value ^VALUE is not valid for the tuning parameter ' //
     : 'TRACE; it should be 0 or 1 (possible programming error).',
     :                       STATUS )
            END IF

*  Do format conversions flag.
*  ==========================
*  If DOCVT was specified, then set the do format conversions flag
*  appropriately.
         ELSE IF ( NDF1_SIMLR( TPAR, 'DOCVT', NDF__MINAB ) ) THEN
            IF ( VALUE .EQ. 0 ) THEN
               TCB_DOCVT = .FALSE.
            ELSE IF ( VALUE .EQ. 1 ) THEN
               TCB_DOCVT = .TRUE.

*  If the value supplied is not valid, then report an error.
            ELSE
               STATUS = NDF__TPVIN
               CALL MSG_SETI( 'VALUE', VALUE )
               CALL ERR_REP( 'NDF_TUNE_DOCV',
     : 'The value ^VALUE is not valid for the tuning parameter ' //
     : 'DOCVT; it should be 0 or 1 (possible programming error).',
     :                       STATUS )
            END IF

*  Keep NDF objects flag.
*  =====================
*  If KEEP was specified, the set the keep NDF objects flag
*  appropriately.
         ELSE IF ( NDF1_SIMLR( TPAR, 'KEEP', NDF__MINAB ) ) THEN
            IF ( VALUE .EQ. 0 ) THEN
               TCB_KEEP = .FALSE.
            ELSE IF ( VALUE .EQ. 1 ) THEN
               TCB_KEEP = .TRUE.

*  If the value supplied is not valid, then report an error.
            ELSE
               STATUS = NDF__TPVIN
               CALL MSG_SETI( 'VALUE', VALUE )
               CALL ERR_REP( 'NDF_TUNE_KEEP',
     : 'The value ^VALUE is not valid for the tuning parameter ' //
     : 'KEEP; it should be 0 or 1 (possible programming error).',
     :                       STATUS )
            END IF

*  Show data conversions flag.
*  ==========================
*  If SHCVT was specified, then set the show data conversions flag
*  appropriately.
         ELSE IF ( NDF1_SIMLR( TPAR, 'SHCVT', NDF__MINAB ) ) THEN
            IF ( VALUE .EQ. 0 ) THEN
               TCB_SHCVT = .FALSE.
            ELSE IF ( VALUE .EQ. 1 ) THEN
               TCB_SHCVT = .TRUE.

*  If the value supplied is not valid, then report an error.
            ELSE
               STATUS = NDF__TPVIN
               CALL MSG_SETI( 'VALUE', VALUE )
               CALL ERR_REP( 'NDF_TUNE_SHCV',
     : 'The value ^VALUE is not valid for the tuning parameter ' //
     : 'SHCVT; it should be 0 or 1 (possible programming error).',
     :                       STATUS )
            END IF

*  Warning message flag.
*  ====================
*  If WARN was specified, then set the warning message flag
*  appropriately.
         ELSE IF ( NDF1_SIMLR( TPAR, 'WARN', NDF__MINAB ) ) THEN
            IF ( VALUE .EQ. 0 ) THEN
               TCB_WARN = .FALSE.
            ELSE IF ( VALUE .EQ. 1 ) THEN
               TCB_WARN = .TRUE.

*  If the value supplied is not valid, then report an error.
            ELSE
               STATUS = NDF__TPVIN
               CALL MSG_SETI( 'VALUE', VALUE )
               CALL ERR_REP( 'NDF_TUNE_WARN',
     : 'The value ^VALUE is not valid for the tuning parameter ' //
     : 'WARN; it should be 0 or 1 (possible programming error).',
     :                       STATUS )
            END IF

*  Unknown tuning parameter.
*  ========================
*  Report an error if the tuning parameter name is not recognised.
         ELSE
            STATUS = NDF__TPNIN
            CALL MSG_SETC( 'TPAR', TPAR )
            CALL ERR_REP( 'NDF_TUNE_TPAR',
     : '''^TPAR'' is not a valid tuning parameter name (possible ' //
     : 'programming error).',
     :                    STATUS )
         END IF
      END IF
 
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_TUNE_ERR',
     : 'NDF_TUNE: Error setting a new value for an NDF_ system ' //
     : 'tuning parameter.',
     :                 STATUS )
         CALL NDF1_TRACE( 'NDF_TUNE', STATUS )
      END IF      

      END
