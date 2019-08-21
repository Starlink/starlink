      SUBROUTINE NDF_PROP( INDF1, CLIST, PARAM, INDF2, STATUS )
*+
*  Name:
*     NDF_PROP

*  Purpose:
*     Propagate NDF information to create a new NDF via the ADAM
*     parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_PROP( INDF1, CLIST, PARAM, INDF2, STATUS )

*  Description:
*     The routine creates a new NDF data structure through the ADAM
*     parameter system, associates it with a parameter and returns an
*     identifier for it. The shape, data type, etc. of this new NDF are
*     based on a existing "template" NDF, and the values of components
*     of this template may be selectively propagated to initialise the
*     new data structure.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for an existing NDF (or NDF section) to act as a
*        template.
*     CLIST = CHARACTER * ( * ) (Given)
*        A comma-separated list of the NDF components which are to be
*        propagated to the new data structure. By default, the HISTORY,
*        LABEL and TITLE components are propagated. All extensions are
*        also propagated by default except for any that have had a zero
*        value assigned to the corresponding "PXT..." tuning parameter
*        using NDF_TUNE. See the "Component Propagation" section for
*        further details.
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter for the new NDF.
*     INDF2 = INTEGER (Returned)
*        Identifier for the new NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF2 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The NDF__NOID
*     constant is defined in the include file NDF_PAR.

*  Component Propagation:
*     -  The template components whose values are to be propagated to
*     initialise the new data structure are specified via the CLIST
*     argument. Thus CLIST='DATA,QUALITY' would cause the new NDF to
*     inherit its DATA and QUALITY values (if available) from the
*     template structure, in addition to those propagated by default.
*     Component propagation may be suppressed by supplying a component
*     name with the prefix 'NO'. Thus CLIST='DATA,NOHISTORY' would
*     propagate the DATA component, but suppress propagation of
*     HISTORY. If component names appear more than once in the CLIST
*     value, then the last occurrence takes precedence.
*     -  Propagation of specific NDF extensions may be suppressed by
*     using 'NOEXTENSION()' as one of the items in the CLIST argument;
*     a list of the extensions to be suppressed should appear between
*     the parentheses. Thus CLIST='AXIS,NOEXTENSION(IRAS,ASTERIX)'
*     would propagate the AXIS component, but suppress propagation of
*     the IRAS and ASTERIX extensions (if present). Propagation of
*     suppressed extensions may be re-enabled by specifying
*     'EXTENSION()' in a similar manner at a later point in the CLIST
*     value.
*     -  An asterisk (*) may be used as a wild card to match all extension
*     names. Thus 'NOEXTENSION(*),EXTENSION(IRAS)' may be used to indicate
*     that only the IRAS extension should be propagated.
*     -  Whether or not a named extension is propagated by default can be
*     controlled via an NDF tuning parameter (see NDF_TUNE). The defaults
*     established using NDF_TUNE can be over-ridden by specifying the
*     extension explicitly within the CLIST parameter; e.g.
*     'EXTENSION(FITS)' or 'NOEXTENSION(FITS)' can be used to over-ride
*     the default established by the PXTFITS tuning parameter.
*     -  Component names in the CLIST argument may be abbreviated to 3
*     characters, but extension names must appear in full.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (JACH, UCLan)
*     {enter_new_authors_here}

*  History:
*     12-OCT-1989 (RFWS):
*        Original, derived from the NDF_CREAT routine.
*     17-OCT-1989 (RFWS):
*        Added initialisation of the INDF2 argument.
*     29-JAN-1990 (RFWS):
*        Eliminated re-prompting to avoid looping under parameter
*        system error conditions. Re-prompting is now handled by the
*        parameter system alone. Changed error messages to include the
*        parameter name using escape characters.
*     26-FEB-1990 (RFWS):
*        Changed the NDF placeholder data type to 'NDF'.
*     26-MAR-1990 (RFWS):
*        Re-structured status checking.
*     4-DEC-1991 (RFWS):
*        Removed a temporary fix which reported an error if creation of
*        the output NDF failed. This is now done by HDS.
*     4-NOV-1993 (RFWS):
*        Changed to support foreign file formats.
*     1-NOV-2007 (DSB):
*        Add support for the PXT... tuning parameters.
*     22-FEB-2010 (DSB):
*        Allow an asterisk to be used as a wild card to match all
*        extension names.
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
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

*  Arguments Given:
      INTEGER INDF1
      CHARACTER * ( * ) CLIST
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) EXTN( NDF__MXEXT ) ! Excluded ext. list
      CHARACTER * ( NDF__SZPAR ) NAME ! NDF name string
      INTEGER IACB1              ! Index to input NDF entry in the ACB
      INTEGER IACB2              ! Index to output NDF entry in the ACB
      INTEGER IPAR               ! Parameter table index
      INTEGER IPCB               ! Index to placeholder entry in the PCB
      INTEGER NEXTN              ! Number of excluded extensions
      INTEGER TSTAT              ! Temporary status variable
      LOGICAL CPF( NDF__MXCPF )  ! Component propagation flags

*.

*  Set an initial value for the INDF2 argument.
      INDF2 = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark the error stack, so that annulling error messages doesn't
*  disturb any pre-existing error stack contents.
      CALL ERR_MARK

*  Import the input NDF identifier.
      CALL NDF1_IMPID( INDF1, IACB1, STATUS )

*  Get a list of all available extensions in the input NDF.
      CALL NDF1_XLST( IACB1, NDF__MXEXT, EXTN, NEXTN, STATUS )

*  Parse the component propagation expression.
      CALL NDF1_PSCPX( CLIST, NDF__MXEXT, EXTN, NEXTN, CPF, STATUS )

*  Find the parameter index in the parameter tables.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop until a valid output NDF structure has been created or a
*  non-recoverable error occurs.
 1       CONTINUE                ! Start of 'DO WHILE' loop

*  Obtain the new NDF name via the parameter.
         CALL SUBPAR_GETNAME( IPAR, NAME, STATUS )
         IACB2 = 0
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Create a placeholder entry for the object in the PCB and selectively
*  propagate the components of the input NDF to create a new base NDF.
            CALL NDF1_PLFOR( DAT__ROOT, NAME, IPCB, STATUS )
            CALL NDF1_PRP( IACB1, NEXTN, EXTN, CPF, IPCB, IACB2,
     :                     STATUS )

*  Annul the PCB entry when done, erasing the object if there has been
*  an error.
            CALL NDF1_ANNPL( ( STATUS .NE. SAI__OK ), IPCB, STATUS )

*  If this failed, then the user must be re-prompted. Report contextual
*  information and flush any error messages.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'PARAM', PARAM )
               CALL ERR_REP( 'NDF_PROP_CTX',
     : 'NDF_PROP: Unable to propagate NDF information to create a ' //
     : 'new NDF via the ''%^PARAM'' parameter.',
     :                       STATUS )
               CALL ERR_FLUSH( STATUS )

*  Cancel the parameter association, annulling any further error
*  messages this may generate.
               CALL SUBPAR_CANCL( IPAR, STATUS )
               CALL ERR_ANNUL( STATUS )

*  Return to re-prompt.
               GO TO 1
            END IF
         END IF
      END IF

*  Export an NDF identifier
      CALL NDF1_EXPID( IACB2, INDF2, STATUS )

*  If an error occurred, then annul any ACB entry which might have been
*  acquired.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF1_ANL( IACB2, STATUS )
      END IF

*  If an error occurred, then classify it...

*  If an "abort" was requested, then annul any error messages and issue
*  an appropriate new one.
      IF ( STATUS .EQ. PAR__ABORT ) THEN
         TSTAT = STATUS
         CALL ERR_ANNUL( TSTAT )
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_PROP_ABT',
     :   'Aborted creation of a new NDF structure via the ' //
     :   '''%^PARAM'' parameter.', STATUS )

*  If an "null" NDF was specified, then annul any error messages and
*  issue an appropriate new one.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         TSTAT = STATUS
         CALL ERR_ANNUL( TSTAT )
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_PROP_NULL',
     :   'Null NDF structure specified for the ''%^PARAM'' ' //
     :   'parameter.', STATUS )

*  For other errors, add context information and call the error tracing
*  routine.
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_PROP_ERR',
     :   'NDF_PROP: Error propagating NDF information to create a ' //
     :   'new NDF via the ''%^PARAM'' parameter.', STATUS )
         CALL NDF1_TRACE( 'NDF_PROP', STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
