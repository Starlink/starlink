      SUBROUTINE NDF_CREAT( PARAM, FTYPE, NDIM, LBND, UBND, INDF,
     :                      STATUS )
*+
*  Name:
*     NDF_CREAT

*  Purpose:
*     Create a new simple NDF via the ADAM parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_CREAT( PARAM, FTYPE, NDIM, LBND, UBND, INDF, STATUS )

*  Description:
*     The routine creates a new simple NDF via the ADAM parameter
*     system, associates it with a parameter, and returns an NDF
*     identifier for it.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter.
*     FTYPE = CHARACTER * ( * ) (Given)
*        Full data type of the NDF's DATA component (e.g. '_DOUBLE' or
*        'COMPLEX_REAL').
*     NDIM = INTEGER (Given)
*        Number of NDF dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        Lower pixel-index bounds of the NDF.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper pixel-index bounds of the NDF.
*     INDF = INTEGER (Returned)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine creates a "simple" NDF, i.e. one whose array
*     components will be stored in "simple" form by default (see
*     SGP/38).
*     -  The full data type of the DATA component is specified via the
*     FTYPE argument and the data type of the VARIANCE component
*     defaults to the same value. These data types may be set
*     individually with the NDF_STYPE routine if required.
*     -  If this routine is called with STATUS set, then a value of
*     NDF__NOID will be returned for the INDF argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The NDF__NOID
*     constant is defined in the include file NDF_PAR.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1989 (RFWS):
*        Original version.
*     11-OCT-1989 (RFWS):
*        Substantial re-write to implement re-prompting and generation
*        of sensible error messages.
*     17-OCT-1989 (RFWS):
*        Added initialisation of the INDF argument.
*     6-DEC-1989 (RFWS):
*        Changed the name of the NDF1_VFTP routine to NDF1_CHFTP to
*        avoid a name clash.
*     29-JAN-1990 (RFWS):
*        Eliminated re-prompting to avoid looping under parameter system
*        error conditions. Re-prompting is now handled by the parameter
*        system alone. Changed error messages to include the parameter
*        name using escape characters.
*     26-FEB-1990 (RFWS):
*        Changed the NDF placeholder data type to 'NDF'.
*     12-MAR-1990 (RFWS):
*        Added missing argument to prologue.
*     22-MAR-1990 (RFWS):
*        Installed initial file size allocation.
*     23-MAR-1990 (RFWS):
*        Added better security against initial file size allocation not
*        reverting to normal after an error during file creation.
*     23-MAR-1990 (RFWS):
*        Re-structured the routine so that the parameter association is
*        cancelled and the data object deleted (if it has already been
*        created) following errors other than "null" and "abort" status
*        values.
*     4-DEC-1991 (RFWS):
*        Removed a temporary fix which reported an error if creation
*        failed. This is now done by HDS.
*     4-NOV-1993 (RFWS):
*        Changed to support foreign format files.
*     9-MAR-1994 (RFWS):
*        Fixed bug: missing argument to NDF1_DCRE.
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
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) FTYPE
      INTEGER NDIM
      INTEGER LBND( * )
      INTEGER UBND( * )

*  Arguments Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZPAR ) NAME ! NDF name string
      CHARACTER * ( NDF__SZTYP ) TYPE ! Numeric data type
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IPAR               ! Parameter table index
      INTEGER IPCB               ! Index to placeholder entry in the PCB
      INTEGER TSTAT              ! Temporary status variable
      LOGICAL CMPLX              ! Whether data type is complex

*.

*  Set an initial value for the INDF argument.
      INDF = NDF__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark the error stack, so that annulling error messages doesn't
*  disturb any pre-existing error stack contents.
      CALL ERR_MARK

*  Find the parameter index in the parameter tables.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )

*  Check the data type and bounds information for validity.
      CALL NDF1_CHFTP( FTYPE, TYPE, CMPLX, STATUS )
      CALL NDF1_VBND( NDIM, LBND, UBND, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop until a valid NDF structure has been created or a
*  non-recoverable error occurs.
 1       CONTINUE                ! Start of 'DO WHILE' loop

*  Obtain the NDF name via the parameter.
         CALL SUBPAR_GETNAME( IPAR, NAME, STATUS )
         IACB = 0
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Create a placeholder entry for the object in the PCB and use this to
*  create the new NDF.
            CALL NDF1_PLFOR( DAT__ROOT, NAME, IPCB, STATUS )
            CALL NDF1_DCRE( FTYPE, NDIM, LBND, UBND, IPCB, IACB,
     :                      STATUS )

*  Annul the PCB entry when done, erasing the object if there has been
*  an error.
            CALL NDF1_ANNPL( ( STATUS .NE. SAI__OK ), IPCB, STATUS )

*  If this failed, then the user must be re-prompted. Report contextual
*  information and flush any error messages.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'PARAM', PARAM )
               CALL ERR_REP( 'NDF_CREAT_CTX',
     : 'NDF_CREAT: Unable to create a new simple NDF via the ' //
     : '''%^PARAM'' parameter.',
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
      CALL NDF1_EXPID( IACB, INDF, STATUS )

*  If an error occurred, then annul any ACB entry which might have been
*  acquired.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF1_ANL( IACB, STATUS )
      END IF

*  If an error occurred, then classify it...

*  If an "abort" was requested, then annul any error messages and issue
*  an appropriate new one.
      IF ( STATUS .EQ. PAR__ABORT ) THEN
         TSTAT = STATUS
         CALL ERR_ANNUL( TSTAT )
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_CREAT_ABT',
     :   'Aborted creation of a new NDF structure via the ' //
     :   '''%^PARAM'' parameter.', STATUS )

*  If an "null" NDF was specified, then annul any error messages and
*  issue an appropriate new one.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         TSTAT = STATUS
         CALL ERR_ANNUL( TSTAT )
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_CREAT_NULL',
     :   'Null NDF structure specified for the ''%^PARAM'' ' //
     :   'parameter.', STATUS )

*  For other errors, add context information and call the error tracing
*  routine.
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_CREAT_ERR',
     :   'NDF_CREAT: Error creating a new simple NDF via the ' //
     :   '''%^PARAM'' parameter.', STATUS )
         CALL NDF1_TRACE( 'NDF_CREAT', STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
