      SUBROUTINE NDF_CREP( PARAM, FTYPE, NDIM, UBND, INDF, STATUS )
*+
*  Name:
*     NDF_CREP

*  Purpose:
*     Create a new primitive NDF via the ADAM parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_CREP( PARAM, FTYPE, NDIM, UBND, INDF, STATUS )

*  Description:
*     The routine creates a new primitive NDF via the ADAM parameter
*     system, associates it with a parameter, and returns an NDF
*     identifier for it.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the ADAM parameter.
*     FTYPE = CHARACTER * ( * ) (Given)
*        Type of the NDF's DATA component (e.g. '_REAL'). Note that
*        complex types are not permitted when creating a primitive NDF.
*     NDIM = INTEGER (Given)
*        Number of NDF dimensions.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper pixel-index bounds of the NDF (the lower bound of each
*        dimension is taken to be 1).
*     INDF = INTEGER (Returned)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine creates a "primitive" NDF, i.e. one whose array
*     components will be stored in "primitive" form by default (see
*     SGP/38).
*     -  The data type of the DATA component is specified via the FTYPE
*     argument and the data type of the VARIANCE component defaults to
*     the same value. These data types may be set individually with the
*     NDF_STYPE routine if required.
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
*     20-FEB-1990 (RFWS):
*        Original, derived from the NDF_CREAT routine.
*     26-FEB-1990 (RFWS):
*        Changed the NDF placeholder data type to 'NDF'.
*     12-MAR-1990 (RFWS):
*        Added missing argument to prologue.
*     23-MAR-1990 (RFWS):
*        Installed initial file size allocation.
*     17-APR-1990 (RFWS):
*        Fixed bug in error status checking which caused a spurious
*        error message to appear when no error had occurred.
*     4-DEC-1991 (RFWS):
*        Removed a temporary fix which reported an error if creation
*        failed. This is now done by HDS.
*     4-NOV-1993 (RFWS):
*        Changed to support foreign format files.
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
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) FTYPE
      INTEGER NDIM
      INTEGER UBND( * )

*  Arguments Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZPAR ) NAME ! NDF name string
      CHARACTER * ( NDF__SZTYP ) TYPE ! Numeric data type
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IPAR               ! Parameter table index
      INTEGER IPCB               ! Index to placeholder entry in the PCB
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
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

*  Check the data type information for validity. Report an error if a
*  complex data type has been specified.
      CALL NDF1_CHFTP( FTYPE, TYPE, CMPLX, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( CMPLX ) THEN
            STATUS = NDF__FTPIN
            CALL MSG_SETC( 'BADTYPE', FTYPE )
            CALL ERR_REP( 'NDF_CREP_TYPE',
     : 'The complex type ''^BADTYPE'' is not valid for a primitive ' //
     : 'NDF (possible programming error).',
     :                    STATUS )
         END IF
      END IF

*  Check the NDF bounds for validity.
      IF ( STATUS .EQ. SAI__OK ) THEN
         DO 1 I = 1, MIN( NDIM, NDF__MXDIM )
            LBND( I ) = 1
1        CONTINUE
         CALL NDF1_VBND( NDIM, LBND, UBND, STATUS )
      END IF
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop until a valid NDF structure has been created or a
*  non-recoverable error occurs.
 2       CONTINUE                ! Start of 'DO WHILE' loop

*  Obtain the NDF name via the parameter.
         CALL SUBPAR_GETNAME( IPAR, NAME, STATUS )
         IACB = 0
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Create a placeholder entry for the object in the PCB and use this to
*  create the new NDF.
            CALL NDF1_PLFOR( DAT__ROOT, NAME, IPCB, STATUS )
            CALL NDF1_DCREP( FTYPE, NDIM, UBND, IPCB, IACB, STATUS )

*  Annul the PCB entry when done, erasing the object if there has been
*  an error.
            CALL NDF1_ANNPL( ( STATUS .NE. SAI__OK ), IPCB, STATUS )

*  If this failed, then the user must be re-prompted. Report contextual
*  information and flush any error messages.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'PARAM', PARAM )
               CALL ERR_REP( 'NDF_CREP_CTX',
     : 'NDF_CREP: Unable to create a new primitive NDF via the ' //
     : '''%^PARAM'' parameter.',
     :                       STATUS )
               CALL ERR_FLUSH( STATUS )

*  Cancel the parameter association, annulling any further error
*  messages this may generate.
               CALL SUBPAR_CANCL( IPAR, STATUS )
               CALL ERR_ANNUL( STATUS )

*  Return to re-prompt.
               GO TO 2
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
         CALL ERR_REP( 'NDF_CREP_ABT',
     :   'Aborted creation of a new NDF structure via the ' //
     :   '''%^PARAM'' parameter.', STATUS )

*  If an "null" NDF was specified, then annul any error messages and
*  issue an appropriate new one.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         TSTAT = STATUS
         CALL ERR_ANNUL( TSTAT )
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_CREP_NULL',
     :   'Null NDF structure specified for the ''%^PARAM'' ' //
     :   'parameter.', STATUS )

*  For other errors, add context information and call the error tracing
*  routine.
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'PARAM', PARAM )
         CALL ERR_REP( 'NDF_CREP_ERR',
     :   'NDF_CREP: Error creating a new primitive NDF via the ' //
     :   '''%^PARAM'' parameter.', STATUS )
         CALL NDF1_TRACE( 'NDF_CREP', STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
