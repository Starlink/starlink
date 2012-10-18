      SUBROUTINE NDF_HDEF( INDF, APPN, STATUS )
*+
*  Name:
*     NDF_HDEF

*  Purpose:
*     Write default history information to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HDEF( INDF, APPN, STATUS )

*  Description:
*     The routine writes default information about the current
*     application to the history component of an NDF, creating a new
*     history record if necessary.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     APPN = CHARACTER * ( * ) (Given)
*        Name of the current application. This will only be used if a
*        new history record is created by this routine, otherwise it is
*        ignored. If a blank value is given, then a suitable default
*        will be used instead.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The values stored in the history component for any program
*     parameters that have not yet been accessed by the program at the
*     time this routine is called may not be correct. The value left by
*     the previous invocation of the program will be used, if it is stored
*     in the program's parameter file. If there is no value for the
*     parameter in the parameter file, a value of "<not yet accessed>"
*     will be stored for the parameter in the history component. For this
*     reason, this routine should usually be called once all program
*     parameters have been accessed.
*     -  Default history information will normally be provided
*     automatically by the NDF_ system when an NDF is released, so a
*     call to this routine is not usually required. It is provided so
*     that premature writing of default history information can be
*     forced in cases where additional text will then be appended to it
*     (using NDF_HPUT).
*     -  It is expected that the APPN argument will usually be left
*     blank. A non-blank value should normally only be given if a more
*     complete identification of the current application can be given
*     than is supplied by default.
*     -  This routine will return without action if (a) there is no
*     history component present in the NDF, (b) the NDF's history
*     update mode is currently 'DISABLED', (c) default history
*     information has already been written to the NDF, or (d) a
*     previous call has been made to NDF_HPUT specifying that default
*     history information is to be replaced.

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     17-MAY-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) APPN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  If OK, check that WRITE access is available to the NDF.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL NDF1_CHACC( IACB, 'WRITE', STATUS )

*  Obtain an index to the data object entry in the DCB and write the
*  default history information.
         IDCB = ACB_IDCB( IACB )
         CALL NDF1_HWDEF( IDCB, APPN, STATUS )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HDEF_ERR',
     :   'NDF_HDEF: Error writing default history information to ' //
     :   'an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_HDEF', STATUS )
      END IF

      END
