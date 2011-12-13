      SUBROUTINE NDF_MTYPE( TYPLST, INDF1, INDF2, COMP, ITYPE, DTYPE,
     :                      STATUS )
*+
*  Name:
*     NDF_MTYPE

*  Purpose:
*     Match the types of the array components of a pair of NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_MTYPE( TYPLST, INDF1, INDF2, COMP, ITYPE, DTYPE, STATUS )

*  Description:
*     The routine matches the types of the array components of a pair
*     of NDFs, selecting a numeric type which an application may use to
*     process these components. It also returns the type which should
*     be used for storing the result of this processing.

*  Arguments:
*     TYPLST = CHARACTER * ( * ) (Given)
*        A comma-separated list of the numeric types which the
*        application can process explicitly; e.g. '_INTEGER,_REAL'. The
*        first type which has sufficient precision will be selected
*        from this list, so they should normally be given in order of
*        increasing computational cost.
*     INDF1 = INTEGER (Given)
*        Identifier for the first NDF whose type is to be matched.
*     INDF2 = INTEGER (Given)
*        Identifier for the second NDF.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component whose type is to be
*        considered.
*     ITYPE = CHARACTER * ( * ) (Returned)
*        Numeric type which the application should use to process the
*        NDF components. This value is returned as an upper case
*        character string of maximum length NDF__SZTYP. Its value is
*        the first entry in the TYPLST list to which the NDF array
*        components may be converted without unnecessary loss of
*        information.
*     DTYPE = CHARACTER * ( * ) (Returned)
*        Data type required to hold the result of processing the NDF
*        array components. This result is returned as an upper case
*        character string of maximum length NDF__SZFTP. It is intended
*        to be used as input to the NDF_STYPE routine to set the type
*        of the output NDF component into which the result will be
*        written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be
*     supplied, in which case the results returned by this routine will
*     take account of the types of all the specified components in both
*     NDFs.
*     -  Matching of the type of a single NDF to an application may be
*     performed by supplying the same identifier value for both the
*     INDF1 and INDF2 arguments. There is no extra cost in doing this.
*     -  If the TYPLST argument does not specify any type to which the
*     NDF components may be converted without loss of information, then
*     the routine will return the highest precision type which is
*     available. An error will be reported, however, and STATUS will be
*     set to NDF__TYPNI (type not implemented).
*     -  The constants NDF__SZTYP and NDF__SZFTP are defined in the
*     include file NDF_PAR. The error code NDF__TYPNI is defined in the
*     include file NDF_ERR.

*  Algorithm:
*     -  Store the identifier of the first NDF in an array.
*     -  Also store the second, if different. Otherwise, process only
*     one.
*     -  Match the types of the NDF component(s) to the application.
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     29-JAN-1990 (RFWS):
*        Original version.
*     13-FEB-1990 (RFWS):
*        Revised the prologue.
*     16-MAR-1992 (RFWS):
*        Fixed wrong type declaration for the ITYPE and DTYPE
*        arguments.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      CHARACTER * ( * ) TYPLST
      INTEGER INDF1
      INTEGER INDF2
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      CHARACTER * ( * ) ITYPE
      CHARACTER * ( * ) DTYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER N                  ! Number of different NDFs to match
      INTEGER NDFS( 2 )          ! Array of NDF identifiers

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the first identifier in the NDFS array.
      NDFS( 1 ) = INDF1

*  Also store the second, if different.
      IF ( INDF2 .NE. INDF1 ) THEN
         NDFS( 2 ) = INDF2
         N = 2

*  Otherwise, just process one.
      ELSE
         N = 1
      END IF

*  Match the types of the array component(s) to the application.
      CALL NDF1_MTYP( TYPLST, N, NDFS, COMP, ITYPE, DTYPE, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_MTYPE_ERR',
     :   'NDF_MTYPE: Error matching the types of the array ' //
     :   'components of a pair of NDFs.', STATUS )
         CALL NDF1_TRACE( 'NDF_MTYPE', STATUS )
      END IF

      END
