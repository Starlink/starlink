      SUBROUTINE NDF_MBNDN( OPTION, N, NDFS, STATUS )
*+
*  Name:
*     NDF_MBNDN

*  Purpose:
*     Match the pixel-index bounds of a number of NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_MBNDN( OPTION, N, NDFS, STATUS )

*  Description:
*     The routine matches the pixel-index bounds of a number of NDFs so
*     that their array components may be compared pixel-for-pixel
*     during subsequent processing. Matching is performed by selecting
*     an appropriate section from each NDF, the method used to define
*     this section being determined by the value given for the OPTION
*     argument.

*  Arguments:
*     OPTION = CHARACTER * ( * ) (Given)
*        This argument determines how the section to be selected from
*        each NDF is defined: 'PAD' or 'TRIM' (see the Notes section
*        for details). Its value may be abbreviated to 3 characters.
*     N = INTEGER (Given)
*        Number of NDFs whose pixel-index bounds are to be matched.
*     NDFS( N ) = INTEGER (Given and Returned)
*        Array of identifiers for the NDFs to be matched.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If OPTION='PAD' is specified, then the NDF bounds will be
*     matched by "padding"; i.e. each NDF will be extended by selecting
*     the smallest section from it which encompasses all the pixels in
*     all the NDFs. In effect, the pixel-index bounds of the NDFs are
*     "maximised" and the "union" of all N sets of pixels is selected.
*     Any new pixels introduced into an NDF will be padded with the
*     "bad" value.  If the NDFs have different numbers of dimensions,
*     then the dimensionality of all the returned sections will match
*     the NDF with the highest dimensionality.
*     -  If OPTION='TRIM' is specified, then the NDF bounds will be
*     matched by "trimming"; i.e. each NDF will be restricted in extent
*     by selecting a section from it which encompasses only those
*     pixels which are present in all the NDFs. In effect, the
*     pixel-index bounds of the NDFs are "minimised" and the
*     "intersection" of all N sets of pixels is selected. An error will
*     result if the NDFs have no pixels in common. If the NDFs have
*     different numbers of dimensions, then the dimensionality of all
*     the returned sections will match the NDF with the lowest
*     dimensionality.
*     -  Note that the initial NDF identifier values will be annulled
*     by this routine and replaced with identifiers describing
*     appropriate new sections from the original NDFs. If access to the
*     original data is still required, then the initial identifiers may
*     be cloned with the routine NDF_CLONE before calling this routine.

*  Algorithm:
*     -  Compare the OPTION value with each permitted value in turn,
*     calling the appropriate routine to match the pixel index bounds of
*     the NDFs.
*     -  If the OPTION value was not recognised, then report an error.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     20-NOV-1989 (RFWS):
*        Original version.
*     13-FEB-1990 (RFWS):
*        Finished writing prologue.
*     1-MAR-1990 (RFWS):
*        Remove un-referenced include file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) OPTION
      INTEGER N

*  Arguments Given and Returned:
      INTEGER NDFS( N )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Compare the OPTION value with each permitted value in turn (allowing
*  abbreviation), calling the appropriate routine to match the pixel
*  index bounds of the NDFs.

*  PAD option:
*  ==========
      IF ( NDF1_SIMLR( OPTION, 'PAD', NDF__MINAB ) ) THEN
         CALL NDF1_MBNDP( N, NDFS, STATUS )

*  TRIM option:
*  ===========
      ELSE IF ( NDF1_SIMLR( OPTION, 'TRIM', NDF__MINAB ) ) THEN
         CALL NDF1_MBNDT( N, NDFS, STATUS )

*  If the OPTION value was not recognised, then report an error.
      ELSE
         STATUS = NDF__BMOIN
         CALL MSG_SETC( 'BADOPT', OPTION )
         CALL ERR_REP( 'NDF_MBNDN_BAD',
     :   'Invalid matching option ''^BADOPT'' specified (possible ' //
     :   'programming error).', STATUS )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_MBNDN_ERR',
     :   'NDF_MBNDN: Error matching the pixel-index bounds of a ' //
     :   'number of NDFs.', STATUS )
         CALL NDF1_TRACE( 'NDF_MBNDN', STATUS )
      END IF

      END
