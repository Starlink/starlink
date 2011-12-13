      SUBROUTINE NDF_MBND( OPTION, INDF1, INDF2, STATUS )
*+
*  Name:
*     NDF_MBND

*  Purpose:
*     Match the pixel-index bounds of a pair of NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_MBND( OPTION, INDF1, INDF2, STATUS )

*  Description:
*     The routine matches the pixel-index bounds of a pair of NDFs so
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
*     INDF1 = INTEGER (Given and Returned)
*        Identifier for the first NDF whose pixel-index bounds are to
*        be matched.
*     INDF2 = INTEGER (Given and Returned)
*        Identifier for the second NDF to be matched.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If OPTION='PAD' is specified, then the NDF bounds will be
*     matched by "padding"; i.e. each NDF will be extended by selecting
*     the smallest section from it which encompasses all the pixels in
*     both NDFs. In effect, the pixel-index bounds of the two NDFs are
*     "maximised" and the "union" of the two sets of pixels is
*     selected.  Any new pixels introduced into either NDF will be
*     padded with the "bad" value.  If the NDFs have different numbers
*     of dimensions, then the dimensionality of both the returned
*     sections will match the NDF with the higher dimensionality.
*     -  If OPTION='TRIM' is specified, then the NDF bounds will be
*     matched by "trimming"; i.e. each NDF will be restricted in extent
*     by selecting a section from it which encompasses only those
*     pixels which are present in both NDFs. In effect, the pixel-index
*     bounds of the two NDFs are "minimised" and the "intersection" of
*     the two sets of pixels is selected. An error will result if the
*     two NDFs have no pixels in common. If the NDFs have different
*     numbers of dimensions, then the dimensionality of both the
*     returned sections will match the NDF with the lower
*     dimensionality.
*     -  Note that the initial NDF identifier values will be annulled
*     by this routine and replaced with identifiers describing
*     appropriate new sections from the original NDFs. If access to the
*     original data is still required, then the initial identifiers may
*     be cloned with the routine NDF_CLONE before calling this routine.

*  Algorithm:
*     -  Copy the NDF identifiers supplied to the elements of an array.
*     -  Compare the OPTION value with each permitted value in turn,
*     calling the appropriate routine to match the pixel-index bounds of
*     the NDFs.
*     -  If the OPTION value was not recognised, then report an error.
*     -  Return the new identifier values.

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
*     6-FEB-1990 (RFWS):
*        Installed 'TRIM' and 'PAD' options.
*     1-MAR-1990 (RFWS):
*        Removed un-referenced include file.
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

*  Arguments Given and Returned:
      INTEGER INDF1
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER NDFS( 2 )          ! List of NDF identifiers to be matched

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the NDF identifiers supplied to the elements of an array.
      NDFS( 1 ) = INDF1
      NDFS( 2 ) = INDF2

*  Compare the OPTION value with each permitted value in turn (allowing
*  abbreviation), calling the appropriate routine to match the pixel
*  index bounds of the NDFs.

*  PAD option:
*  ==========
      IF ( NDF1_SIMLR( OPTION, 'PAD', NDF__MINAB ) ) THEN
         CALL NDF1_MBNDP( 2, NDFS, STATUS )

*  TRIM option:
*  ===========
      ELSE IF ( NDF1_SIMLR( OPTION, 'TRIM', NDF__MINAB ) ) THEN
         CALL NDF1_MBNDT( 2, NDFS, STATUS )

*  If the OPTION value was not recognised, then report an error.
      ELSE
         STATUS = NDF__BMOIN
         CALL MSG_SETC( 'BADOPT', OPTION )
         CALL ERR_REP( 'NDF_MBND_BAD',
     :   'Invalid matching option ''^BADOPT'' specified (possible ' //
     :   'programming error).', STATUS )
      END IF

*  Return the new identifier values.
      IF ( STATUS .EQ. SAI__OK ) THEN
         INDF1 = NDFS( 1 )
         INDF2 = NDFS( 2 )
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_MBND_ERR',
     :   'NDF_MBND: Error matching the pixel-index bounds of a pair ' //
     :   'of NDFs.', STATUS )
         CALL NDF1_TRACE( 'NDF_MBND', STATUS )
      END IF

      END
