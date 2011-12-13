      SUBROUTINE CCD1_RESTA( FTYPES, NNDF, GIDIN, ORIG, DEBICR, DARKCR,
     :                       FLASCR, FLATCR, VALID, STATUS )
*+
*  Name:
*     CCD1_RESTA

*  Purpose:
*     Remove NDFs which have been processed from valid list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      SUBROUTINE CCD1_RESTA( FTYPES, NNDF, GIDIN, ORIG, DEBICR, DARKCR,
*                             FLASCR, FLATCR, VALID, STATUS )

*  Description:
*     This routine attempts to remove NDFs from a list which have
*     already been processed in some way and are represented by
*     another occurence of the same data. Typically this would be useful
*     when a automated reduction has failed (due to a lack of resources)
*     and it is expected that a new reduction should start from the
*     place that the last failed. The way that later occurences of the
*     data are identified is by looking for the name of the original NDF
*     in the extension of the later NDFs and by using the information
*     which indicates how the data has been processed. It is assumed
*     that the "standard" application route has been used
*
*        MAKEBIAS, DEBIAS, MAKECAL(DARK), CALCOR(DARK), MAKECAL(FLASH),
*        CALCOR(FLASH), MAKEFLAT, FLATCOR
*
*     So that the NDF which is furthest along this chain will be the
*     starting point for a new reduction.

*  Arguments:
*     FTYPES( 2, NNDF ) = CHARACTER * ( * ) (Given)
*        List of the input NDF frame and filter types.
*     NNDF = INTEGER (Given)
*        Number of input NDFs.
*     GIDIN = INTEGER (Given)
*        GRP identifier to the group of input NDFs that the information
*        has been derived from.
*     ORIG( NNDF ) = CHARACTER * ( * ) (Given)
*        The names of the "original" NDFs which started a processing
*        chain. This name is passed through applications unchanged.
*     DEBICR( NNDF ) = LOGICAL (Given)
*        Whether the corresponding NDF has been debiassed or not.
*     DARKCR( NNDF ) = LOGICAL (Given)
*        Whether the corresponding NDF has been dark corrected or not.
*     FLASCR( NNDF ) = LOGICAL (Given)
*        Whether the corresponding NDF has been flash corrected or not.
*     FLATCR( NNDF ) = LOGICAL (Given)
*        Whether the corresponding NDF has been flatfielded or not.
*     VALID( NNDF ) = LOGICAL (Given and Returned)
*        Mask of which entries in FTYPES are to be used. On exit any
*        extra NDFs which should be ignored are also flagged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-FEB-1994 (PDRAPER):
*        Original version.
*     23-AUG-1994 (PDRAPER):
*        Modified to use ranks as a test for precedence in the
*        processing chain.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Arguments Given:
      INTEGER NNDF
      CHARACTER * ( * ) FTYPES( 2, NNDF )
      INTEGER GIDIN
      CHARACTER * ( * ) ORIG( NNDF )
      LOGICAL DEBICR( NNDF )
      LOGICAL DARKCR( NNDF )
      LOGICAL FLASCR( NNDF )
      LOGICAL FLATCR( NNDF )

*  Arguments Given and Returned:
      LOGICAL VALID( NNDF )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of NDF
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER IRANK              ! Rank of I'th NDf
      INTEGER JRANK              ! Rank of J'th NDf
      INTEGER KEEP               ! The position of NDF selected to keep

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Look for all the occurences of the original file names.  The
*  precedence rule for processing is:- debicr - darkcr - flascr -
*  flatcr.  The highest ranked NDF in a group of NDFs with the same
*  'Original' file name and the same types etc. will be chosen as the
*  current representative. Using ranks based on a power of two allows
*  finer discrimination between divergent processing chains (say if two
*  NDFs have been flatfielded, and are the same then either the first
*  one will be chosen or the one which has had the most processing
*  performed to it -- possibly it one has also been dark count
*  corrected or pre-flash corrected -- preserving the NDF with most
*  'entropy' must be correct as this requires the most effort to
*  reproduce). If an NDF has been flatfielded exclude it as well and
*  make a report.
      IF ( NNDF .GT. 1 ) THEN
         DO 1 I = 1, NNDF - 1
            IF ( VALID( I ) .AND. ORIG( I ) .NE. ' ' ) THEN

*  Set frame which is highest up processing chain.
               KEEP = I

*  Rank this NDF.
               IRANK = 0
               IF ( FLATCR( I ) ) IRANK = 8
               IF ( FLASCR( I ) ) IRANK = IRANK + 4
               IF ( DARKCR( I ) ) IRANK = IRANK + 2
               IF ( DEBICR( I ) ) IRANK = IRANK + 1

*  Compare this will all other NDFs which are valid.
               DO 2 J = I + 1, NNDF
                  IF ( VALID ( J ) .AND. ORIG( J ) .NE. ' ' ) THEN

*  Scan the list of names looking for a correspondence. This is
*  identified by the same original file name and the same frame type and
*  filter.
                     IF ( ORIG( I ) .EQ. ORIG( J ) .AND.
     :                    FTYPES( 1, I ) .EQ. FTYPES( 1, J ) .AND.
     :                    FTYPES( 2, I ) .EQ. FTYPES( 2, J ) ) THEN

*  Have a match in names, rank this NDF and decide which has the higher
*  processing precedence.
                        JRANK = 0
                        IF ( FLATCR( J ) ) JRANK = 8
                        IF ( FLASCR( J ) ) JRANK = JRANK + 4
                        IF ( DARKCR( J ) ) JRANK = JRANK + 2
                        IF ( DEBICR( J ) ) JRANK = JRANK + 1
                        IF ( JRANK .GT. IRANK ) THEN

*  This NDF has a higher rank to keep it. Also record this rank as one
*  for future matches to beat, rather than the original NDF's rank.
                           VALID( KEEP ) = .FALSE.
                           KEEP = J
                           IRANK = JRANK
                        ELSE

*  Remove this NDF from consideration.
                           VALID( J ) = .FALSE.
                        END IF
                     END IF
                  END IF
 2             CONTINUE

*  Is this flatfielded already?
               IF ( FLATCR( KEEP ) ) THEN

*  Don't process this again. Get the NDF name and continue.
                  VALID( KEEP ) = .FALSE.
                  CALL GRP_GET( GIDIN, KEEP, 1, NDFNAM, STATUS )
                  CALL MSG_SETC( 'NDFNAM', NDFNAM )
                  CALL CCD1_MSG( ' ', '  NDF: ^NDFNAM has already '//
     :'been flatfielded, no further processing will be performed',
     :            STATUS )
               END IF
            END IF
 1       CONTINUE
      END IF
      END
