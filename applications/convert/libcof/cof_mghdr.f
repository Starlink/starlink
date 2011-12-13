      SUBROUTINE COF_MGHDR( NDF1, NDF2, STATUS )
*+
*  Name:
*     COF_MGHDR

*  Purpose:
*     Merges the FITS airlocks of two NDFs

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_MGHDR( NDF1, NDF2, STATUS )

*  Description:
*     This routine serves NDF2FITS.  It merges the FITS airlocks of
*     two NDFs in the first NDF supplied.  The routine first stores any
*     headers in the first NDF airlock.  Then it replaces the first
*     airlock with the second NDF's FITS airlock.  It finally inserts
*     any original headers from the first NDF just before the END card,
*     or appendss the headers should there be no END card.

*  Arguments:
*     NDF1 = INTEGER (Given)
*        The identifier of the primary NDF that will be updated with
*        the merged FITS airlocks.  This NDF must have update access.
*     NDF2 = INTEGER (Given)
*        The identifier of the second NDF whose FITS airlock is to be
*        merged.  This will usually be a header-only NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is no check for duplicated keywords.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2006 April 6 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS public constants

*  Arguments Given:
      INTEGER NDF1
      INTEGER NDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER   SZFITS           ! Size of FITS card-image
      PARAMETER( SZFITS = 80 )

*  Local Variables:
      CHARACTER * ( SZFITS ) CARD ! Header card
      CHARACTER * ( SZFITS ) ECARD ! Final, possibly END, card
      LOGICAL ENDPRS             ! END card present in NDF2 airlock?
      CHARACTER * ( DAT__SZLOC ) FALOC1 ! Locator to FITS airlock, NDF1
      CHARACTER * ( DAT__SZLOC ) FALOC2 ! Locator to FITS airlock, NDF2
      LOGICAL FAPRS1             ! First NDF has FITS extension?
      LOGICAL FAPRS2             ! Second NDF has FITS extension?
      CHARACTER * ( DAT__SZLOC ) HLOC ! Locator to card in first NDF
      CHARACTER * ( DAT__SZLOC ) HLOC2 ! Locator to card to be copied
      INTEGER I                  ! Loop counter
      INTEGER ICARD              ! Card index
      CHARACTER * ( DAT__SZLOC ) MORLOC ! Locator to MORE, NDF1
      INTEGER NCARD              ! Total number of header cards
      INTEGER NCARD1             ! Number of header cards in NDF1
      INTEGER NCARD2             ! Number of header cards in NDF2
      CHARACTER * ( DAT__SZLOC ) TFALOC ! Locator to temporary airlock
                                 ! copy from first NDF
      LOGICAL VALID              ! NDF identifier is valid?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the NDF identifiers.
*  =============================
      CALL NDF_VALID( NDF1, VALID, STATUS )

*  Report an error if the identifier is not valid.
      IF ( .NOT. VALID ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_MGHDR_INVNDF',
     :     'COF_MGHDR: The identifier to the first NDF is invalid. '/
     :     /'(Probable programming error.)', STATUS )
         GOTO 999
      END IF

      CALL NDF_VALID( NDF2, VALID, STATUS )

*  Report an error if the identifier is not valid.
      IF ( .NOT. VALID ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_MGHDR_INVNDF',
     :     'COF_MGHDR: The identifier to the second NDF is invalid. '/
     :     /'(Probable programming error.)', STATUS )
         GOTO 999
      END IF

*  Test existence of FITS airlocks.
*  ================================

*  Check whether or not a FITS extension is present in the second NDF.
*  We can return if there's nothing to do.
      CALL NDF_XSTAT( NDF2, 'FITS', FAPRS2, STATUS )
      IF ( .NOT. FAPRS2 ) GOTO 999

*  Check whether or not a FITS extension is present.
      CALL NDF_XSTAT( NDF1, 'FITS', FAPRS1, STATUS )

*  Shuffle the airlocks.
*  =====================

*  Find the FITS extension and copy it to a temporary extension.
*  Obtain the number of header cards too.
      NCARD1 = 0
      IF ( FAPRS1 ) THEN

         CALL NDF_XLOC( NDF1, 'FITS', 'READ', FALOC1, STATUS )
         CALL DAT_SIZE( FALOC1, NCARD1, STATUS )
         CALL DAT_PAREN( FALOC1, MORLOC, STATUS )
         CALL DAT_RENAM( FALOC1, 'FITSTMP', STATUS )

*  We have finished with this locator, but obtain the new locator
*  to the headers.
         CALL DAT_ANNUL( FALOC1, STATUS )
         CALL DAT_FIND( MORLOC, 'FITSTMP', TFALOC, STATUS )

*  Remove the original airlock.
         CALL NDF_XDEL( NDF1, 'FITS', STATUS )
      END IF

*  Copy the second NDF's FITS airlock into the first NDF's FITS airlock.
*  Obtain the number of cards too.
      CALL NDF_XLOC( NDF2, 'FITS', 'READ', FALOC2, STATUS )
      CALL DAT_SIZE( FALOC2, NCARD2, STATUS )
      CALL DAT_COPY( FALOC2, MORLOC, 'FITS', STATUS )

*  Remove the redundant locator.
      CALL DAT_ANNUL( FALOC2, STATUS )

*  Copy the other headers into a merged airlock.
*  =============================================

*  Since the number of headers should be small in the first
*  NDF, we shall add them one by one.
      IF ( NCARD1 .GT. 0 ) THEN

*  Obtain the locator to the new FITS airlock in the first NDF.
         CALL NDF_XLOC( NDF1, 'FITS', 'UPDATE', FALOC1, STATUS )

*  See if the last card image is the END card.
         CALL DAT_CELL( FALOC1, 1, NCARD2, HLOC, STATUS )
         CALL DAT_GET0C( HLOC, ECARD, STATUS )
         ENDPRS = ECARD( 1:4 ) .EQ. 'END '

*  Set the index of the card where we shall insert the additional
*  cards.
         IF ( ENDPRS ) THEN
            ICARD = NCARD2
         ELSE
            ICARD = NCARD2 + 1
         END IF

*  Enlarge the airlock to accommodate the additional headers.
*  the ICARD takes care of the extra END card.
         NCARD = NCARD1 + ICARD
         CALL DAT_ALTER( FALOC1, 1, NCARD, STATUS )

*  Copy the original headers of the first NDF from the temporary
*  extension
         DO I = 1, NCARD1
            CALL DAT_CELL( TFALOC, 1, I, HLOC2, STATUS )
            CALL DAT_GET0C( HLOC2, CARD, STATUS )
            CALL DAT_ANNUL( HLOC2, STATUS )

            CALL DAT_CELL( FALOC1, 1, ICARD, HLOC, STATUS )
            CALL DAT_PUT0C( HLOC, CARD, STATUS )
            CALL DAT_ANNUL( HLOC, STATUS )

            ICARD = ICARD + 1
         END DO

*  Always end with an END card.
         CALL DAT_CELL( FALOC1, 1, ICARD, HLOC, STATUS )
         IF ( ENDPRS ) THEN
            CALL DAT_PUT0C( HLOC, ECARD, STATUS )
         ELSE
            CALL DAT_PUT0C( HLOC, 'END      ', STATUS )
         END IF
         CALL DAT_ANNUL( HLOC, STATUS )

*  Remove the original first-NDF airlock.
         CALL NDF_XDEL( NDF1, 'FITSTMP', STATUS )
      END IF

* Tidy.
      CALL DAT_ANNUL( FALOC1, STATUS )

  999 CONTINUE

      END
