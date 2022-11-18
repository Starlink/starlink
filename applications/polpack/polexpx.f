      SUBROUTINE POLEXPX( STATUS )
*+
*  Name:
*     POLEXPX

*  Purpose:
*     Copies information from the POLPACK extension to named FITS keywords.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLEXPX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application is not for general use. It is a version of POLEXP
*     which is used within the polexp.sh script to export POLPACK
*     information during on-thr-fly data conversion. In this context, it
*     is known that only a single input file will be specified, and that
*     it will be a native NDF. We can therefore make big speed gains by
*     using NDF directtly instead of indirectly through the NDG library.
*
*     The default conversion table described in POLIMP is used.

*  Usage:
*     polexpx in

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input (native) NDF.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-MAY-1999 (DSB):
*        Original version.
*     22-SEP-2004 (TIMJ):
*        Use CNF_PVAL
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NITEM              ! The number of POLPACK extension items
      PARAMETER( NITEM = 9 )

*  Local Variables:
      CHARACTER CARD*80                    ! A FITS header card
      CHARACTER CELLOC*(DAT__SZLOC)        ! Locator to array cell
      CHARACTER COMMNT( NITEM )*80         ! Comments for FITS keywords
      CHARACTER FTNAM( NITEM )*8           ! Names of FITS keywords
      CHARACTER FTSLOC*(DAT__SZLOC)        ! Locator to FITS block
      CHARACTER ITNAM( NITEM )*(DAT__SZNAM)! Names of POLPACK extension items
      CHARACTER POLLOC*(DAT__SZLOC)        ! Locator to POLPACK extension
      INTEGER DOCVT              ! 0 to suppress NDF data conversion
      INTEGER EL                 ! No. of elements mapped
      INTEGER ICARD              ! Index of next card to be written
      INTEGER INDF               ! NDF identifier
      INTEGER IPFITS             ! Pointer to FITS block
      INTEGER IWCS               ! Pointer to WCS FrameSet
      INTEGER J                  ! Index of current extension item
      INTEGER NCARD              ! Max. no. of cards in FITS extension
      INTEGER UCARD              ! No. of cards in final FITS extension
      LOGICAL NEW                ! Was a new card added?
      LOGICAL THERE              ! Does FITS extension exist?
      REAL    ANGROT             ! ACW angle from X axis to ref dir. in degrees

      DATA FTNAM / 'PPCKFILT', 'PPCKIMID', 'PPCKWPLT',
     :             'PPCKRAY',  'PPCKSTOK', 'PPCKT', 'PPCKEPS',
     :             'PPCKANLA', 'PPCKVERS' /,

     :     ITNAM / 'FILTER',   'IMGID',    'WPLATE',
     :             'RAY',      'STOKES', 'T', 'EPS', 'ANLANG',
     :             'VERSION' /,

     :     COMMNT / 'POLPACK: Filter',
     :              'POLPACK: Image identifier',
     :              'POLPACK: Waveplate position (degs)',
     :              'POLPACK: Ray (O or E)',
     :              'POLPACK: Identifiers for planes of data',
     :              'POLPACK: Analyser transmission factor',
     :              'POLPACK: Analyser efficiency factor',
     :              'POLPACK: Analyser angle',
     :              'POLPACK: Version number'/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the input NDF, ensuring no data conversion takes place.
      CALL NDF_GTUNE( 'DOCVT', DOCVT, STATUS )
      CALL NDF_TUNE( 0, 'DOCVT', STATUS )
      CALL NDF_ASSOC( 'IN', 'UPDATE', INDF, STATUS )

*  Get a locator to the POLPACK extension.
      CALL NDF_XLOC( INDF, 'POLPACK', 'READ', POLLOC, STATUS )

*  See whether or not there is a FITS extension.
      CALL NDF_XSTAT( INDF, 'FITS', THERE, STATUS )
      IF ( .NOT. THERE ) THEN

*  If not, create one, Set its size to hold all the required POLPACK
*  items, plus an END card and a PPCKANGR card.
         NCARD = NITEM + 2
         CALL NDF_XNEW( INDF, 'FITS', '_CHAR*80', 1, NCARD, FTSLOC,
     :                  STATUS )

*  Save the index within the list at which the first new card should be
*  written.
         ICARD = 1

*  Map the FITS extension for WRITE access.
         CALL DAT_MAPV( FTSLOC, '_CHAR*80', 'WRITE', IPFITS, EL,
     :                  STATUS )

      ELSE

*  If the FITS extension exists, find it.
         CALL NDF_XLOC( INDF, 'FITS', 'UPDATE', FTSLOC, STATUS )

*  See how many cards are in it.
         CALL DAT_SIZE( FTSLOC, NCARD, STATUS )

*  See if the last card in the FITS extension is an END card. If so, the
*  last card will be over-written by the first POLPACK card. Otherwise, new
*  cards will be added after the last card. Also set the new size required
*  for the FITS extension, including the new cards being added, and an
*  END card if one does not already exist.
         CALL DAT_CELL( FTSLOC, 1, NCARD, CELLOC, STATUS )
         CALL DAT_GET0C( CELLOC, CARD, STATUS )
         CALL DAT_ANNUL( CELLOC, STATUS )

         IF( CARD(:8) .EQ. 'END     ' ) THEN
            ICARD = NCARD
            NCARD = NCARD + NITEM + 1
         ELSE
            ICARD = NCARD + 1
            NCARD = NCARD + NITEM + 2
         END IF

*  Increase the size of the FITS extension to make room for the new cards.
         CALL DAT_ALTER( FTSLOC, 1, NCARD, STATUS )

*  Map the FITS extension for UPDATE access.
         CALL DAT_MAPV( FTSLOC, '_CHAR*80', 'UPDATE', IPFITS, EL,
     :                  STATUS )

      END IF

*  Check the pointer to the FITS array can be used.
      IF( STATUS .EQ. SAI__OK ) THEN

*  Assume for the moment that no existing keywords will be over-written.
*  This means that all the cards written by this routine will be new and
*  so the size of the FITS header at the end will be the size noted above.
         UCARD = NCARD

*  Loop round, copying each POLPACK extension item to the FITS extension.
*  Include an extra trailing argument giving the length of the mapped
*  characters. This is necessary on Unix when using %VAL to pass mapped
*  character arrays. The mapped array must be the first character
*  variable in the argument list, so that subseqent string lengths get
*  added after the "%val( 80 )".
         DO J = 1, NITEM
            CALL POL1_SETFT( ICARD - 1, %VAL( CNF_PVAL( IPFITS ) ),
     :                       ITNAM( J ), FTNAM( J ), POLLOC, ICARD,
     :                       COMMNT( J ), NEW, STATUS,
     :                       %VAL( CNF_CVAL( 80 ) ) )

*  If a new card was added, increment the index at which the next card will
*  be written.
            IF( NEW ) THEN
               ICARD = ICARD + 1

*  If an existing card was replaced, reduce the number of FITS cards
*  used.
            ELSE
               UCARD = UCARD - 1
            END IF

         END DO

*  Now do the ANGROT item. This has to be handled separately since the
*  keyword value is derived from the POLANAL Frame in the WCS FrameSet,
*  instead of a component of the POLPACK extension. First get a pointer
*  to the FrameSet.
         CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Get the ANGROT value.
         ANGROT = 0.0
         CALL POL1_GTANG( INDF, 0, IWCS, ANGROT, STATUS )

*  Annul the FrameSet pointer.
         CALL AST_ANNUL( IWCS, STATUS )

*  Store the ANGROT value in the FITS extension as keyword PPCKANGR.
         CALL POL1_STFTR( ICARD - 1, %VAL( CNF_PVAL( IPFITS ) ), ANGROT,
     :                    'PPCKANGR', ICARD, 'POLPACK: X-axis '//
     :                    'to ref. direction in degs', NEW,
     :                    STATUS, %VAL( CNF_CVAL( 80 ) ) )

*  If a new card was added, increment the index at which the next card will
*  be written.
         IF( NEW ) THEN
            ICARD = ICARD + 1

*  If an existing card was replaced, reduce the number of FITS cards
*  used.
         ELSE
            UCARD = UCARD - 1
         END IF

      END IF

*  Unmap FITS block.
      CALL DAT_UNMAP( FTSLOC, STATUS )

*  Ensure the FITS extension to no bigger than it needs to be.
      CALL DAT_ALTER( FTSLOC, 1, UCARD, STATUS )

*  Add an END card to the end of the FITS extension.
      CALL DAT_CELL( FTSLOC, 1, UCARD, CELLOC, STATUS )
      CARD = 'END'
      CALL DAT_PUT0C( CELLOC, CARD, STATUS )
      CALL DAT_ANNUL( CELLOC, STATUS )

*  Release this NDF.
      CALL DAT_ANNUL( POLLOC, STATUS )
      CALL DAT_ANNUL( FTSLOC, STATUS )
      CALL NDF_ANNUL( INDF, STATUS )

*  Re-instate the original DOCVT NDF tuning paerameter value.
      CALL ERR_BEGIN( STATUS )
      CALL NDF_TUNE( DOCVT, 'DOCVT', STATUS )
      CALL ERR_END( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
