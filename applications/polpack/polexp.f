      SUBROUTINE POLEXP( STATUS )
*+
*  Name:
*     POLEXP

*  Purpose:
*     Copies information from the POLPACK extension to the FITS extension.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLEXP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine creates FITS header cards describing the contents of
*     the POLPACK extensions in a group of NDFs. The header cards are
*     written to the FITS extensions of the NDFs, replacing any existing
*     cards for the same keywords. The keywords used are listed below.
*     Information exported to the FITS extension can be imported back 
*     into the POLPACK extension using POLIMP.
*
*     The main use of this routine is for exporting POLPACK NDFs for use
*     bu other non-NDF based packages. 

*  Usage:
*     POLEXP in 

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        A list of NDF names. The NDF names should be separated by commas 
*        and may include wildcards.
*     NAMELIST = LITERAL (Read)
*        The name of a file to create containing a list of the
*        succesfully processed NDFs. This file can be used when 
*        specifying the input NDFs for subsequent applications. [!]
*     QUIET = _LOGICAL (Read)
*        If FALSE, then each NDF is listed as it is processed. Otherwise,
*        nothing is written to the screen. [FALSE] 

*  FITS Keywords:
*     The following FITS keywords are created. The POLPACK extension item
*     stored in the keyword is shown in parenthesise (see POLIMP for a 
*     description of these extension items):

*           PPCKANGR  (ANGROT)
*           PPCKFILT  (FILTER)
*           PPCKIMID  (IMGID)     
*           PPCKWPLT  (WPLATE)
*           PPCKRAY   (RAY)
*           PPCKSTOK  (STOKES)

*  Examples:
*     POLEXP in=^names.lis
*        This example processes the NDFs listed in the text file
*        "names.lis". The information stored in the POLPACK extension of
*        each is exported to the FITS extension.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-DEC-1997 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'GRP_PAR'          ! GRP parameters

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NITEM              ! The number of POLPACK extension items
      PARAMETER( NITEM = 6 )

*  Local Variables:
      CHARACTER CARD*80                    ! A FITS header card
      CHARACTER CELLOC*(DAT__SZLOC)        ! Locator to array cell
      CHARACTER COMMNT( NITEM )*80         ! Comments for FITS keywords
      CHARACTER FTNAM( NITEM )*8           ! Names of FITS keywords
      CHARACTER FTSLOC*(DAT__SZLOC)        ! Locator to FITS block
      CHARACTER ITNAM( NITEM )*(DAT__SZNAM)! Names of POLPACK extension items
      CHARACTER NDFNAM*(GRP__SZNAM)        ! Name of the NDF being processed
      CHARACTER POLLOC*(DAT__SZLOC)        ! Locator to POLPACK extension
      INTEGER EL                 ! No. of elements mapped
      INTEGER I                  ! Index of current NDF.
      INTEGER ICARD              ! Index of next card to be written
      INTEGER IGRP1              ! Input NDF group identifier
      INTEGER IGRP2              ! Id for group of names of NDF's processed OK 
      INTEGER INDF               ! NDF identifier
      INTEGER IPFITS             ! Pointer to FITS block
      INTEGER J                  ! Index of current extension item
      INTEGER NCARD              ! Max. no. of cards in FITS extension
      INTEGER NGOOD              ! No. of NDF's processed succesfully
      INTEGER NNDF               ! Number of input NDFs
      INTEGER UCARD              ! No. of cards in final FITS extension
      LOGICAL NEW                ! Was a new card added?
      LOGICAL QUIET              ! Run silently?
      LOGICAL THERE              ! Does FITS extension exist?

      DATA FTNAM / 'PPCKANGR', 'PPCKFILT', 'PPCKIMID', 'PPCKWPLT', 
     :             'PPCKRAY',  'PPCKSTOK' /,

     :     ITNAM / 'ANGROT',   'FILTER',   'IMGID',    'WPLATE', 
     :             'RAY',      'STOKES' /,

     :     COMMNT / 'POLPACK: X-axis -> analyser angle (degs)',
     :              'POLPACK: Filter',
     :              'POLPACK: Image identifier',
     :              'POLPACK: Waveplate position (degs)',
     :              'POLPACK: Ray (O or E)',
     :              'POLPACK: Identifiers for planes of data'/

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if we are running quietly.
      CALL PAR_GET0L( 'QUIET', QUIET, STATUS )

*  Access a group of NDFs for processing.
      CALL NDF_BEGIN
      CALL RDNDF( 'IN', 0, 1, '  Give more image names...', IGRP1, 
     :            NNDF, STATUS )

*  Create a group to hold the names of the NDFs which were processed
*  succesfully.
      CALL GRP_NEW( 'Good NDFs', IGRP2, STATUS )

*  Tell the user how many NDFs there are to process.
      IF( .NOT. QUIET ) THEN
         IF( NNDF .GT. 1 ) THEN
            CALL MSG_SETI( 'N', NNDF )
            CALL MSG_OUT( ' ', '  ^N input images to process... ',
     :                    STATUS )
         ELSE IF( NNDF .EQ. 1 ) THEN
            CALL MSG_OUT( ' ', '  1 input image to process... ',STATUS )
         ELSE
            CALL MSG_OUT( ' ', '  NO input images to process. ',STATUS )
         END IF
   
         CALL MSG_BLANK( STATUS )
      END IF

*  Check that everything is ok so far.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Process each NDF in turn.
      DO 100 I = 1, NNDF

*  Get the name of the NDF now, while we know that no error has occurred.
         CALL GRP_GET( IGRP1, I, 1, NDFNAM, STATUS )

*  Write out name of this NDF.
         IF( .NOT. QUIET ) THEN
            CALL MSG_SETC( 'CURRENT_NDF', NDFNAM )
            CALL MSG_OUT( ' ', '  Processing ''^CURRENT_NDF''',
     :                     STATUS )
         END IF

*  Get the input NDF identifier
         CALL NDG_NDFAS( IGRP1, I, 'UPDATE', INDF, STATUS )

*  Get a locator to the POLPACK extension.
         CALL NDF_XLOC( INDF, 'POLPACK', 'READ', POLLOC, STATUS ) 

*  See whether or not there is a FITS extension.
         CALL NDF_XSTAT( INDF, 'FITS', THERE, STATUS )
         IF ( .NOT. THERE ) THEN

*  If not, create one, Set its size to hold all the required POLPACK
*  items, plus an END card.
            NCARD = NITEM + 1
            CALL NDF_XNEW( INDF, 'FITS', '_CHAR*80', 1, NCARD, FTSLOC, 
     :                     STATUS )

*  Save the index within the list at which the first new card should be
*  written.
            ICARD = 1

*  Map the FITS extension for WRITE access.
            CALL DAT_MAPV( FTSLOC, '_CHAR*80', 'WRITE', IPFITS, EL,
     :                     STATUS )

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
               NCARD = NCARD + NITEM
            ELSE
               ICARD = NCARD + 1
               NCARD = NCARD + NITEM + 1
            END IF

*  Increase the size of the FITS extension to make room for the new cards.
            CALL DAT_ALTER( FTSLOC, 1, NCARD, STATUS ) 

*  Map the FITS extension for UPDATE access.
            CALL DAT_MAPV( FTSLOC, '_CHAR*80', 'UPDATE', IPFITS, EL,
     :                     STATUS )

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
               CALL POL1_SETFT( ICARD - 1, %VAL( IPFITS ), ITNAM( J ), 
     :                          FTNAM( J ), POLLOC, ICARD, 
     :                          COMMNT( J ), NEW, STATUS, %VAL( 80 ) )

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

*  If an error occurred, flush the error and continue to process the next
*  NDF.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )

*  Otherwise, add the name of the NDF to the group of succesfully
*  processed NDFs.
         ELSE
            CALL GRP_PUT( IGRP2, 1, NDFNAM, 0, STATUS )
         END IF

         IF( .NOT. QUIET ) CALL MSG_BLANK( STATUS )

 100  CONTINUE

*  Report an error if no NDFs were processed succesfully.
      CALL GRP_GRPSZ( IGRP2, NGOOD, STATUS )
      IF( NGOOD .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLEXP_ALLBAD', 'None of the input images '//
     :                 'were processed succesfully.', STATUS )
      END IF

*  Write an output list of the NDF names for other applications to use.
      IF ( STATUS .EQ. SAI__OK ) THEN 
         CALL ERR_MARK
         CALL POL1_LNAM( 'NAMELIST', 1, NGOOD, 
     :                   '# POLEXP - NDF name list', IGRP2, .FALSE., 
     :                   STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE
      END IF

*  Break out here if status set BAD.
 99   CONTINUE

*  Release the memory allocations.
      CALL CCD1_MFREE( -1, STATUS )

      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

*  Release NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLEXP_ERR',
     :   'POLEXP: Error exporting FITS information from POLPACK.',
     :   STATUS )
      END IF

      END
