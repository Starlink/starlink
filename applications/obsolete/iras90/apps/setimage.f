      SUBROUTINE SETIMAGE( STATUS )
*+
*  Name:
*     SETIMAGE

*  Purpose:
*     Set selected global properties for an IRAS90 image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETIMAGE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine can be used to change the waveband, units and/or
*     image type of a group of existing IRAS90 images. The new values
*     are copied from other specified IRAS90 images.

*  Usage:
*     SETIMAGE NDF LIKE LIST

*  ADAM Parameters:
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        modified NDFs. See help on "History_in_IRAS90" for more
*        information on history. 
*                                              [current history setting]
*     LIKE = NDF (Read)
*        A group of IRAS90 images from which the new values are to be
*        read. The values from these NDFs are stored in the
*        corresponding NDF specified by parameter NDF.
*     LIST = LITERAL (Read)
*        A group expression giving a list of the items of information
*        which are to be modified in the image specified by parameter
*        NDF. The following items can be specified; UNITS, BAND, TYPE.
*                                                   [UNITS,BAND,TYPE]
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]
*     NDF = NDF (Read and Write)
*        A group of IRAS90 image in which new values are to be stored.

*  Examples:
*     SETIMAGE M51_B3 M51_B4_SIM UNITS,BAND
*        This example causes the waveband and units of image M51_B3 to
*        be set equal to those of image M51_B4_SIM.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JUN-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error values.
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants.
      INCLUDE 'IRI_PAR'          ! IRI_ constants.
      INCLUDE 'I90_DAT'          ! IRAS90 data

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXIT               ! No. of recognised items.
      PARAMETER ( MXIT = 3 )

      INTEGER MXITLN             ! Max. length of an item name.
      PARAMETER ( MXITLN = 5 )

*  Local Variables:
      CHARACTER
     :        INSTR1*(IRI__SZINS),! INSTRM from modified NDF.
     :        INSTR2*(IRI__SZINS),! INSTRM from model NDF.
     :        ITEM*(MXITLN),      ! Current item.
     :        LOC1*(DAT__SZLOC),  ! Locator to modified IMAGE_INFO.
     :        LOC2*(DAT__SZLOC),  ! Locator to model IMAGE_INFO.
     :        TEXT(MXIT)*80,      ! History text.
     :        TYPE1*(IRI__SZTYP), ! TYPE from modified NDF.
     :        TYPE2*(IRI__SZTYP), ! TYPE from model NDF.
     :        UNITS1*(IRI__SZUNI),! UNITS from modified NDF.
     :        UNITS2*(IRI__SZUNI) ! UNITS from model NDF.


      INTEGER
     :        ADDED,             ! No. of items added by last prompt.
     :        BAND1,             ! BAND from modified NDF.
     :        BAND2,             ! BAND from model NDF.
     :        I,                 ! NDF index.
     :        IGRP1,             ! GRP identifier for modified NDFS.
     :        IGRP2,             ! GRP identifier for model NDFS.
     :        IGRP3,             ! GRP identifier for raw items.
     :        IGRP4,             ! GRP identifier for purged items.
     :        INDEX,             ! Item index.
     :        INDF1,             ! NDF identifier for modified NDF.
     :        INDF2,             ! NDF identifier for model NDF.
     :        NIN,               ! No. of input NDFs.
     :        NITEM,             ! No. of items to be changed.
     :        TLEN               ! Length of text string.

      LOGICAL
     :        FLAG               ! True if more items to be given.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the filter level for conditional message output.
      CALL MSG_IFGET( 'MSG_FILTER', STATUS )

*  Get a group containing the names of the NDFs to be modified.
      CALL IRM_RDNDF( 'NDF', 0, 1, '  Give more NDF names...', 
     :                IGRP1, NIN, STATUS )

*  Get a group containing the names of the NDFs from which to read new
*  values.
      CALL IRM_RDNDF( 'LIKE', NIN, NIN, '  Give more NDF names...', 
     :                IGRP2, NIN, STATUS )

*  Create a group to hold the items which are to be modified.
      CALL GRP_NEW( 'ITEMS', IGRP3, STATUS )

*  Ensure that the group is case insensitive.
      CALL GRP_SETCS( IGRP3, .FALSE., STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999 

*  Obtain the items to be modified, storing them in the group
*  just created. Loop until the group expression obtained from the
*  environment does not terminate with a minus sign.
      FLAG = .TRUE.
      DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )
         CALL GRP_GROUP( 'LIST', GRP__NOID, IGRP3, NITEM, ADDED,
     :                    FLAG, STATUS )
      END DO

*  The user may have indicated the end of the group by giving a null
*  value for the parameter. This would normally cause the application to
*  abort, so annul the error in order to prevent this from happening.
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  If there are no items to be modified, abort.
      IF( NITEM .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SETIMAGE_ERR1',
     :                 'SETIMAGE: No items given to be modified',
     :                 STATUS )
         GO TO 999
      END IF

*  Create a group from which duplicated items have been purged.
      CALL GRP_PURGE( IGRP3, IGRP4, STATUS )

*  Abort if an error has occured.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Loop round each NDF to be processed.
      DO I = 1, NIN
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get an NDF identifier for the NDF to be modified.
         CALL NDG_NDFAS( IGRP1, I, 'UPDATE', INDF1, STATUS )

*  Tell the user which NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_OUTIF( MSG__NORM, 'SETIMAGE_MSG1',
     :                   '  Processing ^NDF...', STATUS )

*  Check it is an IRAS90 image, and get its mandatory IMAGE_INFO items.
         CALL IRI_OLD( INDF1, INSTR1, BAND1, TYPE1, UNITS1, LOC1,
     :                 STATUS )

*  Get an NDF identifier for the model NDF.
         CALL NDG_NDFAS( IGRP2, I, 'READ', INDF2, STATUS )

*  Check it is an IRAS90 image, and get its mandatory IMAGE_INFO items.
         CALL IRI_OLD( INDF2, INSTR2, BAND2, TYPE2, UNITS2, LOC2,
     :                 STATUS )
         IF( STATUS .EQ. SAI__OK ) THEN

*  Loop round each item to be modified.
            DO INDEX = 1, NITEM

*  Get the next item, and remove leading blanks.
               CALL GRP_GET( IGRP4, INDEX, 1, ITEM, STATUS )
               CALL CHR_LDBLK( ITEM )
   
*  Store the selected item in the input NDF, create history text, and
*  tell the user what has happened.
               IF( ITEM .EQ. 'UNITS' ) THEN

                  CALL NDF_CPUT( UNITS2, INDF1, 'UNITS', STATUS )

                  CALL MSG_SETC( 'U1', UNITS1 )
                  CALL MSG_SETC( 'U2', UNITS2 )
                  CALL MSG_LOAD( ' ',
     :                          '    Units changed from "^U1" to "^U2"',
     :                           TEXT( INDEX ), TLEN, STATUS )

                  CALL MSG_OUTIF( MSG__NORM, 'SETIMAGE_MSG2', 
     :                            TEXT( INDEX ), STATUS )

               ELSE IF( ITEM .EQ. 'TYPE' ) THEN      
            
                  CALL CMP_PUT0C( LOC1, 'TYPE', TYPE2, STATUS )

                  CALL MSG_SETC( 'T1', TYPE1 )
                  CALL MSG_SETC( 'T2', TYPE2 )
                  CALL MSG_LOAD( ' ',
     :                    '    Image type changed from "^T1" to "^T2"',
     :                           TEXT( INDEX ), TLEN, STATUS )

                  CALL MSG_OUTIF( MSG__NORM, 'SETIMAGE_MSG3', 
     :                            TEXT( INDEX ), STATUS )

               ELSE IF( ITEM .EQ. 'BAND' ) THEN      
            
                  CALL CMP_PUT0I( LOC1, 'BAND', BAND2, STATUS )

                  CALL MSG_SETI( 'B1', I90__WAVEL( BAND1 ) )
                  CALL MSG_SETI( 'B2', I90__WAVEL( BAND2 ) )
                  CALL MSG_LOAD( ' ',
     :            '    Image waveband changed from ^B1 um to ^B2 um',
     :                           TEXT( INDEX ), TLEN, STATUS )

                  CALL MSG_OUTIF( MSG__NORM, 'SETIMAGE_MSG4', 
     :                            TEXT( INDEX ), STATUS )

               ELSE
                  CALL MSG_BLANKIF( MSG__QUIET, STATUS )
                  CALL MSG_SETC( 'IT', ITEM )
                  CALL MSG_OUTIF( MSG__QUIET, 'SETIMAGE_MSG5',
     :                            'WARNING: Cannot modify item "^IT".',
     :                            STATUS )
                  CALL MSG_BLANKIF( MSG__QUIET, STATUS )
               END IF

            END DO

*  Update the history of the modified NDF.
            CALL IRM_HIST( 'HISTORY', INDF1, 'IRAS90:SETIMAGE', NITEM,
     :                     TEXT, STATUS )            

         END IF

*  Release the resources for this pair of input NDFs.
         CALL DAT_ANNUL( LOC1, STATUS )
         CALL DAT_ANNUL( LOC2, STATUS )
         CALL NDF_ANNUL( INDF1, STATUS )
         CALL NDF_ANNUL( INDF2, STATUS )

*  If an error occured processing the current NDF, flush the error.
         IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Process the next NDF.
      END DO

*  Display a blank line.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Store a list of the processed files for use by later applications.
      CALL IRM_LISTN( 'NDFLIST', IGRP1, 'SETIMAGE', STATUS )

*  Delete the groups which holds the item names and NDF names.
 999  CONTINUE

      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )
      CALL GRP_DELET( IGRP3, STATUS )
      CALL GRP_DELET( IGRP4, STATUS )

*  End the NDF context. 
      CALL NDF_END( STATUS )

*  If an error occurred, clear up.
      IF ( STATUS .NE. SAI__OK ) THEN

*  If a null parameter was given or a parameter abort was equested, 
*  annul the error.
         IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )

*  If any other error occurred, then report a contextual message.
         ELSE
            CALL ERR_REP( 'SETIMAGE_ERR2',
     :      'SETIMAGE: Unable to modify global properties of a group '//
     :      'of IRAS90 images.', STATUS )
         END IF

      END IF

      END
