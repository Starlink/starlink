      SUBROUTINE TRACC3( NDFIN, BSMP, ESMP, BDET, EDET, INSCN, DATA,
     :                   NDISP, OFFSET, DTINDX, SCALE, XLMT, YLMT, IDC,
     :                   PQNAME, PXNAME, PXTYPE, PQCOMM, PHIST, COLOUR,
     :                   CURSOR, CLRBLK, SCNDIR, STATUS )
*+
*  Name:
*     TRACC3

*  Purpose:
*     Assign a quality to samples selected using the cursor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACC3( NDFIN, BSMP, ESMP, BDET, EDET, INSCN, DATA, NDISP,
*                  OFFSET, DTINDX, SCALE, XLMT, YLMT, IDC, PQNAME,
*                  PXNAME, PXTYPE, PQCOMM, PHIST, COLOUR, CURSOR,
*                  CLRBLK, SCNDIR, STATUS )

*  Description:
*     If the graphic device does not have a cursor, or is not block
*     clearable, the routine will just report a message and return.
*     Otherwise, the user is asked to mark two diagonally opposite
*     corners of a box on the data window.  If either are outside the
*     data window, the routine quits.  Otherwise, the traces and
*     samples which are closest to the cursor is found and indicated.
*     A quality name is obtained from the environment. If the quality
*     name is not defined in the input NDF, then a definition of the
*     name is added. All samples which fall within the box defined by
*     the two corners selected using the cursor, are then assigned the
*     quality (all other samples are left unchanged).
*
*     Before calling this routine the AUTOGRAPH grid window should be
*     made to match the current SGS zone world coordinates, that is the
*     grid window should have the bounds (0.0, 1.0, 0.0, 1.0 ).

*  Arguments:
*     NDFIN = INTEGER (Given)
*        The NDF identifier for the input CRDD file. This should have
*        UPDATE access.
*     BSMP = INTEGER (Given)
*        Begin index of the samples of the input CRDD data array.
*     ESMP = INTEGER (Given)
*        End index of the samples of the input CRDD data array.
*     BDET = INTEGER (Given)
*        Begin index of the detector of the input CRDD data array.
*     EDET = INTEGER (Given)
*        End index of the detector of the input CRDD data array.
*     INSCN( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The in-scan distance of each sample of each CRDD data trace.
*     DATA( BSMP : ESMP, BDET : EDET ) = REAL (Given)
*        The data array of the input CRDD file.
*     NDISP = INTEGER (Given)
*        The number of the displayed data trace.
*     OFFSET( NDISP ) = REAL (Given)
*        The offset of each trace in the display.
*     DTINDX( NDISP ) = INTEGER (Given)
*        The detector index of each displayed trace.
*     SCALE( NDISP ) = REAL (Given)
*        The scale factor to produce the scaled data in the display.
*     XLMT( 2 ) = REAL (Given)
*        The in-scan limits of the display.
*     YLMT( 2 ) = REAL (Given)
*        The vertical limits of the display.
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     PQNAME = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the quality name.
*     PXNAME = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the name of the extension
*        in which to store new quality name definitions.
*     PXTYPE = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get the HDS type for the
*        quality names extension.
*     PQCOMM = CHARACTER * ( * ) (Given)
*        The name of the parameter used to get a comment to store with
*        the defined quality.
*     PHIST = CHARACTER * ( * ) (Given)
*        The name of the parameter used to determine if history is to be
*        present in the output NDF.
*     COLOUR = LOGICAL (Given)
*        If true, colour is available on the graphic device.
*     CURSOR = LOGICAL (Given)
*        If true, cursor is available on the graphic device.
*     CLRBLK = LOGICAL (Given)
*        If true, graphic device can clear part of its display surface.
*     SCNDIR = LOGICAL (Given)
*        If true, the scan is from north to south. Otherwise from south
*        to north.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-NOV-1992 (DSB):
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
      INCLUDE 'IRQ_PAR'          ! IRQ_ constants
      INCLUDE 'IRQ_ERR'          ! IRQ_ error constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      INTEGER NDFIN
      INTEGER BSMP
      INTEGER ESMP
      INTEGER BDET
      INTEGER EDET
      REAL INSCN( BSMP : ESMP, BDET : EDET )
      REAL DATA( BSMP : ESMP, BDET : EDET )
      INTEGER NDISP
      REAL OFFSET( NDISP )
      INTEGER DTINDX( NDISP )
      REAL SCALE( NDISP )
      REAL XLMT( 2 )
      REAL YLMT( 2 )
      INTEGER IDC
      CHARACTER PQNAME*(*)
      CHARACTER PXNAME*(*)
      CHARACTER PXTYPE*(*)
      CHARACTER PQCOMM*(*)
      CHARACTER PHIST*(*)
      LOGICAL COLOUR
      LOGICAL CURSOR
      LOGICAL CLRBLK
      LOGICAL SCNDIR

*  Status:
      INTEGER STATUS             ! Global status

*  External Referance:
      INTEGER CHR_LEN            ! Used length of a string

*  Local variables:
      CHARACTER COMMNT*(IRQ__SZCOM)! Quality name comment.
      CHARACTER HSTORY*(IRQ__SZQNM+40)! History record added to NDF.
      CHARACTER LOCS(5)*(DAT__SZLOC)! Locators for quality names info.
      CHARACTER QNAME*(IRQ__SZQNM)! Quality name.
      CHARACTER XLOC*(DAT__SZLOC) ! Locator to quality names NDF
                                  ! extension.
      CHARACTER XNAME*(DAT__SZNAM)! Quality names NDF extension name.
      CHARACTER XTYPE*(DAT__SZTYP)! Quality names NDF extension type.


      INTEGER BIT                ! QUALITY array bit used to store the
                                 ! quality.
      INTEGER IPWORK             ! Pointer to mapped mask defining
                                 ! selected samples.
      INTEGER LQNAME             ! Used length of QNAME.
      INTEGER SET                ! No. of samples which hold the
                                 ! quality.


      LOGICAL FIXED              ! True if quality is fixed for all
                                 ! samples.
      LOGICAL FOUND              ! Flag if the quality name was found.
      LOGICAL QADDED             ! True if an attempt has been made to
                                 ! add a new quality name to the NDF.
      LOGICAL VALUE              ! Value of a fixed quality.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get workspace to hold a mask of values for each sample in the
*  input CRDD file.
      CALL PSX_CALLOC( ( ESMP - BSMP + 1 )*( EDET - BDET + 1 ),
     :                 '_REAL', IPWORK, STATUS )

*  Allow the user to select the samples which are to be assigned a
*  quality. The mask is returned holding VAL__BADR at all selected
*  samples, and zero for all other samples. The selected samples are
*  hightlighted on the display by being re-drawn in pen 2.
      CALL TRACC4( BSMP, ESMP, BDET, EDET, INSCN, DATA, NDISP, OFFSET,
     :             DTINDX, SCALE, XLMT, YLMT, IDC, COLOUR, CURSOR,
     :             CLRBLK, SCNDIR, 4, 2, .TRUE., %VAL( IPWORK ),
     :             STATUS )

*  Get a quality name from the environment.
      CALL PAR_CANCL( PQNAME, STATUS )
      CALL PAR_GET0C( PQNAME, QNAME, STATUS )
      LQNAME = CHR_LEN( QNAME )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to locate any existing quality name information in the input
*  NDF. If such information is found, LOCS is returned holding a set of
*  five HDS locators which identify the NDF and various items of
*  quality information. XNAME is returned holding the name of the NDF
*  extension in which the information was found. If no quality name
*  information is found, then an error is reported.
      CALL IRQ_FIND( NDFIN, LOCS, XNAME, STATUS )

*  If existing quality information was found, tell the user which
*  extension it came from.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'X', XNAME )
         CALL MSG_OUTIF( MSG__VERB, 'TRACC3_MSG1',
     :            '  Using quality names defined in NDF extension "^X"',
     :                   STATUS )

*  If no quality name information was found, annul the error and
*  determine the name of the NDF extension which is to hold such
*  information.
      ELSE
         CALL ERR_ANNUL( STATUS )
         CALL PAR_GET0C( PXNAME, XNAME, STATUS )

*  If the specified extension does not exist, create it with HDS data
*  type specified by parameter PXTYPE. The new extension is a scalar
*  object.
         CALL NDF_XSTAT( NDFIN, XNAME, FOUND, STATUS )
         IF( .NOT. FOUND ) THEN
            CALL PAR_DEF0C( PXTYPE, XNAME, STATUS )
            CALL PAR_GET0C( PXTYPE, XTYPE, STATUS )
            CALL NDF_XNEW( NDFIN, XNAME, XTYPE, 0, 0, XLOC, STATUS )
            CALL DAT_ANNUL( XLOC, STATUS )
         END IF

*  Create a new structure to hold quality information in the named NDF
*  extension.
         CALL IRQ_NEW( NDFIN, XNAME, LOCS, STATUS )

* Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

      END IF

*  Attempt to find the quality name within the NDF.
      CALL IRQ_GETQN( LOCS, QNAME, FIXED, VALUE, BIT, COMMNT, STATUS )

*  If the name does not exist, annul the error, get an associated
*  descriptive comment, and add the supplied quality name to the
*  quality name information stored in the input NDF. Tell the user that
*  the name has been added to the NDF.
      QADDED = .FALSE.
      IF( STATUS .EQ. IRQ__NOQNM ) THEN
         QADDED = .TRUE.
         CALL ERR_ANNUL( STATUS )

         CALL PAR_CANCL( PQCOMM, STATUS )
         CALL PAR_GET0C( PQCOMM, COMMNT, STATUS )
         CALL IRQ_ADDQN( LOCS, QNAME, .FALSE., COMMNT, STATUS )

         CALL MSG_SETC( 'QN', QNAME )
         CALL NDF_MSG( 'NDF', NDFIN )
         CALL MSG_OUTIF( MSG__NORM, 'TRACC3_MSG2',
     :               '  Definition of quality name "^QN" added to ^NDF',
     :                   STATUS )

*  If the name was already defined in the NDF, tell the user.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN

         CALL MSG_SETC( 'QN', QNAME )
         CALL MSG_SETC( 'COM', COMMNT )
         CALL MSG_OUTIF( MSG__NORM, 'TRACC3_MSG3',
     :        '  Using pre-existing definition of quality "^QN" - ^COM',
     :                  STATUS )

      END IF

*  Assign the quality to all selected samples.
      CALL IRQ_SETQM( LOCS, .TRUE., QNAME,
     :                ( ESMP - BSMP + 1 )*( EDET - BDET + 1 ),
     :                %VAL( IPWORK ), SET, STATUS )

*  Tell the user how many samples now hold the quality.
      CALL MSG_SETI( 'SET', SET )
      CALL MSG_SETC( 'QN', QNAME )

      IF( QADDED ) THEN
         CALL MSG_OUTIF( MSG__NORM, 'TRACC3_MSG4',
     :                   '  ^SET pixels hold the quality "^QN"',
     :                   STATUS )

      ELSE
         CALL MSG_OUTIF( MSG__NORM, 'TRACC3_MSG5',
     :                   '  ^SET pixels now hold the quality "^QN"',
     :                   STATUS )

      END IF

*  Add a history record to the NDF.
      HSTORY = 'Quality "'//QNAME( : LQNAME )//
     :         '" assigned to selected samples'
      CALL CHR_LDBLK( HSTORY( 10: ) )
      CALL IRM_HIST( PHIST, NDFIN, 'IRAS90:TRACECRDD', 1, HSTORY,
     :               STATUS )

*  If an error has occured, attempt to remove the quality name if it
*  has been created by this application.
      IF( QADDED .AND. STATUS .NE. SAI__OK ) THEN
         CALL ERR_BEGIN( STATUS )
         CALL IRQ_REMQN( LOCS, QNAME, STATUS )
         CALL ERR_END( STATUS )
      END IF

*  Release the quality name information.
      CALL IRQ_RLSE( LOCS, STATUS )

*  Free the workspace.
 999  CONTINUE
      CALL PSX_FREE( IPWORK, STATUS )

*  If an error occurred (other than parameter abort), flush it.
      IF( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT)
     :                                          CALL ERR_FLUSH( STATUS )

      END
