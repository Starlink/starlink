      SUBROUTINE KPG1_GTWCS( INDF, IWCS, STATUS )
*+
*  Name:
*     KPG1_GTWCS

*  Purpose:
*     Get an AST FrameSet from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Description:
*     This routine returns a FrameSet describing the WCS information
*     in an NDF. If the NDF has no WCS component, any IRAS90 IRA 
*     structure is converted into a FrameSet and returned. If the NDF has
*     no IRAS90 IRA structure, then an attempt is made to read a FrameSet
*     from the FITS headers in the FITS extension. If the NDF has no FITS
*     extension, then the default NDF FrameSet is returned.

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier for the NDF.
*     IWCS = INTEGER (Returned)
*        An AST pointer to the WCS FrameSet. Returned equal to AST__NULL
*        if an error occurs. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If a WCS FrameSet is created form an IRAS90 astrometry structure or
*     a FITS extension, it will be stored in the supplied NDF if write
*     access is available for the NDF.
*     -  The preferred AST encodings to use when interpreting FITS headers
*     can be specified as a comma-delimited string using environment
*     variable KAPPA_ENCODINGS. If this variable is not defined, then the
*     normal default encodings are used (see FITSDIN).

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JUL-1998 (DSB):
*        Original version.
*     9-DEC-1998 (DSB):
*        Modified to ignore un-usable WCS information read from an IRAS90 
*        astrometry structure, or a FITS extension.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PSX_ERR'          ! PSX ERROR constants

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXCOD             ! Max. no. of requested AST encodings 
      PARAMETER( MAXCOD = 5 )

      INTEGER CODLEN             ! Max. length of an AST encoding
      PARAMETER( CODLEN = 10 )

*  Local Variables:
      CHARACTER ASNAME*(DAT__SZNAM)! Name of IRA structure
      CHARACTER XNAME*(DAT__SZNAM) ! Name of extension containing IRA structure
      CHARACTER ENCODS( MAXCOD )*( CODLEN )! Preferred AST encodings
      CHARACTER ENV*255          ! Value of KAPPA_ENCODINGS env. variable
      CHARACTER LOC*(DAT__SZLOC) ! Locator to FITS entension or IRA structure
      CHARACTER XLOC*(DAT__SZLOC) ! Locator to IRAS90 extension
      INTEGER ADDED              ! No. of elements added to the group
      INTEGER IDA                ! IRA identifier for IRAS90 astrometry info
      INTEGER IRAFRM             ! AST Frame describing IRA sky co-ords
      INTEGER IRAMAP             ! AST Mapping from IRA "image" to sky co-ords
      INTEGER IGRP               ! GRP identifier for a group
      INTEGER INDFC              ! Identifier for NDF to get new WCS component
      INTEGER IPIX               ! Index of PIXEL Frame in IWCS
      INTEGER NCARD              ! No. of header cards in the FITS extension
      INTEGER NENCOD             ! No. of prefered AST encodings supplied
      INTEGER PLACE              ! Place holder for temporary NDF
      INTEGER PNTR               ! Pointer to mapped array of FITS headers
      LOGICAL FLAG               ! Was group expression flagged? (NO)
      LOGICAL THERE              ! Does object exist?
      LOGICAL VERB               ! Give verbose warnings about bad IRAS90/FITS?
      LOGICAL WRACC              ! Can the supplied NDF be modified?
*.

*  Initialise.
      IWCS = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Ensure all KAPPA AST IntraMaps are registered.
      CALL KPG1_ASREG( STATUS )

*  See if the WCS component in the NDF is in a defined state.
      CALL NDF_STATE( INDF, 'WCS', THERE, STATUS )

*  If so, get and return the WCS component from the NDF.
      IF( THERE ) THEN
         CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  If not, see if an IRAS90 astrometry structure is available.
      ELSE

*  If we have write access to the NDF, then any new WCS component created
*  on the basis of IRAS90 or FITS information can be stored in the supplied 
*  NDF so that it can be accessed faster next time it is needed. 
         CALL NDF_ISACC( INDF, 'WRITE', WRACC, STATUS ) 

*  Initialise the IRA suystem.
         CALL IRA_INIT( STATUS )

*  See if an IRA astrometry structure can be found in the supplied NDF, an 
*  get an HDS locator for it if it can.
         CALL IRA_FIND( INDF, THERE, XNAME, ASNAME, XLOC, STATUS )

*  If an IRA structure was found...
         IF( THERE .AND. STATUS .EQ. SAI__OK ) THEN

*  Get an HDS locator for the astrometry structure.
            CALL DAT_FIND( XLOC, ASNAME, LOC, STATUS )

*  Get an IRA identifier for the structure. This validates the structure.
            CALL IRA_READ( LOC, IDA, STATUS )

*  Create an AST Frame describing the sky co-ordinates in the IRA
*  structure, and a Mapping from IRA "image" co-ordinates to sky co-ordinates.
            CALL KPG1_ASIRA( IDA, IRAFRM, IRAMAP, STATUS )

*  Only proceed if both Mapping and Frame were produced.
            IF( IRAFRM .NE. AST__NULL .AND. IRAMAP .NE. AST__NULL ) THEN

*  Get the default WCS FrameSet from the NDF.
               CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  Find the index of the PIXEL Frame.
               CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )

*  Add the IRA Frame into the FrameSet, using the IRA Mapping to join the 
*  existing PIXEL Frame to the IRA Frame. This makes PIXEL co-ords and IRA 
*  "image" co-ords identical.
               CALL AST_ADDFRAME( IWCS, IPIX, IRAMAP, IRAFRM, STATUS )

            END IF

*  Annul the locator to the IRA structure, and the IRA identifier.
            CALL IRA_ANNUL( IDA, STATUS )
            CALL DAT_ANNUL( LOC, STATUS )
            CALL DAT_ANNUL( XLOC, STATUS )

*  If an error occurred, annul or flush it since failure to read WCS 
*  information is probably not fatal.
            IF( STATUS .NE. SAI__OK ) THEN

*  Annul any FrameSet pointer.
               IF( IWCS .NE. AST__NULL ) CALL AST_ANNUL( IWCS, STATUS )

*  See if we are running in verbose mode.
               CALL KPG1_VERB( VERB, STATUS )

*  If we are, add a context message, and flush the error.
               IF( VERB ) THEN
                  CALL NDF_MSG( 'NDF', INDF )
                  CALL ERR_REP( 'KPG1_GTWCS_ERR1', 'Ignoring '//
     :                          'un-usable IRAS90 astrometry '//
     :                          'information in ''^NDF''.', STATUS )
                  CALL ERR_FLUSH( STATUS )

*  Otherwise, annul the error message.
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF

*  If the IRAS90 information was read succesfully, tell the user.
            ELSE IF( IWCS .NE. AST__NULL ) THEN
               CALL MSG_BLANK( STATUS )
               CALL NDF_MSG( 'NDF', INDF )
               CALL MSG_OUT( 'KPG1_GTWCS_MSG1', 'Using WCS '//
     :                       'information read from an IRAS90 '//
     :                       'astrometry structure in ''^NDF''.', 
     :                       STATUS )

*  If the NDF can be modified, save the new FrameSet.
               IF( WRACC ) CALL NDF_PTWCS( IWCS, INDF, STATUS )

            END IF

         END IF

*  Close down the IRA suystem.
         CALL IRA_CLOSE( STATUS )

      END IF

*  If we still have no WCS FrameSet, try to create a WCS component from the
*  FITS extension.
      IF( IWCS .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN

*  See if there is a FITS extension in the NDF.
         CALL NDF_XSTAT( INDF, 'FITS', THERE, STATUS )

*  If so...
         IF( THERE ) THEN

*  Find the FITS extension and map it.
            CALL NDF_XLOC( INDF, 'FITS', 'READ', LOC, STATUS )
            CALL DAT_MAPV( LOC, '_CHAR*80', 'READ', PNTR, NCARD, 
     :                     STATUS )

*  Check the pointer can be used.
            IF( STATUS .EQ. SAI__OK ) THEN

*  If we have write access to the NDF, then any WCS component can be 
*  stored in the supplied NDF so that it can be accessed faster next
*  time it is needed. If we do not have write access to the NDF, then 
*  we take a copy of the supplied NDF, and attempt to create a WCS component
*  in the copy. If write access is available, use the supplied identifier.
               IF( WRACC ) THEN
                  INDFC = INDF

*  Otherwise, create a temporary copy of the NDF.
               ELSE
                  CALL NDF_TEMP( PLACE, STATUS )
                  CALL NDF_COPY( INDF, PLACE, INDFC, STATUS )               
               END IF

*  Attempt to get the value of the anvironment variable KAPPA_ENCODINGS.
               CALL PSX_GETENV( 'KAPPA_ENCODINGS', ENV, STATUS )

*  If the environment variable was not defined, annul the error, and 
*  indicate that the default encodings should be used.
               IF( STATUS .EQ. PSX__NOENV ) THEN
                  CALL ERR_ANNUL( STATUS )
                  NENCOD = 0

*  If the environment variable was defined, extract comma-delimited strings
*  from it into array ENCODS.
               ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  Create a GRP group to store the encodings in. The default delimiter
*  character (comma) is retained.
                  CALL GRP_NEW( 'Encodings', IGRP, STATUS )

*  Read the encodings from the KAPPA_ENCODINGS string into the group.
                  CALL GRP_GRPEX( ENV, GRP__NOID, IGRP, NENCOD, ADDED, 
     :                            FLAG, STATUS ) 

*  If there are too many encodings in KAPPA_ENCODINGS, warn the user and
*  reduce the number of encodings to be used to the maximum number.
                  IF( NENCOD .GT. MAXCOD ) THEN

                     CALL MSG_SETI( 'N', MAXCOD )
                     CALL MSG_SETC( 'ENV', ENV )
                     CALL MSG_OUT( 'KPG1_GTWCS_1', 'WARNING: No more'//
     :                    ' than ^N comma delimited encodings should '//
     :                    'be specified in the environment variable '//
     :                    'KAPPA_ENCODINGS. The excess encodings will'//
     :                    ' be ignored - ''^ENV''.', STATUS )
   
                     NENCOD = MAXCOD

                  ENDIF

*  Extract the encodings from the group into the ENCODS array.
                  CALL GRP_GET( IGRP, 1, NENCOD, ENCODS, STATUS ) 

*  Delete the group.
                  CALL GRP_DELET( IGRP, STATUS )

               END IF

*  Only proceed if no error has occurred. 
               IF( STATUS .EQ. SAI__OK ) THEN

*  Attempt to create a WCS component within the NDF selected above.
                  CALL FTS1_FTWCS( NCARD, %VAL( PNTR ), 1, INDFC, 
     :                             NENCOD, ENCODS, STATUS, %VAL( 80 ) )

*  If an error occurred reading the FITS WCS information, annul or flush it
*  since failure to read WCS information is probably not fatal. 
                  IF( STATUS .NE. SAI__OK ) THEN

*  See if we are running in verbose mode.
                     CALL KPG1_VERB( VERB, STATUS )

*  If we are, add a context message, and flush the error.
                     IF( VERB ) THEN
                        CALL NDF_MSG( 'NDF', INDF )
                        CALL ERR_REP( 'KPG1_GTWCS_ERR2', 'Ignoring '//
     :                                'un-usable WCS information in '//
     :                                'the FITS extension of ''^NDF''.', 
     :                                STATUS )
                        CALL ERR_FLUSH( STATUS )

*  Otherwise, annul the error.
                     ELSE
                        CALL ERR_ANNUL( STATUS )
                     END IF

*  Ensure the WCS component in the NDF is undefined.
                     CALL NDF_RESET( INDFC, 'WCS', STATUS )

*  If succesful...
                  ELSE

*  See if the NDF now has a defined WCS component.
                     CALL NDF_STATE( INDFC, 'WCS', THERE, STATUS )

*  If so, get it.
                     IF( THERE ) THEN
                        CALL NDF_GTWCS( INDFC, IWCS, STATUS )

*  Warn the user.
                        CALL MSG_BLANK( STATUS )
                        CALL NDF_MSG( 'NDF', INDF )
                        CALL MSG_OUT( 'KPG1_GTWCS_MSG1', 'Using WCS '//
     :                                'information read from the FITS'//
     :                                ' extension of ''^NDF''.', 
     :                                STATUS )
                     END IF         

                  END IF

               END IF

*  If a temporary copy of the NDF was used, delete it.
               IF( .NOT. WRACC ) CALL NDF_DELET( INDFC, STATUS )

            END IF

*  Annul the locator for the FITS extension.
            CALL DAT_ANNUL( LOC, STATUS )

         END IF

      END IF

*  If we still have no WCS FrameSet, get the default WCS FrameSet from the 
*  NDF.
      IF( IWCS .EQ. AST__NULL ) CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  Export the returned FrameSet from the current AST context so that it is 
*  not annulled by the following call to AST_END.
      CALL AST_EXPORT( IWCS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )
    
      END
