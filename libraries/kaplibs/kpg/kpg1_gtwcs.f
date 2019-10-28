      SUBROUTINE KPG1_GTWCS( INDF, IWCS, STATUS )
*+
*  Name:
*     KPG1_GTWCS

*  Purpose:
*     Gets an AST FrameSet from an NDF.

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

*  Copyright:
*     Copyright (C) 1998, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005, 2006 Particle Physics & Astronomy Research Council.
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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-JUL-1998 (DSB):
*        Original version.
*     9-DEC-1998 (DSB):
*        Modified to ignore un-usable WCS information read from an IRAS90
*        astrometry structure, or a FITS extension.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     2005 September 12 (TIMJ):
*        Now use KPG1_ASRGN rather than KPG1_ASREG to avoid
*        dependency on graphics system.
*     7-MAR-2006 (DSB):
*        Create a new temporary NDF (without mapping any array
*        components) rather than taking a copy of the supplied NDF, since
*        the supplied NDF may be very large.
*     18-MAR-2006 (DSB):
*        Use the AXIS Frame from the supplied NDF instead of from the
*        temporary NDF when reading WCS from a FITS extension.
*     4-MAY-2006 (DSB):
*        Guard against NULL IWCS values.
*     4-OCT-2019 (DSB):
*        Changed to use the 8-byte NDF interface.
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
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PSX_ERR'          ! PSX ERROR constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

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
      CHARACTER ENCODS( MAXCOD )*( CODLEN )! Preferred AST encodings
      CHARACTER ENV*255          ! Value of KAPPA_ENCODINGS env. variable
      CHARACTER LOC*(DAT__SZLOC) ! Locator to FITS entension or IRA structure
      CHARACTER XLOC*(DAT__SZLOC) ! Locator to IRAS90 extension
      CHARACTER XNAME*(DAT__SZNAM)! Name of extension containing IRA structure
      INTEGER ADDED              ! No. of elements added to the group
      INTEGER AXFRM              ! AXIS Frame in supplied NDF
      INTEGER AXMAP              ! Mapping from GRID to AXIS in supplied NDF
      INTEGER I                  ! Axis count
      INTEGER IAX                ! Index of AXIS Frame
      INTEGER ICURR              ! Index of original current Frame
      INTEGER IDA                ! IRA identifier for IRAS90 astrometry info
      INTEGER IGRP               ! GRP identifier for a group
      INTEGER INDFC              ! Identifier for NDF to get new WCS component
      INTEGER IPIX               ! Index of PIXEL Frame in IWCS
      INTEGER IRAFRM             ! AST Frame describing IRA sky co-ords
      INTEGER IRAMAP             ! AST Mapping from IRA "image" to sky co-ords
      INTEGER IWCS2              ! Default FrameSet from supplied NDF
      INTEGER*8 LBND( NDF__MXDIM ) ! Lower pixel bounds
      INTEGER NCARD              ! No. of header cards in the FITS extension
      INTEGER NDIM               ! Number of pixel axes
      INTEGER NENCOD             ! No. of prefered AST encodings supplied
      INTEGER PLACE              ! Place holder for temporary NDF
      INTEGER PNTR               ! Pointer to mapped array of FITS headers
      INTEGER*8 UBND( NDF__MXDIM ) ! Upper pixel bounds
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

*  Ensure all KAPPA non-graphical AST IntraMaps are registered.
      CALL KPG1_ASRGN( STATUS )

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
               CALL KPG1_VERB( VERB, 'KAPPA', STATUS )

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

*  If we have write access to the NDF, then any WCS component can be
*  stored in the supplied NDF so that it can be accessed faster next
*  time it is needed. If we do not have write access to the NDF, then
*  we take a copy of the supplied NDF, and attempt to create a WCS component
*  in the copy. If write access is available, use the supplied identifier.
            IF( WRACC ) THEN
               INDFC = INDF

*  Otherwise, create a temporary NDF to be used as a host for the WCS
*  FrameSet with the same pixel origin as the supplied NDF, but a size
*  of only 1 pixel on each axis.
            ELSE
               CALL NDF_TEMP( PLACE, STATUS )
               CALL NDF_BOUND8( INDF, NDF__MXDIM, LBND, UBND, NDIM,
     :                          STATUS )
               DO I = 1, NDIM
                  UBND( I ) = LBND( I )
               END DO

               CALL NDF_NEW8( '_BYTE', NDIM, LBND, UBND, PLACE, INDFC,
     :                        STATUS )
            END IF

*  Check the pointer can be used.
            IF( STATUS .EQ. SAI__OK ) THEN

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
                     CALL MSG_OUT( 'KPG1_GTWCS_MSG2', 'WARNING: No '//
     :                    'more than ^N comma delimited encodings '//
     :                    'should be specified in the environment '//
     :                    'variable KAPPA_ENCODINGS. The excess '//
     :                    'encodings will be ignored - ''^ENV''.',
     :                    STATUS )

                     NENCOD = MAXCOD

                  END IF

*  Extract the encodings from the group into the ENCODS array.
                  CALL GRP_GET( IGRP, 1, NENCOD, ENCODS, STATUS )

*  Delete the group.
                  CALL GRP_DELET( IGRP, STATUS )

               END IF

*  Only proceed if no error has occurred.
               IF( STATUS .EQ. SAI__OK ) THEN

*  Attempt to create a WCS component within the NDF selected above.
                  CALL FTS1_FTWCS( NCARD, %VAL( CNF_PVAL( PNTR ) ),
     :                             1, INDFC,
     :                             NENCOD, ENCODS, STATUS,
     :                             %VAL( CNF_CVAL( 80 ) ) )

*  If an error occurred reading the FITS WCS information, annul or flush it
*  since failure to read WCS information is probably not fatal.
                  IF( STATUS .NE. SAI__OK ) THEN

*  See if we are running in verbose mode.
                     CALL KPG1_VERB( VERB, 'KAPPA', STATUS )

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
                        CALL MSG_OUT( 'KPG1_GTWCS_MSG3', 'Using WCS '//
     :                                'information read from the FITS'//
     :                                ' extension of ''^NDF''.',
     :                                STATUS )
                     END IF

                  END IF

               END IF

*  If a temporary copy of the NDF was used, delete it. We also may need
*  to copy the AXIS Frame from the original NDFs default FrameSet since it
*  will have been truncated in size by using a temporary NDF that is
*  smaller than the supplied NDF.
               IF( .NOT. WRACC ) THEN
                  CALL NDF_DELET( INDFC, STATUS )

*  If the supplied NDF has a defined AXIS structure, we need to replace
*  the AXIS Frame in the FrameSet obtained from the temporary NDF with the
*  AXIS Frame obtained from the supplied NDF. This is because the two
*  NDFs will have different sizes and so the AXIS Frames will differ (e.g.
*  if they use LutMaps, the LutMap will have an inappropriate number of
*  entries).
                  CALL NDF_STATE( INDF, 'AXIS', THERE, STATUS )
                  IF( THERE .AND. IWCS .NE. AST__NULL ) THEN

*  Get the default FrameSet from the supplied NDF, and get the Mapping
*  from GRID to AXIS, and the AXIS Frame (the AXIS Frame will be the
*  current Frame since there is no WCS FrameSet in the supplied NDF).
                     CALL NDF_GTWCS( INDF, IWCS2, STATUS )
                     AXMAP = AST_GETMAPPING( IWCS2, AST__BASE,
     :                                       AST__CURRENT, STATUS )
                     AXFRM = AST_GETFRAME( IWCS2, AST__CURRENT, STATUS )

*  Get the index of the AXIS Frame in the FrameSet read from the
*  temporary NDF
                     CALL KPG1_ASFFR( IWCS, 'AXIS', IAX, STATUS )

*  Remove the AXIS Frame, first noting the index of the current Frame.
                     ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )
                     CALL AST_REMOVEFRAME( IWCS, IAX, STATUS )

*  Add in the AXIS Frame obtained from the supplied NDF. It becomes the
*  current Frame.
                     CALL AST_ADDFRAME( IWCS, AST__BASE, AXMAP, AXFRM,
     :                                  STATUS )

*  Re-instate the original current Frame.
                     IF( ICURR .LT. IAX ) THEN
                        CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

                     ELSE IF( ICURR .GT. IAX ) THEN
                        CALL AST_SETI( IWCS, 'CURRENT', ICURR - 1,
     :                                 STATUS )
                     END IF

                  END IF

               END IF

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
