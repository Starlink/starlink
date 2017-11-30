      SUBROUTINE NDF1_DW( IDCB, STATUS )
*+
*  Name:
*     NDF1_DW

*  Purpose:
*     Ensure that WCS information is available in the DCB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DW( IDCB, STATUS )

*  Description:
*     The routine ensures that information about a data object's WCS
*     (World Coordinate System) component is available in the DCB. It
*     does nothing if this information is already available. Otherwise,
*     it obtains the information by inspecting the actual data object,
*     performing necessary validation checks in the process.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index to the DCB entry for which WCS information is required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2-JUL-1997 (RFWS):
*        Original version.
*     14-JUL-1997 (RFWS):
*        Use AST_EXEMPT.
*     4-AUG-2009 (DSB):
*        Use NDF1_VWCS.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ASTLC = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to HDS _CHAR array holding AST_ data.
*        DCB_ASTLN = INTEGER (Write)
*           Next element to use in HDS _CHAR array holding AST_ data.
*        DCB_ASTPT = INTEGER (Write)
*           Pointer to mapped HDS _CHAR array holding AST_ data.
*        DCB_IWCS( NDF__MXDCB ) = INTEGER (Write)
*           Pointer to AST_ WCS information.
*        DCB_KW( NDF__MXDCB ) = LOGICAL (Read and Write)
*           Whether information about the NDF's WCS component is
*           available in the DCB.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

*  Arguments Given:
      INTEGER IDCB

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL NDF1_RDAST        ! Read AST_ data from an HDS object

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) WCSLOC ! Locator to WCS structure
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS component type string
      INTEGER CHAN               ! Pointer to AST_ Channel
      INTEGER CLEN               ! Character string length
      INTEGER DIM( DAT__MXDIM )  ! HDS object dimensions
      INTEGER IWCS               ! Pointer for original NDF
      INTEGER NDIM               ! Number of HDS object dimensions
      LOGICAL THERE              ! HDS component exists?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if WCS information is already available. There is nothing to do
*  if it is.
      IF ( .NOT. DCB_KW( IDCB ) ) THEN

*  Initialise the DCB pointer for the AST_ Object which will contain
*  the WCS information.
         DCB_IWCS( IDCB ) = AST__NULL

*  WCS structure.
*  ==============
*  See if a WCS component is present in the NDF structure.
         CALL DAT_THERE( DCB_LOC( IDCB ), 'WCS', THERE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If a WCS component is present, obtain a locator for it and determine
*  its type and shape.
            IF ( THERE ) THEN
               WCSLOC = DAT__NOLOC
               CALL DAT_FIND( DCB_LOC( IDCB ), 'WCS', WCSLOC, STATUS )
               CALL DAT_TYPE( WCSLOC, TYPE, STATUS )
               CALL DAT_SHAPE( WCSLOC, DAT__MXDIM, DIM, NDIM, STATUS )

*  Check that the component is of type 'WCS' and report an error if it
*  is not.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( TYPE .NE. 'WCS' ) THEN
                     STATUS = NDF__TYPIN
                     CALL NDF1_DMSG( 'NDF', IDCB )
                     CALL MSG_SETC( 'BADTYPE', TYPE )
                     CALL ERR_REP( 'NDF1_DW_WTYPE',
     :                    'The WCS component in the NDF structure ' //
     :                    '^NDF has an invalid type ' //
     :                    'of ''^BADTYPE''; it should be of ' //
     :                    'type ''WCS''.', STATUS )

*  Also check that the component is scalar and report an error if it is
*  not.
                  ELSE IF ( NDIM .NE. 0 ) THEN
                     STATUS = NDF__NDMIN
                     CALL NDF1_DMSG( 'NDF', IDCB )
                     CALL MSG_SETI( 'BADNDIM', NDIM )
                     CALL ERR_REP( 'NDF1_DW_WNDIM',
     :                    'The WCS component in the NDF structure ' //
     :                    '^NDF is ^BADNDIM-dimensional; it should ' //
     :                    'be scalar.', STATUS )
                  END IF
               END IF

*  DATA component.
*  ===============
*  See if the WCS structure contains the mandatory DATA component.
               CALL DAT_THERE( WCSLOC, 'DATA', THERE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If it does not, then report an error.
                  IF ( .NOT. THERE ) THEN
                     STATUS = NDF__NOWDT
                     CALL DAT_MSG( 'WCS', WCSLOC )
                     CALL ERR_REP( 'NDF1_DW_NODAT',
     :                    'The DATA component is missing from the ' //
     :                    'NDF WCS structure ^WCS', STATUS )

*  Otherwise, obtain a locator for it (storing this locator in the DCB)
*  and determine its type and shape.
                  ELSE
                     DCB_ASTLC = DAT__NOLOC
                     CALL DAT_FIND( WCSLOC, 'DATA', DCB_ASTLC, STATUS )
                     CALL DAT_TYPE( DCB_ASTLC, TYPE, STATUS )
                     CALL DAT_SHAPE( DCB_ASTLC, DAT__MXDIM, DIM, NDIM,
     :                               STATUS )

*  Check that the DATA component has type _CHAR and report an error if
*  it does not.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                           STATUS = NDF__TYPIN
                           CALL DAT_MSG( 'WCS', WCSLOC )
                           CALL MSG_SETC( 'BADTYPE', TYPE )
                           CALL ERR_REP( 'NDF1_DW_DTYPE',
     :                          'The DATA component in the NDF WCS ' //
     :                          'structure ^WCS has an invalid type ' //
     :                          'of ''^BADTYPE''; it should be of ' //
     :                          'type ''_CHAR''.', STATUS )

*  Check that the DATA component is 1-dimensional and report an error
*  if it is not.
                        ELSE IF ( NDIM .NE. 1 ) THEN
                           STATUS = NDF__NDMIN
                           CALL DAT_MSG( 'WCS', WCSLOC )
                           CALL MSG_SETI( 'BADNDIM', NDIM )
                           CALL ERR_REP( 'NDF1_DW_DNDIM',
     :                          'The DATA component in the NDF WCS ' //
     :                          'structure ^WCS is ' //
     :                          '^BADNDIM-dimensional; it should be ' //
     :                          '1-dimensional.', STATUS )

                        END IF
                     END IF

*  Determine the DATA component's character string length and check
*  that it is not too short to hold AST_ data. Report an error if it
*  is.
                     CALL DAT_CLEN( DCB_ASTLC, CLEN, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        IF ( CLEN .LT. NDF__MLAST ) THEN
                           STATUS = NDF__WCDTS
                           CALL DAT_MSG( 'WCS', WCSLOC )
                           CALL MSG_SETI( 'CLEN', CLEN )
                           CALL MSG_SETI( 'MINLEN', NDF__MLAST )
                           CALL ERR_REP( 'NDF1_DW_WCDTS',
     :                          'The DATA component in the NDF WCS ' //
     :                          'structure ^WCS has a character ' //
     :                          'string length of ^CLEN; it should ' //
     :                          'have a length of at least ^MINLEN.',
     :                          STATUS )
                        END IF
                     END IF

*  Map the DATA component for READ access and store the resulting
*  pointer in the DCB.
                     CALL DAT_MAP( DCB_ASTLC, '_CHAR', 'READ',
     :                             NDIM, DIM, DCB_ASTPT, STATUS )

*  Create an AST_ Channel to read from the DATA component. Supply the
*  NDF1_RDAST routine as the "source" routine for extracting the data.
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        CHAN = AST_CHANNEL( NDF1_RDAST, AST_NULL, ' ',
     :                                      STATUS )

*  Initialise the index of the first element in the _CHAR array to be
*  used by the source function.
                        DCB_ASTLN = 1

*  Read an Object from the Channel, thus transferring the data, and
*  store the resulting AST_ pointer in the DCB. Exempt this pointer
*  from AST_ context handling (so it is not annulled if AST_END is
*  called).
                        IWCS = AST_READ( CHAN, STATUS )

*  If an error occurred during data transfer, report a contextual error
*  message.
                        IF ( STATUS .NE. SAI__OK ) THEN
                           CALL DAT_MSG( 'OBJECT', DCB_ASTLC )
                           CALL ERR_REP( 'NDF1_DW_READ',
     :                          'Error while reading AST_ data from ' //
     :                          'the HDS object ^OBJECT.', STATUS )
                        END IF

*  Validate the FrameSet and ensure it contains all the standard dummy
*  Frames expected by the NDF library.
                        CALL NDF1_VWCS( 0, IWCS, DCB_IWCS( IDCB ),
     :                                  STATUS )

*  Annul the original FrameSet pointer.
                        CALL AST_ANNUL( IWCS, STATUS )

*  Exempt this pointer from AST_ context handling (so it is not annulled if
*  AST_END is called).
                        CALL AST_EXEMPT( DCB_IWCS( IDCB ), STATUS )

*  Annul the Channel pointer, thus deleting the Channel.
                        CALL AST_ANNUL( CHAN, STATUS )
                     END IF

*  Annul the DATA component locator.
                     CALL DAT_ANNUL( DCB_ASTLC, STATUS )
                  END IF
               END IF

*  Annul the WCS structure locator.
               CALL DAT_ANNUL( WCSLOC, STATUS )
            END IF
         END IF

*  If an error occurred, annul any DCB pointer that may have been
*  allocated.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL AST_ANNUL( DCB_IWCS( IDCB ), STATUS )
         END IF

*  Note whether WCS information is now available in the DCB.
         DCB_KW( IDCB ) = STATUS .EQ. SAI__OK
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DW', STATUS )

      END
