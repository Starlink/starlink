      SUBROUTINE NDF1_VWCS( IACB, IWCS1, IWCS2, STATUS )
*+
*  Name:
*     NDF1_VWCS

*  Purpose:
*     Validate and strip WCS information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_VWCS( IACB, IWCS1, IWCS2, STATUS )

*  Description:
*     The routine accepts a pointer to an AST_ FrameSet containing WCS
*     information which is to be written to an NDF with an entry in the
*     ACB. It validates the WCS information and produces a copy of the
*     FrameSet. It then strips out of this copy any information which
*     can be recovered from other NDF information (thus minimising
*     storage requirements).
*
*     If the WCS information is valid, a pointer to the validated and
*     stripped copy of the FrameSet is returned. Otherwise, a pointer
*     value of AST__NULL is returned and an appropriate error is
*     reported.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index of the NDF entry in the ACB.
*     IWCS1 = INTEGER (Given)
*        An AST_ pointer to the FrameSet containing the WCS information
*        to be validated.
*     IWCS2 = INTEGER (Returned)
*        An AST_ pointer to the FrameSet containing the validated and
*        stripped copy of the WCS information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - A value of AST__NULL will be returned for the IWCS2 argument if
*     this routine is called with STATUS set, or if it should fail for
*     any reason.

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
*     11-JUL-1997 (RFWS):
*        Original version.
*     4-AUG-2009 (DSB):
*        Added FRACTION Frame. Allow IACB to be zero.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public contstants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

*  Arguments Given:
      INTEGER IACB
      INTEGER IWCS1
      INTEGER IWCS2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NSTD               ! No. standard NDF coordinate systems
      PARAMETER ( NSTD = 4 )

*  Local Variables:
      CHARACTER * ( AST__SZCHR ) CLASS ! Object Class string
      CHARACTER * ( AST__SZCHR ) DOMAIN ! Frame Domain string
      INTEGER FRAME              ! Pointer to Frame
      INTEGER IBASE              ! Base Frame index
      INTEGER ICURR              ! Current Frame index
      INTEGER IFRAME             ! Loop counter for Frame indices
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER NAXES              ! Number of base Frame axes
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NEW                ! Pointer to new FrameSet
      INTEGER NFRAME             ! Number of Frames
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      INTEGER UNIT               ! Pointer to UnitMap

*.

*  Initialise the returned pointer.
      IWCS2 = AST__NULL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make a copy of the AST_ object supplied.
      IWCS2 = AST_COPY( IWCS1, STATUS )

*  Validate the Object supplied.
*  -----------------------------
*  Obtain the Object's Class and check that it is 'FrameSet'. Report an
*  error if it is not.
      CLASS = AST_GETC( IWCS2, 'Class', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( CLASS .NE. 'FrameSet' ) THEN
            STATUS = NDF__WCSIN
            CALL MSG_SETC( 'CLASS', CLASS )
            CALL ERR_REP( 'NDF_PTWCS_CLASS',
     :           'Invalid ^CLASS pointer supplied when a FrameSet ' //
     :           'pointer is required (possible programming error).',
     :           STATUS )
         END IF
      END IF

*  If OK, obtain the Domain of the base Frame and check that this is
*  'GRID'. Report an error if it is not.
      IF ( STATUS .EQ. SAI__OK ) THEN
         FRAME = AST_GETFRAME( IWCS2, AST__BASE, STATUS )
         DOMAIN = AST_GETC( FRAME, 'Domain', STATUS )
         CALL AST_ANNUL( FRAME, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( DOMAIN .NE. 'GRID' ) THEN
               STATUS = NDF__WCSIN
               CALL MSG_SETC( 'DOMAIN', DOMAIN )
               CALL ERR_REP( 'NDF1_VWCS_DOM',
     :              'The base Frame of the FrameSet supplied has ' //
     :              'a Domain value of ''^DOMAIN''; this should be ' //
     :              '''GRID'' (possible programming error).', STATUS )
            END IF
         END IF
      END IF

*  If OK, obtain the number of base Frame axes. If an ACB indesx has been
*  supplied, also obtain the bounds and number of dimensions of the NDF.
      IF ( STATUS .EQ. SAI__OK ) THEN
         FRAME = AST_GETFRAME( IWCS2, AST__BASE, STATUS )
         NAXES = AST_GETI( FRAME, 'Naxes', STATUS )
         CALL AST_ANNUL( FRAME, STATUS )

         IF( IACB .NE. 0 ) THEN
            CALL ARY_BOUND( ACB_DID( IACB ), NDF__MXDIM, LBND, UBND,
     :                      NDIM, STATUS )

*  Check that the number of axes matches the number of NDF dimensions.
*  Report an error if it does not.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( NAXES .NE. NDIM ) THEN
                  STATUS = NDF__NAXIN
                  CALL MSG_SETI( 'NAXES', NAXES )
                  CALL MSG_SETI( 'NDIM', NDIM )
                  CALL ERR_REP( 'NDF1_VWCS_NAXES',
     :                 'The base Frame of the FrameSet supplied has ' //
     :                 '^NAXES axes; this does not match the number ' //
     :                 'of NDF dimensions, ^NDIM (possible ' //
     :                 'programming error).', STATUS )
               END IF
            END IF
         ENDIF
      END IF

*  Strip away unwanted information.
*  --------------------------------
*  We do not need to hold information about any of the standard NDF
*  coordinate systems as part of the FrameSet stored internally, since
*  this can be re-generated from other NDF information when required.
*  However, we must still have Frames present to represent these
*  coordinate systems, so they can be selected as current. To allow
*  this, we remove all the redundant information, but replace it with 4
*  dummy Frames, all inter-related by null Mappings (UnitMaps).
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Start building a new FrameSet containing a base Frame which is a
*  placeholder for the data grid coordinate system. We do not set any
*  unnecessary attributes for it, since we do not want to store these.
         FRAME = AST_FRAME( NAXES, 'Domain=GRID', STATUS )
         NEW = AST_FRAMESET( FRAME, ' ', STATUS )
         CALL AST_ANNUL( FRAME, STATUS )

*  Add a second Frame as a placeholder for the pixel coordinate system,
*  related to the base Frame by a UnitMap.
         UNIT = AST_UNITMAP( NAXES, ' ', STATUS )
         FRAME = AST_FRAME( NAXES, 'Domain=PIXEL', STATUS )
         CALL AST_ADDFRAME( NEW, AST__BASE, UNIT, FRAME, STATUS )
         CALL AST_ANNUL( FRAME, STATUS )

*  Similarly, add a third Frame as a placeholder for the axis
*  coordinate system.
         FRAME = AST_FRAME( NAXES, 'Domain=AXIS', STATUS )
         CALL AST_ADDFRAME( NEW, AST__BASE, UNIT, FRAME, STATUS )
         CALL AST_ANNUL( FRAME, STATUS )

*  Similarly, add a fourth Frame as a placeholder for the normalised
*  pixel coordinate system.
         FRAME = AST_FRAME( NAXES, 'Domain=FRACTION', STATUS )
         CALL AST_ADDFRAME( NEW, AST__BASE, UNIT, FRAME, STATUS )
         CALL AST_ANNUL( FRAME, STATUS )

*  Obtain the base and current Frame indices from the original FrameSet.
         IBASE = AST_GETI( IWCS2, 'Base', STATUS )
         ICURR = AST_GETI( IWCS2, 'Current', STATUS )

*  Make the base Frame current and add the original FrameSet to the new
*  one, related to the new base Frame by a UnitMap.  Annul the UnitMap
*  pointer.
         CALL AST_SETI( IWCS2, 'Current', IBASE, STATUS )
         CALL AST_ADDFRAME( NEW, AST__BASE, UNIT, IWCS2, STATUS )
         CALL AST_ANNUL( UNIT, STATUS )

*  Annul the original FrameSet pointer and replace it with a pointer to
*  the new one.
         CALL AST_ANNUL( IWCS2, STATUS )
         IWCS2 = NEW

*  Restore the original current Frame (allowing for its new index).
         CALL AST_SETI( IWCS2, 'Current', ICURR + NSTD, STATUS )

*  If the current Frame describes the data grid coordinate system,
*  change it to be the new base Frame instead. Otherwise, if it
*  describes the pixel coordinate Frame, then change it to be the new
*  one instead. Similarly for the axis coordinate system. This process
*  records which coordinate system was current, even if we subsequently
*  remove the actual Frame.
         DOMAIN = AST_GETC( IWCS2, 'Domain', STATUS )
         IF ( DOMAIN .EQ. 'GRID' ) THEN
            CALL AST_SET( IWCS2, 'Current=1', STATUS )
         ELSE IF ( DOMAIN .EQ. 'PIXEL' ) THEN
            CALL AST_SET( IWCS2, 'Current=2', STATUS )
         ELSE IF ( DOMAIN .EQ. 'AXIS' ) THEN
            CALL AST_SET( IWCS2, 'Current=3', STATUS )
         ELSE IF ( DOMAIN .EQ. 'FRACTION' ) THEN
            CALL AST_SET( IWCS2, 'Current=4', STATUS )
         END IF

*  Loop through all the Frames acquired from the original FrameSet.
         NFRAME = AST_GETI( IWCS2, 'Nframe', STATUS )
         IFRAME = NSTD + 1
 1       CONTINUE                ! Start of 'DO WHILE' loop
         IF ( IFRAME .LE. NFRAME ) THEN

*  Obtain each Frame's Domain.
            FRAME = AST_GETFRAME( IWCS2, IFRAME, STATUS )
            DOMAIN = AST_GETC( FRAME, 'Domain', STATUS )
            CALL AST_ANNUL( FRAME, STATUS )

*  Remove any Frame with a Domain associated with any of the standard
*  NDF coordinate systems, and decrement the Frame count (we assume
*  there might be more than one Frame with the same domain, although
*  this shouldn't normally happen).
            IF ( ( DOMAIN .EQ. 'GRID' ) .OR.
     :           ( DOMAIN .EQ. 'PIXEL' ) .OR.
     :           ( DOMAIN .EQ. 'AXIS' ) .OR.
     :           ( DOMAIN .EQ. 'FRACTION' ) ) THEN
               CALL AST_REMOVEFRAME( IWCS2, IFRAME, STATUS )
               NFRAME = NFRAME - 1

*  Leave all other Frames in place.
            ELSE
               IFRAME = IFRAME + 1
            END IF
            GO TO 1
         END IF
      END IF

*  If an error occurred, annul the returned AST_ pointer.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( IWCS2, STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_VWCS', STATUS )

      END
