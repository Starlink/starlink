      SUBROUTINE NDF1_INIFR( IACB, IWCS, STATUS )
*+
*  Name:
*     NDF1_INIFR

*  Purpose:
*     Initialise standard Frames in an NDF's WCS information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_INIFR( IACB, IWCS, STATUS )

*  Description:
*     This routine inspects the Domain attribute of the current Frame
*     of an AST_ FrameSet, a pointer to which is supplied. If the
*     domain identifies the Frame as representing one of the standard
*     coordinate systems associated with an NDF, the Frame's other
*     attributes are initialised so as to apply to that coordinate
*     system.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     IWCS = INTEGER (Given)
*        An AST_ pointer to a FrameSet containing WCS information
*        associated with the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - No modification occurs if the current Frame is not recognised
*     as corresponding to one of the standard NDF coordinate systems.
*     - It is assumed that the base Frame of the FrameSet corresponds
*     with the NDF's data grid coordinate system, and that the Mapping
*     between that Frame and the current Frame is correctly set up. The
*     values of coordinates transformed from the base to the current
*     Frame may appear in values set for the current Frame's
*     attributes.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
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
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-JUL-1997 (RFWS):
*        Original version.
*     13-JAN-1998 (RFWS):
*        Changed terminology for data grid title and axes.
*     4-NOV-1998 (RFWS):
*        Set explicit format for coordinates that represent pixels.
*     4-AUG-2009 (DSB):
*        Add FRACTION as a standard Frame.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Arguments Given:
      INTEGER IACB
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ACLOC( NDF__MXDIM, NDF__MXACN, NDF__MXDCB ) =
*        CHARACTER * ( DAT__SZLOC ) (Read)
*           Locators to axis character components.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to NDF axis structures.
*        DCB_DID( NDF__MXDCB ) = INTEGER (Read)
*           Identifier for NDF data array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      CHARACTER * ( 5 ) PIXFMT   ! Default format for pixel cordinates
      PARAMETER ( PIXFMT = '%3.1f' )
      CHARACTER * ( 5 ) FRAFMT   ! Default format for FRACTION cordinates
      PARAMETER ( FRAFMT = '%5.4f' )
      INTEGER SZFMT              ! Max. characters in formatted value
      PARAMETER ( SZFMT = 2 * VAL__SZD )

*  Local Variables:
      CHARACTER * ( AST__SZCHR ) DOMAIN ! Current Frame domain
      CHARACTER * ( NDF__MXDIM * ( SZFMT + 1 ) + 1 ) COSTR
                                 ! Formatted coordinate string
      CHARACTER * ( SZFMT ) FMTVAL ! Buffer for formatted value
      CHARACTER * ( VAL__SZI ) AXIS ! Buffer for axis number
      DOUBLE PRECISION COORD( 1, NDF__MXDIM ) ! Coordinates of 1st pixel
      DOUBLE PRECISION IND( 1, NDF__MXDIM ) ! Indices of 1st pixel
      INTEGER CLEN               ! Length of axis component
      INTEGER DIM( 1 )           ! Dummy dimension array
      INTEGER IAXIS              ! Loop counter for axes
      INTEGER IDCB               ! Index to NDF entry in the DCB
      INTEGER LBND( NDF__MXDIM ) ! Lower pixel-index bounds
      INTEGER LFMT               ! No. characters in formatted value
      INTEGER NAXES              ! Number of current Frame axes
      INTEGER NC                 ! Number of characters in buffer
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER NIN                ! Number of input coordinates
      INTEGER PNTR               ! Pointer to mapped string
      INTEGER UBND( NDF__MXDIM ) ! Upper pixel-index bounds
      LOGICAL THERE              ! Component present?

*  Local Data:
      DATA IND / NDF__MXDIM * 1.0D0 / ! Indices of first pixel

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the number of input coordinates and number of current Frame
*  axes for the FrameSet supplied.
      NIN = AST_GETI( IWCS, 'Nin', STATUS )
      NAXES = AST_GETI( IWCS, 'Naxes', STATUS )

*  Transform the indices of the first pixel in the NDF into the current
*  coordinate system to give the coordinates of the pixel's
*  centre. Then normalise the resulting coordinates.
      CALL AST_TRANN( IWCS, 1, NIN, 1, IND, .TRUE., NAXES, 1, COORD,
     :                STATUS )
      CALL AST_NORM( IWCS, COORD, STATUS )

*  Now convert these coordinates into a character string. Start with an
*  opening '(' and then loop through each axis of the current Frame.
      NC = 0
      CALL CHR_PUTC( '(', COSTR, NC )
      DO 1 IAXIS = 1, NAXES

*  Format each coordinate value and concatenate the resulting strings,
*  separated by commas.
         IF ( IAXIS .GT. 1 ) CALL CHR_PUTC( ',', COSTR, NC )
         FMTVAL = AST_FORMAT( IWCS, IAXIS, COORD( 1, IAXIS ), STATUS )
         LFMT = CHR_LEN( FMTVAL )
         IF ( LFMT .GT. 0 ) THEN
            CALL CHR_PUTC( FMTVAL( : LFMT ), COSTR, NC )
         END IF
 1    CONTINUE

*  Append a closing ')'.
      CALL CHR_PUTC( ')', COSTR, NC )

*  Obtain the Domain attribute value for the current Frame. Then
*  initialise the Frame's attributes according to this domain.
      DOMAIN = AST_GETC( IWCS, 'Domain', STATUS )

*  Grid index coordinates.
*  -----------------------
      IF ( DOMAIN .EQ. 'GRID' ) THEN

*  Set up a suitable Frame title.
         IF ( NAXES .EQ. 1 ) THEN
            CALL AST_SETC( IWCS, 'Title',
     :           'Data grid index; first pixel at ' //
     :           COSTR( : NC ), STATUS )
         ELSE
            CALL AST_SETC( IWCS, 'Title',
     :           'Data grid indices; first pixel at ' //
     :           COSTR( : NC ), STATUS )
         END IF

*  For each axis, set up a format, label, symbol and unit value.
         DO 2 IAXIS = 1, NAXES
            NC = 0
            CALL CHR_PUTI( IAXIS, AXIS, NC )
            CALL AST_SETC( IWCS, 'Format(' // AXIS( : NC ) // ')',
     :                     PIXFMT, STATUS )
            CALL AST_SETC( IWCS, 'Label(' // AXIS( : NC ) // ')',
     :                     'Data grid index ' // AXIS( : NC ), STATUS )
            CALL AST_SETC( IWCS, 'Symbol(' // AXIS( : NC ) // ')',
     :                     'g' // AXIS( : NC ), STATUS )
            CALL AST_SETC( IWCS, 'Unit(' // AXIS( : NC ) // ')',
     :                     'pixel', STATUS )
 2       CONTINUE

*  Pixel coordinate system.
*  ------------------------
      ELSE IF ( DOMAIN .EQ. 'PIXEL' ) THEN

*  Set up a suitable Frame title.
         IF ( NAXES .EQ. 1 ) THEN
            CALL AST_SETC( IWCS, 'Title',
     :                     'Pixel coordinate; first pixel at ' //
     :                     COSTR( : NC ), STATUS )
         ELSE
            CALL AST_SETC( IWCS, 'Title',
     :                     'Pixel coordinates; first pixel at ' //
     :                     COSTR( : NC ), STATUS )
         END IF

*  For each axis, set up a format, label, symbol and unit value.
         DO 3 IAXIS = 1, NAXES
            NC = 0
            CALL CHR_PUTI( IAXIS, AXIS, NC )
            CALL AST_SETC( IWCS, 'Format(' // AXIS( : NC ) // ')',
     :                     PIXFMT, STATUS )
            CALL AST_SETC( IWCS, 'Label(' // AXIS( : NC ) // ')',
     :                     'Pixel coordinate ' // AXIS( : NC ),
     :                     STATUS )
            CALL AST_SETC( IWCS, 'Symbol(' // AXIS( : NC ) // ')',
     :                     'p' // AXIS( : NC ), STATUS )
            CALL AST_SETC( IWCS, 'Unit(' // AXIS( : NC ) // ')',
     :                     'pixel', STATUS )
 3       CONTINUE

*  Axis coordinate system.
*  -----------------------
      ELSE IF ( DOMAIN .EQ. 'AXIS' ) THEN

*  Set up a suitable Frame title.
         IF ( NAXES .EQ. 1 ) THEN
            CALL AST_SETC( IWCS, 'Title',
     :                     'Axis coordinate; first pixel at ' //
     :                     COSTR( : NC ), STATUS )
         ELSE
            CALL AST_SETC( IWCS, 'Title',
     :                     'Axis coordinates; first pixel at ' //
     :                     COSTR( : NC ), STATUS )
         END IF

*  To obtain label and unit strings, we must access the NDF's AXIS
*  component. Obtain an index to the NDF entry in the DCB and determine
*  how many dimensions it has.
         IDCB = ACB_IDCB( IACB )
         CALL ARY_BOUND( DCB_DID( IDCB ), NDF__MXDIM, LBND, UBND,
     :                   NDIM, STATUS )

*  Loop through all the current Frame axes.
         DO 4 IAXIS = 1, NAXES
            NC = 0
            CALL CHR_PUTI( IAXIS, AXIS, NC )

*  Determine if there is a corresponding dimension in the NDF data
*  object.
            THERE = IAXIS .LE. NDIM

*  If so, then ensure that DCB information is available for the axis
*  label component. Then test the component's locator to see if the
*  component exists.
            IF ( THERE ) THEN
               CALL NDF1_DAC( IAXIS, NDF__ALAB, IDCB, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  THERE = DCB_ACLOC( IAXIS, NDF__ALAB, IDCB ) .NE.
     :                    DAT__NOLOC
               END IF
            END IF

*  If there is no axis label information available, then use a default
*  label string.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( .NOT. THERE ) THEN
                  CALL AST_SETC( IWCS, 'Label(' // AXIS( : NC ) // ')',
     :                                 'Axis ' // AXIS( : NC ),
     :                           STATUS )

*  Otherwise, determine the component's length and map it for reading.
               ELSE
                  CALL DAT_CLEN( DCB_ACLOC( IAXIS, NDF__ALAB, IDCB ),
     :                           CLEN, STATUS )
                  CALL DAT_MAP( DCB_ACLOC( IAXIS, NDF__ALAB, IDCB ),
     :                          '_CHAR', 'READ', 0, DIM, PNTR, STATUS )

*  Set the Frame's axis label to the value of the mapped string. Then
*  un-map the label component.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CALL NDF1_ASETC( IWCS, %VAL( CNF_PVAL( PNTR ) ),
     :                                'Label(' // AXIS( : NC ) // ')',
     :                                STATUS, %VAL( CNF_CVAL( CLEN ) ) )
                     CALL DAT_UNMAP( DCB_ACLOC( IAXIS, NDF__ALAB,
     :                                          IDCB ), STATUS )
                  END IF
               END IF
            END IF

*  Repeat the process above to set a value for the axis unit
*  attribute...

*  Check if units available.
            THERE = IAXIS .LE. NDIM

            IF ( THERE ) THEN
               CALL NDF1_DAC( IAXIS, NDF__AUNI, IDCB, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  THERE = DCB_ACLOC( IAXIS, NDF__AUNI, IDCB ) .NE.
     :                    DAT__NOLOC
               END IF
            END IF

*  Supply a suitable default format and a default unit of 'pixel', but
*  only if the axis component is absent. If it is present, clear these
*  attributes, since the physical units are unknown (and may not be
*  pixels).
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( .NOT. THERE ) THEN
                  IF ( IAXIS .GT. NDIM ) THEN
                     CALL AST_SETC( IWCS,
     :                              'Format(' // AXIS( : NC ) // ')',
     :                              PIXFMT, STATUS )
                     CALL AST_SETC( IWCS,
     :                              'Unit(' // AXIS( : NC ) // ')',
     :                              'pixel', STATUS )
                  ELSE IF ( DCB_ALOC( IAXIS, IDCB ) .EQ.
     :                      DAT__NOLOC ) THEN
                     CALL AST_SETC( IWCS,
     :                              'Format(' // AXIS( : NC ) // ')',
     :                              PIXFMT, STATUS )
                     CALL AST_SETC( IWCS,
     :                              'Unit(' // AXIS( : NC ) // ')',
     :                              'pixel', STATUS )
                  ELSE
                     CALL AST_CLEAR( IWCS,
     :                               'Format(' // AXIS( : NC ) // ')',
     :                               STATUS )
                     CALL AST_CLEAR( IWCS,
     :                               'Unit(' // AXIS( : NC ) // ')',
     :                               STATUS )
                  ENDIF

*  Map the units string.
               ELSE
                  CALL DAT_CLEN( DCB_ACLOC( IAXIS, NDF__AUNI, IDCB ),
     :                           CLEN, STATUS )
                  CALL DAT_MAP( DCB_ACLOC( IAXIS, NDF__AUNI, IDCB ),
     :                          '_CHAR', 'READ', 0, DIM, PNTR, STATUS )

*  Use the string and unmap it.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CALL NDF1_ASETC( IWCS, %VAL( CNF_PVAL( PNTR ) ),
     :                                'Unit(' // AXIS( : NC ) // ')',
     :                                STATUS, %VAL( CNF_CVAL( CLEN ) ) )
                     CALL DAT_UNMAP( DCB_ACLOC( IAXIS, NDF__AUNI,
     :                                          IDCB ), STATUS )
                  END IF
               END IF
            END IF

*  Set an axis symbol value for the Frame.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL AST_SETC( IWCS, 'Symbol(' // AXIS( : NC ) // ')',
     :                        'a' // AXIS( : NC ), STATUS )
            END IF
 4       CONTINUE

*  Normalised pixel coordinate system.
*  -----------------------------------
      ELSE IF ( DOMAIN .EQ. 'FRACTION' ) THEN

*  Set up a suitable Frame title.
         IF ( NAXES .EQ. 1 ) THEN
            CALL AST_SETC( IWCS, 'Title',
     :                     'Normalised pixel coordinate; first pixel'//
     :                     ' at '//COSTR( : NC ), STATUS )
         ELSE
            CALL AST_SETC( IWCS, 'Title',
     :                     'Normalised pixel coordinates; first pixel'//
     :                     ' at '//COSTR( : NC ), STATUS )
         END IF

*  For each axis, set up a format, label, symbol and unit value.
         DO 5 IAXIS = 1, NAXES
            NC = 0
            CALL CHR_PUTI( IAXIS, AXIS, NC )
            CALL AST_SETC( IWCS, 'Format(' // AXIS( : NC ) // ')',
     :                     FRAFMT, STATUS )
            CALL AST_SETC( IWCS, 'Label(' // AXIS( : NC ) // ')',
     :                     'Normalised pixel coordinate ' //
     :                     AXIS( : NC ), STATUS )
            CALL AST_SETC( IWCS, 'Symbol(' // AXIS( : NC ) // ')',
     :                     'f' // AXIS( : NC ), STATUS )
            CALL AST_SETC( IWCS, 'Unit(' // AXIS( : NC ) // ')',
     :                     ' ', STATUS )
 5       CONTINUE

      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_INIFR', STATUS )

      END
