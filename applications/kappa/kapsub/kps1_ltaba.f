      SUBROUTINE KPS1_LTABA( NINTS, FULL, IMDSET, PENS, STATUS )
*+
*  Name:
*     KPS1_LTABA

*  Purpose:
*     Manipulates an image-display colour table for LUTABLE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LTABA( NINTS, FULL, IMDSET, PENS, STATUS )

*  Description:
*     This routine is a service routine for LUTABLE.  It performs all
*     the work of LUTABLE, using the supplied work arrays in place of
*     the fixed size arrays which used to be used. Most of the LUTABLE
*     parameter names are hard-wired into this routine.  See the
*     prologue in lutable.f for parameter details.

*  Arguments:
*     NINTS = INTEGER (Given)
*        The total number of entries in the PGPLOT colour table. The
*        lowest entry has index zero (the background colour), and the
*        highest has index NINTS - 1.
*     FULL = LOGICAL (Given)
*        Full colour table is to be used?
*     IMDSET( 3, 0:NINTS - 1 ) = REAL (Returned)
*        Lookup table that will be used for the image-display colour
*        table.
*     PENS( 0:NINTS - 1 ) = INTEGER (Returned)
*        Entries in the colour table assigned to each pen.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-OCT-2001 (DSB):
*        Original version, extracted from lutable.f.  Also added check
*        that greyscale colour tables do not have a hint of colour about
*        them due to different resolutions on the three primary colours.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2006 February 24 (MJC):
*        Added new CUMUL argument set to .FALSE. to KPG1_GHSTx calls.
*        Also remove fourth workspace for the revised KPS1_HEQPx API.
*      2006 March 9 (MJC):
*        Correct colour-index offset when the palette is being used.
*     2011-08-22 (TIMJ):
*        Add new WGTS and WEIGHT arguments to KPG1_GHSTx calls.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'PAR_PAR'          ! PAR_ constants
      INCLUDE 'CTM_PAR'          ! Colour-table management constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF error definitions
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NINTS
      LOGICAL FULL

*  Arguments Returned:
      REAL IMDSET( 3, 0:NINTS - 1 )
      INTEGER PENS( 0:NINTS - 1 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL LFCT1                 ! Parameter in logarithmic scaling
      PARAMETER ( LFCT1 = 8.0 )

      INTEGER NDIM               ! Dimensionality of the NDFs
      PARAMETER ( NDIM = 2 )

      INTEGER NLUTST             ! Number of entries in the standard
                                 ! coloured lookup table
      PARAMETER ( NLUTST = 18 )

      INTEGER NPRCTL             ! Maximum number of percentiles
      PARAMETER( NPRCTL = 2 )

      INTEGER NPRICL             ! Number of primary colours
      PARAMETER ( NPRICL = 3 )

      INTEGER NUMBIN             ! Maximum number of histogram bins
      PARAMETER( NUMBIN = 2048 ) ! should be enough

*  Local Variables:
      INTEGER ANINTS             ! Number of colour indices excluding
                                 ! reserved pens
      LOGICAL BAD                ! May array contain bad pixels and
                                 ! therefore testing should occur?
      INTEGER CIFILL             ! Number of extra colour indices to
                                 ! fill above equal-sized coloured
                                 ! blocks
      INTEGER CIOFF              ! Offset of first colour index to
                                 ! which to write
      REAL CIFRAC                ! Fractional colour-index counter for
                                 ! coloured blocks
      REAL COLSET( NPRICL, NLUTST ) ! Standard coloured LUT
      REAL COPB                  ! Average number of colour indices in a
                                 ! standard colour-set block
      DOUBLE PRECISION DMAXV     ! Minimum value in the array
      DOUBLE PRECISION DMINV     ! Maximum value in the array
      CHARACTER DTYPE * ( NDF__SZFTP ) ! Type of the image after
                                 ! processing (not used)
      INTEGER EL                 ! Number of elements in the input array
      LOGICAL EXL1ST             ! First time to get an external LUT?
      LOGICAL FIRST              ! First time through the loop?
      REAL FROB                  ! Fraction of standard coloured blocks
                                 ! that will have an extra index
      REAL PGPCOL( NPRICL )      ! Used to transfer the colour of one
                                 ! pen to the image-display colour table
      INTEGER HIST( NUMBIN )     ! Array containing histogram for
                                 ! percentiles
      INTEGER I                  ! General variable
      LOGICAL IMAGE              ! Array has been read in successfully?
      INTEGER IPIC2              ! Graphics' database ident. recalled
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing type of the image
      INTEGER J                  ! General variable
      INTEGER K                  ! General variable
      INTEGER L                  ! General variable
      REAL LFCT2                 ! Logarithmic scaling factor
      INTEGER LDIMS( NDIM )      ! Dimensions of lookup table in image
                                 ! file
      INTEGER LEL                ! Number of elements in the input LUT
      LOGICAL LOOP               ! Multiple attempts at setting lookup
                                 ! tables is allowed?
      INTEGER LPNTR( 1 )         ! Pointer to the lookup table
      CHARACTER * ( 12 ) LUT     ! Type of lookup table
      INTEGER LUTMIN             ! Minimum number of colour indices in a
                                 ! standard colour-set block
      INTEGER MAPSTA             ! The state of the MAPPING parameter
      INTEGER MAXPOS             ! Position of the maximum (not used)
      INTEGER MINPOS             ! Position of the minimum (not used)
      INTEGER NCI                ! Number of colour indices in coloured
                                 ! block
      INTEGER NDF                ! NDF identifier for the image array
      INTEGER NDFL               ! NDF identifier for the LUT
      INTEGER NDIMS              ! Total number of NDF dimensions
      INTEGER NFILL              ! Number of extra colour indices used
                                 ! for coloured blocks
      INTEGER NINVAL             ! No. of bad values in the input array
      LOGICAL NN                 ! Mapping the input LUT via
                                 ! nearest-neighbour method?
      REAL OFFSET                ! Logarithmic offset
      CHARACTER * ( 12 ) PENTBL  ! Type of colour distribution to be
                                 ! applied to the pens
      REAL PERCNT( NPRCTL )      ! Percentiles
      REAL PERDEF( NPRCTL )      ! Suggested default percentiles
      DOUBLE PRECISION PERVAL( NPRCTL ) ! Values at the percentiles
      INTEGER PNTRI( 1 )         ! Pointer to input NDF array
      CHARACTER * ( 132 ) REFNAM ! Reference data associated with the
                                 ! last DATA picture
      LOGICAL REFOBJ             ! Is there a reference object?
      REAL RMAXV                 ! Minimum value in the array
      REAL RMINV                 ! Maximum value in the array
      REAL RNINTS                ! Scaling factor to convert lookup
                                 ! table to range 0 to 1
      INTEGER SDIM( NDIM )       ! Indices of significant axes
      REAL SHADE                 ! Type of shading emphasis
      INTEGER SLBND( NDIM )      ! Lower bounds of significant axes
      INTEGER SUBND( NDIM )      ! Upper bounds of significant axes
      INTEGER TABSTA             ! The state of the COLTAB parameter
      LOGICAL VALID              ! NDF identifier is valid?
      INTEGER WPNTR1             ! Pointer to a work array
      INTEGER WPNTR2             ! Pointer to a work array
      INTEGER WPNTR3             ! Pointer to a work array

*  Local Data:
      DATA COLSET/0.0, 0.0, 0.5,
     :            0.0, 0.0, 0.75,
     :            0.0, 0.0, 1.0,  ! Blue
     :            0.0, 0.5, 1.0,  ! SlateBlue
     :            0.0, 0.75, 1.0,
     :            0.0, 1.0, 1.0,  ! Cyan
     :            0.0, 1.0, 0.75,
     :            0.0, 1.0, 0.5,  ! SpringGreen
     :            0.0, 1.0, 0.0,
     :            0.5, 1.0, 0.0,  ! MediumSpringGreen
     :            0.75, 1.0, 0.0,
     :            1.0, 1.0, 0.0,  ! Yellow
     :            1.0, 0.75, 0.0,
     :            1.0, 0.5, 0.0,  ! Coral
     :            1.0, 0.0, 0.0,  ! Red
     :            1.0, 0.0, 0.5,  ! OrangeRed
     :            1.0, 0.0, 1.0,  ! Magenta
     :            1.0, 1.0, 1.0/  ! White

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Map the lookup table to the colour table by interpolation or by
*  nearest neighbour?
      CALL PAR_GTD0L( 'NN', .FALSE., .TRUE., NN, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Derive the colour-index offset depending on whether reserved colour
*  indices are to be written to or not.
      IF( FULL ) THEN
         CIOFF = 0
      ELSE
         CIOFF = CTM__RSVPN - 1
      END IF

*  Find the available number of colour indices and a useful expression
*  needed during the calculations.
      ANINTS = NINTS - CIOFF
      RNINTS = REAL( ANINTS - 1 )

*  Initialise some flags.
      LOOP = .TRUE.
      IMAGE = .FALSE.
      FIRST = .TRUE.
      EXL1ST = .TRUE.

*  Start an NDF context.
      CALL NDF_BEGIN

*  This is the main loop in which the lookup table and mapping may be
*  changed.
      DO WHILE ( LOOP )

*  Determine whether looping is required.
*  ======================================
*
*  The only case where it is not needed is when the lookup table and
*  mapping are both given on the command line.
         CALL LPG_STATE( 'COLTAB', TABSTA, STATUS )
         CALL LPG_STATE( 'MAPPING', MAPSTA, STATUS )
         LOOP = TABSTA .NE. PAR__ACTIVE .OR.
     :          MAPSTA .NE. PAR__ACTIVE

*  Let the user know how to exit the loop the first time around.
         IF( FIRST .AND. LOOP ) CALL MSG_OUT( 'COMMENT',
     :     'Type a ! in response to a prompt to exit the loop.',
     :     STATUS )
         FIRST = .FALSE.

*  Set up the colour distribution/mapping.
*  =======================================

*  Start a new error context.
         CALL ERR_MARK

*  Ask for the manner in which the lookup table to be distributed among
*  the pens.
         CALL PAR_CHOIC( 'MAPPING', 'Linear',
     :                   'Linear,Histogram,Logarithmic', .FALSE.,
     :                   PENTBL, STATUS )

*  If the entry was invalid or null then exit from the loop leaving the
*  display unchanged.
         IF( STATUS .NE. SAI__OK ) THEN

            IF( STATUS .NE. PAR__ABORT ) THEN
               CALL ERR_REP( 'ERR_LUTABLE_DUN',
     :           'LUTABLE: Display unchanged.', STATUS )
            END IF

*  A null is not an error here.
            IF( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_FLUSH( STATUS )
            END IF
            CALL ERR_RLSE

*  Exit from the outer loop.
            GO TO 960
         END IF

*  Release the new error context.
         CALL ERR_RLSE

*  Linear mapping.
*  ===============

*  If the colours are to be mapped straight onto the pens then, set up
*  the colour lookup table for the pens so that each pen is assigned
*  its appropriate colour.
         IF( PENTBL( 1:2 ) .EQ. 'LI' ) THEN

            DO  I = 0, ANINTS - 1, 1
               PENS( I ) = I
            END DO

*  Logarithmic mapping.
*  ====================

*  If the colours are to be mapped logarithmically, then set up the pen
*  lookup array accordingly, with pen n pointing to colour n.
         ELSE IF( PENTBL( 1:2 ) .EQ. 'LO' ) THEN

*  Determine the transformation parameters so the pens span the same
*  range.  First the approximate scaling to compute an offset.
            LFCT2 = ( EXP( LFCT1 * RNINTS / REAL( ANINTS ) ) - 1 ) /
     :              REAL( NINTS )

*  Compute offset for pen 1.
            OFFSET = - REAL( ANINTS ) / LFCT1 * LOG( LFCT2 + 1.0 )

*  Substitute an offset into the equation for LFCT2 (was zero before).
*  If this is not done about 30--50 per cent of the available pens will
*  effectively be unused.
            LFCT2 = ( EXP( LFCT1 * ( RNINTS - OFFSET )/
     :                REAL( ANINTS ) ) - 1 ) / REAL( ANINTS )

*  Set up the pen lookup array with the logarithmic scaling, with pen n
*  pointing to colour n.
            DO  I = 0, ANINTS - 1, 1
               PENS( I ) = MAX( 0, MIN( ANINTS - 1, INT( ANINTS /
     :                     LFCT1 * LOG( REAL( I ) * LFCT2 + 1.0 ) )  ) )
            END DO

*  Histogram-equalisation mapping.
*  ===============================

*  If the colours are to have equal use when displaying the array, then
*  try to do a histogram equalisation.  If this fails, then just use the
*  linear table.
         ELSE IF( PENTBL( 1:2 ) .EQ. 'HI' ) THEN

*  If no array has been read in already, then read one in.
            IF( .NOT. IMAGE ) THEN

*  This flag is needed to know what should be tidied.
               IMAGE = .TRUE.

*  Determine whether or not an array is already displayed.
               CALL AGI_RCL( 'DATA', IPIC2, STATUS )

               IF( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'ERR_LUTABLE_NDB',
     :              'LUTABLE:  Some data must be displayed before '/
     :              /'using this option.', STATUS )
                  GO TO 960
               END IF

*  Report the object associated with the DATA picture.
*  ===================================================
*
*  Preferably, the name should be the suggested default, but until
*  NDF_DEF appears merely report the name.

*  Determine whether or not there is a reference object associated with
*  the current picture.
               CALL KPG1_AGREF( IPIC2, 'READ', REFOBJ, REFNAM, STATUS )

*  If one exists translate its locator reference to a token containing
*  the path name and file name, and tidy the reference locator; or just
*  use the reference name.
               IF( REFOBJ ) THEN
                  CALL DAT_VALID( REFNAM( :DAT__SZLOC ), VALID, STATUS )
                  IF( VALID ) THEN
                     CALL KPG1_HMSG( 'NAME', REFNAM( :DAT__SZLOC ) )
                     CALL REF_ANNUL( REFNAM( :DAT__SZLOC ), STATUS )
                  ELSE
                     CALL MSG_SETC( 'NAME', REFNAM )
                  END IF
                  CALL MSG_OUT( 'NAME',
     :              '   Reference data object: ^NAME', STATUS )
               END IF

*  Associate the NDF and inquire its attributes.
*  =============================================

*  Obtain the identifier of the NDF already displayed.  It must have
*  exactly two significant dimensions.
               CALL KPG1_GTNDF( 'NDF', NDIM, .TRUE., 'Read', NDF, SDIM,
     :                          SLBND, SUBND, STATUS )

*  This application can only process real or double-precision
*  components directly.  Therefore for the given type of the image find
*  in which type it should be processed.
               CALL NDF_MTYPE( '_REAL,_DOUBLE', NDF, NDF, 'Data', ITYPE,
     :                         DTYPE, STATUS )

*  Check whether or not bad pixels may be present.
               CALL NDF_BAD( NDF, 'Data', .FALSE., BAD, STATUS )

*  Map the input image.
               CALL KPG1_MAP( NDF, 'Data', ITYPE, 'READ', PNTRI, EL,
     :                       STATUS )

*  Obtain the maximum and minimum values.
               IF( ITYPE .EQ. '_REAL' ) THEN
                   CALL KPG1_MXMNR( BAD, EL,
     :                              %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              NINVAL,
     :                              RMAXV, RMINV, MAXPOS, MINPOS,
     :                              STATUS )
               ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
                   CALL KPG1_MXMND( BAD, EL,
     :                              %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                              NINVAL,
     :                              DMAXV, DMINV, MAXPOS, MINPOS,
     :                              STATUS )
               END IF

*  If the data were not accessed successfully then leave the
*  application.
               IF( STATUS .NE. SAI__OK ) THEN
                  CALL NDF_VALID( NDF, VALID, STATUS )
                  IF( VALID ) CALL NDF_ANNUL( NDF, STATUS )
                  IMAGE = .FALSE.
                  GO TO 960
               END IF

*  End of image-already-accessed check.
            END IF

*  Get the parameters for the histogram equalisation.
*  ==================================================

*  Get the degree of shading.
            CALL PAR_GDR0R( 'SHADE', 0.0, -1.0, 1.0, .TRUE., SHADE,
     :                      STATUS )

*  Find the percentiles required.  There is no dynamic default.
            DO I = 1, NPRCTL
               PERDEF( I ) = VAL__BADR
            END DO
            CALL PAR_GDR1R( 'PERCENTILES', NPRCTL, PERDEF, 0.0001,
     :                      99.9999, .FALSE., PERCNT, STATUS )

*  Convert the percentiles to fractions.
            DO  I = 1, NPRCTL
               PERCNT( I ) = PERCNT( I ) * 0.01
            END DO

*  The number of bad pixels has been counted so it might be possible to
*  save future processing.
            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*  Generate the histogram between those bounds, calling the routine of
*  the appropriate type.  The d.p. verson of the range is needed for the
*  percentile routine.
            IF( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_GHSTR( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), 0.0D0,
     :                          NUMBIN, .FALSE., RMAXV, RMINV, HIST,
     :                          STATUS )
               DMAXV = DBLE( RMAXV )
               DMINV = DBLE( RMINV )

            ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_GHSTD( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), 0.0D0,
     :                          NUMBIN, .FALSE., DMAXV, DMINV, HIST,
     :                          STATUS )
            END IF

*  Estimate the values at the percentiles.
            CALL KPG1_HSTFD( NUMBIN, HIST, DMAXV, DMINV,
     :                       NPRCTL, PERCNT, PERVAL, STATUS )

*  If the data were not accessed successfully then leave the
*  application
            IF( STATUS .NE. SAI__OK ) GO TO 960

*  Cancel the parameters for the loop.
            IF( LOOP ) THEN
               CALL PAR_CANCL( 'SHADE', STATUS )
               CALL PAR_CANCL( 'PERCENTILES', STATUS )
            END IF

*  Create and map workspace arrays.
*  ================================
            CALL PSX_CALLOC( ANINTS, '_DOUBLE', WPNTR1, STATUS )
            CALL PSX_CALLOC( ANINTS, '_DOUBLE', WPNTR2, STATUS )
            CALL PSX_CALLOC( ANINTS, '_INTEGER', WPNTR3, STATUS )

*  Perform the histogram equalisation.
*  ===================================
*  Start a new error context so that an error can be handled invisibly.
*  Call a routine appropriate for the data type.
            CALL ERR_MARK
            IF( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_HEQPR( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          SHADE,
     :                          REAL( PERVAL( 2 ) ),
     :                          REAL( PERVAL( 1 ) ), ANINTS, PENS,
     :                          %VAL( CNF_PVAL( WPNTR1 ) ),
     :                          %VAL( CNF_PVAL( WPNTR2 ) ),
     :                          %VAL( CNF_PVAL( WPNTR3 ) ), STATUS )

            ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_HEQPD( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          SHADE,
     :                          REAL( PERVAL( 2 ) ),
     :                          REAL( PERVAL( 1 ) ), ANINTS, PENS,
     :                          %VAL( CNF_PVAL( WPNTR1 ) ),
     :                          %VAL( CNF_PVAL( WPNTR2 ) ),
     :                          %VAL( CNF_PVAL( WPNTR3 ) ), STATUS )
            END IF

            IF( STATUS .NE. SAI__OK ) THEN

*  Use a linear lookup table.
               CALL ERR_REP( 'ERR_LUTABLE_HSE',
     :           'LUTABLE: Unable to do histogram equalisation. '/
     :           /'Linear used.', STATUS )
               CALL ERR_FLUSH( STATUS )

               DO  I = 0, ANINTS - 1, 1
                  PENS( I ) = I
               END DO

            END IF
            CALL ERR_RLSE

*  Tidy up all the structures
            CALL PSX_FREE( WPNTR1, STATUS )
            CALL PSX_FREE( WPNTR2, STATUS )
            CALL PSX_FREE( WPNTR3, STATUS )
         END IF

*  End of the section to define the colour mapping.
*  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

         IF( STATUS .EQ. SAI__OK ) THEN

*  Start a new error context.
            CALL ERR_MARK

*  Define the colour sets.
*  =======================

*  Get the name of a colour set.
            CALL PAR_CHOIC( 'COLTAB', 'Colour',
     :                      'Colour,Grey,Negative,External', .FALSE.,
     :                      LUT, STATUS )

            IF( STATUS .NE. SAI__OK ) THEN

               IF( STATUS .NE. PAR__ABORT ) THEN
                  CALL ERR_REP( 'ERR_LUTABLE_COS',
     :              'LUTABLE: Pens unchanged.', STATUS )
               END IF

*  A null is not an error here.
               IF( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_FLUSH( STATUS )
               END IF
               CALL ERR_RLSE

*  Leaving the outer loop.
               GO TO 960
            END IF

*  Release the new error context.
            CALL ERR_RLSE

*  Grey LUT.
*  =========
            IF( LUT( 1:2 ) .EQ. 'GR' ) THEN

*  Set up a grey scale.
               DO  I = 0, ANINTS - 1, 1
                  DO  J = 1, NPRICL, 1
                     IMDSET( J, I ) = REAL( I ) / RNINTS
                  END DO
               END DO

*  Negative grey LUT.
*  ==================
            ELSE IF( LUT( 1:2 ) .EQ. 'NE' ) THEN

*  Set up a negative grey scale.
               DO  I = 0, ANINTS - 1, 1
                  DO  J = 1, NPRICL, 1
                     IMDSET( J, I ) = REAL( ANINTS - 1 - I ) / RNINTS
                  END DO
               END DO

*  Coloured LUT.
*  =============
            ELSE IF( LUT( 1:2 ) .EQ. 'CO' ) THEN

*  Try to make the blocks equal sized, but this is usually not
*  possible.  Therefore make some blocks one index larger to fill the
*  available number of colours.  Find the minimum number of pens in a
*  coloured block and the average number of pens per coloured block.
               LUTMIN = ANINTS / NLUTST

               IF( LUTMIN .GE. 1 ) THEN
                  COPB = REAL( ANINTS ) / REAL( NLUTST )

*  Hence derive the fraction of blocks that are one colour index larger
*  than the minimum and the number of colour indices to fill.  The
*  former has a small offset to allow for rounding errors.
                  FROB = COPB - REAL( INT( COPB ) ) + VAL__EPSR
                  CIFILL = ANINTS - NLUTST * LUTMIN

*  Allow for the case when the number of colour indices is less than
*  the number of colours in the default colour table.  Use one entry
*  for each colour in order, and storing as many colours as there are
*  free slots.
               ELSE
                  FROB = 1.0
                  CIFILL = ANINTS
               END IF

*  Set up a standard colour set.
               K = -1
               CIFRAC = 0.0
               NFILL = 0

*  Allow for there being less colour indices than the number of colours
*  in the default colour table.
               DO  I = 1, MIN( ANINTS, NLUTST ) , 1

*  Increment the colour-index fraction.  This determines whether this
*  block is to be larger.  Also prevent the index from exceeding the
*  colour-table bounds.
                  CIFRAC = CIFRAC + FROB
                  IF( CIFRAC .GT. 1.0 .AND. NFILL .LT. CIFILL ) THEN

*  The block is extended by one colour index.
                      NCI = LUTMIN + 1

*  Reset the fraction.
                      CIFRAC = CIFRAC - 1.0

*  Count the number of extra colour indices used.
                      NFILL = NFILL + 1
                  ELSE
                      NCI = LUTMIN
                  END IF

*  Copy the coloured-block to the lookup table.
                  DO  J = 1, NCI, 1
                     K = K+1

                     DO  L = 1, NPRICL, 1
                        IMDSET( L, K ) = COLSET( L, I )
                     END DO
                  END DO
               END DO

*  Fill in any left over pens by copying the last pen.
               DO J = K + 1, NINTS - 1
                  DO  L = 1, NPRICL
                     IMDSET( L, J ) = IMDSET( L, K )
                  END DO
               END DO


*  LUT in an NDF.
*  ==============
            ELSE IF( LUT( 1:2 ) .EQ. 'EX' ) THEN

*  Start another NDF context.

               CALL NDF_BEGIN

               IF( .NOT. EXL1ST ) THEN

*  We want to be able to store the last LUT file accessed, yet at the
*  same time provide looping for which we must cancel the LUT
*  parameter.  In order to achieve these conflicting requirements,
*  cancel the parameter here, watching out for an error, say because
*  there is no parameter to cancel.  Use a new error context to store
*  and purge any error that arises.
                  CALL ERR_MARK
                  CALL PAR_CANCL( 'LUT', STATUS )
                  IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
                  CALL ERR_RLSE
               END IF

*  Start a new error context.
               CALL ERR_MARK

*  Obtain the NDF identifier and pointer of the input lookup table.
*  Validate the LUT.
               CALL KPG1_AVLUT( 'LUT', NDFL, LPNTR, LEL, STATUS )

*  Obtain the array dimensions.
               CALL NDF_DIM( NDFL, NDIM, LDIMS, NDIMS, STATUS )

*  Null status means exit the loop, but without reporting an error.
*  Also skip over the portion
               IF( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL ERR_RLSE
                  GO TO 960

*  Abort immediately.
               ELSE IF( STATUS .EQ. PAR__ABORT ) THEN
                  CALL ERR_RLSE
                  GO TO 960

*  Something else has gone wrong.
               ELSE IF( STATUS .NE. SAI__OK ) THEN

*  We want to continue if we are in a loop.
                  IF( LOOP ) THEN
                     CALL ERR_FLUSH( STATUS )
                     CALL MSG_OUT( 'GREYSCALE',
     :                 'Using a greyscale lookup table.', STATUS )

*  Set up the greyscale lookup table.
                     DO  I = 0, ANINTS - 1, 1
                        DO  J = 1, NPRICL, 1
                           IMDSET( J, I ) = REAL( I ) / RNINTS
                        END DO
                     END DO

*  We are not looping so tidy up and abort.  We do not want to record
*  this LUT in the parameter system.  This is taken care of by the bad
*  status rather than an explicit PAR_CANCL call.
                  ELSE
                     CALL NDF_VALID( NDFL, VALID, STATUS )
                     IF( VALID ) CALL NDF_ANNUL( NDFL, STATUS )
                     CALL ERR_RLSE
                     GO TO 960
                  END IF

*  A valid LUT has been obtained and mapped.
               ELSE

*  If the structure was found then read in the lookup table.
                  CALL KPG1_LUTIN( LDIMS( 2 ),
     :                             %VAL( CNF_PVAL( LPNTR( 1 ) ) ),
     :                             ANINTS, NN, IMDSET, STATUS )
               END IF

*  Release the error context around finding the LUT in the image file.
               CALL ERR_RLSE

*  Annul the NDF for the time being.  If another LUT is read in we will
*  cancel the parameter just before obtaining the new LUT.
               CALL NDF_ANNUL( NDFL, STATUS )
               EXL1ST = .FALSE.

*  End of the colour-table cases.
            END IF

            IF( STATUS .EQ. SAI__OK ) THEN

*  Load up the image-display colour table.
*  =======================================
                DO  I = 0, ANINTS - 1, 1

*  Apply the mapping to the lookup table.
                   DO  J = 1, NPRICL, 1
                      PGPCOL( J ) = IMDSET( J, PENS( I ) )
                   END DO

*  Finally, allow for the reserved pens when setting the colour
*  representation.  Allow for rounding errors.
                    CALL PGSCR( I + CIOFF,
     :                       MIN( 1.0, MAX( 0.0, PGPCOL( 1 ) ) ),
     :                       MIN( 1.0, MAX( 0.0, PGPCOL( 2 ) ) ),
     :                       MIN( 1.0, MAX( 0.0, PGPCOL( 3 ) ) ) )

                END DO

*  If we want a greyscale (positive or negative), we now check that the
*  pens actually in use by PGPLOT are all grey.  They may not be since
*  some devices have finer resolution on some colours.  For example, a
*  16-bit TrueColour X window will have 5 bits for two colours and 6
*  bits for the other.  The 6-bit colour can be set twice as accurately
*  as the 5-bit colours, resulting in the "grey" scale having a tendency
*  to be shaded in favour of the 6-bit colour.
               IF( LUT(1:2) .EQ. 'GR' .OR. LUT(1:2) .EQ. 'NE' ) THEN

*  Check each pgplot pen.
                  DO  I = 0, ANINTS - 1, 1

*  Get the representation actually being used by PGPLOT for this pen.
                      CALL PGQCR( I + CIOFF, PGPCOL( 1 ), PGPCOL( 2 ),
     :                            PGPCOL( 3 ) )

*  If only two of the three RGB intensities are equal, we set the third
*  (unequal) intensity equal to the other intensity.
                     IF( PGPCOL( 1 ) .EQ. PGPCOL( 2 ) .AND.
     :                   PGPCOL( 1 ) .NE. PGPCOL( 3 ) ) THEN
                        PGPCOL( 3 ) = PGPCOL( 1 )

                     ELSE IF( PGPCOL( 1 ) .EQ. PGPCOL( 3 ) .AND.
     :                        PGPCOL( 1 ) .NE. PGPCOL( 2 ) ) THEN
                        PGPCOL( 2 ) = PGPCOL( 1 )

                     ELSE IF( PGPCOL( 2 ) .EQ. PGPCOL( 3 ) .AND.
     :                        PGPCOL( 1 ) .NE. PGPCOL( 3 ) ) THEN
                        PGPCOL( 1 ) = PGPCOL( 3 )

                     END IF

                     CALL PGSCR( I + CIOFF, PGPCOL( 1 ), PGPCOL( 2 ),
     :                           PGPCOL( 3 ) )

                  END DO

                END IF

*  Make the change visible immediately.
                CALL PGUPDT
             END IF

*  End-no-invalid-entry-in-stage-one check
         END IF

*  Cancel the parameters if there is a loop.
         IF( LOOP ) THEN
            CALL PAR_CANCL( 'MAPPING', STATUS )
            CALL PAR_CANCL( 'COLTAB', STATUS )
         END IF

*  End of the cycle to adjust lookup table and mapping.
      END DO

*  Unmap and annul NDF data.
 960  CONTINUE
      IF( IMAGE ) CALL NDF_END( STATUS )

*  Save the current colour table in $$ADAM_USER/kappa_lut.sdf
      CALL KPG1_LTSAV( STATUS )

*  Tidy up.
 999  CONTINUE

      END
