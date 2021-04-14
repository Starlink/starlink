      SUBROUTINE ARDMASK( STATUS )
*+
*  Name:
*     ARDMASK

*  Purpose:
*     Uses an ARD file to set some pixels of an NDF to be bad.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARDMASK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This task allows regions of an NDF's to be masked, so
*     that they can (for instance) be excluded from subsequent data
*     processing.  ARD (ASCII Region Definition) descriptions stored in
*     a text file define which pixels of the data array are masked.  An
*     output NDF is created which is the same as the input file except
*     that all pixels specified by the ARD file have been assigned either
*     the bad value or a specified constant value. This value can be
*     assigned to either the inside or the outside of the specified
*     ARD region.
*
*     If positions in the ARD description are given using a co-ordinate
*     system that has one fewer axes than the input NDF, then each plane
*     in the NDF will be masked independently using the supplied ARD
*     description. For instance, if a 2-D ARD description that uses (RA,Dec)
*     to specify positions is used to mask a 3-D (ra,dec,velocity) NDF,
*     then each velocity plane in the NDF will be masked independently.

*  Usage:
*     ardmask in ardfile out

*  ADAM Parameters:
*     ARDFILE = FILENAME (Read)
*        The name of the ARD file containing a description of the parts
*        of the image to be masked out, i.e. set to bad. The co-ordinate
*        system in which positions within this file are given should be
*        indicated by including suitable COFRAME or WCS statements within
*        the file (see SUN/183), but will default to pixel or current
*        WCS Frame co-ordinates in the absence of any such statements
*        (see parameter DEFPIX). For instance, starting the file with a
*        line containing the text "COFRAME(SKY,System=FK5)" would indicate
*        that positions are specified in RA/DEC (FK5,J2000). The statement
*        "COFRAME(PIXEL)" indicates explicitly that positions are specified
*        in pixel co-ordinates.
*     COMP = LITERAL (Read)
*        The NDF array component to be masked.  It may be "Data", or
*        "Variance", or "Error", or "All" (where "Error" is equivalent to
*        "Variance"). ["All"]
*     CONST = LITERAL (Given)
*        The constant numerical value to assign to the region, or the string
*        "bad". ["bad"]
*     DEFPIX = _LOGICAL (Read)
*        If a TRUE value is supplied for DEFPIX, then co-ordinates in
*        the supplied ARD file will be assumed to be pixel coordinates.
*        Otherwise, they are assumed to be in the current WCS co-ordinate
*        system of the supplied NDF. [TRUE]
*     IN = NDF (Read)
*        The name of the source NDF.
*     INSIDE = _LOGICAL (Read)
*        If a TRUE value is supplied, the constant value is assigned to the
*        inside of the region specified by the ARD file. Otherwise, it is
*        assigned to the outside. [TRUE]
*     OUT = NDF (Write)
*        The name of the masked NDF.
*     TITLE = LITERAL (Read)
*        Title for the output NDF structure.  A null value (!)
*        propagates the title from the input NDF to the output NDF. [!]

*  Examples:
*     ardmask a1060 galaxies.ard a1060_sky title="A1060 galaxies masked"
*        This flags pixels defined by the ARD file galaxies.ard within
*        the NDF called a1060 to create a new NDF called a1060_sky.
*        a1060_sky has a title="A1060 galaxies masked".  This might be
*        to flag the pixels where bright galaxies are located to
*        exclude them from sky-background fitting.
*     ardmask in=ic3374 ardfil=ardfile.txt out=ic3374a
*        This example uses as the source image the NDF called ic3374
*        and sets the pixels specified by the ARD description contained
*        in ardfile.txt to the bad value.  The resultant image is
*        output to the NDF called ic3374a.  The title is unchanged.

*  ASCII-region-definition Descriptors:
*     The ARD file may be created by ARDGEN or written manually.  In the
*     latter case consult SUN/183 for full details of the ARD
*     descriptors and syntax; however, much may be learnt from looking
*     at the ARD files created by ARDGEN and the ARDGEN documentation.
*     There is also a summary with examples in the main body of SUN/95.

*  Related Applications:
*     KAPPA: ARDGEN, ARDPLOT, LOOK.

*  Implementation Status:
*     -  This routine correctly processes the WCS, AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, HISTORY, and VARIANCE components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All numeric data types can be handled.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995-1998, 2001, 2004 Central Laboratory of the
*     Research Councils.
*     Copyright (C) 2007, 2012 Science & Technology Facilities Council.
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
*     GJP: Grant Privett (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     08-Jun-1994 (GJP):
*        Original version.
*     1995 June 29 (MJC):
*        Converted for KAPPA use: completed and re-ordered the
*        prologue, lowercase usage and examples, various stylistic
*        changes, standard comment indentation, removed unnecessary
*        status checks and code, allowed all numeric data types, added
*        handling of data co-ordinates and COSYS parameter, propagates
*        other components (axes, units, variance and quality), made to
*        work on n-dimensional NDFs, validated the input text file and
*        removed the need for the leading caret, and made the output
*        TITLE a parameter.
*     1996 July 31 (MJC):
*        Made ARDFILE have type FILENAME for IRAF usage.
*     1997 December 12 (MJC):
*        Sets the bad-pixel flag to indicate the possible presence of
*        bad pixels in the output dataset.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     20-AUG-2001 (DSB):
*        Converted to ARD V2 by removing COSYS parameter.
*     25-OCT-2001 (DSB):
*        Modified to make pixel coords the default coord system for ard
*        files.
*     30-NOV-2001 (DSB):
*        Added parameters COMP, CONST and INSIDE.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     27-SEP-2007 (DSB):
*        Added parameter DEFPIX.
*     2-OCT-2007 (DSB):
*        If the ARD description has one less axis than the NDF, mask each
*        plane in the NDF independently.
*     28-SEP-2011 (DSB):
*        Added "All" as an option for parameter COMP, and made it the
*        default.
*     2012 May 8 (MJC):
*        Add _INT64 support.
*     2021 Apr 14 (GSB):
*        Added support for complex data.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constant
      INCLUDE 'AST_PAR'          ! AST_ public constant and functions
      INCLUDE 'GRP_PAR'          ! GRP_ data constants
      INCLUDE 'PRM_PAR'          ! VAL_ data constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER COMP*8           ! Name of array component to mask
      CHARACTER CONTXT*40        ! Text version of constant value
      CHARACTER FILNAM*132       ! Name of ARD file
      CHARACTER TYPE*( NDF__SZTYP )  ! Numeric type for processing
      DOUBLE PRECISION CONST     ! The constant to assign
      INTEGER AWCS               ! New application FrameSet
      INTEGER AXLOOP             ! Grid axis to be looped over
      INTEGER AXVAL              ! Pixel index on axis being looped over
      LOGICAL CMPLX              ! NDF holds complex values?
      INTEGER EL                 ! Total number of pixels in the image
      INTEGER FD                 ! File descriptor
      INTEGER PFRM               ! Reduced PIXEL frame
      INTEGER I                  ! Loop count
      INTEGER ICOMP              ! Component index
      INTEGER IGRP               ! Group identifier
      INTEGER INDF1              ! Identifier for the source NDF
      INTEGER INDF2              ! Identifier for the output NDF
      INTEGER INDFS              ! Identifier for the output NDF section
      INTEGER IPIX               ! Index of PIXEL Frame within IWCS
      INTEGER IPMASK             ! Pointer to ARD logical mask
      INTEGER IPOUT( 2 )         ! Pointers to masked comp.s of o/p NDF
      INTEGER IPOUTI( 2 )        ! Pointers to masked imaginary comp.s of o/p NDF
      INTEGER IWCS               ! NDF WCS FrameSet
      INTEGER J                  ! Loop count
      INTEGER JUNK               ! Unused Mapping
      INTEGER LBND( NDF__MXDIM ) ! Lower limit for image index
      INTEGER LBNDE( NDF__MXDIM )! Lower bounds of external array elements
      INTEGER LBNDI( NDF__MXDIM )! Lower bounds of internal array elements
      INTEGER NCOMP              ! Number of components to mask
      INTEGER NDIM               ! Number of pixel axes in the image
      INTEGER NMASK              ! Number of masking operations
      INTEGER NWCS               ! Number of axes in the user coords system
      INTEGER PIXAX( NDF__MXDIM )! Grid axes being picked
      INTEGER PLBND( NDF__MXDIM )! Lower pixel index bound on picked axes
      INTEGER PUBND( NDF__MXDIM )! Upper pixel index bound on picked axes
      INTEGER REGVAL             ! Value assignied to the first ARD region
      INTEGER SPMAP              ! Mapping from user to picked grid axes
      INTEGER UBND( NDF__MXDIM ) ! Upper limit for image index
      INTEGER UBNDE( NDF__MXDIM )! Upper bounds of external array elements
      INTEGER UBNDI( NDF__MXDIM )! Upper bounds of internal array elements
      INTEGER UMAP               ! GRID->user coords Mapping
      INTEGER UWCS               ! GRID->user coords FrameSet
      INTEGER WCSAX( NDF__MXDIM )! User axes being picked
      LOGICAL BAD                ! Assign bad values to the region?
      LOGICAL CONT               ! ARD description to continue?
      LOGICAL DEFPIX             ! Use pixel coords by default?
      LOGICAL INSIDE             ! Assign value to inside of region?
      LOGICAL THERE              ! Does the requested NDF component exist?
      LOGICAL USED               ! Was the grid axis used?
      REAL TRCOEF( ( NDF__MXDIM + 1 ) * NDF__MXDIM ) ! Data to world co-ordinate conversions
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF structure to be examined.
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Determine which array component is to be masked, converting 'ERROR' into
*  'VARIANCE'.
      CALL PAR_CHOIC( 'COMP', 'All', 'All,Data,Error,Variance', .FALSE.,
     :                COMP, STATUS )
      IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

*  Assume we only have one component to map.
      NCOMP = 1

*  Check that the required component exists and report an error if it
*  does not.
      IF( COMP .NE. 'ALL' ) THEN
         CALL NDF_STATE( INDF1, COMP, THERE, STATUS )
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( .NOT. THERE ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'COMP', COMP )
            CALL NDF_MSG( 'NDF', INDF1 )
            CALL ERR_REP( 'ARDMASK_ERR1', 'The ^COMP component is '//
     :                    'undefined in the NDF structure ^NDF',
     :                    STATUS )
         END IF

*  If 'All' was supplied for COMP, get a comma separated list of the
*  components to mask.
      ELSE
         CALL NDF_STATE( INDF1, 'Data', THERE, STATUS )
         IF( THERE ) THEN
            COMP = 'Data'
         ELSE
            COMP = ' '
         END IF

         CALL NDF_STATE( INDF1, 'Variance', THERE, STATUS )
         IF( THERE ) THEN
            IF( COMP .EQ. ' ' ) THEN
               COMP = 'Variance'
            ELSE
               COMP = 'Data,Variance'
               NCOMP = 2
            END IF
         END IF

      END IF

*  Obtain the numeric type of the NDF array component to be masked
*  and whether it is complex.
      CALL NDF_TYPE( INDF1, COMP, TYPE, STATUS )
      CALL NDF_CMPLX( INDF1, COMP, CMPLX, STATUS )

*  Get the image bounds and also the size of the axes in pixels.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Use a literal parameter to obtain the value to avoid having to give
*  the indirection and continuation.  Call FIO to open the file to
*  ensure that the obtained file exists.  Get the name and add the
*  indirection symbol so that ARD does not treat the filename as a
*  literal ARD description.
      CALL FIO_ASSOC( 'ARDFILE', 'READ', 'LIST', 0, FD, STATUS )
      CALL AIF_FLNAM( 'ARDFILE', FILNAM( 2: ), STATUS )
      FILNAM( 1:1 ) = '^'
      CALL FIO_ANNUL( FD, STATUS )

*  Identify the ARD file name.  Use a literal parameter to obtain the
      IGRP = GRP__NOID
      CALL ARD_GRPEX( FILNAM, GRP__NOID, IGRP, CONT, STATUS )

*  See if pixel coordinates are to be assumed if the ard description
*  contains no specification of the user co-ordinate system.
      CALL PAR_GET0l( 'DEFPIX', DEFPIX, STATUS )

*  Get the WCS FrameSet from the NDF and use it to establish the WCS
*  information used by the following cal to ARD_WORK. If required,
*  select PIXEL coords as the current Frame first (this means that
*  the default co-ord system in the ard file will be pixel coords).
      CALL KPG1_GTWCS( INDF1, IWCS, STATUS )
      IF( DEFPIX ) THEN
         CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )
         CALL AST_SETI( IWCS, 'CURRENT', IPIX, STATUS )
      END IF
      CALL ARD_WCS( IWCS, ' ', STATUS )

*  Propagate the bits of the source NDF required.
      CALL LPG_PROP( INDF1, 'Data,Variance,Quality,Axis,Units,WCS',
     :               'OUT', INDF2, STATUS )

*  Get the title for the output NDF.
      CALL NDF_CINP( 'TITLE', INDF2, 'Title', STATUS )

*  Get the string representing the constant value to assign.
      CALL PAR_MIX0D( 'CONST', 'Bad', VAL__MIND, VAL__MAXD, 'Bad',
     :                 .FALSE., CONTXT, STATUS )

*  Get the appropriate numerical value from the string.
      BAD = ( CONTXT .EQ. 'BAD' )
      IF( .NOT. BAD ) CALL CHR_CTOD( CONTXT, CONST, STATUS )

*  See if the value is to be assigned to the inside or the outside of the
*  region.
      CALL PAR_GET0L( 'INSIDE', INSIDE, STATUS )

*  Get a FrameSet connecting PIXEL coords to user coords.
      CALL ARD_GTWCS( IGRP, NDIM, UWCS, STATUS )

*  Get the user->pixel Mapping from this FrameSet.
      UMAP = AST_GETMAPPING( UWCS, AST__CURRENT, AST__BASE, STATUS )

*  Assume that we will not be looping to mask multiple planes in the NDF.
      AXLOOP = 0

*  If there are more pixel axes than WCS axes, we may be able to mask
*  individual planes repeatedly in the input NDF.
      NWCS = AST_GETI( UMAP, 'Nin', STATUS )
      IF( NWCS .LT. NDIM ) THEN

*  Try splitting the user->pixel Mapping up into two parallel Mappings,
*  and extracting the one that transforms the NWCS current Frame axes into
*  pixel coords.
         DO I = 1, NWCS
            WCSAX( I ) = I
         END DO
         CALL AST_MAPSPLIT( UMAP, NWCS, WCSAX, PIXAX, SPMAP, STATUS )

*  If this was possible...
         IF( SPMAP .NE. AST__NULL ) THEN

*  Get the number of pixel axes that are used to feed the NWCS current
*  Frame axes. If this is one less than the total number of pixel axes in
*  the NDF, then we will loop round every plane along the extra pixel axis
*  and mask each one.
            IF( AST_GETI( SPMAP, 'Nout', STATUS ) .EQ. NDIM - 1 ) THEN

*  Identify the un-used pixel axis.
               DO I = 1, NDIM
                  USED = .FALSE.
                  DO J = 1, NDIM - 1
                     IF( PIXAX( J ) .EQ. I ) USED = .TRUE.
                  END DO
                  IF( .NOT. USED ) AXLOOP = I
               END DO
            END IF
         END IF
      END IF

*  If we will be looping to mask multiple planes, change the application
*  FrameSet so that it spans the reduced number of pixel axes.
      IF( AXLOOP .NE. 0 ) THEN
         PFRM = AST_PICKAXES( AST_GETFRAME( UWCS, AST__BASE, STATUS ),
     :                        NDIM - 1, PIXAX, JUNK, STATUS )
         AWCS = AST_FRAMESET( PFRM, ' ', STATUS )
         CALL AST_INVERT( SPMAP, STATUS )
         CALL AST_ADDFRAME( AWCS, AST__BASE, SPMAP,
     :                      AST_GETFRAME( UWCS, AST__CURRENT, STATUS ),
     :                      STATUS )
         CALL ARD_WCS( AWCS, ' ', STATUS )

*  Find the number of pixels in one plane, and store the bounds of the
*  used plane.
         EL = 1
         J = 1
         DO I = 1, NDIM
            IF( I .NE. AXLOOP ) THEN
               EL = EL*( UBND( I ) - LBND( I ) + 1 )
               PLBND( J ) = LBND( I )
               PUBND( J ) = UBND( I )
               J = J + 1
            END IF
         END DO

*  Allocate the memory needed for the logical mask array covering one
*  plane.
         CALL PSX_CALLOC( EL, '_INTEGER', IPMASK, STATUS )

*  Create the mask.  Value 2 should be used to represent pixels
*  specified by the first keyword in the ARD description. TRCOEF is
*  ignored because we have previously called ARD_WCS.
         REGVAL = 2
         CALL ARD_WORK( IGRP, NDIM - 1, PLBND, PUBND, TRCOEF, .FALSE.,
     :                  REGVAL, %VAL( CNF_PVAL( IPMASK ) ), LBNDI,
     :                  UBNDI, LBNDE, UBNDE, STATUS )

*  Note how many masking operations will be performed.
         NMASK = UBND( AXLOOP ) - LBND( AXLOOP ) + 1

*  Note the pixel index of the first plane.
         AXVAL = LBND( AXLOOP )

*  If we will not be looping, get a mask covering the whole NDF.
      ELSE

*  Allocate the memory needed for the logical mask array.
         CALL NDF_SIZE( INDF1, EL, STATUS )
         CALL PSX_CALLOC( EL, '_INTEGER', IPMASK, STATUS )

*  Create the mask.  Value 2 should be used to represent pixels
*  specified by the first keyword in the ARD description. TRCOEF is
*  ignored because we have previously called ARD_WCS.
         REGVAL = 2
         CALL ARD_WORK( IGRP, NDIM, LBND, UBND, TRCOEF, .FALSE., REGVAL,
     :                  %VAL( CNF_PVAL( IPMASK ) ), LBNDI, UBNDI, LBNDE,
     :                  UBNDE, STATUS )

*  Note how many masking operations will be performed.
         NMASK = 1

      END IF

*  Loop round each masking operation.
      DO I = 1, NMASK

*  Get an NDF identifier for the section to be masked.
         IF( AXLOOP .GT. 0 ) THEN
            LBND( AXLOOP ) = AXVAL
            UBND( AXLOOP ) = AXVAL
            CALL NDF_SECT( INDF2, NDIM, LBND, UBND, INDFS, STATUS )
            AXVAL = AXVAL + 1
         ELSE
            CALL NDF_CLONE( INDF2, INDFS, STATUS )
         END IF

*  Map the output NDF section for updating.
         IF ( CMPLX ) THEN
            CALL NDF_MAPZ( INDFS, COMP, TYPE, 'UPDATE',
     :                     IPOUT, IPOUTI, EL, STATUS )
         ELSE
            CALL NDF_MAP( INDFS, COMP, TYPE, 'UPDATE',
     :                    IPOUT, EL, STATUS )
         END IF

*  Mask each required component.
         DO ICOMP = 1, NCOMP

*  Correct the output image to have bad pixels where indicated on the
*  mask.  Call the appropriate routine for the data type.
            IF( TYPE .EQ. '_REAL' ) THEN
               CALL KPS1_ARDMR( BAD, CONST, INSIDE, EL,
     :                          %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPOUT( ICOMP ) ) ),
     :                          STATUS )

               IF ( CMPLX ) THEN
                  CALL KPS1_ARDMR( BAD, CONST, INSIDE, EL,
     :                             %VAL( CNF_PVAL( IPMASK ) ),
     :                             %VAL( CNF_PVAL( IPOUTI( ICOMP ) ) ),
     :                             STATUS )
               END IF

            ELSE IF( TYPE .EQ. '_BYTE' ) THEN
               CALL KPS1_ARDMB( BAD, CONST, INSIDE, EL,
     :                          %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPOUT( ICOMP ) ) ),
     :                          STATUS )

               IF ( CMPLX ) THEN
                  CALL KPS1_ARDMB( BAD, CONST, INSIDE, EL,
     :                             %VAL( CNF_PVAL( IPMASK ) ),
     :                             %VAL( CNF_PVAL( IPOUTI( ICOMP ) ) ),
     :                             STATUS )
               END IF

            ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_ARDMD( BAD, CONST, INSIDE, EL,
     :                          %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPOUT( ICOMP ) ) ),
     :                          STATUS )

               IF ( CMPLX ) THEN
                  CALL KPS1_ARDMD( BAD, CONST, INSIDE, EL,
     :                             %VAL( CNF_PVAL( IPMASK ) ),
     :                             %VAL( CNF_PVAL( IPOUTI( ICOMP ) ) ),
     :                             STATUS )
               END IF


            ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
               CALL KPS1_ARDMI( BAD, CONST, INSIDE, EL,
     :                          %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPOUT( ICOMP ) ) ),
     :                          STATUS )

               IF ( CMPLX ) THEN
                  CALL KPS1_ARDMI( BAD, CONST, INSIDE, EL,
     :                             %VAL( CNF_PVAL( IPMASK ) ),
     :                             %VAL( CNF_PVAL( IPOUTI( ICOMP ) ) ),
     :                             STATUS )
               END IF

            ELSE IF( TYPE .EQ. '_INT64' ) THEN
               CALL KPS1_ARDMK( BAD, CONST, INSIDE, EL,
     :                          %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPOUT( ICOMP ) ) ),
     :                          STATUS )

               IF ( CMPLX ) THEN
                  CALL KPS1_ARDMK( BAD, CONST, INSIDE, EL,
     :                             %VAL( CNF_PVAL( IPMASK ) ),
     :                             %VAL( CNF_PVAL( IPOUTI( ICOMP ) ) ),
     :                             STATUS )
               END IF

            ELSE IF( TYPE .EQ. '_UBYTE' ) THEN
               CALL KPS1_ARDMUB( BAD, CONST, INSIDE, EL,
     :                           %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPOUT( ICOMP ) ) ),
     :                          STATUS )

               IF ( CMPLX ) THEN
                  CALL KPS1_ARDMUB( BAD, CONST, INSIDE, EL,
     :                              %VAL( CNF_PVAL( IPMASK ) ),
     :                              %VAL( CNF_PVAL( IPOUTI( ICOMP ) ) ),
     :                              STATUS )
               END IF

            ELSE IF( TYPE .EQ. '_UWORD' ) THEN
               CALL KPS1_ARDMUW( BAD, CONST, INSIDE, EL,
     :                           %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPOUT( ICOMP ) ) ),
     :                          STATUS )

               IF ( CMPLX ) THEN
                  CALL KPS1_ARDMUW( BAD, CONST, INSIDE, EL,
     :                              %VAL( CNF_PVAL( IPMASK ) ),
     :                              %VAL( CNF_PVAL( IPOUTI( ICOMP ) ) ),
     :                              STATUS )
               END IF

            ELSE IF( TYPE .EQ. '_WORD' ) THEN
               CALL KPS1_ARDMW( BAD, CONST, INSIDE, EL,
     :                          %VAL( CNF_PVAL( IPMASK ) ),
     :                          %VAL( CNF_PVAL( IPOUT( ICOMP ) ) ),
     :                          STATUS )

               IF ( CMPLX ) THEN
                  CALL KPS1_ARDMW( BAD, CONST, INSIDE, EL,
     :                             %VAL( CNF_PVAL( IPMASK ) ),
     :                             %VAL( CNF_PVAL( IPOUTI( ICOMP ) ) ),
     :                             STATUS )
               END IF

            END IF
         END DO

*  Annul the section identifier.
         CALL NDF_ANNUL( INDFS, STATUS )

      END DO

*  Set the bad-pixel flag.
      IF( BAD ) CALL NDF_SBAD( .TRUE., INDF2, COMP, STATUS )

*  Free the dynamic array space of the logical mask.
      CALL PSX_FREE( IPMASK, STATUS )

*  Close down the group used to hold the pixel mask.
      CALL GRP_DELET( IGRP, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARDMASK_ERR', 'ARDMASK: Failed to mask an NDF'//
     :                 ' using an ARD file.', STATUS )
      END IF

      END
