      SUBROUTINE NDFCOPY( STATUS )
*+
*  Name:
*     NDFCOPY

*  Purpose:
*     Copies an NDF (or NDF section) to a new location.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDFCOPY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application copies an NDF to a new location.  By supplying an
*     NDF section as input it may be used to extract a subset, or to
*     change the size or dimensionality of an NDF. A second NDF may
*     also be supplied to act as a shape template, and hence to define
*     the region of the first NDF which is to be copied.
*
*     Any unused space will be eliminated by the copying operation
*     performed by this routine, so it may be used as a way of
*     compressing NDF structures from which components have been
*     deleted.  This ability also makes NDFCOPY a useful alternative to
*     SETBOUND in cases where an NDF's size is to be reduced.

*  Usage:
*     ndfcopy in out

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF (or section) which is to be copied.
*     LIKE = NDF (Read)
*        This parameter may be used to supply an NDF to be used as a
*        shape template during the copying operation.  If such a
*        template is supplied, then its shape will be used to select a
*        matching section from the input NDF before copying takes
*        place.  By default, no template will be used and the shape of
*        the output NDF will therefore match that of the input NDF (or
*        NDF section). [!]
*     OUT = NDF (Write)
*        The output NDF data structure.
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null value (the default) will
*        cause the title of the NDF supplied for parameter IN to be
*        used instead. [!]
*     TRIM = _LOGICAL (Read)
*        If TRUE, then the number of pixel axes in the output NDF will
*        be reduced if necessary to remove any pixel axes which span
*        only a single pixel.  For instance if "stokes" is a 
*        three-dimensional data cube with pixel bounds
*        (1:100,-50:40,1:3), and the parameter IN is given the value
*        "stokes(,,2)", then the dimensionality of the output depends 
*        on the setting of TRIM: if TRIM=FALSE, the output is
*        three-dimensional with pixel bounds (1:100,-50:40,2:2), and if
*        TRIM=TRUE the output is two-dimensional with pixel bounds
*        (1:100,-50:40).  In this example, the third pixel axis spans
*        only a single pixel and is consequently removed if TRIM=TRUE. 
*        [FALSE]
*     TRIMWCS = _LOGICAL (Read)
*        This parameter is only accessed if parameter TRIM is TRUE.  It
*        controls the number of axes in the current WCS co-ordinate
*        Frame of the output NDF.  If TRIMWCS=TRUE, then the current
*        Frame in the output NDF will have the same number of axes as
*        there are pixel axes in the output NDF.  If this involves
*        removing axes, then the axes to retain are specified by
*        parameter USEAXIS.  If TRIMWCS=FALSE, then all axes are
*        retained in the current WCS Frame of the output NDF.  Using the 
*        example in the description of the TRIM parameter, if the input 
*        NDF "stokes" has a three-dimensional current WCS Frame with 
*        axes (RA,Dec,Stokes) and TRIMWSC=TRUE, then an axis will be
*        removed from the current Frame to make it two-dimensional (that
*        is, to match the number of pixel axes remaining after the
*        removal of insignificant pixel axes).  The choice of which two
*        axes to retain is controlled by parameter USEAXIS.  If, on the
*        other hand, TRIMWCS was set to FALSE, then the output NDF would
*        still have two pixel axes, but the current WCS Frame would
*        retain all three axes from the input NDF.  If one or more
*        current Frame axes are removed, the transformation from the
*        current Frame to pixel Frame may become undefined resulting in
*        some WCS operations being unusable.  The inverse of this
*        transformation (from pixel Frame to current Frame) is
*        unchanged however. [TRUE]
*     USEAXIS = LITERAL (Read)
*        This parameter is only accessed if TRIM and TRIMWCS are both
*        TRUE, and some axes need to be removed from the current WCS
*        Frame of the output NDF.  It gives the axes which are to be
*        retained in the current WCS Frame of the output NDF.  Each axis
*        can be specified using one of the following options.
*
*        - An integer index of an axis within the current Frame of the 
*        input NDF (in the range 1 to the number of axes in the current
*        Frame).
*
*        - An axis symbol string such as "RA" or "VRAD".
*
*        - A generic option where "SPEC" requests the spectral axis, 
*        "TIME" selects the time axis, "SKYLON" and "SKYLAT" picks the 
*        sky longitude and latitude axes respectively.  Only those axis 
*        domains present are available as options.

*        The dynamic default selects the axes with the same indices as 
*        the pixel axes being copied.  The value should be given as a 
*        comma-separated list.  []

*  Examples:
*     ndfcopy infile outfile
*        Copies the contents of the NDF structure infile to the new
*        structure outfile.  Any unused space will be eliminated during
*        the copying operation.
*     ndfcopy in=data1(3:40,-3:17) out=data2 title="Extracted section"
*        Copies the section (3:40,-3:17) of the NDF called data1 to a
*        new NDF called data2.  The output NDF is assigned the new title
*        "Extracted section", which replaces the title derived from the
*        input NDF.
*     ndfcopy galaxy newgalaxy like=oldgalaxy
*        Copies a section of the NDF called galaxy to form a new NDF
*        called newgalaxy.  The section which is copied will correspond
*        in shape with the template oldgalaxy.  Thus, after the copying
*        operation, both newgalaxy and oldgalaxy will have the same
*        pixel-index bounds.
*     ndfcopy aa(20~11,20~11) bb like=aa
*        Copies from the NDF section consisting of an 11x11 pixel
*        region of aa centred on pixel (20,20), into a new NDF called
*        bb.  The shape of the region copied is made to match the
*        original shape of aa.  The effect is to extract the selected
*        square region of pixels into a new NDF of the same shape as
*        the original, setting the surrounding region to the bad-pixel
*        value.

*  Related Applications:
*     KAPPA: SETBOUND; Figaro: ISUBSET.

*  Implementation Status:
*     If present, an NDF's TITLE, LABEL, UNITS, DATA, VARIANCE, QUALITY,
*     AXIS WCS and HISTORY components are copied by this routine,
*     together with all extensions.  The output NDF's title may be
*     modified, if required, by specifying a new value via the TITLE
*     parameter.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2000, 2003-2004 Central Laboratory of
*     the Research Councils. Copyright (C) 2005-2006 Particle Physics &
*     Astronomy Research Council. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1991 (RFWS):
*        Original version.
*     19-MAR-1991 (RFWS):
*        Added the LIKE parameter to allow the use of a shape template.
*     22-MAR-1991 (RFWS):
*        Added the TITLE parameter.
*     1995 April 24 (MJC):
*        Made usage and examples lowercase.  Added Related Applications.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     12-APR-2000 (DSB):
*        Added TRIM, TRIMWCS and USEAXIS parameters.
*     13-FEB-2003 (DSB):
*        Modified to avoid use of KPG1_ASGET since it automatically
*        removes WCS axes corresponding to insignificant pixel axes, but
*        we only want this to happen if TRIMWCS is true.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     9-SEP-2005 (DSB):
*        Modified to use AST_MAPSPLIT if possible when trimming WCS
*        axes.
*     2005 September 12 (MJC):
*        Removed diagnostic ast_show and used 72-character wrap for
*        comments.
*     20-MAR-2006 (DSB):
*        Check if the NDF identifier is for a base or section before
*        using NDF_LOC to copy AXIS components. Re-write variable 
*        comments to avoid multiline comments.
*     2006 April 12 (MJC):
*        Remove unused variables.
*     9-SEP-2008 (DSB):
*        Report an error if TRIM is true but there are no significant
*        output pixel axes.
*     {enter_further_changes_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'DAT_PAR'          ! HDS_ public constants
      INCLUDE 'AST_PAR'          ! AST functions and constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER COMP(3)*(DAT__SZNAM) ! NDF array component names
      CHARACTER LOC1*(DAT__SZLOC) ! Locator to the output NDF
      CHARACTER LOC2*(DAT__SZLOC) ! Locator to output AXIS array
      CHARACTER LOC2C*(DAT__SZLOC)! Loc. for a single o/p AXIS structure
      CHARACTER LOC3*(DAT__SZLOC) ! Locator to the input NDF
      CHARACTER LOC4*(DAT__SZLOC) ! Locator to input AXIS array
      CHARACTER LOC4C*(DAT__SZLOC)! Locator for a single i/p AXIS 
                                 ! structure
      CHARACTER LOC5*(DAT__SZLOC)! Locator to AXIS component
      CHARACTER NAME*(DAT__SZNAM)! Name of AXIS component
      CHARACTER TTL*80           ! Frame title
      CHARACTER TYPE*(DAT__SZTYP)! Numerical type of array component
      INTEGER CAXES( NDF__MXDIM )! Non-degenerate current Frame axes
      INTEGER EL                 ! No. of elements in mapped array
      INTEGER I                  ! Loop index
      INTEGER IAXIS( NDF__MXDIM )! Current Frame axes to retain
      INTEGER ICURR              ! Index of original current Frame
      INTEGER IERR               ! Index of first numerical error
      INTEGER IP1                ! Pointer to mapped input array
      INTEGER IP2                ! Pointer to mapped output array
      INTEGER IPERM( NDF__MXDIM )! Output axis index for each input axis
      INTEGER IWCS               ! WCS FrameSet for output
      INTEGER LBND( NDF__MXDIM ) ! Template NDF lower bounds
      INTEGER LTTL               ! Length of title
      INTEGER MAP                ! Original base->current Mapping
      INTEGER MAP2               ! Non-degenerate axes component of MAP
      INTEGER MAP3               ! Axis permutation Mapping
      INTEGER NCOMP              ! No. of components in AXIS structure
      INTEGER NDF1               ! Input NDF identifier
      INTEGER NDF2               ! Template NDF identifier
      INTEGER NDF3               ! Output NDF identifier
      INTEGER NDFT               ! Temporary NDF identifier
      INTEGER NDIM               ! Number of template dimensions
      INTEGER NERR               ! Number of numerical errors
      INTEGER NEWAX              ! New output axis index
      INTEGER NEWFRM             ! Replacement Frame
      INTEGER NFC                ! Number of frame axes
      INTEGER OLDAX              ! Old output axis index
      INTEGER OPERM( NDF__MXDIM )! Input axis index for each output axis
      INTEGER PLACE              ! Place holder for a temporary NDF
      INTEGER PM                 ! Axis permutation Mapping
      INTEGER SIGDIM             ! No. of significant pixel indices
      INTEGER SLBND( NDF__MXDIM )! Significant axis lower bounds
      INTEGER SUBND( NDF__MXDIM )! Significant axis upper bounds
      INTEGER UBND( NDF__MXDIM ) ! Template NDF upper bounds
      LOGICAL BAD                ! Bad values in the array component?
      LOGICAL ISBAS              ! Is the NDF identifier for a base NDF?
      LOGICAL THERE              ! Does object exists?
      LOGICAL TRIM               ! Remove insignificant pixel axes?
      LOGICAL TRMWCS             ! Remove corresponding WCS axes?
      LOGICAL USEPRM             ! Use PermMap to select Frame axes?

      DATA COMP /'DATA', 'VARIANCE', 'QUALITY' /
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDF1, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Defer error reporting and attempt to obtain a second NDF to act as a
*  shape template.
         CALL ERR_MARK
         CALL LPG_ASSOC( 'LIKE', 'READ', NDF2, STATUS )

*  Interpret a null value as indicating that a template should not be
*  used.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  If a template was supplied, then obtain its bounds and select a
*  matching section from the input NDF.  Annul the original input NDF
*  identifier and replace it with the section identifier.
         ELSE
            CALL NDF_BOUND( NDF2, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
            CALL NDF_SECT( NDF1, NDIM, LBND, UBND, NDFT, STATUS )
            CALL NDF_ANNUL( NDF1, STATUS )
            NDF1 = NDFT
         END IF
         CALL ERR_RLSE
      END IF

*  Find the number of significant axes (i.e. axes panning more than 1
*  pixel). First find the number of dimensions.
      CALL NDF_BOUND( NDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Loop round each dimension, counting the significant axes. Also set up
*  axis permutation arrays which can be used to create an AST PermMap if
*  required.
      SIGDIM = 0
      DO I = 1, NDIM
         IF ( LBND( I ) .LT. UBND( I ) ) THEN
            SIGDIM = SIGDIM + 1
            SLBND( SIGDIM ) = LBND( I )
            SUBND( SIGDIM ) = UBND( I )
            OPERM( SIGDIM ) = I
            IPERM( I ) = SIGDIM
         ELSE
            IPERM( I ) = -1
         END IF
      END DO

*  See if pixel axes spanning a single pixel are to be removed.
      CALL PAR_GET0L( 'TRIM', TRIM, STATUS )

*  If there are no insignificant axes, there is nothing to trim.
      IF( SIGDIM .EQ. NDIM ) THEN
         TRIM = .FALSE.

*  If there are no significant axes, trimming is not possible.
      ELSE IF( SIGDIM .EQ. 0 .AND. TRIM ) THEN
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Cannot trim insignificant output '//
     :                    'pixel axes since all output pixel axes '//
     :                    'are insignificant.', STATUS )
         END IF
      END IF

*  If not, copy all components from the input NDF (or section) to
*  create  the output NDF.
      IF( .NOT. TRIM ) THEN
         CALL LPG_PROP( NDF1,
     :               'Title,Label,Units,Data,Variance,Quality,Axis,' //
     :               'History,WCS', 'OUT', NDF3, STATUS )

*  Otherwise, we do not need to copy the array comnponents, or the WCS
*  or AXIS components since we will be copying these explicitly.
      ELSE
         CALL LPG_PROP( NDF1, 'Title,Label,Units,History', 'OUT', NDF3, 
     :                  STATUS )

*  Get the WCS FrameSet from the input NDF. 
         CALL KPG1_GTWCS( NDF1, IWCS, STATUS )

*  Get the Mapping from base (GRID) Frame to current Frame. 
         MAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, STATUS )

*  Create a PermMap which goes from the NDIM-dimensional GRID Frame to
*  a SIGDIM_dimensional copy of the GRID Frame, supplying a value of 1.0
*  for the insignificant axes.
         PM = AST_PERMMAP( NDIM, IPERM, SIGDIM, OPERM, 1.0D0, ' ',
     :                     STATUS ) 

*  Create a SIGDIM-dimensional GRID Frame, then add it into the WCS
*  FrameSet, deleting the original.  Note, the Mapping returned by
*  AST_PICKAXES supplies AST__BAD for the insignificant axes and so we
*  use the better PermMap created above.
         CALL AST_INVERT( IWCS, STATUS )
         NEWFRM = AST_PICKAXES( IWCS, SIGDIM, OPERM, MAP2, STATUS ) 
         ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )
         CALL AST_ADDFRAME( IWCS, AST__CURRENT, PM, NEWFRM, STATUS ) 
         CALL AST_REMOVEFRAME( IWCS, ICURR, STATUS )
         CALL AST_INVERT( IWCS, STATUS )

*  Get the number of axes in the original Current Frame.
         NFC = AST_GETI( IWCS, 'NAXES', STATUS )

*  See if the current WCS co-ordinate Frame is to be modified so that it
*  has the same number of axes as the pixel Frame.
         CALL PAR_GET0L( 'TRIMWCS', TRMWCS, STATUS )

*  If there are no excess WCS axes, there is nothing to trim.
         IF( NFC .LE. SIGDIM ) TRMWCS = .FALSE.

*  If required, remove WCS axes.   
         IF( TRMWCS ) THEN 

*  See if the non-degenerate GRID axes correspond to a distinct and
*  independent group of current Frame axes. If so, MAP2 is returned
*  holding the Mapping from the non-degenerate GRID axes to the
*  corresponding WCS axes, and CAXES is returned holding the indices of
*  these WCS axes.
            CALL AST_MAPSPLIT( MAP, SIGDIM, OPERM, CAXES, MAP2, STATUS )

*  Now see which WCS axes are to be retained.  If the base->current
*  Mapping can be split into two parallel components, we use a default
*  USEAXIS value which includes the WCS axes corresponding to the
*  non-degenerate pixel axes.  If the base->current Mapping cannot be
*  split, we use a default USEAXIS which assumes that the non-degenerate
*  WCS axes simply have the same index as the non-degenerate pixel axes.
            DO I = 1, SIGDIM
               IAXIS( I ) = OPERM( I )
            END DO
   
            USEPRM = .TRUE.
            IF( MAP2 .NE. AST__NULL ) THEN
               IF( AST_GETI( MAP2, 'NOUT', STATUS ) .EQ. SIGDIM ) THEN
                  USEPRM = .FALSE.
                  DO I = 1, SIGDIM
                     IAXIS( I ) = CAXES( I )
                  END DO
               END IF
            END IF

*  Get a value for USEAXIS from the user using the above default.
            CALL KPG1_GTAXI( 'USEAXIS', IWCS, SIGDIM, IAXIS, STATUS )

*  If the base->current Mapping can be split, check that the user has
*  not chosen to retain any degenerate WCS axes.  If so, we will have to
*  use a PermMap to selected the required WCS axes (rather than
*  replacing the base->current Mapping by the component Mapping found
*  above).
            IF( .NOT. USEPRM ) THEN 
               DO I = 1, SIGDIM
                  IF( IAXIS( I ) .NE. CAXES( I ) ) USEPRM = .TRUE.
               END DO
            END IF

*  Create a new Frame by picking the selected axes from the original
*  Current Frame.  This also returns a PermMap which goes from the 
*  original Frame to the new one, using AST__BAD values for the
*  un-selected axes.  We will only use this Mapping if the base->current
*  Mapping was not succesfully split by AST_MAPSPLIT.
            NEWFRM = AST_PICKAXES( IWCS, SIGDIM, IAXIS, MAP3, STATUS )

*  If the original Current Frame is a CmpFrame, the Frame created from
*  the above call to AST_PICKAXES may not have inherited its Title.  If
*  the Frame created above has no Title, but the original Frame had,
*  then copy the original Frame's Title to the new Frame.
            IF( AST_TEST( IWCS, 'TITLE', STATUS ) .AND.
     :          .NOT. AST_TEST( NEWFRM, 'TITLE', STATUS ) ) THEN
               TTL = AST_GETC( IWCS, 'TITLE', STATUS )
               LTTL = MAX( 1, CHR_LEN( TTL ) )
               CALL AST_SETC( NEWFRM, 'TITLE', TTL( : LTTL ), STATUS )
            END IF

*  If the base->current Mapping cannot be split into two parallel
*  component Mappings, or if the user wants to select WCS axes which
*  depend on the degenerate pixel axes, then we use the PermMap created
*  by AST_PICKAXES above to select the required WCS axes.  Add this new
*  Frame into the FrameSet. It becomes the Current Frame.
            IF( USEPRM ) THEN                     
               CALL AST_ADDFRAME( IWCS, AST__CURRENT, MAP3, NEWFRM, 
     :                            STATUS )

*  If the degenerate pixel axes correspond to a distinct subset of the
*  current Frame axes, then we can use the simpler base->current Mapping
*  returned by AST_MAPSPLIT above.
            ELSE
               CALL AST_ADDFRAME( IWCS, AST__BASE, MAP2, NEWFRM, 
     :                            STATUS )

            END IF

         END IF                

*  Modify the bounds of the output NDF so that the significant axes
*  span axes 1 to SIGDIM.
         CALL NDF_SBND( SIGDIM, SLBND, SUBND, NDF3, STATUS ) 

*  Store the new WCS FrameSet in the output NDF.
         CALL NDF_PTWCS( IWCS, NDF3, STATUS )

*  We now need to copy any AXIS structures into the output NDF so that 
*  they refer to the re-ordered axes. 
         CALL NDF_STATE( NDF1, 'AXIS', THERE, STATUS )
         IF( THERE ) THEN

*  Since we will be using HDS to modify the output NDF, we need to take
*  care that the internal representation of the NDF stored within the
*  common blocks of the NDF library does not get out of step with the 
*  actual HDS structure of the NDF.  For this reason, we get an HDS
*  locator to the NDF and then annul the NDF identifier. We will
*  re-import the modified NDF back into the NDF library once all the
*  changes have been made.  Before annulling the NDF, we need to map the
*  DATA array to put it into a defined state since we are not allowed to
*  release an NDF with an undefined DATA array.
            CALL NDF_MAP( NDF3, 'DATA', '_BYTE', 'WRITE', IP2, EL, 
     :                    STATUS ) 
            CALL NDF_LOC( NDF3, 'READ', LOC1, STATUS ) 
            CALL DAT_PRMRY( .TRUE., LOC1, .TRUE., STATUS ) 
            CALL NDF_ANNUL( NDF3, STATUS )

*  Create a new array of axis structures within the output NDF, and get
*  a locator to it.
            CALL DAT_NEW( LOC1, 'AXIS', 'AXIS', 1, SIGDIM, STATUS ) 
            CALL DAT_FIND( LOC1, 'AXIS', LOC2, STATUS )

*  Get a locator to the array of AXIS structures in the input NDF. If the
*  supplied NDF is not a base NDF we need to take a copy of it so that
*  NDF_LOC will return a locator for a structure describing the slected
*  section rather than the base NDF.
            CALL NDF_ISBAS( NDF1, ISBAS, STATUS )
            IF( .NOT. ISBAS ) THEN
               CALL NDF_TEMP( PLACE, STATUS )
               CALL NDF_SCOPY( NDF1, 'AXIS,NOEXTENSION()', PLACE, 
     :                         NDFT, STATUS )
               CALL NDF_LOC( NDFT, 'READ', LOC3, STATUS ) 
            ELSE
               CALL NDF_LOC( NDF1, 'READ', LOC3, STATUS ) 
            END IF

            CALL DAT_FIND( LOC3, 'AXIS', LOC4, STATUS ) 


*  Loop round each re-ordered axis in the output NDF.
            DO NEWAX = 1, SIGDIM
               OLDAX = OPERM( NEWAX )

*  Get locators to the appropriate cells of the old and new AXIS arrays.
               CALL DAT_CELL( LOC2, 1, NEWAX, LOC2C, STATUS ) 
               CALL DAT_CELL( LOC4, 1, OLDAX, LOC4C, STATUS ) 

*  Copy all components of the old axis structure into the new axis
*  structure.
               CALL DAT_NCOMP( LOC4C, NCOMP, STATUS ) 
               IF( STATUS .EQ. SAI__OK ) THEN 
                  DO I = 1, NCOMP
                     CALL DAT_INDEX( LOC4C, I, LOC5, STATUS ) 
                     CALL DAT_NAME( LOC5, NAME, STATUS ) 
                     CALL DAT_COPY( LOC5, LOC2C, NAME, STATUS ) 
                     CALL DAT_ANNUL( LOC5, STATUS ) 
                  END DO
               END IF

*  Annul the locators to the cells.
               CALL DAT_ANNUL( LOC4C, STATUS )
               CALL DAT_ANNUL( LOC2C, STATUS )

            END DO

*  Re-import the modified NDF into the NDF Library.
            CALL NDF_FIND( LOC1, ' ', NDF3, STATUS )

*  Annul the remaining Locators.
            CALL DAT_ANNUL( LOC4, STATUS )
            CALL DAT_ANNUL( LOC3, STATUS )
            CALL DAT_ANNUL( LOC2, STATUS )
            CALL DAT_ANNUL( LOC1, STATUS )

         END IF

*  Now copy the array components from input to output.
         DO I = 1, 3
            CALL NDF_STATE( NDF1, COMP( I ), THERE, STATUS ) 
            IF( THERE ) THEN
   
               CALL NDF_TYPE( NDF1, COMP( I ), TYPE, STATUS )
               CALL NDF_MAP( NDF1, COMP( I ), TYPE, 'READ', IP1, EL, 
     :                       STATUS ) 
               CALL NDF_BAD( NDF1, COMP( I ), .FALSE., BAD, STATUS ) 
               CALL NDF_MAP( NDF3, COMP( I ), TYPE, 'WRITE', IP2, EL, 
     :                       STATUS ) 
   
               IF( TYPE .EQ. '_DOUBLE' ) THEN
                  CALL VEC_DTOD( BAD, EL, %VAL( CNF_PVAL( IP1 ) ), 
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )
   
               ELSE IF( TYPE .EQ. '_REAL' ) THEN
                  CALL VEC_RTOR( BAD, EL, %VAL( CNF_PVAL( IP1 ) ), 
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )
   
               ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
                  CALL VEC_ITOI( BAD, EL, %VAL( CNF_PVAL( IP1 ) ), 
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )
   
               ELSE IF( TYPE .EQ. '_WORD' ) THEN
                  CALL VEC_WTOW( BAD, EL, %VAL( CNF_PVAL( IP1 ) ), 
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )
   
               ELSE IF( TYPE .EQ. '_UWORD' ) THEN
                  CALL VEC_UWTOUW( BAD, EL, %VAL( CNF_PVAL( IP1 ) ), 
     :                             %VAL( CNF_PVAL( IP2 ) ),
     :                             IERR, NERR, STATUS )
   
               ELSE IF( TYPE .EQ. '_BYTE' ) THEN
                  CALL VEC_BTOB( BAD, EL, %VAL( CNF_PVAL( IP1 ) ), 
     :                           %VAL( CNF_PVAL( IP2 ) ),
     :                           IERR, NERR, STATUS )
   
               ELSE IF( TYPE .EQ. '_UBYTE' ) THEN
                  CALL VEC_UBTOUB( BAD, EL, %VAL( CNF_PVAL( IP1 ) ), 
     :                             %VAL( CNF_PVAL( IP2 ) ),
     :                             IERR, NERR, STATUS )
               END IF

            END IF

         END DO

      END IF

*  Obtain a new title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDF3, 'Title', STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDFCOPY_ERR',
     :     'NDFCOPY: Error copying an NDF to a new location.', STATUS )
      END IF

      END
