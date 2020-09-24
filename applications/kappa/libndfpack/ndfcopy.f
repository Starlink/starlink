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
*     COMP = LITERAL (Read)
*        The name of an array component in the input NDF (specified by
*        Parameter IN) that will become the DATA_ARRAY in the output NDF
*        (specified by Parameter OUT).  It has the following options.
*
*          "Data"     -- Each array component present is propagated to
*                        its counterpart.
*          "Variance" -- The VARIANCE component in the input NDF becomes
*                        the DATA_ARRAY in the output NDF and retains
*                        its data type.  The original DATA_ARRAY is not
*                        copied.
*          "Error"    -- The square root of the VARIANCE component in
*                        the input NDF becomes the DATA_ARRAY in the
*                        output NDF and retains the VARIANCE's data
*                        type.  The original DATA_ARRAY and VARIANCE
*                        components are not copied.
*          "Quality" --  The QUALITY component in the input NDF becomes
*                        the DATA_ARRAY in the output NDF and will be
*                        data type _UBYTE.  The original DATA_ARRAY and
*                        VARIANCE components are not copied.
*        ["Data"]
*     EXTEN = _LOGICAL (Read)
*        If set to FALSE (the default), any NDFs contained within
*        extensions of the input NDF are copied to equivalent places
*        within the output NDF without change. If set TRUE, then any
*        extension NDFs which have the same bounds as the base input
*        NDF are padded or trimmed as necessary in order to ensure that
*        they have the same bounds as the output NDF. [FALSE]
*     IN = NDF (Read)
*        The input NDF (or section) which is to be copied.
*     LIKE = NDF (Read)
*        This parameter may be used to supply an NDF to be used as a
*        shape template during the copying operation.  If such a
*        template is supplied, then its shape will be used to select a
*        matching section from the input NDF before copying takes
*        place.  By default, no template will be used and the shape of
*        the output NDF will therefore match that of the input NDF (or
*        NDF section).  The shape of the template in either pixel
*        indices or the current WCS Frame may be used, as selected by
*        Parameter LIKEWCS. [!]
*     LIKEWCS = _LOGICAL (Read)
*        If TRUE, then the WCS bounds of the template supplied via
*        Parameter LIKE are used to decide on the bounds of the output
*        NDF.  Otherwise, the pixel bounds of the template are used.
*        [FALSE]
*     OUT = NDF (Write)
*        The output NDF data structure.
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null value (the default) will
*        cause the title of the NDF supplied for Parameter IN to be
*        used instead. [!]
*     TRIM = _LOGICAL (Read)
*        If TRUE, then the number of pixel axes in the output NDF will
*        be reduced if necessary to remove any pixel axes which span
*        only a single pixel.  For instance if "stokes" is a
*        three-dimensional data cube with pixel bounds
*        (1:100,-50:40,1:3), and the Parameter IN is given the value
*        "stokes(,,2)", then the dimensionality of the output depends
*        on the setting of TRIM: if TRIM=FALSE, the output is
*        three-dimensional with pixel bounds (1:100,-50:40,2:2), and if
*        TRIM=TRUE the output is two-dimensional with pixel bounds
*        (1:100,-50:40).  In this example, the third pixel axis spans
*        only a single pixel and is consequently removed if TRIM=TRUE.
*        [FALSE]
*     TRIMBAD = _LOGICAL (Read)
*        If TRUE, then the pixel bounds of the output NDF are trimmed to
*        exclude any border of bad pixels within the input NDF. That is,
*        the output NDF will be the smallest NDF that encloses all good
*        data values in the input NDF. [FALSE]
*     TRIMWCS = _LOGICAL (Read)
*        This parameter is only accessed if Parameter TRIM is TRUE.  It
*        controls the number of axes in the current WCS co-ordinate
*        Frame of the output NDF.  If TRIMWCS=TRUE, then the current
*        Frame in the output NDF will have the same number of axes as
*        there are pixel axes in the output NDF.  If this involves
*        removing axes, then the axes to retain are specified by
*        Parameter USEAXIS.  If TRIMWCS=FALSE, then all axes are
*        retained in the current WCS Frame of the output NDF.  Using the
*        example in the description of the TRIM parameter, if the input
*        NDF "stokes" has a three-dimensional current WCS Frame with
*        axes (RA,Dec,Stokes) and TRIMWCS=TRUE, then an axis will be
*        removed from the current Frame to make it two-dimensional (that
*        is, to match the number of pixel axes remaining after the
*        removal of insignificant pixel axes).  The choice of which two
*        axes to retain is controlled by Parameter USEAXIS.  If, on the
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
*     ndfcopy infile outfile comp=var
*        As the previous example except that the VARIANCE component of
*        NDF infile becomes the DATA_ARRAY of NDF outfile.
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
*     ndfcopy survey(12h23m:12h39m,11d:13d50m,) virgo trimwcs trim
*        Copies a section specified by equatorial co-ordinate ranges
*        from the three-dimensional NDF called survey, whose third pixel
*        axis has only one element, to a two-dimensional NDF called
*        virgo.  Information on the third WCS axis is removed too.

*  Related Applications:
*     KAPPA: SETBOUND; Figaro: ISUBSET.

*  Implementation Status:
*     -  If present, an NDF's TITLE, LABEL, UNITS, DATA, VARIANCE, QUALITY,
*     AXIS WCS and HISTORY components are copied by this routine,
*     together with all extensions.  The output NDF's title may be
*     modified, if required, by specifying a new value via the TITLE
*     parameter.
*     -  Huge NDFs are supported.
*
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2000, 2003-2004 Central Laboratory of
*     the Research Councils. Copyright (C) 2005-2006 Particle Physics &
*     Astronomy Research Council.
*     Copyright (C) 2009, 2013 Science & Technology Facilities Council.
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
*     26-JAN-2009 (DSB):
*        Added Parameter LIKEWCS.
*     11-FEB-2009 (DSB):
*        Added Parameter EXTEN. Most of the work moved to KPG1_NDFCP.
*     9-MAR-2009 (DSB):
*        Added Parameter TRIMBAD.
*     2009 July 17 (MJC):
*        Added Parameter COMP.
*     2009-12-03 (TIMJ):
*        Allow COMP=ERROR.
*     2013 July 10 (MJC):
*        Add an example using TRIM and TRIMWCS.
*     22-FEB-2019 (DSB):
*        When changing the shape of an extension NDF, do not delete
*        the original NDF until the new NDF has been created. Deleting
*        the original NDF first seem to trigger a bug in HDF5 that
*        causes dat_copy to report an error.
*     19-DEC-2019 (DSB):
*        Add support for huge NDFs.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'GRP_PAR'          ! GRP_ public constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER COMP*8           ! NDF array component to read
      CHARACTER COMPN*8          ! NDF array component as known to NDF_STATE
      CHARACTER LOC*(DAT__SZLOC) ! Locator for output extension NDF
      CHARACTER NAME*(DAT__SZNAM)! Name for output extension NDF
      CHARACTER PLOC*(DAT__SZLOC)! Locator for LOC's parent
      INTEGER*8 BLBND( NDF__MXDIM )! Input base NDF low bounds
      INTEGER BNDF               ! Base input NDF identifier
      INTEGER BNDIM              ! Dimensionality of input base NDF
      INTEGER BSIZE              ! Size of group IGRPB
      INTEGER*8 BUBND( NDF__MXDIM )! Input base NDF high bounds
      INTEGER I                  ! Loop count
      INTEGER IGRP               ! Paths to output extension NDFs
      INTEGER IGRPB              ! Paths to input extension NDFs
      INTEGER J                  ! Loop count
      INTEGER*8 LBND( NDF__MXDIM ) ! Input extension NDF low bounds
      INTEGER NDF1               ! Input NDF identifier
      INTEGER NDF2               ! Template NDF identifier
      INTEGER NDF3               ! Main output NDF identifier
      INTEGER NDF4               ! Input extension NDF identifier
      INTEGER NDF5               ! Input extension NDF section identifier
      INTEGER NDF6               ! Output extension NDF identifier
      INTEGER NDF7               ! Modified output extension NDF identifier
      INTEGER NDFT               ! Temporary NDF identifier
      INTEGER NDIM               ! Dimensionality of input extension NDF
      INTEGER*8 NGOOD              ! No. of good data values in NDF
      INTEGER*8 OLBND( NDF__MXDIM )! Output NDF low bounds
      INTEGER ONDIM              ! Dimensionality of output NDF
      INTEGER*8 OUBND( NDF__MXDIM )! Output NDF high bounds
      INTEGER PLACE              ! Placeholder for output NDF
      INTEGER SIZE               ! Size of group IGRP
      INTEGER*8 UBND( NDF__MXDIM ) ! Input extension NDF high bounds
      LOGICAL EXTEN              ! Truncate outptu extension NDFs?
      LOGICAL LIKWCS             ! Match WCS bounds with template?
      LOGICAL SAME               ! Extension NDF same as base NDF?
      LOGICAL THERE              ! Array component is present?
      LOGICAL TRIM               ! Remove insignificant pixel axes?
      LOGICAL TRMBAD             ! Remove any boundary of bad pixels?
      LOGICAL TRMWCS             ! Remove corresponding WCS axes?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the input NDF.
      CALL LPG_ASSOC( 'IN', 'READ', NDF1, STATUS )

*  Determine which array component is to be copied.
      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Variance,Quality,Error',
     :                .FALSE., COMP, STATUS )

*  Check that the required component exists and report an error if it
*  does not.
      IF ( STATUS .EQ. SAI__OK ) THEN
         COMPN = COMP
         IF (COMPN .EQ. 'ERROR') THEN
            COMPN = 'VARIANCE'
         END IF
         CALL NDF_STATE( NDF1, COMPN, THERE, STATUS )
         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'COMP', COMP )
            CALL NDF_MSG( 'NDF', NDF1 )
            CALL ERR_REP( 'NDFCOPY_NOCOMP', 'The ^COMP component is '/
     :                    /'undefined in the NDF structure ^NDF',
     :                    STATUS )
         END IF
      END IF

*  See if the output NDF is to be trimmed to exclude any border of bad
*  pixels.
      CALL PAR_GET0L( 'TRIMBAD', TRMBAD, STATUS )

*  If so, obtain the required section of the input NDF. This is the
*  smallest section that encloses all good data values.
      IF( TRMBAD ) THEN
         CALL KPG1_BADBX( NDF1, 0, NDFT, NGOOD, STATUS )

*  Use this section in place of the supplied NDF.
         IF( STATUS .EQ. SAI__OK ) NDF1 = NDFT
      END IF

*  Defer error reporting and attempt to obtain a second NDF to act as a
*  shape template.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK
         CALL LPG_ASSOC( 'LIKE', 'READ', NDF2, STATUS )

*  Interpret a null value as indicating that a template should not be
*  used.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  If a template was supplied, then obtain the pixel or WCS bounds of
*  it, as determined by the LIKEWCS parameter, and select a matching
*  section from the input NDF.  Annul the original input NDF identifier
*  and replace it with the section identifier.
         ELSE
            CALL PAR_GET0L( 'LIKEWCS', LIKWCS, STATUS )
            CALL KPG1_LIKE( NDF1, NDF2, LIKWCS, NDFT, STATUS )
            CALL NDF_ANNUL( NDF1, STATUS )
            CALL NDF_ANNUL( NDF2, STATUS )
            NDF1 = NDFT
         END IF
         CALL ERR_RLSE
      END IF

*  See if pixel axes spanning a single pixel are to be removed.
      CALL PAR_GET0L( 'TRIM', TRIM, STATUS )

*  See if the current WCS co-ordinate Frame is to be modified so that it
*  has the same number of axes as the pixel Frame.
      CALL PAR_GET0L( 'TRIMWCS', TRMWCS, STATUS )

*  Copy the input NDF to the output location.
      PLACE = NDF__NOPL
      CALL KPS1_NDFCP( NDF1, COMP, TRIM, TRMWCS, 'OUT', PLACE, NDF3,
     :                 STATUS )

*  See if we are to copy equivalent sections of any NDFs contained in
*  the extensions of the supplied NDF.
      EXTEN = .FALSE.
      CALL PAR_GET0L( 'EXTEN', EXTEN, STATUS )

*  If so, get a GRP group containing paths to any NDFs contained with
*  extensions of the supplied NDF. Also get a group containing paths to
*  the equivalent NDFs in the output.
      IF( EXTEN ) THEN
         IGRPB = GRP__NOID
         CALL NDG_MOREG( NDF1, IGRPB, BSIZE, STATUS )

         IGRP = GRP__NOID
         CALL NDG_MOREG( NDF3, IGRP, SIZE, STATUS )

*  Sanity check.
         IF( SIZE .NE. BSIZE ) THEN
            IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'Unequal number of extension NDFs '//
     :                       'in input and output (programming error).',
     :                       STATUS )
            END IF
         END IF

*  Get the bounds of the main output NDF.
         CALL NDF_BOUND8( NDF3, NDF__MXDIM, OLBND, OUBND, ONDIM,
     :                    STATUS )

*  Get the bounds of the base input NDF.
         CALL NDF_BASE( NDF1, BNDF, STATUS )
         CALL NDF_BOUND8( BNDF, NDF__MXDIM, BLBND, BUBND, BNDIM,
     :                    STATUS )

*  Loop round each extension NDF. These are stored such that the higher
*  level NDFs come first. That is, an earlier NDF may contain a later
*  NDF, but a later NDF will never contain an earlier NDF. This means
*  that a lower level NDF may be copied several times, but it ensures
*  that the final copy will be the correct one.
         DO I = 1, SIZE
            CALL NDG_NDFAS( IGRPB, I, 'READ', NDF4, STATUS )

*  Get its bounds.
            CALL NDF_BOUND8( NDF4, NDF__MXDIM, LBND, UBND, NDIM,
     :                       STATUS )

*  See if this NDF has the same shape as the base NDF on the pixel axes
*  that they share in common...
            SAME = .TRUE.
            DO J = 1, MIN( NDIM, BNDIM )
               IF( UBND( J ) .NE. BUBND( J ) .OR.
     :             LBND( J ) .NE. BLBND( J ) ) SAME = .FALSE.
            END DO

*  If so, ensure that OLBND/OUBND arrays inherit bounds from the input
*  extension NDF if the extension NDF has more axes than the main NDF.
            IF( SAME ) THEN
               IF( NDIM .GT. ONDIM ) THEN
                  DO J = ONDIM + 1, NDIM
                     OLBND( J ) = LBND( J )
                     OUBND( J ) = UBND( J )
                  END DO
               END IF

*  Get a section of the input extension NDF that matches the bounds of
*  the output NDF on the pixel axes that they share in common.
               CALL NDF_SECT8( NDF4, NDIM, OLBND, OUBND, NDF5, STATUS )

*  Get an identifier for the existing output NDF that is to be replaced.
               CALL NDG_NDFAS( IGRP, I, 'WRITE', NDF6, STATUS )

*  Get an HDS locator for this existing NDF, locate its parent, and
*  get its name. Note, we do not delete the NDF here as doing so seems
*  to trigger a bug in HDF5 that causes datCopy to report an error.
               CALL NDF_LOC( NDF6, 'WRITE', LOC, STATUS )
               CALL DAT_PAREN( LOC, PLOC, STATUS )
               CALL DAT_NAME( LOC, NAME, STATUS )
               CALL DAT_ANNUL( LOC, STATUS )

*  Get a placeholder for a new NDF at the same location but with a
*  temporary name.
               CALL NDF_PLACE( PLOC, 'NDFCOPY_TMPXXX', PLACE, STATUS )

*  Copy the input extension NDF section to the output, using the above
*  placeholder to indicate where the new NDF should be placed.
               CALL KPS1_NDFCP( NDF5, COMP, TRIM, TRMWCS, ' ', PLACE,
     :                          NDF7, STATUS )

*  Delete the original NDF.
               CALL NDF_DELET( NDF6, STATUS )

*  Rename the new NDF to the name of the original NDF. First annull it's NDF
*  identifier, then use HDS to do the rename.
               CALL NDF_ANNUL( NDF7, STATUS )
               CALL DAT_FIND( PLOC, 'NDFCOPY_TMPXXX', LOC, STATUS )
               CALL DAT_RENAM( LOC, NAME, STATUS )
               CALL DAT_ANNUL( LOC, STATUS )

*  Free resources.
               CALL NDF_ANNUL( NDF5, STATUS )
               CALL DAT_ANNUL( PLOC, STATUS )
            END IF

*  Free resources
            CALL NDF_ANNUL( NDF4, STATUS )
         END DO

*  Free resources.
         CALL NDF_ANNUL( BNDF, STATUS )
         CALL GRP_DELET( IGRPB, STATUS )
         CALL GRP_DELET( IGRP, STATUS )

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
