      SUBROUTINE ARDPLOT( STATUS )
*+
*  Name:
*     ARDPLOT

*  Purpose:
*     Plot regions described in an ARD file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ARDPLOT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application draws the outlines of regions described in
*     a supplied two-dimensional ARD file (an `ARD Description'--see
*     SUN/183). The outlines are drawn over the top of a previously
*     displayed picture, aligned (if possible) in the current
*     co-ordinate Frame of the previously drawn picture.

*  Usage:
*     ardplot ardfile [device] [regval]

*  ADAM Parameters:
*     ARDFILE = FILENAME (Read)
*        The name of a file containing an `ARD Description' of the regions
*        to be outlined. The co-ordinate system in which positions within
*        this file are given should be indicated by including suitable
*        COFRAME or WCS statements within the file (see SUN/183), but will
*        default to pixel co-ordinates in the absence of any such
*        statements. For instance, starting the file with a line containing
*        the text "COFRAME(SKY,System=FK5)" would indicate that positions
*        are specified in RA/DEC (FK5,J2000). The statement "COFRAME(PIXEL)"
*        indicates explicitly that positions are specified in pixel
*        co-ordinates. The ARDFILE parameter is only accessed if
*        Parameter REGION is given a null (!) value.
*     DEVICE = DEVICE (Read)
*        The plotting device.  [Current graphics device]
*     REGION = FILENAME (Read)
*        The name of a file containing an AST Region to be outlined, or
*        null (!) if the ARD region defined by Parameter ARDFILE is to be
*        outlined. Suitable files can be created using the ATOOLS
*        package.  [!]
*     REGVAL = _INTEGER (Read)
*        Indicates which regions within the ARD description are to be
*        outlined. If zero (the default) is supplied, then the plotted
*        boundary encloses all the regions within the ARD file. If a
*        positive value is supplied, then only the region with the
*        specified index is outlined (the first region in the ARD file
*        has index 2, for historical reasons). If a negative value is
*        supplied, then all regions with indices greater than or equal
*        to the absolute value of the supplied index are outlined. See
*        SUN/183 for further information on the numbering of regions
*        within an ARD description. The REGVAL parameter is only accessed
*        if Parameter REGION is given a null (!) value.   [0]
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to use
*        for the curves.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text
*        file preceded by an up-arrow character "^".  Such text files
*        should contain further comma-separated lists which will be read
*        and interpreted in the same manner.  Attribute settings are
*        applied in the order in which they occur within the list, with
*        later settings overriding any earlier settings given for the
*        same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*
*        where <name> is the name of a plotting attribute, and <value>
*        is the value to assign to the attribute.  Default values will
*        be used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!)---the initial default---is
*        supplied.  To apply changes of style to only the current
*        invocation, begin these attributes with a plus sign.  A mixture
*        of persistent and temporary style changes is achieved by
*        listing all the persistent attributes followed by a plus sign
*        then the list of temporary attributes.
*
*        See section "Plotting Attributes" in SUN/95 for a description
*        of the available attributes.  Any unrecognised attributes are
*        ignored (no error is reported).
*
*        The appearance of the plotted curves is controlled by the
*        attributes Colour(Curves), Width(Curves), etc.  [current value]

*  Examples:
*     ardplot bulge
*        Draws an outline around all the regions included in the ardfile
*        named "bulge". The outline is drawn on the current graphics device
*        and is drawn in alignment with the previous picture.

*  Notes:
*     -  A DATA picture must already exist on the selected graphics
*     device before running this command. An error will be reported if no
*     DATA picture can be found.
*     -  The application stores a new DATA picture in the graphics
*     database. On exit the current database picture for the chosen
*     device reverts to the input picture.

*  Related Applications:
*     KAPPA: ARDGEN, ARDMASK, LOOK.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2005 Particle Physics & Astronomy
*     Research Council.
*     Copyright (C) 2007, 2010 Science & Technology Facilities Council.
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
*     {enter_new_authors_here}

*  History:
*     13-SEP-2001 (DSB):
*        Original version.
*     25-OCT-2001 (DSB):
*        Make pixel co-ordimnates the default coord system for the ARD
*        file.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     15-NOV-2005 (DSB):
*        Delete the GRP identifier before returning.
*     28-MAY-2007 (DSB):
*        Modified to support outlining of AST Regions as well as ARD files.
*     30-SEP-2009 (DSB):
*        Use ATL_MATCHREGION to ensure the Region axes correspond to the
*        Plot axes. This should speed up the plotting in cases where the
*        axes would not otherwise match.
*     2010 October 14 (MJC):
*        Document temporary style attributes.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ constants and function declarations
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AST_ISAREGION

*  Local Variables:
      CHARACTER ADOM*30          ! Name of alignment Domain
      CHARACTER FILNAM*(GRP__SZFNM)! Name of ARD file
      DOUBLE PRECISION BOX( 4 )  ! Bounds of used region of (X,Y) axes
      INTEGER FD                 ! File descriptor
      INTEGER FS                 ! Pointer to conversion FrameSet
      INTEGER IBASE              ! Index of original Base frame
      INTEGER IGRP               ! Group identifier
      INTEGER IPICD              ! AGI identifier for the DATA picture
      INTEGER IPICF              ! AGI identifier for the frame picture
      INTEGER IPICK              ! AGI identifier for the KEY picture
      INTEGER IPIX               ! Index of PIXEL Frame
      INTEGER IPLOT              ! Pointer to AST Plot for DATA picture
      INTEGER IREG               ! Pointer to supplied AST Region
      INTEGER NEWREG             ! Pointer to AST Region to outline
      INTEGER NFRM               ! Frame index increment between IWCS and IPLOT
      INTEGER REGVAL             ! Requested region value
      INTEGER RV                 ! Max available region value
      LOGICAL ALIGN              ! DATA pic. aligned with a previous picture?
      LOGICAL CONT               ! ARD description to continue?
      LOGICAL SPARSE             ! Were there holds in the coord frame?
      REAL GBOX( 4 )             ! Bounds of used region of (X,Y) axes
      REAL MARGIN( 4 )           ! Margins round DATA picture

*  Initialisations:
      DATA MARGIN / 0.0, 0.0, 0.0, 0.0 /
      DATA BOX / 0.0, 0.0, 1.0, 1.0 /
*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Start up the graphics system, checking that there is an existing DATA
*  picture on the device. This stores a new DATA picture in the AGI
*  database with the same bounds as the existing DATA picture. The
*  PGPLOT viewport is set so that it matches the area of the DATA picture.
*  World co-ordinates within the PGPLOT window are set to millimetres
*  from the bottom left corner of the view surface. An AST Plot is returned
*  for drawing in the DATA picture. The Base (GRAPHICS) Frame in the Plot
*  corresponds to millimetres from the bottom left corner of the view
*  port, and the Current Frame is inherited from the existing DATA
*  picture's WCS FrameSet.
      CALL KPG1_PLOT( AST__NULL, 'OLD', 'KAPPA_ARDPLOT', ' ', MARGIN, 0,
     :                ' ', ' ', 0.0, 0.0, 'PIXEL', BOX, IPICD, IPICF,
     :                IPICK, IPLOT, NFRM, ALIGN, STATUS )

*  Abort if an error occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Indicate we have no ARD file.
      IGRP = GRP__NOID

*  Indicate we have no AST Region.
      IREG = AST__NULL

*  First of all, attempt to get an AST Region.
      CALL KPG1_GTOBJ( 'REGION', 'Region', AST_ISAREGION, IREG,
     :                 STATUS )

*  If no region was supplied, annul the error and proceed to display an
*  ARD file.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Use a literal parameter to obtain the value to avoid having to give
*  the indirection and continuation.  Call FIO to open the file to
*  ensure that the obtained file exists.  Get the name and add the
*  indirection symbol so that ARD does not treat the filename as a
*  literal ARD description.
         CALL FIO_ASSOC( 'ARDFILE', 'READ', 'LIST', 0, FD, STATUS )
         CALL AIF_FLNAM( 'ARDFILE', FILNAM( 2: ), STATUS )
         FILNAM( 1:1 ) = '^'
         CALL FIO_ANNUL( FD, STATUS )

*  Read the ARD description from the file, into a GRP group.
         IGRP = GRP__NOID
         CALL ARD_GRPEX( FILNAM, GRP__NOID, IGRP, CONT, STATUS )

*  Get the index of the region to draw.
         CALL PAR_GET0I( 'REGVAL', REGVAL, STATUS )

*  We want to draw the ARD region over the entire plotting area, so get the
*  bounds of the PGPLOT window (this will be in millimetres).
         CALL PGQWIN( GBOX( 1 ), GBOX( 3 ), GBOX( 2 ), GBOX( 4 ) )

*  Select the PIXEL Frame as the current Frame in the Plot so that
*  pixel co-ordinates become the default coord system for the ARD file.
         CALL KPG1_ASFFR( IPLOT, 'PIXEL', IPIX, STATUS )
         CALL AST_SETI( IPLOT, 'CURRENT', IPIX, STATUS )

*  Plot it.
         RV = REGVAL
         CALL ARD_PLOT( IGRP, IPLOT, GBOX, RV, STATUS )

*  If the supplied REGVAL did not occur in the ARD description, report a
*  warning.
         IF( RV .LE. REGVAL .AND. REGVAL .NE. 0 ) THEN
            CALL MSG_SETI( 'R', ABS( REGVAL ) )
            CALL MSG_SETI( 'RV', RV - 1 )
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( 'ARDPLOT_MSG1', '  The request region index'//
     :                    ' ^R is greater than the highest region '//
     :                    'index (^RV) used in the supplied ARD '//
     :                    'description.', STATUS )
            CALL MSG_BLANK( STATUS )
         END IF


*  Now handle cases where an AST Region was supplied.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  We now try to get a region in which the axes are the same in number and
*  type (but not necessarily order - AST_CONVERT, called later, will take
*  account of any difference in axis order) as those spanned by the current
*  Frame in the Plot.
         CALL ATL_MATCHREGION( IREG, IPLOT, NEWREG, STATUS )

*  Find the Mapping from the existing Plot to the Frame represented by
*  the Region. Record the original Base frame index because AST_CONVERT
*  changes it.
         IBASE = AST_GETI( IPLOT, 'Base', STATUS )
         FS = AST_CONVERT( IPLOT, NEWREG, ' ', STATUS )

*  Get the Domain in which alignment occurred (the Domain of the new
*  base Frame in the Plot), and then reinstate the original Base Frame.
         CALL AST_INVERT( IPLOT, STATUS )
         ADOM = AST_GETC( IPLOT, 'Domain', STATUS )
         CALL AST_INVERT( IPLOT, STATUS )
         CALL AST_SETI( IPLOT, 'Base', IBASE, STATUS )

*  Report an error if there is no way to transform positions from the
*  current Frame of the Plot to the supplied Region.
         IF( FS .EQ. AST__NULL ) THEN
            IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ARDPLOT_ERR1', 'Cannot align the '//
     :                       'supplied Region with the displayed plot.',
     :                       STATUS )
            END IF

*  Otherwise, add the Frame into the plot using the Mapping returned by
*  AST_CONVERT to connect it to the current Frame in the Plot.
         ELSE
            CALL AST_ADDFRAME( IPLOT, AST__CURRENT,
     :                         AST_GETMAPPING( FS, AST__BASE,
     :                                         AST__CURRENT, STATUS ),
     :                         NEWREG, STATUS )

*  Plot a boundary round the Region.
            SPARSE = AST_BORDER( IPLOT, STATUS )

*  Tell the user what Domain alignment occurred in.
            CALL MSG_SETC( 'D', ADOM )
            CALL MSG_OUT( 'ARDPLOT_MSG1', '   Alignment occurred in '//
     :                    'the ^D Domain.', STATUS )
            CALL MSG_BLANK( STATUS )

         END IF
      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  Delete the group, if supplied.
      IF( IGRP .NE. GRP__NOID ) CALL GRP_DELET( IGRP, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARDPLOT_ERR', 'ARDPLOT: Error plotting '//
     :                 'regions described in an ARD file.', STATUS )
      END IF

      END
