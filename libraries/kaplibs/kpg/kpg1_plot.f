      SUBROUTINE KPG1_PLOT( IWCS, STAT, APP, DATREF, MARGIN, NP, PNAME,
     :                      PSIDE, PSIZE, ASPECT, DOMAIN, BOX, IPICD,
     :                      IPICF, IPIC, IPLOT, NFRM, ALIGN, STATUS )
*+
*  Name:
*     KPG1_PLOT

*  Purpose:
*     Prepares for graphics output.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PLOT( IWCS, STAT, APP, DATREF, MARGIN, NP, PNAME, PSIDE,
*                     PSIZE, ASPECT, DOMAIN, BOX, IPICD, IPICF, IPIC,
*                     IPLOT, NFRM, ALIGN, STATUS )

*  Description:
*     This routine opens a graphics device and prepares for graphical
*     output using PGPLOT within a new DATA picture. Optional ancillary
*     pictures around the DATA picture may also be created. A FRAME
*     picture enclosing the DATA picture and any ancillary pictures is
*     created if any ancillary pictures or non-zero margins were
*     requested.
*
*     An AST Plot is returned which allows plotting within the DATA
*     picture. The Base (GRAPHICS) Frame in this Plot corresponds to
*     millimetres from the botttom left corner of the view surface.
*     The Current Frame in the Plot is inherited from the supplied
*     FrameSet (IWCS).
*
*     If there is an existing DATA picture on the device, then the new
*     DATA picture can optionally be aligned with the existing DATA
*     picture. In this case, the returned Plot is formed by adding any
*     supplied FrameSet (IWCS) into the Plot stored with the existing
*     DATA picture in the AGI database (a default Plot is used if the
*     database does not contain a Plot). The FrameSet is added into the
*     Plot by aligning them in the Current Frame of the FrameSet if
*     possible. If this is not possible, they are aligned in the world
*     co-ordinate system of the picture (stored in the AGI database).
*     The Domain of the AGI world co-ordinate system is not stored in
*     the database and must be supplied by the calling application using
*     argument DOMAIN (this will usually be "PIXEL"). If alignment is
*     not possible in AGI world co-ordinates, then they are aligned in
*     the GRID domain. If this is also not possible, they are aligned in
*     any suitable Frame.
*
*     On exit, the current PGPLOT viewport corresponds to area occupied
*     by the new DATA picture. The bounds of the PGPLOT window produce a
*     world co-ordinate system within the viewport corresponding to the
*     Base Frame in the returned Plot (i.e. millimetres from the
*     bottom-left corner of the view surface - NOT pixel co-ordinates).
*     Note, this is different from the world co-ordinate system stored
*     in the AGI database with the new DATA picture.
*
*     If the returned Plot contains an AXIS Frame in which the axes are
*     scaled and shifted versions of the axes of the AGI world
*     co-ordinate Frame (specified by argument DOMAIN), then a
*     TRANSFORM structure is stored with the new DATA picture that
*     defines AGI Data co-ordinates. This is purely for the benefit of
*     non-AST based applications which may use AGI Data co-ordinates
*     (AST-based applications should always use the Plot stored with the
*     picture in preference to the TRANSFORM structure stored in the AGI
*     database).
*
*     Various environment parameters are used to obtain options, etc.
*     The names of these parameters are hard-wired into this subroutine
*     in order to ensure conformity between applications.

*  Environment Parameters:
*     CLEAR = _LOGICAL (Read)
*        TRUE if the graphics device is to be cleared on opening. See
*        argument STAT.
*     DEVICE = DEVICE (Read)
*        The plotting device.
*     FILL = _LOGICAL (Read)
*        TRUE if the supplied aspect ratio is to be ignored, creating
*        the largest possible DATA picture within the current picture.
*        When FILL is FALSE, the DATA picture is created with the
*        supplied aspect ratio. Only used when creating a new DATA
*        picture.
*     STYLE = GROUP (Read)
*        A description of the plotting style required. This the name of
*        a text file containing an AST attribute setting on each line,
*        of the form "name=value", where "name" is an AST Plot
*        attribute name (or synonym recognised by this application--see
*        KPG1_ASPSY), and "value" is the value to assign to the
*        attribute.

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST pointer to a FrameSet. This may be AST__NULL. The
*        Current and Base Frames are unchanged on exit.
*     STAT = CHARACTER * ( * ) (Given)
*        Determines whether or not the new DATA picture should be
*        aligned with an existing DATA picture.
*
*        - "NEW" -- no attempt is made to align the new DATA picture
*        with an existing DATA picture, even if the CLEAR parameter is
*        given a FALSE value.
*
*        - "OLD" -- the new DATA picture is always aligned with an
*        existing DATA picture. The CLEAR parameter is not accessed (it
*        is assumed to have a FALSE value) and an error is reported if
*        no DATA picture is available.
*
*        - "UNKNOWN" -- If CLEAR is given a FALSE value then the new
*        DATA picture is aligned with any existing DATA picture, but no
*        error is reported if no DATA picture exists.
*     APP = CHARACTER * ( * ) (Given)
*        The name of the calling application, in the form
*        <package>_<application> (e.g. "KAPPA_DISPLAY",
*        "POLPACK_POLPLOT", etc.). The supplied string is stored as a
*        comment with all new AGI pictures.
*     DATREF = CHARACTER * ( * ) (Given)
*        A string describing the source of the data being displayed,
*        which will be stored in the AGI database with the new DATA
*        picture. It is ignore if blank.
*     MARGIN( 4 ) = REAL (Given)
*        The width of the borders to leave around the new DATA picture,
*        given as fractions of the corresponding dimension of the DATA
*        picture. These should be supplied in the order bottom, right,
*        top, left.
*     NP = INTEGER (Given)
*        The number of extra pictures to be included in the FRAME
*        pictures (the DATA picture itself is not included in this
*        list). Margins are left around the DATA picture with widths
*        given by MARGIN. Any extra pictures are placed outside these
*        margins, in positions described by PSIDE and PSIZE.
*     PNAME( NP ) = CHARACTER * ( * ) (Given)
*        The names to store in the AGI database with the NP extra
*        pictures.
*     PSIDE( NP ) = CHARACTER * 1 (Given)
*        Each element of this array should be one of L, R, T or B. It
*        indicates which side of the FRAME picture an extra picture is
*        to be placed. For Left and Right, the extra picture occupies
*        the full height of the DATA picture, margins, and any
*        previously created extra pictures. The picture is placed at the
*        far Left or Right of all previously created pictures. For Top
*        or Bottom, the extra picture occupies the full width of the
*        DATA picture, margins, and any previously created extra
*        pictures. The picture is placed at the top or bottom of all
*        previously created pictures. Ignored if NP is zero.
*     PSIZE( NP ) = REAL (Given)
*        The size of each extra picture. For Left and Right pictures,
*        this is the width of the picture, and the value is given as a
*        fraction of the width of the DATA picture. For Top and Bottom
*        pictures, it is the height of the picture, and it is given as a
*        fraction of the height of the DATA picture. Ignored if NP is
*        zero.
*     ASPECT = REAL (Given)
*        The aspect ratio for the DATA picture. This is the height of
*        the DATA picture (in millimetres) divided by the width of the
*        DATA picture (also in millimetres). The new DATA picture is
*        created with this aspect ratio unless the FILL parameter is
*        given a TRUE value, in which case the aspect ratio is adjusted
*        to get the largest DATA picture that can be created within the
*        current picture. If a value of zero is supplied, then the
*        largest DATA picture is used irrespective of FILL (which is
*        then not accessed).
*     DOMAIN = CHARACTER * ( * ) (Given)
*        The Domain name corresponding to the AGI world co-ordinates. If
*        a blank value is supplied then "AGI_WORLD" will be used.
*     BOX( 4 ) = DOUBLE PRECISION (Given)
*        The co-ordinates to be assigned to the bottom-left, and
*        top-right corners of the DATA picture in the AGI database (the
*        co-ordinate system in defined by argument DOMAIN). Only used if
*        the new DATA picture is NOT being aligned with an existing DATA
*        picture. Supplied in the order XLEFT, YBOTTOM, XRIGHT, YTOP.
*        Note, the supplied bounds are stored in the AGI database, but
*        do not effect the PGPLOT window on exit, which always has a
*        world co-ordinate system of millimetres from the bottom-left
*        corner of the view surface. If the supplied box has zero area,
*        then world co-ordinates for the DATA picture in the AGI
*        database will be centimetres from the bottom-left corner of
*        the DATA picture.
*     IPICD = INTEGER (Returned)
*        An AGI identifier for the new DATA picture.
*     IPICF = INTEGER (Returned)
*        An AGI identifier for the new FRAME picture. World co-ordinate
*        system is inherited from the current picture on entry. If no
*        FRAME picture is created then an identifier for the current
*        picture on entry is returned.
*     IPIC( NP ) = INTEGER (Returned)
*        An array of AGI identifiers corresponding to the extra pictures
*        requested in PSIDE and PSIZE. The world co-ordinate system for
*        each picture is inherited from the FRAME picture. The actual
*        size of a picture may be less than the requested size if there
*        is insufficient room left in the FRAME picture to give it its
*        requested size. Identifiers for pictures which would have zero
*        size (i.e. fall completely outside the FRAME picture) are
*        returned equal to -1, but no error is reported.
*     IPLOT = INTEGER (Returned)
*        An AST pointer to the new Plot. Returned equal to AST__NULL if
*        an error occurs.
*     NFRM = INTEGER (Returned)
*        A  Frame with index I in the supplied FrameSet (IWCS) will have
*        index ( I + NFRM ) in the returned Plot (IPLOT). Returned equal
*        to zero if an error occurs.
*     ALIGN = LOGICAL (Returned)
*        Was the new DATA picture aligned with an existing DATA picture?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Picture identifiers are returned equal to -1 if an error
*     occurs, or if any pictures cannot be created.
*     -  Close down AGI and PGPLOT using:
*           CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JUL-1998 (DSB):
*        Original version.
*     4-DEC-1998 (DSB):
*        Added facilities for storing a TRANSFORM structure with the new
*        DATA picture for the benefit of non-AST applications.
*     13-AUG-2007 (DSB):
*        Only store WCS information in the AGI database if the device is
*        cleared upon opening. This stops the database file growing too
*        large.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER IWCS
      CHARACTER STAT*(*)
      CHARACTER APP*(*)
      CHARACTER DATREF*(*)
      REAL MARGIN( 4 )
      INTEGER NP
      CHARACTER PNAME( NP )*(*)
      CHARACTER PSIDE( NP )*(*)
      REAL PSIZE( NP )
      REAL ASPECT
      CHARACTER DOMAIN*(*)
      DOUBLE PRECISION BOX( 4 )

*  Arguments Returned:
      INTEGER IPICD
      INTEGER IPICF
      INTEGER IPIC( NP )
      INTEGER IPLOT
      INTEGER NFRM
      LOGICAL ALIGN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count
      INTEGER ICURR0             ! Index of original current Frame in Plot
      INTEGER IPIC0              ! AGI id for original current picture
      INTEGER IPICD0             ! AGI id for existing DATA picture

*.

*  Initialise returned values.
      IPLOT = AST__NULL
      NFRM = 0
      IPICF = -1
      IPICD = -1
      ALIGN = .FALSE.

      DO I = 1, NP
         IPIC( I ) = -1
      END DO

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Open the grahics device and see if there is an axisting DATA picture
*  with which we can attempt to align the new one. If such a picture is
*  found, a Plot is returned for it.
      CALL KPG1_PLOTA( IWCS, STAT, DOMAIN, IPIC0, IPICD0, IPLOT,
     :                 STATUS )

*  Ensure the original current picture is still current.
      CALL AGI_SELP( IPIC0, STATUS )
      CALL AGP_NVIEW( .FALSE., STATUS )

*  If we have a Plot for an existing DATA picture, attempt to merge the
*  supplied FrameSet with it, aligning them in a suitable common Frame.
      IF( IPLOT .NE. AST__NULL ) THEN

*  Save the number of Frames in the Plot before any supplied FrameSet is
*  added into it.
         NFRM = AST_GETI( IPLOT, 'NFRAME', STATUS )

*  Save the index of the original Current Frame in the Plot of the
*  existing picture.
         ICURR0 = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  If a FrameSet has been supplied, we now add its Frames into the DATA
*  picture Plot, aligning the FrameSet and Plot in a suitable Frame.
         IF( IWCS .NE. AST__NULL ) THEN
            CALL KPG1_ASMRG( IPLOT, IWCS, DOMAIN, .FALSE., 0, STATUS )
         END IF

*  Store a flag indicating if the alignment occurred.
         ALIGN = ( STATUS .EQ. SAI__OK )

*  If no existing DATA picture was found indicate that the stored Plot
*  should retain its original Current Frame.
      ELSE
         ICURR0 = AST__NOFRAME
      END IF

*  Create the required AGI pictures, aligning the new DATA picture with
*  any suitable existing DATA picture found above.
      CALL KPG1_PLOTP( IPICD0, APP, MARGIN, NP, PNAME, PSIDE, PSIZE,
     :                 ASPECT, BOX, IPICD, IPICF, IPIC, STATUS )

*  Obtain a Plot for the new DATA picture if we do not yet have one
*  (i.e. if the new DATA picture was not aligned with an existing DATA
*  picture).
      IF( .NOT. ALIGN ) CALL KPG1_PLOTN( IWCS, DOMAIN, IPICD, .TRUE.,
     :                                   IPLOT, NFRM, STATUS )

*  Set the attributes of the Plot to give the required Plotting style.
      CALL KPG1_ASSET( APP, 'STYLE', IPLOT, STATUS )

*  Unless we are drawing over an existing plot, save the Plot and data
*  reference with the new DATA picture. If possible, a TRANSFORM
*  structure is stored with the AGI picture giving "AGI Data" co-ords
*  for the benefit of non_AST applications. The Data co-ordinates are
*  defined by any AXIS Frame in the Plot. We do not save WCS with the
*  DATA picture if it is drawn over the top of an existing data picture.
*  This prevents the database size from growing (WCS information can be
*  very large). When aligning subsequent pictures, pictures without WCS
*  are ignored, resulting in aligment occurring with the original
*  picture (i.e. the one that was not drawn over the top of another
*  picture).
      IF( IPICD0 .EQ. -1 ) THEN
         CALL KPG1_PLOTS( IPLOT, IPICD, DATREF, ICURR0, DOMAIN, 'AXIS',
     :                    STATUS )
      END IF

*  Export the Plot pointer so that it does not get annulled by the
*  following call to AST_END. If an error has occurred, the pointer will
*  not be exported and so will be annulled by AST_END.
      CALL AST_EXPORT( IPLOT, STATUS )

*  Return null values if an error has occurred.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL AST_ANNUL( IPLOT, STATUS )
         NFRM = 0
         IPICF = -1
         IPICD = -1
         ALIGN = .FALSE.

         DO I = 1, NP
            IPIC( I ) = -1
         END DO

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
