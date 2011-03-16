      SUBROUTINE KPG1_DSFRM( FSET, TEXT, LBND, UBND, FULL, STATUS )
*+
*  Name:
*     KPG1_DSFRM

*  Purpose:
*     Displays a textual description of the Current Frame in a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_DSFRM( FSET, TEXT, LBND, UBND, FULL, STATUS )

*  Description:
*     This routine displays a textual description of the Current Frame
*     in the supplied AST FrameSet.

*  ADAM Parameters:
*     The following ADAM parameter names are hard-wired into this routine:
*
*     FPIXSCALE() = LITERAL (Write)
*        The pixel scale values as displayed on standard output by this
*        routine. Celestial axes are in arc-seconds, all other axes are
*        in the units of the corresponding current frame axis. This
*        parameter is only written if the TEXT string starts with
*        "NDFTRACE:".

*  Arguments:
*     FSET = INTEGER (Given)
*        An AST pointer to the FrameSet.
*     TEXT = CHARACTER * ( * ) (Given)
*        Text to display before the Frame description. May contain MSG
*        tokens. This may be prefixed with a string that indicates
*        further processing options. Currently the only such string
*        recognised is "ndftrace:", which causes the pixel scales to be
*        written out to the output parameter FPIXSCALE. Any such string is
*        not included in the displayed title.
*     LBND( * ) = DOUBLE PRECISION (Given)
*        The lower bounds of a region within the base Frame over which
*        the displayed nominal WCS axis scales are to be determined. Both
*        UBND and LBND are ignored if the first element of either UBND or
*        LBND is AST__BAD, in which case the displayed WCS axis scales are
*        determined at coords (1,1,...) in the base Frame.
*     UBND( * ) = DOUBLE PRECISION (Given)
*        The Upper bounds of a region within the base Frame. See LBND.
*     FULL = LOGICAL (Given)
*        Display full information?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 1999, 2000, 2003 Central Laboratory of the Research Councils.
*     Copyright (C) 2007-2011 Science & Technology Facilities Council.
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
*     4-FEB-1998 (DSB):
*        Original version.
*     25-AUG-1999 (DSB):
*        Allow MSG tokens in TEXT.
*     20-MAR-2000 (DSB):
*        Normalize first pixel centre before display.
*     10-JAN-2003 (DSB):
*        Modified to display details of WCS SpecFrames.
*     1-MAY-2007 (DSB):
*        Display pixel size at first pixel.
*     7-JUN-2007 (DSB):
*        Display meaningful text if the pixel scale cannot be found.
*     19-JUN-2007 (DSB):
*        Allow pixel scales to be written to an output parameter. To
*        avoid changing the calling signature of this function, control
*        over this feature is provided via a special string flag supplied
*        at the start of TEXT.
*     16-MAR-2011 (DSB):
*        Changed so that the displayed WCS axis scales can be the nominal
*        values taken over the whole image rather than just at position
*        1,1,1...).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER FSET
      CHARACTER TEXT*(*)
      DOUBLE PRECISION LBND( * )
      DOUBLE PRECISION UBND( * )
      LOGICAL FULL

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables :
      CHARACTER ATTRIB*20        ! AST Frame attribute name
      CHARACTER FPIXSC( NDF__MXDIM )*50   ! Formatted pixel scale
      CHARACTER STEXT*50         ! Sub-frame header text
      CHARACTER UNIT*15          ! Units string
      CHARACTER UPIXSC( NDF__MXDIM )*20   ! pixel scale unit
      DOUBLE PRECISION CFIRST( 1, NDF__MXDIM ) ! Frame coords of first pixel
      DOUBLE PRECISION GFIRST( 1, NDF__MXDIM ) ! GRID coords of first pixel
      DOUBLE PRECISION PIXSC( NDF__MXDIM )! Pixel scale
      INTEGER CFRM               ! Frame to be described
      INTEGER COUNT              ! Count of displayed sub-frames
      INTEGER FRM( 2*NDF__MXDIM )! Pointers to component Frames
      INTEGER FRMNAX             ! Frame dimensionality
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IAT                ! Current length of a string
      INTEGER IAXIS              ! Loop counter for axes
      INTEGER IBASE              ! Index of Base Frame in FrameSet
      INTEGER IGRID              ! Index of GRID Frame in FrameSet
      INTEGER INV1               ! Invert attribute for first component
      INTEGER INV2               ! Invert attribute for second component
      INTEGER ITEXT              ! Index of start of text string to display
      INTEGER NDIM               ! Number of dimensions
      INTEGER TOP                ! Index of last Frame to be checked
      LOGICAL GOTFS              ! Was a FrameSet supplied?
      LOGICAL NDFTRA             ! Was this routine called from NDFTRACE?
      LOGICAL SERIES             ! Frames in series?
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  See if the TEXT string starts with one of the special control flag strings.
      IF( CHR_SIMLR( TEXT( :9 ), 'NDFTRACE:' ) ) THEN
         ITEXT = 10
         NDFTRA = .TRUE.
      ELSE
         ITEXT = 1
         NDFTRA = .FALSE.
      END IF

*  Get a pointer to the Frame to be described.
      GOTFS = AST_ISAFRAMESET( FSET, STATUS )

      IF( GOTFS ) THEN
         CFRM = AST_GETFRAME( FSET, AST__CURRENT, STATUS )

      ELSE IF( AST_ISAFRAME( FSET, STATUS ) ) THEN
         CFRM = AST_CLONE( FSET, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'CLASS', AST_GETC( FSET, 'CLASS', STATUS ) )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_DSFRM_ERR', 'KPG1_DSFRM: Inappropriate '//
     :                 'AST Object (class ^CLASS) supplied '//
     :                 '(programming error).', STATUS )
      END IF

*  Display the global properties of the Frame.
      CALL KPG1_DSFR1( CFRM, TEXT( ITEXT: ), 8, FULL, STATUS )

*  Get the Frame dimensionality.
      FRMNAX = AST_GETI( CFRM, 'NAXES', STATUS )

*  Initialise things.
      DO I = 1, FRMNAX
         FPIXSC( I ) = '(unavailable)'
         UPIXSC( I ) = ' '
      END DO

*  Only proceed if we have a FrameSet.
      IF( GOTFS ) THEN

*  See if there is a GRID Frame in the FrameSet.
         CALL KPG1_ASFFR( FSET, 'GRID', IGRID, STATUS )
         IF( IGRID .NE. AST__NOFRAME ) THEN

*  Ensure the GRID Frame is the Base Frame.
            IBASE = AST_GETI( FSET, 'BASE', STATUS )
            CALL AST_SETI( FSET, 'BASE', IGRID, STATUS )

*  Gets its dimensionality. Pass on if it is more than NDF__MXDIM.
            NDIM = AST_GETI( FSET, 'NIN', STATUS )
            IF( NDIM .LE. NDF__MXDIM ) THEN

*  Store the GRID co-ordinates of the centre of the first pixel. This is
*  defined to be (1.0,1.0,...). This position will be mapped into the
*  other Frame, to find the co-ordinates of the first pixel.
               DO IAXIS = 1, NDIM
                  GFIRST( 1, IAXIS ) = 1.0
               END DO

*  Map the GRID co-ordinates at the centre of the first pixel to obtain the
*  corresponding co-ordinates in the Frame.
               CALL AST_TRANN( FSET, 1, NDIM, 1, GFIRST, .TRUE., FRMNAX,
     :                         1, CFIRST, STATUS )

*  Normalize the positions.
               CALL AST_NORM( FSET, CFIRST, STATUS )

*  Display the resulting co-ordinates.
               CALL MSG_SETC( 'FIRST', AST_FORMAT( FSET, 1,
     :                                             CFIRST( 1, 1 ),
     :                                             STATUS ) )

               DO IAXIS = 2, FRMNAX
                  CALL MSG_SETC( 'FIRST', ',' )
                  CALL MSG_SETC( 'FIRST', ' ' )
                  CALL MSG_SETC( 'FIRST', AST_FORMAT( FSET, IAXIS,
     :                            CFIRST( 1, IAXIS ), STATUS ) )
               END DO

               CALL MSG_OUT( 'WCS_FIRSTP',
     :      '        First pixel centre  : ^FIRST', STATUS )

*  Get the pixel scales at the first pixel, or the nominal scales over
*  the supplied region within the base Frame.
               IF( LBND( 1 ) .EQ. AST__BAD .OR.
     :             UBND( 1 ) .EQ. AST__BAD ) THEN
                  CALL KPG1_PIXSC( FSET, GFIRST, PIXSC, FPIXSC, UPIXSC,
     :                             STATUS )
               ELSE
                  CALL KPG1_SCALE( FSET, LBND, UBND, PIXSC, FPIXSC,
     :                             UPIXSC, STATUS )
               END IF

            END IF

*  Re-instate the original Base Frame.
            CALL AST_SETI( FSET, 'BASE', IBASE, STATUS )

         END IF

      END IF

*  Now display the axis number, label and units for each axis of the Frame.
      CALL MSG_BLANK( STATUS )
      DO IAXIS = 1, FRMNAX

*  Display the axis number.
         CALL MSG_SETI( 'IAXIS', IAXIS )
         CALL MSG_OUT( 'AXIS_NUMBER',
     :   '           Axis ^IAXIS:', STATUS )

*  Construct the name of the attribute holding the label for this axis.
         ATTRIB = 'LABEL('
         IAT = 6
         CALL CHR_PUTI( IAXIS, ATTRIB, IAT )
         CALL CHR_APPND( ')', ATTRIB, IAT )

*  Get the label and display it.
         CALL MSG_SETC( 'LABEL', AST_GETC( CFRM, ATTRIB( : IAT ),
     :                                     STATUS ) )
         CALL MSG_OUT( 'AXIS_LABEL',
     :   '              Label              : ^LABEL', STATUS )

*  Construct the name of the attribute holding the units for this axis.
         ATTRIB = 'UNIT('
         IAT = 5
         CALL CHR_PUTI( IAXIS, ATTRIB, IAT )
         CALL CHR_APPND( ')', ATTRIB, IAT )

*  Get the units string and display it (if not blank).
         UNIT = AST_GETC( CFRM, ATTRIB( : IAT ), STATUS )
         IF( UNIT .NE. ' ' ) THEN
            CALL MSG_SETC( 'UNIT', UNIT )
            CALL MSG_OUT( 'AXIS_UNITS',
     :      '              Units              : ^UNIT', STATUS )
         END IF

*  display the pixel scale at the first pixel
         CALL MSG_SETC( 'V', FPIXSC( IAXIS ) )
         CALL MSG_SETC( 'U', UPIXSC( IAXIS ) )
         CALL MSG_OUT( 'AXIS_SCALE',
     :      '              Nominal Pixel scale: ^V ^U', STATUS )

*  Add a spacing line after the information for each axis.
         CALL MSG_BLANK( STATUS )

      END DO

*  If called from NDFTRACE,store the pixel scales in an output parameter.
      IF( NDFTRA ) THEN
         CALL PAR_PUT1C( 'FPIXSCALE', FRMNAX, FPIXSC, STATUS )
      END IF

*  If the current Frame is a CmpFrame, we now display details of its
*  component Frames.
      IF( FULL .AND. AST_ISACMPFRAME( CFRM, STATUS ) ) THEN
         FRM( 1 ) = CFRM
         I = 1
         TOP = 1
         COUNT = 1
         DO WHILE( I .LE. TOP .AND. TOP .LE. NDF__MXDIM )
            IF( AST_ISACMPFRAME( FRM( I ), STATUS ) ) THEN
               CALL AST_DECOMPOSE( FRM( I ), FRM( TOP + 1 ),
     :                             FRM( TOP + 2 ), SERIES, INV1, INV2,
     :                             STATUS )
               TOP = TOP + 2
               CALL AST_ANNUL( FRM( I ), STATUS )
            ELSE
               STEXT = ' '
               IAT = 8
               CALL CHR_APPND( 'Sub-frame', STEXT, IAT )
               IAT = IAT + 1
               CALL CHR_PUTI( COUNT, STEXT, IAT )
               CALL CHR_PUTC( ':', STEXT, IAT )

               CALL KPG1_DSFR1( FRM( I ), STEXT( : IAT ), 11, FULL,
     :                          STATUS )
               COUNT = COUNT + 1
               CALL MSG_BLANK( STATUS )
            END IF
            I = I + 1
         END DO

      END IF


*  End the AST context.
      CALL AST_END( STATUS )

      END
