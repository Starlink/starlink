      SUBROUTINE KPS1_CLIPA( IWCS, STAT, DOMAIN, ECLEAR, IPIC0, IPICD, 
     :                       IPLOT, STATUS )
*+
*  Name:
*     KPS1_CLIPA

*  Purpose:
*     Sets up AST graphics and AGI database for CLINPLOT line plots.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CLIPA( IWCS, STAT, DOMAIN, ECLEAR, IPIC0, IPICD, IPLOT,
*    :                 STATUS )

*  Description:
*     This routine serves application CLINPLOT.  If the already opened
*     graphics device has not been cleared, an attempt is made to
*     find a DATA picture within the current picture.  If one is found,
*     it becomes the current AGI picture, the corresponding PGPLOT 
*     viewport is established as the current PGPLOT viewport, and an AST
*     Plot is returned for it in which the Base (GRAPHICS) Frame
*     corresponds to PGPLOT world co-ordinates.  If no DATA picture is 
*     found, or if the device was not cleared on opening, the current
*     AGI picture and PGPLOT viewport are unchanged on exit and no Plot 
*     is returned.

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST pointer to a FrameSet.  This may be AST__NULL.  The
*        Current and Base Frames are unchanged on exit.  If an existing
*        DATA picture was created by a non-AST application it will not 
*        have a Plot stored with it.  A default Plot will be created in
*        this case, containing two Frames; a GRAPHICS Frame
*        corresponding to millimetres from the bottom-left corner of
*        the view surface, and a Frame corresponding to AGI world 
*        co-ordinates.  The AGI database does not contain any 
*        information describing world co-ordinates and so such 
*        information must be supplied by the calling application, on 
*        some assumption such as "AGI world co-ordinates are PIXEL 
*        co-ordinates".   This information is provided through a 
*        FrameSet (IWCS) and a Domain name (DOMAIN).  DOMAIN specifies 
*        the Domain in which AGI world co-ordinates are assumed to live.
*        A FrameSet may then be supplied using argument IWCS containing 
*        a Frame with the same Domain which will be used to describe AGI
*        world co-ordinates in the returned Plot (a default
*        two-dimensional Frame with the specified Domain is used if no
*        FrameSet is supplied or if it does not contain a Frame with
*        the specified Domain).  If DOMAIN is supplied Blank, a default
*        two-dimensional Frame with Domain AGI_WORLD will be used to
*        describe AGI world co-ordinates.
*
*        If the existing DATA picture was created by an AST application,
*        it will have a Plot stored with it which means that AGI world 
*        co-ordinates will be ignored.  Consequently, the values
*        supplied for IWCS and DOMAIN will also be ignored.
*     STAT = CHARACTER * ( * ) (Given)
*        Determines whether or not the new DATA picture creted by the
*        calling application is to be aligned with an existing DATA
*        picture:
*
*        'NEW': no attempt is made to align the new DATA picture with an
*               existing DATA picture, even if the ECLEAR argument is
*               given a FALSE value.
*        'OLD': the new DATA picture is always aligned with an existing
*               DATA picture.  The ECLEAR argumwent is not accessed (it
*               is assumed to have a FALSE value) and an error is 
*               reported  if no DATA picture is available.  
*        'UNKNOWN': If ECLEAR is given a FALSE value then the new DATA
*               picture is aligned with any existing DATA picture, but 
*               no error is reported if no DATA picture exists.
*     DOMAIN = CHARACTER * ( * ) (Given)
*        The Domain for AGI world co-ordinates.  Only used if a 
*        FrameSet is supplied (IWCS).  If a blank value is supplied, 
*        then "AGI_WORLD" will be used.  See the description of argument
*        IWCS above for more details.
*     ECLEAR = LOGICAL (Given)
*        TRUE if the graphics device has been cleared when it was
*        opened.  See argument STAT.
*     IPIC0 = INTEGER (Given)
*        The AGI identifier for the current picture on input.
*     IPICD = INTEGER (Returned)
*        An AGI identifier for the existing DATA picture.  Returned
*        equal to -1 if there is no existing DATA picture or if the 
*        device has been cleared on opening (ECLEAR).
*     IPLOT = INTEGER (Returned)
*        An AST pointer to a Plot associated with an existing DATA 
*        picture.  Returned equal to AST__NULL if an error occurs, or if
*        there is no  existing DATA picture, or if the ECLEAR is TRUE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Close down AGI and PGPLOT using KPG1_PGCLS.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     MJC: MAlcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2005 December 2 (MJC):
*        Original version based upon KPG1_PLOTA by DSB.
*     {enter_further_changes_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR error constants 
      INCLUDE 'AST_PAR'          ! AST constants and function 
                                 ! declarations

*  Arguments Given:
      INTEGER IWCS
      CHARACTER STAT*(*)
      CHARACTER DOMAIN*(*)
      LOGICAL ECLEAR

*  Arguments Returned:
      INTEGER IPIC0
      INTEGER IPICD
      INTEGER IPLOT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      LOGICAL ALIGN              ! Align new picture with existing
                                 ! picture?
      LOGICAL CLEAR              ! Should device be cleared on opening?
      INTEGER FRM                ! Pointer to AGI world co-ordinates 
                                 ! Frame
      INTEGER IFRM               ! Index of AGI world co-ord Frame in
                                 ! IWCS
*.

*  Initialise returned values.
      IPLOT = AST__NULL
      IPICD = -1

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  See whether the current picture is to be cleared before creating the 
*  new plot. It is never cleared if STAT is OLD.
      IF ( STAT .EQ. 'OLD' ) THEN
         CLEAR = .FALSE.

      ELSE IF ( STAT .EQ. 'UNKNOWN' .OR. STAT .EQ. 'NEW' ) THEN
         CLEAR = ECLEAR

      ELSE 
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'STAT', STAT )
         CALL ERR_REP( 'KPS1_CLIPA_1', 'Illegal STAT value ''^STAT'' '//
     :                 'supplied to KPS1_CLIPA (programming error).',
     :                 STATUS )
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Determine whether or not to align the new DATA picture with an
*  existing DATA picture.  If STAT is NEW, or the image was cleared
*  on opening, we do not align them.
      IF ( STAT .EQ. 'NEW' .OR. CLEAR ) THEN
         ALIGN = .FALSE.

*  Otherwise, see if the current picture is a DATA picture or contains 
*  a  DATA picture.  If found, the DATA picture becomes the current
*  picture and its AGI id is returned.  If it is not found, an error 
*  will be reported.
      ELSE 
         CALL KPG1_AGFND( 'DATA', IPICD, STATUS )

*  If no DATA picture was found, we cannot align DATA pictures.  Annul 
*  the error if STAT is UNKNOWN.
         IF ( STATUS .NE. SAI__OK ) THEN
            ALIGN = .FALSE.
            IF ( STAT .EQ. 'UNKNOWN' ) CALL ERR_ANNUL( STATUS )
          
*  If a DATA picture is found, indicate that the DATA pictures should be
*  aligned, and re-instate the original current picture. 
         ELSE
            ALIGN = .TRUE.
            CALL AGI_SELP( IPIC0, STATUS )
         END IF

      END IF

*  If the new DATA picture is to be aligned with an existing DATA
*  picture, get the Plot for the existing DATA picture.
      IF ( ALIGN ) THEN

*  If the DATA picture does not have a Plot stored with it, KPG1_GDGET
*  will create a default Plot containing a GRAPHICS Frame, and a Frame
*  corresponding to AGI world co-ordinates.  The AGI world co-ordinate 
*  Frame is normally just a default 2-D Frame with Domain AGI_WORLD, but
*  we can over-ride this by supplying an alternative Frame containing
*  more appropriate attribute settings.  If we do not know the Domain of
*  the AGI world co-ordinate system, just use the default Frame.  This 
*  is indicated by setting the FRM pointer to AST__NULL.
         FRM = AST__NULL
         IF ( DOMAIN .NE. ' ' ) THEN

*  If we have a Domain, look for a Frame with the specified Domain 
*  within any supplied FrameSet.
            IF ( IWCS .NE. AST__NULL ) THEN
               CALL KPG1_ASFFR( IWCS, DOMAIN, IFRM, STATUS )

*  If one was found, use it to represent AGI world co-ordinates.
               IF ( IFRM .NE. AST__NOFRAME ) FRM = AST_GETFRAME( IWCS, 
     :                                                    IFRM, STATUS )
            END IF
 
*  If we have a Domain name, but no Frame with this Domain was supplied,
*  use a default 2D Frame with the specified Domain name.
            IF ( FRM .EQ. AST__NULL ) THEN
               FRM = AST_FRAME( 2, ' ', STATUS )
               CALL AST_SETC( FRM, 'DOMAIN', 
     :                        DOMAIN( : CHR_LEN( DOMAIN ) ), STATUS )
            END IF
   
         END IF

*  Get the Plot associated with the existing DATA picture.  If no Plot
*  is stored with the existing picture (i.e. if it was created by a
*  non-AST based application), then a default Plot will be created
*  containing a GRAPHICS Frame, and a Frame representing AGI world
*  co-ordinates (the Frame identified above is used for this purpose).
*  The world co-ordinates in the PGPLOT window is set to millimetres
*  from the bottom left corner of the view surface, which corresponds
*  to the Base (GRAPHICS) Frame in the returned Plot.
         CALL KPG1_GDGET( IPICD, FRM, .FALSE., IPLOT, STATUS )

      END IF

 999  CONTINUE

*  Return null values if an error has occurred, or there was no 
*  alignment.
      IF ( .NOT. ALIGN .OR. STATUS .NE. SAI__OK ) THEN
         IF ( IPLOT .NE. AST__NULL ) CALL AST_ANNUL( IPLOT, STATUS )
         IPICD = -1
      END IF

*  Export the Plot pointer so that it does not get annulled by the
*  following call to AST_END. 
      IF ( IPLOT .NE. AST__NULL ) CALL AST_EXPORT( IPLOT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
