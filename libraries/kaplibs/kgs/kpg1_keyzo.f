      SUBROUTINE KPG1_KEYZO( KEY, ASPKEY, ASP, ZONEF, ZONEK, STATUS )
*+
*  Name:
*     KPG1_KEYZO

*  Purpose:
*     Obtains SGS zones for a frame and a key within the current zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_KEYZO( KEY, ASPKEY, ASP, ZONEF, ZONEK, STATUS )

*  Description:
*     This routine defines one frame SGS zone and optionally a zone for
*     a key to the right of the frame.  The way these zones are defined
*     depends on the shape of the data and the current picture.
*
*     If there is no key zone required, the frame zone is the largest
*     zone within the current zone that has a supplied data aspect
*     ratio.  This is usually required to obtain square pixels.
*
*     If there is a key required it's position depends on the aspect
*     ratio of the data.  If the data are prolate a zone whose aspect
*     ratio is the sum of data and key aspect ratios is divided to
*     create the required zones.  For oblate data again the shape of the
*     current zone affects the way the zone is divided.  For a prolate
*     picture, the zones are reduced as far as needed for the key to
*     retain its shape using a temporary zone.  The frame is the
*     largest zone of its aspect ratio within the remainder of
*     temporary zone.   For an oblate picture the key zone is made to
*     the right.  The frame is the largest zone of its aspect ratio
*     within the remainder of current zone, and is justified to the
*     bottom left.  The temporary zone has aspect ratio of the current
*     zone less the key.  The key has world co-ordinate whose bounds
*     are 0.0--1.0 in both x and y directions.
*
*     The current zone on input is re-instated when the routine exits.

*  Arguments:
*     KEY = LOGICAL (Given)
*        If true a key zone is to be defined.
*     ASPKEY = REAL (Given)
*        The aspect ratio of the key.  A value in the range 0.15--0.3
*        is recommended.  A value outside the range 0--1.0, or less
*        than ASPPIC when ASP is less than one will both cause the
*        routine to return with an SAI__ERROR status.
*     ASP = REAL (Given and Returned)
*        The aspect ratio of the data associated with the plot.
*        Usually, it is the ratio of the number of pixels in the x axis
*        to that in the y.  If it is positive, it is unchanged on exit.
*
*        If ASP is negative, this indicates a filled-type plot where
*        the data aspect ratio is undefined.  However, the aspect ratio
*        of the frame can be derived and this is returned on exit.
*     ZONEF = INTEGER (Returned)
*        The SGS zone identifier for the frame.
*     ZONEK = INTEGER (Returned)
*        The SGS zone identifier for the key.  It should be ignored
*        when KEY = FALSE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  SGS should be active.

*  Copyright:
*     Copyright (C) 1991, 1992, 1994 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 March 5 (MJC):
*        Original version.
*     1992 December 16 (MJC):
*        Restored the conditional paths for the different data and
*        picture aspect ratios.  Allowed ASP to be negative and
*        transposed it with ASPKEY in the argument order.
*     1994 June 4 (MJC):
*        Use prolate setup when the non-key area is more prolate than
*        the image.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Smallest positive constants

*  Arguments Given:
      LOGICAL
     :  KEY

      REAL
     :  ASP,
     :  ASPKEY

*  Arguments Returned:
      INTEGER
     :  ZONEF,
     :  ZONEK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL
     :  ASPPIC,                  ! Aspic ratio of the current picture
     :  X1, X2, Y1, Y2,          ! Bounds of the current picture (not
                                 ! used)
     :  XM, YM                   ! Physical sizes of the current
                                 ! picture

      INTEGER
     :  ZONEC,                   ! The current zone
     :  ZONET                    ! Temporary zone

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the current zone identifier and its aspect ratio.
*    =====================================================

*    Find the current zone.

      CALL SGS_ICURZ( ZONEC )

*    Get the aspect ratio of the current zone.

      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*    See if any thing went wrong with GKS.

      CALL GKS_GSTAT( STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
         ASPPIC = XM / YM
      ELSE
         ASPPIC = 0.0
      END IF

*    Get the effective data aspect ratio.
*    ====================================

*    Deal with negative data aspect ratio.  The value depends on the
*    shape of the current picture, because room must be made for the
*    key.  For an oblate picture the key is abutted to the right.  For
*    a prolate current zone, the aspect ratio is scaled down, because
*    there would be no room for the frame zone when the current zone's
*    shape is more prolate than the key.  With no key, it's just the
*    aspect ratio of the input zone.
      IF ( ASP .LT. 0.0 ) THEN
         IF ( KEY ) THEN
            IF ( ASPPIC .LT. 1.0 ) THEN
               ASP = ASPPIC * ( 1.0 - ASPKEY )
            ELSE
               ASP = ASPPIC - ASPKEY
            END IF
         ELSE
            ASP = ASPPIC
         END IF
      END IF

*    Validate input data.
*    ====================

*    Check that the current zone was found.  It acts as a check that
*    GKS is active as well.

      IF ( STATUS .NE. SAI__OK ) THEN

*       Report an appropriate message depending on whether there
*       should be a key or not.

         IF ( KEY ) THEN
            CALL ERR_REP( 'KPG1_KEYNO_NOZONK',
     :       'Error obtaining the current zone when trying to define '/
     :       /'the frame and key zones.', STATUS )
         ELSE
            CALL ERR_REP( 'KPG1_KEYNO_NOZONF',
     :       'Error obtaining the current zone when trying to define '/
     :       /'the frame zone.', STATUS )
         END IF

*    Test the values of the input parameters.

      ELSE IF ( ASP .LT. VAL__SMLR .OR. ( KEY .AND.
     :          ( ASPKEY .LT. VAL__SMLR .OR. ASPKEY .GT. 1.0 ) .OR.
     :          ( ASP .GE. 1 .AND. ASPPIC .LE. ASPKEY ) ) ) THEN

*       Report an appropriate message depending on whether there
*       should be a key or not.

         STATUS = SAI__ERROR
         CALL MSG_SETR( 'ASP', ASP )
         IF ( KEY ) THEN
            CALL MSG_SETR( 'ASPKEY', ASPKEY )
            CALL MSG_SETR( 'ASPPIC', ASPPIC )
            CALL ERR_REP( 'KPG1_KEYNO_ASPRATK',
     :        'Unable to define the frame and key zones.  There is '/
     :        /'an error in the supplied or current-picture aspect '/
     :        /'ratios. Image: ^ASP, Key: ^ASPKEY, Zone: ^ASPPIC.',
     :        STATUS )
         ELSE
            CALL ERR_REP( 'KPG1_KEYNO_ASPRATF',
     :        'Unable to define the frame zone.  Programming error '/
     :        /'in the image aspect ratio (^ASP).', STATUS )
          END IF

*    Define the zones.
*    =================
*
      ELSE

*       The simple case of no key.  Define a zone to the vbottom left of
*       the current zone with the data aspect ratio.

         IF ( .NOT. KEY ) THEN

            CALL SGS_ZSHAP( ASP, 'BL', ZONEF, STATUS )

*       Subdivide the zone into the key part and the frame part
*       depending on the aspect ratio of the data and the current zone.
*       The addition of the two aspect ratios defines a composite zone,
*       which is then fragmented.

         ELSE

*          The image is prolate.
*          =====================

*          This is when the image is effectively prolate and when the
*          key should abut the image zone.
            IF ( ASP .LT. 1.0 .OR. ASPPIC - ASPKEY .GE. ASP ) THEN

*             Define the frame to the bottom left, first allowing space
*             for the key.

               CALL SGS_ZSHAP( ASP + ASPKEY, 'BL', ZONET, STATUS )
               CALL SGS_ZSHAP( ASP, 'BL', ZONEF, STATUS )

*             Now define the zone for the key to the right of the frame.
*             It abuts the frame.

               CALL SGS_SELZ( ZONET, STATUS )
               CALL SGS_ZSHAP( ASPKEY, 'BR', ZONEK, STATUS )

*          Now for the oblate case.
*          ========================

            ELSE

*             This is again subdivided, but this time on the aspect
*             ratio of the current zone.  Start with an prolate picture.

               IF ( ASPPIC .LT. 1.0 ) THEN

*                Define the frame to the bottom left, first creating a
*                temporary zone allowing space for the key.  Note that
*                this time the aspect ratio of key is preserved by
*                multiplication rather than addition.  This enables
*                the plot and key to be drawn in a picture with aspect
*                ratio smaller than the aspect ratio of the key.

                  CALL SGS_ZSHAP( ASPPIC * ( 1.0 - ASPKEY ), 'BL',
     :                            ZONET, STATUS )
                  CALL SGS_ZSHAP( ASP, 'BL', ZONEF, STATUS )

*                Now define the zone for the key to the right of the
*                frame.  It abuts the frame.

                  CALL SGS_SELZ( ZONEC, STATUS )
                  CALL SGS_ZSHAP( ASPPIC * ASPKEY, 'BR', ZONET, STATUS )
                  CALL SGS_ZSHAP( ASPKEY, 'BR', ZONEK, STATUS )

*          Now for the oblate-picture case.

               ELSE

*                Define the frame to the bottom left, first creating a
*                temporary zone allowing space for the key.

                  CALL SGS_ZSHAP( ASPPIC - ASPKEY, 'BL', ZONET, STATUS )
                  CALL SGS_ZSHAP( ASP, 'BL', ZONEF, STATUS )

*                Return to the input zone, and then define the zone for
*                the key to the right of the frame.  It abuts the frame.

                  CALL SGS_SELZ( ZONEC, STATUS )
                  CALL SGS_ZSHAP( ASPKEY, 'BR', ZONEK, STATUS )
               END IF
            END IF

*          Specify the key zone's world co-ordinates.

            CALL SGS_SW( 0.0, 1.0, 0.0, 1.0, STATUS )

*          Release the temporary zone.

            CALL SGS_RELZ( ZONET )

         END IF

*       Re-instate the current zone.

         CALL SGS_SELZ( ZONEC, STATUS )

*       Inquire whether GKS/SGS has reported an error.

         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( KEY ) THEN
               CALL ERR_REP( 'KPG1_KEYNO_DZONK',
     :           'Unable to define the frame and key zones.', STATUS )
            ELSE
               CALL ERR_REP( 'KPG1_KEYNO_DZONF',
     :           'Unable to define the frame zone.', STATUS )
            END IF
         END IF

      END IF

      END
