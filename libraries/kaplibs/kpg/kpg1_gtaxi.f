      SUBROUTINE KPG1_GTAXI( PARAM, FRAME, NAX, AXES, STATUS )
*+
*  Name:
*     KPG1_GTAXI

*  Purpose:
*     Selects Frame axes using an environment parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GTAXI( PARAM, FRAME, NAX, AXES, STATUS )

*  Description:
*     This routine returns the indices of selected axes in a supplied
*     Frame. The axes to select are determined using the supplied
*     environment parameter. Each axis can be specified either by giving
*     its index within the Frame in the range 1 to the number of axes in
*     the Frame, or by giving its symbol. Spectral, time and celestial
*     longitude/latitude axes may also be specified using the options "SPEC",
*     "TIME", "SKYLON" and "SKYLAT". If the first value supplied in
*     AXES is not zero, the supplied axes are used as the dynamic default
*     for the parameter. The parameter value should be given as a GRP group
*     expression, with default GRP control characters.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     FRAME = INTEGER (Given)
*        An AST pointer for the Frame from which axes may be chosen.
*     NAX = INTEGER (Given)
*        The number of axes which must be selected.
*     AXES( NAX ) = INTEGER (Given and Returned)
*        On entry, the axes to be specified as the dynamic default for the
*        parameter (if AXES( 1 ) is not zero). On exit, the indices of the
*        selected axes. If AXES(1) is zero the supplied values are ignored
*        and a PAR__NULL status value is returned if a null (!) value is
*        supplied for the parameter. Otherwise, the PAR_NULL status is
*        annulled if a null value is supplied, and the supplied axes are
*        returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Case is insignificant when comparing supplied strings with
*     available axis symbols.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1998 (DSB):
*        Original version.
*     1-MAY-2008 (DSB):
*        Allow SPEC, TIME, SKYLON and SKYLAT to be used to select axes.
*     2008 May 1 (MJC):
*        Ensure that the Symbols appear in order at the start of the
*        list of options, so that supplied integer values correspond to
*        axis indices.
*     27-MAY-2009 (DSB):
*        Annul attribute access errors in cases where the Frame contains
*        isolated sky axes (i.e. sky axes that are not contained within
*        a SkyFrame).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'AST_ERR'          ! AST error constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER FRAME
      INTEGER NAX

*  Arguments Given and Returned:
      INTEGER AXES( NAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CAXIS( 2*NDF__MXDIM )*20 ! Available axis symbols
      CHARACTER DOM*30             ! Value of Domain attribute
      CHARACTER PAXIS*( VAL__SZI ) ! Buffer for new axis number
      INTEGER AXPOS( 2*NDF__MXDIM )! Index of axis within "CAXIS" array
      INTEGER DEFAX                ! Default axis index
      INTEGER I                    ! Axis index
      INTEGER J                    ! Axis index
      INTEGER NCP                  ! No. of characters in PAXIS text buffer
      INTEGER NFC                  ! No. of axes in original Current Frame
      INTEGER NOPT                 ! No. of options in CAXIS
      LOGICAL DOSKY                ! Save indices of sky axes?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of axes in the supplied Frame.
      NFC = AST_GETI( FRAME, 'NAXES', STATUS )

*  Initialise the number of options stored in CAXIS.  We need to have
*  the symbols first and in axis order for KPG1_GTCHV to work for
*  integer axis indices.  So insert the generic options after those.
      NOPT = NFC

*  Indicate we have not yet added the SKYLON and SKYLAT options to the
*  CAXIS list.
      DOSKY = .TRUE.

*  Get the axis symbols and their lengths for all axes in the Frame.
      DO I = 1, NFC
         NCP = 0
         CALL CHR_PUTI( I, PAXIS, NCP )
         CAXIS( I ) = AST_GETC( FRAME, 'Symbol(' //
     :                          PAXIS( : NCP ) // ')', STATUS )
         CALL CHR_LDBLK( CAXIS( I ) )
         CALL KPG1_PGESC( CAXIS( I ), STATUS )
         AXPOS( I ) = I

*  Search for any sky, spectral and time axes. We use the Domain
*  attribute to identify each type of axis (assuming the Domain values
*  have not been changed from the default values provided by AST).
         DOM = AST_GETC( FRAME, 'Domain(' // PAXIS( : NCP ) // ')',
     :                   STATUS )

*  If this is a spectral axis, allow the user to specify this axis using
*  the option "SPEC".
         IF( DOM .EQ. 'SPECTRUM' .OR. DOM .EQ. 'DSBSPECTRUM' ) THEN
            NOPT = NOPT + 1
            CAXIS( NOPT ) = 'SPEC'
            AXPOS( NOPT ) = I

*  If this is a time axis, allow the user to specify this axis using
*  the option "TIME".
         ELSE IF( DOM .EQ. 'TIME' ) THEN
            NOPT = NOPT + 1
            CAXIS( NOPT ) = 'TIME'
            AXPOS( NOPT ) = I

*  If this is a celestial axis, find the indices of the longitude and
*  latitude axes and allow the user to specify them using the options
*  "SKYLON" and "SKYLAT". Annul any "unknown attribute name" error, since
*  the SKY axis may not form part of a SkyFrame (for instance, it may be
*  a single sky axis picked from a SkyFrame), in which case SkyFrame
*  attributes will not be available.
         ELSE IF( DOM .EQ. 'SKY' .AND. DOSKY ) THEN
            IF( STATUS .EQ. SAI__OK ) THEN
               NOPT = NOPT + 1
               CAXIS( NOPT ) = 'SKYLAT'
               AXPOS( NOPT ) = AST_GETI( FRAME, 'LatAxis(' //
     :                                   PAXIS( : NCP ) // ')', STATUS )

               NOPT = NOPT + 1
               CAXIS( NOPT ) = 'SKYLON'
               AXPOS( NOPT ) = AST_GETI( FRAME, 'LonAxis(' //
     :                                   PAXIS( : NCP ) // ')', STATUS )

               IF( STATUS .EQ. AST__BADAT ) THEN
                  CALL ERR_ANNUL( STATUS )
                  NOPT = NOPT - 2
               END IF

               DOSKY = .FALSE.
            ENDIF
         END IF

      END DO

*  Translate the supplied default axis indices into default option indices.
      IF( AXES( 1 ) .GT. 0 ) THEN
         DO J = 1, NAX
            DEFAX = AXES( J )
            DO I = NOPT, 1, -1
               IF( AXPOS( I ) .EQ. DEFAX ) AXES( J ) = I
            END DO
         END DO
      END IF

*  Get the required number of axis selections. A reasonable guess should
*  be to assume a one-to-one correspondance between Current and Base axes.
*  Therefore, use the significant axes selected above as the defaults to be
*  used if a null (!) parameter value is supplied.
      CALL KPG1_GTCHV( NOPT, CAXIS, PARAM, NAX, AXES, AXES, STATUS )

*  Translate the returned option indices into axis indices.
      DO J = 1, NAX
         AXES( J ) = AXPOS( AXES( J ) )
      END DO

      END
