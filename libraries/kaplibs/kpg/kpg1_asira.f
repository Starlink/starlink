      SUBROUTINE KPG1_ASIRA( IDA, FRM, MAP, STATUS )
*+
*  Name:
*     KPG1_ASIRA

*  Purpose:
*     Creates an AST FrameSet from an IRAS90 astrometry structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASIRA( IDA, FRM, MAP, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine creates an AST Frame describing the sky co-ordinates
*     stored in an IRAS90 astrometry structure (see SUN/165), together with
*     a Mapping from IRAS90 "image co-ordinates" to sky co-ordinates.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA Name of parameter to use to get Frame description.
*     FRM = INTEGER (Return)
*        An AST pointer to the sky co-ordinate Frame. Returned equal to
*        AST__NULL if an error occurs.
*     MAP = INTEGER (Return)
*        An AST pointer to a Mapping from image to sky co-ordinates. Returned
*        equal to AST__NULL if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine requires access to the IRA internal common blocks,
*     and uses IRA internal subroutines.

*  Copyright:
*     Copyright (C) 1999, 2004 Central Laboratory of the Research Councils.
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
*     2-SEP-1999 (DSB):
*        Original version.
*     4-OCT-2004 (DSB):
*        Use IRA accessor routines to access IRA common block values.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'IRA_PAR'          ! IRAS90 astrometry library constants

*  Arguments Given:
      INTEGER IDA

*  Arguments Returned:
      INTEGER FRM
      INTEGER MAP

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER BJ               ! The type of epoch held by variable EQU
      CHARACTER PROJ*(IRA__SZPRJ)! Full projection name
      CHARACTER PROJN*(IRA__SZPRJ)! Projection name for IRA identifier
      CHARACTER SCS*(IRA__SZSCS) ! SCS value for IRA identifier
      CHARACTER SCSNAM*(IRA__SZSCS)! Full SCS name with no equinox specifier
      CHARACTER TEXT*40          ! Attribute setting text
      DOUBLE PRECISION COSROT    ! Cosine of U,V rotation angle
      DOUBLE PRECISION DX        ! Pixel size (radians) in X
      DOUBLE PRECISION DY        ! Pixel size (radians) in Y
      DOUBLE PRECISION EPOCH     ! The epoch for the IRA identifier
      DOUBLE PRECISION EQU       ! The epoch of the SCS reference equinox
      DOUBLE PRECISION INA( 2 )  ! Position A in X,Y (image) co-ords
      DOUBLE PRECISION INB( 2 )  ! Position B in X,Y (image) co-ords
      DOUBLE PRECISION MAT2(4)   ! 2x2 rotation matrix
      DOUBLE PRECISION MAT3(9)   ! 3x3 rotation matrix
      DOUBLE PRECISION OUTA( 2 ) ! Position A in unrotated U,V co-ords
      DOUBLE PRECISION OUTB( 2 ) ! Position B in unrotated U,V co-ords
      DOUBLE PRECISION SINROT    ! Sine of U,V rotation angle
      DOUBLE PRECISION T         ! Temporary storage
      DOUBLE PRECISION T1        ! Projection parameter value
      DOUBLE PRECISION T2        ! Projection parameter value
      DOUBLE PRECISION T7        ! Projection parameter value
      DOUBLE PRECISION T8        ! Projection parameter value
      INTEGER IAT                ! No. of characters in TEXT
      INTEGER M1                 ! Pointer to AST Mapping
      INTEGER M12                ! Pointer to AST Mapping
      INTEGER M1234              ! Pointer to AST Mapping
      INTEGER M123456            ! Pointer to AST Mapping
      INTEGER M2                 ! Pointer to AST Mapping
      INTEGER M3                 ! Pointer to AST Mapping
      INTEGER M34                ! Pointer to AST Mapping
      INTEGER M4                 ! Pointer to AST Mapping
      INTEGER M5                 ! Pointer to AST Mapping
      INTEGER M56                ! Pointer to AST Mapping
      INTEGER M6                 ! Pointer to AST Mapping
      INTEGER M7                 ! Pointer to AST Mapping
      INTEGER NPREQ              ! No. of IRA projection parameters required
*.

*  Initialise.
      FRM = AST__NULL
      MAP = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Check that the IRA identifier is OK.
      CALL IRA1_CHECK( IDA, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  First create the Mapping from image to sky co-ordinates.
*  =======================================================
*  Store the absolute pixel sizes.
      CALL IRA_IDPROJP( IDA, 5, DX, STATUS )
      CALL IRA_IDPROJP( IDA, 6, DY, STATUS )
      DX = ABS( DX )
      DY = ABS( DY )

*  Check the requested pixel area is not zero.
      IF( DX*DY .EQ. 0.0D0 ) THEN
         CALL MSG_OUT( 'KPG1_ASIRA_1', 'WARNING: An IRAS90 astrometry'//
     :               ' structure has been found specifying zero pixel'//
     :               ' size. This cannot be used by this application,'//
     :               ' and so the IRAS90 astrometry information will '//
     :               'be ignored.', STATUS )
         GO TO 999
      END IF

*  The first step in converting from IRA "image co-ordinates" to "sky
*  co-ordinates is to convert the input image co-ordinates to (U,V) co-ordinates.
*  (U,V) co-ordinates are like image (X,Y) co-ordinates, except that they
*  are in units of radians rather than pixels, they are relative to
*  an origin at the reference point, and they are rotated by an angle
*  which puts the Y axis at the position angle specified by p(7). The
*  rotation of the celestial sphere specified by p(8) will result in the
*  V axis being at a position angle of p(8) in sky co-ordinates,
*  therefore, rotating the image co-ordinates by an angle of P(7)-P(8)
*  with respect to (U,V) will put the Y axis at a position angle of
*  P(7). Create a WinMap to implement the mapping from (X,Y) to (U,V)
*  (excluding rotation).
      INA( 1 ) = 0.0D0
      INA( 2 ) = 0.0D0
      INB( 1 ) = 1.0D3
      INB( 2 ) = 1.0D3

      CALL IRA_IDPROJP( IDA, 3, OUTA( 1 ), STATUS )
      OUTA( 1 ) = -OUTA( 1 )*DX

      CALL IRA_IDPROJP( IDA, 4, OUTA( 2 ), STATUS )
      OUTA( 2 ) = -OUTA( 2 )*DY

      OUTB( 1 ) = OUTA( 1 ) + 1.0D3*DX
      OUTB( 2 ) = OUTA( 2 ) + 1.0D3*DY

      M1 = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Now we rotate the U,V axes using a MatrixMap. Note, the U,V co-ords
*  get used as input to a WcsMap later. The FITS-WCS convention is for a
*  left handed system in the projection plane, where-as (U,V) is a
*  right handed system. Therefore, negate the first row of the matrix
*  (i.e. MAT2(1) and MAT2(2) ) to produce a left-handed system as needed
*  by the WcsMap.
      CALL IRA_IDPROJP( IDA, 7, T7, STATUS )
      CALL IRA_IDPROJP( IDA, 8, T8, STATUS )
      COSROT = COS( T7 - T8 )
      SINROT = SIN( T7 - T8 )

      MAT2( 1 ) =  -COSROT
      MAT2( 2 ) =  SINROT
      MAT2( 3 ) =  SINROT
      MAT2( 4 ) =  COSROT

      M2 = AST_MATRIXMAP( 2, 2, 0, MAT2, ' ', STATUS )

*  Get the full projection name.
      CALL IRA_IDPROJN( IDA, PROJN, STATUS )
      CALL IRA1_CHPRJ( PROJN, PROJ, NPREQ, STATUS )

*  Now we transform the (U,V) co-ordinates into "local co-ordinates"
*  using the sky projection. Create a WcsMap equivalent to the IRA
*  projection.
      IF( PROJ .EQ. 'GNOMONIC' .OR. PROJ .EQ. 'TANGENT_PLANE' ) THEN
         M3 = AST_WCSMAP( 2, AST__TAN, 1, 2, 'INVERT=1', STATUS )

      ELSE IF( PROJ .EQ. 'AITOFF' .OR. PROJ .EQ. 'ALL_SKY' ) THEN
         M3 = AST_WCSMAP( 2, AST__AIT, 1, 2, 'INVERT=1', STATUS )

      ELSE IF( PROJ .EQ. 'LAMBERT' .OR. PROJ .EQ. 'CYLINDRICAL' ) THEN
         M3 = AST_WCSMAP( 2, AST__CYP, 1, 2, 'INVERT=1', STATUS )
         CALL AST_SETD( M3, 'PROJP(1)', 1.0D20, STATUS )
         CALL AST_SETD( M3, 'PROJP(2)', 1.0D0, STATUS )

      ELSE IF( PROJ .EQ. 'ORTHOGRAPHIC' ) THEN
         M3 = AST_WCSMAP( 2, AST__SIN, 1, 2, 'INVERT=1', STATUS )

      ELSE
         CALL MSG_SETC( 'PROJ', PROJ )
         CALL MSG_OUT( 'KPG1_ASIRA_1', 'WARNING: An IRAS90 astrometry'//
     :               ' structure has been found with a ^PROJ '//
     :               'projection. This cannot yet be '//
     :               'handled by this application, and so the IRAS90 '//
     :               'astrometry information will be ignored.', STATUS )
         GO TO 999

      END IF

*  Use an inverse SphMap to convert local spherical co-ords to local
*  Cartesian co-ords.
      M4 = AST_SPHMAP( 'INVERT=1', STATUS )

*  Create a MatrixMap which rotates FITS-WCS "native" co-ordinates
*  (produced by the inverse WcsMap), into IRA "local" co-ords. The
*  reference point always has latitude zero in "local" co-ords, but
*  can be +90 degs in "native" coords. Rotate the three-dimensional cartesian frame
*  by "NatLat" radians clockwise about the Y axis.
      CALL SLA_DEULER( 'Y', -AST_GETD( M3, 'NATLAT', STATUS ), 0.0D0,
     :                 0.0D0, MAT3 )

*  SLA_DEULER returns the matrix in column order, AST_MATRIXMAP requires it
*  in row order. So transpose the matrix by swapping the 6 off-diagonal terms.
      T = MAT3( 2 )
      MAT3( 2 ) = MAT3( 4 )
      MAT3( 4 ) = T

      T = MAT3( 3 )
      MAT3( 3 ) = MAT3( 7 )
      MAT3( 7 ) = T

      T = MAT3( 6 )
      MAT3( 6 ) = MAT3( 8 )
      MAT3( 8 ) = T

*  Create the MatrixMap.
      M5 = AST_MATRIXMAP( 3, 3, 0, MAT3, ' ', STATUS )

*  Set up a rotation matrix which will convert sky co-ordinates to a "local"
*  longitude/latitude system in which the requested reference point has
*  a longitude and latitude of zero. This co-ordinate system is referred to
*  here as the "local" co-ordinate system. The matrix also includes the
*  required rotation (about a radius through the reference point) of the
*  celestial sphere so that "north" in the local co-ordinate system is at the
*  position angle specified by projection parameter P(8).
      CALL IRA_IDPROJP( IDA, 1, T1, STATUS )
      CALL IRA_IDPROJP( IDA, 2, T2, STATUS )
      CALL IRA_IDPROJP( IDA, 8, T8, STATUS )
      CALL SLA_DEULER( 'ZYX', T1, -T2, -T8, MAT3 )

*  SLA_DEULER returns the matrix in column order, AST_MATRIXMAP requires it
*  in row order. So transpose the matrix by swapping the 6 off-diagonal terms.
      T = MAT3( 2 )
      MAT3( 2 ) = MAT3( 4 )
      MAT3( 4 ) = T

      T = MAT3( 3 )
      MAT3( 3 ) = MAT3( 7 )
      MAT3( 7 ) = T

      T = MAT3( 6 )
      MAT3( 6 ) = MAT3( 8 )
      MAT3( 8 ) = T

*  Create a MatrixMap which implements the inverse of these rotations
*  (i.e. from local to sky).
      M6 = AST_MATRIXMAP( 3, 3, 0, MAT3, 'INVERT=1', STATUS )

*  Use a forward SphMap to convert sky cartesian co-ords to sky spherical
*  co-ords.
      M7 = AST_SPHMAP( ' ', STATUS )

*  Combine all these Mappings in series to get the required Mapping from
*  image to sky co-ordinates.
      M12 = AST_CMPMAP( M1, M2, .TRUE., ' ', STATUS )
      M34 = AST_CMPMAP( M3, M4, .TRUE., ' ', STATUS )
      M56 = AST_CMPMAP( M5, M6, .TRUE., ' ', STATUS )
      M1234 = AST_CMPMAP( M12, M34, .TRUE., ' ', STATUS )
      M123456 = AST_CMPMAP( M1234, M56, .TRUE., ' ', STATUS )
      MAP = AST_SIMPLIFY( AST_CMPMAP( M123456, M7, .TRUE., ' ',
     :                                STATUS ), STATUS )

*  Now create the Frame describing the sky co-ordinates.
*  =====================================================

*  If a valid Julian epoch value is available, create an AST attribute
*  setting string for it.
      CALL IRA_IDEPOCH( IDA, EPOCH, STATUS )
      IF( EPOCH .NE. VAL__BADD ) THEN
         TEXT = 'EPOCH=J'
         IAT = 7
         CALL CHR_PUTD( EPOCH, TEXT, IAT )
      ELSE
         TEXT = ' '
      END IF

*  Create a SkyFrame.
      FRM = AST_SKYFRAME( TEXT, STATUS )

*  Identify the Sky Co-ordinate System.
      CALL IRA_IDSCS( IDA, SCS, STATUS )
      CALL IRA1_CHSCS( SCS, SCSNAM, EQU, BJ, STATUS )

*  Now set the attributes of the SkyFrame to match the values specified
*  by the SCS. First deal with "EQUATORIAL" systems (AST equivalents are
*  "FK4" and "FK5").
      IF( SCSNAM .EQ. 'EQUATORIAL' ) THEN

*  Assume FK4 if a Besselian equinox was included in the SCS.
         IF( BJ .EQ. 'B' ) THEN
            CALL AST_SETC( FRM, 'SYSTEM', 'FK4', STATUS )

*  Assume FK5 if a Julian equinox was included in the SCS.
         ELSE
            CALL AST_SETC( FRM, 'SYSTEM', 'FK5', STATUS )
         END IF

*  Other IRAS90 SCS names match the corresponding AST names (GALACTIC and
*  ECLIPTIC), so just set them.
      ELSE
         CALL AST_SETC( FRM, 'SYSTEM', SCSNAM( : CHR_LEN( SCSNAM ) ),
     :                  STATUS )
      END IF

*  If an equinox value was implied by the SCS set it. This will be the case
*  for EQUATORIAL and ECLIPTIC systems.
      IF( EQU .NE. VAL__BADD ) THEN
         TEXT = BJ
         IAT = 1
         CALL CHR_PUTD( EQU, TEXT, IAT )
         CALL AST_SETC( FRM, 'EQUINOX', TEXT( : IAT ), STATUS )
      END IF

*  Export the returned Frame and Mapping pointers so that they are not
*  annulled by the following call to AST_END. If an error has occurred, the
*  pointers will not be exported and so will be annulled by AST_END.
      IF( FRM .NE. AST__NULL ) CALL AST_EXPORT( FRM, STATUS )
      IF( MAP .NE. AST__NULL ) CALL AST_EXPORT( MAP, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

      END
