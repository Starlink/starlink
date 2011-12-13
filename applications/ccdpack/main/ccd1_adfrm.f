      SUBROUTINE CCD1_ADFRM( FSET, FSET2, DOMAIN, ROT, FITROT, NCARD,
     :                       IPFITS, STATUS )
*+
*  Name:
*     CCD1_ADFRM

*  Purpose:
*     Add a new frame to a frameset.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_ADFRM( FSET, FSET2, DOMAIN, ROT, FITROT, NCARD, IPFITS,
*                      STATUS )

*  Description:
*     This routine takes two framesets FSET and FSET2, and adds the
*     Current frame from FSET2 on to FSET.  To do this it locates a
*     frame in FSET with the same Domain as the Base frame of FSET2,
*     and adds the Current frame of FSET2 to it using the mapping
*     between the Base and Current frames of FSET2.  Any frames
*     with the same domain as DOMAIN are removed from FSET.  The
*     Current frame of FSET remains the same (unless it has been
*     removed by the previous rule, in which case it becomes the
*     newly added frame).  The newly added frame will be the last
*     (highest-index) frame in the FSET.
*
*     A couple of modifications to this mapping may be made: if the
*     ROT argument is non-zero a further rotation will be added to
*     it, and if FITROT is non-blank a rotation given by the value
*     (in degrees) of the FITS header card with the keyword given
*     by FITROT is also added.
*
*     This routine only works on two-dimensional frames (because of
*     the rotation business).

*  Arguments:
*     FSET = INTEGER (Given)
*        AST pointer to the frameset to which a frame is to be added.
*     FSET2 = INTEGER (Given)
*        AST pointer to the frameset from which a frame is to be added.
*     DOMAIN = CHARACTER * ( * ) (Given)
*        The AST Domain to use in joining the framesets.  Both FSET and
*        FSET2 must contain a frame in this domain.
*     ROT = DOUBLE PRECISION (Given)
*        A fixed rotation angle in degrees to be added to the mapping.
*     FITROT = CHARACTER * ( * ) (Given)
*        The name of a FITS header card giving an additional angle in
*        degrees to be added to the mapping.
*     NCARD = INTEGER (Given)
*        The number of cards in the mapped FITS character array.
*     IPFITS = INTEGER (Given)
*        A pointer to the mapped array of FITS header cards.  These
*        must be mapped as _CHAR*80.  Not accessed if FITROT is blank
*        or NCARD is zero.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-FEB-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! Standard AST constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER FSET
      INTEGER FSET2
      CHARACTER * ( * ) DOMAIN
      DOUBLE PRECISION ROT
      CHARACTER * ( * ) FITROT
      INTEGER NCARD
      INTEGER IPFITS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string without trailing blanks

*  Local Variables:
      CHARACTER * ( AST__SZCHR ) DMMAT ! Name of matching domain
      INTEGER FRM                ! AST pointer to frame being added
      INTEGER ICARD              ! Index of located FITS header card (dummy)
      INTEGER JCUR               ! Frame index of Current frame
      INTEGER JMAT               ! Frame index of matching frame
      INTEGER JNEW               ! Frame index of newly added frame
      INTEGER MAP                ! AST pointer to mapping
      INTEGER MAPROT             ! AST pointer to rotation mapping
      LOGICAL THERE              ! Was FITS header card found?
      DOUBLE PRECISION ANGLR     ! Total rotation angle in radians
      DOUBLE PRECISION DEGRA     ! Degrees to radians conversion factor
      DOUBLE PRECISION FROT      ! Rotation angle from FITS header in degrees
      DOUBLE PRECISION MATRIX( 4 ) ! Matrix for WinMap
      DOUBLE PRECISION PI        ! Pi
      DOUBLE PRECISION ROTATE    ! Total rotation angle in degrees

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start new AST context.
      CALL AST_BEGIN( STATUS )

*  Set degrees to radians conversion factor.
      PI = 4D0 * ATAN( 1D0 )
      DEGRA = PI / 180D0

*  Get the domain of the Base frame of FSET2.
      CALL AST_INVERT( FSET2, STATUS )
      DMMAT = AST_GETC( FSET2, 'Domain', STATUS )
      CALL AST_INVERT( FSET2, STATUS )

*  Locate the corresponding frame in FSET.
      CALL CCD1_FRDM( FSET, DMMAT, JMAT, STATUS )
      IF ( JMAT .EQ. AST__NOFRAME ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'DOMAIN', DMMAT )
         CALL ERR_REP( 'CCD1_ADFRM_NODMN',
     :   'Frameset does not contain frame ^DOMAIN', STATUS )
         GO TO 99
      END IF

*  Get the Base->Current mapping from FSET2.
      MAP = AST_GETMAPPING( FSET2, AST__BASE, AST__CURRENT, STATUS )

*  Set the additional rotation angle.
      ROTATE = ROT

*  Work out a rotation from FITS headers if one has been requested.
      IF ( FITROT .NE. ' ' ) THEN
         IF ( NCARD .GT. 0 ) THEN
            FROT = 0D0
            CALL FTS1_GKEYD( NCARD, %VAL( CNF_PVAL( IPFITS ) ),
     :                       1, FITROT, THERE,
     :                       FROT, ICARD, STATUS,
     :                       %VAL( CNF_CVAL( 80 ) ) )
            IF ( THERE ) THEN
               ROTATE = ROTATE + FROT
            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'HEAD', FITROT )
               CALL ERR_REP( 'CCD1_ADFRM_NOFRT',
     :                       'No FITS header ^HEAD was found.', STATUS )
               GO TO 99
            END IF
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_ADFRM_NOFITS',
     :                    'No FITS headers were found', STATUS )
            GO TO 99
         END IF
      END IF

*  Incorporate additional rotation if required.
      IF ( ROTATE .NE. 0D0 ) THEN
         ANGLR = ROTATE * DEGRA
         MATRIX( 1 ) = COS( ANGLR )
         MATRIX( 2 ) = -SIN( ANGLR )
         MATRIX( 3 ) = SIN( ANGLR )
         MATRIX( 4 ) = COS(ANGLR )
         MAPROT = AST_MATRIXMAP( 2, 2, 0, MATRIX, ' ', STATUS )
         MAP = AST_CMPMAP( MAP, MAPROT, .TRUE., ' ', STATUS )
         CALL MSG_SETR( 'ANGLE', REAL( ROTATE ) )
         CALL CCD1_MSG( ' ',
     :'    Rotating additional ^ANGLE degrees', STATUS )
      END IF

*  Simplify the mapping.
      MAP = AST_SIMPLIFY( MAP, STATUS )

*  Get the frame to be added from FSET2.  Note that we need a 'deep'
*  copy, since we will modify it and do not wish to upset the original
*  frameset.
      FRM = AST_COPY( AST_GETFRAME( FSET2, AST__CURRENT, STATUS ),
     :                STATUS )

*  Change its domain name as required.
      CALL AST_SETC( FRM, 'Domain', DOMAIN( 1:CHR_LEN( DOMAIN ) ),
     :               STATUS )

*  Add the new frame to FSET, leaving the Current frame as before.
      JCUR = AST_GETI( FSET, 'Current', STATUS )
      CALL AST_ADDFRAME( FSET, JMAT, MAP, FRM, STATUS )
      JNEW = AST_GETI( FSET, 'Nframe', STATUS )
      CALL AST_SETI( FSET, 'Current', JCUR, STATUS )

*  Purge the frameset of any frames with duplicate domains to the one
*  we have just added.
      CALL CCD1_DMPRG( FSET, DOMAIN, .TRUE., JNEW, STATUS )

*  Error status exit.
 99   CONTINUE

*  End AST context.
      CALL AST_END( STATUS )

      END
* $Id$
