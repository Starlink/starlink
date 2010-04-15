      SUBROUTINE PON_GETPROJ( PNPROJ, PNRA0, PNDEC0, PROJECTION, RA0,
     :                        DEC0, STATUS )
*+
*  Name:
*     PON_GETPROJ

*  Purpose:
*     Get the parameters associated with the projection.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_GETPROJ( PNPROJ, PNRA0, PNDEC0, PROJECTION, RA0, DEC0,
*                       STATUS )

*  Description:
*     Get the projection type (defined as an integer):
*
*     PROJECTION             TYPE
*         1                  None
*         2               Tangent
*         3                  sine
*         4                   Arc
*         5     Global Sinusoidal
*         6                Aitoff
*         7              Mercator
*         8         Stereographic
*
*     along with the centre of the projection in radians.
*
*     The routine must be given the names of the ADAM parameters which
*     appear in the inteface file for these quantities

*  Arguments:
*     PNPROJ = CHARACTER * ( * ) (Given)
*        ADAM parameter name for the PROJECTION type.
*     PNRA0 = CAHRACTER * ( * ) (Given)
*        ADAM parameter name for the centre of the projection in
*        longitude.
*     PNDEC0 = CHARACTER * ( * ) (Given)
*        ADAM parameter name for the centre of the projection in
*        latitude.
*     PROJECTION = INTEGER (Returned)
*        The projection number.
*     RA0 = DOUBLE PRECISION (Returned)
*        The projection centre in longitude (radians).
*     DEC0 = DOUBLE PRECISION (Returned)
*        The projection centre in latitude (radians).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-APR-1990 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Arguments Given:
      CHARACTER * ( * ) PNPROJ
      CHARACTER * ( * ) PNRA0
      CHARACTER * ( * ) PNDEC0

*  Arguments Returned:
      INTEGER PROJECTION

      DOUBLE PRECISION RA0
      DOUBLE PRECISION DEC0

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER INTCMD             ! [internal_description]
      LOGICAL PARPOS             ! [internal_description]

*  Local Variables:
      CHARACTER * ( 20 ) PROJNAM( NPROJ+1 ) ! Projection names
      CHARACTER * ( 80 ) TEMPPROJ ! Temporary projection value
      CHARACTER * ( 80 ) CRA0    ! CHARACTER representation RA0
      CHARACTER * ( 80 ) CDEC0   ! CHARACTER representation DEC0

*  Local Data:
      DATA PROJNAM / 'TAN', 'SIN', 'ARC', 'GLS', 'AITOFF', 'MERCATOR',
     :               'STG', ' ' /

*.

*  Check inherited global status.
      IF ( STATUS.NE.SAI__OK ) RETURN

      CALL PAR_GET0C( PNPROJ, TEMPPROJ, STATUS )
      CALL CHR_UCASE( TEMPPROJ )

      IF ( TEMPPROJ( 1:4 ).EQ.'NONE' ) THEN
         RA0 = 0.0D+00
         DEC0 = 0.0D+00
         PROJECTION = 1
      ELSE
         PROJECTION = INTCMD( PROJNAM, TEMPPROJ )

         IF ( PROJECTION.GT.0 ) THEN
            PROJECTION = PROJECTION + 1
            CALL PAR_GET0C( PNRA0, CRA0, STATUS )
            CALL PAR_GET0C( PNDEC0, CDEC0, STATUS )

            IF ( PARPOS( CRA0, RA0 ) ) THEN
               RA0 = RA0*DAS2R*15.0D+00
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'PON_GPROJ_NORA',
     :                       'Longitude not recognised.', STATUS )
            END IF

            IF ( PARPOS( CDEC0, DEC0 ) ) THEN
               DEC0 = DEC0*DAS2R
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'PON_GPROJ_NODEC',
     :                       'Latitude not recognised.', STATUS )
            END IF
         ELSE
            CALL MSG_OUT( ' ', 'Projection not recognised - ' //
     :                    'NONE assumed.', STATUS )
            RA0 = 0.0D+00
            DEC0 = 0.0D+00
            PROJECTION = 1
         END IF
      END IF

      END
* $Id$
