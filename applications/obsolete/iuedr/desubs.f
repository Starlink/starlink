      SUBROUTINE DESUBS( RW_SL, MAXL, LMN, LMX, SMN, SMX, NSUB,
     :                   STATUS )
*+
*
*   Name:
*      SUBROUTINE DESUBS
*
*   Description:
*      Define subset for spectrum.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      This defines the image subset needed to extract the current spectrum.
*      The grid is that defined by CMEXTP.
*      The grid is used to generate a polygon which delineates its
*      coverage of the image.
*
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*   Global Variables:
      INCLUDE 'CMDATA'
      INCLUDE 'CMEXTP'
      INCLUDE 'CMWAV'

*  Arguments Given:
      INTEGER MAXL              ! maximum number of lines

*  Arguments Returned:
      INTEGER LMN               ! start L
      INTEGER LMX               ! end L
      INTEGER NSUB              ! pixel count in subset
      INTEGER SMN( MAXL )       ! minimum image sample per line
      INTEGER SMX( MAXL )       ! maximum image sample per line

*  Status:
      INTEGER STATUS            ! Global status.

*  External References:
      EXTERNAL RW_SL            ! (R,W) to (S,L) transform

*  Local variables:
      REAL*8 DR                 ! R difference
      REAL*8 LP(4*(4096 + 1))   ! L-coordinates
      REAL*8 RMAX               ! maximum R
      REAL*8 RMIN               ! minimum R
      REAL*8 RP(4*(4096 + 1))   ! R-coordinates
      REAL*8 R1                 ! R temporary
      REAL*8 SP(4*(4096 + 1))   ! S-coordinates
      REAL*8 WP(4*(4096 + 1))   ! W-coordinates

      INTEGER I                 ! loop index
      INTEGER IBKG              ! loop index
      INTEGER IL                ! loop index
      INTEGER IS                ! loop index
      INTEGER NP                ! number of polygon points
      INTEGER NR                ! DR index

*.

*   Define an (R,W) grid as appropriate
      RMIN = ROBJ( 1 )
      RMAX = ROBJ( 2 )

      DO IBKG = 1, NBKG
         RMIN = MIN( RMIN, RBKG( 1, IBKG ) )
         RMAX = MAX( RMAX, RBKG( 2, IBKG ) )
      END DO

      R1 = RMIN
      DR = 1.0
      NR = IFIX( REAL( ( RMAX - RMIN ) / DR ) ) + 2

*   Define polygon.
      CALL MSC_GRPOG( NWAV, WAV1, DWAV, NR, R1, DR, 4 * ( 4096 + 1 ),
     :                WP, RP, NP, STATUS)
      IF ( STATUS .NE. SAI__OK) THEN
         CALL ERROUT('Error: msc_grpog failed in desubs\\', STATUS)
         RETURN
      END IF

*   Distort polygon.
      DO I = 1, NP
         CALL RW_SL( RP( I ), WP( I ), SP( I ), LP( I ) )
      END DO

*   Use polygon to form image subset.
      CALL MSC_POSUB( NP, SP, LP, NS, NL, LMN, LMX, SMN, SMX )

*   Merge this subset with the primary one.
      CALL MSC_MESUB( NL, LMIN, LMAX, SMIN, SMAX, LMN, LMX, SMN, SMX )

*   Count subset pixels.
      NSUB = 0
      DO IL = LMN, LMX
         DO IS = SMN( IL ), SMX( IL )
            NSUB = NSUB + 1
         END DO
      END DO

      IF ( NSUB .EQ. 0) THEN
         STATUS = -3
      END IF

      END
