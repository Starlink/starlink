      SUBROUTINE CDCRA0( NCRDD, NDFID, IRCID, NSRCE, NAME, RA, DEC,
     :                   BAND, SOP, OBS, SCNDIR, STATUS )
*+
*  Name:
*     CDCRA0

*  Purpose:
*     Get the source positions specified in CRDD NDF files.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDCRA0( NCRDD, NDFID, IRCID, NSRCE, NAME, RA, DEC
*                  BAND, SOP, OBS, SCNDIR, STATUS )

*  Description:
*     This subroutine returns the names, sky coordinates (RA and DEC)
*     of the reference positions and other information of a group of
*     CRDD NDFs.

*  Arguments:
*     NCRDD = INTEGER (Given)
*        Number of CRDD NDF files.
*     NDFID( NCRDD ) = INTEGER (Given)
*        IDs of CRDD NDFs.
*     IRCID( NCRDD ) = INTEGER (Given)
*        IRC IDs of CRDD NDFs.
*     NSRCE = INTEGER (Returned)
*        Number of obtained sources.
*     NAME( NCRDD ) = CHARACTER*( * ) (Returned)
*        Name of the soureces.
*     RA( NCRDD ) = DOUBLE PRECISION (Returned)
*        RA of each sources.
*     DEC( NCRDD ) = DOUBLE PRECISION (Returned)
*        DEC of each sources.
*     BAND( NCRDD ) = INTEGER (Returned)
*        Waveband number of CRDD NDFs.
*     SOP( NCRDD ) = INTEGER (Returned)
*        SOP number of CRDD NDFs.
*     OBS( NCRDD ) = INTEGER (Returned)
*        OBS number of CRDD NDFs.
*     SCNDIR( NCRDD ) = LOGICAL (Returned)
*        Scan direction flag. If true, the scan is from north to south,
*        otherwise, the scan is from south to north.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     19-NOV-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_PAR'          ! IRA_ Package constants
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      INTEGER NCRDD
      INTEGER NDFID( NCRDD ), IRCID( NCRDD )

*  Arguments Returned:
      INTEGER NSRCE
      CHARACTER*( * ) NAME( NCRDD )
      DOUBLE PRECISION RA( NCRDD ), DEC( NCRDD )
      INTEGER BAND( NCRDD ), SOP( NCRDD ), OBS( NCRDD )
      LOGICAL SCNDIR( NCRDD )

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER IRC_DETNO          ! Detector number of a detector
      REAL SLA_RANGE             ! put angle into [-pi, pi]

*  Local Variables:
      DOUBLE PRECISION ANGLE( 1 )! Scan angle of a CRDD NDF
      INTEGER I, J               ! Do loop index
      INTEGER LBND( 2 ), UBND( 2 )
                                 ! Bounds of a CRDD NDF
      INTEGER NDIM               ! Number of dimension of a CRDD NDF
      REAL NOMSPD                ! Nominal scan speed of a CRDD NDF
      DOUBLE PRECISION RAFIL, DECFIL
                                 ! RA & DEC of the source of a CRDD NDF
      DOUBLE PRECISION RASMP( 1 ), DECSMP( 1 )
                                 ! RA & DEC of a particular sample
      REAL SCNANG                ! Scan angle in [-pi, pi]
      REAL SPEED( 1 )            ! Scan speed at a particular sample
      LOGICAL THERE              ! Source position existing flag
      CHARACTER*( 20 ) TITLE     ! Title of a CRDD NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Process CRDD NDFs one by one.
      DO I = 1, NCRDD

*  Get the title of the NDF which is used as the name of the source.
         CALL NDF_CGET( NDFID( I ), 'Title', TITLE, STATUS )
         CALL CHR_LDBLK( TITLE )

*  Get other CRDD information about this CRDD NDF.
         CALL IRC_INFO( IRCID( I ), BAND( I ), RAFIL, DECFIL, NOMSPD,
     :                  SOP( I ), OBS( I ), STATUS )

*  Obtain the scan angle of the CRDD NDF at first sample of first
*  detector.
         CALL NDF_BOUND( NDFID( I ), 2, LBND, UBND, NDIM, STATUS )
         CALL IRC_BPOS( IRCID( I ), 1, REAL( LBND( 1 ) ), LBND( 2 ),
     :                  RASMP, DECSMP, ANGLE, SPEED, STATUS )

*  Put scan angle into [-pi, pi].
         SCNANG = SLA_RANGE( REAL( ANGLE( 1 ) ) )

*  If scan angle is in [-pi/2, pi/2], the scan is from north to south.
         IF ( SCNANG .GT. -IRA__PIBY2 .AND.
     :        SCNANG .LT. IRA__PIBY2 ) THEN
            SCNDIR( I ) = .TRUE.

*  Otherwise, the scan is from south to north.
         ELSE
            SCNDIR( I ) = .FALSE.
         END IF

*  If this is not the first CRDD NDF, see whether the expected source
*  position of this CRDD NDF is the same as the one of the CRDD NDF
*  processed previously.
         IF ( I .GT. 1 ) THEN
            THERE = .FALSE.
            DO J = 1, NSRCE
               IF ( ABS( RAFIL - RA( J ) ) .LE. VAL__SMLD .AND.
     :              ABS( DECFIL - DEC( J ) ) .LE. VAL__SMLD )
     :            THERE = .TRUE.
            END DO

*  If this is a new source position, record it
            IF ( .NOT.THERE ) THEN
               NSRCE = NSRCE + 1
               RA( NSRCE ) = RAFIL
               DEC( NSRCE ) = DECFIL
               NAME( NSRCE ) = TITLE
            END IF

*  For the first CRDD NDF, no such check is needed.
         ELSE
            NSRCE = 1
            RA( NSRCE ) = RAFIL
            DEC( NSRCE ) = DECFIL
            NAME( NSRCE ) = TITLE
         END IF
      END DO

      END
