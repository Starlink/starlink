*+  PSS_CHK_MULT - Check source list for multiple detections
      SUBROUTINE PSS_CHK_MULT( FAC, STATUS )
*    Description :
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     20 Sep 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      REAL                     FAC                       ! RCRIT scale factor
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      REAL                     PSS_PSF_RCRIT             ! Find critical radius
*
*    Local variables :
*
      REAL                     RCRIT                     ! Critical src sep'n

      INTEGER                  ID, NID                   ! Source id's
      INTEGER                  ISRC                      ! Loop over source list
      INTEGER                  NREJ                      ! # of rejects

      LOGICAL                  REJECT                    ! Still rejecting srcs?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Use constant critical radius for constant psf
      IF ( PSF_CONSTANT ) RCRIT = FAC*PSS_PSF_RCRIT( 0.0, 0.0 )

*    Check for multiples
      NREJ = 0
      REJECT = .TRUE.
      DO WHILE ( ( LI_NSRC .GT. 1 ) .AND. REJECT )

*      Rank sources in list by nearest neighbour
        REJECT = .FALSE.
        CALL PSS_SRC_RANK( STATUS )

*      Loop through every source - from closest to widest separation
        ISRC = LI_NSRC
        DO WHILE ( ( ISRC .GT. 0 ) .AND. .NOT. REJECT )

*        Get source ids
          ID = LI_ID(ISRC)
          NID = LI_ID(LI_NNBR_ID(ISRC))

*        Find critical separation if psf not constant. Use average critical
*        separation at each source
          IF ( .NOT. PSF_CONSTANT ) THEN
            RCRIT = FAC* ( PSS_PSF_RCRIT( S_CP(1,ID) ) +
     :              PSS_PSF_RCRIT( S_CP(1,NID) ) ) / 2.0
          END IF

*        Are sources too close
          IF ( LI_NNBR_R(ISRC) .LT. RCRIT ) THEN

*          Keep the brighter one
            IF ( S_SIG(ID) .GT. S_SIG(NID) ) THEN
              LI_ACTIVE(LI_NNBR_ID(ISRC)) = .FALSE.
            ELSE
              LI_ACTIVE(ISRC) = .FALSE.
            END IF

*          One more lost discovery goes down the drain
            REJECT = .TRUE.
            NREJ = NREJ + 1

          END IF

*        Next source
          ISRC = ISRC - 1

        END DO

*      If there was a rejection, squeeze the list, otherwise quit
        IF ( REJECT ) THEN

*        Shuffle entries to beginning of list
          CALL PSS_SRC_SQUEEZE( STATUS )

        END IF

      END DO

      IF ( ( NREJ .GT. 0 ) .AND. DI_ON ) THEN
        CALL MSG_SETI( 'N', NREJ )
        CALL MSG_PRNT( 'Rejecting ^N multiple detections' )
      END IF

      END
