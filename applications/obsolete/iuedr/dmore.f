      SUBROUTINE DMORE( NEEDS, VITAL, STATUS )

*+
*
*   Name:
*      SUBROUTINE DMORE
*
*   Description:
*      There must be an existing dataset (NODSN=FALSE, NOHEAD=FALSE).
*      The NEEDS parameter consists a series of characters indicating the
*      items required from the DATASET (I => UED+UEQ; S => UES; M => UEM).
*      The header/calibration file is never read (UEC).
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          22-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     22-OCT-94     IUEDR Vn. 3.2
*
*   Method:
*      Set one of the Spectrum, Mean or Image needs switches as necessary.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      BYTE NEEDS(*)          ! list of needs from DATASET
      BYTE VITAL(*)          ! whether vital (T) or not (F)

*   Export:
      INTEGER STATUS         ! status return

*   External references:
      INTEGER STR_LEN        ! string length

*   Global variables:
      INCLUDE 'CMFILE'
      INCLUDE 'CMNEED'
      INCLUDE 'CMMUST'
      INCLUDE 'CMDATA'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMCOMB'

*   Local variables:
      INTEGER NNEED          ! number of need characters
      INTEGER NVITA          ! number of vital characters

*   Default needs in advance
      DANEED = .FALSE.
      SPNEED = .FALSE.
      MENEED = .FALSE.

*   Set up specific needs from NEEDS argument
      NNEED = STR_LEN(NEEDS)
      NVITA = STR_LEN(VITAL)

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

 100  CONTINUE
      IF ( .NOT. (NNEED.GT.0) ) THEN

*      Inhibit read of Calibration
         CANEED = .FALSE.

*      Read DSNAME
         CALL RDIMG( DSNAME, STATUS )

      ELSE

*      Image (I)
         IF ( NEEDS(NNEED).EQ.73 .AND. NOIMA ) THEN
            DANEED = .TRUE.

*         Release previous DATA_VM
            IF ( DATA_VM .GT. 0 ) CALL DLADR( DATA_VM, STATUS )

*         Release previous QUAL_VM
            IF ( QUAL_VM .GT. 0 ) CALL DLADR( QUAL_VM, STATUS )
            DAMUST = .FALSE.

            IF ( NVITA .GE. NNEED ) THEN
               IF ( VITAL(NNEED) .EQ. 84 ) DAMUST = .TRUE.
            END IF
         END IF

*      Spectrum (S)
         IF ( NEEDS(NNEED).EQ.83 .AND. NOSPEC ) THEN
            SPNEED = .TRUE.
            SPMUST = .FALSE.
            IF ( NVITA .GE. NNEED ) THEN
               IF ( VITAL(NNEED) .EQ. 84 ) SPMUST = .TRUE.
            END IF
         END IF

*      Mean (M)
         IF ( NEEDS(NNEED).EQ.77 .AND. NOCOMB ) THEN
            MENEED = .TRUE.
            MEMUST = .FALSE.
            IF ( NVITA .GE. NNEED ) THEN
               IF ( VITAL(NNEED) .EQ. 84 ) MEMUST = .TRUE.
            END IF
         END IF
         NNEED = NNEED - 1
         GO TO 100
      END IF
      END
