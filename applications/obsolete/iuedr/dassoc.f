      SUBROUTINE DASSOC( NEEDS, VITAL, STATUS )

*+
*
*   Name:
*      SUBROUTINE DASSOC
*
*   Description:
*      If DATASET differs from the current internal dataset file, then
*      the new file is read.
*      The NEEDS parameter consists a series of characters indicating the
*      items required from the DATASET (I => UED+UEQ; S => UES; M => UEM).
*      The header/calibration file is always read (UEC).
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          22-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     18-SEP-94     IUEDR Vn. 3.1-8
*         Modified to be case sensitive to DATASET.
*
*   Method:
*      Where the current DATASET is required, the set of required items is
*      compared with those actually available, and a set of "need" switches
*      generated. These needs are then satisfied by a call to RDIMG.
*      Where a new DATASET is required, a similar procedure is used, except
*      that it is necessary to read the header part (UEC file).
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      BYTE NEEDS(*)         ! list of needs from DATASET
      BYTE VITAL(*)         ! whether vital (T) or not (F)

*   Export:
      INTEGER STATUS        ! status return

*   External references:
      LOGICAL STR_EQUAL     ! string equality
      INTEGER STR_LEN       ! string length

*   Global includes:
      INCLUDE 'CMFILE'
      INCLUDE 'CMNEED'
      INCLUDE 'CMMUST'
      INCLUDE 'CMDATA'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMCOMB'

*   Local variables:
      BYTE DSNEW(81)        ! new dataset file name

      INTEGER ACTVAL        ! parameter value count
      INTEGER NNEED         ! number of need characters
      INTEGER NVITA         ! number of vital characters

*.

*   Repeated attempt to satisfy needs

 100  CONTINUE
      CALL RDPARC( 'DATASET\\', .FALSE., 81, DSNEW, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'DATASET\\' , STATUS )
         GO TO 999
      END IF

*   Modify needs based on whether DATASET is new or current
      IF ( .NOT. STR_EQUAL( DSNEW, DSNAME ) .OR. NODSN ) THEN

*      Flush any existing dataset
         CALL FRDSN( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: writing previous dataset\\',
     :                   STATUS )
            GO TO 999
         END IF

         CALL CNDSN

*      Force need of Calibration (at least)
         CANEED = .TRUE.
         CAMUST = .TRUE.

*   Inhibit read of Calibration
      ELSE
         CANEED = .FALSE.
      END IF

*   Default needs in advance
      DANEED = .FALSE.
      SPNEED = .FALSE.
      MENEED = .FALSE.

*   Set up specific needs from NEEDS argument
      NNEED = STR_LEN( NEEDS )
      NVITA = STR_LEN( VITAL )

 120  CONTINUE

      IF ( .NOT. ( NNEED .GT. 0) ) THEN
         CALL RDIMG( DSNEW, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL STR_MOVE( DSNEW, 81, DSNAME )
            NODSN = .FALSE.
            GO TO 999

         ELSE
            CALL CNPAR( 'DATASET\\', STATUS )
         END IF

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
         GO TO 120
      END IF
      GO TO 100

 999  CONTINUE
      END
