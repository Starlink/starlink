      SUBROUTINE PANHI( STATUS )
*+
*  Name:
*     SUBROUTINE PANHI

*  Purpose:
*     Create an LBLS array for a HIRES order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PANHI( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     04-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     15-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'
      INCLUDE 'CMSPEC'

*  Status:
      INTEGER STATUS      ! Global status.

*  External References:
      EXTERNAL EXLBLS     ! LBLS extraction routine.

*  Local Variables:
      INTEGER ACTVAL      ! Parameter value count.
      INTEGER DATA_VM     ! VM address for DATA array.
      INTEGER IAPER       ! Aperture index.
      INTEGER NAXIS1      ! Size of axis 1.
      INTEGER NAXIS2      ! Size of axis 2.
      INTEGER ORD         ! Order value index.
      INTEGER QUAL_VM     ! VM address for QUAL array.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  DATASET.
      CALL MRDATA( NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: unable to access data\\', STATUS )
         GO TO 999
      END IF

*  Perform checks, default assignments and prompts.
      CALL DEFAPR( 2, IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: aperture/resolution invalid\\', STATUS )
         GO TO 999
      END IF

*  Define template.
      CALL HITEM( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: setting up dispersion relations\\',
     :                STATUS )
         GO TO 999
      END IF

*  Read extraction parameters.
      CALL DEFPAN( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading extraction parameters\\',
     :                STATUS )
         GO TO 999
      END IF

*  ORDER.
      DO WHILE ( .TRUE. )
         CALL RDPARI( 'ORDER\\', .FALSE., 1, ORD, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'ORDER\\', STATUS )
            GO TO 999

         ELSE
            IF ( ORD.GE.65 .AND. ORD.LE.125 ) THEN
               GO TO 100
            END IF
            CALL ERRPAR( 'ORDER\\' )
            CALL ERROUT( ': out of range\\', STATUS )
         END IF

         CALL CNPAR( 'ORDER\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'ORDER\\', STATUS )
            GO TO 999
         END IF
      END DO
 100  CONTINUE

*  Print intent.
      CALL LINE_WRITI( '%p Creating LBLS for echelle order %i\\', ORD )
      CALL PRTBUF( STATUS )

*  Cancel the current order.
      CALL CNORD

*  Define specific dispersion relations.
      CALL ORSET( ORD )

*  Define the wavelength grid.
      CALL HIGRID( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: creating wavelength grid\\', STATUS )
         GO TO 999
      END IF

*  Define the actual object and background channels.
      CALL HIRAD( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: creating extraction channels\\',
     :                STATUS )
         GO TO 999
      END IF

*  Print details.
      CALL PRPAN( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: printing extraction parameters\\',
     :                STATUS )
         GO TO 999
      END IF

*  Extract spectrum.
      CALL EXSPEC( EXLBLS, NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: extracting spectrum\\', STATUS )
         GO TO 999
      END IF

*  Calibrate wavelengths.
      CALL CALBLS( IAPER, ORD )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: calibrating wavelengths\\', STATUS )
         GO TO 999
      END IF

*  Say good-bye.
      CALL LINE_WCONT( '%p LBLS Creation Completed.\\' )
      CALL PRTBUF( STATUS )

 999  CONTINUE

      END
