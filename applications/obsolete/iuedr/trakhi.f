      SUBROUTINE TRAKHI( STATUS )
*+
*  Name:
*     SUBROUTINE TRAKHI

*  Purpose:
*     Extract spectrum from IUE HIRES image.
*
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRAKHI( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Extract a series of echelle order spectra from an IUE HIRES image.
*     The TRAK approach is used, with further control available
*     for special cases.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     16-DEC-94 (MJC):
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
      EXTERNAL EXTRAK     ! Extraction routine.

*  Local Variables:
      INTEGER ACTVAL      ! Parameter value count.
      INTEGER DATA_VM     ! VM address for DATA array.
      INTEGER IAPER       ! Aperture index.
      INTEGER NAXIS1      ! Size of axis 1.
      INTEGER NAXIS2      ! Size of axis 2.
      INTEGER NORD        ! Number of orders.
      INTEGER ORD         ! Order value index.
      INTEGER ORD1        ! Start order.
      INTEGER ORD2        ! End order.
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
         CALL ERROUT( 'Error: aperture/resolution invalid\\',
     :                STATUS )
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
      CALL DEFEXT( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading extraction parameters\\',
     :                STATUS )
         GO TO 999
      END IF

*  ORDER.
      DO WHILE ( .TRUE. )
         CALL RDPARI( 'ORDER\\', .FALSE., 1, ORD1, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'ORDER\\', STATUS )
            GO TO 999

         ELSE
            IF ( ORD1.LT.65 .OR. ORD1.GT.125 ) THEN
               CALL ERRPAR( 'ORDER\\' )
               CALL ERROUT( ': out of range\\', STATUS )

            ELSE
               GO TO 100
            END IF
         END IF

         CALL CNPAR( 'ORDER\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'ORDER\\', STATUS )
            GO TO 999
         END IF
      END DO
 100  CONTINUE

*  NORDER.
      DO WHILE ( .TRUE. )
         CALL RDPARI( 'NORDER\\', .FALSE., 1, NORD, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'NORDER\\', STATUS )
            GO TO 999

         ELSE
            IF ( NORD .LT. 0 ) THEN
               CALL ERROUT(': out of range\\', STATUS)

            ELSE
               GO TO 200
            END IF
         END IF

         CALL CNPAR( 'NORDER\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'NORDER\\', STATUS )
            GO TO 999
         END IF
      END DO
 200  CONTINUE

*  Print intent.
      IF ( NORD .GT. 0 ) THEN
         ORD2 = MAX( 65, ORD1 - ( NORD - 1 ) )

      ELSE
         ORD2 = ORD1
      END IF

      CALL LINE_WRITI( '%p Will extract echelle orders (%i,\\', ORD1 )
      CALL LINE_WRITI( '%i).\\', ORD2 )
      CALL PRTBUF( STATUS )

*  Print details.
      CALL PREXTP( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: printing extraction parameters\\',
     :                STATUS )
         GO TO 999
      END IF

*  Extract each order seperately.
      DO ORD = ORD1, ORD2, -1

*     Print order number.
         CALL LINE_WCONT( '%p\\' )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITI( '%p Echelle Order %i\\', ORD )
         CALL PRTBUF( STATUS )

*     Cancel the current order.
         CALL CNORD

*     Define specific dispersion relations.
         CALL ORSET( ORD )

*     Define the wavelength grid.
         CALL HIGRID( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: creating wavelength grid\\',
     :                   STATUS )
            GO TO 300
         END IF

*     Define the actual object and background channels.
         CALL HISLIT( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: creating extraction channels\\',
     :                   STATUS )
            GO TO 300
         END IF

*     Print details.
         CALL PREXT( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: printing extraction parameters\\',
     :                   STATUS )
            GO TO 999
         END IF

*     Extract spectrum.
         CALL EXSPEC( EXTRAK, NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: extracting spectrum\\', STATUS )
            GO TO 300
         END IF

*     Print extraction pixel usage statistics (etc).
         CALL PRPIX( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: printing pixel usage statistics\\',
     :                   STATUS )
            GO TO 999
         END IF

         ORDER = ORD

*     Save result.
         CALL WRORD( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: saving result\\', STATUS )
            GO TO 300
         END IF

*     Calibrate spectrum.
         CALL CAHI( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: calibrating spectrum\\', STATUS )
            GO TO 999
         END IF
      END DO
 300  CONTINUE

*  Update NORDER and ORDER if appropriate.
      IF ( NORD .GT. 0 ) THEN
         CALL LINE_WCONT( '%p Changing ORDER and NORDER parameters.\\' )
         CALL PRTBUF( STATUS )
         CALL WRPARI( 'NORDER\\', 1, 0, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRPAR( 'NORDER\\' )
            CALL ERROUT( ': parameter write error\\', STATUS )
            GO TO 999
         END IF

         CALL WRPARI( 'ORDER\\', 1, ORD2, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRPAR( 'ORDER\\' )
            CALL ERROUT( ': parameter write error\\', STATUS )
            GO TO 999
         END IF
      END IF

*  Say good-bye.
      CALL LINE_WCONT( '%p Spectrum Extraction Completed.\\' )
      CALL PRTBUF( STATUS )

 999  CONTINUE

      END
