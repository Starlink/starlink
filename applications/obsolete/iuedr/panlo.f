      SUBROUTINE PANLO( STATUS )

*+
*
*   Name:
*      SUBROUTINE PANLO
*
*   Description:
*      Produce an LBLS array from LORES image.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-NOV-88     IUEDR Vn. 2.0
*      Martin Clayton     30-SEP-94     IUEDR Vn. 3.1-6
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Export:
      INTEGER STATUS      ! status return

*   External references:
      EXTERNAL EXLBLS     ! LBLS extraction routine

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'
      INCLUDE 'CMSPEC'

*   Local variables:
      INTEGER DATA_VM     ! VM address for DATA array
      INTEGER IAPER       ! aperture index
      INTEGER NAXIS1      ! size of axis 1
      INTEGER NAXIS2      ! size of axis 2
      INTEGER QUAL_VM     ! VM address for QUAL array

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   DATASET
      CALL MRDATA( NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: unable to access data\\', STATUS )
         RETURN
      END IF

*    Aperture/resolution
      CALL DEFAPR( 1, IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: aperture/resolution invalid\\', STATUS )
         RETURN
      END IF

*   Cancel current order
      CALL CNORD

*   Define template
      CALL LOTEM( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: setting up dispersion relations\\',
     :                STATUS )
         RETURN
      END IF

*   Read extraction parameters
      CALL DEFPAN( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading parameters\\', STATUS )
         RETURN
      END IF

*   Define the wavelength grid
      CALL LOGRID( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: creating wavelength grid\\', STATUS )
         RETURN
      END IF

*   Define the actual object and background channels
      CALL LORAD( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: defining extraction channels\\',
     :                STATUS )
         RETURN
      END IF

*   Print intent
      CALL LINE_WCONT( '%p Creating LBLS in \\' )
      CALL LINE_WRITS( '%s aperture.\\', APERS(1, IAPER) )
      CALL PRTBUF( STATUS )

*   Print details
      CALL PRPAN( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: printing LBLS parameters\\', STATUS )
         RETURN
      END IF

*   Extract LBLS
      CALL EXSPEC( EXLBLS, NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: creating LBLS\\', STATUS )
         RETURN
      END IF

*   Calibrate wavelengths
      CALL CALBLS( IAPER, 0 )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: calibrating wavelengths\\', STATUS )
         RETURN
      END IF

*   Say good-bye
      CALL LINE_WCONT( '%p LBLS Creation Completed.\\' )
      CALL PRTBUF( STATUS )
      END
