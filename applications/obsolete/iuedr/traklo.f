      SUBROUTINE TRAKLO( STATUS )
*+
*  Name:
*     SUBROUTINE TRAKLO

*  Purpose:
*     Extract spectrum from an IUE LORES image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRAKLO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The TRAK approach is used, with further control available
*     for special cases.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-1982 (JRG):
*       Original version.
*     08-NOV-1988 (PCTR):
*       Version 2.0.
*     16-DEC-1994 (MJC):
*       Version 3.2.  Converted prologue to Starlink Style.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS      ! Global status.

*  External References:
      EXTERNAL EXTRAK     ! Extraction routine

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'
      INCLUDE 'CMSPEC'

*  Local Variables:
      INTEGER DATA_VM     ! VM address for DATA array
      INTEGER IAPER       ! aperture index
      INTEGER NAXIS1      ! size of axis 1
      INTEGER NAXIS2      ! size of axis 2
      INTEGER QUAL_VM     ! VM address for QUAL array

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   DATASET.
      CALL MRDATA( NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: unable to access data\\', STATUS )
         RETURN
      END IF

*   Aperture/resolution.
      CALL DEFAPR( 1, IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: aperture/resolution invalid\\',
     :                STATUS )
         RETURN
      END IF

*   Cancel current order.
      CALL CNORD

*   Define template.
      CALL LOTEM( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: setting up dispersion relations\\',
     :                STATUS )
         RETURN
      END IF

*   Read extraction parameters.
      CALL DEFEXT( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading extraction parameters\\',
     :                STATUS )
         RETURN
      END IF

*   Define the wavelength grid.
      CALL LOGRID( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: creating wavelength grid\\', STATUS )
         RETURN
      END IF

*   Define the actual object and background channels.
      CALL LOSLIT( IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: defining extraction channels\\',
     :                STATUS )
         RETURN
      END IF

*   Print intent.
      CALL LINE_WCONT( '%p Extracting Spectrum in \\' )
      CALL LINE_WRITS( '%s aperture.\\', APERS(1, IAPER) )
      CALL PRTBUF( STATUS )

*   Print details.
      CALL PREXTP( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: printing extraction parameters\\',
     :                STATUS )
         RETURN
      END IF

*   Print details of specific aperture.
      CALL PREXT( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: printing extraction parameters\\',
     :                STATUS )
         RETURN
      END IF

*   Extract spectrum.
      CALL EXSPEC( EXTRAK, NAXIS1, NAXIS2, DATA_VM, QUAL_VM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: extracting spectrum\\', STATUS )
         RETURN
      END IF

*   Print extraction pixel usage statistics (etc).
      CALL PRPIX( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: printing pixel usage statistics\\',
     :                STATUS )
         RETURN
      END IF

*   Save result.
      ORDER = IAPER
      CALL WRORD( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: saving result\\', STATUS )
         RETURN
      END IF

*   Calibrate spectrum.
      CALL CALO( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: calibrating spectrum\\', STATUS )
         RETURN
      END IF

*   Say good-bye.
      CALL LINE_WCONT( '%p Spectrum Extraction Completed.\\' )
      CALL PRTBUF( STATUS )
      END
