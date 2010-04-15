*+  POLRAP - top level Rapi2d subroutine for A-task monolith for polarimetry

      SUBROUTINE POLRAP (STATUS)

*    Description :
*
*     This is the top level monolith subroutine for the Rapi2d suite
*     of A-tasks. The value of NAME is input from the interface and
*     parsed, the requested A-task being called on successful matching
*     of the input string with a valid task name.
*
*     This version of RAPI2D is for polarimetry reduction
*
*    Invocation :
*
*     CALL POLRAP( NAME, STATUS)
*
*    Method :
*
*     The input string NAME is tested against all the valid A-task
*     names after having been forced to upper-case. If a valid test
*     is made, the relevant A-task is called. If not, an error message
*     is output to the environment.
*
*    Deficiencies :
*
*     The input string has to be forced to upper-case (I think).
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Colin Aspin (UKTH::CAA)
*
*    History :
*
*     02-01-1986 : First documented implementation (REVA::MJM)
*     15-01-1987 : Created this version called POLRAP (UKTH::CAA)
*     02-02-1987 : Added STCOADD, BINUP and POLCAL (UKTH::CAA)
*     20-03-1987 : Added ARGSAVE and ABCOM (UKTH::CAA)
*     29-08-1988 : Created POLRAP from OBSRAP (JACH::CAA)
*     20-MAY-1994  Added NDF_BEGIN/END (SKL@JACH)
*     19-AUG-1994  Changed style to new UNIX/VMS Atask monolith style(SKL@JACH)
*     27-Nov-1995: added polthresh2, polcal2 actions (caa@jach)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'PAR_PAR'          ! necessary for non-VMS
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'


      INTEGER  STATUS             ! global status parameter

      CHARACTER*(PAR__SZNAM) NAME   ! action name

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN

         RETURN

      END IF


*    start NDF context
      CALL NDF_BEGIN


*    get the action name

      CALL TASK_GET_NAME( NAME, STATUS )

*    force the input string to upper-case before testing

      CALL UPCASE( NAME, NAME, STATUS)

*    check the string against valid A-task names - if matched then
*    call the relevant A-task

      IF ( NAME .EQ. 'APERPOL' ) THEN

*       calculates the polarization IN APERTURES

         CALL APERPOL ( STATUS )

      ELSE IF ( NAME .EQ. 'CSFIT' ) THEN

*       fits a centro-symmetric polarization pattern to image

         CALL CSFIT ( STATUS )

      ELSE IF ( NAME .EQ. 'CSGEN' ) THEN

*       generates centro-symmetric polarization pattern

         CALL CSGEN ( STATUS )

      ELSE IF ( NAME .EQ. 'DEVFCS' ) THEN

*       calculates deviation from centro-symmetry at point in image

         CALL DEVFCS ( STATUS )

      ELSE IF ( NAME .EQ. 'POLCAL' ) THEN

*       calculates the polarization images from processed waveplate positions
*       IRPOL1 unmasked images

         CALL POLCAL ( STATUS )

      ELSE IF ( NAME .EQ. 'POLCAL2' ) THEN

*       calculates the polarization images from processed waveplate positions
*       IRPOL2 masked images

         CALL POLCAL2 ( STATUS )

      ELSE IF ( NAME .EQ. 'POLLY' ) THEN

*       calculates polarization from input intensities

         CALL POLLY ( STATUS )

      ELSE IF ( NAME .EQ. 'POLLY2' ) THEN

*       calculates polarization from input intensities (dual-beam)

         CALL POLLY2 ( STATUS )

      ELSE IF ( NAME .EQ. 'POLSEP' ) THEN

*       separates dual-beam polarization into separate images

         CALL POLSEP ( STATUS )

      ELSE IF ( NAME .EQ. 'POLSHOT' ) THEN

*       CORRECTS the polarization image for shot-noise polarization

         CALL POLSHOT ( STATUS )

      ELSE IF ( NAME .EQ. 'POLTHRESH' ) THEN

*       thresholds 4 intensity images

         CALL POLTHRESH ( STATUS )

      ELSE IF ( NAME .EQ. 'POLTHRESH2' ) THEN

*       thresholds 8 intensity images

         CALL POLTHRESH2 ( STATUS )

      ELSE IF ( NAME .EQ. 'THETAFIX' ) THEN

*       fixes position angle after addition of constant

         CALL THETAFIX ( STATUS )

      ELSE IF ( NAME .EQ. 'WELCOME_POLRAP' ) THEN

*       welcome info for auto loading of task ...

          CALL WELCOME_POLRAP ( STATUS)

      ELSE

*       no such option exists

        CALL MSG_OUT( 'RAP_ERR',
     :                'Thats quite impossible at the moment ...',
     :                 STATUS )
        CALL MSG_SETI( 'NAME', NAME)
        CALL MSG_OUT( 'MESSAGE', 'Action requested was ^NAME',
     :                 STATUS)

      END IF


* End NDF context, release all locators and unmap arrays
      CALL NDF_END( STATUS )

      END
