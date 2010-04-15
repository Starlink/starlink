      SUBROUTINE POINA1( PNOISP, PSCS, PTHCOR, PTHFIL, PTHSD,
     :                   PTHS2N, LOGFID, LOGREQ, NOISMP, S2NREQ, SCS,
     :                   THCORR, THFILT, THSD, THS2N, STATUS )
*+
*  Name:
*     POINA1

*  Purpose:
*     To obtain parameters such as coordinate system, thresholds,
*     and local noise size for pointcrdd.


*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINA1( PNOISP, PSCS, PTHCOR, PTHFIL, PTHSD,
*                  PTHS2N, LOGFID, LOGREQ, NOISMP, S2NREQ, SCS,
*                  THCORR, THFILT, THSD, THS2N, STATUS )

*  Description:
*     To obtain parameters such as coordinate system, thresholds,
*     and local noise size for pointcrdd.
*
*  Arguments:
*     PNOISP = CHARACTER (Given)
*        The name of the parameter used to get the value of the Number of
*        samples in scan segments used to calculate the local noise
*     PSCS = CHARACTER (Given)
*        The name of the parameter used to get the name of the sky coordinate
*        sytem to be used in subsequent position reference.
*     PTHCOR = CHARACTER (Given)
*        The name of the parameter used to get the value of the threshold of
*        the correlation coefficient.
*     PTHFIL = CHARACTER (Given)
*        The name of the parameter used to get the value of the threshold of
*        square wave filter signal to noise.
*     PTHSD  = CHARACTER (Given)
*        The name of the parameter used to get the value of the threshold
*        for rejecting samples in calculation of various noise values.
*     PTHS2N = CHARACTER (Given)
*        The name of the parameter used to get the value of the threshold
*        of signal to local noise at which a candidate should be accepted
*        as a point source.
*     LOGFID = INTEGER (Given)
*        When logging is required, it gives the ID of the logfile.
*     LOGREQ = LOGICAL (Given)
*        TRUE when logging results to the logfile is required.
*     NOISMP = INTEGER (Returned)
*        Number of samples to be included for each side of expected source
*        in calculation of local noise.
*     S2NREQ = LOGICAL (Returned)
*        TRUE if the final signal to local noise test is required
*     SCS = CHARACTER*( * ) (Returned)
*        The name of the sky coordinate system to used when displaying
*        or obtaining source positions.
*     THCORR = REAL (Returned)
*        Threshold of the correlation coefficient
*     THFILT = REAL (Returned)
*        Threshold of the square wave filter signal to noise.
*     THSD = REAL (Returned)
*        Threshold for rejection of samples in calculation of various noise
*        values.
*     THS2N = REAL (Returned)
*        Threshold of signal to local noise at which a candidate point source
*        is accepted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DCP: Diana Parsons (FIIS\RAL)
*     {enter_new_authors_here}

*  History:
*     29-SEPT-1994 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG system constants
      INCLUDE 'PAR_PAR'          ! PAR system constants
      INCLUDE 'PAR_ERR'          ! PAR system errors

*  Arguments Given:
      CHARACTER*( * ) PNOISP
      CHARACTER*( * ) PSCS
      CHARACTER*( * ) PTHCOR
      CHARACTER*( * ) PTHFIL
      CHARACTER*( * ) PTHSD
      CHARACTER*( * ) PTHS2N
      INTEGER LOGFID
      LOGICAL LOGREQ

*  Arguments Returned:
      INTEGER NOISMP
      LOGICAL S2NREQ
      CHARACTER*( * ) SCS
      REAL THCORR
      REAL THFILT
      REAL THSD
      REAL THS2N

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*( 10 ) THSDS     ! String form of THSD
      INTEGER THSDL              ! Length of string form of THSD
      CHARACTER*( 10 ) THFILS    ! String form of THFILT
      INTEGER THFILL             ! Length of string form of THFILT
      CHARACTER*( 10 ) THCORS    ! String form of THCORR
      INTEGER THCORL             ! Length of string form of THCORR
      CHARACTER*( 10 ) THS2NS    ! String form of THS2N
      INTEGER THS2NL             ! Length of string form of THS2N
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a valid sky coordinate system from the user.
      CALL IRA_GTSCS( PSCS, .FALSE., SCS, STATUS )

*  Get the number of samples in scan segments used to calculate the local
*  noise
      CALL PAR_GET0I( PNOISP, NOISMP, STATUS )

*  Get the threshold of the square wave filter signal to noise
      CALL PAR_GET0R( PTHFIL, THFILT, STATUS )

*  Get the threshold for rejecting samples in the calculation of various noise
*  values
      CALL PAR_GET0R( PTHSD, THSD, STATUS )
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the threshold of the signal to local noise used to accept a candidate
*  as a point source
      CALL PAR_GET0R( PTHS2N, THS2N, STATUS )

*  If the value is ok set the S2NREQ flag true
      IF ( STATUS .EQ. SAI__OK ) THEN
	 S2NREQ = .TRUE.
*  Else if the value given is null indicating test is not required
      ELSE IF ( STATUS .EQ. PAR__NULL) THEN
	 S2NREQ = .FALSE.
         THS2N = 0.0
         CALL ERR_ANNUL( STATUS )
      END IF

*  Get the threshold of the correlation coeficient from the environment.
      CALL PAR_GET0R( PTHCOR, THCORR, STATUS )

*  If the logfile is in use, write values of parameters to it
      IF( LOGREQ ) THEN
         CALL FIO_WRITE( LOGFID, ' ', STATUS )
         CALL FIO_WRITE( LOGFID, 'Parameters used :- ', STATUS )
         CALL CHR_RTOC( THSD, THSDS, THSDL)
         CALL FIO_WRITE( LOGFID,
     :                    'Threshold at which samples are '/
     :                   /'rejected when calculating noise = '/
     :                   /THSDS( :THSDL), STATUS )
         CALL CHR_RTOC( THFILT, THFILS, THFILL)
         CALL FIO_WRITE( LOGFID,
     :                    'Threshold at which candidates '/
     :                   /'are selected from filtered data = '/
     :                   /THFILS( :THFILL), STATUS )
         CALL CHR_RTOC( THCORR, THCORS, THCORL)
         CALL FIO_WRITE( LOGFID,
     :                    'Threshold of correlation '/
     :                   /'coeff. at which candidates are '/
     :                   /'accepted = '//THCORS( :THCORL), STATUS )

         IF ( S2NREQ ) THEN
            CALL CHR_RTOC( THS2N, THS2NS, THS2NL)
            CALL FIO_WRITE( LOGFID,
     :                    'Threshold of signal to '/
     :                   /'local noise at which sources are '/
     :                   /'finally accepted = '/
     :                   /THS2NS( :THS2NL), STATUS )
         ELSE
            CALL FIO_WRITE( LOGFID,
     :                    'Final signal to local noise '/
     :                   /'test is not carried out', STATUS )
         END IF
         CALL FIO_WRITE( LOGFID, ' ', STATUS )
      END IF

      END
