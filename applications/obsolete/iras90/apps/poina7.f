      SUBROUTINE POINA7( MAXDET, DETNUM, DETSCA, IDET, LOGFID, LOGREQ,
     :                   UNITS, WNOSMP, WSTDEV, STATUS )
*+
*  Name:
*     POINA7

*  Purpose:
*     To report the whole scan noise for current detector

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINA7( MAXDET, DETNUM, DETSCA, IDET, LOGFID, LOGREQ,
*                  UNITS, WNOSMP, WSTDEV, STATUS )

*  Description:
*     To report the whole scan noise for current detector
*
*  Arguments:
*     MAXDET = INTEGER (Given)
*        Maximum number of detectors
*     DETNUM( MAXDET )  = INTEGER (Given)
*        Detector number for given detector index
*     DETSCA( MAXDET )  = REAL (Returned)
*        Scale factor used in translating units for this detector, is
*        VAL__BADR for dead detectors
*     IDET = INTEGER (Given)
*        Detector index for the current detector
*     LOGFID = INTEGER (Given)
*        When logging is required, it gives the ID of the logfile.
*     LOGREQ = LOGICAL (Given)
*        TRUE when logging results to the logfile is required.
*     UNITS = CHARACTER (Given)
*        The units in which data values are to be reported.
*     WNOSMP = INTEGER (Given)
*        Number of samples used in calculating the mean and standard deviation
*     WSTDEV = REAL (Given)
*        Standard deviation of sample value over whole range
*	 (with outlying values removed)
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DCP: Diana Parsons (FIIS\RAL)
*     {enter_new_authors_here}

*  History:
*     17-OCT-1994 (DCP):  (My son's 18th birthday)
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

*  Arguments Given:
      INTEGER MAXDET
      INTEGER DETNUM( MAXDET )
      REAL DETSCA( MAXDET )
      INTEGER IDET
      INTEGER LOGFID
      LOGICAL LOGREQ
      CHARACTER*( * ) UNITS
      INTEGER WNOSMP
      REAL WSTDEV

*  Status:
      INTEGER STATUS             ! Global status

* External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER*( 5 )DETST       ! Character string of detector number
      INTEGER DETLN	         ! Length of the DETST
      INTEGER UNITLN             ! Length of the UNITS character string
      REAL WNOISE                ! Noise for whole detector trace in correct
				 ! units
      CHARACTER*( 5 )WNSMST      ! String equivalent of WNOSMP
      INTEGER WNSMLN             ! Length of WNSMST
      CHARACTER*( 8 )WNOIST      ! String equivalent of WNOISE
      INTEGER WNOILN             ! Length of WNSMST
      CHARACTER*( 80 ) TEMPSTR   ! Temporary string
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Translate the detector number associated with the detector index number
*  to a character string
         CALL CHR_ITOC( DETNUM( IDET ), DETST, DETLN )

*  The noise is calculated as the standard deviation times the scale factor
*  to bring the detector to the correct units
         WNOISE = WSTDEV * DETSCA( IDET )

*  Translate the number of samples and the noise value to character strings
         CALL CHR_ITOC( WNOSMP, WNSMST, WNSMLN )
         CALL CHR_RTOC( WNOISE , WNOIST, WNOILN )

*  Find the length of the string containing a description of the units to be
*  output
         UNITLN = CHR_LEN( UNITS )

*  Report whole scan noise
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETC('DETST', DETST)
         CALL MSG_OUT( 'POINA7_MSG1', 'The Noise along the whole '/
     :              /'selected scan length for the trace of detector '/
     :              /'^DETST', STATUS )
         CALL MSG_SETC('UNITS', UNITS)
         CALL MSG_SETC('WNOIST', WNOIST)
         CALL MSG_OUT( 'POINA7_MSG2', 'was ^WNOIST (^UNITS)'/
     :              /'and was calculated on '//WNSMST( : WNSMLN )/
     :              /' samples ',
     :              STATUS )

*  Write to the logging file as well if requested.
         IF ( LOGREQ ) THEN
            CALL FIO_WRITE( LOGFID, ' ', STATUS )
            CALL FIO_WRITE( LOGFID, 'The Noise along the whole '/
     :              /'selected scan length for the trace of detector '/
     :              /DETST(  : DETLN ), STATUS )
            TEMPSTR = UNITS( : UNITLN )
            CALL FIO_WRITE( LOGFID, 'was '//WNOIST( : WNOILN )/
     :              /' ('//TEMPSTR//') '/
     :              /'and was calculated on '//WNSMST( : WNSMLN )/
     :              /' samples ',
     :              STATUS )
         END IF
      END
