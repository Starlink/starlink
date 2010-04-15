      SUBROUTINE CDCRA1( PADD, PNAME, PLON, PLAT,
     :                   SCS, MXSRCE, NSRCE, NAME,
     :                   RA, DEC, LON, LAT, STATUS )
*+
*  Name:
*     CDCRA1

*  Purpose:
*     Get additional expected source positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDCRA1( PADD, PNAME, PLON, PLAT,
*                  SCS, MXSRCE, NSRCE, NAME,
*                  RA, DEC, LON, LAT, STATUS )

*  Description:
*     This subroutine is used to get additional expected source
*     positions from the user. It will keep prompting the user for the
*     next source after successfully getting a source until a null, '!',
*     value is supplied.

*  Arguments:
*     PADD = CHARACTER (Given)
*        Name of the parameter used to get the additional sources parameter.
*     PNAME = CHARACTER (Given)
*        Name of the parameter used to get the name of the sources.
*     PLON = CHARACTER (Given)
*        Name of the parameter used to get the longitude of the sources.
*     PLAT = CHARACTER (Given)
*        Name of the parameter used to get the latitude of the sources.
*     SCS = CHARACTER (Given)
*        Name of the sky coordinate system used.
*     MXSRCE = INTEGER (Given)
*        Max. number of sources can be handled.
*     NSRCE = INTEGER (Given and Returned)
*        On entry it gives the number of sources obtained from the CRDD
*        NDF files. On exit it gives the total number of sources.
*     RA( MXSRCE ) = DOUBLE PRECISION (Given and Returned)
*        RA of the sources.
*     DEC( MXSRCE ) = DOUBLE PRECISION (Given and Returned)
*        DEC of the sources.
*     LON( MXSRCE ) = DOUBLE PRECISION (Returned)
*        LON of the sources under the specified sky coordinate system.
*     LAT( MXSRCE ) = DOUBLE PRECISION (Returned)
*        DEC of the sources under the specified sky coordinate system.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     20-NOV-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_PAR'          ! IRA_ package constants
      INCLUDE 'PAR_ERR'          ! Parameter error definitions

*  Arguments Given:
      CHARACTER*( * ) PADD, PNAME, PLON, PLAT
      CHARACTER*( * ) SCS
      INTEGER MXSRCE

*  Arguments Given and Returned:
      INTEGER NSRCE
      CHARACTER*( * ) NAME( MXSRCE )
      DOUBLE PRECISION RA( MXSRCE ), DEC( MXSRCE )

*  Arguments Returned:
      DOUBLE PRECISION LON( MXSRCE ), LAT( MXSRCE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL ADDSOU             ! Flag indicating new source positions are to
                                 ! be input
      LOGICAL EXIT               ! No more sources flag
      INTEGER I                  ! Do loop index
      CHARACTER*( IRA__SZFSC ) LONST, LATST
                                 ! String form of lon. and lat.
      INTEGER NSRCPR             ! Number of sources on entry
      CHARACTER*( 25 ) TMPNAM    ! Name of a new source
      DOUBLE PRECISION TMPLON, TMPLAT
                                 ! Lon. and lat. of a new source

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there is any exist sources, ...
      IF ( NSRCE .GT. 0 ) THEN

*  Convert the input RA and DEC to the coordinates under specified sky
*  coordinate system.
         CALL IRA_CONVT( NSRCE, RA, DEC, 'EQUATORIAL(1950.0)', SCS,
     :                   IRA__IRJEP, LON, LAT, STATUS )

*  Normalise the output from IRA_CONVT
         CALL IRA_NORM( LON, LAT, STATUS )

*  Report the present expected sources to the user.
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'CDADSR_MSG1', 'Expected source(s) obtained '/
     :                /'from input CRDD NDF files:', STATUS )
         CALL MSG_BLANK( STATUS )
         DO I = 1, NSRCE
            CALL MSG_SETI( 'I', I )
            CALL MSG_SETC( 'NAME', NAME( I ) )
            CALL MSG_OUT( 'CDADSR_MSG2', '^I. SOURCE NAME:     '/
     :                   /'^NAME ', STATUS )
            CALL IRA_DTOC( LON( I ), LAT( I ), SCS, 1, LONST, LATST,
     :                     STATUS )
            CALL MSG_SETC( 'LON', LONST )
            CALL MSG_SETC( 'LAT', LATST )
            CALL MSG_OUT( 'CDADSR_MSG2', '   SOURCE POSITION: '/
     :                   /'^LON, ^LAT', STATUS )
            CALL MSG_BLANK( STATUS )
         END DO
      END IF

*  Determine whether the user wants to enter additional source positions
      CALL PAR_GET0L( PADD, ADDSOU, STATUS )

*  If status is abort return
      IF ( STATUS .EQ. PAR__ABORT) RETURN

*  If the user wants to insert sources and the status is ok
      IF ( ADDSOU .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*  Get additional sources.
         NSRCPR = NSRCE
      	 EXIT = .FALSE.
         DO WHILE ( .NOT.EXIT .AND. STATUS .EQ. SAI__OK )

*  Get the name, longitude and latitude of the new source
            CALL PAR_GET0C( PNAME, TMPNAM, STATUS )
            CALL IRA_GETCO( PLON, PLAT, ' of the source', SCS, .FALSE.,
     :                      TMPLON, TMPLAT, STATUS )

*  If a null is obtained from the user, no longer prompt for further
*  sources.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               EXIT = .TRUE.

*  To prevent application from exiting, reset the status.
               CALL ERR_ANNUL( STATUS )

*  If a valid source is obtained, record it.
            ELSE IF ( STATUS .EQ. SAI__OK ) THEN
               NSRCE = NSRCE + 1
               NAME( NSRCE ) = TMPNAM
               LON( NSRCE ) = TMPLON
               LAT( NSRCE ) = TMPLAT

*  Cancel the parameters for the further use.
               CALL PAR_CANCL( PNAME, STATUS )
               CALL PAR_CANCL( PLON, STATUS )
               CALL PAR_CANCL( PLAT, STATUS )
            END IF
         END DO

*  If any additional sources obtained, convert to RA and DEC.
         IF ( NSRCE .GT. NSRCPR ) THEN

*  Convert all additional sources from input coordinate system to Eq(B1950)
            CALL IRA_CONVT( NSRCE - NSRCPR, LON( NSRCPR + 1 ),
     :                     LAT( NSRCPR + 1 ), SCS, 'EQUATORIAL(1950.0)',
     :                     IRA__IRJEP, RA( NSRCPR + 1 ),
     :                     DEC( NSRCPR + 1 ), STATUS )

*  Normalise the output from IRA_CONVT
            DO I = NSRCPR + 1 , NSRCE
               CALL IRA_NORM( RA( I ), DEC( I ), STATUS )
            END DO

         END IF
* Else if additional sources parameter is set null annul the status and continue
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

* End if for if additional sources are required
      END IF

      END
