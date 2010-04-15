      SUBROUTINE CDCRA9( POUT, PTITL, AUTO, NAME,
     :                   RA, DEC,
     :                   NDF, BAND, INDX,
     :                   FID, OGID, OUTNDF, STATUS )
*+
*  Name:
*     CDCRA9

*  Purpose:
*     Create an NDF to contain coadded crossing trace section.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDCRA9( POUT, PTITL, AUTO, NAME, RA, DEC, NDF, BAND, INDX,
*                  FID, OGID, OUTNDF, STATUS )

*  Description:
*     This subroutine creates an one-dimension NDF file to contain the
*     coadded crossing trace section. The created NDF will inherit the
*     IRAS extension of a given NDF and the row containning the coadded
*     trace will have the given index.

*  Arguments:
*     POUT = CHARACTER*( * ) (Given)
*        Name of the parameter to get the name of the NDF file to be
*        created. In automatic mode the name of the NDF file will be
*        created by the routine and hence this parameter will not be
*        used.
*     PTITL = CHARACTER*( * ) (Given)
*        Name of the parameter to get the title of the new NDF file. In
*        automatic mode the title will be created by the routine and
*        hence this parameter will not be used.
*     AUTO = LOGICAL (Given)
*        If true, the routine will work in automatic mode in which the
*        name and the title of the created NDF will be created by the
*        routine.
*     NAME = CHARACTER*( * ) (Given)
*        The name of the expected source.
*     RA = DOUBLE PRECISION (Given)
*        The RA of the expected source in Eq(B1950) radians
*        to be output as reference position
*     DEC = DOUBLE PRECISION (Given)
*        The DEC of the expected source in Eq(B1950) radians
*        to be output as reference position
*     NDF = INTEGER (Given)
*        The ID of the NDF whose IRAS extension will be inherited by
*        the new NDF.
*     BAND = INTEGER (Given)
*        The waveband of this coadded crossing belongs to.
*     INDX = INTEGER (Given)
*        The index of the row containing the coadded crossing trace in
*        the output NDF.
*     FID = INTEGER (Given)
*        ID of the logging file.
*     OGID = INTEGER (Given)
*        ID of the group containning the name of the output NDF files.
*     OUTNDF = INTEGER (Returned)
*        The ID of the output NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     2-DEC-1992 (WG):
*        Original version.
*     20-SEP-1994 (DCP):
*        Modified to give correct reference position
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT system constants
      INCLUDE 'DAT_ERR'          ! DAT system errors
      INCLUDE 'PAR_PAR'          ! Parameter System constants
      INCLUDE 'PAR_ERR'          ! Parameter System constants

*  Arguments Given:
      CHARACTER*( * ) POUT, PTITL
      LOGICAL AUTO
      CHARACTER*( * ) NAME
      DOUBLE PRECISION RA
      DOUBLE PRECISION DEC
      INTEGER NDF
      INTEGER BAND, INDX
      INTEGER FID
      INTEGER OGID

*  Arguments Returned:
      INTEGER OUTNDF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL CHR_ISNAM          ! Is the string a valid filename

*  Local Variables:
      CHARACTER BANDST           ! String form of waveband number
      CHARACTER*( 80 ) CLIST     ! List of components to be propagated
      CHARACTER*( 40 ) DEFNAM    ! Default file name for request filename
      INTEGER LBND( 2 ), UBND( 2 ) ! Lower and Upper bounds of the NDF
      CHARACTER*( DAT__SZLOC ) LOC1 ! Locator to the IRAS extension
      CHARACTER*( DAT__SZLOC ) LOC2 ! Locator to the CRDD_INFO structure
      CHARACTER*( DAT__SZLOC ) LOC3 ! Locator to the REF_RA component
      INTEGER NCHAR              ! Number of characters in a string
      INTEGER NDIM               ! Number of dimension of ouput NDF
      LOGICAL NAMEFL             ! TRUE if the name is a valid filename
      INTEGER NAMELN             ! Used length of string NAME
      INTEGER NINDX              ! Index of new NDF name in output group
      CHARACTER*( 40 ) ONAM( 1 ) ! Name of the new NDF
      CHARACTER*( 40 ) TITLE     ! Title of the new NDF
      CHARACTER*( 6 ) WVBND      ! Waveband string
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NAMELN = 0

* ***********************************************************************
* Construct the name and title of the output NDF
* ***********************************************************************
*  The subroutine first attempts to use the name of the source as part of the
*  output ndf name. This name is either the title of the first ndf for the
*  source if the position is the reference position of the NDF, or it uses the
*  users supplied source name for any additional sources.
*  It is when the title is used that there may be problems, as the title may
*  contain spaces or other invalid characters. If this is the case the program
*  first tries to truncate the name to the first space. It only aborts if this
*  fails and the auto option is set.
*  If the auto option is not set the user is prompted for individual file names
*  for all files using the best name the subroutine can generate as default.
*
*  Determine whether the name is consistent with being part of a file name
      NAMEFL = CHR_ISNAM( NAME )
      IF ( NAMEFL ) THEN
*  If the name is suitable find its length
         NAMELN = CHR_LEN( NAME )
      ELSE
*  If the name is not suitable truncate the name to the first space
         CALL CHR_TRUNC( ' ', NAME )
*  Check that this is now a suitable filename
         NAMEFL = CHR_ISNAM( NAME )
         IF ( NAMEFL ) THEN
*  If the name is suitable find its length
            NAMELN = CHR_LEN( NAME )
         ELSE
*  If the name is not suitable then either the application is not in auto
*  and therefore the user can supply a valid file name, or the application
*  should be aborted and a message given to run in non auto.
            IF ( AUTO ) THEN
               STATUS = SAI__ERROR
               CALL EMS_REP('COADDCRDD_ERR',
     :         'Coaddcrdd cannot provide a valid output NDF name,'/
     :         /' Please rerun with noautomatic so that the user '/
     :         /'can supply the file name', STATUS )
            ENDIF
*  End if for check of name after truncation
         ENDIF
*  End if for check that the name is a valid NDF filename
      ENDIF

*  If the status is bad return
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the band character to append to the file name
      CALL CHR_ITOC( BAND, BANDST, NCHAR )

*  If the name was a valid file name
      IF ( NAMEFL ) THEN

*  Convert the name to lower case
         CALL CHR_LCASE( NAME )
*  Construct the name of the output NDF containing the file name
         ONAM( 1 ) = 'coadded_'//NAME( : NAMELN )//'_b'//BANDST

      ELSE
*  Construct the name of the output NDF without the filename
         ONAM( 1 ) = 'coadded_b'//BANDST

      END IF

*  Construct the title of the output NDF.
      TITLE = 'IRAS90 COADDCRDD: '//ONAM( 1 )

*  If not in automatic mode, using the constructed name and title as
*  prompt defaults and get a name and a title from the environment.
      IF ( .NOT.AUTO ) THEN

*  Set first attempt at defining a name as default name
         DEFNAM = ONAM( 1 )
100      CONTINUE                    ! Return point for invalid file name

*  Obtain file name from the user
         CALL MSG_OUT('COADDCRDD_MSG',
     :   'Please enter actual file name not a group expression',
     :   STATUS )
         CALL PAR_DEF0C( POUT, DEFNAM, STATUS )
         CALL PAR_GET0C( POUT, ONAM( 1 ), STATUS )
         CALL PAR_CANCL( POUT, STATUS )

*  Check whether the file name is valid
         NAMEFL = CHR_ISNAM( ONAM( 1 ) )
         IF ( ( .NOT. NAMEFL ) .OR. ( STATUS .EQ. PAR__NULL ) ) THEN
            IF ( STATUS .EQ. PAR__NULL ) CALL ERR_FLUSH( STATUS )
            CALL MSG_OUT ('COADDCRDD_MSG',
     :      'Name entered is not a valid filename, please try again',
     :      STATUS )
            GOTO 100

*  Check status
         ELSE IF ( STATUS .NE. SAI__OK) THEN
            RETURN

         ELSE

*  The name entered by the user is a valid filename
*  Create default title from the file name
            TITLE = 'IRAS90 COADDCRDD: '//ONAM( 1 )

*  Obtain title from user
            CALL PAR_DEF0C( PTITL, TITLE, STATUS )
            CALL PAR_GET0C( PTITL, TITLE, STATUS )
            CALL PAR_CANCL( PTITL, STATUS )

*  Check status
            IF ( STATUS .EQ. PAR__NULL ) GOTO 100
            IF ( STATUS .NE. SAI__OK ) RETURN

         END IF
      END IF
      NAMELN = CHR_LEN( ONAM( 1 ) )

*  Put the name of the output NDF in the group.
      CALL GRP_PUT( OGID, 1, ONAM, 0, STATUS )

*  Get the index of the name just appended to the group.
      CALL GRP_GRPSZ( OGID, NINDX, STATUS )

*  Create a new NDF with the given name and the IRAS extension of the
*  given NDF.
      CLIST = 'DATA,UNITS,NOTITLE,NOHISTORY,NOEXTENSION(),'/
     :       /'EXTENSION(IRAS)'
      CALL NDG_NDFPR( NDF, CLIST, OGID, NINDX, OUTNDF, STATUS )

*  Modify the reference RA and Dec - if the reference position is given in an
*  input NDF the same position will overwrite the template reference position,
*  if the reference position was given by the user, this will be entered as the
*  reference position.
*  Obtain access to the IRAS extension via loc1
      CALL NDF_XLOC( OUTNDF, 'IRAS', 'UPDATE', LOC1, STATUS )

*  Find CRDD_INFO component structure
      CALL DAT_FIND( LOC1, 'CRDD_INFO', LOC2, STATUS )

*  Write a new value to the ref_ra and ref_dec components of crdd_info
      CALL CMP_PUT0D( LOC2, 'REF_RA', RA, STATUS )
      CALL CMP_PUT0D( LOC2, 'REF_DEC', DEC, STATUS )

*  Assign the new title to the created NDF.
      CALL NDF_CPUT( TITLE, OUTNDF, 'Title', STATUS )

*  Get the bound of the created NDF.
      CALL NDF_BOUND( OUTNDF, 2, LBND, UBND, NDIM, STATUS )

*  Since sample value of the original NDF will not be retained, reset
*  the data component of the NDF.
      CALL NDF_RESET( OUTNDF, 'Data', STATUS )

*  Change the second dimension of the NDF so that it only contains the
*  specified trace.
      LBND( 2 ) = INDX
      UBND( 2 ) = INDX
      CALL NDF_SBND( 2, LBND, UBND, OUTNDF, STATUS )

*  Write a entry to the logging file.
      IF ( BAND .EQ. 1 ) THEN
         WVBND = ' 12 um'
      ELSE IF ( BAND .EQ. 2 ) THEN
         WVBND = ' 25 um'
      ELSE IF ( BAND .EQ. 3 ) THEN
         WVBND = ' 60 um'
      ELSE IF ( BAND .EQ. 4 ) THEN
         WVBND = '100 um'
      END IF
      CALL FIO_WRITE( FID, '       NDF file '//ONAM( 1 )( : NAMELN )/
     :             /' is created to contain the coadded crossing(s)'/
     :             /' from waveband '//WVBND, STATUS )
      CALL FIO_WRITE( FID, ' ', STATUS )

*  Write a message to the users.
      CALL MSG_BLANK( STATUS )
      CALL MSG_SETC( 'SRNM', NAME )
      CALL MSG_SETC( 'FLNM', ONAM( 1 ) )
      CALL MSG_SETC( 'WAVE', WVBND )
      CALL MSG_OUT( 'CDONDF_MSG1', 'NDF file ^FLNM is created to '/
     :             /'contain the coadded crossing(s), from waveband '/
     :             /'^WAVE, of the expected source ^SRNM.', STATUS )
      CALL MSG_BLANK( STATUS )
      END
