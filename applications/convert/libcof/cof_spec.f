      SUBROUTINE COF_SPEC( FUNIT, NAME, STATUS )
*+
*  Name:
*     COF_SPEC

*  Purpose:
*     Determines if a FITS file is one of a special set.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_SPEC( FUNIT, NDF, STATUS )

*  Description:
*     This routine determines if a FITS file belongs to a special set,
*     requiring specialised processing.  These are usually from
*     space-borne observatories.  The classification is determined by
*     searching the FITS headers for the presence of certain keywords
*     and values.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the special type of FITS file.  The calling routine
*        should allow at least 7 characters for this.  A value of ' ' is
*        returned when the FITS file does not belong to any of the
*        special data sources.  See the "Notes" for details of the
*        supported special FITS files.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The current header and data unit must be the primary one.  The
*     routine aborts with an error status if this requirement is not
*     satisfied.

*  Notes:
*     The supported values of NAME are as follows.
*     -  'SWSAA'    ISO, SWS instrument, auto-analysis product (SWAA)
*     -  'LWSAA'    ISO, LWS instrument, auto-analysis product (LSAN)
*     -  'CAMAA'    ISO, CAM instrument, auto-analysis products (CMxx)
*     -  'IUELI'    IUE, LILO or LIHI linearised flag image products
*     -  'IUEMX'    IUE, MXLO extracted low-dispersion spectra product
*     -  'IUERI'    IUE, RILO or RIHI raw-image products 
*     -  'IUESI'    IUE, SILO or SIHI resampled-image products
*     -  'IUEVD'    IUE, VDLO or VDHI vector-displacement products

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 April 4 (MJC):
*        Original version.
*     1996 June 29 (MJC):
*        Added CAM, and IUE formats.
*     1997 March 3 (MJC):
*        Added 2dF.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants

*  Arguments Given:
      INTEGER FUNIT

*  Arguments Returned:
      CHARACTER * ( * ) NAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      INTEGER CPOS               ! Character position
      CHARACTER * ( 70 ) FILNAM  ! Value of FILENAME keyword
      LOGICAL FILPRE             ! FILENAME keyword is present?
      INTEGER FSTAT              ! FITSIO status
      INTEGER HDUTYP             ! HDU type (primary, IMAGE, ASCII or
                                 ! binary table)
      LOGICAL INSPRE             ! INSTRUME keyword is present?
      CHARACTER * ( 20 ) INSTRU  ! Value of INSTRUME keyword
      INTEGER NHDU               ! Number of the current HDU
      LOGICAL ORIPRE             ! ORIGIN keyword is present?
      CHARACTER * ( 20 ) ORIGIN  ! Value of ORIGIN keyword
      LOGICAL TELPRE             ! TELESCOP keyword is present?
      CHARACTER * ( 20 ) TELESC  ! Value of TELESCOP keyword

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned value.
      NAME = ' '

*  Check that the current HDU is the primary or IMAGE extension.
*  At present there is no inquiry routine, so inquire the number of
*  the HDU.  1 is the primary HDU.
      CALL FTGHDN( FUNIT, NHDU )
      IF ( NHDU .GT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_SPEC',
     :     'Current header and data unit is not primary.  Therefore, '/
     :     /'cannot determine if the FITS file belongs to one of the '/
     :     /'special cases.', STATUS )
         GOTO 999
      END IF

*  Obtain values of relevant keywords.
*  ===================================

*  Obtain the origin from the ORIGIN keyword in the header.
      CALL COF_GKEYC( FUNIT, 'ORIGIN', ORIPRE, ORIGIN, COMENT, STATUS )

*  Obtain the telescope name from the TELESCOP keyword in the header.
      CALL COF_GKEYC( FUNIT, 'TELESCOP', TELPRE, TELESC, COMENT,
     :                STATUS )

*  Obtain the instrument name from the INSTRUME keyword in the header.
      CALL COF_GKEYC( FUNIT, 'INSTRUME', INSPRE, INSTRU, COMENT,
     :                STATUS )

*  Obtain the filename from the FILENAME keyword in the header.
      CALL COF_GKEYC( FUNIT, 'FILENAME', FILPRE, FILNAM, COMENT,
     :                STATUS )

*  Test for ISO data.
*  ==================
      IF ( ORIPRE .AND. ORIGIN .EQ. 'ESA' .AND.
     :     TELPRE .AND. TELESC .EQ. 'ISO' .AND. FILPRE  ) THEN

*  Deal with the individual cases.
         IF ( FILNAM( 1:4 ) .EQ. 'SWAA' ) THEN
            NAME = 'SWSAA'

         ELSE IF ( FILNAM( 1:4 ) .EQ. 'LSAN' ) THEN
            NAME = 'LWSAA'

         ELSE IF ( FILNAM( 1:2 ) .EQ. 'CM' ) THEN
            NAME = 'CAMAA'

         END IF

*  Test for IUE data.
*  ==================
      ELSE IF ( TELPRE .AND. TELESC .EQ. 'IUE' .AND. FILPRE  ) THEN

*  Locate the fullstoip in the filename.
         CPOS = INDEX( FILNAM, '.' )
         IF ( CPOS .GT. 0 ) THEN

*  Deal with the individual cases.
            IF ( FILNAM( CPOS + 1:CPOS + 2 ) .EQ. 'LI' ) THEN
               NAME = 'IUELI'

            ELSE IF ( FILNAM( CPOS + 1:CPOS + 2 ) .EQ. 'MX' ) THEN
               NAME = 'IUEMX'

            ELSE IF ( FILNAM( CPOS + 1:CPOS + 2 ) .EQ. 'RI' ) THEN
               NAME = 'IUERI'

            ELSE IF ( FILNAM( CPOS + 1:CPOS + 2 ) .EQ. 'SI' ) THEN
               NAME = 'IUESI'

            ELSE IF ( FILNAM( CPOS + 1:CPOS + 2 ) .EQ. 'VD' ) THEN
               NAME = 'IUEVD'

            END IF
         END IF

*  Test for IUE data without the filename.
*  =======================================

*  This occurs when the primary header contains only ancillary data and
*  no primary array, i.e. an extension contains the data.
      ELSE IF ( TELPRE .AND. TELESC .EQ. 'IUE' ) THEN

*  Skip forward one HDU.
         CALL FTMRHD( FUNIT, +1, HDUTYP, FSTAT )

*  Only proceed if there there was no error.
         IF ( FSTAT .EQ. FITSOK ) THEN
            
*  Obtain the filename from the FILENAME keyword in the header.
            CALL COF_GKEYC( FUNIT, 'FILENAME', FILPRE, FILNAM, COMENT,
     :                      STATUS )

*  Locate the fullstop in the filename.
            CPOS = INDEX( FILNAM, '.' )
            IF ( CPOS .GT. 0 ) THEN

*  Deal with the individual cases.  Only MX is known not to have its
*  filename in the primary header.
               IF ( FILNAM( CPOS + 1:CPOS + 2 ) .EQ. 'MX' ) THEN
                  NAME = 'IUEMX'

               END IF
            END IF
         END IF

*  Ensure we are located in the primary HDU.
         CALL FTMAHD( FUNIT, 1, HDUTYP, FSTAT )

*  Test for AAO data.
*  ==================
      ELSE IF ( TELPRE .AND.
     :          TELESC .EQ. 'Anglo-Australian Tel' ) THEN
         CALL CHR_UCASE( INSTRU )
         IF ( INSPRE .AND. ( INSTRU .EQ. 'CCD_1' .OR.
     :        INSTRU .EQ. '2DF' ) ) THEN
            NAME = 'AAO2DF'
         END IF

      END IF

  999 CONTINUE

      END
