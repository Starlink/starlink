      SUBROUTINE COF_2DFEX( SNAME, LOC, FUNIT, STATUS )
*+
*  Name:
*     COF_2DFEX

*  Purpose:
*     Exports 2dF FIELD extension in an NDF to a FITS binary table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_2DFEX( SNAME, LOC, FUNIT, STATUS )

*  Description:
*     This routine converts a 2dF FIELD extension witihn an NDF into
*     a binary table within the current FITS file.
*
*     To propagate the OBJECT substructure this routine creates a binary
*     table of constant width (210 bytes) with one row per fibre.  The
*     total number of rows is obtained from component NUM_FIBRES.  If a
*     possible OBJECT component is missing from the NDF, a null column
*     is written for that component.  The columns inherit the data
*     types of the OBJECT structure's components.  Column meanings and
*     units are assigned based upon information in the reference given
*     below.
*
*     The FIELD structure components are converted into additional
*     keywords of the same name in the binary-table header, with the
*     exception that components with names longer than 8 characters
*     have abbreviated keywords: UNALLOCxxx become UNAL-xxx (xxx= OBJ,
*     GUI, or SKY), CONFIGMJD becomes CONFMJD, and xSWITCHOFF become
*     xSWTCHOF (x=X or Y).  If any FIELD component is missing it is
*     ignored.
*
*     Extension-level and name keywords are written.

*  Arguments:
*     SNAME = CHARACTER * ( * ) (Given)
*        The name of structure.  It is used to form the EXTNAME
*        keyword.
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator to the 2dF extension structure whose contents are to
*        be converted to binary tables.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior requirements:
*     -  A primary HDU unit exists in the FITS file, and the file is
*     open.

*  References:
*     Bailey, J.A. 1996, 2dF Software Report 14, version 0.3.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 January 14 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      CHARACTER * ( * ) SNAME
      CHARACTER * ( * ) LOC
      INTEGER FUNIT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Effective string length

*  Local Constants:
      INTEGER   FITSOK           ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

      INTEGER MXCOMP             ! Maximum number of components in the
                                 ! OBJECT structure or fields in the
                                 ! binary table
      PARAMETER ( MXCOMP = 13 )

*  Local Variables:
      CHARACTER * ( 80 ) BUFFER  ! BUFFER for writing error messages
      CHARACTER * ( 80 ) CDUMMY  ! Dummy for reading TFORMn cards
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to a structure component
      CHARACTER * ( 3 ) CN       ! Column number
      CHARACTER * ( DAT__SZNAM ) CNAME ! Component name
      CHARACTER * ( DAT__SZNAM ) CTYPE ! Component type
      CHARACTER * ( 8 ) CRDNAM   ! Header-card name to insert TNULLn
      CHARACTER * 80 CVALUE      ! Character value
      DOUBLE PRECISION DVALUE    ! D.p. value
      INTEGER EL                 ! Number of elements in mapped array
      CHARACTER * ( 68 ) EXTNAM  ! Name of the component
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to FIELD structure
      CHARACTER * ( 256 ) FILE   ! Name of the HDS file (not used)
      INTEGER FSTAT              ! FITSIO status
      INTEGER I                  ! Loop through fields
      INTEGER IC                 ! Loop counter
      INTEGER IVALUE             ! Integer value
      CHARACTER * ( 8 ) KEYWRD   ! FITS keyword
      INTEGER LEL                ! Loop counter for filling column with
                                 ! null values
      INTEGER NC                 ! Number of characters
      INTEGER NCOMP              ! Number of components
      INTEGER NFIELD             ! Number of fields in table
      INTEGER NLEV               ! Number of hierarchical levels (not
                                 ! used)
      INTEGER NROW               ! Number of rows in table
      CHARACTER * 1 NULL         ! ASCII null character
      CHARACTER * ( DAT__SZLOC ) OLOC ! Locator to OBJECT structure
      INTEGER OPNTR              ! Pointer to mapped component
      CHARACTER * ( 6 ) ROUTIN   ! Name of the FITSIO routine used to
                                 ! copy data into the binary table
      INTEGER STRLEN             ! Number of characters in component
                                 ! element
      CHARACTER * ( 32 ) TDESC( MXCOMP ) ! Binary-table field
                                 ! descriptions
      CHARACTER * ( 3 ) TFORM( MXCOMP ) ! Binary-table TFORMn
      LOGICAL THERE              ! Component is present?
      CHARACTER * ( 13 ) TTYPE( MXCOMP ) ! Binary-table field names
      CHARACTER * ( 13 ) TUNIT( MXCOMP ) ! Binary-table units

*  Local Data:
      DATA TDESC / 'Object name', 'J2000 mean Right Ascension',
     :             'J2000 mean Declination', 'X position', 'Y position',
     :             'Button angle', 'P (programme object) or S (sky)',
     :             'Priority', 'Object magnitude',
     :             'Programme identification', 'Comments',
     :             'A or B if beam switching',
     :             'Beam-switch partner fibre' /

      DATA TFORM / '80A', '1D', '1D', '1J', '1J', '1D', '1A',
     :             '1I', '1D', '1J', '80A', '1A', '1I' /

      DATA TTYPE / 'NAME', 'RA', 'DEC', 'X', 'Y', 'THETA', 'TYPE',
     :             'PRIORITY', 'MAGNITUDE', 'PID', 'COMMENT',
     :             'SWITCHFIELD', 'SWITCHPARTNER' /

      DATA TUNIT / ' ', 'radians', 'radians', 'microns', 'microns',
     :             'radians', ' ', ' ', 'magnitudes', ' ', ' ', ' ',
     :             ' ' /

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Define the null character.
      NULL = CHAR( 0 )

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Define the shape of the binary table.
*  =====================================

*  Get the value of NUM_FIBRES scalar primitive object.
      CALL CMP_GET0I( LOC, 'NUM_FIBRES', NROW, STATUS )

*  Create the binary table.
*  ========================

*  Create new header and data section.
      CALL FTCRHD( FUNIT, FSTAT )

*  Get the structure's path name and assign it to the extension name.
      CALL HDS_TRACE( LOC, NLEV, EXTNAM, FILE, STATUS )

*  Create binary-table header.  There are NROW rows in the table.
*  The "variable-length data area" has length of 0 bytes.
      CALL FTPHBN( FUNIT, NROW, MXCOMP, TTYPE, TFORM, TUNIT, EXTNAM,
     :             0, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_2DFEX_ERR1', 'FTPHBN',
     :                   'Error writing binary-table header.', STATUS )
         GOTO 999
      END IF

*  Replace the default comments with the 2dF descriptions.
*  =======================================================
      DO I = 1, MXCOMP

*  The comments appear on the TTYPEn cards.  Form the TTYPEn keyword,
*  and replace the comment.
         CALL FTKEYN( 'TTYPE', I, CRDNAM, FSTAT )
         CALL FTMCOM( FUNIT, CRDNAM, TDESC( I ), FSTAT )

*  Write the TNULLn cards for an integer column.
*  =============================================

*  Convert the column number into character form.
         CALL CHR_ITOC( I, CN, NC )

*  Form the name of the keyword which will immediately precede the
*  inserted TNULLn card.
         NC = 5
         CRDNAM = 'TFORM'
         CALL CHR_APPND( CN, CRDNAM, NC )

*  FITSIO does not permit cards to be placed after a named card; 
*  it requires that we read that named card first.
         CALL FTGCRD( FUNIT, CRDNAM, CDUMMY, FSTAT )

*  Assign the bad/null value for each of the columns.
*  --------------------------------------------------

*  Form the name of the TNULLn keyword.
         NC = 5
         CRDNAM = 'TNULL'
         CALL CHR_APPND( CN, CRDNAM, NC )

*  Process by data type.
         IF ( I .EQ. 1 .OR. I .EQ. 7 .OR.
     :        I .EQ. 11 .OR. I .EQ. 12 ) THEN

*  Insert the TNULLn card.
            CALL FTIKYS( FUNIT, CRDNAM, ' ', 'String null value',
     :                   FSTAT )

         ELSE IF ( I .EQ. 2 .OR. I .EQ. 3 .OR.
     :             I .EQ. 6 .OR. I .EQ. 9 ) THEN

*  Insert the TNULLn card.
            CALL FTIKYS( FUNIT, CRDNAM, -999.0D0, 0, 'Null value',
     :                   FSTAT )

         ELSE IF ( I .EQ. 4 .OR. I .EQ. 5 .OR. I .EQ. 10 ) THEN

*  Insert the TNULLn card.
            CALL FTIKYJ( FUNIT, CRDNAM, VAL__BADI, 'Null value', FSTAT )

         ELSE IF ( I .EQ. 8 .OR. I .EQ. 13 ) THEN

*  Insert the TNULLn card.
            CALL FTIKYJ( FUNIT, CRDNAM, VAL__BADW, 'Null value', FSTAT )
         END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
         IF ( FSTAT .GT. FITSOK ) THEN
            BUFFER = 'Error writing '//CRDNAM( :NC )//' card for a '/
     :               /'binary table.'
            CALL COF_FIOER( FSTAT, 'COF_2DFEX_ERR3', 'FTIKYx',
     :                      BUFFER, STATUS )
            GOTO 999
         END IF
      END DO

*  Write extension-level header card.
*  ==================================

*  Set the extension level.
      CALL FTPKYJ( FUNIT, 'EXTLEVEL', 1, 'Level in the '/
     :             /'hierarchical structure', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_2DFEX_ERR2', 'FTPKYJ',
     :     'Error writing extension level in header.', STATUS )
         GOTO 999
      END IF

*  Write additional header cards based upon the OBJECT structure.
*  ==============================================================

*  Get a locator to the OBJECT structure.
      CALL DAT_FIND( LOC, 'OBJECT', OLOC, STATUS )

*  Determine how many components it contains.  This will be the number
*  of fields in the binary table.
      CALL DAT_NCOMP( OLOC, NFIELD, STATUS )

*  FIELD structure
*  ===============
*
*  The scalar primitive components of this structure are to be made
*  into FITS headers located in the binary table, using their names as
*  keywords, where that is possible.
*  
*  Get a locator to the FIELD structure.
      CALL DAT_FIND( LOC, 'FIELD', FLOC, STATUS )

*  Determine how many components it contains.
      CALL DAT_NCOMP( FLOC, NCOMP, STATUS )

*  Loop for each component.
      DO IC = 1, NCOMP

*  Get a locator to the object.
         CALL DAT_INDEX( FLOC, IC, CLOC, STATUS )

*  Determine its name.  Define the FITS keyword, shortening long names
*  where necessary.
         CALL DAT_NAME( CLOC, CNAME, STATUS )
         IF ( CNAME( 1:7 ) .EQ. 'UNALLOC' ) THEN
            KEYWRD = 'UNAL-'//CNAME( 8:10 )
         ELSE IF ( CNAME .EQ. 'CONFIGMJD' ) THEN
            KEYWRD = 'CONFMJD'
         ELSE IF ( CNAME( 2:13 ) .EQ. 'SWITCHOFFSET' ) THEN
            KEYWRD = CNAME( 1:1 )//'SWTCHOF'
         ELSE
            KEYWRD = CNAME( 1:8 )
         END IF

*  Obtain the component's value using the documented data type for the
*  keyword; and write out a FITS header card using the keyword, and
*  the value.  As the number of decimal places (for floating-point
*  values) and the comment depends on the component, the code tests for
*  each recognised object.

*  CENRA
         IF ( KEYWRD .EQ. 'CENRA' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Field centre mean Right Ascension (Radians)',
     :                   FSTAT )

*  CENDEC
         ELSE IF ( KEYWRD .EQ. 'CENDEC' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Field centre mean Declination (Radians)',
     :                   FSTAT )

*  CENEQNX
         ELSE IF ( KEYWRD .EQ. 'CENEQNX' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, 2,
     :                   'Equinox of Field Centre (FK5 Julian)', FSTAT )


*  APPRA
         ELSE IF ( KEYWRD .EQ. 'APPRA' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Apparent Right Ascension (Radians)', FSTAT )

*  APPDEC
         ELSE IF ( KEYWRD .EQ. 'APPDEC' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, VAL__SZD - 5,
     :                   'Apparent Declination (Radians)', FSTAT )

*  APPEPOCH
         ELSE IF ( KEYWRD .EQ. 'APPEPOCH' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, 6,
     :                   'Epoch of Apparent co-ordinates', FSTAT )

*  CONFIGMJD
         ELSE IF ( KEYWRD .EQ. 'CONFMJD' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, 6,
     :                   'MJD configured for', FSTAT )

*  ACTMJD
         ELSE IF ( KEYWRD .EQ. 'ACTMJD' ) THEN
            CALL DAT_GET0D( CLOC, DVALUE, STATUS )
            CALL FTPKYD( FUNIT, KEYWRD, DVALUE, 3,
     :                   'Actual MJD of observation', FSTAT )

*  UNALLOCOBJ
         ELSE IF ( KEYWRD .EQ. 'UNAL-OBJ' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE, 
     :                   'Number of unallocated objects', FSTAT )

*  UNALLOCGUI
         ELSE IF ( KEYWRD .EQ. 'UNAL-GUI' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE, 
     :                   'Number of unallocated guide stars', FSTAT )

*  UNALLOCSKY
         ELSE IF ( KEYWRD .EQ. 'UNAL-SKY' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE, 
     :                   'Number of unallocated skies', FSTAT )

*  ALLOCOBJ
         ELSE IF ( KEYWRD .EQ. 'ALLOCOBJ' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE, 
     :                   'Number of allocated objects', FSTAT )

*  ALLOCGUI
         ELSE IF ( KEYWRD .EQ. 'ALLOCGUI' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE, 
     :                   'Number of allocated guide stars', FSTAT )

*  ALLOCSKY
         ELSE IF ( KEYWRD .EQ. 'ALLOCSKY' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE, 
     :                   'Number of unallocated skies', FSTAT )

*  XSWITCHOFFSET
         ELSE IF ( KEYWRD .EQ. 'XSWTCHOF' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE, 
     :                   'Beam switch offset X (microns)', FSTAT )

*  YSWITCHOFFSET
         ELSE IF ( KEYWRD .EQ. 'YSWTCHOF' ) THEN
            CALL DAT_GET0I( CLOC, IVALUE, STATUS )
            CALL FTPKYJ( FUNIT, KEYWRD, IVALUE, 
     :                   'Beam switch offset Y (microns)', FSTAT )

*  The 2dF software seems to put in ASCII null characters perhaps due to
*  some C usage.  These must be replaced with a blank.  Find the
*  effective length up to a maximum that will fit into the header
*  allowing for the comment and standard header syntax for character
*  values.

*  PROGID
         ELSE IF ( KEYWRD .EQ. 'PROGID' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :41 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ),
     :                   'Programme Identification', FSTAT )

*  MODE
         ELSE IF ( KEYWRD .EQ. 'MODE' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :43 ) )
            CALL FTPKYS( FUNIT, KEYWRD, CVALUE( :NC ),
     :                   'Mode (SWITCH or NORMAL)', FSTAT )

*  LABEL
         ELSE IF ( KEYWRD .EQ. 'LABEL' ) THEN
            CALL DAT_GET0C( CLOC, CVALUE, STATUS )
            CALL CHR_TRCHR( NULL, ' ', CVALUE, STATUS )
            NC = CHR_LEN( CVALUE( :64 ) )
            CALL FTPKYS( FUNIT, 'LABEL', CVALUE( :NC ), ' ', FSTAT )

         END IF

*  Tidy the locator to the object.
         CALL DAT_ANNUL( CLOC, STATUS )

      END DO

*  Free the locator to the FIELD structure.
      CALL DAT_ANNUL( FLOC, STATUS )

*  Write components into table columns.
*  ====================================

*  At this point all the headers have been written.  Now it's time
*  to create the columns.  Deal with each column in turn.  If the
*  component is present, map it and transfer the data to the
*  binary-table column; if not, just write a column of null values.
      DO I = 1, MXCOMP

*  Search for the object.
         CALL DAT_THERE( OLOC, TTYPE( I ), THERE, STATUS )

*  Obtain a locator to the object and determine its type.
         IF ( THERE ) THEN
            CALL DAT_FIND( OLOC, TTYPE( I ), CLOC, STATUS )
            CALL DAT_TYPE( CLOC, CTYPE, STATUS )
         END IF

*  Process by data type.  Start with the character arrays.
*  -------------------------------------------------------
         IF ( I .EQ. 1 .OR. I .EQ. 7 .OR.
     :        I .EQ. 11 .OR. I .EQ. 12 ) THEN

            IF ( THERE ) THEN

*  Obtain the length in characters of the component and map its values.
                CALL DAT_LEN( CLOC, STRLEN, STATUS )
                CALL DAT_MAPV( CLOC, CTYPE, 'READ', OPNTR, EL, STATUS )
*                CALL CON_TRCHA( NULL, ' ', EL, %VAL( OPNTR ), STATUS,
*     :                          %VAL( STRLEN ) )

*  Transfer the values to the binary-table column.
                CALL FTPCLS( FUNIT, I, 1, 1, EL, %VAL( OPNTR ),
     :                       FSTAT, %VAL( STRLEN ) )

*  Unmap the component.
                CALL DAT_UNMAP( CLOC, STATUS )

*  There is no null character value, so it is set to be blank by
*  convention.  Since this is really a fault in the NDF, the user can
*  suffer a little by calling the FITSIO routine for every element,
*  instead of getting workspace and filling it with blank values.
             ELSE
                CVALUE = ' '
                DO LEL = 1, EL
                   CALL FTPCLS( FUNIT, I, LEL, 1, 1,
     :                          CVALUE( 1:STRLEN ), FSTAT )
                END DO
             END IF
             ROUTIN = 'FTPCLS'

*  Double-precision components
*  ---------------------------
         ELSE IF ( I .EQ. 2 .OR. I .EQ. 3 .OR.
     :             I .EQ. 6 .OR. I .EQ. 9 ) THEN

            IF ( THERE ) THEN

*  Map the values of the component.
                CALL DAT_MAPV( CLOC, CTYPE, 'READ', OPNTR, EL, STATUS )

*  Transfer the values to the binary-table column.
                CALL FTPCLD( FUNIT, I, 1, 1, EL, %VAL( OPNTR ), FSTAT )

*  Unmap the component.
                CALL DAT_UNMAP( CLOC, STATUS )

*  Fill the array by calling the FITSIO routine for every element,
*  instead of getting workspace and filling it with a constant.
             ELSE
                DO LEL = 1, EL
                   CALL FTPCLD( FUNIT, I, LEL, 1, 1, -999.0D0, FSTAT )
                END DO
             END IF
             ROUTIN = 'FTPCLD'

*  Integer components
*  ------------------
         ELSE IF ( I .EQ. 4 .OR. I .EQ. 5 .OR. I .EQ. 10 ) THEN

            IF ( THERE ) THEN

*  Map the values of the component.
                CALL DAT_MAPV( CLOC, CTYPE, 'READ', OPNTR, EL, STATUS )

*  Transfer the values to the binary-table column.
                CALL FTPCLJ( FUNIT, I, 1, 1, EL, %VAL( OPNTR ), FSTAT )

*  Unmap the component.
                CALL DAT_UNMAP( CLOC, STATUS )

*  Fill the array by calling the FITSIO routine for every element,
*  instead of getting workspace and filling it with a constant.
             ELSE
                DO LEL = 1, EL
                   CALL FTPCLJ( FUNIT, I, LEL, 1, 1, VAL__BADI, FSTAT )
                END DO
             END IF
             ROUTIN = 'FTPCLJ'

*  Word components
*  ---------------
         ELSE IF ( I .EQ. 8 .OR. I .EQ. 13 ) THEN

            IF ( THERE ) THEN

*  Map the values of the component.
                CALL DAT_MAPV( CLOC, CTYPE, 'READ', OPNTR, EL, STATUS )

*  Transfer the values to the binary-table column.
                CALL FTPCLI( FUNIT, I, 1, 1, EL, %VAL( OPNTR ), FSTAT )

*  Unmap the component.
                CALL DAT_UNMAP( CLOC, STATUS )

*  Fill the array by calling the FITSIO routine for every element,
*  instead of getting workspace and filling it with a constant.
             ELSE
                DO LEL = 1, EL
                   CALL FTPCLI( FUNIT, I, LEL, 1, 1, VAL__BADW, FSTAT )
                END DO
             END IF
             ROUTIN = 'FTPCLI'

         END IF

*  Tidy the locator to the component.
         IF ( THERE ) CALL DAT_ANNUL( CLOC, STATUS )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  Specify from which routine the error arose.
         IF ( FSTAT .GT. FITSOK ) THEN
            NC = CHR_LEN( TTYPE( I ) )
            BUFFER = 'Error copying vector component '/
     :               /TTYPE( I )( :NC )//' to the binary table.'
            CALL COF_FIOER( FSTAT, 'COF_2DFEX_ERR4', ROUTIN, BUFFER,
     :                      STATUS )
            GOTO 999
         END IF
      END DO

*  Tidy the locator to the OBJECT structure.
      CALL DAT_ANNUL( OLOC, STATUS )

  999 CONTINUE

      END
