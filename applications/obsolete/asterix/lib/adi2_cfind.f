      SUBROUTINE ADI2_CFIND( FITID, EXTNAM, CNAME, SNAME, CREATE,
     :                       DELETE, TYPE, NDIM, DIMS,
     :                       DIDCRE, CACHEID, STATUS )
*+
*  Name:
*     ADI2_CFIND

*  Purpose:
*     Locate/create/delete FITS component for a given item

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_CFIND( FITID, EXTNAM, CNAME, SNAME, CREATE, DELETE, TYPE,
*                      NDIM, DIMS, DIDCRE, CACHEID, STATUS )

*  Description:
*     Given a fully specified FITS component we attempt to either locate,
*     create and locate, or delete the ADI cache object associated with
*     the component. The component is specified using 3 strings, and this
*     determines nature of the cache object located/created/destroyed. The
*     rules are given by this table.
*
*     To summarise these rules here is a complete listing of the allowed
*     combinations of these 3 strings,
*
*       EXTNAM  CNAME   SNAME         Cache obj type    Meaning
*
*                                     file, hdu, key    Object named in the
*                                       value           file open call
*       <name>                        hdu               Whole named HDU
*       <name>  @                     value             HDU image data
*       <name>  .<key>                key               Whole named keyword
*       <name>  .<key>  Value         value             Keyword value scalar
*       <name>  .<key>  Comm          value             Keyword value string
*       <name>  |<col>                column            Whole named column
*       <name>  |<col>  <name>        key               Keyword T<name><N>
*       <name>  |<col>  <name>.Value  value             Key T<name><N> value
*       <name>  |<col>  <name>.Comm   value             Key T<name><N> comment
*       <name>  ,<num>                value             The <num>'th comment
*       <name>  #<num>                value             The <num>'th history

*  Arguments:
*     FITID = INTEGER (given)
*        The FITS object containing the component we're interested in
*     EXTNAM = CHARACTER*(*) (given)
*        The name of the extension
*     CNAME = CHARACTER*(*) (given)
*        The name of the extension sub-component (see above)
*     SNAME = CHARACTER*(*) (given)
*        The name of the extension sub-component component (see above)
*     CREATE = LOGICAL (given)
*        Create component if it doesn't exist?
*     DELETE = LOGICAL (given)
*        Delete component if it exists?
*     TYPE = CHARACTER*(*) (given)
*        The type of the component if we have to create it
*     NDIM = INTEGER (given)
*        The dimensionality of the component if we have to create it
*     DIMS[] = INTEGER (given)
*        The dimensions of the component if we have to create it
*     DIDCRE = LOGICAL (returned)
*        Did we create the object?
*     CACHEID = INTEGER (returned)
*        Identifier of FITS cache object. If we are locating and it doesn't
*        exist, set to ADI__NULLID
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			FITID, NDIM, DIMS(*)
      CHARACTER*(*)		EXTNAM, CNAME, SNAME, TYPE
      LOGICAL			CREATE, DELETE

*  Arguments Returned:
      LOGICAL			DIDCRE
      INTEGER                   CACHEID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL 			CHR_LEN
        INTEGER			CHR_LEN

*  Local Constants:
      INTEGER			MAXASC
        PARAMETER		( MAXASC = 4 )

*  Local Variables:
      CHARACTER*30		LHDU			! Local HDU name copy

      INTEGER			ASC(MAXASC)		! Ascendants
      INTEGER			HDUID			! HDU identifier
      INTEGER		        HLEN			! Length of LHDU
      INTEGER			I			! Loop over dimensions
      INTEGER			IASC			! Loop over ascendants
      INTEGER			IMID			! Image identifier
      INTEGER			KEYID			! Keyword identifier
      INTEGER			NASC			! # ascendants

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise return value
      CACHEID = ADI__NULLID
      DIDCRE = .FALSE.

*  No ascendants initially
      NASC = 0

*  Import the HDU name
*  primary HDU
      IF ( EXTNAM(1:1) .EQ. ' ' ) THEN
        LHDU = 'PRIMARY'
        HLEN = 7
      ELSE

*    Make local copy of name, and translate embedded spaces to underscores
        LHDU = EXTNAM
        HLEN = CHR_LEN(LHDU)
        IF ( INDEX(LHDU(:HLEN),' ') .GT. 0 ) THEN
          DO I = 1, HLEN
            IF ( LHDU(I:I) .EQ. ' ' ) LHDU(I:I) = '_'
          END DO
        END IF

      END IF

*  Does the HDU exist? (if HDUID=-1 then what !?!)
      CALL ADI2_CFIND_HDU( FITID, LHDU(:HLEN), CREATE, HDUID,
     :                     DIDCRE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      if ( hduid .eq. adi__nullid ) goto 99

*  Did we just create the primary HDU?
      IF ( DIDCRE .AND. (LHDU(:HLEN).EQ.'PRIMARY') ) THEN
        CALL ADI2_CFIND_SCMT( HDUID, STATUS )
      END IF

*  Access to HDU sub-component?
      IF ( CNAME .GT. ' ' ) THEN

*    Add HDU to ascendants list
        NASC = NASC + 1
        ASC(NASC) = HDUID

*   What kind of sub-component?
*    Keyword?
        IF ( CNAME(1:1) .EQ. '.' ) THEN

*      Reinitialise
          DIDCRE = .FALSE.

*      Look for keyword
          CALL ADI2_CFIND_KEY( HDUID, CNAME(2:), CREATE, KEYID,
     :                         DIDCRE, STATUS )

*      Sub-component of keyword?
          IF ( SNAME .GT. ' ' ) THEN

*      Add key to ascendants list
          NASC = NASC + 1
          ASC(NASC) = KEYID

*        Keyword value
            IF ( SNAME .EQ. 'Value' ) THEN
            ELSE IF ( SNAME .EQ. 'Comm' ) THEN
            ELSE
              STATUS = SAI__ERROR
              CALL MSG_SETC( 'C', SNAME )
              CALL MSG_SETC( 'K', CNAME(2:) )
              CALL ERR_REP( ' ', 'Illegal keyword subcomponent '/
     :                  /'specifier /^C/ to keyword ^K', STATUS )

            END IF

*      Delete of keyword wanted?
          ELSE IF ( DELETE ) THEN
            CALL ADI2_SETDEL( KEYID, STATUS )
            CALL ADI_ERASE( KEYID, STATUS )

*      Otherwise whole keyword wanted
          ELSE
            CACHEID = KEYID

          END IF

*    BINTABLE extension column?
        ELSE IF ( CNAME(1:1) .EQ. '|' ) THEN

*    Comment?
        ELSE IF ( CNAME(1:1) .EQ. ',' ) THEN

*    History?
        ELSE IF ( CNAME(1:1) .EQ. '#' ) THEN

*    Image extension data?
        ELSE IF ( CNAME(1:1) .EQ. '@' ) THEN

*      Does image data exist?
          DIDCRE = .FALSE.
          CALL ADI_THERE( HDUID, 'Image', THERE, STATUS )
          IF ( THERE ) THEN

*        Define the extension if write access
            IF ( CREATE ) THEN
              CALL ADI2_DEFIMG( HDUID, TYPE, NDIM, DIMS, .TRUE., IMID,
     :                          STATUS )
            ELSE
              CALL ADI_FIND( HDUID, 'Image', IMID, STATUS )
            END IF

*        Delete?
            IF ( DELETE ) THEN
              CALL ADI2_SETDEL( IMID, STATUS )
              CALL ADI_ERASE( IMID, STATUS )

            ELSE
              CACHEID = IMID
            END IF

*      Create?
          ELSE IF ( CREATE ) THEN

*        Define the extension
            CALL ADI2_DEFIMG( HDUID, TYPE, NDIM, DIMS, .TRUE., IMID,
     :                        STATUS )

*        Set the identifier
            CACHEID = IMID

*        Flag as created
            DIDCRE = .TRUE.

*        Not there and not flagged for create - signal error
          ELSE
            STATUS=SAI__ERROR

          END IF

*    Otherwise bomb
        ELSE
          STATUS = SAI__ERROR

          CALL ERR_REP( ' ', 'Illegal HDU component character "'/
     :                  /CNAME(1:1)//'"', STATUS )
        END IF

*  HDU level operation
      ELSE

*    Create operation? We've already done it
        IF ( HDUID .NE. ADI__NULLID ) THEN

*      Check not marked for delete
          CALL ADI2_CHKDEL( HDUID, STATUS )

*      Set return value
          CACHEID = HDUID

*    Reset delete flag just in case user does a destroy/create operation
        ELSE IF ( CREATE ) THEN

          CALL ADI_CPUT0L( HDUID, 'MarkedForDelete', .FALSE., STATUS )

*    Delete operation?
        ELSE IF ( DELETE ) THEN

          CALL ADI2_SETDEL( HDUID, STATUS )
          CALL ADI_ERASE( HDUID, STATUS )

        END IF

*  End of switch on presence of HDU sub-component
      END IF

*  Whatever the object, if it was accessed for CREATE or DELETE access
*  then it is marked as modified, as are all its ascendants
      IF ( (CREATE.OR.DELETE) .AND. (CACHEID.NE.ADI__NULLID) ) THEN
        CALL ADI_CPUT0L( CACHEID, 'Modified', .TRUE., STATUS )
        DO IASC = 1, NASC
          CALL ADI_CPUT0L( ASC(IASC), 'Modified', .TRUE., STATUS )
        END DO
      END IF

*  Abort point
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_CFIND', STATUS )
      END IF

      END



      SUBROUTINE ADI2_CFIND_HDU( FITID, HDU, CREATE, HDUID, DIDCRE,
     :                           STATUS )
*+
*  Name:
*     ADI2_CFIND_HDU

*  Purpose:
*     Locate/create the named HDU in a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_CFIND_HDU( FITID, HDU, CREATE, HDUID, DIDCRE, STATUS )

*  Description:
*     Locates, creating if required, the named HDU in the specified FITS
*     file.

*  Arguments:
*     FITID = INTEGER (given)
*        The FITS object containing the component we're interested in
*     HDU = CHARACTER*(*) (given)
*        The name of the HDU
*     CREATE = LOGICAL (given)
*        Create component if it doesn't exist?
*     HDUID = INTEGER (returned)
*        Identifier of FITS cache object. If we are locating and it doesn't
*        exist, set to ADI__NULLID
*     DIDCRE = LOGICAL (returned)
*        If CREATE tru on entry tells whether HDU was created
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			FITID
      CHARACTER*(*)		HDU
      LOGICAL			CREATE

*  Arguments Returned:
      LOGICAL			DIDCRE
      INTEGER                   HDUID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER		        CHR_LEN
      EXTERNAL			CHR_SIMLR
        LOGICAL			CHR_SIMLR
      EXTERNAL			ADI2_MKIDX
        CHARACTER*8		ADI2_MKIDX

*  Local Variables:
      CHARACTER*72		CMNT			! Keyword comment
      CHARACTER*6		MODE			! File access mode
      CHARACTER*20		SHDU			! Scanned HDU name

      INTEGER			FSTAT			! FITSIO status code
      INTEGER			HCID			! HDU container
      INTEGER			HDUTAB			! HDU table object
      INTEGER			HDUTYPE			! FITSIO HDU type
      INTEGER			IHDU			! HDU counter
      INTEGER			IMID			! IMAGE data
      INTEGER			LUN			! Logical unit number
      INTEGER			NHDU			! HDU count

      LOGICAL			FOUND			! Found our HDU?
      LOGICAL			MORE			! More HDUs left?
      LOGICAL			SCANNEDALL		! Scanned all HDUs?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      HDUID = ADI__NULLID
      IF ( DIDCRE ) DIDCRE = .FALSE.
      FOUND = .FALSE.

*  Locate the HDU container and the table
      CALL ADI_FIND( FITID, 'Hdus', HCID, STATUS )
      CALL ADI_FIND( HCID, 'HduTable', HDUTAB, STATUS )

*  Does the HDU exist in the container property list?
      CALL ADI2_CFIND_NAM( HDUTAB, HDU, HDUID, STATUS )

*  Not in container...
      IF ( HDUID .EQ. ADI__NULLID ) THEN

*    Scan for the HDU if the file mode is other than write
        CALL ADI_CGET0C( FITID, 'MODE', MODE, STATUS )
        IF ( INDEX( 'Ww', MODE(1:1) ) .EQ. 0 ) THEN

*      Scanned as far as we can already?
          CALL ADI_CGET0L( HCID, 'ScannedAll', SCANNEDALL, STATUS )
          IF ( .NOT. SCANNEDALL ) THEN

*        Get HDU we've scanned up to
            CALL ADI_CGET0I( HCID, 'MaxScan', IHDU, STATUS )

*        Get the file logical unit
            CALL ADI2_GETLUN( FITID, LUN, STATUS )

*        While not found the one we want and more HDUs
            MORE = .TRUE.
            DO WHILE ( MORE .AND. .NOT. FOUND )

*          Try the next HDU
              IHDU = IHDU + 1

*          Try moving to this HDU
              FSTAT = 0
              CALL FTMAHD( LUN, IHDU, HDUTYPE, FSTAT )

*          End of file?
              IF ( FSTAT .EQ. 107 ) THEN
                MORE = .FALSE.

*          Otherwise test if found
              ELSE

*            Create the cache object
                CALL ADI2_CFIND_CREC( HCID, 'Hdu', ' ', 'FITShduCache',
     :                                HDUID, STATUS )

*            Mark as table type
                CALL ADI_CPUT0L( HDUID, 'IsTable',
     :                           ((HDUTYPE.EQ.1).OR.(HDUTYPE.EQ.2)),
     :                           STATUS )

*            Scan the HDU cards
                CALL ADI2_CFIND_SCAN( LUN, HDUID, STATUS )

*            Get name for the HDU
                IF ( IHDU .EQ. 1 ) THEN
                  SHDU = 'PRIMARY'
                ELSE
                  CALL ADI2_HGKYC( HDUID, 'EXTNAME', SHDU, CMNT,
     :                             STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                    CALL ERR_ANNUL( STATUS )
                    SHDU = ADI2_MKIDX( 'HDU_', IHDU )
                  END IF
                END IF
                CALL ADI2_CFIND_SETN( HCID, 'Hdu', HDUID,
     :                                SHDU(:CHR_LEN(SHDU)), STATUS )

*            Does the name match that required?
                IF ( CHR_SIMLR( HDU, SHDU ) ) THEN
                  FOUND = .TRUE.
                ELSE IF ( HDU(1:4) .EQ. 'HDU_' ) THEN
                  FOUND = (SHDU(5:) .EQ. HDU(5:) )
                END IF

              END IF

*        End of scan loop
            END DO

*        Found?
            IF ( FOUND ) THEN

*          Update scan counter
              CALL ADI_CPUT0I( HCID, 'MaxScan', IHDU, STATUS )

*        Not found and no more HDUs?
            ELSE IF ( .NOT. FOUND .AND. .NOT. MORE ) THEN

*          Mark as having scanned inputs
              CALL ADI_CPUT0L( HCID, 'ScannedAll', .TRUE., STATUS )

            END IF

          END IF

*    The mode is write. Check that the PRIMARY hdu has been created unless
*    this *is* the PRIMARY HDU
        ELSE IF ( HDU .NE. 'PRIMARY' ) THEN

*      Get number of HDUs
          CALL ADI_CGET0I( HCID, 'HduCount', NHDU, STATUS )
          IF ( NHDU .EQ. 0 ) THEN

*        Create primary with zero dimensions
            CALL ADI2_CFIND_CREC( HCID, 'Hdu', 'PRIMARY',
     :                            'FITShduCache', HDUID, STATUS )

*        Define IMAGE extension
            CALL ADI2_DEFIMG( HDUID, 'BYTE', 0, 0, .TRUE., IMID,
     :                        STATUS )
            CALL ADI_ERASE( IMID, STATUS )

*        Write standard comments
            CALL ADI2_CFIND_SCMT( HDUID, STATUS )

*        Release primary HDU
            CALL ADI_ERASE( HDUID, STATUS )

          END IF

        END IF

*    HDU not there, but we can create?
        IF ( CREATE .AND. .NOT. FOUND ) THEN

*      Create cache object
          CALL ADI2_CFIND_CREC( HCID, 'Hdu', HDU, 'FITShduCache',
     :                          HDUID, STATUS )

*      Write name of extension (bad news? -rb)
c         IF ( HDU .NE. 'PRIMARY' ) THEN
c           CALL ADI2_HPKYC( HDUID, 'EXTNAME', HDU, '~', STATUS )
c         END IF

*      Mark as created
          DIDCRE = .TRUE.

        END IF

      END IF

*  Release the container
      CALL ADI_ERASE( HDUTAB, STATUS )
      CALL ADI_ERASE( HCID, STATUS )

      END



      SUBROUTINE ADI2_CFIND_KEY( HDUID, KEY, CREATE, KEYID, DIDCRE,
     :                           STATUS )
*+
*  Name:
*     ADI2_CFIND_KEY

*  Purpose:
*     Locate/create the named keyword in an HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_CFIND_KEY( HDUID, KEY, CREATE, KEYID, DIDCRE, STATUS )

*  Description:
*     Locates, creating if required, the named keyword in the specified
*     HDU.

*  Arguments:
*     HDUID = INTEGER (given)
*        The HDU in which the keyword will be found/created
*     KEY = CHARACTER*(*) (given)
*        The name of the keyword
*     CREATE = LOGICAL (given)
*        Create component if it doesn't exist?
*     HDUID = INTEGER (returned)
*        Identifier of FITS keyword cache object. If we are locating and
*        it doesn't exist, set to ADI__NULLID
*     DIDCRE = LOGICAL (returned)
*        If CREATE true on entry tells whether keyword was created
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			HDUID
      CHARACTER*(*)		KEY
      LOGICAL			CREATE

*  Arguments Returned:
      LOGICAL			DIDCRE
      INTEGER                   KEYID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER		        CHR_LEN
      EXTERNAL			CHR_SIMLR
        LOGICAL			CHR_SIMLR
      EXTERNAL			ADI2_MKIDX
        CHARACTER*8		ADI2_MKIDX

*  Local Variables:
      INTEGER			TABID			! Card table object
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      KEYID = ADI__NULLID
      IF ( CREATE ) DIDCRE = .FALSE.

*  Locate the card table
      CALL ADI_FIND( HDUID, 'CrdTable', TABID, STATUS )

*  Keyword exists in card table?
      CALL ADI2_CFIND_NAM( TABID, KEY, KEYID, STATUS )

*  Doesn't exist, and we can create?
      IF ( (KEYID .EQ. ADI__NULLID) .AND. CREATE ) THEN

*    Create cache object
        CALL ADI2_CFIND_CREC( HDUID, 'Crd', KEY, 'FITSkeyCache',
     :                        KEYID, STATUS )

*    We did create it
        DIDCRE = .TRUE.

      END IF

*  Release card table
      CALL ADI_ERASE( TABID, STATUS )

      END



      SUBROUTINE ADI2_CFIND_NAM( TABID, NAME, ID, STATUS )
*+
*  Name:
*     ADI2_CFIND_NAM

*  Purpose:
*     Locate a name in a table where the name/number is on the property list

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_CFIND_NAM( TABID, NAME, ID, STATUS )

*  Description:
*     ADI named cache objects are stored in indexed tables stored in an
*     ADI STRUC object. Numbering preserves the original order of creation
*     of the object, and the name to number mapping is stored in the table
*     property list.

*  Arguments:
*     TABID = INTEGER (given)
*        The table to search
*     NAME = CHARACTER*(*) (given)
*        The entry to search for
*     ID = INTEGER (returned)
*        Identifier of table entry
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			TABID
      CHARACTER*(*)		NAME

*  Arguments Returned:
      INTEGER                   ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			ADI2_MKIDX
        CHARACTER*8		ADI2_MKIDX

*  Local Variables:
      CHARACTER*8		IDXSTR			! Index string
      INTEGER			INDEX			! Index value

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      ID = ADI__NULLID

*  Does the HDU exist in the container property list?
      CALL ADI_THERE( TABID, '.'//NAME, THERE, STATUS )
      IF ( THERE ) THEN

*    Get the index string
        CALL ADI_CGET0I( TABID, '.'//NAME, INDEX, STATUS )

*    Make the index string
        IDXSTR = ADI2_MKIDX( 'Obj_', INDEX )

*    Locate the HDU
        CALL ADI_FIND( TABID, IDXSTR, ID, STATUS )

*    Check not marked for delete
        CALL ADI2_CHKDEL( ID, STATUS )

*  Not in container...
c     ELSE
c       STATUS = SAI__ERROR
      END IF

c     IF ( STATUS .NE. SAI__OK ) THEN
c       CALL MSG_SETC( 'E', NAME )
c       CALL ERR_REP( ' ', 'Unable to find entry ^E '/
c    :                  /'in table', STATUS )
c       CALL AST_REXIT( 'ADI2_CFIND_NAM', STATUS )
c     END IF

      END



      SUBROUTINE ADI2_CFIND_SCAN( LUN, HDUID, STATUS )
*+
*  Name:
*     ADI2_CFIND_SCAN

*  Purpose:
*     Read keywords from an HDU and store

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_CFIND_SCAN( LUN, HDUID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     LUN = INTEGER (given)
*        Logical unit for file
*     HDUID = INTEGER (given)
*        ADI identifier of FITShdu object
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Sep 1995 (DJA):
*        Original version.
*     31 Jan 1997 (RB):
*        Junk keyword records when the value cannot be interpreted
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			LUN, HDUID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
      EXTERNAL			ADI2_MKIDX
        CHARACTER*8		ADI2_MKIDX

*  Local Variables:
      CHARACTER*72		CMNT			! Comment
      CHARACTER*8		CNAME			! Card name
      CHARACTER*8		KEYWRD			! Keyword name
      CHARACTER*15		TYPE			! Data type
      CHARACTER*50		VALUE			! Keyword value

      DOUBLE PRECISION		DVAL			! Numeric key value

      INTEGER			BITPIX			! Bits per pixel
      INTEGER			CID			! Card cache object
      INTEGER			FSTAT			! FITSIO status code
      INTEGER			ICARD			! Loop over HDU cards
      INTEGER			ICRAP			! Crap card counter
      INTEGER			IMID			! Image cache object
      INTEGER			IVAL			! Integer key value
      INTEGER			NDECIM			! # decimals used
      INTEGER			NDIM, DIMS(ADI__MXDIM)	! Dimensions
      INTEGER			VID			! Value identifier

      LOGICAL			ISTABLE			! I/p is a table?
      LOGICAL			MORE			! More cards?
      LOGICAL			SCINOT			! Scientific notation?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      ICRAP = 0
      ICARD = 1
      FSTAT = 0
      MORE = .TRUE.
      DO WHILE ( MORE .AND. (FSTAT .EQ. 0) )
        if (status.ne.0) print*, icard, keywrd, value, cmnt, fstat

*    Extract name and value
        CALL FTGKYN( LUN, ICARD, KEYWRD, VALUE, CMNT, FSTAT )

*    Ok?
        IF ( KEYWRD .EQ. 'END' ) THEN
          MORE = .FALSE.

*    Comment card?
        ELSE IF ( KEYWRD .EQ. 'COMMENT' ) THEN

*      Store the comment
          CALL ADI2_HPCMT( HDUID, '@'//CMNT(3:), STATUS )

*      Next card
          ICARD = ICARD + 1

*    History card?
        ELSE IF ( KEYWRD .EQ. 'HISTORY' ) THEN

*      Store the history
          CALL ADI2_HPHIS( HDUID, '@'//CMNT(3:), STATUS )

*      Next card
          ICARD = ICARD + 1

*    Crap card
        ELSE IF ( (FSTAT .EQ. 0) .AND. (KEYWRD.LE.' ') ) THEN

*      Re-read the card
          CALL FTGREC( LUN, ICARD, CMNT, FSTAT )

*      Create crap name
          ICRAP = ICRAP + 1
          CNAME = ADI2_MKIDX( 'CRAP', ICRAP )

*      Create crap cache object
          CALL ADI2_CFIND_CREC( HDUID, 'Crd', CNAME, 'FITScrapCache',
     :                          CID, STATUS )

*      Write comment string
          CALL ADI_CPUT0C( CID, 'Value', CMNT, STATUS )

*      Release card
          CALL ADI_ERASE( CID, STATUS )

*      Next card
          ICARD = ICARD + 1

*    Good card?
        ELSE IF ( (FSTAT .EQ. 0) .AND. (KEYWRD.GT.' ') ) THEN

*      Create keyword object
          SCINOT = .FALSE.
          NDECIM = 0
          IF ( VALUE(1:1) .EQ. '''' ) THEN
            CALL ADI_NEWV0C( VALUE(2:CHR_LEN(VALUE)-1), VID, STATUS )
          ELSE
            CALL CHR_LDBLK( VALUE )
            IF ( INDEX( VALUE, '.' ) .GT. 0 ) THEN
              CALL CHR_CTOD( VALUE, DVAL, STATUS )
              CALL ADI_NEWV0D( DVAL, VID, STATUS )

*          Scientific notation?
              SCINOT = (INDEX( VALUE, 'E' ).GT.0)

*          Determine number of stored decimal places
              IF ( SCINOT ) THEN
                NDECIM = INDEX(VALUE,'E') - INDEX(VALUE,'.') - 1
              ELSE
                NDECIM = CHR_LEN(VALUE) - INDEX(VALUE,'.') - 1
              END IF

            ELSE
              IF ( (VALUE(1:1) .EQ. 'T') .OR. (VALUE(1:1).EQ.'F') ) THEN
                CALL ADI_NEWV0L( (VALUE(1:1) .EQ. 'T'), VID, STATUS )
              ELSE
                CALL CHR_CTOI( VALUE, IVAL, STATUS )
                CALL ADI_NEWV0I( IVAL, VID, STATUS )
              END IF
            END IF

          END IF

*      Did we interpret the record properly?
          IF ( STATUS .EQ. SAI__OK ) THEN

*        Store format
            CALL ADI_CPUT0L( VID, '.Scientific', SCINOT, STATUS )
            CALL ADI_CPUT0I( VID, '.Ndecimal', MAX(1,NDECIM), STATUS )

*        Write value to cache object
            CALL ADI2_ADDKEY( HDUID, '@'//KEYWRD, VID, CMNT, STATUS )

*        Next card
            ICARD = ICARD + 1

*      If not then just junk the card.
          ELSE
            CALL ERR_ANNUL( STATUS )
            ICARD = ICARD + 1
          END IF

*    Blank? Do nothing
        ELSE IF ( KEYWRD .LE. ' ' ) THEN

*      Next card
          ICARD = ICARD + 1

        ELSE
          MORE = .FALSE.

        END IF

      END DO

*  Mark as scanned
      CALL ADI_CPUT0L( HDUID, 'Scanned', .TRUE., STATUS )

*  Write comment and history counters
      CALL ADI_CPUT0I( HDUID, 'CrapCount', ICRAP, STATUS )

*  Fill in cache objects according to HDU type
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Table?
        CALL ADI_CGET0L( HDUID, 'IsTable', ISTABLE, STATUS )
        IF ( ISTABLE ) THEN
        ELSE

*      Read type and dimensions from keywords
          CALL ADI2_IMGTSHP( HDUID, .FALSE., BITPIX, NDIM, DIMS,
     :                       STATUS )

*      Convert BITPIX to TYPE
          CALL ADI2_BP2TYP( BITPIX, TYPE, STATUS )

*      Define as an IMAGE extension
          CALL ADI2_DEFIMG( HDUID, TYPE, NDIM, DIMS, .FALSE.,
     :                      IMID, STATUS )

*      Write logical unit number in case data is to be read later
          CALL ADI_CPUT0I( IMID, 'Lun', LUN, STATUS )

*      Release object
          CALL ADI_ERASE( IMID, STATUS )

        END IF

      END IF

*  Warn if not END card
      IF ( MORE ) THEN
        CALL ADI2_FITERP( FSTAT, STATUS )
        CALL ERR_REP( ' ', 'Unable to locate END keyword at '/
     :                  /'end of HDU', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_CFIND_SCAN', STATUS )
      END IF

      END



      SUBROUTINE ADI2_CFIND_CREC( PARENT, CROOT, NAME, CTYPE, ID,
     :                            STATUS )
*+
*  Name:
*     ADI2_CFIND_CREC

*  Purpose:
*     Create a new cache object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_CFIND_CREC( PARENT, CROOT, NAME, CTYPE, ID, STATUS )

*  Description:
*     Creates a new cache object, maintaining an index

*  Arguments:
*     PARENT = INTEGER (given)
*        The ADI identifier of the parent object. The parent contains
*        the child count and child name/number index structures
*     CROOT = CHARACTER*(*) (given)
*        The root name of the child count and index structures
*     NAME = CHARACTER*(*) (given)
*        The name of the new cache object
*     CTYPE = CHARACTER*(*) (given)
*        The ADI type of the new cache object
*     ID = INTEGER (returned)
*        The ADI identifier of the new object
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			PARENT
      CHARACTER*(*)		CROOT, NAME, CTYPE

*  Arguments Returned:
      INTEGER                   ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			ADI2_MKIDX
        CHARACTER*8		ADI2_MKIDX

*  Local Variables:
      CHARACTER*8		CCOUNT			! Counter name
      CHARACTER*8		CTABLE			! Table name
      CHARACTER*8		IDXSTR			! Index name

      INTEGER			COUNT			! Object counter
      INTEGER			TABID			! Table structure
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Name of counter and index structures
      CCOUNT = CROOT//'Count'
      CTABLE = CROOT//'Table'

*  Get value of counter
      CALL ADI_CGET0I( PARENT, CCOUNT, COUNT, STATUS )

*  Increment counter
      COUNT = COUNT + 1

*  Locate the child table
      CALL ADI_FIND( PARENT, CTABLE, TABID, STATUS )

*  Construct the index string
      IDXSTR = ADI2_MKIDX( 'Obj_', COUNT )

*  Create the cache object
      CALL ADI_CNEW0( TABID, IDXSTR, CTYPE, STATUS )
      CALL ADI_FIND( TABID, IDXSTR, ID, STATUS )

*  Write the counter to the cache object
      IF ( NAME .GT. ' ' ) THEN
        CALL ADI_CPUT0C( ID, 'Name', NAME, STATUS )
        CALL ADI_CPUT0I( TABID, '.'//NAME, COUNT, STATUS )
      END IF
      CALL ADI_CPUT0I( ID, 'Number', COUNT, STATUS )

*  Write the parent to the cache object
      CALL ADI_CPUT0I( ID, 'Parent', PARENT, STATUS )

*  Release the table
      CALL ADI_ERASE( TABID, STATUS )

*  Write back the counter
      CALL ADI_CPUT0I( PARENT, CCOUNT, COUNT, STATUS )

      END



      SUBROUTINE ADI2_CFIND_SETN( PARENT, CROOT, ID, NAME, STATUS )
*+
*  Name:
*     ADI2_CFIND_SETN

*  Purpose:
*     Set name and index entry of existing cache object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_CFIND_SETN( PARENT, CROOT, ID, NAME, STATUS )

*  Description:
*     Creates a new cache object, maintaining an index

*  Arguments:
*     PARENT = INTEGER (given)
*        The ADI identifier of the parent object. The parent contains
*        the child count and child name/number index structures
*     CROOT = CHARACTER*(*) (given)
*        The root name of the child count and index structures
*     ID = INTEGER (given)
*        The ADI identifier of the existing cache object
*     NAME = CHARACTER*(*) (given)
*        The name of the cache object
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			PARENT, ID
      CHARACTER*(*)		CROOT, NAME

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*8		CTABLE			! Table name

      INTEGER			COUNT			! Object counter
      INTEGER			TABID			! Table structure
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Name of index structure
      CTABLE = CROOT//'Table'

*  Locate the child table
      CALL ADI_FIND( PARENT, CTABLE, TABID, STATUS )

*  Extract counter from cache object
      CALL ADI_CGET0I( ID, 'Number', COUNT, STATUS )

*  Write the counter to the cache object
      CALL ADI_CPUT0C( ID, 'Name', NAME, STATUS )
      CALL ADI_CPUT0I( TABID, '.'//NAME, COUNT, STATUS )

*  Release the table
      CALL ADI_ERASE( TABID, STATUS )

      END



      SUBROUTINE ADI2_CFIND_SCMT( HDUID, STATUS )
*+
*  Name:
*     ADI2_CFIND_SCMT

*  Purpose:
*     Write standard comments to FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_CFIND_SCMT( HDUID, STATUS )

*  Description:
*     Writes FITSIO style comments to an HDU. Makes people think we
*     use kosher FITSIO to our output which keeps them quiet.

*  Arguments:
*     HDUID = INTEGER (given)
*        Identifier of HDU cache object
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     5 Jun 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			HDUID

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the comments
      CALL ADI2_HPCMT( HDUID, 'FITS (Flexible Image Transport '//
     : 		'System) format defined in Astronomy and', STATUS )
      CALL ADI2_HPCMT( HDUID, 'Astrophysics Supplement Series '//
     : 		'v44/p363, v44/p371, v73/p359, v73/p365.', STATUS )
      CALL ADI2_HPCMT( HDUID, 'Contact the NASA Science '//
     : 		'Office of Standards and Technology for the', STATUS )
      CALL ADI2_HPCMT( HDUID, 'FITS Definition document '//
     :         '#100 and other FITS information.', STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_CFIND_SCMT', STATUS )
      END IF

      END
