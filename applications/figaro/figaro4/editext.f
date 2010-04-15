      SUBROUTINE EDITEXT( STATUS )
*+
*  Name:
*     EDITEXT

*  Purpose:
*     Edit the Specdre Extension.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL EDITEXT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine allows the user to modify the Specdre Extension. See
*     the topic "Requests" for details. Users should also consult the
*     description of the Specdre Extension in SUN/140.

*  Requests:
*     The request or action required consists of blank-separated
*     words. The first word is a verb specifying the kind of action.
*     The verb can be LIST, CREATE, DELETE, SET, TYPE or SHAPE. The
*     verb is case-insensitive. The length of the request is
*     restricted to 130 characters.
*
*     There may or may not follow a second word specifying the
*     structure affected. This can be any of the scalar structures in
*     the Specdre Extension, i.e. SPECAXIS, RESTFRAME, INDEXREFR,
*     FREQREF, FREQUNIT. It can also be any of the NDF-type structures
*     in the Specdre Extension, i.e. SPECVALS, SPECWIDS, COVRS, RESULTS.
*     Finally it can be any structure which is an extension to the
*     (NDF-)structure RESULTS. These latter structures are all HDS
*     vectors, their names are LINENAME, LABFREQ, COMPTYPE, NPARA,
*     MASKL, MASKR, PARATYPE. The structure specification is
*     case-insensitive.
*
*     Further words contain parameter values, usually one word per
*     parameter. But if the last parameter is a string, it may
*     consist of several words. No quotes are necessary.
*
*     There is only one LIST request, namely the sole word LIST. This
*     will cause the complete Specdre Extension - except the contents of
*     NDF arrays - to be listed to the log file or to the screen.
*
*     There are two possible CREATE requests.
*     -  "CREATE" on its own will create an empty Specdre Extension,
*        or fail if a Specdre Extension already exists.
*     -  "CREATE RESULTS type1 type2 type3 int1 int2" needs five
*        parameters. Three parameters are case-insensitive HDS data
*        types. These are either _DOUBLE or assumed to be _REAL. The
*        result structure is an NDF-type structure and the different
*        type specifications apply to (i) the data and variance
*        structures of the NDF, (ii) the laboratory frequency
*        extension to the result NDF, (iii) the left and right mask
*        extensions to the result NDF. All extensions to the result
*        NDF are HDS vectors. Some of these have one element for
*        each spectral component, their created length is specified
*        by the fourth (last but one) request parameter, i.e. the
*        sixth word. This word must convert to an integer greater
*        than zero. Other HDS vectors in the extension to the result
*        NDF have one element for each result parameter, their
*        created length is specified by the fifth (last) request
*        parameter, i.e. the seventh word. This word must convert to
*        an integer greater than zero. "CREATE RESULTS" fails if the
*        result NDF already exists.
*
*     "DELETE" on its own will delete the whole Specdre Extension.
*     "DELETE struc" will delete the specified structure. This can be
*     any of the NDF-type structures SPECVALS, SPECWIDS, COORD,
*     COVRS, RESULTS. Deleting a structure does not include deleting
*     the whole Extension, even if it becomes empty.
*
*     All SET request will create the Specdre Extension, even if the
*     request is not recognised as a valid one.
*
*     "SET ndf-struct", where the second word specifies an NDF-type
*     structure, will set the values of the specified structure to
*     the default values. This does not work for COVRS, since it
*     defaults to non-existence. The structure is created if it does
*     not already exist. For SPECVALS and SPECWIDS only the NDF's data
*     structure is affected. For RESULTS the NDF's data and variance
*     structures are set to bad values, but all the vectors in the
*     result NDF's extension remain unchanged.
*     -  "SET SPECVALS" will set the values in the data array of
*        spectroscopic values to the default values. These are copies
*        of the spectroscopic axis centres in the main NDF.
*     -  "SET SPECWIDS" will set the values in the data array of
*        spectroscopic widths to the default values. These are copies
*        of the spectroscopic axis widths in the main NDF.
*     -  "SET COORD" will set the values in the data array of
*        COORD1 and COORD2 to the default values. These are copies
*        axis centres for the first and second non-spectroscopic axes
*        in the main NDF.
*     -  "SET RESULTS" will set the values in the data and variance
*        arrays of the result NDF to bad values.
*
*     "SET SPECVALS.comp value" can be used to set the label and unit
*     components of the spectroscopic values' NDF.
*     -  "SET SPECVALS.LABEL label" will set the value of the label
*        of the spectroscopic values' NDF.
*     -  "SET SPECVALS.UNITS unit" will set the value of the unit of
*        the spectroscopic values' NDF.
*     -  "SET COORD1.LABEL label1" will set the value of the label
*        of the COORD1 NDF. Similarly for COORD2.
*     -  "SET COORD1.UNITS unit1" will set the value of the units of
*        the COORD1 NDF. Similarly for COORD2.
*
*     "SET scalar value" will convert the third word to a value and
*     put it in the scalar structure specified by the second word.
*     -  "SET SPECAXIS int" will try to convert the third word into
*        an integer. It must be between 1 and the number of axes in
*        the NDF to which this Specdre Extension is an extension. If
*        the value is actually changed, then this command will also
*        delete the NDF-type structures SPECVALS, COVRS and RESULTS.
*        This is because the contents of those structures depends on
*        the choice of spectroscopic axis and become invalid when the
*        value is changed. This command will also create the Specdre
*        Extension and spectroscopic axis structure if they do not
*        yet exist.
*     -  "SET RESTFRAME more words" will put the third and following
*        words (case-sensitive) into the reference frame structure.
*        This command will also create the Specdre Extension and
*        reference frame structure if they do not yet exist.
*     -  "SET INDEXREFR value" will try to convert the third word
*        into a real or double value, depending on the current type
*        of the refractive index structure. This command will also
*        create the Specdre Extension and refractive index structure
*        if they do not yet exist.
*     -  "SET FREQREF value" will try to convert the third word
*        into a real or double value, depending on the current type
*        of the reference frequency structure. This command will also
*        create the Specdre Extension and reference frequency
*        structure if they do not yet exist.
*     -  "SET FREQUNIT int" will try to convert the third word into
*        an integer. This command will also create the Specdre
*        Extension and frequency unit structure if they do not yet
*        exist.
*
*     "SET vector element value" will change the value of the
*     specified element in the specified vector. The vector must be
*     one of the extensions of the result NDF. The result NDF must
*     exist beforehand, which implies the existence of the vector.
*     The vector must also be long enough to contain the element
*     specified and the element number must be integer and greater
*     than zero. There are two kinds of vectors, those indexed by
*     spectral component and those indexed by result parameter.
*     -  "SET LINENAME comp more words" will put the forth and
*        following words (case-sensitive) into the comp-th element
*        of the line name structure.
*     -  "SET LABFREQ comp value" will try to convert the fourth
*        word into a real or double value, depending on the current
*        type of the laboratory frequency structure. It will then put
*        the value into the comp-th element of the laboratory
*        frequency structure.
*     -  "SET COMPTYPE comp more words" will put the forth and
*        following words (case-sensitive) into the comp-th element
*        of the component type structure.
*     -  "SET NPARA comp npara" will try to convert the fourth word
*        into an integer greater than or equal to zero. This is the
*        new number of parameters allocated to the comp-th component.
*        Changing this value will affect several parts of the result
*        structure both in their shapes and values. If the comp-th
*        spectral component is allocated more parameters than before,
*        then it may be necessary to provide for a higher total
*        number of parameters, which implies increasing the size of
*        .MORE.SPECDRE.RESULTS.DATA_ARRAY and VARIANCE and of
*        .MORE.SPECDRE.RESULTS.MORE.PARATYPE. At any rate, the
*        information about spectral components with indices higher
*        than comp must be relocated within those arrays.
*     -  "SET MASKL comp value" and "SET MASKR comp value" will try
*        to convert the fourth word into a real or double value,
*        depending on the current type of the mask structures. It
*        will then put the value into the comp-th element of the
*        relevant mask structure.
*     -  "SET PARATYPE para more words" will put the forth and
*        following words (case-sensitive) into the para-th element
*        of the parameter type structure.
*
*     A TYPE request can be applied to _REAL or _DOUBLE structures,
*     and of these to scalars and NDF-type structures. Changing the
*     type(s) of the result NDF needs specification of three separate
*     types.
*     -  "TYPE scalar type" can be applied to INDEXREFR and FREQREF.
*        The type specification is case-insensitive. If it is not
*        _DOUBLE, then _REAL is assumed.
*     -  "TYPE ndf-struct type", will change the type of the
*        specified NDF. The type specification is case-insensitive. It
*        must be _DOUBLE or is assumed to be _REAL. This command can
*        be applied to SPECVALS, SPECWIDS, COORD, and COVRS. SPECVALS,
*        SPECWIDS, COORD1 and COORD2 will be created if necessary,
*        COVRS will not be created.
*     -  "TYPE RESULTS type1 type2 type3" will change the types of
*        (i) the NDF's data and variance, (ii) the NDF's laboratory
*        frequency extension, (iii) the NDF's mask extensions. the
*        parameters are case-insensitive. They must be _DOUBLE or are
*        assumed to be _REAL. This command includes creation of the
*        result structure if necessary.
*
*     "SHAPE RESULTS int1 int2" will change the shape of the result
*     structure. The two command parameters must convert to integers
*     greater than zero. The first is the number of spectral
*     components to be provided for, the second is the total number
*     of parameters. If the result structure does not exist, then it
*     is created. If it exists, then existing values are retained
*     unless they were stored outside the new bounds.

*  Usage:
*     editext request in

*  ADAM Parameters:
*     REQUEST = _CHAR (Read)
*        The action required. This consists of blank separated words.
*        The following is a brief reminder of the syntax and permissible
*        requests. For the full details refer to the "Requests" topic.
*        -  LIST
*        -  CREATE
*        -  CREATE RESULTS type1 type2 type3 int1 int2
*        -  DELETE
*        -  DELETE struc
*        -  SET ndf-struct
*        -  SET SPECVALS.comp value
*        -  SET COORD1.comp value
*        -  SET COORD2.comp value
*        -  SET scalar value
*        -  SET vector element value
*        -  TYPE scalar type
*        -  TYPE ndf-struct type
*        -  TYPE RESULTS type1 type2 type3
*        -  SHAPE RESULTS int1 int2
*     IN = NDF (Read)
*        The NDF the Specdre Extension of which is to be modified. The
*        modification is done in situ, i.e. there is no separate output
*        NDF. In most modes, the routine requires update access. Only in
*        list mode is read access sufficient.
*     LOGFIL = FILENAME (Read)
*        The filename for the ASCII output file in list mode. If this
*        file exists, it is opened for append access. A null value for
*        this parameter will signal that no file is to be used. The
*        output will then be directed to the standard output device (the
*        user's screen).
*        [!]

*  Examples:
*     editext list in accept
*        This will look for the Specdre Extension to the main NDF called IN
*        and list the Extension's contents to the default output device
*        (usually the user's screen). Some character strings that may be
*        up to 32 characters long are truncated to 16 characters in
*        order to fit on the screen.
*     editext list in logfil=out
*        This will look for the Specdre Extension to the main NDF called IN
*        and list the Extension's contents to the ASCII file OUT.DAT.
*        This happens without string truncation.
*     editext delete in
*        This will look for the Specdre Extension to the main NDF called IN
*        and delete the Extension.
*     editext "set restframe heliocentric" in
*        This will access the main NDF called IN, find or create its Specdre
*        Extension, find or create the RESTFRAME structure in the
*        Extension, and put the string "heliocentric" into the RESTFRAME
*        structure.
*     editext "set frequnit 6"
*        This will access the main NDF called IN, find or create its Specdre
*        Extension, find or create the FREQUNIT structure in the
*        Extension, and put the value 6 into the FREQUNIT structure.
*        This is to mean that reference and laboratory frequencies will
*        be expressed in MHz (10**6 Hz).
*     editext "set labfreq 5 1420" in
*        This will access the main NDF called IN, find its Specdre Extension
*        and find the RESULTS structure in the Extension (which is an
*        NDF). If this is successful the routine will find the LABFREQ
*        extension of the result NDF and set its fifth element to 1420.
*        This is the laboratory frequency of the fifth spectral
*        component. In conjunction with a FREQUNIT of 6, this is
*        (very roughly) the frequency of the 21 cm ground state
*        hyperfine transition of neutral atomic hydrogen.
*     editext "set npara 5 3" in
*        This will access the main NDF called IN, find its Specdre Extension
*        and find the RESULTS structure in the Extension (which is an
*        NDF). If this is successful the routine will find the NPARA
*        extension of the result NDF and set its fifth element to 3.
*        This is to mean that the fifth spectral component is allocated
*        space for three parameters in the result NDF. Changing this
*        number may require to increase the total number of parameters
*        which in turn affects the shape of the result NDF and of the
*        PARATYPE extension to the result NDF. Changing NPARA(5) also
*        makes it necessary to shift information in the result NDF's
*        data and variance structures as well as in the PARATYPE
*        extension to the result NDF. All this is handled consistently by
*        this routine.
*     editext "shape results 6 20" in
*        This will access the main NDF called IN, find or create its Specdre
*        Extension, find or create the RESULTS structure in the
*        Extension, and shape it to provide for six spectral components
*        and a total of 20 parameters. If results existed before, it
*        will be expanded or contracted "at the end". That is, existing
*        components 1 to 6 and parameter 1 to 20 would be retained.

*  Notes:
*     This routine recognises the Specdre Extension v. 1.1.
*
*     This routine works in situ and modifies the input file.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     13 Mar 1992 (hme):
*        Original version.
*     22 Jun 1992 (hme):
*        Use SPEPAR include file.
*     09 Sep 1992 (hme):
*        Don't set TITLE.
*     24 Nov 1994 (hme):
*        Use new libraries. Report errors immediately.
*     20 Nov 1995 (hme):
*        Open ASCII file with type LIST instead of FORTRAN.
*     24 Nov 1995 (hme):
*        Add support for COORD (Extension v. 0.7 -> 1.1)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PAR_ERR'          ! PAR status values
      INCLUDE 'SPD_EPAR'         ! Specdre Extension constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXNWRD             ! Maximum number of words in request
      PARAMETER ( MXNWRD = 7 )

*  Local Variables:
      CHARACTER * ( 130 ) REQEST
      INTEGER NDF
      INTEGER FD
      INTEGER FU
      LOGICAL FILE               ! True if list goes to a file
      LOGICAL EXIST              ! True if structure exists
      LOGICAL EXIST2             ! True if structure exists
      INTEGER I                  ! Loop index
      INTEGER LSTAT              ! Status for CHR_DCWRD
      INTEGER NWORD              ! Number of words in request
      INTEGER STARTW( MXNWRD )   ! Positions where words start
      INTEGER STOPW( MXNWRD )    ! Positions where words stop
      INTEGER NCOMP              ! Number of components in results
      INTEGER TNPAR              ! Total number of parameters in results
      CHARACTER * ( 64 ) MESSAG  ! Contextual error message
      CHARACTER * ( 130 ) WORD( MXNWRD ) ! The words in the request
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator of .MORE.SPECDRE
      CHARACTER * ( NDF__SZTYP ) TYPE( 3 ) ! Data types

*  Local Data:
      DATA TYPE / XT9D, XT9C2, XT9C5 /

*.


*  Get the information.
*  ====================

*  Check inherited global status.
*  Set default error message and file switch.
      IF ( STATUS .NE. SAI__OK ) RETURN
      MESSAG = 'Unspecified failure.'
      FILE = .FALSE.

*  Get request parameter and split it into words.
      CALL PAR_GET0C( 'REQUEST', REQEST, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      LSTAT = 0
      CALL CHR_DCWRD( REQEST, MXNWRD, NWORD, STARTW, STOPW, WORD,
     :   LSTAT )
      CALL CHR_UCASE( WORD(1) )
      CALL CHR_UCASE( WORD(2) )

*  Get the NDF.
      CALL NDF_BEGIN
      IF (  WORD(1) .EQ. 'LIST' ) THEN
         CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )
      ELSE
         CALL NDF_ASSOC( 'IN', 'UPDATE', NDF, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
         MESSAG = 'Error accessing NDF.'
         CALL ERR_REP( 'EDITEXT_ERR', 'EDITEXT: ' // MESSAG, STATUS )
         GO TO 500
      END IF


*  List request.
*  =============

*  If list request.
      IF ( WORD(1) .EQ. 'LIST' ) THEN

*     Locate Extension.
         CALL SPD_EAAA( NDF, 'READ', EXIST, XLOC, STATUS )

*     See if the Extension exists.
         IF ( .NOT. EXIST ) THEN
            CALL NDF_MSG( 'CHAR', NDF )
            CALL MSG_OUT( 'EDITEXT_LIST',
     :         'The NDF ^CHAR does not have a Specdre Extension.' ,
     :         STATUS )
            GO TO 500
         END IF

*     Find out about list file.
         FILE = .TRUE.
         CALL FIO_ASSOC( 'LOGFIL', 'APPEND', 'LIST', 0, FD, STATUS )

*     A null value indicates to use the screen for output.
*     FIO translates a null parameter into a status of "parameter is not
*     a file parameter".
*     If a file is to be used (else case), find out the Fortran unit
*     number.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            FILE = .FALSE.
         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            MESSAG = 'Error opening list file.'
            CALL ERR_REP( 'EDITEXT_ERR', 'EDITEXT: ' // MESSAG, STATUS )
            CALL DAT_ANNUL( XLOC, STATUS )
            GO TO 500
         ELSE
            CALL FIO_UNIT( FD, FU, STATUS )
         END IF

*     Go and do the listing.
         CALL SPD_CZWD( FILE, FU, NDF, XLOC, STATUS )

*     Release the locator.
         CALL DAT_ANNUL( XLOC, STATUS )


*  Creation request.
*  =================

*  Else if creation request.
      ELSE IF ( WORD(1) .EQ. 'CREATE' ) THEN

*     If creation of Extension requested.
         IF ( NWORD .EQ. 1 ) THEN

*        Create if not already there.
            CALL SPD_EAAA( NDF, 'UPDATE', EXIST, XLOC, STATUS )
            CALL DAT_ANNUL( XLOC, STATUS )

*     Else if creation of result structure requested.
         ELSE IF ( WORD(2) .EQ. XCMP9 ) THEN

*        See if parameters are present.
            IF ( NWORD .LT. 7 ) THEN
               STATUS = SAI__ERROR
               MESSAG = 'Error reading parameters.'
               CALL ERR_REP( 'EDITEXT_ERR', 'EDITEXT: ' //
     :            MESSAG, STATUS )
               GO TO 500
            END IF

*        Check and repair the types specified.
            DO 1 I = 3, 5
               CALL CHR_UCASE( WORD(I) )
               IF ( WORD(I) .NE. '_DOUBLE' ) WORD(I) = '_REAL'
 1          CONTINUE

*        Get the numeric parameters to shape the structure.
            CALL CHR_CTOI( WORD(6), NCOMP, STATUS )
            CALL CHR_CTOI( WORD(7), TNPAR, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               MESSAG = 'Error reading parameters.'
               CALL ERR_REP( 'EDITEXT_ERR', 'EDITEXT: ' //
     :            MESSAG, STATUS )
               GO TO 500
            END IF

*        Locate or create Specdre Extension.
            CALL SPD_EAAA( NDF, 'UPDATE', EXIST, XLOC, STATUS )

*        Create results, if not already there.
            CALL DAT_THERE( XLOC, XCMP9, EXIST, STATUS )
            IF ( EXIST ) THEN
               CALL DAT_ANNUL( XLOC, STATUS )
               STATUS = SAI__ERROR
               MESSAG = 'Error: Result structure already exists.'
               CALL ERR_REP( 'EDITEXT_ERR', 'EDITEXT: ' //
     :            MESSAG, STATUS )
               GO TO 500
            ELSE
               CALL SPD_FDHF( NDF, XLOC, NCOMP, TNPAR, WORD(3), STATUS )
            END IF

*        Release locator.
            CALL DAT_ANNUL( XLOC, STATUS )

*     Else (unrecognised creation request).
         ELSE
            STATUS = SAI__ERROR
            MESSAG = 'Error: Invalid CREATE request.'
            CALL ERR_REP( 'EDITEXT_ERR', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF


*  Deletion request.
*  =================

*  Else if deletion request.
      ELSE IF ( WORD(1) .EQ. 'DELETE' ) THEN

*     If deletion of whole Extension requested.
         IF ( NWORD .EQ. 1 ) THEN
            CALL NDF_XDEL( NDF, XNAME, STATUS )

*     Else if one of the NDF-type structures to be deleted.
         ELSE IF ( WORD(2) .EQ. XCMP6 .OR.
     :             WORD(2) .EQ. XCMP7 .OR.
     :             WORD(2) .EQ. XCMP8 .OR.
     :             WORD(2) .EQ. XCMP9 ) THEN

*        Locate Specdre Extension and delete the structure.
*        (First cautiously look if Extension exists at all.)
            CALL SPD_EAAA( NDF, 'READ', EXIST, XLOC, STATUS )
            IF ( EXIST ) THEN
               CALL DAT_ANNUL( XLOC, STATUS )
               CALL SPD_EAAA( NDF, 'UPDATE', EXIST, XLOC, STATUS )
               CALL DAT_THERE( XLOC, WORD(2), EXIST2, STATUS )
               IF ( EXIST2 ) CALL DAT_ERASE( XLOC, WORD(2), STATUS )
               CALL DAT_ANNUL( XLOC, STATUS )
            END IF

*     Else if the COORD NDF-type structures to be deleted.
         ELSE IF ( WORD(2) .EQ. 'COORD' ) THEN

*        Locate Specdre Extension and delete the structures.
*        (First cautiously look if Extension exists at all.)
            CALL SPD_EAAA( NDF, 'READ', EXIST, XLOC, STATUS )
            IF ( EXIST ) THEN
               CALL DAT_ANNUL( XLOC, STATUS )
               CALL SPD_EAAA( NDF, 'UPDATE', EXIST, XLOC, STATUS )
               CALL DAT_THERE( XLOC, 'COORD1', EXIST2, STATUS )
               IF ( EXIST2 ) CALL DAT_ERASE( XLOC, 'COORD1', STATUS )
               CALL DAT_THERE( XLOC, 'COORD2', EXIST2, STATUS )
               IF ( EXIST2 ) CALL DAT_ERASE( XLOC, 'COORD2', STATUS )
               CALL DAT_ANNUL( XLOC, STATUS )
            END IF

*     Else (unrecognised deletion request).
         ELSE
            STATUS = SAI__ERROR
            MESSAG = 'Error: Invalid DELETE request.'
            CALL ERR_REP( 'EDITEXT_ERR', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF


*  Set request.
*  ============

*  Else if request to set something.
      ELSE IF ( WORD(1) .EQ. 'SET' ) THEN

*     All setting requests require the Specdre Extension to exist.
*     And some need a locator.
*     Do the setting action.
*     Release locator.
         CALL SPD_EAAA( NDF, 'UPDATE', EXIST, XLOC, STATUS )
         CALL SPD_CZWE( NDF, XLOC, REQEST, MXNWRD, NWORD, WORD, STARTW,
     :      STATUS )
         CALL DAT_ANNUL( XLOC, STATUS )


*  Type request.
*  =============

*  Else if type request.
      ELSE IF ( WORD(1) .EQ. 'TYPE' ) THEN

*     All typing requests require the Specdre Extension to exist.
*     And some need a locator.
*     Do the typing action.
*     Release locator.
*     This should be only one call to SPABF().
         CALL SPD_EAAA( NDF, 'UPDATE', EXIST, XLOC, STATUS )
         CALL SPD_CZWF( NDF, XLOC, MXNWRD, NWORD, WORD, STATUS )
         CALL DAT_ANNUL( XLOC, STATUS )


*  Shape request.
*  ==============

*  Else if shape request.
      ELSE IF ( WORD(1) .EQ. 'SHAPE' ) THEN

*     If result structure to be shaped.
         IF ( WORD(2) .EQ. XCMP9 ) THEN

*        See if command parameters are present.
            IF ( NWORD .LT. 4 ) THEN
               STATUS = SAI__ERROR
               MESSAG = 'Error reading parameters.'
               CALL ERR_REP( 'EDITEXT_ERR', 'EDITEXT: ' //
     :            MESSAG, STATUS )
               GO TO 500
            END IF

*        Get the numbers to shape the structure.
            CALL CHR_CTOI( WORD(3), NCOMP, STATUS )
            CALL CHR_CTOI( WORD(4), TNPAR, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               MESSAG = 'Error reading parameters.'
               CALL ERR_REP( 'EDITEXT_ERR', 'EDITEXT: ' //
     :            MESSAG, STATUS )
               GO TO 500
            END IF

*        Locate or create Specdre Extension.
*        Reshape or create results, types default to _REAL.
*        Release locator.
            CALL SPD_EAAA( NDF, 'UPDATE', EXIST, XLOC, STATUS )
            CALL DAT_THERE( XLOC, XCMP9, EXIST, STATUS )
            IF ( EXIST ) THEN
               CALL SPD_FDHB( NDF, XLOC, NCOMP, TNPAR, STATUS )
            ELSE
               CALL SPD_FDHF( NDF, XLOC, NCOMP, TNPAR, TYPE, STATUS )
            END IF
            CALL DAT_ANNUL( XLOC, STATUS )

*     Else (unregocnised shape request).
         ELSE
            STATUS = SAI__ERROR
            MESSAG = 'Error: Invalid SHAPE request.'
            CALL ERR_REP( 'EDITEXT_ERR', 'EDITEXT: ' //
     :         MESSAG, STATUS )
            GO TO 500
         END IF


*  Nonsensical request.
*  ====================

*  Else (first word not recognised).
      ELSE
         STATUS = SAI__ERROR
         MESSAG = 'Error: Unrecognised request verb.'
         CALL ERR_REP( 'EDITEXT_ERR', 'EDITEXT: ' //
     :      MESSAG, STATUS )
         GO TO 500
      END IF


*  Close down.
*  ===========

*  Tidy up.
 500  CONTINUE

*  Close down FIO.
      IF ( FILE ) THEN
         CALL FIO_CANCL( 'LOGFIL', STATUS )
         CALL FIO_DEACT( STATUS )
      END IF

*  Close down NDF.
      CALL NDF_ANNUL( NDF, STATUS )
      CALL NDF_END( STATUS )

*  Return.
      END
