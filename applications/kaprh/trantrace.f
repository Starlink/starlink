      SUBROUTINE TRANTRACE( STATUS )
*+
*  Name:
*     TRANTRACE

*  Purpose:
*     Lists the contents of a transformation structure.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TRANTRACE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application reports or write to a text file the contents of
*     a TRANSFORM structure.  Items listed include:
*
*        - the structure's name;
*        - the version of the TRANSFORM software used to write the
*          structure;
*        - the number of input and output variables for the nett
*        transformation and for each transformation where the structure
*        contains more than one;
*        - the classification of the forward and inverse mappings; and
*        - for each transformation its precision, comment, forward and
*        inverse functions.

*  Usage:
*     trantrace transform [logfile]

*  ADAM Parameters:
*     CLASSFOR = LITERAL (Write)
*        A comma-separated list of classifications that describe the
*        properties of the forward mapping of the transformation.  The
*        possible values in the list are:
*           "Linear"       --- linear and preserves straight lines,
*           "Independent"  --- preserves the independence of the axes,
*           "Diagonal"     --- preserves the axes themselves,
*           "Isotropic"    --- preserves angles and shapes,
*           "Positive_det" --- a component of reflection is absent,
*           "Negative_det" --- a component of reflection is present,
*           "Constant_det" --- the scale factor is constant,
*           "Unit_det"     --- areas (or volumes etc.) are preserved.
*
*        See SUN/61 Appendix B for more details of transformation
*        classification and a table of the classifications of common
*        mappings.
*     CLASSINV = LITERAL (Write)
*        A comma-separated list of classifications that describe the
*        properties of the inverse mapping of the transformation.  See
*        parameter CLASSFOR for further details.
*     COMMENT = LITERAL (Write)
*        The comment string associated with the transformation.  A
*        "-->" symbol, if present, indicates the forward
*        transformation.
*     FORWARD = LITERAL (Write)
*        The expression that defines the last forward mapping of the
*        transformation.
*     INVERSE = LITERAL (Write)
*        The expression that defines the last inverse mapping of the
*        transformation.
*     LOGFILE = FILENAME (Write)
*        The name of the text file to store a list of the
*        transformation structure.  If it is null (!) the list of the
*        transformation structure is reported directly to you. [!]
*     PREC = LITERAL (Write)
*        The arithmetic precision of the transformation.  This may be
*        either "_REAL" for single precision, "_DOUBLE" for double
*        precision, or "_INTEGER" for integer precision.
*     TRANSFORM = TRN (Read)
*        The transformation structure to be listed.  This may be an HDS
*        container file, in which case the transformation structure is
*        assumed to be called TRANSFORM at the top level of the file;
*        or a path to the HDS object.  The suggested default is the
*        current transformation structure.
*     VERSION = LITERAL (Write)
*       The version number of the TRANSFORM software used to write the
*       transformation structure.

*  Examples:
*     trantrace rot45.transform
*        This reports the contents of the transformation structure
*        within the HDS container file rot45.sdf, component TRANSFORM.
*     trantrace rot45
*        This has the same affect as the previous example.
*     trantrace \
*        This reports the contents of the current TRANSFORM structure.
*     trantrace jkt256.more.ccdpack.transform trn.lis
*        This lists to the text file trn.lis the contents of the
*        transformation structure located within the HDS file
*        jkt256.sdf, as component MORE.CCDPACK.TRANSFORM.
*     trantrace stretch.limit nvin=(nvi) comment=(trncom)
*        This reports the contents of the transformation structure
*        within the HDS container file stretch.sdf, component LIMIT.
*        The number of input transformation variables is written to the
*        ICL variable called NVI, and the transformation comment is
*        stored in ICL variable TRNCOM.

*  Notes:
*     -  Where a value is not optional but is absent, "<undefined>" appears
*     in the listing.
*     -  TRANTRACE attempts to compile the forward and inverse mappings
*     to check that it is a TRANSFORM structure, and will exit with an
*     error if both of these compilations fail.
*     -  On completion, the current transformation global parameter
*     takes the value of parameter TRANSFORM.

*  [optional_A_task_items]...
*  Related Applications:
*     KAPRH: TRANSFORMER, TRANINVERT, TRANJOIN, TRANMAKE;
*     CCDPACK: CCDEDIT, TRANLIST, TRANNDF.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 June 18 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'TRN_PAR'          ! TRANSFORM constants
      INCLUDE 'DAT_PAR'          ! HDS public constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER BUFLEN             ! Buffer length in characters
      PARAMETER ( BUFLEN = 132 )

      INTEGER FUNLEN             ! Buffer length in characters
      PARAMETER ( FUNLEN = BUFLEN - 12 )

*  Local Variables:
      CHARACTER * ( BUFLEN ) BUFFER ! Buffer for creating line of output
      CHARACTER * ( FUNLEN ) CLALST ! List of classifications
      LOGICAL CLASPR             ! Transformation is classified
      LOGICAL CLASS( TRN__MXCLS ) ! Transformation classes
      CHARACTER * ( FUNLEN ) COMENT ! Transformation comment
      INTEGER FD                 ! Logfile descriptor
      CHARACTER * ( 256 ) FILNAM ! HDS file name
      CHARACTER * ( FUNLEN ) FOREXP ! Forward mapping expression
      LOGICAL FORWRD             ! Forward transformation is present
      LOGICAL INDMAP             ! Only one transformation in structure
      LOGICAL INVERS             ! Inverse transformation is present
      CHARACTER * ( FUNLEN ) INVEXP ! Inverse mapping expression
      INTEGER ISTAT              ! Local status
      INTEGER ITRANS             ! Transformation loop counter
      INTEGER IVAR               ! Variable loop counter
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to the input object
      CHARACTER * ( DAT__SZLOC ) LOCEXP ! Locator to a single mapping
                                 ! expression
      CHARACTER * ( DAT__SZLOC ) LOCFUN ! Locator to mapping expressions
      CHARACTER * ( DAT__SZLOC ) LOCMA ! Locator to the mapping array
                                 ! structure
      CHARACTER * ( DAT__SZLOC ) LOCMOD ! Locator to individual mapping
                                 ! structure
      CHARACTER * ( DAT__SZLOC ) LOCTR ! Locator to the implied
                                 ! transformation structure
      LOGICAL LOGF               ! Use logfile if true
      INTEGER NC                 ! Number of characters in a string
      INTEGER NLEV               ! Number of levels in the path
      INTEGER NTRANS             ! Number of transformations in the
                                 ! structure
      INTEGER NVIN               ! Number of input variables
      INTEGER NVOUT              ! Number of output variables
      CHARACTER * ( TRN__SZPRC ) PREC ! Precision of transformation
      CHARACTER * ( 132 ) PATH   ! Path of the transformation structure
      LOGICAL THERE              ! Data component is present or
                                 ! TRANSFORM object exists at top level
      INTEGER TRIDF              ! Identifier to the forward input
                                 ! transformation
      INTEGER TRIDI              ! Identifier to the inverse input
                                 ! transformation
      REAL VERSIO                ! TRANSFORM version number

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a locator to the transformation structure.
*  ==============================================

*  Obtain the TRN transform file.
      CALL DAT_ASSOC( 'TRANSFORM', 'READ', LOC, STATUS )

*  Look to see if this the top level or to a structure within the file.
*  Get the path of the object and the number of nodes within it.
      CALL HDS_TRACE( LOC, NLEV, PATH, FILNAM, STATUS )

*  There is only one level.
      IF ( NLEV .EQ. 1 ) THEN

*  Check that there is a TRANSFORM structure.
         CALL DAT_THERE( LOC, 'TRANSFORM', THERE, STATUS )

*  Report an error if the TRANSFORM structure is absent.
         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'NOTRANSFORM',
     :        'The container file does not contain a TRANSFORM '/
     :        /'structure at the top level.', STATUS )
            GOTO 980
         ELSE

*  Obtain a locator to the TRANSFORM structure.
            CALL DAT_FIND( LOC, 'TRANSFORM', LOCTR, STATUS )

*  Annul the original locator.
            CALL DAT_ANNUL( LOC, STATUS )
         END IF
      ELSE

*  The original locator was to the transformation structure, so assign
*  its locator to the transformation locator.
         LOCTR = LOC
      END IF

*  Exit here if something has gone wrong to avoid receiving a spurious
*  error message.
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Validate the transformation structure.
*  ======================================

*  Assume for the moment that this is not a transformation structure and
*  so does not have the forward or inverse mappings.
      FORWRD = .FALSE.
      INVERS = .FALSE.

*  Validate the transformation.  Need at least a forward or a backward
*  mapping.  Since there is no inquiry function this must be done by
*  testing the status.  Also any error message should be supressed
*  unless both structures fail to compile; use nested error contexts to
*  achieve this.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  First attempt to compile the forward mapping with a new error
*  context.
         CALL ERR_MARK
         CALL TRN_COMP( LOCTR, .TRUE., TRIDF, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            FORWRD = .TRUE.

*  Use a temporary status, so the next compilation test will be made.
         ELSE
            ISTAT = STATUS
            STATUS = SAI__OK
         END IF

*  Now try the inverse mapping, with another new error context.
         CALL ERR_MARK
         CALL TRN_COMP( LOCTR, .FALSE., TRIDI, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            INVERS = .TRUE.

*  Since there was a valid forward mapping, annul the error associated
*  with the bad status.
         ELSE IF ( FORWRD ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF

*  Release the error context associated with the inverse-mapping
*  compilation test.
         CALL ERR_RLSE

*  When there is a valid inverse mapping, annul the error associated
*  with forward mapping.  Release the error context associated with the
*  inverse-mapping compilation test.
         IF ( .NOT. FORWRD ) THEN
            IF ( INVERS ) THEN
               CALL ERR_ANNUL( STATUS )

*  Restore the original bad status.
            ELSE
               STATUS = ISTAT
            END IF
         END IF
         CALL ERR_RLSE
      END IF

*  Write a contextual error report and abort when neither mapping is
*  present or will compile.
      IF ( .NOT. ( FORWRD .OR. INVERS ) ) THEN
         CALL ERR_REP( 'TRANTRACE_NOMAP',
     :     'TRANTRACE: No valid forward or inverse mappings in '/
     :     /'$TRANSFORM.', STATUS )
         GOTO 980
      END IF

*  Report or write the list to a logfile?
*  ======================================
*
*  Open the log file.  If null is returned from the parameter system,
*  the list of transformation properties are reported to the user
*  directly.
      CALL ERR_MARK
      LOGF = .FALSE.
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', BUFLEN, FD, STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGF = .TRUE.
      END IF
      CALL ERR_RLSE

*  Write out the name.
*  ===================

*  Form the output text.
      CALL MSG_LOAD( 'HEADER', '   TRANSFORM structure $TRANSFORM:',
     :               BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
      IF ( LOGF ) THEN
         CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
         CALL FIO_WRITE( FD, ' ', STATUS )
      ELSE
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETC( 'BUF', BUFFER )
         CALL MSG_OUT( 'HEADER', '^BUF', STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

*  Inquire and report the version.
*  ===============================
*
*  Use the named component because there are almost no inquiry routines
*  in TRANSFORM.
      CALL CMP_GET0R( LOCTR, 'TRN_VERSION', VERSIO, STATUS )

*  Write the value to the output parameter.
      CALL PAR_PUT0R( 'VERSION', VERSIO, STATUS )

*  Form the output text.
      CALL MSG_SETR( 'VER', VERSIO )
      CALL MSG_LOAD( 'VERSION2', '      TRANSFORM software version: '/
     :               /'^VER.', BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
      IF ( LOGF ) THEN
         CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
         CALL FIO_WRITE( FD, ' ', STATUS )
      ELSE
         CALL MSG_SETC( 'BUF', BUFFER )
         CALL MSG_OUT( 'VERSION2', '^BUF', STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

*  Obtain the details of the transformation and list them.
*  =======================================================

*  Obtain the number of variables in the transformation.
      CALL TRN_GTNV( LOCTR, NVIN, NVOUT, STATUS )

*  Write the values to the output parameters.
      CALL PAR_PUT0I( 'NVIN', NVIN, STATUS )
      CALL PAR_PUT0I( 'NVOUT', NVOUT, STATUS )

*  Form the output text.
      CALL MSG_SETI( 'NVIN', NVIN )
      CALL MSG_LOAD( 'NVAR_IN', '      Number of input variables:  '/
     :               /'^NVIN.', BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
      IF ( LOGF ) THEN
         CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
      ELSE
         CALL MSG_SETC( 'BUF', BUFFER )
         CALL MSG_OUT( 'NVAR_IN', '^BUF', STATUS )
      END IF

*  Form the output text.
      CALL MSG_SETI( 'NVOUT', NVOUT )
      CALL MSG_LOAD( 'NVAR_OUT', '      Number of output variables: '/
     :               /'^NVOUT.', BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
      IF ( LOGF ) THEN
         CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
         CALL FIO_WRITE( FD, ' ', STATUS )
      ELSE
         CALL MSG_SETC( 'BUF', BUFFER )
         CALL MSG_OUT( 'NVAR_OUT', '^BUF', STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

*  Inquire if there is a CLASSIFICATION structure?
*  ===============================================
      CALL DAT_THERE( LOCTR, 'CLASSIFICATION', CLASPR, STATUS )
      IF ( CLASPR ) THEN

*  Form a list of the true classes of the forward mapping.
*  =======================================================
*
*  There are separate classifications for the forward and inverse
*  mappings.
         IF ( FORWRD ) THEN

*  Obtain the classes for the forward mapping.
            CALL TRN_GTCL( LOCTR, .TRUE., CLASS, STATUS )

*  Form a comma-separated list of the classifications.
            CALL KPS1_TRNCL( CLASS, CLALST, NC, STATUS )

*  Check that the mapping has a class.
            IF ( NC .GE. 0 ) THEN

*  Write the values to the output parameter.
               CALL PAR_PUT0C( 'CLASSFOR', CLALST( :NC ), STATUS )

*  Form the output text.  Use the length as this excludes a trailing
*  comma.  There should be room, but one might feed the list through
*  CHR_PFORM to make a paragraph to ensure this.
               CALL MSG_SETC( 'CLALST', CLALST( :NC ) )
               CALL MSG_LOAD( 'CLASFOR', '      Classification '/
     :           /'(forward mapping): ^CLALST.', BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
               IF ( LOGF ) THEN
                  CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
                  CALL FIO_WRITE( FD, ' ', STATUS )
               ELSE
                  CALL MSG_SETC( 'BUF', BUFFER )
                  CALL MSG_OUT( 'CLASFOR', '^BUF', STATUS )
                  CALL MSG_BLANK( STATUS )
               END IF
            END IF
         END IF

*  Form a list of the true classes of the inverse mapping.
*  =======================================================
*
*  There are separate classifications for the forward and inverse
*  mappings.
         IF ( INVERS ) THEN

*  Obtain the classes for the forward mapping.
            CALL TRN_GTCL( LOCTR, .FALSE., CLASS, STATUS )

*  Form a comma-separated list of the classifications.
            CALL KPS1_TRNCL( CLASS, CLALST, NC, STATUS )

*  Check that the mapping has a class.
            IF ( NC .GE. 0 ) THEN

*  Write the values to the output parameter.
               CALL PAR_PUT0C( 'CLASSINV', CLALST( :NC ), STATUS )

*  Form the output text.  Use the length as this excludes a trailing
*  comma.  There should be room, but one might feed the list through
*  CHR_PFORM to make a paragraph to ensure this.
               CALL MSG_SETC( 'CLALST', CLALST( :NC ) )
               CALL MSG_LOAD( 'CLASINV', '      Classification '/
     :           /'(inverse mapping): ^CLALST.', BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
               IF ( LOGF ) THEN
                  CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
                  CALL FIO_WRITE( FD, ' ', STATUS )
               ELSE
                  CALL MSG_SETC( 'BUF', BUFFER )
                  CALL MSG_OUT( 'CLASINV', '^BUF', STATUS )
                  CALL MSG_BLANK( STATUS )
               END IF
            END IF
         END IF
      END IF

*  Leave an extra blank line to separate the global properies from the
*  individual transformations.
      IF ( LOGF ) THEN
         CALL FIO_WRITE( FD, ' ', STATUS )
      ELSE
         CALL MSG_BLANK( STATUS )
      END IF

*  Find the number of mappings.
*  ============================

*  Get a locator to the MODULE_ARRAY structure.
      CALL DAT_FIND( LOCTR, 'MODULE_ARRAY', LOCMA, STATUS )
      CALL DAT_SIZE( LOCMA, NTRANS, STATUS )

*  This flag is used to judge wthether or not to give the number of
*  variables in the mappings.  For a single transformation, the number
*  of variables given earlier will be the same for the transformation as
*  a whole as for the inidividual mappings.
      INDMAP = NTRANS .GT. 1

*  Loop for all the structures.  Usually, there will be but one set of
*  mappings.
      DO ITRANS = 1, NTRANS

*  Form the output text.
         CALL MSG_SETI( 'ITRANS', ITRANS )
         CALL MSG_LOAD( 'TRANSNO', '   Transformation number: ^ITRANS.',
     :                  BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
         IF ( LOGF ) THEN
            CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
            CALL FIO_WRITE( FD, ' ', STATUS )
         ELSE
            CALL MSG_SETC( 'BUF', BUFFER )
            CALL MSG_OUT( 'MAPPINGNO', '^BUF', STATUS )
            CALL MSG_BLANK( STATUS )
         END IF

*  Get a cell to the individual TRN_MODULE structure.
         CALL DAT_CELL( LOCMA, 1, ITRANS, LOCMOD, STATUS )

*  Obtain and report the number of variables of the current mappings.
*  ==================================================================
         IF ( INDMAP ) THEN

*  Obtain the number of input variables.  This is not optional but
*  in order to exit, search for its existence and flag a missing value.
*  Set tokens for the output.
            CALL DAT_THERE( LOCMOD, 'NVAR_IN', THERE, STATUS )
            IF ( THERE ) THEN
               CALL CMP_GET0I( LOCMOD, 'NVAR_IN', NVIN, STATUS )
               CALL MSG_SETI( 'NVIN', NVIN )
            ELSE
               CALL MSG_SETC( 'NVIN', '<undefined>' )
            END IF

*  Form the output text.  The extra space is deliberate to align with
*  the input variables.
            CALL MSG_LOAD( 'NVAR_IN', '      Number of input '/
     :                     /'variables:  ^NVIN.', BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
            IF ( LOGF ) THEN
               CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
            ELSE
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL MSG_OUT( 'NVAR_IN', '^BUF', STATUS )
            END IF

*  Obtain the number of output variables.  This is not optional but
*  in order to exit, search for its existence and flag a missing value.
*  Set tokens for the output.
            CALL DAT_THERE( LOCMOD, 'NVAR_OUT', THERE, STATUS )
            IF ( THERE ) THEN
               CALL CMP_GET0I( LOCMOD, 'NVAR_OUT', NVOUT, STATUS )
               CALL MSG_SETI( 'NVOUT', NVOUT )
            ELSE
               CALL MSG_SETC( 'NVOUT', '<undefined>' )
            END IF

*  Form the output text.
            CALL MSG_LOAD( 'NVAR_OUT', '      Number of output '/
     :                     /'variables: ^NVOUT.', BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
            IF ( LOGF ) THEN
               CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
               CALL FIO_WRITE( FD, ' ', STATUS )
            ELSE
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL MSG_OUT( 'NVAR_OUT', '^BUF', STATUS )
               CALL MSG_BLANK( STATUS )
            END IF
         END IF

*  Obtain and report the comment of the current mappings.
*  ======================================================

*  Obtain the comment, if present.  Set a token for the output.
         CALL DAT_THERE( LOCMOD, 'COMMENT', THERE, STATUS )
         IF ( THERE ) THEN
            CALL CMP_GET0C( LOCMOD, 'COMMENT', COMENT, STATUS )
            CALL MSG_SETC( 'COM', COMENT )

*  Write the value to the output parameter.
            CALL PAR_PUT0C( 'COMMENT', COMENT, STATUS )

*  Form the output text.
            CALL MSG_LOAD( 'COMENT', '      Comment: ^COM.',
     :                     BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
            IF ( LOGF ) THEN
               CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
               CALL FIO_WRITE( FD, ' ', STATUS )
            ELSE
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL MSG_OUT( 'COMENT', '^BUF', STATUS )
               CALL MSG_BLANK( STATUS )
            END IF
         END IF

*  Obtain and report the precision of the current mappings.
*  =======================================================

*  Obtain the precision.  This is mandatory, so indicate if this is
*  missing.  Set a token for the output.
         CALL DAT_THERE( LOCMOD, 'PRECISION', THERE, STATUS )
         IF ( THERE ) THEN
            CALL CMP_GET0C( LOCMOD, 'PRECISION', PREC, STATUS )
            CALL MSG_SETC( 'PREC', PREC )

*  Write the value to the output parameter.
            CALL PAR_PUT0C( 'PREC', PREC, STATUS )

         ELSE
            CALL MSG_SETC( 'PREC', '<undefined>' )
         END IF

*  Form the output text.
         CALL MSG_LOAD( 'PRECISION', '      Precision: ^PREC.',
     :                  BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
         IF ( LOGF ) THEN
            CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
            CALL FIO_WRITE( FD, ' ', STATUS )
         ELSE
            CALL MSG_SETC( 'BUF', BUFFER )
            CALL MSG_OUT( 'PRECISION', '^BUF', STATUS )
            CALL MSG_BLANK( STATUS )
         END IF

*  Obtain and report the forward mappings.
*  =======================================

*  Write a header.
         CALL MSG_LOAD( 'HEADER2', '      Forward Mappings:',
     :                  BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
         IF ( LOGF ) THEN
            CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
         ELSE
            CALL MSG_SETC( 'BUF', BUFFER )
            CALL MSG_OUT( 'HEADER2', '^BUF', STATUS )
         END IF

*  The forward mappings are mandatory, so indicate if this is missing.
*  Set a token for the output.
         CALL DAT_THERE( LOCMOD, 'FORWARD_FUNC', THERE, STATUS )
         IF ( THERE ) THEN

*  Get a locator to the object.
            CALL DAT_FIND( LOCMOD, 'FORWARD_FUNC', LOCFUN, STATUS )

*  Loop for each of the input variables.
            DO IVAR = 1, NVIN

*  Get an element of the array.  Do it this way to avoid having to
*  declare an arbitrary-length character array.
               CALL DAT_CELL( LOCFUN, 1, IVAR, LOCEXP, STATUS )

*  Obtain the forward mapping expression.
               CALL DAT_GET0C( LOCEXP, FOREXP, STATUS )

*  Set tokens for the output.  Indent the numbered expression(s).
               CALL MSG_SETC( 'EXP', FOREXP )
               CALL MSG_FMTI( 'I', 'I9', IVAR )

*  Form the output text.
               CALL MSG_LOAD( 'FORWARD2', '^I: ^EXP.',
     :                         BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
               IF ( LOGF ) THEN
                  CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
               ELSE
                  CALL MSG_SETC( 'BUF', BUFFER )
                  CALL MSG_OUT( 'FORWARD2', '^BUF', STATUS )
               END IF

*  Free the locator to the array element.
               CALL DAT_ANNUL( LOCEXP, STATUS )
            END DO

*  Write the value to the output parameter.
            CALL PAR_PUT0C( 'FORWARD', FOREXP, STATUS )

*  Annul locator to the array of forward functions.
            CALL DAT_ANNUL( LOCFUN, STATUS )

*  The object could not be found so report it.
         ELSE

*  Form the output text.
            CALL MSG_LOAD( 'FORWARD2', '         <undefined>',
     :                     BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
            IF ( LOGF ) THEN
               CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
            ELSE
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL MSG_OUT( 'FORWARD2', '^BUF', STATUS )
               CALL MSG_BLANK( STATUS )
            END IF
         END IF

*  Leave a blank line at the end of the forward mappings.
         IF ( LOGF ) THEN
            CALL FIO_WRITE( FD, ' ', STATUS )
         ELSE
            CALL MSG_BLANK( STATUS )
         END IF

*  Obtain and report the inverse mappings.
*  =======================================

*  Write a header.
         CALL MSG_LOAD( 'HEADER3', '      Inverse Mappings:',
     :                  BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
         IF ( LOGF ) THEN
            CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
         ELSE
            CALL MSG_SETC( 'BUF', BUFFER )
            CALL MSG_OUT( 'HEADER3', '^BUF', STATUS )
         END IF

*  The inverse mappings are mandatory, so indicate if this is missing.
         CALL DAT_THERE( LOCMOD, 'INVERSE_FUNC', THERE, STATUS )
         IF ( THERE ) THEN

*  Get a locator to the object.
            CALL DAT_FIND( LOCMOD, 'INVERSE_FUNC', LOCFUN, STATUS )

*  Loop for each of the input variables.
            DO IVAR = 1, NVOUT

*  Get an element of the array.  Do it this way to avoid having to
*  declare an arbitrary-length character array.
               CALL DAT_CELL( LOCFUN, 1, IVAR, LOCEXP, STATUS )

*  Obtain the forward mapping expression.
               CALL DAT_GET0C( LOCEXP, INVEXP, STATUS )

*  Set tokens for the output.  Indent the numbered expression(s).
               CALL MSG_SETC( 'EXP', INVEXP )
               CALL MSG_FMTI( 'I', 'I9', IVAR )

*  Form the output text.
               CALL MSG_LOAD( 'INVERSE2', '^I: ^EXP.',
     :                         BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
               IF ( LOGF ) THEN
                  CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
               ELSE
                  CALL MSG_SETC( 'BUF', BUFFER )
                  CALL MSG_OUT( 'INVERSE2', '^BUF', STATUS )
               END IF

*  Free the locator to the array element.
               CALL DAT_ANNUL( LOCEXP, STATUS )
            END DO

*  Write the value to the output parameter.
            CALL PAR_PUT0C( 'INVERSE', INVEXP, STATUS )

*  Annul locator to the array of inverse functions.
            CALL DAT_ANNUL( LOCFUN, STATUS )

         ELSE

*  Form the output text.
            CALL MSG_LOAD( 'INVERSE2', '         <undefined>',
     :                     BUFFER, NC, STATUS )

*  Report the message or write it to the logfile.
            IF ( LOGF ) THEN
               CALL FIO_WRITE( FD, BUFFER( :NC ), STATUS )
            ELSE
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL MSG_OUT( 'INVERSE2', '^BUF', STATUS )
               CALL MSG_BLANK( STATUS )
            END IF
         END IF

*  Leave a blank line at the end of the forward mappings.
         IF ( LOGF ) THEN
            CALL FIO_WRITE( FD, ' ', STATUS )
         ELSE
            CALL MSG_BLANK( STATUS )
         END IF

*  Release the cell locator for the current transformation.
         CALL DAT_ANNUL( LOCMOD, STATUS )
      END DO

*  Annul the locator to the TRN_MODULE.
      CALL DAT_ANNUL( LOCMA, STATUS )

  980 CONTINUE

*  Annul the mapping identifiers.
      IF ( FORWRD ) CALL TRN_ANNUL( TRIDF, STATUS )
      IF ( INVERS ) CALL TRN_ANNUL( TRIDI, STATUS )

*  Annul the locator to the structure.
      CALL DAT_ANNUL( LOCTR, STATUS )

*  Close the file.
      IF ( LOGF ) CALL FIO_ANNUL( FD, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'TRANTRACE_ERR',
     :     'TRANTRACE: Failed to list the transformation structure.',
     :     STATUS )
      END IF

      END
