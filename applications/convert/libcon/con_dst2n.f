      SUBROUTINE CON_DST2N( FIGFIL, NDFFIL, STATUS )
*+ 
*  Name:
*     CON_DST2N

*  Purpose:
*     Converts a Figaro version 2 file into an NDF (Figaro version 3
*     file)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_DST2N( FIGFIL, NDFFIL, STATUS )

*  Description:
*     This routine reads through a file in Figaro version 2 format.
*     Objects that it finds whose function is understood---that is, 
*     those objects whose purpose has been documented (in the version
*     2 Figaro structures document) will be converted to the 
*     appropriate object in the output file, whose format corresponds 
*     to that described in SGP/38 and which is used by Version 3 of 
*     Figaro.  

*     Non-standard objects will be copied to the .MORE.FIGARO extension.
*     FITS items will be copied to the .MORE.FITS extension.  

*  Arguments:
*     FIGFIL = CHARACTER * ( * ) (Given)
*        The name of the file to be converted. The file name is assumed
*        to be the top level name with an extension '.DST'
*     NDFFIL = CHARACTER * ( * ) (Given)
*        The name of the new format file to be produced. Extension of
*        the new file is '.SDF'.
*     STATUS = INTEGER (Given annd Returned)
*        Global status. If passed as non-zero, this routine returns
*        immediately.  If returned as zero, indicates Conversion
*        performed OK.  Non zero codes will be DSA package error codes.
*        If an error occurs, this routine will have output a detailed
*        description before returning.

*  Format-conversion Rules:
*     .Z.DATA  ->        .DATA_ARRAY
*     .Z.ERRORS ->       .VARIANCE (after processing)
*     .Z.QUALITY ->      .QUALITY.QUALITY (must be BYTE array)
*                        (see Bad-pixel handling below).
*     .Z.LABEL ->        .LABEL
*     .Z.UNITS ->        .UNITS
*     .Z.IMAGINARY ->    .DATA_ARRAY.IMAGINARY_DATA
*     .Z.MAGFLAG ->      .MORE.FIGARO.MAGFLAG
*     .Z.RANGE ->        .MORE.FIGARO.RANGE
*     .Z.xxxx  ->        .MORE.FIGARO.Z.xxxx
*
*     .X.DATA    ->      .AXIS[1].DATA_ARRAY
*     .X.ERRORS  ->      .AXIS[1].VARIANCE  (after processing)
*     .X.WIDTH   ->      .AXIS[1].WIDTH
*     .X.LABEL   ->      .AXIS[1].LABEL
*     .X.UNITS   ->      .AXIS[1].UNITS
*     .X.LOG     ->      .AXIS[1].MORE.FIGARO.LOG
*     .X.xxxx    ->      .AXIS[1].MORE.FIGARO.xxxx
*     (Similarly for .Y .T .U .V or .W structures which are renamed to
*     AXIS[2], ..., AXIS[6] in the NDF.)

*     .OBS.OBJECT   ->   .TITLE
*     .OBS.SECZ    ->    .MORE.FIGARO.SECZ
*     .OBS.TIME    ->    .MORE.FIGARO.TIME
*     .OBS.xxxx    ->    .MORE.FIGARO.OBS.xxxx
*
*     .FITS.xxxx     ->  .MORE.FITS.xxxx (into value part of the string)
*     .COMMENTS.xxxx ->  .MORE.FITS.xxxx ( "   comment "   "   "   "   )
*     .FITS.xxxx.DATA -> .MORE.FITS.xxxx (into value part of the string)
*     .FITS.xxxx.DESCRIPTION -> .MORE.FITS.xxxx

*     .MORE.xxxx    ->   .MORE.xxxx
*                                     ( "   comment "   "   "   "   )
*     .TABLE   ->        .MORE.FIGARO.TABLE
*     .xxxx    ->        .MORE.FIGARO.xxxx

*     -  Axis arrays with dimensionality greater than one are not
*     supported by the NDF.  Therefore, if the application encounters
*     such an axis array, it processes the array using the following
*     rules, rather than those given above.
*
*     .X.DATA    ->      .AXIS[1].MORE.FIGARO.DATA
*                        (AXIS[1].DATA_ARRAY is filled with pixel
*                        co-ordinates)
*     .X.ERRORS  ->      .AXIS[1].MORE.FIGARO.VARIANCE (after
*                        processing)
*     .X.WIDTH   ->      .AXIS[1].MORE.FIGARO.WIDTH

*  Bad-pixel handling:
*     The QUALITY array is only copied if the bad-pixel flag
*     (.Z.FLAGGED) is false or absent.  A simple NDF with the bad-pixel
*     flag set to false (meaning that there are no bad-pixels present)
*     is created when .Z.FLAGGED is absent or false.  Otherwise a
*     primitive NDF, where the data array is at the top level of the
*     data structure, is produced.

*  Authors: 
*     JM: Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
     
*  History:
*     21st June 1988 (JM):
*        Original version.
*     13th November 1990 (JM):
*        Quality structure name and type corrected.
*     4th February 1991 (JM):
*        BADBITS set to 255 rather than 1.  FIGARO.OBS.xxx moved to
*        NDF.MORE.FIGARO.OBS.xxx (rather than NDF.MORE.FIGARO.xxx).
*        This change is made to ensure that the sequence "DST2NDF file"
*        followed by "NDF2DST file" can restore the original file.
*     22nd March 1991 (JM):
*        Figaro .MORE structure copied to .MORE in NDF.
*     23rd July 1991 (JM):
*       Errors are squared to produce Variance (a bug whereby the
*       square root was taken instead was introduced at the last
*       release.  If any axis data is contained in the input Figaro
*       file, then an AXIS(N) structure is created in the output NDF,
*       where N is the dimensionality of the main data array.  If an
*       AXIS(N) structure is created but no actual data array exists
*       for some of the axis structures, an axis data array of the
*       appropriate size containing 0.5,1.5,2.5.... is inserted into
*       those structures without a data array.
*     1992 January 31 (MJC):
*        Fixed bugs in the generation of card images in the FITS
*        extension, notably the inclusion of quotes around character
*        strings.  Made the OBS structure preserve its type, namely
*        OBS, within the NDF.
*     1992 February 4 (MJC):
*        The location of the FITS comment is not hardwired for long
*        (>18) character values.  Improved efficiency by removing
*        unnecessary code from a second-level loop.  Made the maximum
*        number of dimensions 7.  Added many source comments.  Handled
*        axis width as a numeric array, rather than a character scalar.
*     1992 August 15 (MJC):
*        Fixed bug that caused a phantom 2-d FITS structure to be
*        created when there is an empty FITS structure within the DST
*        file. Fixed bug where a created structure was given a type
*        equal to a logical flag STRUCT, rather than the character
*        value 'STRUCT'.  Renamed from DSA_CONVERT_FORMAT.  Converted
*        to SST prologue.
*     1992 September 3 (MJC):
*        Made provision for the FLAGGED component, also affecting
*        whether or not quality is propagated.  Creates simple NDF where
*        required. Fixed another occurrence of STRUCT being used for
*        'STRUCT'.  Convert the MSG_OUTs to error reports.  Reordered
*        the axis validation to before the closedown sequence.
*        Corrected some typo's in the prologue.
*     1992 September 8 (MJC):
*        Fixed bug in the initialisation of the flags indiciating
*        whether an axis data array or Figaro extension are created
*        or not.  Tested for the non-1-dimensional axis arrays.  These
*        are written to the Figaro axis extension.  Handles axis
*        variance in addition to axis errors.
*     1992 September 10 (MJC):
*        Moved special cases of .OBS.SECZ, .OBS.TIME, .Z.MAGFLAG,
*        .Z.RANGE to the top-level Figaro extension as this is where
*        DSA_ now expects to find them in an NDF.
*     1992 September 28 (MJC):
*        Corrected the closedown sequence of DTA-error reporting.
*        Added message tokens for the filenames to clarify some error
*        reports.
*     1992 October 13 (MJC):
*        Fixed a bug where given a DST that has a non-empty FITS
*        structure, and an axis array that is missing from the first or
*        second axis, the corresponding NDF axis-centre array is given
*        a dimension equal to the number of FITS headers.
*     1992 October 21 (MJC):
*        Fixed another couple of bugs along the same lines as above.
*        Now use separate variables for the diferent object dimensions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! ADAM symbolic constants.
      INCLUDE 'DTACODES'         ! Data structure error codes
      INCLUDE 'DYNAMIC_MEMORY'   ! Dynamic memory support (defines %VAL)

*  Arguments Given:
      CHARACTER FIGFIL * ( * )   ! Name of input file
      CHARACTER NDFFIL * ( * )   ! Name of output file

*  Status:
      INTEGER STATUS             ! Global status

*  External Functions:
      INTEGER CHR_LEN            ! Length of a character string ignoring
                                 ! trailing blanks  
*  Local Constants:
      INTEGER NDSTAX             ! Maximum number of axes in a DST.
      PARAMETER ( NDSTAX = 6 )

*  Local Variables:
      LOGICAL   ANAXIS           ! True if a substructure is an axis
      LOGICAL   AXTHER(NDSTAX)   ! TRUE if AXIS(1...6).DATA_ARRAY
                                 ! created
      LOGICAL   AXMFEX(NDSTAX)   ! TRUE if AXIS(1...6).MORE.FIGARO
                                 ! exists
      CHARACTER AXMOR*40         ! AXIS.MORE name
      CHARACTER AXMORF*40        ! AXIS.MORE.FIGARO name
      CHARACTER AXNAME*32        ! Axis structure element name
      CHARACTER AXOUT*32         ! Name of output axis 
      CHARACTER AXOUTD*50        ! Name of output axis data array
      CHARACTER AXOUTV*50        ! Name of output axis variance array
      CHARACTER AXOUTW*50        ! Name of output axis width array
      BYTE      BARRAY(100)      ! Used to read in BYTE type data items
      INTEGER   CDIMS(7)         ! Dimensions of character objects
      CHARACTER COMMENT*50       ! FITS item comment
      DOUBLE PRECISION DARRAY(100) ! Used to read in DP type data items
      INTEGER   DIMS(7)          ! Dimensions of output data
      INTEGER   DSTAT            ! DTA_ routine returned status
      CHARACTER ERROR*64         ! Error description
      LOGICAL   EXIST            ! True if a FITS item has an comment
      REAL      FARRAY(100)      ! Used to read in FLOAT type data items
      INTEGER   FDIMS(2)         ! Dimensions of FITS extension
      CHARACTER FITCOM*80        ! Object containing FITS comment
      CHARACTER FITNAM*90        ! Name of FITS item
      LOGICAL   FITS             ! True is a FITS structure is found
      CHARACTER FITSTR*80        ! Contains FITS string
      CHARACTER FITVAL*72        ! Contains FITS value 
      LOGICAL   FLAGGD           ! True if .Z.DATA contains flagged
                                 ! values
      INTEGER   I                ! Loop variable
      INTEGER   IARRAY(100)      ! Used to read in INTEGER type data
                                 ! items
      INTEGER   IAXIS            ! Loop index through axes
      INTEGER   IERR             ! First element to cause numerical
                                 ! errors
      INTEGER   IFLAG            ! Value of .Z.FLAGGED
      REAL      INCREM           ! Incremental value for creating axes
      INTEGER   IKOUNT           ! Counter
      INTEGER   IPOSN            ! Number of object at first level
      INTEGER   IPOSN1           ! Number of object at second level
      INTEGER   IPTR             ! Pointer to mapped data array
      INTEGER   INQPTR           ! Pointer to output qality array
      INTEGER   K                ! Loop variable
      INTEGER   LENAME           ! Length of name
      CHARACTER LEVEL1*80        ! Full name of environment at 1st level
      CHARACTER LEVEL2*80        ! Full name of environment at 2nd level
      LOGICAL   MORE             ! Determine necessity for .MORE
      LOGICAL   MOREFG           ! Determine necessity for .MORE.FIGARO
      LOGICAL   MORFGO           ! Determine necessity for
                                 ! .MORE.FIGARO.OBS
      LOGICAL   MORFGZ           ! Determine necessity for
                                 ! .MORE.FIGARO.Z
      CHARACTER MORNAM*80        ! Name of item in .MORE structure
      INTEGER   N                ! Loop variable
      CHARACTER NAME*64          ! Name of data object
      CHARACTER NAME1*40         ! 1st level object
      CHARACTER NAME2*40         ! 2nd level object
      CHARACTER NAMOUT*80        ! Name in output structure
      INTEGER   NAXIS            ! Axis number
      INTEGER   NBAD             ! No. of numerical errorsfrom CNV
      INTEGER   NBYTES           ! No. of BYTES
      INTEGER   NDATA            ! No. of data values
      INTEGER   NDIM             ! No of dimensions
      LOGICAL   NEEDAX           ! True if axis data present
      INTEGER   NERR             ! Count of numerical errors 
      INTEGER   NFITS            ! No. of FITS items
      INTEGER   NMSTAT           ! Status from 1st-level call to
                                 ! DTA_NMVAR
      INTEGER   NMSTA1           ! Status from 2nd-level call to
                                 ! DTA_NMVAR
      INTEGER   NMSTA3           ! Status from call to DTA_NMVAR with
                                 ! .MORE
      INTEGER   NSTR             ! Length of string
      INTEGER   NF               ! Length of string
      INTEGER   NVALS            ! No of data values 
      INTEGER   NCC              ! Column from where the comment
                                 ! appears in the FITS card image
      INTEGER   NCOM             ! Length of FITS comment
      LOGICAL   OBOPEN           ! Flags output file as opened
      LOGICAL   OUOPEN           ! Flags output file as opened
      INTEGER   OTQPTR           ! Pointer to output quality array
      LOGICAL   PRIM             ! True if output data array has
                                 ! primitive form (as opposed to simple)
      LOGICAL   QUPRES           ! True if .Z.QUALITY is present
      INTEGER*2 SARRAY(100)      ! Used to read in SHORT type data items
      REAL      START            ! Start value for creating axes arrays
      CHARACTER STRING*64        ! Used for units and labels
      LOGICAL   STRUCT           ! True if item is a structure
      CHARACTER TYPE*16          ! Data object type

*.

*   Return immediately on bad status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Open the files.
*   ===============

*   Open the input file.
      NSTR = CHR_LEN( FIGFIL )
      CALL DTA_ASFNAM( 'INPUT', FIGFIL(:NSTR), 'OLD', 0, FIGFIL, DSTAT )
      IF ( ( DSTAT .NE. 0 ) .AND. ( DSTAT .NE. DTA_EXIST ) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILNAM', FIGFIL )
         CALL ERR_REP( 'DST2NDF_FNF',
     :     'DST2NDF: Unable to open input file ^FILNAM.', STATUS )
         GOTO 500
      END IF
      OBOPEN = .TRUE.

*   Open output file with the extension .SDF.
      NSTR = CHR_LEN( NDFFIL )
      CALL DTA_ASFNAM( 'OUTPUT', NDFFIL( :NSTR ), 
     :                 'NEW', 10, 'NDF', DSTAT )
      IF (DSTAT .NE. 0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILNAM', NDFFIL )
         CALL ERR_REP( 'DST2NDF_FNF',
     :     'DST2NDF: Unable to open output NDF ^FILNAM.', STATUS )
         GOTO 500
      END IF
      OUOPEN = .TRUE.

*  Determine which structures need to be created in the output NDF.
*  ================================================================

*  Before copying anything to the output structure we ascertain 
*  a) what dimensionality is necessary for the OUTPUT.AXIS(N) structure,
*  b) whether or not an OUTPUT.MORE structure is needed, 
*  c) whether or not an OUTPUT.MORE.FITS structure is needed, 
*  d) whether or not an OUTPUT.MORE.FIGARO structure is needed, 
*  e) and whether or not an OUTPUT.DATA_ARRAY structure is needed (in
*  other words determine the form of the output data array).
*
*  Initialise some counters and flags.
      NMSTAT = 0
      IPOSN = 0
      NAXIS = 0
      NEEDAX = .FALSE.
      MORE = .FALSE.
      MOREFG = .FALSE.
      MORFGO = .FALSE.

      DO I = 1, NDSTAX
         AXMFEX( I ) = .FALSE.
         AXTHER( I ) = .FALSE.
      END DO

      MORFGZ = .FALSE.
      FITS = .FALSE.
      NFITS = 0

*   Initialise flags for bad-pixel handling.  Note that Figaro assumes
*   that if the FLAGGED value is absent, that there are no bad values.
*   Therefore, initialise the integer flag value to 0, i.e. false.
      IFLAG = 0
      FLAGGD = .FALSE.
      QUPRES = .FALSE.

*   The input structure is searched - the loop terminates
*   once the DTA_NMVAR call fails and sets NMSTAT to a non-zero
*   value indicating that no more data objects can be found.
      DO WHILE ( NMSTAT .EQ. 0 )
         IPOSN = IPOSN + 1
         IPOSN1 = 0
         NMSTA1 = 0

*      Obtain the name of the IPOSNth object.
         CALL DTA_NMVAR( 'INPUT', IPOSN, NAME1, NMSTAT )

*      Create name of first-level object.
         IF ( NMSTAT .EQ. 0 ) THEN
            CALL DTA_CRNAM( 'INPUT', NAME1, 0, 0, LEVEL1, DSTAT )

*          Test for an axis structure.
            IF (NAME1 .EQ. 'X' .OR. NAME1 .EQ. 'Y' .OR.
     :          NAME1 .EQ. 'T' .OR. NAME1 .EQ. 'U' .OR.
     :          NAME1 .EQ. 'V' .OR. NAME1 .EQ. 'W') THEN

*            If structure is an axis, an axis structure which matches
*            the dimensionality of the main data array must be created.
*            This logical variable is used to flag the need for this 
*            structure.
               NEEDAX = .TRUE.

               ANAXIS = .TRUE.
            ELSE
               ANAXIS = .FALSE.

*            If it is not a standard object, set logicals so that
*            appropriate extensions can be created.

               IF (.NOT. ( NAME1 .EQ. 'Z' .OR. NAME1 .EQ. 'FITS' .OR.
     :              NAME1 .EQ. 'COMMENTS' .OR. NAME1 .EQ. 'OBS' .OR.
     :              NAME1 .EQ. 'MORE')) THEN
                  MORE = .TRUE.
                  MOREFG = .TRUE.

*            Test for the object being the FITS structure or comments.
               ELSE IF ( NAME1 .EQ. 'FITS' .OR.
     :                   NAME1 .EQ. 'COMMENTS' ) THEN

*               Find the number of dimensions and whether or not it is a
*               structure.

                  CALL DTA_SZVAR( LEVEL1, 7, NDIM, DIMS, DSTAT )
                  CALL DTA_STRUC( LEVEL1, STRUCT, DSTAT )

*               Looking at the name is not good enough.  It must be a
*               non-empty scalar structure.
                  IF ( STRUCT .AND. NDIM .EQ. 0 ) THEN
                     CALL DTA_NMVAR( LEVEL1, 1, NAME2, DSTAT )
                     IF ( DSTAT .EQ. 0 ) THEN
                        MORE = .TRUE.
                        FITS = .TRUE.
                     END IF
                  END IF
                  DSTAT = 0

*            Test for the object being the extension structure.
               ELSE IF ( NAME1 .EQ. 'MORE' ) THEN
                  MORE = .TRUE.
               END IF
            END IF

*         Search through the second-level objects.
            DO WHILE ( NMSTA1 .EQ. 0 )
               IPOSN1 = IPOSN1 + 1

*            Obtain the IPOSN1th object's name.
               CALL DTA_NMVAR( LEVEL1, IPOSN1, NAME2, NMSTA1 )
               IF ( NMSTA1 .EQ. 0 ) THEN

*               Create the composite name at level 2.
                  CALL DTA_CRNAM( LEVEL1, NAME2, 0, 0, LEVEL2, DSTAT )

*               Find the form of the NDF.
*               =========================

*               Find which type of NDF form is required by finding out
*               whether bad pixels and/or quality are present in the
*               DST file.

*               Test for the structure containing the data array.
                  IF ( NAME1 .EQ. 'Z' ) THEN

*                  Test for the bad-pixel flag.
                     IF ( NAME2 .EQ. 'FLAGGED' ) THEN

*                     Obtain the value of the bad-pixel flag.
                        CALL DTA_RDVARI( LEVEL2, 1, IFLAG, DSTAT )
                        FLAGGD = IFLAG .NE. 0

*                  Test for the presence of quality.
                     ELSE IF ( NAME2 .EQ. 'QUALITY' ) THEN
                        QUPRES = .TRUE.
                     END IF
                  END IF

*               See which extensions are required.
*               ==================================

*               Check to determine if any second-level objects require
*               OUTPUT.MORE or OUTPUT.MORE.FIGARO.
                  IF ( ( .NOT. MOREFG .OR. .NOT. FITS ) .AND.
     :                   .NOT. ANAXIS ) THEN

*                  Test for the structure containing the data array.
                     IF ( NAME1 .EQ. 'Z' ) THEN

*                     Data structure - only DATA, ERRORS, LABEL, 
*                     UNITS, FLAGGED, QUALITY, and IMAGINARY are
*                     standard objects, anything else goes into
*                     .MORE.FIGARO.
                        IF ( .NOT. ( NAME2 .EQ. 'DATA' .OR.
     :                               NAME2 .EQ. 'ERRORS' .OR.
     :                               NAME2 .EQ. 'LABEL' .OR.
     :                               NAME2 .EQ. 'UNITS' .OR.
     :                               NAME2 .EQ. 'FLAGGED' .OR.
     :                               NAME2 .EQ. 'IMAGINARY'  .OR.
     :                               NAME2 .EQ. 'QUALITY' ) ) THEN

*                        It is not a standard object. Therefore the
*                        MORE.FIGARO structure must be created.
                           MORE = .TRUE.
                           MOREFG = .TRUE.
                        END IF

*                  Test for the OBS structure.
                     ELSE IF ( NAME1 .EQ. 'OBS' ) THEN

*                     Only OBJECT is a standard object in the OBS
*                     structure.
                        IF ( NAME2 .NE. 'OBJECT' ) THEN
                           MORE = .TRUE.
                           MOREFG = .TRUE.
                        END IF
                     ELSE

*                     It is a non-standard object, therefore need to
*                     make MORE.FIGARO.
                        MORE = .TRUE.
                        MOREFG = .TRUE.
                     END IF
                  END IF
                END IF
            END DO
         END IF
      END DO

*   Look for an invalid DST.
      IF ( QUPRES .AND. FLAGGD ) THEN
         CALL MSG_SETC( 'FILE', FIGFIL )
         CALL MSG_OUT( 'INVALID_DST',
     :     'WARNING:  ^FILE is an invalid DST as it contains both '/
     :     /'flagged data values and quality.  This application will '/
     :     /'ignore the quality information.', STATUS )
         QUPRES = .FALSE.
      END IF

*   Determine whether the output NDF has a simple or primitive form.
*   It just depends on whether or not there needs to be a BAD_PIXEL
*   flag in the output NDF.
      PRIM = FLAGGD

*   Create the required structures just identified.
*   ===============================================

*   Test whether or not an axis structure is required.
      IF ( NEEDAX ) THEN

*      Inquire the dimensions of the data array.  The number of axes
*      must equal the dimensionality of the data array.
         CALL DTA_SZVAR( 'INPUT.Z.DATA',7, NDIM, DIMS, DSTAT )
         NAXIS = NDIM

*      Create an axis structure of the appropriate dimensions, first
*      generating the name.
         CALL DTA_CRNAM( 'OUTPUT', 'AXIS', 1, NAXIS, AXNAME, DSTAT )
         CALL DTA_CRVAR( AXNAME, 'AXIS', DSTAT )

*      Report what has happened should something have gone wrong.
         IF ( DSTAT .NE. 0 ) THEN
            STATUS = DSTAT
            CALL ERR_REP( 'DST2NDF_CRAXIS', 
     :        'DST2NDF: Unable to create the output axis structure.', 
     :        STATUS )
            GOTO 500 
         END IF
      END IF

*   Create the .DATA_ARRAY structure, if necessary.
      IF ( .NOT. PRIM ) THEN
         CALL DTA_CRVAR( 'OUTPUT.DATA_ARRAY', 'ARRAY', DSTAT )
      END IF
      IF ( DSTAT .NE. 0 ) THEN
         STATUS = DSTAT
         CALL ERR_REP('DST2NDF_CR1',
     :     'DST2NDF: Unable to create output .DATA_ARRAY structure.',
     :     STATUS)
         GOTO 500 
      END IF

*   Create the .MORE structure, if necessary.
      IF ( MORE ) THEN
         CALL DTA_CRVAR( 'OUTPUT.MORE', 'EXT', DSTAT )
      END IF
      IF ( DSTAT .NE. 0 ) THEN
         STATUS = DSTAT
         CALL ERR_REP('DST2NDF_CR2',
     :     'DST2NDF: Unable to create output .MORE structure.',
     :     STATUS)
         GOTO 500 
      END IF

*   Create the .MORE.FIGARO structure, if necessary.
      IF ( MOREFG ) THEN
         CALL DTA_CRVAR( 'OUTPUT.MORE.FIGARO', 'EXT', DSTAT )
      END IF
      IF ( DSTAT .NE. 0 ) THEN
         STATUS = DSTAT
         CALL ERR_REP('DST2NDF_CR2',
     :     'DST2NDF: Unable to create output .MORE.FIGARO structure.',
     :     STATUS)
         GOTO 500 
      END IF

*   Main loop, processing each component in turn.
*   =============================================

*   Now we attempt to reformat everything else from the input structure
*   according to SGP/38. 
      NMSTAT = 0
      IPOSN = 0
      DO WHILE ( NMSTAT .EQ. 0 )
         IPOSN = IPOSN + 1
         IPOSN1 = 0
         NMSTA1 = 0
         LEVEL2 = ' '

*      Get the name of the the IPOSNth level-one object.
         CALL DTA_NMVAR( 'INPUT', IPOSN, NAME1, NMSTAT )
         IF ( NMSTAT .EQ. 0 ) THEN

*         Generate its full name.
            CALL DTA_CRNAM( 'INPUT', NAME1, 0, 0, LEVEL1, DSTAT )

*         Deal with the Z structure.
*         ==========================
            IF ( NAME1 .EQ. 'Z' ) THEN
               IPOSN1 = IPOSN1 + 1

*            Search through the objects within .Z.
               DO WHILE ( NMSTA1 .EQ. 0 )

*               Get the name of the level-two object.
                  CALL DTA_NMVAR( LEVEL1, IPOSN1, NAME2, NMSTA1 )
                  IF ( NMSTA1 .NE. 0 ) GOTO 111
                  IPOSN1 = IPOSN1 + 1

*               Generate the full name of the component.
                  CALL DTA_CRNAM( LEVEL1, NAME2, 0, 0, LEVEL2, DSTAT )

*               Test for the data array.
*               ========================
                  IF ( NAME2 .EQ. 'DATA' ) THEN

*                  Inquire the data type and dimensions.
                     CALL DTA_TYVAR( LEVEL2, TYPE, DSTAT )
*                     IF ( TYPE .EQ. 'CSTRUCT' ) THEN
                     CALL DTA_SZVAR( LEVEL2, 7, NDIM, DIMS, DSTAT )

*                  Generate the name of the output object.  This will
*                  depend on whether the NDF is simple or primitive. In
*                  the former case the data array lies within the
*                  DATA_ARRAY structure.  The data array .Z.DATA is
*                  copied to .DATA_ARRAY directly or the DATA component
*                  within .DATA_ARRAY.
                     IF ( PRIM ) THEN
                        CALL DTA_CRNAM( 'OUTPUT', 'DATA_ARRAY', NDIM,  
     :                                   DIMS, NAMOUT, DSTAT )
                        CALL DTA_CYVAR( LEVEL2, 'OUTPUT.DATA_ARRAY',
     :                                  DSTAT )
                     ELSE
                        CALL DTA_CRNAM( 'OUTPUT.DATA_ARRAY', 'DATA',
     :                                  NDIM, DIMS, NAMOUT, DSTAT )
                        CALL DTA_CYVAR( LEVEL2, 'OUTPUT.'/
     :                                  /'DATA_ARRAY.DATA', DSTAT )
                     END IF

                     IF ( DSTAT .NE. 0 ) GOTO 400

*                  Add the BAD_PIXEL flag in the simple NDF.
                     IF ( .NOT. PRIM ) THEN
                        CALL DTA_CRVAR( 'OUTPUT.DATA_ARRAY.BAD_PIXEL',
     :                                  '_LOGICAL', DSTAT )
                        CALL DTA_WRVARI( 'OUTPUT.DATA_ARRAY.BAD_PIXEL',
     :                                   1, IFLAG, DSTAT )
                     END IF

*               Test for the errors component.
*               ===============================
                  ELSE IF ( NAME2 .EQ. 'ERRORS' ) THEN

*                  Inquire its type and dimensions.
                     CALL DTA_TYVAR( LEVEL2, TYPE, DSTAT )
                     CALL DTA_SZVAR( LEVEL2, 7, NDIM, DIMS, DSTAT )

*                  Find the number of elements in the error array,
*                  needed because the error values must be processed
*                  to form variances.
                     NDATA = 1
                     DO K = 1, NDIM
                       NDATA = NDATA * DIMS( K )
                     END DO

*                  Copy the error array to the variance.
                     CALL DTA_CYVAR( LEVEL2, 'OUTPUT.VARIANCE', DSTAT )
                     IF ( DSTAT .NE. 0 ) GOTO 400

*                  Map the variance (still errors though).
                     CALL DTA_MUVARF( 'OUTPUT.VARIANCE', NDATA, 
     :                                IPTR, DSTAT )

*                  Report what has happened should something have gone
*                  wrong.
                     IF ( DSTAT .NE. 0 ) THEN
                        STATUS = DSTAT
                        CALL ERR_REP( 'DST2NDF_MAPVAR', 
     :                    'DST2NDF: Error mapping the output variance '/
     :                    /'array.', STATUS )
                        GOTO 500
                     END IF
                     
*                  The errors in INPUT.Z.ERRORS are standard deviations.
*                  Therefore square the values within OUTPUT.VARIANCE.
                     CALL VEC_MULR( .TRUE., NDATA, %VAL( IPTR ), 
     :                              %VAL( IPTR ), %VAL( IPTR ), IERR, 
     :                              NERR, STATUS )

*                  Unmap the variance array.
                     CALL DTA_FRVAR( 'OUTPUT.VARIANCE', DSTAT )

*                  Report what has happened should something have gone
*                  wrong.
                     IF ( DSTAT .NE. 0 ) THEN
                        STATUS = DSTAT
                        CALL ERR_REP( 'DST2NDF_UMPVAR', 
     :                    'DST2NDF: Error unmapping the output '/
     :                    /'variance array.', STATUS )
                        GOTO 500
                     END IF

*               Test for quality.
*               =================
*
*               Only permit quality if there are no flagged data.
                  ELSE IF ( NAME2 .EQ. 'QUALITY' .AND.
     :                      .NOT. FLAGGD ) THEN

*                  Create a structure .QUALITY of type QUALITY.
                     CALL DTA_CRVAR( 'OUTPUT.QUALITY', 'QUALITY',
     :                               DSTAT )  

*                  Create and assign a BADBITS item to indicate that 1
*                  is the bad quality.
                     CALL DTA_CRVAR( 'OUTPUT.QUALITY.BADBITS', 
     :                               'BYTE', DSTAT )  
                     BARRAY( 1 ) = 255
                     CALL DTA_WRVARB ('OUTPUT.QUALITY.BADBITS', 1, 
     :                                BARRAY, DSTAT )

*                  Inquire the type of the input QUALITY array.
                     CALL DTA_TYVAR( LEVEL2, TYPE, DSTAT )

*                  The type must be BYTE.  If it is, merely copy the
*                  QUALITY array to the NDF's QUALITY structure.
                     IF ( TYPE .EQ. 'BYTE' ) THEN
                        CALL DTA_CYVAR( LEVEL2, 'OUTPUT.'/
     :                                  /'QUALITY.QUALITY', DSTAT )
                        IF ( DSTAT .NE. 0 ) GOTO 400

*                  If type is not BYTE, it must be converted.
                     ELSE
                        CALL DTA_SZVAR( LEVEL2, 7, NDIM, DIMS, DSTAT )

*                     Generate a destination quality array of the
*                     required dimensions and BYTE data type.
                        CALL DTA_CRNAM( 'OUTPUT.QUALITY', 'QUALITY', 
     :                                  NDIM, DIMS, NAMOUT, DSTAT )
                        CALL DTA_CRVAR( NAMOUT, 'BYTE', DSTAT )

*                     Find the total number of elements in the quality
*                     array.
                        NDATA = 1
                        DO K = 1, NDIM
                          NDATA = NDATA * DIMS( K )
                        END DO

*                     Map the input quality array.
                        CALL DTA_MRVARB( LEVEL2, NDATA, INQPTR, DSTAT )

*                     Report error conditions.
                        IF ( DSTAT .EQ. DTA_BADCON ) THEN
                           STATUS = DSTAT
                           CALL ERR_REP( 'DST2NDF_CNVQUA', 
     :                       'DST2NDF: Error converting the quality '/
     :                       /'array to BYTE.', STATUS )
                           GOTO 500
                        ELSE IF ( DSTAT .NE. 0 ) THEN
                           STATUS = DSTAT
                           CALL ERR_REP( 'DST2NDF_REAQUA', 
     :                       'DST2NDF: Error reading the input '/
     :                       /'quality array.', STATUS )
                           GOTO 500
                        END IF

*                     Map the output quality array.
                        CALL DTA_MUVARB( 'OUTPUT.QUALITY.QUALITY', 
     :                                   NDATA, OTQPTR, DSTAT )

*                     Report error conditions.
                        IF ( DSTAT .NE. 0 ) THEN
                           STATUS = DSTAT
                           CALL ERR_REP( 'DST2NDF_MAPQUA', 
     :                       'DST2NDF: Error mapping the output '/
     :                       /'quality array.', STATUS )
                           GOTO 500
                        END IF

*                     Copy the quality.
                        NBYTES = NDATA
                        CALL CON_MOVE (NBYTES, %VAL( INQPTR ), 
     :                                 %VAL( OTQPTR ), STATUS )

*                     Unmap the quality arrays.
                        CALL DTA_FRVAR( LEVEL2, DSTAT )
                        CALL DTA_FRVAR( 'OUTPUT.QUALITY.QUALITY', 
     :                                  DSTAT )
                        IF ( DSTAT .NE. 0 ) THEN
                           CALL ERR_REP( 'DST2NDF_UMPQUA ', 
     :                      'DST2NDF: Error unmapping the output '/
     :                      /'quality array.', STATUS )
                           GOTO 500
                        END IF
                     END IF

                     IF ( DSTAT .NE. 0 ) GOTO 400

*               Test for the data label or units.
*               =================================
                  ELSE IF ( NAME2 .EQ. 'LABEL' .OR.
     :                       NAME2 .EQ. 'UNITS' ) THEN

*                  Inquire the dimensions and object name.  Generate the
*                  full component name.
                     CALL DTA_SZVAR( LEVEL2, 7, NDIM, CDIMS, DSTAT )
                     CALL DTA_CRNAM( 'OUTPUT', NAME2, NDIM, CDIMS, 
     :                               NAMOUT, DSTAT )
                     CALL DTA_CRVAR( NAMOUT, 'CHAR', DSTAT )

*                  The .Z.LABEL and .Z.UNITS are copied to .LABEL
*                  and .UNITS in the output structure.  This is done by
*                  reading the value, creating the named object in the
*                  NDF, and writing the value to it.
                     NDATA = CDIMS( 1 )
                     CALL DTA_RDVARC( LEVEL2, NDATA, STRING, DSTAT )
                     CALL DTA_CRNAM( 'OUTPUT', NAME2, 0, 0, 
     :                               NAMOUT, DSTAT )
                     CALL DTA_WRVARC( NAMOUT, NDATA, STRING, DSTAT )

*               Test for the IMAGINARY component.
*               =================================
                  ELSE IF ( NAME2 .EQ. 'IMAGINARY' ) THEN
                     CALL MSG_OUT( ' ', 
     :                 'WARNING: Imaginary data are not copied.', 
     :                 STATUS )
*                     CALL DTA_CYVAR( LEVEL2, 
*     :                             'OUTPUT.DATA_ARRAY.IMAGINARY_DATA', 
*     :                              DSTAT )
                     IF ( DSTAT .NE. 0 ) GOTO 400

*               Ignore FLAGGED as this information has been used
*               already.
                  ELSE IF ( NAME2 .EQ. 'FLAGGED' ) THEN
                     CONTINUE

*               Test for the magnitude flag or the data range.  These
*               are not copied to the .Z structure within the
*               extension, but go in at the top level of the Figaro
*               extension.
                  ELSE IF ( NAME2 .EQ. 'MAGFLAG' .OR.
     :                      NAME2 .EQ. 'RANGE' ) THEN
                     CALL DTA_CYVAR( LEVEL2, 'OUTPUT.MORE.FIGARO.'/
     :                               /NAME2, DSTAT )

*               Must be a non-standard component.
                  ELSE

*                  Create the extension for these non-standard objects,
*                  if not already done so.
                     IF ( .NOT. MORFGZ ) THEN
                        CALL DTA_CRVAR( 'OUTPUT.MORE.FIGARO.Z', 
     :                                  'STRUCT', DSTAT )
                        MORFGZ = .TRUE.
                     END IF                                           

*                  Copy the non-standard object to the FIGARO.Z
*                  extension.
                     CALL DTA_CYVAR( LEVEL2, 
     :                               'OUTPUT.MORE.FIGARO.Z.'//NAME2, 
     :                               DSTAT )
                  END IF
                  IF ( DSTAT .NE. 0 ) GOTO 400
111               CONTINUE
               END DO

*         Test for the OBS structure.
*         ===========================
            ELSE IF ( NAME1 .EQ. 'OBS' ) THEN

*            Loop through all the components of the OBS structure.
               DO WHILE ( NMSTA1 .EQ. 0 ) 
                  IPOSN1 = IPOSN1 + 1

*               Inquire the IPOSN1th object's name.
                  CALL DTA_NMVAR( LEVEL1, IPOSN1, NAME2, NMSTA1 )
                  IF ( NMSTA1 .EQ. 0 ) THEN

*                  Generate the full component name.
                     CALL DTA_CRNAM( LEVEL1, NAME2, 0, 0, LEVEL2,
     :                               DSTAT )

*                  Inquire whether or not the object is a structure.
                     CALL DTA_STRUC( LEVEL2, STRUCT, DSTAT )

*                  INPUT.OBS.OBJECT is copied into OUTPUT.TITLE provided
*                  it is not a structure.
                     IF ( NAME2 .EQ. 'OBJECT' .AND. .NOT. STRUCT ) THEN
                        CALL DTA_CYVAR( LEVEL2, 'OUTPUT.TITLE', DSTAT )
                        IF ( DSTAT .NE. 0 ) GOTO 400

*                  Test for the airmass or time.  These are not copied
*                  to the .OBS structure within the extension, but go in
*                  at the top level of the Figaro extension.
                     ELSE IF ( NAME2 .EQ. 'SECZ' .OR.
     :                         NAME2 .EQ. 'TIME' ) THEN
                        CALL DTA_CYVAR( LEVEL2, 'OUTPUT.MORE.FIGARO.'/
     :                                  /NAME2, DSTAT )
                           
                     ELSE

*                     Create the Figaro OBS extension for other
*                     ancillary data.
                        IF ( .NOT. MORFGO ) THEN
                           CALL DTA_CRVAR( 'OUTPUT.MORE.FIGARO.OBS', 
     :                                     'OBS', DSTAT )
                           MORFGO = .TRUE.
                        END IF

*                     Any other items in the .OBS structure are copied
*                     into the .MORE.FIGARO.OBS structure.
                        CALL DTA_CYVAR( LEVEL2,
     :                                  'OUTPUT.MORE.FIGARO.OBS.'/
     :                                  /NAME2, DSTAT )

                     END IF
                     IF ( DSTAT .NE. 0 ) GOTO 400
                  END IF
               END DO

*         Deal with axis structures.
*         ==========================
            ELSE IF ( NAME1 .EQ. 'X'.OR. NAME1 .EQ. 'Y' .OR.
     :                NAME1 .EQ. 'T'.OR. NAME1 .EQ. 'U' .OR.
     :                NAME1 .EQ. 'V'.OR. NAME1 .EQ. 'W' ) THEN

*            Convert the axis name into an axis dimension.
               IF ( NAME1 .EQ. 'X' ) THEN
                  IAXIS = 1
               ELSE IF ( NAME1 .EQ. 'Y' ) THEN
                  IAXIS = 2
               ELSE IF ( NAME1 .EQ. 'T' ) THEN
                  IAXIS = 3
               ELSE IF ( NAME1 .EQ. 'U' ) THEN
                  IAXIS = 4
               ELSE IF ( NAME1 .EQ. 'V' ) THEN
                  IAXIS = 5
               ELSE IF ( NAME1 .EQ. 'W' ) THEN
                  IAXIS = 6
               END IF

*            Generate the full component name.
               CALL DTA_CRNAM( 'OUTPUT', 'AXIS', 1, IAXIS, AXOUT,
     :                         DSTAT )

*            Loop through all the components of the axis structure.
               DO WHILE ( NMSTA1 .EQ. 0 ) 
                  IPOSN1 = IPOSN1 + 1

*               Inquire the IPOSN1th object's name.
                  CALL DTA_NMVAR( LEVEL1, IPOSN1, NAME2, NMSTA1 )

*               Generate the full component name.
                  CALL DTA_CRNAM( LEVEL1, NAME2, 0, 0, LEVEL2, DSTAT )
                  IF ( NMSTA1 .EQ. 0 ) THEN

*                  Inquire its dimensions.
                     CALL DTA_SZVAR( LEVEL2, 7, NDIM, DIMS, DSTAT )

*                  Deal with the axis centres.
*                  ===========================
                     IF ( NAME2 .EQ. 'DATA' ) THEN

*                     Only 1-d axes are supported within NDFs.
                        IF ( NDIM .EQ. 1 ) THEN

*                        Create the full name of the output axis array.
                           CALL DTA_CRNAM( AXOUT, 'DATA_ARRAY', 0, 0, 
     :                                     AXOUTD, DSTAT )

*                        Copy the axis data array (axis centres).
                           CALL DTA_CYVAR( LEVEL2, AXOUTD, DSTAT )
                           IF ( DSTAT .NE. 0 ) GOTO 400

*                        Record that AXIS(IAXIS).DATA_ARRAY has been 
*                        created.
                           AXTHER( IAXIS ) = .TRUE.

*                     The axes are not 1-dimensional so move them to the
*                     Figaro axis extension.
                        ELSE

*                        Create the Figaro axis extension if not already
*                        present in the NDF, by generating the full
*                        component names of the input and output
*                        objects.
                           IF ( .NOT. AXMFEX( IAXIS ) ) THEN
                              CALL DTA_CRNAM( AXOUT, 'MORE', 0, 0, 
     :                                        AXMOR, DSTAT )
                              CALL DTA_CRVAR( AXMOR, 'STRUCT', DSTAT )
                              CALL DTA_CRNAM( AXMOR, 'FIGARO', 0, 0, 
     :                                        AXMORF, DSTAT )
                              CALL DTA_CRVAR( AXMORF, 'STRUCT', DSTAT )

*                           Record that the OUTPUT.AXIS(n).MORE.FIGARO
*                           structure is created.
                              AXMFEX( IAXIS ) = .TRUE. 
                           END IF

*                        Copy the non-standard data to 
*                        OUTPUT.AXIS(n).MORE.FIGARO.DATA.  Since the
*                        flag to indicate whether or not the NDF's
*                        AXIS(n).DATA_ARRAY has been created is still
*                        false, the NDF axis centres will be filled
*                        with pixel co-ordinates near the end of this
*                        routine.
                           CALL DTA_CRNAM( AXMORF, 'DATA', 0, 0,
     :                                     AXOUTD, DSTAT )
                           CALL DTA_CYVAR( LEVEL2, AXOUTD, DSTAT )
                           IF ( DSTAT .NE. 0 ) GOTO 400
                        END IF

*                  Deal with axis errors.
*                  ======================
                     ELSE IF ( NAME2 .EQ. 'ERRORS' .OR.
     :                         NAME2 .EQ. 'VARIANCE' ) THEN

*                     Find the the total number of elements.  This is
*                     not 1-dimensional because Figaro can have
*                     2-dimensional axis arrays.
                        NDATA = 1
                        DO N = 1, NDIM
                           NDATA = DIMS( N ) * NDATA
                        END DO

*                     Only 1-d axes are supported within NDFs.
                        IF ( NDIM .EQ. 1 ) THEN

*                        Create the full name of the output axis
*                        variance array.
                           CALL DTA_CRNAM( AXOUT, 'VARIANCE', 0, 0, 
     :                                     AXOUTV, DSTAT )

*                     The axis variance array is not 1-dimensional so
*                     move it to the Figaro axis extension.
                        ELSE

*                        Create the Figaro axis extension if not already
*                        present in the NDF, by generating the full
*                        component names of the input and output
*                        objects.
                           IF ( .NOT. AXMFEX( IAXIS ) ) THEN
                              CALL DTA_CRNAM( AXOUT, 'MORE', 0, 0, 
     :                                        AXMOR, DSTAT )
                              CALL DTA_CRVAR( AXMOR, 'STRUCT', DSTAT )
                              CALL DTA_CRNAM( AXMOR, 'FIGARO', 0, 0, 
     :                                        AXMORF, DSTAT )
                              CALL DTA_CRVAR( AXMORF, 'STRUCT', DSTAT )

*                           Record that the OUTPUT.AXIS(n).MORE.FIGARO
*                           structure is created.
                              AXMFEX( IAXIS ) = .TRUE. 
                           END IF

*                        Copy the non-standard variance to 
*                        OUTPUT.AXIS(n).MORE.FIGARO.VARIANCE.
                           CALL DTA_CRNAM( AXMORF, 'VARIANCE', 0, 0,
     :                                     AXOUTV, DSTAT )
                        END IF

*                     Create the output variance array and copy the
*                     errors or variances to it.
                        CALL DTA_CYVAR( LEVEL2, AXOUTV, DSTAT )
                        IF ( DSTAT .NE. 0 ) GOTO 400

*                     Convert standard deviations to variances.
*                     =========================================
                        IF ( NAME2 .EQ. 'ERRORS' ) THEN

*                        Map the variance array.
                           CALL DTA_MUVARF( AXOUTV, NDATA, IPTR, DSTAT )

*                        Report an error condition.
                           IF ( DSTAT .NE. 0 ) THEN
                              STATUS = DSTAT
                              CALL MSG_SETI( 'AXNO', IAXIS )
                              CALL ERR_REP( 'DST2NDF_MAPAVA', 
     :                          'DST2NDF: Error mapping output axis '/
     :                          /'variance array in dimension ^AXNO.', 
     :                          STATUS )
                              GOTO 500
                           END IF

*                        Axis error arrays are converted from standard
*                        deviations to variance in situ.
                           CALL VEC_MULR( .TRUE., NDATA, %VAL( IPTR ), 
     :                                    %VAL( IPTR ), %VAL( IPTR ),
     :                                    IERR, NERR, STATUS)

*                        Unmap the axis variance.
                           CALL DTA_FRVAR( AXOUTV, DSTAT )

*                        Report an error condition.
                           IF ( DSTAT .NE. 0 ) THEN
                              STATUS = DSTAT
                              CALL MSG_SETI( 'AXNO', IAXIS )
                              CALL ERR_REP( 'DST2NDF_UMPAVA', 
     :                          'DST2NDF: Error unmapping output axis '/
     :                          /'variance array in dimension ^AXNO.', 
     :                          STATUS )
                              GOTO 500
                           END IF
                        END IF

*                  Deal with axis width.
*                  =====================

                     ELSE IF ( NAME2 .EQ. 'WIDTH' ) THEN

*                     Only 1-d axes are supported within NDFs.
                        IF ( NDIM .EQ. 1 ) THEN

*                        Create the full name of the output axis array.
                           CALL DTA_CRNAM( AXOUT, 'WIDTH', 0, 0, 
     :                                     AXOUTW, DSTAT )

*                       Copy the axis width array.
                           CALL DTA_CYVAR( LEVEL2, AXOUTW, DSTAT )
                           IF ( DSTAT .NE. 0 ) GOTO 400

*                     The axis widths are not 1-dimensional so move
*                     them to the Figaro axis extension.
                        ELSE

*                        Create the Figaro axis extension if not already
*                        present in the NDF, by generating the full
*                        component names of the input and output
*                        objects.
                           IF ( .NOT. AXMFEX (IAXIS) ) THEN
                              CALL DTA_CRNAM( AXOUT, 'MORE', 0, 0, 
     :                                        AXMOR, DSTAT )
                              CALL DTA_CRVAR( AXMOR, 'STRUCT', DSTAT )
                              CALL DTA_CRNAM( AXMOR, 'FIGARO', 0, 0, 
     :                                        AXMORF, DSTAT )
                              CALL DTA_CRVAR( AXMORF, 'STRUCT', DSTAT )

*                           Record that the OUTPUT.AXIS(n).MORE.FIGARO
*                           structure is created.
                              AXMFEX( IAXIS ) = .TRUE. 
                           END IF

*                        Copy the non-standard widths to 
*                        OUTPUT.AXIS(n).MORE.FIGARO.WIDTH.
                           CALL DTA_CRNAM( AXMORF, 'WIDTH', 0, 0,
     :                                     AXOUTW, DSTAT )
                           CALL DTA_CYVAR( LEVEL2, AXOUTW, DSTAT )
                           IF ( DSTAT .NE. 0 ) GOTO 400
                        END IF

*                  Deal with other standard axis components.
*                  =========================================

*                  LABEL and UNITS are standard objects.
                     ELSE IF ( NAME2 .EQ. 'LABEL' .OR.
     :                         NAME2 .EQ. 'UNITS' ) THEN

*                     Inquire the dimensions of the object.
                        CALL DTA_SZVAR( LEVEL2, 7, NDIM, CDIMS, DSTAT )

*                     Generate the full name of the output component.
                        CALL DTA_CRNAM( AXOUT, NAME2, NDIM, CDIMS, 
     :                                  NAMOUT, DSTAT )

*                     Make the output structure.
                        CALL DTA_CRVAR( NAMOUT, 'CHAR', DSTAT )
                        NDATA = CDIMS( 1 )

*                     The .n.LABEL and .n.UNITS are copied to .LABEL
*                     and .UNITS in the output axis structure.  This is
*                     done by reading the value, creating the named
*                     object in the NDF, and writing the value to it.
                        CALL DTA_RDVARC( LEVEL2, NDATA, STRING, DSTAT )
                        CALL DTA_CRNAM( AXOUT, NAME2, 0, 0, 
     :                                  NAMOUT, DSTAT )
                        CALL DTA_WRVARC( NAMOUT, NDATA, STRING, DSTAT )
                        IF ( DSTAT .NE. 0 ) GOTO 400

*                  Deal with non-standard axis components.
*                  =======================================
                     ELSE

*                     Create the Figaro axis extension if not already
*                     present in the NDF, by generating the full
*                     component names of the input and output objects.
                        IF ( .NOT. AXMFEX( IAXIS ) ) THEN
                           CALL DTA_CRNAM( AXOUT, 'MORE', 0, 0, 
     :                                     AXMOR, DSTAT )
                           CALL DTA_CRVAR( AXMOR, 'STRUCT', DSTAT )
                           CALL DTA_CRNAM( AXMOR, 'FIGARO', 0, 0, 
     :                                     AXMORF, DSTAT )
                           CALL DTA_CRVAR( AXMORF, 'STRUCT', DSTAT )

*                        Record that the OUTPUT.AXIS(n).MORE.FIGARO
*                        structure is created.
                           AXMFEX( IAXIS ) = .TRUE. 
                        END IF

*                     Any non-standard objects are copied to 
*                     OUTPUT.AXIS(n).MORE.FIGARO.
                        CALL DTA_CRNAM( AXMORF, NAME2, 0, 0, NAMOUT, 
     :                                  DSTAT )
                        CALL DTA_CYVAR( LEVEL2, NAMOUT, DSTAT )
                        IF ( DSTAT .NE. 0 ) GOTO 400
                     END IF

                  END IF
               END DO

*         Deal with extensions.
*         =====================
            ELSE IF ( NAME1 .EQ. 'MORE' ) THEN

*            Loop through all the extensions.
               IKOUNT = 0
               DO WHILE ( NMSTA3 .EQ. 0 ) 
                  IKOUNT = IKOUNT + 1

*               Obtain the component's name.
                  CALL DTA_NMVAR( 'INPUT.MORE', IKOUNT, MORNAM, NMSTA3 )

*               Items stored in FIGARO.MORE structure copied to
*               NDF.MORE.
                  IF ( NMSTA3 .EQ. 0 ) THEN
                     CALL DTA_CYVAR( 'INPUT.MORE.'//MORNAM, 
     :                               'OUTPUT.MORE.'//MORNAM, DSTAT )
                  END IF
               END DO
               IF ( DSTAT .NE. 0 ) GO TO 400

*         Deal with the TABLE.
*         ====================
            ELSE IF ( NAME1 .EQ. 'TABLE' ) THEN

*            TABLE used for FIGARO SPIKETRUM routines. Copy this to 
*            .MORE.FIGARO.TABLE extension.
               CALL DTA_CYVAR( 'INPUT.TABLE', 
     :                         'OUTPUT.MORE.FIGARO.TABLE', 
     :                         DSTAT )
               IF ( DSTAT .NE. 0 ) GOTO 400

*         Deal with FITS objects in the FITS and COMMENTS structures.
*         ===========================================================
            ELSE IF ( ( NAME1 .EQ. 'FITS' ) .OR.
     :                ( NAME1 .EQ. 'COMMENTS' ) ) THEN

*            FITS items dealt with below.
               CONTINUE
 
            ELSE

*            Any other data objects are copied into .MORE.FIGARO
               CALL DTA_CYVAR( LEVEL1, 'OUTPUT.MORE.FIGARO.'//NAME1, 
     :                         DSTAT )
               IF ( DSTAT .NE. 0 ) GOTO 400
            END IF
         END IF
      END DO


*     Deal with the FITS structure.
*     =============================

      IF ( .NOT. FITS ) GOTO 500

*   Count the number of FITS items, ending when the list of components
*   has been exhausted.
      NFITS = 0
      IPOSN1 = 1
      DO WHILE ( NMSTA1 .EQ. 0 )
         CALL DTA_NMVAR( 'INPUT.FITS', IPOSN1, NAME2, NMSTA1 )
         IPOSN1 = IPOSN1 + 1
         IF ( NMSTA1 .EQ. 0 ) THEN
            NFITS = NFITS + 1
         END IF
      END DO

*   Create a .MORE.FITS array of 80-character card images.
      FDIMS( 1 ) = 80
      FDIMS( 2 ) = NFITS
      CALL DTA_CRNAM( 'OUTPUT.MORE', 'FITS', 2, FDIMS, NAMOUT, DSTAT )
      CALL DTA_CRVAR( NAMOUT, 'CHAR', DSTAT )

*   Now deal with the FITS items one by one.
      NFITS = 0
      NMSTA1 = 0
      IPOSN1 = 1
      DO WHILE ( NMSTA1 .EQ. 0 )

*      Obtain the IPOSN1th object's name.
         CALL DTA_NMVAR( 'INPUT.FITS', IPOSN1, NAME2, NMSTA1 )
         IPOSN1 = IPOSN1  +  1
         IF ( NMSTA1 .EQ. 0 ) THEN

*         Generate the name of the FITS item.  This can be either a
*         primitive item, or a structure.  If it is a structure, it
*         contains the comment as well as the data.  If it is not, the
*         comment may be in a separate .COMMENTS structure.
            FITNAM = 'INPUT.FITS.'//NAME2
            LENAME = CHR_LEN( FITNAM )
            CALL DTA_STRUC( FITNAM, STRUCT, DSTAT )
            EXIST = DSTAT .EQ. 0
            NFITS = NFITS + 1      
            IF ( EXIST ) THEN
               IF ( STRUCT ) THEN
                  NAME = FITNAM( :LENAME )//'.DATA'
               ELSE
                  NAME = FITNAM
               END IF
               CALL DTA_TYVAR( NAME, TYPE, DSTAT )
               EXIST = DSTAT.EQ.0
            END IF

*         Now read the comment associated with the object, if any.
*         FITS items may be stored with the values in the .FITS
*         structure and comments in a separate .COMMENTS structure.
*         Alternatively, both may be in the FITS structure, with the
*         values in the .FITS.DATA structure and the comments in
*         .FITS.DESCRIPTION.
 
            LENAME = CHR_LEN( FITNAM )
            IF (STRUCT) THEN
               FITCOM = FITNAM( :LENAME )//'.DESCRIPTION'
            ELSE
               FITCOM = 'INPUT.COMMENTS.'//NAME2
            END IF
            CALL DTA_RDVARC( FITCOM, 50, COMMENT, DSTAT )
            IF ( DSTAT .NE. 0 ) COMMENT = ' '

*         Initialise the FITS value as part of it may only be
*         overwritten otherwise.
            FITVAL = ' '

*         Read the FITS item value. Each type possibility must be
*         catered for separately. The item value is then converted
*         into a character string.  Note that numeric and logical
*         types are right justified to 20 characters.  Character
*         values may be longer, and are left justified.
            NDATA = 1
            NVALS = 1
            IF ( TYPE .EQ. 'BYTE' ) THEN    
               CALL DTA_RDVARB( NAME, NDATA, BARRAY, DSTAT )
               IF ( DSTAT .NE. 0 ) GOTO 450
               CALL CNV_FMTCNV( TYPE, 'CHAR', BARRAY, FITVAL( :20 ),
     :                          NVALS, NBAD ) 

            ELSE IF ( TYPE .EQ. 'CHAR' ) THEN    
               CALL DTA_RDVARC( NAME, FDIMS( 1 ), FITVAL, DSTAT )
               IF ( DSTAT .NE. 0 ) GOTO 450

            ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN    
               CALL DTA_RDVARD( NAME, NDATA, DARRAY, DSTAT )
               IF ( DSTAT .NE. 0 ) GOTO 450
               CALL CNV_FMTCNV( TYPE, 'CHAR', DARRAY, FITVAL( :20 ),
     :                          NVALS, NBAD )

            ELSE IF ( TYPE .EQ. 'FLOAT' ) THEN    
               CALL DTA_RDVARF( NAME, NDATA, FARRAY, DSTAT )
               IF ( DSTAT .NE. 0 ) GOTO 450
               CALL CNV_FMTCNV( TYPE, 'CHAR', FARRAY, FITVAL( :20 ),
     :                          NVALS, NBAD )

            ELSE IF ( TYPE .EQ. 'INT' ) THEN    
               CALL DTA_RDVARI( NAME, NDATA, IARRAY, DSTAT )
               IF ( DSTAT .NE. 0 ) GOTO 450
               CALL CNV_FMTCNV( TYPE, 'CHAR', IARRAY, FITVAL( :20 ),
     :                          NVALS, NBAD )

            ELSE IF ( TYPE .EQ. 'SHORT' ) THEN    
                CALL DTA_RDVARS( NAME, NDATA, SARRAY, DSTAT )
                IF ( DSTAT .NE. 0 ) GOTO 450
                CALL CNV_FMTCNV( TYPE, 'CHAR', SARRAY, FITVAL( :20 ),
     :                           NVALS, NBAD )
            END IF

*         Initialise the FITS card-image character string.
            FITSTR = ' '

*         Format the FITS character string or "card image".  It is
*         composed of the following items.
*           o  A keyword occupies the first eight columns.
*           o  An equals sign and a following blank go into spaces 8
*              and 9.
*           o  The remaining spaces up to space 31 are used for the
*              value of the FITS item.  Hence the length of FITVAL is
*              20 (it used to be variable STRING*64), except for
*              character strings which may fill the card.
*              -  Character type items are left justified and other
*                 types are right justified.
*              -  Character items are enclosed in quotes and must be at
*                 least 8 characters long.  Given the comments, it is
*                 limited to 18 characters.
*           o  The comment delimiter is in column 32, and comments start
*              at column 34.
*           o  Columns 31 and 33 are spaces.

*         Find the lengths of the value and keyword.
            NSTR = CHR_LEN( FITVAL )
            NF = CHR_LEN( NAME2 )

*         Start to build the FITS card image.
            FITSTR( 1:NF ) = NAME2( 1:NF )
            FITSTR( 9:10 ) = '= '

*         Insert the value strings.
            IF ( TYPE .EQ. 'CHAR' ) THEN

*            Constrain the length of the character value.
               NSTR = MIN( 68, MAX( 8, NSTR ) )

*            Insert the leading quote, the value, and then the trailing
*            quote.
               FITSTR( 11:11 ) = ''''
               FITSTR( 12:11 + NSTR ) = FITVAL( 1:NSTR )
               FITSTR( NSTR+12:NSTR+12 ) = ''''
            ELSE

*            Insert the non-character value, right justified.
               FITSTR( 31-NSTR:30 ) = FITVAL( 1:NSTR )

*            By definition the length of the value must be 20.
               NSTR = 20
            END IF                

*         The backslash to separate the item value from the comment and
*         a following blank are inserted after the value if there is
*         room. It must have capacity to write at least four characters
*         of comment plus 3 for the comment delimiter and the spaces
*         bracketing it.
            IF ( NSTR .LE. 61 ) THEN

*            For numeric data the delimiter will occur in column 32.
               IF ( TYPE .NE. 'CHAR' ) THEN
                  NCC = 32

*            The furthest left the delimiter can go is column 32,
*            and therefore the comment must come after column 33.
               ELSE
                  NCC = MAX( 32, NSTR + 14 )
               END IF

*            Write the delimiter to the card image.
               FITSTR( NCC:NCC+1 ) = '/ '

*            The comment itself is copied into the remaining space,
*            provided there is a comment.
               NCOM = CHR_LEN( COMMENT )
               IF ( NCOM .GT. 0 ) THEN
                  NCOM = MAX( CHR_LEN( COMMENT ), 79 - NCC )
                  FITSTR( NCC + 2:NCC + 1 + NCOM ) = COMMENT( 1:NCOM )
               END IF
            END IF

*         Obtain the name of the FITS extension.  (Why is this in the
*         loop?---MJC.)
            FDIMS( 1 ) = 1
            FDIMS( 2 ) = NFITS
            CALL DTA_CRNAM( 'OUTPUT.MORE', 'FITS', 2, FDIMS, 
     :                      NAMOUT, DSTAT )

*         Write the FITS card image to the FITS extension.
            CALL DTA_WRVARC( NAMOUT, 80, FITSTR, DSTAT )
            IF ( DSTAT .NE. 0 ) GOTO 500
         END IF
      END DO

*   Validate the axis structure.
*   ============================

*   Make sure that an axis data array is present for each axis
*   structure in the output NDF. If none is present an array filled
*   with 0.5,1.5,2.5... is created.  Re-obtain the dimensions of the
*   data array.  Output file cannot be used in case the NDF has the
*   simple form.
      CALL DTA_SZVAR( 'INPUT.Z.DATA', 7, NDIM, DIMS, DSTAT )
      IF ( NEEDAX .AND. NAXIS .GT. 1 ) THEN
         DO IAXIS = 1, NAXIS

*         Check if axis data array has been created for each axis.
            IF ( .NOT. AXTHER( IAXIS ) ) THEN

*            Generate the full name of the output axis data-array
*            structure.
               CALL DTA_CRNAM( 'OUTPUT', 'AXIS', 1, IAXIS, AXOUT,
     :                         DSTAT )
               CALL DTA_CRNAM( AXOUT, 'DATA_ARRAY', 1, DIMS( IAXIS ), 
     :                         AXOUTD, DSTAT )

*            Create an axis data array of the appropriate size.
               CALL DTA_CRVAR( AXOUTD, 'FLOAT', DSTAT )

*            Generate the full name of the output axis data-array
*            primitive component.
               CALL DTA_CRNAM( AXOUT, 'DATA_ARRAY', 0, 0, 
     :                         AXOUTD, DSTAT )

*            Map the newly created structure for update.
               CALL DTA_MUVARF( AXOUTD( 1:CHR_LEN( AXOUTD ) ),
     :                          DIMS( IAXIS ), IPTR, DSTAT )

*            Report an error condition.
               IF ( DSTAT .NE. 0 ) THEN
                  STATUS = DSTAT
                  CALL MSG_SETI( 'AXNO', IAXIS )
                  CALL ERR_REP( 'DST2NDF_MAPACE', 
     :              'DST2NDF: Error mapping the output array of axis '/
     :              /'centres in dimension ^AXNO.', STATUS )
                  GOTO 500
               END IF

*            Fill array with 0.5,1.5...
               START = 0.5
               INCREM = 1.0
               CALL CON_FILL( DIMS( IAXIS ), START, INCREM,
     :                        %VAL( IPTR ), STATUS )

*            Unmap the data array.
               CALL DTA_FRVAR( AXOUTD, DSTAT )

*            Report an error condition.
               IF ( DSTAT .NE. 0 ) THEN
                  STATUS = DSTAT
                  CALL MSG_SETI( 'AXNO', IAXIS )
                  CALL ERR_REP( 'DST2NDF_UMPACE', 
     :              'DST2NDF: Error unmapping the output array of '/
     :              /'axis centres in dimension ^AXNO.', STATUS )
                  GOTO 500
               END IF
                     
            END IF
         END DO
      END IF

      GOTO 500

  400 CONTINUE
      STATUS = DSTAT
      IF ( LEVEL2 .EQ. ' ' ) THEN
         CALL MSG_SETC( 'LEVEL', LEVEL1 )
      ELSE
         CALL MSG_SETC( 'LEVEL', LEVEL2 )
      END IF
      CALL ERR_REP( 'DST2NDF_CP1',
     :  'DST2NDF: Error copying ^LEVEL.', STATUS )
      GOTO 500
  450 CONTINUE
      STATUS = DSTAT
      CALL ERR_REP( 'DST2NDF_READER',
     :  'DST2NDF: Error reading '//NAME//'.', STATUS )
  500 CONTINUE

*   Close down everything.
*   ======================

*   Report any DTA errors.
      IF ( DSTAT .NE. 0 ) THEN
         IF ( STATUS .EQ. SAI__OK ) STATUS = DSTAT
         CALL DTA_ERROR( DSTAT, ERROR )
         CALL ERR_REP( 'CON_DST2N_DTAERR', ERROR, STATUS )
         DSTAT = 0
      END IF

*   Close down the input and output files.
      IF ( OUOPEN ) CALL DTA_FCLOSE( 'OUTPUT', DSTAT )
      IF ( OBOPEN ) CALL DTA_FCLOSE( 'INPUT', DSTAT )

      IF ( DSTAT .NE. 0 ) THEN
         IF ( STATUS .EQ. SAI__OK ) STATUS = DSTAT
         CALL DTA_ERROR( DSTAT, ERROR )
         CALL ERR_REP(  'CON_DST2N_DTAERR', ERROR, STATUS )
      END IF

      END
