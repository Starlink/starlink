      SUBROUTINE POLIMP( STATUS )
*+
*  Name:
*     POLIMP

*  Purpose:
*     Copies FITS keyword values into the POLPACK extension.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLIMP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application should be used to prepare data files prior to 
*     processing them with POLPACK. It records the values of various items 
*     of information required by POLPACK (half-wave plate position, filter, 
*     etc). These values can either be supplied explicitly or can be copied 
*     ("imported") from FITS keywords stored in the files. Such keywords 
*     may, for instance, be provided by the instrument/telescope control 
*     systems. The specified values are stored in the POLPACK extensions
*     of the supplied data files for use 
*
*     The import is controlled by a "table" which specifies how FITS
*     keyword values should be used to create the corresponding POLPACK
*     extension items. Each extension item may be assigned a specified 
*     constant value, the value of a specified FITS keyword, or the value
*     of an arbitrary function of several FITS keywords.
*
*     During the processing of data, POLPACK adds items to the POLPACK
*     extension to indicate the state of the processing which has been
*     applied to the data. This routine also allows values to be assigned 
*     to these extra extension items and thus can be used to import partially 
*     processed data. POLIMP can be used in conjunction with POLEXP to
*     allow data to be moved backwards and forwards between POLPACK and
*     other non-NDF based packages.

*  Usage:
*     polimp in table

*  ADAM Parameters:
*     IN = NDF (Read)
*        A group of data files. This may take the form of a comma separated 
*        list of file names , or any of the other forms described in the help 
*        on "Group Expressions".
*     NAMELIST = LITERAL (Read)
*        The name of a file to create containing a list of the successfully 
*        processed data files. This file can be used when specifying the input 
*        data files for subsequent applications. No file is created if a null
*        (!) value is given. [!]
*     QUIET = _LOGICAL (Read)
*        If FALSE, then the values stored in each data file are listed as it is 
*        processed. Otherwise, nothing is written to the screen. [FALSE] 
*     TABLE = LITERAL (Read)
*        The name of the file containing the table describing how FITS
*        keyword values are to be translated into POLPACK extension
*        items. If a null value (!) is supplied, then the following default 
*        table is used which corresponds to the FITS keywords written by 
*        POLEXP:
*
*              ANGROT?  PPCKANGR
*              ANLANG?  PPCKANLA
*              EPS?     PPCKEPS
*              FILTER?  PPCKFILT
*              IMGID?   PPCKIMID
*              RAY?     PPCKRAY
*              STOKES?  PPCKSTOK
*              T?       PPCKT
*              WPLATE?  PPCKWPLT
*              VERSION? PPCKVERS
*
*        See the topic "Table Format" for information on how to create 
*        translation tables. [!]

*  Table Format:
*     The import control (translation) table is an ordinary text file
*     which contains instructions on how to assign values to the components
*     of the POLPACK extension. Constant values specified in
*     the file may be used, or the values may be derived from the values
*     of FITS keywords stored in the FITS extension.
*
*     In its most simple format each line in a FITS control table contains
*     the name of a POLPACK extension item, followed by a constant value 
*     or FITS keyword. This causes the value of the specified FITS keyword
*     or constant, to be assigned to the specified extension item. Some 
*     examples:
*
*        ANGROT             ANROTA
*
*     This copies the value of the FITS keyword ANROTA from the FITS
*     extension to the ANGROT component in the POLPACK extension.
*
*        ANGROT             45.0
*
*     This assigns the value 45.0 to the ANGROT component of the POLPACK 
*     extension.
*    
*        IMGID              "M51_PLATEB"
*
*     This assigns the value M51_PLATE to the IMGID component of the 
*     POLPACK extension. Note, textual constants must be enclosed within 
*     quotes.
*
*     In addition to using the values of FITS keywords directly, it is also
*     possible to use arbitrary functions of one or more keywords. To do
*     this, each keyword used in the function must first be "declared" so
*     that a data type may be associated with it. This is done by including
*     lines with the following form in the control table prior to the
*     function reference:
*
*        Data-type          FITS-keyword
*
*     Here "Data-type" must be one of _INTEGER, _REAL, _DOUBLE, _WORD, _BYTE, 
*     _CHAR. So for instance if you wanted to assign a value to the ANGROT
*     extension item, the orientation of the analyser in degrees, from the 
*     FITS keyword ROTA which gives the required value in radians, you 
*     could use this sequence of commands:
*
*        _REAL             ROTA
*        ANGROT            57.29578*ROTA
*
*     The function may use any of the usual Fortran operators; +, -, *, /, 
*     ** and built-in functions (SIN, COS, TAN, LOG, etc). See SUN/61
*     (appendix A) for complete details.
*
*     Characters strings cannot be manipulated by these functions so two
*     special formats for translating their values are provided.
*     The first form allows for the concatenation of keywords and
*     the second the translation from a known word to another
*     (which is usually one of the POLPACK special names). The
*     concatenation form looks like:
*
*        _INTEGER      OBSNUM
*        _INTEGER      IDATE
*        IMGID        OBSNUM//IDATE
*
*     Which results in the IMGID extension item being set to the
*     concatenation of the values of the FITS keywords OBSNUM and
*     IDATE (you can concatentate more than two values). Note, conversion
*     of numeric values to character strings occurs automatically.
*
*     In the second special form, the name of the destination extension item 
*     is given as usual followed by a FITS-keyword which supplies the string 
*     to be translated. This is then followed by statements which translate 
*     an "input" string into an "output" string. So for instance if you 
*     were doing circular polarimetry, and wanted to translate quarter 
*     waveplate positions to the equivalent strings recognised by POLPACK 
*     you might use something like:
*
*        WPLATE  POLPLATE        48.0=0.0 -
*                                138.0=45.0
*
*     This does a case insensitive comparison between the value of the FITS 
*     keyword POLPLATE and the strings on the left hand sides of the equals 
*     signs ("48.0" and "138.0"), If a match is found (ignoring leading
*     and trailing blanks), it assigns the value from the right hand side 
*     of the equals sign ("0.0" or "45.0") to the WPLATE component in the 
*     POLPACK extension. An error is reported if no match is found. The "-" 
*     sign at the end of the first line indicates that the list continues 
*     on the next line. Note, the FITS keyword value and the supplied test 
*     values are converted to character strings before doing the comparisons. 
*
*     If a control table contains more than one line for an extension
*     item, then each line is processed in turn, replacing any value
*     established by earlier lines. Thus the final value of the extension
*     item will be given by the last line in the table refering to the 
*     extension item.
*
*     If it is not known in advance if the FITS extension will contain the 
*     keyword values needed to assign a value to a particular POLPACK
*     extension item, then a question mark may be appended to the name of
*     the POLPACK extension item. If the required FITS keyword values
*     cannot be found, then the error messages which would normally be 
*     issued are suppressed, and any remaining lines in the control table
*     are processed as normal. If no value has been assigned to the item
*     when the entire table has been processed, then the item will be set
*     to its default value if it has one, or left undefined otherwise (see 
*     below). For instance:
*
*        RAY?  OLDRAY
*        RAY?  PPCKRAY
*
*     causes the POLPACK extension item RAY to be assigned the value of the
*     FITS keyword PPCKRAY if the keyword has a value in the FITS
*     extension. If not, then the FITS keyword OLDRAY is used instead. If
*     this does not exist either, then RAY is left undefined.
*
*     Logical data types are restricted to a single keyword whose value
*     must be "YES", "TRUE", "T", "Y" for TRUE or "NO", "FALSE", "N",
*     "F".
*
*     Fields in the table may be separated by commas if desired, any
*     amount of white space and tabs are also allowed. Comments may be
*     placed anywhere and should start with the characters "#" or "!".
*     Continuation onto a new line is indicated by use of "-". 

*  Examples:
*     polimp in='*' table=mytable.dat
*        This example processes all the data files in the current directory 
*        using the import control table mytable.dat.
*
*     polimp in=^names.lis
*        This example processes the data files listed in the text file
*        "names.lis" using the default control table appropriate for
*        partially processed data which has previously been exported using
*        POLEXP.

*  Notes:
*     -  Any existing values in the POLPACK extension are deleted before 
*     processing the supplied control table.
*     -  A new Frame is added to the WCS component of each NDF and is given the
*     Domain "POLANAL". This Frame is formed by rotating the pixel coordinates 
*     Frame so that the first axis is parallel to the analyser axis. The
*     angle of rotation is given by the ANGROT item in the POLPACK extension
*     and defaults to zero if ANGROT is not specified in the control table.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-DEC-1997 (DSB):
*        Original version, based on CCDPACK:IMPORT
*     2-JUL-1998 (DSB):
*        Default control table changed to make all items optional. Delete
*        POLPACK extensions if an error occurs.
*     1-APR-1999 (DSB):
*        Added VERSION.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'FIO_PAR'          ! FIO parameters
      INCLUDE 'GRP_PAR'          ! GRP parameters

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CELLOC ! Locator to array cell
      CHARACTER * ( DAT__SZLOC ) POLLOC ! Locator to POLPACK extension
      CHARACTER * ( DAT__SZLOC ) FITLOC ! Locator to FITS block
      CHARACTER * ( FIO__SZFNM ) FNAME  ! FITS control table name
      CHARACTER * ( GRP__SZNAM ) NDFNAM ! Name of the NDF being processed
      INTEGER DESGRP( 3 )        ! Destination extension item information
      INTEGER FDIN               ! FIO identifier to input table
      INTEGER FITGRP( 2 )        ! FITS items to be extracted
      INTEGER FITLEN             ! Number of cards in FITS block
      INTEGER IGRP1              ! Input NDF group identifier
      INTEGER IGRP2              ! Id for group of names of NDF's processed OK 
      INTEGER IGRP3              ! Id for group of used IMGID values
      INTEGER INDF               ! NDF identifier
      INTEGER INDEX              ! Loop variable
      INTEGER IPCHR              ! Pointer to integer storage
      INTEGER IPDBLE             ! Pointer to double precision storage
      INTEGER IPFIT              ! Pointer to FITS block
      INTEGER IPINT              ! Pointer to integer storage
      INTEGER IPGOT              ! Pointer to existence flag workspace
      INTEGER IPLOG              ! Pointer to logical workspace
      INTEGER IPREAL             ! Pointer to real storage
      INTEGER LINGRP             ! GRP identifier for line nos in table
      INTEGER NAMLEN             ! Length of name string
      INTEGER NGOOD              ! No. of NDF's processed successfully
      INTEGER NLINES             ! Number of "lines" read from table
      INTEGER NNDF               ! Number of input NDFs
      INTEGER WRDGRP( 3 )        ! GRP identifiers for table "words"
      LOGICAL QUIET              ! Run silently?
      LOGICAL THERE              ! Object exists
      LOGICAL TOPEN              ! Translation table is open
      LOGICAL OK                 ! Obtained value ok
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if we are running quietly.
      CALL PAR_GET0L( 'QUIET', QUIET, STATUS )

*  Input table is not open.
      TOPEN = .FALSE.

*  Access a group of NDFs for processing.
      CALL NDF_BEGIN
      CALL RDNDF( 'IN', 0, 1, '  Give more image names...', IGRP1, 
     :            NNDF, STATUS )

*  Access the control table for items in the FITS block. 
      CALL CCD1_ASFIO( 'TABLE', 'READ', 'LIST', 0, FDIN, TOPEN, STATUS )

*  If successful, get the file name.
      FNAME = '<unknown>'
      IF ( TOPEN ) THEN
         CALL FIO_FNAME( FDIN, FNAME, STATUS )

*  Transform the input table into word separated GRP groups (dynamic
*  string allocation is performed using this method).
         CALL CCD1_CFGRP( FDIN, 3, 2, .FALSE., WRDGRP, LINGRP, STATUS )

*  Translate these groups into groups which contain the keyword and
*  type of any FITS items which should be extracted from the NDFs
*  FITS blocks. Also create groups which describe the destination item
*  its type and the function of fits keywords which result in the value
*  to be stored at this location.
         CALL GRP_GRPSZ( LINGRP, NLINES, STATUS )
         CALL CCD1_MALL( NLINES, '_LOGICAL', IPLOG, STATUS )
         CALL CCD1_FTGRP( WRDGRP, LINGRP, %VAL( IPLOG ), FITGRP,
     :                    DESGRP, STATUS )
         CALL CCD1_MFREE( IPLOG, STATUS )

*  Free table groups.
         CALL GRP_DELET( WRDGRP( 1 ), STATUS )
         CALL GRP_DELET( WRDGRP( 2 ), STATUS )
         CALL GRP_DELET( WRDGRP( 3 ), STATUS )
         CALL GRP_DELET( LINGRP, STATUS )

*  If this section has a bad status, must be to do with the contents of
*  the table. Issue the name of this file.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL ERR_REP( 'POLIMP_BADTAB',
     :      '  Error processing FITS control table: ^FNAME', STATUS )
         END IF

*  If no control table was opened, set up groups representing the default 
*  table.
      ELSE 
         CALL POL1_DEFTB( FITGRP, DESGRP, STATUS )
      END IF

*  Get the number of entries in the FITS groups. Need this to allocate
*  memory for FITS values. Ensure that wqe do not try to allocate zero
*  memory.
      CALL GRP_GRPSZ( FITGRP( 1 ), NLINES, STATUS )
      IF( NLINES .EQ. 0 ) NLINES = 1

*  Allocate memory for _INTEGER, _REAL, _DOUBLE and _LOGICAL types,
*  plus pointers for CHARACTER memory.
      CALL CCD1_MALL( NLINES, '_INTEGER', IPINT, STATUS )
      CALL CCD1_MALL( NLINES, '_REAL', IPREAL, STATUS )
      CALL CCD1_MALL( NLINES, '_DOUBLE', IPDBLE, STATUS )
      CALL CCD1_MALL( NLINES, '_LOGICAL', IPLOG, STATUS )
      CALL CCD1_MALL( NLINES, '_LOGICAL', IPGOT, STATUS )
      CALL CCD1_MALL( NLINES, '_INTEGER', IPCHR, STATUS )

*  Create a group to hold the names of the NDFs which were processed
*  successfully.
      CALL GRP_NEW( 'Good NDFs', IGRP2, STATUS )

*  Tell the user how many NDFs there are to process.
      IF( .NOT. QUIET ) THEN
         IF( NNDF .GT. 1 ) THEN
            CALL MSG_SETI( 'N', NNDF )
            CALL MSG_OUT( ' ', '  ^N input images to process... ',
     :                    STATUS )
         ELSE IF( NNDF .EQ. 1 ) THEN
            CALL MSG_OUT( ' ', '  1 input image to process... ',STATUS )
         ELSE
            CALL MSG_OUT( ' ', '  NO input images to process. ',STATUS )
         END IF
   
         CALL MSG_BLANK( STATUS )
      END IF

*  Check that everything is ok so far.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Initialise the identifier for the group holding used IMGID values.
      IGRP3 = GRP__NOID

*  Process each NDF in turn.
      DO 100 INDEX = 1, NNDF

*  Get the name of the NDF now, while we know that no error has occurred.
         CALL GRP_GET( IGRP1, INDEX, 1, NDFNAM, STATUS )

*  Write out name of this NDF.
         IF( .NOT. QUIET ) THEN
            CALL MSG_SETC( 'CURRENT_NDF', NDFNAM )
            CALL MSG_OUT( ' ', '  Processing ''^CURRENT_NDF''',
     :                     STATUS )
         END IF

*  Get the input NDF identifier
         CALL NDG_NDFAS( IGRP1, INDEX, 'UPDATE', INDF, STATUS )

*  Ensure that the NDF does not already have a POLPACK extension, and
*  then create one.
         CALL NDF_XDEL( INDF, 'POLPACK', STATUS )
         CALL NDF_XNEW( INDF, 'POLPACK', 'POLPACK', 0, 0, POLLOC, 
     :                  STATUS )            

*  Look for a FITS extension in the NDF. Create one if there isn't one
*  already, containing a single END card.
         CALL NDF_XSTAT( INDF, 'FITS', THERE, STATUS )
         IF( THERE ) THEN
            CALL NDF_XLOC( INDF, 'FITS', 'READ', FITLOC, STATUS )
         ELSE
            CALL NDF_XNEW( INDF, 'FITS', '_CHAR*80', 1, 1, FITLOC, 
     :                     STATUS ) 
            CALL DAT_CELL( FITLOC, 1, 1, CELLOC, STATUS )
            CALL DAT_PUT0C( CELLOC, 'END', STATUS )
            CALL DAT_ANNUL( CELLOC, STATUS )
         END IF

*  Map in the fits block of the NDF, if it exists
         CALL DAT_MAPV( FITLOC, '_CHAR*80', 'READ', IPFIT, FITLEN,
     :                  STATUS )
            
*  Now interpret and import the FITS information into the NDF. Note
*  that the lengths of the FITS block character strings are appended
*  after the last genuine argument. This is the usual method in UNIX
*  systems (normally implemented by the compiler), on VMS this makes
*  no difference.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL POL1_IMFIT( FITGRP, DESGRP, INDF, %VAL( IPFIT ),
     :                       FITLEN, %VAL( IPINT ), %VAL( IPREAL ),
     :                       %VAL( IPDBLE ), %VAL( IPLOG ),
     :                       %VAL( IPCHR), %VAL( IPGOT), QUIET, STATUS, 
     :                       %VAL( 80 ) )
         END IF

*  Check the values in the POLPACK extension are usable.
         CALL POL1_CHKEX( INDF, POLLOC, IGRP3, QUIET, STATUS )

*  Unmap FITS block.
         CALL DAT_UNMAP( FITLOC, STATUS )

*  Release the extensions.
         CALL DAT_ANNUL( POLLOC, STATUS )
         CALL DAT_ANNUL( FITLOC, STATUS )

*  If an error occurred, delete any POLPACK extension, and 
*  continue to process the next NDF.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL NDF_XDEL( INDF, 'POLPACK', STATUS )

*  Otherwise, add the name of the NDF to the group of successfully
*  processed NDFs.
         ELSE
            CALL GRP_PUT( IGRP2, 1, NDFNAM, 0, STATUS )
         END IF

*  Release the NDF.
         CALL NDF_ANNUL( INDF, STATUS )

*  Flush any error.
         IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Space the screen output.
         IF( .NOT. QUIET ) CALL MSG_BLANK( STATUS )

 100  CONTINUE

*  Report an error if no NDFs were processed successfully.
      CALL GRP_GRPSZ( IGRP2, NGOOD, STATUS )
      IF( NGOOD .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLIMP_ALLBAD', 'None of the input images '//
     :                 'were processed successfully.', STATUS )
      END IF

*  Write an output list of the NDF names for other applications to use.
      IF ( STATUS .EQ. SAI__OK ) THEN 
         CALL ERR_MARK
         CALL POL1_LNAM( 'NAMELIST', 1, NGOOD, 
     :                   '# POLIMP - NDF name list', IGRP2, .FALSE., 
     :                   STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE
      END IF

*  Break out here if status set BAD.
 99   CONTINUE

*  Release the memory allocations.
      CALL CCD1_MFREE( -1, STATUS )

*  Free GRP groups.
      CALL GRP_DELET( FITGRP( 1 ), STATUS )
      CALL GRP_DELET( FITGRP( 2 ), STATUS )
      CALL GRP_DELET( DESGRP( 1 ), STATUS )
      CALL GRP_DELET( DESGRP( 2 ), STATUS )
      CALL GRP_DELET( DESGRP( 3 ), STATUS )
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )
      IF( IGRP3 .NE. GRP__NOID ) CALL GRP_DELET( IGRP3, STATUS )

*  Close the translation table (if open).
      IF ( TOPEN ) CALL FIO_CLOSE( FDIN, STATUS )

*  Release calibration frame.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLIMP_ERR',
     :   'POLIMP: Error importing FITS information into POLPACK.',
     :   STATUS )
      END IF

      END
