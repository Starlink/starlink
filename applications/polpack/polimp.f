      SUBROUTINE POLIMP( STATUS )
*+
*  Name:
*     POLIMP

*  Purpose:
*     Imports FITS information into a set of POLPACK NDFs.

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
*     This routine imports FITS information into the POLPACK extension
*     of a list of NDFs. It can be used to prepare raw data prior to processing
*     with POLPACK by copying required items of information (wave-plate
*     position, filter, etc) from the FITS headers provided by the 
*     instrument/telescope control systems, to the POLPACK extension.
*     It can also be used to import partially processed data which has 
*     previously been exported using POLEXP.
*
*     The import is controlled by a "table" which specifies how FITS
*     keyword values should be used to create the corresponding POLPACK
*     extension items. This allows the evaluation of functions containing
*     many FITS keywords as well as the translation of arbitrary strings.

*  Usage:
*     polimp in table

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        A list of NDF names. The NDF names should be separated by commas 
*        and may include wildcards.
*     NAMELIST = LITERAL (Read)
*        The name of a file to contain a listing of the name of the
*        succesfully processed NDFs. This is intended to be of use when 
*        using these same names with other applications (such as POLREG). [!]
*     TABLE = LITERAL (Read)
*        The name of the table containing the description of how FITS
*        keyword values are to be translated into POLPACK extension
*        items. See the topic "Table Format" for information on how to
*        create a translation table. If a null parameter value (!) is 
*        supplied the following table is used which corresponds to the FITS
*        keywords written by POLEXP:
*
*           ANGROT      PPCKANGR
*           FILTER      PPCKFILT
*           IMGID       PPCKIMID
*           RAY?        PPCKRAY
*           ROTATION?   PPCKROT
*           WPLATE      PPCKWPLT
*           YROTATION?  PPCKYROT
*        [!]

*  Table Format:
*     The import control (translation) table is an ordinary text file
*     which contains instructions on how to assign values to the components
*     of the POLPACK extension in an NDF. Constant values specified in
*     the file may be used, or the values may be derived from the values
*     of FITS keywords stored in the FITS extension of the NDF.
*
*     In its most simple format a FITS control table is just a series of
*     lines which contain the names of POLPACK extension items and the
*     names of the constant value or FITS keyword to which they map.
*
*        Extension-item     Constant
*        Extension-item     FITS-keyword
*
*     Some examples:
*
*        ANGROT             ANROTA
*
*           This copies the value of the FITS keyword ANROTA from the FITS
*           extension to the ANGROT component in the POLPACK extension.
*
*        ANGROT             45.0
*
*           This assigns the value 45.0 to the ANGROT component of the 
*           POLPACK extension.
*    
*        IMGID              "M51_PLATEB"
*
*           This assigns the value M51_PLATE to the IMGID component of the 
*           POLPACK extension. Note, textual constants must be enclosed
*           within quotes.
*
*     To allow functions of FITS-keywords to be possible a second
*     "declarative" form of statement is necessary.
*
*        _HDS-type          FITS-keyword
*
*     So for instance if you wanted to assign a value to the ANGROT
*     extension item (the orientation of the analyser), from the FITS
*     keyword ROTA which gives the required value in radians, you 
*     could use this sequence of commands:
*
*        _REAL             ROTA
*        ANGROT            57.29578*ROTA
*
*     The "_REAL ROTA" tells this application to find a FITS
*     keyword named ROTA and extract its value as a single precision
*     floating point value. The other available data types are _INTEGER,
*     _DOUBLE, _WORD, _BYTE, _CHAR. The function may use any of the usual 
*     Fortran operators; +, -, *, /, ** and many others that are supported 
*     by the TRANSFORM package (SUN/61).
*
*     Characters strings cannot be manipulated by functions so a single
*     special format for translating their values is provided. The name
*     of the destination extension item is given as usual followed by a 
*     FITS-keyword which supplies the string to be translated. This is then 
*     followed by statements which translate an "input" string into an 
*     "output" string. I.e.
*
*        FITS1 = Ext1 FITS2 = Ext2 FITS3 = Ext3 ... FITSn = Extn
*
*     So for instance if you wanted to translate waveplate positions to those
*     recognised by POLPACK you might use something like.
*
*        WPLATE     WAVEPLT  A=0.0 -
*                            B=22.5 -
*                            C=45.0 -
*                            D=67.5
*
*     This does a case insensitive comparison between the value of the FITS 
*     keyword WAVEPLT and the strings on the left hand sides of the equals 
*     signs ("A", "B", etc), If a match is found, it assigns the value from 
*     the right hand side of the equals sign to the WPLATE component in
*     the POLPACK extension. An error is reported if no match is found.
*     The "-" signs at the end of each line indicate that the list
*     continues on the next line. Note, if specified FITS keyword takes
*     numeric values, then these are converted into textual form before
*     doing the comparisons.
*
*     Logical data types are restricted to a single keyword whose value
*     must be "YES", "TRUE", "T", "Y" for TRUE or "NO", "FALSE", "N",
*     "F".
*
*     Fields in the table may be separated by commas if desired, any
*     amount of white space and tabs are also allowed. Comments may be
*     placed anywhere and should start with the characters "#" or "!".
*     Continuation onto a new line is indicated by use of "-". 
*
*     If it is not known in advance if the FITS extension will contain the 
*     keyword values needed to assign a value to a particular POLPACK
*     extension item, then a question mark may be appended to the name of
*     the POLPACK extension item. If the required FITS keyword values
*     cannot be found, then the error messages which would normally be 
*     issued are suppressed, and the POLPACK extension item is assigned 
*     its default value if it has one, or is left undefined otherwise (see 
*     below). For instance:
*
*        RAY?  PPCKRAY
*
*     causes the POLPACK extension item RAY to be assigned the value of the
*     FITS keyword PPCKRAY if the keyword has a value in the FITS
*     extension. RAY is left undefined otherwise.

*  POLPACK extension items:
*     The POLPACK extension of an NDF may contain the following items.
*     The names and types of the extension items are those as used in
*     import tables. Of these, ANGROT, FILTER, WPLATE and possibly IMGID 
*     are the only ones that most users need be concerned with. Values
*     should be assigned to these extension items before processing of 
*     raw data commences. Their values will often be derived from FITS
*     headers written by the instrument/telescope control system. Values
*     for the remaining extension items are produced by POLPACK as the
*     processing proceeds and need only be included in the control
*     table if you are importing partially processed data.
*
*     Some extension items have default values which are used if the 
*     control table does not specify a value for them. These are
*     indicated in the descriptions below:
*
*        ANGROT (_REAL):  The anti-clockwise angle from the first axis of 
*        the image to the analyser position corresponding to WPLATE=0.0, 
*        in degrees. Defaults to 0.0.
*
*        FILTER (_CHAR):  The filter name. If a value is supplied, then 
*        the value of WPLATE is appended to it (unless the filter already
*        includes the WPLATE value). This value is also copied to the FILTER
*        item in the CCDPACK extension. Defaults to the value of WPLATE.
*
*        IMGID (_CHAR):  An arbitrary textual identifier for each input 
*        image, used to associate corresponding O and E ray images. It must 
*        be unique amongst the input images. Defaults to the name of the
*        input image.
*
*        RAY (_CHAR):  This item should only be specified in the control
*        table if the images being imported are partially processed images 
*        which have been split into separate O and E ray images. It
*        identifies which ray the image is derived from, and can take the 
*        two values "O" and "E" (upper case). Left undefined by default.
*
*        ROTATION (_REAL): This item should only be specified in the 
*        control table if the images being imported are partially processed 
*        images which have already been aligned. It is the clockwise 
*        rotation introduced into the image in order to align it with other 
*        images, in degrees. Left undefined by default.
*
*        WPLATE (_CHAR):  The wave-plate position, in degrees. Must be one 
*        of; "0.0", "22.5", "45.0", "67.5". There is no default (an error 
*        is reported if no value is supplied).
*
*        YROTATION (_REAL): This item should only be specified in the 
*        control table if the images being imported are partially processed 
*        images which have already been aligned, and if the final map may 
*        contain shear. It gives the clockwise rotation in degrees of the 
*        Y axis introduced into the image in order to align it with other 
*        images. In this case the ROTATION item (see above) is understood 
*        as giving the rotation of the X axis. Left undefined by default.

*  Examples:
*     polimp in='*' table=$POLPACK_DIR/WHTSKY.DAT
*        This example shows all the NDFs in the current directory being
*        processed using the import control table $POLPACK_DIR/WHTSKY.DAT.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-DEC-1997 (DSB):
*        Original version, based on CCDPACK:IMPORT
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
      INTEGER NGOOD              ! No. of NDF's processed succesfully
      INTEGER NLINES             ! Number of "lines" read from table
      INTEGER NNDF               ! Number of input NDFs
      INTEGER WRDGRP( 3 )        ! GRP identifiers for table "words"
      LOGICAL TOPEN              ! Translation table is open
      LOGICAL OK                 ! Obtained value ok
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Input table is not open.
      TOPEN = .FALSE.

*  Access a group of NDFs for processing.
      CALL NDF_BEGIN
      CALL RDNDF( 'IN', 0, 1, '  Give more image names...', IGRP1, 
     :            NNDF, STATUS )

*  Access the control table for items in the FITS block.
      CALL CCD1_ASFIO( 'TABLE', 'READ', 'LIST', 0, FDIN, TOPEN, STATUS )
      FNAME = '<unknown>'
      CALL FIO_FNAME( FDIN, FNAME, STATUS )

*  Transform the input table into word separated GRP groups (dynamic
*  string allocation is performed using this method).
      IF ( STATUS .EQ. SAI__OK ) THEN
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
      END IF

*  Create a group to hold the names of the NDFs which were processed
*  succesfully.
      CALL GRP_NEW( 'Good NDFs', IGRP2, STATUS )

*  Tell the user how many NDFs there are to process.
      IF( NNDF .GT. 1 ) THEN
         CALL MSG_SETI( 'N', NNDF )
         CALL MSG_OUT( ' ', '  ^N input images to process... ',STATUS )
      ELSE IF( NNDF .EQ. 1 ) THEN
         CALL MSG_OUT( ' ', '  1 input image to process... ',STATUS )
      ELSE
         CALL MSG_OUT( ' ', '  NO input images to process. ',STATUS )
      END IF

      CALL MSG_BLANK( STATUS )

*  Check that everything is ok so far.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Initialise the identifier for the group holding used IMGID values.
      IGRP3 = GRP__NOID

*  Process each NDF in turn.
      DO 100 INDEX = 1, NNDF

*  Get the name of the NDF now, while we know that no error has occurred.
         CALL GRP_GET( IGRP1, INDEX, 1, NDFNAM, STATUS )

*  Get the input NDF identifier
         CALL NDG_NDFAS( IGRP1, INDEX, 'UPDATE', INDF, STATUS )

*  Write out name of this NDF.
         CALL NDF_MSG( 'CURRENT_NDF', INDF )
         CALL MSG_OUT( ' ', '  Processing ''^CURRENT_NDF''',
     :                  STATUS )

*  Check that the POLPACK extension is present in the NDF. 
*  This creates it if it doesn't exist.
         CALL POL1_CEXT( INDF, .TRUE., 'UPDATE', POLLOC, STATUS )

*  Look for a FITS extension in the NDF.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL NDF_XLOC( INDF, 'FITS', 'READ', FITLOC, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL NDF_MSG( 'NDF', INDF )
               CALL ERR_REP( 'POLIMP_NOFITS',
     :         '  NDF ^NDF has no fits information', STATUS )
            END IF
         END IF

*  Map in the fits block of the NDF.
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
     :                       %VAL( IPCHR), %VAL( IPGOT), STATUS, 
     :                       %VAL( 80 ) )
         END IF

*  Check the values in the POLPACK extension are usable.
         CALL POL1_CHKEX( INDF, POLLOC, IGRP3, STATUS )

*  Unmap FITS block.
         CALL DAT_UNMAP( FITLOC, STATUS )

*  Release this NDF.
         CALL DAT_ANNUL( POLLOC, STATUS )
         CALL DAT_ANNUL( FITLOC, STATUS )
         CALL NDF_ANNUL( INDF, STATUS )

*  If an error occurred, flush the error and continue to process the next
*  NDF.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )

*  Otherwise, add the name of the NDF to the group of succesfully
*  processed NDFs.
         ELSE
            CALL GRP_PUT( IGRP2, 1, NDFNAM, 0, STATUS )
         END IF

         CALL MSG_BLANK( STATUS )

 100  CONTINUE

*  Report an error if no NDFs were processed succesfully.
      CALL GRP_GRPSZ( IGRP2, NGOOD, STATUS )
      IF( NGOOD .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLIMP_ALLBAD', 'None of the input images '//
     :                 'were processed succesfully.', STATUS )
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
      CALL GRP_DELET( IGRP3, STATUS )

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
