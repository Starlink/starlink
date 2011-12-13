      SUBROUTINE IMPORT( STATUS )
*+
*  Name:
*     IMPORT

*  Purpose:
*     Imports FITS information into CCDPACK image extensions.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IMPORT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine imports FITS information into the CCDPACK extension
*     of a list of images. FITS information (probably provided by the
*     instrument/telescope control systems) can be used to specify
*     certain parameters which are required by CCDPACK to perform
*     "automated" reductions. These might cover such items as the type
*     of data (target, flatfield, bias frame etc.), the
*     Analogue-to-Digital Conversion factor, the nominal readout noise,
*     the position of any bias strips (over-scan regions) etc.
*
*     The import is controlled by a "table" which specifies how FITS
*     keyword values should be interpreted. This allows the evaluation
*     of functions containing many FITS keywords as well as the mapping
*     of CCDPACK recognised character items to arbitrary strings.

*  Usage:
*     import in table

*  ADAM Parameters:
*     IN = LITERAL (Read)
*        A list of image names which contain the raw bias frame data.
*        The image names should be separated by commas and may include
*        wildcards.
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     NAMELIST = LITERAL (Read)
*        The name of a file to contain a listing of the name of the
*        input images. This is intended to be of use when using these
*        same names with other applications (such as SCHEDULE).
*        [!]
*     TABLE = LITERAL (Read)
*        The name of the table containing the description of how FITS
*        keyword values are to be translated into CCDPACK extension
*        items. See the topic "Table Format" for information on how to
*        create a translation table.
*        ['import.tab']

*  Examples:
*     import in='*' table=$CCDPACK_DIR/WHTSKY.DAT
*        This example shows all the images in the current directory being
*        processed using the import control table $CCDPACK_DIR/WHTSKY.DAT.

*  CCDPACK Extension Items:
*     The CCDPACK extension of an image may contain the following items.
*     The names and types of the extension items are those as used in
*     import tables. More complete descriptions of the items can be
*     found with the applications that use these values.
*
*        Name            HDS data type    Description
*
*        ADC             _DOUBLE          The analogue to digital
*                                         conversion factor.
*        BOUNDS.END1     _INTEGER         The end row or column of the
*                                         first bias strip region.
*        BOUNDS.END2     _INTEGER         The end row or column of the
*                                         second bias strip region.
*        BOUNDS.START1   _INTEGER         The first row or column of the
*                                         first bias strip region.
*        BOUNDS.START2   _INTEGER         The first row or column of the
*                                         second bias strip region.
*        DEFERRED        _DOUBLE          The deferred charge.
*        DIRECTION       _CHAR            The "readout" direction (X or Y).
*        EXTENT.MAXX     _INTEGER         Maximum X coordinate of useful
*                                         region.
*        EXTENT.MAXY     _INTEGER         Maximum Y coordinate of useful
*                                         region.
*        EXTENT.MINX     _INTEGER         Minimum X coordinate of useful
*                                         region.
*        EXTENT.MINY     _INTEGER         Minimum Y coordinate of useful
*                                         region.
*        FILTER          _CHAR            Filter name.
*        FTYPE           _CHAR            Frame type (TARGET, BIAS, FLAT,
*                                         DARK or FLASH)
*        RNOISE          _DOUBLE          Readout noise (ADUs)
*        SATURATION      _DOUBLE          Pixel saturation count.
*        TIMES.DARK      _DOUBLE          Dark count time.
*        TIMES.FLASH     _DOUBLE          Pre-flash time.

*  Table Format:
*     The import control (translation) table is an ordinary text file
*     which contains instructions on how to transfer FITS information
*     from the FITS extension to the CCDPACK extension of an image.
*     "Translation" is required since no standard interpretation of
*     FITS keywords can be made and because the items which may be
*     required can be compounds of single FITS keyword values.
*
*     In its most simple format a FITS control table is just a series of
*     lines which contain the names of CCDPACK extension items and the
*     names of the FITS keywords to which they map:
*
*        Extension-item     FITS-keyword
*
*     If the HDS type of the destination Extension-item is known this
*     may be included:
*
*        Extension-item     _HDS-type     FITS-keyword
*
*     Normally this is inferred. This is the format used by the KAPPA
*     application FITSIMP (as of KAPPA version 0.8-6U). Extension items
*     are the names of CCDPACK items (such as FTYPE, FILTER etc.).
*     These may be heirarchical, e.g. TIMES.DARK. Note that they exclude the
*     "NDF_NAME.MORE.CCDPACK." part of the extension path name.
*
*     To allow functions of FITS-keywords to be possible a second
*     "declarative" form of statement is necessary:
*
*        _HDS-type          FITS-keyword
*
*     So for instance if you wanted to derive an exposure time for an
*     observation which was given in milliseconds and which you wanted
*     to convert into seconds you would use this sequence of commands:
*
*        _INTEGER          EXPOSURE
*        TIMES.DARK        _DOUBLE    1000.0D0*EXPOSURE
*
*     The "_INTEGER EXPOSURE" tells this application to find a FITS
*     keyword of EXPOSURE and extract its value as an integer.  If you
*     wanted to estimate the dark time from a knowledge of the start
*     and end times (TAI0 and TAI1):
*
*        _DOUBLE        TAI0
*        _DOUBLE        TAI1
*        TIMES.DARK    _DOUBLE       (TAI1-TAI0)
*
*     The function may use any of the usual Fortran operators; +, -, *,
*     /, ** and many others that are supported by the TRANSFORM package
*     (SUN/61).
*
*     Functions are allowed to not contain any FITS-keywords in which
*     case the extension item will be assigned to the value, so for
*     instance numerical constants may be given:
*
*        EXTENT.MINX   _INTEGER       1
*        EXTENT.MINY   _INTEGER       1
*
*     In this way import tables could actually be used to set all the
*     required values in the CCDPACK extension (but see PRESENT also).
*
*     Characters strings cannot be manipulated by functions so two
*     special formats for translating their values are provided.
*     The first form allows for the concatenation of keywords and
*     the second the translation from a known word to another
*     (which is usually one of the CCDPACK special names). The
*     concatenation form looks like:
*
*        _CHAR         FILTER
*        _CHAR         HWP
*        FILTER        _CHAR          FILTER//HWP
*
*     Which results in the FILTER extension item being set to the
*     concatenation of the values of the FITS keywords FILTER and
*     HWP (you can concatentate more than two values).
*
*     In the second special form the name of the destination extension
*     item and (optionally) its type are given as usual followed by a
*     FITS-keyword which contains the string to be translated. This is
*     then followed by statements which translate an "input" string
*     into an "output" string. I.e.
*
*        FITS1 = Ext1 FITS2 = Ext2 FITS3 = Ext3 ... FITSn = Extn
*
*     So for instance if you wanted to translate frame types to those
*     recognised by CCDPACK you might use something like.
*
*        FTYPE    _CHAR   OBSTYPE  OBJECT=TARGET -
*                                  FF=FLAT -
*                                  ZERO=BIAS
*
*     Which would compare the value of the FITS-keyword OBSTYPE with
*     the strings "OBJECT", "FF" and "ZERO" (case insensitive) and
*     convert these into the values in the right-hand side of the equals
*     sign.
*
*     Logical data types are restricted to a single keyword whose value
*     must be "YES", "TRUE", "T", "Y" for TRUE or "NO", "FALSE", "N",
*     "F".
*
*     The FITS keywords may be hierarchical, and on the whole are
*     specified simply by giving their name in the normal way.
*     However, there is one special case: if the value of a FITS
*     header is known to be a string of the form '[A:B,C:D]' the
*     numbers A, B, C and D may be extracted individually by appending
*     '<X1>', '<X2>', '<Y1>' or '<Y2>' respectively to the name of
*     the keyword.  Hence:
*
*        EXTENT.MINX   TRIMSEC<X1>
*        EXTENT.MAXX   TRIMSEC<X2>
*
*     would set the extents from the first two fields of a suitably
*     formatted TRIMSEC header.
*
*     Fields in the table may be separated by commas if desired, any
*     amount of white space and tabs are also allowed. Comments may be
*     placed anywhere and should start with the characters "#" or "!".
*     Continuation onto a new line is indicated by use of "-".

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997-1998, 2000 Central Laboratory of the
*     Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-DEC-1993 (PDRAPER):
*        Original version.
*     12-SEP-1995 (PDRAPER):
*        Added more prologue.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator control (foreign data access upgrade).
*     12-SEP-1997 (PDRAPER):
*        Added the concatenate option for character import.
*     29-JUL-1998 (PDRAPER):
*        Added missing CCD1_END call.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     3-AUG-2000 (MBT):
*        Added ability to index into [X1:X2,Y1:Y2] FITS header values,
*        and fixed handling of hierarchical FITS keywords.
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
      INCLUDE 'GRP_PAR'          ! GRP system constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CCDLOC ! Locator to CCDPACK extension
      CHARACTER * ( DAT__SZLOC ) FITLOC ! Locator to FITS block
      CHARACTER * ( FIO__SZFNM ) FNAME ! FITS control table name
      CHARACTER * ( FIO__SZFNM ) NDFNAM ! Name of the NDF being processed
      INTEGER DESGRP( 3 )        ! Destination extension item information
      INTEGER FDIN               ! FIO identifier to input table
      INTEGER FITGRP( 2 )        ! FITS items to be extracted
      INTEGER FITLEN             ! Number of cards in FITS block
      INTEGER GIDIN              ! Input NDF group identifier
      INTEGER IDIN               ! NDF identifier
      INTEGER INDEX              ! Loop variable
      INTEGER IPCHR              ! Pointer to integer storage
      INTEGER IPDBLE             ! Pointer to double precision storage
      INTEGER IPFIT              ! Pointer to FITS block
      INTEGER IPINT              ! Pointer to integer storage
      INTEGER IPLOG              ! Pointer to logical workspace
      INTEGER IPREAL             ! Pointer to real storage
      INTEGER LINGRP             ! GRP identifier for line nos in table
      INTEGER NAMLEN             ! Length of name string
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

*  Startup logging and write task introduction.
      CALL CCD1_START( 'IMPORT', STATUS )

*  Access a group of NDFs for processing.
      CALL NDF_BEGIN
      CALL CCD1_NDFGR( 'IN', GIDIN, NNDF, STATUS )

*  Access the control table for items in the FITS block.
      CALL CCD1_ASFIO( 'TABLE', 'READ', 'LIST', 0, FDIN, TOPEN, STATUS )
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
         CALL CCD1_FTGRP( WRDGRP, LINGRP, %VAL( CNF_PVAL( IPLOG ) ),
     :                    FITGRP, DESGRP, STATUS )
         CALL CCD1_MFREE( IPLOG, STATUS )

*  Free table groups.
         CALL CCD1_GRDEL( WRDGRP( 1 ), STATUS )
         CALL CCD1_GRDEL( WRDGRP( 2 ), STATUS )
         CALL CCD1_GRDEL( WRDGRP( 3 ), STATUS )
         CALL CCD1_GRDEL( LINGRP, STATUS )

*  If this section has a bad status, must be to do with the contents of
*  the table. Issue the name of this file.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FNAME', FNAME )
            CALL ERR_REP( 'IMPORT_BADTAB',
     :      '  Error processing FITS control table: ^FNAME', STATUS )
         END IF

*  Get the number of entries in the FITS groups. Need this to allocate
*  memory for FITS values.
         CALL GRP_GRPSZ( FITGRP( 1 ), NLINES, STATUS )

*  Allocate memory for _INTEGER, _REAL, _DOUBLE and _LOGICAL types,
*  plus pointers for CHARACTER memory.
         CALL CCD1_MALL( NLINES, '_INTEGER', IPINT, STATUS )
         CALL CCD1_MALL( NLINES, '_REAL', IPREAL, STATUS )
         CALL CCD1_MALL( NLINES, '_DOUBLE', IPDBLE, STATUS )
         CALL CCD1_MALL( NLINES, '_LOGICAL', IPLOG, STATUS )
         CALL CCD1_MALL( NLINES, '_INTEGER', IPCHR, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      END IF

*  Write message about the name of the control table.
      CALL MSG_SETC( 'FNAME', FNAME )
      CALL CCD1_MSG( ' ', '  Using FITS control table: ^FNAME', STATUS )

*  Process each NDF in turn.
      DO 100 INDEX = 1, NNDF

*  Get the input NDF identifier
         CALL NDG_NDFAS( GIDIN, INDEX, 'UPDATE', IDIN, STATUS )

*  Write out name of this NDF.
         CALL CCD1_MSG( ' ',  ' ', STATUS )
         CALL NDF_MSG( 'CURRENT_NDF', IDIN )
         CALL CCD1_MSG( ' ', '  +++ Processing NDF: ^CURRENT_NDF',
     :                  STATUS )

*  Inform user how many NDFs we've processed out of the total number.
         CALL MSG_SETI( 'CURRENT_NUM', INDEX )
         CALL MSG_SETI( 'MAX_NUM', NNDF )
         CALL CCD1_MSG( ' ', '  (Number ^CURRENT_NUM of ^MAX_NUM)',
     :                  STATUS )

*  Check that a CCDPACK extension is present in the NDF. This creates
*  it if one doesn't exist.
         CALL CCD1_CEXT( IDIN, .TRUE., 'UPDATE', CCDLOC, STATUS )

*  Look for a FITS extension in the NDF.
         CALL NDF_XLOC( IDIN, 'FITS', 'READ', FITLOC, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL NDF_MSG( 'NDF', IDIN )
            CALL ERR_REP( 'IMPORT_NOFITS',
     :      '  NDF ^NDF has no fits information', STATUS )
         ELSE

*  Map in the fits block of the NDF.
            CALL DAT_MAPV( FITLOC, '_CHAR*80', 'READ', IPFIT, FITLEN,
     :                     STATUS )

*  Now interpret and import the FITS information into the NDF. Note
*  that the lengths of the FITS block character strings are appended
*  after the last genuine argument. This is the usual method in UNIX
*  systems (normally implemented by the compiler), on VMS this makes
*  no difference.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL CCD1_IMFIT( FITGRP, DESGRP, IDIN,
     :                          %VAL( CNF_PVAL( IPFIT ) ),
     :                          FITLEN, %VAL( CNF_PVAL( IPINT ) ),
     :                          %VAL( CNF_PVAL( IPREAL ) ),
     :                          %VAL( CNF_PVAL( IPDBLE ) ),
     :                          %VAL( CNF_PVAL( IPLOG ) ),
     :                          %VAL( CNF_PVAL( IPCHR )), STATUS,
     :                          %VAL( CNF_CVAL( 80 ) ) )
            END IF

*  Unmap FITS block.
            CALL DAT_UNMAP( FITLOC, STATUS )
         END IF

*  Add original filename information to this NDF if none exists already.
         CALL DAT_THERE( CCDLOC, 'ORIGINAL', OK, STATUS )
         IF ( .NOT. OK ) THEN

*  Item doesn't exist add the name of this NDF.
            CALL NDF_MSG( 'CURRENT_NDF', IDIN )
            CALL MSG_LOAD( ' ', '^CURRENT_NDF', NDFNAM, NAMLEN, STATUS )
            NAMLEN = MAX( 1, NAMLEN )
            CALL CCG1_STO0C( IDIN, 'ORIGINAL', NDFNAM( :NAMLEN ),
     :                       STATUS )
         END IF

*  Release this NDF.
         CALL DAT_ANNUL( CCDLOC, STATUS )
         CALL DAT_ANNUL( FITLOC, STATUS )
         CALL NDF_ANNUL( IDIN, STATUS )

*  Write terminator for Processing NDF: message.
         CALL CCD1_MSG( ' ', '  ---',STATUS )

*  Break out if status set BAD (do not do this before releasing memory).
         IF ( STATUS .NE. SAI__OK ) GO TO 99
 100  CONTINUE

*  Write an output list of the NDF names for other applications to use.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK
         CALL CCD1_LNAM( 'NAMELIST', 1, NNDF,
     :                   '# IMPORT - NDF name list', GIDIN, GRP__NOID,
     :                   .FALSE., STATUS )
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
      CALL CCD1_GRDEL( GIDIN, STATUS )
      CALL CCD1_GRDEL( FITGRP( 1 ), STATUS )
      CALL CCD1_GRDEL( FITGRP( 2 ), STATUS )
      CALL CCD1_GRDEL( DESGRP( 1 ), STATUS )
      CALL CCD1_GRDEL( DESGRP( 2 ), STATUS )
      CALL CCD1_GRDEL( DESGRP( 3 ), STATUS )

*  Close the translation table (if open).
      IF ( TOPEN ) CALL FIO_CLOSE( FDIN, STATUS )

*  Release calibration frame.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IMPORT_ERR',
     :   'IMPORT: Error importing FITS information into CCDPACK.',
     :   STATUS )
      END IF
      CALL CCD1_END( STATUS )

      END
* $Id$
