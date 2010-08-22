      SUBROUTINE FTS1_SDSCF( NCARD, HEADER, SCARD, PNDSCF, AUTO, TABNAM,
     :                       DSCFNM, NDIM, AXIS, STATUS )
*+
*  Name:
*     FTS1_SDSCF

*  Purpose:
*     Creates a SCAR description file from a FITS table-format headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL FTS1_SDSCF( NCARD, HEADER, SCARD, PNDSCF, AUTO, TABNAM,
*     :                 DSCFNM, NDIM, AXIS, STATUS )

*  Description:
*     This routine searches for the mandatory FITS-Tables-format header
*     cards stored in a buffer, and their values are returned, if they
*     are present. Should an item be missing or have an unsupported
*     value an error is reported, a bad status is set and the routine
*     exits. This version supports mandatory descriptors that are not
*     in the correct order.

*     There are two methods of opening the FACTS description file. The
*     first associates the file with a parameter. Unfortunately, FIO
*     filename parameters cannot be given dynamic values.  Therefore,
*     the name of the table in the header card images is presented, and
*     the naming rule for an FACTS description file given.  The user has
*     to type in the file name, when prompted for the FACTS file to be
*     created for ADC(/SCAR) usage. However, in the alternative,
*     automatic mode, the supplied file name is used with the DSCF
*     prefix to open the FACTS file without recourse to the parameter
*     system. Information in the FITS headers is transferred to this
*     description file.  The mandatory FACTS parameters for a sequential
*     disk file are written plus EPOCH, VERSION, NRECORDS and AUTHOR
*     where these are known. These are followed by FACTS records
*     describing the fields in the table, and the ENDFIELD record.
*     Finally all the comment lines in the FITS header are copied, in
*     order, to the FACTS file.  Blank comment entries are inserted in
*     the

*     The syntax of the field's format descriptor is checked to see that
*     it is standard.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the header.
*     HEADER( NCARD ) = CHARACTER * ( * ) (Given)
*        The buffer containing the table-format FITS header.
*     SCARD = INTEGER (Given)
*        The number of the card from where the searches of the header
*        will begin.  This is needed because the headers make contain a
*        dummy header prior to an extension.
*     PNDSCF = CHARACTER * ( * ) (Given)
*        The parameter name used to create and associate the FACTS
*        description file. It is only used when %AUTO is false.
*     AUTO = LOGICAL (Given)
*        If true the supplied file name is used to open the file
*        rather than obtaining the file name via the parameter system.
*     TABNAM = CHARACTER * ( * ) (Given and Returned)
*        The name of the table file. It is read if %AUTO is true and
*        written when %AUTO is false.
*     DSCFNM = CHARACTER * ( * ) (Returned)
*        The name of the description file.
*     NDIM = INTEGER (Returned)
*        The number of active dimensions.
*     AXIS( DAT__MXDIM ) = INTEGER (Returned)
*        The dimensions of the data array.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Check for extension and that it is tables format
*     If not present report error, set bad status and return
*     If tape is not SIMPLE, report that, set bad status and return
*     Locate and decode BITPIX value and check status - report context
*       and return if bad
*     If BITPIX not present, report error, set bad status and return
*     Check that BITPIX has valid value
*     Locate and decode NAXIS value and check status - report context
*       and return if bad
*     If NAXIS not present, report error, set bad status and return
*     Check that NAXIS is 2(-dimensional)
*     For each AXIS
*        Generate name of the NAXISn keyword
*        Locate and decode AXISn value, and check status - report
*          context and return if bad
*        If AXISn not present report error, set bad status and return
*        Check that NAXISn has a valid value
*        If AXISn value is meaningless or non-integer, report error,
*          set bad status and return
*     Endfor
*     Look for the group and group-count keywords
*     If either has a non-standard value, report an error and exit
*     Get the number of fields (TFIELDS)
*     If TFIELDS is not present, report error, set bad status and return
*     Check that TFIELDS has a valid value
*     Get the catalogue name, version and level if present, and use a
*       default name if the first is not there
*     If automatic mode then
*        Open the FACTS file with the DSCF prefix
*        If status is bad then exit
*     Else
*        Report the name of the table file in the headers and give the
*          naming rule for the description file
*        Associate and open the FACTS file
*        Generate the suggested filename for the table itself, given the
*          name of the description file
*        If status is bad then exit
*     Endif
*     Write FACTS records for the mandatory parameters (TITLE, MEDIUM,
*       ACCESSMODE and RECORDSIZE) plus NRECORDS, and also EPOCH,
*       VERSION and AUTHOR where these are known
*     Check status at the end reporting at which point the error
*       occurred
*     For each field
*        Obtain values for the mandatory keywords TBCOLnnn, TFORMnnn
*        Report an error if either is not present or has an invalid
*          value (latter has checks for the syntax of the format string)
*        Obtain values for the keyword TTYPEnnn, recording the number of
*          its header record
*        Extract the description of the catalogue's field and append '!'
*          terminator
*        If not character format obtain values for the keywords TSCALnnn
*          and TZEROnnn if present (assuming single precision)
*        If either is present and is not the default (no scaling and no
*          offset) then
*           Increment count of fields with scaled values
*           Write a C(omment) string describing the field and the values
*             since it cannot be processed directly by ADC
*        Endif
*        Obtain the value of the null string (keyword TNULLnnn)
*        Write the field record to the description file
*        If an error occurred, report its context and exit
*     Endfor
*     Write endfield record to signify the end of the fields
*     If an error occurred, report its context and exit
*     If there were any scaled values then
*        Write details to a C(omment) fields using the strings composed
*          in the field loop
*        If an error occurred, report its context and exit
*     Endif
*     Extract all comments from the header card images and write them
*       in turn to the description file
*     If an error occurred, report its context and exit
*     Close the description file
*     End

*  Notes:
*     - The format specifier for each field is validated.

*  Copyright:
*     Copyright (C) 1989, 1990, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1989 Jul 27 (MJC):
*        Original version.
*     1989 Nov 24 (MJC):
*        Rewrite the TFORM validation and fixed bug therein;
*        used default name if not present in the FITS headers.
*     1990 Jan 11 (MJC):
*        Correct format statement (one too many Xs) for writing the
*        table version.
*     1990 Feb 20 (MJC):
*        Added auto mode and two extra parameters, replaced SUBPAR
*        calls by AIF_FLNAM call, and AIF_OPFIO renamed AIF_ASFIO .
*     1990 Oct 29 (MJC):
*        Added one to the RECORDSIZE to allow for Fortran carriage
*        control .
*     1990 November 20 (MJC):
*        Renamed from FITSTH, added SCARD argument, and converted to
*        the SST prologue style.
*     1991 February 1 (MJC):
*        Fixed bug in reporting SCALE and ZERO.
*     1991 February 28 (MJC):
*        Converted BUFFER from an assumed-size to an adjustable array
*        via the NCARD argument for revised FTS1_GKEYx calls.
*     1991 November 1 (MJC):
*        Reordered MSG_SET of the field number to prevent concatenation
*        of the tokens.
*     1992 March 4 (MJC):
*        Will tolerate floating-point format specifications Fn.n-1.
*     1993 January 26 (MJC):
*        Used FIO_ASSOC and FIO_CANCL as the former reprompts.
*     1993 April 21 (MJC):
*        Revised null handling to allow for undocumented feature of
*        FIO_CANCL.
*     1996 November 24 (MJC):
*        Modern style.  Revised FTS1_GKEYx calls.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants
      INCLUDE 'PRM_PAR'          ! Machine-precision definitions

*  Arguments Given:
      INTEGER NCARD              ! Number of card images in header
      INTEGER SCARD              ! Search start card number
      CHARACTER * ( * ) HEADER( NCARD ) ! FITS tape buffer
      CHARACTER * ( * ) PNDSCF   ! Parameter name for the description
                                 ! file
      LOGICAL AUTO               ! Supplied table name is to be used?

*  Arguments Given and Returned:
      CHARACTER * ( * ) TABNAM   ! Name of the table file

*  Arguments Returned:
      CHARACTER * ( * ) DSCFNM   ! Name of the description file
      INTEGER NDIM               ! Dimensionality of the table
      INTEGER AXIS( DAT__MXDIM ) ! Dimensions of the table

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Number characters excluding trailing
                                 ! blanks
      LOGICAL CHR_ISDIG          ! Character is a digit?

*  Local Constants:
      INTEGER MXFIEL             ! The maximum number of fields
      PARAMETER ( MXFIEL = 999 )

*  Local Variables:
      CHARACTER * ( 68 ) AUTHOR  ! Author of the table
      INTEGER BITPIX             ! Bits per pixel
      CHARACTER * ( 132 ) BUFFER ! Buffer used to create description-
                                 ! file records
      INTEGER CARDES             ! Number of the header card image
                                 ! containing the description of the field
      CHARACTER * ( 3 ) CI       ! Field number
      CHARACTER * ( 3 ) CNDIM    ! Axis number
      INTEGER COLD               ! Column position of the FITS comment
                                 ! delimiter
      INTEGER COLP               ! Column position of the DSCF string in
                                 ! the description file name
      CHARACTER * ( 48 ) COMENT  ! Comment from card (not used)
      INTEGER CP                 ! Column position of shriek
      CHARACTER * ( 17 ) DATE    ! Creation date of the table
      INTEGER DDUMMY             ! Value of format specifier dd
      CHARACTER * ( 50 ) DESCRP  ! Description of the field
      INTEGER ERECOM             ! Header image containing last-found
                                 ! FITS comment keyword
      INTEGER EXPONT             ! Column position of exponent in format
                                 ! specifier
      INTEGER FD                 ! file description
      CHARACTER * ( 85 ) FMT1    ! FORMAT specification
      CHARACTER * ( 1 ) FORMAT   ! Format code
      INTEGER GCOUNT             ! Group count
      INTEGER I                  ! Loop counter
      INTEGER LEVEL              ! Extension level
      INTEGER N                  ! Loop counter
      CHARACTER * ( 68 ) NAME    ! Name of the table given in the FITS
                                 ! header
      CHARACTER * ( 8 ) NAXNAM   ! NAXISn keyword name
      INTEGER NC                 ! Number of characters in axis number
      REAL NDSCAL( MXFIEL )      ! Field scale factors that are non equal
                                 ! to 1
      REAL NDZERO( MXFIEL )      ! Field offsets that are non equal to 0
      CHARACTER * ( 17 ) NDTYPE( MXFIEL ) ! Field types that do have a
                                 ! non-default scale and offset
      INTEGER NKC                ! Number of a header card image that
                                 ! contains a specified keyword
      INTEGER NSCALD             ! The number of scaled parameters
      LOGICAL OPEN               ! DSCF file opened?
      INTEGER PCOUNT             ! Number of group parameters
      INTEGER POINT              ! Position of the decimal point in the
                                 ! format specifier
      CHARACTER * ( 68 ) REFRNC  ! Reference of the table
      REAL SCALE                 ! Scale factor to be applied to the
                                 ! field
      INTEGER SRECOM             ! Record number in the header to start
                                 ! search for the next comment keyword
      INTEGER TBCOL              ! Beginning column of the field
      INTEGER TFIELD             ! Number of fields per line of the table
      CHARACTER * ( 6 ) TFORM    ! Format code of the field
      LOGICAL THERE              ! There is a card with the requested
                                 ! keyword in the header?
      LOGICAL THERES             ! There is a card with the SCALnnn
                                 ! keyword in the header?
      CHARACTER * ( 68 ) TNULL   ! Null field
      CHARACTER * ( 20 ) TTYPE   ! Name of the field
      CHARACTER * ( 17 ) TUNIT   ! Units of the field variable
      LOGICAL VALFMT             ! Current format specifier is valid?
      INTEGER VER                ! Version number of the table
      INTEGER WDUMMY             ! Value of format specifier ww
      INTEGER WIDTH              ! Width of the field in characters
      REAL ZERO                  ! Zero offset to be applied to the
                                 ! field

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  The first header record must not be an extension.
      IF ( HEADER( SCARD )( 1:8 ) .NE. 'XTENSION' .OR.
     :     HEADER( SCARD )( 12:19 ) .NE. 'TABLE    ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FTS1_SDSCF_NOTAB',
     :     'Buffer does not a valid Tables-format header',
     :     STATUS )
         GOTO 999
      END IF

*  Continue checking the mandatory descriptors... BITPIX is number of
*  bits per pixel.
      CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'BITPIX', 1, THERE, BITPIX,
     :                 COMENT, NKC, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FTS1_SDSCF_FBITPI',
     :     'Error evaluating BITPIX', STATUS )
         GOTO 999
      END IF

*  See if there was a BITPIX header card.
      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FTS1_SDSCF_BITPIX',
     :     'BITPIX not present', STATUS )
         GOTO 999
      END IF

*  BITPIX can currently only have the value 8.
      IF ( BITPIX .NE. 8 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FTS1_SDSCF_IBITPI',
     :     'BITPIX is not 8.', STATUS )
         GOTO 999
      END IF

*  Now to the number of dimensions.
      CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'NAXIS', 1, THERE, NDIM,
     :                 COMENT, NKC, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FTS1_SDSCF_INAXIS',
     :     'Error converting NAXIS', STATUS )
         GOTO 999
      END IF

      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FTS1_SDSCF_NAXIS',
     :     'NAXIS not present', STATUS )
         GOTO 999
      END IF

*  Must be 2-dimensional for a table.
      IF ( NDIM .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NAXIS', NDIM )
         CALL ERR_REP( 'FTS1_SDSCF_INAXIS',
     :     'Cannot process NAXIS = ^NAXIS.  Must be '/
     :     /'2-dimensional.', STATUS )
         GOTO 999
      END IF

*  For each dimension...
      DO N = 1, NDIM

*  Generate name of the Nth axis dimension, NAXISn.
         CALL CHR_ITOC( N, CNDIM, NC )
         NAXNAM = 'NAXIS'//CNDIM( :NC )

*  Get value of NAXISn.
         CALL FTS1_GKEYI( NCARD, HEADER, SCARD, NAXNAM, 1, THERE,
     :                    AXIS( N ), COMENT, NKC, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETI( 'N', N )
            CALL ERR_REP( 'FTS1_SDSCF_FAXISN',
     :        'Error converting AXIS^N value', STATUS )
            GOTO 999
         END IF

         IF ( .NOT. THERE ) THEN
            CALL MSG_SETI( 'N', N )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FTS1_SDSCF_NAXISn',
     :        'NAXIS^N not present', STATUS )
            GOTO 999
         END IF

*  Check that it is physical or does not have integer value.
         IF ( AXIS( N ) .LT. 1  ) THEN
            CALL MSG_SETI( 'N', N )
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'AXIS', AXIS( N ) )
            CALL ERR_REP( 'FTS1_SDSCF_AXISIZ',
     :        'Axis ^N has invalid dimension ^AXIS', STATUS )
            GOTO 999
         END IF

      END DO

*  Now look for the group and parameter counts.
      CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'GCOUNT', 1, THERE, GCOUNT,
     :                 COMENT, NKC, STATUS )
      IF ( .NOT. THERE ) GCOUNT = 1

      IF ( GCOUNT .NE. 1 ) THEN
         CALL ERR_REP( 'FTS1_SDSCF_GCOUNT',
     :     'GCOUNT not equal to one.', STATUS )
         GOTO 999
      END IF

      CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'PCOUNT', 1, THERE, PCOUNT,
     :                 COMENT, NKC, STATUS )
      IF ( .NOT. THERE ) PCOUNT = 0

      IF ( PCOUNT .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FTS1_SDSCF_PCOUNT',
     :     'PCOUNT not equal to zero.', STATUS )
         GOTO 999
      END IF

*  Get the number of fields.
      CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'TFIELDS', 1, THERE,
     :                 TFIELD, COMENT, NKC, STATUS )
      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FTS1_SDSCF_TFIELD',
     :     'TFIELDS not present', STATUS )
         GOTO 999
      END IF

*  Check that the number of fields is a valid range.  999 limitation is
*  imposed by the 8-character FITS keywords.
      IF ( TFIELD .LT. 1 .OR. TFIELD .GT. 999 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'TFN', TFIELD )
         CALL ERR_REP( 'FTS1_SDSCF_I',
     :     'TFIELDS = ^TFN is out of range.', STATUS )
         GOTO 999
      END IF

*  Now see whether there is a name for the table.
      NAME = ' '
      CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'EXTNAME', 1, THERE, NAME,
     :                 COMENT, NKC, STATUS )

*  Get the extension level.  even if this is not one we are not going
*  to preserve an hierarchy.
      CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'EXTLEVEL', 1, THERE,
     :                 LEVEL, COMENT, NKC, STATUS )
      IF ( .NOT. THERE ) LEVEL = 1

*  Get the extension version number.
      VER = - 1
      CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'EXTVER', 1, THERE, VER,
     :                 COMENT, NKC, STATUS )

      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*  Open the DSCF file.
*  ===================
      IF ( AUTO ) THEN

         DSCFNM = 'DSCF'//TABNAM

*  The FACTS file is opened, since its name is known.
         CALL AIF_OPFIO( DSCFNM, 'WRITE', 'FORTRAN', 132, FD,
     :                   OPEN, STATUS )

         IF ( STATUS .NE. SAI__OK .OR. .NOT. OPEN ) GOTO 980
      ELSE

*  Suggest a default name for the catalogue.
         IF ( NAME( 1:1 ) .NE. ' ' ) THEN
            CALL MSG_SETC( 'TABNAME', NAME )
            CALL MSG_OUT( 'DSCFS', 'The name of the table is ^TABNAME',
     :        STATUS )

*  Use some arbitrary name for out put messages.
         ELSE
            NAME = 'TABLE'
         END IF

*  Take precaution against a null description file, so define a default
*  table name.
         DSCFNM = 'DSCF'//NAME
         TABNAM = NAME

*  Let the user know what to do, since the environment cannot provide a
*  dynamic default.
         CALL MSG_OUT( 'DSCFS', 'The name of the FACTS description '/
     :     /'file should be the name of the catalogue', STATUS )
         CALL MSG_OUT( 'DSCFS1', 'prefixed by DSCF. Therefore the '/
     :     /'suggested name for the description file', STATUS )
         CALL MSG_SETC( 'DSCFNAME', DSCFNM )
         CALL MSG_OUT( 'DSCFS2', 'is ^DSCFNAME', STATUS )

*  Open the file via the parameter.
         CALL FIO_ASSOC( PNDSCF, 'WRITE', 'FORTRAN', 132, FD, STATUS )

*  A null file name causes no DSCF file to be created.
         IF ( STATUS .NE. SAI__OK ) THEN
            DSCFNM = ' '
            CALL FIO_CANCL( PNDSCF, STATUS )
            IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
            GOTO 999
         END IF

*  Obtain the file name associated with the parameter.
         CALL AIF_FLNAM( PNDSCF, DSCFNM, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'FTS1_SDSCF_GTDSCF',
     :        'Unable to obtain name of '/
     :        /'description file', STATUS )
            GOTO 980
         END IF

*  Generate a table name based on the FACTS name, excluding any DSCF
*  prefix.
         CALL CHR_UCASE( DSCFNM )
         COLP = 0
         COLP = INDEX( DSCFNM, 'DSCF' )

*  Perserve user's non-standard name for the description file, and set
*  the default table file to take the name in the FITS headers.
         IF ( COLP .EQ. 0 ) THEN
            TABNAM = NAME
         ELSE IF ( COLP .EQ. 1 ) THEN
            TABNAM = DSCFNM( COLP+4: )
         ELSE
            TABNAM = DSCFNM( 1:COLP-1 )//DSCFNM( COLP+4: )
         END IF
      END IF

      CALL MSG_SETC( 'DSCFNAME', DSCFNM )
      CALL MSG_OUT( 'DSCF', 'The FACTS description file is being '/
     :  /'written to ^DSCFNAME', STATUS )

*  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*  Get the author and reference of the catalogue.
      AUTHOR = ' '
      CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'AUTHOR', 1, THERE, AUTHOR,
     :                 COMENT, NKC, STATUS )
      REFRNC = ' '
      CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'REFERENC', 1, THERE,
     :                 REFRNC, COMENT, NKC, STATUS )
      DATE = ' '
      CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'DATE', 1, THERE, DATE,
     :                 COMENT, NKC, STATUS )

*-----------------------------------------------------------------------

*  Write P(arameter) entries in the DSCF file.
*  ===========================================

*  Title.
      IF ( STATUS .EQ. SAI__OK ) THEN
         BUFFER = ' '
         FMT1 = '( '' P TITLE'',47X,''A'',6X,A17,1X,A50,1X,''X'')'
         WRITE( BUFFER, FMT1 ) NAME, REFRNC
      END IF
      CALL FIO_WRITE( FD, BUFFER, STATUS )

*  The files are on disk.
      IF ( STATUS .EQ. SAI__OK ) THEN
         BUFFER = ' '
         FMT1 = '( '' P MEDIUM'',46X,''A4     DISK'',65X,''X'')'
         WRITE( BUFFER, FMT1 )
      END IF
      CALL FIO_WRITE( FD, BUFFER, STATUS )

*  Date.
      IF ( DATE( 1:2 ) .NE. '  ' ) THEN
         IF ( STATUS .EQ. SAI__OK ) THEN
            BUFFER = ' '
            FMT1 = '( '' P EPOCH'',36X,''YEAR'',7X,''A'',7XA,51X,''X'')'
            WRITE( BUFFER, FMT1 ) DATE
         END IF
         CALL FIO_WRITE( FD, BUFFER, STATUS )
      END IF

*  The table is formatted sequential.
      IF ( STATUS .EQ. SAI__OK ) THEN
         BUFFER = ' '
         FMT1 = '( '' P ACCESSMODE'',42X,''A6     SEQUENTIAL'',59X,'/
     :          /'''X'')'
         WRITE( BUFFER, FMT1 )
      END IF
      CALL FIO_WRITE( FD, BUFFER, STATUS )

*  Version number.
      IF ( VER .NE. -1 ) THEN
         IF ( STATUS .EQ. SAI__OK ) THEN
            BUFFER = ' '
            FMT1 = '( '' P VERSION'',45X,''I2'',5X,I2,67X,''X'')'
            WRITE( BUFFER, FMT1 ) VER
         END IF
         CALL FIO_WRITE( FD, BUFFER, STATUS )
      END IF

*  Author.
      IF ( AUTHOR( 1:2 ) .NE. '  ' ) THEN
         IF ( STATUS .EQ. SAI__OK ) THEN
            BUFFER = ' '
            FMT1 = '( '' P AUTHOR'',46X,''A'',6X,A17,52X,''X'')'
            WRITE( BUFFER, FMT1 ) AUTHOR
         END IF
         CALL FIO_WRITE( FD, BUFFER, STATUS )
      END IF

*  Recordsize, allowing for Fortran carriage control by adding one.
      IF ( STATUS .EQ. SAI__OK ) THEN
         BUFFER = ' '
         FMT1 = '( '' P RECORDSIZE'',31X,''BYTE'',7X,''I5'',5X,I5,'/
     :          /'64X,''X'')'
         WRITE( BUFFER, FMT1 ) AXIS( 1 ) + 1
      END IF
      CALL FIO_WRITE( FD, BUFFER, STATUS )

*  Number of entries in the catalogue.
      IF ( STATUS .EQ. SAI__OK ) THEN
         BUFFER = ' '
         FMT1 = '( '' P NRECORDS'',44X,''I10'',4X,I10,59X,''X'')'
         WRITE( BUFFER, FMT1 ) AXIS( 2 )
      END IF
      CALL FIO_WRITE( FD, BUFFER, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'BUFFER', BUFFER )
         CALL ERR_REP( 'FTS1_SDSCF_DSCFP',
     :     'Error writing P(arameter) records to the '/
     :     /'description file. The buffer was "^BUFFER"', STATUS )
         GOTO 980
      END IF

*-----------------------------------------------------------------------

      NSCALD = 0
      DO  I = 1, TFIELD

*  This is inefficient because the T*nnn descriptors can appear in any
*  order.  Sorting would jumble the comments.  The brute force approach
*  is simplest to follow.

*  Get column position for Ith field.
         CALL CHR_ITOC( I, CI, NC )
         CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'TBCOL'//CI, 1, THERE,
     :                    TBCOL, COMENT, NKC, STATUS )
         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'I', I )
            CALL ERR_REP( 'FTS1_SDSCF_TBCOL',
     :        'TBCOL^I is not present', STATUS )
            GOTO 980
         END IF
         IF ( TBCOL .LT. 1 .OR. TBCOL .GT. AXIS( 1 ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'I', I )
            CALL ERR_REP( 'FTS1_SDSCF_ITBCOL',
     :        'The value of TBCOL^I is in error', STATUS )
            GOTO 980
         END IF

*  Now get the Fortran format.
         CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'TFORM'//CI, 1, THERE,
     :                    TFORM, COMENT, NKC, STATUS )
         IF ( .NOT. THERE ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'I', I )
            CALL ERR_REP( 'FTS1_SDSCF_TFORM',
     :        'TFORM^I is not present', STATUS )
            GOTO 980
         END IF

*  Check that the format is one of the acceptable forms...
         FORMAT = TFORM( 1:1 )
         CALL CHR_UCASE( FORMAT )
         NC = CHR_LEN( TFORM )

*  Specify the character containing decimal point.
         POINT = INDEX( TFORM, '.' )
         IF ( POINT .EQ. 0 ) POINT = 4

*  Assume format specifier is correct unless proven otherwise.
         VALFMT = .TRUE.

*  First A and I format.  Start in the second column following the
*  format specifier checking they are digits.  Check there are no more
*  than 5 digits to follow standard.
         IF ( FORMAT .EQ. 'A' .OR. FORMAT .EQ. 'I' ) THEN
            IF ( NC .GT. 6 ) THEN
               VALFMT = .FALSE.
            ELSE
               DO  N = 2, NC
                  VALFMT = VALFMT .AND. CHR_ISDIG( TFORM(N:N) )
               END DO

*  Check that the number is not too big.
               IF ( VALFMT .AND. NC .EQ. 5 ) THEN
                  CALL CHR_CTOI( TFORM(2:NC), WDUMMY, STATUS )
                  IF ( WDUMMY .GT. 32767 ) VALFMT = .FALSE.
               END IF
            END IF

*  Now D, E, F formats are checked.  The operation has two main parts:
*  validate the digits before and after the decimal point.  The string
*  can only be up to 10 characters long, i.e.  x32767.255, and there
*  must be at least one digit before and after decimal point.
         ELSE IF ( FORMAT .EQ. 'D' .OR. FORMAT .EQ. 'E' .OR.
     :             FORMAT .EQ. 'F' ) THEN

*  So first look before the decimal point.
            IF ( NC .GT. 10 .OR. POINT .EQ. 2 ) THEN
               VALFMT = .FALSE.
            ELSE
               DO  N = 2, POINT - 1
                  VALFMT = VALFMT .AND. CHR_ISDIG( TFORM(N:N) )
               END DO

               IF ( VALFMT ) THEN

*  Store as integer value for comparison with value after the decimal
*  point.
                  CALL CHR_CTOI( TFORM(2:POINT-1), WDUMMY, STATUS )

*  Check that the number is not too big.
                  IF ( POINT - 2 .GE. 5 ) THEN
                     IF ( WDUMMY .GT. 32767 ) VALFMT = .FALSE.
                  END IF
               END IF
            END IF

*  Now check digits following the point.
            IF ( VALFMT ) THEN
               IF ( NC .EQ. POINT ) THEN
                  VALFMT = .FALSE.
               ELSE
                  DO N = POINT + 1, NC
                     VALFMT = VALFMT .AND. CHR_ISDIG( TFORM(N:N) )
                  END DO
               END IF

               IF ( VALFMT ) THEN

*  Store as integer value for comparison with value before the decimal
*  point.
                  CALL CHR_CTOI( TFORM(POINT+1:NC), DDUMMY, STATUS )

*  Check the F format has space to accommodate the decimal point and a
*  number after it.
                  IF ( FORMAT .EQ. 'F' .AND. WDUMMY .LE. DDUMMY )
     :              THEN
                     VALFMT = .FALSE.

*  Two cases for D or E depending on presence or not of an exponent.
                  ELSE IF ( FORMAT .EQ. 'D' .OR.
     :                      FORMAT .EQ. 'E' ) THEN

*  Is there an exponent (EXPONT = 0)?
                     IF ( FORMAT .EQ. 'D' ) THEN
                        EXPONT = INDEX( TFORM( POINT + 1:NC ), 'D' )
                     ELSE
                        EXPONT = INDEX( TFORM( POINT + 1:NC ), 'E' )
                     END IF

*  No exponent so treat like F format.
                     IF ( EXPONT .EQ. 0 .AND. WDUMMY .LE. DDUMMY )
     :                 THEN
                        VALFMT = .FALSE.

*  This is not rigorous as there may be a sign.
                     ELSE IF ( EXPONT .GT. 0 .AND. WDUMMY .LE. DDUMMY
     :                         + 1 + NC - EXPONT ) THEN
                        VALFMT = .FALSE.
                     END IF
                  END IF

*  Check that the number is not too big.
                  IF ( VALFMT .AND. NC - POINT .GE. 3 ) THEN
                     IF ( DDUMMY .GT. 255 ) VALFMT = .FALSE.
                  END IF
               END IF
            END IF
         END IF

*  Format type must be A, D, E, F or I.
         IF ( ( FORMAT .NE. 'A' .AND. FORMAT .NE. 'D' .AND.
     :          FORMAT .NE. 'E' .AND. FORMAT .NE. 'F' .AND.
     :          FORMAT .NE. 'I' ) .OR. .NOT. VALFMT ) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETI( 'I', I )
            CALL MSG_SETC( 'TFORM', TFORM )
            CALL ERR_REP( 'FTS1_SDSCF_ITFORM',
     :        'The value of TFORM^I = ^TFORM is not a valid '/
     :        /'format', STATUS )
            GOTO 980
         END IF

*  Get the name of the field.  Default should be ' ', but it is.
*  mandatory for DSCF file so an alternative is constructed.
         TTYPE = 'FIELD'//CI
         CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'TTYPE'//CI, 1, THERE,
     :                    TTYPE, COMENT, CARDES, STATUS )

*  Extract the description of the catalogue field.
         DESCRP = ' '
         IF ( THERE ) THEN
            COLD = INDEX( HEADER( CARDES )( 30:80 ), '/' )
            IF ( COLD .EQ. 0 ) THEN

*  Check the next card image for a continuation comment.
               IF ( HEADER( CARDES + 1 )( 1:8 ) .EQ. '        ' ) THEN
                  COLD = INDEX( HEADER( CARDES + 1 )( 30:80 ), '/' )
                  IF ( COLD .NE. 0 )
     :               DESCRP = HEADER( CARDES + 1 )( COLD+31:80 )
               END IF
            ELSE
               DESCRP = HEADER( CARDES )( COLD+31:80 )
            END IF

            IF ( COLD .NE. 0 ) THEN

*  Get its length and append the ! terminator.
               CP = CHR_LEN( DESCRP )
               CALL CHR_APPND( '!', DESCRP, CP )
            END IF
         END IF

         TUNIT = ' '
         IF ( FORMAT .NE. 'A' ) THEN

*  Get the units.
            CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'TUNIT'//CI, 1,
     :                       THERE, TUNIT, COMENT, NKC, STATUS )

*  Next obtain the scale factor and offset, assuming single precision.
            CALL FTS1_GKEYR( NCARD, HEADER, SCARD, 'TSCAL'//CI, 1,
     :                       THERES, SCALE, COMENT, NKC, STATUS )
            IF ( .NOT. THERES ) SCALE = 1.0

            CALL FTS1_GKEYR( NCARD, HEADER, SCARD, 'TZERO'//CI, 1,
     :                       THERE, ZERO, COMENT, NKC, STATUS )
            IF ( .NOT. THERE ) ZERO = 0.0

*  SCAR/ADC cannot handle scaled data values directly and will require
*  some manual intervention.  Therefore store the scale factor, zero
*  point, and the associated type in arrays, when the scaling is not
*  the identity transformation.  When all the fields have been written
*  to the descriptor file, the scale and offset information may then be
*  written to a comment record.
            IF ( ABS( SCALE - 1.0 ) .GT. VAL__EPSR .OR.
     :           ABS( ZERO - 0.0 ) .GT. VAL__EPSR ) THEN
               NSCALD = NSCALD + 1
               NDTYPE( NSCALD ) = TTYPE( 1:MIN( CHR_LEN( TTYPE ), 17 ) )
               NDSCAL( NSCALD ) = SCALE
               NDZERO( NSCALD ) = ZERO
            END IF
         END IF

*  Get the null string.
         TNULL = ' '
         CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'TNULL'//CI, 1, THERE,
     :                    TNULL, COMENT, NKC, STATUS )

*  Find the width of the field.
         CALL CHR_CTOI( TFORM( 2:POINT-1 ), WIDTH, STATUS )

*  Write the F(ield) record in the description file.
         IF ( STATUS .EQ. SAI__OK ) THEN
            BUFFER = ' '
            FMT1 = '( '' F '',A17,1X,I5,1X,I5,1X,A6,3X,''0'',1X,A10,'/
     :             /'1X,A6,1X,A17,1X,A50,1X,''X'')'
            WRITE( BUFFER, FMT1 ) TTYPE( 1:17 ), TBCOL+1, WIDTH, TFORM,
     :        TUNIT( 1:10 ), TFORM, TNULL( 1:17 ), DESCRP
         END IF
         CALL FIO_WRITE( FD, BUFFER, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'BUFFER', BUFFER )
            CALL ERR_REP( 'FTS1_SDSCF_DSCFF',
     :        'Error writing a F(ield) record to the '/
     :        /'description file. The buffer was "^BUFFER"', STATUS )
            GOTO 980
         END IF
      END DO

*  Write endfile record to signify end of the parameters and fields.
      CALL FIO_WRITE( FD, ' E', STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'BUFFER', BUFFER )
         CALL ERR_REP( 'FTS1_SDSCF_DSCFE',
     :     'Error writing the E(ndfield) record to the '/
     :     /'description file. The buffer was "^BUFFER"', STATUS )
         GOTO 980
      END IF

*  Are there any scaled fields?
      IF ( NSCALD .NE. 0 ) THEN
         DO  I = 1, NSCALD

*  Write a comment about the scale and offset.
            FMT1 = '( '' C Note field '',A,'' has a scale factor of'','/
     :             /'G14.6,'' and an offset of'',G14.6)'
            WRITE( BUFFER, FMT1 )
     :        NDTYPE( I )( 1:CHR_LEN( NDTYPE( I ) ) ),
     :        NDSCAL( I ), NDZERO( I )
            CALL FIO_WRITE( FD, BUFFER( :98 ), STATUS )

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'BUFFER', BUFFER( :98 ) )
               CALL ERR_REP( 'FTS1_SDSCF_DSCFC',
     :           'Error writing a C(omment) record to the '/
     :           /'description file. The buffer was "^BUFFER"', STATUS )
               GOTO 980
            END IF
         END DO
      END IF

*  Find all comments one at a time.  Initial card is the first in the
*  header section.
      THERE = .TRUE.
      SRECOM = SCARD
      DO WHILE ( STATUS .EQ. SAI__OK .AND. THERE )
         CALL FTS1_COMNT( NCARD, HEADER, SRECOM, THERE, BUFFER, ERECOM,
     :                    STATUS )

         IF ( THERE ) THEN

*  Separate non-contiguous comments.
            IF ( ERECOM .NE. SRECOM .AND. SRECOM .NE. 1 ) THEN
               CALL FIO_WRITE( FD, ' C ', STATUS )
            END IF

*  Write comment field to description file.
            CALL FIO_WRITE( FD, ' C '//BUFFER( 1:72 ), STATUS )
         END IF
         SRECOM = ERECOM + 1
      END DO

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'BUFFER', BUFFER )
         CALL ERR_REP( 'FTS1_SDSCF_DSCFC',
     :     'Error writing a C(omment) record to the '/
     :     /'description file. The buffer was "^BUFFER"', STATUS )
         GOTO 980
      END IF

*  Close the description file.
 980  CONTINUE
      IF ( AUTO ) THEN
         CALL FIO_CLOSE( FD, STATUS )

*  Cancel the description-file parameter as we may be in a loop.
      ELSE
         CALL FIO_CANCL( PNDSCF, STATUS )
      END IF

 999  CONTINUE

      END
