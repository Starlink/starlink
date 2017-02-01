      SUBROUTINE TRANDAT ( STATUS )
*+
*  Name:
*     TRANDAT

*  Purpose:
*     Converts free-format text data into an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TRANDAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application takes grid data contained in a free-format text
*     file and stores them in the data array of an NDF.  The data file
*     could contain, for example, mapping data  or results from
*     simulations which are to be converted into an image for analysis.
*
*     There are two modes of operation which depend on whether the
*     text file contains co-ordinate information, or solely data
*     values (determined by parameter AUTO).
*
*     a) AUTO=FALSE   If the file contains co-ordinate information the
*     format of the data is tabular; the positions and values are
*     arranged in columns and a record may contain information for only
*     a single point.  Where data points are duplicated only the last
*     value appears in the NDF.  Comment lines can be given, and are
*     indicated by a hash or exclamation mark in the first column.
*     Here is an example file (the vertical ellipses indicate missing
*     lines in the file):
*
*         # Model 5, phi = 0.25,  eta = 1.7
*         1 -40.0   40.0   1121.9
*         2  0.0   30.0     56.3
*         3 100.0   20.0   2983.2
*         4 120.0   85.0    339.3
*         .    .      .       .
*         .    .      .       .
*         .    .      .       .
*         <EOF>
*
*     The records do not need to be ordered (but see the warning in the
*     Notes), as the application searches for the maximum and minimum
*     co-ordinates in each dimension so that it can define the size of
*     the output image.  Also, each record may contain other data
*     fields (separated by one or more spaces), which need not be all
*     the same data type.  In the example above only columns 2, 3 and 4
*     are required.  There are parameters (POSCOLS, VALCOL) which
*     select the co-ordinate and value columns.
*
*     The distance between adjacent pixels (given by parameter PSCALE)
*     defaults to 1, and is in the same units as the read-in
*     co-ordinates.  The pixel index of a data value is calculated
*     using the expression
*
*        index = FLOOR( ( x - xoff ) / scale ) + 1
*
*     where x is the supplied co-ordinate and xoff is the value of the
*     POFFSET parameter (which defaults to the minimum supplied
*     co-ordinate along an axis), scale is the value of parameter PSCALE,
*     and FLOOR is a function that returns the largest integer that is
*     smaller (i.e. more negative) than its argument.
*
*     You are informed of the number of points found and the maximum
*     and minimum co-ordinate values for each dimension.  There is no
*     limit imposed by the application on the number of points or the
*     maximum output array size, though there may be external
*     constraints.  The derived array size is reported in case you have
*     made a typing error in the text file.  If you realise that this
*     has indeed occurred just abort (!!) when prompted for the output
*     NDF.
*
*     b) AUTO=TRUE   If the text file contains no co-ordinates, the
*     format is quite flexible, however, the data are read into the
*     data array in Fortran order, i.e. the first dimension is the most
*     rapidly varying, followed by the second dimension and so on.  The
*     number of data values that may appear on a line is variable; data
*     values are separated by at least a space, comma, tab or carriage
*     return.  A line can have up to 255 characters.  In addition a
*     record may have trailing comments designated by a hash or
*     exclamation mark.  Here is an example file, though a more regular
*     format would be clearer for the human reader.
*
*         # test for the new TRANDAT
*         23 45.3 ! a comment
*         50.7,47.5 120. 46.67  47.89 42.4567
*         .1 23.3 45.2 43.2  56.0 30.9 29. 27. 26. 22.4 20. 18. -12. 8.
*          9.2 11.
*         <EOF>
*
*     Notice that the shape of the NDF is defined by a parameter rather
*     than explicitly in the file.

*  Usage:
*     trandat freename out [poscols] [valcol] [pscale] [dtype] [title]

*  ADAM Parameters:
*     AUTO = _LOGICAL (Read)
*        If TRUE the text file does not contain co-ordinate
*        information. [FALSE]
*     BAD = _LOGICAL (Read)
*        If TRUE the output NDF data array is initialised with the
*        bad value, otherwise it is filled with zeroes. [TRUE]
*     DTYPE = LITERAL (Read)
*        The HDS type of the data values within the text file, and
*        the type of the data array in the output NDF. The options
*        are: '_REAL', '_DOUBLE', '_INTEGER', '_BYTE', '_UBYTE',
*        '_WORD', '_UWORD'. (Note the leading underscore.) ['_REAL']
*     FREENAME = FILENAME (Read)
*        Name of the text file containing the free-format data.
*     LBOUND( ) = _INTEGER (Read)
*        The lower bounds of the NDF to be created.  The number of
*        values must match the number supplied to parameter SHAPE.  It
*        is only accessed in automatic mode.  If a null (!) value is
*        supplied, the value used is 1 along each axis. [!]
*     POFFSET() = _REAL (Read)
*        The supplied co-ordinates that correspond to the origin of
*        floating point pixel co-ordinates.  It is only used in co-ordinate
*        mode.  Its purpose is to permit an offset from some arbitrary units
*        to pixels. If a null (!) value is supplied, the value used is the
*        minimum supplied co-ordinate value for each dimension. [!]
*     POSCOLS() = _INTEGER (Read)
*        Column positions of the co-ordinates in an input record
*        of the text file, starting from x to higher dimensions.  It
*        is only used in co-ordinate mode.  The columns must be
*        different amongst themselves and also different from the
*        column containing the values.  If there is duplication,
*        new values for both POSCOLS and VALCOL will be requested.
*        [1,2]
*     PSCALE() = _REAL (Read)
*        Pixel-to-pixel distance in co-ordinate units for each
*        dimension.  It is only used in co-ordinate mode.  Its purpose
*        is to permit linear scaling from some arbitrary units to
*        pixels. If a null (!) value is supplied, the value used is
*        1.0 for each co-ordinate dimension. [!]
*     QUANTUM = _INTEGER (Read)
*        You can safely ignore this parameter.  It is used for fine-
*        tuning performance in the co-ordinate mode.
*
*        The application obtains work space to store the position-value
*        data before they can be copied into the output NDF so that the
*        array bounds can be computed.  Since the number of lines in
*        the text file is unknown, the application obtains chunks of
*        work space whose size is three times this parameter whenever
*        it runs out of storage.  (Three because the parameter
*        specifies the number of lines in the file rather than the
*        number of data items.)  If you have a large number of points
*        there are efficiency gains if you make this parameter either
*        about 20--30 per cent or slightly greater than or equal to the
*        number of lines your text file.  A value slightly less than
*        the number of lines is inefficient as it creates nearly 50 per
*        cent unused space.  A value that is too small can cause
*        unnecessary unmapping, expansion and re-mapping of the work
*        space.  For most purposes the default should give acceptable
*        performance.  It must lie between 32 and 2097152. [2048]
*     SHAPE( ) = _INTEGER (Read)
*        The shape of the NDF to be created.  For example, [50,30,20]
*        would create 50 columns by 30 lines by 10 bands.  It is only
*        accessed in automatic mode.
*     NDF = NDF (Write)
*        Output NDF for the generated data array.
*     TITLE = LITERAL (Read)
*        Title for the output NDF. ["KAPPA - Trandat"]
*     VALCOL = _INTEGER (Read)
*        Column position of the array values in an input record of
*        the text file.  It is only used in co-ordinate mode.  The
*        column position must be different from those specified for
*        the co-ordinate columns.  If there is duplication, new values
*        for both POSCOLS and VALCOL will be requested. [3]

*  Examples:
*     trandat simdata.dat model
*        Reads the text file simdata.dat and stores the data into the
*        data array of a two-dimensional, _REAL NDF called model.  The
*        input file should have the co-ordinates and real values
*        arranged in columns, with the x-y positions in columns 1 and 2
*        respectively, and the real data in column 3.
*     trandat freename=simdata out=model auto shape=[50,40,9]
*        Reads the text file simdata and stores the data into the data
*        array of a three-dimensional, _REAL NDF called model.  Its x
*        dimension is 50, y is 40 and z is 9.  The input file only
*        contains real values and comments.
*     trandat freename=simdata out=model auto shape=[50,40,9] dtype=_i
*        As the previous example except an _INTEGER NDF is created, and
*        the text file must contain integer data.
*     trandat simdata.dat model [6,3,4] 2
*        Reads the text file simdata.dat and stores the data into the
*        data array of a three-dimensional, _REAL NDF called model.  The
*        input file should have the co-ordinates and real values
*        arranged in columns, with the x-y-z positions in columns 6, 3
*        and 4 respectively, and the real data in column 2.
*     trandat spectrum.dat lacertid noauto poscols=2 valcol=4 pscale=2.3
*        Reads the text file spectrum.dat and stores the data into the
*        data array of a one-dimensional, _REAL NDF called lacertid.
*        The input file should have the co-ordinate and real values
*        arranged in columns, with its co-ordinates in columns 2, and
*        the real data in column 4.  A one-pixel step in the NDF
*        corresponds to 2.3 in units of the supplied co-ordinates.

*  Notes:
*     - Bad data values may be represented by the string "BAD" (case
*     insensitive) within the input text file.
*     -  All non-complex numeric data types can be handled.  However,
*     byte, unsigned byte, word and unsigned word require data
*     conversion, and therefore involve additional processing.
*     to a vector element (for n-d generality).
*     -  WARNING: In non-automatic mode it is strongly advisable for
*     large output NDFs to place the data in Fortran order, i.e. the
*     first dimension is the most rapidly varying, followed by the
*     second dimension and so on.  This gives optimum performance.  The
*     meaning of "large" will depend on working-set quotas on your
*     system, but a few megabytes gives an idea.  If you jump randomly
*     backwards and forwards, or worse, have a text file in reverse-
*     Fortran order, this can have disastrous performance consequences
*     for you and other users.
*     -  In non-automatic mode, the co-ordinates for each dimension are
*     stored in the NDF axis structure.  The first centre is at the
*     minimum value found in the list of positions for the dimension
*     plus half of the scale factor.  Subsequent centres are
*     incremented by the scale factor.
*     -  The output NDF may have between one and seven dimensions.
*     -  In automatic mode, an error is reported if the shape does not
*     use all the data points in the file.

*  Algorithm:
*     -  Get the output data type and the implementation type.
*     -  Get type (co-ordinates or automatic) of the file and open it.
*     -  If co-ordinates are expected in the file then obtain the
*     column numbers for each co-ordinate and the values, checking
*     that there are no duplications. Get the pixel-to-pixel distance
*     and the workspace quantum.  Otherwise obtain the shape and origin
*     output NDF (for data values only).
*     -  Get the initialisation value.
*     -  For a co-ordinate file:
*        o  Obtain workspace for the co-ordinates and data values.
*        o  Read in the data in the requested type until complete.
*        Increase the work space if there are further data to read.
*        Continue to read the data file.  Repeat the process until
*        the input file is read completely or an error has occurred.
*        o  Report the number of points read, and the upper and lower
*        bounds. Derive and report the array dimensions.
*     -  Create an output NDF of the requested or computed dimensions.
*     Initialise and map its data array.
*     -  For an auto file, read the data directly from the input file
*     to the NDF.  Otherwise fill the data array with the data values
*     using the associated co-ordinates, converting from n-d positions
*     to a vector element (for n-d generality).
*     -  Obtain the NDF title. Unmap the NDF and close the NDF context.
*     -  Tidy the work space and close the input file.

*  Related Applications:
*     CONVERT: ASCII2NDF, NDF2ASCII; SPECDRE: ASCIN, ASCOUT.

*  Copyright:
*     Copyright (C) 1990-1992 Science & Engineering Research Council.
*     Copyright (C) 1995-1996, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.
*
*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1990 June 15 (MJC):
*        Original NDF version, derived from the previous non-NDF
*        routine of the same number, featuring generic data processing
*        and n-d arrays.
*     1991 May 27 (MJC):
*        Writes axis centres to the NDF in co-ordinate mode.
*     1991 July 25 (MJC):
*        Output NDF parameter now called NDF for consistency with other
*        applications.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1995 November 16 (MJC):
*        Added LBOUND parameter, and creates simple rather than
*        primitive NDFs.
*     1996 November 14 (MJC):
*        Added a final error report, and improved the documentation.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2010 August 25 (MJC):
*        Used KPG_DIMLS instead of old DIMLST.
*     20-SEP-2011 (DSB):
*        Added parameter POFFSET.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'FIO_ERR'          ! Fortran-I/O-system errors
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER KPG1_FLOOR         ! Largest int smaller than a given float

*  Local Constants:
      INTEGER MAXREC             ! Maximum number of values per record
      PARAMETER ( MAXREC = 65536 ) ! up to 65536 values per record

      INTEGER NCHLIN             ! Maximum number of characters in a
                                 ! an input record
      PARAMETER ( NCHLIN = 132 )

*  Local Variables:
      LOGICAL AUTO               ! Co-ordinates are not supplied, merely
                                 ! data values?
      INTEGER AXPNTR( 1 )        ! Pointer to axis centres
      LOGICAL BAD                ! The output array is to be initialised
                                 ! with bad pixels? Otherwise zeroes are
                                 ! inserted
      CHARACTER * ( 80 ) BUF     ! Text buffer for shape information
      LOGICAL CMPLET             ! Completely read the text file?
      INTEGER COUNT              ! Number of data points input
      INTEGER DLBND( DAT__MXDIM ) ! Default lower bounds of output array
      CHARACTER * ( 32 ) DIMSTR  ! Dimensions
      CHARACTER * ( DAT__SZTYP ) DTYPE ! HDS type of the data values
      INTEGER FD                 ! File description
      INTEGER I                  ! Loop counter
      CHARACTER * ( 4 ) INIT     ! Initialisation option for the output
                                 ! data array
      CHARACTER * ( DAT__SZTYP ) ITYPE ! Implemention HDS type
      INTEGER J                  ! Loop counter
      REAL LBND( DAT__MXDIM )    ! Minimum co-ordinates input alias the
                                 ! origin co-ordinates
      INTEGER MXCOL              ! Largest column number containing
                                 ! co-ordinate data
      INTEGER NC                 ! Number of characters in BUF
      INTEGER NDFOUT             ! NDF identifier
      INTEGER NDIMS              ! Number of dimensions of output array
      INTEGER NELM               ! Number of elements in the output
                                 ! array
      INTEGER ODIMS( DAT__MXDIM ) ! Dimensions of output array
      LOGICAL OFFNUL              ! Was NULL supplied for parameter POFFSET?
      INTEGER OLBND( DAT__MXDIM ) ! Lower bounds of output array
      INTEGER OUBND( DAT__MXDIM ) ! Upper bounds of output array
      INTEGER PACKET             ! Work array initial size and
                                 ! incremental size
      INTEGER PNTRO( 1 )         ! Pointer to output DATA_ARRAY
      REAL POFFSET( DAT__MXDIM ) ! Offset from supplied to pixel coordinates
      INTEGER POSCOD( DAT__MXDIM + 1 ) ! Positions of co-ordinates and data
                                 ! values in records respectively
      LOGICAL POSDUP             ! Column positions for co-ordinates and
                                 ! values duplicated?
      REAL PSCALE( DAT__MXDIM )  ! Pixel-to-pixel scales
      REAL PSDEF( DAT__MXDIM )   ! Suggested default pixel-to-pixel scales
      INTEGER SIZFAC             ! Number of packets in work array
      REAL UBND( DAT__MXDIM )    ! Maximum co-ordinates input
      INTEGER WDIMSC( 2 )        ! Dimensions of the co-ords work array
      INTEGER WDIMSV( 1 )        ! Dimensions of the values work array
      CHARACTER *( DAT__SZLOC ) WLOCC ! Locator to work array for
                                 ! co-ordinates
      CHARACTER *( DAT__SZLOC ) WLOCV ! Locator to work array for data
                                 ! values
      INTEGER WPNTRC( 1 )        ! Pointer to work array for
                                 ! co-ordinates
      INTEGER WPNTRV( 1 )        ! Pointer to work array for data values
      LOGICAL WRKPOS             ! Workspace for co-ordinates obtained?

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain type of the data values.
      CALL PAR_CHOIC( 'DTYPE', '_REAL', '_REAL,_DOUBLE,_INTEGER,_WORD,'/
     :                /'_UWORD,_BYTE,_UBYTE', .TRUE., DTYPE, STATUS )

*  Since there are no character-conversion routines for byte and word
*  data types these must be read into integers, and at the end
*  converted to the requested type.
      IF ( DTYPE .EQ. '_BYTE' .OR. DTYPE .EQ. '_UBYTE' .OR.
     :     DTYPE .EQ. '_WORD' .OR. DTYPE .EQ. '_UWORD' ) THEN
         ITYPE = '_INTEGER'
      ELSE
         ITYPE = DTYPE
      END IF

*  Find whether co-ordinate information is supplied.
      CALL PAR_GTD0L( 'AUTO', .FALSE., .TRUE., AUTO, STATUS )

*  Attempt to obtain and open a free-format data file.
      CALL FIO_ASSOC( 'FREENAME', 'READ', 'LIST', 0, FD, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  The file exists, so either ask user for positions of co-ordinates
*  and data value on each line of the input free-format file or get the
*  shape of the output array depending on the mode.  In the former case
*  loop until different positions are given.
      IF ( .NOT. AUTO ) THEN
         POSDUP = .TRUE.

*  Set dynamic defaults for position columns.  If an error occurs later
*  getting the POSCOLS another default, e.g. current, may be required.
         POSCOD( 1 ) = 1
         POSCOD( 2 ) = 2
         CALL PAR_DEF1I( 'POSCOLS', 2, POSCOD, STATUS )

         DO WHILE ( POSDUP .AND. STATUS .EQ. SAI__OK )

*  Start a new error context.
            CALL ERR_MARK

*  First the co-ordinates.  Default is 2-d.
            CALL PAR_GDRVI( 'POSCOLS', DAT__MXDIM, 1, MAXREC, POSCOD,
     :                      NDIMS, STATUS )

*  Find a suitable default for the value column by assuming
*  co-ordinates come before the data values.
            MXCOL = - 1
            DO  I = 1, NDIMS
               MXCOL = MAX( MXCOL, POSCOD( I ) )
            END DO
            IF ( MXCOL .EQ. MAXREC ) MXCOL = 2

*  Now the intensity.
            CALL PAR_GDR0I( 'VALCOL', MXCOL + 1, 1, MAXREC, .FALSE.,
     :                      POSCOD( NDIMS + 1 ), STATUS )

*  Assume no duplication for the moment.
            POSDUP = .FALSE.

*  Check for duplication of a co-ordinate column with the column of
*  values or another co-ordinate column.  Could use DO WHILE to reduce
*  the number of loops when a duplication is found.  This is unlikely
*  to be common or important.
            IF ( STATUS .EQ. SAI__OK ) THEN
               DO  I = 1, NDIMS
                  DO  J = I + 1, NDIMS + 1

                     IF ( POSCOD( J ) .EQ. POSCOD( I ) ) THEN
                        CALL MSG_SETI( 'DIM', I )
                        STATUS = SAI__ERROR

*  Write an appropriate error message.
                        IF ( J .EQ. NDIMS + 1 ) THEN
                           CALL ERR_REP( 'TRANDAT_VEQCP',
     :                       'TRANDAT: Value column number is the '/
     :                       /'same as co-ordinate column ^DIM.',
     :                       STATUS )
                        ELSE
                           CALL MSG_SETI( 'DIM2', J )
                           CALL ERR_REP( 'TRANDAT_CEQCP',
     :                       'TRANDAT: Duplication of co-ordinate '/
     :                       /'column numbers ^DIM and ^DIM2.', STATUS )
                        END IF

*  Report the error immediately, and reset the status to OK.
                        CALL ERR_FLUSH( STATUS )

*  There is duplication so try again.
                        POSDUP = .TRUE.
                     END IF
                  END DO
               END DO
            END IF

*  Cancel the parameters so that the user can be re-prompted.
            CALL PAR_CANCL( 'POSCOLS', STATUS )
            CALL PAR_CANCL( 'VALCOL', STATUS )

*  End the error context.
            CALL ERR_RLSE
         END DO

*  Report an error and abort.
         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( STATUS .NE. PAR__ABORT ) THEN
               CALL ERR_REP( 'TRANDAT_DATPOS',
     :           'TRANDAT: Error getting column positions.', STATUS )
            END IF

            GOTO 980
         END IF

*  Now get the pixel-to-pixel distance, defaulting to no scaling.
         DO  I = 1, NDIMS
            PSDEF( I ) = 1.0
         END DO
         CALL PAR_GDR1R( 'PSCALE', NDIMS, PSDEF, VAL__SMLR, VAL__MAXR,
     :                   .TRUE., PSCALE, STATUS )

*  Now get the data coordinates corresponding to pixel coords
*  (0.0,0.0,...). Set a flag if a null value is supplied, and annul
*  the error. This flag indicates that the minimum value on each axis
*  should be used.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL PAR_EXACR( 'POFFSET', NDIMS, POFFSET, STATUS )
            OFFNUL = ( STATUS .EQ. PAR__NULL )
            IF( OFFNUL ) CALL ERR_ANNUL( STATUS )
         END IF

*  Now get the work-array packet size.  Tuned for 512-byte blocks.
         CALL PAR_GDR0I( 'QUANTUM', 2048, 32, 2097152, .TRUE., PACKET,
     :                   STATUS )

*  No co-ordinates so therefore there are no missing values.
      ELSE

*  Get the shape of the output NDF
         CALL PAR_GDRVI( 'SHAPE', DAT__MXDIM, 1, VAL__MAXI, ODIMS,
     :                   NDIMS, STATUS )

*  Get the lower bounds of the output NDF within the integer range,
*  defaulting to 1.  There must be the same number of bounds as
*  dimensions defined by parameter SHAPE.
         DO  I = 1, NDIMS
            DLBND( I ) = 1
         END DO
         CALL PAR_GDR1I( 'LBOUND', NDIMS, DLBND, VAL__MINI, VAL__MAXI,
     :                   .TRUE., OLBND, STATUS )

*  Derive the upper bounds of the output NDF.
         DO  I = 1, NDIMS
            OUBND( I ) = OLBND( I ) + ODIMS( I ) - 1
         END DO
      END IF

*  Get the initialisation state for the output data array.
      CALL PAR_GTD0L( 'BAD', .TRUE., .TRUE., BAD, STATUS )

*  Check for an error.
      IF ( STATUS .NE. SAI__OK ) GOTO 980

      IF ( .NOT. AUTO ) THEN

*  Get an initial quantity of work space to store up to PACKET pixels.
*  First the co-ordinates...
         WRKPOS = .FALSE.
         WDIMSC( 1 ) = NDIMS
         WDIMSC( 2 ) = PACKET
         CALL AIF_GETVM( '_REAL', 2, WDIMSC, WPNTRC, WLOCC, STATUS )
         IF ( STATUS .EQ. SAI__OK ) WRKPOS = .TRUE.

*  Next the data values.  Note that the implementation type is used so
*  that byte and word data may be read.
         WDIMSV( 1 ) = PACKET
         CALL AIF_GETVM( ITYPE, 1, WDIMSV, WPNTRV, WLOCV, STATUS )

*  Report error context, tidy the work array and abort.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'TRANDAT_WSP',
     :        'TRANDAT: Unable to get workspace to store the '/
     :        /'co-ordinate-and-value data.', STATUS )
            CALL AIF_ANTMP( WLOCV, STATUS )
            IF ( WRKPOS ) CALL AIF_ANTMP( WLOCC, STATUS )
            GOTO 980
         END IF

*  Initialise some values for the loop.
         CMPLET = .FALSE.
         COUNT = 1
         SIZFAC = 1
         DO  I = 1, NDIMS
            UBND( I ) = VAL__MINR
            LBND( I ) = VAL__MAXR
         END DO

*  Read the file into the work array.  If the allocated space is not
*  sufficient to hold the points a further multiple of PACKET points is
*  obtained, and the reading routine is called again, continuing where
*  it left off.
         DO WHILE ( STATUS .EQ. SAI__OK .AND. ( .NOT. CMPLET ) )

*  Choose the appropriate routine for each data type.
            IF ( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPS1_TRDRD( FD, WDIMSC( 1 ), WDIMSC( 2 ), POSCOD,
     :                          COUNT, %VAL( CNF_PVAL( WPNTRC( 1 ) ) ),
     :                          %VAL( CNF_PVAL( WPNTRV( 1 ) ) ),
     :                          LBND, UBND, CMPLET,
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPS1_TRDRI( FD, WDIMSC( 1 ), WDIMSC( 2 ), POSCOD,
     :                          COUNT, %VAL( CNF_PVAL( WPNTRC( 1 ) ) ),
     :                          %VAL( CNF_PVAL( WPNTRV( 1 ) ) ),
     :                          LBND, UBND, CMPLET,
     :                          STATUS )

            ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
               CALL KPS1_TRDRR( FD, WDIMSC( 1 ), WDIMSC( 2 ), POSCOD,
     :                          COUNT, %VAL( CNF_PVAL( WPNTRC( 1 ) ) ),
     :                          %VAL( CNF_PVAL( WPNTRV( 1 ) ) ),
     :                          LBND, UBND, CMPLET,
     :                          STATUS )
            END IF

            IF ( .NOT. CMPLET .AND. STATUS .EQ. SAI__OK ) THEN

*  Insufficient room, so increase the work space size.
*  ===================================================
               SIZFAC = SIZFAC + 1
               WDIMSC( 2 ) = PACKET * SIZFAC
               WDIMSV( 1 ) = WDIMSC( 2 )

*  Unmap the work arrays so that they can be extended.
               CALL DAT_UNMAP( WLOCC, STATUS )
               CALL DAT_UNMAP( WLOCV, STATUS )

*  Enlarge the work arrays.   Note this would not be possible
*  for the co-ordinate array if the axes were reversed.
               CALL DAT_ALTER( WLOCC, 2, WDIMSC, STATUS )
               CALL DAT_ALTER( WLOCV, 1, WDIMSV, STATUS )

*  Re-map the work arrays, but this time with update mode in order to
*  retain those values already read from the text file.
               CALL DAT_MAP( WLOCC, '_REAL', 'UPDATE', 2, WDIMSC,
     :                       WPNTRC, STATUS )
               CALL DAT_MAP( WLOCV, ITYPE, 'UPDATE', 1, WDIMSV,
     :                       WPNTRV, STATUS )

               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'TRANDAT_WSP',
     :              'TRANDAT: Unable to get workspace to store the '/
     :              /'co-ordinate and value data.', STATUS )
                  GOTO 970
               END IF

*  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            END IF
         END DO

*  Exit if any other error occurred in the loop.
         IF ( STATUS .NE. SAI__OK .AND. STATUS .NE. FIO__EOF ) GOTO 970

*  Tell the user the number of data points found, and the maximum and
*  minimum co-ordinate values for each dimension.
         CALL MSG_OUT( 'BLANK', ' ', STATUS )
         CALL MSG_SETI( 'COUNT', COUNT )
         CALL MSG_OUT( 'HOW_MANY', 'Number of data points input was '/
     :                 /'^COUNT.', STATUS )

         DO  I = 1, NDIMS
            CALL MSG_SETR( 'POSMIN', LBND( I ) )
            CALL MSG_SETR( 'POSMAX', UBND( I ) )
            CALL MSG_SETI( 'N', I )
            CALL MSG_OUT( 'POS_LIMITS', 'Dimension ^N: co-ordinate '/
     :        /'minimum was ^POSMIN, maximum was ^POSMAX.', STATUS )

*  Store the offset to use if a null value was given for parameter POFFSET.
            IF( OFFNUL ) POFFSET( I ) = LBND( I )

*  From these and the input pixel-to-pixel distance and offset, we can
*  calculate the dimensions of the output array, and hence the bounds.
            OLBND( I ) = KPG1_FLOOR( ( LBND( I ) - POFFSET( I ) ) /
     :                               PSCALE( I ) ) + 1
            OUBND( I ) = KPG1_FLOOR( ( UBND( I ) - POFFSET( I ) ) /
     :                               PSCALE( I ) ) + 1
            ODIMS( I ) = OUBND( I ) - OLBND( I ) + 1
         END DO

*  Tell user the output array bounds.
         NC = 0
         DO I = 1, NDIMS
            IF ( I .GT. 1 ) CALL CHR_PUTC( ', ', BUF, NC )
            CALL CHR_PUTI( OLBND( I ), BUF, NC )
            CALL CHR_PUTC( ':', BUF, NC )
            CALL CHR_PUTI( OUBND( I ), BUF, NC )
         END DO
         CALL MSG_SETC( 'BNDS', BUF( : NC ) )
         CALL MSG_OUT( 'OUT_DIMS', 'Output array pixel bounds are '//
     :                 '(^BNDS).', STATUS )
         CALL MSG_OUT( 'BLANK', ' ', STATUS )
      END IF

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Create the output NDF structure.
      CALL LPG_CREAT( 'NDF', DTYPE, NDIMS, OLBND, OUBND, NDFOUT,
     :                STATUS )

*  Set the initialisation control string.  Set the (NDF) bad-pixel
*  flag.
      IF ( BAD ) THEN
         INIT = 'BAD'
      ELSE
         INIT = 'ZERO'
      END IF

*  Map the output NDF's DATA component with write access.  Note the use
*  of the implementation type.
      CALL KPG1_MAP( NDFOUT, 'DATA', ITYPE, 'WRITE/'//INIT, PNTRO,
     :              NELM, STATUS )

      IF ( .NOT. AUTO ) THEN

*  Now call the subroutine that does the work of filling the array with
*  the input data, given co-ordinate information.  Choose the
*  appropriate one for the data type.
         IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_TRNVD( NDIMS, OLBND, OUBND, COUNT,
     :                       %VAL( CNF_PVAL( WPNTRC( 1 ) ) ),
     :                       %VAL( CNF_PVAL( WPNTRV( 1 ) ) ),
     :                       PSCALE, POFFSET,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_TRNVI( NDIMS, OLBND, OUBND, COUNT,
     :                       %VAL( CNF_PVAL( WPNTRC( 1 ) ) ),
     :                       %VAL( CNF_PVAL( WPNTRV( 1 ) ) ),
     :                       PSCALE, POFFSET,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_TRNVR( NDIMS, OLBND, OUBND, COUNT,
     :                       %VAL( CNF_PVAL( WPNTRC( 1 ) ) ),
     :                       %VAL( CNF_PVAL( WPNTRV( 1 ) ) ),
     :                       PSCALE, POFFSET,
     :                       %VAL( CNF_PVAL( PNTRO( 1 ) ) ), STATUS )

         END IF

*  Create the axis data.
*  =====================
         DO  I = 1, NDIMS

*  First create and map the axis centres.
            CALL NDF_AMAP( NDFOUT, 'Centre', I, '_REAL', 'WRITE',
     :                     AXPNTR, ODIMS( I ), STATUS )

*  Write a linear axis using the base and offset.  Note lower bound is
*  floating point, not integer.
            CALL KPG1_SSCOF( ODIMS( I ), DBLE( PSCALE( I ) ),
     :                       DBLE( POFFSET( I ) ) +
     :                       0.5D0 * DBLE( PSCALE( I ) ),
     :                       %VAL( CNF_PVAL( AXPNTR( 1 ) ) ), STATUS )

*  Unmap the axis centres.
            CALL NDF_AUNMP( NDFOUT, 'Centre', I, STATUS )
         END DO
      ELSE

*  Append sequentially each value to the vector.  Choose the
*  appropriate subroutine for the data type.
         IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_TRNDD( FD, NELM, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_TRNDI( FD, NELM, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_TRNDR( FD, NELM, %VAL( CNF_PVAL( PNTRO( 1 ) ) ),
     :                       STATUS )

         END IF
      END IF

*  Set the NDF's bad-pixel flag.
      CALL NDF_SBAD( BAD, NDFOUT, 'Data', STATUS )

*  Obtain a title for the output NDF.
      CALL NDF_CINP( 'TITLE', NDFOUT, 'TITLE', STATUS )

*  Unmap the output data array. Implicit type conversion of byte and
*  word types from the integer implementation type will occur.
      CALL NDF_UNMAP( NDFOUT, 'DATA', STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

  970 CONTINUE

*  Tidy the workspace.
      IF ( .NOT. AUTO ) THEN
         CALL AIF_ANTMP( WLOCC, STATUS )
         CALL AIF_ANTMP( WLOCV, STATUS )
      END IF

  980 CONTINUE

*  On return, close free-format data file.
      CALL FIO_ANNUL( FD, STATUS )

  999 CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'TRANDAT_ERR',
     :     'TRANDAT: Error creating an NDF from data stored in a text '/
     :     /'file.', STATUS )
      END IF

      END
