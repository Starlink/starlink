      SUBROUTINE FINDCENT( STATUS )
*+
*  Name:
*     FINDCENT

*  Purpose:
*     Centroids image features.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FINDCENT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine determines the centroids of image features located
*     in the data components of a list of NDFs. It is useful for
*     locating accurate values for the positions of stars given hand
*     selected positions. It can also be used for centroiding any
*     sufficiently peaked image features.
*
*     The initial positions associated with each NDF are given in
*     formatted files whose names are determined either using the
*     CCDPACK NDF extension item CURRENT_LIST (which is maintained by
*     list processing CCDPACK applications) or from an explicit list of
*     names.

*  Usage:
*     findcent in outlist

*  ADAM Parameters:
*     AUTOSCALE = _LOGICAL (Read)
*        Whether to "automatically" adjust the centroid location
*        parameters to reflect the fact that picking good initial
*        positions is less likely when dealing with very large images
*        (these tend to be displayed using one display pixel to
*        represent many image pixels).
*
*        If TRUE then the values of the parameters ISIZE, TOLER and
*        MAXSHIFT are scaled by an amount that maps the largest
*        dimension of each input image to an image of size 1024
*        square (so an image of size 2048 square will have these
*        parameters increased by a factor of two).
*        [FALSE]
*     IN = LITERAL (Read)
*        The names of the NDFs whose data components contain image
*        features which are to be centroided.  The NDF names should be
*        separated by commas and may include wildcards.
*     INLIST = LITERAL (Read)
*        If NDFNAMES is FALSE then this parameter will be used to
*        access the names of the lists which contain the initial
*        positions. The format of the data in the files is described in
*        the notes section.
*
*        The names of the input lists may use modifications of the
*        input NDF names, so for instance if the position lists are
*        stored in files with the same name as the input NDFs but with
*        a file type of ".dat" instead of ".sdf" then use
*
*           INLIST > *.dat
*
*        (.sdf is always removed from NDF names). If the input list
*        names are a modification of the NDF names say with a trailing
*        type of "_initial.positions". Then a response of
*
*           INLIST > *_initial.positions
*
*        will access the correct files. Names may also use substitution
*        elements, say the NDF names are *_data and the position lists
*        are *_pos.dat, then a response like
*
*            INLIST > *|data|pos.dat|
*
*        may be used. If a naming scheme has not been used then an
*        explicit list of names should be returned (wildcards cannot be
*        used to specify list names). These names should be given in
*        the same order as the input NDF names and may use indirection
*        elements as well as names separated by commas. A listing of
*        the input NDF name order (after any wildcard expansions etc.
*        have been made) is shown to make sure that the order is
*        correct.
*     ISIZE = _INTEGER (Read)
*        The size of a box side (in pixels) centered on current
*        position which will be used to form the marginal profiles used
*        to estimate the centroid.
*        [9]
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter, then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP,
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
*        If the logging system has been initialised using CCDSETUP,
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     MAXITER = _INTEGER (Read)
*        The maximum number of iterations which may be used in
*        estimating the centroid. Only used if the tolerance criterion
*        is not met in this number of iterations.
*        [3]
*     MAXSHIFT = _DOUBLE (Read)
*        The maximum shift (in pixels) allowed from an initial position.
*        [5.5]
*     NAMELIST = LITERAL (Read)
*        Only used if NDFNAMES is FALSE. If this is the case then this
*        specifies the name of a file to contain a listing of the names
*        of the output lists. This file may then be used to pass the
*        names onto another CCDPACK application using indirection.
*        [FINDCENT.LIS]
*     NDFNAMES = _LOGICAL (Read)
*        If TRUE then the routine will assume that the names of the
*        input position lists are stored in the CCDPACK extension item
*        "CURRENT_LIST" of the input NDFs. The names will be present
*        in the extension if the positions were located using a CCDPACK
*        application (such as IDICURS). Using this facility allows the
*        transparent propagation of position lists through processing
*        chains.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [TRUE]
*     POSITIVE = _LOGICAL (Read)
*        If TRUE then the image features have increasing values
*        otherwise they are negative.
*        [TRUE]
*     OUTLIST = FILENAME (Write)
*        A list of names specifying the centroid result files. The
*        names of the lists may use modifications of the input NDF
*        names.  So if you want to call the output lists the same name
*        as the input NDFs except to add a type use.
*
*           OUTLIST > *.cent
*
*        Or alternatively you can use an explicit list of names.
*        These may use indirection elements as well as names separated
*        by commas.
*        [*.cent]
*     TOLER = _DOUBLE (Read)
*        The required tolerance in the positional accuracy of the
*        centroid. On each iteration the box of data from which the
*        centroid is estimated is updated. If the new centroid does not
*        differ from the previous value by more than this amount (in X
*        and Y) then iteration stops. Failure to meet this level of
*        accuracy does not result in the centroid being rejected, the
*        centroiding process just stops after the permitted number of
*        iterations (MAXITER).
*        [0.05]

*  Examples:
*     findcent in='*' outlist='*.cent'
*        In this example all the NDFs in the current directory are
*        processed. It is assumed that the NDFs are associated with
*        positions lists of inaccurate positions (via the item
*        CURRENT_LIST in the NDF CCDPACK extensions). These position
*        lists are accessed and centroided with the appropriate NDFs.
*        On exit the new lists are named *.cent and are associated with
*        the NDFs (instead of the original "input" lists).
*
*     findcent ndfnames=false in='"ndf1,ndf2,ndf3"'
*              inlist='"ndf1.pos,ndf2.pos,ndf3.pos"' outlist='*.acc'
*              namelist=new_position_lists
*        In this example the position list names are not previously
*        associated with the NDFs and must have their names given
*        explicitly (and in the same order as the NDF names). The
*        output lists are called the same names as the input NDFs except
*        with the extension .acc. The names of the output lists are
*        written into the file new_position_lists which can be used to
*        pass these names onto another application using indirection
*        (in which invoke the next application with ndfnames=false
*        inlist=^new_position_lists).

*  Implementation Status:
*     - This routine correctly processes the DATA and QUALITY components
*       of an NDF data structure. Bad pixels and all non-complex numeric
*       data types can be handled.

*  Implementation Deficiencies:
*     - There is no support positions other than in pixel coordinates.
*     - No use is made of variance information.

*  Notes:
*     - Position list formats.
*
*       CCDPACK supports data in two formats.
*
*       CCDPACK format - the first three columns are interpreted as the
*       following.
*
*          - Column 1: an integer identifier
*          - Column 2: the X position
*          - Column 3: the Y position
*
*       The column one value must be an integer and is used to identify
*       positions which are the same but which have different locations
*       on different images. Values in any other (trailing) columns are
*       usually ignored.
*
*       EXTERNAL format - positions are specified using just an X and
*       a Y entry and no other entries.
*
*          - Column 1: the X position
*          - Column 2: the Y position
*
*       This format is used by KAPPA applications such as CURSOR.
*
*       Comments may be included in a file using the characters "#" and
*       "!". Columns may be separated by the use of commas or spaces.
*
*       Data following the third column is copied without modification
*       into the results files
*
*       In all cases, the coordinates in position lists are pixel
*       coordinates.
*
*     - NDF extension items.
*
*       If NDFNAMES is TRUE then the item "CURRENT_LIST" of the
*       .MORE.CCDPACK structure of the input NDFs will be located
*       and assumed to contain the names of the lists whose positions
*       are to be centroided. On exit this item will be updated to
*       reference the name of the centroided list of positions.

*  Behaviour of Parameters:
*     All parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when using the application
*     on new datasets or after a break of sometime.  The intrinsic
*     default behaviour of the application may be restored by using the
*     RESET keyword on the command line.
*
*     Certain parameters (LOGTO, LOGFILE and NDFNAMES) have global
*     values. These global values will always take precedence, except
*     when an assignment is made on the command line.  Global values may
*     be set and reset using the CCDSETUP and CCDCLEAR commands.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997, 1999-2001 Central Laboratory of the
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
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUL-1992 (PDRAPER):
*        Original version.
*     8-MAR-1993 (PDRAPER):
*        First version for release.
*     6-OCT-1995 (PDRAPER):
*        Updated for CCDPACK 2.0.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator control (foreign data access upgrades).
*     1-MAR-1999 (PDRAPER):
*        Added autoscale parameter.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     10-APR-2001 (MBT):
*        Slight changes to the way it writes output lists.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PAR_ERR'         ! Parameter system error codes
      INCLUDE 'DAT_PAR'         ! HDS/DAT parameterisations
      INCLUDE 'NDF_PAR'         ! NDF parameterisations
      INCLUDE 'FIO_PAR'         ! FIO system parameters
      INCLUDE 'GRP_PAR'         ! GRP system constants
      INCLUDE 'CCD1_PAR'        ! CCDPACK parameterisations
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION AUTOSZ
      PARAMETER ( AUTOSZ = 1024.0D0 ) ! Typical size of image

*  Local Variables:
      CHARACTER * ( 6 ) ACCESS  ! NDF access mode.
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer used to read in data
      CHARACTER * ( FIO__SZFNM ) FNAME ! Position list filename buffer
      CHARACTER * ( FIO__SZFNM ) NDFNAM ! NDF filename buffer
      CHARACTER * ( NDF__SZTYP ) TYPE ! Input NDF data type
      DOUBLE PRECISION CSCALE   ! Automatic centroid scaling factor
      DOUBLE PRECISION MAXSHF   ! Maximum shift in position
      DOUBLE PRECISION MAXSHS   ! Autoscaled maximum shift in position
      DOUBLE PRECISION TOLER    ! Tolerance required in centroid
      DOUBLE PRECISION TOLERS   ! Autoscaled tolerance required in centroid
      INTEGER DUMMY             ! Integer
      INTEGER EL                ! Number of elements in input NDF
      INTEGER FDIN              ! Input file identifier
      INTEGER FDOUT             ! Output file identifier
      INTEGER FIOGR             ! Input position list name group
      INTEGER FIOGRO            ! Output position list name group
      INTEGER I                 ! Loop counter
      INTEGER IDIN              ! Input NDF identifier
      INTEGER IPDAT             ! Pointer to positions data
      INTEGER IPDIN             ! Input position identifiers
      INTEGER IPDOUT            ! Input position identifiers
      INTEGER IPIN              ! Pointer to input data array
      INTEGER IPXIN             ! Pointer to input X positions
      INTEGER IPXOUT            ! Pointer to output X positions
      INTEGER IPYIN             ! Pointer to input Y positions
      INTEGER IPYOUT            ! Pointer to output Y positions
      INTEGER ISIZE             ! Size of box to search
      INTEGER ISIZES            ! Autoscaled size of box to search
      INTEGER LBND( 2 )         ! Lower bounds of NDF data component
      INTEGER MAXIT             ! Maximum number of iterations
      INTEGER NCOL              ! First dimension of input NDF
      INTEGER NDFGR             ! Input group of NDF names
      INTEGER NDIM              ! Dimensionality of input NDF
      INTEGER NLGR              ! Group of NDFs with no associated lists
      INTEGER NLINE             ! Second dimension of input NDF
      INTEGER NNDF              ! Number of input NDFs
      INTEGER NNOLIS            ! Number of NDFs with no associated lists
      INTEGER NOUT              ! Number of centroids output
      INTEGER NREC              ! Number of records read from positions file
      INTEGER NVAL              ! Number values per input record
      INTEGER UBND( 2 )         ! Upper bounds of NDF data component
      LOGICAL AUTOSC            ! Whether to scale centroid parameters by image size
      LOGICAL NDFS              ! True if position lists are named in NDF extensions
      LOGICAL SIGN              ! If true features are positive

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start the log system.
      CALL CCD1_START( 'FINDCENT', STATUS )

*=======================================================================
*  Get input NDFs & files
*=======================================================================
*  See how the user wants to supply the positions list information. This
*  may be provided using either the named extension item CURRENT_LIST
*  within the NDFs or by directly supplying the names. If directly
*  suppling the names then list the names of the input NDFs.
      NDFS = .TRUE.
      CALL PAR_GET0L( 'NDFNAMES', NDFS, STATUS )
      IF ( NDFS ) THEN

*  Enable UPDATE mode to allow the extension to be modified.
         ACCESS = 'UPDATE'
      ELSE
         ACCESS = 'READ'
      END IF

*  Get the names of the NDFs to be searched for the object centroids.
      CALL NDF_BEGIN
      IF ( NDFS ) THEN

*  NDF names will also supply the position list names.
         CALL CCD1_GTLIG( .TRUE., 'CURRENT_LIST', 'IN', 1, CCD1__MXNDF,
     :                    .FALSE., NNDF, FIOGR, NDFGR, NNOLIS, NLGR,
     :                    STATUS )
         CALL CCD1_GRDEL( NLGR, STATUS )

*  If not all supplied NDFs have position lists, warn the user of
*  this fact and continue.
         IF ( NNOLIS .GT. 0 ) THEN
            CALL CCD1_MSG( ' ', '  NDFs with no associated position '//
     :                     'lists will be ignored.', STATUS )
            CALL CCD1_MSG( ' ', ' ', STATUS )
         END IF
      ELSE

*  Just get a group of NDF names.
         CALL CCD1_NDFGR( 'IN', NDFGR, NNDF, STATUS )

*  Write out the NDF names so that the comparison between the lists
*  and NDFs is obvious.
         CALL CCD1_MSG( ' ', '  ', STATUS )
         CALL CCD1_MSG( ' ', '    Input NDF names  ', STATUS )
         CALL CCD1_MSG( ' ', '    ---------------', STATUS )
         DO 1 I = 1, NNDF
            CALL GRP_GET( NDFGR, I, 1, NDFNAM, STATUS )
            CALL MSG_SETC( 'NDFNAM', NDFNAM )
            CALL MSG_SETI( 'N', I )
            CALL CCD1_MSG( ' ', '  ^N) ^NDFNAM', STATUS )
 1       CONTINUE
         CALL CCD1_MSG( ' ', '  ', STATUS )

*  And get the names of the position lists, using the NDF names as a
*  modification element list.
         CALL CCD1_GTMLG( 'INLIST', NDFGR, NNDF, NNDF, DUMMY, FIOGR,
     :                    STATUS )
      END IF

*=======================================================================
*  Get the names of the output position lists. Use the NDF input group
*  as a modification group for these names.
*=======================================================================
      CALL CCD1_STRGR( 'OUTLIST', NDFGR, NNDF, NNDF, FIOGRO, DUMMY,
     :                 STATUS )

*=======================================================================
*  Get the parameters controlling the centroiding process.
*=======================================================================

*  See if autoscaling of the parameters is required. This attempts to
*  adapt to the size of the image, since larger images will tend to have
*  less accurate initial positions (since they are displayed at lower
*  resolution). The scaling works assuming that an image AUTOSZ square
*  does not need any scaling, otherwise the search box and maximum
*  shift are scaled to normalise to an image this big.
      CALL PAR_GET0L( 'AUTOSCALE', AUTOSC, STATUS )

*  Size of the search box. Larger than 3 and odd.
      CALL PAR_GET0I( 'ISIZE', ISIZE, STATUS )
      ISIZE = MAX( 3, ISIZE )
      ISIZE = ( ISIZE / 2 ) * 2 + 1

*  The maximum shift in the centroid position.
      CALL PAR_GET0D( 'MAXSHIFT', MAXSHF, STATUS )
      MAXSHF = MAX( 0.0001D0, MAXSHF )

*  The maximum number of iterations to achieve tolerance.
      CALL PAR_GET0I( 'MAXITER', MAXIT, STATUS )
      MAXIT = MAX( 1, MAXIT )

*  The tolerance in the positions estimate that is required.
      CALL PAR_GET0D( 'TOLER', TOLER, STATUS )
      TOLER = ABS( TOLER )

*  The sign of the features to cenroid +ve or -ve.
      CALL PAR_GET0L( 'POSITIVE', SIGN, STATUS )

*  Write log information about these parameters
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Centroid parameters', STATUS )
      CALL CCD1_MSG( ' ', '    -------------------', STATUS )

*  Size of centroid box side.
      CALL MSG_SETI( 'ISIZE', ISIZE )
      CALL CCD1_MSG( ' ', '  Search box side: ^ISIZE', STATUS )

*  Maximum shift in position.
      CALL MSG_SETD( 'MAXSHIFT', MAXSHF )
      CALL CCD1_MSG( ' ',
     : '  Maximum shift allowed in position: ^MAXSHIFT' , STATUS )

*  Maximum number of iterations.
      CALL MSG_SETI( 'MAXIT', MAXIT )
      CALL CCD1_MSG( ' ',
     : '  Maximum number of iterations: ^MAXIT', STATUS)

*  Tolerence in centroids.
      CALL MSG_SETD( 'TOLER', TOLER )
      CALL CCD1_MSG( ' ' , '  Minimum tolerance: ^TOLER', STATUS )

*  Sign of features.
      IF ( SIGN ) THEN
         CALL CCD1_MSG( ' ', '  Locating positive features', STATUS )
      ELSE
         CALL CCD1_MSG( ' ', '  Locating negative features', STATUS )
      END IF

*  Note if autoscaling centroid parameters.
      IF ( AUTOSC ) THEN
         CALL MSG_SETD( 'AUTOSZ', AUTOSZ )
         CALL CCD1_MSG( ' ',
     : '  * tuning parameters to image size: ^AUTOSZ x ^AUTOSZ',STATUS )
      END IF

*=======================================================================
* Main loop - process pairs of NDFs and list names.
*=======================================================================
      DO 2 I = 1, NNDF

*  Access the NDF associated with this position list.
         CALL NDG_NDFAS( NDFGR, I, ACCESS, IDIN, STATUS )

*  Write informational message about it.
         CALL CCD1_MSG( ' ',  ' ', STATUS )
         CALL NDF_MSG( 'CURRENT_NDF', IDIN )
         CALL CCD1_MSG( ' ', '  +++ Processing NDF: ^CURRENT_NDF',
     :                  STATUS )

*  Inform user how many NDFs we've processed out of the total number.
         CALL MSG_SETI( 'CURRENT_NUM', I )
         CALL MSG_SETI( 'MAX_NUM', NNDF )
         CALL CCD1_MSG( ' ', '  (Number ^CURRENT_NUM of ^MAX_NUM)',
     :                  STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )

*  Get the size of the data array.
         CALL NDF_BOUND( IDIN, 2, LBND, UBND, NDIM, STATUS )
         NCOL = UBND( 1 ) - LBND( 1 ) + 1
         NLINE = UBND( 2 ) - LBND( 2 ) + 1

*  Get the data type of the NDF data component.
         CALL NDF_TYPE( IDIN, 'Data', TYPE, STATUS )

*  Map in the data component of the NDF.
         CALL NDF_MAP( IDIN, 'Data', TYPE, 'READ', IPIN, EL, STATUS )

*  Get the name of the position list from the group and open the file.
         CALL GRP_GET( FIOGR, I, 1, FNAME, STATUS )
         CALL CCD1_OPFIO( FNAME, 'READ', 'LIST', 0, FDIN, STATUS )

*  Determine the number of fields per input record.
         CALL CCD1_LTEST( FDIN, LINE, CCD1__BLEN, 2, 0, NVAL, STATUS )

*  Only attempt to process these data if the file was not empty.
         IF ( NVAL .GT. 0 ) THEN

*  Now map them in. If the number of fields in the file is 2 then
*  the values are interpreted as X and Y, otherwise the standard file
*  format is assumed.
            IF ( NVAL .EQ. 2 ) THEN

*  Map in X and Y positions.
               CALL CCD1_NLMAP( FDIN, LINE, CCD1__BLEN, IPDAT, NREC,
     :                          NVAL, STATUS )

*  Generate some identifiers to go with these positions
               CALL CCD1_MALL( NREC, '_INTEGER', IPDIN, STATUS )
               CALL CCD1_GISEQ( 1, 1, NREC, %VAL( CNF_PVAL( IPDIN ) ),
     :                          STATUS )
            ELSE

*  Standard file format map these in.
               CALL CCD1_LMAP( FDIN, LINE, CCD1__BLEN, IPDIN, IPDAT,
     :                         NREC, NVAL, STATUS )
            END IF

*  Get workspace for storing the X and Y values.
            CALL CCD1_MALL( NREC, '_DOUBLE', IPXIN, STATUS )
            CALL CCD1_MALL( NREC, '_DOUBLE', IPYIN, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Extract the values from the mapped positions data array.
            CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT ) ), NREC, NVAL, 1,
     :                      %VAL( CNF_PVAL( IPXIN ) ),
     :                      STATUS )
            CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT ) ), NREC, NVAL, 2,
     :                      %VAL( CNF_PVAL( IPYIN ) ),
     :                      STATUS )

*  Get memory for centroiding results.
            CALL CCD1_MALL( NREC, '_DOUBLE', IPXOUT, STATUS )
            CALL CCD1_MALL( NREC, '_DOUBLE', IPYOUT, STATUS )
            CALL CCD1_MALL( NREC, '_INTEGER', IPDOUT, STATUS )

*  If autoscaling adjust the centroid parameters. Note parameters are
*  not allowed to be smaller.
            IF ( AUTOSC ) THEN
               CSCALE = MAX( DBLE( NCOL )/AUTOSZ, DBLE( NLINE )/AUTOSZ )
               CSCALE = MAX( 1.0D0, CSCALE )
            ELSE
               CSCALE = 1.0D0
            END IF
            ISIZES = NINT( CSCALE * ISIZE )
            ISIZES = ( ISIZES / 2 ) * 2 + 1  ! Make sure it's odd
            MAXSHS = CSCALE * MAXSHF
            TOLERS = CSCALE * TOLER

*  Perform the centroiding.
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            CALL CCD1_CENT( TYPE, IPIN, NCOL, NLINE, LBND,
     :                      %VAL( CNF_PVAL( IPDIN ) ),
     :                      %VAL( CNF_PVAL( IPXIN ) ),
     :                      %VAL( CNF_PVAL( IPYIN ) ),
     :                      NREC, ISIZES, SIGN, MAXSHS, MAXIT, TOLERS,
     :                      %VAL( CNF_PVAL( IPDOUT ) ),
     :                      %VAL( CNF_PVAL( IPXOUT ) ),
     :                      %VAL( CNF_PVAL( IPYOUT ) ), NOUT, STATUS )

*  If there was no input file, there are no output centroids.
         ELSE
            NREC = 0
            NOUT = 0
         END IF
         IF ( STATUS .NE. SAI__OK ) THEN

*  An severe error has occured accompany the message with the file name.
            CALL NDF_MSG( 'NDF', IDIN)
            CALL FIO_FNAME( FDIN, FNAME, STATUS )
            CALL MSG_SETC( 'POS', FNAME )
            CALL ERR_REP( 'FINDCENT_NOCENT',
     :      '  No image feature centroids found in NDF ^NDF, '//
     :      'using positions ^POS', STATUS )
            GO TO 99
         END IF

*  Get the name of the output list of positions and open it.
         CALL GRP_GET( FIOGRO, I, 1, FNAME, STATUS )
         CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FDOUT, STATUS )

*  Write the output results.
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL CCD1_FIOHD( FDOUT, 'Output from FINDCENT', STATUS )
         CALL CCD1_WRIXY( FDOUT, %VAL( CNF_PVAL( IPDOUT ) ),
     :                    %VAL( CNF_PVAL( IPXOUT ) ),
     :                    %VAL( CNF_PVAL( IPYOUT ) ),
     :                    NOUT, LINE, CCD1__BLEN,
     :                    STATUS )

*  Close the file.
         CALL FIO_CLOSE( FDOUT, STATUS )

*  Write the report about this loop. The input positions.
         CALL GRP_GET( FIOGR, I, 1, FNAME, STATUS )
         CALL MSG_SETC( 'FDIN', FNAME )
         CALL CCD1_MSG( ' ',
     : '  Associated positions list: ^FDIN', STATUS )

*  Number of records read from input list.
         CALL MSG_SETI( 'NREC', NREC )
         CALL CCD1_MSG( ' ',
     : '  Number of input positions: ^NREC', STATUS )

*  Output parameters: Name of output position list.
         CALL GRP_GET( FIOGRO, I, 1, FNAME, STATUS )
         CALL MSG_SETC( 'FDOUT', FNAME )
         CALL CCD1_MSG( ' ', '  Output positions list: ^FDOUT', STATUS )

*  Number of entries.
         CALL MSG_SETI( 'NOUT', NOUT )
         CALL CCD1_MSG( ' ', '  Number of output positions: ^NOUT',
     :                  STATUS )

*  Number of positions not centroided.
         IF ( NREC - NOUT .GT. 0 ) THEN
            CALL MSG_SETI( 'NFAIL', NREC - NOUT )
            CALL CCD1_MSG( ' ',
     :'  Number of positions NOT centroided: ^NFAIL', STATUS )
         END IF

*  If the position list names were accessed using the NDF extension item
*  'CURRENT_LIST' then update the NDF extension.
         IF ( NDFS ) THEN
            CALL CCG1_STO0C( IDIN, 'CURRENT_LIST', FNAME, STATUS )
         END IF

*  Close files and release resources used on this loop.
         CALL NDF_ANNUL( IDIN, STATUS )
         CALL FIO_CLOSE( FDIN, STATUS )
         CALL CCD1_MFREE( IPDAT, STATUS )
         CALL CCD1_MFREE( IPDIN, STATUS )
         CALL CCD1_MFREE( IPXOUT, STATUS )
         CALL CCD1_MFREE( IPYOUT, STATUS )
         CALL CCD1_MFREE( IPDOUT, STATUS )
         CALL CCD1_MFREE( IPXIN, STATUS )
         CALL CCD1_MFREE( IPYIN, STATUS )

*  Write terminator for Processing NDF: message.
         CALL CCD1_MSG( ' ', '  ---', STATUS )

*=======================================================================
*  End of main processing loop.
*=======================================================================
 2    CONTINUE

*  If the filenames were supplied directly then write an output list of
*  the names for other applications to use.
      IF ( .NOT. NDFS ) THEN

*  Write the names of the output files to a file which may be used for
*  indirection into other applications.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CCD1_LNAM( 'NAMELIST', 1, NNDF,
     :   '# FINDCENT - output position lists', FIOGRO, GRP__NOID,
     :                      .TRUE., STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               CALL CCD1_MSG( ' ', '  No namelist written', STATUS )
            END IF
         END IF
      END IF

*  Abort label, cleanup after this.
 99   CONTINUE

*  Release group resources.
      CALL CCD1_GRDEL( NDFGR, STATUS )
      CALL CCD1_GRDEL( FIOGR, STATUS )
      CALL CCD1_GRDEL( FIOGRO, STATUS )

*  Free any allocated dynamic memory.
      CALL CCD1_MFREE( -1, STATUS )

*  Release the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FINDCENT_ERR',
     :   'FINDCENT: Error locating object centroids.',
     :   STATUS )
      END IF

*  Close the logging system.
      CALL CCD1_END( STATUS )

      END
* $Id$
