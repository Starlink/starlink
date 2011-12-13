      SUBROUTINE CON_DST2N( FIGFIL, NDFFIL, FORM, NLEV, PATH, STATUS )
*+
*  Name:
*     CON_DST2N

*  Purpose:
*     Converts a Figaro version 2 file into an NDF (Figaro version 3
*     file)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_DST2N( FIGFIL, NDFFIL, FORM, NLEV, PATH, STATUS )

*  Description:
*     This routine reads through a file in Figaro version 2 format.
*     Objects that it finds whose function is understood---that is,
*     those objects whose purpose has been documented (in the version
*     2 Figaro structures document) will be converted to the
*     appropriate object in the output file, whose format corresponds
*     to that described in SGP/38 and which is used by Version 3 of
*     Figaro.
*
*     Non-standard objects will be copied to the .MORE.FIGARO extension.
*     FITS items will be copied to the .MORE.FITS extension.

*  Arguments:
*     FIGFIL = CHARACTER * ( * ) (Given)
*        The name of the file to be converted. The file name is assumed
*        to be the top level name with an extension '.DST'
*     NDFFIL = CHARACTER * ( * ) (Given)
*        The name of the new format file to be produced. Extension of
*        the new file is '.SDF'.
*     FORM = CHARACTER * ( * ) (Given)
*        The storage form of the data and variance arrays.  It can take
*        one of two values.  "SIMPLE" gives the simple form, and
*        "PRIMITIVE" gives the primitive form.
*     NLEV = INTEGER (Given)
*        The number of path levels to locate the NDF within the
*        container file.  This is needed in case the NDF is not the
*        top-level object in the container file.
*     PATH = CHARACTER * ( * ) (Given)
*        The path to the NDF within the output container file.
*        This is ignored when NLEV is 1.
*     STATUS = INTEGER (Given annd Returned)
*        Global status. If passed as non-zero, this routine returns
*        immediately.  If returned as zero, indicates Conversion
*        performed OK.  Non zero codes will be DSA package error codes.
*        If an error occurs, this routine will have output a detailed
*        description before returning.
*
*  Format-conversion Rules:
*     .Z.DATA  ->        .DATA_ARRAY.DATA (when FORM = "SIMPLE")
*     .Z.DATA  ->        .DATA_ARRAY (when FORM = "PRIMITIVE")
*     .Z.ERRORS ->       .VARIANCE.DATA (after processing when
*                        FORM = "SIMPLE")
*     .Z.ERRORS ->       .VARIANCE (after processing when
*                        FORM = "PRIMITIVE")
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
*
*     .OBS.OBJECT   ->   .TITLE
*     .OBS.SECZ    ->    .MORE.FIGARO.SECZ
*     .OBS.TIME    ->    .MORE.FIGARO.TIME
*     .OBS.xxxx    ->    .MORE.FIGARO.OBS.xxxx
*
*     .FITS.xxxx     ->  .MORE.FITS(n) (into value part of the string)
*     .COMMENTS.xxxx ->  .MORE.FITS(n) ( "   comment "   "   "   "   )
*     .FITS.xxxx.DATA -> .MORE.FITS(n) (into value part of the string)
*     .FITS.xxxx.DESCRIPTION -> .MORE.FITS(n)
*                                      ( "   comment "   "   "   "   )
*     .FITS.xxxx.yyyy -> .MORE.FITS(n) (in blank-keyword hierarchical)
*
*     .MORE.xxxx    ->   .MORE.xxxx
*
*     .TABLE   ->        .MORE.FIGARO.TABLE
*     .xxxx    ->        .MORE.FIGARO.xxxx
*
*     -  Axis arrays with dimensionality greater than one are not
*     supported by the NDF.  Therefore, if the application encounters
*     such an axis array, it processes the array using the following
*     rules, rather than those given above.
*
*     .X.DATA    ->      .AXIS[1].MORE.FIGARO.DATA_ARRAY
*                        (AXIS[1].DATA_ARRAY is filled with pixel
*                        co-ordinates)
*     .X.ERRORS  ->      .AXIS[1].MORE.FIGARO.VARIANCE (after
*                        processing)
*     .X.WIDTH   ->      .AXIS[1].MORE.FIGARO.WIDTH
*
*     -  In addition to creating a blank-keyword NDF FITS-extension
*     header for each component of a non-standard DST FITS structure
*     (.FITS.xxxx.yyyy where yyyy is not DATA or DESCRIPTION), this set
*     of related headers are bracketed by blank lines and a comment
*     containing the name of the structure (i.e. xxxx).
*
*  Bad-pixel Handling:
*     The QUALITY array is only copied if the bad-pixel flag
*     (.Z.FLAGGED) is false or absent.  A simple NDF with the bad-pixel
*     flag set to false (meaning that there are no bad-pixels present)
*     is created when .Z.FLAGGED is absent or false and FORM = "SIMPLE".

*  Copyright:
*     Copyright (C) 1988, 1990-1993 Science & Engineering Research
*     Council. Copyright (C) 1995-1996, 2004 Central Laboratory of the
*     Research Councils.  2009 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     JM: Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
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
*     1993 October 25 (MJC):
*        Added FORM argument to control the NDF storage format rather
*        than use the value of the bad-pixel flag to decide.
*     1995 December 19 (MJC):
*        Places an n-dimensional axis into AXIS.MORE.FIGARO.DATA_ARRAY.
*        Previously, the component name was DATA.
*     1996 February 10 (MJC):
*        Allowed for scalar width in DST.  Fixed bug which prevented a
*        missing axis being created whenever there was no FITS extension
*        to write.
*     1996 September 21 (MJC):
*        Placed the FITS writing code in a separate subroutine, so that
*        that ING hierarchical keywords may be written when the DST's
*        FITS structure contains non-standard structures.  Modified the
*        count of the FITS extension's size accordingly.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2009 June 29 (MJC):
*        Replaced deprecated CON_MOVE with KPG1_COPY from KAPLIBS.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! ADAM symbolic constants.
      INCLUDE 'DTACODES'         ! Data structure error codes
      INCLUDE 'DYNAMIC_MEMORY'   ! Dynamic memory support (defines %VAL)
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Arguments Given:
      CHARACTER*( * ) FIGFIL     ! Name of input file
      CHARACTER*( * ) FORM       ! NDF storage format
      CHARACTER*( * ) NDFFIL     ! Name of output file
      INTEGER NLEV               ! Number of structure levels in output
                                 ! file
      CHARACTER*( * ) PATH       ! Internal path to NDF in output file

*  Status:
      INTEGER STATUS             ! Global status

*  External Functions:
      INTEGER CHR_LEN            ! Length of a character string ignoring
                                 ! trailing blanks
*  Local Constants:
      INTEGER NDSTAX             ! Maximum number of axes in a DST.
      PARAMETER ( NDSTAX = 6 )

*  Local Variables:
      LOGICAL ANAXIS             ! Substructure is an axis?
      LOGICAL AXTHER( NDSTAX )   ! AXIS(1...6).DATA_ARRAY created?
      LOGICAL AXMFEX( NDSTAX )   ! AXIS(1...6).MORE.FIGARO exists?
      CHARACTER*256 AXMOR        ! AXIS.MORE name
      CHARACTER*256 AXMORF       ! AXIS.MORE.FIGARO name
      CHARACTER*256 AXNAME       ! Axis structure element name
      CHARACTER*256 AXOUT        ! Name of output axis
      CHARACTER*256 AXOUTD       ! Name of output axis data array
      CHARACTER*256 AXOUTV       ! Name of output axis variance array
      CHARACTER*256 AXOUTW       ! Name of output axis width array
      BYTE BARRAY( 100 )         ! Used to read in BYTE type data items
      INTEGER CDIMS( DAT__MXDIM ) ! Dimensions of character objects
      CHARACTER*50 COMENT        ! FITS item comment
      LOGICAL COMPRE             ! FITS item has a comment?
      INTEGER CW                 ! Length of the output width component
      INTEGER DIMS( DAT__MXDIM ) ! Dimensions of output data
      DOUBLE PRECISION DWIDTH    ! Scalar width
      INTEGER DSTAT              ! DTA_ routine returned status
      CHARACTER*256 ENVIRN       ! Environment structure name
      CHARACTER*256 EPATH        ! The effective path
      CHARACTER*64 ERROR         ! Error description
      LOGICAL ERPRES             ! .Z.ERRORS is present?
      INTEGER FDIMS( 2 )         ! Dimensions of FITS extension
      CHARACTER*80 FITCOM        ! Object containing FITS comment
      CHARACTER*132 FITNAM       ! Name of FITS item
      LOGICAL FITS               ! A FITS structure is found?
      LOGICAL FLAGGD             ! .Z.DATA contains flagged values?
      INTEGER I                  ! Loop variable
      INTEGER IAXIS              ! Loop index through axes
      INTEGER IERR               ! First element to cause numerical
                                 ! errors
      INTEGER IFLAG              ! Value of .Z.FLAGGED
      REAL INCREM                ! Incremental value for creating axes
      INTEGER IKOUNT             ! Counter
      INTEGER IPOSN              ! Number of object at first level
      INTEGER IPOSN1             ! Number of object at second level
      INTEGER IPOSN2             ! Number of object at third level
      INTEGER IPTR               ! Pointer to mapped data array
      INTEGER INQPTR             ! Pointer to output qality array
      INTEGER K                  ! Loop variable
      INTEGER LENAME             ! Length of name
      CHARACTER*256 LEVEL1       ! Full name of environment at 1st level
      CHARACTER*256 LEVEL2       ! Full name of environment at 2nd level
      LOGICAL MORE               ! Determine necessity of .MORE
      LOGICAL MOREFG             ! Determine ecessity for .MORE.FIGARO
      LOGICAL MORFGO             ! Necessity for .MORE.FIGARO.OBS
      LOGICAL MORFGZ             ! Necessity for .MORE.FIGARO.Z
      CHARACTER*256 MORNAM       ! Name of item in .MORE structure
      INTEGER N                  ! Loop variable
      CHARACTER*64 NAME          ! Name of data object
      CHARACTER*256 NAME1        ! 1st level object
      CHARACTER*256 NAME2        ! 2nd level object
      CHARACTER*256 NAME3        ! 3rd level object
      CHARACTER*256 NAMOUT       ! Name in output structure
      INTEGER NAXIS              ! Axis number
      INTEGER NBYTES             ! No. of BYTES
      INTEGER NCPC               ! Number of characters in path comp.
      INTEGER NDATA              ! Number of data values
      CHARACTER*( DAT__SZNAM ) NDFNAM ! Name of the NDF structure
      CHARACTER*256 NDFPAT       ! Path to the NDF structure
      INTEGER NDIM               ! Number of dimensions
      LOGICAL NEEDAX             ! Axis data are present?
      INTEGER NERR               ! Count of numerical errors
      INTEGER NFITS              ! Number of FITS items
      INTEGER NMSTAT             ! Status from 1st-level call to
                                 ! DTA_NMVAR
      INTEGER NMSTA1             ! Status from 2nd-level call to
                                 ! DTA_NMVAR
      INTEGER NMSTA2             ! Status from FITS-substructure call to
                                 ! DTA_NMVAR
      INTEGER NMSTA3             ! Status from call to DTA_NMVAR with
                                 ! .MORE
      INTEGER NPC                ! Number of characters in output name
      INTEGER NSTR               ! Effective length of string
      LOGICAL OBOPEN             ! Input file opend?
      LOGICAL OUOPEN             ! Output file opened?
      CHARACTER*132 OUTNDF       ! Name of the NDF including the path in
                                 ! the container file
      INTEGER OTQPTR             ! Pointer to output quality array
      INTEGER PATHHI             ! Character column of the end of a
                                 ! structure level in the path
      INTEGER PATHLO             ! Character column of the start of a
                                 ! structure level in the path
      LOGICAL PRIM               ! Output data array has ! primitive form
                                 ! (as opposed to simple)
      LOGICAL QUPRES             ! .Z.QUALITY is present?
      REAL START                 ! Start value for creating axes arrays
      CHARACTER*16 STNAME        ! Name of a non-standard structure in
                                 ! the DST's FITS structure
      CHARACTER*64 STRING        ! Used for units and labels
      LOGICAL STRUCT             ! Item is a structure?
      CHARACTER*16 TYPE          ! Data object type
      LOGICAL VALFIT             ! A FITS item is valid?
      REAL  WIDTH                ! Scalar width

*.

*   Return immediately on bad status.
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

*   Open the output container file.  An NDF at the top level requires a
*   new HDS file to be created.  Should the file already exist and the
*   the NDF will reside in a larger container file, use access mode of
*   read (OLD in DTA speak).  Also create the name to the output NDF for
*   creating the output NDF's structures, and find the string length.
*   Note the the top-level name is not required.
      NSTR = CHR_LEN( NDFFIL )
      IF ( NLEV .EQ .1 ) THEN
         CALL DTA_ASFNAM( 'OUTPUT', NDFFIL( :NSTR ),
     :                    'NEW', 10, 'NDF', DSTAT )
         OUTNDF = 'OUTPUT'
         NPC = 6
      ELSE
         CALL DTA_ASFNAM( 'OUTPUT', NDFFIL( :NSTR ),
     :                    'OLD', 10, 'NDF', DSTAT )
         EPATH = PATH( INDEX( PATH, '.' ) : )
         OUTNDF = 'OUTPUT'//EPATH
         NPC = CHR_LEN( OUTNDF )
      END IF

*   Exit if anything has gone wrong.
      IF ( DSTAT .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILNAM', NDFFIL )
         CALL ERR_REP( 'DST2NDF_FNF',
     :     'DST2NDF: Unable to open output NDF ^FILNAM.', STATUS )
         GOTO 500
      END IF
      OUOPEN = .TRUE.

*  Create the intermediate `environments' for a nested NDF.
*  ========================================================

      IF ( NLEV .GT. 1 ) THEN

*  All the objects exist except the NDF to be created.  Therefore, we
*  must extract the path of the environment structure to contain
*  the NDF, and the name of the NDF in order to create the NDF
*  structure.

*  Initialise the substring delimiters.
         PATHHI = 1
         PATHLO = 0
         NCPC = 0

*  Loop for each level in the file, excluding the top (zeroth) level.
         DO I = 2, NLEV

*  Increment the counter of the column.
            NCPC = NCPC + PATHHI - 1

*  Find the next structure delimiter, a fullstop.
            CALL CHR_DELIM( EPATH( PATHHI: ), '.', PATHLO, PATHHI )

         END DO

*  Extract the path component.  The last level does not have a
*  terminating fullstop, so there is no need to exclude it.
         ENVIRN = 'OUTPUT'//EPATH( :NCPC + PATHLO - 1 )
         NDFNAM = EPATH( NCPC + PATHLO + 1: NCPC + PATHHI )

*   Create the NDF structure of type NDF.
         CALL DTA_CRNAM( ENVIRN, NDFNAM( 1:CHR_LEN( NDFNAM ) ), 0, 1,
     :                   NDFPAT, DSTAT )
         CALL DTA_CRVAR( NDFPAT, 'NDF', DSTAT )
      END IF

*  Determine which structures need to be created in the output NDF.
*  ================================================================

*  Before copying anything to the output structure we ascertain
*  a) what dimensionality is necessary for the OUTNDF.AXIS(N) structure,
*  b) whether or not an OUTNDF.MORE structure is needed,
*  c) whether or not an OUTNDF.MORE.FITS structure is needed,
*  d) whether or not an OUTNDF.MORE.FIGARO structure is needed,
*  e) and whether or not an OUTNDF.DATA_ARRAY structure is needed (in
*  other words determine the form of the output data array).
*
*  Initialise some counters and flags.
      NMSTAT = 0
      IPOSN = 0
      NAXIS = 0
      ERPRES = .FALSE.
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
               IF ( .NOT. ( NAME1 .EQ. 'Z' .OR. NAME1 .EQ. 'FITS' .OR.
     :               NAME1 .EQ. 'COMMENTS' .OR. NAME1 .EQ. 'OBS' .OR.
     :               NAME1 .EQ. 'MORE') ) THEN
                  MORE = .TRUE.
                  MOREFG = .TRUE.

*            Test for the object being the FITS structure or comments.
               ELSE IF ( NAME1 .EQ. 'FITS' .OR.
     :                   NAME1 .EQ. 'COMMENTS' ) THEN

*               Find the number of dimensions and whether or not it is a
*               structure.
                  CALL DTA_SZVAR( LEVEL1, DAT__MXDIM, NDIM, DIMS,
     :                            DSTAT )
                  CALL DTA_STRUC( LEVEL1, STRUCT, DSTAT )

*               Looking at the name is not good enough.  It must be a
*               non-empty scalar structure.
                  IF ( STRUCT .AND. NDIM .EQ. 0 ) THEN
                     CALL DTA_NMVAR( LEVEL1, 1, NAME2, DSTAT )
                     IF ( DSTAT .EQ. 0 ) THEN
                        MORE = .TRUE.
                        FITS = .TRUE.
                     ELSE
                        NMSTA1 = DSTAT
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

*               Find which ancillary-data information is present.
*               =================================================

*               Find out whether bad pixels are present in the DST file.

*               Test for the structure containing the data array.
                  IF ( NAME1 .EQ. 'Z' ) THEN

*                  Test for the bad-pixel flag.
                     IF ( NAME2 .EQ. 'FLAGGED' ) THEN

*                     Obtain the value of the bad-pixel flag.
                        CALL DTA_RDVARI( LEVEL2, 1, IFLAG, DSTAT )
                        FLAGGD = IFLAG .NE. 0

*                  Test for the presence of errors.
                     ELSE IF ( NAME2 .EQ. 'ERRORS' ) THEN
                        ERPRES = .TRUE.

*                  Test for the presence of quality.
                     ELSE IF ( NAME2 .EQ. 'QUALITY' ) THEN
                        QUPRES = .TRUE.
                     END IF
                  END IF

*               See which extensions are required.
*               ==================================

*               Check to determine if any second-level objects require
*               OUTNDF.MORE or OUTNDF.MORE.FIGARO.
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
*   We know that the value supplied will always be in uppercase.
      PRIM = FORM .EQ. 'PRIMITIVE'

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
         CALL DTA_CRNAM( OUTNDF( :NPC ), 'AXIS', 1, NAXIS, AXNAME,
     :                   DSTAT )
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
         CALL DTA_CRVAR( OUTNDF( :NPC )//'.DATA_ARRAY', 'ARRAY', DSTAT )
      END IF
      IF ( DSTAT .NE. 0 ) THEN
         STATUS = DSTAT
         CALL ERR_REP('DST2NDF_CRD',
     :     'DST2NDF: Unable to create output .DATA_ARRAY structure.',
     :     STATUS)
         GOTO 500
      END IF

*   Create the .VARIANCE structure, if necessary.
      IF ( .NOT. PRIM .AND. ERPRES ) THEN
         CALL DTA_CRVAR( OUTNDF( :NPC )//'.VARIANCE', 'ARRAY', DSTAT )
      END IF
      IF ( DSTAT .NE. 0 ) THEN
         STATUS = DSTAT
         CALL ERR_REP('DST2NDF_CRV',
     :     'DST2NDF: Unable to create output .VARIANCE structure.',
     :     STATUS)
         GOTO 500
      END IF

*   Create the .MORE structure, if necessary.
      IF ( MORE ) THEN
         CALL DTA_CRVAR( OUTNDF( :NPC )//'.MORE', 'EXT', DSTAT )
      END IF
      IF ( DSTAT .NE. 0 ) THEN
         STATUS = DSTAT
         CALL ERR_REP('DST2NDF_CRM',
     :     'DST2NDF: Unable to create output .MORE structure.',
     :     STATUS)
         GOTO 500
      END IF

*   Create the .MORE.FIGARO structure, if necessary.
      IF ( MOREFG ) THEN
         CALL DTA_CRVAR( OUTNDF( :NPC )//'.MORE.FIGARO', 'EXT', DSTAT )
      END IF
      IF ( DSTAT .NE. 0 ) THEN
         STATUS = DSTAT
         CALL ERR_REP('DST2NDF_CRMF',
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

*                  Copy the data array to the output primitive array.
*                  The name of the output object will depend on whether
*                  the NDF is simple or primitive. In the former case
*                  the data array lies within the DATA_ARRAY structure.
                     IF ( PRIM ) THEN
                        CALL DTA_CYVAR( LEVEL2, OUTNDF( :NPC )/
     :                                  /'.DATA_ARRAY', DSTAT )
                     ELSE
                        CALL DTA_CYVAR( LEVEL2, OUTNDF( :NPC )//'.'/
     :                                  /'DATA_ARRAY.DATA', DSTAT )
                     END IF

                     IF ( DSTAT .NE. 0 ) GOTO 400

*                  Add the BAD_PIXEL flag in the simple NDF, where
*                  valid.
                     IF ( .NOT. PRIM .AND. FLAGGD ) THEN
                        CALL DTA_CRVAR( OUTNDF( :NPC )/
     :                                  /'.DATA_ARRAY.BAD_PIXEL',
     :                                  '_LOGICAL', DSTAT )
                        CALL DTA_WRVARI( OUTNDF( :NPC )/
     :                                   /'.DATA_ARRAY.BAD_PIXEL',
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

*                  Generate the name of the output object.  This will
*                  depend on whether the NDF is simple or primitive.  In
*                  the former case the data array lies within the
*                  VARIANCE structure.
                     IF ( PRIM ) THEN
                        NAMOUT = OUTNDF( :NPC )//'.VARIANCE'
                     ELSE
                        NAMOUT = OUTNDF( :NPC )//'.VARIANCE.DATA'
                     END IF

*                  Copy the input error array to the output primitive
*                  variance.
                     CALL DTA_CYVAR( LEVEL2, NAMOUT, DSTAT )
                     IF ( DSTAT .NE. 0 ) GOTO 400

*                  Map the variance (still errors though).
                     CALL DTA_MUVARF( NAMOUT, NDATA, IPTR, DSTAT )

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
                     CALL VEC_MULR( .TRUE., NDATA,
     :                              %VAL( CNF_PVAL( IPTR ) ),
     :                              %VAL( CNF_PVAL( IPTR ) ),
     :                              %VAL( CNF_PVAL( IPTR ) ), IERR,
     :                              NERR, STATUS )

*                  Unmap the variance array.
                     CALL DTA_FRVAR( NAMOUT, DSTAT )

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
                     CALL DTA_CRVAR( OUTNDF( :NPC )//'.QUALITY',
     :                               'QUALITY', DSTAT )

*                  Create and assign a BADBITS item to indicate that 1
*                  is the bad quality.
                     CALL DTA_CRVAR( OUTNDF( :NPC )/
     :                               /'.QUALITY.BADBITS', 'BYTE',
     :                               DSTAT )
                     BARRAY( 1 ) = 255
                     CALL DTA_WRVARB( OUTNDF( :NPC )/
     :                                /'.QUALITY.BADBITS', 1,
     :                                BARRAY, DSTAT )

*                  Inquire the type of the input QUALITY array.
                     CALL DTA_TYVAR( LEVEL2, TYPE, DSTAT )

*                  The type must be BYTE.  If it is, merely copy the
*                  QUALITY array to the NDF's QUALITY structure.
                     IF ( TYPE .EQ. 'BYTE' ) THEN
                        CALL DTA_CYVAR( LEVEL2, OUTNDF( :NPC )//'.'/
     :                                  /'QUALITY.QUALITY', DSTAT )
                        IF ( DSTAT .NE. 0 ) GOTO 400

*                  If type is not BYTE, it must be converted.
                     ELSE
                        CALL DTA_SZVAR( LEVEL2, 7, NDIM, DIMS, DSTAT )

*                     Generate a destination quality array of the
*                     required dimensions and BYTE data type.
                        CALL DTA_CRNAM( OUTNDF( :NPC )//'.QUALITY',
     :                                  'QUALITY', NDIM, DIMS, NAMOUT,
     :                                  DSTAT )
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
                        CALL DTA_MUVARB( OUTNDF( :NPC )/
     :                                   /'.QUALITY.QUALITY',
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
                        CALL KPG1_COPY( '_UBYTE', NDATA, INQPTR, OTQPTR,
     :                                  STATUS )

*                     Unmap the quality arrays.
                        CALL DTA_FRVAR( LEVEL2, DSTAT )
                        CALL DTA_FRVAR( OUTNDF( :NPC )/
     :                                  /'.QUALITY.QUALITY', DSTAT )
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
                     CALL DTA_CRNAM( OUTNDF( :NPC ), NAME2, NDIM,
     :                               CDIMS, NAMOUT, DSTAT )
                     CALL DTA_CRVAR( NAMOUT, 'CHAR', DSTAT )

*                  The .Z.LABEL and .Z.UNITS are copied to .LABEL
*                  and .UNITS in the output structure.  This is done by
*                  reading the value, creating the named object in the
*                  NDF, and writing the value to it.
                     NDATA = CDIMS( 1 )
                     CALL DTA_RDVARC( LEVEL2, NDATA, STRING, DSTAT )
                     CALL DTA_CRNAM( OUTNDF( :NPC ), NAME2, 0, 0,
     :                               NAMOUT, DSTAT )
                     CALL DTA_WRVARC( NAMOUT, NDATA, STRING, DSTAT )

*               Test for the IMAGINARY component.
*               =================================
                  ELSE IF ( NAME2 .EQ. 'IMAGINARY' ) THEN
                     CALL MSG_OUT( ' ',
     :                 'WARNING: Imaginary data are not copied.',
     :                 STATUS )
*                     CALL DTA_CYVAR( LEVEL2,
*     :                               OUTNDF( :NPC )/
*     :                               /'.DATA_ARRAY.IMAGINARY_DATA',
*     :                               DSTAT )
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
                     CALL DTA_CYVAR( LEVEL2, OUTNDF( :NPC )/
     :                               /'.MORE.FIGARO.'//NAME2, DSTAT )

*               Must be a non-standard component.
                  ELSE

*                  Create the extension for these non-standard objects,
*                  if not already done so.
                     IF ( .NOT. MORFGZ ) THEN
                        CALL DTA_CRVAR( OUTNDF( :NPC )/
     :                                  /'.MORE.FIGARO.Z', 'STRUCT',
     :                                  DSTAT )
                        MORFGZ = .TRUE.
                     END IF

*                  Copy the non-standard object to the FIGARO.Z
*                  extension.
                     CALL DTA_CYVAR( LEVEL2, OUTNDF( :NPC )/
     :                               /'.MORE.FIGARO.Z.'//NAME2, DSTAT )
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
                        CALL DTA_CYVAR( LEVEL2, OUTNDF( :NPC )/
     :                                  /'.TITLE', DSTAT )
                        IF ( DSTAT .NE. 0 ) GOTO 400

*                  Test for the airmass or time.  These are not copied
*                  to the .OBS structure within the extension, but go in
*                  at the top level of the Figaro extension.
                     ELSE IF ( NAME2 .EQ. 'SECZ' .OR.
     :                         NAME2 .EQ. 'TIME' ) THEN
                        CALL DTA_CYVAR( LEVEL2, OUTNDF( :NPC )/
     :                                  /'.MORE.FIGARO.'//NAME2, DSTAT )

                     ELSE

*                     Create the Figaro OBS extension for other
*                     ancillary data.
                        IF ( .NOT. MORFGO ) THEN
                           CALL DTA_CRVAR( OUTNDF( :NPC )/
     :                                     /'.MORE.FIGARO.OBS', 'OBS',
     :                                     DSTAT )
                           MORFGO = .TRUE.
                        END IF

*                     Any other items in the .OBS structure are copied
*                     into the .MORE.FIGARO.OBS structure.
                        CALL DTA_CYVAR( LEVEL2, OUTNDF( :NPC )/
     :                                  /'.MORE.FIGARO.OBS.'//NAME2,
     :                                  DSTAT )

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
               CALL DTA_CRNAM( OUTNDF( :NPC ), 'AXIS', 1, IAXIS, AXOUT,
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
*                        OUTPUT.AXIS(n).MORE.FIGARO.DATA_ARRAY.  Since
*                        the flag to indicate whether or not the NDF's
*                        AXIS(n).DATA_ARRAY has been created is still
*                        false, the NDF axis centres will be filled
*                        with pixel co-ordinates near the end of this
*                        routine.
                           CALL DTA_CRNAM( AXMORF, 'DATA_ARRAY', 0, 0,
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
                           CALL VEC_MULR( .TRUE., NDATA,
     :                                    %VAL( CNF_PVAL( IPTR ) ),
     :                                    %VAL( CNF_PVAL( IPTR ) ),
     :                                    %VAL( CNF_PVAL( IPTR ) ),
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

*                     DST supports scalar widths.  This must be expanded
*                     into a vector for NDF.
                        ELSE IF ( NDIM .EQ. 0 ) THEN

*                        Find the true dimension of the axis.
                           CALL DTA_SZVAR( 'INPUT.Z.DATA', 7, NDIM,
     :                                     DIMS, DSTAT )

*                        Create the full name of the output axis-width
*                        array, including the axis dimension.
                           CALL DTA_CRNAM( AXOUT, 'WIDTH', 1,
     :                                     DIMS( IAXIS ), AXOUTW,
     :                                     DSTAT )

*                        Obtain the data type of the scalar width.
                           CALL DTA_TYVAR( NAME2, TYPE, DSTAT )

*                        Obtain the real scalar width value.
                           IF ( TYPE .EQ. 'FLOAT' ) THEN
                              CALL DTA_RDVARF( LEVEL2, 1, WIDTH,
     :                                         STATUS )

*                           Create an axis data array of the
*                           appropriate size and type.
                              CALL DTA_CRVAR( AXOUTW, 'FLOAT', DSTAT )

*                           Create the name of the output axis-width
*                           array, excluding the axis dimension.
                              CALL DTA_CRNAM( AXOUT, 'WIDTH', 0, 0,
     :                                        AXOUTW, DSTAT )
                              CW = CHR_LEN( AXOUTW )

*                           Map the newly created structure for update.
                              CALL DTA_MUVARF( AXOUTW( 1:CW ),
     :                                         DIMS( IAXIS ), IPTR,
     :                                         DSTAT )

*                        Obtain the double-precision scalar width value.
                           ELSE
                              CALL DTA_RDVARD( NAME2, 1, DWIDTH,
     :                                         STATUS )

*                           Create an axis data array of the
*                           appropriate size and type.
                              CALL DTA_CRVAR( AXOUTW, 'DOUBLE', DSTAT )

*                           Create the name of the output axis-width
*                           array, excluding the axis dimension.
                              CALL DTA_CRNAM( AXOUT, 'WIDTH', 0, 0,
     :                                        AXOUTW, DSTAT )
                              CW = CHR_LEN( AXOUTW )

*                           Map the newly created structure for update.
                              CALL DTA_MUVARD( AXOUTW( 1:CW ),
     :                                         DIMS( IAXIS ), IPTR,
     :                                         DSTAT )
                           END IF

*                        Report an error condition.
                           IF ( DSTAT .NE. 0 ) THEN
                              STATUS = DSTAT
                              CALL MSG_SETI( 'AXNO', IAXIS )
                              CALL ERR_REP( 'DST2NDF_MAPACEW',
     :                          'DST2NDF: Error mapping the output '/
     :                          /'array of axis widths in dimension '/
     :                          /'^AXNO.', STATUS )
                              GOTO 500
                           END IF

*                       Fill array with the constant.
                           IF ( TYPE .EQ. 'FLOAT' ) THEN
                              CALL KPG1_FILLR( WIDTH, DIMS( IAXIS ),
     :                                         %VAL( CNF_PVAL( IPTR ) ),
     :                                         STATUS )
                           ELSE
                              CALL KPG1_FILLD( DWIDTH, DIMS( IAXIS ),
     :                                         %VAL( CNF_PVAL( IPTR ) ),
     :                                         STATUS )
                           END IF

*                       Unmap the width array.
                          CALL DTA_FRVAR( AXOUTW, DSTAT )

*                       Report an error condition.
                          IF ( DSTAT .NE. 0 ) THEN
                             STATUS = DSTAT
                             CALL MSG_SETI( 'AXNO', IAXIS )
                             CALL ERR_REP( 'DST2NDF_UMPACEW',
     :                         'DST2NDF: Error unmapping the output '/
     :                         /'array of axis widths in dimension '/
     :                         /'^AXNO.', STATUS )
                             GOTO 500
                          END IF

*                     The axis widths are not 1-dimensional so move
*                     them to the Figaro axis extension.
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
                        CALL DTA_SZVAR( LEVEL2, DAT__MXDIM, NDIM, CDIMS,
     :                                  DSTAT )

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
     :                               OUTNDF( :NPC )//'.MORE.'//MORNAM,
     :                               DSTAT )
                  END IF
               END DO
               IF ( DSTAT .NE. 0 ) GO TO 400

*         Deal with the TABLE.
*         ====================
            ELSE IF ( NAME1 .EQ. 'TABLE' ) THEN

*            TABLE used for FIGARO SPIKETRUM routines. Copy this to
*            .MORE.FIGARO.TABLE extension.
               CALL DTA_CYVAR( 'INPUT.TABLE', OUTNDF( :NPC )/
     :                         /'.MORE.FIGARO.TABLE', DSTAT )
               IF ( DSTAT .NE. 0 ) GOTO 400

*         Deal with FITS objects in the FITS and COMMENTS structures.
*         ===========================================================
            ELSE IF ( ( NAME1 .EQ. 'FITS' ) .OR.
     :                ( NAME1 .EQ. 'COMMENTS' ) ) THEN

*            FITS items dealt with below.
               CONTINUE

            ELSE

*            Any other data objects are copied into .MORE.FIGARO
               CALL DTA_CYVAR( LEVEL1, OUTNDF( :NPC )//'.MORE.FIGARO.'/
     :                         /NAME1, DSTAT )
               IF ( DSTAT .NE. 0 ) GOTO 400
            END IF
         END IF
      END DO


*     Deal with the FITS structure.
*     =============================

      IF ( FITS ) THEN

*      Count the number of FITS items, ending when the list of
*      components has been exhausted.  Allow for one level of
*      substructure within the FITS extension.  A substructure can be
*      counted if it is a regulation one containing the .DATA (and
*      .COMMENT) components.  Each non-standard structure will have
*      cause a heading comment and a blank line preceding it to appear
*      in the FITS extension, hence these headers are added to the
*      count.
         NFITS = 0
         IPOSN1 = 1
         DO WHILE ( NMSTA1 .EQ. 0 )
            CALL DTA_NMVAR( 'INPUT.FITS', IPOSN1, NAME2, NMSTA1 )
            IPOSN1 = IPOSN1 + 1
            IF ( NMSTA1 .EQ. 0 ) THEN
               NFITS = NFITS + 1

*            Test if the the component is a structure not containing a
*            .DATA component.
               FITNAM = 'INPUT.FITS.'//NAME2
               CALL DTA_STRUC( FITNAM, STRUCT, DSTAT )
               IF ( STRUCT ) THEN
                  LENAME = CHR_LEN( FITNAM )
                  NAME = FITNAM( :LENAME )//'.DATA'
                  CALL DTA_TYVAR( NAME, TYPE, DSTAT )

*               Non-standard structure.  Count the number of components.
*               Allow for blank cards before and after the block of
*               hierarchical keywords.
                  IF ( DSTAT .NE. 0 ) THEN
                     NMSTA2 = 0
                     NFITS = NFITS + 2
                     IPOSN2 = 1
                     DO WHILE ( NMSTA2 .EQ. 0 )
                        CALL DTA_NMVAR( 'INPUT.FITS.'//NAME2, IPOSN2,
     :                                  NAME3, NMSTA2 )
                        IPOSN2 = IPOSN2 + 1
                        IF ( NMSTA2 .EQ. 0 ) NFITS = NFITS + 1
                     END DO
                  END IF
               END IF
            END IF
         END DO

*      Create a .MORE.FITS array of 80-character card images.
         FDIMS( 1 ) = 80
         FDIMS( 2 ) = NFITS
         CALL DTA_CRNAM( OUTNDF( :NPC )//'.MORE', 'FITS', 2, FDIMS,
     :                   NAMOUT, DSTAT )
         CALL DTA_CRVAR( NAMOUT, 'CHAR', DSTAT )

*      Now deal with the FITS items one by one.
         NFITS = 0
         NMSTA1 = 0
         IPOSN1 = 1
         DO WHILE ( NMSTA1 .EQ. 0 )

*         Obtain the IPOSN1th object's name.
            CALL DTA_NMVAR( 'INPUT.FITS', IPOSN1, NAME2, NMSTA1 )
            IPOSN1 = IPOSN1  +  1
            IF ( NMSTA1 .EQ. 0 ) THEN

*            Generate the name of the FITS item.  This can be either a
*            primitive item, or a .  If it is a structure, it
*            contains the comment as well as the data.  If it is not,
*            the comment may be in a separate .COMMENTS structure.
               FITNAM = 'INPUT.FITS.'//NAME2
               LENAME = CHR_LEN( FITNAM )
               CALL DTA_STRUC( FITNAM, STRUCT, DSTAT )
               COMPRE = DSTAT .EQ. 0
               VALFIT = .TRUE.
               IF ( COMPRE ) THEN
                  IF ( STRUCT ) THEN
                     NAME = FITNAM( :LENAME )//'.DATA'
                  ELSE
                     NAME = FITNAM
                  END IF
                  CALL DTA_TYVAR( NAME, TYPE, DSTAT )
                  VALFIT = DSTAT.EQ.0
               END IF

*            Process standard structures within the FITS structure.
               IF ( VALFIT ) THEN

*               Now read the comment associated with the object, if
*               any.  FITS items may be stored with the values in the
*               .FITS structure and comments in a separate .COMMENTS
*               structure.  Alternatively, both may be in the FITS
*               structure, with the values in the .FITS.DATA structure
*               and the comments in .FITS.DESCRIPTION.
                  LENAME = CHR_LEN( FITNAM )
                  IF ( STRUCT ) THEN
                     FITCOM = FITNAM( :LENAME )//'.DESCRIPTION'
                  ELSE
                     FITCOM = 'INPUT.COMMENTS.'//NAME2
                  END IF
                  CALL DTA_RDVARC( FITCOM, 50, COMENT, DSTAT )
                  IF ( DSTAT .NE. 0 ) COMENT = ' '

*               Create the header in the NDF's FITS extension.  The
*               current header number is incremented by this routine.
                  CALL CON_D2NFT( NAME, NAME2, TYPE, COMENT, OUTNDF,
     :                            NPC, .FALSE., ' ', NFITS, STATUS )

               ELSE

*               Loop through the non-standard substructure.  Set the
*               initial structure name so that for the first item
*               a blank header and a comment containing the structure
*               name is written.
                  NMSTA2 = 0
                  IPOSN2 = 1
                  STNAME = NAME2
                  DO WHILE ( NMSTA2 .EQ. 0 )

*                  Obtain the component's name and type.
                     CALL DTA_NMVAR( FITNAM, IPOSN2, NAME3, NMSTA2 )
                     IF ( NMSTA2 .EQ. 0 ) THEN
                        NAME = FITNAM( :LENAME )//'.'//NAME3
                        CALL DTA_TYVAR( NAME, TYPE, DSTAT )

*                  Write the FITS header to the NDF's FITS extension.
                        CALL CON_D2NFT( NAME, NAME3, TYPE, ' ', OUTNDF,
     :                                  NPC, .TRUE., STNAME, NFITS,
     :                                  STATUS )
                        STNAME = ' '
                        IPOSN2 = IPOSN2 + 1

*                  Write the trailing blank line.
                     ELSE
                        CALL CON_D2NFT( NAME, ' ', ' ', ' ', OUTNDF,
     :                                  NPC, .TRUE., ' ', NFITS,
     :                                  STATUS )

                     END IF
                  END DO

               END IF
            END IF
         END DO
      END IF

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
               CALL DTA_CRNAM( OUTNDF( :NPC ), 'AXIS', 1, IAXIS, AXOUT,
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

*            Fill array with 0.5, 1.5...
               START = 0.5
               INCREM = 1.0
               CALL KPG1_SSAZR( DIMS( IAXIS ), INCREM, START,
     :                          %VAL( CNF_PVAL( IPTR ) ), STATUS )

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
