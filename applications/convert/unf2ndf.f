      SUBROUTINE UNF2NDF( STATUS )
*+
*  Name:
*     UNF2NDF

*  Purpose:
*     Converts a sequential unformatted file to an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL UNF2NDF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts a sequential unformatted Fortran file
*     to an NDF.  Only one of the array components may be created from
*     the input file.  Preceding the input data there may be an
*     optional header.  This header may be skipped, or may consist of a
*     simple FITS header.  In the former case the shape of the NDF has
*     be to be supplied.

*  Usage:
*     unf2ndf in out [comp] noperec [skip] shape [type]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The NDF component to be copied.  It may be "Data", "Quality"
*        or "Variance".  To create a variance or quality array the NDF
*        must already exist. ["Data"]
*     FITS = _LOGICAL (Read)
*        If TRUE, the initial records of the unformatted file are
*        interpreted as a FITS header (with one card image per record)
*        from which the shape, data type, and axis centres are derived.
*        The last record of the FITS-like header must be terminated by
*        an END keyword; subsequent records in the input file are
*        treated as an array component given by COMP.  [FALSE]
*     IN = FILENAME (Read)
*        Name of the input sequential unformatted Fortran file.  The
*        file will normally have variable-length records when there is
*        a header, but always fixed-length records when there is no
*        header.
*     NOPEREC = _INTEGER (Read)
*        The number of data values per record of the input file.  It
*        must be positive on UNIX systems.  The suggested default is
*        the size of the first dimension of the array if there is no
*        current value.  A null (!) value for NOPEREC causes the size
*        of first dimension to be used.
*     OUT = NDF (Read and Write)
*        Output NDF data structure.  When COMP is not "Data" the NDF
*        is modified rather than a new NDF created.  It becomes the new
*        current NDF.
*     SHAPE = _INTEGER (Read)
*        The shape of the NDF to be created.  For example, [40,30,20]
*        would create 40 columns by 30 lines by 10 bands.  It is only
*        accessed when FITS is FALSE.
*     SKIP = INTEGER (Read)
*        The number of header records to be skipped at the start of the
*        input file before finding the data array or FITS-like header.
*        [0]
*     TYPE = LITERAL (Read)
*        The data type of the input file and output NDF. It must be one
*        of the following HDS types: "_BYTE", "_WORD", "_REAL",
*        "_INTEGER", "_INT64", "_DOUBLE", "_UBYTE", "_UWORD"
*        corresponding to signed byte, signed word, real, integer,
*        64-bit integer, double precision, unsigned byte, and unsigned
*        word.  See SUN/92 for further details.  An unambiguous
*        abbreviation may be given.  TYPE is ignored when
*        COMP = "Quality" since the QUALITY component must comprise
*        unsigned bytes (equivalent to TYPE = "_UBYTE") to be a valid
*        NDF. The suggested default is the current value.  TYPE is
*        also only accessed when FITS is FALSE.  ["_REAL"]

*  Examples:
*     unf2ndf ngc253.dat ngc253 shape=[100,60] noperec=8
*        This copies a data array from the unformatted file ngc253.dat
*        to the NDF called ngc253.  The input file does not contain a
*        header section.  The NDF is two-dimensional: 100 elements in x
*        by 60 in y.  Its data array has type _REAL.  The data records
*        each have 8 values.
*     unf2ndf ngc253q.dat ngc253 q 100 shape=[100,60]
*        This copies a quality array from the unformatted file
*        ngc253q.dat to an existing NDF called ngc253 (such as created
*        in the first example).  The input file does not contain a header
*        section.  The NDF is two-dimensional: 100 elements in x by 60
*        in y.  Its data array has type _UBYTE.  The data records
*        each have 100 values.
*     unf2ndf ngc253.dat ngc253 fits noperec=!
*        This copies a data array from the unformatted file ngc253.dat
*        to the NDF called ngc253.  The input file contains a FITS-like
*        header section, which is copied to the FITS extension of the
*        NDF.  The shape of the NDF is controlled by the mandatory FITS
*        keywords NAXIS, AXIS1, ..., AXISn, and the data type by
*        keywords BITPIX and UNSIGNED.  Each data record has AXIS1
*        values (except perhaps for the last).
*     unf2ndf type="_uword" in=ngc253.dat out=ngc253 \
*        This copies a data array from the unformatted file ngc253.dat
*        to the NDF called ngc253.  The input file does not contain a
*        header section.  The NDF has the current shape and data type
*        is unsigned word.  The current number of values per record is
*        used.
*     unf2ndf spectrum zz skip=2 shape=200 noperec=!
*        This copies a data array from the unformatted file spectrum to
*        the NDF called zz.  The input file contains two header records that
*        are ignored.  The NDF is one-dimensional comprising 200
*        elements of type _REAL.  There is one data record containing
*        the whole array.
*     unf2ndf spectrum.lis ZZ skip=1 fits noperec=20
*        This copies a data array from the unformatted file spectrum.lis
*        to the NDF called ZZ.  The input file contains one header
*        record, that is ignored, followed by a FITS-like header
*        section, which is copied to the FITS extension of the NDF.
*        The shape of the NDF is controlled by the mandatory FITS
*        keywords NAXIS, AXIS1, ..., AXISn, and the data type by
*        keywords BITPIX and UNSIGNED.  Each data record has AXIS1
*        values (except perhaps for the last).

*  Notes:
*     The details of the conversion are as follows:
*        -  the unformatted-file array is written to the NDF array as
*        selected by COMP.  When the NDF is being modified, the shape
*        of the new component must match that of the NDF.
*        -  If the input file contains a FITS-like header, and a new
*        NDF is created, i.e. COMP = "Data", the header records are
*        placed within the NDF's FITS extension.  This enables more
*        than one array (input file) to be used to form an NDF.  Note
*        that the data array must be created first to make a valid NDF,
*        and it's the FITS structure associated with that array that is
*        wanted.  Indeed the application prevents you from doing
*        otherwise.
*
*        -  The FITS-like header defines the properties of the NDF as
*        follows:
*           o  BITPIX defines the data type: 8 gives _BYTE, 16 produces
*           _WORD, 32 makes _INTEGER, 64 creates _INT64, -32 gives
*           _REAL, and -64 generates _DOUBLE.  For the first two, if
*           there is an extra header record with the keyword UNSIGNED
*           and logical value T, these types become _UBYTE and _UWORD
*           respectively.  UNSIGNED is non-standard, since unsigned
*           integers would not follow in a proper FITS file.  However,
*           here it is useful to enable unsigned types to be input into
*           an NDF.  UNSIGNED may be created by this application's
*           sister, NDF2UNF.  BITPIX is ignored for QUALITY data; type
*           _UBYTE is used.
*           o  NAXIS, and NAXISn define the shape of the NDF.
*           o  The TITLE, LABEL, and BUNIT are copied to the NDF
*           TITLE, LABEL, and UNITS NDF components respectively.
*           o  The CDELTn, CRVALn, CTYPEn, and CUNITn keywords make
*           linear axis structures within the NDF.  CUNITn define the
*           axis units, and the axis labels are assigned to CTYPEn If
*           some are missing, pixel co-ordinates are used for those
*           axes.
*           o  BSCALE and BZERO in a FITS extension are ignored.
*           o  BLANK is not used to indicate which input array values
*           should be assigned to a standard bad value.
*           o  END indicates the last header record unless it
*           terminates a dummy header, and the actual data is in an
*           extension.
*        -  Other data item such as HISTORY, data ORIGIN, and axis
*        widths are not supported, because the unformatted file has a
*        simple structure to enable a diverse set of input files to be
*        converted to NDFs, and to limitations of the standard FITS
*        header.

*  Related Applications:
*     CONVERT: NDF2UNF.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1996, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1992 September 18 (MJC):
*        Original version.
*     1996 September 16 (MJC):
*        Corrected usage of CTYPEn (was CRTYPEn) and introduced CUNITn
*        for axis units.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2012 April 30 (MJC):
*        Add _INT64 type.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXHDR             ! Maximum number of header sections in
                                 ! a sub-file, including dummy header
                                 ! sections before the extension *with*
                                 ! data
      PARAMETER( MAXHDR = 2 )

*  Local Variables:
      LOGICAL BADPIX             ! True if data array has undefined
                                 ! pixels
      INTEGER BITPIX             ! Number of bits per data value
      INTEGER BLANK              ! Tape data value assigned to
                                 ! undefined-value pixels
      INTEGER BPV                ! Number of bytes per pixel
      REAL BSCALE                ! Scale factor used to convert file
                                 ! values to true values
      REAL BZERO                 ! Offset applied to derive true values
      CHARACTER*( 8 ) COMP       ! The component of NDF to plot
      LOGICAL DARRAY             ! There is a data array in the
                                 ! FITS-like file
      LOGICAL DESCRP             ! If true the FITS header is reported
      INTEGER DIMS( NDF__MXDIM ) ! The dimensions of the input NDF
      INTEGER EL                 ! Number of mapped elements
      LOGICAL EXTEND             ! True if there is a FITS extension
                                 ! (XTENSION=T)
      INTEGER FD                 ! File descriptor
      LOGICAL HEADER             ! If true there will be FITS-like
                                 ! header
      INTEGER HDNUM( MAXHDR )    ! Number of headers cards in each
                                 ! header section in the sub-file
      INTEGER HPNTR              ! Pointer to the mapped header array
      INTEGER HSTART( MAXHDR )   ! Start card number of each header
                                 ! section of the sub-file in the full
                                 ! header
      INTEGER I                  ! Loop counter
      CHARACTER MACHIN * ( 24 )  ! Machine name
      INTEGER NCARD              ! Number of cards in the FITS-like
                                 ! header
      INTEGER NDF                ! Identifier for NDF
      INTEGER NDIM               ! The dimensionality of the input array
      INTEGER NHEADS             ! Number of header sections in the
                                 ! FITS-like header and including the
                                 ! dummy header section
      CHARACTER NODE * ( 20 )    ! Node name
      LOGICAL NONSDA             ! True if there is a non-standard data
                                 ! array in the FITS-like file
      INTEGER NUMPRE             ! Number of data values per record
      INTEGER ODIMS( NDF__MXDIM ) ! The dimensions of the existing NDF
      INTEGER ONDIM              ! The dimensionality of the existing
                                 ! NDF
      INTEGER PNTR( 1 )          ! Pointer to NDF mapped array
      CHARACTER RELEAS * ( 10 )  ! Release of operating system
      INTEGER SKIP               ! Number of header records to skip
      CHARACTER SYSNAM * ( 10 )  ! Operating system
      CHARACTER * ( DAT__SZLOC ) TLOC ! Locator to workspace holding the
                                 ! FITS-like headers
      CHARACTER * ( NDF__SZTYP ) TYPE ! Data type for processing
      LOGICAL UNSIGN             ! True if data type is unsigned B or W
      LOGICAL UPDATE             ! True if an NDF is to be modified
      LOGICAL VALID              ! True if it is valid to insert the new
                                 ! array into an existing NDF
      CHARACTER VERSIO * ( 10 )  ! Sub-version of operating system
      LOGICAL VMS                ! True if running on a VAX/VMS system

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the sequential unformatted file.
*  =====================================

*  Open the FORTRAN file.
      CALL FIO_ASSOC( 'IN', 'READ', 'UNFORMATTED', 0, FD, STATUS )

*  See how to handle a header.
*  ===========================

*  Ask if it there is to be a FITS-like header?
      CALL PAR_GET0L( 'FITS', HEADER, STATUS )

*  Find how many records to skip.
      CALL PAR_GDR0I( 'SKIP', 0, 0, VAL__MAXI, .FALSE., SKIP, STATUS )

*  Find out which component is to be processed.
*  ============================================

*  Find which component to copy.
      CALL PAR_CHOIC( 'COMP', 'Data', 'Data,Variance,Quality',
     :                .FALSE., COMP, STATUS )
      UPDATE = COMP .NE. 'DATA'

*  Check the status to prevent possibly adding confusing error messages.
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Obtain additional parameters for no-FITS header.
*  ================================================
      IF ( .NOT. HEADER ) THEN

*  There is no FITS-like header.
*
*  Obtain the shape of the NDF.
         CALL PAR_GDRVI( 'SHAPE', NDF__MXDIM, 1, VAL__MAXI, DIMS,
     :                   NDIM, STATUS )

*  Get type of data required, selected from the menu of HDS numeric
*  types.  Note that the QUALITY component must be unsigned byte.
         IF ( COMP .EQ. 'QUALITY' ) THEN
            TYPE = '_UBYTE'
         ELSE
            CALL PAR_CHOIC( 'TYPE', '_REAL', '_BYTE,_DOUBLE,_INTEGER,'/
     :                      /'_INT64,_REAL,_UBYTE,_UWORD,_WORD',
     :                      .FALSE., TYPE, STATUS )
         END IF
      END IF

*  Exit here in case an error arose so as to prevent spurious error
*  messages.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Read the FITS header.
*  =====================
      IF ( HEADER ) THEN

*  Create a dummy header structure.
         NCARD = 100
         CALL AIF_TEMP( '_CHAR*80', 1, NCARD, TLOC, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'UNF2NDF_HDS',
     :        'UNF2NDF: Error creating a dummy FITS header '/
     :        /'structure.', STATUS )
            GOTO 980
         END IF

*  Process the header blocks.  It is assumed that there is but a simple
*  FITS header.
         CALL CON_PFITH( FD, 'UNFORMATTED', TLOC, MAXHDR, DESCRP,
     :                   HSTART, HDNUM, EXTEND, NHEADS, STATUS )

         IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Find the current size of the header structure.
         CALL DAT_SIZE( TLOC, NCARD, STATUS )

*  Now map the headers for later access.  This call is situated after
*  reading in all the headers because the size of the work space may
*  have been changed, and we don't want to mix mapping with direct
*  access to the work structure.
         CALL DAT_MAPC( TLOC, 'UPDATE', 1, NCARD, HPNTR, STATUS )

*  Check we have a valid pointer before accessing it.
         IF ( STATUS .NE. SAI__OK ) GOTO 980

*   Now the complete header is stored in the buffer.  The values of
*   certain items stored in the buffer are required to generate the
*   data array.  This is not the most efficient method to obtain
*   selected descriptor values, but, in the main, the important
*   descriptors will be in the early cards.  Efficiency has been
*   sacrificed for more-structured and flexible code.

*   Check that the mandatory descriptors are present.  Want to start
*   the searchs in the last (NHEADS) header.  When there are two
*   headers, the first will be a dummy header.
         CALL CON_MANDH( .TRUE., NCARD, %VAL( CNF_PVAL( HPNTR ) ),
     :                   HSTART( NHEADS ),
     :                   BITPIX, NDIM, DIMS, DARRAY, NONSDA, EL,
     :                   STATUS, %VAL( CNF_CVAL( 80 ) ) )

*   Cannot handle grouped data, or no data array, so set a bad status
*   and make an error report follows.
         IF ( NONSDA .OR. .NOT. DARRAY ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'UNF2NDF_NODATA',
     :        'UNF2NDF: No array information in the header, or '/
     :        /'there is a random-group type header.', STATUS )
            GOTO 980
         END IF

*   Check for an error, because file is not in FITS format, or has an
*   unsupported FITS extension.
         IF ( STATUS .NE. SAI__OK ) THEN

*   Now report error context.
            CALL ERR_REP( 'UNF2NDF_IVHEAD',
     :        'UNF2NDF: Unable to process the header as simple FITS.',
     :        STATUS )
            GOTO 980
         END IF

*  Obtain the scale and zero, the blank value.
         CALL CON_FTYPE( BITPIX, HSTART( NHEADS ), NCARD,
     :                   %VAL( CNF_PVAL( HPNTR ) ),
     :                   BSCALE, BZERO, BLANK,
     :                   BADPIX, UNSIGN, STATUS,
     :                   %VAL( CNF_CVAL( 80 ) ) )

*  Determine the HDS data type of the data array.  Note that the QUALITY
*  component must be unsigned byte.
         IF ( COMP .EQ. 'QUALITY' ) THEN
            TYPE = '_UBYTE'
         ELSE
            CALL CON_FFRMT( BITPIX, UNSIGN, BPV, TYPE, STATUS )
         END IF

      END IF

*  Check the status to prevent possibly adding confusing error messages.
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Obtain the input NDF.
*  =====================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the updated NDF.
      IF ( UPDATE ) THEN
         CALL NDF_ASSOC( 'OUT', 'UPDATE', NDF, STATUS )

*  Validate the dimensions.
         CALL NDF_DIM( NDF, NDF__MXDIM, ODIMS, ONDIM, STATUS )
         VALID = ONDIM .EQ. NDIM
         I = 0
         DO WHILE ( VALID .AND. I .LT. ONDIM )
            I = I + 1
            VALID = VALID .AND. DIMS( I ) .EQ. ODIMS( I )
         END DO

*  Report an error if copying the new array would produce an invalid
*  NDF.
         IF ( .NOT. VALID ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', NDF )
            CALL ERR_REP( 'UNF2NDF_SHAPE',
     :        'UNF2NDF: The shape of the new array is not the same '/
     :        /'as that of the NDF ^NDF.', STATUS )
            GOTO 970
         END IF

*  Create a new primitive NDF.
      ELSE
         CALL NDF_CREP( 'OUT', TYPE, NDIM, DIMS, NDF, STATUS )
      END IF

*  Fix the data type.
*  ==================

*  This is required so that that one- and two-byte integer types may be
*  mapped as integer for reading, but are actually stored with the
*  correct type.  The type cannot be altered for the QUALITY array.
      IF ( COMP .EQ. 'VARIANCE' )
     :  CALL NDF_STYPE( TYPE, NDF, COMP, STATUS )

*  Determine whether or not the operating system is VMS.
*  =====================================================
*
*  This assumes that the system is either VMS or UNIX.  It is needed
*  to specify the path of the file containing the global parameters.
      CALL PSX_UNAME( SYSNAM, NODE, RELEAS, VERSIO, MACHIN, STATUS )
      VMS = INDEX( SYSNAM, 'VMS' ) .NE. 0

*  Obtain the number of values per record.
*  =======================================
      IF ( VMS ) THEN
         CALL PAR_GDR0I( 'NOPEREC', MIN( 16383, DIMS( 1 ) ), 1, 16383,
     :                   .TRUE., NUMPRE, STATUS )
      ELSE
         CALL PAR_GDR0I( 'NOPEREC', DIMS( 1 ), 1, VAL__MAXI,
     :                   .TRUE., NUMPRE, STATUS )
      END IF

*  Process the input array.
*  ========================

*  Map the input data array using the input data type.
      CALL NDF_MAP( NDF, COMP, TYPE, 'WRITE', PNTR, EL, STATUS )

*  Call a routine to read the data from the unformatted Fortran file.
*  The selected routine depending on the data type of the array.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL CON_IFUFB( FD, EL, NUMPRE, SKIP,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL CON_IFUFD( FD, EL, NUMPRE, SKIP,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL CON_IFUFI( FD, EL, NUMPRE, SKIP,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   STATUS )

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL CON_IFUFK( FD, EL, NUMPRE, SKIP,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL CON_IFUFR( FD, EL, NUMPRE, SKIP,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL CON_IFUFUB( FD, EL, NUMPRE, SKIP,
     :                    %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL CON_IFUFUW( FD, EL, NUMPRE, SKIP,
     :                    %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL CON_IFUFW( FD, EL, NUMPRE, SKIP,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   STATUS )

      END IF

*  Complete the NDF by creating and assigning values to the other
*  top-level components, the axis structure and the FITS extension.
*  The first argument is the number of header cards from the start of
*  the headers up to the end of the current header.
      IF ( HEADER ) THEN
         CALL CON_NDFCM( HSTART( NHEADS ) - 1 + NCARD,
     :                   %VAL( CNF_PVAL( HPNTR ) ),
     :                   HSTART( NHEADS ), .NOT. UPDATE, NDF, STATUS,
     :                   %VAL( CNF_CVAL( 80 ) ) )
      END IF

*  End the NDF context.
  970 CONTINUE
      CALL NDF_END( STATUS )

*  Tidy workspace used to obtain the FITS headers.
  980 CONTINUE
      IF ( HEADER ) CALL AIF_ANTMP( TLOC, STATUS )

  999 CONTINUE

*  Close the output file.
      CALL FIO_CLOSE( FD, STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'UNF2NDF_ERR',
     :     'UNF2NDF: Error converting a sequential unformatted file '/
     :     /'to an NDF.', STATUS )
      END IF

      END

