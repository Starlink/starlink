#if HAVE_CONFIG_H
# include "config.h"
#endif
      SUBROUTINE NDF2IRAF( STATUS )
*+
*  Name:
*     NDF2IRAF

*  Purpose:
*     Converts an NDF to an IRAF image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDF2IRAF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts an NDF to an IRAF image.  See the Notes
*     for details of the conversion.
*
*  Usage:
*     ndf2iraf in out [fillbad]

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF data structure.  The suggested default is the
*        current NDF if one exists, otherwise it is the current value.
*     FILLBAD = _REAL (Read)
*        The value used to replace bad pixels in the NDF's data array
*        before it is copied to the IRAF file.  A null value (!) means
*        no replacements are to be made.  This parameter is ignored if
*        there are no bad values.  [0]
*     OUT = LITERAL (Write)
*        The name of the output IRAF image.  Two files are produced
*        with the same name but different extensions. The ".pix" file
*        contains the data array, and ".imh" is the associated header
*        file that may contain a copy of the NDF's FITS extension.
*        The suggested default is the current value.
*     PROFITS = _LOGICAL (Read)
*        If TRUE, the contents of the FITS extension of the NDF are
*        merged with the header information derived from the standard
*        NDF components.  See the Notes for details of the merger.
*        [TRUE]
*     PROHIS = _LOGICAL (Read)
*        If TRUE, any NDF history records are written to the primary
*        FITS header as HISTORY cards.  These follow the mandatory
*        headers and any merged FITS-extension headers (see parameter
*        PROFITS).  [TRUE]

*  Examples:
*     ndf2iraf abell119 a119
*        Converts an NDF called abell119 into the IRAF image comprising
*        the pixel file a119.pix and the header file a119.imh.  If there
*        are any bad values present they are copied verbatim to the IRAF
*        image.
*     ndf2iraf abell119 a119 noprohis
*        As the previous example, except that NDF HISTORY records are
*        not transferred to the headers in a119.imh.
*     ndf2iraf qsospe qsospe fillbad=0
*         Converts the NDF called qsospe to an IRAF image comprising the
*         pixel file qsospe.imh and the header file qsospe.pix.  Any bad
*         values in the data array are replaced by zero.
*     ndf2iraf qsospe qsospe fillbad=0 profits=f
*        As the previous example, except that any FITS airlock
*        information in the NDF are not transferred to the headers in
*        qsospe.imh.

*  Notes:
*     The rules for the conversion are as follows:
*     -  The NDF data array is copied to the ".pix" file.  Ancillary
*     information listed below is written to the ".imh" header file in
*     FITS-like headers.
*     -  The IRAF "Mini World Coordinate System" (MWCS) is used to
*     record axis information whenever one of the following criteria is
*     satisfied:
*
*        1) the dataset has some linear axes (system=world);
*
*        2) the dataset is one-dimensional with a non-linear axis, or is
*           two-dimensional with the first axis non-linear and the
*           second being some aperture number or index
*           (system=multispec);
*
*        3) the dataset has a linear spectral/dispersion axis along the
*           first dimension and all other dimensions are pixel indices
*           (system=equispec).
*
*     -  The NDF title, label, units are written to the header keywords
*     TITLE, OBJECT, and BUNIT respectively if they are defined.
*     Otherwise any values for these keywords found in the FITS
*     extension are used (provided Parameter PROFITS is TRUE).  There
*     is a limit of twenty characters for each.
*     -  The NDF pixel origins are stored in keywords LBOUNDn for the
*     nth dimension when any of the pixel origins is not equal to 1.
*     -  Keywords HDUCLAS1, HDUCLASn are set to "NDF" and the
*     array-component name respectively.
*     -  The BLANK keyword is set to the Starlink standard bad value,
*     but only for the _WORD data type and not for a quality array.  It
*     appears regardless of whether or not there are bad values
*     actually present in the array.
*     -  HISTORY headers are propagated from the FITS extension when
*     PROFITS is TRUE, and from the NDF history component when PROHIS
*     is TRUE.
*     -  If there is a FITS extension in the NDF, then the elements up
*     to the first END keyword of this are added to the `user area' of
*     the IRAF header file, when PROFITS=TRUE.  However, certain
*     keywords are excluded: SIMPLE, NAXIS, NAXISn, BITPIX, EXTEND,
*     PCOUNT, GCOUNT, BSCALE, BZERO, END, and any already created from
*     standard components of the NDF listed above.
*     -  A HISTORY record is added to the IRAF header file indicating
*     that it originated in the named NDF and was converted by
*     NDF2IRAF.
*     -  All other NDF components are not propagated.

*  Related Applications:
*     CONVERT: IRAF2NDF.

*  Pitfalls:
*     The IMFORT routines refuse to overwrite an IRAF image if an image
*     with the same name exists.  The application then aborts.
*
*     Some of the routines required for accessing the IRAF header image
*     are written in SPP. Macros are used to find the start of the
*     header line section, this constitutes an `Interface violation' as
*     these macros are not part of the IMFORT interface specification.
*     It is possible that these may be changed in the future, so
*     beware.

*  Implementation Status:
*     -  Only handles one-, two-, and three-dimensional NDFs.
*     -  Of the NDF's array components only the data array may be
*     copied.
*     -  The IRAF image produced has type SIGNED WORD or REAL dependent
*     of the type of the NDF's data array.  (The IRAF IMFORT FORTRAN
*     subroutine library only supports these data types.)  For _BYTE,
*     _UBYTE, and _WORD data arrays the IRAF image will have type
*     SIGNED WORD; for all other data types of the NDF data array a
*     REAL IRAF image is made.  The pixel type of the image can be
*     changed from within IRAF using the 'chpixtype' task in the
*     'images' package.
*     -  Bad values may arise due to type conversion.  These too are
*     substituted by the (non-null) value of FILLBAD.
*     -  See "Release Notes" for IRAF version compatibility.

*  Implementation Deficiencies
*     Should create an IRAF bad-pixel-mask file rather than replacing
*     the bad values.

*  External Routines Used:
*     IRAF IMFORT subroutine library.
*
*  References:
*     IRAF User Handbook Volume 1A: "A User's Guide to FORTRAN
*     Programming in IRAF, the IMFORT Interface", by Doug Tody.

*  Keywords:
*     CONVERT, IRAF

*  Authors:
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-AUG-1992 (RAHM):
*        Original version.
*     1992 September 29 (MJC):
*        Standardised the prologue.  Corrected the error reporting and
*        closedown.  Made the data copying more efficient.  Corrected
*        some minor bugs.
*     1993 July 21 (MJC):
*        Added FILLBAD option, full support for one-dimensional NDFs,
*        allow conversion of cubes, and able to make a signed-word IRAF
*        image.
*     1997 March 28 (MJC):
*        Writes keywords for the label, units, axes, history, origin,
*        bad value, and class.  Avoid duplication of keywords when merging
*        the FITS extension.  Control of propagation of the history
*        records and the FITS extension.  Used more efficient workspace
*        mechanism.  Restriction to lowercase IRAF file names removed.
*        Parameter FILLBAD defaults to 0.
*     1997 July 31 (MJC):
*        No longer write an END header.  The END keyword is not
*        significant in the IRAF headers, and additional headers may be
*        written following it.  Added examples.
*     1997 Nov 24 (AJC):
*        Remove use of CHR_MOVE (obsolete).  Remove unnecessary
*        initialisations of CARD.
*     2008 March 15 (MJC):
*        Use KAPLIBS routines instead of their cloned CON equivalents.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'NDF_ERR'          ! NDF error constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_CVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER*2 VAL_RTOW         ! Converts real value to word data type

*  Local Constants:
      INTEGER FITSLN             ! Length of FITS card image
      PARAMETER ( FITSLN = 80 )

      INTEGER NDIM               ! Maximum number of dimensions the
      PARAMETER ( NDIM = 3 )     ! application can handle

      INTEGER STRLEN             ! Length of output title string.
      PARAMETER ( STRLEN = 80 )

*  Local Variables:
      INTEGER ACCESS             ! Access mode for opening IRAF image
                                 ! using imopen()
      CHARACTER*( NDF__SZTYP ) ATYPE ! Type of the NDF's data array
      LOGICAL CHECK              ! Check for bad values?
      INTEGER DIMS( NDF__MXDIM ) ! Image dimensions
      INTEGER EL                 ! Number of pixels in image
      INTEGER ERR                ! IMFORT error code
      REAL FILBAD                ! Value to replace bad pixels
      LOGICAL HISPRE             ! History records present?
      INTEGER IMDESC             ! The image descriptor returned by
                                 ! IMOPEN()
      CHARACTER*( STRLEN ) IMERRM ! IMFORT error message text
      INTEGER IMTYPE             ! Integer describing the IRAF image
                                 ! data type (only REAL and short
                                 ! INTEGER types are supported)
      CHARACTER*( STRLEN ) IRAFIL ! Output IRAF image name
      LOGICAL ISBAD              ! True if the mapped data array
                                 ! contains bad values
      CHARACTER*( NDF__SZTYP ) ITYPE ! Type used to map the NDF
      INTEGER JUNKIT             ! Integer to absorb unwanted output
                                 ! form PSX_LOCALTIME()
      INTEGER LBND( NDIM )       ! Lower bounds array
      CHARACTER*( FITSLN ) LINE  ! FITS format header line
      INTEGER LIPNTR             ! Pointer returned by PSX_CALLOC to
                                 ! contain each line of pixels
      INTEGER MDIM               ! Number of dimensions of the NDF
      INTEGER NAMELN             ! Variable to absorb an unwanted
                                 ! integer returned by MSG_LOAD
      INTEGER NDF                ! Identifier for input NDF
      INTEGER NREP               ! The number of bad values replaced
      INTEGER NTICKS             ! Integer returned by PSX_TIME()
      INTEGER PNTR( 1 )          ! Pointer for the input data array
      INTEGER PNTRT( 1 )         ! Pointer for the temporary data array
      LOGICAL PROFIT             ! Propagate the FITS extension?
      LOGICAL PROHIS             ! Propagate the history records?
      LOGICAL REPBAD             ! True if there are bad values to
                                 ! replace
      CHARACTER*25 TSTRING       ! Current time and date from
                                 ! PSX_ASCTIME()
      INTEGER TSTRUCT            ! Time structure from PSX_LOCALTIME()
      INTEGER UBND( NDIM )       ! Upper bounds array
      INTEGER*2 WFILBA           ! Value to replace word-type bad values

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! Declarations of conversion routines
      INCLUDE 'NUM_DEF_CVT'      ! Definitions of conversion routines

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

#if !HAVE_IRAF_LIBIMFORT_A

*  We do not have IRAF libraries so give an error message
	STATUS = SAI__ERROR
	CALL ERR_REP('NDF2IRAF', 'NDF2IRAF: Convert not built with '//
     :		'IRAF libraries. You will need to rebuild CONVERT',
     :		 STATUS)

#else

*  Initialise the IRAF status.
      ERR = 0

*  Determine whether or not the FITS extension is to be merged.
      CALL PAR_GET0L( 'PROFITS', PROFIT, STATUS )

*  Determine whether or not the HISTORY component is to be propagated.
      CALL PAR_GET0L( 'PROHIS', PROHIS, STATUS )

*  Access the input NDF and find its shape.
*  ========================================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Associate NDF identifier with IN, via the parameter system.
      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the bounds of the NDF.
      CALL NDF_BOUND( NDF, NDIM, LBND, UBND, MDIM, STATUS )

*  Check dimensions of the input image.  Strictly we should look for
*  two significant dimensions.  Just check for too many dimensions.
*  Have to determine how many dimensions there are, since NDF_BOUND
*  has a bug and will not return the actual number of dimensions, but 1
*  instead.  To make this work the status must be temporarily set to
*  good, then reinstated.
      IF ( STATUS .EQ. NDF__XSDIM ) THEN
         CALL NDF_MSG( 'NDF', NDF )
         STATUS = SAI__OK
         CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, MDIM, STATUS )
         CALL MSG_SETI( 'MDIM', MDIM )
         STATUS = NDF__XSDIM
         CALL ERR_REP( 'NDF2IRAF',
     :     'NDF2IRAF: Can only deal with one-, two-, and '/
     :     /'three-dimensional NDFs. ^NDF has ^MDIM dimensions.',
     :     STATUS )
         GOTO 999
      END IF

*  Derive the number of columns in the input NDF.
      DIMS( 1 ) = UBND( 1 ) - LBND( 1 ) + 1

*  Derive the number of lines in the input NDF.
      DIMS( 2 ) = UBND( 2 ) - LBND( 2 ) + 1

*  Derive the number of bands in the input NDF.
      DIMS( 3 ) = UBND( 3 ) - LBND( 3 ) + 1

*  Map the NDF data array using the appropriate data type.
*  =======================================================

*  Find the data type in which to process the NDF data array.
*  Determine the type of the data array in order to select the data
*  type of the IRAF image, which may be the equivalent of _WORD or
*  _REAL.
      CALL NDF_TYPE( NDF, 'Data', ATYPE, STATUS )

*  Use signed word for one- and two-byte integers, and real for all
*  other types. Unsigned word is excluded since any value greater than
*  32767 would be made bad.
      IF ( ATYPE .EQ. '_BYTE' .OR. ATYPE .EQ. '_WORD' .OR.
     :     ATYPE .EQ. '_UBYTE' ) THEN

         IMTYPE = 3
         ITYPE = '_WORD'
      ELSE

*  The output image will have REAL pixels. If users want to change the
*  pixel type, there are tasks within IRAF to do that.
         IMTYPE = 6
         ITYPE = '_REAL'
      END IF

*  Map the data array with the type needed by the IRAF image.
      CALL NDF_MAP( NDF, 'Data', ITYPE, 'READ', PNTR, EL, STATUS )

*  Look for any bad pixels and get a replacement value.
*  ====================================================

*  Check whether NDF contains bad pixels.
      CHECK = .TRUE.
      CALL NDF_BAD( NDF, 'Data', CHECK, ISBAD, STATUS )

*  Decide what to do with the bad values.  Choose limiting values for
*  the replacement value that prevent it itself being invalid.
      IF ( ISBAD ) THEN
         CALL ERR_MARK
         IF ( IMTYPE .EQ. 3 ) THEN
            CALL PAR_GDR0R( 'FILLBAD', VAL__BADR, REAL( NUM__MINW ),
     :                      REAL( NUM__MAXW ), .FALSE., FILBAD, STATUS )
         ELSE
            CALL PAR_GDR0R( 'FILLBAD', VAL__BADR, NUM__MINR, NUM__MAXR,
     :                      .FALSE., FILBAD, STATUS )
         END IF

*  Look for the null value to indicate no replacements.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            REPBAD = .FALSE.
         ELSE
            REPBAD = .TRUE.
         END IF
         CALL ERR_RLSE
      END IF

*  Replace the bad values in a temporary NDF.
*  ==========================================
      IF ( REPBAD ) THEN

*  Call PSX_CALLOC() for a one-dimensional array of the appropriate
*  data type in which to replace the bad values with the nominated
*  value.  As a temporary measure until PSX_CALLOC supports _WORD type
*  we have to call PSX_MALLOC instead.
         IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL PSX_MALLOC( EL * VAL__NBW, PNTRT, STATUS )
         ELSE
            CALL PSX_CALLOC( EL, ITYPE, PNTRT, STATUS )
         END IF

*  Call the appropriate routine to copy the data array from the original
*  to the workspace, and substitute the bad values.
         IF ( ITYPE .EQ. '_WORD' ) THEN

*  Use VAL_ to protect against potentionally harmful values when there
*  is a bad status.
            WFILBA = VAL_RTOW( .FALSE., FILBAD, STATUS )

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF.
            CALL KPG1_CHVAW( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       VAL__BADW, WFILBA,
     :                       %VAL( CNF_PVAL( PNTRT( 1 ) ) ),
     :                       NREP, STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF.
            CALL KPG1_CHVAR( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                       VAL__BADR, FILBAD,
     :                       %VAL( CNF_PVAL( PNTRT( 1 ) ) ),
     :                       NREP, STATUS )

         END IF

*  To save resources unmap the input NDF data array.
         CALL NDF_UNMAP( NDF, 'Data', STATUS )

*  To fool the later code that nothing has happened to the input NDF,
*  copy the pointer to the temporary data array into the pointer for
*  the input array.
         PNTR( 1 ) = PNTRT( 1 )
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Create the IRAF image.
*  ======================

*  Get a character string, via the parameter system, which will be the
*  name of the IRAF image.
      CALL PAR_GET0C( 'OUT', IRAFIL, STATUS )

*  Create the IRAF image.
      CALL IMCREA( IRAFIL, DIMS, MDIM, IMTYPE, ERR )
      IF ( ERR .NE. 0 ) GOTO 999

*  Set up some constants required by the IMFORT imopen() routine.

*  Access mode is 1 for read only and 3 for read and write access.
      ACCESS = 3

*  Open the IRAF image.
      CALL IMOPEN( IRAFIL, ACCESS, IMDESC, ERR )
      IF ( ERR .NE. 0 ) GOTO 999

*  Call PSX_CALLOC() for a one-dimensional array of the appropriate
*  data type to hold each line of the image.  As a temporary measure
*  until PSX_CALLOC supports _WORD type we have to call PSX_MALLOC
*  instead.
      IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL PSX_MALLOC( DIMS( 1 ) * VAL__NBW, LIPNTR, STATUS )
      ELSE
         CALL PSX_CALLOC( DIMS( 1 ), ITYPE, LIPNTR, STATUS )
      END IF

*  Copy the data array to the IRAF image, using the line buffer to
*  reduce the required virtual memory in an appropriate routine for
*  the type.
      IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CON_CD2IW( MDIM, DIMS( 1 ), DIMS( 2 ), DIMS( 3 ),
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), IMDESC, ERR,
     :                   %VAL( CNF_PVAL( LIPNTR ) ), STATUS )

      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CON_CD2IR( MDIM, DIMS( 1 ), DIMS( 2 ), DIMS( 3 ),
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), IMDESC, ERR,
     :                   %VAL( CNF_PVAL( LIPNTR ) ), STATUS )
      END IF

*  Report the number of replacements.
      IF ( NREP .GT. 0 .AND. REPBAD ) THEN
         CALL MSG_SETI( 'NR', NREP )
         CALL MSG_SETC( 'IRAFIL', IRAFIL )
         CALL MSG_SETR( 'RV', FILBAD )
         CALL NDF_MSG( 'NDF', NDF )
         CALL MSG_OUTIF( MSG__NORM, 'NUMREP',
     :      '^NR bad values in the NDF ^NDF have been replaced by ^RV '/
     :      /'in the IRAF file ^IRAFIL.', STATUS )
      END IF

*  Free the CALLOCked array.
      CALL PSX_FREE( LIPNTR, STATUS )

*  Unmap the NDF if it has not already been unmapped, or the workspace
*  when bad pixels had to be substituted.
      IF ( REPBAD ) THEN
         CALL PSX_FREE( PNTRT, STATUS )
      ELSE
         CALL NDF_UNMAP( NDF, 'Data', STATUS )
      END IF

*  Now that the tidying has been done, exit if there was an error
*  copying the array tothe IRAF file.
      IF ( ERR .NE. 0 ) GOTO 999

*  Propagate ancillary information to the header file.
*  ===================================================
      IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL COI_WHEAD( NDF, 'Data', IMDESC, 16, PROFIT, STATUS )
      ELSE
         CALL COI_WHEAD( NDF, 'Data', IMDESC, -32, PROFIT, STATUS )
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 999


*  Write HISTORY to the IRAF image.
*  ================================
      CALL NDF_STATE( NDF, 'History', HISPRE, STATUS )
      IF ( PROHIS .AND. HISPRE ) CALL COI_WHISR( NDF, IMDESC, STATUS )

*  Write supplementary HISTORY to the IRAF image.
*  ==============================================

*  Update the history information of the IRAF image by adding a
*  FITS-format HISTORY line to say where the image came from.

*  Initialise the line.
      LINE = 'HISTORY Image converted using STARLINK utility NDF2IRAF'/
     :       /' from the NDF:'

*  Add the line to the image.
      CALL ADLINE( IMDESC, LINE )

*  Find the name of the NDF and the current time counter.
      CALL NDF_MSG( 'NDFNAME', NDF )
      CALL PSX_TIME( NTICKS, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*  Find the date and time.
         CALL PSX_LOCALTIME( NTICKS, JUNKIT, JUNKIT, JUNKIT, JUNKIT,
     :                       JUNKIT, JUNKIT, JUNKIT, JUNKIT, JUNKIT,
     :                       TSTRUCT, STATUS )
         CALL PSX_ASCTIME( TSTRUCT, TSTRING, STATUS )
         CALL MSG_SETC( 'TIME', TSTRING )

*  Create the history text.
         CALL MSG_LOAD( ' ', 'HISTORY ^NDFNAME on ^TIME', LINE,
     :                  NAMELN, STATUS )
      ELSE
         CALL MSG_LOAD( ' ', 'HISTORY ^NDFNAME.', LINE, NAMELN, STATUS )
      END IF

*  Add the card image to the IRAF image.
      CALL ADLINE( IMDESC, LINE )

*  Close the IRAF image.
      CALL IMCLOS( IMDESC, ERR )

  999 CONTINUE

*  Check for an error from IMFORT.
      IF ( ERR .NE. 0 ) THEN

*  Convert the IMFORT error status into the appropriate error text.
         CALL IMEMSG( ERR, IMERRM )

*  Make the error report.
         CALL MSG_SETC( 'IRAFER', IMERRM )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'NDF2IRAF_IRAFERR',
     :     'NDF2IRAF: There has been an error creating the IRAF file. '/
     :     /'The error text is: ^IRAFER', STATUS )
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

*   Issue the standard error message if something has failed.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF2IRAF_ERR',
     :     'NDF2IRAF: Unable to convert the NDF into an IRAF image.',
     :     STATUS )
      END IF

#endif

      END

