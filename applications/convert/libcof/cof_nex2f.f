      SUBROUTINE COF_NEX2F( SNAME, FUNIT, NDFP, NDF, FILNAM, NOARR,
     :                      ARRNAM, BITPIX, BLOCKF, ORIGIN, PROFIT,
     :                      DUPLEX, PROHIS, SUMS, ENCOD, NATIVE,
     :                      STATUS )
*+
*  Name:
*     COF_NEX2F

*  Purpose:
*     Converts a SMURF extension NDF into FITS IMAGE extensions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_NEX2F( SNAME, FUNIT, NDFP, NDF, FILNAM, NOARR, ARRNAM,
*                     BITPIX, BLOCKF, ORIGIN, PROFIT, DUPLEX, PROHIS,
*                     SUMS, ENCOD, NATIVE, STATUS )

*  Description:
*     This routine converts an NDF into a FITS file.  It uses as much
*     standard NDF information as possible to define the headers...

*  Arguments:
*     SNAME = CHARACTER * ( * ) (Given)
*        The name of structure.  It is used to form the EXTNAME
*        keyword.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     NDFP = INTEGER (Given)
*        The identifier of the parent NDF whose FITS extension and
*        HISTORY records may be accessed.
*     NDF = INTEGER (Given)
*        The identifier of the NDF to be converted to a FITS file.
*     FILNAM = CHARACTER * ( * ) (Given)
*        The name of the output FITS file.
*     NOARR = INTEGER (Given)
*        The number of NDF arrays to copy.
*     ARRNAM( NOARR ) = CHARACTER * ( * ) (Given)
*        The names (in uppercase) of the NDF array components to write
*        to the FITS file.  These should be in the order to be written.
*        If the DATA component is present it should be first.
*     BITPIX = INTEGER (Given)
*        The number of bits per pixel (FITS BITPIX) required for the
*        output FITS file.  In addition there are three special values.
*        A value of 0 means use the BITPIX of the input array.  A value
*        of -1 means use the value of the BITPIX keyword in the NDF's
*        FITS extension; if the extension or BITPIX card is absent, the
*        BITPIX of the input array is used.  BITPIX=1 requests that any
*        scaled arrays in the NDF be copied to the scaled data type.
*        In the absence of a scaled array, behaviour reverts to
*        BITPIX=-1, which may in turn be effectively BITPIX=0.
*     BLOCKF = INTEGER (Given)
*        The blocking factor for the output file.  It must be a positive
*        integer between 1 and 10.
*     ORIGIN = CHARACTER * ( * ) (Given)
*        The name of the institution where the FITS file originates.
*        This is used to create the ORIGIN card in the FITS header.
*        A blank value gives a default of "Starlink Software".
*     PROFIT = LOGICAL (Given)
*        If .TRUE., the contents of the FITS airlock, if present, are
*        merged into the FITS header.  Argument DUPLEX qualifies to
*        which arrays this applies.  Certain cards in this extension
*        are not propagated ever and others may only be propagated when
*        certain standard items are not present in the NDF.  See routine
*        COF_WHEAD for details.
*     DUPLEX = LOGICAL (Give)
*        This qualifies the effect of PROFIT=.TRUE.  A .FALSE. value
*        means that the airlocks headers only appear with the primary
*        array.  Supplying .TRUE., propagates the FITS airlock headers
*        for other array components of the NDF.
*     PROHIS = LOGICAL (Given)
*        If .TRUE., any NDF history records are written to the primary
*        FITS header as HISTORY cards.  These follow the mandatory
*        headers and any merged FITS-extension headers (see PROFIT).
*     SUMS = LOGICAL (Given)
*        If .TRUE., DATASUM and CHECKSUM headers are written to each
*        HDU.
*     ENCOD = CHARACTER * ( * ) (Given)
*        The encoding to use. If this is blank, then a default encoding
*        is chosen based on the contents of the FITS extension. The
*        supplied string should be a recognised AST encoding such as
*        'DSS', 'FITS-WCS', 'NATIVE', etc (or a blank string).
*     NATIVE = LOGICAL (Given)
*        Should a NATIVE encoding of the WCS info be included in the
*        header?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The rules for the conversion of an NDF are as follows:
*     -  The NDF main data array becomes the primary data array of the
*     FITS file if it is in value of parameter COMP, otherwise the first
*     array defined by parameter COMP will become the primary data
*     array.  A conversion from floating point to integer or to a
*     shorter integer type will cause the output array to be scaled and
*     offset, the values being recorded in keywords BSCALE and BZERO.
*     There is an offset (keyword BZERO) applied to signed byte and
*     unsigned word types to make them unsigned-byte and signed-word
*     values respectively in the FITS array (this is because FITS does
*     not support these data types).
*     -  The FITS keyword BLANK records the bad values for integer
*     output types.  Bad values in floating-point output arrays are
*     denoted by IEEE not-a-number values.
*     -  The NDF's quality and variance arrays appear in individual
*     FITS IMAGE extensions immediately following the primary header
*     and data unit, unless that component already appears as the
*     primary data array.  The quality array will always be written as
*     an unsigned-byte array in the FITS file, regardless of the value
*     of the parameter BITPIX.
*     -  Here are details of the processing of standard items from the
*     NDF into the FITS header, listed by FITS keyword.
*        SIMPLE, EXTEND, PCOUNT, GCOUNT --- all take their default
*          values.
*        BITPIX, NAXIS, NAXISn --- are derived directly from the NDF
*          data array; however the BITPIX in the FITS airlock extension
*          is transferred when argument BITPIX is -1.
*        CRVALn, CDELTn, CRPIXn, CTYPEn, CUNITn --- are derived from
*          the NDF axis structures if possible.  If no linear NDF axis
*          structures are present, the values in the NDF's FITS
*          extension are copied (when argument PROFIT is .TRUE.).  If
*          any axes are non-linear, all FITS axis information is lost.
*        OBJECT, LABEL, BUNIT --- the values held in the NDF's title,
*          label, and units components respectively are used if
*          they are defined; otherwise any values found in the FITS
*          extension are used (provided parameter PROFIT is .TRUE.).
*        DATE --- is created automatically.
*        ORIGIN --- inherits any existing ORIGIN card in the NDF FITS
*          extension, unless you supply a value through argument
*          ORIGIN other than the default "Starlink Software" or
*          a blank string.
*        EXTNAME --- is the is the path of the extension within the
*          NDF.  If the path is too long to fit within the header
*          (68 characters), EXTNAME is set to  '@EXTNAMEF'.  The full
*          path is then stored in keyword EXTNAMEF using the HEASARC
*          Long-string CONTINUE convention
*          (http://fits.gsfc.nasa.gov/registry/continue_keyword.html).
*        EXTVER --- is only set when EXTNAME (q.v.) cannot accommodate
*          the component name and is assigned the HDU index to provide a
*          unique identifier.
*        EXTLEVEL --- is the level in the hierarchical structure of the
*          extensions.  Thus a top-level extension has value 1,
*          sub-components of this extension have value 2 and so on.
*        EXTTYPE --- is set to "NDF".
*        HDUCLAS1, HDUCLASn --- "NDF" and the value of COMP
*          respectively.
*        LBOUNDn --- is the pixel origin for the nth dimension when
*          any of the pixel origins is not equal to 1.  (This is not a
*          standard FITS keyword.)
*        XTENSION, BSCALE, BZERO, BLANK and END --- are not propagated
*          from the NDF's FITS extension.  XTENSION will be set for
*          any extension.  BSCALE and BZERO will be defined based on
*          the chosen output data type in comparison with the NDF
*          array's type, but cards with values 1.0 and 0.0 respectively
*          are written to reserve places in the header section.  These
*          `reservation' cards are for efficiency and they can always
*          be deleted later.  BLANK is set to the Starlink standard bad
*          value corresponding to the type specified by the BITPIX
*          keyword, but only for integer types and not for the quality
*          array.  It appears regardless of whether or not there are
*          bad values actually present in the array; this is for the
*          same efficiency reasons as before.  The END card terminates
*          the FITS header.  The END card is written by FITSIO
*          automatically once the header is closed.
*        HISTORY cards are propagated from the FITS airlock when
*          PROFIT is .TRUE., and from the NDF HISTORY component when
*          PROHIS is .TRUE..

*  Prior Requirements:
*     -  A primary HDU unit exists in the FITS file, and the file is
*     open.

*  Copyright:
*     Copyright (C) 2007, 2009, 2011 Science & Technology Facilities
*     Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 October 21 (MJC):
*        Original version based upon COF_NDF2F.
*     2007 October 25 (MJC):
*        Use revised COF_WHEAD API.
*     2007 October 26 (MJC):
*        Implement the further revised COF_WHEAD API.
*     2011 January 12 (MJC):
*        Use KPG_TYPSZ instead of COF_TYPSZ.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'CNF_PAR'          ! CNF functions
      INCLUDE 'MSG_PAR'          ! Message-system constants

*  Arguments Given:
      CHARACTER * ( * ) SNAME
      INTEGER FUNIT
      INTEGER NDFP
      INTEGER NDF
      CHARACTER * ( * ) FILNAM
      INTEGER NOARR
      CHARACTER * ( * ) ARRNAM( NOARR )
      INTEGER BITPIX
      INTEGER BLOCKF
      CHARACTER * ( * ) ORIGIN
      LOGICAL PROFIT
      LOGICAL DUPLEX
      LOGICAL PROHIS
      LOGICAL SUMS
      CHARACTER * ( * ) ENCOD
      LOGICAL NATIVE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER   FITSOK           ! Value of good FITSIO status
      PARAMETER ( FITSOK = 0 )

      INTEGER   GCOUNT           ! Value of FITS GCOUNT keyword
      PARAMETER ( GCOUNT = 1 )

      INTEGER   PCOUNT           ! Value of FITS PCOUNT keyword
      PARAMETER ( PCOUNT = 0 )

*  Local Variables:
      LOGICAL BAD                ! Bad values may be present in array?
      INTEGER BLANK              ! Data blank for integer arrays
      INTEGER BP                 ! Local, possibly modified, BITPIX
      INTEGER BPIN               ! Input array's BITPIX
      INTEGER BPINU              ! Input array's BITPIX unsigned version
      INTEGER BPOUT              ! Output array's BITPIX
      INTEGER BPOUTU             ! Output array's BITPIX unsigned
                                 ! version
      DOUBLE PRECISION BSCALE    ! Block-integer scale factor
      CHARACTER * ( 200 ) BUFFER ! Buffer for error messages
      DOUBLE PRECISION BZERO     ! Block-integer offset
      CHARACTER * ( 48 ) COMENT  ! Comment from FITS-extension header
      DOUBLE PRECISION DELTA     ! Machine precision for scaling
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      INTEGER EL                 ! Number of elements in array
      LOGICAL FEXIST             ! FITS already exists?
      CHARACTER FORM * ( NDF__SZFRM ) ! Storage form
      DOUBLE PRECISION FSCALE    ! Reduction factor for scale
      INTEGER FSTAT              ! FITSIO error status
      INTEGER FSTATC             ! FITSIO error status for file closure
      LOGICAL HISPRE             ! HISTORY records present?
      INTEGER I                  ! Loop counter
      INTEGER ICOMP              ! Loop counter
      INTEGER IDEL               ! Increment to reduce an integer
                                 ! scaling range
      INTEGER IPNTR              ! Pointer to input array
      DOUBLE PRECISION MAXV      ! Max. value to appear in scaled array
      DOUBLE PRECISION MINV      ! Min. value to appear in scaled array
      INTEGER NBYTES             ! Number of bytes per array value
      INTEGER NC                 ! Number of character in string
      INTEGER NCF                ! Number of characters in filename
      INTEGER NDECIM             ! Number of decimal places in header
                                 ! value
      INTEGER NDIM               ! Number of dimensions
      LOGICAL NSCALE             ! Array component is scaled in the NDF?
      BYTE NULL8                 ! Null value for BITPIX=8
      INTEGER * 2 NULL16         ! Null value for BITPIX=16
      INTEGER NULL32             ! Null value for BITPIX=32
      REAL NUL_32                ! Null value for BITPIX=-32
      DOUBLE PRECISION NUL_64    ! Null value for BITPIX=-64
      LOGICAL OPEN               ! FITS file exists?
      LOGICAL PROPEX             ! Propagate FITS extension for the
                                 ! current header?
      INTEGER SBYTES             ! No. of bytes per scaled array value
      LOGICAL SCALE              ! The array is to be scaled?
      CHARACTER SCTYPE * ( DAT__SZTYP )! Data type for scaled arrays
      LOGICAL SHIFT              ! A BZERO offset is required?
      LOGICAL THERE              ! BITPIX FITS header card ! is present?
      CHARACTER * ( NDF__SZTYP ) TYPE ! NDF array's data type
      LOGICAL VALID              ! The NDF identifier is valid?

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Validate the NDF identifier.
*  ============================
      CALL NDF_VALID( NDF, VALID, STATUS )

*  Report an error if the identifier is not valid.
      IF ( .NOT. VALID ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_NEX2F_INVNDF',
     :     'COF_NEX2F: The identifier to the input NDF is invalid. '/
     :     /'(Probable programming error.)', STATUS )
         GOTO 999
      END IF

*  Inquire the shape of the NDF.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Take a local copy of the supplied BITPIX in case in needs to be
*  modified.
      BP = BITPIX

*  Loop for each array component.
*  ==============================
      DO ICOMP = 1, NOARR

         NSCALE = .FALSE.

*  Define the structure of the array.
*  ==================================

*  Obtain the NDF numeric type.
         CALL NDF_TYPE( NDF, ARRNAM( ICOMP ), TYPE, STATUS )

*  Obtain the bad-pixel flag.
         CALL NDF_BAD( NDF, ARRNAM( ICOMP ), .FALSE., BAD, STATUS )

*  Obtain the array form, and the scale and offset for a scaled array.
         NSCALE = .FALSE.
         IF ( BITPIX .EQ. 1 ) THEN
            CALL NDF_FORM( NDF, 'Data', FORM, STATUS )
            NSCALE = FORM .EQ. 'SCALED' .AND.
     :               ARRNAM( ICOMP ) .NE. 'QUALITY'

            IF ( NSCALE ) THEN
               CALL NDF_SCTYP( NDF, ARRNAM( ICOMP ), SCTYPE, STATUS )
               CALL NDF_GTSZD( NDF, ARRNAM( ICOMP ), BSCALE, BZERO,
     :                         STATUS )

*  Try to obtain the native type from the FITS airlock as there's no
*  SCALED array.
            ELSE
               BP = -1
            END IF
         END IF

*  Find the input BITPIX.
*  ======================

*  Get the the number of bytes per data type.
         CALL KPG_TYPSZ( TYPE, NBYTES, STATUS )
         IF ( NSCALE ) CALL KPG_TYPSZ( SCTYPE, SBYTES, STATUS )

*  Convert this to a pseudo-BITPIX value, where floating point values
*  are designated by being greater than the integer types.  The unsigned
*  versions are needed to determine ascendency, and so whether or not
*  scaling is required.
         IF ( TYPE .EQ. '_REAL' .OR. TYPE .EQ. '_DOUBLE' ) THEN
            BPIN = -NBYTES * 8
            BPINU = 1 - BPIN
         ELSE
            BPIN = NBYTES * 8
            BPINU = BPIN
         END IF

*  Define the effective output data type (BITPIX).
*  ===============================================

*  Note the QUALITY will always be unsigned byte and therefore we
*  ignore the supplied BITPIX.
         IF ( ARRNAM( ICOMP ) .EQ. 'QUALITY' ) THEN
            BPOUT = 8
            BPOUTU = BPOUT

*  Use the BITPIX from the airlock.
*  ---------------------------------
*  Search the FITS extension for the first BITPIX keyword, and return
*  its value.
         ELSE IF ( BP .EQ. -1 ) THEN
            CALL CON_EKEYI( NDFP, 'BITPIX', 1, THERE, BPOUT, COMENT,
     :                      STATUS )

            IF ( THERE ) THEN
               BPOUTU = BPOUT

*  Use the input array's values if the BITPIX keyword does not exist.
             ELSE
               BPOUT = BPIN
               BPOUTU = BPINU
            END IF

*  Just use the input array's BITPIX.
*  ----------------------------------
         ELSE IF ( BITPIX .EQ. 0 ) THEN
            BPOUT = BPIN
            BPOUTU = BPINU

*  Use the Native BITPIX.
*  ----------------------
         ELSE IF ( BITPIX .EQ. 1 ) THEN
            BPOUT = SBYTES * 8
            BPOUTU = BPOUT

*  Use the user-defined regular BITPIX value.
*  ------------------------------------------
         ELSE
            BPOUT = BITPIX
            BPOUTU = BPOUT
         END IF

*  Allow for the sign of floating-point BITPIX values to form the
*  unsigned BITPIX.
         IF ( BPOUTU .EQ. -32 .OR. BPOUTU .EQ. -64 ) BPOUTU = 1 - BPOUTU

*  Open a new header and data unit for extensions.
*  ===============================================

*  A new HDU is created when the file is opened, or when the file
*  was left open for another NDF, so here a new header need only
*  be opened for extensions.
         CALL FTCRHD( FUNIT, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_NEX2F_NHDU', 'FTCRHD',
     :         'Error creating the header and data unit for an '/
     :         /'IMAGE extension.', STATUS )
            GOTO 999
         END IF

*  Write the header.
*  =================
*
*  Decide whether or not to propagate the FITS extension.  It will
*  only appear in the primary array's header, if requested.
         PROPEX = ( PROFIT .AND. ( ICOMP .EQ. 1 .OR. DUPLEX ) )

*  First write the standard headers, and merge in the FITS extension
*  when requested to do so.
         CALL COF_WHEAD( NDF, NDFP, ARRNAM( ICOMP ), FUNIT, BPOUT,
     :                   PROPEX, ORIGIN, ENCOD, NATIVE, .FALSE., SNAME,
     :                   STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine whether or not there are history records in the NDF.
*  Append the history records for the first array.
         IF ( PROHIS .AND. ICOMP .EQ. 1 ) THEN
            CALL NDF_STATE( NDFP, 'History', HISPRE, STATUS )
            IF ( HISPRE ) CALL COF_WHISR( NDFP, FUNIT, STATUS )
         END IF

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine the block-floating point conversion and blank value.
*  ==============================================================
*
*  This code is a little messy for efficiency.  We have to determine
*  certain properties of the transformation between FITS values and
*  input array value in order to set the BLANK, BSCALE, and BZERO
*  cards, so that the cards must be written before we write the
*  array.  This means a two-stage process.

*  Set the defaults.
         SCALE = .FALSE.
         SHIFT = .FALSE.
         DELTA = DBLE( VAL__EPSR )
         FSCALE = 1.0D0 - DELTA

*  We already know the scaling coefficients for a scaled array.
         IF ( .NOT. NSCALE ) THEN
            BSCALE = 1.0D0
            BZERO = 0.0D0
         END IF

*  Set the null values.  Only one will be needed, depending on the
*  value of BPOUT, but it as efficient to assign them all.
         NULL32 = VAL__BADI
         NULL16 = VAL__BADW
         NULL8 = VAL__BADUB
         NUL_32 = VAL__BADR
         NUL_64 = VAL__BADD

*  Is format conversion required?
*  ------------------------------
*
*  Scaling is required when the requested BITPIX has lower precision
*  than the array type and BITPIX is an integer type.  Also scaling
*  or the application of an offset is needed when the input data type
*  does not match the FITS data types, namely _BYTE and _UWORD.  Deal
*  with these special cases first...
*
*  Unsigned word
*  -------------
*  Map the input data using the next integer data type (_INTEGER) that
*  encompasses the dynamic range of values, as there is no FITSIO
*  routine for writing a _UWORD array to the FITS file.  Adjust the
*  input BITPIX accordingly.
         IF ( TYPE .EQ. '_UWORD' ) THEN
            TYPE = '_INTEGER'
            BPIN = 32

*  Unsigned words can be stored in FITS as signed words with an offset
*  of 32k.
            IF ( BPOUT .EQ. 16 ) THEN
               SHIFT = .TRUE.
               BZERO = 32768.0D0
               BLANK = NUM_UWTOI( VAL__BADUW ) - 32768

*  Eight-bit output data implies that the values will need scaling.
            ELSE IF ( BPOUT .EQ. 8 ) THEN
               SCALE = .TRUE.
               BLANK = NUM_UBTOI( VAL__BADUB )

            ELSE IF ( BPOUT .GT. 0 ) THEN
               BLANK = VAL__BADI
            END IF

*  Signed byte
*  -----------
*  Map the input data using the next integer data type (_WORD) that
*  encompasses the dynamic range of values, as there is no FITSIO
*  routine for writing a _BYTE array to the FITS file.  Adjust the
*  input BITPIX accordingly.
         ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
            TYPE = '_WORD'
            BPIN = 16

*  Record the fact that an offset is required, and reset the bad pixel
*  (BLANK) value.
            IF ( BPOUT .EQ. 8 ) THEN
               SHIFT = .TRUE.
               BZERO = -128.0D0
               BLANK = NUM_BTOI( VAL__BADB ) + 128

            ELSE IF ( BPOUT .GT. 0 ) THEN
               BLANK = NUM_WTOI( VAL__BADW )

            END IF

*  Compare the component's BITPIX with that supplied.
         ELSE IF ( BPOUTU .LT. BPINU ) THEN

*  The data must be rescaled and the bad-pixel value altered to that of
*  the output type.
            SCALE = .TRUE.

*  Integer types will have a blank value.  Note that it is the output
*  type that is assigned.
            IF ( BPOUT .EQ. 32 ) THEN
               BLANK = VAL__BADI

            ELSE IF ( BPOUT .EQ. 16 ) THEN
               BLANK = NUM_WTOI( VAL__BADW )

            ELSE IF ( BPOUT .EQ. 8 ) THEN
               BLANK = NUM_UBTOI( VAL__BADUB )

            END IF

*  Deal with the types where no scaling or offset is required.
         ELSE

*  Integer types will have a blank value.  These are based upon the type
*  of the NDF array.  The FITSIO routine will perform any type
*  conversion required.
            IF ( TYPE .EQ. '_INTEGER' ) THEN
               BLANK = VAL__BADI

            ELSE IF ( TYPE .EQ. '_WORD' ) THEN
               BLANK = NUM_WTOI( VAL__BADW )

            ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
               BLANK = NUM_UBTOI( VAL__BADUB )

            END IF
         END IF

*  Set the blank value in the header.
*  ==================================

*  Only required for the integer types.  BLANK has no meaning for
*  floating-point in FITS.
         IF ( BPOUT .GT. 0 .AND.
     :        ARRNAM( ICOMP ) .NE. 'QUALITY' ) THEN

*  The header should already contain a BLANK keyword.
*  Reset the BLANK keyword in the header. Ampersand instructs the
*  routine not to modify the comment of the BLANK header card.
            CALL FTMKYJ( FUNIT, 'BLANK', BLANK, '&', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL COF_FIOER( FSTAT, 'COF_NEX2F_BLANK1', 'FTMKYJ',
     :           'Error modifying the BLANK header card.', STATUS )
               GOTO 999
            END IF

         END IF

*  Find the scaling.
*  =================
         IF ( SCALE .AND. .NOT. NSCALE ) THEN

*  To scale we have to map the input array, find the extreme values,
*  and hence derive the scale and offset.  These are then applied to
*  form a new work array of the same data type wherein the bad values
*  are replaced by the bad values for the output data type.  This array
*  is then passed to the FITSIO routine which performs a type
*  conversion to the required output type.

*  First map the input array component.
            CALL NDF_MAP( NDF, ARRNAM( ICOMP ), TYPE, 'READ', IPNTR,
     :                    EL, STATUS )

*  Set the scaling limits to double precision.  The _DOUBLE output
*  would not need scaling so it is omitted.  This is done so that the
*  scaling routine need only be generic for the input data type, and
*  not for the output too.
            IF ( BPOUT .EQ. 8 ) THEN
               MAXV = NUM_UBTOD( VAL__MAXUB )
               MINV = NUM_UBTOD( VAL__MINUB )

            ELSE IF ( BPOUT .EQ. 16 ) THEN
               MAXV = NUM_WTOD( VAL__MAXW ) - 1.0D0
               MINV = NUM_WTOD( VAL__MINW ) + 1.0D0

            ELSE IF ( BPOUT .EQ. 32 ) THEN
               IDEL = MAX( INT( DBLE( VAL__MAXI ) * DELTA ) ,
     :                     INT( DBLE( VAL__MINI ) * DELTA ) ) + 1
               MAXV = DBLE( VAL__MAXI - SIGN( IDEL, VAL__MAXI ) )
               MINV = DBLE( VAL__MINI - SIGN( IDEL, VAL__MINI ) )

            ELSE IF ( BPOUT .EQ. -32 ) THEN
               MAXV = DBLE( VAL__MAXR ) * FSCALE
               MINV = DBLE( VAL__MINR ) * FSCALE

            END IF

*  Abort if an error has occurred.  We do this check so that we can be
*  confident that any error which is detected after the next block of
*  calls to COF_ESCOx) was produiced by COF_ESCOx.
            IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Evaluate the scaling and offset.  Call the appropriate routine
*  dependent on the array-component's type to evaluate the scaling.
*  Note that _UBYTE and _BYTE will never need scaling; _BYTE and _UWORD
*  need a shift of BZERO.  The scaling itself is done by FITSIO (FTPSCL
*  sets the scale and offset).
            IF ( TYPE .EQ. '_WORD' ) THEN
               CALL COF_ESCOW( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                         MINV, MAXV, BSCALE, BZERO, STATUS )

            ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
               CALL COF_ESCOI( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                         MINV, MAXV, BSCALE, BZERO, STATUS )

            ELSE IF ( TYPE .EQ. '_REAL' ) THEN
               CALL COF_ESCOR( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                         MINV, MAXV, BSCALE, BZERO, STATUS )

            ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
               CALL COF_ESCOD( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                         MINV, MAXV, BSCALE, BZERO, STATUS )

            END IF

*  An error (SAI__ERROR) will have been reported if all the pixel values
*  were bad.  In this case, we annul the error and use default BSCALE
*  and BZERO values (the resulting NDF will be filled with BLANK
*  values).
            IF ( STATUS .EQ. SAI__ERROR .AND. BAD ) THEN
               CALL ERR_ANNUL( STATUS )
               BSCALE = 1.0
               BZERO = 0.0
            END IF

         ELSE

*  No scaling required.
*  ====================
*
*  Any type conversion will be performed by the FITSIO array-writing
*  routine.
            CALL NDF_MAP( NDF, ARRNAM( ICOMP ), TYPE, 'READ', IPNTR,
     :                    EL, STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Revise the scale and zero cards.
*  ================================
         IF ( SCALE .OR. SHIFT .OR. NSCALE ) THEN

*  Decide the appropriate number of decimals needed to represent the
*  block floating point scale and offset.
            IF ( ( TYPE .EQ. '_DOUBLE'  .AND. .NOT. NSCALE ) .OR.
     :           SCTYPE .EQ. '_DOUBLE' ) THEN
               NDECIM = INT( -LOG10( VAL__EPSD ) )
            ELSE
               NDECIM = INT( -LOG10( VAL__EPSR ) )
            END IF

*  Reset the BSCALE keyword in the header. Ampersand instructs the
*  routine not to modify the comment of the BSCALE header card.
            CALL FTMKYD( FUNIT, 'BSCALE', BSCALE, NDECIM, '&', FSTAT )

*  Similarly for the BZERO card.
            CALL FTMKYD( FUNIT, 'BZERO', BZERO, NDECIM, '&', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL COF_FIOER( FSTAT, 'COF_NEX2F_HSCOF', 'FTMKYD',
     :           'Error modifying the BSCALE or BZERO header card.',
     :           STATUS )
               GOTO 999
            END IF
         END IF

*  Set the data scaling and offset.
*  ================================
         CALL FTPSCL( FUNIT, BSCALE, BZERO, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL COF_FIOER( FSTAT, 'COF_NEX2F_SCOF', 'FTPSCL',
     :        'Error defining the scale and offset.', STATUS )
            GOTO 999
         END IF

*  Set the blank data value.
*  =========================

*  Only required for the integer types.  BLANK has no meaning for
*  floating-point in FITS.  Note that this moust be done after the call
*  to FTPDEF, and so cannot be done when the header value is modified.
         IF ( BPOUT .GT. 0 ) THEN

*  Set the data blank value.
            CALL FTPNUL( FUNIT, BLANK, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL COF_FIOER( FSTAT, 'COF_NEX2F_BLANK2', 'FTPNUL',
     :           'Error modifying the BLANK value.', STATUS )
               GOTO 999
            END IF

         END IF

*  Write the output array to the FITS file.
*  ========================================
         IF ( BAD ) THEN

*  Call the appropriate routine for the data type of the supplied
*  array.  The group is 0, and we always start at the first element.
*  Remember that the input BITPIX values for floating point are one
*  minus the true BITPIX (the non-standard values were needed to
*  determine whether or not scaling was required).  The arrays may have
*  bad pixels.
            IF ( BPIN .EQ. 8 ) THEN
               CALL FTPPNB( FUNIT, 0, 1, EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                      NULL8, FSTAT )

            ELSE IF ( BPIN .EQ. 16 ) THEN
               CALL FTPPNI( FUNIT, 0, 1, EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                      NULL16, FSTAT )

            ELSE IF ( BPIN .EQ. 32 ) THEN
               CALL FTPPNJ( FUNIT, 0, 1, EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                      NULL32, FSTAT )

            ELSE IF ( BPIN .EQ. -32 ) THEN
               CALL FTPPNE( FUNIT, 0, 1, EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                      NUL_32, FSTAT )

            ELSE IF ( BPIN .EQ. -64 ) THEN
               CALL FTPPND( FUNIT, 0, 1, EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                      NUL_64, FSTAT )
            END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
            IF ( FSTAT .GT. FITSOK ) THEN
               NC = CHR_LEN( ARRNAM( ICOMP ) )
               BUFFER = 'Error writing '//ARRNAM( ICOMP )( :NC )/
     :                  /' array component to FITS file '/
     :                  /FILNAM( :NCF )//'.'
               CALL COF_FIOER( FSTAT, 'COF_NEX2F_WRDATAERR',
     :                         'FTPPNx', BUFFER, STATUS )
               GOTO 999
            END IF

         ELSE

*  Call faster routine when there are no bad pixels.  Call the
*  appropriate routine for the data type of the supplied array.  The
*  group is 0, and we always start at the first element.  Remember that
*  the input BITPIX values for floating point are one minus the true
*  BITPIX (the non-standard values were needed to determine whether or
*  not scaling was required).
            IF ( BPIN .EQ. 8 ) THEN
               CALL FTPPRB( FUNIT, 0, 1, EL,
     :                      %VAL( CNF_PVAL( IPNTR ) ), FSTAT )

            ELSE IF ( BPIN .EQ. 16 ) THEN
               CALL FTPPRI( FUNIT, 0, 1, EL,
     :                      %VAL( CNF_PVAL( IPNTR ) ), FSTAT )

            ELSE IF ( BPIN .EQ. 32 ) THEN
               CALL FTPPRJ( FUNIT, 0, 1, EL,
     :                      %VAL( CNF_PVAL( IPNTR ) ), FSTAT )

            ELSE IF ( BPIN .EQ. -32 ) THEN
               CALL FTPPRE( FUNIT, 0, 1, EL,
     :                      %VAL( CNF_PVAL( IPNTR ) ), FSTAT )

            ELSE IF ( BPIN .EQ. -64 ) THEN
               CALL FTPPRD( FUNIT, 0, 1, EL,
     :                      %VAL( CNF_PVAL( IPNTR ) ), FSTAT )

            END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
            IF ( FSTAT .GT. FITSOK ) THEN
               NC = CHR_LEN( ARRNAM( ICOMP ) )
               BUFFER = 'Error writing '//ARRNAM( ICOMP )( :NC )/
     :                  /' array component to FITS file '/
     :                  /FILNAM( :NCF )//'.'
               CALL COF_FIOER( FSTAT, 'COF_NEX2F_WRDATAERR',
     :                         'FTPPRx', BUFFER, STATUS )
               GOTO 999
            END IF

         END IF

*  Tidy the array.
*  ===============
*  Unmap the input array.
         CALL NDF_UNMAP( NDF, ARRNAM( ICOMP ), STATUS )

*  Write integrity-check headers.  Note any existing values present
*  derived from the FITS airlock will be replaced.
         IF ( SUMS ) CALL FTPCKS( FUNIT, STATUS )

      END DO

  999 CONTINUE

      END
