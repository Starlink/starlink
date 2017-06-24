      SUBROUTINE COF_NDF2F( NDF, FILNAM, NOARR, ARRNAM, BITPIX, BLOCKF,
     :                      ORIGIN, PROFIT, DUPLEX, PROEXT, PROHIS,
     :                      PROPROV, PROVEN, SUMS, ENCOD, NATIVE, FOPEN,
     :                      FCLOSE, USEAXS, ALWTAB, AXORD, STATUS )
*+
*  Name:
*     COF_NDF2F

*  Purpose:
*     Converts an NDF into a FITS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_NDF2F( NDF, FILNAM, NOARR, ARRNAM, BITPIX, BLOCKF,
*                     ORIGIN, PROFIT, DUPLEX, PROEXT, PROHIS, PROPROV,
*                     PROVEN, SUMS, ENCOD, NATIVE, USEAXS, ALWTAB, AXORD,
*                     STATUS )

*  Description:
*     This routine converts an NDF into a FITS file.  It uses as much
*     standard NDF information as possible to define the headers.  See
*     The Notes for the details.  Multiple NDF container files may be
*     converted.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF to be converted to a FITS file.
*     FILNAM = CHARACTER * ( * ) (Given)
*        The name of the output FITS file.
*     NOARR = INTEGER (Given)
*        The number of NDF arrays to copy.  Where there are no arrays,
*        just a header-only NDF is to be written to the
*        current header and data unit of the FITS file, do not set this
*        to zero, as it is used for the adjustable array ARRNAM; instead
*        specify NOARR=1 and and set ARRNAM to 'HEADER'.
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
*        A blank value gives a default of "Starlink Project, U.K.".
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
*     PROEXT = LOGICAL (Given)
*        If .TRUE., the NDF extensions (other than the FITS extension)
*        are propagated to the FITS files as FITS binary-table
*        extensions, one per structure of the hierarchy.
*     PROHIS = LOGICAL (Given)
*        If .TRUE., any NDF history records are written to the primary
*        FITS header as HISTORY cards.  These follow the mandatory
*        headers and any merged FITS-extension headers (see PROFIT).
*     PROPROV = LOGICAL (Given)
*        If not .TRUE., exclude PROVENANCE from extensions being propagated.
*     PROVEN = CHARACTER * ( * ) (Given)
*        This controls the export of NDF provenance information to the
*        FITS file.  Allowed values are as follows.
*
*        "NONE" -- No provenance is written.
*
*        "CADC" -- The CADC headers are written.  These record the
*          number and paths of both the direct parents of the NDF being
*          converted, and its root ancestors (the ones without parents).
*          Also modify the PRODUCT keyword to be unique for each FITS
*          extension.
*
*        "GENERIC" -- Encapsulates the entire PROVENANCE structure in
*          FITS headers in sets of five character-value indexed headers.
*          There is a set for the current NDF and each parent.
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
*     FOPEN = LOGICAL (Given)
*        If TRUE a new FITS file should be opened.
*     FCLOSE = LOGICAL (Given)
*        If TRUE a the open FITS file should be closed.
*     USEAXS = CHARACTER * ( * ) (Given)
*        Whether or not to export AXIS co-ordinates to an alternate
*        world co-ordinate representation in the FITS headers.  Such an
*        alternate may require a FITS extension to store lookup tables
*        of co-ordinates using the -TAB projection type.  The allowed
*        options are as follows.
*
*        "CHECK" --- requests no AXIS information be stored unless the
*                    current NDF contains AXIS information but no WCS.
*        "YES"   --- May create an alternate world co-ordinate
*                    representation irrespective of how the current
*                    NDF stores co-ordinate information.
*        "NO"    --- Must not create an alternate world co-ordinate
*                    representation in the current NDF.
*     ALWTAB = LOGICAL (Given)
*        If TRUE,then WCS co-ordinates in tabular form may be written
*        using the TAB algorithm as defined in FITS WCS Paper III.
*     AXORD = CHARACTER * ( * ) (Given)
*        The string defining the ordering of WCS in the FITS file. See
*        the AST FitsAxisOrder attribute.
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
*          the NDF WCS component if possible (i.e. exists and maps to
*          a FITS WCS projection type).  If this is not possible, and
*          if argument PROFIT is .TRUE., then it copies the headers of
*          a valid WCS specified in the NDF's FITS airlock.  Should
*          that attempt fail, the last resort tries the NDF AXIS
*          component, if it exists.  If its co-ordinates are non-linear,
*          the AXIS co-ordinates may be exported in a -TAB sub-file
*          subject to the value of argument USEAXS.
*        OBJECT, LABEL, BUNIT --- the values held in the NDF's title,
*          label, and units components respectively are used if
*          they are defined; otherwise any values found in the FITS
*          extension are used (provided parameter PROFIT is .TRUE.).
*        DATE --- is created automatically.
*        ORIGIN --- inherits any existing ORIGIN card in the NDF FITS
*          extension, unless you supply a value through argument
*          ORIGIN other than the default "Starlink Project, U.K." or
*          a blank string.
*        EXTNAME --- is the component name of the object from the COMP
*          argument when the EXTNAME appears in the primary header or
*          an IMAGE extension.  In a binary-table derived from an NDF
*          extension, EXTNAME is the path of the extension within the
*          NDF.  If the component is too long to fit within the header
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
*        EXTTYPE --- is the data type of the NDF extension used to
*          create a binary table.
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
*        DATASUM and CHECKSUM --- are written afresh when argument
*          SUMS is TRUE, otherwise any existing values inherited from
*          the FITS airlock are removed.
*
*     -  The following PROVENANCE headers are written if argument PROVEN
*     is set to "GENERIC".
*        PRVPn --- is the path of the nth NDF.
*        PRVIn --- is a comma-separated list of the identifiers of the
*          direct parents for the nth ancestor.
*        PRVDn --- is the creation date in ISO order of the $n$th
*          ancestor.
*        PRVCn --- is the software used to create the nth ancestor.
*        PRVMn --- lists the contents of the MORE structure of the nth
*          parent.
*     All have value '<unknown>' if the information could not be found,
*     except for the PRVMn header, which is omitted if there is no MORE
*     information to record.
*
*     The following PROVENANCE headers are written if argument PROVEN
*     is set to "CADC".
*        PRVCNT --- is the number of immediate parents.
*        PRVm --- is name of the mth immediate parent.
*        OBSCNT --- is the number of root ancestor OBSm headers.
*        OBSm --- is mth root ancestor identifier from its
*          MORE.OBSIDSS component.
*        FILEID --- is the name of the output FITS file, omitting any
*          file extension.
*
*        PRODUCT is modified or added to each extension's header to
*        be the primary header's value of PRODUCT with a '_<extnam>'
*        suffix, where <extnam> is the extension name in lowercase.
*
*     -  There are additional rules if a multi-NDF container file is
*     being converted.  This excludes the case where there are but two
*     NDFs---one data and the other just headers---that have already
*     been merged:
*     -  For multiple NDFs a header-only HDU may be created followed by
*     an IMAGE extension containing the data array (or whichever other
*     array is first specified by COMP).
*     -  BITPIX for the header HDU is set to an arbitrary 8.
*     -  Additional keywords are written for each IMAGE extension.
*        HDSNAME is the NDF name for a component NDF in a multi-NDF
*          container file, for example I2.
*        HDSTYPE is set to "NDF" for a component NDF in a multi-NDF
*          container file.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995-2000, 2003-2004 Central Laboratory of the
*     Research Councils. Copyright (C) 2006 Particle Physics &
*     Astronomy Research Council. Copyright (C) 2007-2009, 2011-2013
*     Science & Technology Facilities Council. All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     JAB: Jeremy Bailey (AAO)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MNB: Mike N Birchall (AAO)
*     {enter_new_authors_here}

*  History:
*     1994 May 31 (MJC):
*        Original version.
*     1995 November 22 (MJC):
*        Some bug fixes and improvements.
*     1996 September 11 (MJC):
*        Simplified the error message when the FITS file exists.
*     1996 December 13 (MJC):
*        Released the FIO file unit number, and used a buffer for some
*        concatenated error messages."
*     1997 January 13 (MJC):
*        Added PROHIS argument and calls to enable propagation of
*        history information from the HISTORY component to the FITS
*        headers.  Checked for 2dF extensions, and created special
*        binary tables for these.
*     1997 March 16 (MJC):
*        Allowed BITPIX argument to select the NDF's FITS extension
*        BITPIX value to be propagated.
*     1997 August 17 (MJC):
*        Initialised FITSIO status.
*     1997 November 7 (MJC):
*        Changed the data type of the 2dF fibres extension.
*     1997 December 2 (MJC):
*        Initialised the second FITSIO status.
*     18-DEC-1997 (DSB):
*        Added AST encoding arguments.
*     1998 January 5 (MJC):
*        Use ORIGIN argument as intended.
*     1998 April 21 (JAB):
*        Changes to handling of UWORD format NDF's to allow values to be
*        scaled correctly and avoid overflow errors.
*     1998 April 22 (MJC):
*        Rewrote much of the section which determines the block-floating
*        point conversion and the blank value, paying special note to
*        _BYTE and _UWORD conversions.
*     9-NOV-1998 (DSB):
*        Replaced arguments NENCOD and ENCODS by NATIVE.
*     22-JUN-1999 (DSB):
*        Added ENCOD argument.
*     11-APR-2000 (DSB):
*        Updated ENCOD argument description.
*     21-MAR-2003 (DSB):
*        Annull error caused by all input pixels being BAD, in order to
*        create a FITS file containing all blank pixels.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2006 April 5 (MJC):
*        Added FOPEN and FCLOSE arguments and their functionality.
*        When FOPEN is FALSE, create a new IMAGE extension.  Detected
*        header-only NDFs through ARRNAM = 'HEADER'.
*     2006 April 7 (MJC):
*        Insert HDSNAME and HDSTYPE keywords for multi-NDF conversions.
*        Document the multi-NDF rules.
*     2006 April 13 (MJC):
*        Remove unused variables.
*     6-JUN-2006 (DSB):
*        Guard against CHR_FIND not finding a dot.
*     2006 June 15 (MNB):
*        Added that AAO fibre-table conversion to occur for NDF
*        extension with name "FIBRES_IFU" as well as "FIBRES".
*     2006 October 24 (MJC):
*        Moved code for HDSNAME and HDSTYPE for multi-NDF containers
*        to COF_WHEAD where any existing keywords in the airlock with
*        those names are filtered.  Corrected the logic for computing
*        BPOUTU when the supplied BITPIX argument is -1 and the airlock
*        value thus accessed is negative.
*     2007 January 5 (MJC):
*        Allowed propagation of scaled arrays, selected if BITPIX set to
*        the new special value of 1.
*     2007 July 6 (MJC):
*        Write CHECKSUM and DATASUM headers if new argument SUMS is
*        .TRUE.
*     2007 October 19 (MJC):
*        Add DUPLEX argument.
*     2007 October 25 (MJC):
*        Implement the revised COF_WHEAD API.
*     2007 October 26 (MJC):
*        Implement the further revised COF_WHEAD API.
*     2008 January 8 (MJC):
*        Add PROVEN argument and invoke its CADC option.
*     2008 February 6 (MJC):
*        Invoke and document PROVEN argument's GENERIC option.
*     2008 October 9 (MJC):
*        Update documentation on the CADC provenance.
*     2009 October 16 (MJC):
*        Disable automatic quality masking if the conversions includes
*        the QUALITY component along with either DATA or VARIANCE
*        array.
*     2011 January 12 (MJC):
*        Use KPG_TYPSZ instead of COF_TYPSZ.
*     2011 February 24 (MJC):
*        Add USEAXS argument.
*     2012 April 30 (MJC):
*        Add _INT64 support.
*     2012 May 29 (MJC):
*        Add scaling using 64-bit integers.
*     2012 June 2 (MJC):
*        Need to treat TYPE=_REAL -> BITPIX=64 as a special case.
*        Hitherto all the scaled integer output data type used fewer
*        bits than the floating-point values.  For 64-bit integers
*        this breaks down.
*     2013 November 15 (MJC):
*        Add ALWTAB argument and pass it to other routines now using
*        it.
*     19-NOV-2013 (DSB):
*        Re-structured to use CVG provenance writing routines.
*     9-JUL-2014 (DSB):
*        Added argument AXORD.
*     23-JUN-2017 (GSB):
*        Added argument PROPROV.
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
      INTEGER NDF
      CHARACTER * ( * ) FILNAM
      INTEGER NOARR
      CHARACTER * ( * ) ARRNAM( NOARR )
      INTEGER BITPIX
      INTEGER BLOCKF
      CHARACTER * ( * ) ORIGIN
      LOGICAL PROFIT
      LOGICAL DUPLEX
      LOGICAL PROEXT
      LOGICAL PROHIS
      LOGICAL PROPROV
      CHARACTER * ( * ) PROVEN
      LOGICAL SUMS
      CHARACTER * ( * ) ENCOD
      LOGICAL NATIVE
      LOGICAL FOPEN
      LOGICAL FCLOSE
      CHARACTER * ( * ) USEAXS
      LOGICAL ALWTAB
      CHARACTER * ( * ) AXORD

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
      CHARACTER*200  BUFFER      ! Buffer for error messages
      DOUBLE PRECISION BZERO     ! Block-integer offset
      CHARACTER*48 COMENT        ! Comment from FITS-extension header
      DOUBLE PRECISION DELTA     ! Machine precision for scaling
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      LOGICAL E2DF               ! Extension is from 2dF?
      INTEGER EL                 ! Number of elements in array
      LOGICAL FEXIST             ! FITS already exists?
      LOGICAL FITPRE             ! FITS airlock extension is present?
      CHARACTER*( NDF__SZFRM ) FORM ! Storage form
      INTEGER FSTAT              ! FITSIO error status
      INTEGER FSTATC             ! FITSIO error status for file closure
      INTEGER FUNIT              ! Fortran I/O unit for the FITS file
      LOGICAL HDONLY             ! NDF only contains metadata?
      LOGICAL HISPRE             ! HISTORY records present?
      INTEGER I                  ! Loop counter
      INTEGER ICOMP              ! Loop counter
      INTEGER IDEL               ! Increment to reduce an integer
                                 ! scaling range
      INTEGER IPNTR              ! Pointer to input array
      INTEGER IPROV              ! Identifier for provenance structure
      INTEGER*8 KBLANK           ! Data blank for 64-bit integer arrays
      INTEGER*8 KDEL             ! Increment to reduce a 64-bit integer
                                 ! scaling range
      DOUBLE PRECISION MAXV      ! Max. value to appear in scaled array
      DOUBLE PRECISION MINV      ! Min. value to appear in scaled array
      LOGICAL MULTIN             ! Multi-NDF container?
      INTEGER NBYTES             ! Number of bytes per array value
      INTEGER NC                 ! Number of character in string
      INTEGER NCF                ! Number of characters in filename
      INTEGER NDECIM             ! Number of decimal places in header
                                 ! value
      INTEGER NDIM               ! Number of dimensions
      INTEGER NEXTN              ! Number of extensions
      INTEGER NEX2PR             ! Number of extensions to process
      LOGICAL NSCALE             ! Array component is scaled in the NDF?
      BYTE NULL8                 ! Null value for BITPIX=8
      INTEGER*2 NULL16           ! Null value for BITPIX=16
      INTEGER NULL32             ! Null value for BITPIX=32
      INTEGER*8 NULL64           ! Null value for BITPIX=64
      REAL NUL_32                ! Null value for BITPIX=-32
      DOUBLE PRECISION NUL_64    ! Null value for BITPIX=-64
      LOGICAL OPEN               ! FITS file exists?
      LOGICAL PROPEX             ! Propagate FITS extension for the
                                 ! current header?
      LOGICAL PRVPRS             ! PROVENANCE present?
      CHARACTER*6 ROUTIN         ! FITSIO routine name for error report
      INTEGER SBYTES             ! No. of bytes per scaled array value
      LOGICAL SCALE              ! The array is to be scaled?
      CHARACTER*( DAT__SZTYP ) SCTYPE ! Data type for scaled arrays
      LOGICAL SHIFT              ! A BZERO offset is required?
      LOGICAL SMURF              ! Extension is from SMURF package?
      LOGICAL UNWNTD             ! Extension is unwatned
      LOGICAL THERE              ! BITPIX FITS header card ! is present?
      CHARACTER*( NDF__SZTYP ) TYPE ! NDF array's data type
      LOGICAL VALID              ! The NDF identifier is valid?
      CHARACTER*( DAT__SZLOC ) XLOC ! Locator to an NDF extension
      CHARACTER*( NDF__SZXNM ) XNAME ! Name of NDF extension
      CHARACTER*( DAT__SZTYP ) XTYPE ! Name of NDF extension

*  Save persistent variables.
      SAVE FUNIT, NCF

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Open the FITS file.
*  ===================

*  Find a free logical-unit.
      IF ( FOPEN ) THEN
         CALL FIO_GUNIT( FUNIT, STATUS )

*  Open the FITS file.
         CALL FTINIT( FUNIT, FILNAM, BLOCKF, FSTAT )

*  Get the length of the filename.
         NCF = CHR_LEN( FILNAM )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.  To simplify the error message, test to see if the file
*  exists, and if it does make a shorter report (note that the status
*  must be set bad too) and clear the FITSIO error-message stack.
*  Record whether the file was actually opened or not.
         IF ( FSTAT .GT. FITSOK ) THEN
            INQUIRE( FILE=FILNAM, EXIST=FEXIST )
            IF ( FEXIST ) THEN
               STATUS = SAI__ERROR
               BUFFER = 'Error creating the output FITS file '/
     :                  /FILNAM( :NCF )//' because it already exists.'
               CALL MSG_SETC( 'BUF', BUFFER )
               CALL ERR_REP( 'COF_NDF2F_FILEEXIST', '^BUF', STATUS )
               CALL FTCMSG

            ELSE
               BUFFER = 'Error creating the output FITS file '/
     :                  /FILNAM( :NCF )//'.'
               CALL CVG_FIOER( FSTAT, 'COF_NDF2F_OPENERR', 'FTINIT',
     :                         BUFFER, STATUS )
            END IF
            OPEN = .FALSE.
            GOTO 999
         ELSE
            OPEN = .TRUE.
         END IF
      ELSE

*  Move to the next HDU.
         OPEN = .TRUE.
      END IF

*  Validate the NDF identifier.
*  ============================
      CALL NDF_VALID( NDF, VALID, STATUS )

*  Report an error if the identifier is not valid.
      IF ( .NOT. VALID ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_NDF2F_INVNDF',
     :     'COF_NDF2F: The identifier to the input NDF is invalid. '/
     :     /'(Probable programming error.)', STATUS )
         GOTO 999
      END IF

*  Inquire the shape of the NDF.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  See if this is a header-only NDF.
      HDONLY = ARRNAM( 1 ) .EQ. 'HEADER'

*  Take a local copy of the supplied BITPIX in case in needs to be
*  modified.
      BP = BITPIX

*  Disable Quality masking.
*  ========================

*  When a QUALITY array component is being exported, its information
*  should not be used twice to mask bad pixels in the DATA or VARIANCE
*  components.  There must be more than one array being propagated,
*  because if it were DATA or VARIANCE, the automatic masking should be
*  applied, and if it were QUALITY there is no self masking.  Therefore
*  disable automatic quality masking in these cases.
      IF ( NOARR .GT. 1. AND. ( ARRNAM( 1 ) .EQ. 'DATA' .OR.
     :                          ARRNAM( 1 ) .EQ. 'VARIANCE' ) ) THEN
         DO I = 2, NOARR
            IF ( ARRNAM( I ) .EQ. 'QUALITY' ) THEN
               CALL NDF_SQMF( .FALSE., NDF, STATUS )
            END IF
         END DO
      END IF

*  Loop for each array component.
*  ==============================
      DO ICOMP = 1, NOARR

         NSCALE = .FALSE.
         IF ( .NOT. HDONLY ) THEN

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
     :                  ARRNAM( ICOMP ) .NE. 'QUALITY'

               IF ( NSCALE ) THEN
                  CALL NDF_SCTYP( NDF, ARRNAM( ICOMP ), SCTYPE, STATUS )
                  CALL NDF_GTSZD( NDF, ARRNAM( ICOMP ), BSCALE, BZERO,
     :                            STATUS )

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
               CALL CON_EKEYI( NDF, 'BITPIX', 1, THERE, BPOUT, COMENT,
     :                         STATUS )

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

*  This is arbitrary.
         ELSE
            BPOUT = 8
         END IF

*  Allow for the sign of floating-point BITPIX values to form the
*  unsigned BITPIX.
         IF ( BPOUTU .EQ. -32 .OR. BPOUTU .EQ. -64 ) BPOUTU = 1 - BPOUTU

*  Open a new header and data unit for extensions.
*  ===============================================

*  A new HDU is created when the file is opened, or when the file
*  was left open for another NDF, so here a new header need only
*  be opened for extensions.
         IF ( ICOMP .GT. 1 .OR. .NOT. FOPEN ) THEN
            CALL FTCRHD( FUNIT, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL CVG_FIOER( FSTAT, 'COF_NDF2F_NHDU', 'FTCRHD',
     :           'Error creating the header and data unit for an '/
     :           /'IMAGE extension.', STATUS )
               GOTO 999
            END IF

         END IF

*  Write the header.
*  =================
*
*  Decide whether or not to propagate the FITS extension.  It will
*  only appear in the primary array's header, if requested.
         PROPEX = ( PROFIT .AND. ( ICOMP .EQ. 1 .OR. DUPLEX ) ) .OR.
     :            HDONLY

*  Is this a multi-NDF container?
         MULTIN = .NOT. ( HDONLY .AND. FOPEN )

*  First write the standard headers, and merge in the FITS extension
*  when requested to do so.
         CALL COF_WHEAD( NDF, NDF, ARRNAM( ICOMP ), FUNIT, BPOUT,
     :                   PROPEX, ORIGIN, ENCOD, NATIVE, MULTIN, ' ',
     :                   USEAXS, ALWTAB, AXORD, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine whether or not there are history records in the NDF.
*  Append the history records for the first array.
         IF ( PROHIS .AND. ICOMP .EQ. 1 ) THEN
            CALL NDF_STATE( NDF, 'History', HISPRE, STATUS )
            IF ( HISPRE ) CALL CVG_WHISR( NDF, FUNIT, STATUS )
         END IF

*  Write provenance headers. First check that there is provenance
*  to record.
         CALL NDF_XSTAT( NDF, 'PROVENANCE', PRVPRS, STATUS )
         IF ( PRVPRS ) THEN

*  Get an identifier for a structure holding provenance info in the NDF.
            CALL NDG_READPROV( NDF, ' ', IPROV, STATUS )

*  Now write the records in the required format.
            IF ( PROVEN .EQ. 'CADC' ) THEN
               CALL CVG_PCADC( IPROV, FUNIT, STATUS )

            ELSE IF ( PROVEN .EQ. 'GENERIC' ) THEN
               CALL CVG_WPROV( IPROV, FUNIT, STATUS )

            END IF

*  Free the provenance structure.
            CALL NDG_FREEPROV( IPROV, STATUS )
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
         IF ( .NOT. HDONLY ) THEN

*  Set the defaults.
            SCALE = .FALSE.
            SHIFT = .FALSE.
            IF ( ABS( BPOUT ) .EQ. 64 ) THEN
               DELTA = VAL__EPSD
            ELSE
               DELTA = DBLE( VAL__EPSR )
            END IF

*  We already know the scaling coefficients for a scaled array.
            IF ( .NOT. NSCALE ) THEN
               BSCALE = 1.0D0
               BZERO = 0.0D0
            END IF

*  Set the null values.  Only one will be needed, depending on the
*  value of BPOUT, but it as efficient to assign them all.
            NULL32 = VAL__BADI
            NULL64 = VAL__BADK
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

*  Determine whether or not scaling is required.  In most cases,
*  we can compare the component's BITPIX with that supplied for the
*  output FITS file.  However, there is one exception, converting
*  a _REAL array to 64-bit integer.
            ELSE IF ( BPOUTU .LT. BPINU .OR.
     :                ( BPOUT .EQ. 64 .AND. BPIN .EQ. -32 ) ) THEN

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

               ELSE IF ( BPOUT .EQ. 64 ) THEN
                  KBLANK = VAL__BADK

               END IF

*  Deal with the types where no scaling or offset is required.
            ELSE

*  Integer types will have a blank value.  These are based upon the type
*  of the NDF array.  The FITSIO routine will perform any type
*  conversion required.
               IF ( TYPE .EQ. '_INTEGER' ) THEN
                  BLANK = VAL__BADI

               ELSE IF ( TYPE .EQ. '_INT64' ) THEN
                  KBLANK = VAL__BADK

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
     :           ARRNAM( ICOMP ) .NE. 'QUALITY' ) THEN

*  The header should already contain a BLANK keyword.
*  Reset the BLANK keyword in the header.  Ampersand instructs the
*  routine not to modify the comment of the BLANK header card.
               IF ( BPOUT .EQ. 64 ) THEN
                  CALL FTMKYK( FUNIT, 'BLANK', KBLANK, '&', FSTAT )
                  ROUTIN = 'FTMKYK'
               ELSE
                  CALL FTMKYJ( FUNIT, 'BLANK', BLANK, '&', FSTAT )
                  ROUTIN = 'FTMKYJ'
               END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
               IF ( FSTAT .GT. FITSOK ) THEN
                  CALL CVG_FIOER( FSTAT, 'COF_NDF2F_BLANK1', ROUTIN,
     :              'Error modifying the BLANK header card.', STATUS )
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
     :                       EL, STATUS )

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
     :                        INT( DBLE( VAL__MINI ) * DELTA ) ) + 1
                  MAXV = DBLE( VAL__MAXI - SIGN( IDEL, VAL__MAXI ) )
                  MINV = DBLE( VAL__MINI - SIGN( IDEL, VAL__MINI ) )

               ELSE IF ( BPOUT .EQ. 64 ) THEN
                  KDEL = MAX( INT( DBLE( VAL__MAXK ) * DELTA ) ,
     :                        INT( DBLE( VAL__MINK ) * DELTA ) ) + 1
                  MAXV = DBLE( VAL__MAXK - SIGN( KDEL, VAL__MAXK ) )
                  MINV = DBLE( VAL__MINK - SIGN( KDEL, VAL__MINK ) )

               ELSE IF ( BPOUT .EQ. -32 ) THEN
                  MAXV = DBLE( VAL__MAXR ) * ( 1.0D0 - DELTA )
                  MINV = DBLE( VAL__MINR ) * ( 1.0D0 - DELTA )

               ELSE IF ( BPOUT .EQ. -64 ) THEN
                  MAXV = VAL__MAXD * ( 1.0D0 - DELTA )
                  MINV = VAL__MIND * ( 1.0D0 - DELTA )

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
     :                            MINV, MAXV, BSCALE, BZERO, STATUS )

               ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
                  CALL COF_ESCOI( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                            MINV, MAXV, BSCALE, BZERO, STATUS )

               ELSE IF ( TYPE .EQ. '_REAL' ) THEN
                  CALL COF_ESCOR( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                            MINV, MAXV, BSCALE, BZERO, STATUS )

               ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
                  CALL COF_ESCOD( BAD, EL, %VAL( CNF_PVAL( IPNTR ) ),
     :                            MINV, MAXV, BSCALE, BZERO, STATUS )

               END IF

*  An error (SAI__ERROR) will have been reported if all the pixel values
*  were bad.  In this case, we annul the error and use default BSCALE
*  and BZERO values (the resulting NDF will be filled with BLANK
*  values).
               IF ( STATUS .EQ. SAI__ERROR .AND. BAD ) THEN
                  CALL ERR_ANNUL( STATUS )
                  BSCALE = 1.0D0
                  BZERO = 0.0D0
               END IF

            ELSE

*  No scaling required.
*  ====================
*
*  Any type conversion will be performed by the FITSIO array-writing
*  routine.
               CALL NDF_MAP( NDF, ARRNAM( ICOMP ), TYPE, 'READ', IPNTR,
     :                       EL, STATUS )
            END IF
            IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Revise the scale and zero cards.
*  ================================
            IF ( SCALE .OR. SHIFT .OR. NSCALE ) THEN

*  Decide the appropriate number of decimals needed to represent the
*  block floating point scale and offset.
               IF ( ( TYPE .EQ. '_DOUBLE'  .AND. .NOT. NSCALE ) .OR.
     :              SCTYPE .EQ. '_DOUBLE' ) THEN
                  NDECIM = INT( -LOG10( VAL__EPSD ) )
               ELSE
                  NDECIM = INT( -LOG10( VAL__EPSR ) )
               END IF

*  Reset the BSCALE keyword in the header. Ampersand instructs the
*  routine not to modify the comment of the BSCALE header card.
               CALL FTMKYD( FUNIT, 'BSCALE', BSCALE, NDECIM, '&',
     :                      FSTAT )

*  Similarly for the BZERO card.
               CALL FTMKYD( FUNIT, 'BZERO', BZERO, NDECIM, '&', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
               IF ( FSTAT .GT. FITSOK ) THEN
                  CALL CVG_FIOER( FSTAT, 'COF_NDF2F_HSCOF', 'FTMKYD',
     :              'Error modifying the BSCALE or BZERO header card.',
     :              STATUS )
                  GOTO 999
               END IF
            END IF

*  Set the data scaling and offset.
*  ================================
            CALL FTPSCL( FUNIT, BSCALE, BZERO, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL CVG_FIOER( FSTAT, 'COF_NDF2F_SCOF', 'FTPSCL',
     :           'Error defining the scale and offset.', STATUS )
               GOTO 999
            END IF

*  Set the blank data value.
*  =========================

*  Only required for the integer types.  BLANK has no meaning for
*  floating-point in FITS.  Note that this moust be done after the call
*  to FTPDEF, and so cannot be done when the header value is modified.
            IF ( BPOUT .GT. 0 ) THEN

*  Set the data blank value.
               IF ( BPOUT .EQ. 64 ) THEN
                  CALL FTPNULLL( FUNIT, KBLANK, FSTAT )
               ELSE
                  CALL FTPNUL( FUNIT, BLANK, FSTAT )
               END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
               IF ( FSTAT .GT. FITSOK ) THEN
                  CALL CVG_FIOER( FSTAT, 'COF_NDF2F_BLANK2', 'FTPNUL',
     :              'Error modifying the BLANK value.', STATUS )
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
                  CALL FTPPNB( FUNIT, 0, 1, EL,
     :                         %VAL( CNF_PVAL( IPNTR ) ), NULL8, FSTAT )

               ELSE IF ( BPIN .EQ. 16 ) THEN
                  CALL FTPPNI( FUNIT, 0, 1, EL,
     :                         %VAL( CNF_PVAL( IPNTR ) ), NULL16,
     :                         FSTAT )

               ELSE IF ( BPIN .EQ. 32 ) THEN
                  CALL FTPPNJ( FUNIT, 0, 1, EL,
     :                         %VAL( CNF_PVAL( IPNTR ) ), NULL32,
     :                         FSTAT )

               ELSE IF ( BPIN .EQ. 64 ) THEN
                  CALL FTPPNK( FUNIT, 0, 1, EL,
     :                         %VAL( CNF_PVAL( IPNTR ) ), NULL64,
     :                         FSTAT )

               ELSE IF ( BPIN .EQ. -32 ) THEN
                   CALL FTPPNE( FUNIT, 0, 1, EL,
     :                          %VAL( CNF_PVAL( IPNTR ) ), NUL_32,
     :                          FSTAT )

               ELSE IF ( BPIN .EQ. -64 ) THEN
                  CALL FTPPND( FUNIT, 0, 1, EL,
     :                         %VAL( CNF_PVAL( IPNTR ) ), NUL_64,
     :                         FSTAT )
               END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
               IF ( FSTAT .GT. FITSOK ) THEN
                  NC = CHR_LEN( ARRNAM( ICOMP ) )
                  BUFFER = 'Error writing '//ARRNAM( ICOMP )( :NC )/
     :                     /' array component to FITS file '/
     :                     /FILNAM( :NCF )//'.'
                  CALL CVG_FIOER( FSTAT, 'COF_NDF2F_WRDATAERR',
     :                            'FTPPNx', BUFFER, STATUS )
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
     :                         %VAL( CNF_PVAL( IPNTR ) ), FSTAT )

               ELSE IF ( BPIN .EQ. 16 ) THEN
                  CALL FTPPRI( FUNIT, 0, 1, EL,
     :                         %VAL( CNF_PVAL( IPNTR ) ), FSTAT )

               ELSE IF ( BPIN .EQ. 32 ) THEN
                  CALL FTPPRJ( FUNIT, 0, 1, EL,
     :                         %VAL( CNF_PVAL( IPNTR ) ), FSTAT )

               ELSE IF ( BPIN .EQ. 64 ) THEN
                  CALL FTPPRK( FUNIT, 0, 1, EL,
     :                         %VAL( CNF_PVAL( IPNTR ) ), FSTAT )

               ELSE IF ( BPIN .EQ. -32 ) THEN
                  CALL FTPPRE( FUNIT, 0, 1, EL,
     :                         %VAL( CNF_PVAL( IPNTR ) ), FSTAT )

               ELSE IF ( BPIN .EQ. -64 ) THEN
                  CALL FTPPRD( FUNIT, 0, 1, EL,
     :                         %VAL( CNF_PVAL( IPNTR ) ), FSTAT )

               END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
               IF ( FSTAT .GT. FITSOK ) THEN
                  NC = CHR_LEN( ARRNAM( ICOMP ) )
                  BUFFER = 'Error writing '//ARRNAM( ICOMP )( :NC )/
     :                     /' array component to FITS file '/
     :                     /FILNAM( :NCF )//'.'
                  CALL CVG_FIOER( FSTAT, 'COF_NDF2F_WRDATAERR',
     :                            'FTPPRx', BUFFER, STATUS )
                  GOTO 999
               END IF

            END IF

*  Tidy the array.
*  ===============
*  Unmap the input array.
            CALL NDF_UNMAP( NDF, ARRNAM( ICOMP ), STATUS )

         END IF

*  Write integrity-check headers.  Note any existing values present
*  derived from the FITS airlock will be replaced.
         IF ( SUMS ) THEN
            CALL FTPREC( FUNIT, ' ', STATUS )
            CALL FTPCKS( FUNIT, STATUS )
         END IF

      END DO

*  Process extensions.
*  ===================
      IF ( PROEXT ) THEN

*  Use binary tables for all extensions other than FITS.  Special
*  software for handling standard extensions will be provided as it
*  becomes available.

*  Look for NDF extensions.  Check whether or not there are any present.
         CALL NDF_XNUMB( NDF, NEXTN, STATUS )

         IF ( NEXTN .GE. 1 ) THEN

*  See if one of these is the FITS extension.
            CALL NDF_XSTAT( NDF, 'FITS', FITPRE, STATUS )

*  Find the number of extensions to process, as the FITS extension
*  is handled elsewhere.
            IF ( FITPRE ) THEN
               NEX2PR = NEXTN - 1
            ELSE
               NEX2PR = NEXTN
            END IF

*  Are there any extensions to process?
            IF ( NEX2PR .GE. 1 ) THEN

*  Loop through the extensions.
               DO I = 1, NEXTN

*  Get the name of the next extension.
                  CALL NDF_XNAME( NDF, I, XNAME, STATUS )

*  Get a locator to the extension.
                  CALL NDF_XLOC( NDF, XNAME, 'READ', XLOC, STATUS )

*  Obtain the data type of the extension.
                  CALL DAT_TYPE( XLOC, XTYPE, STATUS )

*  Define any special extensions.
                  E2DF = ( XNAME .EQ. 'FIBRES' .OR.
     :                   XNAME .EQ. 'FIBRES_IFU' ) .AND.
     :                   XTYPE .EQ. 'FIBRE_EXT'

*  The type was originally SMURF, but was changed 2009 September 19
*  to SMURF_EXT.  So we have to support both types.
                  SMURF = XTYPE .EQ. 'SMURF' .OR.
     :                    XTYPE .EQ. 'SMURF_EXT'

*  Is the extension otherwise unwanted?
                  UNWNTD = ( ( XTYPE .EQ. 'PROVENANCE' )
     :                       .AND. .NOT. PROPROV )

*  Skip over the FITS extension.
                  IF ( XNAME .NE. 'FITS' .AND. .NOT. E2DF .AND.
     :                 .NOT. SMURF .AND. .NOT. UNWNTD ) THEN

*  Process the extension into a hierarchy.
                     CALL COF_THIER( XNAME, XLOC, FUNIT, STATUS )

*  Handle 2dF special case separately.
                  ELSE IF ( E2DF ) THEN
                     CALL COF_2DFEX( XNAME, XLOC, FUNIT, STATUS )

                  ELSE IF ( SMURF ) THEN

*  Handle SMURF extension's NDFs as NDFs rather than a general HDS
*  structure, but also may inherit the metadata (FITS airlock, HISTORY)
*  of the parent NDFs.
                     CALL COF_SMURF( XNAME, XLOC, FUNIT, NDF, FILNAM,
     :                               NOARR, ARRNAM, BITPIX, BLOCKF,
     :                               ORIGIN, PROFIT, DUPLEX, PROEXT,
     :                               PROHIS, PROPROV, SUMS, ENCOD,
     :                               NATIVE, USEAXS, ALWTAB, AXORD,
     :                               STATUS )
                  END IF

*  Write integrity-check headers.
                  IF ( SUMS ) CALL FTPCKS( FUNIT, STATUS )

*  Annul the locator so it may be reused.
                  CALL DAT_ANNUL( XLOC, STATUS )
               END DO
            END IF
         END IF

      END IF

  999 CONTINUE

*  Close the FITS file.  Use another status to ensure that the file is
*  closed even if there has been an earlier FITSIO error.
      IF ( FCLOSE ) THEN
         IF ( OPEN ) THEN
            FSTATC = FITSOK
            CALL FTCLOS( FUNIT, FSTATC )
            IF ( FSTATC .GT. FITSOK ) THEN
               BUFFER = 'Error closing the FITS file '/
     :                   /FILNAM( :NCF )//'.'
               CALL CVG_FIOER( FSTATC, 'COF_NDF2F_CLOSE', 'FTCLOS',
     :                         BUFFER, STATUS )
            END IF
         END IF

*  Release the logical-unit.
         CALL FIO_PUNIT( FUNIT, STATUS )
      END IF

      END
