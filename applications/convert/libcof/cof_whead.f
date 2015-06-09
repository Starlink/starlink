      SUBROUTINE COF_WHEAD( NDFI, NDFFAI, COMP, FUNIT, BITPIX, PROPEX,
     :                      ORIGIN, ENCOD, NATIVE, MULTI, EXTNAM,
     :                      USEAXS, ALWTAB, AXORD, STATUS )
*+
*  Name:
*     COF_WHEAD

*  Purpose:
*     Writes the FITS header derived from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_WHEAD( NDFI, NDFFAI, COMP, FUNIT, BITPIX, PROPEX,
*                     ORIGIN, ENCOD, NATIVE, MULTI, EXTNAM, USEAXS,
*                     ALWTAB, AXORD, STATUS )

*  Description:
*     This routine creates the header section of the primary array or
*     IMAGE extension of an output FITS file based upon information in
*     an NDF.  The IMAGE extension is written when the current header
*     and data unit (CHDU) is not the first.

*     There are three stages.
*     a) Inquire of the NDF its shape, type, character components,
*     and axis components; and write these to the header.
*     b) Look for a FITS extension if requested to do so.  If one is
*     present append the headers contained therein to the FITS header
*     section, but not replacing any of the headers created in Stage a).
*     c) Creates keywords describing the NDF's WCS component.  These
*     overwrite any keywords created in Stages a) and b).

*  Arguments:
*     NDFI = INTEGER (Given)
*        The identifier of the NDF.  Ths is used to access those
*        characteristics such as shape and the character components that
*        are stored outside of the FITS airlock.
*     NDFFAI = INTEGER (Given)
*        The identifier of another NDF whose FITS airlock is used.  In
*        normal circumstances this will be the same identifier as
*        argument NDFI.  However, when processing an extension NDF,
*        this argument may point to the primary NDF.
*     COMP = CHARACTER * ( * ) (Given)
*        The array component to write to the HDU.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     BITPIX = INTEGER (Given)
*        The BITPIX value for the output FITS file.
*     PROPEX = LOGICAL (Given)
*        The NDF FITS extension, if present, is folded into the output
*        FITS header when PROPEX is .TRUE..  When PROPEX is .FALSE.,
*        the FITS extension is ignored.
*     ORIGIN = CHARACTER * ( * ) (Given)
*        The value of the ORIGIN card.
*     ENCOD = CHARACTER * ( * ) (Given)
*        The encoding to use. If this is blank, then a default encoding
*        is chosen based on the contents of the FITS extension.  The
*        supplied string should be a recognised AST encoding such as
*        'DSS', 'FITS-WCS', 'NATIVE', etc (or a blank string).
*     NATIVE = LOGICAL (Given)
*        Include a NATIVE encoding of the WCS info in the header?
*     MULTI = LOGICAL (Given)
*        This should only be set .TRUE. when input NDF is part of a
*        multi-NDF container file,
*     EXTNAM = CHARACTER * ( * ) (Given)
*        If the input NDF is part of an extension, set this to the
*        extension's path (including its name) within the NDF
*        hierarchy.  A blank string indicates that the NDF being
*        processed in the primary NDF.  This argument affects the
*        values or presence of FITS keywords.  See the Notes
*        for EXTNAME, EXTLEVEL, and EXTTYPE keywords.
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
*        If TRUE, then WCS co-ordinates in tabular form may be written
*        using the TAB algorithm as defined in FITS WCS Paper III.
*     AXORD = CHARACTER * ( * ) (Given)
*        The string defining the ordering of WCS in the FITS file. See
*        the AST FitsAxisOrder attribute.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Here are details of the processing of standard items from the
*     NDF into the FITS header:
*        SIMPLE, EXTEND, PCOUNT, GCOUNT --- all take their default
*          values.
*        BITPIX, NAXIS, NAXISn --- are derived directly from the NDF
*          data array;
*        LBOUNDn --- the pixel lower bounds of an NDF whose origin is
*          not 1 along each axis.  (These are not part of the standard.)
*        CRVALn, CDELTn, CRPIXn, CTYPEn, CUNITn --- are derived from
*          the NDF WCS component if possible (i.e. exists and maps to
*          a FITS WCS projection type).  If this is not possible, and
*          if argument PROPEX is .TRUE., then it copies the headers of
*          a valid WCS specified in the NDF's FITS airlock.  Should
*          that attempt fail, the last resort tries the NDF AXIS
*          component, if it exists, but it only creates the headers
*          provided all of the axis centre co-ordinates are linear.
*          When any non-zero CROTAn card is present in the FITS
*          extension, the extension axis information is propagated, and
*          not that of the NDF AXIS component.  [This rule is to enable
*          rotated axis (not supported in the NDF) to be retained in the
*          cycle from FITS to NDF and back to FITS.]
*        OBJECT, LABEL, BUNIT --- the values held in NDF TITLE, LABEL,
*          and UNITS respectively are used if present, otherwise any
*          units found in the FITS extension are used.  For a variance
*          array, BUNIT is assigned to "(<unit>)**2", where
*          <unit> is the unit; for a quality array, the BUNIT header
*          is absent.
*        DATE --- is created automatically.
*        ORIGIN --- inherits any existing ORIGIN card in the NDF FITS
*          extension, unless you supply a value through argument
*          ORIGIN other than the default "Starlink Software" or
*          a blank string.
*        EXTNAME --- is the component name of the object from the COMP
*          argument, unless argument EXTNAM is not blank when keyword
*          EXTNAME is set to EXTNAM.  If the component is too long to
*          fit within the header (68 characters), EXTNAME is set to
*          '@EXTNAMEF'.  The full path is then stored in keyword
*          EXTNAMEF using the HEASARC Long-string CONTINUE convention
*          (http://fits.gsfc.nasa.gov/registry/continue_keyword.html).
*        EXTVER --- is only set when EXTNAME (q.v.) cannot accommodate
*          the component name and is assigned the HDU index to provide a
*          unique identifier.
*        EXTLEVEL --- when argument EXTNAM is not blank, this is the
*          level in the hierarchical structure of the extension.  A
*          top-level extension has value 1, sub-components of this
*          extension have value 2 and so on.
*        EXTTYPE --- when argument EXTNAM is not blank, this is set to
*          "NDF".
*        HDUCLAS1, HDUCLASn --- "NDF" and the value of COMP
*          respectively.
*        XTENSION, BSCALE, BZERO, BLANK and END --- are not propagated
*          from the extension.  The first will be set for any
*          extension.  BSCALE and BZERO will be defined based on the
*          chosen output data type in comparison with the NDF array's
*          type, but cards with values 1.0 and 0.0 respectively are
*          written to reserve places in the header section.  Likewise a
*          dummy BLANK card is written for the integer types.  The
*          BLANK value may be written as required, dependent on the
*          data type and any scaling.  These `reservation' cards are
*          for efficiency and they can always be deleted later.  The
*          END card is written by FITSIO automatically once the header
*          is closed.
*        HDSNAME is the NDF name for a component NDF in a multi-NDF
*          container file, for example I2.
*        HDSTYPE is set to "NDF" for a component NDF in a multi-NDF
*          container file.
*        DATASUM and CHECKSUM --- are removed to avoid spurious values
*          being stored.
*        PRVn, OBSn, and FILEID --- CADC provenance headers are removed.
*        PRVPn, PRVIn, PRVDn, PRVCn, PRVMn ---  provenance headers are
*          removed.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995-2000, 2006 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2008, 2009, 2011, 2013 Science & Technology
*     Facilities Council.
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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1994 May 31 (MJC):
*        Original version.
*     1995 November 16 (MJC):
*        Fixed a bug that could occur in some circumstances when the
*        a rare CROTAn card is present.
*     1996 February 8 (MJC):
*        Added checks not to propagate the FITSIO banner and Starlink
*        ORIGIN card in the NDF's FITS extension to the output FITS
*        file.
*     1996 September 16 (MJC):
*        Corrected usage of CTYPEn (was CRTYPEn) and introduced CUNITn
*        for axis units.
*     1997 November 14 (MJC):
*        Filtered LBOUNDn keywords.
*     18-DEC-1997 (DSB):
*        Propagation of NDF's WCS component added. The old code which
*        generated keywords CRVALn, CRPIXn etc, has been retained for
*        compatibility with old NDFs. Values generated from the WCS
*        component over-write any created by the old code.
*     1998 January 5 (MJC):
*        Added ORIGIN argument.  Inherits airlock ORIGIN card if
*        present unless ORIGIN argument is neither the default nor
*        blank.
*     1998 February 4 (MJC):
*        Now issues a warning meesage if an invalid header is located.
*        Substitutes space for null in headers.
*     9-NOV-1998 (DSB):
*        Replaced arguments NENCOD and ENCODS by NATIVE.
*     22-JUN-1999 (DSB):
*        Added ENCOD argument.
*     11-APR-2000 (DSB):
*        Updated description of ENCOD argument.
*     23-OCT-2000 (AJC):
*        Ignore all after END (could be garbage)
*     30-NOV-2000 (AJC):
*        Correctly omit airlock ORIGIN card from output if ORIGIN
*        argument is not default or blank.
*     2006 April 5 (MJC):
*        Allow for COMP='HEADER'.
*     2006 October 24 (MJC):
*        Added MULTI argument.  Moved code for HDSNAME and HDSTYPE from
*        COF_NDF2F and filtered any existing keywords with those names
*        in the FITS airlock.
*     2007 October 19 (MJC):
*        Revise documented BUNIT for VARIANCE and QUALITY.
*     2007 October 25 (MJC):
*        Add EXTNAM argument.
*     2007 October 26 (MJC):
*        Add NDFFAI argument.
*     2008 March 12 (MJC):
*        Remove CHECKSUM and DATASUM keywords, and CADC provenance
*        keywords.
*     2008 April 28 (MJC):
*        Do not exclude provenance keywords if the NDF does not have a
*        PROVENANCE extension.  Allow for latest FITS citation COMMENT
*        cards.
*     2008 May 20 (MJC):
*        Exclude CADC new FILEID provenance header.
*     10-JUN-2008 (DSB):
*        Do not propagate ASTWARN cards from FITS extension to output
*        header.
*     12-JUN-2008 (TIMJ):
*        Fix valgrind warning.
*     25-JUN-2009 (TIMJ):
*        Use HDS_SPLIT to find whether we have an NDF inside an HDS container.
*        This is more reliable than looking for a "." in the filename (since
*        sometimes directories have "."s in them).
*     2009 November 25 (MJC):
*        Allow for long extension names.
*     20-JAN-2011 (DSB):
*        Only write out the WCS component once.
*     10-FEB-2011 (DSB):
*        Write out the WCS component for each NDF array component. Each
*        IMAGE extension needs to have its own WCS so that it can be
*        handled independently of the other IMAGE extensions. (Also, the
*        20-JAN-2011 change was bad as it resulted in WCS inherited from
*        the NDF's FITS extension being stored instead of the potentially
*        modifed WCS from the NDF's WCS component).
*     2011 February 25 (MJC):
*        Add USEAXS argument.
*     2013 November 15 (MJC):
*        Add ALWTAB argument and pass it to other routines now using
*        it.
*     9-JUL-2014 (DSB):
*        Added argument AXORD.
*     2015 June 8 (MJC):
*        Respect FITS WCS headers when there is neither a WCS nor an
*        AXIS component, and PROPEX is true.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data system constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      INTEGER   NDFI             ! NDF identifier
      INTEGER   NDFFAI           ! NDF identifier for FITS airlock
      CHARACTER * ( * ) COMP     ! The array component
      INTEGER   FUNIT            ! Logical-unit number of FITS file
      INTEGER   BITPIX           ! Bits per pixel
      LOGICAL   PROPEX           ! Propagate FITS extension?
      CHARACTER * ( * ) ORIGIN   ! The ORIGIN card value
      CHARACTER * ( * ) ENCOD    ! The AST encoding to use for WCS info
      LOGICAL   NATIVE           ! Include NATIVE encoding of WCS info?
      LOGICAL   MULTI            ! Multi-NDF container file?
      CHARACTER * ( * ) EXTNAM
      CHARACTER * ( * ) USEAXS
      LOGICAL ALWTAB
      CHARACTER * ( * ) AXORD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER FITSOK             ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

      INTEGER   NFLAGS           ! Number of flags to indicate
                                 ! presence special NDF components
      PARAMETER( NFLAGS = 6 )

      INTEGER   SZKEY            ! Length of keyword names
      PARAMETER( SZKEY = 8 )     ! Columns 1 to 8

      INTEGER   SZFITS           ! Length of FITS string
      PARAMETER( SZFITS = 80 )

      INTEGER   SZNVAL           ! Character length of numeric values
      PARAMETER( SZNVAL = 20 )   ! Columns 11 to 30

      INTEGER   SZVAL            ! Length of keyword string values
      PARAMETER( SZVAL = 70 )    ! Columns 11 to 80

*  Local Variables:
      INTEGER   ADIM             ! Axis loop counter
      LOGICAL   AXIFND           ! NDF contains a linear axis comps.?
      LOGICAL   AXLFND           ! NDF contains axis label?
      REAL      AXROT            ! Rotation angle of an axis
      LOGICAL   AXUFND           ! NDF contains axis units?
      LOGICAL   BANKEY           ! Part of the FITSIO banner header?
      CHARACTER C*1              ! Accommodates character string
      LOGICAL   CADCKY           ! Not a CADC provenance header?
      CHARACTER CARD * ( SZFITS ) ! Header card
      LOGICAL   CCKEY            ! Not a character-compoent header?
      CHARACTER CDELT * ( SZKEY ) ! Keyword name of CDELTn
      INTEGER   CHEAD            ! Index number of current header
      LOGICAL   CMPFND( NFLAGS ) ! True if certain special NDF
                                 ! components are present
      CHARACTER COMENT *( SZFITS ) ! Comment string in header
      INTEGER   CPFE             ! Position of end of HDS file name
      INTEGER   CPFS             ! Position of start of HDS file name
      INTEGER   CPPE             ! Position of end of HDS path name
      INTEGER   CPPS             ! Position of start of HDS path name
      CHARACTER CRPIX * ( SZKEY ) ! Keyword name of CRPIXn
      CHARACTER CRVAL * ( SZKEY ) ! Keyword name of CRVALn
      INTEGER   DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      INTEGER   DOTPOS           ! Position of HDS component separator
      INTEGER   EXTLEV           ! Extension hierarchy level
      LOGICAL   FITSPR           ! FITS extension is present?
      CHARACTER FITSTR * ( SZFITS ) ! FITS header
      INTEGER   FSTAT            ! FITSIO status
      CHARACTER FTLOC * ( DAT__SZLOC ) ! Locator to NDF FITS extension
      CHARACTER FTLOCI * ( DAT__SZLOC ) ! Locator to element of NDF
                                 ! FITS extension
      LOGICAL   GOTWCS           ! Does NDF have a WCS component?
      INTEGER   I                ! Loop variable
      LOGICAL   ICKEY            ! Not a data-integrity header?
      INTEGER   IVALUE           ! Indexed header number
      LOGICAL   ISNUM            ! Sequence number present?
      INTEGER   J                ! Loop variable
      CHARACTER KEYWRD * ( SZKEY ) ! Accommodates keyword name
      LOGICAL   LABFND           ! NDF LABEL found?
      CHARACTER LORIGN * ( SZVAL ) ! Local value of the ORIGIN argument
      LOGICAL   MNDKEY           ! Not a mandatory header?
      INTEGER   NCHAR            ! Length of a character string
      INTEGER   NCOMP            ! Number of components
      CHARACTER * ( MSG__SZMSG ) NDFNAM ! NDF name for multi-NDF
      INTEGER   NDIM             ! Number of dimensions
      LOGICAL   NDFKEY           ! Not a NDF structure header?
      INTEGER   NHEAD            ! Number of headers
      CHARACTER NULL*1           ! ASCII null character
      LOGICAL   PRORIG           ! Use supplied ORIGIN argument?
      LOGICAL   PROVPR           ! PROVENANCE extension present?
      LOGICAL   PRVCOM           ! Not the provenance caption comment?
      LOGICAL   PRVKEY           ! Not a provenance header?
      LOGICAL   ROTAX( DAT__MXDIM ) ! An axis is rotated in the FITS
                                 ! extension?
      LOGICAL   SCAKEY           ! Not a data sacling header?
      LOGICAL   TITFND           ! NDF TITLE found?
      LOGICAL   UNTFND           ! NDF UNITS found?
      CHARACTER VALUE * ( SZVAL ) ! Accommodates keyword value
      LOGICAL   WCSKEY           ! Not a AXIS co-ordinate header?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Define the null character.
      NULL = CHAR( 0 )

*  Prepare the ORIGIN.
*  ===================

*  Use local variable for the origin to deal with a null value.
      LORIGN = ORIGIN
      IF ( ORIGIN . EQ. ' ' ) LORIGN = 'Starlink Software'

*  Record whether or not an existing ORIGIN card in the FITS airlock
*  is inherited.
      PRORIG = LORIGN .EQ. 'Starlink Software'

*  Write special header cards.
*  ===========================

*  Write out the few special headers irrespective of the presence or
*  otherwise of a FITS extension in the NDF.  These are:
*    SIMPLE, EXTEND, PCOUNT, GCOUNT --- all take their default values.
*    BITPIX, NAXIS, NAXISn --- are derived directly from the NDF data
*      array;
*    LBOUNDn --- the pixel lower bounds of an NDF whose origin is
*      not 1 along each axis.  (These are not part of the standard.)
*    CRVALn, CDELTn, CRPIXn, CTYPEn, CUNITn --- are derived from the
*      NDF WCS component if possible (i.e. exists and maps to a FITS WCS
*      projection type), but added at the end of this routine by
*      routine COF_FPWCS.  If this is not possible, and if argument
*      PROPEX is .TRUE., then it copies the headers of a valid WCS
*      specified in the NDF's FITS airlock.  Should that attempt fail,
*      the last resort tries the NDF AXIS component, if it exists, but
*      it only creates the headers  provided all of the axis centre
*      co-ordinates are linear.
*    OBJECT, LABEL, BUNIT --- the values held in NDF TITLE, LABEL,
*      and UNITS respectively are used if present, otherwise any values
*      found in the FITS extension are used.
*    DATE --- is created automatically.
*    ORIGIN --- inherits any existing ORIGIN card in the NDF FITS
*      extension, unless you supply a value through argument ORIGIN
*      other than the default "Starlink Software" or a blank string.
*    BLANK --- is created for integer data types from the bad value.
*
      CALL COF_WNDFH( NDFI, COMP, FUNIT, NFLAGS, BITPIX, LORIGN,
     :                PROPEX, CMPFND, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Write classification, naming, and extension headers.
*  ====================================================
      CALL FTPKYS( FUNIT, 'HDUCLAS1', 'NDF', 'Starlink NDF '/
     :             /'(hierarchical n-dim format)', FSTAT )

      CALL FTPKYS( FUNIT, 'HDUCLAS2', COMP, 'Array component subclass',
     :             FSTAT )

      NCHAR = CHR_LEN( EXTNAM )
      IF ( EXTNAM( 1:1 ) .NE. ' ' ) THEN

*  Some structures can generate long names, for which the CONTINUE
*  convention is in use by writing the LONGSTRN keyword containing the
*  version number of the convention.
         IF ( NCHAR .GT. 68 ) THEN

*  Write the NDF's component name.  This writes a dummy EXTNAME,
*  a unique EXTVER, and the full component name in keyword EXTNAMEF.
            CALL COF_WENAM( FUNIT, EXTNAM, 'Component', STATUS )

         ELSE
            CALL FTPKYS( FUNIT, 'EXTNAME', EXTNAM( :NCHAR ),
     :                   'Component', FSTAT )
         END IF

*  Set the extension type.
         CALL FTPKYS( FUNIT, 'EXTTYPE', 'NDF', 'HDS data '/
     :                /'type of the extension object', FSTAT )


      ELSE IF ( COMP .NE. 'DATA' .AND. COMP .NE. 'HEADER' ) THEN

*  Write the NDF's component name.
         CALL FTPKYS( FUNIT, 'EXTNAME', COMP, 'Array component', FSTAT )
      END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL CVG_FIOER( FSTAT, 'COF_WHEAD_ERR1', 'FTPKYS',
     :     'Error writing an EXTNAME, EXTTYPE or HDUCLASn header card.',
     :     STATUS )
         GOTO 999
      END IF

      IF ( EXTNAM( 1:1 ) .NE. ' ' ) THEN

*  Count the level delimiters.  The first two words will be the
*  NDF name and MORE, so initialise so that the first extension has
*  level 1.
         DOTPOS = 1
         EXTLEV = -1
         DO WHILE ( DOTPOS .LE. NCHAR )
            CALL CHR_FIND( EXTNAM, '.', .TRUE., DOTPOS )
            IF ( DOTPOS .LE. NCHAR ) EXTLEV = EXTLEV + 1
            DOTPOS = DOTPOS + 1
         END DO

*  Set the extension level.
         IF ( EXTLEV .GT. 0 ) THEN
            CALL FTPKYJ( FUNIT, 'EXTLEVEL', EXTLEV, 'Level in the '/
     :                   /'hierarchical structure', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
            IF ( FSTAT .GT. FITSOK ) THEN
               CALL CVG_FIOER( FSTAT, 'COF_WHEAD_ERR7', 'FTPKYJ',
     :           'Error writing extension level in header.', STATUS )
               GOTO 999
            END IF
         END IF
      END IF

*  Write additional header cards for the multi-NDF component.
*  ==========================================================

*    HDSNAME, HDSTYPE --- used when the input file is a multi-NDF
*      container file and specify the NDF component name and type.
      IF ( MULTI ) THEN

*  Obtain the NDF name.
         CALL NDF_MSG( 'NDF', NDFI )
         CALL MSG_LOAD( 'NDFNAME', '^NDF', NDFNAM, NCHAR, STATUS )
         CALL HDS_SPLIT( NDFNAM, CPFS, CPFE, CPPS, CPPE, STATUS )

*  Set the component name, if one exists.  This indicated by the start
*  character position of the HDS path name being before its
*  corresponding end position.  Otherwise NDFNAM is just the container
*  file, and HDSNAM is not needed.
         IF ( CPPS .LT. CPPE ) THEN
            CALL FTPKYS( FUNIT, 'HDSNAME', NDFNAM( CPPS+1:CPPE ),
     :                   'Component name hierarchical structure',
     :                   FSTAT )
         END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL CVG_FIOER( FSTAT, 'COF_WHEAD_ERR2', 'FTPKYJ',
     :        'Error writing extension level in header.', STATUS )
            GOTO 999
         END IF

*  Set the component type.
         CALL FTPKYS( FUNIT, 'HDSTYPE', 'NDF', 'HDS data '/
     :                /'type of the component', FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            CALL CVG_FIOER( FSTAT, 'COF_WHEAD_ERR3', 'FTPKYS',
     :        'Error writing extension type in header.', STATUS )
            GOTO 999
         END IF
      END IF

*  Deal with the FITS extension that is present.
*  =============================================

*  Check for presence of NDF FITS extension.
      CALL NDF_XSTAT( NDFFAI, 'FITS', FITSPR, STATUS )
      IF ( FITSPR .AND. PROPEX ) THEN

*  Proceed to merge the headers in the FITS extension into the
*  FITS-file header.  Some items should be ignored including those
*  already set above except:
*    ORIGIN which may be overriden by the entry in the NDF extension.
*  In addition:
*    XTENSION, BLANK and END --- are not propagated from the extension.
*      The first will be set for any extension.  The second may be
*      written as required dependent on the data type and any scaling.
*      The END card is written by FITSIO automatically once the header
*      is closed.
*    ASTWARN cards are also not propagated.

*  Use more obvious flags to indicate the certain items have been
*  written to the keywords already.
         AXIFND = CMPFND( 1 )
         AXLFND = CMPFND( 2 )
         AXUFND = CMPFND( 3 )
         TITFND = CMPFND( 4 )
         LABFND = CMPFND( 5 )
         UNTFND = CMPFND( 6 )

*  Check for presence of NDF FITS extension.
         CALL NDF_XSTAT( NDFI, 'PROVENANCE', PROVPR, STATUS )

*  Can use provenance keywords in the airlock provided there is no
*  no PROVENANCE extension to supersede them.
         IF ( .NOT. PROVPR ) THEN
            CADCKY = .TRUE.
            PRVKEY = .TRUE.
            PRVCOM = .TRUE.
         END IF

*  Obtain the number of dimensions of the NDF.
         CALL NDF_DIM( NDFI, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Initialise the flags that indicate a rotated axis.
         DO I = 1, NDIM
            ROTAX( I ) = .FALSE.
         END DO

*  Deal with the items in the NDF FITS extension one by one.
         CALL NDF_XLOC( NDFFAI, 'FITS', 'READ', FTLOC, STATUS )
         CALL DAT_SIZE( FTLOC, NCOMP, STATUS )

*  Loop for each header in the NDF FITS extension.
*  -----------------------------------------------
         DO I = 1, NCOMP

*  Get a locator to successive elements in the FITS extension.
            CALL DAT_CELL( FTLOC, 1, I, FTLOCI, STATUS )

*  Read the FITS string, and extract the keyword and value.
            CALL DAT_GET0C( FTLOCI, FITSTR, STATUS )

*  Assume that it is a valid FITS header card.  Extract the keyword
*  and value.
            KEYWRD = FITSTR( 1:SZKEY )
            VALUE = FITSTR( 11:SZFITS )

*  Filter the keywords.
*  --------------------
*  Leave out SIMPLE, XTENSION, BITPIX, EXTEND, PCOUNT, GCOUNT, NAXIS,
*  NAXISn, ASTWARN keywords.  Also possibly remove LBOUNDn, CDELTn,
*  CRVALn, CRPIXn, CRTYPEn, CTYPEn, CUNITn, OBJECT, LABEL, BUNIT, DATE,
*  BLANK, HDUCLASn, HDSNAME, HDSTYPE, CHECKSUM, DATASUM, and END as
*  described above.  Note CROTAn are also excluded.  To avoid duplicate
*  FITSIO banners these are also omitted, as they are written when
*  FITSIO creates the primary headers.
*
*  Use an intermediate variables to reduce the number of continuation
*  lines in the test.

*  This combines tests for the absence of mandatory headers.
            MNDKEY = ( KEYWRD .NE. 'SIMPLE' ) .AND.
     :               ( KEYWRD .NE. 'BITPIX' ) .AND.
     :               ( KEYWRD .NE. 'EXTEND' ) .AND.
     :               ( KEYWRD .NE. 'XTENSION' ) .AND.
     :               ( KEYWRD .NE. 'GCOUNT' ) .AND.
     :               ( KEYWRD .NE. 'PCOUNT' ) .AND.
     :               ( KEYWRD( 1:5 ) .NE. 'NAXIS' ) .AND.
     :               ( KEYWRD .NE. 'END' )

*  This combines tests for the FITSIO FITS banner, new and old.
            BANKEY = ( KEYWRD .EQ. 'COMMENT' ) .AND. (
     :               ( VALUE( 1:14 ) .EQ. 'FITS (Flexible' ) .OR.
     :               ( VALUE( 1:40 ) .EQ. 'Astrophysics Supplement '/
     :                 /'Series v44/p363,' ) .OR.
     :               ( VALUE( 1:31 ) .EQ. 'Contact the NASA Science '/
     :                 /'Office' ) .OR.
     :               ( VALUE( 1:39 ) .EQ. 'FITS Definition document '/
     :                 /'#100 and other' ) .OR.
     :               ( VALUE( 1:39 ) .EQ. 'and Astrophysics'', '/
     :                 /'volume 376, page 359') )

*  This tests for the absence of scaling keywords.
            SCAKEY = KEYWRD .NE. 'BSCALE' .AND. KEYWRD .NE. 'BZERO'

*  This tests for the absence of co-ordinate system keywords when there
*  NDF AXIS values.  It includes the spurious CRTYPE that was
*  incorrectly written for a while.
            WCSKEY =
     :        ( KEYWRD( 1:5 ) .NE. 'CDELT'  .OR. .NOT. AXIFND ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CRVAL'  .OR. .NOT. AXIFND ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CRPIX'  .OR. .NOT. AXIFND ) .AND.
     :        ( KEYWRD( 1:6 ) .NE. 'CRTYPE' .OR. .NOT. AXLFND ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CTYPE'  .OR. .NOT. AXLFND ) .AND.
     :        ( KEYWRD( 1:5 ) .NE. 'CUNIT'  .OR. .NOT. AXUFND )

*  This tests for NDF character components.
            CCKEY = ( KEYWRD .NE. 'LABEL'  .OR. .NOT. LABFND ) .AND.
     :              ( KEYWRD .NE. 'BUNIT'  .OR. .NOT. UNTFND ) .AND.
     :              ( KEYWRD .NE. 'OBJECT' .OR. .NOT. TITFND )

*  This tests for inherited NDF information.
            NDFKEY =  ( KEYWRD( 1:6 ) .NE. 'LBOUND' ) .AND.
     :                ( KEYWRD( 1:7 ) .NE. 'HDUCLAS' ) .AND.
     :                ( KEYWRD( 1:7 ) .NE. 'HDSNAME' ) .AND.
     :                ( KEYWRD( 1:7 ) .NE. 'HDSTYPE' )

*  This tests for integrity-check keywords.
            ICKEY = ( KEYWRD .NE. 'CHECKSUM' ) .AND.
     :              ( KEYWRD .NE. 'DATASUM' )


*  Cannot use provenance keywords in the airlock when there is a
*  PROVENANCE extension to supersede them.
            IF ( PROVPR ) THEN

*  Test for CADC provenance keywords.  While these may not be unique,
*  it is more likely they are provenance than some other institution's
*  keywords.  First test whether or not there is an index number after
*  the first three characters of the keyword.
               CALL ERR_MARK
               CALL CHR_CTOI( KEYWRD( 4:8 ), IVALUE, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  ISNUM = .FALSE.
               ELSE
                  ISNUM = .TRUE.
               END IF
               CALL ERR_RLSE

*  Test for absence of PRVCNT, PRVn, OBSCNT, and OBSn keywords unless
*  there is no PROVENANCE extension.
               CADCKY = ( KEYWRD .NE. 'PRVCNT' ) .AND.
     :                  ( KEYWRD .NE. 'OBSCNT' ) .AND.
     :                  ( KEYWRD .NE. 'FILEID' ) .AND.
     :                  ( KEYWRD( 1:3 ) .NE. 'PRV' .OR. .NOT. ISNUM )
     :                  .AND.
     :                  ( KEYWRD( 1:3 ) .NE. 'OBS' .OR. .NOT. ISNUM )

*  Test for general provenance keywords.  While these may not be unique,
*  it is more likely they are provenance than some other institution's
*  keywords.  First test whether or not there is an index number after
*  the first four characters of the keyword.
               CALL ERR_MARK
               CALL CHR_CTOI( KEYWRD( 5:8 ), IVALUE, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  ISNUM = .FALSE.
               ELSE
                  ISNUM = .TRUE.
               END IF
               CALL ERR_RLSE

*  Test for absence of PRVPn, PRVIn, PRVDn, PRVCn, PRVMn.
               PRVKEY = .NOT. ISNUM .OR.
     :                  ( ( KEYWRD( 1:4 ) .NE. 'PRVP' ) .AND.
     :                    ( KEYWRD( 1:4 ) .NE. 'PRVI' ) .AND.
     :                    ( KEYWRD( 1:4 ) .NE. 'PRVD' ) .AND.
     :                    ( KEYWRD( 1:4 ) .NE. 'PRVC' ) .AND.
     :                    ( KEYWRD( 1:4 ) .NE. 'PRVM' ) )

*  Exclude the provenance caption.

*  First remove leading blanks from the header in case this is the
* "Provenance:" comment-only header.
               COMENT = FITSTR
               CALL CHR_LDBLK( COMENT )

*  Also remove the the blank line before the caption too otherwise
*  repeated conversions to and from FITS could generate lots of blank
*  lines.  Find the current header position and delete the header
*  provided it is blank.
               PRVCOM = COMENT .NE. '/ Provenance:'
               IF ( .NOT. PRVCOM ) THEN
                  CALL FTGHSP( FUNIT, NHEAD, CHEAD, STATUS )
                  CALL FTGREC( FUNIT, CHEAD, CARD, STATUS )
                  IF ( CARD .EQ. ' ' )
     :              CALL FTDREC( FUNIT, CHEAD, STATUS )
               END IF
            END IF

*  Use an intermediate variable to reduce the number of continuation
*  lines in the test.  This tests for the ORIGIN card.
            IF ( KEYWRD .EQ. 'ORIGIN' ) THEN
               IF ( PRORIG ) THEN

*  Overwrite the ORIGIN card written by COF_WNDFH rather than making a
*  second card.
                  CALL FTMCRD( FUNIT, 'ORIGIN', FITSTR, FSTAT )
               END IF

*  Do the test whether to copy the FITS extension header into the output
*  FITS file's header.
            ELSE IF ( MNDKEY .AND. .NOT. BANKEY .AND. SCAKEY .AND.
     :                WCSKEY .AND. CCKEY .AND. NDFKEY .AND. ICKEY .AND.
     :                CADCKY .AND. PRVKEY .AND. PRVCOM .AND.
     :                ( KEYWRD .NE. 'DATE' ) .AND.
     :                ( KEYWRD .NE. 'EXTNAME' ) .AND.
     :                ( KEYWRD .NE. 'ASTWARN' ) .AND.
     :                ( KEYWRD .NE. 'BLANK' ) ) THEN

*  Look for a rotated axis in the FITS extension (CROTAn is present and
*  non-zero).  If there is one, the NDF AXIS structure will contain
*  pixel co-ordinates, which are probably not the original co-ordinates
*  for the axis.  If there are rotated axes, the keywords defining the
*  axis will be re-written later using the values of CRVALn and CDELTn
*  in the FITS extension.  When there is no AXIS component in the NDF,
*  then the axis keywords can be written immediately to the output
*  FITS header.
               IF ( INDEX( KEYWRD, 'CROTA' ) .NE. 0 .AND.
     :              AXIFND ) THEN
                  CALL CHR_CTOI( KEYWRD( 6: ), ADIM, STATUS )
                  CALL CHR_CTOR( VALUE( :SZNVAL ), AXROT, STATUS )
                  ROTAX( ADIM ) = ABS( AXROT ) .GT. VAL__EPSR

               ELSE

*  Write the header card.
                  CALL FTPREC( FUNIT, FITSTR, FSTAT )
               END IF

*  Deal with problem headers.
*  --------------------------
*  A common cause for problems here are invalid (i.e. not text)
*  characters, by far the most frequent being null for space.  This is
*  FITSIO error number 207.
               IF ( FSTAT .EQ. 207 ) THEN

*  Flush the FITSIO error stack and set the FITSIO status to be good.
                  CALL FTCMSG
                  FSTAT = FITSOK

*  Replace the nulls with spaces.  One could substitute more at the
*  cost of efficiency.  This seems a good compromise.
                  CALL CHR_TRCHR( NULL, ' ', FITSTR, STATUS )

*  Make a second attempt to write the header card.
                  CALL FTPREC( FUNIT, FITSTR, FSTAT )

*  If that's cured the problem, continue to inform the user of the
*  errorneous header to encourage correction at the source, e.g. an
*  observatory.
                  IF ( FSTAT .EQ. FITSOK ) THEN

*  Issue a warning message.
                     CALL MSG_OUTIF( MSG__NORM, 'COF_WHEAD_WAR1',
     :                 'Warning: NDF''s FITS airlock header contained '/
     :                 /'at least one invalid null character.  Each '/
     :                 /'null was replaced with a space before '/
     :                 /'propagation to the FITS file.  Header was:',
     :                 STATUS )
                     CALL MSG_OUTIF( MSG__NORM, 'COF_WHEAD_WAR2',
     :                 FITSTR, STATUS )
                     CALL MSG_OUTIF( MSG__NORM, 'COF_WHEAD_WAR3',
     :                 ' ', STATUS )
                  END IF

*  If that didn't cure the problem, the FITSIO error stack will contain
*  messages, and we can continue as if there was no attempt to
*  substitute the null characters.
               END IF

*  Skip over non-valid cards.  Thus defective cards are not propagated.
*  However, issue a warning message and flush the FITSIO error stack.
*  We should aim to have header-validation software.
               IF ( FSTAT .GT. FITSOK ) THEN
                  CALL ERR_MARK
                  CALL CVG_FIOER( FSTAT, 'COF_WHEAD_ERR4', 'FTPREC',
     :              'Warning: error unable to propagate a header from '/
     :              /'NDF''s FITS airlock.', STATUS )

*  The error is not fatal so reset the FIO status to OK.
                  FSTAT = FITSOK
                  CALL ERR_FLUSH( STATUS )
                  CALL ERR_RLSE
               END IF

*  End loop if END header
            ELSE IF ( KEYWRD .EQ. 'END' ) THEN
               GO TO 100

            END IF
         END DO

  100    CONTINUE

*  Deal with rotated axes.
*  =======================

*  For each dimension check if there are any rotated axes when the
*  the NDF contains an AXIS component.
         DO J = 1, NDIM
            IF ( ROTAX( J ) ) THEN

*  Search through the FITS headers to find the values of CRVALn,
*  CDELTn, and CRPIXn.
               DO I = 1, NCOMP

*  Get locator to successive elements in the FITS extension.
                  CALL DAT_CELL( FTLOC, 1, I, FTLOCI, STATUS )

*  Read the FITS string, and extract the keyword.
                  CALL DAT_GET0C( FTLOCI, FITSTR, STATUS )
                  KEYWRD = FITSTR( 1:SZKEY )

*  Create the keywords being searched.
                  CALL CHR_ITOC( J, C, NCHAR )
                  CDELT = 'CDELT'//C(:1)
                  CRPIX = 'CRPIX'//C(:1)
                  CRVAL = 'CRVAL'//C(:1)

*  Test the current keyword.
                  IF ( ( INDEX( KEYWRD, CDELT ) .NE. 0 ) .OR.
     :                 ( INDEX( KEYWRD, CRPIX ) .NE. 0 ) .OR.
     :                 ( INDEX( KEYWRD, CRVAL ) .NE. 0 ) ) THEN

*  Write the header card.
                     CALL FTPREC( FUNIT, FITSTR, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
                     IF ( FSTAT .GT. FITSOK ) THEN
                        CALL CVG_FIOER( FSTAT, 'COF_WHEAD_ERR5',
     :                    'FTPREC', 'Error copying FITS axis-rotation '/
     :                    /'header card to the FITS file.', STATUS )
                        GOTO 999
                     END IF
                  END IF
               END DO
            END IF
         END DO
      END IF

*  Write out the NDF WCS information.
*  ==================================

*  If the NDF has a WCS component do not propagate the AXIS information
*  in a table.
      CALL NDF_STATE( NDFI, 'WCS', GOTWCS, STATUS )

      IF ( PROPEX .AND. .NOT. ( GOTWCS .OR. AXIFND ) ) THEN
         CALL MSG_OUTIF( MSG__VERB, 'COF_WHEAD_WAR4',
     :     'Note that the WCS information is progapated verbatim '/
     :     /'from the NDF''s FITS airlock.', STATUS )
      ELSE
         CALL COF_FPWCS( FUNIT, NDFI, ENCOD, NATIVE, USEAXS, ALWTAB,
     :                   AXORD, STATUS )
      END IF

  999 CONTINUE

      END
