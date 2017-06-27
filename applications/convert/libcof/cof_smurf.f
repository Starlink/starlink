      SUBROUTINE COF_SMURF( SNAME, LOC, FUNIT, NDF, FILNAM, NOARR,
     :                      ARRNAM, BITPIX, BLOCKF, ORIGIN, PROFIT,
     :                      DUPLEX, PROEXT, PROHIS, SUMS, ENCOD,
     :                      NATIVE, USEAXS, ALWTAB, AXORD, STATUS )
*+
*  Name:
*     COF_SMURF

*  Purpose:
*     Converts a SMURF extension into FITS extensions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_SMURF( SNAME, LOC, FUNIT, NDF, FILNAM, NOARR, ARRNAM,
*                     BITPIX, BLOCKF, ORIGIN, PROFIT, DUPLEX, PROEXT,
*                     PROHIS, ENCOD, NATIVE, USEAXS, ALWTAB, AXORD,
*                     STATUS )

*  Description:
*     This routine converts contents of a SMURF extension to FITS.  Each
*     NDF is converted in turn in standard fashion.  It is assumed that
*     the NDFs do not contain their own metadata, thus may inherit the
*     metadata in the FITS airlock and HISTORY records from the parent
*     NDF.  Any other components of the structure are converted to a
*     binary table, as in the general case of extension conversion.
*
*     Most of the arguments are merely passed to the routine that
*     converts each NDF within the SMURF extension.

*  Arguments:
*     SNAME = CHARACTER * ( * ) (Given)
*        The name of structure.  It is used to form the EXTNAME
*        keyword.
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator to the SMURF extension structure whose contents are to
*        be converted to binary tables.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     NDF = INTEGER (Given)
*        The identifier of the parent NDF.
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

*  Prior Requirements:
*     -  A primary HDU unit exists in the FITS file, and the file is
*     open.

*  Copyright:
*     Copyright (C) 2007, 2009, 2011, 2013 Science & Technology
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
*     {enter_new_authors_here}

*  History:
*     2007 October 21 (MJC):
*        Original version.
*     2009 November 28 (MJC):
*        Allow for optional processing of extensions within the NDFs,
*        which are converted in binary tables using the generic
*        recursion.  Write the dummy header for SMURF structure as type
*        SMURF_EXT.
*     2011 March 3 (MJC):
*        Add USEAXS argument.
*     2013 November 15 (MJC):
*        Add ALWTAB argument and pass it to other routines now using
*        it.
*     9-JUL-2014 (DSB):
*        Added argument AXORD.
*     2017 June 27 (MJC):
*        Always create a dummy binary table to hold the SMURF extension
*        regardless of the number of non-NDF components.  The
*        functionality elsewhere that obviated the need to create the
*        dummy extension when there is a non-NDF component present is no
*        longer operational.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      CHARACTER * ( * ) SNAME
      CHARACTER * ( * ) LOC
      INTEGER FUNIT
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
      LOGICAL SUMS
      CHARACTER * ( * ) ENCOD
      LOGICAL NATIVE
      CHARACTER * ( * ) USEAXS
      LOGICAL ALWTAB
      CHARACTER * ( * ) AXORD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER   FITSOK           ! Value of good FITSIO status
      PARAMETER ( FITSOK = 0 )

*  Local Variables:
      CHARACTER*( DAT__SZLOC ) CLOC ! Locator to a SMURF component
      CHARACTER*256 FILE         ! Name of the HDS file (dummy)
      LOGICAL FITPRE             ! FITS airlock extension is present?
      INTEGER I                  ! Component loop counter
      INTEGER IEXT               ! Extension loop counter
      CHARACTER*68 NAME          ! Name of HDS path
      INTEGER NCOMP              ! Number of components SMURF extension
      INTEGER NDFE               ! Extension NDF identifier
      INTEGER NEXTN              ! Number of extensions
      INTEGER NEX2PR             ! Number of extensions to process
      INTEGER NLEV               ! Number of hierarchical levels
      CHARACTER*( DAT__SZTYP ) TYPE ! Type of the NDF component
      LOGICAL VALID              ! The NDF identifier is valid?
      LOGICAL WRITTN             ! Dummy structure is written?
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to an NDF extension
      CHARACTER * ( NDF__SZXNM ) XNAME ! Name of NDF extension

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the NDF identifier.
*  ============================
      CALL NDF_VALID( NDF, VALID, STATUS )

*  Report an error if the identifier is not valid.
      IF ( .NOT. VALID ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_SMURF_INVNDF',
     :     'COF_SMURF: The identifier to the input NDF is invalid. '/
     :     /'(Probable programming error.)', STATUS )
         GOTO 999
      END IF

*  Validate that we have a SMURF extension.
      CALL DAT_TYPE( LOC, TYPE, STATUS )
      IF ( TYPE .NE. 'SMURF' .AND. TYPE .NE. 'SMURF_EXT' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_SMURF_NOTSMURFF',
     :     'COF_SMURF: The supplied extension is not of type SMURF '/
     :     /'or SMURF_EXT.  (Probable programming error.)', STATUS )
         GOTO 999
      END IF

*  Find the number of components within the extension.
      CALL DAT_NCOMP( LOC, NCOMP, STATUS )

*  Look for any non-NDF components.
*  ================================

*  The SMURF/SMURF_EXT type should define the contents, but it is
*  not clear how rigid or final this structure is.  Since we need
*  FITS2NDF to be able to recreate the original NDF structure, yet
*  use generic code, the SMURF structure must be first recreated to
*  hold its named NDFs.  So create a small table with one dummy value.
*  FITS2NDF will recognise this as being a dummy component and will not
*  create an NDF extension component for it.
      CALL HDS_TRACE( LOC, NLEV, NAME, FILE, STATUS )

      CALL COF_WSTR( FUNIT, NAME, 'SMURF_EXT', NLEV, WRITTN, STATUS )

*  Enumerate the SMURF extension components.
      DO I = 1, NCOMP
         CALL DAT_INDEX( LOC, I, CLOC, STATUS )

*  Get the object's path name and assign it to the extension name.
         CALL HDS_TRACE( CLOC, NLEV, NAME, FILE, STATUS )

*  Check if the component is an NDF.
         CALL DAT_TYPE( CLOC, TYPE, STATUS )
         IF ( TYPE .EQ. 'NDF' ) THEN

*  Obtain an identifier to the NDF.
            CALL NDF_FIND( CLOC, ' ', NDFE, STATUS )

*  Convert the NDF, but using the parent NDF's metadata.
            CALL COF_NEX2F( NAME, FUNIT, NDF, NDFE, FILNAM, 1,
     :                      ARRNAM, BITPIX, BLOCKF, ORIGIN, PROFIT,
     :                      DUPLEX, PROHIS, SUMS, ENCOD, NATIVE,
     :                      USEAXS, ALWTAB, AXORD, STATUS )

*  Process extensions.
*  ===================
            IF ( PROEXT ) THEN

*  Use binary tables for all extensions other than FITS.  Special
*  software for handling standard extensions will be provided as it
*  becomes available.

*  Look for NDF extensions.  Check whether or not there are any present.
               CALL NDF_XNUMB( NDFE, NEXTN, STATUS )

               IF ( NEXTN .GE. 1 ) THEN

*  See if one of these is the FITS extension.
                  CALL NDF_XSTAT( NDFE, 'FITS', FITPRE, STATUS )

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
                     DO IEXT = 1, NEXTN

*  Get the name of the next extension.
                        CALL NDF_XNAME( NDFE, IEXT, XNAME, STATUS )

*  Get a locator to the extension.
                        CALL NDF_XLOC( NDFE, XNAME, 'READ', XLOC,
     :                                 STATUS )

*  Process the extension into a hierarchy.
                        CALL COF_THIER( XNAME, XLOC, FUNIT, STATUS )

*  Write integrity-check headers.
                        IF ( SUMS ) CALL FTPCKS( FUNIT, STATUS )

*  Annul the locator so it may be reused.
                        CALL DAT_ANNUL( XLOC, STATUS )
                     END DO
                  END IF
               END IF
            END IF

*   Free resources.
            CALL NDF_ANNUL( NDFE, STATUS )
         ELSE

*  Process the component into a hierarchy if it's a structure.
            CALL COF_THIER( NAME, CLOC, FUNIT, STATUS )
         END IF

         CALL DAT_ANNUL( CLOC, STATUS )
      END DO

  999 CONTINUE

      END
