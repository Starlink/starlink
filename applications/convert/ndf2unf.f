      SUBROUTINE NDF2UNF( STATUS )
*+
*  Name:
*     NDF2UNF

*  Purpose:
*     Converts an NDF to a sequential unformatted file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDF2UNF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts an NDF to a sequential unformatted
*     Fortran file.  Only one of the array components may be copied to
*     the output file.  Preceding the data there is an optional header
*     consisting of either the FITS extension with the values of
*     certain keywords replaced by information derived from the NDF, or
*     a minimal FITS header also derived from the NDF.

*  Usage:
*     ndf2unf in out [comp] [noperec]

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The NDF component to be copied.  It may be "Data", "Quality"
*        or "Variance".  ["Data"]
*     FITS = _LOGICAL (Read)
*        If TRUE, any FITS extension is written to start of the output
*        file, unless there is no extension whereupon a minimal FITS
*        header is written to the unformatted file.  [FALSE]
*     IN = NDF (Read)
*        Input NDF data structure.  The suggested default is the current
*        NDF if one exists, otherwise it is the current value.
*     NOPEREC = _INTEGER (Read)
*        The number of data values per record of the output file.  It
*        must be positive.  The suggested default is the current value.
*        [The first dimension of the NDF]
*     OUT = FILENAME (Write)
*        Name of the output sequential unformatted file.  The file will
*        normally have variable-length records when there is a header,
*        but always fixed-length records when there is no header.

*  Examples:
*     ndf2unf cluster cluster.dat
*        This copies the data array of the NDF called cluster to an
*        unformatted file called cluster.dat.  The number of data values
*        per record is equal to the size of the first dimension of the
*        NDF.
*     ndf2unf cluster cluster.dat v
*        This copies the variance of the NDF called cluster to an
*        unformatted file called cluster.dat.  The number of variance
*        values per record is equal to the size of the first dimension
*        of the NDF.
*     ndf2unf cluster cluster.dat noperec=12
*        This copies the data array of the NDF called cluster to an
*        unformatted file called cluster.dat.  There are twelve data
*        values per record in cluster.dat.
*     ndf2unf out=ndf234.dat fits in=@234
*        This copies the data array of the NDF called 234 to an
*        unformatted file called ndf234.dat.  The number of data values
*        per record is equal to the size of the first dimension of the
*        NDF.  If there is a FITS extension, it is copied to ndf234.dat
*        with substitution of certain keywords, otherwise a minimal
*        FITS header is produced.

*  Notes:
*     The details of the conversion are as follows:
*        -  the NDF array as selected by COMP is written to the
*        unformatted file in records following an optional header.
*        -  HISTORY is not propagated.
*        -  ORIGIN information is lost.
*
*        - When a header is to be made, it is composed of FITS-like card
*        images as follows:
*           -  The number of dimensions of the data array is written
*           to the keyword NAXIS, and the actual dimensions to NAXIS1,
*           NAXIS2 etc. as appropriate.
*           -  If the NDF contains any linear axis structures the
*           information necessary to generate these structures is
*           written to the FITS-like headers. For example, if a linear
*           AXIS(1) structure exists in the input NDF the value of the
*           first data point is stored with the keyword CRVAL1,
*           and the incremental value between successive axis data is
*           stored in keyword CDELT1.  By definition the reference
*           pixel is 1.0 and is stored in keyword CRPIX1.  If there is
*           an axis label it is written to keyword CTYPE1, and axis
*           unit is written to CUNIT1.  (Similarly for AXIS(2)
*           structures etc.) FITS does not have a standard method of
*           storing axis widths and variances, so these NDF components
*           will not be propagated to the header.  Non-linear axis data
*           arrays cannot be represented by CRVALn and CDELTn, and must
*           be ignored.
*           -  If the input NDF contains TITLE, LABEL or UNITS
*           components these are stored with the keywords TITLE, LABEL
*           or BUNIT respectively.
*           -  If the input NDF contains a FITS extension, the FITS
*           items may be written to the FITS-like header, with the
*           following exceptions:
*              o  BITPIX is derived from the type of the NDF data array,
*              and so it is not copied from the NDF FITS extension.
*              o  NAXIS, and NAXISn are derived from the dimensions of
*              the NDF data array as described above, so these items
*              are not copied from the NDF FITS extension.
*              o  The TITLE, LABEL, and BUNIT descriptors are only
*              copied if no TITLE, LABEL, and UNITS NDF components
*              respectively have already been copied into these
*              headers.
*              o  The CDELTn, CRVALn, CTYPEn, CUNITn, and CRTYPEn
*              descriptors in the FITS extension are only copied if the
*              input NDF contained no linear axis structures.
*              o  The standard order of the FITS keywords is preserved,
*              thus BITPIX, NAXIS and NAXISn appear immediately after
*              the first card image, which should be SIMPLE.
*              o  BSCALE and BZERO in a FITS extension are copied when
*              BITPIX is positive, i.e. the array is not floating-point.
*           -  An extra header record with keyword UNSIGNED and logical
*           value T is added when the array data type is one of the HDS
*           unsigned integer types.  This is done because standard FITS
*           does not support unsigned integers, and allows (in
*           conjunction with BITPIX) applications reading the
*           unformatted file to determine the data type of the array.
*           -  The last header record card will be the standard FITS
*           END.
*        -  Other extensions are not propagated.

*  Related Applications:
*     CONVERT: UNF2NDF.

*  Implementation Status:
*     -  The value of bad pixels is not written to a FITS-like header
*     record with keyword BLANK.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 1995-1996, 2004 Central Laboratory of the Research
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
*     AJC: Alan J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1992 September 16 (MJC):
*        Original version.
*     1993 August 25 (MJC):
*        Corrected the calculations of record lengths.
*     1995 November 8 (AJC):
*        Set RECMIN to be 80 in all cases where header is required.
*     1996 September 16 (MJC):
*        Corrected usage of CTYPEn (was CRTYPEn) and introduced CUNITn
*        for axis units.  Also writes CRPIXn FITS keyword when the NDF
*        has linear axis centres.
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
      INTEGER   NFLAGS           ! Number of flags to indicate
                                 ! presence special NDF components
      PARAMETER( NFLAGS = 6 )
      INTEGER   SZDESC           ! Size of descriptor names
      PARAMETER( SZDESC = 8 )
      INTEGER   SZFITS           ! Size of FITS string
      PARAMETER( SZFITS = 80 )
      INTEGER   SZVAL            ! Size of descriptor values
      PARAMETER( SZVAL = 70 )

*  Local Variables:
      LOGICAL AXIFND             ! True if NDF contains a linear axis
                                 ! comps.
      LOGICAL AXLFND             ! True if NDF contains axis label
      LOGICAL AXUFND             ! True if NDF contains axis units
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to single FITS card
      LOGICAL CMPFND( NFLAGS )   ! True if certain special NDF
                                 ! components are present
      CHARACTER*( 25 ) COMLIS    ! List of the NDF components
      INTEGER COMLN              ! The used length of COMLIS
      CHARACTER*( 8 ) COMP       ! The component of NDF to plot
      LOGICAL CPFITS             ! True if there is a FITS extension to
                                 ! copy to the unformatted file's header
      CHARACTER DESCR * ( SZDESC ) ! Accommodates descriptor name
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions (axis length)
      INTEGER EL                 ! Number of mapped elements
      LOGICAL END                ! True if there is no END card
                                 ! terminating the FITS extension
      INTEGER FD                 ! File descriptor
      INTEGER FIOSTA             ! Fortran I/O status
      LOGICAL FIRST              ! True if the FITS mandatory headers
                                 ! have yet to be written
      CHARACTER * ( DAT__SZLOC ) FLOC ! Locator to FITS extension
      CHARACTER FITSTR *( SZFITS ) ! A FITS-header card
      LOGICAL HEADER             ! If true there will be FITS-like
                                 ! header
      INTEGER I                  ! Loop counter
      LOGICAL IDATA              ! True if NDF array is integer type
      LOGICAL LABFND             ! True if NDF LABEL found
      INTEGER LUN                ! Logical-unit number
      CHARACTER MACHIN * ( 24 )  ! Machine name
      INTEGER NCARD              ! Number of cards in FITS extension
      INTEGER NDIM               ! Number of dimensions
      INTEGER NUMMAX             ! Maximum number of values per record
      INTEGER NDF                ! Identifier for NDF
      CHARACTER NODE * ( 20 )    ! Node name
      INTEGER NUMPRE             ! Number of data values per record
      INTEGER PNTR( 1 )          ! Pointer to NDF mapped array
      INTEGER RECL               ! Maximum recordlength of unformatted
                                 ! file in bytes
      INTEGER RECMIN             ! Minimum record length of the
                                 ! unformatted file
      CHARACTER RELEAS * ( 10 )  ! Release of operating system
      CHARACTER SYSNAM * ( 10 )  ! Operating system
      LOGICAL THERE              ! FITS extension is present
      LOGICAL TITFND             ! True if NDF TITLE found
      CHARACTER * ( NDF__SZTYP ) TYPE ! Data type for processing
      LOGICAL UNTFND             ! True if NDF UNITS found
      CHARACTER VALUE * ( SZVAL ) ! Accommodates descriptor value
      CHARACTER VERSIO * ( 10 )  ! Sub-version of operating system
      LOGICAL VMS                ! True if running on a VAX/VMS system

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the input NDF.
*  =====================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )

*  Find out which component is to be processed.
*  ============================================

*  Form the component list of the input NDF. Data-array component must
*  exist for the file to be an NDF.
      COMLIS = 'Data'
      COMLN = 4

*  If the Quality component exists, append it to component list.
      CALL NDF_STATE( NDF, 'Quality', THERE, STATUS )
      IF ( THERE ) THEN
         CALL CHR_APPND( ','//'Quality', COMLIS, COMLN )
      END IF

*  If the Variance component exists, append it to component list.
      CALL NDF_STATE( NDF, 'Variance', THERE, STATUS )
      IF ( THERE ) THEN
         CALL CHR_APPND( ','//'Variance', COMLIS, COMLN )
      END IF

*  Find which component to copy.
      CALL PAR_CHOIC( 'COMP', 'Data', COMLIS( :COMLN ), .FALSE., COMP,
     :                 STATUS )

*  Find the data type of the component.
      CALL NDF_TYPE( NDF, COMP, TYPE, STATUS )

*  Decide whether BSCALE and BZERO will need copying, i.e. is the type
*  floating point?
      IDATA = .NOT. ( TYPE .EQ. '_REAL' .OR. TYPE .EQ. '_DOUBLE' )

*  Process the FITS extension
*  ==========================

*  Is there a FITS extension?
      CALL NDF_XSTAT( NDF, 'FITS', THERE, STATUS )

*  Ask if it there is to be a FITS-like header?
      CALL PAR_GET0L( 'FITS', HEADER, STATUS )

*  Is there a FITS extension to be copied?  If there is, set the minimum
*  recordlength to 80 bytes for the unformatted FITS headers.
      CPFITS = HEADER .AND. THERE
      IF ( HEADER ) THEN
         RECMIN = 80
      ELSE
         RECMIN = 1
      END IF

*  Determine whether or not the operating system is VMS.
*  =====================================================
*
*  This assumes that the system is either VMS or UNIX.  It is needed
*  to specify the path of the file containing the global parameters.
      CALL PSX_UNAME( SYSNAM, NODE, RELEAS, VERSIO, MACHIN, STATUS )
      VMS = INDEX( SYSNAM, 'VMS' ) .NE. 0

*  Find the formatting arrangement for the output file.
*  ====================================================

*  Derive the maximum number of values per record for the data type.
*  The maximum is imposed by VMS (8191 maximum number of longwords).
*  On UNIX set it to the largest innteger, i.e. no practical limit.
      IF ( .NOT. VMS ) THEN
         NUMMAX = VAL__MAXI

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         NUMMAX = 32764 / VAL__NBD

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         NUMMAX = 32764 / VAL__NBR

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         NUMMAX = 32764 / VAL__NBI

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         NUMMAX = 32764 / VAL__NBK

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         NUMMAX = 32764 / VAL__NBUW

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         NUMMAX = 32764 / VAL__NBW

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         NUMMAX = 32764 / VAL__NBUB

      ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
         NUMMAX = 32764 / VAL__NBB

      END IF

*   Obtain the NDF dimensions.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Obtain the number of values per record.  Constrain the dynamic
*  default so that it is in range.  In normal circumstances it will
*  be the first dimension of the NDF.
      CALL PAR_GDR0I( 'NOPEREC', MIN( NUMMAX, DIMS( 1 ) ), 1, NUMMAX,
     :                .FALSE., NUMPRE, STATUS )

*  Derive the recordlength in bytes.
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         RECL = NUMPRE * VAL__NBD

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         RECL = NUMPRE * VAL__NBR

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         RECL = NUMPRE * VAL__NBI

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         RECL = NUMPRE * VAL__NBK

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         RECL = NUMPRE * VAL__NBUW

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         RECL = NUMPRE * VAL__NBW

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         RECL = NUMPRE * VAL__NBUB

      ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
         RECL = NUMPRE * VAL__NBB

      END IF

*  Ensure that any FITS header is not truncated by the record length.
      RECL = MAX( RECL, RECMIN )

*  Open the unformatted file.
*  ==========================

*  Open the FORTRAN file.
      CALL FIO_ASSOC( 'OUT', 'WRITE', 'UNFORMATTED', RECL, FD, STATUS )

*  Obtain the logical-unit number to the file.
      CALL FIO_UNIT( FD, LUN, STATUS )

*  Process the FITS header.
*  ========================
      IF ( CPFITS ) THEN

*      Each item is copied to a header record except:
*         NAXIS, NAXISn - these are derived directly from the NDF data
*           array;
*         CRVALn, CDELTn, CTYPEn, CUNITn - derived from the NDF axis
*           structures if possible. If no linear NDF axis structures
*           are present, the values in the NDF FITS extension are
*           copied.  If any are non-linear all FITS axis information is
*           lost.
*         TITLE, LABEL, BUNIT - the values held in NDF TITLE, LABEL,
*           and UNITS are used if present, otherwise any values found in
*           the FITS extension are used.

*      Initialise flags to indicate that these components have not
*      been found.
         AXIFND = .FALSE.
         AXLFND = .FALSE.
         AXUFND = .FALSE.
         TITFND = .FALSE.
         LABFND = .FALSE.
         UNTFND = .FALSE.

*  Obtain a locator to the FITS extension.
         CALL NDF_XLOC( NDF, 'FITS', 'READ', FLOC, STATUS )

*  Get its shape, i.e. the number of FITS cards.
         CALL DAT_SIZE( FLOC, NCARD, STATUS )

*  Initialise flag to indicate whether or not an END card-image needs to
*  be added to the header.
         END = .FALSE.

*  Initialise the flag to indicate whether or not the mandatory headers
*  have been written.
         FIRST = .TRUE.

*  Loop for each card.
         DO  I = 1, NCARD

*  Access the cell.
            CALL DAT_CELL( FLOC, 1, I, CLOC, STATUS )

*  Obtain its value.
            CALL DAT_GET0C( CLOC, FITSTR, STATUS )

*  Extract the FITS string.
            DESCR = FITSTR( 1:SZDESC )
            VALUE( 1:SZVAL ) = FITSTR( 11:SZFITS )

*  Update the end flag.
            END = I .EQ. NCARD .AND. DESCR( 1:3 ) .NE. 'END'

*  Leave out BITPIX, NAXIS, NAXISn, and possibly CDELTn, CRVALn,
*  CRTYPEn, CTYPEn, TITLE, LABEL, and BUNIT as described above.  Note
*  CROTAn are also excluded.
            IF ( ( INDEX( DESCR, 'NAXIS' ) .EQ. 0 ) .AND.
     :        ( INDEX( DESCR, 'BITPIX' ) .EQ. 0 ) .AND.
     :        ( INDEX( DESCR, 'BSCALE') .EQ. 0 .OR. IDATA ) .AND.
     :        ( INDEX( DESCR, 'BZERO' ) .EQ. 0 .OR. IDATA ) .AND.
     :        ( INDEX( DESCR, 'CDELT' ) .EQ. 0 .OR. .NOT. AXIFND ) .AND.
     :        ( INDEX( DESCR, 'CRVAL' ) .EQ. 0 .OR. .NOT. AXIFND ) .AND.
     :        ( INDEX( DESCR, 'CROTA' ) .EQ. 0 .OR. .NOT. AXIFND ) .AND.
     :        ( INDEX( DESCR, 'CRTYPE') .EQ. 0 .OR. .NOT. AXLFND ) .AND.
     :        ( INDEX( DESCR, 'CTYPE' ) .EQ. 0 .OR. .NOT. AXLFND ) .AND.
     :        ( INDEX( DESCR, 'CUNIT' ) .EQ. 0 .OR. .NOT. AXUFND ) .AND.
     :        ( INDEX( DESCR, 'LABEL' ) .EQ. 0 .OR. .NOT. LABFND ) .AND.
     :        ( INDEX( DESCR, 'BUNIT') .EQ. 0 .OR. .NOT. UNTFND ) .AND.
     :        ( INDEX( DESCR, 'TITLE') .EQ. 0 .OR. .NOT. TITFND ) ) THEN

*  This code assumes that the FITS header will begin with the mandatory
*  headers in the mandatory order.  It is not worth the great effort to
*  validate the FITS descriptors of a defunct format.  However, since
*  some applications validate the headers, insure the first header is
*  SIMPLE.  Want to write after this.
               IF ( FIRST ) THEN
                  IF ( INDEX( DESCR, 'SIMPLE' ) .EQ. 0 ) THEN

*  Create the SIMPLE card.
                     FITSTR = ' '
                     FITSTR( 1:9 ) = 'SIMPLE  ='
                     FITSTR( 30: ) = 'T / File is simple'

*  Ensure that the value is valid, i.e. true or false.  Assume that it
*  is just a simple file.
                  ELSE IF ( FITSTR( 30:30 ) .NE. 'T' .AND.
     :                      FITSTR( 30:30 ) .NE. 'F' ) THEN
                     FITSTR( 30:30 ) = 'T'
                  END IF

*  Write the SIMPLE card header to the unformatted file.
                  WRITE( LUN, IOSTAT=FIOSTA ) FITSTR
                  CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a '/
     :              /'header record to file ^FNAME.  Reason was '/
     :              /'^IOSTAT.', STATUS )

*  Insert NAXIS, AXISn, and optional keywords to the header if the
*  appropriate special objects are present in the NDF.
                  CALL CON_SPHEA( NDF, FD, TYPE, 'UNFORMATTED', NFLAGS,
     :                            CMPFND, STATUS )

*  Record that the mandatory headers have been written to the output
*  file.
                  FIRST = .FALSE.

*  Use more obvious flags to indicate the certain items have been
*  written to the descriptors already.
                  AXIFND = CMPFND( 1 )
                  AXLFND = CMPFND( 2 )
                  AXUFND = CMPFND( 3 )
                  TITFND = CMPFND( 4 )
                  LABFND = CMPFND( 5 )
                  UNTFND = CMPFND( 6 )

                  IF ( INDEX( DESCR, 'SIMPLE' ) .EQ. 0 ) THEN

*  Obtain its value which was overwritten by the SIMPLE card.
                     CALL DAT_GET0C( CLOC, FITSTR, STATUS )


*  Write the original first header, now it appears after the mandatory
*  headers.
                     WRITE( LUN, IOSTAT=FIOSTA ) FITSTR
                     CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a '/
     :                 /'header record to file ^FNAME.  Reason was '/
     :                 /'^IOSTAT.', STATUS )
                  END IF

               ELSE

*  Write the ordinary FITS card to the unformatted file.
                  WRITE( LUN, IOSTAT=FIOSTA ) FITSTR
                  CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a '/
     :              /'header record to file ^FNAME.  Reason was '/
     :              /'^IOSTAT.', STATUS )
               END IF
            END IF

*  Free the locator for the next card.
            CALL DAT_ANNUL( CLOC, STATUS )

*  Avoid unnecessary looping if something has gone wrong.
            IF ( STATUS .NE. SAI__OK ) GOTO 999
         END DO

*  Append the END card image if necessary.
         IF ( END ) THEN
            FITSTR = ' '
            FITSTR( 1:3 ) = 'END'

*   Write this header to the unformatted file.
            WRITE( LUN, IOSTAT=FIOSTA ) FITSTR
            CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a header '/
     :        /'record to file ^FNAME.  Reason was ^IOSTAT.', STATUS )

         END IF

*  Tidy the locator.
         CALL DAT_ANNUL( FLOC, STATUS )

*  Write a brief FITS header.
*  ==========================
      ELSE IF ( HEADER ) THEN

*  Create the SIMPLE card.
         FITSTR = ' '
         FITSTR( 1:9 ) = 'SIMPLE  ='
         FITSTR( 30: ) = 'T / File is simple'

*  Write the FITS card header to the unformatted file.
         WRITE( LUN, IOSTAT=FIOSTA ) FITSTR
         CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a header '/
     :     /'record to file ^FNAME.  Reason was ^IOSTAT.', STATUS )

*  Insert NAXIS, AXISn, and optional keywords to the header.
         CALL CON_SPHEA( NDF, FD, TYPE, 'UNFORMATTED', NFLAGS, CMPFND,
     :                   STATUS )

*  Create the END card.
         FITSTR = ' '
         FITSTR( 1:3 ) = 'END'

*  Write the FITS card to the unformatted file.
         WRITE( LUN, IOSTAT=FIOSTA ) FITSTR
         CALL FIO_REP( LUN, ' ', FIOSTA, 'Error writing a header '/
     :     /'record to file ^FNAME.  Reason was ^IOSTAT.', STATUS )

      END IF

*  Process the input array.
*  ========================

*  Map the input data array using its actual data type.
      CALL NDF_MAP( NDF, COMP, TYPE, 'READ', PNTR, EL, STATUS )

*  Call a routine to write the data to the unformatted Fortran file;
*  the selected routine depending on the data type of the array.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL CON_OFUFB( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL CON_OFUFD( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL CON_OFUFI( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL CON_OFUFK( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL CON_OFUFR( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL CON_OFUFUB( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL CON_OFUFUW( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    NUMPRE, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL CON_OFUFW( FD, EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                   NUMPRE, STATUS )

      END IF

  999 CONTINUE

*  Close the output file.
      CALL FIO_CLOSE( FD, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF2UNF_ERR',
     :     'NDF2UNF: Error dumping an NDF array to an unformatted '/
     :     /'file.', STATUS )
      END IF

      END

