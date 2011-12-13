      SUBROUTINE COF_IUESI( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL,
     :                      FMTCNV, STATUS )
*+
*  Name:
*     COF_IUESI

*  Purpose:
*     Converts an IUE SI data product into an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_IUESI( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL, FMTCNV,
*                     STATUS )

*  Description:
*     This routine converts an IUE LI or SI product stored as a FITS
*     primary data array and IMAGE extension containing the quality
*     into an NDF.  Other FITS headers are used to create AXIS
*     structures (SI only), and character components.  The primary HDU
*     headers may be written to the standard FITS airlock extension.
*     Only the most-significant 8 bits of the quality flags are
*     transferred to the NDF.  See the "Notes" for further details.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        Logical-unit number of the FITS file.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the FITS file or device being converted.  This is
*        only used for error messages.
*     NDF = INTEGER (Given)
*        The NDF identifier of the output NDF.
*     PROFIT = LOGICAL (Given)
*        If .TRUE., the FITS headers are written to the NDF's FITS
*        extension.
*     LOGHDR = LOGICAL (Given)
*        If .TRUE., a record of the FITS headers is written to a log
*        file given by descriptor FDL.  If .FALSE., no log is made and
*        argument FDL is ignored.
*     FDL = INTEGER (Given)
*        The file descriptor for the log file.  This is ignored when
*        LOGHDR is .FALSE..
*     FMTCNV = LOGICAL (Given)
*        This specifies whether or not format conversion will occur.
*        The conversion applies the values of the FITS `keywords'
*        BSCALE and BZERO  to the FITS data to generate the "true" data
*        values.  Keywords is in quotes because the scale and offset
*        are actually in columns of the binary table called BSCALE and
*        BZERO.
*
*        If FMTCNV=.FALSE., the HDS type of the data array in the NDF
*        will be the equivalent of the FITS data format (e.g.
*        BITPIX = 16 creates a _WORD array).  If FMTCNV=.TRUE., the
*        data array in the NDF will be converted from the FITS data
*        type on tape to _REAL or _DOUBLE in the NDF.  The selection
*        of the floating-point type is equivalent to the data types
*        of the BSCALE and BZERO values in the header.
*
*        FMTCNV = .TRUE. is recommended.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Details of the conversion are:
*     -  The primary data array of the FITS file becomes NDF main data
*     array.  The value of argument FMTCNV controls whether keywords
*     BSCALE and BZERO are applied to scale the data; FMTCNV along with
*     the number of significant characters in the keywords decide the
*     data type of the array.  It is expected that this will be _REAL
*     if FMTCNV is .TRUE., and _WORD otherwise.
*     -  The quality array comes from the IMAGE extension of the FITS
*     file.  The 2's complement values are divided by -128 to obtain
*     the most-significant 8 bits of the 14 in use.  There is no check
*     that the dimension and axis-defining FITS headers in this
*     extension match those of the main data array.  The standard
*     indicates that they will be the same.
*     -  The FILENAME header value becomes the NDF's TITLE component.
*     -  The BUNIT header value becomes the NDF's UNITS component.
*     -  The CDELTn, CRPIXn, and CRVALn define the axis centres.  CTYPEn
*     defines the axis labels.
*     -  The primary headers may be written to the NDF's FITS extension.

*  Implementation Deficiencies:
*     - There is no propagation of HISTORY cards in the FITS header to
*     NDF history records.
*     - There is no support for FITS World Co-ordinate Systems.

*  Prior Requirements:
*     -  The FITS file should already have been opened by FITSIO, and
*     is in the HDU immediately prior to the BINTABLE extension that is
*     going to define the NDF.
*     [routine_prior_requirements]...

*  Copyright:
*     Copyright (C) 1996, 1999, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     1996 June 29 (MJC):
*        Original version.
*     8-JAN-1999 (DSB):
*        Added FMTCNV to argument list for COF_STYPE call.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER * ( * ) FILE
      INTEGER NDF
      LOGICAL PROFIT
      LOGICAL LOGHDR
      INTEGER FDL
      LOGICAL FMTCNV

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER   FITSOK           ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      LOGICAL BAD                ! True if bad values may be present in
                                 ! array
      INTEGER BITPIX             ! FITS file's BITPIX
      CHARACTER * ( 256 ) BUFFER ! Used to form error messages
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      LOGICAL DARRAY             ! True if the current HDU contains a
                                 ! data array
      INTEGER DIMS( NDF__MXDIM ) ! NDF dimensions
      INTEGER EL                 ! Number of elements in array
      LOGICAL EXTEND             ! Value of FITS EXTEND keyword
      INTEGER FSTAT              ! FITSIO error status
      INTEGER GCOUNT             ! Value of FITS GCOUNT keyword
      INTEGER HDUTYP             ! HDU type (primary, IMAGE, ASCII or
                                 ! binary table)
      INTEGER I                  ! Loop counter
      CHARACTER * ( NDF__SZTYP ) ITYPE ! NDF implementation data type
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      INTEGER NC                 ! Number of characters
      INTEGER NCF                ! Number of characters in filename
      INTEGER NDIM               ! Number of dimensions
      LOGICAL NONSDA             ! True if the current HDU contains a
                                 ! non-standard data array
      INTEGER PCOUNT             ! Value of FITS PCOUNT keyword
      INTEGER PNTR( 1 )          ! Pointer to NDF array
      LOGICAL SIMPLE             ! True if the FITS file is simple
      LOGICAL THERE              ! Keyword is present?
      CHARACTER * ( 13 ) TITLE   ! NDF title
      CHARACTER * ( DAT__SZNAM ) TYPE ! Data type
      CHARACTER * ( DAT__SZLOC ) WLOC ! Locator to workspace
      INTEGER WPNTR              ! Pointer to workspace
      INTEGER * 2 WVALUE         ! A word value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Obtain the length of the filename.
      NCF = CHR_LEN( FILE )

*  Report the full set of headers and/or write to NDF's FITS extension.
*  ====================================================================
*
*  This is slightly less efficient than combining the two operations,
*  but re-using subroutines does make the code easier to follow.

*  Read the main header into the FITS extension of the NDF.  The FITS
*  headers for the random groups will appear in each group NDF.
      IF ( PROFIT ) CALL COF_WFEXT( FUNIT, NDF, 0, 0, FILE, STATUS )

*  Write out the headers to a logfile, if desired.
      IF ( LOGHDR ) CALL COF_HDLOG( FUNIT, FDL, FILE, 1, STATUS )

*  Data scaling.
*  =============

*  The FMTCNV flag decides whether or not the data scaling is required.
*  The FITSIO routines that obtain the data array(s) will apply the
*  block floating-point scaling as prescribed by the BSCALE and BZERO
*  keywords.
      IF ( FMTCNV ) THEN

*  Scaling is to be applied.  Find the data type required for the
*  output array based upon the number of significant digits in the
*  BSCALE and BZERO keywords.  If these have values of 1.0D0 and 0.0D0
*  respectively either explicitly, or because one or both are absent,
*  then the data type can be set to the null string.  This instructs
*  later routines like COF_STYPC to use the data type specified by the
*  FITSIO data-type code (based on BITPIX).
         CALL COF_DSTYP( FUNIT, 'BSCALE', 'BZERO', TYPE, STATUS )

*  To prevent scaling, the scale and offset must be set to
*  one and zero respectively.  Note that this does not affect the
*  keywords in the header of the input FITS file.  Note that the values
*  are double precision.
      ELSE
         CALL FTPSCL( FUNIT, 1.0D0, 0.0D0, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            BUFFER = 'Error defaulting the scale and '/
     :               /'offset for FITS file '//FILE( :NCF )//'.'
            CALL COF_FIOER( FSTAT, 'COF_IUESI_SCOF', 'FTPSCL',
     :                      BUFFER, STATUS )
            GOTO 999
         END IF

*  Set the recommended data type to a null string.  This instructs later
*  routines like COF_STYPC to use the data type specified by the FITSIO
*  data-type code (based on BITPIX).
         TYPE = ' '
      END IF

*  Determine the main properties of the FITS object.
*  =================================================

*  Get the mandatory headers of the primary HDU.
      CALL COF_MANDH( FUNIT, .TRUE., 2, SIMPLE, BITPIX, NDIM, DIMS,
     :                PCOUNT, GCOUNT, EXTEND, DARRAY, NONSDA, EL,
     :                STATUS )

*  Report the error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         BUFFER = 'Error occurred during accessing headers in the '/
     :            /'primary header and data unit of FITS file '/
     :            /FILE( :NCF )//'.'
         CALL ERR_REP( 'COF_IUESI_MANDH', BUFFER, STATUS )
         GOTO 999
      END IF

*  Cannot processed non-standard files.
      IF ( .NOT. SIMPLE ) THEN
         STATUS = SAI__ERROR
         BUFFER = 'The FITS file '//FILE( :NCF )//' is not simple and '/
     :            /'therefore cannot be processed.'
         CALL ERR_REP( 'COF_IUESI_NOTSIM', BUFFER, STATUS )
         GOTO 999
      END IF

*  Modify the shape and type of the NDF, now that it is known.
*  ===========================================================

*  Test whether or not there is a data array present.
      IF ( DARRAY ) THEN

*  Set the data type of the data array.
         CALL COF_STYPE( NDF, 'Data', TYPE, BITPIX, FMTCNV, ITYPE,
     :                   STATUS )

*  Set the shape of the NDF.
         DO I = 1, NDIM
            LBND( I ) = 1
         END DO
         CALL NDF_SBND( NDIM, LBND, DIMS, NDF, STATUS )

*  Copy the data values into the array component.
*  ==============================================

*  First map the input array component with the desired data type.
*  Any type conversion will be performed by the FITSIO array-reading
*  routine.
         CALL NDF_MAP( NDF, 'Data', ITYPE, 'WRITE', PNTR, EL, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Call the appropriate routine for the data type of the created
*  array.  The group is 0, and we always start at the first element.
*  Remember that the input BITPIX values for floating point are one
*  minus the true BITPIX (the non-standard values were needed to
*  determine whether or not scaling was required).  The arrays may have
*  bad pixels.
         IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL FTGPVB( FUNIT, 0, 1, EL, VAL__BADUB,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL FTGPVI( FUNIT, 0, 1, EL, VAL__BADW,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL FTGPVJ( FUNIT, 0, 1, EL, VAL__BADI,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL FTGPVE( FUNIT, 0, 1, EL, VAL__BADR,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL FTGPVD( FUNIT, 0, 1, EL, VAL__BADD,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            BUFFER = 'Error reading the data array in FITS file '/
     :               /FILE( :NCF )//'.'
            CALL COF_FIOER( FSTAT, 'COF_IUESI_READ', 'FTGPVx',
     :                      BUFFER, STATUS )
            CALL NDF_UNMAP( NDF, 'Data', STATUS )
            GOTO 999
         END IF

*  Set the bad-pixel flag.
         CALL NDF_SBAD( BAD, NDF, 'Data', STATUS )

*  The header is only a dummy, so fill the array with bad values by
*  mapping with the appropriate initialisation.  The values will be
*  returned to the output NDF when the array component is unmapped.
      ELSE
         CALL NDF_MAP( NDF, 'Data', '_REAL', 'WRITE/BAD',
     :                 PNTR, EL, STATUS )

      END IF

*  Unmap the array.
      CALL NDF_UNMAP( NDF, 'Data', STATUS )

*  Other components.
*  =================

*  Create the NDF character components from the FITS headers.
      CALL COF_NDFCC( FUNIT, NDF, STATUS )

*  Override any existing title, though the OBJECT header is not expected
*  from the IUEFA documentation.  Instead use the FILENAME.
      CALL COF_GKEYC( FUNIT, 'FILENAME', THERE, TITLE, COMENT, STATUS )

*  If it is present, set the NDF title, truncating unnecessary blanks.
      IF ( THERE .AND. TITLE .NE. ' ' ) THEN
         NC = CHR_LEN( TITLE )
         CALL NDF_CPUT( TITLE( :NC ), NDF, 'Title', STATUS )
      END IF

*  Create the NDF AXIS structure from the FITS headers.
      CALL COF_NDFAX( FUNIT, NDF, STATUS )

*  Move to the IMAGE extension.
*  ============================

*  Skip to the next HDU.
      CALL FTMRHD( FUNIT, 1, HDUTYP, FSTAT )
      IF ( FSTAT .NE. FITSOK ) THEN

*  Report the error, if it is not the expected end of file (error 107)
*  or has some garbage at the end (unrecognisable FITS record, error
*  252).  In the latter case just annul the error and remove the FITSIO
*  error messages from the stack.
         IF ( FSTAT .EQ. 107 .OR. FSTAT .EQ. 252 ) THEN
            CALL FTCMSG
            FSTAT = FITSOK
         ELSE
            STATUS = SAI__ERROR
            BUFFER = 'Error skipping to the extension of the '/
     :               /'FITS file '//FILE( :NCF )//'.'
            CALL ERR_REP( 'COF_IUESI_WREXT', BUFFER, STATUS )
            CALL COF_FIOER( FSTAT, 'COF_IUESI_WREXT', 'FTMRHD',
     :                      ' ', STATUS )
         END IF
         GOTO 999

*  The next HDU is defined to be an IMAGE for the IUE LI and SI
*  products.
      ELSE IF ( HDUTYP .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_REP( 'COF_IUESI_NOIMAGE',
     :     'The first extension of ^FILE is not IMAGE.', STATUS )
      END IF

*  Copy the quality values into the array component.
*  =================================================

*  Get some workspace for the 16-bit 2's complement quality data.
      CALL AIF_GETVM( '_WORD', 1, EL, WPNTR, WLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( WLOC, STATUS )
         GOTO 999
      END IF

*  Read the IMAGE extension array into the workspace.  By definition
*  there can be no bad values (hence fifth argument is zero).
      WVALUE = 0
      CALL FTGPVI( FUNIT, 0, 1, EL, WVALUE, %VAL( CNF_PVAL( WPNTR ) ),
     :             BAD, FSTAT )

*  Map the input array component with the desired data type.  Any type
*  conversion will be performed by the FITSIO array-reading routine.
      CALL NDF_MAP( NDF, 'Quality', '_UBYTE', 'WRITE', PNTR, EL,
     :              STATUS )

*  Transfer the most-significant IUE quality flags across to the NDF's
*  QUALITY component.
      CALL COF_IUEQ( EL, %VAL( CNF_PVAL( WPNTR ) ),
     :               %VAL( CNF_PVAL( PNTR( 1 ) ) ), STATUS )

*  Tidy up the workspace and quality.
      CALL DAT_ANNUL( WLOC, STATUS )
      CALL NDF_UNMAP( NDF, 'Quality', STATUS )

  999 CONTINUE

      END
