      SUBROUTINE COF_CAMAA( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL,
     :                      FMTCNV, STATUS )
*+
*  Name:
*     COF_CAMAA

*  Purpose:
*     Converts an ISO CAMAA binary table into a series of NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_CAMAA( FUNIT, FILE, NDF, PROFIT, LOGHDR, FDL, FMTCNV,
*                     STATUS )

*  Description:
*     This routine converts an ISO CAM AA product stored in a FITS
*     binary table into a series of NDFs.  Each NDF contains data
*     and variance arrays, axis information.  It may include an
*     optional FITS airlock extension comprising the primary headers,
*     and data from any remaining columns in a FITS-header format.
*     Some of these remaining columns overwrite the values in the
*     primary headers.  The associated integration times are stored
*     as an NDF within an extension called EXPOSURE.
*     See the "Notes" for further details.
*
*     If there is only one observation, a normal NDF is produced; if
*     there are more than one, the HDS container file of the supplied
*     NDF is used to store a series of NDFs---one for each
*     observation---called OBSn, where n is the observation number.
*     Each observation comprises three rows in the binary table
*     corrsponding to the flux, the rms errors, and the integration
*     times.

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
*        of the BSCALE and BZERO table columns.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The CAM AA FITS products have a binary table using the "Green
*     Bank" convention, where rows of the table represent a series of
*     observations, and each row is equivalent to a normal simple header
*     and data unit.  Thus most of the columns have the same names as
*     the standard FITS keywords.
*     -  The conversion from binary-table columns to NDF objects is as
*     follows:
*
*          ARRAY                  Data, error, exposure arrays depending
*                                 on the value of column TYPE
*          BLANK                  Data blank (i.e. undefined value)
*          BUNIT                  Data units
*          BSCALE                 Data scale factor
*          BZERO                  Data offset
*          CDELTn                 Pixel increment along axis n
*          CRPIXn                 Axis n reference pixel
*          CRVALn                 Axis n co-ordinate of reference pixel
*          CTYPEn                 Label for axis n.
*          CUNITn                 Units for axis n.
*          NAXIS                  Number of dimensions
*          NAXISn                 Dimension of axis n
*          remaining columns      keyword in FITS extension
*
*          The creation of axis information and extensions does not
*          occur for the error array, as these are already generated
*          when the data-array row in the binary table is processed.
*
*          The BITPIX column is ignored as the data type is determined
*          through the use the TFORMn keyword and the value of FMTCNV in
*          conjunction with the BSCALE and BZERO columns.
*
*     [optional_subroutine_items]...

*  Prior Requirements:
*     -  The FITS file should already have been opened by FITSIO, and
*     is in the HDU immediately prior to the BINTABLE extension that is
*     going to define the NDF.
*     [routine_prior_requirements]...

*  Copyright:
*     Copyright (C) 1996-1997, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2008 Science & Technology Facilities
*     Council. All Rights Reserved.

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
*     1996 June 14 (MJC):
*        Original version.
*     1997 January 24 (MJC):
*        More information about the format has come to light.  Combine
*        each set of three rows (data, error, exposure) into a single
*        NDF.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2008 June 18 (MJC):
*        Trim trailing blanks from output NDF character components.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
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
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

      INTEGER MXCOLS             ! Maximum number of columns
      PARAMETER( MXCOLS = 36 )

      INTEGER NAHEAD             ! Maximum number of additional headers
      PARAMETER( NAHEAD = 21 )

*  Local Variables:
      CHARACTER * ( 8 ) ARTYPE   ! Array type: FLUX|FLUX_ERR|EXPOSURE
      LOGICAL BAD                ! Column array contains bad values?
      INTEGER BLANK              ! Data blank (undefined) value
      REAL BSCALE                ! Data scale factor
      CHARACTER * ( 256 ) BUFFER ! Used to form error messages
      REAL BZERO                 ! Data offset
      INTEGER COLNUM             ! Column number
      CHARACTER * ( 48 ) COMENT  ! Keyword coment
      CHARACTER * ( 8 ) COMP     ! NDF array component name (error->var)
      CHARACTER * ( 2 ) CON      ! Column number
      INTEGER DATCOD             ! FITSIO data code
      INTEGER EL                 ! Number of rows in the table
      INTEGER FSTAT              ! FITSIO status
      INTEGER HDUTYP             ! HDU type (primary, IMAGE, ASCII or
                                 ! binary table)
      INTEGER HPNTR              ! Pointer to array of headers
      INTEGER I                  ! Loop counter
      INTEGER IOBS               ! Observation index number
      CHARACTER * ( NDF__SZTYP ) ITYPE ! Processing and actual type for
                                 ! a column array
      INTEGER KEYADD             ! Number of headers that can be added
      CHARACTER * ( 8 ) KEYWRD   ! FITS header keyword
      INTEGER LBND( NDF__MXDIM ) ! NDF lower bounds
      CHARACTER * ( 5 ) MCOMP    ! NDF array component name for mapping
      LOGICAL MULTIP             ! More than one observation?
      INTEGER NC                 ! Number of characters in observation
                                 ! number
      INTEGER NCF                ! Number of characters in filename
      INTEGER NCU                ! Used length of string
      CHARACTER * ( 10 ) NDFNAM  ! Component name for an NDF in a
                                 ! multiple-observation HDS file
      INTEGER NDFE               ! Effective NDF identifier
      INTEGER NDFO               ! NDF identifier for an observation
      INTEGER NDIM               ! Number of dimensions
      INTEGER NFIELD             ! Number of fields in table
      INTEGER NHEAD              ! Number of FITS headers in primary HDU
      CHARACTER * ( DAT__SZLOC ) NLOC ! Locator to NDF
      INTEGER NOBS               ! Number of observations in dataset
      INTEGER PLACE              ! NDF placeholder
      CHARACTER * ( DAT__SZLOC ) PLOC ! Locator to NDF MORE structure
      INTEGER PNTR( 1 )          ! Pointer to a mapped column array
      INTEGER REPEAT             ! Column repeat count
      LOGICAL THERE              ! Header keyword is present?
      INTEGER TNHEAD             ! Total number of FITS headers
      INTEGER TOBS               ! Number of observation set/NDF
      CHARACTER * ( 72 ) UNITS   ! Units of the data or axis array
      INTEGER UBND( NDF__MXDIM ) ! NDF upper bounds
      LOGICAL USED( MXCOLS )     ! Table column used?
      LOGICAL VALID              ! True if the NDF identifier is valid
      INTEGER WIDTH              ! Column width
      CHARACTER * ( DAT__SZLOC ) WKLOC ! Locator to header workspace
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to an NDF extension

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Get the length of the filename.
      NCF = CHR_LEN( FILE )

*  Collect the primary HDU headers when a FITS extension is needed.
*  ================================================================
      IF ( PROFIT ) THEN

*  Find the number of headers.
         CALL FTGHSP( FUNIT, NHEAD, KEYADD, FSTAT )
         IF ( FSTAT .NE. FITSOK ) THEN
            BUFFER =  'Error obtaining the number of header cards '/
     :                /'from FITS file '//FILE( :NCF )//'.'

            CALL COF_FIOER( FSTAT, 'COF_CAMAA_NHEAD', 'FTGHSP', BUFFER,
     :                      STATUS )
            GOTO 999
         END IF

*  Add the number maximum of additional cards expected and one for the
*  END card.
         TNHEAD = NHEAD + 1 + NAHEAD

*  Create workspace for the headers.
         CALL AIF_GETVM( '_CHAR*80', 1, TNHEAD, HPNTR, WKLOC, STATUS )

*  Transfer the headers from the primary HDU to the workspace.
         CALL COF_RHEAD( FUNIT, FILE, NHEAD, TNHEAD,
     :                   %VAL( CNF_PVAL( HPNTR ) ),
     :                   STATUS, %VAL( CNF_CVAL( 80 ) ) )

      END IF

*  Move to the binary table.
*  =========================

*  Skip to the next HDU.  This is defined to be a BINTABLE for the
*  CAMAA product.
      CALL FTMRHD( FUNIT, 1, HDUTYP, FSTAT )
      IF ( FSTAT .NE. FITSOK ) THEN
         BUFFER = 'Error skipping to the extension of the CAM FITS '/
     :            /'file '//FILE( :NCF )//'.'

         CALL COF_FIOER( FSTAT, 'COF_CAM_WREXT', 'FTMRHD', BUFFER,
     :                   STATUS )

      ELSE IF ( HDUTYP .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_REP( 'COF_CAM_NOBINTAB',
     :     'The first extension of ^FILE is not a BINTABLE.', STATUS )
      END IF

*  Find the shape of the binary table.
*  ===================================

*  Obtain the number of observations.
      CALL COF_GKEYI( FUNIT, 'NAXIS2', THERE, NOBS, COMENT, STATUS )

*  Obtain the number of fields in the table.
      CALL COF_GKEYI( FUNIT, 'TFIELDS', THERE, NFIELD, COMENT, STATUS )

*  Define the structure of the HDS container file.
*  ===============================================

*  In general the number of observations is not going to be one, so
*  a series of NDFs has to be made in the container file.  When the
*  number is one, then a normal NDF can be created.  This assumes that
*  the observations are stored as triplets (for the data, error, and
*  expsoure arrays).
      MULTIP = NOBS .GT. 3

      IF ( MULTIP ) THEN

*  Get a locator for the dummy NDF already created.
         CALL NDF_LOC( NDF, 'WRITE', NLOC, STATUS )

*  Delete the existing dummy data array.
         CALL DAT_ERASE( NLOC, 'DATA_ARRAY', STATUS )

      END IF

*  By definition the lower bounds are one.
      DO I = 1, NDF__MXDIM
         LBND( I ) = 1
      END DO

*  Initialise flags to indicate that none of the binary-table columns
*  has been used.
      NFIELD = MIN( NFIELD, MXCOLS )
      DO I = 1, NFIELD
         USED( I ) = .FALSE.
      END DO

*  Initialise the NDF identifier to be invalid, and the count of NDFs
*  created.
      NDFO = NDF__NOID
      TOBS = 0

*  Loop for each of the observations.
*  ==================================
      DO IOBS = 1, NOBS

*  Convert the observation number to a string.  It is needed for error
*  messages.
         CALL CHR_ITOC( IOBS, CON, NC )

*  Read the columns defining the array's shape, and scaling.
*  =========================================================

*  Do this for each row for efficiency, as there is a significant data
*  array at the end of each row.

*  Find the column number of the dimensionality.
         CALL FTGCNO( FUNIT, .FALSE., 'NAXIS', COLNUM, FSTAT )

*  Read the NAXIS value for the current observation.  Note that there
*  are no bad values.
         CALL FTGCVJ( FUNIT, COLNUM, IOBS, 1, 1, 0, NDIM, BAD, FSTAT )
         USED( COLNUM ) = .TRUE.

         DO I = 1, NDIM

*  Generate the name of the keyword for the nth axis's length.
            CALL FTKEYN( 'NAXIS', I, KEYWRD, FSTAT )

*  Find the column number of the length of the ith axis.
            CALL FTGCNO( FUNIT, .FALSE., KEYWRD, COLNUM, FSTAT )

*  Read the axis length for the current observation.  Note that there
*  are no bad values.
            CALL FTGCVJ( FUNIT, COLNUM, IOBS, 1, 1, 0, UBND( I ),
     :                   BAD, FSTAT )
            USED( COLNUM ) = .TRUE.
         END DO

*  Find the column number of the data scale factor.
         CALL FTGCNO( FUNIT, .FALSE., 'BSCALE', COLNUM, FSTAT )

*  Read the BSCALE value for the current observation.  Note that there
*  are no bad values. Handle the error in CCIM files where BSCALE can be
*  zero, but should be 1.
         CALL FTGCVE( FUNIT, COLNUM, IOBS, 1, 1, 0.0, BSCALE, BAD,
     :                FSTAT )
         IF ( BSCALE .EQ. 0.0 ) BSCALE = 1.0
         USED( COLNUM ) = .TRUE.

*  Find the column number of the data offset
         CALL FTGCNO( FUNIT, .FALSE., 'BZERO', COLNUM, FSTAT )

*  Read the BZERO value for the current observation.  Note that there
*  are no bad values.
         CALL FTGCVE( FUNIT, COLNUM, IOBS, 1, 1, 0.0, BZERO, BAD,
     :                FSTAT )
         USED( COLNUM ) = .TRUE.

*  Find the column number of the undefined value.
         CALL FTGCNO( FUNIT, .FALSE., 'BLANK', COLNUM, FSTAT )

*  Read the BLANK value for the current observation.  Note that there
*  are no bad values.
         CALL FTGCVJ( FUNIT, COLNUM, IOBS, 1, 1, 0, BLANK, BAD, FSTAT )
         USED( COLNUM ) = .TRUE.

*  Find the column number of the array type.
         CALL FTGCNO( FUNIT, .FALSE., 'TYPE', COLNUM, FSTAT )

*  Read the TYPE value for the current observation.  Note that there
*  are no bad values.
         CALL FTGCVS( FUNIT, COLNUM, IOBS, 1, 1, ' ', ARTYPE, BAD,
     :                FSTAT )
         CALL CHR_UCASE( ARTYPE )
         USED( COLNUM ) = .TRUE.

*  From the array type assign the array component name.
         IF ( ARTYPE .EQ. 'FLUX_ERR' ) THEN
            MCOMP = 'Error'
            COMP = 'Variance'
         ELSE
            MCOMP = 'Data'
            COMP = 'Data'
         END IF

*  Find the column number of the data array.
         CALL FTGCNO( FUNIT, .FALSE., 'ARRAY', COLNUM, FSTAT )
         USED( COLNUM ) = .TRUE.

*  Report an contextual error message if something went wrong.
         IF ( FSTAT .NE. FITSOK ) THEN
            BUFFER = 'Error obtaining the array shape and scaling '/
     :               /'parameters for FITS file '//FILE( :NCF )/
     :               /', observation'//CON( :NC )//'.'

            CALL COF_FIOER( FSTAT, 'COF_CAMAA_AXCOL', 'FTGCVx',
     :                      BUFFER, STATUS )
            GOTO 999
         END IF

*  Data scaling and type.
*  ======================

*  The FMTCNV flag decides whether or not the data scaling is required.
*  The FITSIO routines that obtain the data array(s) will not apply the
*  block floating-point scaling, they are not prescribed by TSCALn and
*  TZEROn keywords.  Instead the data from the BSCALE and BZERO columns
*  must be used explicitly.
         IF ( FMTCNV ) THEN

*  Scaling is to be applied. Note that this does not affect the
*  tabulated keyword in the header of the input FITS file, and that the
*  values are double precision.
            CALL FTTSCL( FUNIT, COLNUM, DBLE( BSCALE ),
     :                   DBLE( BZERO ), FSTAT )

*  By definition the type is _REAL, since the values are stored as 1E
*  in the binary table.
            ITYPE = '_REAL'

*  To prevent scaling, the scale and offset must be set to one and zero
*  respectively.  This is probably not necessary, but is included here
*  for defensive reasons.  Note that this does not affect the tabulated
*  `keywords' in the header of the input FITS file, and that the values
*  are double precision.
         ELSE
            CALL FTTSCL( FUNIT, COLNUM, 1.0D0, 0.0D0, FSTAT )

*  Set the recommended data type to a null string.  This instructs later
*  routines like COF_STYPC to use the data type specified by the FITSIO
*  data-type code (based on the TFORMn for the ARRAY column).
            ITYPE = ' '
         END IF

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            BUFFER = 'Error defaulting the scale and offset for FITS '/
     :        /'file '//FILE( :NCF )//', observation'//CON( :NC )//'.'

            CALL COF_FIOER( FSTAT, 'COF_CAMAA_SCOF', 'FTTSCL',
     :                      BUFFER, STATUS )
            GOTO 999
         END IF

*  Find the data code (effectively the data type) for the column.  Use
*  it along with the preferred data type to define the implementation
*  type.
         IF ( ITYPE .EQ. ' ' ) THEN
            CALL FTGTCL( FUNIT, COLNUM, DATCOD, REPEAT, WIDTH, FSTAT )
            CALL COF_FD2HT( DATCOD, ITYPE, STATUS )
         END IF

*  There is no TNULLn so the value of column BLANK indicates the
*  undefined value.  However, FITSIO must be told explicitely to use
*  this value.
         CALL FTTNUL( FUNIT, COLNUM, BLANK, FSTAT )

*  Report an contextual error message if something went wrong.
         IF ( FSTAT .NE. FITSOK ) THEN
            BUFFER = 'Error assigning the data-blank value for FITS '/
     :        /'file '//FILE( :NCF )//', observation'//CON( :NC )//'.'

            CALL COF_FIOER( FSTAT, 'COF_CAMAA_BLANK', 'FTTNUL',
     :                      BUFFER, STATUS )
            GOTO 999
         END IF

*  Create a NDF for a multiple-image dataset.
*  ==========================================

*  Only create an NDF when the observation is a data array.
         IF ( ARTYPE .EQ. 'FLUX' ) THEN
            IF ( MULTIP ) THEN

*  Could not annul an earlier NDF because it was needed for the error
*  and exposure arrays.  Test whether there is an open NDF (won't be
*  first time).
               CALL NDF_VALID( NDFO, VALID, STATUS )
               IF ( VALID ) CALL NDF_ANNUL( NDFO, STATUS )

*  Increment the count of the number of NDFs.
               TOBS = TOBS + 1

*  Create the name of the component.
               NDFNAM = 'OBS'
               NC = 3
               CALL CHR_PUTI( TOBS, NDFNAM, NC )

*  Create new NDF placeholder.
               CALL NDF_PLACE( NLOC, NDFNAM, PLACE, STATUS )

*  Create the new NDF using the placeholder.
               CALL NDF_NEW( ITYPE, NDIM, LBND, UBND, PLACE, NDFO,
     :                       STATUS )

*  Use the existing NDF.
            ELSE

*  Clone the input NDF identifier.
               CALL NDF_CLONE( NDF, NDFO, STATUS )

*  Set the type of the NDF.
               CALL NDF_STYPE( ITYPE, NDFO, 'Data', STATUS )

*  Set the bounds of the NDF.
               CALL NDF_SBND( NDIM, LBND, UBND, NDFO, STATUS )
            END IF

*  Select the NDF identifier to use.
            NDFE = NDFO

*  Create an extension for the exposures.  Store as an NDF.
*  ========================================================
         ELSE IF ( ARTYPE .EQ. 'EXPOSURE' ) THEN

*  Create an extension of type NDF.
            CALL NDF_XNEW( NDFO, 'EXPOSURE', 'NDF', 0, 0, XLOC, STATUS )

*  Find the parent structure.
            CALL DAT_PAREN( XLOC, PLOC, STATUS )

*  Create a new NDF in the extension via an NDF placeholder.  The data
*  type and bounds will be changed below once they are known.
            CALL NDF_PLACE( PLOC, 'EXPOSURE', PLACE, STATUS )
            CALL NDF_NEW( ITYPE, NDIM, LBND, UBND, PLACE, NDFE,
     :                    STATUS )

*  Need to set the data type of the variance array in a single NDF.
         ELSE IF ( ARTYPE .EQ. 'FLUX_ERR' .AND. .NOT. MULTIP ) THEN

*  Set the type of the variance array.
            CALL NDF_STYPE( ITYPE, NDFO, 'Variance', STATUS )

         END IF

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the flux and store as the data array.
*  =============================================

*  Map the NDF component.
         CALL NDF_MAP( NDFE, MCOMP, ITYPE, 'WRITE', PNTR, EL, STATUS )

*  Read the column into the data array.  Call the appropriate routine
*  for the chosen type.
         IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL FTGCVB( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADUB,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL FTGCVI( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADW,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL FTGCVJ( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADI,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL FTGCVE( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADR,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL FTGCVD( FUNIT, COLNUM, IOBS, 1, EL, VAL__BADD,
     :                   %VAL( CNF_PVAL( PNTR( 1 ) ) ), BAD, FSTAT )

         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'IT', ITYPE )
            CALL MSG_SETC( 'CON', CON( :NC ) )
            CALL ERR_REP( 'COF_CAMAA_ITYPE',
     :        'Invalid data type (^IT) selected for the CAM flux '/
     :        /'array in observation ^CON.', STATUS )
            GOTO 999
         END IF

*  Set the bad-pixel flag.
         CALL NDF_SBAD( BAD, NDFE, COMP, STATUS )

*  Unmap the data array.
         CALL NDF_UNMAP( NDFE, COMP, STATUS )

*  Check that the transfer was correct.
         IF ( FSTAT .NE. FITSOK ) THEN
            BUFFER = 'Error copying the CAM flux to the NDF data '/
     :               /'array in observation '//CON( :NC )//'.'
            CALL COF_FIOER( FSTAT, 'COF_CAMAA_CRDAT', 'FTGCVx', BUFFER,
     :                      STATUS )
            GOTO 999
         END IF

*  Data Units and Label.
*  =====================

         IF ( ARTYPE .NE. 'FLUX_ERR' ) THEN

*  Find the column number of the data units.
            CALL FTGCNO( FUNIT, .FALSE., 'BUNIT', COLNUM, FSTAT )

*  Read the BUNITS value for the current observation.  Note that there
*  are no bad values.
            CALL FTGCVS( FUNIT, COLNUM, IOBS, 1, 1, ' ', UNITS, BAD,
     :                   FSTAT )
            USED( COLNUM ) = .TRUE.

*  Report a contextual error message if something went wrong.
            IF ( FSTAT .NE. FITSOK ) THEN
               BUFFER = 'Error obtaining the array units for FITS '/
     :                  /'file '//FILE( :NCF )//', observation'/
     :                  /CON( :NC )//'.'
               CALL COF_FIOER( FSTAT, 'COF_CAMAA_AXCOL', 'FTGCVx',
     :                         BUFFER, STATUS )
               GOTO 999
            END IF

            IF ( UNITS .NE. ' ' ) THEN
               NCU = CHR_LEN( UNITS )
               CALL NDF_CPUT( UNITS( :NCU ), NDFE, 'Units', STATUS )
            END IF

*  Set the label for the NDF.
            IF ( ARTYPE .EQ. 'EXPOSURE' ) THEN
               CALL NDF_CPUT( 'Exposure time', NDFE, 'Label', STATUS )
            ELSE
               CALL NDF_CPUT( 'Flux', NDFE, 'Label', STATUS )
            END IF

            IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Create the AXIS structure.
*  ==========================
            CALL COF_CAMAX( FUNIT, FILE, NDFE, IOBS, MXCOLS, USED,
     :                      STATUS )

*  Exclude unwanted columns.
*  =========================

*  Find the column number of the unwanted ancillary data.
            CALL FTGCNO( FUNIT, .FALSE., 'BITPIX', COLNUM, FSTAT )

*  Record that this column is used.
            USED( COLNUM ) = .TRUE.

*  Create the FITS extension.
*  ==========================
            IF ( PROFIT .OR. LOGHDR ) THEN

*  Merge the data in the unused columns for this observation with the
*  primary HDU and a trailing END card added.  This is to give the
*  appearance of being one of a series of simple FITS files.  The
*  merged headers are written to the FITS extension if one is desired.
               CALL COF_WGBFE( FUNIT, FILE, NDFE, PROFIT, IOBS, NFIELD,
     :                         USED, NHEAD, TNHEAD,
     :                         %VAL( CNF_PVAL( HPNTR ) ),
     :                         STATUS, %VAL( CNF_CVAL( 80 ) ) )
         END IF

*  Write out the headers to a logfile.  This also needs the FITS file
*  name and HDU number (1) for the heading, and the length of the FITS
*  header card as the array is mapped.
            IF ( LOGHDR )
     :        CALL COF_HALOG( FDL, TNHEAD, %VAL( CNF_PVAL( HPNTR ) ),
     :                        FILE, 1,
     :                        STATUS, %VAL( CNF_CVAL( 80 ) ) )

*  Tidy the NDF and locators need to make the extension.
            IF ( ARTYPE .EQ. 'EXPOSURE' ) THEN
               CALL DAT_ANNUL( XLOC, STATUS )
               CALL DAT_ANNUL( PLOC, STATUS )
               CALL NDF_ANNUL( NDFE, STATUS )
            END IF
         END IF

      END DO

*  Could not annul an earlier NDF because it was needed for the error
*  and exposure arrays.  Test whether there is an open NDF (should
*  always be so, but no harm to be defensive).
      CALL NDF_VALID( NDFO, VALID, STATUS )
      IF ( VALID ) CALL NDF_ANNUL( NDFO, STATUS )

*  Tidy the workspace.
      CALL DAT_ANNUL( WKLOC, STATUS )

  999 CONTINUE

      END

