      SUBROUTINE COF_CAMAX( FUNIT, FILE, NDF, OBSNO, NCOL, USED,
     :                      STATUS )
*+
*  Name:
*     COF_CAMAX

*  Purpose:
*     Create an axis structure for CAM AA data within an NDF from a FITS
*     binary table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_CAMAX( FUNIT, FILE, NDF, OBSNO, NCOL, USED, STATUS )

*  Description:
*     The routine searches the CAM AA FITS binary-table columns for the
*     keywords that describe the axis structure for the given
*     observation number.  If at least one reference value, CRVALn,
*     exists then an axis component is created and filled with the
*     appropriate values.  CDELTn defines the step between axis values.
*     If it is not present in the header it is set to 1.  CRPIXn
*     defines the reference pixel to which the reference value
*     corresponds.  If is absent form the header pixel 1 is assumed to
*     be the reference pixel.  If CTYPEn is in the header it is used to
*     assigned a value to the nth axis's label component.  CTYPEn and
*     CUNITn define the axis label and units respectively.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the FITS file or device being converted.  This is
*        only used for error messages.
*     NDF = INTEGER (Given)
*        The identifier for the NDF to contain the axis structure.
*     OBSNO = INTEGER (Given)
*        The observation number.  This is equivalent to the row in the
*        binary table.
*     NCOL = INTEGER (Given)
*        Number of columns in the binary table (and dimension of USED).
*     USED( NCOL ) = INTEGER (Given and Returned)
*        A set of flags that indicate whether or not a column has been
*        used to define some attribute of the NDF.  Those columns
*        accessed in this routine will be flagged .TRUE. in this array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The CAM AA FITS products have a binary table using the "Green
*     Bank" convention, where rows of the table represent a series of
*     observations that would be equivalent to a normal simple header
*     and data unit.  Thus most of the columns have the same names as
*     the standard FITS keywords.

*  Implementation Deficienices:
*     The CD00n00m columns are ignored for the time being until there is
*     an astrometry system available.

*  Copyright:
*     Copyright (C) 1996, 2004 Central Laboratory of the Research
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
*     1996 June 17 (MJC):
*        Original version.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2008 March 15 (MJC):
*        Use KAPLIBS routine instead of its cloned CON equivalent.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER * ( * ) FILE
      INTEGER NDF
      INTEGER OBSNO
      INTEGER NCOL

*  Arguments Given & Returned:
      LOGICAL USED( NCOL )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string less
                                 ! trailing blanks

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      LOGICAL BAD                ! Column value is bad? (not used)
      CHARACTER * ( 256 ) BUFFER ! Used to form error messages
      INTEGER COLNUM             ! Column number
      CHARACTER * ( 2 ) CON      ! COlumn number
      DOUBLE PRECISION DELT      ! The co-ordinate increment between
                                 ! pixels
      INTEGER DIMS( NDF__MXDIM ) ! The dimensions of the NDF
      INTEGER EL                 ! Dimension of the current axis
      INTEGER FSTAT              ! FITSIO status
      INTEGER I                  ! Loop counter
      CHARACTER * ( 8 ) KEYWRD   ! FITS header keyword
      CHARACTER * ( 70 ) LABEL   ! Axis type equated to LABEL component
      INTEGER NC                 ! Number of characters
      INTEGER NCF                ! Number of characters in file name
      INTEGER NDIM               ! Number of dimensions of the NDF
      DOUBLE PRECISION OFFSET    ! Offset after allowing for the
                                 ! position of reference pixel
      INTEGER PNTR( 1 )          ! Pointer the mapped CENTRE component
      DOUBLE PRECISION REFP      ! Pixel position of the reference pixel
      DOUBLE PRECISION REFV      ! Co-ordinate at the reference pixel
      CHARACTER * ( 70 ) UNITS   ! Axis units equated to UNITS component

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Get the length of the filename.
      NCF = CHR_LEN( FILE )

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Obtain the dimensions of the NDF.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Loop for each dimension.
      DO I = 1, NDIM

*  Read the binary table for the axis parameters.
*  ==============================================
*
*  Note rotation cannot be mapped on to an axis structure and requires
*  a more-sophisticated astrometric system.

*  Find the column number of the CRVALn (value at the reference pixel.
         CALL FTKEYN( 'CRVAL', I, KEYWRD, FSTAT )
         CALL FTGCNO( FUNIT, .FALSE., KEYWRD, COLNUM, FSTAT )

*  Read the CRVALn value for the current observation.  Note that there
*  are no bad values.  Update the record of which columns have been
*  used.  These last two remarks apply to all the axis parameters.
         CALL FTGCVD( FUNIT, COLNUM, OBSNO, 1, 1, 0.0D0, REFV, BAD,
     :                FSTAT )
         USED( COLNUM ) = .TRUE.

*  Find the column number of the CRPIXn (the reference pixel).
         CALL FTKEYN( 'CRPIX', I, KEYWRD, FSTAT )
         CALL FTGCNO( FUNIT, .FALSE., KEYWRD, COLNUM, FSTAT )

*  Read the CRPIXn value for the current observation.
         CALL FTGCVD( FUNIT, COLNUM, OBSNO, 1, 1, 0.0D0, REFP, BAD,
     :                FSTAT )
         USED( COLNUM ) = .TRUE.

*  Find the column number of the CDELTn (the increment per pixel).
         CALL FTKEYN( 'CDELT', I, KEYWRD, FSTAT )
         CALL FTGCNO( FUNIT, .FALSE., KEYWRD, COLNUM, FSTAT )

*  Read the CRPIXn value for the current observation.
         CALL FTGCVD( FUNIT, COLNUM, OBSNO, 1, 1, 0.0D0, DELT, BAD,
     :                FSTAT )
         USED( COLNUM ) = .TRUE.

*  Find the column number of the CTYPEn (the axis label).
         CALL FTKEYN( 'CTYPE', I, KEYWRD, FSTAT )
         CALL FTGCNO( FUNIT, .FALSE., KEYWRD, COLNUM, FSTAT )

*  Read the CTYPEn value for the current observation.
         CALL FTGCVS( FUNIT, COLNUM, OBSNO, 1, 1, ' ', LABEL, BAD,
     :                FSTAT )
         USED( COLNUM ) = .TRUE.

*  Find the column number of the CUNITn (the axis units).
         CALL FTKEYN( 'CUNIT', I, KEYWRD, FSTAT )
         CALL FTGCNO( FUNIT, .FALSE., KEYWRD, COLNUM, FSTAT )

*  Read the CTYPEn value for the current observation.
         CALL FTGCVS( FUNIT, COLNUM, OBSNO, 1, 1, ' ', UNITS, BAD,
     :                FSTAT )
         USED( COLNUM ) = .TRUE.

*  Report an contextual error message if something went wrong.  Convert
*  the observation number to a string.
         IF ( FSTAT .NE. FITSOK ) THEN
            CALL CHR_ITOC( OBSNO, CON, NC )
            BUFFER = 'Error obtaining the axis parameters for FITS '/
     :        /'file '//FILE( :NCF )//', observation'//CON( :NC )//'.'
            CALL COF_FIOER( FSTAT, 'COF_CAMAX_AXCOL', 'FTGCVx',
     :                      BUFFER, STATUS )
            GOTO 999
         END IF

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Decide the data type of the axis centres.
*  =========================================

*  Find the offset for a scale-and-offset calculation of the axis
*  information.
         OFFSET = REFV - ( REFP - 1.0D0 ) * DELT

*  Map the centre array in the axis structure.  By definition the
*  processing type is single precision.
         CALL NDF_AMAP( NDF, 'Centre', I, '_REAL', 'WRITE', PNTR,
     :                  EL, STATUS )

*  Test status before accessing the pointer.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL KPG1_SSAZR( EL, DELT, OFFSET,
     :                       %VAL( CNF_PVAL( PNTR( 1 ) ) ), STATUS )

*  Unmap the axis array.
            CALL NDF_AUNMP( NDF, 'Centre', I, STATUS )
         END IF

         IF ( LABEL .NE. ' ' ) THEN

*  Find the length of the label.
            NC = CHR_LEN( LABEL )

*  Write the label to the axis structure.
            CALL NDF_ACPUT( LABEL( :NC ), NDF, 'Label', I, STATUS )
         END IF

         IF ( UNITS .NE. ' ' ) THEN

*  Find the length of the label.
            NC = CHR_LEN( UNITS )

*  Write the units to the axis structure.
            CALL NDF_ACPUT( UNITS( :NC ), NDF, 'Units', I, STATUS )
         END IF

*  End of the loop for each dimension.
      END DO

  999 CONTINUE

*  Close the new NDF context.
      CALL NDF_END( STATUS )

      END
