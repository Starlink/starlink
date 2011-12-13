      SUBROUTINE COF_NDFAX( FUNIT, NDF, STATUS )
*+
*  Name:
*     COF_NDFAX

*  Purpose:
*     Create an axis structure within an NDF from FITS header
*     information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_NDFAX( FUNIT, NDF, STATUS )

*  Description:
*     The routine searches the FITS header for the keywords that
*     describe the axis structure.  At least one complete set of
*     keywords defining the axis must be present.  These keywords are
*     CRVALn, the reference value; CRPIXn, the reference pixel to which
*     the reference value corresponds; and CDELTn, the step size
*     between axis values.  If they all exist for axis n, then an axis
*     component is created and filled with the appropriate
*     values.  When only some of the axes have defined centres, the
*     remaining axes are assigned pixel co-ordinates.  If CTYPEn is in
*     the header it is used to assign a value to the nth axis's label
*     component.  If CUNITn is present, its value is stored in the nth
*     axis's units component.
*
*     The precision of the output axis-centre array depends on the
*     absolute relative size of the offset to the scale.  Single
*     precision is used if this ratio is greater than one hundred times
*     the floating-point precision.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     NDF = INTEGER (Given)
*        The identifier for the NDF to contain the axis structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     1996 January 25 (MJC):
*        Original version.
*     1996 September 16 (MJC):
*        Added support for CUNITn keyword.
*     1997 March 4 (MJC):
*        Creates a default axis structure for missing axes, and deletes
*        it if there are no valid axis centres created from the headers.
*        There are no defaults for CDELTn and CRPIXn; they must be
*        present to define the nth axis's axis centres.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2004 September 10 (TIMJ):
*        Fix valgrind warning by initialising strings before sending
*        them to FITSIO,
*     2008 March 15 (MJC):
*        Use KAPLIBS routines instead of their cloned CON equivalents.
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
      INTEGER NDF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string less
                                 ! trailing blanks

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )


      DOUBLE PRECISION PRECF     ! The factor times the machine
                                 ! precision above which the ratio of
                                 ! the scale factor to offset must lie
                                 ! for single-precision axis centres
                                 ! to be created
      PARAMETER ( PRECF = 100.0D0 )

*  Local Variables:
      CHARACTER * ( 1 ) CNDIM    ! The axis number
      CHARACTER * ( 48 ) COMENT  ! Keyword's comment (not used)
      DOUBLE PRECISION DELT      ! The co-ordinate increment between
                                 ! pixels
      LOGICAL DFAXIS             ! At least one axis-centre array
                                 ! defined?
      INTEGER DIMS( NDF__MXDIM ) ! The dimensions of the NDF
      INTEGER EL                 ! Dimension of the current axis
      INTEGER FSTAT              ! FITSIO status
      INTEGER I                  ! Loop counter
      CHARACTER * ( DAT__SZTYP ) ITYPE ! Data type of the axis centres
      CHARACTER * ( 8 ) KEYWRD   ! FITS keyword
      CHARACTER * ( 70 ) LABEL   ! Axis type equated to LABEL component
      INTEGER NC                 ! Number of characters in keyword
      INTEGER NCL                ! Number of characters in label
      INTEGER NCU                ! Number of characters in units
      INTEGER NDIM               ! Number of dimensions of the NDF
      INTEGER NSDIG              ! Number of significant digits
      DOUBLE PRECISION OFFSET    ! Offset after allowing for the
                                 ! position of reference pixel
      INTEGER PNTR( 1 )          ! Pointer the mapped CENTRE component

      DOUBLE PRECISION REFP      ! Pixel position of the reference pixel
      DOUBLE PRECISION REFV      ! Co-ordinate at the reference pixel

      LOGICAL THERE              ! The nominated FITS keyword is present
                                 ! in the header?
      CHARACTER * ( 70 ) UNITS   ! Axis units equated to UNITS component
      CHARACTER * ( 70 ) VALUE   ! Keyword value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Obtain the dimensions of the NDF.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Initialise
      UNITS = ' '
      VALUE = ' '
      COMENT = ' '

*  Create a default axis system in case only some of the FITS axes are
*  defined in the headers.
      CALL NDF_ACRE( NDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Initialise flag to indicate whether any axis has been written.
      DFAXIS = .FALSE.

*  Loop for each dimension.
      DO I = 1, NDIM

*  Read the header for the four axis parameters.
*  =============================================
*
*  Note rotation cannot be mapped on to an axis structure and requires
*  a more-sophisticated astrometric system.

*  Create the extension to the keyword name for the current dimension.
         CALL CHR_ITOC( I, CNDIM, NC )

*  Obtain the value of the physical co-ordinate at the reference pixel.
         KEYWRD = ' '
         KEYWRD = 'CRVAL'//CNDIM( :NC )
         CALL COF_GKEYD( FUNIT, KEYWRD, THERE, REFV, COMENT, STATUS )

*  The co-ordinate must be present to make any sense of an axis
*  structure.
         IF ( THERE ) THEN

*  Obtain the value of the element number of the reference pixel.
            KEYWRD = 'CRPIX'//CNDIM( :NC )
            CALL COF_GKEYD( FUNIT, KEYWRD, THERE, REFP,
     :                      COMENT, STATUS )

*  The reference pixel must be present to make any sense of an axis
*  structure.
            IF ( THERE ) THEN

*  Obtain the increment of physical co-ordinate between adjacent
*  pixels.  The co-ordinate increment per pixel must be present to make
*  any sense of an axis structure.
               KEYWRD = 'CDELT'//CNDIM( :NC )
               CALL COF_GKEYD( FUNIT, KEYWRD, THERE,
     :                         DELT, COMENT, STATUS )
            END IF
         END IF

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Decide the data type of the axis centres.
*  =========================================
         IF ( THERE ) THEN

*  Set the flag to indicate that at least one axis-centre array has been
*  defined.
            DFAXIS = .TRUE.

*  Find the offset for a scale-and-offset calculation of the axis
*  information.
            OFFSET = REFV - ( REFP - 1.0D0 ) * DELT

*  Determine the data type of the axis centres when there is only a
*  constant value.
            IF ( ABS( DELT ) .LT. VAL__SMLD ) THEN

*  Obtain the value as a string.  There is no need to test for an error
*  because it would have been detected earlier.
               KEYWRD = 'CRVAL'//CNDIM( :NC )
               CALL FTGKEY( FUNIT, KEYWRD, VALUE, COMENT, FSTAT )

*  Find the number of significant digits in the numerical value.
               CALL KPG1_SGDIG( VALUE, NSDIG, STATUS )

*  Determine the appropriate type by comparing the number of
*  significant digits present with the maximum number of significant
*  digits afforded by a real number.
               IF ( NSDIG .LE. -INT( LOG10( VAL__EPSR ) ) ) THEN
                  ITYPE = '_REAL'
               ELSE
                  ITYPE = '_DOUBLE'
               END IF

*  Define the criterion for deciding whether or not single-precision
*  axis centres are adequate.  It is important that the increments
*  between elements are greater than the floating-point precision.  To
*  reduce possible errors the fractional difference is scaled by a
*  constant.  However, the values may conspire to give a zero offset
*  (e.g. US-style co-ordinates of pixels).  A zero ratio is tested
*  also, since the type required for a constant depends on the number of
*  significant figures.
            ELSE
               IF ( ABS( OFFSET ) .LT. VAL__EPSD ) THEN
                  ITYPE = '_REAL'
               ELSE

                  IF ( ABS( DELT / OFFSET ) .GT. DBLE( VAL__EPSR ) *
     :                 PRECF ) THEN
                     ITYPE = '_REAL'
                  ELSE
                     ITYPE = '_DOUBLE'
                  END IF
               END IF
            END IF

            IF ( ITYPE .EQ. '_REAL' ) THEN

*  Map the centre array in the axis structure.
               CALL NDF_AMAP( NDF, 'Centre', I, '_REAL', 'WRITE', PNTR,
     :                        EL, STATUS )

*  Test status before accessing the pointer.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL KPG1_SSAZR( EL, DELT, OFFSET,
     :                             %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                             STATUS )

*  Unmap the axis array.
                  CALL NDF_AUNMP( NDF, 'Centre', I, STATUS )
               END IF

*  Double precision is required.
            ELSE

*  Ensure that the type is double precision.
               CALL NDF_ASTYP( '_DOUBLE', NDF, 'Centre', I, STATUS )

*  Map the centre array in the axis structure.
               CALL NDF_AMAP( NDF, 'Centre', I, '_DOUBLE', 'WRITE',
     :                        PNTR, EL, STATUS )

*  Test status before accessing the pointer.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL KPG1_SSAZD( EL, DELT, OFFSET,
     :                             %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                             STATUS )

*  Unmap the axis array.
                  CALL NDF_AUNMP( NDF, 'Centre', I, STATUS )
               END IF
            END IF

*  Obtain the label of the axis.  The label is not mandatory, so the
*  presence flag will only determine whether or not the axis label
*  component is written.
            KEYWRD = 'CTYPE'//CNDIM( :NC )
            CALL COF_GKEYC( FUNIT, KEYWRD, THERE, LABEL, COMENT,
     :                      STATUS )

            IF ( THERE .AND. LABEL .NE. ' ' ) THEN

*  Find the length of the label.
               NCL = CHR_LEN( LABEL )

*  Write the label to the axis structure.
               CALL NDF_ACPUT( LABEL( :NCL ), NDF, 'Label', I, STATUS )
            END IF

*  Obtain the units of the axis.  Axis units is not part of the FITS
*  standard yet, but the CUNITn keywords are widely used and they are
*  in the WCS proposal.  The presence flag will only determine whether
*  or not the axis units component is written.
            KEYWRD = 'CUNIT'//CNDIM( :NC )
            CALL COF_GKEYC( FUNIT, KEYWRD, THERE, UNITS, COMENT,
     :                      STATUS )

            IF ( THERE .AND. UNITS .NE. ' ' ) THEN

*  Find the length of the units.
               NCU = CHR_LEN( UNITS )

*  Write the units to the axis structure.
               CALL NDF_ACPUT( UNITS( :NCU ), NDF, 'Units', I, STATUS )
            END IF

*  End of a check for a physical co-ordinate.
         END IF

*  End of the loop for each dimension.
      END DO

*  Delete the axis structure if it is just the default created earlier.
      IF ( .NOT. DFAXIS ) CALL NDF_RESET( NDF, 'Axis', STATUS )

  999 CONTINUE

*  Close the new NDF context.
      CALL NDF_END( STATUS )

      END
