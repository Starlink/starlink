      SUBROUTINE CON_FITAX( NCARD, HEADER, SCARD, NDF, STATUS )
*+
*  Name:
*     CON_FITAX

*  Purpose:
*     Creates an axis structure within an NDF from FITS header
*     information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_FITAX( NCARD, HEADER, SCARD, NDF, STATUS )

*  Description:
*     The routine searches the FITSD header for the keywords that
*     describe the axis structure.  If at least one reference value,
*     CRVALn, exists then an axis component is created and filled with
*     the appropriate values.  CDELTn defines the step between axis
*     values.  If it is not present in the header it is set to 1.
*     CRPIXn defines the reference pixel to which the reference value
*     corresponds.  If is absent form the header pixel 1 is assumed to
*     be the reference pixel.  If CTYPEn is in the header it is used to
*     assign a value to the nth axis's label component, and CUNITn
*     is used to assign the units for the nth axis.
*
*     The precision of the output axis-centre array depends on the
*     absolute relative size of the offset to the scale.  Single
*     precision is used if this ratio is greater than one hundred times
*     the floating-point precision.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the buffer.
*     HEADER( NCARD ) = CHARACTER * 80 (Given)
*        The FITS header 'cards', each element corresponding to a
*        80-character card.
*     SCARD = INTEGER (Given)
*        The number of the card from where the search will begin.  This
*        is needed because the headers make contain a dummy header
*        prior to an extension.
*     NDF = INTEGER (Given)
*        The identifier for the NDF to contain the axis structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990-1992 Science & Engineering Research Council.
*     Copyright (C) 1996, 2001, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2008, 2009 Science & Technology Facilities
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
*     AJC: Alan J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1990 November 15 (MJC):
*        Original version.
*     1991 February 28 (MJC):
*        Converted BUFFER from an assumed-size to an adjustable array
*        via the NCARD argument for revised FTS1_GKEYx calls.
*     1992 July 15 (MJC):
*        Selects appropriate precision of the AXIS arrays.
*     1992 September 18 (MJC):
*        Renamed from FTS1_AXIS for CONVERT.  All FTS1_ AND KPG1 calls
*        replaced by CON_ equivalents.
*     1996 September 16 (MJC):
*        Modern style commenting and declarations.  Added support for
*        CUNITn and axis units.
*     2001 August 30 (AJC):
*        Correct CON_GKEYD arguments.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2008 March 15 (MJC):
*        Use KAPLIBS routines instead of their cloned CON equivalents.
*     2009 June 29 (MJC):
*        Replace cloned CON_GKEYx with KAPLIBS FTS1_GKEYx.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NCARD
      CHARACTER*80 HEADER( NCARD )
      INTEGER NDF
      INTEGER SCARD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string less
                                 ! trailing blanks
*  Local Constants:
      DOUBLE PRECISION PRECF     ! The factor times the machine
                                 ! precision above which the ratio of
                                 ! the scale factor to offset must lie
                                 ! for single-precision axis centres
                                 ! to be created
      PARAMETER ( PRECF = 100.0D0 )

*  Local Variables:
      CHARACTER*2 CNDIM          ! Axis number
      CHARACTER*80 COM           ! Header comment
      DOUBLE PRECISION DELT      ! Co-ordinate increment between pixels
      INTEGER DIMS( DAT__MXDIM ) ! Dimensions of the NDF
      INTEGER EL                 ! Dimension of the current axis
      INTEGER I                  ! Loop counter
      CHARACTER * ( 70 ) LABEL   ! Axis type equated to LABEL component
      LOGICAL LTHERE             ! CTYPEn keyword is present in header?
      INTEGER NC                 ! Number of characters
      INTEGER NDIM               ! Number of dimensions of the NDF
      INTEGER NKC                ! Number of the card where a FITS
                                 ! keyword was located (not used)
      DOUBLE PRECISION OFFSET    ! Offset after allowing for the
                                 ! position of reference pixel
      INTEGER PNTR( 1 )          ! Pointer the mapped CENTRE component
      DOUBLE PRECISION RATIO     ! Ratio of increment to offset of axis
      DOUBLE PRECISION REFP      ! Pixel position of the reference pixel
      DOUBLE PRECISION REFV      ! Co-ordinate at the reference pixel
      LOGICAL THERE              ! Nominated FITS keyword is present?
      CHARACTER*70 UNITS         ! Axis units
      LOGICAL UTHERE             ! CUNITn keyword is present in header?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Obtain the dimensions of the NDF.
      CALL NDF_DIM( NDF, DAT__MXDIM, DIMS, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Loop for each dimension.
      DO  I = 1, NDIM

*  Read the header for the four axis parameters.
*  =============================================
*
*  Note rotation cannot be mapped on to an axis structure and requires
*  a more-sophisticated astrometric system.

*  Create the extension to the keyword name for the current dimension.
         CALL CHR_ITOC( I, CNDIM, NC )

*  Obtain the value of the physical co-ordinate at the reference pixel.
         CALL FTS1_GKEYD( NCARD, HEADER, SCARD, 'CRVAL'//CNDIM( :NC ),
     :                    1, THERE, REFV, COM, NKC, STATUS )

*  The co-ordinate must be present to make any sense of an axis
*  structure.
         IF ( THERE ) THEN

*  Obtain the value of the element number of the reference pixel.
            CALL FTS1_GKEYD( NCARD, HEADER, SCARD, 'CRPIX'/
     :                       /CNDIM( :NC ), 1, THERE, REFP, COM, NKC,
     :                       STATUS )

*  If the keyword is not present assume the reference pixel is the
*  first along the axis.
            IF ( .NOT. THERE ) REFP = 1.0D0

*  Obtain the increment of physical co-ordinate between adjacent
*  pixels.
            CALL FTS1_GKEYD( NCARD, HEADER, SCARD, 'CDELT'/
     :                       /CNDIM( :NC ), 1, THERE, DELT, COM, NKC,
     :                       STATUS )

*  If the keyword is not present assume unit increments.
            IF ( .NOT. THERE ) DELT = 1.0D0

*  Obtain the label of the axis.  The label is not mandatory, so the
*  presence flag will only determine whether or not the axis label
*  component is written.  Unfortunately, the units may only be defined
*  in the comment of the CTYPEn keyword--- there is no CUNITn keyword.
            CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'CTYPE'/
     :                       /CNDIM( :NC ), LTHERE, LABEL, NKC, STATUS )

*  However, CUNITn looks to be used and is proposed in the WCS draft.
            CALL FTS1_GKEYC( NCARD, HEADER, SCARD, 'CUNIT'/
     :                       /CNDIM( :NC ), UTHERE, UNITS, NKC, STATUS )


*  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*  Find the offset for a scale-and-offset calculation of the axis
*  information.
            OFFSET = REFV - ( REFP - 1.0D0 ) * DELT

*  Define the criterion for deciding whether or not single-precision
*  axis centres are adequate.  It is important that the increments
*  between elements are greater than the floating-point precision.  To
*  reduce possible errors the fractional difference is scaled by a
*  constant.  However, the values may conspire to give a zero offset
*  (e.g. US-style co-ordinates of pixels).
            IF ( ABS( OFFSET ) .LT. VAL__EPSD ) THEN
               RATIO = 1.0
            ELSE
               RATIO = ABS( DELT / OFFSET )
            END IF
            IF ( RATIO .GT. DBLE( VAL__EPSR ) * PRECF ) THEN

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

            IF ( LTHERE .AND. LABEL .NE. ' ' ) THEN

*  Find the length of the label.
               NC = CHR_LEN( LABEL )

*  Write the label to the axis structure.
               CALL NDF_ACPUT( LABEL( :NC ), NDF, 'Label', I, STATUS )
            END IF

            IF ( UTHERE .AND. UNITS .NE. ' ' ) THEN

*  Find the length of the units.
               NC = CHR_LEN( UNITS )

*  Write the label to the axis structure.
               CALL NDF_ACPUT( UNITS( :NC ), NDF, 'Units', I, STATUS )
            END IF

*  End of a check for a physical co-ordinate.
         END IF

*  End of the loop for each dimension.
      END DO

  999 CONTINUE

*  Close the new NDF context.
      CALL NDF_END( STATUS )

      END
