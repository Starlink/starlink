      SUBROUTINE KPS1_RMASK( INDF, COMP, THIS, MAP, INSIDE, VAL,
     :                       STATUS )
*+
*  Name:
*     KPS1_RMASK

*  Purpose:
*     Masks an array component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_RMASK( INDF, COMP, THIS, MAP, INSIDE, VAL, STATUS )

*  Description:
*     This routine masks an array component of an NDF using an AST Region
*     to define the area to be masked.

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF to be masked.
*     COMP = CHARACTER * ( * ) (Given)
*        The name of the NDF component to be masked.
*     THIS = INTEGER (Given)
*        Pointer to a Region.
*     MAP = INTEGER (Given)
*        Pointer to a Mapping. The forward transformation should map
*	 positions in the coordinate system of the supplied Region into
*	 a coordinate system that is offset on each axis by half a pixel
*        from pixel coordinates. So, if a 2D NDF has pixel origin [i,j],
*        then the **centre** of the bottom left pixel should be
*        [REAL(I),REAL(J)]. With the usual Starlink pixel coordinate
*        system, these coordinates would be assigned to the **top left
*        corner** of the bottom right pixel.
*     INSIDE = INTEGER (Given)
*        A boolean value which indicates which pixel are to be masked. If
*	 .TRUE. is supplied, then all grid pixels with centres inside the
*	 supplied Region are assigned the value given by VAL, and all
*	 other pixels are left unchanged. If .FALSE. is supplied, then
*	 all grid pixels with centres not inside the supplied Region are
*	 assigned the value given by VAL, and all other pixels are left
*	 unchanged. Note, the Negated attribute of the Region is used to
*	 determine which pixel are inside the Region and which are
*	 outside. So the inside of a Region which has not been negated is
*	 the same as the outside of the corresponding negated Region.
*
*        For types of Region such as PointList which have zero volume,
*	 pixel centres will rarely fall exactly within the Region. For
*	 this reason, the inclusion criterion is changed for zero-volume
*	 Regions so that pixels are included (or excluded) if any part of
*	 the Region passes through the pixel. For a PointList, this means
*	 that pixels are included (or excluded) if they contain at least
*	 one of the points listed in the PointList.
*     VAL = DOUBLE PRECISION (Given)
*        This specifies the value used to flag the masked data (see INSIDE).
*        It is converted to the data type of the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-OCT-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INDF
      CHARACTER COMP * ( * )
      INTEGER THIS
      INTEGER MAP
      LOGICAL INSIDE
      DOUBLE PRECISION VAL

*  Status:
      INTEGER STATUS              ! Global status

*  External references:
      INTEGER VAL_DTOI
      REAL VAL_DTOR
      DOUBLE PRECISION VAL_DTOD
      INTEGER*2 VAL_DTOW
      INTEGER*2 VAL_DTOUW
      BYTE VAL_DTOB
      BYTE VAL_DTOUB

*  Local Variables:
      CHARACTER DTYPE*(NDF__SZFTP)! Data type for output components
      CHARACTER ITYPE*(NDF__SZTYP)! Data type for processing
      INTEGER EL                  ! No. of mapped array elements
      INTEGER IPD                 ! Pointer to mapped array
      INTEGER LBND( NDF__MXDIM )  ! Lower pixel index bounds
      INTEGER N                   ! Number of pixels flagged
      INTEGER NDIM                ! Number of pixel axes in the image
      INTEGER UBND( NDF__MXDIM )  ! Upper pixel index bounds

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Choose the data type in which to process the data.
      CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'//
     :                '_DOUBLE', INDF, INDF, COMP, ITYPE, DTYPE,
     :                STATUS )

*  Get the pixel bounds of the NDF.
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Map the required NDF array component for update.
      CALL NDF_MAP( INDF, COMP, ITYPE, 'UPDATE', IPD, EL, STATUS )

*  Call appropriate routines depending on the data type.
      IF( ITYPE .EQ. '_INTEGER' ) THEN
         N = AST_MASKI( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                  %VAL( CNF_PVAL( IPD ) ),
     :                  VAL_DTOI( .TRUE., VAL, STATUS ), STATUS )

      ELSE IF( ITYPE .EQ. '_REAL' ) THEN
         N = AST_MASKR( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                  %VAL( CNF_PVAL( IPD ) ),
     :                  VAL_DTOR( .TRUE., VAL, STATUS ), STATUS )

      ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
         N = AST_MASKD( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                  %VAL( CNF_PVAL( IPD ) ),
     :                  VAL_DTOD( .TRUE., VAL, STATUS ), STATUS )

      ELSE IF( ITYPE .EQ. '_WORD' ) THEN
         N = AST_MASKW( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                  %VAL( CNF_PVAL( IPD ) ),
     :                  VAL_DTOW( .TRUE., VAL, STATUS ), STATUS )

      ELSE IF( ITYPE .EQ. '_UWORD' ) THEN
         N = AST_MASKUW( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                   %VAL( CNF_PVAL( IPD ) ),
     :                  VAL_DTOUW( .TRUE., VAL, STATUS ), STATUS )

      ELSE IF( ITYPE .EQ. '_BYTE' ) THEN
         N = AST_MASKB( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                  %VAL( CNF_PVAL( IPD ) ),
     :                  VAL_DTOB( .TRUE., VAL, STATUS ), STATUS )

      ELSE IF( ITYPE .EQ. '_UBYTE' ) THEN
         N = AST_MASKUB( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                   %VAL( CNF_PVAL( IPD ) ),
     :                   VAL_DTOUB( .TRUE., VAL, STATUS ), STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( 'T', ITYPE )
         CALL ERR_REP( 'KPS1_RMASK_ERR1', 'KPS1_RMASK: This '//
     :                 'application does not yet support the '//
     :                 '^T data type.', STATUS )
      END IF

*  Map the NDF component.
      CALL NDF_UNMAP( INDF, COMP, STATUS )

      END

