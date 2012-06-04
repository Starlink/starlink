      SUBROUTINE AUTOCROP( STATUS )
*+
*  Name:
*     AUTOCROP

*  Purpose:
*     Copies an image cropping off excess bad pixels.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AUTOCROP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine copies an input image to an output image. The output
*     image consists of the rectangular region of the original that
*     contains all the non-bad pixels. Thus any unnecessary regions
*     in the original image are removed reducing the disk space
*     necessary to store the image.

*  Usage:
*     autocrop in out

*  ADAM Parameters:
*     IN = IMAGE (Read)
*        The input image.
*     OUT = IMAGE (Read)
*        The output image.

*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  History:
*     2-DEC-1996 (PWD):
*        Original version.
*     15-MAY-1998 (PWD):
*        Added WCS to components that are propagated to new NDF.
*     02-SEP-2004 (PWD):
*        Converted to use CNF_PVAL for pointers.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'NDF_PAR'         ! NDF constants
      INCLUDE 'CNF_PAR'         ! CNF functions

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) TYPE ! Input NDF data type
      INTEGER EL                ! Number of pixels in input NDF
      INTEGER IDIN              ! Input NDF identifier
      INTEGER IDOUT             ! Output NDF identifier
      INTEGER IDSEC             ! NDF section identifier
      INTEGER IPIN              ! Pointer to input DATA
      INTEGER LBND( 2 )         ! Lower bounds of input NDF
      INTEGER LBNDS( 2 )        ! Lower bounds of NDF section
      INTEGER NDIM              ! Number of dimensions of input NDF
      INTEGER UBND( 2 )         ! Upper bounds of input NDF
      INTEGER UBNDS( 2 )        ! Upper bounds of NDF section
      INTEGER XDIM              ! X dimension of input NDF
      INTEGER YDIM              ! Y dimension of input NDF
      INTEGER XDIMS             ! X dimension of NDF section
      INTEGER YDIMS             ! Y dimension of NDF section
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the input NDF.
      CALL NDF_BEGIN
      CALL NDF_ASSOC( 'IN', 'READ', IDIN, STATUS )
      CALL NDF_BOUND( IDIN, 2, LBND, UBND, NDIM, STATUS )
      CALL NDF_TYPE( IDIN, 'DATA', TYPE, STATUS )
      CALL NDF_MAP( IDIN, 'DATA', TYPE, 'READ', IPIN, EL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      XDIM = UBND( 1 ) - LBND( 1 ) + 1
      YDIM = UBND( 2 ) - LBND( 2 ) + 1

*  Now find the region that contains valid pixels.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL RTD1_GBNDB( %VAL( CNF_PVAL( IPIN ) ), XDIM, YDIM,
     :                    LBNDS, UBNDS, STATUS )
      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL RTD1_GBNDUB( %VAL( CNF_PVAL( IPIN ) ), XDIM, YDIM,
     :                     LBNDS, UBNDS, STATUS )
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL RTD1_GBNDW( %VAL( CNF_PVAL( IPIN ) ), XDIM, YDIM,
     :                    LBNDS, UBNDS, STATUS )
      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL RTD1_GBNDUW( %VAL( CNF_PVAL( IPIN ) ), XDIM, YDIM,
     :                     LBNDS, UBNDS, STATUS )
      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL RTD1_GBNDR( %VAL( CNF_PVAL( IPIN ) ), XDIM, YDIM,
     :                    LBNDS, UBNDS, STATUS )
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL RTD1_GBNDI( %VAL( CNF_PVAL( IPIN ) ), XDIM, YDIM,
     :                    LBNDS, UBNDS, STATUS )
      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL RTD1_GBNDK( %VAL( CNF_PVAL( IPIN ) ), XDIM, YDIM,
     :                    LBNDS, UBNDS, STATUS )
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL RTD1_GBNDD( %VAL( CNF_PVAL( IPIN ) ), XDIM, YDIM,
     :                    LBNDS, UBNDS, STATUS )
      END IF

*  Create the new section.
      LBNDS( 1 ) = LBNDS( 1 ) + LBND( 1 ) - 1
      LBNDS( 2 ) = LBNDS( 2 ) + LBND( 2 ) - 1
      UBNDS( 1 ) = UBNDS( 1 ) + LBND( 1 ) - 1
      UBNDS( 2 ) = UBNDS( 2 ) + LBND( 2 ) - 1
      XDIMS = UBNDS( 1 ) - LBNDS( 1 ) + 1
      YDIMS = UBNDS( 2 ) - LBNDS( 2 ) + 1
      CALL NDF_SECT( IDIN, 2, LBNDS, UBNDS, IDSEC, STATUS )

*  And copy this to an output NDF.
      CALL NDF_PROP( IDSEC, 'Title,Label,Units,Data,Variance,'//
     :               'Quality,Axis,History,WCS', 'OUT', IDOUT, STATUS )

*  Write a bit about the process.
      CALL MSG_BLANK( STATUS )
      CALL MSG_SETI( 'XDIM', XDIM )
      CALL MSG_SETI( 'YDIM', YDIM )
      CALL MSG_OUT( ' ', '  Input image size: ^XDIM,^YDIM', STATUS )
      CALL MSG_SETI( 'LBND1', LBND( 1 ) )
      CALL MSG_SETI( 'UBND1', UBND( 1 ) )
      CALL MSG_SETI( 'LBND2', LBND( 2 ) )
      CALL MSG_SETI( 'UBND2', UBND( 2 ) )
      CALL MSG_OUT( ' ',
     : '  Input image bounds: (^LBND1:^UBND1,^LBND2:^UBND2)', STATUS )
      CALL MSG_BLANK( STATUS )
      CALL MSG_SETI( 'XDIM', XDIMS )
      CALL MSG_SETI( 'YDIM', YDIMS )
      CALL MSG_OUT( ' ', '  Autocropped image size: ^XDIM,^YDIM',
     :              STATUS )
      CALL MSG_SETI( 'LBNDS1', LBNDS( 1 ) )
      CALL MSG_SETI( 'UBNDS1', UBNDS( 1 ) )
      CALL MSG_SETI( 'LBNDS2', LBNDS( 2 ) )
      CALL MSG_SETI( 'UBNDS2', UBNDS( 2 ) )
      CALL MSG_OUT( ' ',
     : '  Autocropped image bounds: (^LBNDS1:^UBNDS1,^LBNDS2:^UBNDS2)',
     :     STATUS )
      CALL MSG_BLANK( STATUS )
      IF ( XDIM .EQ. XDIMS .AND. YDIM .EQ. YDIMS ) THEN
         CALL MSG_OUT( ' ', '  ***No data cropped.', STATUS )
      END IF

*  Exit in error label.
 99   CONTINUE
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'AUTOCROP_ERR',
     :        'AUTOCROP: Error cropping image.', STATUS )
      END IF

      END
* @(#)autocrop.f   1.1   96/12/03 16:44:13   96/12/03 16:44:34
