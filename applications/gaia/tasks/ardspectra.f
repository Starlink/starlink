      SUBROUTINE ARDSPECTRA( STATUS )
*+
*  Name:
*     ARDSPECTRA

*  Purpose:
*     Extract an ARD region as a spectrum in a cube.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ARDSPECTRA( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine accepts a three dimensional NDF and extracts
*     a 1D "spectrum". The spectrum is the average of all the values
*     that within the projection of a 2D ARD description stepped along
*     the third dimension. Its use is therefore for creating spectra
*     that are averaged over a sky region.

*  Usage:
*     ARDSPECTRA IN ARD OUT

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input NDF cube.
*     FIXORIGIN = LOGICAL (Read)
*        Ignore origin information.
*     REGION = FILENAME (Read)
*        ARD description. To give a file use the indirection character
*        "^". I.e.
*           ^regions.ard
*        will read in the ARD description in the file "regions.ard".
*     OUT = NDF (Read)
*        Name for output NDF.

*  Implementation Deficiencies:
*     - Just uses pixel coordinates (no axes support).

*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 2005 Central Laboratory of the Research Councils.
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
*     07-OCT-2005 (PWD):
*        Original version. Test version for GAIA, not general enough
*        for other purposes.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'NDF_PAR'         ! NDF constants
      INCLUDE 'GRP_PAR'         ! GRP constants
      INCLUDE 'PRM_PAR'         ! PRIMDAT constants
      INCLUDE 'CNF_PAR'         ! CNF functions

*  Status:
      INTEGER STATUS            ! Global status

*  Local variables:
      CHARACTER * ( NDF__SZTYP ) TYPE ! NDF data type
      INTEGER DIMS( 3 )         ! Dimensions of input data
      INTEGER EL                ! Number of elements in input data
      INTEGER IDIN              ! Input NDF identifier
      INTEGER IDOUT             ! Output NDF identifier
      INTEGER IDSECT            ! Spectra section of input NDF
      INTEGER IGRP              ! ARD description
      INTEGER IPIN              ! Pointer to input data
      INTEGER IPMASK            ! Pointer to ARD mask
      INTEGER IPOUT             ! Pointer to output data
      INTEGER LBND( 3 )         ! Lower bounds of input data
      INTEGER LBNDE( 3 )        ! Lower bounds of ARD mask
      INTEGER LBNDI( 3 )        ! Lower bounds of ARD mask
      INTEGER LBNDS( 3 )        ! Lower bounds of spectral section
      INTEGER NDIM              ! Number of dimensions
      INTEGER REGVAL            ! ARD region value
      INTEGER UBND( 3 )         ! Upper bounds of input data
      INTEGER UBNDE( 3 )        ! Upper bounds of ARD mask
      INTEGER UBNDI( 3 )        ! Upper bounds of ARD mask
      INTEGER UBNDS( 3 )        ! Upper bounds of spectral section
      LOGICAL BAD               ! Whether input data has BAD pixels
      LOGICAL FIXORI            ! Whether to set origin to 1,1.
      REAL TRCOEF( 1 )          ! ARD description transformation
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the input NDF.
      CALL NDF_BEGIN
      CALL NDF_ASSOC( 'IN', 'READ', IDIN, STATUS )

*  Obtain the data the data type.
      CALL NDF_TYPE( IDIN, 'DATA', TYPE, STATUS )

*  Check bounds.
      CALL NDF_BOUND( IDIN, 3, LBND, UBND, NDIM, STATUS )
      IF ( NDIM .NE. 3 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'BADDIM', 'Must have three dimensions', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Dimensions of NDF.
      DIMS( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      DIMS( 2 ) = UBND( 2 ) - LBND( 2 ) + 1
      DIMS( 3 ) = UBND( 3 ) - LBND( 3 ) + 1

*  Does data have BAD pixels?
      CALL NDF_BAD( IDIN, 'DATA', .FALSE., BAD, STATUS )

*  Get the ARD descriptions that define the part of the image to
*  process.
      CALL ARD_GROUP( 'REGION', GRP__NOID, IGRP, STATUS )

*  Get output NDF. Create this as a section of the input NDF so we
*  preserve the WCS etc.
      LBNDS( 1 ) = LBND( 1 )
      LBNDS( 2 ) = LBND( 2 )
      LBNDS( 3 ) = LBND( 3 )
      UBNDS( 1 ) = LBND( 1 )
      UBNDS( 2 ) = LBND( 2 )
      UBNDS( 3 ) = UBND( 3 )
      CALL NDF_SECT( IDIN, NDIM, LBNDS, UBNDS, IDSECT, STATUS )

      CALL NDF_PROP( IDSECT, 'Quality,Units,Label,Axis,WCS', 'OUT',
     :               IDOUT, STATUS )

*  Map the data components.
      CALL NDF_MAP( IDIN, 'DATA', TYPE, 'READ', IPIN, EL, STATUS )
      CALL NDF_MAP( IDOUT, 'DATA', TYPE, 'WRITE', IPOUT, EL, STATUS )

*  Get workspace for storing mask.
      CALL PSX_CALLOC( DIMS( 1 ) * DIMS( 2 ), '_INTEGER', IPMASK,
     :                 STATUS )

*  See if we should ignore the origin information for the ARD
*  descriptions (GAIA doesn't handle this).
      CALL PAR_GET0L( 'FIXORIGIN', FIXORI, STATUS )
      IF ( FIXORI ) THEN
         LBND( 1 ) = 1
         LBND( 2 ) = 1
         LBND( 3 ) = 1
         UBND( 1 ) = DIMS( 1 )
         UBND( 2 ) = DIMS( 2 )
         UBND( 3 ) = DIMS( 3 )
      END IF

*  Convert the ARD descriptions into a MASK.
      REGVAL = 2
      TRCOEF( 1 ) = VAL__BADR
      CALL ARD_WORK( IGRP, 2, LBND, UBND, TRCOEF, .FALSE., REGVAL,
     :               %VAL( CNF_PVAL( IPMASK ) ), LBNDI, UBNDI,
     :               LBNDE, UBNDE, STATUS )

*  Now calculate the statistics.
      IF ( TYPE .EQ. '_BYTE' ) THEN
         CALL GAI1_ARDSPB( BAD, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPMASK ) ), DIMS( 1 ),
     :                     DIMS( 2 ), DIMS( 3 ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
         CALL GAI1_ARDSPUB( BAD, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPMASK ) ), DIMS( 1 ),
     :                      DIMS( 2 ), DIMS( 3 ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
         CALL GAI1_ARDSPW( BAD, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPMASK ) ), DIMS( 1 ),
     :                     DIMS( 2 ), DIMS( 3 ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
         CALL GAI1_ARDSPUW( BAD, %VAL( CNF_PVAL( IPIN ) ),
     :                      %VAL( CNF_PVAL( IPMASK ) ), DIMS( 1 ),
     :                      DIMS( 2 ), DIMS( 3 ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL GAI1_ARDSPI( BAD, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPMASK ) ), DIMS( 1 ),
     :                     DIMS( 2 ), DIMS( 3 ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_INT64' ) THEN
         CALL GAI1_ARDSPK( BAD, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPMASK ) ), DIMS( 1 ),
     :                     DIMS( 2 ), DIMS( 3 ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL GAI1_ARDSPR( BAD, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPMASK ) ), DIMS( 1 ),
     :                     DIMS( 2 ), DIMS( 3 ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL GAI1_ARDSPD( BAD, %VAL( CNF_PVAL( IPIN ) ),
     :                     %VAL( CNF_PVAL( IPMASK ) ), DIMS( 1 ),
     :                     DIMS( 2 ), DIMS( 3 ),
     :                     %VAL( CNF_PVAL( IPOUT ) ), STATUS )
      END IF

*  Free up dynamic memory.
      CALL PSX_FREE( IPMASK, STATUS )

*  Release the ARD description.
      CALL GRP_DELET( IGRP, STATUS )

*  Exit in error.
 999  CONTINUE

*  Release all NDFs.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'ARDSPECTRA_ERR',
     :    'ARDSPECTRA: Error extracting ARD spectrum.', STATUS )
      END IF

      END
