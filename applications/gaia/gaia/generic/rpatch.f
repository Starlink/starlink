      SUBROUTINE RPATCH( NDFID, TYPE, SCALE, NFIT, FITFIL, NEWFIL,
     :                   USEVAR, NX, NY, XLOW, YLOW, XHIGH, YHIGH,
     :                   HVQUAL, IPQUAL, IPIN, STATUS )
*     +
*  Name:
*     RPATCH

*  Purpose:
*     Replaces a region of an image with a patch.

*  Description:
*     This routine process a given image replacing any elements of an
*     image that are within a given ARD region by those derived from a
*     fit of a surface to another ARD region.  The replaced values have
*     a noise distribution added so that they look cosmetically correct.
*     If given then an associated quality array will have its BADBITS
*     reset.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL RPATCH( NDFID, TYPE, SCALE, NFIT, FITFIL, NEWFIL, USEVAR
*                   NX, NY, XLOW, YLOW, XHIGH, YHIGH, HVQUAL, IPQUAL,
*                   IPIN, STATUS )

*  Arguments:
*     NDFID = INTEGER (Given)
*        The original NDF data file identifier. Used to map in variance
*        if not set to 0.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the image. In a form understood by NDF.
*     SCALE = REAL (Given)
*        Scaling factor for standard deviation estimates.
*     NFIT = INTEGER (Given)
*        Order of fit.
*     FITFIL = CHARACTER * ( * ) (Given)
*        Name of file containing ARD region to use for fit.
*     NEWFIL = CHARACTER * ( * ) (Given)
*        Name of file containing ARD region to patch.
*     USEVAR = LOGICAL (Given)
*        Whether to use variance component if present.
*     NX = INTEGER (Given)
*        Size of image in X.
*     NY = INTEGER (Given)
*        Size of image in Y.
*     XLOW = INTEGER (Given)
*        Lower bound in pixel indices of region to process.
*     YLOW = INTEGER (Given)
*        Lower bound in pixel indices of region to process.
*     XHIGH = INTEGER (Given)
*        Upper bound in pixel indices of region to process.
*     YHIGH = INTEGER (Given)
*        Upper bound in pixel indices of region to process.
*     HVQUAL = LOGICAL (Given)
*        If .TRUE. then a quality array, which is the same size as the
*        image data, has been passed. Any pixels which are fitted should
*        have their BADBITS bits reset.
*     IPQUAL = INTEGER (Given)
*        Pointer to any quality data (same dimension as IPIN data).
*     IPIN = INTEGER (Given)
*        Pointer to the image data to be modified (usually a section of
*        the total image).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine is designed to be called from RTD, the real-time
*     display tool and is not intended for use in other ways.

*  Copyright:
*     Copyright (C) 1996-2001 Central Laboratory of the Research Councils
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


*  Authors:
*     PWD: P.W. Draper (STARLINK-Durham University)
*     {enter_new_authors_here}

*  History:
*     14-JUN-1996 (PWD):
*        Original version based on ADAM task NPATCH.
*     18-JUN-1996 (PWD):
*        Added code to use an input image section. Needed as
*        optimisation for speed improvements.
*     17-NOV-1997 (PWD):
*        Added code to deal with quality arrays.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'GRP_PAR'         ! GRP parameters (used by ARD)
      INCLUDE 'FIO_ERR'         ! FIO status constants

*  Arguments Given:
      INTEGER NDFID
      CHARACTER * ( * ) TYPE
      REAL SCALE
      INTEGER NFIT
      CHARACTER * ( * ) FITFIL
      CHARACTER * ( * ) NEWFIL
      LOGICAL USEVAR
      INTEGER NX
      INTEGER NY
      INTEGER XLOW
      INTEGER YLOW
      INTEGER XHIGH
      INTEGER YHIGH
      LOGICAL HVQUAL
      INTEGER IPQUAL
      INTEGER IPIN

*  Status:
      INTEGER STATUS            ! Global status

*  Local variables:
      CHARACTER * ( GRP__SZGEX ) BUFFER ! Input buffer
      INTEGER EL                ! Number of pixels in variance
      INTEGER FDFIT             ! FIO file descriptor
      INTEGER FDNEW             ! FIO file descriptor
      INTEGER FITGRP            ! ARD description
      INTEGER IDSECT            ! Sectioned NDF if available
      INTEGER IPVAR             ! Pointer to variance component
      INTEGER LBND( 2 )         ! Lower bounds of NDF
      INTEGER LBNDS( 2 )        ! Lower bounds of NDF section
      INTEGER NDIM              ! Number of NDF dimensions
      INTEGER NLINES            ! Number of lines in input file
      INTEGER NEWGRP            ! ARD description
      INTEGER UBND( 2 )         ! Upper bounds of NDF
      INTEGER UBNDS( 2 )        ! Upper bounds of NDF section
      LOGICAL FLAG              ! Dummy
      LOGICAL HAVVAR            ! TRUE when variance is available
      BYTE BADBIT               ! BADBITS of NDF quality
*.

*  Set the inherited global status.
      STATUS = SAI__OK

*  Initialise bounds of image section and presence of variance
*  component.
      IF ( NDFID .NE. 0 ) THEN
         CALL NDF_BOUND( NDFID, 2, LBND, UBND, NDIM, STATUS )
         LBNDS( 1 ) = LBND( 1 ) + XLOW - 1
         LBNDS( 2 ) = LBND( 2 ) + YLOW - 1
         UBNDS( 1 ) = LBND( 1 ) + XHIGH - 1
         UBNDS( 2 ) = LBND( 2 ) + YHIGH - 1
      ELSE
         LBNDS( 1 ) = XLOW
         LBNDS( 2 ) = YLOW
         UBNDS( 1 ) = XHIGH
         UBNDS( 2 ) = YHIGH
      END IF
      HAVVAR = .FALSE.

*  Read in the ARD regions.
      CALL ERR_MARK
      FITGRP = GRP__NOID
      NLINES = 0
      CALL FIO_OPEN( FITFIL, 'READ', 'LIST', 0, FDFIT, STATUS )
 1    CONTINUE
         CALL FIO_READF( FDFIT, BUFFER, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Add line to ARD description.
            CALL ARD_GRPEX( BUFFER, GRP__NOID, FITGRP, FLAG, STATUS )
            NLINES = NLINES + 1
         ELSE
            IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )
            CALL FIO_CLOSE( FDFIT, STATUS )
            GO TO 2
         END IF
         GO TO 1
 2    CONTINUE
      CALL ERR_RLSE
      IF ( NLINES .EQ. 0 ) THEN
         IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         CALL MSG_SETC( 'FITFIL', FITFIL )
         CALL ERR_REP( 'RPATCH_NOLINES',
     :   'Input file (^FITFIL) does not contain an ARD description',
     :   STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Same for second description.
      CALL ERR_MARK
      NEWGRP = GRP__NOID
      CALL FIO_OPEN( NEWFIL, 'READ', 'LIST', 0, FDNEW, STATUS )
 3    CONTINUE
         CALL FIO_READF( FDNEW, BUFFER, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Add line to ARD description.
            CALL ARD_GRPEX( BUFFER, GRP__NOID, NEWGRP, FLAG, STATUS )
         ELSE
            IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )
            CALL FIO_CLOSE( FDNEW, STATUS )
            GO TO 4
         END IF
         GO TO 3
 4    CONTINUE
      CALL ERR_RLSE
      IF ( NLINES .EQ. 0 ) THEN
         IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         CALL MSG_SETC( 'NEWFIL', FITFIL )
         CALL ERR_REP( 'RPATCH_NOLINES',
     :   'Input file (^NEWFIL) does not contain an ARD description',
     :   STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Check for variance component if data is an NDF. If found map it in.
      IF ( NDFID .NE. 0 ) THEN
         CALL NDF_STATE( NDFID, 'VARIANCE', HAVVAR, STATUS )
         IF ( HAVVAR .AND. USEVAR ) THEN

*  Make section same size as image.
            CALL NDF_SECT( NDFID, 2, LBNDS, UBNDS, IDSECT, STATUS )
            CALL NDF_MAP( IDSECT, 'VARIANCE', TYPE, 'READ', IPVAR,
     :                    EL, STATUS )
         ELSE
            HAVVAR = .FALSE.
            IPVAR = 0
         END IF
      END IF

*  If given some quality then get the associated BADBITS value.
      IF ( HVQUAL ) CALL NDF_BB( NDFID, BADBIT, STATUS )

*  And apply patch to data.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL RTD1_PATCH( FITGRP, NEWGRP, SCALE, NFIT, LBNDS, UBNDS,
     :                    NX, NY, TYPE, HAVVAR, IPVAR, HVQUAL, IPQUAL,
     :                    BADBIT, IPIN, STATUS )
      END IF
      IF ( HAVVAR .AND. USEVAR ) THEN
         CALL NDF_ANNUL( IDSECT, STATUS )
      END IF

*  Exit in error label. All clean up code goes after here.
 99   CONTINUE

*  Release the ARD descriptions.
      CALL GRP_DELET( FITGRP, STATUS )
      CALL GRP_DELET( NEWGRP, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PATCH_ERR','RPATCH: failed to patch image.',
     :   STATUS )
      END IF

      END

