      SUBROUTINE ASTMASK( STATUS )
*+
*  Name:
*     ASTMASK

*  Purpose:
*     Mask a region of a data grid.

*  Language:
*     Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTMASK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application masks out regions within an NDF. It creates a copy
*     of the supplied input NDF and then modifies the copy by assigning a
*     specified value to all samples which are inside (or outside
*     if INSIDE is FALSE) the specified Region.

*  Usage:
*     astmask this map inside val in out

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF that is to be masked.
*     INSIDE = _LOGICAL (Read)
*        Indicates which pixel of the input NDF are to be masked. If TRUE is
*        supplied, then all pixels with centres inside the supplied Region
*        are assigned the value given by parameter VAL, and all other pixels
*        are left unchanged. If FALSE is supplied, then all pixels with
*        centres not inside the supplied Region are assigned the value given
c        by VAL, and all other pixels are left unchanged. Note, the Negated
*        attribute of the Region is used to determine which pixel are
*        inside the Region and which are outside. So the inside of a Region
*        which has not been negated is the same as the outside of the
*        corresponding negated Region.
*
*        For types of Region such as PointList which have zero volume,
*        pixel centres will rarely fall exactly within the Region. For
*        this reason, the inclusion criterion is changed for zero-volume
*        Regions so that pixels are included (or excluded) if any part of
*        the Region passes through the pixel. For a PointList, this means
*        that pixels are included (or excluded) if they contain at least
*        one of the points listed in the PointList.
*     MAP = LITERAL (Read)
*        An NDF or text file holding a Mapping (if an NDF is supplied,
*        the Mapping from PIXEL coordinates to the Current WCS Frame is
*        used). The inverse (note, inverse) transformation is used to map
*        positions in the coordinate system of the supplied Region into
*        PIXEL coordinates of the input NDF. A null (!) value can be supplied
*        if the coordinate system of the supplied Region corresponds to
*        PIXEL coordinates. This is equivalent to supplying a UnitMap.
*
*        The number of inputs for this Mapping (as given by its Nin
*        attribute) should match the number of axes in the supplied Region
*        (as given by the Naxes attribute of the Region). The number of
*        outputs for the Mapping (as given by its Nout attribute) should
*        match the number of pixel axes in the input NDF.
*
*        Note, in this context "pixel coordinates" are standard NDF pixel
*        coordinates. The AST_GRID function within the AST library uses a
*        slightly different form of pixel coordinates (it assumes integral
*        values are at the centre rather than the corners of each pixel)
*        and so requires a slightly different Mapping.
*
*        The suggested default is the input NDF. If accepted, this default
*        means that he supplied Region is assumed to be defined in the
*        current WCS Frame of the supplied NDF.
*     OUT = NDF (Read)
*        The output NDF.
*     RESULT = _INTEGER (Write)
*        The number of output pixels that were modified.
*     THIS = LITERAL (Read)
*        A text file holding the Region to use.
*     VAL = _DOUBLE (Read)
*        The value used to flag values in the output NDF (see parameter
*        INSIDE). This can also be "Bad".

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (Starlink, UCLan)
*     {enter_new_authors_here}

*  History:
*     7-SEP-2009 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type definitions
      IMPLICIT NONE             ! No default typing

*  Global includes
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'NDF_PAR'         ! NDF constants
      INCLUDE 'AST_PAR'         ! AST constants and function declarations
      INCLUDE 'PAR_ERR'         ! PAR errors
      INCLUDE 'DAT_PAR'         ! DAT__ constants
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function
      INCLUDE 'PRM_PAR'         ! For VAL__ constants

*  External references
      EXTERNAL AST_ISAMAPPING
      EXTERNAL AST_ISAREGION
      LOGICAL CHR_SIMLR

*  Status
      INTEGER STATUS

*  Local variables
      BYTE BVAL
      BYTE UBVAL
      CHARACTER DTYPE*( NDF__SZFTP )
      CHARACTER ITYPE*( NDF__SZTYP )
      CHARACTER PATH*255
      CHARACTER VTEXT*30
      DOUBLE PRECISION DVAL
      DOUBLE PRECISION SHIFT( NDF__MXDIM )
      INTEGER EL
      INTEGER I
      INTEGER IGRP
      INTEGER INDF1
      INTEGER INDF2
      INTEGER IPDATA
      INTEGER IVAL
      INTEGER LBND( NDF__MXDIM )
      INTEGER MAP
      INTEGER NAXREG
      INTEGER NDIM
      INTEGER NMIN
      INTEGER NMOUT
      INTEGER RESULT
      INTEGER SIZE
      INTEGER THIS
      INTEGER UBND( NDF__MXDIM )
      INTEGER*2 UWVAL
      INTEGER*2 WVAL
      LOGICAL BAD
      LOGICAL INSIDE
      REAL RVAL

*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin AST and NDF contexts
      CALL AST_BEGIN( STATUS )
      CALL NDF_BEGIN

*  Get the Region, and the number of axes.
      CALL KPG1_GTOBJ( 'THIS', 'Region', AST_ISAREGION, THIS, STATUS )
      NAXREG = AST_GETI( THIS, 'NAXES', STATUS )

*  Get the input NDF, and its path as supplied by the user (i.e. not its
*  full path).
      CALL KPG1_RGNDF( 'IN', 1, 1, ' ', IGRP, SIZE, STATUS )
      CALL NDG_NDFAS( IGRP, 1, 'Read', INDF1, STATUS )
      CALL GRP_GET( IGRP, 1, 1, PATH, STATUS )
      CALL GRP_DELET( IGRP, STATUS )

*  Set the default for the MAP parameter to be the NDF path.
      CALL PAR_DEF0C( 'MAP', PATH, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the Mapping, and the  number of inputs and outputs.
      CALL KPG1_GTOBJ( 'MAP', 'Mapping', AST_ISAMAPPING, MAP, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAP = AST_UNITMAP( NAXREG, ' ', STATUS )
         NMIN = NAXREG
         NMOUT = NAXREG

      ELSE
         CALL AST_INVERT( MAP, STATUS )
         NMIN = AST_GETI( MAP, 'NIN', STATUS )
         NMOUT = AST_GETI( MAP, 'NIN', STATUS )

*  Report an error if the number of Mapping inputs is not the same as the
*  number of axes in the Region.
         IF( NMIN .NE. NAXREG .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', NMIN )
            CALL MSG_SETI( 'M', NAXREG )
            CALL ERR_REP( ' ', 'Number of Mapping inputs (^N) is not '//
     :                    'the same as the number of Region axes (^M).',
     :                    STATUS )
         END IF
      END IF

*  Get the NDF pixel bounds, and ensure it has the correct number of pixel
*  axes.
      CALL NDF_BOUND( INDF1, NMOUT, LBND, UBND, NDIM, STATUS )
      IF( NDIM .NE. NMOUT .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N', NDIM )
         CALL MSG_SETI( 'M', NMOUT )
         CALL ERR_REP( ' ', 'Number of Mapping outputs (^M) is not '//
     :                 'the same as the number of NDF pixel axes (^N).',
     :                 STATUS )
      END IF

*  The supplied Mapping uses normal NDF pixel coords, but the AST_GRID
*  function uses FITS-like pixel coords (i.e. whole values at the pixel
*  centre). Add a half pixel shift to take account of this. Also, add on
*  the pixel origin in order to convert from GRID to PIXEL coordinates.
      DO I = 1, NDIM
         SHIFT( I ) = LBND( I ) - 1.0D0
      END DO
      MAP = AST_CMPMAP( MAP, AST_SHIFTMAP( NMOUT, SHIFT, ' ', STATUS ),
     :                  .TRUE., ' ', STATUS )

*  Create the output by propagation of everything from the input.
      CALL NDF_PROP( INDF1, 'LABEL,UNITS,AXIS,WCS,HISTORY,LABEL,'//
     :               'TITLE,DATA,QUALITY,VARIANCE', 'OUT', INDF2,
     :               STATUS )

*  Determine a data type which can be used for operations on the
*  Data components of the NDF.
      CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'//
     :                '_DOUBLE', 1, INDF2, 'DATA', ITYPE, DTYPE,
     :                STATUS )

*  Get the value that defines the required Polygon as a string, and note
*  if it is "BAD".
      CALL PAR_GET0C( 'VAL', VTEXT, STATUS )
      BAD = CHR_SIMLR( VTEXT, 'BAD' )

*  Get the INSIDE parameters.
      CALL PAR_GET0L( 'INSIDE', INSIDE, STATUS )

*  Map the output Data array for update.
      CALL NDF_MAP( INDF2, 'DATA', ITYPE, 'UPDATE', IPDATA, EL, STATUS )

*  If no error has occurred, get the VAL parameter with the appropriate
*  data type and call the appropriate AST function to mask the data.
      IF( STATUS .EQ. SAI__OK ) THEN
         IF ( ITYPE .EQ. '_BYTE' ) THEN

            IF( BAD ) THEN
               BVAL = VAL__BADB
            ELSE
               CALL PAR_GDR0I( 'VAL', 0, INT( VAL__MINB ),
     :                         INT( VAL__MAXB ),
     :                         .FALSE., IVAL, STATUS )
               BVAL = IVAL
            END IF

            RESULT = AST_MASKB( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                          %VAL( CNF_PVAL( IPDATA )), BVAL,
     :                          STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN

            IF( BAD ) THEN
               UBVAL = VAL__BADUB
            ELSE
               CALL PAR_GDR0I( 'VAL', 0, INT( VAL__MINUB ),
     :                         INT( VAL__MAXUB ),
     :                         .FALSE., IVAL, STATUS )
               UBVAL = IVAL
            END IF

            RESULT = AST_MASKUB( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                           %VAL( CNF_PVAL( IPDATA )), UBVAL,
     :                           STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

            IF( BAD ) THEN
               WVAL = VAL__BADW
            ELSE
               CALL PAR_GDR0I( 'VAL', 0, INT( VAL__MINW ),
     :                         INT( VAL__MAXW ),
     :                         .FALSE., IVAL, STATUS )
               WVAL = IVAL
            END IF

            RESULT = AST_MASKW( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                          %VAL( CNF_PVAL( IPDATA )), WVAL,
     :                          STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN

            IF( BAD ) THEN
               UWVAL = VAL__BADUW
            ELSE
               CALL PAR_GDR0I( 'VAL', 0, INT( VAL__MINUW ),
     :                         INT( VAL__MAXUW ),
     :                         .FALSE., IVAL, STATUS )
               UWVAL = IVAL
            END IF

            RESULT = AST_MASKUW( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                           %VAL( CNF_PVAL( IPDATA )), UWVAL,
     :                           STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

            IF( BAD ) THEN
               IVAL = VAL__BADI
            ELSE
               CALL PAR_GET0I( 'VAL', IVAL, STATUS )
            END IF

            RESULT = AST_MASKI( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                          %VAL( CNF_PVAL( IPDATA )), IVAL,
     :                          STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN

            IF( BAD ) THEN
               RVAL = VAL__BADR
            ELSE
               CALL PAR_GET0R( 'VAL', RVAL, STATUS )
            END IF

            RESULT = AST_MASKR( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                          %VAL( CNF_PVAL( IPDATA )), RVAL,
     :                          STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

            IF( BAD ) THEN
               DVAL = VAL__BADD
            ELSE
               CALL PAR_GET0D( 'VAL', DVAL, STATUS )
            END IF

            RESULT = AST_MASKD( THIS, MAP, INSIDE, NDIM, LBND, UBND,
     :                          %VAL( CNF_PVAL( IPDATA )), DVAL,
     :                          STATUS )

         END IF

*  Report the number of masked pixels.
         CALL MSG_SETI( 'N', RESULT )
         CALL MSG_SETC( 'V', VTEXT )
         CALL MSG_OUT( ' ', 'The value "^V" has been assigned to ^N '//
     :                 'output pixels.', STATUS )
         CALL PAR_PUT0I( 'RESULT', RESULT, STATUS )
      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

* End the AST and NDF contexts
      CALL NDF_END( STATUS )
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTMASK_ERR', 'Error masking an NDF using a '//
     :                 'Region.', STATUS )
      END IF

      END
