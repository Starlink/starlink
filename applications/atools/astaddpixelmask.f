      SUBROUTINE ASTADDPIXELMASK( STATUS )
*+
*  Name:
*     ASTADDPIXELMASK

*  Purpose:
*     Add a set of pixels to a Moc

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTADDPIXELMASK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application modifies a Moc by combining it with a subset of
*     the pixel positions contained within a supplied 2-dimensional NDF.
*     The current WCS Frame of the NDF must be a SkyFrame or a CmpFrame
*     containing a SkyFrame.
*
*     The subset of pixels to be combined with the Moc are selected using
*     the VALUE and OPER parameters. The way in which the existing Moc
*     and the selected pixels are combined together is determined by the
*     CMODE parameter.
*
*     An adaptive alogorithm is used to find the HEALPix cells that are
*     inside the selected area in the pixel array. An initial grid,
*     corresponding to the HEALPix cells at the order given by the Moc's
*     "MinOrder" attribute, is placed over the pixel array. Each of these
*     cells is tested at 9 positions (corners, edge-centres and cell-centre).
*     If all 9 positions are inside the selected area of pixels, then the
*     whole cell is assumed to be inside. If no positions are inside the
*     selected area, then the whole cell is assumed to be outside. If there
*     is a mix of inside and outside positions, the cell is divided into
*     four sub-cells at HEALPix order "MinOrder+1", and the same test is
*     applied to each sub-cell in turn. When the HEALPix order reaches the
*     value of the Moc's "MaxOrder" attribute, each cell is tested only at
*     the cell centre, and is assumed to be inside the selected area if the
*     cell centre is inside the selected area.
*
*     This process means that contiguous "islands" or "holes" in the
*     supplied pixel mask may be missed if they are smaller than the cell
*     size associated with HEALPix order "MinOrder".
*
*     If no value has yet been set for the MaxOrder attribute, then this
*     function will automatically set it to the smallest value that results
*     in the cells in the Moc being no larger than half the size of the pixels
*     in the centre of the array.

*  Usage:
*     astaddpixelmask this cmode value oper array result

*  ADAM Parameters:
*     ARRAY = NDF (Read)
*        A 2-dimensional NDF containing the data to be processed.
*     CMODE = LITERAL (Read)
*        Indicates how the Moc and select pixels are to be combined. Any
*        of the following values may be supplied:
*        - "AND": The modified Moc is the intersection of the original
*        Moc and the selected pixels.
*        - "OR": The modified Moc is the union of the original Moc and
*        the selected pixels.
*        - "XOR": The modified Moc is the exclusive disjunction of the
*        original Moc and the selected pixels.
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     OPER = LITERAL (Read)
*        Indicates how the VALUE parameter is used to select the outlined
*        pixels. It can have any of the following values:
*        - "LT": outline pixels with value less than VALUE.
*        - "LE": outline pixels with value less than or equal to VALUE.
*        - "EQ": outline pixels with value equal to VALUE.
*        - "NE": outline pixels with value not equal to VALUE.
*        - "GE": outline pixels with value greater than or equal to VALUE.
*        - "GT": outline pixels with value greater than VALUE.
*     RESULT = LITERAL (Read)
*        A file to receive the modified Moc. If it ends with ".fit", or
*        ".fits" (case insensitive), then a FITS file will be created
*        with the given name and the modified Moc is written out as a
*        binary table to an extension called "BINTABLE" in the named
*        FITS file. Any pre-existing FITS file is first deleted.
*     THIS = LITERAL (Read)
*        The Moc to be modified.
*     VALUE = _DOUBLE (Read)
*        A data value that specifies the pixels to be outlined, or "bad".

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     10-DEC-2018 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'CNF_PAR'          ! CNF constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AST_ISAMOC
      LOGICAL CHR_SIMLR

*  Local Variables:
      BYTE BVAL
      BYTE UBVAL
      CHARACTER DTYPE*( NDF__SZFTP )
      CHARACTER ITYPE*( NDF__SZTYP )
      CHARACTER TEXT*40
      DOUBLE PRECISION DVAL
      INTEGER CMODE
      INTEGER DIMS( 2 )
      INTEGER EL
      INTEGER FLAGS
      INTEGER INDF
      INTEGER IPDATA
      INTEGER IPIX
      INTEGER IVAL
      INTEGER IWCS
      INTEGER LBND( 2 )
      INTEGER MAP
      INTEGER OPER
      INTEGER RESULT
      INTEGER SDIM( 2 )
      INTEGER THIS
      INTEGER UBND( 2 )
      INTEGER*2 UWVAL
      INTEGER*2 WVAL
      LOGICAL BAD
      REAL RVAL
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get the Moc to modify.
      CALL KPG1_GTOBJ( 'THIS', 'Moc', AST_ISAMOC, THIS, STATUS )

*  Get an identifier for an NDF with exactly two significant pixel axes,
*  and get their bounds.
      CALL NDF_ASSOC( 'ARRAY', 'Read', INDF, STATUS )
      CALL KPG1_ASGET( INDF, 2, .FALSE., .FALSE., .TRUE., SDIM,
     :                 LBND, UBND, IWCS, STATUS )
      DIMS( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      DIMS( 2 ) = UBND( 2 ) - LBND( 2 ) + 1

*  Get the WCS FrameSet.
      CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Determine a data type which can be used for operations on the
*  Data components of the NDF.
      CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'//
     :                '_DOUBLE', 1, INDF, 'DATA', ITYPE, DTYPE, STATUS )

*  Get the value that defines the required area as a string, and note
*  if it is "BAD".
      CALL PAR_GET0C( 'VALUE', TEXT, STATUS )
      BAD = CHR_SIMLR( TEXT, 'BAD' )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the other required parameter values.
      CALL PAR_CHOIC( 'CMODE', 'OR', 'AND,OR,XOR', .FALSE., TEXT,
     :                STATUS )
      IF( TEXT .EQ. 'AND' ) THEN
         CMODE = AST__AND
      ELSE IF( TEXT .EQ. 'XOR' ) THEN
         CMODE = AST__XOR
      ELSE
         CMODE = AST__OR
      END IF

      CALL PAR_CHOIC( 'OPER', 'EQ', 'LT,LE,EQ,NE,GE,GT', .FALSE., TEXT,
     :                STATUS )
      IF( TEXT .EQ. 'LT' ) THEN
         OPER = AST__LT
      ELSE IF( TEXT .EQ. 'LE' ) THEN
         OPER = AST__LE
      ELSE IF( TEXT .EQ. 'EQ' ) THEN
         OPER = AST__EQ
      ELSE IF( TEXT .EQ. 'NE' ) THEN
         OPER = AST__NE
      ELSE IF( TEXT .EQ. 'GE' ) THEN
         OPER = AST__GE
      ELSE
         OPER = AST__GT
      END IF

*  Map the Data array of the input NDF.
      CALL NDF_MAP( INDF, 'DATA', ITYPE, 'READ', IPDATA, EL, STATUS )

*  Get the flags to use with the AST_ADDPIXELMASK function.
      FLAGS = AST__USEBAD
      IF( BAD ) FLAGS = 0

*  If no error has occurred, get the VALUE parameter with the appropriate
*  data type and call the appropriate AST function to modify the Moc.
      IF( STATUS .EQ. SAI__OK ) THEN
         IF ( ITYPE .EQ. '_BYTE' ) THEN

            IF( BAD ) THEN
               BVAL = VAL__BADB
            ELSE
               CALL PAR_GDR0I( 'VALUE', 0, INT( VAL__MINB ),
     :                         INT( VAL__MAXB ),
     :                         .FALSE., IVAL, STATUS )
               BVAL = IVAL
            END IF

            CALL AST_ADDPIXELMASKB( THIS, CMODE, IWCS, BVAL,
     :                              OPER, FLAGS, VAL__BADB,
     :                              %VAL( CNF_PVAL( IPDATA )), DIMS,
     :                              STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN

            IF( BAD ) THEN
               UBVAL = VAL__BADUB
            ELSE
               CALL PAR_GDR0I( 'VALUE', 0, INT( VAL__MINUB ),
     :                         INT( VAL__MAXUB ),
     :                         .FALSE., IVAL, STATUS )
               UBVAL = IVAL
            END IF

            CALL AST_ADDPIXELMASKUB( THIS, CMODE, IWCS, UBVAL,
     :                               OPER, FLAGS, VAL__BADUB,
     :                               %VAL( CNF_PVAL( IPDATA )), DIMS,
     :                               STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

            IF( BAD ) THEN
               WVAL = VAL__BADW
            ELSE
               CALL PAR_GDR0I( 'VALUE', 0, INT( VAL__MINW ),
     :                         INT( VAL__MAXW ),
     :                         .FALSE., IVAL, STATUS )
               WVAL = IVAL
            END IF

            CALL AST_ADDPIXELMASKW( THIS, CMODE, IWCS, WVAL,
     :                              OPER, FLAGS, VAL__BADW,
     :                              %VAL( CNF_PVAL( IPDATA )), DIMS,
     :                              STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN

            IF( BAD ) THEN
               UWVAL = VAL__BADUW
            ELSE
               CALL PAR_GDR0I( 'VALUE', 0, INT( VAL__MINUW ),
     :                         INT( VAL__MAXUW ),
     :                         .FALSE., IVAL, STATUS )
               UWVAL = IVAL
            END IF

            CALL AST_ADDPIXELMASKUW( THIS, CMODE, IWCS, UWVAL,
     :                               OPER, FLAGS, VAL__BADUW,
     :                               %VAL( CNF_PVAL( IPDATA )), DIMS,
     :                               STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

            IF( BAD ) THEN
               IVAL = VAL__BADI
            ELSE
               CALL PAR_GET0I( 'VALUE', IVAL, STATUS )
            END IF

            CALL AST_ADDPIXELMASKI( THIS, CMODE, IWCS, IVAL,
     :                              OPER, FLAGS, VAL__BADI,
     :                              %VAL( CNF_PVAL( IPDATA )), DIMS,
     :                              STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN

            IF( BAD ) THEN
               RVAL = VAL__BADR
            ELSE
               CALL PAR_GET0R( 'VALUE', RVAL, STATUS )
            END IF

            CALL AST_ADDPIXELMASKR( THIS, CMODE, IWCS, RVAL,
     :                              OPER, FLAGS, VAL__BADR,
     :                              %VAL( CNF_PVAL( IPDATA )), DIMS,
     :                              STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

            IF( BAD ) THEN
               DVAL = VAL__BADD
            ELSE
               CALL PAR_GET0D( 'VALUE', DVAL, STATUS )
            END IF

            CALL AST_ADDPIXELMASKD( THIS, CMODE, IWCS, DVAL,
     :                              OPER, FLAGS, VAL__BADD,
     :                              %VAL( CNF_PVAL( IPDATA )), DIMS,
     :                              STATUS )

         END IF

*  Write the results out to a text file.
         CALL ATL1_PTOBJ( 'RESULT', 'THIS', THIS, STATUS )

      END IF

 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTADDPXMASK_ERR', 'Error adding a pixel mask'//
     :                 ' to a Moc.', STATUS )
      END IF

      END
