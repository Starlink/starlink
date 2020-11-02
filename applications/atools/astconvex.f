      SUBROUTINE ASTCONVEX( STATUS )
*+
*  Name:
*     ASTCONVEX

*  Purpose:
*     Create a new Polygon representing the convex hull of a 2D data grid.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTCONVEX( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates the shortest Polygon that encloses all
*     pixels with a specified value within a 2-dimensional NDF.
*
*     By default, the returned Polygon is defined in the NDF PIXEL
*     coordinate system, but can be mapped into the current Frame of the
*     NDF using parameter CURRENT.

*  Usage:
*     astconvex value oper array result

*  ADAM Parameters:
*     ARRAY = NDF (Read)
*        A 2-dimensional NDF containing the data to be processed.
*     CURRENT = _LOGICAL (Read)
*        If TRUE, then the polygon is mapped into the current frame of
*        the supplied NDF before being returned. Otherwise, it is left in
*        PIXEL coordinates as created by the astConvex function. [FALSE]
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     NVERT = _INTEGER (Write)
*        The number of vertices in the returned polygon.
*     OPER = LITERAL (Given)
*        Indicates how the VALUE parameter is used to select the included
*        pixels. It can have any of the following values:
*        - "LT": include pixels with value less than VALUE.
*        - "LE": include pixels with value less than or equal to VALUE.
*        - "EQ": include pixels with value equal to VALUE.
*        - "NE": include pixels with value not equal to VALUE.
*        - "GE": include pixels with value greater than or equal to VALUE.
*        - "GT": include pixels with value greater than VALUE.
*     RESULT = LITERAL (Read)
*        An text file to receive the new Polygon.
*     VALUE = _DOUBLE (Read)
*        A data value that specifies the pixels to be outlined, or "bad".

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
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
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     24-FEB-2014 (DSB):
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
      LOGICAL CHR_SIMLR

*  Local Variables:
      BYTE BVAL
      BYTE UBVAL
      CHARACTER DTYPE*( NDF__SZFTP )
      CHARACTER ITYPE*( NDF__SZTYP )
      CHARACTER TEXT*40
      DOUBLE PRECISION DVAL
      INTEGER EL
      INTEGER IAST
      INTEGER INDF
      INTEGER IPDATA
      INTEGER IPIX
      INTEGER IVAL
      INTEGER LBND( 2 )
      INTEGER MAP
      INTEGER NV
      INTEGER OPER
      INTEGER RESULT
      INTEGER SDIM( 2 )
      INTEGER UBND( 2 )
      INTEGER*2 UWVAL
      INTEGER*2 WVAL
      LOGICAL BAD
      LOGICAL CURRENT
      REAL RVAL
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get an identifier for an NDF with exactly two significant pixel axes,
*  and get their bounds.
      CALL KPG1_GTNDF( 'ARRAY', 2, .TRUE., 'Read', INDF, SDIM,
     :                 LBND, UBND, STATUS )

*  Determine a data type which can be used for operations on the
*  Data components of the NDF.
      CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'//
     :                '_DOUBLE', 1, INDF, 'DATA', ITYPE, DTYPE, STATUS )

*  Get the value that defines the required Polygon as a string, and note
*  if it is "BAD".
      CALL PAR_GET0C( 'VALUE', TEXT, STATUS )
      BAD = CHR_SIMLR( TEXT, 'BAD' )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the operation used to select the required pixels.
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

*  If no error has occurred, get the VALUE parameter with the appropriate
*  data type and call the appropriate AST function to create the Polygon.
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

            RESULT = AST_CONVEXB( BVAL, OPER,
     :                             %VAL( CNF_PVAL( IPDATA )),
     :                             LBND, UBND, .TRUE., STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN

            IF( BAD ) THEN
               UBVAL = VAL__BADUB
            ELSE
               CALL PAR_GDR0I( 'VALUE', 0, INT( VAL__MINUB ),
     :                         INT( VAL__MAXUB ),
     :                         .FALSE., IVAL, STATUS )
               UBVAL = IVAL
            END IF

            RESULT = AST_CONVEXUB( UBVAL, OPER,
     :                              %VAL( CNF_PVAL( IPDATA )),
     :                              LBND, UBND, .TRUE., STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

            IF( BAD ) THEN
               WVAL = VAL__BADW
            ELSE
               CALL PAR_GDR0I( 'VALUE', 0, INT( VAL__MINW ),
     :                         INT( VAL__MAXW ),
     :                         .FALSE., IVAL, STATUS )
               WVAL = IVAL
            END IF

            RESULT = AST_CONVEXW( WVAL, OPER,
     :                             %VAL( CNF_PVAL( IPDATA )),
     :                             LBND, UBND, .TRUE., STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN

            IF( BAD ) THEN
               UWVAL = VAL__BADUW
            ELSE
               CALL PAR_GDR0I( 'VALUE', 0, INT( VAL__MINUW ),
     :                         INT( VAL__MAXUW ),
     :                         .FALSE., IVAL, STATUS )
               UWVAL = IVAL
            END IF

            RESULT = AST_CONVEXUW( UWVAL, OPER,
     :                              %VAL( CNF_PVAL( IPDATA )),
     :                              LBND, UBND, .TRUE., STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

            IF( BAD ) THEN
               IVAL = VAL__BADI
            ELSE
               CALL PAR_GET0I( 'VALUE', IVAL, STATUS )
            END IF

            RESULT = AST_CONVEXI( IVAL, OPER,
     :                             %VAL( CNF_PVAL( IPDATA )),
     :                             LBND, UBND, .TRUE., STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN

            IF( BAD ) THEN
               RVAL = VAL__BADR
            ELSE
               CALL PAR_GET0R( 'VALUE', RVAL, STATUS )
            END IF

            RESULT = AST_CONVEXR( RVAL, OPER,
     :                             %VAL( CNF_PVAL( IPDATA )),
     :                             LBND, UBND, .TRUE., STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

            IF( BAD ) THEN
               DVAL = VAL__BADD
            ELSE
               CALL PAR_GET0D( 'VALUE', DVAL, STATUS )
            END IF

            RESULT = AST_CONVEXD( DVAL, OPER,
     :                             %VAL( CNF_PVAL( IPDATA )),
     :                             LBND, UBND, .TRUE., STATUS )

         END IF

* Report the number of vertices in the polygon.
         CALL AST_GETREGIONPOINTS( RESULT, 0, 0, NV, 0, STATUS )
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETI( 'N', NV )
         CALL MSG_OUT( ' ', 'Polygon has ^N vertices', STATUS )
         CALL MSG_BLANK( STATUS )
         CALL PAR_PUT0I( 'NVERT', NV, STATUS )

*  If required, map the Polygon into the current Frame of the NDF.
         CALL PAR_GET0L( 'CURRENT', CURRENT, STATUS )
         IF( CURRENT ) THEN
            CALL KPG1_GTWCS( INDF, IAST, STATUS )
            CALL KPG1_ASFFR( IAST, 'PIXEL', IPIX, STATUS )
            MAP = AST_GETMAPPING( IAST, IPIX, AST__CURRENT, STATUS )
            RESULT = AST_MAPREGION( RESULT, MAP, IAST, STATUS )
         END IF

*  Write the results out to a text file.
         CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

      END IF

 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTCONVEX_ERR', 'Error creating a new Convex '//
     :                 'Polygon.', STATUS )
      END IF

      END
