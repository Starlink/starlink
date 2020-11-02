      SUBROUTINE ASTOUTLINE( STATUS )
*+
*  Name:
*     ASTOUTLINE

*  Purpose:
*     Create a new Polygon outling values in a 2D NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTOUTLINE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a Polygon enclosing a single contiguous
*     set of pixels that have a specified value within a 2-dimensional
*     NDF.
*
*     By default, the returned Polygon is defined in the NDF PIXEL
*     coordinate system, but can be mapped into the current Frame of the
*     NDF using parameter CURRENT.
*
*     The MAXERR and MAXVERT parameters can be used to control how
*     accurately the returned Polygon represents the required region in
*     the data array. The number of vertices in the returned Polygon will
*     be the minimum needed to achieve the required accuracy.

*  Usage:
*     astoutline value oper array maxerr maxvert inside result

*  ADAM Parameters:
*     ARRAY = NDF (Read)
*        A 2-dimensional NDF containing the data to be processed.
*     CURRENT = _LOGICAL (Read)
*        If TRUE, then the polygon is mapped into the current frame of
*        the supplied NDF before being returned. Otherwise, it is left in
*        PIXEL coordinates as created by the astOutline function. [FALSE]
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     INSIDE( 2 ) = _INTEGER (Read)
*        The indices of a pixel known to be inside the required region.
*        This is needed because the supplied data array may contain several
*        disjoint areas of pixels that satisfy the criterion specified by
*        VALUE and OPER. In such cases, the area described by the returned
*        Polygon will be the one that contains the pixel specified by
*        INSIDE. If a null (!) value is supplied, or if the specified pixel
*        is outside the bounds of the NDF given by paramater ARRAY, or has
*        a value that does not meet the criterion specified by VALUE and
*        OPER, then this routine will search for a suitable pixel. The
*        search starts at the central pixel and proceeds in a spiral manner
*        until a pixel is found that meets the specified crierion.
*     MAXERR = _DOUBLE (Read)
*        Together with MAXVERT, this determines how accurately the
*        returned Polygon represents the required region of the data
*        array. It gives the maximum allowed discrepancy between the
*        returned Polygon and the accurate outline in the datta array,
*        expressed as a number of pixels. If this is zero or less, the
*        returned Polygon will have the number of vertices specified by
*        MAXVERT. Note, this value should be expressed in units of pixels
*        even if parameter CURRENT is set TRUE.
*     MAXVERT = _INTEGER (Read)
*        Together with MAXERR, this determines how accurately the returned
*        Polygon represents the required region of the data array. It gives
*        the maximum allowed number of vertices in the returned Polygon. If
*        this is less than 3, the number of vertices in the returned Polygon
*        will be the minimum needed to achieve the maximum discrepancy
*        specified by MAXERR.
*     NVERT = _INTEGER (Write)
*        The number of vertices in the returned polygon.
*     OPER = LITERAL (Given)
*        Indicates how the VALUE parameter is used to select the outlined
*        pixels. It can have any of the following values:
*        - "LT": outline pixels with value less than VALUE.
*        - "LE": outline pixels with value less than or equal to VALUE.
*        - "EQ": outline pixels with value equal to VALUE.
*        - "NE": outline pixels with value not equal to VALUE.
*        - "GE": outline pixels with value greater than or equal to VALUE.
*        - "GT": outline pixels with value greater than VALUE.
*     RESULT = LITERAL (Read)
*        An text file to receive the new Polygon.
*     VALUE = _DOUBLE (Read)
*        A data value that specifies the pixels to be outlined, or "bad".

*  Copyright:
*     Copyright (C) 2009,2014 Science & Technology Facilities Council.
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
*     2-JUN-2009 (DSB):
*        Original version.
*     10-JAN-2014 (DSB):
*        Aded parameters CURRENT and NVERT.
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
      DOUBLE PRECISION MAXERR
      DOUBLE PRECISION DVAL
      INTEGER EL
      INTEGER IAST
      INTEGER INDF
      INTEGER INSIDE( 2 )
      INTEGER IPDATA
      INTEGER IPIX
      INTEGER IVAL
      INTEGER LBND( 2 )
      INTEGER MAP
      INTEGER MAXVERT
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

*  Get the pixle indices of a point in the required polygon. If a null
*  value is supplied, annul the error and set the INSIDE array to
*  indicate a pixel outside the bounds of the NDF.
      CALL PAR_EXACI( 'INSIDE', 2, INSIDE, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         INSIDE( 1 ) = LBND( 1 ) - 1
         INSIDE( 2 ) = LBND( 2 ) - 1
      END IF

*  Get the other required parameter values.
      CALL PAR_GET0D( 'MAXERR', MAXERR, STATUS )
      CALL PAR_GET0I( 'MAXVERT', MAXVERT, STATUS )

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

            RESULT = AST_OUTLINEB( BVAL, OPER,
     :                             %VAL( CNF_PVAL( IPDATA )),
     :                             LBND, UBND, MAXERR, MAXVERT, INSIDE,
     :                             .TRUE., STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN

            IF( BAD ) THEN
               UBVAL = VAL__BADUB
            ELSE
               CALL PAR_GDR0I( 'VALUE', 0, INT( VAL__MINUB ),
     :                         INT( VAL__MAXUB ),
     :                         .FALSE., IVAL, STATUS )
               UBVAL = IVAL
            END IF

            RESULT = AST_OUTLINEUB( UBVAL, OPER,
     :                              %VAL( CNF_PVAL( IPDATA )),
     :                              LBND, UBND, MAXERR, MAXVERT, INSIDE,
     :                              .TRUE., STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

            IF( BAD ) THEN
               WVAL = VAL__BADW
            ELSE
               CALL PAR_GDR0I( 'VALUE', 0, INT( VAL__MINW ),
     :                         INT( VAL__MAXW ),
     :                         .FALSE., IVAL, STATUS )
               WVAL = IVAL
            END IF

            RESULT = AST_OUTLINEW( WVAL, OPER,
     :                             %VAL( CNF_PVAL( IPDATA )),
     :                             LBND, UBND, MAXERR, MAXVERT, INSIDE,
     :                             .TRUE., STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN

            IF( BAD ) THEN
               UWVAL = VAL__BADUW
            ELSE
               CALL PAR_GDR0I( 'VALUE', 0, INT( VAL__MINUW ),
     :                         INT( VAL__MAXUW ),
     :                         .FALSE., IVAL, STATUS )
               UWVAL = IVAL
            END IF

            RESULT = AST_OUTLINEUW( UWVAL, OPER,
     :                              %VAL( CNF_PVAL( IPDATA )),
     :                              LBND, UBND, MAXERR, MAXVERT, INSIDE,
     :                              .TRUE., STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

            IF( BAD ) THEN
               IVAL = VAL__BADI
            ELSE
               CALL PAR_GET0I( 'VALUE', IVAL, STATUS )
            END IF

            RESULT = AST_OUTLINEI( IVAL, OPER,
     :                             %VAL( CNF_PVAL( IPDATA )),
     :                             LBND, UBND, MAXERR, MAXVERT, INSIDE,
     :                             .TRUE., STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN

            IF( BAD ) THEN
               RVAL = VAL__BADR
            ELSE
               CALL PAR_GET0R( 'VALUE', RVAL, STATUS )
            END IF

            RESULT = AST_OUTLINER( RVAL, OPER,
     :                             %VAL( CNF_PVAL( IPDATA )),
     :                             LBND, UBND, MAXERR, MAXVERT, INSIDE,
     :                             .TRUE., STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

            IF( BAD ) THEN
               DVAL = VAL__BADD
            ELSE
               CALL PAR_GET0D( 'VALUE', DVAL, STATUS )
            END IF

            RESULT = AST_OUTLINED( DVAL, OPER,
     :                             %VAL( CNF_PVAL( IPDATA )),
     :                             LBND, UBND, MAXERR, MAXVERT, INSIDE,
     :                             .TRUE., STATUS )

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
         CALL ERR_REP( 'ASTOUTLINE_ERR', 'Error creating a new '//
     :                 'Polygon.', STATUS )
      END IF

      END
