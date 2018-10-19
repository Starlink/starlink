      SUBROUTINE ASTWCSMAP( STATUS )
*+
*  Name:
*     ASTWCSMAP

*  Purpose:
*     Create a WcsMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTWCSMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new WcsMap and optionally initialises
*     its attributes.
*
*     A WcsMap is used to represent sky coordinate projections as
*     described in the (draft) FITS world coordinate system (FITS-WCS)
*     paper by E.W. Griesen and M. Calabretta (A & A, in preparation).
*     This paper defines a set of functions, or sky projections, which
*     transform longitude-latitude pairs representing spherical
*     celestial coordinates into corresponding pairs of Cartesian
*     coordinates (and vice versa).
*
*     A WcsMap is a specialised form of Mapping which implements these
*     sky projections and applies them to a specified pair of coordinates.
*     All the projections in the FITS-WCS paper are supported, plus the now
*     deprecated "TAN with polynomial correction terms" projection which
*     is refered to here by the code "TPN". Using the FITS-WCS terminology,
*     the transformation is between "native spherical" and "projection
*     plane" coordinates.  These coordinates may, optionally, be embedded
*     in a space with more than two dimensions, the remaining coordinates
*     being copied unchanged. Note, however, that for consistency with
*     other AST facilities, a WcsMap handles coordinates that represent
*     angles in radians (rather than the degrees used by FITS-WCS).
*
*     The type of FITS-WCS projection to be used and the coordinates
*     (axes) to which it applies are specified when a WcsMap is first
*     created. The projection type may subsequently be determined
*     using the WcsType attribute and the coordinates on which it acts
*     may be determined using the WcsAxis(lonlat) attribute.
*
*     Each WcsMap also allows up to 100 "projection parameters" to be
*     associated with each axis. These specify the precise form of the
*     projection, and are accessed using PVi_m attribute, where "i" is
*     the integer axis index (starting at 1), and m is an integer
*     "parameter index" in the range 0 to 99. The number of projection
*     parameters required by each projection, and their meanings, are
*     dependent upon the projection type (most projections either do not
*     use any projection parameters, or use parameters 1 and 2 associated
*     with the latitude axis). Before creating a WcsMap you should consult
*     the FITS-WCS paper for details of which projection parameters are
*     required, and which have defaults. When creating the WcsMap, you must
*     explicitly set values for all those required projection parameters
*     which do not have defaults defined in this paper.

*  Usage:
*     astwcsmap ncoord type lonax latax options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. Can be "AST", "XML",
*        "STCS", or any FitsChan encoding such as FITS-WCS. Only used
*        if the output object is written to a text file. An error is
*        reported if the output object cannot be written using the
*        requested format. ["AST"]
*     LATAX = _INTEGER (Read)
*        The index of the latitude axis. This should lie in the range
*        1 to NCOORD and be distinct from LONAX.
*     LONAX = _INTEGER (Read)
*        The index of the longitude axis. This should lie in the range
*        1 to NCOORD.
*     NCOORD = _INTEGER (Read)
*        The number of coordinate values for each point to be
*        transformed (i.e. the number of dimensions of the space in
*        which the points will reside). This must be at least 2. The
*        same number is applicable to both input and output points.
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new WcsMap.
*     RESULT = LITERAL (Read)
*        A text file to receive the new WcsMap.
*     TYPE = LITERAL (Read)
*        The type of FITS-WCS projection to apply. This should be
*        given as a string value such as "AST__TAN" (for a tangent
*        plane projection), where the characters following the double
*        underscore give the projection type code (in upper case) as
*        used in the FITS-WCS "CTYPEi" keyword. You should consult the
*        FITS-WCS paper for a list of the available projections. The
*        additional code of "AST__TPN" can be supplied which represents a
*        TAN projection with polynomial correction terms as defined in an
*        early draft of the FITS-WCS paper.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     17-MAR-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER TYPE*10
      INTEGER ITYPE
      INTEGER LATAX
      INTEGER LONAX
      INTEGER NCOORD
      INTEGER RESULT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the projection type string.
      CALL PAR_CHOIC( 'TYPE', 'AST__TAN', 'AST__AZP,AST__SZP,AST__TAN'//
     :                ',AST__STG,AST__SIN,AST__ARC,AST__ZPN,AST__ZEA,'//
     :                'AST__AIR,AST__CYP,AST__CEA,AST__CAR,AST__MER,'//
     :                'AST__SFL,AST__PAR,AST__MOL,AST__AIT,AST__COP,'//
     :                'AST__COE,AST__COD,AST__COO,AST__BON,AST__PCO,'//
     :                'AST__TSC,AST__CSC,AST__QSC,AST__NCP,AST__GLS,'//
     :                'AST__HPX,AST__TPN,AST__XPH', .FALSE., TYPE,
     :                STATUS )

*  Find the corresponding integer value.
      IF( TYPE .EQ. 'AST__AZP' ) THEN
         ITYPE = AST__AZP
      ELSE IF( TYPE .EQ. 'AST__SZP' ) THEN
         ITYPE = AST__SZP
      ELSE IF( TYPE .EQ. 'AST__TAN' ) THEN
         ITYPE = AST__TAN
      ELSE IF( TYPE .EQ. 'AST__STG' ) THEN
         ITYPE = AST__STG
      ELSE IF( TYPE .EQ. 'AST__SIN' ) THEN
         ITYPE = AST__SIN
      ELSE IF( TYPE .EQ. 'AST__ARC' ) THEN
         ITYPE = AST__ARC
      ELSE IF( TYPE .EQ. 'AST__ZPN' ) THEN
         ITYPE = AST__ZPN
      ELSE IF( TYPE .EQ. 'AST__ZEA' ) THEN
         ITYPE = AST__ZEA
      ELSE IF( TYPE .EQ. 'AST__AIR' ) THEN
         ITYPE = AST__AIR
      ELSE IF( TYPE .EQ. 'AST__CYP' ) THEN
         ITYPE = AST__CYP
      ELSE IF( TYPE .EQ. 'AST__CEA' ) THEN
         ITYPE = AST__CEA
      ELSE IF( TYPE .EQ. 'AST__CAR' ) THEN
         ITYPE = AST__CAR
      ELSE IF( TYPE .EQ. 'AST__MER' ) THEN
         ITYPE = AST__MER
      ELSE IF( TYPE .EQ. 'AST__SFL' ) THEN
         ITYPE = AST__SFL
      ELSE IF( TYPE .EQ. 'AST__PAR' ) THEN
         ITYPE = AST__PAR
      ELSE IF( TYPE .EQ. 'AST__MOL' ) THEN
         ITYPE = AST__MOL
      ELSE IF( TYPE .EQ. 'AST__AIT' ) THEN
         ITYPE = AST__AIT
      ELSE IF( TYPE .EQ. 'AST__COP' ) THEN
         ITYPE = AST__COP
      ELSE IF( TYPE .EQ. 'AST__COE' ) THEN
         ITYPE = AST__COE
      ELSE IF( TYPE .EQ. 'AST__COD' ) THEN
         ITYPE = AST__COD
      ELSE IF( TYPE .EQ. 'AST__COO' ) THEN
         ITYPE = AST__COO
      ELSE IF( TYPE .EQ. 'AST__BON' ) THEN
         ITYPE = AST__BON
      ELSE IF( TYPE .EQ. 'AST__PCO' ) THEN
         ITYPE = AST__PCO
      ELSE IF( TYPE .EQ. 'AST__TSC' ) THEN
         ITYPE = AST__TSC
      ELSE IF( TYPE .EQ. 'AST__CSC' ) THEN
         ITYPE = AST__CSC
      ELSE IF( TYPE .EQ. 'AST__QSC' ) THEN
         ITYPE = AST__QSC
      ELSE IF( TYPE .EQ. 'AST__NCP' ) THEN
         ITYPE = AST__NCP
      ELSE IF( TYPE .EQ. 'AST__GLS' ) THEN
         ITYPE = AST__GLS
      ELSE IF( TYPE .EQ. 'AST__HPX' ) THEN
         ITYPE = AST__HPX
      ELSE IF( TYPE .EQ. 'AST__TPN' ) THEN
         ITYPE = AST__TPN
      ELSE IF( TYPE .EQ. 'AST__XPH' ) THEN
         ITYPE = AST__XPH
      ELSE
         ITYPE = AST__WCSBAD
      END IF

*  Get the other required parameter values.
      CALL PAR_GET0I( 'NCOORD', NCOORD, STATUS )
      CALL PAR_GDR0I( 'LONAX', 1, 1, NCOORD, .FALSE., LONAX,
     :                STATUS )
      CALL PAR_GDR0I( 'LATAX', 2, 1, NCOORD, .FALSE., LATAX,
     :                STATUS )
      IF( LONAX .EQ. LATAX .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'LONAX and LATAX are equal.', STATUS )
      END IF

*  Create the required WcsMap.
      RESULT = AST_WCSMAP( NCOORD, ITYPE, LONAX, LATAX, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTWCSMAP_ERR', 'Error creating a new '//
     :                 'WcsMap.', STATUS )
      END IF

      END
