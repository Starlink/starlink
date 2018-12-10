      SUBROUTINE ASTMOC( STATUS )
*+
*  Name:
*     ASTMOC

*  Purpose:
*     Create a Moc.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTMOC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This command creates a new Moc object and optionally initialises
*     its attributes.
*
*     The Moc class uses the IVOA MOC (Multi-Order Coverage) recommendation
*     to describe a region on the sky. The region is made up of an
*     arbitrary collection of cells from the HEALPix sky tessellation,
*     and thus may represent any area on the sky, subject to the
*     constraint that the edges of the area correspond to edges of the
*     HEALPix cells. See the MOC recommendation for further information
*     (http://www.ivoa.net/documents/MOC/).
*
*     The Moc class describes an arbitrary collection of cells on the sky,
*     whereas other subclasses of Region describe exact geometric shapes
*     in any arbitrary domain. This results in some differences between
*     Mocs and other types of Region, the main one being that Mocs have
*     no associated uncertainty.
*
*     The MOC recommendation requires that a MOC always describes a sky
*     area using the ICRS coordinate system. However, the Moc class, like
*     other subclasses of Region, allows its attributes to be changed so
*     that it represents the equivalent area in any celestial coordinate
*     system that can be mapped to ICRS. See attribute Adaptive.
*
*     In practice, to use this class an empty Moc object (i.e. a Moc
*     describing a null area of the sky) should first be created using the
*     this command. Areas of the sky should then be added into the empty
*     Moc using one or more of the class methods.
*
*     If it is required to write a Moc out to a FITS binary table, the
*     data value and headers to put in the table can be obtained using
*     methods astgetmocdata and astgetmocheader. The MOC described by
*     an existing FITS binary table can be added into a Moc object using
*     the astAddMocData method.
*
*     Note, this class is limited to MOCs for which the number of cells
*     in the normalised MOC can be represented in a four byte signed integer.
*     No support is yet provided for the JSON or ASCII formats described
*     in the MOC reommendation.

*  Usage:
*     astmoc options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. Can be "AST", "XML",
*        "STCS", or any FitsChan encoding such as FITS-WCS. Only used
*        if the output object is written to a text file. An error is
*        reported if the output object cannot be written using the
*        requested format. ["AST"]
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new Moc.
*     RESULT = LITERAL (Read)
*        A text file to receive the new Moc.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory.
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
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER RESULT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Create the required Moc.
      RESULT = AST_MOC( ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTMOC_ERR', 'Error creating a new Moc.',
     :                 STATUS )
      END IF

      END
