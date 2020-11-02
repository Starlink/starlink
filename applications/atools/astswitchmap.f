      SUBROUTINE ASTSWITCHMAP( STATUS )
*+
*  Name:
*     ASTSWITCHMAP

*  Purpose:
*     Create a SwitchMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTSWITCHMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new SwitchMap and optionally initialises its
*     attributes. An option is provided to create a SwitchMap from an
*     output file created by FIGARO:IARC (see parameter IARCFILE).
*
*     A SwitchMap is a Mapping which represents a set of alternate
*     Mappings, each of which is used to transform positions within a
*     particular region of the input or output coordinate system of the
*     SwitchMap.
*
*     A SwitchMap can encapsulate any number of Mappings, but they must
*     all have the same number of inputs (Nin attribute value) and the
*     same number of outputs (Nout attribute value). The SwitchMap itself
*     inherits these same values for its Nin and Nout attributes. Each of
*     these Mappings represents a "route" through the switch, and are
*     referred to as "route" Mappings below. Each route Mapping transforms
*     positions between the input and output coordinate space of the entire
*     SwitchMap, but only one Mapping will be used to transform any given
*     position. The selection of the appropriate route Mapping to use with
*     any given input position is made by another Mapping, called the
*     "selector" Mapping. Each SwitchMap encapsulates two selector
*     Mappings in addition to its route Mappings; one for use with the
*     SwitchMap's forward transformation (called the "forward selector
*     Mapping"), and one for use with the SwitchMap's inverse transformation
*     (called the "inverse selector Mapping"). The forward selector Mapping
*     must have the same number of inputs as the route Mappings, but
*     should have only one output. Likewise, the inverse selector Mapping
*     must have the same number of outputs as the route Mappings, but
*     should have only one input.
*
*     When the SwitchMap is used to transform a position in the forward
*     direction (from input to output), each supplied input position is
*     first transformed by the forward transformation of the forward selector
*     Mapping. This produces a single output value for each input position
*     referred to as the selector value. The nearest integer to the selector
*     value is found, and is used to index the array of route Mappings (the
*     first supplied route Mapping has index 1, the second route Mapping has
*     index 2, etc). If the nearest integer to the selector value is less
*     than 1 or greater than the number of route Mappings, then the SwitchMap
*     output position is set to a value of AST__BAD on every axis. Otherwise,
*     the forward transformation of the selected route Mapping is used to
*     transform the supplied input position to produce the SwitchMap output
*     position.
*
*     When the SwitchMap is used to transform a position in the inverse
*     direction (from "output" to "input"), each supplied "output" position
*     is first transformed by the inverse transformation of the inverse
*     selector Mapping. This produces a selector value for each "output"
*     position. Again, the nearest integer to the selector value is found,
*     and is used to index the array of route Mappings. If this selector
*     index value is within the bounds of the array of route Mappings, then
*     the inverse transformation of the selected route Mapping is used to
*     transform the supplied "output" position to produce the SwitchMap
*     "input" position. If the selector index value is outside the bounds
*     of the array of route Mappings, then the SwitchMap "input" position is
*     set to a value of AST__BAD on every axis.
*
*     In practice, appropriate selector Mappings should be chosen to
*     associate a different route Mapping with each region of coordinate
*     space. Note that the SelectorMap class of Mapping is particularly
*     appropriate for this purpose.
*
*     If a compound Mapping contains a SwitchMap in series with its own
*     inverse, the combination of the two adjacent SwitchMaps will be
*     replaced by a UnitMap when the compound Mapping is simplified using
*     astsimplify.

*  Usage:
*     astswitchmap fsmap ismap route1 route2 options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     FSMAP = LITERAL (Read)
*        An NDF or text file holding the forward selector Mapping. If an NDF
*        is supplied, the Mapping from the Base Frame to the Current Frame
*        of its WCS FrameSet will be used. The supplied Mapping must have a
*        defined forward transformation, but need not have a defined
*        inverse transformation. It must have one output, and the number of
*        inputs must match the number of inputs of each of the supplied
*        route Mappings. A null (!) value may be supplied, in which case the
*        SwitchMap will have an undefined forward Mapping. This parameter
*        is only used if a null value is supplied for IARCFILE.
*     IARCFILE = LITERAL (Read)
*        The name of a text file containing the coefficients of the polynomial
*        fit produced by the FIGARO:IARC command. If a null value (!) is
*        supplied, the parameters ISMAP, FSMAP and ROUTEMAP1, etc, are used
*        instead to determine the nature of the required SwitchMap. Otherwise,
*        the returned SwitchMap will have two inputs and 1 output. The
*        inputs are channel number and row number (in that order), and
*        the one output is wavelength in Angstroms. [!]
*     ISMAP = LITERAL (Read)
*        An NDF or text file holding the inverse selector Mapping. If an NDF
*        is supplied, the Mapping from the Base Frame to the Current Frame
*        of its WCS FrameSet will be used. The supplied Mapping must have a
*        defined inverse transformation, but need not have a defined
*        forward transformation. It must have one input, and the number of
*        outputs must match the number of outputs of each of the supplied
*        route Mappings. A null (!) value may be supplied, in which case the
*        SwitchMap will have an undefined inverse Mapping. This parameter
*        is only used if a null value is supplied for IARCFILE.
*     ROUTEMAP1-ROUTEMAP25 = LITERAL (Given)
*        A set of 25 parameters associated with the NDFs or text files holding
*        the route Mappings. If an NDF is supplied, the Mapping from the Base
*        Frame to the Current Frame of its WCS FrameSet will be used. All the
*        supplied route Mappings must have common values for the Nin and Nout
*        attributes, and these values define the number of inputs and outputs
*        of the SwitchMap. There can be no missing Mappings; if ROUTEMAP3 is
*        to be processed then ROUTEMAP1 and ROUTEMAP2 must also be supplied.
*        A null value (!) should be supplied to indicate that there are no
*        further Mappings. ROUTEMAP3 to ROUTEMAP25 default to null (!).  At
*        least one Mapping must be supplied. These parameters are only used
*        if a null value is supplied for IARCFILE.
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new SwitchMap.
*     RESULT = LITERAL (Read)
*        A text file to receive the new SwitchMap.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     14-MAR-2006 (DSB):
*        Original version.
*     17-MAR-2006 (DSB):
*        Added IARCFILE parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'GRP_PAR'

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AST_ISAMAPPING

*  Local Constants:
      INTEGER MAXROUTE
      PARAMETER ( MAXROUTE = 25 )

      INTEGER MAXORDER
      PARAMETER ( MAXORDER = 8 )

*  Local Variables:
      CHARACTER EL*(GRP__SZNAM)
      CHARACTER IARCFILE*80
      CHARACTER PARAM*15
      DOUBLE PRECISION COEFF
      DOUBLE PRECISION FCOEFF( 4*( MAXORDER + 1 ) )
      INTEGER BY
      INTEGER FIN
      INTEGER FSMAP
      INTEGER I
      INTEGER IAT
      INTEGER ICO
      INTEGER IGRP
      INTEGER INPERM( 2 )
      INTEGER IPW1
      INTEGER IPW2
      INTEGER IROW
      INTEGER ISMAP
      INTEGER LUTMAP
      INTEGER NEL
      INTEGER NROUTE
      INTEGER NROW
      INTEGER ORDER
      INTEGER PERMMAP
      INTEGER PM
      INTEGER RESULT
      INTEGER ROUTEMAPS( MAXROUTE )
      LOGICAL MORE
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  See if the SwitchMap is to be created on the basis of a file containing
*  the output from the FIGARO:ARC command. Attempt to get a GRP group holding
*  the content of the IARC output file.
      CALL ATL_GTGRP( 'IARCFILE', IGRP, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Get the number of elements in the group.
         CALL GRP_GRPSZ( IGRP, NEL, STATUS )

*  The first 5 rows are header text. We read the number of rows from the
*  2nd line, and read the order of the polynomial from the 5th.
         CALL GRP_GET( IGRP, 2, 1, EL, STATUS )
         NROW = -1
         IF( EL( : 16 ) .EQ. 'Image dimensions' ) THEN
            BY = INDEX( EL, ' by ' )
            IF( BY .GT. 0 ) THEN
               CALL CHR_CTOI( EL( BY + 3 : ), NROW, STATUS )
               IF( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'S', EL )
                  CALL ERR_REP( ' ', 'Failed to read number of rows '//
     :                          'from string ''^S''.', STATUS )
                  GO TO 999
               END IF
            END IF
         END IF

         CALL GRP_GET( IGRP, 5, 1, EL, STATUS )
         ORDER = -1
         IF( EL( : 32 ) .EQ. 'Maximum degree polynomial used =' ) THEN
            CALL CHR_CTOI( EL( 36 : ), ORDER, STATUS )
            IF( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'S', EL )
               CALL ERR_REP( ' ', 'Failed to read IARC polynomial '//
     :                       'order from string ''^S''.', STATUS )
               GO TO 999
            END IF
         END IF

         IF( ( ORDER .EQ. -1 .OR. NROW .EQ. -1 ) .AND.
     :       STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Unexpected header format in IARC '//
     :                    'output file.', STATUS )
            GO TO 999
         END IF

*  Check the order is usable.
         IF( ORDER .GT. MAXORDER .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'O', ORDER )
            CALL MSG_SETI( 'MO', MAXORDER )
            CALL ERR_REP( ' ', 'Polynomial order (^O) is too big- it '//
     :                    'must be no more than ^MO.', STATUS )
            GO TO 999
         END IF

*  Allocate work space to hold the pointers to the route Mappings (one
*  for each row). Some of this array may not be used if the supplied file
*  does not contain a fit for every row.
         CALL PSX_CALLOC( NROW, '_INTEGER', IPW1, STATUS )

*  Indicate that the above array is currently empty.
         NROUTE = 0

*  Allocate work space to hold the index of the route Mappings associated
*  with every row index.
         CALL PSX_CALLOC( NROW, '_DOUBLE', IPW2, STATUS )

*  Fill this array with AST__BAD values.
         CALL KPG1_FILLD( AST__BAD, NROW, %VAL( CNF_PVAL( IPW2 ) ),
     :                    STATUS )

*  Read every subsequent non-header element in turn. ICO notes which
*  coefficient we are about to read from a fit (ORDER+1 refers to the row
*  index, ORDER to the X^ORDER coeff, etc, down to zero which refers to
*  the constant term).
         ICO = ORDER + 1
         FIN = 1
         DO I = 6, NEL
            CALL GRP_GET( IGRP, I, 1, EL, STATUS )

*  Each fit is described by an integer row index followed by (ORDER+1)
*  polynomial coefficient values. However these may be split over several
*  lines within the IARC outfile. We enter a loop in which we read
*  numerical values from the start of the line of text, and then remove
*  the read text. Once the text is empty, we continue to get a new
*  element from the group.
            MORE = .TRUE.
            DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Find the start of the first remaining word in the string, then find
*  the end of that word.
               IAT = 1
               CALL CHR_FIWS( EL, IAT, STATUS )
               CALL CHR_FIWE( EL, IAT, STATUS)

*  Attempt to read an integer or floating point value from this first word.
               IF( ICO .EQ. ORDER + 1 ) THEN

                  CALL CHR_CTOI( EL( : IAT ), IROW, STATUS )
                  IF (STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETC( 'EL', EL )
                     CALL ERR_REP( ' ', 'Failed to read integer from '//
     :                             '''^EL''.', STATUS )
                     GO TO 999
                  ELSE
                     ICO = ICO - 1
                  END IF

               ELSE

                  CALL CHR_CTOD( EL( : IAT ), COEFF, STATUS )
                  IF (STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETC( 'EL', EL )
                     CALL ERR_REP( ' ', 'Failed to read floating '//
     :                             'point value from ''^EL''.', STATUS )
                     GO TO 999
                  END IF

*  Store the values for this coefficient.
                  FCOEFF( FIN ) = COEFF
                  FCOEFF( FIN + 1 ) = 1.0
                  FCOEFF( FIN + 2 ) = ICO
                  FCOEFF( FIN + 3 ) = 0.0
                  FIN = FIN + 4

*  If we have not yet got all the coefficients for this row, move on to get
*  the next coefficient.
                  IF( ICO .GT. 0 ) THEN
                     ICO = ICO - 1

*  If we have got all the coefficients for this row, create the route
*  Mapping (a PolyMap). These PolyMaps have no inverse transformation.
                  ELSE
                     PM = AST_POLYMAP( 2, 1, ORDER + 1, FCOEFF, 0,
     :                                 0.0D0, ' ', STATUS )

*  Increment the number of route Mappings, and append the new route
*  Mapping pointer in the end of the list in the work array.
                     NROUTE = NROUTE + 1
                     CALL KPG1_STORI( NROW, NROUTE, PM,
     :                                %VAL( CNF_PVAL( IPW1 ) ), STATUS )

*  Store the index of the route Mapping associated with this row.
                     CALL KPG1_STORD( NROW, IROW, DBLE( NROUTE ),
     :                                %VAL( CNF_PVAL( IPW2 ) ), STATUS )

*  Prepare to start reading the coefficients of the fit for the next row.
                     ICO = ORDER + 1
                     FIN = 1
                  END IF
               END IF

*  Replace the word with spaces. If this means the text is now entirely blank,
*  indicate that we should leave the loop to read another element from the
*  group.
               EL( : IAT ) = ' '
               IF( EL .EQ. ' ' ) MORE = .FALSE.

            END DO
         END DO

*  Create a LutMap to use within the forward selector function. This
*  transforms the row number supplied as the second input of the SwitchMap
*  into the index of the associated route Mapping.
         LUTMAP = AST_LUTMAP( NROW, %VAL( CNF_PVAL( IPW2 ) ), 1.0D0,
     :                        1.0D0, ' ', STATUS )

*  Create a PermMap which feeds the second of its two inputs to its
*  single output.
         INPERM( 1 ) = 0
         INPERM( 2 ) = 1
         PERMMAP = AST_PERMMAP( 2, INPERM, 1, 2, 0.0D0, ' ', STATUS )

*  Combine these in series to create the forward selector Mapping.
         FSMAP = AST_CMPMAP( PERMMAP, LUTMAP, .TRUE., ' ', STATUS )

*  Create the required SwitchMap (it does not define an inverse transformation)
         RESULT = AST_SWITCHMAP( FSMAP, AST__NULL, NROUTE,
     :                           %VAL( CNF_PVAL( IPW1 ) ), ' ', STATUS )

*  Free the work space.
         CALL PSX_FREE( IPW1, STATUS )
         CALL PSX_FREE( IPW2, STATUS )

*  If a null value was supplied for IARCFILE, annul the error and get the
*  required Mappings from the other parameters.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Get the forward selector Mapping.
         CALL KPG1_GTOBJ( 'FSMAP', 'Mapping', AST_ISAMAPPING, FSMAP,
     :                    STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            FSMAP = AST__NULL
         END IF

*  Get the inverse selector Mapping.
         CALL KPG1_GTOBJ( 'ISMAP', 'Mapping', AST_ISAMAPPING, ISMAP,
     :                    STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            ISMAP = AST__NULL
         END IF

*  Get the first two route Mappings. These must be supplied.
         CALL KPG1_GTOBJ( 'ROUTEMAP1', 'Mapping', AST_ISAMAPPING,
     :                    ROUTEMAPS( 1 ), STATUS )
         CALL KPG1_GTOBJ( 'ROUTEMAP2', 'Mapping', AST_ISAMAPPING,
     :                    ROUTEMAPS( 2 ), STATUS )

*  Loop round getting route Mappings until a null value is supplied.
*  These can be omitted.
         NROUTE = 3
         DO WHILE( NROUTE .LE. MAXROUTE .AND. STATUS .EQ. SAI__OK )
            PARAM = 'ROUTEMAP'
            IAT = 8
            CALL CHR_PUTI( NROUTE, PARAM, IAT )
            CALL KPG1_GTOBJ( PARAM, 'Mapping', AST_ISAMAPPING,
     :                       ROUTEMAPS( NROUTE ), STATUS )
            IF( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               NROUTE = NROUTE - 1
               GO TO 10
            ELSE
               NROUTE = NROUTE + 1
            END IF
         END DO
 10      CONTINUE

*  Create the required SwitchMap.
         RESULT = AST_SWITCHMAP( FSMAP, ISMAP, NROUTE, ROUTEMAPS, ' ',
     :                           STATUS )

      END IF

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTSWITCHMAP_ERR', 'Error creating a new '//
     :                 'SwitchMap.', STATUS )
      END IF

      END
