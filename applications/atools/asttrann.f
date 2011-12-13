      SUBROUTINE ASTTRANN( STATUS )
*+
*  Name:
*     ASTTRANN

*  Purpose:
*     Transform N-dimensional coordinates.

*  Language:
*     Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTTRANN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application uses a Mapping to transform the coordinates of
*     a set of points in an arbitrary number of dimensions. The input
*     positions may be supplied either as columns of pixel values in an
*     NDF, or as a group of formatted axis values (see parameter POSIN).

*  Usage:
*     asttrann this incols in forward outcols out

*  ADAM Parameters:
*     FORWARD = _LOGICAL (Read)
*        A TRUE value indicates that the Mapping's forward coordinate
*        transformaton is to be used (in which case the number of values
*        supplied for the INCOLS parameter must be equal to the Nin
*        attribute of the Mapping, and the number of values supplied for
*        the OUTCOLS parameter must be equal to the Nout attribute).
*        A FALSE value indicates that the Mapping's inverse coordinate
*        transformaton is to be used (in which case the number of values
*        supplied for the INCOLS parameter must be equal to the Nout
*        attribute of the Mapping, and the number of values supplied for
*        the OUTCOLS parameter must be equal to the Nin attribute).
*     IN = LITERAL (Read)
*        Only used if a null(!) value is supplied for POSIN. A 2-dimensional
*        NDF holding the positions to be transformed. The DATA array of this
*        NDF is interpreted  as a table in which each column of pixels holds
*        values for a specified quantity, some of which are the axis values
*        at the positions to be transformed. Each row of pixels corresponds
*        to a separate position. The columns holding the axis values are
*        specified using parameter INCOLS.
*     INCOLS() = INTEGER (Read)
*        Only used if a null(!) value is supplied for POSIN. A set of
*        distinct column indices within the NDF specified by parameter
*        IN. These should identify the columns holding
*        the axis values to be transformed, in the order required by
*        the Mapping. If a null (!) value is supplied the lowest N
*        columns will be used, where N is the number of axes required
*        by the Mapping (see parameter FORWARD). [!]
*     OUT = LITERAL (Read)
*        Only used if a null(!) value is supplied for POSIN. A 2-dimensional
*        NDF to receive the transformed positions. The DATA array of this
*        NDF is interpreted  as a table in which each column of pixels holds
*        values for a specified quantity, some of which are the axis values
*        at the transformed positions. Each row of pixels corresponds to a
*        separate position. The columns to receive the transformed axis
*        values are specified using parameter OUTCOLS. The output NDF is
*        formed by taking a copy of the input NDF, and then expanding its
*        bounds to accomodate any extra columns specified by parameter
*        OUTCOLS (any such extra columns are initialized to hold bad
*        values). The initial values for the columns specified by parameter
*        OUTCOLS are then over-written with the transformed axis values.
*     OUTCOLS() = INTEGER (Read)
*        Only used if a null(!) value is supplied for POSIN. A set of
*        distinct column indices within the NDF specified by parameter
*        OUT. These should identify the columns in which the transformed
*        axis values should be stored, in the order produced by the
*        Mapping (see parameter FORWARD). There is no restriction on the
*        values which may be supplied (the output NDF will be expanded
*        to accomodate all supplied column indices). If the number of
*        input and output axes required by the Mapping are equal, the
*        run-time default is to use the same columns as those used for
*        parameter INCOLS. If the number of input and output axes are
*        different, there is no run-time default and the user is prompted. []
*     POSIN = LITERAL (Read)
*        A comma-separated list of floating point values to be used as the
*        input axis values. The list should start with all the values for
*        input axis 1, followed by all the values for input axis 2, etc. A
*        text file may be specified by preceeding the name of the file with
*        an up arrow character "^". If the supplied value ends with a minus
*        sign, the user is re-prompted for additional values. If a null (!)
*        value is supplied, the input positions are obtained using
*        parameter IN.   [!]
*     POSOUT = LITERAL (Read)
*        Only accessed if a value is supplied for parameter POSIN. The name
*        of a text file in which to put the transformed axis values. No file
*        is produced if a null (!) value is supplied. One axis value is
*        stored on each line of the file. All the values for axis 1 comes
*        first, followed by all the values for aixs 2, etc. [!]
*     THIS = LITERAL (Read)
*        An NDF or text file holding the Mapping to use. If an NDF is
*        supplied, the Mapping from the Base Frame to the Current Frame
*        of its WCS FrameSet will be used.

*  Copyright:
*     Copyright (C) 2001-2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.

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
*     NG: Norman Gray (Starlink, Glasgow University)
*     DSB: David S. Berry (Starlink, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     05-JUN-2001 (NG):
*        Original version.
*     11-JUN-2001 (DSB):
*        Tidied up (removed unused variables and INCLUDE statements,
*        split into separate files, etc), and modified in order to make
*        coding and documentation conform to various ATOOLS conventions.
*        Remove restriction on lower NDF bounds being equal to 1. Remove
*        all restrictions on OUTCOLS indices and expand the output NDF to
*        accomodate the extreme values.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     22-SEP-2006 (DSB):
*        Added parameters POSIN and POSOUT.
*     23-JAN-2007 (DSB):
*        Report error if the supplied input axis values are not an exact
*        mutiple of the number of input values for the Mapping.
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
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function
      INCLUDE 'GRP_PAR'          ! GRP constants

*  External references
      EXTERNAL AST_ISAMAPPING

*  Status
      INTEGER STATUS

*  Local variables
      CHARACTER TEMP*80         ! Temporary string
      INTEGER ELIN              ! No. of mapped elements in input DATA array
      INTEGER ELOUT             ! No. of mapped elements in output DATA array
      INTEGER I                 ! Loop counter
      INTEGER IGRP              ! Input group identifier
      INTEGER INCOL( NDF__MXDIM ) ! Which columns of the input NDF to select
      INTEGER INDF1             ! NDF reference for input file
      INTEGER INDF2             ! NDF reference for output file
      INTEGER IPIN              ! Pointer to mapped input DATA array
      INTEGER IPOUT             ! Pointer to mapped output DATA array
      INTEGER IPW1              ! Pointer to first work array
      INTEGER IPW2              ! Pointer to second work array
      INTEGER J                 ! Loop counter
      INTEGER LBND( 2 )         ! Lower bounds of the input NDF
      INTEGER LBNDO( 2 )        ! Lower bounds of the output NDF
      INTEGER NAXIN             ! No. of input axes
      INTEGER NAXOUT            ! No. of output axes
      INTEGER NAXVAL            ! No. of input axis values
      INTEGER NCIN              ! Number of columns in the input NDF
      INTEGER NCOUT             ! Number of columns in the output NDF
      INTEGER NDIM              ! Number of dimensions in input NDF
      INTEGER NIN               ! Nin attribute of Mapping
      INTEGER NOUT              ! Nout attribute of Mapping
      INTEGER NP                ! Number of points to be transformed
      INTEGER NROW              ! Number of rows in the input NDF
      INTEGER OUTCOL( NDF__MXDIM )! Which columns of the output NDF to select
      INTEGER THIS              ! Object reference for given Mapping
      INTEGER UBND( 2 )         ! Lower bounds of the output NDF
      INTEGER UBNDO( 2 )        ! Upper bounds of the output NDF
      INTEGER WS                ! Workspace for atl1_cltrn
      LOGICAL FORWRD            ! Are we to do a forward transformation?
      LOGICAL MORE              ! Continue looping?
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin AST and NDF contexts
      CALL AST_BEGIN( STATUS )
      CALL NDF_BEGIN

*  Get the Mapping.
      CALL KPG1_GTOBJ( 'THIS', 'Mapping', AST_ISAMAPPING, THIS,
     :                 STATUS )

*  Determine the Nin and Nout attributes of the Mapping
      NIN = AST_GETI (THIS, 'Nin', STATUS)
      NOUT = AST_GETI (THIS, 'Nout', STATUS)
      IF( ( NIN .GT. NDF__MXDIM .OR. NOUT .GT. NDF__MXDIM )
     :    .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NIN )
         CALL MSG_SETI( 'NO', NOUT )
         CALL MSG_SETI( 'NC', NDF__MXDIM )
         CALL ERR_REP( 'ASTTRANN_ERR1', 'The supplied Mapping has '//
     :                 '^NI input axes and ^NO output axes, but the '//
     :                 'maximum allowed number of axes is ^NC', STATUS)
         GO TO 999
      END IF

*  Get the FORWARD parameter.
      CALL PAR_GET0L( 'FORWARD', FORWRD, STATUS )

*  Store the number of axes in the input positions and the number in the
*  output positions.
      IF( FORWRD ) THEN
         NAXIN = NIN
         NAXOUT = NOUT
      ELSE
         NAXIN = NOUT
         NAXOUT = NIN
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get a group holding the input axis values.
      IGRP = GRP__NOID
      CALL KPG1_GTGRP( 'POSIN', IGRP, NAXVAL, STATUS )

*  If a null value was supplied, annul the error and use NDFs to store
*  the input and output positions..
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Get an identifier for the input NDF and get its bounds.
         CALL NDF_ASSOC( 'IN', 'READ', INDF1, STATUS )
         CALL NDF_BOUND( INDF1, 2, LBND, UBND, NDIM, STATUS )

*  Calculate the dimensions of the input NDF.
         NCIN = UBND( 1 ) - LBND( 1 ) + 1
         NROW = UBND( 2 ) - LBND( 2 ) + 1

*  The number of columns in the input NDF must be greater than or equal to
*  NAXIN.
         IF( NCIN .LT. NAXIN .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'NI', NAXIN )
            CALL MSG_SETI( 'NC', NCIN )
            IF( FORWRD ) THEN
               CALL MSG_SETC( 'W', 'forward' )
            ELSE
               CALL MSG_SETC( 'W', 'inverse' )
            END IF
            CALL NDF_MSG( 'NDF', INDF1 )
            CALL ERR_REP( 'ASTTRANN_ERR2', 'The ^W transformation of '//
     :                    'the supplied Mapping requires ^NI input '//
     :                    'axes but the input NDF ''^NDF'' only has '//
     :                    '^NC columns.', STATUS )
            GO TO 999
         END IF

*  Set up the default for INCOLS.
         DO I = 1, NAXIN
            INCOL( I ) = LBND( 1 ) + I - 1
         END DO

*  Loop until we have good values for INCOLS.
         MORE = .TRUE.
         DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Read the INCOLS parameter, getting exactly one value in the range
*  LBND( 1 ) to UBND( 1 ) for each input axis. The initial pass will use
*  the defaults set up above.
            CALL PAR_GDR1I( 'INCOLS', NAXIN, INCOL, LBND( 1 ),
     :                      UBND( 1 ), .TRUE., INCOL, STATUS)

*  Assume for the moment that we have a usable set of column indices.
            MORE = .FALSE.

*  Check there are no duplicate columns. If there are, report an error,
*  flush it to the user, cancel the parameter and indicate that we need
*  to re-prompt.
            DO I = 1, NAXIN - 1
               DO J = I + 1, NAXIN

                  IF( INCOL( I ) .EQ. INCOL( J ) .AND.
     :                STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR

                     CALL MSG_SETI( 'AX', INCOL( I ) )
                     CALL ERR_REP( 'ASTTRANN_ERR3', 'Column ^AX was '//
     :                           'specified more than once in the '//
     :                           'value supplied for parameter INCOLS.',
     :                           STATUS )

                     CALL ERR_FLUSH( STATUS )
                     CALL PAR_CANCL( 'INCOLS', STATUS )
                     MORE = .TRUE.
                     GO TO 10
                  END IF

               END DO

            END DO

 10         CONTINUE

         END DO

*  If the number of input and output axes are the same, set up a dynamic
*  default for parameter OUTCOLS equal to INCOLS.
         IF( NIN .EQ. NOUT ) THEN
            DO I = 1, NIN
               OUTCOL( I ) = INCOL( I )
            END DO
            CALL PAR_DEF1I( 'OUTCOLS', NIN, INCOL, STATUS )
         END IF

*  Loop until we have good values for OUTCOLS.
         MORE = .TRUE.
         DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Read the OUTCOLS parameter, getting exactly one value (without any
*  restriction on value) for each output axis. The initial pass will use
*  the defaults set up above.
            CALL PAR_EXACI( 'OUTCOLS', NAXOUT, OUTCOL, STATUS )

*  Assume for the moment that we have a usable set of column indices.
            MORE = .FALSE.

*  Check there are no duplicate columns. If there are, report an error,
*  flush it to the user, cancel the parameter and indicate that we need
*  to re-prompt.
            DO I = 1, NAXOUT - 1
               DO J = I + 1, NAXOUT

                  IF( OUTCOL( I ) .EQ. OUTCOL( J ) .AND.
     :                STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR

                     CALL MSG_SETI( 'AX', OUTCOL( I ) )
                     CALL ERR_REP( 'ASTTRANN_ERR4', 'Column ^AX was '//
     :                          'specified more than once in the '//
     :                          'value supplied for parameter OUTCOLS.',
     :                          STATUS )

                     CALL ERR_FLUSH( STATUS )
                     CALL PAR_CANCL( 'OUTCOLS', STATUS )
                     MORE = .TRUE.
                     GO TO 20
                  END IF

               END DO

            END DO

 20         CONTINUE

         END DO

*  Obtain the bounds required for the output NDF. These are the minimum
*  values required to encompass the input NDF and the specified output
*  columns.
         LBNDO( 1 ) = LBND( 1 )
         LBNDO( 2 ) = LBND( 2 )
         UBNDO( 1 ) = UBND( 1 )
         UBNDO( 2 ) = UBND( 2 )
         DO I = 1, NAXOUT
            LBNDO( 1 ) = MIN( LBNDO( 1 ), OUTCOL( I ) )
            UBNDO( 1 ) = MAX( UBNDO( 1 ), OUTCOL( I ) )
         END DO

*  Create the output NDF by propagating the input NDF, including the values
*  in the DATA array.
         CALL NDF_PROP( INDF1, 'DATA', 'OUT', INDF2, STATUS )

*  Now expand the output NDF to accomodate the requested output columns.
         CALL NDF_SBND( 2, LBNDO, UBNDO, INDF2, STATUS )

*  Map the DATA arrays.
         CALL NDF_MAP( INDF1, 'DATA', '_DOUBLE', 'READ', IPIN, ELIN,
     :                 STATUS)
         CALL NDF_MAP( INDF2, 'DATA', '_DOUBLE', 'UPDATE', IPOUT, ELOUT,
     :                 STATUS)

*  Allocate workspace in which to store the input and output positions.
         CALL PSX_CALLOC( NAXIN*NROW, '_DOUBLE', IPW1, STATUS )
         CALL PSX_CALLOC( NAXOUT*NROW, '_DOUBLE', IPW2, STATUS )

*  Copy the input positions from the input NDF to the first work array.
         CALL ATL1_CPCOL( LBND( 1 ), UBND( 1 ), LBND( 2 ), UBND( 2 ),
     :                   NAXIN, INCOL, .TRUE., %VAL( CNF_PVAL( IPIN ) ),
     :                   %VAL( CNF_PVAL( IPW1 ) ),
     :                   STATUS )

*  Transform the positions.
         CALL AST_TRANN( THIS, NROW, NAXIN, NROW,
     :                   %VAL( CNF_PVAL( IPW1 ) ), FORWRD,
     :                   NAXOUT, NROW, %VAL( CNF_PVAL( IPW2 ) ),
     :                   STATUS )

*  Copy the output positions from the second work array to the output NDF.
         CALL ATL1_CPCOL( LBNDO( 1 ), UBNDO( 1 ), LBNDO( 2 ),
     :                    UBNDO( 2 ), NAXOUT, OUTCOL, .FALSE.,
     :                    %VAL( CNF_PVAL( IPOUT ) ),
     :                    %VAL( CNF_PVAL( IPW2 ) ), STATUS )


*  If a group of input axis values was supplied, use it in preference to
*  any input NDF supplied via IN.
      ELSE

*  Find the number of input positions.
         NP = NAXVAL/NAXIN

*  Report an error if no complete positions were specified.
         IF( NP .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'NAXIN', NAXIN )
            CALL MSG_SETI( 'NAXVAL', NAXVAL )
            CALL ERR_REP( 'ASTTRANN_ERR5', 'At least ^NAXIN comma '//
     :                    'separated axis values are required, but '//
     :                    'only ^NAXVAL were supplied.', STATUS )
            GO TO 999

*  Report an error if surplus axis values were specified.
         ELSE IF( NP*NAXIN .NE. NAXVAL .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'NAXIN', NAXIN )
            CALL MSG_SETI( 'NAXVAL', NAXVAL )
            CALL ERR_REP( 'ASTTRANN_ERR6', 'The number of comma '//
     :                    'separated axis values supplied (^NAXVAL) '//
     :                    'is not an exact multiple of ^NAXIN.',
     :                    STATUS )
            GO TO 999
         END IF

*  Allocate work space for the input and output axis values.
         CALL PSX_CALLOC( NP*NAXIN, '_DOUBLE', IPW1, STATUS )
         CALL PSX_CALLOC( NP*NAXOUT, '_DOUBLE', IPW2, STATUS )

*  Read the values from the group into the memory.
         CALL ATL1_GTOFL( IGRP, NAXVAL, 1, %VAL( CNF_PVAL( IPW1 ) ),
     :                    STATUS )

*  Transform the positions.
         CALL AST_TRANN( THIS, NP, NAXIN, NP, %VAL( CNF_PVAL( IPW1 ) ),
     :                   FORWRD, NAXOUT, NP, %VAL( CNF_PVAL( IPW2 ) ),
     :                   STATUS )

*  Output the results.
         CALL ATL1_PRNTN( NP, NAXOUT, %VAL( CNF_PVAL( IPW2 ) ),
     :                    'POSOUT', STATUS )

*  Delete the group
         CALL GRP_DELET( IGRP, STATUS )

      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

*  Deallocate workspace
      CALL PSX_FREE( IPW1, STATUS)
      CALL PSX_FREE( IPW2, STATUS)

* End the AST and NDF contexts
      CALL NDF_END( STATUS )
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTTRANN_ERR', 'Error transforming a set of '//
     :                 'N-dimensional positions.', STATUS )
      END IF

      END
