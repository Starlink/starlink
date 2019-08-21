      SUBROUTINE NDF1_MPANL( MAPIN, NMAP, MAPS, HASINV, INMAP, ININD,
     :                       OUTMAP, OUTIND, MAP, STATUS )
*+
*  Name:
*     NDF1_MPANL

*  Purpose:
*     Analyse a Mapping into a set of parallel Mappings.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_MPANL( MAPIN, NMAP, MAPS, HASINV, INMAP, ININD,
*                      OUTMAP, OUTIND, MAP, STATUS )

*  Description:
*     This routine returns a set of Mappings that, when applied in
*     parallel, are equivalent to the supplied Mapping. Each returned
*     Mapping has the fewest possible number of inputs. Thus, the supplied
*     Mapping will be split up into the largest possible number of Mappings.

*  Arguments:
*     MAPIN = INTEGER (Given)
*        AST pointer to the supplied Mapping.
*     NMAP = INTEGER (Returned)
*        The number of Mappings returned.
*     MAPS( NDF__MXDIM ) = INTEGER (Returned)
*        An array of "NMAP" returned Mapping pointers. Each of these
*        Mappings will have a defined inverse transformation. If the
*        supplied Mapping does not include an inverse for a particular
*        input, then the Mapping for that input will be a TranMap that
*        encapsulated the supplied forward Mapping and an inverse Mapping
*        that generates AST__BAD values.
*     HASINV( NDF__MXDIM ) = LOGICAL (Returned)
*        An array of "NMAP" returned flags. Each one is set TRUE if the
*        correspnding Mapping in MAPS has an inverse transformation that
*        was inherited from the supplied Mapping, or FALSE if the
*        inverse transformation was created by this routine.
*     INMAP( NDF__MXDIM ) = INTEGER (Returned)
*        Element "i" is returned holding the index into the "MAPS" array
*        that holds the Mapping used to transforms input "i" in the
*        supplied Mapping.
*     ININD( NDF__MXDIM ) = INTEGER (Returned)
*        Element "i" is returned holding the index of the input of the
*        Mapping identified by "INMAP[i]" that corresponds to input
*        "i" in the supplied Mapping.
*     OUTMAP( NDF__MXDIM ) = INTEGER (Returned)
*        Element "i" is returned holding the index into the "MAPS" array
*        that holds the Mapping that generates values for output index "i"
*        in the supplied Mapping.
*     OUTIND( NDF__MXDIM ) = INTEGER (Returned)
*        Element "i" is returned holding the index of the output of the
*        Mapping identified by "OUTMAP[i]" that corresponds to output
*        "i" in the supplied Mapping.
*     MAP = INTEGER (Returned)
*        The full Mapping. This is constructed by joining all the parallel
*        Mappings back together again, and so should always have an inverse
*        transformation (so long as the supplied Mapping can be split
*        succesfully). If the supplied Mapping cannot be split, a clone
*        of the supplied Mapping is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry (JACH, UCLan)
*     {enter_new_authors_here}

*  History:
*     8-OCT-2007 (DSB):
*        Original version.
*     10-OCT-2007 (DSB):
*        Terminate MPAX loop early if all axes have been used up.
*     23-NOV-2007 (DSB):
*        Ensure MAP is returned holding a valid Mapping even if the
*        supplied Mapping cannot be split.
*     7-NOV-2008 (DSB):
*        Take account of the possibility that the supplied Mapping may
*        have some constant-valued outputs.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions

*  Arguments Given:
      INTEGER MAPIN

*  Arguments Returned:
      INTEGER NMAP
      INTEGER MAPS( NDF__MXDIM )
      LOGICAL HASINV( NDF__MXDIM )
      INTEGER INMAP( NDF__MXDIM )
      INTEGER ININD( NDF__MXDIM )
      INTEGER OUTMAP( NDF__MXDIM )
      INTEGER OUTIND( NDF__MXDIM )
      INTEGER MAP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION CONOUT( NDF__MXDIM )
      DOUBLE PRECISION ZERO( NDF__MXDIM )
      INTEGER I
      INTEGER IIN
      INTEGER IMAP
      INTEGER INPRM( NDF__MXDIM )
      INTEGER INVMAP
      INTEGER IOUT
      INTEGER J
      INTEGER MPAX
      INTEGER NDONE
      INTEGER NIN
      INTEGER NOUT
      INTEGER OUT( NDF__MXDIM )
      INTEGER OMAP
      INTEGER OUTPRM( NDF__MXDIM )
      INTEGER P( NDF__MXDIM )
      INTEGER PERM( NDF__MXDIM )
      INTEGER PMAP
      INTEGER TMAP
      INTEGER TNIN
      INTEGER TNOUT
      INTEGER USED
      LOGICAL IGNORE
      LOGICAL MORE
      LOGICAL NEEDPM
      LOGICAL OVFLOW

      DATA PERM /NDF__MXDIM*0/

*.

*  Initialise the returned values.
      NMAP = 0
      DO J = 1, NDF__MXDIM
         MAPS( J ) = AST__NULL
         HASINV( J ) = .FALSE.
         INMAP( J ) = 0
         ININD( J ) = 0
         OUTMAP( J ) = 0
         OUTIND( J ) = 0
         OUTPRM( J ) = 0
      END DO
      MAP = AST__NULL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of inputs and outputs for the Mapping.
      NIN = AST_GETI( MAPIN, 'Nin', STATUS )
      NOUT = AST_GETI( MAPIN, 'Nout', STATUS )

*  We first try to split the Mapping up into a set of parallel Mappings
*  each of which has only a single input (i.e. a separate Mapping for
*  each input). If any inputs remain we then try splitting them up
*  into a set of parallel Mappings that have two inputs. We continue like
*  this, increasing the number of axes in each parallel Mapping, until all
*  inputs have been assigned to a parallel Mapping.
      NDONE = 0
      DO MPAX = 1, NIN

*  Initialise the array holding the current selection of inputs.
         DO J = 1, MPAX
            P( J ) = 1
         END DO

*  We now loop through all possible combinations of "MPAX" inputs.
         MORE = ( NDONE .LT. NIN )
         DO WHILE( MORE )

*  If the current input selection includes any axes that have
*  already been assigned to a Mapping, or if it contains any duplicated
*  axes, we ignore it.
            IGNORE = .FALSE.
            DO J = 1, MPAX
               IF( INMAP( P( J ) ) .NE. 0 ) IGNORE = .TRUE.
            END DO

            DO J = 1, MPAX - 1
               DO I = J + 1, MPAX
                  IF( P( J ) .EQ. P( I ) ) IGNORE = .TRUE.
               END DO
            END DO

*  If we can use the current selection of inputs, test to see if these
*  inputs can be split off from the complete Mapping.
            IF( .NOT. IGNORE ) THEN
               CALL AST_MAPSPLIT( MAPIN, MPAX, P, OUT, OMAP, STATUS )

*  If they can, increment the number of returned Mappings.
               IF( OMAP .NE. AST__NULL ) THEN
                  NMAP = NMAP + 1

*  See if the Mapping has an inverse transformation. If not, replace the
*  Mapping with a TranMap that uses the original Mapping to define the
*  forward transformation, and a PermMap that generates bad values to
*  define the inverse transformation.
                  TNOUT = AST_GETI( OMAP, 'Nout', STATUS )

                  IF( .NOT. AST_GETL( OMAP, 'TranInverse',
     :                                STATUS ) ) THEN
                     HASINV( NMAP ) = .FALSE.
                     INVMAP = AST_PERMMAP( MPAX, PERM, TNOUT, PERM,
     :                                     0.0D0, ' ', STATUS )
                     TMAP = AST_TRANMAP( OMAP, INVMAP, ' ', STATUS )
                     CALL AST_ANNUL( OMAP, STATUS )
                     CALL AST_ANNUL( INVMAP, STATUS )
                     OMAP = TMAP
                  ELSE
                     HASINV( NMAP ) = .TRUE.
                  END IF

*  Store the Mapping and the values needed to associate each input
*  with an input of the returned Mapping.
                  MAPS( NMAP ) = OMAP

                  DO J = 1, MPAX
                     INMAP( P( J ) ) = NMAP
                     ININD( P( J ) ) = J
                  END DO

                  DO J = 1, TNOUT
                     OUTMAP( OUT( J ) ) = NMAP
                     OUTIND( OUT( J ) ) = J
                  END DO

*  Record the number of inputs that have been assigned to a Mapping.
*  If all inputs have been assigned we can abort the loop.
                  NDONE = NDONE + MPAX
                  IF( NDONE .EQ. NIN ) MORE = .FALSE.

               END IF
            END IF

*  Move on to the next set of inputs.
            J = 1
            OVFLOW = .TRUE.
            DO WHILE( OVFLOW )
               P( J ) = P( J ) + 1
               IF( P( J ) .GT. NIN ) THEN
                  P( J ) = 1
                  J = J + 1
                  IF( J .GT. MPAX ) THEN
                     MORE = .FALSE.
                     OVFLOW = .FALSE.
                  END IF
               ELSE
                  OVFLOW = .FALSE.
               END IF
            END DO

         END DO

      END DO

*  See how many inputs have been used.
      USED = 0
      DO J = 1, NIN
         IF( INMAP( J ) .NE. AST__NULL ) USED = USED + 1
      END DO

*  If any inputs have not been used, we cannot split the supplied
*  Mapping up so use the Mapping as supplied for all inputs.
      IF( USED .LT. NIN ) THEN

*  Annul AST objects for any inputs that have been used.
         DO J = 1, NIN
            IF( MAPS( J ) .NE. AST__NULL ) CALL AST_ANNUL( MAPS( J ),
     :                                                     STATUS )
         END DO

*  Return the supplied Mapping.
         MAPS( 1 ) = AST_CLONE( MAPIN, STATUS )
         HASINV( 1 ) = AST_GETL( MAPIN, 'TranInverse', STATUS )
         MAP = AST_CLONE( MAPIN, STATUS )

*  Reset the number of Mappings being returned to 1.
         NMAP = 1

*  Ensure all inputs use the same Mapping.
         DO J = 1, NIN
            INMAP( J ) = 1
            ININD( J ) = J
         END DO

*  Ensure all outputs use the same Mapping.
         DO J = 1, NOUT
            OUTMAP( J ) = 1
            OUTIND( J ) = J
         END DO

*  If the Mapping was split succesfully, join all the parallel Mappings
*  back together again to create the complete Mapping. We do this to
*  create a Mapping that we know will have an inverse transformation
*  (because each of the individual parallel Mappings has an inverse).
      ELSE

*  Loop round each of the returned Mappings.
         IIN = 1
         IOUT = 1
         DO IMAP = 1, NMAP

*  If this is the first Mapping, just take a clone of it. Otherwise, join
*  it in parallel with the current total Mapping.
            IF( IMAP .EQ. 1 ) THEN
               MAP = AST_CLONE( MAPS( 1 ), STATUS )
            ELSE
               TMAP = AST_CMPMAP( MAP, MAPS( IMAP ), .FALSE., ' ',
     :                            STATUS )
               CALL AST_ANNUL( MAP, STATUS )
               MAP = TMAP
            END IF

*  Update the array holding the input input that corresponds to each
*  input of the current total Mapping. Loop round each input of the
*  returned Mapping just added into the total Mapping.
            TNIN = AST_GETI( MAPS( IMAP ), 'Nin', STATUS )
            DO I = 1, TNIN

*  Search through all the inputs, looking for the one that feeds
*  input "I" of Mapping "IMAP". When found, store its index in the INPRM
*  array.
               DO J = 1, NIN
                  IF( INMAP( J ) .EQ. IMAP .AND.
     :                ININD( J ) .EQ. I ) THEN
                     INPRM( IIN ) = J
                     IIN = IIN + 1
                  END IF
               END DO

            END DO

*  Update the array holding the output index that corresponds to each
*  output of the current total Mapping. Loop round each output of the
*  returned Mapping just added into the total Mapping.
            TNOUT = AST_GETI( MAPS( IMAP ), 'Nout', STATUS )
            DO I = 1, TNOUT

*  Search through all the outputs of the supplied Mapping, looking for the
*  one that corresponds to output "I" of Mapping "IMAP". When found, store
*  its index in the OUTPRM array.
               DO J = 1, NOUT
                  IF( OUTMAP( J ) .EQ. IMAP .AND.
     :                OUTIND( J ) .EQ. I ) THEN
                     OUTPRM( IOUT ) = J
                     IOUT = IOUT + 1
                  END IF
               END DO

            END DO

         END DO

*  Sanity check...
         IF( STATUS .EQ. SAI__OK ) THEN
            IF( IIN .NE. NIN + 1 ) THEN
               STATUS = NDF__FATIN
               CALL MSG_SETI( 'IIN', IIN )
               CALL MSG_SETI( 'NP', NIN + 1 )
               CALL ERR_REP( ' ', 'NDF1_MPANL: IIN (^IIN) is not '//
     :                       'NIN+1 (^NP) (internal programming '//
     :                       'error).', STATUS )
            END IF
         END IF

*  If required, add a PermMap to the start of the total Mapping that
*  permutes the input indices into the order required by the total
*  Mapping.
         NEEDPM = .FALSE.

         DO I = 1, NIN
            PERM( INPRM( I ) ) = I
            IF( INPRM( I ) .NE. I ) NEEDPM = .TRUE.
         END DO

         IF( NEEDPM ) THEN
            PMAP = AST_PERMMAP( NIN, PERM, NIN, INPRM, 0.0D0, ' ',
     :                          STATUS )

            TMAP = AST_CMPMAP( PMAP, MAP, .TRUE., ' ', STATUS )
            CALL AST_ANNUL( PMAP, STATUS )
            CALL AST_ANNUL( MAP, STATUS )
            MAP = TMAP
         END IF

*  If required, add a PermMap to the end of the total Mapping that
*  permutes the output indices from the order produced by the total
*  Mapping to the order in the supplied Mapping. Also, add in constants
*  values for any outputs which are not created by any of the returned
*  Mappings. First check if the returned Mappings do not have the same
*  number of outputs as the supplied Mapping...
         IF( IOUT - 1 .NE. NOUT ) THEN

*  If so, we will definitely require a PermMap.
            NEEDPM = .TRUE.

*  Initialise the PermMap inputs corresponding to each PermMap output.
            DO I = 1, NOUT
               PERM( I ) = 0
            END DO

*  Transform the input position (0,0,0,...) into the output using the
*  supplied Mapping. This gives us the constant values to use for the
*  missing outputs (in CONOUT).
            DO I = 1, NIN
               ZERO( I ) = 0.0
            END DO
            CALL AST_TRANN( MAPIN, 1, NIN, 1, ZERO, .TRUE., NOUT, 1,
     :                      CONOUT, STATUS )

*  If we have got the right number of outputs, assume we do not need to
*  use the PermMap.
         ELSE
            NEEDPM = .FALSE.
         ENDIF

*  Now check each output from each of the returned Mappings.
         DO I = 1, IOUT - 1

*  Set up a one to one correspondance between the PermMap input and the
*  required PermMap output.
            PERM( OUTPRM( I ) ) = I

*  If the corresponding input and output do not both have the same index,
*  we will need to use the PermMap.
            IF( OUTPRM( I ) .NE. I ) NEEDPM = .TRUE.

         END DO

*  Only proceed if we need to use the PermMap.
         IF( NEEDPM ) THEN

*  Replace any zero axis indices with the (negated) index of the
*  corresponding constant output value.
            DO I = 1, NOUT
               IF( PERM( I ) .EQ. 0 ) PERM( I ) = -I
            END DO

*  Create the PermMap.
            PMAP = AST_PERMMAP( IOUT - 1, OUTPRM, NOUT, PERM, CONOUT,
     :                          ' ', STATUS )

*  Put in series with the returned Mapping.
            TMAP = AST_CMPMAP( MAP, PMAP, .TRUE., ' ', STATUS )
            CALL AST_ANNUL( PMAP, STATUS )
            CALL AST_ANNUL( MAP, STATUS )
            MAP = TMAP
         END IF

      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_MPANL', STATUS )

      END
