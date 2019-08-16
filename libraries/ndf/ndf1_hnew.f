      SUBROUTINE NDF1_HNEW( LOC1, NAME, TYPE, NDIM, DIM, LOC2, STATUS )
*+
*  Name:
*     NDF1_HNEW

*  Purpose:
*     Create a new HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HNEW( LOC1, NAME, TYPE, NDIM, DIM, LOC2, STATUS )

*  Description:
*     The routine creates a new HDS object. It performs a similar task
*     to the HDS routine DAT_NEW, except that it will accept a compound
*     component name consisting of a sequence of component names
*     separated by '.' (array slice/cell subscripts may also be
*     included). A complete object specification including a container
*     file name may also be given if the input locator supplied is
*     set to DAT__ROOT. All components which appear in the NAME
*     argument except for the last one must already exist.  The object
*     to be created must not itself exist.  If successful, the routine
*     returns a locator to the new object. This will be a primary
*     locator if a new container file was opened or created (i.e. if
*     LOC1 was set to DAT__ROOT), otherwise it will be a secondary
*     locator.

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*        Locator to an existing HDS structure (or DAT__ROOT if the NAME
*        argument contains a complete object specification).
*     NAME = CHARACTER * ( * ) (Given)
*        Relative HDS path name of the object to be created.
*     TYPE = CHARACTER * ( * ) (Given)
*        HDS object type.
*     NDIM = INTEGER (Given)
*        Number of object dimensions.
*     DIM( NDIM ) = INTEGER (Given)
*        Object dimensions.
*     LOC2 = CHARACTER * ( * ) (Returned)
*        Locator to the new object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     If this routine is called with STATUS set, then an invalid
*     locator will be returned via the LOC2 argument. The same value
*     will also be returned if the routine should fail for any reason.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-DEC-1990 (RFWS):
*        Original version.
*     23-JUN-1993 (RFWS):
*        Upgraded to accept DAT__ROOT as an input locator value and to
*        interpret NAME as a complete object specification, returning a
*        primary locator in this case.
*     8-OCT-1993 (RFWS):
*        Removed unnecessary status checks.
*     12-OCT-1993 (RFWS):
*        Added extra arguments to NDF1_FSPLT call.
*     27-DEC-2005 (TIMJ):
*        Call HDS_SPLIT rather than NDF1_HSPLT.
*        Call HDS_FIND rather than NDF1_HFIND
*     16-AUG-2019 (DSB):
*        HDF5 does not do tilde expansion, so do it here.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) LOC1
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) TYPE
      INTEGER NDIM
      INTEGER DIM( * )

*  Arguments Returned:
      CHARACTER * ( * ) LOC2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( NDF__SZFIL ) EXPFIL ! Expanded file name string
      CHARACTER * ( DAT__SZLOC ) LOC ! Temporary locator
      CHARACTER * ( DAT__SZNAM ) OBJNAM ! New top-level object name
      INTEGER D1                 ! First character in directory name
      INTEGER D2                 ! Last character in directory name
      INTEGER DOT                ! Position of final delimiting '.'
      INTEGER F1                 ! First character in file name
      INTEGER F2                 ! Last character in file name
      INTEGER LEXP               ! Length of expanded file name string
      INTEGER N1                 ! First character in file name field
      INTEGER N2                 ! Last character in file name field
      INTEGER P1                 ! First character in HDS path
      INTEGER P2                 ! Last character in HDS path
      INTEGER T1                 ! First character in file type field
      INTEGER T2                 ! Last character in file type field
      INTEGER V1                 ! First character in file version field
      INTEGER V2                 ! Last character in file version field
      LOGICAL DONE               ! Finished?

*.

*  Initialise the returned locator.
      LOC2 = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the input locator is DAT__ROOT, then the NAME value contains a
*  full object specification, including a container file name. Split
*  this into its file name and HDS path fields.
      DONE = .FALSE.
      IF ( LOC1 .EQ. DAT__ROOT ) THEN
         CALL HDS_SPLIT( NAME, F1, F2, P1, P2, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the HDS path is absent, then we must create a new top-level
*  object. Obtain a name for this object from the name field of the
*  container file specification (truncate it if it is too long).
            IF ( P1 .GT. P2 ) THEN
               CALL NDF1_FSPLT( NAME( F1 : F2 ), D1, D2, N1, N2, T1, T2,
     :                          V1, V2, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  N1 = N1 + F1 - 1
                  N2 = N2 + F1 - 1
                  OBJNAM = '?'
                  IF ( N1 .LE. N2 ) OBJNAM = NAME( N1 : N2 )

*  Create the new file (and top-level object) and note there is nothing
*  more to do. HDF5 does not interpret the tilde character, so expand the
*  file name if it starts with a tilde.
                  IF( NAME( F1 : F1 ) .EQ. '~' ) THEN
                     CALL NDF1_EXPFN( NAME( F1 : F2 ), .FALSE., EXPFIL,
     :                                LEXP, 0, STATUS )
                     CALL HDS_NEW( EXPFIL, OBJNAM, TYPE, NDIM, DIM,
     :                             LOC2, STATUS )
                  ELSE
                     CALL HDS_NEW( NAME( F1 : F2 ), OBJNAM, TYPE, NDIM,
     :                             DIM, LOC2, STATUS )
                  END IF
                  DONE = .TRUE.
               END IF

*  If both a container file name and an HDS path are present, then open
*  the container file in update mode.
            ELSE
               CALL HDS_OPEN( NAME( F1 : F2 ), 'UPDATE', LOC2, STATUS )
            END IF
         END IF

*  If the input locator is not DAT__ROOT, then find the first and last
*  non-blank characters in the HDS path and report an error if it is
*  completely blank.
      ELSE
         CALL CHR_FANDL( NAME, P1, P2 )
         IF ( P1 .GT. P2 ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF1_HNEW_NONE',
     :                    'No HDS component name given (possible ' //
     :                    'programming error).', STATUS )

*  If OK, clone the input locator.
         ELSE
            CALL DAT_CLONE( LOC1, LOC2, STATUS )
         END IF
      END IF

*  If OK, and there is still an HDS component to create, then search
*  backwards from the end of the HDS path for a '.' which delimits the
*  final name field.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( .NOT. DONE ) ) THEN
         DO 1 DOT = P2, P1, -1
            IF ( NAME( DOT : DOT ) .EQ. '.' ) GO TO 2
 1       CONTINUE
 2       CONTINUE

*  If the '.' occurs at the end of the path, then there is a field
*  missing, so report an error.
         IF ( DOT. EQ. P2 ) THEN
            STATUS = NDF__CNMIN
            CALL MSG_SETC( 'NAME', NAME( P1 : P2 ) )
            CALL ERR_REP( 'NDF1_HNEW_MSF',
     :                    'Missing field in HDS component name ' //
     :                    '''^NAME''.', STATUS )

*  Otherwise, if a '.' was found but it was not at the end of the path,
*  then search for the structure in which to create the new object,
*  using the part of the path which precedes the '.'. Promote the
*  resulting locator to be a primary locator if necessary (to hold the
*  container file open).
         ELSE IF ( DOT .GT. P1 ) THEN
            CALL HDS_FIND( LOC2, NAME( P1 : DOT - 1 ), 'UPDATE', LOC,
     :                       STATUS )
            IF ( LOC1 .EQ. DAT__ROOT ) THEN
               CALL DAT_PRMRY( .TRUE., LOC, .TRUE., STATUS )
            END IF

*  Retain the locator to the structure.
            CALL DAT_ANNUL( LOC2, STATUS )
            LOC2 = LOC
            LOC = DAT__NOLOC
         END IF

*  Check that the name of the object to be created is valid and create
*  the required new object within this structure.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_CHSCN( NAME( DOT + 1 : P2 ), STATUS )
            CALL DAT_NEW( LOC2, NAME( DOT + 1 : P2 ), TYPE, NDIM, DIM,
     :                    STATUS )

*  Obtain a locator to the new object, promoting it if necessary.
            CALL DAT_FIND( LOC2, NAME( DOT + 1 : P2 ), LOC, STATUS )
            IF ( LOC1 .EQ. DAT__ROOT ) THEN
               CALL DAT_PRMRY( .TRUE., LOC, .TRUE., STATUS )
            END IF
            CALL DAT_ANNUL( LOC2, STATUS )
            LOC2 = LOC
            LOC = DAT__NOLOC
         END IF
      END IF

*  If an error occurred, then annul the returned locator.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( LOC2, STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HNEW', STATUS )

      END
