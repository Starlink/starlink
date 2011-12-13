      SUBROUTINE AXCONV( STATUS )
*+
*  Name:
*     AXCONV

*  Purpose:
*     Expands spaced axes in an NDF into the primitive form.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AXCONV( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application routine converts in situ an NDF's axis centres
*     in the `spaced' form into `simple' form.  Applications using the
*     NDF_ library, such as KAPPA, are not currently capable of
*     supporting spaced arrays, but there are packages that produce NDF
*     files with this form of axis, notably Asterix.  This application
*     provides a temporary method of allowing KAPPA et al. to handle
*     these NDF datasets.

*  Usage:
*     axconv ndf

*  ADAM Parameters:
*     NDF = NDF (Read and Write)
*        The NDF to be modified.

*  Examples:
*     axconv rosat256
*        This converts the spaced axes in the NDF called rosat256 into
*        simple form.

*  Related Applications:
*     KAPPA: SETAXIS.

*  Implementation Status:
*     -  Only axes with a real data type are created.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     RDS: Richard Saxton  (STARLINK, Leicester)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-JUN-1992 (RDS):
*        Original version.
*     1992 November 26 (MJC):
*        Brought up to KAPPA standards, and various improvements to the
*        prologue.
*     1995 April 24 (MJC):
*        Made Usage lowercase.  Added Related Applications and an
*        example.  Guessed previous history entry.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE                 ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'             ! Standard SAE constants
      INCLUDE 'DAT_PAR'             ! Data-system constants
      INCLUDE 'NDF_PAR'             ! Standard NDF constants
      INCLUDE 'MSG_PAR'             ! MSG constants
      INCLUDE 'CNF_PAR'             ! For CNF_PVAL function

*  Status:
      INTEGER STATUS                ! Global status

*  Local Variables:
      CHARACTER * (DAT__SZLOC) ADLOC ! Locator to the data_array in the
                                    ! axis
      CHARACTER * (DAT__SZLOC) ALOC ! Locator to the axis structure
      REAL BASE                     ! Value of the centre of the first
                                    ! pixel
      CHARACTER * (DAT__SZLOC) CLOC ! Locator to an individual axis
      CHARACTER * (DAT__SZLOC) DDLOC ! Locator to the data in the
                                    ! data array
      INTEGER DIMS( NDF__MXDIM )    ! Dimensions of the data array
      INTEGER DPNTR                 ! Pointer to axis data array
      CHARACTER * (DAT__SZLOC) LOC  ! Locator to the datafile
      INTEGER LP                    ! Loop counter
      INTEGER NDF                   ! NDF identifier
      INTEGER NDIM                  ! Dimensionality of the data array
      LOGICAL PRIM                  ! True if the data_array object is
                                    ! primitive
      REAL SCALE                    ! Width of each axis bin
      LOGICAL THERE                 ! True if the HDS object is present
      CHARACTER * ( 6 ) VARIANT     ! Axis variant e.g. 'spaced',
                                    ! 'simple'
      LOGICAL VTHERE                ! True if the variant object is
                                    ! present

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an NDF context.
      CALL NDF_BEGIN

*  Associate the NDF.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', NDF, STATUS )

*  See if the axis coordinate system is defined.  If not, abort.
*  header for it.
      CALL NDF_STATE( NDF, 'Axis', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'AXCONV_NOAXIS',
     :     'AXCONV: NDF ^NDF does not have an AXIS component.', STATUS )
         GOTO 999
      END IF

*  Find the dimensionality of the NDF.
      CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, NDIM, STATUS )

*  Get a locator to the NDF.
      CALL NDF_LOC( NDF, 'UPDATE', LOC, STATUS )

*  Get a locator to the axis structure.
      CALL DAT_FIND( LOC, 'AXIS', ALOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Loop over each axis.
      DO LP = 1, NDIM

*  Initialise values.
         THERE = .FALSE.
         VARIANT = '      '

*  Get a locator to this axis.
         CALL DAT_CELL( ALOC, 1, LP, CLOC, STATUS )

*  Get a locator to the axis data array.
         CALL DAT_FIND( CLOC, 'DATA_ARRAY', ADLOC, STATUS )

*  Find out whether or not the axis is a structure.
         CALL DAT_PRIM( ADLOC, PRIM, STATUS )

*  No action is required if the axis is already in the primitive form.
*  Convert a spaced axis array into a primitive array.
         IF ( .NOT. PRIM ) THEN

*  Start a new error context.
            CALL ERR_MARK

*  Get the variant.
            CALL DAT_THERE( ADLOC, 'VARIANT', VTHERE, STATUS )

*  Although the variant is mandatory, we will not assume its presence
*  or value.  So read the variant if available, otherwise look for a
*  base value, which is unique to the spaced form.  If neither is
*  present it must be regarded as an error, and cannot be converted.
            IF ( VTHERE ) THEN
               CALL CMP_GET0C( ADLOC, 'VARIANT', VARIANT, STATUS )
            ELSE
               CALL DAT_THERE( ADLOC, 'BASE', THERE, STATUS )
               IF ( THERE ) THEN
                  VARIANT = 'SPACED'
               ELSE
                  VARIANT = 'JUNK'
                  STATUS = SAI__ERROR
               END IF
            END IF

*  Is it a spaced form of array?
            IF ( INDEX( VARIANT, 'SPACED' ) .NE. 0 .AND.
     :           STATUS .EQ. SAI__OK ) THEN

*  Release the error context.
               CALL ERR_RLSE

*  Get the values for each part of the spaced array
               CALL CMP_GET0R( ADLOC, 'BASE', BASE, STATUS )
               CALL CMP_GET0R( ADLOC, 'SCALE', SCALE, STATUS )

*  Make a contextual error report, and tidy the locators.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETI( 'IAXIS', LP )
                  CALL ERR_REP( 'AXCONV_BASESCALE',
     :              'AXCONV: Error reading the base and scale values '/
     :              /'from axis ^AXNO.', STATUS )

                  CALL DAT_ANNUL( CLOC, STATUS )
                  CALL DAT_ANNUL( ADLOC, STATUS )
                  GOTO 980
               END IF

*  Create a data-array component in the simple form.
               CALL DAT_NEW1R( ADLOC, 'DATA', DIMS( LP ), STATUS )

*  Map the data array.
               CALL DAT_FIND( ADLOC, 'DATA', DDLOC, STATUS )
               CALL DAT_MAPR( DDLOC, 'WRITE', 1, DIMS( LP ), DPNTR,
     :                        STATUS )

*  Make a contextual error report and erase the data component.  Tidy
*  the locators.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( 'AXCONV_CRESIM',
     :              'AXCONV: Error creating the output axis-centre '/
     :              /'array for axis ^IAXIS.', STATUS )

                  CALL DAT_ERASE( ADLOC, 'DATA', STATUS )
                  CALL DAT_ANNUL( DDLOC, STATUS )
                  CALL DAT_ANNUL( CLOC, STATUS )
                  CALL DAT_ANNUL( ADLOC, STATUS )

                  GOTO 980
               END IF

*  Fill the data array.
               CALL KPG1_SSCOF( DIMS( LP ), DBLE( SCALE ), DBLE( BASE ),
     :                          %VAL( CNF_PVAL( DPNTR ) ), STATUS )

*  Unmap the data and annul the data-array locator.
               CALL DAT_ANNUL( DDLOC, STATUS )

*  Create the variant component if it's not already present.
               IF ( .NOT. VTHERE ) THEN
                  CALL DAT_NEW0C( ADLOC, 'VARIANT', 6, STATUS )
               END IF

*  Assign the variant component to be the simple form.
               CALL CMP_PUT0C( ADLOC, 'VARIANT', 'SIMPLE', STATUS )

*  Now it is safe to delete the base, scale, and dimensions components.
               CALL DAT_ERASE( ADLOC, 'BASE', STATUS )
               CALL DAT_ERASE( ADLOC, 'SCALE', STATUS )

               CALL DAT_THERE( ADLOC, 'DIMENSIONS', THERE, STATUS )
               IF ( THERE ) THEN
                  CALL DAT_ERASE( ADLOC, 'DIMENSIONS', STATUS )

*  Asterix sometimes writes the wrong object name in here, so check for
*  this as well.
               ELSE
                  CALL DAT_THERE( ADLOC, 'DIMENSION', THERE, STATUS )
                  IF ( THERE )
     :              CALL DAT_ERASE( ADLOC, 'DIMENSION', STATUS )
               END IF

*  Exit if something has gone wrong.  Tidy the locators.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL DAT_ANNUL( CLOC, STATUS )
                  CALL DAT_ANNUL( ADLOC, STATUS )
                  GOTO 980
               END IF

*  Report what has happened.
               CALL MSG_SETI( 'IAXIS', LP )
               CALL MSG_OUTIF( MSG__NORM, 'AXCONV_PROGRESS',
     :           'Expanded axis ^IAXIS into its simple form.', STATUS )

            ELSE

*  Report an error, but flush it immediately as we are in a loop, and
*  this is not a fatal error.
               CALL MSG_SETI( 'IAXIS', LP )
               CALL ERR_REP( 'AXCONV_AXERR',
     :           'AXCONV: Error reading axis ^IAXIS centres.  The '/
     :           /'axis is not in the spaced form.', STATUS )
               CALL ERR_FLUSH( STATUS )

*  Release the new error context.
               CALL ERR_RLSE
            END IF

*  Annul some more locators, so that they may be re-used for another
*  axis.
            CALL DAT_ANNUL( ADLOC, STATUS )
            CALL DAT_ANNUL( CLOC, STATUS )

*  The axis did not require conversion.
         ELSE
            CALL MSG_SETI( 'IAXIS', LP )
            CALL MSG_OUTIF( MSG__VERB, 'AXCONV_NOTEXPAND',
     :        'Not expanding axis ^IAXIS.', STATUS )
         END IF

      END DO

  980 CONTINUE

*  Annul the remaining locators.
      CALL DAT_ANNUL( ALOC, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'AXCONV_ERR',
     :     'AXCONV: Error expanding spaced axis-centre arrays.',
     :     STATUS )
      END IF

      END
