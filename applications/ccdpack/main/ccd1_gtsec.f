      SUBROUTINE CCD1_GTSEC( USEEXT, ID, LBND, UBND, LBND2,
     :                       UBND2, EXTSEC, STATUS )
*+
*  Name:
*     CCD1_GTSEC

*  Purpose:
*     To get the section extents for an output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GTSEC( USEEXT, ID, LBND, UBND, LBND2, UBND2,
*                      EXTSEC, STATUS )

*  Description:
*     This routine returns bounds for a NDF. The bounds are either
*     gotten from the NDF CCDPACK extension or via the ADAM
*     parameter EXTENT. Checks that these values are sensible,
*     ie the lower values are less than the upper values, and are
*     within the bounds of the actual NDF (although this is not really a
*     problem it will not be allowed for efficiency sake) are made.
*     The values are modified until they make sense, or the user is
*     (re)prompted.
*
*     If _any_ bounds are present in the NDF then the others default
*     to the size of the input bounds (i.e. any missing bounds are
*     defaulted provided at least one is present).

*  Arguments:
*     USEEXT = LOGICAL (Given)
*        Whether or not to attempt to retrieve the bounds from the
*        extension of the given NDF.
*     ID = INTEGER (Given)
*        NDF identifier -- not used if USEEXT is false.
*     LBND( 2 ) = INTEGER (Given)
*        Lower bounds of the actual NDF.
*     UBND( 2 ) = INTEGER (Given)
*        Upper bounds of the actual NDF.
*     LBND2( 2 ) = INTEGER (Returned)
*        New lower bounds of NDF section.
*     UBND2( 2 ) = INTEGER (Returned)
*        New upper bounds of NDF section.
*     EXTSEC = LOGICAL (Returned)
*        Whether or not the NDF section was obtained from the NDF
*        extension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1993-1994 Science & Engineering Research
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-APR-1991 (PDRAPER):
*        Original version.
*     1-AUG-1991 (PDRAPER):
*        Bounds modified without reprompting.
*     30-SEP-1993 (PDRAPER):
*        Added ability to access values from the NDF.
*     18-JAN-1994 (PDRAPER):
*        Added EXTSEC argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message system parameters

*  Arguments Given:
      LOGICAL USEEXT
      INTEGER ID
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )

*  Arguments Returned:
      INTEGER LBND2( 2 )
      INTEGER UBND2( 2 )
      LOGICAL EXTSEC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EXTENT( 4 )        ! Section extents from user.
      INTEGER IMAX               ! Maximum value
      INTEGER IMIN               ! Minimum value
      INTEGER MAXX               ! Maximum X value from NDF
      INTEGER MAXY               ! Maximum Y value from NDF
      INTEGER MINX               ! Minimum X value from NDF
      INTEGER MINY               ! Minimum Y value from NDF
      INTEGER NACT               ! Actual number of value returns.
      INTEGER NTRY               ! Number of trys to get section right.
      LOGICAL OK1                ! MINX value ok
      LOGICAL OK2                ! MAXX value ok
      LOGICAL OK3                ! MINY value ok
      LOGICAL OK4                ! MAXY value ok
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the default values.
      EXTENT( 1 ) = LBND( 1 )
      EXTENT( 2 ) = UBND( 1 )
      EXTENT( 3 ) = LBND( 2 )
      EXTENT( 4 ) = UBND( 2 )

*  If required get the values from the NDF.
      EXTSEC = .FALSE.
      IF ( USEEXT ) THEN
         CALL CCG1_FCH1I( ID, 'EXTENT', 'MINX,', 1, MINX, OK1,
     :                    STATUS )
         CALL CCG1_FCH1I( ID, 'EXTENT', 'MAXX,', 1, MAXX, OK2,
     :                    STATUS )
         CALL CCG1_FCH1I( ID, 'EXTENT', 'MINY,', 1, MINY, OK3,
     :                    STATUS )
         CALL CCG1_FCH1I( ID, 'EXTENT', 'MAXY,', 1, MAXY, OK4,
     :                    STATUS )

*  Transfer any ok values into EXTENT.
         IF ( OK1 .OR. OK2 .OR. OK3 .OR. OK4 ) THEN
            EXTSEC = .TRUE.
            IF ( OK1 ) EXTENT( 1 ) = MINX
            IF ( OK2 ) EXTENT( 2 ) = MAXX
            IF ( OK3 ) EXTENT( 3 ) = MINY
            IF ( OK4 ) EXTENT( 4 ) = MAXY
         END IF
      END IF

*  If we do not have any valid values by this stage then prompt
*  the user.
      IF ( .NOT. EXTSEC ) THEN

*  Set the dynamic default for this value.
         CALL PAR_DEF1I( 'EXTENT', 4, EXTENT, STATUS )
         NTRY = 0

*  Show the user what the extent actually is.
         CALL MSG_SETI( 'GTSEC_E1', EXTENT( 1 ) )
         CALL MSG_SETI( 'GTSEC_E2', EXTENT( 2 ) )
         CALL MSG_SETI( 'GTSEC_E3', EXTENT( 3 ) )
         CALL MSG_SETI( 'GTSEC_E4', EXTENT( 4 ) )
         CALL MSG_OUTIF( MSG__NORM, ' ',
     :                   '  Extent of the input NDF:  '//
     :                   '(^GTSEC_E1:^GTSEC_E2,^GTSEC_E3:^GTSEC_E4)',
     :                   STATUS )

*  Prompt the user for the sections extents.
 1       CONTINUE
         CALL PAR_GET1I( 'EXTENT', 4, EXTENT, NACT, STATUS )
         IF ( NACT .LT. 4 .AND. STATUS .EQ. SAI__OK ) THEN

*  Incorrect number of returns - issue warning and try again.
             CALL MSG_OUT( 'GTSEC_MESS1',
     :       '  Must supply four values : x-min,x-max,y-min,y-max ',
     :       STATUS )

*  Increment try counter and test for give up value.
             NTRY = NTRY + 1
             IF ( NTRY .GT. 3 ) THEN

*  Give up set status and return.
                STATUS = SAI__ERROR
                GO TO 99
             END IF

*  Try again.
             CALL PAR_CANCL( 'EXTENT', STATUS )
             GO TO 1
         END IF
      END IF

*  Check the ordering of the values.
      IMIN = MIN( EXTENT( 1 ) , EXTENT( 2 ) )
      IMAX = MAX( EXTENT( 1 ) , EXTENT( 2 ) )
      EXTENT( 1 ) = IMIN
      EXTENT( 2 ) = IMAX
      IMIN = MIN( EXTENT( 3 ) , EXTENT( 4 ) )
      IMAX = MAX( EXTENT( 3 ) , EXTENT( 4 ) )
      EXTENT( 3 ) = IMIN
      EXTENT( 4 ) = IMAX

*  See if the values are with bounds of input NDF.
      IF ( EXTENT( 1 ) .GE. LBND( 1 ) .AND.
     :     EXTENT( 2 ) .LE. UBND( 1 ) .AND.
     :     EXTENT( 3 ) .GE. LBND( 2 ) .AND.
     :     EXTENT( 4 ) .LE. UBND( 2 ) ) THEN

*  Everything ok write the values out
           LBND2( 1 ) = EXTENT( 1 )
           UBND2( 1 ) = EXTENT( 2 )
           LBND2( 2 ) = EXTENT( 3 )
           UBND2( 2 ) = EXTENT( 4 )
      ELSE

*  Things out of bounds, try again. Reset values.
         CALL MSG_OUT( 'GTSEC_MESS2',
     :   '  Given section extents were outside of input NDF bounds',
     :      STATUS )
         CALL MSG_OUT( 'GTSEC_MESS3', '  They have been modified to'//
     :                 ' lie within the NDF bounds', STATUS )
           LBND2( 1 ) = MAX( EXTENT( 1 ), LBND( 1 ) )
           UBND2( 1 ) = MIN( EXTENT( 2 ), UBND( 1 ) )
           LBND2( 2 ) = MAX( EXTENT( 3 ), LBND( 2 ) )
           UBND2( 2 ) = MIN( EXTENT( 4 ), UBND( 2 ) )
      END IF
 99   CONTINUE
      END
* $Id$
