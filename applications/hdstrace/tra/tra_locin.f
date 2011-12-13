      SUBROUTINE TRA_LOCIN( OBJLOC, NAME, PRIM, TYPE, SIZE, NDIM, DIMS,
     :                      STATUS )
*+
*  Name:
*     TRA_LOCIN

*  Purpose:
*     Get information about an object with a given locator.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL TRA_LOCIN( OBJLOC, NAME, PRIM, TYPE, SIZE, NDIM, DIMS,
*    :                STATUS )

*  Description:
*     Information regarding the data system object with the specified
*     locator is obtained from the data system.  Data returned are the
*     name, type, size, dimensions, and whether the object is primitive
*     or not.

*  Arguments:
*     OBJLOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator to the object for which information is to be obtained.
*     NAME = CHARACTER * ( DAT__SZNAM ) (Returned)
*        The name associated with the object.
*     PRIM = LOGICAL (Returned)
*        Set to true if the object is of primitive type.
*     TYPE = CHARACTER * ( DAT__SZTYP ) (Returned)
*        The type of the object.
*     SIZE = INTEGER (Returned)
*        The total number of elements associated with the object,
*        i.e. if treated as a vector.
*     NDIM = INTEGER (Returned)
*        The actual dimensionality of the object.
*     DIMS( DAT__MXDIM ) = INTEGER (Returned)
*        The first NDIM elements will be the actual dimensions of the
*        object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Use the DAT_ enquiry routines to find out the information.

*  Copyright:
*     Copyright (C) 1984, 1989, 1991 Science & Engineering Research
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
*     DB: Dave Baines (ROE)
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     07/04/1984 (DB):
*        Original version.
*     1989 Jun 14 (MJC):
*        Added status check and error report, tidied and renamed from
*        LOCINF to avoid confusion with the original TRACE version.
*     1991 January 30 (MJC):
*        Converted to the SST prologue.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! Switch off the default typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Environment constants
      INCLUDE 'DAT_PAR'        ! Data-system constants

*  Arguments Given:
      CHARACTER*( DAT__SZLOC )
     :  OBJLOC                 ! Locator to the object to be examined

*  Arguments Returned:
      CHARACTER*( DAT__SZNAM )
     :  NAME                   ! Object name
      LOGICAL                  ! True if:
     :  PRIM                   ! Object is of primitive type
      CHARACTER*( DAT__SZTYP )
     :  TYPE                   ! The type of the object
      INTEGER
     :  SIZE,                  ! Number of elements for the object if
                               ! treated as a vector
     :  NDIM,                  ! Dimensionality of the object
     :  DIMS( DAT__MXDIM )     ! Actual dimensions of the object

*  Status:
      INTEGER STATUS           ! Global status

*.

*    Check the global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the name of the object associated with this locator.

      CALL DAT_NAME( OBJLOC, NAME, STATUS )

*    Find out if the object is primitive.

      CALL DAT_PRIM( OBJLOC, PRIM, STATUS )

*    Find out what type it is.

      CALL DAT_TYPE( OBJLOC, TYPE, STATUS )

*    How many elements are associated with it.

      CALL DAT_SIZE( OBJLOC, SIZE, STATUS )

*    What is its dimensionality and actual dimensions.

      CALL DAT_SHAPE( OBJLOC, DAT__MXDIM, DIMS, NDIM, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'TRA__NOINF',
     :     'Unable to get information about current object', STATUS )
      END IF

      END
