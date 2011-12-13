      SUBROUTINE KPG1_CROUT( PARNAM, TLENAM, NDIM, DIMS, ORIGIN, LOCAT,
     :                       DATLOC, DNAME, STATUS )
*+
*  Name:
*     KPG1_CROUT

*  Purpose:
*     Creates and returns locators to the top-level and data-array of
*     an NDF-type structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CROUT( PARNAM, TLENAM, NDIM, DIMS, ORIGIN, LOCAT,
*                      DATLOC, DNAME, STATUS )

*  Description:
*     An NDF-type data structure, associated with the supplied
*     parameter name is created.  A locator for this structure is
*     returned.  A TITLE component, associated with another parameter
*     name is created within the structure and a character value, is
*     obtained from the parameter system and written to the TITLE
*     component.  A DATA_ARRAY component is created.  If the supplied
*     origins are all one, DATA_ARRAY is primitive, having the supplied
*     dimensionality and dimensions, and is of type _REAL.  Otherwise
*     DATA_ARRAY is a simple ARRAY-type structure containing the
*     primitive _REAL data array and origin information.  The locator to
*     the primitive data array is also returned.

*  Arguments:
*     PARNAM = CHARACTER * ( * ) (Given)
*        Parameter name associated with the NDF-type structure to
*        be created.
*     TLENAM = CHARACTER * ( * ) (Given)
*        Parameter name associated with the TITLE component created
*        in the new NDF-type structure.
*     NDIM = INTEGER (Given)
*        Dimensionality of the data-array component of the new
*        NDF-type structure.
*     DIMS( NDIM ) = INTEGER (Given)
*        Dimensions of the data-array component of the new NDF-type
*        structure.
*     ORIGIN( NDIM ) = INTEGER (Given)
*        The origin information of the data array.  If a primitive
*        NDF is to be created these should all be set to 1.  Note though
*        if other ARRAY-type components---QUALITY and VARIANCE---are
*        propagated, the origin data of ARRAY-type components must be
*        consistent not to invalidate the NDF.  Therefore use the values
*        returned by KPG1_GETIM unless QUALITY and VARIANCE are not
*        propagated.
*     LOCAT = CHARACTER * ( * ) (Returned)
*        Locator to the new NDF-type structure.  If status is bad on
*        exit this locator is annulled.  If status is bad on exit this
*        locator is annulled.
*     DATLOC  = CHARACTER * ( DAT__SZLOC ) (Returned)
*        The locator to the structure containing the primitive
*        form of the data array.  If it is the top-level of the NDF
*        structure, it will be a clone of LOCAT.  Either way it will
*        require annulment.  If status is bad on exit this locator is
*        annulled.
*     DNAME = CHARACTER * ( DAT__SZNAM ) (Returned)
*        The name of the data array as this will be needed for access,
*        and it is different depending on its location.  This argument
*        is probably unnecessary, but it is here defensively, in case
*        the origin is changed to or from the default.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This a stop-gap routine until the remainder of KAPPA
*     IMAGE-format applications are converted to NDF.  It enables both
*     primitive (i.e. IMAGE-format) and simple NDFs to be processed.
*     -  The maximum length of the TITLE component produced by this
*     routine is 72 characters.

*  aLGOrithm:
*     -  Create a new NDF-type data structure associated with the
*     parameter name PARNAM.  Get a locator to this NDF-type structure.
*     -  If no errors so far then get a character string associated
*     with parameter name TLENAM and write it to the TITLE component.
*     A null title value prevents creation of the TITLE component.
*     -  Determine whether the DATA_ARRAY component WILL BE primitive or
*     a structure.
*        o  For the former all the origins must be 1.  Create a
*        DATA_ARRAY component of dimensions DIMS( NDIM ).  Assign the
*        returned name to 'DATA_ARRAY'.
*        o  For the latter, create an ARRAY structure and create the
*        DATA component of dimensions DIMS( NDIM ).  Create and write
*        the origin data to this structure.  Create an ORIGIN component
*        and write the origin data to it.  Clone the DATA_ARRAY
*        structure's locator for return.  Assign the returned name to
*        'DATA'.
*     -  If error has occurred Annul the returned locators.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 February 23 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! ADAM global constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants

*  Arguments Given:
      CHARACTER*(*)
     :  PARNAM,
     :  TLENAM

      INTEGER
     :  NDIM,
     :  DIMS( NDIM ),
     :  ORIGIN( NDIM )

*  Arguments Returned:
      CHARACTER * ( * )
     :  LOCAT,
     :  DATLOC,
     :  DNAME

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER
     :  DLOC * ( DAT__SZLOC ),  ! Locator to data-array structure
     :  TITLE * ( 72 )          ! Value of TITLE component

      INTEGER
     :  I                       ! Loop counter

      LOGICAL                   ! True if:
     :  PRIM                    ! DATA_ARRAY component is primitive

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned values.
      LOCAT = ' '
      DATLOC = ' '
      DNAME = ' '

*  Create the new NDF-type structure
      CALL DAT_CREAT( PARNAM, 'NDF', 0, 0, STATUS )

*  Associate the locator with the structure.
      CALL DAT_ASSOC( PARNAM, 'WRITE', LOCAT, STATUS )

*  Check for an error.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get a title for inclusion in new structure as TITLE component.  Start
*  a new error context so that a null response may be handled without
*  an error.
         CALL ERR_MARK
         CALL PAR_GET0C( TLENAM, TITLE, STATUS )

*  Check for a null response to the request for a title.
         IF ( STATUS .EQ. PAR__NULL ) THEN

*  Just annul the error and continue.
            CALL ERR_ANNUL( STATUS )
         ELSE

*  Create the TITLE component and write the title to it.
            CALL DAT_NEWC( LOCAT, 'TITLE', 72, 0, 0, STATUS )
            CALL CMP_PUT0C( LOCAT, 'TITLE', TITLE, STATUS )
         END IF

*  Release the error context.
         CALL ERR_RLSE

*  Decide whether the DATA_ARRAY component is primitive or a simple
*  ARRAY structure.  This is done by examining the origin data.  The
*  origins all must be one to create a simple NDF (IMAGE-format).
         PRIM = .TRUE.
         DO I = 1, NDIM
            PRIM = PRIM .AND. ORIGIN( I ) .EQ. 1
         END DO

*  Create a _REAL primitive DATA_ARRAY component with dimensionality
*  NDIM and dimensions DIMS( NDIM ).
         IF ( PRIM ) THEN
            CALL DAT_NEW( LOCAT, 'DATA_ARRAY', '_REAL', NDIM, DIMS,
     :                    STATUS )

*  It is primitive so clone the structure locator.
            CALL DAT_CLONE( LOCAT, DATLOC, STATUS )

*  The name of the primitive component containing the data array is
*  'DATA_ARRAY'.
            DNAME = 'DATA_ARRAY'

         ELSE

*  Create a DATA_ARRAY structure of type ARRAY.
            CALL DAT_NEW( LOCAT, 'DATA_ARRAY', 'ARRAY', 0, 0, STATUS )

*  Obtain a locator to the structure.
            CALL DAT_FIND( LOCAT, 'DATA_ARRAY', DLOC, STATUS )

*  Create a _REAL DATA component with dimensionality NDIM and
*  dimensions DIMS( NDIM ).
            CALL DAT_NEW( DLOC, 'DATA', '_REAL', NDIM, DIMS, STATUS )

*  Create an ORIGIN component.
            CALL DAT_NEW( DLOC, 'ORIGIN', '_INTEGER', 1, NDIM, STATUS )
            CALL CMP_PUT1I( DLOC, 'ORIGIN', NDIM, ORIGIN, STATUS )

*  The data locator is therefore the locator of the DATA_ARRAY
*  structure.  Clone it.  Assign the name of the component containing
*  the primitive array of data.
            CALL DAT_CLONE( DLOC, DATLOC, STATUS )
            DNAME = 'DATA'

*  Tidy the locator.
            CALL DAT_ANNUL( DLOC, STATUS )
         END IF
      END IF

*  If either an abort was given in response to the request for a TITLE
*  or something else has gone wrong should annul the locator.

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( DATLOC, STATUS )
         CALL DAT_ANNUL( LOCAT, STATUS )
      END IF

      END
