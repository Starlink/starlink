      SUBROUTINE CREOUT( PARNAM, TLENAM, NDIM, DIMS, LOCAT, STATUS )
*+
*  Name:
*     CREOUT

*  Purpose:
*     creates and returns a locator to an IMAGE type structure.

*  Language:
*     Starlink

*  Invocation:
*     CALL CREOUT( PARNAM, LABNAM, NDIM, DIMS, LOCAT, STATUS )

*  Description:
*     An IMAGE-type data structure, associated with the parameter name
*     in PARNAM, is created. A locator for this structure is returned
*     in LOCAT. A TITLE component, associated with the parameter name in
*     TLENAM, is created within the structure and a character value,
*     up to 72 characters long, is obtained from the parameter system
*     and written to the TITLE component. A DATA_ARRAY component is
*     created, this has dimensionality NDIM and dimensions DIMS( NDIM )
*     and is of type _REAL.
*     An immediate return will occur if STATUS has an error value on
*     entry.

*  Arguments:
*     PARNAM = CHAR*(*)( READ )
*        Parameter name associated with the IMAGE type structure to
*        be created.
*     TLENAM = CHAR*(*)( READ )
*        Parameter name associated with the TITLE component created
*        in the new IMAGE-type structure.
*     NDIM   = INTEGER( READ )
*        Dimensionality of the DATA_ARRAY component of the new
*        IMAGE-type structure.
*     DIMS( NDIM ) = INTEGER( READ )
*        Dimensions of the DATA_ARRAY component of the new IMAGE-type
*        structure.
*     LOCAT  = CHAR*(*)( WRITE )
*        Locator to the new IMAGE-type structure.
*     STATUS = INTEGER( UPDATE )
*        This is the global status, if this variable has an error
*        value on entry then an immediate return will occur. If an
*        error occurs during the execution of this routine STATUS
*        will be returned containing the appropriate error value.

*  Algorithm:
*     If no error on entry then
*        Create a new IMAGE-type data structure associated with the
*          parameter name PARNAM.
*        Get a locator, LOCAT, to this IMAGE-type structure.
*        If no errors so far then
*           Get a character string associated with parameter name TLENAM
*           If reponse was not null then
*              Write this character string to the structure as TITLE
*                component.
*           Endif
*           Create a DATA_ARRAY component of dimensions DIMS( NDIM ).
*           If error has occured while creating TITLE or DATA_ARRAY then
*              Annul the locator LOCAT
*           Endif
*        Endif
*     Endif

*  Copyright:
*     Copyright (C) 1983, 1986 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*     {enter_new_authors_here}

*  History:
*     01/12/1983 : Original version                       (ROE::ASOC5)
*     20/02/1983 : Handles null response to TITLE request (ROE::ASOC5)
*     1986 Sep 12: Renamed parameters section to arguments and tidied
*        (RL.STAR::CUR)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'


*  Arguments Given:
      CHARACTER*(*)
     :  PARNAM,
     :  TLENAM

      INTEGER
     :  NDIM,
     :  DIMS( NDIM )


*  Arguments Returned:
      CHARACTER*(*)
     :  LOCAT


*  Status:
      INTEGER STATUS


*  Local Variables:
      CHARACTER*72
     :  TITLE   ! label written to new structure as TITLE component

*.


*    check for error on entry

      IF( STATUS .EQ. SAI__OK ) THEN

*       create the new IMAGE-type structure

         CALL DAT_CREAT( PARNAM, 'IMAGE', 0, 0, STATUS )

*       associate the locator with it

         CALL DAT_ASSOC( PARNAM, 'WRITE', LOCAT, STATUS )

*       check for error

         IF( STATUS .EQ. SAI__OK ) THEN

*          get a title for inclusion in new structure as TITLE component

            CALL PAR_GET0C( TLENAM, TITLE, STATUS )

*          check for null response to request for TITLE

            IF( STATUS .EQ. PAR__NULL ) THEN

*             just annul the error and continue

               CALL ERR_ANNUL( STATUS )
            ELSE

*             create the TITLE component and write TITLE to it

               CALL DAT_NEWC( LOCAT, 'TITLE', 72, 0, 0, STATUS )
               CALL CMP_PUT0C( LOCAT, 'TITLE', TITLE, STATUS )

            ENDIF

*          create a _REAL DATA_ARRAY component with dimensionality
*          NDIM and dimensions DIMS( NDIM )

            CALL DAT_NEW( LOCAT, 'DATA_ARRAY', '_REAL', NDIM, DIMS,
     :                    STATUS )

*          if either an abort was given in response to the request for
*          a TITLE or something else has gone wrong should annul the
*          locator

            IF( STATUS .NE. SAI__OK ) THEN
               CALL DAT_ANNUL( LOCAT, STATUS )
            ENDIF
         ENDIF
      ENDIF

      END
