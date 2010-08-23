      SUBROUTINE KPG1_GETIM( PARNAM, LOCAT, DATLOC, DNAME, ORIGIN,
     :                       STATUS )
*+
*  Name:
*     KPG1_GETIM

*  Purpose:
*     Gets locators to an IMAGE-type structure and structure holding the
*     data array for data input.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GETIM( PARNAM, LOCAT, DATLOC, DNAME, ORIGIN, STATUS )

*  Description:
*     Returns a locator to an IMAGE-type data-structure associated with
*     a supplied parameter name, if a valid locator is not supplied.
*     The DATA_ARRAY component is examined to determine whether it is
*     primitive or a structure.  Given the former, the second locator
*     returned is the same as that to the IMAGE structure; on the other
*     hand, DATA_ARRAY is tested for being a simple ARRAY, if it is not
*     an error status is set, if it is the second locator returned
*     points to the DATA_ARRAY structure.  The simple ARRAY structure
*     is searched for origin and bad-pixel information.  Should the
*     origin be not at 0 for each dimension, and/or the bad-pixel flag
*     be set to false, warning messages are made.
*
*     It will not handle other NDF variants save report an error.

*  Arguments:
*     PARNAM = CHARACTER * ( * ) (Given)
*        Parameter name associated with the input IMAGE-type
*        structure.  If this is blank, the supplied locator, LOCAT,
*        will be assumed to be the locator to the top-level of an NDF
*        (IMAGE-type) structure.
*     LOCAT  = CHARACTER * ( DAT__SZLOC ) (Given & Returned)
*        On input when PARNAM is blank, there will be no association
*        with a parameter; the locator is assumed to point at the
*        top level of an NDF, and will be unchanged on exit.
*
*        If PARNAM is non blank, the input value of LOCAT is ignored.
*        On exit LOCAT is the locator to the object associated with the
*        given parameter name, unless status is bad, whereupon this
*        locator is annulled.
*     DATLOC  = CHARACTER * ( DAT__SZLOC ) (Returned)
*        The locator to the structure containing the primitive form of
*        the data array.  If it is the top-level of the NDF (IMAGE)
*        structure, it will be a clone of LOCAT.  Either way it will
*        require annulment.  If status is bad on exit this locator is
*        annulled.
*     DNAME = CHARACTER * ( DAT__SZNAM ) (Returned)
*        The name of the data array as this will be needed for access,
*        and it is different depending on its location.
*     ORIGIN( DAT__MXDIM ) = INTEGER (Returned)
*        The origin of the data array for each dimension.  It is set to
*        1 for non-existent dimensions.  This argument returned so that
*        if an output NDF is being created is will not be invalidated
*        because of different origins in its ARRAY-type components.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This a stop-gap routine until the remainder of KAPPA
*     IMAGE-format applications are converted to NDF.
*
*  Algorithm:
*     -  Get a locator to the IMAGE-type data structure associated with
*     parameter name given in PARNAM. Look for a DATA_ARRAY component.
*     If there is no DATA_ARRAY report an error and exit.
*     -  See whether or not the DATA_ARRAY is primitive.  If it is just
*     clone the IMAGE-structure locator and assign the name to
*     'DATA_ARRAY'.  Assigned unit origins.
*     -  If it is a structure
*        o  Check that it has the ARRAY type.  Abort if not.
*        o  Check that the variant is simple or not present (meaning
*        simple).  Abort if not.
*        o  Check that there is a primitive DATA component.  Abort if
*        not.
*        o  Clone the DATA_ARRAY structure's locator for return.
*        Assign the returned name to 'DATA'.
*        o  Look for any non-default (not 1) origin information, and
*        report a warning message if there are any.
*        o  Look for a false bad-pixel flag, and issue a warning if it
*        is false.
*        o  Tidy the work locator.

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
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 February 22 (MJC):
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
      INCLUDE 'DAT_ERR'          ! Data-system error constants

*  Arguments Given:
      CHARACTER * ( * )
     :  PARNAM

*  Arguments Given and Returned:
      CHARACTER * ( * )
     :  LOCAT

*  Arguments Returned:
      CHARACTER * ( * )
     :  DATLOC,
     :  DNAME

      INTEGER
     :  ORIGIN( DAT__MXDIM )

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER
     :  DLOC * ( DAT__SZLOC ),  ! Locator to DATA_ARRAY structure
     :  DTYPE * ( DAT__SZNAM ), ! Type of the DATA_ARRAY structure
     :  VARIAN * ( DAT__SZNAM ) ! ARRAY structure's variant

      INTEGER
     :  EL,                     ! Number of origin values
     :  I                       ! Loop counter

      LOGICAL                   ! True if:
     :  BADPIX,                 ! Bad pixels may be present in the data
                                ! array
     :  DATA,                   ! Data structure has a DATA_ARRAY
                                ! component
     :  PRIM,                   ! DATA_ARRAY component is primitive
     :  LOSSOR,                 ! There will be a loss of origin
                                ! information
     :  THERE                   ! Data structure has a named component

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Test which type of usage is required by testing for a blank parameter
*  name.
      IF ( PARNAM .NE. ' ' ) THEN

*  Initialise the returned values.
         LOCAT = ' '
      END IF
      DATLOC = ' '
      DNAME = ' '
      DO I = 1, DAT__MXDIM
         ORIGIN( I ) = 1
      END DO

*  Get a locator to the input IMAGE data structure associated with
*  PARNAM.
      IF ( PARNAM .NE. ' ' ) THEN
         CALL DAT_ASSOC( PARNAM, 'READ', LOCAT, STATUS )
      END IF

*  Find out if the IMAGE structure has a TITLE component.
      DATA = .TRUE.
      CALL DAT_THERE( LOCAT, 'DATA_ARRAY', DATA, STATUS )
      IF ( .NOT. DATA .OR. STATUS .NE. SAI__OK ) THEN
         IF ( .NOT. DATA ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_GETIM_NODAC',
     :        'Cannot find a DATA_ARRAY component.', STATUS )
         END IF
         GOTO 999
      END IF

*  See whether the file is in IMAGE/NDF primitive format or an NDF
*  variant.
      PRIM = .TRUE.
      CALL DAT_FIND( LOCAT, 'DATA_ARRAY', DLOC, STATUS )
      CALL DAT_PRIM( DLOC, PRIM, STATUS )

*  It is primitive so clone the structure locator.
      IF ( PRIM ) THEN
         CALL DAT_CLONE( LOCAT, DATLOC, STATUS )

*  The name of the primitive component containing the data array is
*  'DATA_ARRAY', and its origin is the default.
         DNAME = 'DATA_ARRAY'

      ELSE

*  It is a structure.  Check its type, which must be ARRAY.
         CALL DAT_TYPE( DLOC, DTYPE, STATUS )
         CALL CHR_UCASE( DTYPE )

*  Report an error if it is not an ARRAY structure.
         IF ( DTYPE .NE. 'ARRAY' ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_GETIM_NOTARR',
     :        'The structure is not an NDF---the data array is not '/
     :        /'a standard structure.', STATUS )
            GOTO 999
         END IF

*  Look to see whether there is a variant present.
         CALL ERR_MARK
         CALL CMP_GET0C( DLOC, 'VARIANT', VARIAN, STATUS )

*  Object is not found means a simple ARRAY structure.  Therefore,
*  annul the error.
         IF ( STATUS .EQ. DAT__OBJNF ) THEN
            CALL ERR_ANNUL( STATUS )
            VARIAN = 'SIMPLE'
         ELSE
            CALL CHR_UCASE( VARIAN )
         END IF
         CALL ERR_RLSE

*  This routine only supports simple ARRAY structures so report an error
*  when the structure is another variant.  Tidy the locator to the
*  DATA_ARRAY structure.
         IF ( VARIAN .NE. 'SIMPLE' ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_GETIM_NOTSIM',
     :        'The NDF is not primitive or simple and so cannot '/
     :        /'be processed by this application.', STATUS )
            CALL DAT_ANNUL( DLOC, STATUS )
            GOTO 999
         END IF

*  Check that there is a primitive array present.  Report an error if it
*  is absent.  Tidy the locator to the DATA_ARRAY structure.
         CALL CMP_PRIM( DLOC, 'DATA', PRIM, STATUS )
         IF ( .NOT. PRIM .OR. STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_GETIM_NODATA',
     :        'The NDF has an invalid DATA_ARRAY structure: the DATA '/
     :        /'component is missing or is not primitive.', STATUS )
            CALL DAT_ANNUL( DLOC, STATUS )
            GOTO 999
         END IF

*  The data locator is therefore the locator of the DATA_ARRAY
*  structure.  Clone it.  Assign the name of the component containing
*  the primitive array of data.
         CALL DAT_CLONE( DLOC, DATLOC, STATUS )
         DNAME = 'DATA'

*  Perform checks for losses of origin information.  Test whether it is
*  present or not.  If it is, obtain the origin values and test all
*  of them for being non-zero.  When there is a non-zero origin the
*  user is warned.
         CALL DAT_THERE( DLOC, 'ORIGIN', THERE, STATUS )
         IF ( THERE ) THEN
            CALL CMP_GET1I( DLOC, 'ORIGIN', DAT__MXDIM, ORIGIN, EL,
     :                      STATUS )

            LOSSOR = .FALSE.
            DO I = 1, EL
               IF ( ORIGIN( I ) .NE. 1 ) LOSSOR = .TRUE.
            END DO

            IF ( LOSSOR ) THEN
               CALL MSG_OUT( 'LOSE_ORIGIN',
     :           'Warning: Non-zero origin information may be ignored.',
     :           STATUS )
            END IF

*  Ensure the origin of any higher dimensions is still one.
            IF ( EL .LT. DAT__MXDIM ) THEN
               DO I = EL + 1, DAT__MXDIM
                  ORIGIN( I ) = 1
               END DO
            END IF

         END IF

*  Perform checks for loss of bad-pixel flag.  Test whether it is
*  present or not.  If it is, obtain the origin values and test all
*  of them for being non-zero.  When there is a non-zero origin the
*  user is warned.
         CALL DAT_THERE( DLOC, 'BAD_PIXEL', THERE, STATUS )
         IF ( THERE ) THEN
            CALL CMP_GET0L( DLOC, 'BAD_PIXEL', BADPIX, STATUS )

            IF ( .NOT. BADPIX ) THEN
               CALL MSG_OUT( 'LOSE_BADPIX',
     :           'Warning: The knowledge that bad pixels are not '/
     :           /'present is ignored.', STATUS )
            END IF
         END IF

*  Tidy the temporary locator.
         CALL DAT_ANNUL( DLOC, STATUS )
      END IF

*  Annul the locators if status is bad.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( DATLOC, STATUS )
         IF ( PARNAM .NE. ' ' ) CALL DAT_ANNUL( LOCAT, STATUS )
      END IF

  999 CONTINUE

      END
