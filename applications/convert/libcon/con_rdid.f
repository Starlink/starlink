      SUBROUTINE CON_RDID( NAME, MAXDES, DESNAM, DESVAL, NUMDES,
     :                     STATUS )
*+
*  Name:
*     CON_RDID

*  Purpose:
*     Writes all the descriptors associated with an Interim bulk data
*     frame to an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_RDID( NAME, MAXDES, DESNAM, DESVAL, NUMDES, STATUS )

*  Description:
*     This routine reads all the descriptors associated with an Interim-
*     environment bulk data frame.  The standard calls do not work when
*     there are multiple occurrences of the same descriptor name
*     interspered with other descriptors as found within a FITS header.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The parameter name of the bulk data frame whose descriptors
*        are to be obtained.
*     MAXDES = INTEGER (Given)
*        The maximum number of descriptors permitted.
*     DESNAM( MAXDES ) = CHARACTER * ( 8 ) (Returned)
*        The array of descriptor names.
*     DESVAL( MAXDES ) = CHARACTER * ( * ) (Returned)
*        The array of descriptor values.  Up to 72 characters may be
*        returned.
*     NUMDES = INTEGER (Returned)
*        Number of descriptor values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine is VMS specific.
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 July 20 (MJC):
*        Original version based loosely on RDDSCN.
*     1992 November 17 (MJC):
*        Skip over any descriptors marked for deletion.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'INTERIM(PCTCOM)'
      INCLUDE 'INTERIM(FCBCOM)'
      INCLUDE 'INTERIM(LDBCOM)'
      INCLUDE 'INTERIM(ERRPAR)'

*  Arguments Given:
      CHARACTER * ( * ) NAME
      INTEGER MAXDES

*  Arguments Returned:
      CHARACTER * ( * ) DESNAM( MAXDES )
      CHARACTER * ( * ) DESVAL( MAXDES )
      INTEGER NUMDES

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) DESCR   ! Descriptor name
      INTEGER DWN                ! Descriptor word number
      INTEGER ENTRY              ! Slot number of the BDF
      INTEGER IOSTAT             ! Local status
      LOGICAL LOOP               ! Loop for more descriptors
      INTEGER VALLEN             ! Maximum length of a value in
                                 ! characters.
      CHARACTER * ( 80 ) VALUE   ! Descriptor value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the bulk data frame
      CALL STL_ACCFRM( NAME, ENTRY, IOSTAT )
      IF ( IOSTAT .NE. ERR_NORMAL ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ISTAT', IOSTAT )
         CALL ERR_REP( 'CON_RDID_BDF',
     :     'Unable to open BDF.  Interim status code ^ISTAT.', STATUS )
         GOTO 999
      ENDIF

*  Read the Frame Control and first Local Descriptor Block.
      CALL STL_RVB( PCT_IOCHAN( ENTRY ), FCB, 1, IOSTAT )
      CALL STL_RVB( PCT_IOCHAN( ENTRY ), LDB, FCB_PTRLDB, IOSTAT )

*  Update the Frame Control Block (if allowed).
      IF ( .NOT. PCT_RDONLY( ENTRY ) ) THEN
         CALL SYS$ASCTIM( , FCB_ACCESS, , )
         CALL STL_WVB( PCT_IOCHAN( ENTRY ), FCB, 1, IOSTAT )
      END IF

*  Obtain the length of the value field.
      VALLEN = MIN( LEN( DESVAL( 1 ) ), 72 )

*  Initialise the Descriptor Word Number to the start of the block.
      DWN = 1

*  Search through the descriptor block(s).
      NUMDES = 0
      LOOP = .TRUE.

      DO WHILE ( LOOP )

*  Skip over the current descriptor if it is not marked for deletion,
*  and the end of the descriptors has not been reached.
         DO WHILE ( LDB_DWORD( DWN ) .LT. 0 .AND. LOOP )

*  Read the current descriptor name and its associated value.
            CALL STL_RATLDB( ENTRY, DESCR, DWN )
            CALL STL_RATLDB( ENTRY, VALUE, DWN )

*  Check if the end of the descriptor list is encountered.
            LOOP = LDB_BLKNUM .NE. FCB_ENDLDB( 1 ) .OR.
     :             DWN .NE. FCB_ENDLDB( 2 )
         END DO

*  Store the descriptor if the end of the descriptors has not been
*  reached.
         IF ( LOOP ) THEN

*  Read the current descriptor name and its associated value.
            CALL STL_RATLDB( ENTRY, DESCR, DWN )
            CALL STL_RATLDB( ENTRY, VALUE, DWN )

*  This is another descriptor, so store its name and value in the
*  arrays.
            NUMDES = NUMDES + 1
            DESNAM( NUMDES ) = DESCR( 1:8 )
            DESVAL( NUMDES ) = VALUE( 1:VALLEN )
         END IF

*  Check if the end of the descriptor list is encountered.
         LOOP = LDB_BLKNUM .NE. FCB_ENDLDB( 1 ) .OR.
     :          DWN .NE. FCB_ENDLDB( 2 )

      END DO

 999  CONTINUE

      END
