      SUBROUTINE FTS1_CRNDF( PNNDF, FILROO, GCOUNT, NG, AUTO, FORMAT,
     :                       NDIM, DIMS, NDF, FILNAM, ASSOC, STATUS )
*+
*  Name:
*     FTS1_CRNDF

*  Purpose:
*     Creates an NDF for a FITS data array, generating the NDF's name
*     in some circumstances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_CRNDF( PNNDF, FILROO, GCOUNT, NG, AUTO, FORMAT, NDIM,
*    :                 DIMS, NDF, FILNAM, ASSOC, STATUS )

*  Description:
*     This is a server routine for FITSIN.  It packages up the
*     operations required to create an output NDF.  In automatic mode
*     the filename is generated and put into the parameter to prevent
*     prompting.  In manual mode the user is prompted for the file
*     name of the NDF.

*  Arguments:
*     PNNDF = CHARACTER * ( * ) (Given)
*        The name of the parameter by which the filename of the output
*        NDF will be obtained.
*     FILROO = CHARACTER * ( * ) (Given)
*        The rootname of the output NDF.  The suffix Gn, where n=%NG, is
*        appended in group-format mode to generate the filename.
*        Otherwise in automatic mode the NDF filename is the rootname.
*     GCOUNT = INTEGER (Given)
*        Number of data arrays in the FITS sub-file.  It should be one
*        if there are no groups.
*     NG = INTEGER (Given)
*        Number of the group.  It should be one if there are no groups.
*     AUTO = LOGICAL (Given)
*        If true the processing should be done in automatic mode, where
*        the user is not prompted for file names, since these are
*        generated from the prefix, file and sub-file numbers.
*     FORMAT = CHARACTER * ( * ) (Given)
*        The destination HDS format of the data array in the output
*        file.
*     NDIM = INTEGER (Given)
*        Dimensionality of the NDF.
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions of the NDF.
*     NDF = INTEGER (Returned)
*        The identifier of the created NDF.
*     FILNAM = CHARACTER * ( * ) (Returned)
*        The filename of the output NDF.
*     ASSOC = LOGICAL (Returned)
*        If false the NDF name was generated automatically.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     1990 November 26 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  GCOUNT,
     :  NG,
     :  NDIM,
     :  DIMS( NDIM )

      CHARACTER * ( * )
     :  PNNDF,
     :  FILROO,
     :  FORMAT

      LOGICAL
     :  AUTO

*  Arguments Returned:
      INTEGER
     :  NDF

      CHARACTER * ( * )
     :  FILNAM

      LOGICAL
     :  ASSOC


*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER
     :  CHR_LEN                  ! String length less trailing blanks

*  Local Variables:
      INTEGER
     :  NCFILN,                  ! Number of characters in file number
     :  NCROOT                   ! Number of characters in file root
                                 ! name

      CHARACTER
     :  FILNO * 6                ! Number of file/subfile used to
                                 ! generate output file names

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Decide how to create the output NDF.
*    ====================================

*    First time get file by asking the user, but subsequent group
*    members have group number appended to the name so an indirect
*    route is required to prevent prompting.  The exception is
*    automatic mode where no prompting is wanted.

      ASSOC = NG .EQ. 1 .AND. .NOT. AUTO

*    Simple-FITS manual-mode NDF creation.
*    =====================================

      IF ( ASSOC ) THEN

*       Start a new error context.

         CALL ERR_MARK

*       Create the NDF obtaining the name via the parameter system.

         CALL NDF_CREP( PNNDF, FORMAT, NDIM, DIMS, NDF, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*          Get the file name.

            CALL AIF_FLNAM( PNNDF, FILNAM, STATUS )
            CALL ERR_ANNUL( STATUS )
         END IF

*       Release the new error context.

         CALL ERR_RLSE

*    Group format or automatic mode NDF creation.
*    ============================================

      ELSE

*       Generate the new filename, which is either the root
*       followed by the group suffix for group-format data (except
*       for the first group), or...

         IF ( GCOUNT .GT. 1 ) THEN
            NCROOT = CHR_LEN( FILROO )
            IF ( NG .GT. 1 ) THEN
               CALL CHR_ITOC( NG, FILNO, NCFILN )
               FILNAM = FILROO( :NCROOT )//'G'//FILNO( :NCFILN )
            ELSE
               FILNAM = FILROO( :NCROOT )
            END IF

         ELSE

*          ... the name generated by the application in
*          automatic mode and no groups

            FILNAM = FILROO
         END IF

*       Put the file name into the parameter.

         CALL AIF_PTFNM( PNNDF, FILNAM, STATUS )

*       Create the new NDF using the supplied name.

         CALL NDF_CREP( PNNDF, FORMAT, NDIM, DIMS, NDF, STATUS )

*       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

*    End of first-NDF check.

      END IF

      END
