      SUBROUTINE NDF2GASP( STATUS )
*+
*  Name:
*     NDF2GASP

*  Purpose:
*     Converts a two-dimensional NDF into a GASP image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDF2GASP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts a two-dimensional NDF into the GAlaxy
*     Surface Photometry (GASP) package's format.  See the Notes for the
*     details of the conversion.

*  Usage:
*     ndf2gasp in out [fillbad]

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF data structure. The suggested default is the
*        current NDF if one exists, otherwise it is the current value.
*     FILLBAD = _INTEGER (Read)
*        The value used to replace bad pixels in the NDF's data array
*        before it is copied to the GASP file.  The value must be in the
*        range of signed words (-32768 to 32767).  A null value (!)
*        means no replacements are to be made.  This parameter is
*        ignored if there are no bad values.  [!]
*     OUT = FILENAME (Write)
*        The name of the output GASP image.  Two files are produced
*        with the same name but different extensions.  The ".dat" file
*        contains the data array, and ".hdr" is the associated header
*        file that specifies the dimensions of the image.  The
*        suggested default is the current value.

*  Examples:
*     ndf2gasp abell1367 a1367
*        Converts an NDF called abell1367 into the GASP image comprising
*        the pixel file a1367.dat and the header file a1367.hdr.  If
*        there are any bad values present they are copied verbatim to
*        the GASP image.
*     ndf2gasp ngc253 ngc253 fillbad=-1
*         Converts the NDF called ngc253 to a GASP image comprising the
*         pixel file ngc253.dat and the header file ngc253.hdr.  Any bad
*         values in the data array are replaced by minus one.

*  Notes:
*     The rules for the conversion are as follows:
*     -  The NDF data array is copied to the ".dat" file.
*     -  The dimensions of the NDF data array is written to the ".hdr"
*     header file.
*     -  All other NDF components are ignored.

*  References:
*     GASP documentation (MUD/66).

*  Related Applications:
*     CONVERT: GASP2NDF.

*  Implementation Status:
*     -  The GASP image produced has by definition type SIGNED WORD.
*     There is type conversion of the data array to this type.
*     -  Bad values may arise due to type conversion.  These too are
*     substituted by the (non-null) value of FILLBAD.
*     -  For an NDF with an odd number of columns, the last column from
*     the GASP image is trimmed.

*  Implementation Deficiencies:
*     Does not handle wildcards yet.

*  Keywords:
*     CONVERT, GASP

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1999, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2008 Science & Technology Facilities
*     Council. All Rights Reserved.

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
*     RAHM: Rhys Morris (STARLINK, University of Wales, Cardiff)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     18-JAN-1993 (RAHM):
*        Original version.
*     12-JUL-1993 (RAHM):
*        Tidied up code ready for submission to CONVERT package.
*     1993 July 28 (MJC):
*        Largely rewritten.  Tidied to prologue to the standard style.
*     11_JUN-1999 (AJC):
*        Correct NDF to NDF in call to NDF_MSG.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL
*     2008 March 15 (MJC):
*        Use KAPLIBS routine instead of its cloned CON equivalent.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'NDF_ERR'          ! NDF error constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FILLEN             ! Length of output GASP image name
      PARAMETER ( FILLEN = 132 )

      INTEGER NDIM               !
      PARAMETER ( NDIM=2 )       ! Only arrays of this dimensionality
                                 ! will be processed

*  Local Variables:
      LOGICAL CHECK              ! True if check for bad values
      INTEGER DIMS( NDF__MXDIM ) ! Image dimensions
      INTEGER EL                 ! Number of pixels in image
      INTEGER FILBAD             ! Value to replace bad pixels
      CHARACTER*( FILLEN ) GASPFI ! Output GASP image name
      LOGICAL ISBAD              ! Mapped data array contains bad
                                 ! values?
      INTEGER LBND( NDIM )       ! Lower bounds array
      INTEGER MDIM               ! Number of dimensions of the NDF
      INTEGER NDF                ! Identifier for input NDF
      INTEGER NDFT               ! Identifier for temporary NDF
      INTEGER NREP               ! The number of bad values replaced
      INTEGER PLACE              ! Placeholder to the temporary NDF
      INTEGER PNTR( 1 )          ! Pointer for the input data array
      INTEGER PNTRT( 1 )         ! Pointer for the temporary data array
      LOGICAL REPBAD             ! There are bad values to replace?
      INTEGER UBND( NDIM )       ! Upper bounds array
      INTEGER*2 WFILBA           ! Value to replace word-type bad values

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the input NDF and find its shape.
*  ========================================

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Associate NDF identifier with IN, via the parameter system.
      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Obtain the bounds of the NDF.
      CALL NDF_BOUND( NDF, NDIM, LBND, UBND, MDIM, STATUS )

*  Check dimensions of the input image.  Strictly we should look for
*  two significant dimensions.  Just check for too many dimensions.
*  Have to determine how many dimensions there are, since NDF_BOUND
*  has a bug and will not return the actual number of dimensions, but 1
*  instead.  To make this work the status must be temporarily set to
*  good, then reinstated.
      IF ( STATUS .EQ. NDF__XSDIM .OR. MDIM .LT. NDIM ) THEN
         CALL NDF_MSG( 'NDF', NDF )
         STATUS = SAI__OK
         CALL NDF_DIM( NDF, NDF__MXDIM, DIMS, MDIM, STATUS )
         CALL MSG_SETI( 'MDIM', MDIM )
         STATUS = NDF__XSDIM
         CALL ERR_REP( 'NDF2GASP',
     :     'NDF2GASP: Can only deal with two-dimensional NDFs. ^NDF '/
     :     /'has ^MDIM dimensions.',
     :     STATUS )
         GOTO 999
      END IF

*  Derive the number of columns in the input NDF.
      DIMS( 1 ) = UBND( 1 ) - LBND( 1 ) + 1

*  Derive the number of lines in the input NDF.
      DIMS( 2 ) = UBND( 2 ) - LBND( 2 ) + 1

*  Map the data array with the type needed by the GASP image.
      CALL NDF_MAP( NDF, 'Data', '_WORD', 'READ', PNTR, EL, STATUS )

*  Deal with any bad pixels.
*  =========================

*  Check whether NDF contains bad pixels.
      CHECK = .TRUE.
      CALL NDF_BAD( NDF, 'Data', CHECK, ISBAD, STATUS )

*  Decide what to do with the bad values.  Choose limiting values for
*  the replacement value that prevent it itself being invalid.
      IF ( ISBAD ) THEN
         CALL ERR_MARK
         CALL PAR_GDR0I( 'FILLBAD', VAL__BADW, REAL( NUM__MINW ),
     :                   REAL( NUM__MAXW ), .FALSE., FILBAD, STATUS )

*  Look for the null value to indicate no replacements.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            REPBAD = .FALSE.
         ELSE
            REPBAD = .TRUE.
         END IF
         CALL ERR_RLSE

*  Get a temporary NDF in which we can replace the bad values by the
*  nominated value.
         CALL NDF_TEMP( PLACE, STATUS )
         CALL NDF_NEW( '_WORD', NDIM, LBND, UBND, PLACE, NDFT, STATUS )

*  Map the temporary NDF.
         CALL NDF_MAP( NDFT, 'Data', '_WORD', 'WRITE', PNTRT, EL,
     :                 STATUS )

*  Since the value is already clamped to be within the signed-word
*  range, just copy the value to a variable of that type.
         WFILBA = FILBAD

*  Replace the magic values in the output array, otherwise copy from
*  the input to the output NDF.
         CALL KPG1_CHVAW( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                    VAL__BADW, WFILBA,
     :                    %VAL( CNF_PVAL( PNTRT( 1 ) ) ), NREP, STATUS )

*  To save resources unmap the input NDF data array.
         CALL NDF_UNMAP( NDF, 'Data', STATUS )

*  To fool the later code that nothing has happened to the input NDF,
*  copy the pointer to the temporary data array into the pointer for
*  the input array.
         PNTR( 1 ) = PNTRT( 1 )
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Get the name of the GASP image.
*  ===============================

*  Get a character string, via the parameter system, which will be the
*  name of the GASP image.
      CALL PAR_GET0C( 'OUT', GASPFI, STATUS )

*  Create the GASP image.
*  ======================

*  Create the GASP pixel file.  Copy the data array to the GASP image.
      CALL CON_WGASP( GASPFI, DIMS( 1 ), DIMS( 2 ),
     :                %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                STATUS )

*  Report the number of replacements.
      IF ( NREP .GT. 0 ) THEN
         CALL MSG_SETI( 'NR', NREP )
         CALL MSG_SETI( 'RV', FILBAD )
         CALL NDF_MSG( 'NDF', NDF )
         CALL MSG_OUTIF( MSG__NORM, 'NUMREP',
     :      '^NR bad values in the NDF ^NDF have been replaced by ^RV.',
     :      STATUS )
      END IF

  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF2GASP_ERR',
     :     'NDF2GASP: Unable to convert the NDF to a GASP image.',
     :     STATUS )
      END IF

      END
