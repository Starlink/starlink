      SUBROUTINE DST2NDF( STATUS )
*+
*  Name:
*      DST2NDF

*  Purpose:
*      Converts a Figaro (Version 2) DST file to an NDF.

*  Language:
*      Starlink Fortran 77

*  Type of Module:
*      ADAM A-task

*  Invocation:
*      CALL DST2NDF( STATUS )

*  Arguments:
*      STATUS = INTEGER (Given and Returned)
*         The global status.

*  Description:
*      This application converts a Figaro Version-2 DST file to a
*      Version-3 file, i.e. to an NDF.  The rules for converting the
*      various components of a DST are listed in the notes.  Since
*      both are hierarchical formats most files can be be converted with
*      little or no information lost.

*  Usage:
*      dst2ndf in out [form]

*  ADAM Parameters:
*      FORM = LITERAL (Read)
*         The storage form of the NDF's data and variance arrays.
*         FORM = "Simple" gives the simple form, where the array of data
*         and variance values is located in an ARRAY structure.  Here it
*         can have ancillary data like the origin.  This is the normal
*         form for an NDF.  FORM = "Primitive" offers compatibility with
*         earlier formats, such as IMAGE.  In the primitive form the
*         data and variance arrays are primitive components at the top
*         level of the NDF structure, and hence it cannot have
*         ancillary information. ["Simple"]
*      IN = Figaro file (Read)
*         The file name of the version 2 file.  A file extension must
*         not be given after the name, since ".dst" is appended by the
*         application.  The file name is limited to 80 characters.
*      OUT = NDF (Write)
*         The file name of the output NDF file.  A file extension must
*         not be given after the name, since ".sdf" is appended by the
*         application.  Since the NDF_ library is not used, a section
*         definition may not be given following the name.  The file
*         name is limited to 80 characters.

*  Examples:
*     dst2ndf old new
*        This converts the Figaro file old.dst to the NDF called new
*        (in file new.sdf).  The NDF has the simple form.
*     dst2ndf horse horse p
*        This converts the Figaro file horse.dst to the NDF called
*        horse (in file HORSE.SDF).  The NDF has the primitive form.

*  Notes:
*     The rules for the conversion of the various components are as
*     follows:
*     _________________________________________________________________
*            Figaro file          NDF
*     -----------------------------------------------------------------
*            .Z.DATA         ->   .DATA_ARRAY.DATA (when FORM =
*                                 "SIMPLE")
*            .Z.DATA         ->   .DATA_ARRAY (when FORM = "PRIMITIVE")
*            .Z.ERRORS       ->   .VARIANCE.DATA (after processing when
*                                 FORM = "SIMPLE")
*            .Z.ERRORS       ->   .VARIANCE (after processing when
*                                 FORM = "PRIMITIVE")
*            .Z.QUALITY      ->   .QUALITY.QUALITY (must be BYTE array)
*                                 (see Bad-pixel handling below).
*                            ->   .QUALITY.BADBITS = 255
*            .Z.LABEL        ->   .LABEL
*            .Z.UNITS        ->   .UNITS
*            .Z.IMAGINARY    ->   .DATA_ARRAY.IMAGINARY_DATA
*            .Z.MAGFLAG      ->   .MORE.FIGARO.MAGFLAG
*            .Z.RANGE        ->   .MORE.FIGARO.RANGE
*            .Z.xxxx         ->   .MORE.FIGARO.Z.xxxx
*
*            .X.DATA         ->   .AXIS(1).DATA_ARRAY
*            .X.ERRORS       ->   .AXIS(1).VARIANCE  (after processing)
*            .X.WIDTH        ->   .AXIS(1).WIDTH
*            .X.LABEL        ->   .AXIS(1).LABEL
*            .X.UNITS        ->   .AXIS(1).UNITS
*            .X.LOG          ->   .AXIS(1).MORE.FIGARO.LOG
*            .X.xxxx         ->   .AXIS(1).MORE.FIGARO.xxxx
*            (Similarly for .Y .T .U .V or .W structures which are
*             renamed to AXIS(2), ..., AXIS(6) in the NDF.)
*
*            .OBS.OBJECT     ->   .TITLE
*            .OBS.SECZ       ->   .MORE.FIGARO.SECZ
*            .OBS.TIME       ->   .MORE.FIGARO.TIME
*            .OBS.xxxx       ->   .MORE.FIGARO.OBS.xxxx
*
*            .FITS.xxxx      ->   .MORE.FITS(n)  (into value part of
*                                                 the string)
*            .COMMENTS.xxxx  ->   .MORE.FITS(n)  (into comment part of
*                                                 the string)
*            .FITS.xxxx.DATA ->   .MORE.FITS(n)  (into value part of
*                                                 the string)
*            .FITS.xxxx.DESCRIPTION -> .MORE.FITS(n) (into comment part
*                                                 of the string)
*            .FITS.xxxx.yyyy ->   .MORE.FITS(n)  (into blank-keyword
*                                                 comment containing
*                                                 yyyy=value)
*
*            .MORE.xxxx      ->   .MORE.xxxx
*
*            .TABLE          ->   .MORE.FIGARO.TABLE
*            .xxxx           ->   .MORE.FIGARO.xxxx
*
*     -  Axis arrays with dimensionality greater than one are not
*     supported by the NDF.  Therefore, if the application encounters
*     such an axis array, it processes the array using the following
*     rules, rather than those given above.
*
*            .X.DATA         ->   .AXIS(1).MORE.FIGARO.DATA_ARRAY
*                                 (AXIS(1).DATA_ARRAY is filled with
*                                 pixel co-ordinates)
*            .X.ERRORS       ->   .AXIS(1).MORE.FIGARO.VARIANCE (after
*                                 processing)
*            .X.WIDTH        ->   .AXIS(1).MORE.FIGARO.WIDTH
*
*     -  In addition to creating a blank-keyword NDF FITS-extension
*     header for each component of a non-standard DST FITS structure
*     (.FITS.xxxx.yyyy where yyyy is not DATA or DESCRIPTION), this set
*     of related headers are bracketed by blank lines and a comment
*     containing the name of the structure (i.e. xxxx).
*
*     Bad-pixel handling:
*     The QUALITY array is only copied if the bad-pixel flag
*     (.Z.FLAGGED) is false or absent.  A simple NDF with the bad-pixel
*     flag set to false (meaning that there are no bad-pixels present)
*     is created when .Z.FLAGGED is absent or false and FORM = "SIMPLE".

*  Related Applications:
*     CONVERT: NDF2DST.

*  Implementation Status:
*     -  The maximum number of dimensions is 6.

*  Copyright:
*     Copyright (C) 1989, 1992-1993 Science & Engineering Research
*     Council. Copyright (C) 1995-1996 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     JM: Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-June-1989 (JM):
*        Original version.  See the history of DSA_CONVERT_FORMAT.
*     1992 January 31 (MJC):
*        Greatly expanded the prologue into a form suitable for
*        documentation.  Fixed bugs in the generation of card images
*        in the FITS extension, notably the inclusion of quotes around
*        character strings.  Made the OBS structure preserve its type,
*        namely OBS, within the NDF.
*     1992 February 4 (MJC):
*        The location of the FITS comment is not hardwired for long
*        (>18) character values.  Improved efficiency by removing
*        unnecessary code from a second-level loop.  Made the maximum
*        number of dimensions 7.  Handled axis width as a numeric
*        array, rather than a character scalar.
*     1992 August 15 (MJC):
*        Fixed bug that caused a phantom 2-d FITS structure to be
*        created when there is an empty FITS structure within the DST
*        file.
*     1992 September 3 (MJC):
*        Made provision for the FLAGGED component, also affecting
*        whether or not quality is propagated.  Creates simple NDF where
*        required.  Fixed another occurrence of a logical being used for
*        a data type 'STRUCT'.
*     1992 September 8 (MJC):
*        Improved the documentation and added the rules for
*        non-1-dimensional axis arrays.   Handles axis variance in
*        addition to axis errors.
*     1992 September 10 (MJC):
*        Moved special cases of .OBS.SECZ, .OBS.TIME, .Z.MAGFLAG,
*        .Z.RANGE to the top-level Figaro extension as this is where
*        DSA_ now expects to find them in an NDF.
*     1992 September 28 (MJC):
*        Obtained NDF by SUBPAR calls, not PAR so that the global value
*        is not written in quotes, and thus may be accepted by other
*        applications.  Corrected the closedown sequence of DTA-error
*        reporting.  Added message tokens for the filenames to clarify
*        some error reports.
*     1992 October 13 (MJC):
*        Fixed a bug where given a DST that has a non-empty FITS
*        structure, and an axis array that is missing from the first or
*        second axis, the corresponding NDF axis-centre array is given
*        a dimension equal to the number of FITS headers.
*     1993 October 25 (MJC):
*        Added FORM parameter to control the NDF's storage form rather
*        than use the value of the bad-pixel flag to decide.
*     1993 November 10 (MJC):
*        Allowed the output NDF to be written to an arbitrary structure.
*        Used NDF_ library to access the NDF to enable NDF automatic
*        conversion to work.
*     1995 December 19 (MJC):
*        Places an n-dimensional axis into AXIS.MORE.FIGARO.DATA_ARRAY.
*        Previously, the component name was DATA.
*     1996 February 10 (MJC):
*        Allowed for scalar width in DST.  Fixed bug which prevented a
*        missing axis being created whenever there was no FITS extension
*        to write.
*     1996 September 21 (MJC):
*        ING-style hierarchical keywords may be written when the DST's
*        FITS structure contains non-standard structures.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard ADAM constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Get effective length of string

*  Local Variables:
      INTEGER EL                 ! Number of elements in NDF
      CHARACTER*256 FIGFIL       ! Name of input file
      CHARACTER*9 FORM           ! NDF storage form
      CHARACTER*( DAT__SZLOC ) LOC ! HDS locator to a transient NDF
      INTEGER NCF                ! Length of the Figaro file name
      INTEGER NCN                ! Length of the NDF file name
      INTEGER NDF                ! NDF identifier
      INTEGER NLEV               ! Number of levels in NDF
      CHARACTER*256 NDFFIL       ! Name of output file
      CHARACTER*132 PATH         ! Path to the NDF within the
                                 ! container file
      INTEGER   PNTR             ! Pointer to mapped NDF data array

*.

*   Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get the form of the output NDF.  A null value will choose the simple
*   form.
      CALL PAR_CHOIC( 'FORM', 'Simple', 'Simple,Primitive', .TRUE.,
     :                FORM, STATUS )

*   Get the input file name.
      CALL PAR_GET0C( 'IN', FIGFIL, STATUS )

*   Start an NDF context.
      CALL NDF_BEGIN

*   Create a dummy output NDF.  This is need to obtain the name of the
*   NDF using the normal route, and therefore that automatic data
*   conversion can be performed by the NDF library.  A simple PAR_GET0C
*   call cannot be made because this will write a global value of the
*   current NDF in quotes, rather than @ndfname as required.
      CALL NDF_CREP( 'OUT', '_REAL', 1, 1, NDF, STATUS )

*   Get an HDS locator to the NDF.
      CALL NDF_LOC( NDF, 'READ', LOC, STATUS )

*   Get the output file name.
      CALL HDS_TRACE( LOC, NLEV, PATH, NDFFIL, STATUS )

*   Map the NDF array so that it is defined, and hence can be annulled
*   with NDF_ complaining.
      CALL NDF_MAP( NDF, 'Data', '_REAL', 'WRITE/ZERO', PNTR, EL,
     :              STATUS )

*   End NDF context.
      CALL NDF_END( STATUS )

*   Delete the temporary NDF... now by removing the last locator.
      CALL DAT_DELET( 'OUT', STATUS )
*      CALL HDS_ERASE( LOC, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Add the appropriate extensions to the input and output files.
         NCF = CHR_LEN( FIGFIL )
         CALL CHR_APPND( '.dst', FIGFIL, NCF )
         NCN = CHR_LEN( NDFFIL )

*   Call the conversion subroutine.
         CALL CON_DST2N( FIGFIL ( :NCF ), NDFFIL ( :NCN ), FORM, NLEV,
     :                   PATH, STATUS )

      END IF

      END

