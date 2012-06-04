      SUBROUTINE RTD_WRNDF( NAME, TYPE, ID, IP, NX, NY, COMP, HEAD,
     :                      NHEAD, STATUS )
*+
*  Name:
*     RTD_WRNDF

*  Purpose:
*     Write a new NDF with the given NDF component using an existing
*     NDF.

*  Language:
*     Fortran-77

*  Invocation:
*     CALL RTD_WRNDF( NAME, TYPE, ID, IP, NX, NY, COMP, HEAD, NHEAD,
*                     STATUS )

*  Description:
*      This routine copies a given NDF, writing the data pointed to
*      by IP into it as the appropriate component. It also copies any
*      quality information that has been generated for this NDF.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the NDF to be created.
*     TYPE = INTEGER (Given)
*        The data type of the image. This will be one of the codes:
*            8 = unsigned byte image
*            8 = X image data (written as byte).
*           16 = signed word
*          -16 = unsiged word
*           32 = integer
*          -32 = floating point
*           64 = long integer
*          -64 = double precision
*        Obviously these types limit those that an NDF can have.
*     ID = INTEGER (Given)
*        An NDF identifier. If 0 then a new NDF is created.
*     IP = INTEGER (Given)
*        Pointer to the image data.
*     NX = INTEGER (Given)
*        The X dimension of the NDF.
*     NY = INTEGER (Given)
*        The Y dimension of the NDF.
*     COMP = CHARACTER * ( * ) (Given)
*        The data component of the NDF to replace (must be 'data' or
*        'variance', if 'quality' then the behaviour is undefined).
*     HEAD = CHARACTER( NHEAD ) * ( * ) (Given)
*        The FITS headers to write out with the image.
*     NHEAD = INTEGER (Given)
*        Number of headers given.
*     STATUS = INTEGER (Returned)
*        The global status on exit.

*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  History:
*     16-JAN-1996 (PWD):
*        Original version.
*     28-JUN-1996 (PWD):
*        Now propagates NDF.
*     03-DEC-1997 (PWD):
*        Now accepts the name of the component to replace.
*     28-JUL-1999 (PWD):
*        Added control of the NDF history component. This is because the
*        command-line args may not exist (compiler dependent).
*     04-AUG-1999 (PWD):
*        Followup to above. There is now a call
*        ndfInit(argc,argv,status), that will do this job
*        correctly. Left new code in place as more informative than
*        default.
*     30-MAY-2001 (PWD):
*        Now supports _DOUBLE data directly rather than presuming _REAL.
*     02-SEP-2004 (PWD):
*        Converted to use CNF_PVAL for pointers.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! HDS/DAT parameters
      INCLUDE 'NDF_PAR'         ! NDF parameters
      INCLUDE 'AST_PAR'         ! AST parameters
      INCLUDE 'PSX_ERR'         ! PSX error codes
      INCLUDE 'CNF_PAR'         ! CNF functions

*  Arguments Given:
      CHARACTER * ( * ) NAME
      INTEGER TYPE
      INTEGER ID
      INTEGER IP
      INTEGER NX
      INTEGER NY
      CHARACTER * ( * ) COMP
      INTEGER NHEAD
      CHARACTER * ( * ) HEAD( NHEAD )

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of string

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) FITLOC ! Locator to FITS extension
      CHARACTER * ( NDF__SZTYP ) DTYPE ! Type of NDF data array
      INTEGER DIM( 1 )          ! Dimension of FITS extension
      INTEGER EL                ! Number of pixels in NDF
      INTEGER IAT               ! Position of .sdf in name
      INTEGER IDNEW             ! Identifier of new NDF
      INTEGER IERR              ! Not used
      INTEGER IPDAT             ! Pointer to NDF data array
      INTEGER IPFIT             ! Pointer to mapped FITS extension
      INTEGER IPQUAL            ! Pointer to quality data
      INTEGER NERR              ! Not used
      INTEGER NFITS             ! Number of FITS cards mapped.
      INTEGER PLACE             ! NDF placeholder
      INTEGER UBND( 2 )         ! NDF dimensions
      INTEGER IWCS              ! Pointer to WCS object
      LOGICAL HVQUAL            ! Do we have some additional quality
      LOGICAL THERE             ! FITS extension exists
      CHARACTER * ( NDF__SZHIS ) TEXT( 1 ) ! History text
      CHARACTER * ( NDF__SZHMX ) APPN ! History application name

*.

*  Set the global status.
      STATUS = SAI__OK

*  Start an NDF context.
      CALL NDF_BEGIN

*  Remove the .sdf part of the name (it if exists).
      IAT = INDEX( NAME, '.sdf' )
      IF ( IAT .EQ. 0 ) THEN
         IAT = CHR_LEN( NAME )
      ELSE
         IAT = IAT - 1
      END IF
      IF ( IAT .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'No name given', STATUS )
      END IF

*  Attempt to open the new NDF.
      CALL NDF_PLACE( DAT__ROOT, NAME( :IAT ), PLACE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Set the size of the NDF.
         UBND( 1 ) = NX
         UBND( 2 ) = NY

*  Define the NDF data type.
         IF ( TYPE .EQ. -32 ) THEN
            DTYPE = '_REAL'
         ELSE IF ( TYPE .EQ. -64 ) THEN
            DTYPE = '_DOUBLE'
         ELSE IF ( TYPE .EQ. 64 ) THEN
            DTYPE = '_INT64'
         ELSE IF ( TYPE .EQ. 32 ) THEN
            DTYPE = '_INTEGER'
         ELSE IF ( TYPE .EQ. 16 ) THEN
            DTYPE = '_WORD'
         ELSE IF ( TYPE .EQ. -16 ) THEN
            DTYPE = '_UWORD'
         ELSE IF ( TYPE .EQ. 8 ) THEN
            DTYPE = '_UBYTE'
         ELSE IF ( TYPE .EQ. -8 ) THEN
            DTYPE = '_BYTE'
         END IF

*  If the NDF identifier is 0 then create a new NDF, otherwise propagate
*  the NDF.
         IF ( ID .EQ. 0 ) THEN

*  Create the NDF (use a primitive as no lower bounds info).
            CALL NDF_NEWP( DTYPE, 2, UBND, PLACE, IDNEW, STATUS )
         ELSE

*  Copy the existing NDF.
            CALL NDF_COPY( ID, PLACE, IDNEW, STATUS )
         END IF

*  Now map in the appropriate component.
         CALL NDF_MAP( IDNEW, COMP, DTYPE, 'WRITE', IPDAT, EL,
     :                 STATUS )

*  And copy the data into it.
         IF ( DTYPE .EQ. '_REAL' ) THEN
            CALL VEC_RTOR( .FALSE., EL, %VAL( CNF_PVAL( IP ) ),
     :                     %VAL( CNF_PVAL( IPDAT ) ),
     :                     IERR, NERR, STATUS )
         ELSE IF ( DTYPE .EQ. '_DOUBLE' ) THEN
            CALL VEC_DTOD( .FALSE., EL, %VAL( CNF_PVAL( IP ) ),
     :                     %VAL( CNF_PVAL( IPDAT ) ),
     :                     IERR, NERR, STATUS )
         ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
            CALL VEC_ITOI( .FALSE., EL, %VAL( CNF_PVAL( IP ) ),
     :                     %VAL( CNF_PVAL( IPDAT ) ),
     :                     IERR, NERR, STATUS )
         ELSE IF ( DTYPE .EQ. '_INT64' ) THEN
            CALL VEC_KTOK( .FALSE., EL, %VAL( CNF_PVAL( IP ) ),
     :                     %VAL( CNF_PVAL( IPDAT ) ),
     :                     IERR, NERR, STATUS )
         ELSE IF ( DTYPE .EQ. '_WORD' ) THEN
            CALL VEC_WTOW( .FALSE., EL, %VAL( CNF_PVAL( IP ) ),
     :                     %VAL( CNF_PVAL( IPDAT ) ),
     :                     IERR, NERR, STATUS )
         ELSE IF ( DTYPE .EQ. '_UWORD' ) THEN
            CALL VEC_UWTOUW( .FALSE., EL, %VAL( CNF_PVAL( IP ) ),
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       IERR, NERR, STATUS )
         ELSE IF ( DTYPE .EQ. '_BYTE' ) THEN
            CALL VEC_BTOB( .FALSE., EL, %VAL( CNF_PVAL( IP ) ),
     :                     %VAL( CNF_PVAL( IPDAT ) ),
     :                     IERR, NERR, STATUS )
         ELSE IF ( DTYPE .EQ. '_UBYTE' ) THEN
            CALL VEC_UBTOUB( .FALSE., EL, %VAL( CNF_PVAL( IP ) ),
     :                       %VAL( CNF_PVAL( IPDAT ) ),
     :                       IERR, NERR, STATUS )
         END IF

*  Check for any new quality information
         IF ( ID .NE. 0 ) THEN
            CALL RTD1_AQUAL( ID, .TRUE., IPQUAL, HVQUAL )
            IF ( HVQUAL ) THEN
               CALL NDF_MAP( IDNEW, 'QUALITY', '_BYTE', 'WRITE', IPDAT,
     :                       EL, STATUS )
               CALL VEC_BTOB( .FALSE., EL, %VAL( CNF_PVAL( IPQUAL ) ),
     :                        %VAL( CNF_PVAL( IPDAT ) ), IERR, NERR,
     :                        STATUS )
            END IF
         END IF

*  Look for a WCS that can be saved in the NDF WCS component. Note this
*  also removes an AST native encoding from the FITS headers, if used.
         CALL RTD1_DEWCS( HEAD, NHEAD, .TRUE., IWCS, STATUS )
         IF ( IWCS .NE. AST__NULL ) THEN
            CALL NDF_PTWCS( IWCS, IDNEW, STATUS )
            CALL AST_ANNUL( IWCS, STATUS )
         END IF

*  Use the header information to construct the FITS extension. The
*  header information that we have replaces any that exists already (it
*  should be fundamentally the same).
         CALL NDF_XSTAT( IDNEW, 'FITS', THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_XDEL( IDNEW, 'FITS', STATUS )
         END IF
         DIM( 1 ) = NHEAD
         CALL NDF_XNEW( IDNEW, 'FITS', '_CHAR*80', 1, DIM, FITLOC,
     :                  STATUS )
         CALL DAT_MAPV( FITLOC, '_CHAR*80', 'WRITE', IPFIT, NFITS,
     :                  STATUS )
         CALL RTD1_CHEAD( HEAD, NHEAD, %VAL( CNF_PVAL( IPFIT ) ),
     :                    STATUS, %VAL( CNF_CVAL( 80 ) ) )
         CALL DAT_UNMAP( FITLOC, STATUS )

*  Take control of the NDF history component, by giving values to
*  override the default values. These values override the default ones.
         APPN = 'GAIA'
         CALL ERR_MARK
         CALL PSX_GETENV( 'GAIA_VERSION', APPN( 7: ), STATUS )
         IF ( STATUS .EQ. PSX__NOENV ) CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE
         TEXT( 1 ) = 'Created by GAIA save command'
         CALL NDF_HPUT( 'QUIET', APPN, .TRUE., 1, TEXT, .FALSE.,
     :                  .FALSE., .FALSE., IDNEW, STATUS )
      END IF

*  Make sure that all the NDF resources etc. are released.
      CALL NDF_END( STATUS )
      END
