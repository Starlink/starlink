      SUBROUTINE BIG1_OBJST( DTYPE, FRAME, XDIM, YDIM, XORIG, YORIG,
     :                       XPOS, YPOS, INTENS, NOBJ, GSIGM, CROSS,
     :                       COMIX, ELLIP, ANGLE, BACK, SAT, NSIGMA,
     :                       EXPAND, SCALE, STATUS )
*+
*  Name:
*     BIG1_OBJST

*  Purpose:
*     To add PISA type model stars to an image at the positions and
*     with the intensities given.  These stars can also have an
*     ellipticity at a specific position angle.
*     This routine is a harness for the BGG1_OBJS<T> routines.

*  Language:
*     Starlink-Fortran-77

*  Invocation:
*     SUBROUTINE BIG1_OBJST( DTYPE, FRAME, XDIM, YDIM, XORIG, YORIG,
*                            XPOS, YPOS, INTENS, NOBJ, GSIGM, CROSS,
*                            COMIX, ELLIP, ANGLE, BACK, SAT, NSIGMA,
*                            EXPAND, SCALE, STATUS )

*  Description:
*     The routine generates model objects using the analytical function
*     used by PISAFIND ie. a mixed gaussian - lorentzian - exponential.
*     The objects positions and integrated intensities are given.  The
*     output objects can be elliptical. If the expand factor is true
*     then the objects are produced on an expanded image scale (
*     expanded by factor scale ) with preserved intensity. This
*     oversamples the objects and is useful if the output is to be used
*     for PSF resampling. If expand is set to false the output image is
*     at the normal scale. If scale is set to larger than one in this
*     mode then a pseudo integration is performed on a grid scale*scale
*     within each pixel. This improves the accuracy of the intensities
*     within each pixel. The output values are confined to be less
*     equal to a given saturation value. An illegal secondary function
*     is used for the model intensities, this is a hang-on from the
*     routine APM original.

*  Arguments:
*     FRAME( 1 ) = unknown (Returned)
*        The output array containing the generated objects.
*     XDIM, YDIM = INTEGER (Given)
*        The size of the output array.
*     XORIG, YORIG = INTEGER (Given)
*        The offsets of the start of the output array in the coordinates
*        of the output objects.
*     XPOS( NOBJ ) = DOUBLE PRECISION (Given)
*        X position  of objects
*     YPOS( NOBJ ) = DOUBLE PRECISION (Given)
*        Y position of objects
*     INTENS( NOBJ ) = DOUBLE PRECISION (Given)
*        Integrated intensities of objects.
*     NOBJ = INTEGER (Given)
*        The number of objects to generate.
*     GSIGM = _REAL (Read)
*        The value of the gaussian sigma.
*     CROSS = _REAL (Read)
*        The value of the percentage cross over point, from the
*        gaussian core to the exponential wings.
*     COMIX = _REAL (Read)
*        The value of the mixture ratio between the lorentzian
*        and the other functions
*     ELLIP( NOBJ )= DOUBLE PRECISION (Given)
*        The ellipticity of the generated objects.
*     ANGLE = REAL (Given)
*        The angle of the major axis of the output objects, measure in
*        degrees anti-clockwise with respect to the x-axis.
*     BACK = REAL (Given)
*        The background value for output array.
*     SAT = REAL (Given)
*        The highest possible value an output pixel can take. Any output
*        pixels with a value greater than this value will be set to it.
*     NSIGMA = REAL (Given)
*        The number of gaussian sigma to generate object out to.
*     EXPAND = LOGICAL (Given)
*        If true then the output data array is scale times larger than
*        the actual coordinates of the objects. The model data is
*        expanded by scale to fit this data frame. This mode essentially
*        produces an oversampled version of the output data. If false
*        then the data array is the same as the object coordinates. This
*        mode is the same as the oversampled mode except that the data
*        is then binned down by scale. This performs an integration
*        within each pixel if scale is larger than 1.
*     SCALE = INTEGER (Given)
*        The factor by which the data is to be oversampled by. See the
*        EXPAND description.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1990 (PDRAPER):
*        Original version.
*     15-MAY-1992 (PDRAPER):
*        Changed to use passed lists of positions and intensities.
*     28-JUN-1993 (PDRAPER):
*        Added variable ellipticity option.
*     3-JUL-1998 (MBT):
*        Rewritten as harness for generic routines.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      COMMON /PM    / PARM1, PARM2, PARMN, PARSQ, CHANGE, PARRAD
      COMMON /PMN   / PARMN1, PARMN2, PARMNN, CMIX

*  Arguments Given:
      CHARACTER * ( * ) DTYPE
      INTEGER XDIM, YDIM
      INTEGER XORIG, YORIG
      INTEGER NOBJ
      REAL GSIGM, CROSS, COMIX
      REAL ANGLE
      REAL BACK
      REAL SAT
      REAL NSIGMA
      LOGICAL EXPAND
      INTEGER SCALE
      DOUBLE PRECISION ELLIP( NOBJ )
      DOUBLE PRECISION XPOS( NOBJ )
      DOUBLE PRECISION YPOS( NOBJ )
      DOUBLE PRECISION INTENS( NOBJ )

*  Arguments Returned:
      INTEGER FRAME( 1 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

*  External references:

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call appropriate routine according to data type.
      IF      ( DTYPE .EQ. '_WORD'    ) THEN
         CALL BIG1_OBJSW( DTYPE, FRAME, XDIM, YDIM, XORIG, YORIG,
     :                    XPOS, YPOS, INTENS, NOBJ, GSIGM, CROSS,
     :                    COMIX, ELLIP, ANGLE, BACK, SAT, NSIGMA,
     :                    EXPAND, SCALE, STATUS )
      ELSE IF ( DTYPE .EQ. '_INTEGER' ) THEN
         CALL BIG1_OBJSI( DTYPE, FRAME, XDIM, YDIM, XORIG, YORIG,
     :                    XPOS, YPOS, INTENS, NOBJ, GSIGM, CROSS,
     :                    COMIX, ELLIP, ANGLE, BACK, SAT, NSIGMA,
     :                    EXPAND, SCALE, STATUS )
      ELSE IF ( DTYPE .EQ. '_REAL'    ) THEN
         CALL BIG1_OBJSR( DTYPE, FRAME, XDIM, YDIM, XORIG, YORIG,
     :                    XPOS, YPOS, INTENS, NOBJ, GSIGM, CROSS,
     :                    COMIX, ELLIP, ANGLE, BACK, SAT, NSIGMA,
     :                    EXPAND, SCALE, STATUS )
      ELSE IF ( DTYPE .EQ. '_DOUBLE'  ) THEN
         CALL BIG1_OBJSD( DTYPE, FRAME, XDIM, YDIM, XORIG, YORIG,
     :                    XPOS, YPOS, INTENS, NOBJ, GSIGM, CROSS,
     :                    COMIX, ELLIP, ANGLE, BACK, SAT, NSIGMA,
     :                    EXPAND, SCALE, STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'DTYPE', DTYPE )
         CALL ERR_REP( 'CCDBGEN_TYPE',
        :              'CCDBGEN: Invalid data type (^DTYPE)', STATUS )
      END IF

*  All done
      END
* $Id: ccd1_objs.f,v 1.1 1997/06/27 09:01:41 pwd Exp $
