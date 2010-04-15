*+  OBSRAP - top level Rapi2d subroutine for A-task monolith for observing

      SUBROUTINE OBSRAP (STATUS)

*    Description :
*
*     This is the top level monolith subroutine for the Rapi2d suite
*     of A-tasks. The value of NAME is input from the interface and
*     parsed, the requested A-task being called on successful matching
*     of the input string with a valid task name.
*
*     This version of RAPI2D is for observing and contains several A-tasks
*     not for general release.
*
*    Invocation :
*
*     CALL OBSRAP( NAME, STATUS)
*
*    Method :
*
*     The input string NAME is tested against all the valid A-task
*     names after having been forced to upper-case. If a valid test
*     is made, the relevant A-task is called. If not, an error message
*     is output to the environment.
*
*    Deficiencies :
*
*     The input string has to be forced to upper-case (I think).
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Colin Aspin (UKTH::CAA)
*
*    History :
*
*     02-01-1986 : First documented implementation (REVA::MJM)
*     15-01-1987 : Created this version called OBSRAP (UKTH::CAA)
*     02-02-1987 : Added STCOADD, BINUP and POLCAL (UKTH::CAA)
*     20-03-1987 : Added ARGSAVE and ABCOM (UKTH::CAA)
*     29-08-1987 : Removed POLCAL, POLSHOT, THETAFIX to POLRAP (JACH::CAA)
*     27-Apr-1994  Deleted some routines with move to NDF (SKL@JACH)
*     19-Aug-1994  Changed style to new UNIX/VMS Atask monolith style(SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'PAR_PAR'          ! Necessary for non-VMS
      INCLUDE  'NDF_PAR'
      INCLUDE  'NDF_ERR'

      EXTERNAL INDEX              ! so as not to confuse with the intrinsic

      INTEGER  STATUS             ! global status parameter

      CHARACTER*(PAR__SZNAM) NAME        ! action name

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN

         RETURN

      END IF

*    start NDF context

      CALL NDF_BEGIN

*    get the action name

      CALL TASK_GET_NAME( NAME, STATUS )

*    force the input string to upper-case before testing

      CALL UPCASE( NAME, NAME, STATUS)

*    check the string against valid A-task names - if matched then
*    call the relevant A-task

      IF( NAME .EQ. 'ABCOM') THEN

*       combines odd and even channels into two images

         CALL ABCOM ( STATUS )

      ELSE IF( NAME .EQ. 'ABSEP') THEN

*       separate odd and even channels into two images

         CALL ABSEP ( STATUS )

      ELSE IF( NAME .EQ. 'AMCORR') THEN

*       air mass correction program

         CALL AMCORR ( STATUS )

      ELSE IF( NAME .EQ. 'ANNSTATS') THEN

*       annulus statistics using whole pixels

         CALL ANNSTATS ( STATUS )

      ELSE IF( NAME .EQ. 'ASCIIFILE') THEN

*       opens, writes and closes ascii file

         CALL ASCIIFILE ( STATUS )

      ELSE IF( NAME .EQ. 'ASCIILIST') THEN

*       creates an ascii list file from image

         CALL ASCIILIST ( STATUS )

      ELSE IF ( NAME .EQ. 'APERPHOT' ) THEN

*       aperture photometry using concentric sky annulus

         CALL APERPHOT ( STATUS )

      ELSE IF ( NAME .EQ. 'APPLYMASK' ) THEN

*       applies a bad pixel mask to image settign bad pixels to magic no.

         CALL APPLYMASK ( STATUS )

      ELSE IF ( NAME .EQ. 'AUTOMOS' ) THEN

*       auto matches N images into mosaicable images

         CALL AUTOMOS ( STATUS )

      ELSE IF ( NAME .EQ. 'BINUP' ) THEN

*       bins up an array by M by N to increase S/N

         CALL BINUP ( STATUS )

      ELSE IF ( NAME .EQ. 'CALCOL' ) THEN

*       calculates colours from input magnitudes images

         CALL CALCOL ( STATUS )

      ELSE IF ( NAME .EQ. 'JSDCLEAN' ) THEN

*       cleans an image usinh James Dunlop's algorithm

!         CALL CLEAN ( STATUS )

      ELSE IF ( NAME .EQ. 'COLCYCLE' ) THEN

*       cycles colour tables N times

         CALL COLCYCLE ( STATUS )

      ELSE IF ( NAME .EQ. 'COLMED' ) THEN

*       median files down columns and creates 1d slice image

         CALL COLMED ( STATUS )

      ELSE IF ( NAME .EQ. 'COPFILE') THEN

*       copy ascii file to another file line by line

	  CALL COPFILE( STATUS)

      ELSE IF ( NAME .EQ. 'CRECOLT' ) THEN

*       creates a colour table by selection of pens and colours/intensities

         CALL CRECOLT ( STATUS )

      ELSE IF ( NAME .EQ. 'CREQUILT' ) THEN

*       allows user to generate QUILT offset file for mosaicing

         CALL CREQUILT ( STATUS )

      ELSE IF ( NAME .EQ. 'DEFGRAD' ) THEN

*       defines gradient across image from median of start/end columns

         CALL DEFGRAD ( STATUS )

      ELSE IF ( NAME .EQ. 'DELFILE' ) THEN

*       delete a file using status='delete' on f77 close statement

         CALL DELFILE ( STATUS )

      ELSE IF ( NAME .EQ. 'DIST' ) THEN

*       calculates the distance and position angle between points

         CALL DIST ( STATUS )

      ELSE IF ( NAME .EQ. 'FCOADD' ) THEN

*       coadd N images from either full names or prefix and numbers

         CALL FCOADD ( STATUS )

      ELSE IF ( NAME .EQ. 'FIGNDF' ) THEN

*       puts a .DATA_ARRAY from one IRCAM image into FIGARO compatible
*       NDF image using template FIGARO file

         CALL FIGNDF ( STATUS )

      ELSE IF ( NAME .EQ. 'FINDPEAK' ) THEN

*       find peak pixel in input sub-image

         CALL FINDPEAK ( STATUS )

      ELSE IF ( NAME .EQ. 'GAUSSTH' ) THEN

*       gaussian smooth of image below threshold

         CALL GAUSSTH ( STATUS )

      ELSE IF ( NAME .EQ. 'HISTGEN' ) THEN

*       calculates histogram of an image/sub-image and stores in HDS file

         CALL HISTGEN ( STATUS )

      ELSE IF ( NAME .EQ. 'HOTSHOT' ) THEN

*       marks or removes hot pixels via sigma cut

         CALL HOTSHOT ( STATUS )

      ELSE IF ( NAME .EQ. 'IMCOMB' ) THEN

*       combines images by sclaing and insertion

         CALL IMCOMB ( STATUS )


      ELSE IF ( NAME .EQ. 'INDEX' ) THEN

*       index's through an observation container file creating index list

         CALL INDEX ( STATUS )

      ELSE IF ( NAME .EQ. 'INSETB' ) THEN

*       sets pixels inside box to new value

         CALL INSETB ( STATUS )

      ELSE IF ( NAME .EQ. 'INSETC' ) THEN

*       sets pixels inside circle to new value

         CALL INSETC ( STATUS )

      ELSE IF ( NAME .EQ. 'INTLK' ) THEN

*       creates an integer picture of an image/sub-image

         CALL INTLK ( STATUS )

      ELSE IF ( NAME .EQ. 'LINCONT' ) THEN

*       linearity correction of whole container file

         CALL LINCONT ( STATUS )

      ELSE IF ( NAME .EQ. 'LINIMAG_NDR' ) THEN

*       linearity correction of single image NDR

         CALL LINIMAG_NDR ( STATUS )

      ELSE IF ( NAME .EQ. 'LOWCASE' ) THEN

*       convert string to lower case

         CALL LOWCASE ( STATUS )

      ELSE IF ( NAME .EQ. 'MAKEBAD' ) THEN

*       makes bad pixel list from n-sigma cut on image

         CALL MAKEBAD ( STATUS )

      ELSE IF ( NAME .EQ. 'MAKEGLITCH' ) THEN

*       makes bad pixel list from image

         CALL MAKEGLITCH ( STATUS )

      ELSE IF ( NAME .EQ. 'MAKEMASK' ) THEN

*       makes bad pixel maks from n-sigma cut on image

         CALL MAKEMASK ( STATUS )

      ELSE IF ( NAME .EQ. 'MANYCOL' ) THEN

*       combines several colour tables into one

         CALL MANYCOL ( STATUS )

      ELSE IF ( NAME .EQ. 'MED3D' ) THEN

*       median filters a stack of images

         CALL MED3D ( STATUS )

      ELSE IF ( NAME .EQ. 'MOSAIC2' ) THEN

*       combines 2 images into one with offsets

         CALL MOSAIC2 ( STATUS )

      ELSE IF ( NAME .EQ. 'MOSCOR' ) THEN

*       calculates dc offset in overlap region between two images

         CALL MOSCOR ( STATUS )

      ELSE IF ( NAME .EQ. 'OBSEXT' ) THEN

*       extracts an observation from a container file and creates image

         CALL OBSEXT ( STATUS )

      ELSE IF ( NAME .EQ. 'OBSLIST' ) THEN

*       list out one parameter from observation file by number

         CALL OBSLIST ( STATUS )

      ELSE IF ( NAME .EQ. 'OEFIX' ) THEN

*       fixes the difference between odd and even channel data in image

         CALL OEFIX ( STATUS )

      ELSE IF ( NAME .EQ. 'OUTSETB' ) THEN

*       sets pixels outside box to new value

         CALL OUTSETB ( STATUS )

      ELSE IF ( NAME .EQ. 'PSGREY' ) THEN

*       plots a greyscale image to postscript printer

!         CALL PSGREY ( STATUS )

      ELSE IF ( NAME .EQ. 'RADIM' ) THEN

*       creates a radial profile image from image and point

         CALL RADIM ( STATUS )

      ELSE IF ( NAME .EQ. 'ROOT' ) THEN

*       takes the square root of an image

         CALL ROOT ( STATUS )

      ELSE IF ( NAME .EQ. 'ROWMED' ) THEN

*       median filters across rows of an image

         CALL ROWMED ( STATUS )

      ELSE IF ( NAME .EQ. 'SETVAL' ) THEN

*       sets value to another value

         CALL SETVAL ( STATUS )

      ELSE IF ( NAME .EQ. 'STEPIM' ) THEN

*       steps an image so that between limits values are mean of limits

         CALL STEPIM ( STATUS )

      ELSE IF ( NAME .EQ. 'STCOADD' ) THEN

*       coadds N observation images from container file

         CALL STCOADD ( STATUS )

      ELSE IF ( NAME .EQ. 'TYPFILE') THEN

*       type a file to terminal line by line

	 CALL TYPFILE( STATUS)

      ELSE IF ( NAME .EQ. 'WELCOME_OBSRAP' ) THEN

*       welcome info for auto loading of task ...

          CALL WELCOME_OBSRAP ( STATUS)

      ELSE IF ( NAME .EQ. 'WMOSAIC' ) THEN

*       mosaic using user defined weighting

          CALL WMOSAIC ( STATUS)

      ELSE IF ( NAME .EQ. 'WQUILT' ) THEN

*       mosaic using user defined weighting

          CALL WQUILT ( STATUS)

      ELSE IF ( NAME .EQ. 'WRAPCOR' ) THEN

*       corrects for wrap around due to bit threshold

          CALL WRAPCOR ( STATUS)

      ELSE IF ( NAME .EQ. 'XGROW' ) THEN

*       duplicates 1d slice image into 2d image

         CALL XGROW ( STATUS )

      ELSE IF ( NAME .EQ. 'YADD' ) THEN

*       Adds up rows and creates average 1d slice image

         CALL YADD ( STATUS )

      ELSE IF ( NAME .EQ. 'YGROW' ) THEN

*       duplicates 1d slice image into 2d image

         CALL YGROW ( STATUS )

      ELSE

*       no such option exists

        CALL MSG_OUT( 'RAP_ERR',
     :                 'Thats quite impossible at the moment ...',
     :                 STATUS )
        CALL MSG_SETC( 'NAME', NAME)
        CALL MSG_OUT( 'MESSAGE', 'Action requested was ^NAME',
     :                 STATUS)

      END IF


* make sure all NDF locators are released

      CALL NDF_END( STATUS )


      END
