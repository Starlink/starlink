#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "ast.h"
#include "star/kaplibs.h"
#include "star/grp.h"
#include "par.h"
#include "prm_par.h"
#include "cupid.h"
#include <math.h>
#include <string.h>
#include <stdio.h>

void clumps() {
/*
*+
*  Name:
*     CLUMPS

*  Purpose:
*     Identify clumps of emission within a 1, 2 or 3 dimensional NDF.

*  Language:
*     C

*  Type of Module:
*     ADAM A-task

*  Synopsis:
*     void clumps();

*  Description:
*     This application identifies clumps within a 1, 2 or 3 dimensional NDF,
*     storing information about the clumps in the CUPID extension of the
*     supplied NDF. Optionally, an output catalogue can be created containing 
*     the clump parameters. The algorithm used to identify the clumps can be 
*     chosen from a list of supported algorithms.

*  Usage:
*     clumps in outcat method mask

*  ADAM Parameters:
*     CONFIG = GROUP (Read)
*        Specifies values for the configuration parameters used by the
*        clump finding algorithms. If the string "def" (case-insensitive)
*        or a null (!) value is supplied, a set of default configuration 
*        parameter values will be used.
*
*        A comma-separated list of strings should be given in which each
*        string is either a "keyword=value" setting, or the name of a text 
*        file preceded by an up-arrow character "^". Such text files should
*        contain further comma-separated lists which will be read and 
*        interpreted in the same manner (any blank lines or lines beginning 
*        with "#"). Settings are applied in the order in which they occur 
*        within the list, with later settings over-riding any earlier 
*        settings given for the same keyword.
*
*        Each individual setting should be of the form:
*
*           <keyword>=<value>
*        
*        where <keyword> has the form "algorithm.param"; that is, the name
*        of the algorithm, followed by a dot, followed by the name of the
*        parameter to be set. The parameters available for each algorithm
*        are listed in the "Configuration Parameters" section below. Default 
*        values will be used for any unspecified parameters. [current value]
*     ILEVEL = _INTEGER (Read)
*        Controls the amount of information displayed on the screen. It
*        should be in the range 0 to 4. A value of zero will suppress all 
*        screen output. Larger values give more information (the precise 
*        information displayed depends on the algorithm being used). [1]
*     IN = NDF (Update)
*        The 1, 2 or 3 dimensional NDF to be analysed. Information about
*        the identified clumps and the configuration parameters used will 
*        be stored in the CUPID extension of the supplied NDF, and so the 
*        NDF must not be write protected. Other applications within the 
*        CUPID package can be used to display this information in various 
*        ways.
*     MASK = _LOGICAL (Read)
*        If true, then any Quality component in the supplied NDF is
*        replaced by a mask which indicates if each pixel is inside or
*        outside a clump. Two quality bits will be used; one is set if
*        and only if the pixel is contained within one or more clumps,
*        the other is set if and only if the pixel is not contained within 
*        any clump. These two quality bits have names associated with
*        them which can be used with the KAPPA applications SETQUAL, 
*        QUALTOBAD, REMQUAL, SHOWQUAL. The names used are "CLUMP" and
*        "BACKGROUND". [current value]
*     METHOD = LITERAL (Read)
*        The algorithm to use. Each algorithm is described in more detail
*        in the "Algorithms:" section below. Can be one of:
*
*        - GaussClumps
*        - ClumpFind
*
*        Each algorithm has a collection of extra tuning values which are
*        set via the CONFIG parameter.   [current value]
*     OUT = NDF (Write)
*        Only used if METHOD is GaussClumps. It is an optional output NDF in 
*        which to store the sum of all the fitted Gaussian clumps. No output 
*        NDF will be produced if a null (!) value is supplied. Otherwise, the 
*        output NDF will be the same shape and size as the input NDF, and will 
*        inherit its AXIS, WCS and QUALITY components (plus any extensions). 
*        The Data array will be filled with the sum of the fitted Gaussian 
*        models for all the clumps which have been found. [!]
*     OUTCAT = FILENAME (Write)
*        An optional output catalogue in which to store the clump parameters.
*        No catalogue will be produced if a null (!) value is supplied [!]

*  Algorithms:
*     - GaussClumps: Described by Stutski & Gusten (1990, ApJ 356, 513).
*     This algorithm proceeds by fitting a Gaussian profile to the 
*     brightest peak in the data. It then subtracts the fit from the data 
*     and iterates, fitting a new ellipse to the brightest peak in the 
*     residuals. This continues until the total value in the fitted ellipses 
*     equals the total value in the original data. Each fitted ellipse is 
*     taken to be a single clump and is added to the output catalogue. In
*     this algorithm, clumps may overlap. Any input variance component is
*     used to scale the weight associated with each pixel value when
*     performing the Gaussian fit.
*     - ClumpFind: Described by Williams et al (1994, ApJ 428, 693). This 
*     algorithm works by first contouring the data at a multiple of the
*     noise, then searches for peaks of emission which locate the clumps,
*     and then follows them down to lower intensities. No a priori clump
*     profile is assumed. In this algorithm, clumps never overlap.

*  Configuration Parameters:
*     Each supported algorithm uses several configuration parameters,
*     values for which can be specified using the CONFIG parameter. These
*     are listed below. Each parameter is specified by the algorithm
*     name, followed by a dot, followed by the parameter name. Default
*     values are shown in square brackets:
*
*     - GaussClumps.FwhmBeam: The FWHM of the instrument beam, in
*     pixels. [3.0]
*     - GaussClumps.FwhmStart: An initial guess at the ratio of the typical 
*     observed clump size to the instrument beam width. This is used to
*     determine the starting point for the algorithm which finds the best
*     fitting Gaussian for each clump. If no value is supplied, the
*     initial guess at the clump size is based on the local profile
*     around the pixel with peak value. []
*     - GaussClumps.MaxClumps: Specifies a termination criterion for
*     the GaussClumps algorithm. The algorithm will terminate when
*     "MaxClumps" clumps have been identified, or when one of the other 
*     termination criteria is met. [unlimited]
*     - GaussClumps.MinIntegral: Specifies a termination criterion 
*     for the GaussClumps algorithm. The algorithm will terminate when the 
*     difference between the integrated intensity of all subtracted clumps 
*     and the integrated intensity in the supplied data array is less than
*     or equal to "MinIntegral" times the integrated intensity in the
*     supplied data array, or when one of the other termination criteria 
*     is met. If the supplied value is negative, this termination
*     criterion is not used. [0.01]
*     - GaussClumps.MaxNF: The maximum number of function evaluations
*     allowed when fitting an individual clump. [50]
*     - GaussClumps.ModelLim: Determines the value at which each Gaussian
*     model is truncated to zero. Model values below ModelLim times the RMS
*     noise are treated as zero. [0.5]
*     - GaussClumps.RMS: The RMS noise in the data. The default value is
*     determined by the Variance component in the input data. If there is
*     no Variance component, an estimate of the global noise level in the
*     data is made and used. []
*     - GaussClumps.S0: The Chi-square stiffness parameter "S0" which 
*     encourages the peak amplitude of each fitted gaussian to be below 
*     the corresponding maximum value in the observed data (see the Stutski 
*     & Gusten paper). [0.0]
*     - GaussClumps.Sa: The Chi-square stiffness parameter "Sa" which 
*     encourages the peak amplitude of each fitted gaussian to be close to 
*     the corresponding maximum value in the observed data (see the Stutski 
*     & Gusten paper). [0.0]
*     - GaussClumps.Sc: The Chi-square stiffness parameter "Sc" which 
*     encourages the peak position of each fitted gaussian to be close to 
*     the corresponding peak position in the observed data (see the Stutski 
*     & Gusten paper). [0.0]
*     - GaussClumps.Threshold: Specifies a termination criterion for
*     the GaussClumps algorithm. The algorithm will terminate when the peak
*     value in the residuals left after subtraction of the fitted clumps 
*     is less than "Threshold" times the RMS noise in the original data,
*     or when one of the other termination criteria is met. [2.0]
*     - GaussClumps.VeloRes: The velocity resolution of the instrument, in
*     channels. [3.0]
*     - GaussClumps.VeloStart: An initial guess at the ratio of the typical 
*     observed clump velocity width to the velocity resolution. This is used to
*     determine the starting point for the algorithm which finds the best
*     fitting Gaussian for each clump. If no value is supplied, the
*     initial guess at the clump velocity width is based on the local profile
*     around the pixel with peak value. []
*     - GaussClumps.Wmin: This parameter, together with GaussClumps.Wwidth, 
*     determines which input data values are used when fitting a Gaussian to 
*     a given peak in the data array. It specifies the minimum weight
*     which is to be used (normalised to a maximum weight value of 1.0). 
*     Pixels with weight smaller than this value are not included in the 
*     fitting process. [0.05]
*     - GaussClumps.Wwidth: This parameter, together with GaussClumps.Wmin, 
*     determines which input data values are used when fitting a Gaussian to 
*     a given peak in the data array. It is the ratio of the width of the
*     Gaussian weighting function (used to weight the data around each clump 
*     during the fitting process), to the width of the initial guess Guassian 
*     used as the starting point for the Gaussian fitting process. The
*     Gaussian weighting function has the same centre as the initial guess 
*     Gaussian. [2.0]

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     28-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/      

/* Local Variables: */
   AstFrameSet *iwcs;           /* Pointer to the WCS FrameSet */
   AstKeyMap *keymap;           /* Pointer to KeyMap holding config settings */
   AstMapping *map;             /* Current->base Mapping from WCS FrameSet */
   AstMapping *tmap;            /* Unused Mapping */
   Grp *grp;                    /* GRP identifier for configuration settings */
   char *clist;                 /* List of HDS locators for Clump structures */
   char *value;                 /* Pointer to GRP element buffer */
   char attr[ 30 ];             /* AST attribute name */
   char buffer[ GRP__SZNAM ];   /* Buffer for GRP element */
   char dtype[ 20 ];            /* NDF data type */
   char itype[ 20 ];            /* NDF data type */
   char method[ 15 ];           /* Algorithm string supplied by user */
   char qlocs[ 5 ][ DAT__SZLOC + 1 ]; /* HDS locators for quality name information */
   char xloc[ DAT__SZLOC + 1 ]; /* HDS locator for CUPID extension */
   const char *lab;             /* AST Label attribute for an axis */
   const char *sys;             /* AST System attribute for an axis */
   double *ipv;                 /* Pointer to Variance array */
   double rms;                  /* Global rms error in data */
   double sum;                  /* Sum of variances */
   float *rmask;                /* Pointer to cump mask array */
   int dim[ NDF__MXDIM ];       /* Pixel axis dimensions */
   int el;                      /* Number of array elements mapped */
   int i;                       /* Loop count */
   int ifr;                     /* Index of Frame within WCS FrameSet */
   int ilevel;                  /* Interaction level */
   int indf2;                   /* Identifier for output NDF */
   int indf;                    /* Identifier for input NDF */
   int mask;                    /* Write a mask to the supplied NDF? */
   int n;                       /* Number of values summed in "sum" */
   int nclump;                  /* Number of locators in "clist" */
   int ndim;                    /* Total number of pixel axes */
   int nfr;                     /* Number of Frames within WCS FrameSet */
   int nsig;                    /* Number of significant pixel axes */
   int sdim[ NDF__MXDIM ];      /* The indices of the significant pixel axes */
   int size;                    /* Size of the "grp" group */
   int slbnd[ NDF__MXDIM ];     /* The lower bounds of the significant pixel axes */
   int subnd[ NDF__MXDIM ];     /* The upper bounds of the significant pixel axes */
   int there;                   /* Extension exists? */
   int type;                    /* Integer identifier for data type */
   int var;                     /* Does the i/p NDF have a Variance component? */
   int vax;                     /* Index of the velocity WCS axis (if any) */
   int velax;                   /* Index of the velocity pixel axis (if any) */
   void *ipd;                   /* Pointer to Data array */
   void *ipo;                   /* Pointer to output Data array */
   
/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* astSetWatchId( 4377 );  */

/* Start an AST context */
   astBegin;

/* Start an NDF context */
   ndfBegin();

/* Get an identifier for the input NDF. */
   ndfAssoc( "IN", "UPDATE", &indf, status );

/* Get the dimensions of the NDF, and count the significant ones. */
   ndfDim( indf, NDF__MXDIM, dim, &ndim, status );
   nsig = 0;
   for( i = 0; i < ndim; i++ ) {
      if( dim[ i ] > 1 ) nsig++;
   }

/* Abort if the NDF is not 1-, 2- or 3- dimensional. */
   if( nsig > 3 ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         ndfMsg( "NDF", indf );
         msgSeti( "NSIG", nsig );
         errRep( "CLUMPS_ERR2", "\"^NDF\" has ^NSIG significant "
                 "pixel axes", status );
         errRep( "CLUMPS_ERR4", "This application requires 1, 2 or 3 "
                 "significant pixel axes", status );
      }
      goto L999;
   }          

/* Get the WCS FrameSet and the significant axis bounds. */
   kpg1Asget( indf, nsig, 1, 0, 0, sdim, slbnd, subnd, &iwcs, status );

/* If the NDF has 3 pixel axes, identify the velocity axis. */
   if( nsig == 3 && astGetI( iwcs, "Naxes" ) == 3 ) {

/* Search all Frames in the FrameSet, starting with the current Frame. */
      vax = 0;
      nfr = astGetI( iwcs, "Nframe" );
      for( ifr = 0; ifr <= nfr && vax == 0; ifr++ ) {
         if( ifr > 0 ) astSetI( iwcs, "Current", ifr );

/* Check the AST System attribute associated with each axis of the
   current WCS Frame, looking for an axis with a known velocity system. 
   Note the one-based index of the axis when and if found. */
         for( i = 1; i <= 3; i++ ) {
            sprintf(attr, "System(%d)", i );
            sys = astGetC( iwcs, attr );
            if( sys ) {
               if( !strcmp( "VRAD", sys ) || 
                   !strcmp( "VOPT", sys ) ||
                   !strcmp( "VELO", sys ) ) {
                  vax = i;
                  break;
               }
            }
         }

/* If no SpecFrame was found, look for an axis labelcontaining a letter
   "V". */
         if( vax == 0 ) {
            for( i = 1; i <= 3; i++ ) {
               sprintf(attr, "Label(%d)", i );
               lab = astGetC( iwcs, attr );
               if( lab && strpbrk( lab, "Vv" ) ){
                  vax = i;
                  break;
               }
            }
         }
      }

/* Identify the pixel axis corresponding to the velocity WCS axis.
   astMapSplit uses one-based axis indices, so we need to convert to and
   from zero-based for further use. */
      velax = -1;
      if( vax != 0 ) {
         map = astGetMapping( iwcs, AST__CURRENT, AST__BASE );
         astMapSplit( map, 1, &vax, &velax, &tmap );
         if( tmap ) {
            velax--;
         } else {
            velax = -1;
         }         
      }

/* Issue a warnining if no velocity axis was found, and use pixel axis 3. */
      if( velax == -1 ) {
         velax = 2;
         msgOut( "", "WARNING: Cannot identify a velocity axis within the "
                 "supplied NDF. Assuming pixel axis 3 is the velocity axis.", 
                 status );
         goto L999;
      }
   }         

/* Choose the data type to use when mapping the NDF Data array. */
   ndfMtype( "_REAL,_DOUBLE", indf, indf, "DATA", itype, 20, dtype, 20,
             status );
   if( !strcmp( itype, "_DOUBLE" ) ) {
      type = CUPID__DOUBLE;
   } else {
      type = CUPID__FLOAT;
   }

/* Map the Data array. */
   ndfMap( indf, "DATA", itype, "READ", &ipd, &el, status );

/* Get the interaction level. */
   parGdr0i( "ILEVEL", 1, 0, 4, 1, &ilevel, status );

/* If there is a variance array, map it and find the global RMS error. */
   ndfState( indf, "VARIANCE", &var, status );
   if( *status == SAI__OK && var ) {   
      ndfMap( indf, "VARIANCE", "_DOUBLE", "READ", (void *) &ipv, &el, status );

      sum = 0.0;
      n = 0;
      for( i = 0; i < el; i++ ) {
         if( ipv[ i ] != VAL__BADD ) {
            sum += ipv[ i ]*ipv[ i ];
            n++;
         }
      }

      if( n > 0 ) {
         rms = sqrtf( sum/n );
         if( ilevel > 2 ) {
            msgSetd( "N", rms );
            msgOut( "", "RMS noise from Variance component: ^N", status );
         }
      } else {
         *status = SAI__ERROR;
         errRep( "CLUMPS_ERR1", "The supplied data contains insufficient "
                 "good values to continue.", status );
      }         

/* If there is no Variance component, get an estimate of the RMS noise
   based on the variations of the data values. */
   } else {
      ipv = NULL;
      rms = cupidRms( type, ipd, el, subnd[ 0 ] - slbnd[ 0 ] + 1 );
      if( ilevel > 2 ) {
         msgSetd( "N", rms );
         msgOut( "", "RMS noise from Data component: ^N", status );
      }
   }

/* Determine which algorithm to use. */
   parChoic( "METHOD", "GAUSSCLUMPS", "GAUSSCLUMPS,CLUMPFIND", 1, method,
             15,  status );

/* GaussClumps can produce an optional output NDF holding the fitted 
   Gaussians. See if this is necessary. If not, annull the error. If so,
   map the data array, filling it with zeros. */
   ipo = NULL;
   if( method && !strcmp( method, "GAUSSCLUMPS" ) ) {
      ndfProp( indf, "AXIS,WCS,QUALITY", "OUT", &indf2, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else {
         ndfMap( indf2, "DATA", itype, "WRITE/ZERO", &ipo, &el, status );
         ndfSbad( 1, indf2, "DATA", status );
      }
   }

/* If required allocate room for a mask holding bad values for points which 
   are not inside any clump. Fill it with bad values. */
   parGet0l( "MASK", &mask, status );
   if( mask ) {
      rmask = astMalloc( sizeof( float )*(size_t) el );
      if( rmask ) {
         for( i = 0; i < el; i++ ) rmask[ i ] = VAL__BADR;
      }

   } else {
      rmask = NULL;
   }

/* Abort if an error has occurred. */
   if( *status != SAI__OK ) goto L999;

/* Read a group of configuration setting. */
   grp = NULL;
   kpg1Gtgrp( "CONFIG", &grp, &size, status );

/* If no group was supplied, annul the error and create an empty KeyMap. */
   if( *status == PAR__NULL || size == 0 ) {
      if( *status != SAI__OK ) errAnnul( status );
      keymap = astKeyMap( "" );

/* If a group was supplied, see if it consists of the single value "def".
   If so,create an empty KeyMap. */
   } else {
      keymap = NULL;
      if( size == 1 ) {
         value = buffer;
         grpGet( grp, 1, 1, &value, GRP__SZNAM, status );
         if( astChrMatch( value, "DEF" ) ) keymap = astKeyMap( "" );
      }

/* Otherwise, create an AST KeyMap holding the value for each configuration 
   setting, indexed using its name, then delete the GRP group. */
      if( !keymap ) kpg1Kymap( grp, &keymap, status );
      grpDelet( &grp, status );      
   }

/* Switch for each method */
   if( !strcmp( method, "GAUSSCLUMPS" ) ) {
      clist = cupidGaussClumps( type, nsig, slbnd, subnd, ipd, ipv, rmask, 
                                rms, keymap, velax, ilevel, ipo, &nclump ); 

   } else if( !strcmp( method, "CLUMPFIND" ) ) {
      cupidClumpFind( type, nsig, slbnd, subnd, ipd, ipv, rmask, keymap, 
                      velax, ilevel ); 

   } else if( *status == SAI__OK ) {
      msgSetc( "METH", method );
      errRep( "CLUMPS_ERR1", "Requested Method ^METH has not yet been "
              "implemented.", status );
   }

/* Delete any existing CUPID extension in the NDF, and then create a new
   one. */
   ndfXstat( indf, "CUPID", &there, status );
   if( there ) ndfXdel( indf, "CUPID", status );
   ndfXnew( indf, "CUPID", "CUPID_EXT", 0, NULL, xloc, status );

/* If a quality mask is being added to the input NDF... */
   if( mask ) {

/* Delete any existing quality name information from the supplied NDF, and 
   create a structure to hold new quality name info. */
      irqDelet( indf, status ); 
      irqNew( indf, "CUPID", qlocs, status );

/* Add in two quality names; "CLUMP"and "BACKGROUND". */
      irqAddqn( qlocs, "CLUMP", 0, "set iff a pixel is within a clump", 
                status );
      irqAddqn( qlocs, "BACKGROUND", 0, "set iff a pixel is not within a clump", 
                status );

/* Transfer the pixel mask to the NDF quality array. */
      irqSetqm( qlocs, 1, "BACKGROUND", el, rmask, &n, status );
      irqSetqm( qlocs, 0, "CLUMP", el, rmask, &n, status );
   }

/* Store the configuration in the CUPID extension. */
   cupidStoreConfig( xloc, keymap );

/* Store the clump properties in the CUPID extensionand output catalogue
   (if needed). This also annuls the HDS locators stored within "clist". */
   cupidStoreClumps( "OUTCAT", xloc, clist, nclump, nsig, 
                     "Output from CUPID:CLUMPS" );

/* Tidy up */
L999:

/* Release the quality name information. */
   if( mask ) {
      rmask = astFree( rmask );
      irqRlse( qlocs, status );
   }

/* Relase the extension locator.*/
   datAnnul( xloc, status );

/* Release the memory containing the list of HDS structures describing the 
   clumps. The actual locators should already have been freed in
   cupidStoreClumps. */
   clist = astFree( clist );

/* If an error has occurred, delete the QUality component if a mask was
   being created, and also delete the CUPID extension. */
   if( *status != SAI__OK ) {
      errBegin( status );
      if( mask ) ndfReset( indf, "QUALITY", status );
      ndfXdel( indf, "CUPID", status );
      errEnd( status );
   }

/* End the NDF context */
   ndfEnd( status );

/* End the AST context */
   astEnd;

/*
printf("\n\n NEED TO REMOVE THE CALL TO astListIssued IN CLUMPS.C\n\n");
astListIssued( "At end of CLUMPS");
*/

/* If an error has occurred, issue another error report identifying the 
   program which has failed (i.e. this one). */
   if( *status != SAI__OK ) {
      errRep( "CLUMPS_ERR", "CLUMPS: Failed to identify clumps of emission "
              "within a 1, 2 or 3-D NDF.", status );
   }
}

