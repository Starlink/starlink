/* dxml - routines for parsing DREAM XML files */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <errno.h>
#include <expat.h>
#include <ctype.h>

#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"

#include "smurf_par.h"

#include "dream_par.h"
#include "dxml_struct.h"
#include "dxml.h"

static char errmess[132];              /* error message string */

/* Values stored from parsing XML file */

static int XML_add_atm;                /* Add atmospheric emission 1=yes, 0=no */
static int XML_add_fnoise;             /* Add 1/f noise 1=yes, 0=no */
static int XML_add_hnoise;             /* Add heater noise 1=yes, 0=no */
static int XML_add_pns;                /* Add photon noise 1=yes, 0=no */
static double XML_airmass;             /* Airmass of simulated observation */
static double XML_anang;               /* polarisation angle of analyser (deg) */
static double XML_anpol;               /* polarisation of analyser (%) */
static double XML_antrans;             /* transmission of analyser (%) */
static double XML_aomega;              /* coupling factor 0.179(850) 0.721(450) */
static double XML_astang;              /* polarisation angle of source (deg) */
static char XML_astname[DREAM__FLEN];  /* name of file for ast simulation */
static double XML_astpol;              /* polarisation of source (%) */
static double XML_atend;               /* Ambient temp. at end (Celcius) */
static char XML_atmname[DREAM__FLEN];  /* name of file for atm simulation */
static double XML_atmrefnu;            /* atm reference corner frequency (Hz)*/
static double XML_atmrefvel;           /* atm reference velocity (m/sec) */
static double XML_atmxvel;             /* atm background velocity in X */
static double XML_atmyvel;             /* atm background velocity in Y */
static double XML_atmzerox;            /* atm background offset in X */
static double XML_atmzeroy;            /* atm background offset in X */
static double XML_atstart;             /* Ambient temp. at start (Celcius) */
static double XML_bandGHz;             /* optical bandwidth in GHz */
static double XML_blindang;            /* polarisation angle of blind (deg) */
static double XML_blindpol;            /* polarisation of blind (%) */
static double XML_blindtrans;          /* transmission of blind (%) */
static double XML_bol_distx;           /* average bolometer distance */
static double XML_bol_disty;           /* average bolometer distance */
static char XML_bolfile[DREAM__FLEN];  /* name of file for bolometer
                                          details */
static double XML_bous_angle;          /* angle of pattern relative to tel. 
					  axes in rad. anticlockwise */
static double XML_bous_width;          /* width of bous pattern (arcsec) */
static double XML_bous_height;         /* height of bous pattern (arcsec) */
static double XML_bous_spacing;        /* scan line spacing in arcsec */
static double XML_bous_vmax;           /* Telescope max vel. (arcsec/sec) */
static double XML_cassang;             /* polarisation angle of Cass optics (deg) */
static double XML_casspol;             /* polarisation of Cass optics (%) */
static double XML_casstrans;           /* transmission of Cass optics (%) */
static int XML_conv_shape;             /* Possible convolution functions are Gaussian (=0) */
static double XML_conv_sig;            /* convolution function parameter */
static char XML_coordframe[80];        /* Map coordinate frame */
static double XML_dec;                 /* declination in radians */
static double XML_distfac;             /* distortion factor (0=no distortion) */
static char XML_flatname[DREAM__FLEN]; /* name of flatfield algorithm */
static int XML_heatnum;                /* number of heater steps */
static double XML_heatstart;           /* initial heater value (pW) */
static double XML_heatstep;            /* increment of heater value (pW) */
static int XML_flux2cur;               /* Convert power to current 1=yes, 0=no */
static int XML_grid_max_x;             /* The reconstruction grid max X */
static int XML_grid_max_y;             /* The reconstruction grid max Y */ 
static int XML_grid_min_x;             /* The reconstruction grid min X */
static int XML_grid_min_y;             /* The reconstruction grid min Y */                                                
static double XML_grid_step_x;         /* The reconstruction grid step size */
static double XML_grid_step_y;         /* The reconstruction grid step size */
static double XML_jig_step_x;          /* The step size in -X- direction
                                          between Jiggle positions in arcsec */
static double XML_jig_step_y;          /* The step size in -Y- direction
                                          between Jiggle positions in arcsec */
static int XML_jig_vert[DREAM__MXVERT][2]; /* Array with relative vertex
                                           coordinates in units 
                                           of pixel distance  */
static double XML_lambda;              /* wavelength in metres */
static double XML_meanatm;             /* mean expected atmospheric signal pW */
static double XML_mjdaystart;          /* Mod. Jul. Date at start */
static double XML_nasang;              /* polarisation angle of Nasmyth optics
                                          (deg) */
static double XML_naspol;              /* polarisation of Nasmyth optics (%) */
static double XML_nastrans;            /* transmission of Nasmyth optics (%) */
static int XML_nbolx;                  /* number of bolometers in X */
static int XML_nboly;                  /* number of bolometers in Y */
static int XML_ncycle;                 /* Number of cycles through the pattern */
static int XML_ngrid;                  /* Nr of points in reconstruction
                                          grid */
static int XML_numsamples;             /* number of samples */
static int XML_nvert;                  /* Nr of vertices in the Jiggle 
                                          pattern */
static char XML_obsmode[80];           /* Type of observation */
static int XML_platenum;               /* number of waveplate rotations */
static double XML_platerev;            /* waveplate rotation rev/sec */
static double XML_pong_angle;          /* angle of pattern relative to tel. 
					  axes in rad. anticlockwise */
static int XML_pong_gridcount;         /* number of grid lines (odd) */
static double XML_pong_spacing;        /* grid spacing in arcsec */
static double XML_pong_vmax;           /* Telescope max vel. (arcsec/sec) */
static double XML_ra;                  /* Right Ascension in radians */
static double XML_sample_t;            /* sample interval in msec */
static double XML_scan_angle;          /* angle of pattern relative to tel. 
					  axes in rad. anticlockwise */
static double XML_scan_pathlength;     /* length of scan path across sky (arcsec) */
static double XML_scan_vmax;           /* Telescope max vel. (arcsec/sec) */
static int XML_smu_move;               /* Code for the SMU move algorithm */
static double XML_smu_offset;          /* SMU phase shift */
static int XML_smu_samples;            /* Nr of samples per jiggle vertex */
static double XML_smu_terr;            /* SMU timing error in msec */
static char XML_subname[80];           /* name of subarray */
static int XML_subsysnr;               /* subsystem number */
static double XML_targetpow;           /* target bolometer power input pW */
static double XML_tauzen;              /* optical depth at 225GHz at the zenith */
static double XML_telemission;         /* telescope background pW */
static char XML_wt0_name[DREAM__FLEN]; /* name of file for pixel weights */
static char XML_wt1_name[DREAM__FLEN]; /* name of file for piston weights */
static double XML_xpoint;              /* X pointing offset */
static double XML_ypoint;              /* Y pointing offset */


/*+ dxml_cvtdouble - convert a parameter value to double */

void dxml_cvtdouble
(
const char *name,          /* name of parameter (given) */
const char *value,         /* value string of parameter (given) */
double *x,                 /* converted number (returned) */
int *status                /* global status (given and returned) */
)

/* Method:
    Convert a parameter value string to a double and store the result,
    checking and reporting errors.
   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    30Jun2004 : Original (bdk)
*/

{
   if ( !StatusOkP(status) ) return;

   *x = dxml_strtod ( value, status );
   if ( !StatusOkP(status) )
   {
      dxml_cvterr ( name, value, status );
   }

}



/*+ dxml_cvtsexdouble - convert a sexagesimal parameter value to double */

void dxml_cvtsexdouble
(
const char *name,          /* name of parameter (given) */
const char *value,         /* value string of parameter (given) */
double *x,                 /* converted number (returned) */
int *status                /* global status (given and returned) */
)

/* Method:
    Convert a parameter value sexagesimal string to a double and store
    the result. checking and reporting errors.
   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    13May2005 : Original (bdk)
*/

{
   if ( !StatusOkP(status) ) return;

   *x = dxml_sextod ( value, status );
   if ( !StatusOkP(status) )
   {
      dxml_cvterr ( name, value, status );
   }

}



/*+ dxml_cvterr - report value conversion error */

void dxml_cvterr
(
const char *name,          /* name of parameter (given) */
const char *value,         /* value string of parameter (given) */
int *status                /* global status (given and returned) */
)

/* Method:
    An error has been found in converting value to a number. Report this.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    30Jun2004 : Original (bdk)
*/

{
   sprintf ( errmess, 
     "mcexml:Error converting parameter: name = %s  value = %s", 
     name, value );
   ErsRep ( 0, status, errmess );
}



/*+ dxml_cvtint - convert a parameter value to int */

void dxml_cvtint
(
const char *name,          /* name of parameter (given) */
const char *value,         /* value string of parameter (given) */
int *x,                    /* converted number (returned) */
int *status                /* global status (given and returned) */
)

/* Method:
    Convert a parameter value string to an int and store the result,
    checking and reporting errors.
   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    30Jun2004 : Original (bdk)
*/

{
   if ( !StatusOkP(status) ) return;

   *x = dxml_strtol ( value, status );
   if ( !StatusOkP(status) )
   {
      dxml_cvterr ( name, value, status );
   }

}



/*+ dxml_endsimXML - callback for XML parser */

void dxml_endsimXML
(
void *userData,                 /* unused (given) */
const char *name                /* name of item (given) */
)

/* Method:
    The parser is set-up so that it calls this routine whenever it finds
    the end of an entry in the configuration file being parsed. 

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    12Nov2002 : Original (bdk)
*/

{
}



/*+ dxml_endXML - callback for XML parser */

void dxml_endXML
(
void *userData,                 /* unused (given) */
const char *name                /* name of item (given) */
)

/* Method:
    The parser is set-up so that it calls this routine whenever it finds
    the end of an entry in the configuration file being parsed. 

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    12Nov2002 : Original (bdk)
*/

{
}


/*+ dxml_initpars - initialise parameters */

void dxml_initpars
( 
int *status                /* global status (given and returned) */
)

/* Method :
    Set values which are to be updated during parsing the XML file.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    03Oct2003 : Original (bdk)
    13May2005 : add XML_dec and XML_ra (bdk)
    06Oct2005 : add flatname (bdk)
*/

{

   if ( !StatusOkP(status) ) return;

   XML_add_atm = 0;
   XML_add_fnoise = 0;
   XML_add_hnoise = 0;
   XML_add_pns = 0;
   XML_airmass = 1.2;
   XML_anang = 0.0;
   XML_anpol = 100.0;
   XML_antrans = 100.0;
   XML_aomega = 0.179;
   XML_astang = 45.0;
   strcpy ( XML_astname, "" );
   XML_astpol = 10.0;
   XML_atend = 5.00;
   strcpy ( XML_atmname, "" );
   XML_atmrefnu = 0.5;
   XML_atmrefvel = 15.0;
   XML_atmxvel = 5000.0;
   XML_atmyvel = 0.0;
   XML_atmzerox = 5000.0;
   XML_atmzeroy = 50000.0;
   XML_atstart = 5.00;
   XML_bandGHz = 35.0;
   XML_blindang = 10.0;
   XML_blindpol = 1.0;
   XML_blindtrans = 93.0; 
   XML_bol_distx = 6.28;
   XML_bol_disty = 6.28;
   strcpy ( XML_bolfile, "" );
   XML_bous_angle = 0.4636476;
   XML_bous_width = 2000.0;
   XML_bous_height = 2000.0;
   XML_bous_spacing = 240.0;
   XML_bous_vmax = 200.0;
   XML_cassang = 135.0;
   XML_casspol = 1.0;
   XML_casstrans = 98.0;   
   XML_conv_shape = 1;
   XML_conv_sig = 1.0;
   strcpy ( XML_coordframe, "" );
   XML_dec = 0.0;
   XML_distfac = 0.0;
   strcpy ( XML_flatname, "POLYNOMIAL" );
   XML_flux2cur = 1;
   XML_grid_max_x = 1;
   XML_grid_max_y = 1;
   XML_grid_min_x = 1;
   XML_grid_min_y = 1;
   XML_grid_step_x = 6.28;
   XML_grid_step_y = 6.28;
   XML_heatnum = 1;
   XML_heatstart = 0.0;
   XML_heatstep = 0.0;
   XML_jig_step_x = 6.28;
   XML_jig_step_y = 6.28;
   XML_lambda = 0.85e-3;
   XML_meanatm = 7.0;
   XML_mjdaystart = 53795.0;
   XML_nasang = 90.0;
   XML_naspol = 1.0;
   XML_nastrans = 98.0;
   XML_nbolx = 40;
   XML_nboly = 32;
   XML_ncycle = 1;
   XML_ngrid = 0;            /* must be initialised to zero */
   XML_numsamples = 128;
   XML_nvert = 0;            /* must be initialised to zero */
   strcpy ( XML_obsmode, "" );
   XML_platenum = 1;
   XML_platerev = 2.0;
   XML_pong_angle = 0.4636476;
   XML_pong_gridcount = 7;
   XML_pong_spacing = 240.0;
   XML_pong_vmax = 200.0;
   XML_ra = 0.0;
   XML_sample_t = 5.0;
   XML_scan_angle = 0.4636476;
   XML_scan_pathlength = 2000.0;
   XML_scan_vmax = 200.0;
   XML_smu_move = 8;
   XML_smu_offset = 0.0;
   XML_smu_samples = 0;
   XML_smu_terr = 0.0;
   strcpy ( XML_subname, "" );
   XML_subsysnr = 1;
   XML_targetpow = 25.0;
   XML_tauzen = 0.052583;
   XML_telemission = 4.0;
   strcpy ( XML_wt0_name, "" );
   strcpy ( XML_wt1_name, "" );
   XML_xpoint = 20.0;
   XML_ypoint = 20.0;

}

/*+ dxml_makeupper - convert a string to uppercase */

char * dxml_makeupper 
(
char *lower,              /* string to be converted */
int *status               /* global status (given and returned) */
)

/* Method :
    Returns the uppercase equivalent of the supplied string. 
  
   Authors :
    J.Balfour (jbalfour@physics.ubc.ca)
    E.Chapin (echapin@phas.ubc.ca)

   History :
    15May2006 : Original (jb)
    16Aug2006 : Fixed memory leak (EC)
*/

{

  int length;             /* length of the supplied string */
  int i = 0;              /* iterator to change each character */
  char *convert;          /* uppercase version of the string */
  
  if ( *status != 0 ) return lower;
  
  /* Get the length of the original string excluding NULL terminator*/
  length = strlen(lower);
  
  /* Copy and convert the string to uppercase */
  
  convert = (char *)malloc(length+1); /* Include the terminator in length */
  strcpy (convert, lower);
  
  while (*convert != '\0') {
    *convert = toupper(*convert);
    convert++;
    i++;
  }
  
  /* Check to make sure the entire string converted */
  
  if ( length != strlen(convert - i) ) {
    *status = DITS__APP_ERROR;
    ErsRep ( 0, status, 
             "fhead library: error converting string to uppercase" );
  }
  
  return convert - i;

}


/*+ dxml_readsimXML - read simulation details from a file */

void dxml_readsimXML
( 
char *filename,            /* name of file (given) */
int *status                /* global status (given and returned) */
)

/* Method :
    Open the file, read it line-by-line and pass it to an XML parser.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    26Jan2003 : Original (bdk)
*/

{
   static char buf[132];
   XML_Parser parser = XML_ParserCreate(NULL);
   int done;
   int depth = 0;
   FILE *fd;

   if ( !StatusOkP(status) ) return;

   /* Initialise variables before reading their values from the file */

   dxml_initpars ( status );

   /* Open the XML file and read and parse its contents */

   fd = fopen ( filename, "r" );

   if ( fd == 0 )
   {
      *status = DITS__APP_ERROR;
      sprintf ( errmess, "dxml:Error opening simulator XML file %s", 
        filename );
      ErsRep ( 0, status, errmess );
   }
   else
   {

      XML_SetUserData ( parser, &depth );
      XML_SetElementHandler ( parser, dxml_startsimXML, dxml_endsimXML );

      do 
      {
         size_t len = fread ( buf, 1, sizeof(buf), fd );
         done = len < sizeof(buf);
         if (!XML_Parse(parser, buf, len, done)) 
         {
            sprintf( buf,
              "%s at line %d\n",
              XML_ErrorString(XML_GetErrorCode(parser)),
	      (double)XML_GetCurrentLineNumber(parser) );
            *status = DITS__APP_ERROR;
            sprintf ( errmess, 
              "dxml:Error parsing simulator XML file, status = %s", 
              buf );
            *status = DITS__APP_ERROR;
            ErsRep ( 0, status, errmess );
            break;
         }
      } while (!done);

      XML_ParserFree(parser);
      fclose ( fd );
   }

}



/*+ dxml_readXML - read details from a file */

void dxml_readXML
( 
char *filename,            /* name of file (given) */
int *status                /* global status (given and returned) */
)

/* Method :
    Open the file, read it line-by-line and pass it to an XML parser.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    12Nov2002 : Original (bdk)
*/

{

   static char buf[132];
   XML_Parser parser = XML_ParserCreate(NULL);
   int done;
   int depth = 0;
   FILE *fd;

   if ( !StatusOkP(status) ) return;

/* Initialise variables before reading their values from the file */
   dxml_initpars ( status );

/* Open the XML file and read and parse its contents */

   fd = fopen ( filename, "r" );

   if ( fd == 0 )
   {
      *status = DITS__APP_ERROR;
      sprintf ( errmess, "dxml:Error opening XML file %s\n", filename );
      ErsRep ( 0, status, errmess );
      strcpy ( errmess, strerror(errno) );
      ErsRep ( 0, status, errmess );
   }
   else
   {

      XML_SetUserData ( parser, &depth );
      XML_SetElementHandler ( parser, dxml_startXML, dxml_endXML );

      do 
      {
         size_t len = fread ( buf, 1, sizeof(buf), fd );
         done = len < sizeof(buf);
         if (!XML_Parse(parser, buf, len, done)) 
         {
            sprintf( buf,
              "%s at line %d\n",
              XML_ErrorString(XML_GetErrorCode(parser)),
	      (double)XML_GetCurrentLineNumber(parser) );
            *status = DITS__APP_ERROR;
            sprintf ( errmess, "dxml:Error parsing XML file, status = %s", 
              buf );
            *status = DITS__APP_ERROR;
            ErsRep ( 0, status, errmess );
            break;
         }
      } while (!done);

      XML_ParserFree(parser);
      fclose ( fd );
   }

}


/*+ dxml_report - report values from XML parse */

void dxml_report
(
char *rpt_name,           /* name of output file (given) */
struct dxml_struct inx,   /* structure for setup values (given) */
int *status                /* global status (given and returned) */
)

/* Description :
    Report the values previously stored when parsing the XML file.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    12Oct2003 : Original (bdk)
    06Oct2005 : add flatname (bdk)
*/

{
   int j;
   FILE *fd;

   fd = fopen ( rpt_name, "w" );
   fprintf ( fd, " bol_distx = %e\n", inx.bol_distx );
   fprintf ( fd, " bol_disty = %e\n", inx.bol_disty );
   fprintf ( fd, " bolfile = %s\n", inx.bolfile );
   fprintf ( fd, " conv_shape = %d\n", inx.conv_shape );
   fprintf ( fd, " conv_sig = %e\n", inx.conv_sig );
   fprintf ( fd, " coordframe = %s\n", inx.coordframe );
   fprintf ( fd, " dec = %e\n", inx.dec );
   fprintf ( fd, " distfac = %e\n", inx.distfac );
   fprintf ( fd, " flatname = %s\n", inx.flatname );
   fprintf ( fd, " heatnum = %e\n", inx.heatnum );
   fprintf ( fd, " heatstart = %e\n", inx.heatstart );
   fprintf ( fd, " heatstep = %e\n", inx.heatstep );
   fprintf ( fd, " grid_step_x = %e\n", inx.grid_step_x );
   fprintf ( fd, " grid_step_y = %e\n", inx.grid_step_y );
   fprintf ( fd, " lambda = %e\n", inx.lambda );
   fprintf ( fd, " nbolx = %d\n", inx.nbolx );
   fprintf ( fd, " nboly = %d\n", inx.nboly );
   fprintf ( fd, " ngrid = %d\n", inx.ngrid );
   fprintf ( fd, " numsamples = %d\n", inx.numsamples );
   fprintf ( fd, " obsmode = %s\n", inx.obsmode );
   fprintf ( fd, " nvert = %d\n", inx.nvert );
   fprintf ( fd, " platenum = %d\n", inx.platenum );
   fprintf ( fd, " platerev = %e\n", inx.platerev );
   fprintf ( fd, " ra = %e\n", inx.ra );
   fprintf ( fd, " sample_t = %e\n", inx.sample_t );
   fprintf ( fd, " smu_move = %d\n", inx.smu_move );
   fprintf ( fd, " smu_offset = %e\n", inx.smu_offset );
   fprintf ( fd, " smu_samples = %d\n", inx.smu_samples );
   fprintf ( fd, " subsysnr = %d\n", inx.subsysnr );
   fprintf ( fd, " targetpow = %e\n", inx.targetpow );
   fprintf ( fd, " wt0_name = %s\n", inx.wt0_name );
   fprintf ( fd, " wt1_name = %s\n", inx.wt1_name );

   fprintf ( fd, "Grid points are\n" );
   for ( j=0; j<inx.ngrid; j++ )
   {
      fprintf ( fd, "  %d  %d\n", inx.gridpts[j][0], inx.gridpts[j][1] );
   }

   fprintf ( fd, " jig_step_x = %e\n", inx.jig_step_x );
   fprintf ( fd, " jig_step_y = %e\n", inx.jig_step_y );

   fprintf ( fd, "Pattern vertices are\n" );

   for ( j=0; j<XML_nvert; j++ )
   {
      fprintf ( fd, "  %d  %d\n", inx.jig_vert[j][0], inx.jig_vert[j][1] );
   }

}


/*+ dxml_returnsimXML - return values from XML parse */

void dxml_returnsimXML
(
struct dxml_sim_struct *inx,   /* structure for returning values (returned) */
int *status                    /* global status (given and retuned) */
)

/* Description :
    Return the values previously stored when parsing the XML file.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)
    A.G.Gibb (agg@astro.ubc.ca)
    E.L.Chapin (echapin@phas.ubc.ca)

   History :
    26Jun2003 : Original (bdk)
    17Oct2003 : add meanatm and hnoise (bdk)
    16Jan2004 : add bandGHz (bdk)
    26Jan2005 : add xpoint, ypoint (agg)
    04Feb2005 : add telemission (bdk)
    14Apr2005 : add airmass (agg)
    24Jan2006 : airmass derived quantity, added pwvzen/atstart/atend (ec)
    15Jun2006 : replaced pwvzen with tauzen (jb)
*/

{
   inx->add_atm = XML_add_atm;
   inx->add_fnoise = XML_add_fnoise;
   inx->add_hnoise = XML_add_hnoise;
   inx->add_pns = XML_add_pns;
   inx->airmass = XML_airmass;
   inx->anang = XML_anang;
   inx->anpol = XML_anpol;
   inx->antrans = XML_antrans;
   inx->aomega = XML_aomega;
   inx->astang = XML_astang;
   strcpy ( inx->astname, XML_astname );
   inx->astpol = XML_astpol;
   inx->atend= XML_atend;
   strcpy ( inx->atmname, XML_atmname );
   inx->atmrefnu = XML_atmrefnu;
   inx->atmrefvel = XML_atmrefvel;
   inx->atmxvel = XML_atmxvel;
   inx->atmyvel = XML_atmyvel;
   inx->atmzerox = XML_atmzerox;
   inx->atmzeroy = XML_atmzeroy;
   inx->atstart = XML_atstart;
   inx->bandGHz = XML_bandGHz;
   inx->blindang = XML_blindang;
   inx->blindpol = XML_blindpol;
   inx->blindtrans = XML_blindtrans;
   inx->cassang = XML_cassang;
   inx->casspol = XML_casspol;
   inx->casstrans = XML_casstrans;
   inx->flux2cur = XML_flux2cur;
   inx->meanatm = XML_meanatm;
   inx->nasang = XML_nasang;
   inx->naspol = XML_naspol;
   inx->nastrans = XML_nastrans;
   inx->ncycle = XML_ncycle;
   inx->smu_terr = XML_smu_terr;
   strcpy ( inx->subname, XML_subname );
   inx->tauzen = XML_tauzen;
   inx->telemission = XML_telemission;
   inx->xpoint = XML_xpoint;
   inx->ypoint = XML_ypoint;
}


/*+ dxml_returnXML - return values from XML parse */

void dxml_returnXML
(
struct dxml_struct *inx,   /* structure for returning values (returned) */
int *status                /* global status (given and retuned) */
)

/* Description :
    Return the values previously stored when parsing the XML file.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)
    E.Chapin

   History :
    12Nov2002 : Original (bdk)
    14Feb2003 : return values in a structure (bdk)
    02Oct2003 : calculate gridpts from range instead of copying list (bdk)
    17Oct2003 : add targetpow (bdk)
    06Oct2005 : add flatname (bdk)
    18Aug2006 : Fixed leak in upper-casing of obsmode (EC)
*/

{

   int ix;                 /* grid offset */
   int iy;                 /* grid offset */
   int j;
   char *obsmode_upcase=NULL;    /* upper case string for obsmode */
   char *coordframe_upcase=NULL; /* upper case string for coordinate frame */

   inx->bol_distx = XML_bol_distx;
   inx->bol_disty = XML_bol_disty;
   strcpy ( inx->bolfile, XML_bolfile );
   inx->bous_angle=XML_bous_angle;
   inx->bous_width=XML_bous_width;
   inx->bous_height=XML_bous_height;
   inx->bous_spacing=XML_bous_spacing;
   inx->bous_vmax=XML_bous_vmax;
   inx->conv_shape = XML_conv_shape;
   inx->conv_sig = XML_conv_sig;

   /* Convert coordframe to uppercase */
   coordframe_upcase = dxml_makeupper(XML_coordframe, status );
   strcpy ( XML_coordframe, coordframe_upcase );
   free( coordframe_upcase );
   strcpy( inx->coordframe, XML_coordframe );

   inx->dec = XML_dec;
   inx->distfac = XML_distfac;
   strcpy ( inx->flatname, XML_flatname );
   inx->grid_step_x = XML_grid_step_x;
   inx->grid_step_y = XML_grid_step_y;
   inx->heatnum = XML_heatnum;
   inx->heatstart = XML_heatstart;
   inx->heatstep = XML_heatstep;

   XML_ngrid = 
     (1+XML_grid_max_y-XML_grid_min_y) * (1+XML_grid_max_x-XML_grid_min_x);
   if ( XML_ngrid <= DREAM__MXGRID )
   {
      j = 0;
      for ( iy=XML_grid_min_y; iy<=XML_grid_max_y; iy++ )
      {
         for ( ix=XML_grid_min_x; ix<=XML_grid_max_x; ix++ )
         {
            inx->gridpts[j][0] = ix;
            inx->gridpts[j][1] = iy;
            j++;
         }
      }
   }
   else
   {   
      *status = DITS__APP_ERROR;
      sprintf ( errmess, 
        "XML file: too many reconstruction grid points requested" );
      ErsRep ( 0, status, errmess );
   }

   inx->jig_step_x = XML_jig_step_x;
   inx->jig_step_y = XML_jig_step_y;

   for ( j=0; j<XML_nvert; j++ )
   {
      inx->jig_vert[j][0] = XML_jig_vert[j][0];
      inx->jig_vert[j][1] = XML_jig_vert[j][1];
   }

   inx->lambda = XML_lambda;
   inx->mjdaystart = XML_mjdaystart;
   inx->nbolx = XML_nbolx;
   inx->nboly = XML_nboly;
   inx->ngrid = XML_ngrid;
   inx->numsamples = XML_numsamples;
   inx->nvert = XML_nvert;

   /* Convert obsmode to uppercase */
   obsmode_upcase = dxml_makeupper(XML_obsmode, status );
   strcpy ( XML_obsmode, obsmode_upcase );
   free( obsmode_upcase );
   strcpy( inx->obsmode, XML_obsmode );

   inx->platenum = XML_platenum;
   inx->platerev = XML_platerev;
   inx->pong_angle=XML_pong_angle;
   inx->pong_gridcount=XML_pong_gridcount;
   inx->pong_spacing=XML_pong_spacing;
   inx->pong_vmax=XML_pong_vmax;
   inx->ra = XML_ra;
   inx->sample_t = XML_sample_t;
   inx->scan_angle=XML_scan_angle;
   inx->scan_pathlength=XML_scan_pathlength;
   inx->scan_vmax=XML_scan_vmax;
   inx->smu_move = XML_smu_move;
   inx->smu_offset = XML_smu_offset;
   inx->smu_samples = XML_smu_samples;
   inx->subsysnr = XML_subsysnr;
   inx->targetpow = XML_targetpow;
   strcpy ( inx->wt0_name, XML_wt0_name );
   strcpy ( inx->wt1_name, XML_wt1_name );

}


/*+ dxml_sextod - convert a sexagesimal string into a double */

double dxml_sextod
(
const char *string,        /* string (given) */
int *status                /* global status (given and returned) */
)

/* Method:
    Use strtod and trap errors.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    12May2005 : Original (bdk)
*/

{
   double a;             /* temporary value */
   double b;             /* temporary value */
   double c;             /* temporary value */
   int j;                /* loop counter */
   char *p;              /* pointer into work string */
   int sign;             /* sign of value */
   char *t;              /* pointer to string after translation */
   double val;           /* converted value */
   static char w[64];    /* work string */


   if ( !StatusOkP(status) ) return 0;

/* Copy into the work string, replacing ':' by space */

   val = 0.0;
   a = 0.0;
   b = 0.0;
   c = 0.0;
   sign = 1;

   for ( j=0; j<64; j++ )
   {
      w[j] = string[j];
      if ( w[j] == ':' ) w[j] = ' ';
      if ( w[j] == 0 ) break;
   }
   w[63] = 0;

   p = w;

   a = strtod ( p, &t ); 

   if ( t == p )
   {
      *status = DITS__APP_ERROR;
   }
   else
   {
      sign = 1;
      if ( a < 0 )
      {
         a = fabs(a);
         sign = -1;
      }
      p = t;
      b = strtod ( p, &t ); 

      if ( t == p )
      {
         b = 0.0;
      }
      else
      {
         p = t;
         c = strtod ( p, &t ); 

         if ( t == p )
         {
            c = 0.0;
         }
      }

      val = sign * ( a + b/60.0 + c/3600.0 );
   }

   return val;
}


/*+ dxml_startsimXML - callback for XML parser when reading simulator file */

void dxml_startsimXML
(
void *userData,                /* unused (given and returned)*/
const char *name,              /* name of item (given) */
const char **atts              /* array of name-value pairs (given) */
)

/* Method:
    The parser is set-up so that it calls this routine whenever it finds
    an entry in the sequence file being parsed. It passes-in the strings
    declaring the attributes of the entry. This routine interprets tham
    and stores them in global data structures. 

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)
    A.G.Gibb (agg@astro.ubc.ca)

   History :
    26Jun2003 : Original (bdk)
    26Jan2005 : add xpoint, ypoint (agg)
    04Feb2005 : add telemission (bdk)
    14Apr2005 : add airmass (agg)
    13May2005 : add subname (bdk)
*/

{
   int status;         /* status */

   status = STATUS__OK;

/* Look for simulator characteristics */

   if ( strcmp ( name, "add_atm" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_add_atm, &status );
   }
   else if ( strcmp ( name, "add_fnoise" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_add_fnoise, &status );
   }
   else if ( strcmp ( name, "add_hnoise" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_add_hnoise, &status );
   }
   else if ( strcmp ( name, "add_pns" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_add_pns, &status );
   }
   else if ( strcmp ( name, "anang" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_anang, &status );
   }
   else if ( strcmp ( name, "anpol" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_anpol, &status );
   }
   else if ( strcmp ( name, "antrans" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_antrans, &status );
   }
   else if ( strcmp ( name, "aomega" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_aomega, &status );
   }
   else if ( strcmp ( name, "astang" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_astang, &status );
   }
   else if ( strcmp ( name, "astname" ) == 0 )
   {
      strcpy ( XML_astname, atts[1] );
   }
   else if ( strcmp ( name, "astpol" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_astpol, &status );
   }
   else if ( strcmp ( name, "atmname" ) == 0 )
   {
      strcpy ( XML_atmname, atts[1] );
   }
   else if ( strcmp ( name, "atmrefnu" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_atmrefnu, &status );
   }
   else if ( strcmp ( name, "atmrefvel" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_atmrefvel, &status );
   }
   else if ( strcmp ( name, "atmxvel" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_atmxvel, &status );
   }
   else if ( strcmp ( name, "atmyvel" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_atmyvel, &status );
   }
   else if ( strcmp ( name, "atmzerox" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_atmzerox, &status );
   }
   else if ( strcmp ( name, "atmzeroy" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_atmzeroy, &status );
   }
   /* Airmass is now a derived quantity - EC
   else if ( strcmp ( name, "airmass" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_airmass, &status );
   }
   */
   else if ( strcmp ( name, "xpoint" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_xpoint, &status );
   }
   else if ( strcmp ( name, "ypoint" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_ypoint, &status );
   }
   else if ( strcmp ( name, "bandGHz" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_bandGHz, &status );
   }
   else if ( strcmp ( name, "blindang" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_blindang, &status );
   }
   else if ( strcmp ( name, "blindpol" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_blindpol, &status );
   }
   else if ( strcmp ( name, "blindtrans" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_blindtrans, &status );
   }
   else if ( strcmp ( name, "cassang" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_cassang, &status );
   }
   else if ( strcmp ( name, "casspol" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_casspol, &status );
   }
   else if ( strcmp ( name, "casstrans" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_casstrans, &status );
   }
   else if ( strcmp ( name, "flux2cur" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_flux2cur, &status );
   }
   else if ( strcmp ( name, "meanatm" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_meanatm, &status );
   }
   else if ( strcmp ( name, "nasang" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_nasang, &status );
   }
   else if ( strcmp ( name, "naspol" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_naspol, &status );
   }
   else if ( strcmp ( name, "nastrans" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_nastrans, &status );
   }
   else if ( strcmp ( name, "ncycle" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_ncycle, &status );
   }
   else if ( strcmp ( name, "smu_terr" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_smu_terr, &status );
   }
   else if ( strcmp ( name, "subname" ) == 0 )
   {
      strcpy ( XML_subname, atts[1] );
   }
   else if ( strcmp ( name, "telemission" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_telemission, &status );
   }
   else if ( strcmp ( name, "tauzen" ) == 0 )
   {
     dxml_cvtdouble ( name, atts[1], &XML_tauzen, &status );
   }
   else if ( strcmp ( name, "atstart" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_atstart, &status );
   }
   else if ( strcmp ( name, "atend" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_atend, &status );
   }
}



/*+ dxml_startXML - callback for XML parser when reading file */

void dxml_startXML
(
void *userData,                /* unused (given and returned)*/
const char *name,              /* name of item (given) */
const char **atts              /* array of name-value pairs (given) */
)

/* Method:
    The parser is set-up so that it calls this routine whenever it finds
    an entry in the sequence file being parsed. It passes-in the strings
    declaring the attributes of the entry. This routine interprets tham
    and stores them in global data structures. 

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    12Nov2002 : Original (bdk)
    13May2005 : add ra, dec (bdk)
    06Oct2005 : add flatname (bdk)
*/

{
   char errmess[132];  /* error message string */
   int gotX;           /* flag for found X offset */
   int gotY;           /* flag for found Y offset */
   int j;              /* attribute counter */
   int jignum;         /* jiggle position number */
   int X;              /* X jiggle offset */
   int Y;              /* Y jiggle offset */
   int status;         /* status */

   status = STATUS__OK;



   if ( strcmp ( name, "jig_position" ) == 0 )
   {

/* Check for the declaration of a jiggle position */

      jignum = 0;
      gotX = 0;
      gotY = 0;
      X = 0;
      Y = 0;

      for ( j=0; j<6; j=j+2 )
      {

         if ( ( atts[j] != 0 ) && ( atts[j+1] != 0 ) )
         {
            if ( strcmp ( atts[j], "pos" ) == 0 )
            {
               dxml_cvtint ( atts[j], atts[j+1], &jignum, &status );
               if ( StatusOkP(&status) && ( jignum > DREAM__MXVERT ) )
               {
                  sprintf ( errmess,
          "dxml:Error jiggle position > tableLength: name = %s  value = %s", 
                   atts[j], atts[j+1] );
                  ErsRep ( 0, &status, errmess );
               }

            }
            if ( strcmp ( atts[j], "X" ) == 0 )
            {
               gotX = 1;
               dxml_cvtint ( atts[j], atts[j+1], &X, &status );
            }
            if ( strcmp ( atts[j], "Y" ) == 0 )
            {
               gotY = 1;
               dxml_cvtint ( atts[j], atts[j+1], &Y, &status );
            }

         }
      }
      if ( ( jignum > 0 ) && ( jignum <= DREAM__MXVERT ) && gotX && gotY )
      {
         XML_jig_vert[jignum-1][0] = X;
         XML_jig_vert[jignum-1][1] = Y;
         if ( jignum > XML_nvert )
         {
             XML_nvert = jignum;                                 
         }            
      }
      else
      {
         sprintf ( errmess, "dxml:Error parsing jiggle position" );
      }

   }

/* Look for simple parameters */

   else if ( strcmp ( name, "bol_distx" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_bol_distx, &status );
   }
   else if ( strcmp ( name, "bol_disty" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_bol_disty, &status );
   }
   else if ( strcmp ( name, "bolfile" ) == 0 )
   {
      strcpy ( XML_bolfile, atts[1] );
   }
   else if ( strcmp ( name, "conv_shape" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_conv_shape, &status );
   }
   else if ( strcmp ( name, "conv_sig" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_conv_sig, &status );
   }
   else if ( strcmp ( name, "coordframe" ) == 0 )
   {
     strcpy ( XML_coordframe, atts[1] );
   }
   else if ( strcmp ( name, "dec" ) == 0 )
   {
      dxml_cvtsexdouble ( name, atts[1], &XML_dec, &status );
      XML_dec *= DD2R;
   }
   else if ( strcmp ( name, "distfac" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_distfac, &status );
   }
   else if ( strcmp ( name, "flatname" ) == 0 )
   {
      strcpy ( XML_flatname, atts[1] );
   }
   else if ( strcmp ( name, "grid_max_x" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_grid_max_x, &status );
   }
   else if ( strcmp ( name, "grid_max_y" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_grid_max_y, &status );
   }
   else if ( strcmp ( name, "grid_min_x" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_grid_min_x, &status );
   }
   else if ( strcmp ( name, "grid_min_y" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_grid_min_y, &status );
   }
   else if ( strcmp ( name, "grid_step_x" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_grid_step_x, &status );
   }
   else if ( strcmp ( name, "grid_step_y" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_grid_step_y, &status );
   }
   else if ( strcmp ( name, "heatnum" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_heatnum, &status );
   }
   else if ( strcmp ( name, "heatstart" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_heatstart, &status );
   }
   else if ( strcmp ( name, "heatstep" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_heatstep, &status );
   }
   else if ( strcmp ( name, "jig_step_x" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_jig_step_x, &status );
   }
   else if ( strcmp ( name, "jig_step_y" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_jig_step_y, &status );
   }
   else if ( strcmp ( name, "lambda" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_lambda, &status );
   }
   else if ( strcmp ( name, "nbolx" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_nbolx, &status );
   }
   else if ( strcmp ( name, "nboly" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_nboly, &status );
   }
   else if ( strcmp ( name, "ngrid" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_ngrid, &status );
   }
   else if ( strcmp ( name, "numsamples" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_numsamples, &status );
   }
   else if ( strcmp ( name, "obsmode" ) == 0 )
   {
     strcpy ( XML_obsmode, atts[1] );
   }
   else if ( strcmp ( name, "platenum" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_platenum, &status );
   }
   else if ( strcmp ( name, "platerev" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_platerev, &status );
   }
   else if ( strcmp ( name, "sample_t" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_sample_t, &status );
   }
   else if ( strcmp ( name, "smu_move" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_smu_move, &status );
   }
   else if ( strcmp ( name, "smu_offset" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_smu_offset, &status );
   }
   else if ( strcmp ( name, "smu_samples" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_smu_samples, &status );
   }
   else if ( strcmp ( name, "ra" ) == 0 )
   {
      dxml_cvtsexdouble ( name, atts[1], &XML_ra, &status );
      XML_ra *= DH2R;
   }
   else if ( strcmp ( name, "subsysnr" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_subsysnr, &status );
   }
   else if ( strcmp ( name, "targetpow" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_targetpow, &status );
   }
   else if ( strcmp ( name, "wt0_name" ) == 0 )
   {
      strcpy ( XML_wt0_name, atts[1] );
   }
   else if ( strcmp ( name, "wt1_name" ) == 0 )
   {
      strcpy ( XML_wt1_name, atts[1] );
   }   
   else if ( strcmp ( name, "mjdaystart" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_mjdaystart, &status );
   }   
   else if ( strcmp ( name, "pong_angle" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_pong_angle, &status );
   }
   else if ( strcmp ( name, "pong_gridcount" ) == 0 )
   {
      dxml_cvtint ( name, atts[1], &XML_pong_gridcount, &status );
   }
   else if ( strcmp ( name, "pong_spacing" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_pong_spacing, &status );
   }
   else if ( strcmp ( name, "pong_vmax" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_pong_vmax, &status );
   }
   else if ( strcmp ( name, "bous_angle" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_bous_angle, &status );
   }
   else if ( strcmp ( name, "bous_width" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_bous_width, &status );
   }
   else if ( strcmp ( name, "bous_height" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_bous_height, &status );
   }
   else if ( strcmp ( name, "bous_spacing" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_bous_spacing, &status );
   }
   else if ( strcmp ( name, "bous_vmax" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_bous_vmax, &status );
   }
   else if ( strcmp ( name, "scan_angle" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_scan_angle, &status );
   }
   else if ( strcmp ( name, "scan_pathlength" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_scan_pathlength, &status );
   }
   else if ( strcmp ( name, "scan_vmax" ) == 0 )
   {
      dxml_cvtdouble ( name, atts[1], &XML_scan_vmax, &status );
   }
}


/*+ dxml_strtod - convert a string into a double */

double dxml_strtod
(
const char *string,        /* string (given) */
int *status                /* global status (given and returned) */
)

/* Method:
    Use strtod and trap errors.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    30Jun2004 : Original (bdk)
*/

{
   char *t;           /* pointer to string after translation */
   double val;           /* converted value */


   if ( !StatusOkP(status) ) return 0;

   val = strtod ( string, &t ); 

   if ( t == string )
   {
      *status = DITS__APP_ERROR;
   }

   return val;
}


/*+ dxml_strtol - convert a string into an integer */

int dxml_strtol
(
const char *string,        /* string (given) */
int *status                /* global status (given and returned) */
)

/* Method:
    Use strtol and trap errors.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    13May2004 : Original (bdk)
*/

{
   char *t;           /* pointer to string after translation */
   int val;           /* converted value */


   if ( !StatusOkP(status) ) return 0;

   val = strtol ( string, &t, 0 ); 

   if ( t == string )
   {
      *status = DITS__APP_ERROR;
   }

   return val;
}
