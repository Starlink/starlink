#include "star/thr.h"

/* Transitional latitude (in deg.s) between polar and equatorial regions
   of HPX projection. */
#define SMF__HPX_TRANS 41.8103

/* JCMT instruments */

typedef enum {
  SMF__INST_NONE,
  SMF__INST_SCUBA_2_450,
  SMF__INST_SCUBA_2_850,
  SMF__INST_ACSIS,
  SMF__INST_DAS
} smf_inst_t;


/* The four HPX-related projections available for mosaics of JSA
   tiles. The projection for a mosaic is chosen in order to avoid any
   discontinuities being present within the mosaic. Individual JSA tiles
   always use SMF__JSA_HPX. */
typedef enum {
  SMF__JSA_NULL,          /* Not an JSA projection */
  SMF__JSA_HPX,           /* HPX projection centred on RA=0, Dec=0 */
  SMF__JSA_HPX12,         /* HPX projection centred on RA=12h, Dec=0 */
  SMF__JSA_XPHN,          /* XPH projection centred on RA=0, Dec=90 */
  SMF__JSA_XPHS           /* XPH projection centred on RA=0, Dec=-90 */
} smf_jsaproj_t;


/* Struct to store information about the JSA tiling scheme for a given
   JCMT instrument. */

typedef struct smfJSATiling {
  smf_inst_t instrument;  /* Identifier for instrument */
  const char *name;       /* Instrument name */
  const char *subdir;     /* Sub-directory for instrument tiles */
  const char *type;       /* Data type for tile values */
  int var;                /* Should tiles have variance arrays? */
  int ntpf;               /* No. of tiles along one edge of an HEALPix facet */
  int ppt;                /* No. of pixels along one edge of a tile */
  int ntiles;             /* Number of tiles over whole sky */
  float fov;              /* Diameter of field of view (arcsec) */
} smfJSATiling;



/* Prototypes */
void         smf_jsainstrument( const char *param, AstFitsChan *fc,
                                smf_inst_t def, smfJSATiling *tiling,
                                int *status );
void         smf_jsadicer( int indf, const char *base, int trim,
                           smf_inst_t instrument, smf_jsaproj_t proj,
                           int *ntile, Grp *grp, int *status );
void         smf_jsatile( int itile, smfJSATiling *jsatiling, int local_origin,
                          smf_jsaproj_t proj, AstFitsChan **fc,
                          AstFrameSet **fs, AstRegion **region, dim_t lbnd[2],
                          dim_t ubnd[2], int *status );
AstFitsChan *smf_jsatileheader( int itile, smfJSATiling *skytiling,
                                int local_origin, smf_jsaproj_t proj,
                                int *move, int *status );
void         smf_jsatilei2xy( int itile, smfJSATiling *jsatiling, int *xt,
                              int *yt, int *fi, int *status );
int *        smf_jsatiles_region( AstRegion *region, smfJSATiling *tiling,
                                  int *ntile, int *status );
int *        smf_jsatiles_data( ThrWorkForce *wf, Grp *igrp, dim_t size,
                                smfJSATiling *tiling, int *ntile, int *status );
int          smf_jsatilexy2i( int xt, int yt, smfJSATiling *jsatiling,
                              int *status );
void         smf_jsatilexyconv( smfJSATiling *skytiling, smf_jsaproj_t proj,
                                int xt_hpx, int yt_hpx, int raw, int *xt_out,
                                int *yt_out, int *status );
void         smf_jsatiling( smf_inst_t instrument, smfJSATiling *jsatiling,
                            int *status );
smf_jsaproj_t smf_jsaproj( int ntile, const int *tiles,
                           smfJSATiling *skytiling, int *status );
smf_jsaproj_t smf_jsaproj_fromstr( const char *str, int rep, int *status );
                           const char * smf_jsaproj_tostr( smf_jsaproj_t proj );
