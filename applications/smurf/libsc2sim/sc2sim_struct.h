/*  sc2sim_struct.h - structure definitions for simulator */

#ifndef SC2SIM_STRUCT_DEFINED
#define SC2SIM_STRUCT_DEFINED

struct bolpix             /* pixel location of bolometer */
       {
            int quad;     /* array quadrant, 0-3 */
            int x;        /* X-index in quadrant 0-39 */
            int y;        /* Y-index in quadrant 0-39 */
       };

/* Enumerated type for observing modes */
typedef enum {stare, dstare, dream, pong, polspin, heatrun, none} obsMode;
#endif /* SC2SIM_STRUCT_DEFINED */
