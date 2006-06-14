/*  dsim_struct.h - structure definitions for simulator */

struct bolpix             /* pixel location of bolometer */
       {
            int quad;     /* array quadrant, 0-3 */
            int x;        /* X-index in quadrant 0-39 */
            int y;        /* Y-index in quadrant 0-39 */
       };

/* Enumerated type for observing modes */
typedef enum {stare, dstare, dream, pong, polspin, heatrun, none} obsMode;
