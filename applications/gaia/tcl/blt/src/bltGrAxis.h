/*
 * -------------------------------------------------------------------
 *
 * Interval --
 *
 *	Designates a range of values by a minimum and maximum limit.
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    double min, max, range;
} Limits;

#define SetLimits(lim, lo, hi) \
	((lim).min = (lo), (lim).max = (hi), (lim).range = ((hi) - (lo)))
#define SetRange(lim) \
	((lim).range = ((lim).max > (lim).min) ? ((lim).max - (lim).min) : 0.0)

/*
 * -------------------------------------------------------------------
 *
 * AxisSite --
 *
 *	Enumerated type representing the different sites for axes
 *
 * -------------------------------------------------------------------
 */

typedef enum {
    AXIS_BOTTOM,		/* Axis in bottom margin */
    AXIS_LEFT,			/* Axis in left margin */
    AXIS_TOP,			/* Axis in top margin */
    AXIS_RIGHT,			/* Axis in right margin */
    AXIS_VIRTUAL		/* Axis isn't drawn */
} AxisSite;

/*
 * ----------------------------------------------------------------------
 *
 * TickLabel --
 *
 * 	Structure containing the X-Y screen coordinates of the tick
 * 	label (anchored at its center).
 *
 * ---------------------------------------------------------------------- 
 */
typedef struct TickLabel {
    short int x, y;
} TickLabel;

/*
 * ----------------------------------------------------------------------
 *
 * TickPositions --
 *
 * 	Structure containing information where the ticks (major or
 *	minor) will be displayed on the graph.
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    int numTicks;		/* # of ticks on axis */
    double tickArr[1];		/* Array of tick values (malloc-ed). */
} TickPositions;

/*
 * ----------------------------------------------------------------------
 *
 * TickSweep --
 *
 * 	Structure containing information where the ticks (major or
 *	minor) will be displayed on the graph.
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    double initial;		/* Initial value */
    double step;		/* Size of interval */
    int numSteps;		/* Number of intervals. */
} TickSweep;

/*
 * ----------------------------------------------------------------------
 *
 * VirtualAxis --
 *
 * 	Structure contains options controlling how the axis will be
 * 	displayed.
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    char *name;			/* Axis identifier */
    int deletePending;		/* Indicates that the axis was scheduled for
				 * deletion. The actual deletion may be deferred
				 * until the axis is no longer in use.  */
    int refCount;		/* Number of elements referencing this axis. */
    Tcl_HashEntry *hashPtr;	/* Points to axis entry in hash table. Used
				 * to quickly remove axis entries. */
    int logScale;

    unsigned int flags;		/* Set bit field definitions below */
    /*
     * AXIS_AUTO_MIN		Set axis minimum based upon data points
     * AXIS_AUTO_MAX		Set axis maximum based upon data points
     * AXIS_MAPS_ELEM		Axis is used to map element coordinates
     * AXIS_DRAWN		Axis is designated as a logical axis
     * AXIS_CONFIG_DIRTY
     * AXIS_TYPE_X		Axis is used as a X-axis
     * AXIS_TYPE_Y		Axis is used as a Y-axis
     */
    int hidden;			/* If non-zero, don't display the axis. */
    int showTicks;
    int loose;
    int descending;

    char *titleText;		/* Title of axis */
    int titleX, titleY;

    int lineWidth;		/* Width of lines representing axis and ticks.
				 * Zero indicates than now axis or ticks are to
				 * be drawn. */

    char **limitFormats;	/* One or two strings of sprintf-like formats
				 * describing how to display virtual axis
				 * limits. If NULL, display no limits. */
    int numFormats;

    double theta;		/* # of degrees to rotate tick labels. */

    double autoRange;		/* Automatically set max limit from range */

    double shiftBy;		/* Shift maximum by this interval */

    int tickLength;		/* Specifies the length of major ticks
				 * in pixels */

    TextAttributes tickAttr;	/* Text attributes (color, font, rotation, etc.) 
				 * of labels drawn at each major tick mark */

    TextAttributes limitAttr;	/* Text attributes (color, font, rotation, etc.)
				 * of limit labels */

    TextAttributes titleAttr;	/* Text attributes (color, font, rotation, etc.)
				 * of axis title */

    char *formatCmd;		/* Specifies a Tcl proc to be call whenever to
				 * axis needs to formats its tick labels See the
				 * manual for its usage. */

    TickPositions *reqMajorPtr;	/* Requested major tick positions */

    TickPositions *reqMinorPtr;	/* Requested minor tick positions */

    int reqNumMinorTicks;	/* Requested number of minor ticks. */

    TickSweep minorTicks, majorTicks;


    Limits dataLimits;		/* Range of data values displayed on the axis.
				 * These values are either specified by the user
				 * or (if auto-scaled) determined by querying
				 * all elements using this axis. */
    Limits tickLimits;		/* Smallest and largest possible major ticks on
				 * the plot */
    Limits *limitsPtr;		

    double prevMin, prevMax;

    double range;		/* Range of axis */

    double reqStep;		/* Requested interval between ticks.  If
				 * negative or zero, a "best" step size is
				 * automatically computed based on the range of
				 * data values mapped to the axis. The default
				 * value is 0.0 */
    AxisSite site;

    GC tickGC;			/* Graphics context for axis and tick labels */

} VirtualAxis;

#define AXIS_AUTO_MIN	(1<<0)	/* Set axis minimum based upon data points */
#define AXIS_AUTO_MAX	(1<<1)	/* Set axis maximum based upon data points */

#define AXIS_HORIZONTAL	(1<<2)
#define AXIS_VERTICAL    0
#define AXIS_MAPS_ELEM	(1<<3)	/* Axis is used to map element coordinates */
#define AXIS_DRAWN	(1<<4)	/* Axis is designated as a logical axis */

#define AXIS_CONFIG_DIRTY (1<<5)

#define AXIS_TYPE_X	(1<<10)
#define AXIS_TYPE_Y	(1<<11)
#define AXIS_TYPE_MASK  (AXIS_TYPE_X | AXIS_TYPE_Y)
#define AXIS_ALLOW_NULL (1<<12)

/*
 * ----------------------------------------------------------------------
 *
 * Axis --
 *
 * 	Structure contains options controlling how the axis will be
 * 	displayed.
 *
 * ----------------------------------------------------------------------
 */
typedef struct {
    VirtualAxis *virtAxisPtr;	/* Pointer to the virtual axis corresponding
				 * to this site */

    /* The following fields are specific to logical axes */

    int width, height;		/* Extents of axis */

    int posArr[4];		/* Screen location of axis, major tick,
				 * minor tick, and tick label */

    TickPositions *genMajorPtr;	/* Generated major tick positions */
    TickPositions *genMinorPtr;	/* Generated minor tick positions */
    TickPositions *majorPtr;	/* major tick positions */
    TickPositions *minorPtr;	/* minor tick positions */

    int numSegments;		/* Size of the above segment array */
    XSegment *segArr;		/* Array of computed tick line segments.  Also
				 * includes the axis line */
    Blt_List  labelList;
} Axis;

#define AXIS_AUTO_MIN	(1<<0)	/* Set axis minimum based upon data points */
#define AXIS_AUTO_MAX	(1<<1)	/* Set axis maximum based upon data points */

#define AXIS_HORIZONTAL	(1<<2)
#define AXIS_VERTICAL    0
#define AXIS_MAPS_ELEM	(1<<3)	/* Axis is used to map element coordinates */
#define AXIS_DRAWN	(1<<4)	/* Axis is designated as a logical axis */


#define AXIS_CONFIG_DIRTY (1<<5)

#define AXIS_TYPE_X	(1<<10)
#define AXIS_TYPE_Y	(1<<11)
#define AXIS_TYPE_MASK  (AXIS_TYPE_X | AXIS_TYPE_Y)
#define AXIS_ALLOW_NULL (1<<12)

/*
 * -------------------------------------------------------------------
 *
 * Axis2D --
 *
 *	The pair of axes mapping a point onto the graph.
 *
 * -------------------------------------------------------------------
 */
typedef struct {
    VirtualAxis *x, *y;
} Axis2D;
