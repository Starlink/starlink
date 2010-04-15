/***********************************************************************
*                                                                      *
*   file IDI.H                                                         *
*                                                                      *
*   IDI  CONSTANTS definition                                          *
*                                                                      *
*                                                                      *
************************************************************************
*   V 2.0    881026                                                    *
*   Author : P. Santin  - Trieste Astronomical Observatory             *
*   Update : Nick Eaton  921102  - Added hard cursor options           *
***********************************************************************/

# define MAX_DEV          8        /* max no. of open displays on WS  */
# define MAX_CONFIG       8        /* max no. of configurations       */
                                   /* (static + dynamic)              */
# define MAX_MEM          8        /* max no. of memories for each    */
                                   /* configuration                   */
# define MAX_LUT          8        /* max no. of luts for display     */
# define MAX_ITT          4        /* max no. of luts for display     */
# define MAX_CURS         8        /* max no. of cursors for display  */
# define MAX_ROI          8        /* max no. of roi per display      */
# define MAX_INTER       32        /* max no. of contemporary         */
                                   /* interactions for display        */

# define MAX_INT_DEV      2        /* max no. of available            */
                                   /* interactors on device           */
# define MAX_LOC          2        /* max_no. of locators for         */
                                   /* each interactor                 */
# define MAX_EVL          8        /* max_no. of evaluators for       */
                                   /* each interactor                 */
# define MAX_TRG         64        /* max_no. of triggers for         */
                                   /* each interactor                 */

# define II_NULL             0

# define II_USER             0        /* interactive operations      */
# define II_MOVE             1
# define II_ROTATE           2
# define II_ZOOM             3
# define II_UNZOOM           4
# define II_CLZOOM           5
# define II_BLINK            6
# define II_MODIFY           7
# define II_SLICE            8

# define II_CURSOR           1        /* interactive objects         */
# define II_ITT              2
# define II_LUT              3
# define II_ROI              4
# define II_MEMORY           5
# define II_DISPLAY          6
# define II_COLOR            7

# define II_MOUSE            1        /* interactive devices         */
# define II_KEYB             2

# define II_LOC              0        /* Interactor types            */
# define II_EVLR             1
# define II_EVLI             2
# define II_EVLT             3
# define II_EVLS             4
# define II_TRG              5

# define II_KEY              1        /* trigger description         */
# define II_BUTT             2

# define II_CROSSHAIR        1        /* cursor shape                */
# define II_CROSS            2

# define II_SOLID            1        /* line style                  */
# define II_DASHED           2
# define II_DOTTED           3
# define II_DASH_DOTTED      4

# define II_RECTANGLE        1        /* region of interest shape    */
# define II_CIRCLE           2


# define II_IMAGE            1        /* memory type                 */
# define II_TEXT             2
# define II_GRAPHIC          4

# define II_DEFAULT          0        /* Write Mode                  */
# define II_XOR              1
# define II_OR               2

# define II_BLACK            1        /* Graphic colors              */
# define II_WHITE            2
# define II_RED              3
# define II_GREEN            4
# define II_BLUE             5
# define II_YELLOW           6
# define II_MAGENTA          7
# define II_CYAN             8

# define HC_UNDO             0        /* Hard cursor options */
# define HC_DO               1
# define HC_WRITE            2
# define HC_READ             3

# define MOUSEMOVED          1        /* Interaction type */
# define ENTERWINDOW         2
# define BUTTONPRESSED       3
# define KEYPRESSED          4

/**********************************************************************/
