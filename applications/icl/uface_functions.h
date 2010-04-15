/*****************************************************************************
 *
 *    U F A C E _ F U N C T I O N S . H
 *
 * External definitions from uface module (uface.c)
 *
 * History:
 *   10-DEC-1993 (AJC):
 *      Original
 *
 ******************************************************************************
 */
extern
void uface_askparam(int path, char *svalue, int messid, int *status);
extern
void uface_startobey(char *taskname, char *actionname, char valu[], int vallen,
                     int *path, int *messid, int *status);
extern
void uface_endobey(int path, int messid, char valu[], int *status);
extern
void uface_obeyw(char *taskname, char *actionname, char valu[], int vallen,
                 int *status);
void uface_getcom(char *line, int *status);
extern
void uface_inform(int path, char *svalue, int svlen, int *status);
extern
void uface_askparam(int path, char *message_value, int messid, int *status);
extern
void uface_msginfo(int path, int context, char *mname, char *mvalue,
                   int mvlen, int messid, int messtatus, int *status);
extern
void uface_splitval(char *valu, char paramt[], int *parlen,
                    char prstr[], int *prlen, char dfault[], int *deflen,
                    char hlptxt[], int *hlplen, char hlpkey[], int *hkylen,
                    char errmess[], int *errlen, int *status);
