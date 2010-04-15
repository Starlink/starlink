

/*   Timer queue structure */

struct timer_q {
    struct timer_q *next;     /* Next element in list */
    void (*func)(int);        /* address of associated routine */
    int timerid;              /* Timer request ID    */
    struct timeval delta_t;   /* Time interval    */
    };



/*   Macro to subtract two time values (tvp and uvp) where tvp>=uvp.
     The result is placed in tvp  */

#define timer_sub(tvp,uvp) \
   (tvp)->tv_sec = (tvp)->tv_sec - (uvp)->tv_sec; \
   (tvp)->tv_usec = (tvp)->tv_usec - (uvp)->tv_usec; \
   if ((tvp)->tv_usec < 0 ) \
   { \
      (tvp)->tv_usec = (tvp)->tv_usec + 1000000; \
      --((tvp)->tv_sec);\
   }
