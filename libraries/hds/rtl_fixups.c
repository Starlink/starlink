#if HAVE_CONFIG_H
#  include <config.h>
#endif

/* Routines which fix up deficiencies in the run-time-libraries of various  */
/* machines. */

#if HAVE_STDDEF_H
#  include <stddef.h>
#endif

#include <stdio.h>

#if !HAVE_ATEXIT
   int atexit( void ( *func ) ( void ) )
   {
/*+									    */
/* Name:								    */
/*    atexit								    */

/* Purpose:								    */
/*    Register a function to execute at program termination.		    */

/* Invocation:								    */
/*    atexit( func )							    */

/* Description:								    */
/*    This function implements the ANSI C run-time-library function of the  */
/*    same name for the GNU C compiler on SUN systems which lack this	    */
/*    facility.								    */

/* Parameters:								    */
/*    void ( *func ) ( void )						    */
/*	 Pointer to the function to be registered.			    */

/* Returned Value:							    */
/*    int atexit							    */
/*	 Zero if successful, otherwise non-zero.			    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    15-MAY-1991 (RFWS):						    */
/*       Original version.						    */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* External References:							    */
#if HAVE_ON_EXIT
      int on_exit( void ( *func ) ( ), int arg ); /* Name termination	    */
						  /* handler		    */
#else
   Error neither on_exit nor atexit are available
#endif

/*.									    */

/* Register the function, using a dummy argument value and returning the    */
/* result.								    */
      return on_exit( func, 0 );
   }

#else
/* Dummy function so the compiler has something to do */
void hds_rtl_fixup_on_exit_dummy () {}
#endif

#if !HAVE_MEMMOVE
   void *memmove( void *s, const void *ct, size_t n )
   {
/*+									    */
/* Name:								    */
/*    memmove								    */

/* Purpose:								    */
/*    Move memory contents regardless of overlap.			    */

/* Invocation:								    */
/*    memmove( s, ct, n )						    */

/* Description:								    */
/*    This function implements the ANSI C run-time-library function of the  */
/*    same name for SUN systems which lack it.				    */

/* Parameters:								    */
/*    void *s								    */
/*	 Pointer to the start of the destination memory area.		    */
/*    const void *ct							    */
/*	 Pointer to the start of the source memory area.		    */
/*    size_t n								    */
/*       Number of chars to move.					    */

/* Returned Value:							    */
/*    void *memmove							    */
/*	 returns the value of s.					    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    15-MAY-1991 (RFWS):						    */
/*       Original version.						    */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      int i;			 /* Loop counter for copying operation	    */
      ptrdiff_t offs;		 /* Offset between source and destination   */

/*.									    */

/* Determine the number of characters offset between the source and	    */
/* destination regions. If this is zero, then no copying is needed.	    */
      offs = ( (unsigned char *) s ) - ( (unsigned char *) ct );

/* If the offset is positive, then copy starting at the last byte.	    */
      if ( offs > 0 )
      {
         for ( i = n - 1; i >= 0; i-- )
	 {
            ( (unsigned char *) s )[ i ] = ( (unsigned char *) ct )[ i ];
	 }
      }

/* If the offset is negative, then copy starting at the first byte.	    */
      else if ( offs < 0 )
      {
         for ( i = 0; i < n; i++ )
	 {
            ( (unsigned char *) s )[ i ] = ( (unsigned char *) ct )[ i ];
	 }
      }

/* Exit the routine.							    */
      return s;
   }
#else
/* Dummy function so the compiler has something to do */
void hds_rtl_fixup_memmove_dummy () {}
#endif

#if !HAVE_STRERROR
   char *strerror( int errnum )
   {
/*+									    */
/* Name:								    */
/*    strerror								    */

/* Purpose:								    */
/*    Return pointer to error message.					    */

/* Invocation:								    */
/*    strerror( errnum )						    */

/* Description:								    */
/*    This function implements the ANSI C run-time-library function of the  */
/*    same name for the GNU C compiler on SUN systems which lack this	    */
/*    facility.								    */

/* Parameters:								    */
/*    int errnum							    */
/*       The error number for which an error message is required.	    */

/* Returned Value:							    */
/*    char *strerror							    */
/*	 Pointer to the error message string.				    */

/* GNU C-specific features used:					    */
/*    external char *sys_errlist					    */
/*       The vector of error message strings provided by <errno.h>.	    */
/*    external int sys_nerr						    */
/*       The number of messages provided in sys_errlist.		    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    15-MAY-1991 (RFWS):						    */
/*       Original version.						    */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* External Variables:							    */
      extern char *sys_errlist[ ]; /* Error message table		    */
      extern int sys_nerr;	 /* Length of error message table	    */

/* Local Variables:							    */
      char nomsg[ 81 ];		 /* Local error message buffer		    */

/*.									    */

/* If the error number is in range, return a pointer to the appropriate	    */
/* error message.							    */
      if ( ( errnum >= 0 ) && ( errnum < sys_nerr ) )
      {
         return sys_errlist[ errnum ];
      }

/* Otherwise, construct a message showing the error number and return a	    */
/* pointer to this message.						    */
      else
      {
         (void) sprintf( nomsg,
	                 "strerror: no translation for error number %d",
			 errnum );
         return nomsg;
      }
   }
#else
/* Dummy function so the compiler has something to do */
void hds_rtl_fixup_strerror_dummy () {}
#endif
