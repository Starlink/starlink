/* Ers.h - Drama-compatible errors */

/* This provides a standalone, but DRAMA compatible way of handling
errors to allow developing code for subsequent use in a DRAMA task */

#ifndef ERS_DEFINED
#define ERS_DEFINED

#define STATUS__OK 0

#define StatusOkP(_value_)  (*(_value_) == STATUS__OK)

void ErsRep ( int flags, int *status, const char *string );

#endif /* ERS_DEFINED */
