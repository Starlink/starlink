#if __MINGW32__
int win_get_inodes( const char *fns, ino_t *st_ino, dev_t *st_rdev );
#endif
