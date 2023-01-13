#ifndef NBS_H
#define NBS_H
int nbc_tune(char*, int, int*, int*);
int nbc_tune_noticeboard(int, char*, int, int*, int*);
int nbc_begin_definition(item_id*, int*);
int nbc_define_structure(int, char*, char*, item_id*, int*);
int nbc_define_primitive(int, char*, char*, int, int, item_id*, int*);
int nbc_define_shape(int, int, int[], int*);
int nbc_end_definition(char*, char*, int*);
int nbc_restore_definition(char*, char*, int*);
int nbc_restore_noticeboard(char*, char*, int*);
int nbc_save_noticeboard(int, int*);
int nbc_find_noticeboard(char*, item_id*, int*);
int nbc_find_item(int, char*, item_id*, int*);
int nbc_find_nth_item(int, int, item_id*, int*);
int nbc_lose_noticeboard(int, char*, int*);
int nbc_lose_item(int, char*, int*);
int nbc_put_value(int, int, int, char[], int*);
int nbc_put_cvalue(int, int, char*, int*);
int nbc_put_shape(int, int, int[], int*);
int nbc_inc_modified(int, int*);
int nbc_put_size(int, int, int*);
int nbc_put_trigger(int, int(*)(), int*);
int nbc_get_cvalue(int, int, char*, int*, int*);
int nbc_get_value(int, int, int, char[], int*, int*);
int nbc_get_shape(int, int*, int[], int*, int*);
int nbc_get_modified(int, int*, int*);
int nbc_get_modified_pointer(int, data_id*, int*);
int nbc_get_updated(int, int*, int*);
int nbc_get_pointer(int, data_id*, int*);
int nbc_get_name(int, char*, int*);
int nbc_get_type(int, char*, int*);
int nbc_get_size(int, int*, int*, int*);
int nbc_get_primitive(int, int*, int*);
int nbc_get_parent(int, item_id*, int*);
int nbc_get_children(int, int*, int*);
int nbc_get_info(int, char*, int*, int*);
int nbc_get_cinfo(int, char*, char*, int*);
#endif
