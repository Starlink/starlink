/*  AMS_STRUC.H - adam message system message structures */


struct a_loc_ack_in {
	int     this_task_t_path_num;
	int     other_task_t_path_num;
	};

struct a_loc_gsoc_start_in {
	int	this_task_t_path_num;
	int	other_task_t_trans_num;
	int	gsoc_flag;
	char 	gsoc_name[MSG_NAME_LEN];
	int	gsoc_len;
	char 	gsoc_value[MSG_VAL_LEN];
	};

/*
!	struct a_loc_gsoc_ack_in /
!
!	int	this_task_t_trans_num
!	int	other_task_t_trans_num
!	int	gsoc_flag
!       char (msg_name_len) gsoc_name
!	int	gsoc_len
!	int	gsoc_status
!       char (msg_val_len) gsoc_value
!
!	};
*/

struct a_loc_msg_in {				/* same as loc_gsoc_ack_in */
	int	this_task_t_trans_num;
	int	other_task_t_trans_num;
	int	gsoc_flag;
	char 	gsoc_name[MSG_NAME_LEN];
	int	gsoc_len;
	int	gsoc_status;
	char 	gsoc_value[MSG_VAL_LEN];
	};

struct a_loc_deinit_in {
	int	this_task_t_path_num;
	};

struct a_loc_init_in {
	char 	other_taskname[MESSYS__TNAME];
	char 	this_taskname[MESSYS__TNAME];
	int	other_task_t_path_num;
	};

struct a_rem_ack_in {
	int	local_task_t_path_num;
	int	local_nettask_n_path_num;
	};

struct a_rem_gsoc_start_in {
	int	local_task_t_path_num;
	int	local_nettask_n_trans_num;
	int	gsoc_flag;
	char	gsoc_name[MSG_NAME_LEN];
	int	gsoc_len;
	char	gsoc_value[MSG_VAL_LEN];
	};

/*
!struct a_rem_gsoc_ack_in {
!	int	local_task_t_trans_num
!	int	local_nettask_n_trans_num
!	int	gsoc_flag
!	char (msg_name_len) gsoc_name
!	int	gsoc_len
!	int	gsoc_status
!	char (msg_val_len) gsoc_value
!	};
*/

struct a_rem_msg_in {				/* same as rem_gsoc_ack_in */
	int	local_task_t_trans_num;
	int	local_nettask_n_trans_num;
	int	gsoc_flag;
	char	gsoc_name[MSG_NAME_LEN];
	int	gsoc_len;
	int	gsoc_status;
	char	gsoc_value[MSG_VAL_LEN];
	};

struct a_rem_deinit_in {
	int	local_task_t_path_num;
	};

struct a_rem_init_in {
	char	remote_taskname[MESSYS__TNAME];
	char	local_taskname[MESSYS__TNAME];
	char	remote_machine_name[MESSYS__MNAME];
	int	local_nettask_n_path_num;
	};

struct a_rem_accept_in {
	int	accept_status;
	};




/* ****************************************************
 *   structure for an incoming message
 * **************************************************** */

struct a_mess_in {
	int	mess_in_type;		/* type of message - defines which
					 * map is relevant for the message */
 	union {
		struct a_loc_ack_in 		loc_ack_in;
		struct a_loc_gsoc_start_in 	loc_gsoc_start_in;
		struct a_loc_msg_in 		loc_msg_in;
		struct a_loc_deinit_in 		loc_deinit_in;
		struct a_loc_init_in 		loc_init_in;
		struct a_rem_ack_in 		rem_ack_in;
		struct a_rem_gsoc_start_in 	rem_gsoc_start_in;
		struct a_rem_msg_in 		rem_msg_in;
		struct a_rem_deinit_in 		rem_deinit_in;
		struct a_rem_init_in 		rem_init_in;
		struct a_rem_accept_in 		rem_accept_in;
		} u;
	};


/* *********************************************
 *   types of outgoing message
 * ********************************************* */

struct a_loc_ack_out {

	int	other_task_t_path_num;
	int	this_task_t_path_num;
	};

struct a_loc_gsoc_start_out {

	int	other_task_t_path_num;
	int	this_task_t_trans_num;
	int	gsoc_flag;
	char	gsoc_name[MSG_NAME_LEN];
	int	gsoc_len;
	char	gsoc_value[MSG_VAL_LEN];

	};

/*
!struct a_loc_gsoc_ack_out {
!
!	int	other_task_t_trans_num
!	int	this_task_t_trans_num
!	int	gsoc_flag
!	char (msg_name_len) gsoc_name
!	int	gsoc_len
!	int	gsoc_status
!	char (msg_val_len) gsoc_value
!
!	};
*/

struct a_loc_msg_out {   			/* same as loc_gsoc_ack_out */

	int	other_task_t_trans_num;
	int	this_task_t_trans_num;
	int	gsoc_flag;
	char	gsoc_name[MSG_NAME_LEN];
	int	gsoc_len;
	int	gsoc_status;
	char	gsoc_value[MSG_VAL_LEN];
	};

struct a_loc_deinit_out {
	int	other_task_t_path_num;
	};

struct a_loc_init_out {
	char	this_taskname[MESSYS__TNAME];
	char	other_taskname[MESSYS__TNAME];
	int	this_task_t_path_num;
	};

struct a_rem_ack_out {
	int	local_nettask_n_path_num;
	int	local_task_t_path_num;
	};

struct a_rem_gsoc_start_out {
	int	local_nettask_n_path_num;
	int	local_task_t_trans_num;
	int	gsoc_flag;
	char	gsoc_name[MSG_NAME_LEN];
	int	gsoc_len;
	char	gsoc_value[MSG_VAL_LEN];
	};

/*
!struct a_rem_gsoc_ack_out {
!	int	local_nettask_n_trans_num
!	int	local_task_t_trans_num
!	int	gsoc_flag
!	char (msg_name_len) gsoc_name
!	int	gsoc_len
!	int	gsoc_status
!	char (msg_val_len) gsoc_value
!	};
*/

struct a_rem_msg_out {				/* same as rem_gsoc_ack_out */
	int	local_nettask_n_trans_num;
	int	local_task_t_trans_num;
	int	gsoc_flag;
	char	gsoc_name[MSG_NAME_LEN];
	int	gsoc_len;
	int	gsoc_status;
	char	gsoc_value[MSG_VAL_LEN];
	};

struct a_rem_deinit_out {
	int	local_nettask_n_path_num;
	};

struct a_rem_init_out {
	char	local_taskname[MESSYS__TNAME];
	char	remote_taskname[MESSYS__TNAME];
	char	remote_machine_name[MESSYS__MNAME];
	int	local_task_t_path_num;
	};

struct a_rem_call_out {
	char	remote_machine_name[MESSYS__MNAME];
	};





/* ************************************************
 *   structure for an outgoing message
 * ************************************************ */

struct a_mess_out {

	int	mess_out_type;		/* type of message - says which map is
					   relevant for the message */
         union
		{
			struct a_loc_ack_out 		loc_ack_out;
			struct a_loc_gsoc_start_out 	loc_gsoc_start_out;
			struct a_loc_msg_out 		loc_msg_out;
			struct a_loc_deinit_out 	loc_deinit_out;
			struct a_loc_init_out 		loc_init_out;
			struct a_rem_ack_out 		rem_ack_out;
			struct a_rem_gsoc_start_out 	rem_gsoc_start_out;
			struct a_rem_msg_out 		rem_msg_out;
			struct a_rem_deinit_out 	rem_deinit_out;
			struct a_rem_init_out 		rem_init_out;
			struct a_rem_call_out 		rem_call_out;
		} u;
	};
