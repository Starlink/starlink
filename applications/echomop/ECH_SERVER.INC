       LOGICAL USR_TUNE_SERVER
       LOGICAL USR_TUNE_CLIENT

       INTEGER MAX_MESSAGE_LENGTH
       PARAMETER ( MAX_MESSAGE_LENGTH = 8000 )

       INTEGER MAX_MESSAGE_BYTES
       PARAMETER ( MAX_MESSAGE_BYTES = 32000 )

       INTEGER MESSAGE_LENGTH
       INTEGER MESSAGE_OBJECTS
       INTEGER NET_EFN

       INTEGER*2 NET_IOSB( 4 )
       INTEGER*2 NET_CHAN

       INTEGER MAX_SERVERS
       PARAMETER ( MAX_SERVERS = 100 )

       INTEGER*2 SERV_IOSBS ( 4, MAX_SERVERS )

       INTEGER*2 serv_chan ( max_servers )

       INTEGER serv_buff_addr ( max_servers )

       INTEGER serv_status ( max_servers )

       CHARACTER*255   server_path ( max_servers )

       INTEGER         message ( max_message_length )


       COMMON  / ECH_SERVER_1 /   usr_tune_server,
     :                          usr_tune_client,
     :                          message_objects,
     :                          message_length,
     :                          net_efn

       COMMON  / ECH_SERVER_2 /
     :                          net_iosb, net_chan,
     :                          serv_iosbs, serv_chan

       COMMON  / ECH_SERVER_3 /
     :                          serv_buff_addr,
     :                          serv_status


       COMMON  / ECH_SERVER_4 /   message, server_path
