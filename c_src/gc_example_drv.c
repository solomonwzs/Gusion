#include "gc_example_drv.h"
#include "example.h"

zlog_category_t *logc;

static ErlDrvData example_drv_start(ErlDrvPort port, char *buff){
    zlog_init("example_log.conf");
    logc=zlog_get_category("gc_example");

    example_data* d=(example_data*)driver_alloc(sizeof(example_data));
    d->port=port; 
    return (ErlDrvData)d;
}

static void example_drv_stop(ErlDrvData handle){
    zlog_fini();
    driver_free((char *)handle);
}

static void example_drv_output(ErlDrvData handle, char *buff, ErlDrvSizeT bufflen){
    example_data* d=(example_data*)handle;
    char res[1024];
    int len=process((unsigned char*)buff, (int)bufflen, (unsigned char *)res);
    driver_output(d->port, res, len);
}

ErlDrvEntry example_driver_entry = {
    NULL,                   /* F_PTR init, N/A */
    example_drv_start,      /* L_PTR start, called when port is opened */
    example_drv_stop,       /* F_PTR stop, called when port is closed */
    example_drv_output,     /* F_PTR output, called when erlang has sent
                               data to the port */
    NULL,                   /* F_PTR ready_input,
                               called when input descriptor ready to read*/
    NULL,                   /* F_PTR ready_output,
                               called when output descriptor ready to write */
    "gusion_example_drv",   /* char *driver_name, the argument to open_port */
    NULL,                   /* F_PTR finish, called when unloaded */
    NULL,                   /* handle */
    NULL,                   /* F_PTR control, port_command callback */
    NULL,                   /* F_PTR timeout, reserved */
    NULL,                   /* F_PTR outputv, reserved */
    NULL,                   /* ready_async */
    NULL,                   /* flush */
    NULL,                   /* call */
    NULL,                   /* event */
    ERL_DRV_EXTENDED_MARKER,        /* extended_marker */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* major_version */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* minor_version */
    ERL_DRV_FLAG_USE_PORT_LOCKING,  /* driver_flags */
    NULL,                   /* handle2 */
    NULL,                   /* process_exit */
    NULL                    /* stop_select */
};

DRIVER_INIT(gusion_drv){
    return &example_driver_entry;
}
