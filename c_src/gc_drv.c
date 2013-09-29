#include "gc_drv.h"
#include "example.h"

static ErlDrvData example_drv_start(ErlDrvPort port, char *buff){
    example_data* d=(example_data*)driver_alloc(sizeof(example_data));
    d->port=port;
    return (ErlDrvData)d;
}

static void example_drv_stop(ErlDrvData handle){
    driver_free((char *)handle);
}

static void example_drv_output(ErlDrvData handle, char *buff, ErlDrvSizeT bufflen){
    example_data* d=(example_data*)handle;
    char fn=buff[0], arg=buff[1], res;
    if (fn==1) {
        res=twice(arg);
    } else if (fn==2) {
        res=sum(buff[1], buff[2]);
    }
    driver_output(d->port, &res, 1);
}

ErlDrvEntry example_driver_entry = {
    NULL,               /* F_PTR init, N/A */
    example_drv_start,  /* L_PTR start, called when port is opened */
    example_drv_stop,   /* F_PTR stop, called when port is closed */
    example_drv_output, /* F_PTR output, called when erlang has sent
                           data to the port */
    NULL,               /* F_PTR ready_input,
                           called when input descriptor ready to read*/
    NULL,               /* F_PTR ready_output,
                           called when output descriptor ready to write */
    "gusion_drv",       /* char *driver_name, the argument to open_port */
    NULL,               /* F_PTR finish, called when unloaded */
    NULL,               /* F_PTR control, port_command callback */
    NULL,               /* F_PTR timeout, reserved */
    NULL                /* F_PTR outputv, reserved */
};

DRIVER_INIT(gusion_drv){
    return &example_driver_entry;
}
