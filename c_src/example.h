#ifndef _EXAMPLE_H
#define _EXAMPLE_H

#include <erl_interface.h>
#include <ei.h>
#include <string.h>
#include "gc_example_drv.h"

int twice(int x);
int sum(int x, int y);
int process(unsigned char *buff, int bufflen, unsigned char *res);

#endif
