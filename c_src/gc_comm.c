#include "gc_comm.h"

int read_exact(unsigned char *buf, int len, int k){
    int i, got=0;

    do{
        if ((i=read(0, buf+got, len-got))<=0){
            return i;
        }
        got+=i;
    } while(got<len);

    return len;
}
