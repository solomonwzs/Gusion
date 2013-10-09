#include "example.h"

int twice(int x){
    return x*2;
}

int sum(int x, int y){
    return x+y;
}

int process(unsigned char *buff, int bufflen, unsigned char *res){
    int len;

    erl_init(NULL, 0);
    ETERM *et=erl_decode(buff);
    len=erl_encode(et, res);
    erl_free_term(et);

    return len;
}
