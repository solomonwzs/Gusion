#include "example.h"

extern zlog_category_t *logc;
static char log_str[1024];

#define log_info(_f_, ...) do{\
    sprintf(log_str, _f_, ## __VA_ARGS__);\
    zlog_info(logc, log_str);\
} while(0)

int twice(int x){
    return x*2;
}

int sum(int x, int y){
    return x+y;
}

int process(unsigned char *buff, int bufflen, unsigned char *res){
    int len, index;
    unsigned long allocated, freed;
    char p[1024];

    erl_init(NULL, 0);

    ETERM *et=erl_decode(buff);
    erl_eterm_statistics(&allocated, &freed);
    log_info("currently allocated blocks: %ld", allocated);
    log_info("length of freelist: %ld", freed);

    len=erl_encode(et, res);

    erl_free_term(et);
    erl_eterm_release();
    erl_eterm_statistics(&allocated, &freed);
    log_info("currently allocated blocks: %ld", allocated);
    log_info("length of freelist: %ld", freed);

    ei_decode_string((const char *)buff, &index, p);
    log_info("string: %s", p);
    log_info("index: %d", index);

    return len;
}
