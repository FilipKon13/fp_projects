#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kvstore.h"

#define STORE_SIZE 100

static struct {
    int key;
    char value[KV_MAXLEN];
} store[STORE_SIZE];

void * post_1_svc(kvpair *arg, struct svc_req *rqstp) {
    static char *result = NULL;
    int k = arg->key;
    const char *val = arg->value;
    fprintf(stderr, "[POST] Received key=%d, value=%s\n", k, val);

    for (int i = 0; i < STORE_SIZE; i++) {
        if (store[i].key == 0 || store[i].key == k) {
            store[i].key = k;
            strncpy(store[i].value, val, KV_MAXLEN - 1);
            store[i].value[KV_MAXLEN - 1] = '\0';
            fprintf(stderr, "[POST] key=%d, value=%s\n", k, val);
            break;
        }
    }
    return (void *)&result;  /* void return, just acknowledge */
}

char ** get_1_svc(int *key, struct svc_req *rqstp) {
    static char *result;
    int k = *key;

    fprintf(stderr, "[GET] Received key=%d\n", k);

    result = "";
    for (int i = 0; i < STORE_SIZE; i++) {
        if (store[i].key == k) {
            result = store[i].value;
            break;
        }
    }

    fprintf(stderr, "[GET] key=%d -> %s\n", k, result);
    return &result;
}
