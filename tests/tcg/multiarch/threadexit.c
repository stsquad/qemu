#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>

static int n = 32;
pthread_t tid[32];

void *fnc(void *arg)
{
    unsigned long i = 0;
    pthread_t id = pthread_self();
    int c = 0;

    for (i = 0; i < (1024 * 64); i++) {
        pthread_t rid = tid[rand() % 32];
        if (id == rid) {
            c++;
        }
    }
    return NULL;
}

int main(void)
{
    int i;
    for (i = 0; i < n; i++) {
        pthread_create(&(tid[i]), NULL, &fnc, NULL);
    }
    return 0;
}
