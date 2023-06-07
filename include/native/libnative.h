#ifndef __LIBNATIVE_H__
#define __LIBNATIVE_H__

void *memcpy(void *dest, const void *src, size_t n);
int memcmp(const void *s1, const void *s2, size_t n);
void *memset(void *s, int c, size_t n);
char *strcpy(char *dest, const char *src);
int strcmp(const char *s1, const char *s2);
char *strcat(char *dest, const char *src);

#endif /* __LIBNATIVE_H__ */
