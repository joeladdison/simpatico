/* this file contains no violations of any kind,
 * however, some lines will cause compiler warnings and the comments would
 * normally require content
 */
#include <stdio.h>
#include <stdlib.h>
/* these next few are unused, but here for testing purposes */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <netdb.h>
#include <unistd.h>
#define SIMPATICO_IS_GREAT /* just as you would in an include guard */
#include "dummy.h"

#define FOREVER for (; ; ) /* defines like this would be a bad idea normally*/

/* the beautiful thing that it is */
extern int errno;

/* these two vars are for testing type-qualifiers */
static long int stat = 0532L;
volatile unsigned char vuc;

/* this is testing the #include "dummy" correctly checks type */
RidiculousIntType heresAnotherGlobal = 0b01010; // a GCC extension

typedef struct Struct {
    char contents;
} Struct; // not really wise

struct Nested {
    Struct s;
    struct Nested *n;
} __attribute__ ((aligned)); 
/* don't touch __attribute__ unless you really know what you're doing,
 * especially if you think you'll be compiling with not GCC (hint: bad things)
 */

/* commented global */
int global = 1;
char globalChar = '\0'; /*also commented*/

/* commented function */
func_a(char *str, int i) {
    return linux * str[i]; //what's linux, you ask? a standard #define
}

/* a function prototype */
void proto(int);

/* fun with 'arry */
void arry(int potter[]) {
    float f = 0.3f;
}

/* the definition of that prototype */
void proto(param) { // with bonus omitted type. DO NOT DO THIS, SILLY PEOPLE
}

/* a global int, declare it this way and face terrible vengeance with a bunch
 * of compile warnings */
weirdGlobal;

/* here's an array of function pointers */
int (*funcArray[3])(int, int) = {NULL, NULL, NULL};

/* commented function (returns function pointer)*/
(*func_b(void))(char *, int) {
    int a = weirdGlobal+++weirdGlobal++; /* undefined behaviour*/
    a++;
    return &func_a;
}

/* func_b, but this time missing args */
(*func_b_mk2())(char *, int) {
    int a = weirdGlobal+++weirdGlobal++; /* undefined behaviour*/
    a++;
    return &func_a;
}

/* commented function pointer (takes function pointer as an arg) */
int func_c(int (*func_ptr(char *, int)), int i)
{ // linebreaks before opening braces with functions are A-OK
    return *func_ptr("test", i);
}

/* to test line continuations */
void if_continutation(void) {
    size_t a;
    a++;
    if (1 && 2 && \
            3 && 4) {
        a <<= 1;
        a |= 0x01;
        a &= 0x10;
    } else if (5 && 7
        && 6) {
        a >>= 1;
    }
    return;
}

void test_do(struct Nested n) {
    int a = 5;
    do {
        a--;
    } while (a, 5, !NULL);
}

/* heres a comment*/
int main(int argc, char **argv) {
    int a = 1;
    int b = -a, *c = &b;
    int *d = malloc(sizeof(int) * 6);
    int **e = malloc(sizeof(int *) * 6);
    int f = sizeof e, **g = e;
    f = sizeof(e);
    ;
    Struct s;
    (&s)->contents = 'a';
    s.contents = 'b';
    struct Nested t = {.s = s, .n = NULL};
#define INIT_NESTED(x) {.s = s, .n = x}
    struct Nested u = INIT_NESTED(NULL);
#undef INIT_NESTED
    d[0] = 1;
    3;
    {
        ; //random miniblock, doesn't do anything
    }
    sizeof e[0];
    *c += d[0];
    if (!a && b) {
        b = a | b | f;
    } else if (a) {
        int wizard[] = {1, 2, 3};
        char robe[] = "robe";
        char hat[] = {"hat"};
        int arry[7] = {[2] = 3, [4] = 2}; //init only some members
    } else {
    }
    //a = 0 ? a : b;
    for (int x = 0; x < 1; x++) {
        fprintf(stderr, "testerino\"\n"); //prints: testerino"
    }
    for (a = 1; a; a--, b++) {
        b = b + -a;
    }
    for (a = 1; a < 5; a++) {
        continue;
    }
    for (int new = 7; ; a++) {
        break;
    }
    while (a) {
        fprintf(stdout, "\\%c\\" "\n", func_c(func_b(), 2)); //prints: \s\
        --a;
    }
    switch (a) {
        case 0:
            return 0;
        default:
            break;
    }
    printf("%c\n", (int) 's'); //prints: s
    FOREVER { /* this is just testing defines, using this is a bad idea */
        break;
    }
    return f;
}
