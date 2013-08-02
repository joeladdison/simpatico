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
#define LONG_ONE "here's a nice long string that's\
        interrupted"
#define LONG_TWO "here's another without strange" \
        "whitespace in the middle"
#define MACRO(x, ...) fprintf(stderr, x, ...)

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

/* these next lines are madness */
#ifndef THING_THAT_DOES_NOT_EXIST
#define THING_THAT_DOES_NOT_EXIST
#endif

#ifdef THING_THAT_DOES_NOT_EXIST
#undef \
        THING_THAT_DOES_NOT_EXIST
#endif

/* lastly... */
#ifdef linux
    //don't do anything
#endif

struct Nested {
    Struct s;
    struct Nested *n;
} __attribute__ ((aligned)); 
/* don't touch __attribute__ unless you really know what you're doing,
 * especially if you think you'll be compiling with not GCC (hint: bad things)
 */

static struct Awkward {
    int i;
} bob; /* bob is a variable */

struct MoreAwkward {
    int i;
} jane = {.i = 0}; /* jane is also a variable, and is being initialised */

struct StillAwkward {
    int i;
} *thisIsAnArrayOfStillAwkward; /* not a fan of this at allll */

/* commented global */
int global = 1;
char globalChar = '\0'; /*also commented*/

/* commented function */
func_a(char *str, int i) {
    return linux * str[i]; //what's linux, you ask? a standard #define
}

/* a function prototype */
void proto(const int * const param);

/* fun with 'arry */
void arry(int potter[]) {
    float f = 0.3f;
}

/* the definition of that prototype */
void proto(const int * const param) {
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

/* doooooodoodooodooodoooo */
void test_do(struct Nested n) {
    int a = 5;
    int;
    struct Nested;
    do {
        a--;
    } while (a, 5, !NULL);
}

/* and another silly comment */
void test_switch(int a) {
#define BOB 1
    switch (a) {
        case -1:
        case 0:
        case BOB:
            return 0;
        case 2:
            a++;
        default:
            break;
    }
#if BOB
#else
#endif
}

/* random test bits, would be in main but the length of main would be > 50*/
void test_misc(int a, int b, int *e) {
    a = 0 ? a : b;
    ;
#ifdef INIT_NESTED
    3;
#endif
    for (int x = 0; x < 1; x++) {
        fprintf(stderr, "testerino\"\n"); //prints: testerino"
    }
#define NUMBERS (1 << 2)
    for (a = 1; a; a--, b++) {
        b = b + NUMBERS;
    }
#define MORE_NUMBERS NUMBERS
    for (a = 1; a < 5; a++) {
        b += e[MORE_NUMBERS];
    }
#undef MORE_NUMBERS
    for (int new = 7; ; a++) {
        break;
    }
    while (a) {
        fprintf(stdout, "\\%c\\" "\n", func_c(func_b(), 2)); /*prints: \s\ */
        --a;
    }
    printf("%c\n", (int) "string"[0]); //prints: s
    FOREVER { /* this is just testing defines, using this is a bad idea */
        break;
    }
    {
        ; //random miniblock, doesn't do anything
    }
}

/* heres a comment*/
int main(int argc, char **argv) {
    int a = 1;
    int b = -a, *c = &b;
    int *d = malloc(sizeof(int) * 6);
    int **e = malloc(sizeof(int *) * 6);
    struct NewTypeMidFunction {
        int yesReally;
    } awkward = {7};
    int f = sizeof e, **g = e;
    f = sizeof(e);
    f = sizeof(awkward.yesReally);
    Struct s;
    (&s)->contents = 'a';
    s.contents = 'b';
    struct Nested t = {.s = s, .n = NULL};
#define INIT_NESTED(x) {.s = s, .n = x}
    struct Nested u = INIT_NESTED(NULL);
#undef INIT_NESTED
    Struct *p = &s;
    d[0] = 1;
    /*//double comments are fun
    these next lines are pointless
    */
    sizeof e[0];
    *c += d[0];
    if (!a && b && '@') {
        b = a | b | f;
    } else if (a) {
        int wizard[] = {1, 2, 3};
        if (wizard) {
        } else if (a) {
        } else {
        }
        char robe[] = "robe";
        char at[] = {'a', 't', '@'}; //unsafe since not terminated with '/0'
        char hat[] = {"hat"};
        int arry[7] = {[2] = 3, [4] = 2}; //init only some members
    } else {
    }
    if(p->contents%2 == 0) {
        a = 'O';
    } else if(p->contents%2 != 0) {
        a = 'X';
    }
    if(p->contents == 1) {
        p->contents = NULL;
        return;
    }
    return f;
}
