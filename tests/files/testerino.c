/* this file contains no quantitative violations of any kind,
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
#include <stdarg.h>

#define SIMPATICO_IS_GREAT /* just as you would in an include guard */
#include "dummy.h"

#define FOREVER for (; ; ) /* defines like this would be a bad idea normally*/
#define LONG_ONE "here's a nice long string that's\
        interrupted"
#define LONG_TWO "here's another without strange" \
        "whitespace in the middle"
#define MACRO(x, ...) fprintf(stderr, x, ...)

extern int POINTLESS_INT;
/*the above didn't need comments or to pass naming since extern*/

/* these two vars are for testing type-qualifiers */
static long int stat = 0532L;
volatile unsigned char vuc; /* not a good name since it's entirely the type */

/* this is testing the #include "dummy" correctly checks type */
RidiculousIntType heresAnotherGlobal = 0b01010; // a GCC extension

/* testing that pointer binding works correctly */
/*char (*pointerToArrays)[2];*/

typedef struct SimpleStructType {
    char contents;
} SimpleStructType; // not really wise

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

//test a function pointer type with variadic args
typedef int (*FuncPtrWithVarArgs)(void *this, const char *format, ...); 

struct Nested {
    SimpleStructType s;
    struct Nested *n;
} __attribute__ ((aligned)); 
/* don't touch __attribute__ unless you really know what you're doing,
 * especially if you think you'll be compiling with not GCC (hint: bad things)
 */

typedef struct AnonymousEnumContainer {
    enum {SOME_NAME, ANOTHER_NAME} member;
    int i;
    char c;
}; // not actually specifying the type is dumb, but still compiles

struct AnonymousStructContainer {
    struct {
        int i;
    } member;
} testVariable, *pointer;

enum EnumTest { // this is legit
    BOB, 
    JANE,
    MARY
};

//just a simple array of enums because it broke at one point
enum EnumTest enums[4];

struct NamedEnumContainer {
    enum EnumTest member;
    int i;
    char c;
} *variable;

typedef enum TypedEnumTest {
    COW,
    CAT,
    NOPE
} TypedEnumTest;

typedef enum {
    OK = 0,
    USAGE = 1
} ExplicitEnumTest;

enum MoreEnum {
    YEP, THIS, IS, OKTOO
};
static struct Awkward {
    int i;
} bob; /* bob is a variable */

struct MoreAwkward {
    int i;
} jane = {.i = 0}; /* jane is also a variable, and is being initialised */

struct StillAwkward {
    int i;
} *thisIsAnArrayOfStillAwkward, absolutely; /* not a fan of this at allll */

void proto(const int * const param);

/* commented global */
int global = 1;
char anotherGlobal = '\0'; /*also commented*/

/* commented function */
*func_a(char *str, int i) {
    long val = (linux * str[i]); //what's linux, you ask? a standard #define
    return (int *) val;
}


/* fun with 'arry */
void arry(int potter[]) {
    float f = 0.3f;
    potter[0] = (int) f;
    char *inlined[] = {"yep", "this", "is".
            "right"};
    char *block[] = {
        "this",
        "is",
        "also"
    };
}

/* the definition of that prototype */
void proto(const int * const param) {
}

/* a global int, declare it this way and face terrible vengeance with a bunch
 * of compile warnings */
weirdGlobal;

/* here's an array of function pointers */
int (*functions[3])(int, int) = {NULL, NULL, NULL};

/* commented function (returns function pointer)*/
int *(*func_b(void))(char *, int) {
    int a = weirdGlobal++ + weirdGlobal++; /* undefined behaviour*/
    a++;
    return &func_a;
}

/* func_b, but this time missing args */
*(*func_b_mk2())(char *, int) {
    int a = weirdGlobal + ++weirdGlobal++; /* undefined behaviour*/
    a++;
    return &func_a;
}

/* commented function (takes function pointer as an arg) */
/* the pointer is actually: int * (*) (char *, int) */
int func_c(int (*dummyFunc(char *, int)), int i)
{ // linebreaks before opening braces with functions are A-OK
    return *(dummyFunc("test", i));
}

/* alternate form of function pointer as an arg */
/* the pointer is actually: int (*) (char *, int) */
int func_d(int (*dummyFunc)(char *, int), int i)
{
    return (*dummyFunc)("test", i);
}


/* test breaks in for setup */
void for_continuations(void) {
    int someReallyLongInt = 1;
    for (int i = 0; i < someReallyLongInt;
	    i++) {
	for (int letsMakeThisLongToo = 0;
		letsMakeThisLongToo < someReallyLongInt;
		letsMakeThisLongToo++) {
	    printf("well that's slightly awful\n");
	}
    }

}

/* to test line continuations */
void if_continutation(void) {
    size_t a = 0;
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
    int; /* useless type name in empty declaration */
    struct Nested;
    do {
        a--;
    } while (a, 5, !NULL);
}

/* make sure we can cope with variable args */
void test_args(const char *s, ...) {
    struct Nested *head = NULL;
    for (struct Nested *c = head; c != NULL; c = c->n) {
    }
    return;
}

/* and another silly comment */
void test_switch(int a) {
#define BOB 1
    switch (a) {
        default:
            break;
        case '!':
        case -1:
        case 17 ... 20: /* non-standard range expression */
        case BOB:
            return;
        case 'x':
            a++;
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
    printf("%d", -0);
    printf("%d", ~b);
#endif
    for (int x = 0; x < 1; x++) {
        fprintf(stderr, "testerino\"\n"); //prints: testerino"
    }
#define NUMBERS (1 << 2)
    if NUMBERS { /* this is horrible, don't do it */
    }

    for (a = 1; a; a--, b++) {
        b = b + NUMBERS;
    }
    
#define MORE_NUMBERS NUMBERS
    for (a = 1; a < 5; a++) {
        b += e[MORE_NUMBERS];
    }
#undef MORE_NUMBERS
    for (int New = 7; New < 5e10; a++) {
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

/*another func due to the length of main*/
int struct_test(int f, SimpleStructType *p) {
    char a = f % 256;
    if(p->contents % 2 == 0) {
        a = 'O';
    } else if(p->contents % 2 != 0) {
        a = 'X';
    }
    if(p->contents == 1) {
        p->contents = '\0';
        return 1;
    }
    return f;
}

/* check partial struct inits */
void partial() {
    SimpleStructType s;
    struct Nested t = {.s = s, .n = NULL};
#define INIT_NESTED(x) {.s = s, .n = (x)}
    struct Nested u = INIT_NESTED(NULL);
#undef INIT_NESTED
    struct Nested v = {
        .s = s,
        .n = NULL
    };
}

/* test that we don't treat pointers to arrays as func pointers */
void pointer_to_array(int (*p)[7]) {
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
    SimpleStructType s;
    (&s)->contents = 'a';
    s.contents = 'b';
    SimpleStructType *p = &s;
    d[0] = f;
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
            printf("wizard: %d\n", wizard[0]);
        }
        char robe[] = "robe";
        char at[] = {'a', 't', '@'}; //unsafe since not terminated with '/0'
        char hat[] = {"hat"};
        int arry[7] = {[2] = 3, [4] = 2}; //init only some members
        hat[0] = at[0] = robe[0] = arry[0] = '\0';
        d[0] += hat[0] + at[0] + robe[0] + arry[0];
        
    } else {
    }
    return struct_test(f, p);
}

