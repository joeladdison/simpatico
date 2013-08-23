#include <stdbool.h>
#define NUMBER_ONE 1
#define two 2 //violation
#define Three 3 //violation
typedef struct {
    int x;
    int Y; //violation
} Point;
#include <unistd.h>
typedef struct {
} lowercase; //violation

int main() {
    int four = 4;
    char letter;
    bool trueOrFalse;

    int Array[5]; //violation
    int Five, Six = 6, *Seven; //violation
    bool AlsoInvalid = false; //violation
    pid_t** UPPERCASE; //violation
    char *String; //violation
    unsigned long  BigNumber=1<<42; //violation

    void* (*Function)(void*); //violation

    Point point1;
    Point Point2; //violation
}

// int NotAVar;
/*
int NotAVar;
*/
