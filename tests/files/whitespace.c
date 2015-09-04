#include<stdio.h> //error

int one =1; //error

void f() {
}

int main(){ //error
    int sum = 0;
    int    i; //error

    for (i = 0; i <= 4; ++i){ //error
        printf("sum is now %d\n", sum);
        sum += i;
    }

    // A really long statement that has to cover lots of lines=something
    if (sum == 1|| sum == 2 ||sum == 3 || sum == 4||sum == 5 ||
            sum == 6 || sum==7 || sum == 8 || sum == 9 || sum == 11 ) { //error
        printf("something went %s wrong!\n","terribly"); //error
        printf("oh %s!\n", "noes");
    } else {
        printf( "the sum is %d!\n", sum); //error
    }
    
    if ( i ) { //error
        i++;
    }
    int x = 2,y; //error
    switch(x) {
        case 0:
            y = 0;
            break;
        case 1:
            y= 1; //error
        case 2:
            y = 2;
            break;
        case 3:
        case'4': //error
            y = 4;
            break;
        default:
            x +=0; //error
            y-= 1; //error
            break;
    }
}

void g(int a) {
    if (a) {
        a++;        b++; //error, missing newline
    }
}
/*commented func but no vertical whitespace*/
void h(int a) { //violation for no vertical whitespace
}
void j(int a,int b) { //violation for no vertical whitespace AND comma
}

void pointers() {
    int *a;
    int* b; //error for consistency only
    int * c; //error
    int * *d; //error for space in pointer types
    int*e; //error for no spaces
}

void prototype_check();
void prototype_check() { //error for no vertical whitespace
}



void big_gap() { //error for too much whitespace
}

