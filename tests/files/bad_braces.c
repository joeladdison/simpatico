/* Contains bad brace violations of different types, including mixes of
** good and bad. Currently contains
** 20
** violations.
*/

int fb(int);

/* not a violation, functions may be either */
void good_func(void)
{
    return;
}

/* 1 violation */
void bad_func(void) { return; }

/* 5 violations */
int fa(int a) {
    if (a)
    { //violation
        a++;
    } //violation (the whole line of } else if {)
    else if (a+1)
    { //violation
        a /= 2;
    } //violation
    else//token tester
    { //violation
        a--;
    }
    return fb(a);
}

/* 3 violations */
int fb(int a) {
    if (a) 
    { //violation
        a++;
    } else if (a + 1)
    { //violation
        a--;
    } else
    { //violation
        return 0;
    }
    return a;
}

/* 3 violations */
int missing_braces(void) {
    int a = 1;
    if (a) //violation
        return 0;
    else if (a - 1) //violation
        return 1;
    else //violation
        return 2;
}

/* 5 violations */
void bad_loops(void) {
    int a;
    for (int i = 0; i < 10; i++) 
    { //violation
        a = i;
    }
    for (; ; ) { break; } //violation
    while (!a)  
    { //violation
        a++;
    }
    do
    { //violation
        a++;
    } //violation
    while (!a);
}


/* 3 violations */
int main() {
    int a = 1;
    switch (a)
    { //violation
        case 0:
            return 0;
        default:
            break;
    }
    if (fa(a)) {
        a = 1;
    } //violation
    else if (fb(a)) {
        a = 2;
    } //violation
    else {
        a = 3;
    }
}
