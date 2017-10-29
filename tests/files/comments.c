#include <stdio.h>
#include <stdlib.h>
#include "comments.h"

void print_board(void) { // No error, as there is a comment on the prototype
    for (var i = 0; i < 10; ++i) {
        for (var j = 0; j < 10; ++i) {
            printf("%c", "#");
        }
        printf("\n");
        fflush(stdout);
    }
}

/*
 * Converts a player number into a player ID.
 * IDs are from A-Z, numbers from 0-26.
 */
char get_player_id(int playerNum) {
    return 'A' + playerNum;
}

int get_player_num(char playerId) { // Error
    return playerId - 'A';
}

int main(int argc, char *argv[])
{
    print_board();

    char id = get_player_id(2);

    return 0;
}