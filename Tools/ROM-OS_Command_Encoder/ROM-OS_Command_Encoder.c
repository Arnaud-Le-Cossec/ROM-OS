/*************************************************
Title                   : ROM-OS Command Encoder -
                          Decoder
--------------------------------------------------
Version                 : v1.1
Author                  : LE COSSEC Arnaud
Date of creation        : 21/01/22
Date of modification    : 03/01/24
--------------------------------------------------
Description :
->
--------------------------------------------------
Notes :

*************************************************/

/*************************************************
        Libraries
*************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/*************************************************
        Structures & Global Variables
*************************************************/

/*************************************************
        Prototypes
*************************************************/
uint8_t Encode(unsigned char *String);
/*************************************************
        Main Program
*************************************************/
int main()
{
    unsigned char String[40];
    printf("ROM-OS Command Encode/Decoder\n"
            "Version 1.1 - (c)2024 LE COSSEC ARNAUD\n");
    while(1){
        printf("\nEnter command to encode :");
        scanf("%s",String);
        printf("%s",String);
        uint8_t Gen = Encode(String);
        printf("\nResult : %d (decimal) %x (hex)\n",Gen, Gen);
    }

    return 0;
}/*************************************************
        Functions
**************************************************/

uint8_t Encode(unsigned char *String){

    uint8_t Output=0;
    uint8_t i=0;

    while(String[i] != '\0'){
        Output = (Output + ((String[i])&0b11011111)-65);
        Output = (Output <<1)|(Output>>7);
        i++;
    }

    return Output;
}
