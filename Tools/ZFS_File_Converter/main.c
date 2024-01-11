/*************************************************
Title                   : ROM-OS ZFS File converter
--------------------------------------------------
Version                 : v1.1
Author                  : LE COSSEC Arnaud
Date of creation        : 09/07/22
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
#include <string.h>

/*************************************************
        Structures & Global Variables
*************************************************/

/*************************************************
        Prototypes
*************************************************/
unsigned long SeekFileStart(FILE *TargetFile);
unsigned long SeekFileEnd(FILE *TargetFile);
int Conversion(FILE *SourceFile, FILE *OutputFile);

/*************************************************
        Main Program
*************************************************/
int main(int argc, char* argv[])
{
    if((argc < 2) || (argc > 3) ){
        printf("Wrong number of argument.\n"
               "Please give me the name of the file you want to convert\n");
        return 1;
    }
    //init

    printf("ROM-OS ZFS File Converter\n"
            "Version 1.2 - (c)2024 LE COSSEC ARNAUD\n");



    char* SourceFileName = argv[1];
    //printf("arg length %d", strlen(argv[1]));

    char* OutputFileName;
    if(argc == 3){
        OutputFileName = argv[2];
    }else{

        // search for the extension
        if(strchr(argv[1], '.') == NULL){
            OutputFileName = (char*)malloc(strlen(argv[1])+4+1);
            strcpy(OutputFileName, argv[1]);
            strcpy(OutputFileName+strlen(OutputFileName), ".zfs");
        }else{
            OutputFileName = (char*)malloc(strlen(argv[1])+1);
            strcpy(OutputFileName,argv[1]);
            strcpy(strchr(OutputFileName, '.'), ".zfs");
        }
    }
    printf("Source file : %s\n", SourceFileName);
    printf("Output file : %s\n", OutputFileName);

    FILE *OutputFile;
    FILE *SourceFile;

    //Open Source file
    SourceFile = fopen(SourceFileName,"rb");
    if ( SourceFile == NULL ){
        printf ( "Could not open file %s\n",SourceFileName);
        goto end;
    }
    //Open Output file
    OutputFile = fopen(OutputFileName, "wb+");
    if ( OutputFile == NULL ){
        printf ( "Could not open or create file %s\n",OutputFileName);
        goto end;
    }

    //Conversion
    int error;
    error = Conversion(SourceFile, OutputFile);
    if(error){
        printf("Conversion failed !\n");
        goto end;
    }   // Error detection

    //End
    end :
    printf("Closing files...\n");
    fclose(SourceFile);
    fclose(OutputFile);
    free(OutputFileName);
    return 0;
}

/*************************************************
        Functions
**************************************************/
unsigned long SeekFileStart(FILE *TargetFile){
    fseek(TargetFile, 0, SEEK_SET);
    unsigned char c=0;
    while(c==0x00){
        c = fgetc ( TargetFile ) ; // read the file
        if ( c == EOF ){
            fcloseall();
            printf("File Start Not Found.\n");
            return -1;
        }
    }
    fseek(TargetFile,-1,SEEK_CUR);
    return ftell(TargetFile);
}

unsigned long SeekFileEnd(FILE *TargetFile){
    fseek(TargetFile, 1, SEEK_END);
    unsigned char c=0;
    while(c==0x00){
        fseek(TargetFile,-2,SEEK_CUR);
        c = fgetc ( TargetFile ) ; // read the file
        if ( c == EOF ){
            fcloseall();
            printf("File End Not Found.\n");
            return -1;
        }
    }
    fseek(TargetFile,2,SEEK_CUR);
    return ftell(TargetFile);
}

int Conversion(FILE *SourceFile, FILE *OutputFile){
    //Search for file Start
    printf("Seeking File Start...\n");
    unsigned long FileStartPointer=SeekFileStart(SourceFile);
    if(FileStartPointer == -1){return 1;}   // Error detection
    printf("File Start Found.\n");


    //Search for file End
    printf("Seeking File End...\n");
    unsigned int FileEndPointer=SeekFileEnd(SourceFile);
    if(FileEndPointer == -1){return 1;}   // Error detection
    printf("File End Found.\n");



    printf("Conversion Starting...\n");



    fseek(SourceFile,FileStartPointer,SEEK_SET);
    fseek(OutputFile, 0, SEEK_SET);

    fputc(0x01, OutputFile);            // File Start (ZFS Protocol)

    while(ftell(SourceFile)<FileEndPointer){
        fputc(fgetc(SourceFile), OutputFile);
    }

    for(unsigned char i=0; i<8; i++){   // File End (ZFS Protocol)
        fputc(0x04, OutputFile);
    }

    printf("Done.\n");

    return 0;
}
