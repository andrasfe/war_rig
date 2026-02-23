           ACCESS MODE  IS SEQUENTIAL                                   00120026
           FILE STATUS IS WS-INFIL1-STATUS.                             00130053
                                                                        00140026
      *                                                                 00150026
           SELECT INFILE2 ASSIGN TO INFILE2                             00151053
           ORGANIZATION IS SEQUENTIAL                                   00152026
           ACCESS MODE  IS SEQUENTIAL                                   00153026
           FILE STATUS IS WS-INFIL2-STATUS.                             00154053
                                                                        00155026
      *                                                                 00156026
      *----------------------------------------------------------------*00160026
       DATA DIVISION.                                                   00170026
      *----------------------------------------------------------------*00180026
      *                                                                 00190026
       FILE SECTION.                                                    00200026
       FD INFILE1.                                                      00210053
       01 INFIL1-REC                    PIC X(100).                     00220053
       FD INFILE2.                                                      00221053
       01 INFIL2-REC.                                                   00222053
