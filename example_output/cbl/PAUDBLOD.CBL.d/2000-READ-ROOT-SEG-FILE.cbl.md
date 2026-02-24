```cobol
       2000-READ-ROOT-SEG-FILE.                                         02020053
      *----------------------------------------------------------------*02030026
      *                                                                 02040026
      *     DISPLAY 'IN 2000 READ ROOT SEG FILE PARA'                   02041061
            READ INFILE1                                                02042053
                                                                        02042153
            IF WS-INFIL1-STATUS =  SPACES OR '00'                       02042253
               MOVE INFIL1-REC TO PENDING-AUTH-SUMMARY                  02042353
               PERFORM 2100-INSERT-ROOT-SEG THRU 2100-EXIT              02042454
            ELSE                                                        02042553
               IF WS-INFIL1-STATUS = '10'                               02042653
                  MOVE 'Y' TO END-ROOT-SEG-FILE                         02042753
               ELSE                                                     02042853
                  DISPLAY 'ERROR READING ROOT SEG INFILE'               02042953
               END-IF                                                   02043053
            END-IF.                                                     02043153
                                                                        02043253
       2000-EXIT.                                                       02043353
            EXIT.                                                       02043453
```
