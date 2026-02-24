```cobol
       3000-READ-CHILD-SEG-FILE.                                        02340053
      *----------------------------------------------------------------*02350026
      *     DISPLAY 'IN 3000 READ CHILD SEG FILE PARA'                  02351067
            READ INFILE2                                                02352053
                                                                        02353053
            IF WS-INFIL2-STATUS =  SPACES OR '00'                       02354053
               IF ROOT-SEG-KEY IS NUMERIC                               02354162
      *        DISPLAY 'GNGTO ROOT SEG KEY'                             02355067
               MOVE ROOT-SEG-KEY  TO QUAL-SSA-KEY-VALUE                 02355260
      *        DISPLAY 'ROOT-SEG-KEY : '    QUAL-SSA-KEY-VALUE          02355367
      *        DISPLAY 'MOVED ROOT SEG KEY'                             02355467
               MOVE CHILD-SEG-REC TO PENDING-AUTH-DETAILS               02355562
               PERFORM 3100-INSERT-CHILD-SEG THRU 3100-EXIT             02356054
               END-IF                                                   02356162
            ELSE                                                        02357053
               IF WS-INFIL2-STATUS = '10'                               02358053
                  MOVE 'Y' TO END-CHILD-SEG-FILE                        02359053
               ELSE                                                     02359153
                  DISPLAY 'ERROR READING CHILD SEG INFILE'              02359253
               END-IF                                                   02359353
            END-IF.                                                     02359453
       3000-EXIT.                                                       02359553
            EXIT.                                                       02359653
```
