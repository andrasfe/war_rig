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
       3100-INSERT-CHILD-SEG.                                           02359753
      *                                                                 02360026
      *     DISPLAY 'IN 3100 INSERT CHILD SEG PARA'                     02361061
            INITIALIZE PAUT-PCB-STATUS                                  02362058
            CALL 'CBLTDLI'       USING  FUNC-GU                         02370053
                                        PAUTBPCB                        02380030
                                        PENDING-AUTH-SUMMARY            02390053
                                        ROOT-QUAL-SSA.                  02400053
               DISPLAY '***************************'                    02401043
      *        DISPLAY ' AFTER ROOT SEG GU CALL    '                    02402067
      *        DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                    02410067
      *        DISPLAY 'SEG NAME : ' PAUT-SEG-NAME                      02411067
               DISPLAY '***************************'                    02412043
               IF PAUT-PCB-STATUS = SPACES                              02420030
                  DISPLAY 'GU CALL TO ROOT SEG SUCCESS'                 02430053
      *           ADD 2 TO PA-AUTH-DATE-9C                              02430167
      *           ADD 2 TO PA-AUTH-TIME-9C                              02430267
                  PERFORM 3200-INSERT-IMS-CALL  THRU 3200-EXIT          02430353
               IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'II'         02520059
                  DISPLAY 'ROOT GU CALL FAIL:' PAUT-PCB-STATUS          02530053
                  DISPLAY 'KFB AREA IN CHILD:' PAUT-KEYFB               02531048
                    PERFORM 9999-ABEND                                  02540049
               END-IF.                                                  02550051
       3100-EXIT.                                                       02590053
            EXIT.                                                       02600026
      *                                                                 02610026
       3200-INSERT-IMS-CALL.                                            02611053
      *                                                                 02611153
      *     DISPLAY 'IN 3200 INSERT CALL'                               02611261
                  CALL 'CBLTDLI' USING  FUNC-ISRT                       02611353
                                        PAUTBPCB                        02611453
                                        PENDING-AUTH-DETAILS            02611553
                                        CHILD-UNQUAL-SSA.               02611654
                                                                        02611753
            IF PAUT-PCB-STATUS = SPACES                                 02611866
               DISPLAY 'CHILD SEGMENT INSERTED SUCCESS'                 02611966
            END-IF                                                      02612055
            IF PAUT-PCB-STATUS = 'II'                                   02612166
               DISPLAY 'CHILD SEGMENT ALREADY IN DB'                    02612266
            END-IF                                                      02612366
            IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'II'            02612466
                  DISPLAY 'INSERT CALL FAIL FOR CHILD:' PAUT-PCB-STATUS 02612566
                  DISPLAY 'KFB AREA IN CHILD:' PAUT-KEYFB               02612666
                    PERFORM 9999-ABEND                                  02612766
            END-IF.                                                     02612866
                                                                        02612966
       3200-EXIT.                                                       02613066
            EXIT.                                                       02614066
```
