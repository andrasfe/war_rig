```cobol
                    INITIALIZE ROOT-SEG-KEY                             02120000
                    INITIALIZE CHILD-SEG-REC                            02130000
                    MOVE PA-ACCT-ID           TO ROOT-SEG-KEY           02140000
      *             DISPLAY 'WRITING FIRST FILE'                        02150000
                    IF PA-ACCT-ID IS NUMERIC                            02160000
      *             WRITE OPFIL1-REC                                    02170000
                    PERFORM 3100-INSERT-PARENT-SEG-GSAM THRU 3100-EXIT  02171000
                    INITIALIZE WS-END-OF-CHILD-SEG                      02180000
                    PERFORM 3000-FIND-NEXT-AUTH-DTL THRU 3000-EXIT      02190000
                    UNTIL  WS-END-OF-CHILD-SEG='Y'                      02200000
                    END-IF                                              02210000
               END-IF                                                   02220000
               IF PAUT-PCB-STATUS = 'GB'                                02230000
                    SET END-OF-AUTHDB     TO TRUE                       02240000
                    MOVE 'Y' TO WS-END-OF-ROOT-SEG                      02250000
               END-IF                                                   02260000
               IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'GB'         02270000
                  DISPLAY 'AUTH SUM  GN FAILED  :' PAUT-PCB-STATUS      02280000
                  DISPLAY 'KEY FEEDBACK AREA    :' PAUT-KEYFB           02290000
                    PERFORM 9999-ABEND                                  02300000
            .                                                           02310000
       2000-EXIT.                                                       02320000
            EXIT.                                                       02330000
      *                                                                 02340000
      *                                                                 02350000
      *----------------------------------------------------------------*02360000
       3000-FIND-NEXT-AUTH-DTL.                                         02370000
      *----------------------------------------------------------------*02380000
      *                                                                 02390000
      *     DISPLAY 'IN 3000 READ CHILD SEGMENT PARA'                   02400002
            CALL 'CBLTDLI'            USING  FUNC-GNP                   02410000
                                        PAUTBPCB                        02420000
                                        PENDING-AUTH-DETAILS            02430000
                                        CHILD-UNQUAL-SSA.               02440000
      *        DISPLAY '***************************'                    02450002
      *        DISPLAY ' AFTER CHILD SEG IMS CALL  '                    02460002
      *        DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                    02470002
      *        DISPLAY 'SEG NAME   : ' PAUT-SEG-NAME                    02480002
      *        DISPLAY '***************************'                    02490002
               IF PAUT-PCB-STATUS = SPACES                              02500000
                    SET MORE-AUTHS       TO TRUE                        02510000
                    ADD 1                 TO WS-NO-SUMRY-READ           02520000
                    ADD 1                 TO WS-AUTH-SMRY-PROC-CNT      02530000
                    MOVE PENDING-AUTH-DETAILS TO CHILD-SEG-REC          02540000
      *             WRITE OPFIL2-REC                                    02550000
                    PERFORM 3200-INSERT-CHILD-SEG-GSAM THRU 3200-EXIT   02551000
               END-IF                                                   02560000
               IF PAUT-PCB-STATUS = 'GE'                                02570000
      *             SET NO-MORE-AUTHS    TO TRUE                        02580000
                    MOVE 'Y' TO WS-END-OF-CHILD-SEG                     02590000
                    DISPLAY 'CHILD SEG FLAG GE : '                      02600000
                             WS-END-OF-CHILD-SEG                        02610000
               END-IF                                                   02620000
               IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'GE'         02630000
                  DISPLAY 'GNP CALL FAILED  :' PAUT-PCB-STATUS          02640000
                  DISPLAY 'KFB AREA IN CHILD:' PAUT-KEYFB               02650000
                    PERFORM 9999-ABEND                                  02660000
               END-IF.                                                  02670000
               INITIALIZE PAUT-PCB-STATUS.                              02680000
       3000-EXIT.                                                       02690000
            EXIT.                                                       02700000
```
