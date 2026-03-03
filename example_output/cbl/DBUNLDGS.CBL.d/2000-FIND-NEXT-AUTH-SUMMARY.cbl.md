```cobol
       2000-FIND-NEXT-AUTH-SUMMARY.                                     01910000
      *----------------------------------------------------------------*01920000
      *                                                                 01930000
      *     DISPLAY 'IN 2000 READ ROOT SEGMENT PARA'                    01940002
      *              PAUT-PCB-STATUS                                    01950000
            INITIALIZE PAUT-PCB-STATUS                                  01960000
            CALL 'CBLTDLI'            USING  FUNC-GN                    01970000
                                        PAUTBPCB                        01980000
                                        PENDING-AUTH-SUMMARY            01990000
                                        ROOT-UNQUAL-SSA.                02000000
      *     DISPLAY ' *******************************'                  02010002
      *     DISPLAY ' AFTER THE ROOT SEG IMS CALL    '                  02020002
      *     DISPLAY 'SEG LEVEL: ' PAUT-SEG-LEVEL                        02030002
      *     DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                       02040002
      *     DISPLAY 'SEG NAME   : ' PAUT-SEG-NAME                       02050002
      *     DISPLAY ' *******************************'                  02060000
               IF PAUT-PCB-STATUS = SPACES                              02070000
      *             SET NOT-END-OF-AUTHDB TO TRUE                       02080000
                    ADD 1                 TO WS-NO-SUMRY-READ           02090000
                    ADD 1                 TO WS-AUTH-SMRY-PROC-CNT      02100000
                    MOVE PENDING-AUTH-SUMMARY TO OPFIL1-REC             02110000
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
      *                                                                 02710000
      *----------------------------------------------------------------*02710100
       3100-INSERT-PARENT-SEG-GSAM.                                     02710200
      *     DISPLAY 'IN 3100 INSERT-PARENT-SEG-GSAM'                    02710302
            CALL 'CBLTDLI'       USING  FUNC-ISRT                       02710400
                                        PASFLPCB                        02710500
                                        PENDING-AUTH-SUMMARY.           02710600
      *        DISPLAY '***************************'                    02710802
      *        DISPLAY ' AFTER PARENT GSAM IMS CALL'                    02710902
      *        DISPLAY ' PASFL-DBDNAME : ' PASFL-DBDNAME                02711002
      *        DISPLAY ' PASFL-PCB-PROCOPT : ' PASFL-PCB-PROCOPT        02711102
      *        DISPLAY 'PCB STATUS: ' PASFL-PCB-STATUS                  02711202
      *        DISPLAY '***************************'                    02711302
               IF PASFL-PCB-STATUS NOT EQUAL TO SPACES                  02711401
                  DISPLAY 'GSAM PARENT FAIL :' PASFL-PCB-STATUS         02711501
                  DISPLAY 'KFB AREA IN GSAM:' PASFL-KEYFB               02711601
                  PERFORM 9999-ABEND                                    02711701
               END-IF.                                                  02711801
       3100-EXIT.                                                       02712000
            EXIT.                                                       02713000
      *----------------------------------------------------------------*02720000
       3200-INSERT-CHILD-SEG-GSAM.                                      02721000
      *     DISPLAY 'IN 3200 INSERT-CHILD-SEG-GSAM'                     02721102
            CALL 'CBLTDLI'       USING  FUNC-ISRT                       02721200
                                        PADFLPCB                        02721300
                                        PENDING-AUTH-DETAILS.           02721400
      *        DISPLAY '***************************'                    02721502
      *        DISPLAY ' AFTER CHILD GSAM IMS CALL'                     02721602
      *        DISPLAY 'PADFL-DBDNAME : ' PADFL-DBDNAME                 02721702
      *        DISPLAY 'PCB STATUS: ' PADFL-PCB-STATUS                  02721802
      *        DISPLAY 'PADFL-PCB-PROCOPT : ' PADFL-PCB-PROCOPT         02721902
      *        DISPLAY '***************************'                    02722002
               IF PADFL-PCB-STATUS NOT EQUAL TO SPACES                  02722101
                  DISPLAY 'GSAM PARENT FAIL :' PADFL-PCB-STATUS         02722201
                  DISPLAY 'KFB AREA IN GSAM:' PADFL-KEYFB               02722301
                  PERFORM 9999-ABEND                                    02722401
               END-IF.                                                  02722501
       3200-EXIT.                                                       02722601
            EXIT.                                                       02723000
```
