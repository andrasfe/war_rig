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
```
