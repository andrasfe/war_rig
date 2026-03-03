```cobol
      *        DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                    02410057
      *        DISPLAY 'SEG NAME   : ' PAUT-SEG-NAME                    02411057
      *        DISPLAY '***************************'                    02412057
               IF PAUT-PCB-STATUS = SPACES                              02420030
                    SET MORE-AUTHS       TO TRUE                        02430030
                    ADD 1                 TO WS-NO-SUMRY-READ           02440030
                    ADD 1                 TO WS-AUTH-SMRY-PROC-CNT      02450030
                    MOVE PENDING-AUTH-DETAILS TO CHILD-SEG-REC          02460036
                    WRITE OPFIL2-REC                                    02470030
               END-IF                                                   02480030
               IF PAUT-PCB-STATUS = 'GE'                                02490030
      *             SET NO-MORE-AUTHS    TO TRUE                        02500050
                    MOVE 'Y' TO WS-END-OF-CHILD-SEG                     02500150
                    DISPLAY 'CHILD SEG FLAG GE : '                      02501044
                             WS-END-OF-CHILD-SEG                        02502050
               END-IF                                                   02510030
               IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'GE'         02520030
```
