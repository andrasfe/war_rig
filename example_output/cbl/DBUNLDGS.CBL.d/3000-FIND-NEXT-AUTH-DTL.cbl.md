```cobol
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
