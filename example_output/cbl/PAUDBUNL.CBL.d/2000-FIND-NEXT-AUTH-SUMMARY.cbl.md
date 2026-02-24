```cobol
       2000-FIND-NEXT-AUTH-SUMMARY.                                     02020026
      *----------------------------------------------------------------*02030026
      *                                                                 02040026
      *     DISPLAY 'IN 2000 READ ROOT SEGMENT PARA'                    02041057
      *              PAUT-PCB-STATUS                                    02065050
            INITIALIZE PAUT-PCB-STATUS                                  02066047
            CALL 'CBLTDLI'            USING  FUNC-GN                    02070034
                                        PAUTBPCB                        02080029
                                        PENDING-AUTH-SUMMARY            02090029
                                        ROOT-UNQUAL-SSA.                02100029
      *     DISPLAY ' *******************************'                  02130057
      *     DISPLAY ' AFTER THE ROOT SEG IMS CALL    '                  02130157
      *     DISPLAY 'SEG LEVEL: ' PAUT-SEG-LEVEL                        02132057
      *     DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                       02133057
      *     DISPLAY 'SEG NAME   : ' PAUT-SEG-NAME                       02135057
      *     DISPLAY ' *******************************'                  02138043
               IF PAUT-PCB-STATUS = SPACES                              02140029
      *             SET NOT-END-OF-AUTHDB TO TRUE                       02160050
                    ADD 1                 TO WS-NO-SUMRY-READ           02170026
                    ADD 1                 TO WS-AUTH-SMRY-PROC-CNT      02180026
                    MOVE PENDING-AUTH-SUMMARY TO OPFIL1-REC             02190030
                    INITIALIZE ROOT-SEG-KEY                             02190156
                    INITIALIZE CHILD-SEG-REC                            02190256
                    MOVE PA-ACCT-ID           TO ROOT-SEG-KEY           02190356
      *             DISPLAY 'WRITING FIRST FILE'                        02190456
                    IF PA-ACCT-ID IS NUMERIC                            02190556
                    WRITE OPFIL1-REC                                    02190656
                    INITIALIZE WS-END-OF-CHILD-SEG                      02190756
                    PERFORM 3000-FIND-NEXT-AUTH-DTL THRU 3000-EXIT      02190856
                    UNTIL  WS-END-OF-CHILD-SEG='Y'                      02190956
                    END-IF                                              02191056
               END-IF                                                   02191156
               IF PAUT-PCB-STATUS = 'GB'                                02192029
                    SET END-OF-AUTHDB     TO TRUE                       02194029
                    MOVE 'Y' TO WS-END-OF-ROOT-SEG                      02195050
               END-IF                                                   02197029
               IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'GB'         02200029
                  DISPLAY 'AUTH SUM  GN FAILED  :' PAUT-PCB-STATUS      02230029
                  DISPLAY 'KEY FEEDBACK AREA    :' PAUT-KEYFB           02240048
                    PERFORM 9999-ABEND                                  02260026
            .                                                           02280026
       2000-EXIT.                                                       02290026
            EXIT.                                                       02300026
```
