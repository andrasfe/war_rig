```cobol
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
      *                                                                 02310026
      *                                                                 02320026
      *----------------------------------------------------------------*02330026
       3000-FIND-NEXT-AUTH-DTL.                                         02340026
      *----------------------------------------------------------------*02350026
      *                                                                 02360026
      *     DISPLAY 'IN 3000 READ CHILD SEGMENT PARA'                   02361057
            CALL 'CBLTDLI'            USING  FUNC-GNP                   02370034
                                        PAUTBPCB                        02380030
                                        PENDING-AUTH-DETAILS            02390030
                                        CHILD-UNQUAL-SSA.               02400030
```
