```cobol
      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT                   01210000
       01 PENDING-AUTH-SUMMARY.                                         01220000
       COPY CIPAUSMY.                                                   01230000
                                                                        01240000
      *- PENDING AUTHORIZATION DETAILS SEGMENT - CHILD                  01250000
       01 PENDING-AUTH-DETAILS.                                         01260000
       COPY CIPAUDTY.                                                   01270000
                                                                        01280000
      *                                                                 01290000
      *----------------------------------------------------------------*01300000
       LINKAGE SECTION.                                                 01310000
      *----------------------------------------------------------------*01320000
      * PCB MASKS FOLLOW                                                01330000
        COPY PAUTBPCB.                                                  01340000
        COPY PASFLPCB.                                                  01341000
        COPY PADFLPCB.                                                  01342000
      *                                                                 01350000
      *----------------------------------------------------------------*01360000
       PROCEDURE DIVISION                  USING PAUTBPCB               01370000
                                                 PASFLPCB               01380000
                                                 PADFLPCB.              01381000
      *----------------------------------------------------------------*01390000
      *                                                                 01400000
       MAIN-PARA.                                                       01410000
            ENTRY 'DLITCBL'                 USING PAUTBPCB              01420000
                                                  PASFLPCB              01421000
                                                  PADFLPCB.             01422000
                                                                        01430000
      *                                                                 01440000
           PERFORM 1000-INITIALIZE                THRU 1000-EXIT        01450000
      *                                                                 01460000
           PERFORM 2000-FIND-NEXT-AUTH-SUMMARY    THRU 2000-EXIT        01470000
           UNTIL   WS-END-OF-ROOT-SEG = 'Y'                             01480000
                                                                        01490000
           PERFORM 4000-FILE-CLOSE THRU 4000-EXIT                       01500000
      *                                                                 01510000
      *                                                                 01520000
      *                                                                 01530000
           GOBACK.                                                      01540000
```
