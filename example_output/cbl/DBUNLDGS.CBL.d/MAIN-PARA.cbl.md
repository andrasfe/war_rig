```cobol
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
