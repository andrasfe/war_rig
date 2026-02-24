```cobol
       MAIN-PARA.                                                       01220026
            ENTRY 'DLITCBL'                 USING PAUTBPCB.             01225033
                                                                        01226029
      *                                                                 01230026
           PERFORM 1000-INITIALIZE                THRU 1000-EXIT        01240026
      *                                                                 01250026
           PERFORM 2000-FIND-NEXT-AUTH-SUMMARY    THRU 2000-EXIT        01260026
           UNTIL   WS-END-OF-ROOT-SEG = 'Y'                             01280050
                                                                        01531150
           PERFORM 4000-FILE-CLOSE THRU 4000-EXIT                       01532030
      *                                                                 01540026
      *                                                                 01560026
      *                                                                 01650026
           GOBACK.                                                      01660026
      *                                                                 01670026
      *----------------------------------------------------------------*01680026
```
