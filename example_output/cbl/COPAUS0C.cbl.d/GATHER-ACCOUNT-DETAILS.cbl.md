```cobol
       GATHER-ACCOUNT-DETAILS.
      *****************************************************************

           PERFORM GETCARDXREF-BYACCT
           PERFORM GETACCTDATA-BYACCT
           PERFORM GETCUSTDATA-BYCUST

           MOVE CUST-ID                TO CUSTIDO
           STRING CUST-FIRST-NAME DELIMITED BY SPACES
                  ' ' DELIMITED BY SIZE
                  CUST-MIDDLE-NAME(1:1) DELIMITED BY SIZE
                  ' ' DELIMITED BY SIZE
                  CUST-LAST-NAME DELIMITED BY SPACES
                  INTO CNAMEO
           END-STRING

           STRING CUST-ADDR-LINE-1 DELIMITED BY '  '
                  ',' DELIMITED BY SIZE
                  CUST-ADDR-LINE-2 DELIMITED BY '  '
                  INTO ADDR001O
           END-STRING
           STRING CUST-ADDR-LINE-3 DELIMITED BY '  '
                  ',' DELIMITED BY SIZE
                  CUST-ADDR-STATE-CD DELIMITED BY SIZE
                  ',' DELIMITED BY SIZE
                  CUST-ADDR-ZIP(1:5) DELIMITED BY SIZE
                  INTO ADDR002O
           END-STRING

           MOVE CUST-PHONE-NUM-1       TO PHONE1O
           MOVE ACCT-CREDIT-LIMIT      TO WS-DISPLAY-AMT12
           MOVE WS-DISPLAY-AMT12       TO CREDLIMO
           MOVE ACCT-CASH-CREDIT-LIMIT TO WS-DISPLAY-AMT9
           MOVE WS-DISPLAY-AMT9        TO CASHLIMO

           PERFORM GET-AUTH-SUMMARY

           IF FOUND-PAUT-SMRY-SEG
              MOVE PA-APPROVED-AUTH-CNT   TO WS-DISPLAY-COUNT
              MOVE WS-DISPLAY-COUNT       TO APPRCNTO
              MOVE PA-DECLINED-AUTH-CNT   TO WS-DISPLAY-COUNT
              MOVE WS-DISPLAY-COUNT       TO DECLCNTO
              MOVE PA-CREDIT-BALANCE      TO WS-DISPLAY-AMT12
              MOVE WS-DISPLAY-AMT12       TO CREDBALO
              MOVE PA-CASH-BALANCE        TO WS-DISPLAY-AMT9
              MOVE WS-DISPLAY-AMT9        TO CASHBALO
              MOVE PA-APPROVED-AUTH-AMT   TO WS-DISPLAY-AMT9
              MOVE WS-DISPLAY-AMT9        TO APPRAMTO
              MOVE PA-DECLINED-AUTH-AMT   TO WS-DISPLAY-AMT9
              MOVE WS-DISPLAY-AMT9        TO DECLAMTO
           ELSE
              MOVE ZERO                   TO APPRCNTO
                                             DECLCNTO
                                             CREDBALO
                                             CASHBALO
                                             APPRAMTO
                                             DECLAMTO
           END-IF
           .


      *****************************************************************
```
