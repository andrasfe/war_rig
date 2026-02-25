```cobol
              SET IMS-PSB-NOT-SCHD      TO TRUE
              EXEC CICS SYNCPOINT
              END-EXEC
           END-IF

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF COPAU0AO

           IF SEND-ERASE-YES
               EXEC CICS SEND
                         MAP('COPAU0A')
                         MAPSET('COPAU00')
                         FROM(COPAU0AO)
                         ERASE
                         CURSOR
               END-EXEC
           ELSE
               EXEC CICS SEND
                         MAP('COPAU0A')
                         MAPSET('COPAU00')
                         FROM(COPAU0AO)
                         CURSOR
               END-EXEC
           END-IF.

      *****************************************************************
       RECEIVE-PAULST-SCREEN.
      *****************************************************************

           EXEC CICS RECEIVE
                     MAP('COPAU0A')
                     MAPSET('COPAU00')
                     INTO(COPAU0AI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC
           .


      *****************************************************************
       POPULATE-HEADER-INFO.
      *****************************************************************

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COPAU0AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COPAU0AO
           MOVE WS-CICS-TRANID         TO TRNNAMEO OF COPAU0AO
           MOVE WS-PGM-AUTH-SMRY       TO PGMNAMEO OF COPAU0AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COPAU0AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COPAU0AO
           .

      *****************************************************************
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
       GETCARDXREF-BYACCT.
      *****************************************************************

      *    Read the Card file. Access via alternate index ACCTID
      *
           MOVE WS-ACCT-ID          TO WS-CARD-RID-ACCT-ID-X
           EXEC CICS READ
                DATASET   (WS-CARDXREFNAME-ACCT-PATH)
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
                INTO      (CARD-XREF-RECORD)
                LENGTH    (LENGTH OF CARD-XREF-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
```
