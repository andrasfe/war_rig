```cobol
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  MOVE XREF-CUST-ID               TO CDEMO-CUST-ID
                  MOVE XREF-CARD-NUM              TO CDEMO-CARD-NUM
               WHEN DFHRESP(NOTFND)
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Account:'
                   WS-ACCT-ID
                  ' not found in XREF file. Resp:' WS-RESP-CD-DIS
                  ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' System error while reading XREF file. Resp:'
                  WS-RESP-CD-DIS ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE
           .

      *****************************************************************
       GETACCTDATA-BYACCT.
      *****************************************************************

           MOVE XREF-ACCT-ID            TO WS-CARD-RID-ACCT-ID
           EXEC CICS READ
                DATASET   (WS-ACCTFILENAME)
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
                INTO      (ACCOUNT-RECORD)
                LENGTH    (LENGTH OF ACCOUNT-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  continue
               WHEN DFHRESP(NOTFND)
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' not found in ACCT file. Resp:' WS-RESP-CD-DIS
                  ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Account:'
                   WS-CARD-RID-ACCT-ID-X
                  ' System error while reading ACCT file. Resp:'
                  WS-RESP-CD-DIS ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE
           .

      *****************************************************************
       GETCUSTDATA-BYCUST.
      *****************************************************************

           MOVE XREF-CUST-ID              TO WS-CARD-RID-CUST-ID

           EXEC CICS READ
                DATASET   (WS-CUSTFILENAME)
                RIDFLD    (WS-CARD-RID-CUST-ID-X)
                KEYLENGTH (LENGTH OF WS-CARD-RID-CUST-ID-X)
                INTO      (CUSTOMER-RECORD)
                LENGTH    (LENGTH OF CUSTOMER-RECORD)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                  CONTINUE
               WHEN DFHRESP(NOTFND)
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Customer:'
                   WS-CARD-RID-CUST-ID-X
                  ' not found in CUST file. Resp:' WS-RESP-CD-DIS
                  ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG
                  MOVE WS-RESP-CD        TO WS-RESP-CD-DIS
                  MOVE WS-REAS-CD        TO WS-REAS-CD-DIS

                  STRING
                  'Customer:'
                   WS-CARD-RID-CUST-ID-X
                  ' System error while reading CUST file. Resp:'
```
