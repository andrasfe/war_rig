```cobol
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

```
